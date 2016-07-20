
#include "liteprobe_injector.hpp"
#include "control_flow_router.hpp"
#include "patcher.h"
#include "assembly.hpp"
#include "process.hpp"
#include "addr_range.hpp"
#include "alloc.hpp"
#include "code_jitter.hpp"

#include <algorithm>
#include <cstdlib>
#include <cassert>
#include <stdexcept>
#include <memory>

namespace liteinst {
namespace liteprobes {

using namespace utils::process;
using namespace utils::range;
using namespace utils::alloc;
using namespace utils::assembly;

using std::abort;
using std::list;
using std::map;
using std::array;
using std::pair;
using std::vector;
using utils::Address;
using std::move;
using std::unique_ptr;
using std::invalid_argument;

const array<uint8_t, 14> invalid_opcodes = {0x06, 0x07, 0x0E, 0x16, 0x17,
        0x1E, 0x1F, 0x27, 0x2F, 0x37, 0x3F, 0x60, 0x61, 0x62};

map<Address, Probe*> LiteProbeInjector::probes_by_addr;
vector<unique_ptr<Probe>> LiteProbeInjector::probes;
map<Address, unique_ptr<Springboard>> LiteProbeInjector::relocations;

Address punAddress(Address addr, int64_t size,
    const Sequence* seq, int index) {
  _DInst* decoded = static_cast<_DInst*>(seq->instructions);

  int8_t probe_size = 0;
  uint8_t ins_boundaries[5] = {0};
  int64_t i = index;
  uint8_t clobbered_instruction_count = 0;
  uint8_t current_boundary = 0;
  while (probe_size < 5 && i < seq->n_instructions) {
    probe_size += decoded[i].size;
    ins_boundaries[i - index] = current_boundary + decoded[i].size - 1;
    current_boundary += decoded[i].size;
    i++; clobbered_instruction_count++;
  }

  assert(probe_size >= 5);

  int32_t rel_addr = 0x0;
  int32_t int3_mask = 0x62;
  for (int i=0; i < clobbered_instruction_count - 1; i++) { 
    rel_addr |= (int3_mask << (ins_boundaries[i] * 8));
  }

  Process p;
  Range stack = p.getStack();
  Range heap = p.getHeap();
  Range text = p.getText();
  Address target = nullptr;
  for (int i = invalid_opcodes.size() - 1; i >= 0 ; i--) {
    for (int j = clobbered_instruction_count - 2; j >= 0 ; j--) {
      int32_t rel_copy = rel_addr;
      if (((uint8_t*)&rel_addr)[ins_boundaries[j]] == 0x62) {
        // fprintf(stderr, "\n[swap] A : %p\n", rel_addr);
        // fprintf(stderr, "[swap]Swapped MSB..\n");
        ((uint8_t*)&rel_copy)[ins_boundaries[j]] = invalid_opcodes[i];
        // fprintf(stderr, "[swap] B : %p\n\n", rel_addr);
      }

      target = addr + 5 + rel_copy;
      if ((int64_t) target > 0 && target > text.end) {
        if (!(stack.withinRange(target, Range::INCLUSIVE) 
            || heap.withinRange(target, Range::INCLUSIVE))) {
          // fprintf(stderr, "[Allocator] Trying : %p\n", target);
          unique_ptr<Allocator> allocator = 
            AllocatorFactory::getAllocator(AllocatorType::FIXED);

          target = allocator->getAllocation(target, size);
          if (target != nullptr) {
            printf("Trampoline at : %p\n", target);
            goto exit;
          }
        }
      }
    }
  }

exit:
  return target;
}

unique_ptr<Springboard> makeSpringboard(const CoalescedProbes& cp, 
     const Sequence* seq, const InstrumentationProvider& provider) {

  Address addr = cp.range.start;

  Disassembler disas;
  int index = disas.findInstructionIndex(addr, seq);
  _DInst* decoded = static_cast<_DInst*>(seq->instructions);

  CodeJitter cj;
  int64_t springboard_size = cj.getSpringboardSize(cp);

  Address target = punAddress(addr, springboard_size, seq, index);
  if (target != nullptr) {
    int32_t relative = target - addr - 5;
    uint64_t punned = *reinterpret_cast<uint64_t*>(addr);
    *reinterpret_cast<uint8_t*>(&punned) = 0xe9; 
    *reinterpret_cast<uint32_t*>(&(reinterpret_cast<uint8_t*>(&punned)[1])) = 
      relative; 
    
    unique_ptr<Springboard> sb = cj.emitSpringboard(cp, target, provider);
    sb->punned = punned;
    sb->original = *reinterpret_cast<uint64_t*>(addr);

    return move(sb);
    // patch addr
    //
  } else {
    return nullptr;
  }
}

list<CoalescedProbes> LiteProbeInjector::coalesceProbes(
    utils::process::Function* fn, vector<Address>& addrs, const Sequence* seq) {

  _DInst* decoded = static_cast<_DInst*>(seq->instructions);
  list<CoalescedProbes> cps;

  Disassembler disas;
  // Coalesce probes by proximity
  for (int i = (int) addrs.size()-1; i >= 0; i--) {
    CoalescedProbes cp;
    map<Address, Probe*> probes;

    Probe* p = probes_by_addr.find(addrs[i])->second;
    probes.emplace(addrs[i], p);

    int index = disas.findInstructionIndex(addrs[i], seq);
    cp.range.end = addrs[i] + decoded[index].size ;
    Address start = addrs[i];
    while (i > 0 && addrs[i] - addrs[i-1] <= 5) {
      p = probes_by_addr.find(addrs[i])->second;
      probes.emplace(addrs[i-1], p);
      start = addrs[i-1];
      i--;
    }

    cp.range.start = start;

    if (cp.range.end - cp.range.start < 5) {
      int probe_size = cp.range.end - cp.range.start;
      if (cp.range.start - fn->start < 5) {
        int index = disas.findInstructionIndex(cp.range.end, seq);
        while (probe_size < 5 && index < seq->n_instructions) {
          probe_size += decoded[index++].size;
        }

        assert(probe_size >= 5);

        cp.range.end = cp.range.start + probe_size;
      } else {
        int index = disas.findInstructionIndex(cp.range.start, seq);
        while (probe_size < 5 && index > 0) {
          probe_size += decoded[--index].size;
        }

        assert(probe_size >= 5);

        cp.range.start = cp.range.end - probe_size;
      } 
    }

    cp.probes = probes;
    cps.push_back(cp);
  }

  ControlFlowRouter router;
  // Now do a second iteration of coalescing of any existing springboards 
  // which are overlapping with the coalesced probes.
  for (CoalescedProbes& cp : cps) {
    list<Springboard*> sbs = router.getOverlappingSpringboards(cp.range); 
    for (Springboard* sb : sbs) {
      cp.range.unionRange(sb->displaced);
      cp.probes.insert(sb->probes.begin(), sb->probes.end());
      cp.springboards.push_back(sb);
    }
  }

  // Finally coalesce all the coalesced probes which may now be overlapping 
  // with each other due to coalescing with springboards.
  auto it = cps.begin();
  CoalescedProbes& current = *it;
  while (++it != cps.end()) {
    CoalescedProbes& next = *it;

    if (current.range.overlapsWith(next.range, Range::EXCLUSIVE)) {
      current.range.unionRange(next.range); 
      current.probes.insert(next.probes.begin(), next.probes.end());
      current.springboards.insert(current.springboards.end(), 
        next.springboards.begin(), next.springboards.end());
      cps.erase(it++);
    } else {
      current = next;
    }
  }

  // Now remove duplicates and add some more meta data
  for (CoalescedProbes& cp : cps) {
    cp.springboards.sort();
    cp.springboards.unique();

    int index = disas.findInstructionIndex(cp.range.end, seq);
    cp.is_end_a_control_transfer = disas.isControlTransferInstruction(seq, 
        --index);
  }

  return cps;

}

bool LiteProbeInjector::injectProbes(map<Address, ProbeContext>& locs,
    const InstrumentationProvider& provider) {
  vector<Address> addrs;
  for(auto it = locs.begin(); it != locs.end(); it++) {
    Address addr = it->first;

    addrs.push_back(addr);
    auto it1 = probes_by_addr.find(addr);
    if (it1 != probes_by_addr.end()) {
      return false;
    }
  }

  for(auto it : locs) {
    Probe* probe = new Probe;

    meta_data_lock.writeLock();
    probe->p_id = static_cast<RegistrationId>(probes.size());
    probes.emplace_back(unique_ptr<Probe>(probe));
    meta_data_lock.writeUnlock();

    probe->address = it.first;
    probe->context = it.second;
    probe->context.p_id = probe->p_id;
    probe->context.i_id = provider.id;
    probes_by_addr.emplace(it.first, probe);
  }

  Process p;
  utils::process::Function* start_probe_fn = p.getContainedFunction(
      addrs.front());
  utils::process::Function* end_probe_fn = p.getContainedFunction(addrs.back());

  if ((!start_probe_fn->is_valid || !end_probe_fn->is_valid) &&
       start_probe_fn != end_probe_fn) {
    throw invalid_argument("All probe addresses should be within a single " 
        "function");
  } 

  if ((start_probe_fn->end - start_probe_fn->start) < 5) {
    return false;
  }

  utils::process::Function* fn = start_probe_fn;
  Disassembler disas;
  const Sequence* seq = disas.disassemble(fn->start, fn->end);
  _DInst* decoded = static_cast<_DInst*>(seq->instructions);

  list<CoalescedProbes> cps = coalesceProbes(fn, addrs, seq);

  list<unique_ptr<Springboard>> sbs;
  map<Address, const CoalescedProbes> cps_map;
  for (const CoalescedProbes& cp : cps) {

    // Create the new springboard for this coalesced probe
    unique_ptr<Springboard> sb = makeSpringboard(cp, seq, provider);

    if (sb == nullptr) {
      // Release all the created springboards
      goto fail; // Fail fast. Either we probe all of them or none.
      // For the backtracking implementation gather the failed springboards and
      // then try to do back tracking while also considering current coalesced
      // probes.
    } else {
      Springboard* temp_ptr = sb.get();
      sbs.push_back(move(sb));
      cps_map.insert(pair<Address, const CoalescedProbes>(temp_ptr->base, cp));
    }
  }

  for (auto& springboard : sbs) {
    // Inform of this springboard to the control flow router
    ControlFlowRouter router;

    // Making a temporary non owning copy before we transfer ownership to 
    // ControlFlowRouter
    Springboard* sb = springboard.get();
    router.addSpringboard(move(springboard));

    init_patch_site(sb->base, (sb->displaced.end - sb->displaced.start));

    uint64_t original = *reinterpret_cast<uint64_t*>(sb->base);
    uint64_t mask = 0x000000FFFFFFFFFF;
    uint64_t punned_masked = sb->punned & mask;
    uint64_t original_masked = original & ~mask;

    uint64_t punned = original_masked | punned_masked; 

    // patch in the the springboard jump 
    patch_64(sb->base, punned);
    // *reinterpret_cast<uint64_t*>(sb->base) = sb->punned;

    assert(*reinterpret_cast<uint64_t*>(sb->base) == punned);
    printf("PUNNED : %p\n", punned);

    CoalescedProbes cp;
    auto res = cps_map.find(sb->base);
    if (res != cps_map.end()) {
      cp = res->second;
    } else {
      abort();
    }

    // Now we need to release any existing springboards this new sprinboard 
    // subsumes. We need to do it carefully so as not to expose any 
    // inconsistent state to control flow routing mechanism which may be
    // serving many requests about this region concurrently.
    list<Springboard*> subsumed = cp.springboards;

    // subsumed should already be sorted according to springboard start addresses. 
    // But making sure. Make this an assert instead : TODO
    /*
    sort(subsumed.begin(), subsumed.end(), 
          [](const Springboard* a, const Springboard* b) -> bool { 
              return a->base > b->base; 
             });
             */

    Address addr = cp.range.start;
    Address ip = addr + sb->probe_length;
    int index = disas.findInstructionIndex(ip, seq);
    auto it = subsumed.begin();
    bool no_springboards = false;
    while (ip < cp.range.end && index < seq->n_instructions) {

      no_springboards = (it == subsumed.end()) ? true : false;
      if (no_springboards) {
        while (ip < cp.range.end && index < seq->n_instructions) {
          *ip = 0x62; // Single byte write. Should be safe. 
          ip += decoded[index++].size;
        }

        assert(ip == cp.range.end);
        break;
      }
 
      while (ip < (*it)->base) {
        *ip = 0x62; // Single byte write. Should be safe. 
        ip += decoded[++index].size;
      }

      assert(ip == (*it)->base);

      *ip = 0x62; // Single byte write. Should be safe. 

      // Remove the springboard from the consideration of router
      router.removeSpringboard(*it);

      // Skip the spring board punned probe
      ip += (*it)->probe_length;
      ++it;
    }
  }

  return true;

fail:
  // Release all springboards
  return false;

    // Check function boundaries
    // Backtrack if necessary
    // Check if it is within a trampoline
    // If not {
    //   Try to pun
    //   Backtrack until punnable instruction is met
    //   Generate trampoline or super trampoline
    //   Generate probe and trampoline meta data
    // } else {
    //   Try pun trampoline'd instruction with a new super trampoline
    //   Back track until punnable instruction is met
    //   Generate super trampoline
    //   Generate probe and trampoline meta data
    // }


}

Probe* LiteProbeInjector::getProbe(Address address) {
  auto it = probes_by_addr.find(address);
  if (it != probes_by_addr.end()) {
    return it->second;
  } 
  return nullptr;
}

}
}
