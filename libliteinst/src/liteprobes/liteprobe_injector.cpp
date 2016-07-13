
#include "liteprobe_injector.hpp"
#include "control_flow_router.hpp"
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
// map<Address, unique_ptr<Probe>> LiteProbeInjector::probes;

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

      target = addr + decoded[index].size + rel_copy;
      if ((int64_t) target > 0 && target > text.end) {
        if (!(stack.withinRange(target, true) 
            || heap.withinRange(target, true))) {
          // fprintf(stderr, "[Allocator] Trying : %p\n", target);
          unique_ptr<Allocator> allocator = 
            AllocatorFactory::getAllocator(AllocatorType::FIXED);

          target = allocator->getAllocation(target, size);
          if (target != nullptr) {
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
     ProbeContext& context, InstrumentationFunction fn, 
     const Sequence* seq) {

  Address addr = cp.probes.front();

  Disassembler disas;
  int index = disas.findInstructionIndex(addr, seq);
  _DInst* decoded = static_cast<_DInst*>(seq->instructions);

  CodeJitter cj;
  int64_t trampoline_size = cj.getTrampolineSize(cp.range.end - cp.range.start);

  Address target = punAddress(addr, trampoline_size, seq, index);
  if (target != nullptr) {
    int32_t relative = target - addr - decoded[index].size;
    
    unique_ptr<Springboard> sb = cj.emitSpringboard(cp, target, context, fn);
    sb->relative_jump = relative;
    return sb;
    // patch addr
    //
  } else {
    return nullptr;
  }
}

list<CoalescedProbes> coalesceProbes(utils::process::Function* fn,
    vector<Address>& addrs, const Sequence* seq) {

  _DInst* decoded = static_cast<_DInst*>(seq->instructions);
  list<CoalescedProbes> cps;

  Disassembler disas;
  // Coalesce probes by proximity
  for (unsigned i = addrs.size()-2; i >= 0; i--) {
    CoalescedProbes cp;
    list<Address> probes;
    probes.push_back(addrs[i]);
    int index = disas.findInstructionIndex(addrs[i], seq);
    cp.range.end = addrs[i] + decoded[index].size ;
    Address start = addrs[i];
    while (i > 0 && addrs[i] - addrs[i-1] <= 5) {
      probes.push_back(addrs[i-1]);
      start = addrs[i-1];
      i--;
    }
    cp.range.start = start;
    cp.probes = probes;
    cps.push_back(cp);
  }

  // Handle the last probe separately because it might need more space
  // if it happens to be near the end of the function.
  Address last = addrs.back();
  int index = disas.findInstructionIndex(last, seq);

  Address last_probe_start = last;
  while (last_probe_start - fn->end < 5 && index >= 0) {
    last_probe_start -= decoded[--index].size;
  }

  // Handles the case where part of the probe belonging to this instruction may
  // already have been punned already when instrumenting an instruction 
  // following this instruction. In that case the probe end should include the 
  // current probe length for proper springboard processing later on.
  int last_index = disas.findInstructionIndex(last, seq);
  int probe_size = 0;
  while (probe_size < 5 && last_index < seq->n_instructions) {
    probe_size += decoded[last_index].size;
    last_index++; 
  }

  Address last_probe_end = last + probe_size;


  // If an existing coalesced probe (has to be the first one since we are 
  // at the end of the function and we are inserting probes in reverse order of
  // addresses) overlaps with the last probes range coalesce 
  // it with that coalsced probe. If not just create a new coalesced probe.
  if (cps.front().range.withinRange(last_probe_start, true)) {
    cps.front().range.end = last + decoded[index].size;
  } else {
    CoalescedProbes cp;
    list<Address> probes;
    probes.push_back(last);
    cp.range = Range(last_probe_start, last_probe_end);
    cp.probes = probes;
    // Add it to the front since we want later probes first
    cps.push_front(cp);
  }
  
  ControlFlowRouter router;
  // Now do a second iteration of coalescing of any existing springboards 
  // which are overlapping with the coalesced probes.
  for (CoalescedProbes& cp : cps) {
    list<Springboard*> sbs = router.getOverlappingSpringboards(cp.range); 
    for (Springboard* sb : sbs) {
      cp.range.unionRange(sb->range);
      cp.probes.insert(cp.probes.end(), sb->probed_addrs.begin(),
          sb->probed_addrs.end());
      cp.springboards.push_back(sb);
    }
  }

  // Finally coalesce all the coalesced probes which may now be overlapping 
  // with each other due to coalescing with springboards.
  for (auto it = cps.begin(); it != cps.end(); ) {
    CoalescedProbes& cp = *it;
    while (++it != cps.end() && (*it).range.overlapsWith(cp.range)) {
     cp.range.unionRange((*it).range); 
     cp.probes.insert(cp.probes.end(), (*it).probes.begin(), (*it).probes.end());
     cp.springboards.insert(cp.springboards.end(), (*it).springboards.begin(), 
         (*it).springboards.end());
     cps.erase(it++);
    }
  }

  // Now remove duplicates
  for (CoalescedProbes& cp : cps) {
    cp.probes.sort();
    cp.probes.unique();
    cp.springboards.sort();
    cp.springboards.unique();
  }

  return cps;

}

bool LiteProbeInjector::injectProbes(list<Address>& addresses, 
    ProbeContext& context, InstrumentationFunction trampoline_fn) {
  addresses.sort();
  vector<Address> addrs;
  for(auto it = addresses.rbegin(); it != addresses.rend(); it++) {
    Address addr = *it;

    addrs.push_back(addr);
    auto it1 = probes_by_addr.find(addr);
    if (it1 != probes_by_addr.end()) {
      return false;
    }
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
    unique_ptr<Springboard> sb = makeSpringboard(cp, context, trampoline_fn, 
        seq);

    if (sb == nullptr) {
      // Release all the created springboards
      goto fail; // Fail fast. Either we probe all of them or none.
      // For the backtracking implementation gather the failed springboards and
      // then try to do back tracking while also considering current coalesced
      // probes.
    } else {
      sbs.push_back(move(sb));
      cps_map.insert(pair<Address, const CoalescedProbes>(sb->base, cp));
    }
  }

  for (auto& springboard : sbs) {
    // Inform of this springboard to the control flow router
    ControlFlowRouter router;

    // Making a temporary non owning copy before we transfer ownership to 
    // ControlFlowRouter
    Springboard* sb = springboard.get();
    router.addSpringboard(unique_ptr<Springboard>(sb));

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
          ip += decoded[++index].size;
        }

        assert(ip == cp.range.end);
        break;
      }
 
      while (ip < (*it)->base) {
        *ip = 0x62; // Single byte write. Should be safe. 
        ip += decoded[++index].size;
      }

      assert(ip == (*it)->base);

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

}
}
