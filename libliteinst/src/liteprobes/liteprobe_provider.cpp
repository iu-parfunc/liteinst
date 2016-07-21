
#include "liteprobe_provider.hpp"
#include "liteprobe_injector.hpp"
#include "control_flow_router.hpp"
#include "strings.hpp"
#include "patcher.h"
#include <string>
#include <stack>
#include <map>
#include <list>
#include <memory>
#include <vector>
#include <iterator>
#include <stdexcept>
#include <algorithm>
#include <atomic>

namespace liteinst {
namespace liteprobes {

using namespace utils::process;
using namespace utils::strings;

using std::string;
using std::stack;
using std::list;
using std::vector;
using std::copy;
using std::map;
using std::invalid_argument;
using std::move;
using std::sort;
using std::unique_ptr;
using std::back_insert_iterator;
using std::memory_order_relaxed; 
using std::atomic;
using utils::Address;

// Inserting a probe
// 1. Find the immediate context for probing 
// 2. Ensure the coordinates have been analyzed
// 3. Generate the probes within the context
// 4. Find the corresponding probe group, create if not existing
// 5. For each probe
//     - Find if it is within a relocated range
//       - Find if it is already instrumented
//         - If so
//           1. See if this probe is already instrumented in the same context
//           2. If not then extend the trampoline
//         - If not
//           1. Try to repun the address with a newly generated super trampoline
//           2. If it fails backtrack until a punnable address is available
//     - If not 
//      1. Try to pun it.
//      2. If not track back until a punnable address is available 

ProbeGroupVec LiteProbeProvider::probe_groups;
ProbeGroupByName LiteProbeProvider::pg_by_name;
ProbeRegistrationVec LiteProbeProvider::probe_registrations;

typedef map<Function, list<ProbeGroup*>> ProbeGroupsByFunction; 

ProbeGroup* LiteProbeProvider::generateProbeGroupForBasicBlock(
    utils::process::Function* fn, utils::process::BasicBlock* bb, 
    Coordinates coord, string probe_group_name) {
  ProbePlacement placement = coord.getBasicBlock().getPlacement();
  map<Address, ProbePlacement> probe_sites;
  switch (placement) {
    case ProbePlacement::ENTRY:
      probe_group_name += "/ENTRY/";
      probe_sites.emplace(bb->start, placement);
      break;
    case ProbePlacement::EXIT:
      probe_group_name += "/EXIT/";
      probe_sites.emplace(bb->end, placement);
      break;
    case ProbePlacement::BOUNDARY:
      probe_group_name += "/BOUNDARY/";
      probe_sites.emplace(bb->start, ProbePlacement::ENTRY);
      probe_sites.emplace(bb->end, ProbePlacement::EXIT);
      break;
    default:
      throw invalid_argument("Unrecognized probe placement..\n");
  }

  ProbeGroup* pg = nullptr;
  auto it = pg_by_name.find(probe_group_name);
  if (it != pg_by_name.end()) {
    pg = it->second;
  } else {
    pg = new ProbeGroup(probe_group_name);
    pg->fn = fn;
    pg->probe_sites = probe_sites;

    meta_data_lock.writeLock();
    // Vector index is the id for efficient access
    pg->pg_id = static_cast<ProbeGroupId>(probe_groups.size()); 
    probe_groups.emplace_back(unique_ptr<ProbeGroup>(pg));
    pg_by_name.emplace(probe_group_name, pg);
    meta_data_lock.writeUnlock();
  }

  return pg;
}

ProbeGroup* LiteProbeProvider::generateProbeGroupForFunction(
    utils::process::Function* fn, 
    Coordinates coord, string probe_group_name) {
  ProbePlacement placement = coord.getFunction().getPlacement();
  map<Address, ProbePlacement> probe_sites;
  switch (placement) {
    case ProbePlacement::ENTRY:
      probe_group_name += "/ENTRY/";
      probe_sites.emplace(fn->start, placement);
      break;
    case ProbePlacement::EXIT:
      probe_group_name += "/EXIT/";
      for (ControlReturn* r : fn->getReturns()) {
        probe_sites.emplace(r->addr, placement);
      }
      break;
    case ProbePlacement::BOUNDARY:
      probe_group_name += "/BOUNDARY/";
      probe_sites.emplace(fn->start, ProbePlacement::ENTRY);
      for (ControlReturn* r : fn->getReturns()) {
        probe_sites.emplace(r->addr, ProbePlacement::EXIT);
      }
      break;
    default:
      throw invalid_argument("Unrecognized probe placement..\n");
  }

  printf("Generating probe groups for : %s\n", probe_group_name.c_str());

  auto it = pg_by_name.find(probe_group_name);
  if (it != pg_by_name.end()) {
    return it->second;
  } else {
    ProbeGroup* pg = new ProbeGroup(probe_group_name);
    pg->fn = fn;
    pg->probe_sites = probe_sites;

    meta_data_lock.writeLock();
    pg->pg_id = static_cast<ProbeGroupId>(probe_groups.size()); 
    probe_groups.emplace_back(unique_ptr<ProbeGroup>(pg));
    // Vector index is the id for efficient access
    pg_by_name.emplace(probe_group_name, pg);
    meta_data_lock.writeUnlock();

    return pg;
  }
}

list<ProbeGroup*> LiteProbeProvider::generateProbeGroups(Coordinates original, 
    Coordinates specific, stack<CoordinateType> block_coords, 
    string probe_group_name = "") {
  CoordinateType type = block_coords.top();
  block_coords.pop();

  utils::process::Process p;
  list<ProbeGroup*> pg_list;
  switch (type) {
    case CoordinateType::FUNCTION:
      if (!original.getFunction().getSpec().compare("*")) {
        vector<utils::process::Function*> fns = p.getFunctions();
        for (utils::process::Function* fn : fns) {
          string name = probe_group_name;
          Coordinates new_specific = specific;
          Function f = original.getFunction();
          f.setSpec(fn->name);
          new_specific.setFunction(f);
          name += "func(" + f.getSpec() + ")";

          if (block_coords.empty()) {
            ProbeGroup* pg = generateProbeGroupForFunction(fn, new_specific,
              name);
            pg_list.push_back(pg);
          } else {
            list<ProbeGroup*> fn_pg_list = generateProbeGroups(original, 
              new_specific, block_coords, name);
            std::copy(fn_pg_list.begin(), fn_pg_list.end(),
              std::back_insert_iterator<std::list<ProbeGroup*>>(pg_list));
          }
        }
      } else {
        utils::process::Function* fn = p.getFunction(original.getFunction().
            getSpec());
        specific.setFunction(original.getFunction());
        probe_group_name += "func(" + original.getFunction().getSpec() + ")";

        if (block_coords.empty()) {
          ProbeGroup* pg = generateProbeGroupForFunction(fn, specific,
            probe_group_name);
          pg_list.push_back(pg);
        } else {
          list<ProbeGroup*> fn_pg_list = generateProbeGroups(original, 
            specific, block_coords, probe_group_name);
          std::copy(fn_pg_list.begin(), fn_pg_list.end(),
            std::back_insert_iterator<std::list<ProbeGroup*>>(pg_list));
        }
      }
      break;
    case CoordinateType::BASIC_BLOCK:
      assert(block_coords.empty());
      if (!original.getBasicBlock().getSpec().compare("*")) {
        utils::process::Function* fn = p.getFunction( // TODO : Get function by name at Process
            specific.getFunction().getSpec());
        vector<utils::process::BasicBlock*> bbs = fn->getBasicBlocks();
        string name = probe_group_name;
        for (utils::process::BasicBlock* bb : bbs) {
          Coordinates new_specific = specific;
          BasicBlock b = original.getBasicBlock();
          b.setSpec(int_to_hex_str(bb->start));
          // BasicBlock b(""); // TODO : Fix this
          new_specific.setBasicBlock(b);
          name += "bb(" + b.getSpec() + ")";

          ProbeGroup* pg = generateProbeGroupForBasicBlock(fn, bb, new_specific,
              name);
          pg_list.push_back(pg);
        } 
      } else {
        utils::process::Function* fn = p.getFunction( // TODO : Get function by name at Process
            specific.getFunction().getSpec());
        utils::process::BasicBlock* bb = fn->getBasicBlock(
           reinterpret_cast<Address>(hex_str_to_int(original.getBasicBlock().
               getSpec())));
        // utils::process::BasicBlock* bb; // TODO: Fix this        
        specific.setBasicBlock(original.getBasicBlock());
        probe_group_name += "bb(" + original.getBasicBlock().getSpec() + ")";

        ProbeGroup* pg = generateProbeGroupForBasicBlock(fn, bb, specific,
            probe_group_name);
        pg_list.push_back(pg);
      }
      break;
    case CoordinateType::LOOP:
      throw invalid_argument("Loop based coordinates not yet implemented..\n");
    default:
      throw invalid_argument("Invalid probe coordinate provided..\n");
  }

  return pg_list;
}

ProbeRegistration LiteProbeProvider::registerProbes(Coordinates coords,
        string instrumentation_provider) {

  stack<CoordinateType> block_coords;
  CoordinateType point_coord;
  bool is_point_coord_set = false;
  if (coords.getInstructionType().getSpec().compare("")) {
    point_coord = CoordinateType::INS_TYPE;
    is_point_coord_set = true;
  }
  
  if (coords.getOffset().getSpec().compare("")) {
    if (is_point_coord_set) {
      throw invalid_argument("Only one point coordinate type can be given..\n");
    }

    point_coord = CoordinateType::OFFSET;
    is_point_coord_set = true;
  } 

  if (coords.getBasicBlock().getSpec().compare("")) {
    block_coords.push(CoordinateType::BASIC_BLOCK);
  } 
  
  if(coords.getLoop().getSpec().compare("")) {
    block_coords.push(CoordinateType::LOOP);
  } 
  
  if (coords.getFunction().getSpec().compare("")) {
    block_coords.push(CoordinateType::FUNCTION);
  }

  Coordinates specific;
  list<ProbeGroup*> pgs = generateProbeGroups(coords, specific, block_coords); 

  printf("After generating probe groups..\n");

  ProbeRegistration* pr = new ProbeRegistration;
  for (ProbeGroup* pg : pgs) {
    auto it = pr->pg_by_function.find(pg->fn->name);
    if (it != pr->pg_by_function.end()) {
      it->second.emplace_back(pg->pg_id, pg->name, pg->start);
    } else {
      vector<ProbeGroupInfo> pgs;
      pgs.emplace_back(pg->pg_id, pg->name, pg->start);
      pr->pg_by_function.emplace(pg->fn->name, pgs);
    }
  }

  meta_data_lock.writeLock();
  pr->reg_id = static_cast<RegistrationId>(probe_registrations.size());
  probe_registrations.emplace_back(unique_ptr<ProbeRegistration>(pr));
  meta_data_lock.writeUnlock();

  for (auto it = pr->pg_by_function.begin(); it != pr->pg_by_function.end(); 
      it++) {
    sort(it->second.begin(), it->second.end(),
        [] (ProbeGroupInfo const& a, ProbeGroupInfo const& b) -> bool {
          return a.start < b.start;
          });
  }

  printf("After generating probe registration..\n");

  LiteProbeInjector lpi;
  InstrumentationProvider instrumentation = getInstrumentationProvider(
      instrumentation_provider);
  // Probe injection for each function
  for (auto it = pr->pg_by_function.begin(); it != pr->pg_by_function.end(); 
      it++) {

    vector<ProbeGroupInfo> pgis = it->second;
    map<Address, ProbeContext> locs;
    for (ProbeGroupInfo pgi : pgis) {

      printf("Injecting probes for %s\n", pgi.name.c_str());
      ProbeGroup* pg = probe_groups[pgi.id].get();

      if (pg->fn->end - pg->fn->start < 5) {
        printf("Skipping small function %s\n", pg->fn->name.c_str());
        goto outer;
      }

      for (auto it : pg->probe_sites) {
        ProbeContext context;
        context.pg_id = pg->pg_id;
        context.i_id = instrumentation.id;
        context.placement = it.second;
        locs.emplace(it.first, context);
      }

      lpi.injectProbes(locs, instrumentation);
    }

outer:
    ;
  }   

  return *pr;
}

bool LiteProbeProvider::activate(ProbeInfo ctx) {
  Address addr = ctx.address;
  Springboard* sb = ControlFlowRouter::getContainingSpringboard(addr);

  Callout* c = sb->getCalloutForProbe(addr);

  assert(c != nullptr);

  Address patch_addr = c->short_circuit.target;
  patch_64(patch_addr, c->short_circuit.on_state);

  /*
  sb->lock.lock();
  if (*reinterpret_cast<uint64_t*>(sb->base) == sb->original) {
    patch_64(sb->base, sb->punned);
  }
  sb->lock.unlock();
  */
}

bool LiteProbeProvider::deactivate(ProbeInfo ctx) {
  Address addr = ctx.address;
  Springboard* sb = ControlFlowRouter::getContainingSpringboard(addr);

  Callout* c = sb->getCalloutForProbe(addr);

  assert(c != nullptr);

  Address patch_addr = c->short_circuit.target;
  patch_64(patch_addr, c->short_circuit.off_state);
}

bool LiteProbeProvider::activate(ProbeGroupInfo pgi) {
  ProbeGroup* pg = probe_groups[pgi.id].get();
  for (auto it : pg->probes) {
    ProbeInfo probe_info;
    probe_info.address = it.second->address;
    activate(probe_info);
  }
}

bool LiteProbeProvider::deactivate(ProbeGroupInfo pgi) {
  ProbeGroup* pg = probe_groups[pgi.id].get();
  for (auto it : pg->probes) {
    ProbeInfo probe_info;
    probe_info.address = it.second->address;
    deactivate(probe_info);
  }
}

bool LiteProbeProvider::activate(ProbeRegistration registration) {
  // To be done later
}

bool LiteProbeProvider::deactivate(ProbeRegistration registraiton) {
  // To be done later
}

} /* End liteprobe */
} /* End liteprobe */

