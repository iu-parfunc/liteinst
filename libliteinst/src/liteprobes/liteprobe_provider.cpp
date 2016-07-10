
#include "liteprobe_provider.hpp"
// #include "strings.hpp"
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
// using namespace utils::string;

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
  list<Address> probe_addresses;
  switch (placement) {
    case ProbePlacement::ENTRY:
      probe_group_name += "/ENTRY/";
      probe_addresses.push_back(bb->start);
      break;
    case ProbePlacement::EXIT:
      probe_group_name += "/EXIT/";
      probe_addresses.push_back(bb->end);
      break;
    case ProbePlacement::BOUNDARY:
      probe_group_name += "/BOUNDARY/";
      probe_addresses.push_back(bb->start);
      probe_addresses.push_back(bb->end);
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
    pg->probes = probe_addresses;

    meta_data_lock.writeLock();
    probe_groups.emplace_back(unique_ptr<ProbeGroup>(pg));
    // Vector index is the id for efficient access
    pg->pg_id = static_cast<ProbeGroupId>(probe_groups.size()); 
    pg_by_name.emplace(probe_group_name, pg);
    meta_data_lock.writeUnlock();
  }

  return pg;
}

ProbeGroup* LiteProbeProvider::generateProbeGroupForFunction(
    utils::process::Function* fn, 
    Coordinates coord, string probe_group_name) {
  ProbePlacement placement = coord.getFunction().getPlacement();
  list<Address> probe_addresses;
  switch (placement) {
    case ProbePlacement::ENTRY:
      probe_group_name += "/ENTRY/";
      probe_addresses.push_back(fn->start);
      break;
    case ProbePlacement::EXIT:
      probe_group_name += "/EXIT/";
      for (ControlReturn* r : fn->getReturns()) {
        probe_addresses.push_back(r->addr);
      }
      break;
    case ProbePlacement::BOUNDARY:
      probe_group_name += "/BOUNDARY/";
      probe_addresses.push_back(fn->start);
      for (ControlReturn* r : fn->getReturns()) {
        probe_addresses.push_back(r->addr);
      }
      break;
    default:
      throw invalid_argument("Unrecognized probe placement..\n");
  }

  auto it = pg_by_name.find(probe_group_name);
  if (it != pg_by_name.end()) {
    return it->second;
  } else {
    ProbeGroup* pg = new ProbeGroup(probe_group_name);
    pg->fn = fn;

    probe_addresses.sort();
    pg->probes = probe_addresses;
    pg->start = probe_addresses.front();

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
          Coordinates new_specific = specific;
          Function f(fn->name);
          new_specific.setFunction(f);
          probe_group_name += "func(" + f.getSpec() + ")";

          if (block_coords.empty()) {
            ProbeGroup* pg = generateProbeGroupForFunction(fn, new_specific,
              probe_group_name);
            pg_list.push_back(pg);
          } else {
            list<ProbeGroup*> fn_pg_list = generateProbeGroups(original, 
              new_specific, block_coords, probe_group_name);
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
        for (utils::process::BasicBlock* bb : bbs) {
          Coordinates new_specific = specific;
          // BasicBlock b(int_to_hex_str(bb->start));
          BasicBlock b(""); // TODO : Fix this
          new_specific.setBasicBlock(b);
          probe_group_name += "bb(" + b.getSpec() + ")";

          ProbeGroup* pg = generateProbeGroupForBasicBlock(fn, bb, new_specific,
              probe_group_name);
          pg_list.push_back(pg);
        } 
      } else {
        utils::process::Function* fn = p.getFunction( // TODO : Get function by name at Process
            specific.getFunction().getSpec());
        // BasicBlock* bb = fn->getBasicBlock(
        //    hex_str_to_int(original.getBasicBlock().getSpec()));
        utils::process::BasicBlock* bb; // TODO: Fix this        
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
  if (!coords.getInstructionType().getSpec().compare("")) {
    point_coord = CoordinateType::INS_TYPE;
    is_point_coord_set = true;
  }
  
  if (!coords.getOffset().getSpec().compare("")) {
    if (is_point_coord_set) {
      throw invalid_argument("Only one point coordinate type can be given..\n");
    }

    point_coord = CoordinateType::OFFSET;
    is_point_coord_set = true;
  } 

  if (!coords.getBasicBlock().getSpec().compare("")) {
    block_coords.push(CoordinateType::BASIC_BLOCK);
  } 
  
  if(!coords.getLoop().getSpec().compare("")) {
    block_coords.push(CoordinateType::LOOP);
  } 
  
  if (!coords.getFunction().getSpec().compare("")) {
    block_coords.push(CoordinateType::FUNCTION);
  }

  Coordinates specific;
  list<ProbeGroup*> pgs = generateProbeGroups(coords, specific, block_coords); 

  ProbeRegistration* pr = new ProbeRegistration;
  for (ProbeGroup* pg : pgs) {
    auto it = pr->pg_by_function.find(pg->fn->name);
    if (it != pr->pg_by_function.end()) {
      it->second.emplace_back(pg->pg_id, pg->name);
    } else {
      vector<ProbeGroupInfo> pgs;
      pgs.emplace_back(pg->pg_id, pg->name);
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

  // Probe injection for each function
    
}

} /* End liteprobe */
} /* End liteprobe */

