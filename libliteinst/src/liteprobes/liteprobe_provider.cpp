
#include "liteprobe_provider.hpp"
#include "liteprobe_injector.hpp"
#include "control_flow_router.hpp"
#include "strings.hpp"
#include "process.hpp"
#include "range.hpp"
#include "assembly.hpp"
#include "patcher.h"
#include "cycle.h"
#include <string>
#include <regex>
#include <stack>
#include <map>
#include <list>
#include <memory>
#include <vector>
#include <iterator>
#include <stdexcept>
#include <set>
#include <algorithm>
#include <atomic>
#include <iostream>

#ifdef AUDIT
#include "audit.hpp"
#endif

namespace liteinst {
namespace liteprobes {

using namespace utils::process;
using namespace utils::strings;
using namespace utils::assembly;
using namespace utils::range;

using std::string;
using std::stack;
using std::regex;
using std::smatch;
using std::regex_search;
using std::list;
using std::vector;
using std::set;
using std::remove;
using std::copy;
using std::inserter;
using std::map;
using std::invalid_argument;
using std::move;
using std::sort;
using std::set_difference;
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

set<utils::process::Function*> getMatchingFunctions(
    vector<utils::process::Function*> fns, string pattern) {
  regex include("[^~].+");
  regex exclude("[~].+");
  regex both("[^~].+[~].+");

  vector<string> include_patterns;
  vector<string> exclude_patterns;

  if (pattern.find("*") == 0) {
    pattern = "." + pattern; // Make it regex friendly
  }

  // printf("PATTERN : %s\n", pattern.c_str());
  smatch match;
  if (regex_search(pattern, match, both,
        std::regex_constants::match_continuous)) {
    printf("Pattern : %s\n", pattern.c_str());
    if (match.size() > 0) {
      vector<string> tokens;
      tokenize(pattern, tokens, "~");

      tokenize(tokens[0], include_patterns, "/");
      tokenize(tokens[1], exclude_patterns, "/");
    }
  } else if (regex_search(pattern, match, include,
        std::regex_constants::match_continuous)) {
    if (match.size() > 0) {
      tokenize(pattern, include_patterns, "/");
    }
  } else if (regex_search(pattern, match, exclude,
        std::regex_constants::match_continuous)) {
    if (match.size() > 0) {
      tokenize(pattern, exclude_patterns, "/");
    }
  }

  set<utils::process::Function*> all;
  set<utils::process::Function*> included;
  set<utils::process::Function*> excluded;

  for (utils::process::Function* fn : fns) {
    all.insert(fn);
    for (string& include_pattern : include_patterns) {
      if (!include_pattern.compare("*")) {
        include_pattern = ".*"; // Make it regex friendly
      }
      regex include(include_pattern);
      if(regex_match(fn->name, include, 
            std::regex_constants::match_continuous)) {
        included.insert(fn);
      }
    }

    for (string& exclude_pattern : exclude_patterns) {
      if (!exclude_pattern.compare("*")) {
        exclude_pattern = ".*"; // Make it regex friendly
      }
      regex exclude(exclude_pattern);
      /*
      printf("Matching %s with pattern %s\n", fn->name.c_str(), 
          exclude_pattern.c_str());
          */
      if(regex_match(fn->name, exclude, 
            std::regex_constants::match_continuous)) {
        excluded.insert(fn);
      }
    }
  }

  /*
  printf("INCLUDED : \n");
  for (utils::process::Function* fn : included) {
    printf("%s\n", fn->name.c_str());
  }
  */

  /*
  printf("EXCLUDED : \n");
  for (utils::process::Function* fn : excluded) {
    printf("%s\n", fn->name.c_str());
  }
  
  */
  

  set<utils::process::Function*> result;
  if (included.size() > 0 && excluded.size() > 0) {
    set_difference(included.begin(), included.end(), excluded.begin(), 
        excluded.end(), inserter(result, result.begin()));
  } else if (excluded.size() > 0) {
    set_difference(all.begin(), all.end(), excluded.begin(),
        excluded.end(), inserter(result, result.begin()));
  } else {
    return included;
  }

  return result;
}

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

  // printf("Generating probe groups for : %s\n", probe_group_name.c_str());

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
    case CoordinateType::FUNCTION: {
        vector<utils::process::Function*> fns = p.getFunctions();
        string spec = original.getFunction().getSpec();

        set<utils::process::Function*> filtered = getMatchingFunctions(fns, 
            spec); 

        // printf("FILTERED SIZE : %lu\n", filtered.size());

        for (utils::process::Function* fn : filtered) {

          if (fn->start == 0x0 || fn->end == 0x0) {
            continue;
          }

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

  // printf("After generating probe groups..\n");

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

  FILE* fp = fopen("probes.out", "w");

  if (fp == nullptr) {
    assert(false);
  } 

  // printf("After generating probe registration..\n\n");

  LiteProbeInjector lpi;
  InstrumentationProvider instrumentation = getInstrumentationProvider(
      instrumentation_provider);

  ticks init_start = getticks();
  list<string> failed_funcs;
  // Probe injection for each function
  ticks total = 0; // Total probe injection cost
  ticks punning_costs = 0;
  ticks injection_costs = 0;
  ticks start = 0;
  ticks end = 0;
  InjectionResult ir;
  int64_t num_probes = 0;
  int64_t failed_probes = 0;
  int64_t skipped_probes = 0;
  int64_t num_funcs = pr->pg_by_function.size();
  int64_t skipped_funcs = 0;
  int probed = num_funcs;
  for (auto it = pr->pg_by_function.begin(); it != pr->pg_by_function.end(); 
      it++) {

    ticks fn_punning_cost = 0;
    ticks fn_injection_cost = 0; 
    ticks fn_probing_cost = 0;
    vector<ProbeGroupInfo>& pgis = it->second;
    map<Address, ProbeContext> locs;
    vector<ProbeGroupInfo> failed;
    for (ProbeGroupInfo pgi : pgis) {

      // printf("Injecting probes for %s\n", pgi.name.c_str());

      ProbeGroup* pg = probe_groups[pgi.id].get();

      if (pg->fn->end - pg->fn->start < 5) {
        printf("Skipping small function %s\n", pg->fn->name.c_str());
        skipped_funcs++;
        skipped_probes += pg->probe_sites.size();
        failed.push_back(pgi);
        continue;
        // goto outer;
      }

      for (auto it : pg->probe_sites) {
        fprintf(fp, "%04x,", it.first); 
        ProbeContext context;
        context.pg_id = pg->pg_id;
        context.i_id = instrumentation.id;
        context.placement = it.second;
        locs.emplace(it.first, context);
      }

      start = getticks();
      ir = lpi.injectProbes(locs, instrumentation);
      end = getticks();

      fn_probing_cost += (end - start);
      fn_injection_cost += ir.injection_costs;
      fn_punning_cost += ir.punning_costs;

      bool success = ir.success;

      if (success) {
        num_probes += locs.size();

        for (auto& it : locs) {
          Probe* probe = lpi.getProbe(it.first);
          pg->probes.emplace(it.first, probe);
        } 
      } else {
        // printf("Failed instrumenting probe group at function %s\n", pg->fn->name.c_str());
        failed.push_back(pgi);
        failed_probes += locs.size();
      }

      locs.clear();
    }

outer:
    int before_size = pgis.size();
    for (ProbeGroupInfo f : failed) {
      pgis.erase(remove(pgis.begin(), pgis.end(), f), pgis.end());
    }
    int after_size = pgis.size();
    assert(failed.size() == (before_size - after_size));

    if (after_size == 0) {
      probed--;
      failed_funcs.push_back(it->first);
    } else if (failed.size() == 0) {
      total += fn_probing_cost;
      punning_costs += fn_punning_cost;
      injection_costs += fn_injection_cost;
    }
  }   

  ticks init_end = getticks();

  printf("INIT: %ld\n", (init_end - init_start));

  fclose(fp);

  for (string failed : failed_funcs) {
    pr->pg_by_function.erase(failed);
  }

  int64_t num_funcs_probed = pr->pg_by_function.size();

  assert(probed == num_funcs_probed);

  printf("FUNCS: %ld\n", num_funcs_probed);
  printf("SKIPPED_FUNCS: %ld\n", skipped_funcs);
  printf("FAILED_FUNCS: %ld\n", num_funcs - num_funcs_probed - 
      skipped_funcs);
  printf("PROBSITES: %ld\n", num_probes);
  printf("FAILED_PROBESITES: %ld\n", failed_probes);
  printf("SKIPPED_PROBESITES: %ld\n\n", skipped_probes);

  printf("After injecting probes..\n");

  if (failed_probes > 0) {
    pr->failures = true;
  } else {
    pr->failures = false;
  }

  pr->num_probed_pgs = num_funcs_probed;
  pr->num_skipped_pgs = skipped_funcs; 
  pr->num_failed_pgs = num_funcs - num_funcs_probed - skipped_funcs;
  pr->discoverd_pgs = num_funcs;
  pr->probing_costs = total;
  pr->meta_data_costs = total - punning_costs - injection_costs;
  pr->injection_costs = injection_costs;
  pr->punning_costs = punning_costs;

  return *pr;
}

bool LiteProbeProvider::activate(ProbeInfo ctx) {

#ifdef AUDIT
  ticks start = getticks();
  g_activation_count++;
#endif

  Address addr = ctx.address;
  Springboard* sb = ControlFlowRouter::getContainingSpringboard(addr);

  sb->lock.lock();

  if (sb->n_probes > 1) {
    Callout* c = sb->getCalloutForProbe(addr);

    assert(c != nullptr);

    Address patch_addr = c->short_circuit.start;
    
    if (*reinterpret_cast<uint64_t*>(patch_addr) == c->short_circuit.on_state) {
      sb->lock.unlock();
      return false;
    } else {
      patch_64(patch_addr, c->short_circuit.on_state);
    }
  } else {
    if (sb->active_probes == 1) {
      sb->lock.unlock();
      return false;
    }
  }


  if (sb->active_probes == 0) {
    Address probe_end  = sb->base + sb->probe_length;

    Range r(sb->base - 8, sb->base + 24);
    // LiteProbeInjector::range_map.lockRange(r);
    
    patch_64_plus(sb->base, sb->punned | (*(reinterpret_cast<uint64_t*>(sb->base))
          & 0xFFFFFF0000000000));
    // LiteProbeInjector::range_map.unlockRange(r);

    for (const auto& it : sb->saved_probe_heads) {
      if (it.first >= probe_end) {
        *it.first = 0x62; // Single byte write. Should be fine.
        assert(it.first < sb->displaced.end);
      }
    }
  }   
  
  sb->active_probes++;
  sb->lock.unlock();

#ifdef AUDIT
  ticks end = getticks();
  g_activation_cost += (end - start);
#endif

  return true;
}

bool LiteProbeProvider::deactivate(ProbeInfo ctx) {
#ifdef AUDIT
  ticks start = getticks();
  g_deactivation_count++;
#endif
  Address addr = ctx.address;
  Springboard* sb = ControlFlowRouter::getContainingSpringboard(addr);

  sb->lock.lock();

  if (sb->active_probes == 0) {
    sb->lock.unlock();
    return false;
  }

  if (sb->n_probes > 1) {
    Callout* c = sb->getCalloutForProbe(addr);

    assert(c != nullptr);

    Address patch_addr = c->short_circuit.start;
    patch_64(patch_addr, c->short_circuit.off_state);
  }

  if (sb->active_probes == 1) {
    Address probe_end  = sb->base + sb->probe_length;

    Range r(sb->base - 8, sb->base + 24);
    // LiteProbeInjector::range_map.lockRange(r);

    patch_64_plus(sb->base, sb->original | (*(reinterpret_cast<uint64_t*>(sb->base))
          & 0xFFFFFF0000000000));

    // LiteProbeInjector::range_map.unlockRange(r);

    for (const auto& it : sb->saved_probe_heads) {
      if (it.first >= probe_end) {
        *it.first = it.second; // Single byte write. Should be fine.
        assert(it.first < sb->displaced.end);
      }
    }
  }  
  
  sb->active_probes--;
  sb->lock.unlock();

#ifdef AUDIT
  ticks end = getticks();
  g_deactivation_cost += (end - start);
#endif
  return true;
}

bool LiteProbeProvider::activate(ProbeGroupInfo pgi) { 
  ProbeGroup* pg = probe_groups[pgi.id].get();
  bool result = false;

  if (pg != nullptr) {
    for (auto it : pg->probes) {
      ProbeInfo probe_info;
      probe_info.address = it.second->address;
      result |= activate(probe_info);
    }
  }

  return result;
}

bool LiteProbeProvider::deactivate(ProbeGroupInfo pgi) {
  ProbeGroup* pg = probe_groups[pgi.id].get();
  bool result = false;
  for (auto it : pg->probes) {
    ProbeInfo probe_info;
    probe_info.address = it.second->address;
    result |= deactivate(probe_info);
  }

  return result;
}

bool LiteProbeProvider::activate(ProbeRegistration registration) {
  // To be done later
}

bool LiteProbeProvider::deactivate(ProbeRegistration registraiton) {
  // To be done later
}

} /* End liteprobe */
} /* End liteprobe */

