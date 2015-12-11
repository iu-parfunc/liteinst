
#include "zca_probe_provider.hpp"
#include "zca-types.hpp"
// #include "calibrate.hpp"
#include "patcher.h"
#include "wait_free.h"

#include <cstdio>
#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>

using namespace std;
using namespace lock;
using namespace utils;
using namespace calibrate;

#ifdef AUDIT_PROBES 
static __thread ToggleStatistics* stats = NULL;
#endif


void ZCAProbeProvider::initializeProvider() {
  probe_meta_data = StubUtils::setupStubs();
  toggle_stats = new ToggleStatistics*[64](); // Number of threads fixed to 64.

#ifdef AUDIT_INIT_COST
  init_costs = new ticks*[64]();
#endif

  thread_counter = 0;
  // calibrateInstrumentationOverhead();
}

void ZCAProbeProvider::calibrateInstrumentationOverhead() {
  Callback temp = this->callback; // Backup the current callback function
  this->callback = calibrationCallback; // Sets a temporary callback to
  // initialize the special calibration function probes

  this->instrumentationOverheadEstimate = getInstrumentationOverheadPerProbe();

  this->callback = temp; // Restore the original callback

}

uint64_t ZCAProbeProvider::getNumberOfFunctions() {
  return func_addr_mappings.size();
}

void ZCAProbeProvider::initialize(ProbeId probe_id, ProbeArg arg) {
  ProbeMetaData* pmd = (*probe_meta_data)[probe_id];
  pmd->probe_arg = arg;

  uint64_t* probe_address = (uint64_t*) pmd->probe_addr;
  uint64_t* stub_address = (uint64_t*) pmd->stub_address;

  uint64_t old_val = *probe_address;
  pmd->inactive_seq = old_val;
  uint64_t mask = 0x0FFFFFFFFFFFFFFF;

  int shift_size = (sizeof(uint64_t) - PROBE_SIZE) * 8 - 4;
  mask = (mask >> shift_size);

  uint64_t msb = (old_val & ~mask);

  uint64_t active_seq = 0;
  uint8_t* active_seq_ptr = (uint8_t*) &active_seq;
  active_seq_ptr[0] = 0xE9;

  long relative = (long)(pmd->stub_address - pmd->probe_addr - 5); 
  *(uint32_t*)(active_seq_ptr+1) = (uint32_t)relative;
  active_seq_ptr[5] = 0x90;

  active_seq = active_seq | msb;
  pmd->active_seq = active_seq;
  pmd->state = ProbeState::INITIALIZING;

  // Probably check if we are 64 bit mode before this
  pmd->is_straddler = is_straddler_64((void*)pmd->probe_addr);
  pmd->straddle_point = straddle_point_64((void*)pmd->probe_addr);

  init_patch_site((void*) pmd->probe_addr, 8);

}

bool ZCAProbeProvider::activate(const ProbeId probe_id,
    InstrumentationFunc func) {

#ifdef AUDIT_PROBES
  if (stats == NULL) { 
    stats = new ToggleStatistics; 
    stats->activation_count = 1;  
    stats->deactivation_count = 0;
    stats->activation_costs = 0;
    stats->deactivation_costs = 0;

    toggle_stats[thread_counter++] = stats; // Assumes atomic update of int var
  } else {
    stats->activation_count++;
  }

  ticks start = getticks();
#endif 


  ProbeMetaData* pmd =  (*probe_meta_data)[probe_id];

  if (pmd->state == ProbeState::UNINITIALIZED) {
    throw -1;
  }

  if (func.load() != pmd->instrumetnation_func.load()) {
    pmd->instrumentation_func.store(func,
      std::memory_order_seq_cst);
    Address stub_call_addr = pmd->stub_address + pmd->probe_offset; 

    int CALL_INSTRUCTION_SIZE = 5; // Get this using distorm
    uint64_t old_val = (uint64_t) *stub_call_addr;
    long relative = (Address)func.load() - (uint64_t) stub_call_addr - 5;

    uint64_t mask = 0x0FFFFFFFFFFFFFFF;
    int shift_size = (sizeof(uint64_t) - CALL_INSTRUCTION_SIZE) * 8 - 4;
    mask = (mask >> shift_size);
    uint64_t msb = (old_val & ~mask);

    uint64_t new_val;
    uint8_t* new_val_ptr = (uint8_t*) & new_val;
    new_val_ptr[0] = 0xE9;

    *(uint32_t*)(new_val_ptr+1) = (int32_t)relative;
    new_val = new_val | msb;
  } 

  /*
  if (pmd->state == ProbeState::ACTIVE) {
    return true;
  } */

  // Patch the probe site
  bool b;
#if defined(INVOKE_PATCH_SYNC)
    b = patch_64((void*) pmd->probe_addr, pmd->active_seq);
#elif defined(INVOKE_PATCH_CALL)
    b = patch_call_64((void*) pmd->probe_addr, pmd->active_seq); 
#else
    b = patch_call_64((void*) pmd->probe_addr, pmd->active_seq); // The default is callpatch
#endif

  pmd->state = ProbeState::ACTIVE;


#ifdef AUDIT_PROBES
  ticks end = getticks();
  stats->activation_costs = (end - start);
#endif
  
  return b;
}

bool ZCAProbeProvider::activate_async(ProbeId probe_id, InstrumentationFunc func) {

#ifdef AUDIT_PROBES
  if (stats == NULL) { 
    stats = new ToggleStatistics; 
    stats->activation_count = 1;  
    stats->deactivation_count = 0;
    stats->activation_costs = 0;
    stats->deactivation_costs = 0;

    toggle_stats[thread_counter++] = stats; // Assumes atomic update of int var
  } else {
    stats->activation_count++;
  }

  ticks start = getticks();
#endif 

  ProbeMetaData* pmd =  (*probe_meta_data)[probe_id];

  if (pmd->state == ProbeState::UNINITIALIZED) {
    throw -1;
  }

  if (pmd->state == ProbeState::ACTIVE) {
    return false;
  }

  (*probe_meta_data)[probe_id]->instrumentation_func.store(func,
      std::memory_order_seq_cst);

  // Patch the probe site
  bool b = async_patch_64((void*) pmd->probe_addr, pmd->active_seq);

  pmd->state = ProbeState::ACTIVE;

#ifdef AUDIT_PROBES
  ticks end = getticks();
  stats->activation_costs = (end - start);
#endif

  // printf("[Finstrument Probe Provider] Asynchronously activated probe site..\n");
  return b;
}

void ZCAProbeProvider::activate_async_finish(ProbeId probe_id) {
  ProbeMetaData* pmd =  (*probe_meta_data)[probe_id];
  // finish_patch_64((void*) pmd->probe_addr);
}

bool ZCAProbeProvider::deactivate(const ProbeId probe_id) {

#ifdef AUDIT_PROBES
  if (stats == NULL) { 
    stats = new ToggleStatistics; 
    stats->deactivation_count = 1;  
    stats->activation_count = 0;
    stats->activation_costs = 0;
    stats->deactivation_costs = 0;

    toggle_stats[thread_counter++] = stats; // Assumes atomic update of int var
  } else {
    stats->deactivation_count++;
  }

  ticks start = getticks();
#endif 


  ProbeMetaData* pmd =  (*probe_meta_data)[probe_id];

  if (pmd->state == ProbeState::UNINITIALIZED) {
    throw -1;
  }
  if (pmd->state == ProbeState::DEACTIVATED ) {
    return false;
  }

  // Patch the probe site
  bool b;
#if defined(INVOKE_PATCH_SYNC)
    b = patch_64((void*) pmd->probe_addr, pmd->inactive_seq);
#elif defined(INVOKE_PATCH_CALL)
    b = patch_call_64((void*) pmd->probe_addr, pmd->inactive_seq); 
#else
    b = patch_call_64((void*) pmd->probe_addr, pmd->inactive_seq); // The default is callpatch
#endif

  pmd->state = ProbeState::DEACTIVATED;

#ifdef AUDIT_PROBES
  ticks end = getticks();
  stats->deactivation_costs += (end - start);
#endif

  return b;
}

bool ZCAProbeProvider::deactivate_async(const ProbeId probe_id) {

#ifdef AUDIT_PROBES
  if (stats == NULL) { 
    stats = new ToggleStatistics; 
    stats->deactivation_count = 1;  
    stats->activation_count = 0;
    stats->activation_costs = 0;
    stats->deactivation_costs = 0;

    toggle_stats[thread_counter++] = stats; // Assumes atomic update of int var
  } else {
    stats->deactivation_count++;
  }

  ticks start = getticks();
#endif 

  ProbeMetaData* pmd =  (*probe_meta_data)[probe_id];

  if (pmd->state == ProbeState::UNINITIALIZED) {
    throw -1;
  }
  if (pmd->state == ProbeState::DEACTIVATED ) {
    return false;
  }

  // Patch the probe site
  bool b = async_patch_64((void*) pmd->probe_addr, pmd->inactive_seq);

  pmd->state = ProbeState::DEACTIVATING;

#ifdef AUDIT_PROBES
  ticks end = getticks();
  stats->deactivation_costs += (end - start);
#endif

  // printf("[Finstrument Probe Provider] Asynchronously deactivated probe site..\n");
  return b;
}

void ZCAProbeProvider::deactivate_async_finish(ProbeId probe_id) {
  ProbeMetaData* pmd =  (*probe_meta_data)[probe_id];
  // finish_patch_64((void*) pmd->probe_addr);
}

void ZCAProbeProvider::setupStubs() {

  fprintf(stderr, "[ZCA Probe Provider] Setting up stubs..\n");

  string line;
  ifstream fp ("functions.txt");
  std::string::size_type sz = 16;

  if (fp.fail()) {
    fprintf(stderr, "[Finstrument Probe Provider] ERROR : Failed opening "
        "functions.txt. Exiting program\n");
    throw -1;
  }

  if (fp.is_open()) {
    while (getline (fp,line)){
      string token;
      istringstream iss(line);

      vector<string> tokens;
      int num_tokens = tokenize(line, tokens, ",");
      if (num_tokens < 2) {
        continue;
      }

      Address func_addr = (Address) strtoul(tokens[0].c_str(), NULL, 16);
      string func_name = tokens[1];

      func_addr_mappings.insert(make_pair(func_addr, func_name));
      // func_rw_locks.insert(make_pair(func_addr, new CASLock));
    }

    // For now explicitly add calibrationFunction contributed by our library to
    // the mappings. In future we should read ELF entries for all the loaded
    // libraries for the application and include that in the mapping.
    func_addr_mappings.insert(make_pair((Address) calibrationFunction,
          string("calibrationFunction")));
    fp.close();
  }

  /*
  if (func_count == 1) {
    fprintf(stderr, "[Ubiprof] ERROR : functions.txt not present. Ubiprof will"
                    " not profile this application...\n");
  }*/
}

string ZCAProbeProvider::getFunctionName(Address func_addr) {
  auto val = func_addr_mappings.find(func_addr);
  if (val == func_addr_mappings.end()) {
    throw -1;
  } else {
    return val->second;
  }
}

ProbeMetaData* FinstrumentProbeProvider::getNewProbeMetaDataContainer(
    Address probe_addr) {
  // Get exclusive access to probe meta data vector before updating.
  // This is required since we need to get a unique probe id which directly
  // maps to the index of ProbeMetaData element for efficent future access.
  // [WARNING] This is a potential scalability bottleneck since all threads
  // will be locking on this lock to gain access to probe meta data during
  // probe initialization phase.
  probe_lock.lock();
  if (probe_lookup.find(probe_addr) != probe_lookup.end()) {
    probe_lock.unlock();
    return NULL;
  } else {
    ProbeMetaData* pmd = new ProbeMetaData;
    probe_meta_data->push_back(pmd);
    pmd->probe_id = probe_meta_data->size()-1;
    probe_lookup.insert(make_pair(probe_addr, 1)); // Value we put is
                                                   // inconsequentail
    probe_lock.unlock();
    return pmd;
  }

  return NULL;

}

void ZCAProbeProvider::registerProbe(ProbeMetaData* pmd) {
  (*callback)(pmd);
}

ProbeMetaData* ZCAProbeProvider::getProbeMetaData(ProbeId probe_id) {
  return (*probe_meta_data)[probe_id];
}
