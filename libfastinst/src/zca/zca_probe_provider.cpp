
#include "zca_probe_provider.hpp"
#include "zca_types.hpp"
#include "stub_utils.hpp"
// #include "calibrate.hpp"
#include "patcher.h"
#include "wait_free.h"

#include <cstdio>
#include <map>
#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>

using namespace std;
using namespace lock;
using namespace utils;
// using namespace calibrate;
using namespace stubutils;

#ifdef AUDIT_PROBES 
static __thread ToggleStatistics* stats = NULL;
#endif


void ZCAProbeProvider::initializeProvider(InstrumentationFunc prolog, 
    InstrumentationFunc epilog) {

  // This initializes the probe_meta_data structure with information read from
  // binary
  setupStubs(probe_meta_data, prolog, epilog); 
#ifdef AUDIT_PROBES
  toggle_stats = new ToggleStatistics*[64](); // Number of threads fixed to 64.
#endif

#ifdef AUDIT_INIT_COST
  init_costs = new ticks*[64]();
#endif

  thread_counter = 0;
  func_count = 0;
  // calibrateInstrumentationOverhead();
  fprintf(stderr, "[ZCA Probe Provier] Done initializing the probe provider..\n");
}

void ZCAProbeProvider::calibrateInstrumentationOverhead() {
  Callback temp = this->callback; // Backup the current callback function
  // this->callback = calibrationCallback; // Sets a temporary callback to
  // initialize the special calibration function probes

  // this->instrumentationOverheadEstimate = getInstrumentationOverheadPerProbe();

  this->callback = temp; // Restore the original callback

}

uint64_t ZCAProbeProvider::getNumberOfFunctions() {
  if (func_count == 0) {
    map<string, int> func_map;
    for (auto it = probe_meta_data->begin(); it != probe_meta_data->end(); 
        ++it) {
      ZCAProbeMetaData* pmd = (ZCAProbeMetaData*) *it;
      func_map.insert(pair<string, int>(pmd->func_name, 1));
    }
    func_count = func_map.size();
  }

  return func_count;
}

void ZCAProbeProvider::initialize(ProbeId probe_id, ProbeArg arg) {
  ZCAProbeMetaData* pmd = (ZCAProbeMetaData*)(*probe_meta_data)[probe_id];
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

  int64_t relative = (int64_t)(pmd->stub_address - pmd->probe_addr - 5); 
  *(int32_t*)(active_seq_ptr+1) = (int32_t)relative;
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


  ZCAProbeMetaData* pmd =  (ZCAProbeMetaData*)(*probe_meta_data)[probe_id];

  if (pmd->state == ProbeState::UNINITIALIZED) {
    throw -1;
  }

  if (func != pmd->instrumentation_func.load()) {
    pmd->instrumentation_func.store(func,
      std::memory_order_seq_cst);
    Address stub_call_addr = pmd->stub_address + pmd->call_addr_offset; 

    /*
    uint64_t old_val = (uint64_t) *stub_call_addr;
    int64_t relative = (int64_t) ((Address)func - (uint64_t) stub_call_addr 
        - 5);

    uint64_t mask = 0x0FFFFFFFFFFFFFFF;
    int shift_size = (sizeof(uint64_t) - pmd->tramp_call_size) * 8 - 4;
    mask = (mask >> shift_size);
    uint64_t msb = (old_val & ~mask);

    uint64_t new_val;
    uint8_t* new_val_ptr = (uint8_t*) & new_val;
    new_val_ptr[0] = 0xE9;

    *(uint32_t*)(new_val_ptr+1) = (int32_t)relative;
    new_val = new_val | msb;
    */

    bool b;
#if defined(INVOKE_PATCH_SYNC)
    b = patch_64((void*) stub_call_addr, (uint64_t)func);
#elif defined(INVOKE_PATCH_CALL)
    b = patch_call_64((void*) stub_call_addr, (uint64_t) func); 
#else
    b = patch_call_64((void*) stub_call_addr, (uint64_t) func); // The default is callpatch
#endif
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

  ZCAProbeMetaData* pmd = (ZCAProbeMetaData*)(*probe_meta_data)[probe_id];

  if (pmd->state == ProbeState::UNINITIALIZED) {
    throw -1;
  }

  if (func != pmd->instrumentation_func.load()) {
    pmd->instrumentation_func.store(func,
      std::memory_order_seq_cst);
    Address stub_call_addr = pmd->stub_address + pmd->call_addr_offset; 

    /*
    uint64_t old_val = (uint64_t) *stub_call_addr;
    int64_t relative = (int64_t) ((Address)func - (uint64_t) stub_call_addr 
        - 5);

    uint64_t mask = 0x0FFFFFFFFFFFFFFF;
    int shift_size = (sizeof(uint64_t) - pmd->tramp_call_size) * 8 - 4;
    mask = (mask >> shift_size);
    uint64_t msb = (old_val & ~mask);

    uint64_t new_val;
    uint8_t* new_val_ptr = (uint8_t*) & new_val;

    *(uint32_t*)(new_val_ptr) = (int32_t)relative;
    new_val = new_val | msb;
    */

    bool b;
    b = async_patch_64((void*) stub_call_addr, (uint64_t)func);

    if (!b) {
      throw -1;
    }
  } 

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

void ZCAProbeProvider::registerProbe(ProbeMetaData* pmd) {
  (*callback)(pmd);
}

ProbeMetaData* ZCAProbeProvider::getProbeMetaData(ProbeId probe_id) {
  return (*probe_meta_data)[probe_id];
}
