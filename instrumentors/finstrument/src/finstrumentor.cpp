
#include "cstdio"
#include "finstrumentor.hpp"
#include "patch_utils.hpp"
#include "dynamicarray.h"

using namespace std;

// func_table functions; // Function address to function id mappings
volatile uint16_t func_id_counter;
// probe_map probe_info;
Instrumentor* INSTRUMENTOR_INSTANCE = 0;
InstrumentationFunc prologFunction;
InstrumentationFunc epilogFunction;
void* probeInfo;

Finstrumentor::Finstrumentor(InstrumentationFunc prolog, InstrumentationFunc epilog) {
  this->prologFunc = prolog; 
  this->epilogFunc = epilog; 
  prologFunction = prolog;
  epilogFunction = epilog;
}

void Finstrumentor::initialize() {
  // Here we are leaking object data to global variables so that we don't incur
  // overhead of object access to get to these data. This should be fine since 
  // Finstrumentor is really a singleton object.
  INSTRUMENTOR_INSTANCE = this;
  // this->probeInfo;

  fprintf(stderr, "[Finstrumentor] Initializing the instrumentor\n");

  //setting up global data structures
  // this->probeInfo = new DynamicArray<FinsProbeInfo>(DEFAULT_PROBE_COUNT); 
  this->functions = new func_table; 
  this->function_ids = new func_id_table;
  this->probe_info = new probe_map; 
}


int Finstrumentor::activateProbeByName(void* probe_id, int flag) {

  uint64_t func_addr = (uint64_t)probe_id;
  uint64_t func_id =  functions->find(func_addr)->second;

  return activateProbe(&func_id, flag);

}

// Here probe_id is actually function id. We only have function level 
// probe toggling granularity at the moment
// TODO : Protect this call with a lock
int Finstrumentor::activateProbe(void* probe_id, int flag) {

  uint16_t func_id = *(uint16_t*)probe_id;
  uint64_t func_addr =  function_ids->find(func_id)->second;

  std::list<FinsProbeInfo*>* ls = probe_info->find(func_addr)->second;  
  for (std::list<FinsProbeInfo*>::iterator it = ls->begin(); it != ls->end(); it++) {
    FinsProbeInfo* info = *it;

    if (info->isActive) {
      continue;
    }

    bool status = modify_page_permissions(info->probeStartAddr);
    if (!status) {
      LOG_ERROR("Patching the probesite failed at %p. Skipping..\n", info->probeStartAddr);
      return -1;
    }

    // fprintf(stderr, "Activating with sequence : %p\n", info->activeSequence);
    uint64_t fetch = *((uint64_t*) info->probeStartAddr);
    uint64_t res = __sync_val_compare_and_swap((uint64_t*)info->probeStartAddr, 
        *((uint64_t*)info->probeStartAddr), info->activeSequence); 
    // fprintf(stderr, "Result : %p\n", res);

    // __sync_bool_compare_and_swap(info->probeStartAddr, 
    //     *(info->probeStartAddr), info->activeSequence); 

    info->isActive = true;
  }

  return 0;

}

int Finstrumentor::deactivateProbeByName(void* probe_id, int flag) {

  uint64_t func_addr = (uint64_t)probe_id;
  uint64_t func_id =  functions->find(func_addr)->second;

  return deactivateProbe(&func_id, flag);

}

// Here probe_id is actually function id. We only have function level 
// probe toggling granularity at the moment
// TODO : Protect this call with a lock
int Finstrumentor::deactivateProbe(void* probe_id, int flag) {

  uint16_t func_id = *(uint16_t*)probe_id;
  uint64_t func_addr =  function_ids->find(func_id)->second;
  // fprintf(stderr, "Deactivating the probes for function %d..\n", func_id);
  // fprintf(stderr, "probe_info address is : %p\n", probe_info);

  std::list<FinsProbeInfo*>* ls = probe_info->find(func_addr)->second;  

  // fprintf(stderr, "List address for func id %d : %p\n", func_id, ls);
  int count = 0;
  for (std::list<FinsProbeInfo*>::iterator it = ls->begin(); it != ls->end(); it++) {
    FinsProbeInfo* info = *it;

    if (!info->isActive) {
      continue;
    }

    bool status = modify_page_permissions(info->probeStartAddr);
    if (!status) {
      LOG_ERROR("Patching the probesite failed at %p. Skipping..\n", info->probeStartAddr);
      return -1;
    }

    // fprintf(stderr, "Probe start address %04x\n", (info->probeStartAddr));

    /*
    uint64_t sequence = *((uint64_t*)info->probeStartAddr);
    uint64_t mask = 0x0000000000FFFFFF;
    uint64_t deactiveSequence = (uint64_t) (sequence & mask); 
    mask = 0x9090909090000000;
    deactiveSequence = deactiveSequence | mask;
    */

    // fprintf(stderr, "Active sequence %08x\n", info->activeSequence);
    // fprintf(stderr, "Deactive sequence %08x\n", info->deactiveSequence);

    // fprintf(stderr, "Deactivating with sequence : %p\n", info->deactiveSequence);
    uint64_t fetch = *((uint64_t*) info->probeStartAddr);
    uint64_t res = __sync_val_compare_and_swap((uint64_t*)info->probeStartAddr, 
        *((uint64_t*)info->probeStartAddr), info->deactiveSequence); 

    // fprintf(stderr, "Result : %p\n", res);

    info->isActive = false;
    count++;
  }

  // fprintf(stderr, "Deactivated probe count: %d\n", count);

  return 0;

}

functionInfo* Finstrumentor::readFunctionInfo() {
  return NULL;
}

Finstrumentor::~Finstrumentor() {
  fprintf(stderr, "NUM_PROBES : %lu\n", ((Finstrumentor*) INSTRUMENTOR_INSTANCE)->probe_info->size()); 
  delete this->probe_info;
  delete this->functions;
}
