
#include "finstrumentor.hpp"
#include "dynamicarray.h"

using namespace std;

// func_table functions; // Function address to function id mappings
volatile uint16_t func_id_counter;
// probe_map probe_info;
Instrumentor* INSTRUMENTOR_INSTANCE;
InstrumentationFunc prologFunc;
InstrumentationFunc epilogFunc;
void* probeInfo;

Finstrumentor::Finstrumentor(InstrumentationFunc prolog, InstrumentationFunc epilog) {
  this->prologFunc = prolog; 
  this->epilogFunc = epilog; 
  prologFunc = prolog;
  epilogFunc = epilog;
}

void Finstrumentor::initialize() {
  // Here we are leaking object data to global variables so that we don't incur
  // overhead of object access to get to these data. This should be fine since 
  // Finstrumentor is really a singleton object.
  INSTRUMENTOR_INSTANCE = this;
  // this->probeInfo;

  fprintf(stderr, "Initializing the finstrumentor\n");

  //setting up global data structures
  // this->probeInfo = new DynamicArray<FinsProbeInfo>(DEFAULT_PROBE_COUNT); 
  this->functions = new func_table; 
  this->probe_info = new probe_map; // C++ value intialization. Behaves like calloc  
}

// Here probe_id is actually function id. We only have function level 
// probe toggling granularity at the moment
// TODO : Protect this call with a lock
int Finstrumentor::activateProbe(void* probe_id) {

  std::list<FinsProbeInfo*>* ls = probe_info->find(*((uint64_t*)probe_id))->second;  
  for (std::list<FinsProbeInfo*>::iterator it = ls->begin(); it != ls->end(); it++) {
    FinsProbeInfo* info = *it;

    if (info->isActive) {
      continue;
    }

    __sync_bool_compare_and_swap(info->probeStartAddr, 
        *(info->probeStartAddr), info->deactiveSequence); 

    info->isActive = true;
  }

  return 0;

}

// Here probe_id is actually function id. We only have function level 
// probe toggling granularity at the moment
// TODO : Protect this call with a lock
int Finstrumentor::deactivateProbe(void* probe_id) {

  std::list<FinsProbeInfo*>* ls = probe_info->find(*((uint64_t*)probe_id))->second;  
  for (std::list<FinsProbeInfo*>::iterator it = ls->begin(); it != ls->end(); it++) {
    FinsProbeInfo* info = *it;

    if (!info->isActive) {
      continue;
    }

    __sync_bool_compare_and_swap(info->probeStartAddr, 
        *(info->probeStartAddr), info->activeSequence); 

    info->isActive = false;
  }

  return 0;

}

Finstrumentor::~Finstrumentor() {

}
