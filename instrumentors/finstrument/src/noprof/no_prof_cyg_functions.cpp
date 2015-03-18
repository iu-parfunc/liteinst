
#include <inttypes.h>
#include <stdlib.h>

#include "../cyg_functions.hpp"
#include "../finstrumentor.hpp"

__attribute__((constructor, no_instrument_function))
void init_no_prof() {
  if(!INSTRUMENTOR_INSTANCE) {
    INSTRUMENTOR_INSTANCE = new Finstrumentor(NULL, NULL);
    ((Finstrumentor*)INSTRUMENTOR_INSTANCE)->initialize();
  }
}

inline void init_probe_info(uint64_t func_addr, uint8_t* probe_addr) {

  // fprintf(stderr, "Function address at init probe : %p\n", func_addr);
  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;

  // fprintf(stderr, "probe_info address at init probe : %p\n", ins->probe_info);
  if(ins->probe_info->find(func_addr) == ins->probe_info->end()) {
    std::list<FinsProbeInfo*>* probe_list = new std::list<FinsProbeInfo*>;
    // fprintf(stderr, "Probe list address for func id %d : %p\n", func_addr, probe_list);
    FinsProbeInfo* probeInfo = new FinsProbeInfo;
    probeInfo->probeStartAddr = (probe_addr-8);
    // probeInfo->activeSequence = (uint64_t)(probe_addr - 8); // Should be return address - 8
    probeInfo->isActive = 1;

    uint64_t sequence = *((uint64_t*)(probe_addr-8));
    uint64_t mask = 0x0000000000FFFFFF;
    uint64_t deactiveSequence = (uint64_t) (sequence & mask); 
    mask = 0x0000441F0F000000; // Mask with a 5 byte NOP

    probeInfo->activeSequence = sequence;
    probeInfo->deactiveSequence = deactiveSequence | mask;

    probe_list->push_back(probeInfo);
    // fprintf(stderr, "Adding probe %p for function %p\n", probe_addr, (uint64_t*) func_addr);
    ins->probe_info->insert(make_pair(func_addr, probe_list));
  } else {
    std::list<FinsProbeInfo*>* probe_list = ins->probe_info->find(func_addr)->second;
    for(std::list<FinsProbeInfo*>::iterator iter = probe_list->begin(); iter != probe_list->end(); iter++) {
      FinsProbeInfo* probeInfo= *iter;
      if (probeInfo->probeStartAddr == (probe_addr-8)) {
        return; // Probe already initialized. Nothing to do.
      }
    }

    FinsProbeInfo* probeInfo= new FinsProbeInfo;
    probeInfo->probeStartAddr = (probe_addr - 8);
    probeInfo->isActive = 1;

    uint64_t sequence = *((uint64_t*)(probe_addr-8));
    uint64_t mask = 0x0000000000FFFFFF; // Little endianness
    uint64_t deactiveSequence = (uint64_t) (sequence & mask); 
    mask = 0x0000441F0F000000;

    probeInfo->activeSequence = sequence;
    probeInfo->deactiveSequence = deactiveSequence | mask;

    probe_list->push_back(probeInfo);
  }
}

void __cyg_profile_func_enter(void* func, void* caller) {

  fprintf(stderr, "At function entry..\n");
  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;
  uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
  init_probe_info((uint64_t)func, (uint8_t*)addr);
}

void __cyg_profile_func_exit(void* func, void* caller) {

  fprintf(stderr, "At function exit..\n");
  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;
  uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
  init_probe_info((uint64_t)func, (uint8_t*)addr);
  ins->deactivateProbeByName(func, 0); // Deactivate the probes for this function
}
