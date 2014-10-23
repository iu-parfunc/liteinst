
#ifndef _FINSTRUMENTOR_HPP_
#define _FINSTRUMENTOR_HPP_

#include "../../../api/probe_API.hpp"
#include <cstdint> 
#include <list>
#include <unordered_map>
#include <map>
#include "dynamicarray.h"

#define DEFAULT_PROBE_COUNT 1024 


typedef void (*InstrumentationFunc)(uint16_t);

typedef struct FinsProbeInfo {
    // uint8_t patch_strategy;
    uint8_t* probeStartAddr;
    // uint8_t* probe_end_addr;
    // uint8_t size;
    /* argument patching strategy specific data */
    // uint8_t* rsi_addr;
    // uint8_t* rdi_addr;
    // uint8_t rsi_ins_size;
    // uint8_t rdi_ins_size;
    // uint8_t param_ins_distance;
    // uint64_t original_info;

    /* call site redirection strategy specific data */
    uint8_t callSequence[1];
} FinsProbeInfo;

typedef struct FinsStatistics {
    uint64_t funcAddr;
    uint64_t count;
} FinsStatistics;

#ifdef __cplusplus
extern "C"
{
  extern Instrumentor* INSTRUMENTOR_INSTANCE; // Need to be set by the Profiler
  extern InstrumentationFunc prologFunc;
  extern InstrumentationFunc epilogFunc;
  extern FinsProbeInfo* probeInfo;
}
#endif
  
class Finstrumentor : public Instrumentor {

  public :
    InstrumentationFunc prologFunc;
    InstrumentationFunc epilogFunc;
    // DynamicArray<FinsProbeInfo>* probeInfo;
    FinsProbeInfo* probeInfo;

    Finstrumentor(InstrumentationFunc prologFunc, InstrumentationFunc epilogFunc); 
    void initialize();
    int activateProbe(std::string name, void* funcPtr);
    int deactivateProbe(std::string name);
    virtual ~Finstrumentor();

};

/* Global Data */
// typedef std::unordered_map<uint64_t, std::list<Probe_Info*>*> probe_map;
// extern probe_map probe_info;

typedef std::map<uint64_t, uint16_t> func_table;
extern func_table functions;

// extern volatile uint16_t func_id_counter = 0;

// Statistics* ubi_global_stats;

#endif /* _FINSTRUMENTOR_HPP_ */

