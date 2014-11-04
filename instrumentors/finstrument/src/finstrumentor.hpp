
#ifndef _FINSTRUMENTOR_HPP_
#define _FINSTRUMENTOR_HPP_

#include "../../../api/probe_API.hpp"
#include <cstdint> 
#include <list>
#include <unordered_map>
#include <map>
#include "dynamicarray.h"

#define DEFAULT_PROBE_COUNT 1024 

// typedef void (*InstrumentationFunc)(uint16_t);

typedef struct FinsProbeInfo {
    uint8_t patch_strategy;
    uint8_t* probeStartAddr;
    // uint8_t* probe_end_addr;
    uint8_t size;
    /* argument patching strategy specific data */
    // uint8_t* rsi_addr;
    // uint8_t* rdi_addr;
    // uint8_t rsi_ins_size;
    // uint8_t rdi_ins_size;
    // uint8_t param_ins_distance;
    // uint64_t original_info;

    /* call site redirection strategy specific data */
    uint64_t activeSequence;
    uint64_t deactiveSequence;
    bool isActive;
} FinsProbeInfo;

typedef struct FinsStatistics {
    uint64_t funcAddr;
    uint64_t count;
} FinsStatistics;

/* Global Data */
typedef std::map<uint64_t, std::list<FinsProbeInfo*>*> probe_map;
extern probe_map probe_info;

typedef std::map<uint64_t, volatile uint16_t> func_table;
extern func_table functions;

extern volatile uint16_t func_id_counter;

#ifdef __cplusplus
extern "C"
{
  // extern Instrumentor* INSTRUMENTOR_INSTANCE; // Need to be set by the Profiler
  extern InstrumentationFunc prologFunction;
  extern InstrumentationFunc epilogFunction;
  extern void* probeInfo;
}
#endif
  
class Finstrumentor : public Instrumentor {

  public :
    probe_map* probe_info;
    func_table* functions;
    volatile uint16_t func_id_counter;
    InstrumentationFunc prologFunc;
    InstrumentationFunc epilogFunc;
    // DynamicArray<FinsProbeInfo>* probeInfo;
    // FinsProbeInfo* probeInfo;

    Finstrumentor(InstrumentationFunc prologFunc, InstrumentationFunc epilogFunc); 
    void initialize();
    int activateProbe(void* id, int type);
    int deactivateProbe(void* id, int type);
    virtual ~Finstrumentor();

};

// Statistics* ubi_global_stats;

#endif /* _FINSTRUMENTOR_HPP_ */

