
#ifndef _FINSTRUMENTOR_HPP_
#define _FINSTRUMENTOR_HPP_

#include "../../../api/probe_API.hpp"
#include <cstdint> 
#include <string>
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

typedef std::map<uint16_t, uint64_t> func_id_table;
extern func_id_table function_ids;

typedef struct FunctionInfo {
  uint16_t func_id;
  uint64_t func_addr;
  std::string func_name;
  uint64_t lock;
} FunctionInfo;

typedef std::map<uint64_t, FunctionInfo*> FuncAddrMappings;
typedef std::map<uint16_t, FunctionInfo*> FuncIDMappings;

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
    func_id_table* function_ids;
    volatile uint16_t func_id_counter;
    InstrumentationFunc prologFunc;
    InstrumentationFunc epilogFunc;
    // DynamicArray<FinsProbeInfo>* probeInfo;
    // FinsProbeInfo* probeInfo;

    Finstrumentor(InstrumentationFunc prologFunc, InstrumentationFunc epilogFunc); 
    void initialize();
    int activateProbe(void* id, int type);
    int activateProbeByName(void* id, int type);
    int deactivateProbe(void* id, int type);
    int deactivateProbeByName(void* id, int type);
    uint16_t getFunctionId(uint64_t addr);
    uint64_t getFunctionAddress(uint16_t id);
    uint64_t* getLock(uint64_t addr);
    void addFunction(uint64_t addr, char* name);
    std::string getFunctionName(uint16_t id);
    virtual ~Finstrumentor();

  private:
    FuncAddrMappings* func_addr_mappings;
    FuncIDMappings* func_id_mappings;
    void readFunctionInfo();

};

// Statistics* ubi_global_stats;

#endif /* _FINSTRUMENTOR_HPP_ */

