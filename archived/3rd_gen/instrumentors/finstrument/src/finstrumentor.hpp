/**
 * @file finstrumentor.hpp
 * @author Buddhika Chamith
 * @brief 
 * @details 
 */
#ifndef _FINSTRUMENTOR_HPP_
#define _FINSTRUMENTOR_HPP_

#include "../../../api/probe_API.hpp"
#include "../../../common/include/cycle.h"
#include <cstdint> 
#include <string>
#include <list>
#include <unordered_map>
#include <map>
#include <vector>

#define DEFAULT_PROBE_COUNT 1024 

struct FinsProbeInfo {
    uint8_t patch_strategy;
    uint8_t* probeStartAddr;
    uint8_t size;
    uint64_t activeSequence;
    uint64_t deactiveSequence;
    bool isActive;

    // Straddler specific
    bool straddler;
    bool unpatched;
    uint64_t* straddle_part_1_start;
    uint64_t* straddle_part_2_start;
    uint64_t straddle_int3_sequence;
    uint64_t activation_sequence_1;
    uint64_t activation_sequence_2;
    uint64_t deactivation_sequence_1;
    uint64_t deactivation_sequence_2;

};

typedef struct PatchResult {
  uint8_t* edi_set_addr;
  bool success;
  bool conflict;
} PatchResult;

/* Global Data */

typedef std::map<uint64_t, std::vector<FinsProbeInfo*>*> probe_map;
extern probe_map probe_info;

typedef std::map<uint64_t, volatile uint16_t> func_table;
extern func_table functions;

typedef std::map<uint16_t, uint64_t> func_id_table;
extern func_id_table function_ids;

template<class T>
struct FunctionInfo {
  uint16_t func_id;
  uint64_t func_addr;
  std::string func_name;
  uint64_t lock;
  std::vector<T> probe_Info; 
};

// Using some alias templates
template <class T>
using FuncAddrMappings = std::map<uint64_t, FunctionInfo<T>*> ;
template <class T>
using FuncIDMappings = std::map<uint16_t, FunctionInfo<T>*>;
template <class T>
using FuncNameMappings = std::map<std::string, FunctionInfo<T>*>;

extern volatile uint16_t func_id_counter;

extern uint8_t* g_straddlers_bitmap;

extern uint64_t g_straddler_count;

#ifdef __cplusplus
extern "C"
{
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

    Finstrumentor(InstrumentationFunc prologFunc, InstrumentationFunc epilogFunc); 
    void initialize();
    Instrumentor* configureInstance(InstrumentationFunc prolog,
        InstrumentationFunc epilog);

    // Function granularity activation and deactivation
    bool activateFunction(std::string name);
    bool activateFunction(uint16_t id);

    bool deactivateFunction(std::string name);
    bool deactivateFunction(uint16_t id);

    // Call site / Probe granularity activation and deactivation
    // Not all instrumentors may support this
    // int activateProbe(void* id, int type);
    // int activateProbeByName(void* id, int type);
    // int deactivateProbe(void* id, int type);
    // int deactivateProbeByName(void* id, int type);

    FinsProbeInfo* getProbeInfo(uint64_t func_addr, uint8_t* addr); 
    bool hasProbeInfo(uint64_t func_addr);
    void addProbeInfo(uint64_t func_addr, uint8_t* probe_addr, bool unpatched); 
    void addProbeInfo_(uint64_t func_addr, uint8_t* probe_addr, bool unpatched); 
  
    // Function name to id mapping (generic)
    uint16_t getFunctionId(std::string name);
    std::string getFunctionName(uint16_t id);

    // Function address to id mapping (Finstrumentor specific)
    uint16_t getFunctionId(uint64_t addr);
    uint64_t getFunctionAddress(uint16_t id);

    uint64_t getInstrumentorBackgroundOverhead();

    // Given function address 
    uint64_t* getLock(uint64_t addr);
    void addFunction(uint64_t addr, std::string name);
    std::vector<FinsProbeInfo*>* getProbes(uint16_t func_id);
    virtual ~Finstrumentor();

  private:
    FuncAddrMappings<FinsProbeInfo>* func_addr_mappings;
    FuncIDMappings<FinsProbeInfo>* func_id_mappings;
    FuncNameMappings<FinsProbeInfo>* func_name_mappings;
    void readFunctionInfo();

};

#endif /* _FINSTRUMENTOR_HPP_ */

