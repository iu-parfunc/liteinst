
#include "dynprobes.hpp"
#include "lock.hpp"

#include "distorm.h"

#include <map>
#include <list>
#include <vector>

/******** Private type definitions ********/
typedef struct ProbedFunction {
  uint32_t lock;
  ProbeContext probed_contexts;
} ProbedFunction; 

typedef struct MemIslet {
  Address start;
  Address end;
} MemIslet;

typedef struct MemIsle {
  Address start;
  Address end;
  std::atomic<uint32_t> next_space_ptr;
} MemIsle;

typedef struct MemIslandGroup{
  std::list<MemIslet*>* islets; 
  std::list<MemIsle*>* isles;
  MemIsle* current_isle;
  lock::CASLock isle_lock;
  lock::CASLock islet_lock;
} MemIslandGroup;

typedef struct { 
  _DInst *decoded_instructions; 
  unsigned int n_instructions; 
} Decoded; 

enum TraverseDirection { UP, DOWN};

typedef struct {
  int offset;
  TrverseDirection direction;
  int n_intermediate_instructions;
  std::vector<Address>* instruction_addrs;
} ProbeOffset;

typedef std::map<Address, ProbedFunction> ProbedFunctionInfo; 
typedef std::map<Address, Probe> ProbeInfo;
typedef std::map<int32_t, MemIslandGroup> IslandInfo;  
typedef std::map<std::string, Address> FuncNameMapping;
typedef std::map<Address, std::string> FuncAddrMapping;
