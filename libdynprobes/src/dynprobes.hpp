
#include <cstdint> 
#include <string>

// Probe context definitions
#define ENTRY 0x01          // Function entry context
#define EXIT  0x02          // Function exit context
#define OUTCALL_ENTRY 0x04  // Function entry contexts for functions called within
                            // current function
#define OUTCALL_EXIT  0x08  // Function exit contexts for functions called within
                            // current function
#define LINE_NUM      0x10  // Line number context 
#define ADDRESS       0x20  // Instruction aligned address within function

/// A byte addressible data type
typedef uint8_t* Address;

typedef struct ProbeInfo {
  Address probe_addr;
  Address stub_addr;
  Address trampoline_call_addr;
} ProbeInfo;

typedef struct Probes {
  ProbeInfo* probe_info;
  int size;
} Probes;

/// Type definition for probe context. e.g: What is the context of a 
/// particular probe location within a function?  
typedef uint8_t ProbeContext;

void initialize();
Probes* injectProbe(Address addr);
Probes* injectProbesAtFunction(std::string function, ProbeContext ctx); 
Probes* injectProbesAtFunction(Address func_addr, ProbeContext ctx); 
