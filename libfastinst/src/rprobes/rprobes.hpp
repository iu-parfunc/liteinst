
#include <functional>
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
#define BB_ENTRY      0x40  // Basic block entry context
#define BB_EXIT       0x80  // Basic block exit context 

class PatchPoint {
  public:
    Address addr;
    uint8_t active_seq;
    uint8_t inactive_seq;
}

enum class ControlTransferType {
  TRAMPOLINE,
  SPRINGBOARD
}

class ControlTransfer {
  public:
    Range range; 
    Range relocation_range;
    PatchPoint patch_point; 
};

class SpringBoard : ControlTransfer {
  public:
    int num_trampolines;
    int active_trampolines;
};

class Trampoline : ControlTransfer {
};

class ProbeInstrumentation {
  public:
    Trampoline trampoline;
    std::vector<PatchPoint> patch_points;
    ControlTransfer control_transfer;
    ControlTransferType transfer_type;
};

typedef void (*ProbeFn)(Address fn_addr, Address call_site_addr);

typedef void (*CallbackFn)();

/// Type definition for probe context. e.g: What is the context of a 
/// particular probe location within a function?  
typedef uint8_t ProbeContext;

void initialize(CallbackFn cb);
void injectProbe(Address addr);
Probes* injectProbesAtFunction(std::string function, ProbeContext ctx); 
Probes* injectProbesAtFunction(Address func_addr, ProbeContext ctx); 
