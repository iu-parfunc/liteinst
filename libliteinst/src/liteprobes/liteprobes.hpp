
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

typedef uint64_t ProbeGroupId;
typedef uint64_t ProbeId;

class PatchPoint {
  public:
    Address addr;
    bool is_patched;
    uint8_t original_seq;
    uint8_t patched_seq;
};

enum class ControlTransferType {
  TRAMPOLINE,
  SPRINGBOARD
};

class ControlTransfer {
  public:
    Range range; 
    Range relocation_range;
    PatchPoint patch_point; 
    ControlTransferType type;
};

class SpringBoard : ControlTransfer {
  public:
    int num_trampolines;
    int active_trampolines;

    // Contains patch points to enable and disable instrumentation for
    // instructions contained with the springboard. The key is the address of 
    // an instruction in the original program.
    std::map<Address, PatchPoint> inst_points; 
    // Patch point to next chained springboard.
    PatchPoint next_springboard_ptr;
    shared_ptr<SpringBoard> previous;
    shared_ptr<SpringBoard> next; 
};

// Protect with a reader/writer lock
std::map<Range, shared_ptr<ControlTransfer>> transfers; 

class CalloutContainer {
  public:
    Range range;
    int available_slots = 4;
    int occupied_slots;
    PatchPoint next_callout_container_ptr;
    shared_Ptr<CalloutContainer> previous;
    shared_ptr<CalloutContainer> next;
    std::vector<PatchPoint> callouts;
};

class Trampoline : ControlTransfer {
  public:
    std::vector<PatchPoint> callout_containers;
    std::unordered_map<InstrumentationId, PatchPoint> patch_points;
};

// ProbeGroup * -> Probe * -> Sprinboard * -> Trampoline -> Instrumentation
//                        -> Trampoline * -> Instrumentation

class ProbeInstrumentation {
  public:
    Address addr;
    ProbeId probe_id;
    ProbeGroupId group_id;
    Trampoline current;
    std::vector<Trampoline> trampolines;
    ControlTransfer control_transfer;
    ControlTransferType transfer_type;
};

class ProbeGroupInstrumentation {
  public:
    ProbeGroupId group_id;
    std::vector<ProbeId> probes;
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
