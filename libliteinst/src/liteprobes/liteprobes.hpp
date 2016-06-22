
#include "liteinst.hpp"
#include "range.hpp"

#include <functional>
#include <cstdint> 
#include <string>
#include <map>
#include <memory> // shared_ptr

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

namespace liteinst {
namespace liteprobes {

class PatchPoint {
  public:
    utils::Address addr;
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
    std::map<utils::Address, PatchPoint> inst_points; 
    // Patch point to next chained springboard.
    PatchPoint next_springboard_ptr;
    std::shared_ptr<SpringBoard> previous;
    std::shared_ptr<SpringBoard> next; 
};

// Protect with a reader/writer lock
std::map<Range, std::shared_ptr<ControlTransfer>> transfers; 

class CalloutContainer {
  public:
    Range range;
    int available_slots = 4;
    int occupied_slots;
    PatchPoint next_callout_container_ptr;
    std::shared_ptr<CalloutContainer> previous;
    std::shared_ptr<CalloutContainer> next;
    std::vector<PatchPoint> callouts;
};

class Trampoline : ControlTransfer {
  public:
    std::vector<PatchPoint> callout_bypasses;
    std::unordered_map<utils::Address, PatchPoint> patch_points; 
    // Key instrumentation fn address
};

// ProbeGroup * -> Probe * -> Sprinboard * -> Trampoline -> Instrumentation
//                        -> Trampoline * -> Instrumentation

class ProbeInstrumentation {
  public:
    utils::Address addr;
    ProbeId probe_id;
    std::vector<ProbeContext> contexts;
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

typedef void (*ProbeFn)(utils::Address fn_addr, utils::Address call_site_addr);

typedef void (*CallbackFn)();

/// Type definition for probe context. e.g: What is the context of a 
/// particular probe location within a function?  
typedef uint8_t ProbeContext;

void initialize(CallbackFn cb);
void injectProbe(utils::Address addr);
// Probes* injectProbesAtFunction(std::string function, ProbeContext ctx); 
// Probes* injectProbesAtFunction(Address func_addr, ProbeContext ctx); 

}
}
