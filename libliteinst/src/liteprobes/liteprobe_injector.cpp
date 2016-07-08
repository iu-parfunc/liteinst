
#include "liteprobe_injector.hpp"

namespace liteinst {
namespace liteprobes {

using namespace utils::process;

using std::list;
using utils::Address;

map<Address, unique_ptr<Probe>> LiteProbeInjector::probes;

bool LiteProbeInjector::injectProbes(vector<Address>& addrs, 
    Probecontext context) {
  addrs.sort();
  for(auto it = addrs.rbegin(); it = addrs.end(); i++) {
    Address addr = *it;

    auto it = probes_by_addr.find(addr);
    if (it != probes_by_addr.end()) {
      return false;
    }
  }

  Process p;
  Function* start_probe_fn = p.getContainedFunction(addrs.front());
  Function* end_probe_fn = p.getContainedFunction(addrs.back());

  if (!start_probe_fn.is_valid || !end_probe_fn.is_valid &&
       start_probe_fn != end_probe_fn) {
    throw invalid_argument("All probe addresses should be within a single " 
        "function");
  } 

  // Lock for function
  // Disassemble function
  Function* fn = start_probe_fn;
  Disassembler disas;
  Sequence seq = disas.disassemble(fn->start, fn->end);
  _DInst* decoded = static_cast<_DInst*>(seq.instructions);

  for(unsigned i = addrs.size(); i-- > 0;) {
    Address next = nullptr;
    if (i > 0) {
      next = addrs[i-1];
    }

    Address addr = addrs[i];
    int index = findInstructionIndex(addr, seq);
    Address end = addr + decoded[index].size;

    list<Address> probes;
    probes.push_back(addr);
    while (fn->end - addr >= 5 && index >= 0) {
      addr -= decoded[--index].size;
      if (addr == next) {
        probes.push_back(addr);

      }
    }

    if (fn->end - addr < 5) {
      return false;
    }

    SpringBoard* sb = getContainedSpringBoard(addr);
    if (sb == nullptr) {
      if (isProbeReady(index, seq)) {

      }

      Address target = punAddress(addr, end, index, seq);
      if (target != nullptr) {

      }
    } else {

    }


    // Check function boundaries
    // Backtrack if necessary
    // Check if it is within a trampoline
    // If not {
    //   Try to pun
    //   Backtrack until punnable instruction is met
    //   Generate trampoline or super trampoline
    //   Generate probe and trampoline meta data
    // } else {
    //   Try pun trampoline'd instruction with a new super trampoline
    //   Back track until punnable instruction is met
    //   Generate super trampoline
    //   Generate probe and trampoline meta data
    // }


  }
}

}
}
