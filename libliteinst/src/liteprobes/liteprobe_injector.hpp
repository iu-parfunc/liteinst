
#ifndef _LITEPROBE_INJECTOR_HPP_
#define _LITEPROBE_INJECTOR_HPP_

#include "liteprobes.hpp"
#include "concurrency.hpp"
#include "cycle.h"
#include "assembly.hpp"
#include "addr_range.hpp"
#include "process.hpp"
#include "defs.hpp"
#include <list>
#include <map>
#include <vector>

namespace liteinst {
namespace liteprobes {

struct JITResult {
  public:
    std::unique_ptr<Springboard> sb;
    ticks punning_cost;
};

struct InjectionResult {
  public:
    bool success;
    ticks meta_data_costs;
    ticks punning_costs;
    ticks injection_costs;
};

class LiteProbeInjector {
  public:
    InjectionResult injectProbes(std::map<utils::Address, ProbeContext>& locs, 
        const InstrumentationProvider& provider);
    bool disableProbe(utils::Address addr);
    bool enableProbe(utils::Address addr);
    void rerouteControl(utils::Address);

    Probe* getProbe(utils::Address addr);

  // private:
    utils::concurrency::ReadWriteLock meta_data_lock;

    static std::map<utils::Address, Probe*> probes_by_addr;
    static std::vector<std::unique_ptr<Probe>> probes;
    static std::map<utils::Address, std::unique_ptr<Springboard>> relocations;

    std::list<CoalescedProbes> coalesceProbes(
      utils::process::Function* fn, std::vector<utils::Address>& addrs, 
      const utils::assembly::Sequence* seq);

};

} // End liteprobes 
} // End liteinst

#endif /* _LITEPROBE_INJECTOR_HPP_ */
