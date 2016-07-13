
#ifndef _LITEPROBE_INJECTOR_HPP_
#define _LITEPROBE_INJECTOR_HPP_

#include "liteprobes.hpp"
#include "addr_range.hpp"
#include "defs.hpp"
#include <list>
#include <map>

namespace liteinst {
namespace liteprobes {


struct PunningResult {
  public:
    utils::Address target;
    uint8_t punned_bytes[8];
};

class LiteProbeInjector {
  public:
    bool injectProbes(std::list<utils::Address>& addrs, ProbeContext& context,
        InstrumentationFunction fn);
    bool disableProbe(utils::Address addr);
    bool enableProbe(utils::Address addr);
    void rerouteControl(utils::Address);

  private:
    static std::map<utils::Address, Probe*> probes_by_addr;
    static std::vector<std::unique_ptr<Probe>> probes;
    static std::map<utils::Address, std::unique_ptr<Springboard>> relocations;

};

} // End liteprobes 
} // End liteinst

#endif /* _LITEPROBE_INJECTOR_HPP_ */
