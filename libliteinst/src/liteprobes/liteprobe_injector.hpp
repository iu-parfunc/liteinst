
#include "liteinst.hpp"
#include "defs.hpp"
#include <list>
#include <map>

namespace liteinst {
namespace liteprobes {

class SpringBoard {

};

class Probe {
  public:
    ProbeId p_id;
    ProbeContext context;
    Trampoline* trampoline;
};

class LiteProbeInjector {
  public:
    bool injectProbes(std::list<utils::Address>& addrs, ProbeContext context);
    bool disableProbe(utils::Address addr);
    bool enableProbe(utils::Address addr);
    void rerouteControl(utils::Address);

  private:
    static std::map<utils::Address, Probe*> probes_by_addr;
    static std::vector<std::unique_ptr<Probe>> probes;
    static std::map<utils::Address, std::unique_ptr<SpringBoard>> relocations;

    SpringBoard* getContainedSpringBoard(utils::Address addr);

};

} // End liteprobes 
} // End liteinst
