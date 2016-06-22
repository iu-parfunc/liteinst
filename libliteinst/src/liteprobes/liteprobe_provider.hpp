
#include <unordered_map>

#include "liteprobes.hpp"
#include "range.hpp"

namespace liteinst {
namespace liteprobes {

// typedef std::unordered_map<Range, ControlTransfer> ControlTransferMap;

class LiteProbeProvider : public ProbeProvider {
  public:
    LiteProbeProvider(Callback cb) : LiteProbeProvider(cb) {
    }

    ProbeRegistration configure(Coordinates coords,
        std::string instrumentation_provider);

    bool activate(ProbeContext ctx);
    bool deactivate(ProbeContext ctx);

    bool activate(ProbeRegistration registration);
    bool deactivate(ProbeRegistration registraiton);

  private:
    // std::map<int, InstrumentationProvider> i_providers;
    // ControlTransferMap control_transfers;

}; 

} /* End liteprobes */
} /* End liteinst */
