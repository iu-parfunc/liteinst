
#include "liteinst.hpp"
#include "liteprobe_provider.hpp"

namespace liteinst {

using namespace liteinst;
using namespace liteinst::liteprobes;

ProbeProvider* PROBE_PROVIDER = NULL;

ProbeProvider* initializeGlobalProbeProvider(ProviderType type, 
    Callback callback) {
  if (PROBE_PROVIDER == NULL) {
    switch(type) {
      case ProviderType::LITEPROBES:
        fprintf(stderr, "[Probe Provider] Initializing liteprobes provider.\n"); 
        PROBE_PROVIDER = new LiteProbeProvider(callback);
        return PROBE_PROVIDER;
      default:
        fprintf(stderr, "Unknown provider type.\n");
        throw -1;
    }
  } else {
    throw -1;
  }

  throw -1;
}

ProbeProvider* getGlobalProbeProvider() {
  return PROBE_PROVIDER;
}

}
