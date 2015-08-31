
#include "fastinst.hpp"
#include "finstrument_probe_provider.hpp"

ProbeProvider* PROBE_PROVIDER = NULL;

ProbeProvider* initializeGlobalProbeProvider(ProviderType type, Callback callback) {
  if (PROBE_PROVIDER == NULL) {
    switch(type) {
      case ProviderType::FINSTRUMENT:
        PROBE_PROVIDER = new FinstrumentProbeProvider(callback);
        // PROBE_PROVIDER->initializeProvider();
        return PROBE_PROVIDER;
      case ProviderType::ZCA:
        fprintf(stderr, "ZCAProbeProvider not yet supported..\n");
        throw -1;
      case ProviderType::DTRACE:
        fprintf(stderr, "DtraceProbeProvider not yet supported..\n");
        throw -1;
      case ProviderType::DYNINST:
        fprintf(stderr, "DyninstProbeProvider not yet supported..\n");
        throw -1;
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
