
#include "fastinst.hpp"
#include "finstrument_probe_provider.hpp"

ProbeProvider* PROBE_PROVIDER;

ProbeProvider* ProbeProvider::initializeProbeProvider(ProviderType type, Callback callback) {
  if (PROBE_PROVIDER != NULL) {
    switch(type) {
      case ProviderType::FINSTRUMENT:
        PROBE_PROVIDER = new FinstrumentProbeProvider(callback);
        return PROBE_PROVIDER;
      case ProviderType::ZCA:
        fprintf(stderr, "ZCAProbeProvider not yet supported..\n");
        return NULL;
      case ProviderType::DTRACE:
        fprintf(stderr, "DtraceProbeProvider not yet supported..\n");
        return NULL;
      case ProviderType::DYNINST:
        fprintf(stderr, "DyninstProbeProvider not yet supported..\n");
        return NULL;
      default:
        fprintf(stderr, "Unknown provider type.\n");
        return NULL;
    }
  } else {
    throw -1;
  }
}

ProbeProvider* ProbeProvider::getProbeProvider() {
  return PROBE_PROVIDER;
}
