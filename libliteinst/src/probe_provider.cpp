
#include "liteinst.hpp"
// #include "finstrument/finstrument_probe_provider.hpp"
// #include "zca/zca_probe_provider.hpp"
#include "liteprobes/liteprobe_provider.hpp"

ProbeProvider* PROBE_PROVIDER = NULL;

ProbeProvider* initializeGlobalProbeProvider(ProviderType type, Callback callback,
    InstrumentationFunc prolog, InstrumentationFunc epilog) {
  if (PROBE_PROVIDER == NULL) {

    switch(type) {
      case ProviderType::FINSTRUMENT:
        fprintf(stderr, "[Probe Provider] Initializing -finstrument-functions " 
            "based probe provider..\n");
        PROBE_PROVIDER = new FinstrumentProbeProvider(callback);
        PROBE_PROVIDER->initializeProvider(prolog, epilog);
        return PROBE_PROVIDER;
      case ProviderType::ZCA:
        fprintf(stderr, "[Probe Provider] Initializing intel ZCA " 
            "based probe provider..\n");
        PROBE_PROVIDER = new ZCAProbeProvider(callback);
        PROBE_PROVIDER->initializeProvider(prolog, epilog);
        return PROBE_PROVIDER;
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
