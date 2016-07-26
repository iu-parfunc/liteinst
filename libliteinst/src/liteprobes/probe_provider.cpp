
#include "liteinst.hpp"
#include "liteprobe_provider.hpp"

namespace liteinst {

using namespace liteinst;
using namespace liteinst::liteprobes;

using std::unique_ptr;

unique_ptr<ProbeProvider> ProbeProvider::p;
utils::concurrency::SpinLock ProbeProvider::lock;

ProbeProvider* ProbeProvider::initializeGlobalProbeProvider(ProviderType type, 
    Callback callback, InitCallback init) {
  if (p == nullptr) {
    switch(type) {
      case ProviderType::LITEPROBES:
        fprintf(stderr, "[Probe Provider] Initializing liteprobes provider.\n"); 
        lock.lock();
        if (p == nullptr) {
          p = unique_ptr<ProbeProvider>(new LiteProbeProvider(callback, init));
        }
        lock.unlock();
        break;
      default:
        fprintf(stderr, "Unknown provider type.\n");
        throw -1;
    }
  }

  printf("Init call back : %p\n", p->init_callback);
  return p.get();
}

ProbeProvider* ProbeProvider::getGlobalProbeProvider() {
  return p.get();
}

}
