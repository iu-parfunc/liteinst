
#include <atomic>

namespace lock {

  class CASLock {

    private:
      std::atomic<uint32_t> cas_lock;

    public:
      CASLock();
      void lock();
      void unlock();
  };
}
