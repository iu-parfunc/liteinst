
#ifndef _LOCk_HPP_
#define _LOCK_HPP_

#include <atomic>

namespace lock {

  class CASLock {

    private:
      // Buddhika : std::atomic version not working at the moment.
      // Revertng plain __sync_compare_and_swap
      // std::atomic<uint32_t> cas_lock;
      uint32_t cas_lock_raw;

    public:
      CASLock();
      bool tryLock();
      void lock();
      void unlock();
  };
}

#endif /* _LOCK_HPP_ */
