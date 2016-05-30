
#ifndef _LOCK_HPP_
#define _LOCK_HPP_

#include <atomic>

namespace lock {

  class CASLock {

    // This lock doesn't check ownership. Hence non reentrant.
    // Also proper API usage is assumed where acquire is first done before 
    // the release from a given thread
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
