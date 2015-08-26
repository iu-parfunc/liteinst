
#ifndef _LOCk_HPP_
#define _LOCK_HPP_

#include <atomic>

namespace lock {

  class CASLock {

    private:
      std::atomic<uint32_t> cas_lock;

    public:
      CASLock();
      bool tryLock();
      void lock();
      void unlock();
  };
}

#endif /* _LOCK_HPP_ */
