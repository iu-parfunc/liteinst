
#include "lock.hpp"


namespace lock {

  uint32_t UNLOCKED_VAL = 0;
  uint32_t LOCKED_VAL = 1;

  CASLock::CASLock() {
    cas_lock = 0;
  }

  bool CASLock::tryLock() {
    if(cas_lock.compare_exchange_weak(UNLOCKED_VAL, LOCKED_VAL)) {
      return true;
    }
    return false;
  }

  void CASLock::lock() {
    while(!cas_lock.compare_exchange_weak(UNLOCKED_VAL, LOCKED_VAL));
    return;
  }

  void CASLock::unlock() {
    while(!cas_lock.compare_exchange_weak(LOCKED_VAL, UNLOCKED_VAL));
    return;
  }
}
