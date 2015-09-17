
#include "lock.hpp"

namespace lock {

  uint32_t UNLOCKED_VAL = 0;
  uint32_t LOCKED_VAL = 1;

  CASLock::CASLock() {
    // cas_lock = 0;
    cas_lock_raw = 0;
  }

  bool CASLock::tryLock() {
    // if(cas_lock.compare_exchange_strong(UNLOCKED_VAL, LOCKED_VAL)) {
    if(__sync_bool_compare_and_swap(&cas_lock_raw, UNLOCKED_VAL, LOCKED_VAL)) {
      return true;
    }
    return false;
  }

  void CASLock::lock() {
    // while(!cas_lock.compare_exchange_strong(UNLOCKED_VAL, LOCKED_VAL));
    while (!__sync_bool_compare_and_swap(&cas_lock_raw, UNLOCKED_VAL, LOCKED_VAL));
    return;
  }

  void CASLock::unlock() {
    // while(!cas_lock.compare_exchange_strong(LOCKED_VAL, UNLOCKED_VAL));
    while (!__sync_bool_compare_and_swap(&cas_lock_raw, LOCKED_VAL, UNLOCKED_VAL));
    return;
  }
}
