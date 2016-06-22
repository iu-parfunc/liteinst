
#ifndef _CONCURRENCY_HPP_
#define _CONCURRENCY_HPP_

#include <atomic>
#include <cassert>
#include <thread>
#include <functional> 
#include <map>
#include <utility>

namespace utils {
namespace concurrency {

/** \brief A non blocking spin lock.
 * 
 * Lock is reentrant.
 */
class SpinLock {
  public:
    SpinLock(bool is_reentrant = true) : is_reentrant(is_reentrant) {
      std::atomic_flag_clear_explicit(&cas_lock, std::memory_order_release);
    }

    inline bool tryLock() {
      if (is_reentrant) {
        if (owner != std::this_thread::get_id()) {
          is_set = !std::atomic_flag_test_and_set_explicit(&cas_lock, 
            std::memory_order_acquire);
        }
      } else {
        is_set = !std::atomic_flag_test_and_set_explicit(&cas_lock, 
          std::memory_order_acquire);
      }

      return is_set;
    }

    inline void lock() {
      if (is_reentrant) {
        if (owner != std::this_thread::get_id()) {
          while (std::atomic_flag_test_and_set_explicit(&cas_lock, 
              std::memory_order_acquire));
          owner = std::this_thread::get_id();
          is_set = true;
        } 
      } else {
        while (std::atomic_flag_test_and_set_explicit(&cas_lock, 
            std::memory_order_acquire));
        is_set = true;
      }

    }

    inline void unlock() {
      if (is_reentrant) {
        if (owner == std::this_thread::get_id()) {
          std::atomic_flag_clear_explicit(&cas_lock, std::memory_order_release);
          is_set = false;
        }
      } else {
        std::atomic_flag_clear_explicit(&cas_lock, std::memory_order_release);
        is_set = false;
      }
    }

    inline bool isSet() {
      return is_set;
    }

    inline bool isOwner() {
      if (is_reentrant) {
        return (owner == std::this_thread::get_id());
      } else {
        // throw invalid_argument
      }
    }

  private:
    bool is_reentrant;
    bool is_set = false;
    std::thread::id owner;
    // Buddhika : std::atomic version not working at the moment.
    // Revertng plain __sync_compare_and_swap
    // std::atomic<uint32_t> cas_lock;
    // uint32_t cas_lock_raw;
    std::atomic_flag cas_lock = ATOMIC_FLAG_INIT;
};

/** \brief A non blocking readers writers lock.
 *  
 *  1. The lock is reentrant.
 *  2. The lock implmentation gives weak priority to writers.
 */
class ReadWriteLock {
  public:
    inline void readLock() {
      /*
      if (tx_lock.isSet() && tx_lock.isOwner()) {
        // Short circuit. We already have the exclusive lock.
        return;
      }*/

      r_lock.lock();
      while (num_writers != 0); 

      if (num_readers == 0) {
        w_lock.lock();
      }

      num_readers++;
      assert(num_writers == 0);

      r_lock.unlock();
    }

    inline void readUnlock() {
      /*
      if (tx_lock.isSet() && tx_lock.isOwner()) {
        // Short circuit. We already have the exclusive lock.
        return;
      }*/

      r_lock.lock();
      num_readers--;
      if (num_readers == 0) {
        assert(num_writers == 0);
        w_lock.unlock();
      }
      r_lock.unlock();
    }

    inline void writeLock() {
      /*
      if (tx_lock.isSet() && tx_lock.isOwner()) {
        // Short circuit. We already have the exclusive lock.
        return;
      }*/

      w_lock.lock();
      num_writers++;
      assert(num_writers == 1);
    }

    inline void writeUnlock() {
      /*
      if (tx_lock.isSet() && tx_lock.isOwner()) {
        // Short circuit. We already have the exclusive lock.
        return;
      }*/

      num_writers--;
      assert(num_writers == 0);
      w_lock.unlock();
    }

    /*
    inline void exclusiveLock() {
      w_lock.lock();
      tx_lock.lock();
    }

    inline void exclusiveLock() {
      tx_lock.unlock();
      w_lock.unlock();
    }

    */

  private:
    SpinLock r_lock;
    SpinLock w_lock;
    // SpinLock tx_lock(true);
    int32_t num_readers = 0;
    int32_t num_writers = 0;
};

/** \brief A concurrent ordered map.
 */
template <class Key, class T, class Compare = std::less<Key>> 
class ConcurrentMap {
  public:
    typedef typename std::map<Key, T, Compare>::iterator Iterator;
    typedef typename std::pair<Iterator, bool> InsertResult;

    void acquireUpdateLock() {
      rw_lock.writeLock();
    }

    void releaseUpdateLock() {
      rw_lock.writeUnlock();
    }

    // TODO: Make a proper wrapper. Currently this exposes 
    // underlying map iterator.
    void upsert(Key k, T value) {
      rw_lock.writeLock();
      auto it = map.find(k);
      if (it != map.end()) {
        if (it.first != k) {
          map.erase(it.first);
        } 
      }

      map[k] = value;
      rw_lock.writeUnlock();
    }

    template<class... Args>
    InsertResult emplace(Args&&... args) {
      rw_lock.writeLock();
      InsertResult res = map.emplace(std::forward<Args>(args)...);
      rw_lock.writeUnlock();
      return res;
    }

    Iterator find(Key k) {
      rw_lock.readLock();
      auto result = map.find(k);
      rw_lock.readUnlock();
      return result;
    }

    InsertResult insert(Key k, T value) {
      rw_lock.writeLock();
      auto result = map.insert(std::pair<Key,T>(k, value));
      rw_lock.writeUnlock();
      return result;
    }

    void erase(Key k) {
      rw_lock.writeLock();
      map.erase(k);
      rw_lock.writeUnlock();
    }

    void clear() {
      rw_lock.writeLock();
      map.clear();
      rw_lock.writeUnlock();
    }

    Iterator end() {
      return map.end();
    }

    int size() {
      return map.size();
    }

    Iterator lower_bound(const Key& k) {
      rw_lock.readLock();
      map.lower_bound(k);
      rw_lock.readUnlock();
    }

    Iterator upper_bound(const Key& k) {
      rw_lock.readLock();
      map.upper_bound(k);
      rw_lock.readUnlock();
    }

  private:
    ReadWriteLock rw_lock;
    std::map<Key, T, Compare> map;

};

} /* End concurrency */
} /* End utils */

#endif /* _CONCURRENCY_HPP_ */
