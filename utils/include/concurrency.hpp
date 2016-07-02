
#ifndef _CONCURRENCY_HPP_
#define _CONCURRENCY_HPP_

#include <atomic>
#include <cassert>
#include <thread>
#include <functional> 
#include <map>
#include <utility>
#include <stdexcept>

namespace utils {
namespace concurrency {

class SpinLock {
  public:
    SpinLock() : lock_owner(lock_is_free), is_latched(false), lock_count(0) {
    }

    inline bool tryLock() {
      if (lock_owner != std::this_thread::get_id()) {
        bool non_latched_value = false;
        bool locked = is_latched.compare_exchange_strong(non_latched_value, 
            true, std::memory_order_acquire, std::memory_order_relaxed);
        if (locked) {
          lock_count++;
        }
        return locked;
      }

      lock_count++;
      return true;
    }

    inline void lock() {
      if (lock_owner != std::this_thread::get_id()) {

        bool non_latched_value = false;
        // Test until we see a potential unlocked state
        while(is_latched.load(std::memory_order_relaxed) != non_latched_value) {
          __asm__("pause"); // Gentle spinning.
        }

        // Now try test and set
        while(!is_latched.compare_exchange_weak(non_latched_value,
              true, std::memory_order_acquire,
              std::memory_order_relaxed)) {
          non_latched_value = false;
          // Again test until we a potential unlocked state
          while(is_latched.load(std::memory_order_relaxed) != non_latched_value) {
            __asm__("pause"); // Gentle spinning.  
          }
        }

        lock_owner = std::this_thread::get_id();
      }

      lock_count++;
    }

    inline void unlock() {
      assert(lock_owner == std::this_thread::get_id());
      assert(lock_count != 0);

      --lock_count;

      // There can be a window of is_latched locked but without any owner due 
      // to non atomicity of this update. But that doesn't negatively affect 
      // the correctness of the lock.      
      lock_owner = lock_is_free;
      if (lock_count == 0) {
        is_latched.store(false, std::memory_order_release);
      }
    }

    inline bool isOwner() {
      return lock_owner == std::this_thread::get_id();
    }

    inline bool isSet() {
      return is_latched.load(std::memory_order_relaxed);
    }

  private:
    std::thread::id lock_is_free;
    std::thread::id lock_owner;
    std::atomic<bool> is_latched;
    int lock_count;
};

/** \brief A non blocking readers writers lock.
 *  
 *  1. The lock is recursive.
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
      assert(w_lock.isOwner());
      assert(w_lock.isSet());
      if (num_writers >= 1) {
        // printf("Is already owned : %d Is already set : %d \n", is_already_owned, isSet);
        abort();
      }
      num_writers++;
      assert(num_writers == 1 && w_lock.isSet());
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

    inline void clear() {
      writeLock();
      writeUnlock();
    }

    inline bool isSet() {
      return r_lock.isSet() && w_lock.isSet();
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
