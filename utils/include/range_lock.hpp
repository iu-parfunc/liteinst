
#ifndef RANGE_LOCK_H
#define RANGE_LOCK_H

#include "addr_range.hpp"
#include "concurrency.hpp"

#include <map>
#include <memory>

namespace utils {
namespace range {

class RangeComparator {
  public:
    bool operator() (const Range& lhs, const Range& rhs) const {
      return !((Range)lhs).overlapsWith(rhs);
    }
};

/// RangeLock enables locking based on address ranges
class RangeLock {
  public:
    /** \brief Locks on given address range.
     *  \param r The range to be locked on.
     *
     *  Any subsequent lockRange invocations with ranges which overlaps the 
     *  given range r will be blocked until unlockRange on r is called.
     */
    void lockRange(Range r);

    /** \brief Unlocks the given address range.
     *  \param r The range to be unlocked.
     */
    void unlockRange(Range r);

  private:
    utils::concurrency::SpinLock range_map_lock; ///< Lock to protect range_map
                                                 ///< insertions
    std::map<Range, std::unique_ptr<utils::concurrency::SpinLock>, 
      RangeComparator> range_map;
    ///< Existing range lock mappings. If two ranges overlap with each other
    ///< they are considered equal.
};

} /* End range */
} /* End utils */

#endif /*RANGE_H*/
