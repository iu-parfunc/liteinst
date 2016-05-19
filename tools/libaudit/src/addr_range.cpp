
#include "addr_range.hpp"

namespace range {

  using namespace defs;
  using std::vector;

  Range::Range() : start(NULL), end(NULL) {

  }

  Range::Range(defs::Address start_addr, defs::Address end_addr) : 
    start(start_addr), end(end_addr) {

  }

  bool Range::withinRange(defs::Address addr, bool inclusive) {
    if (inclusive) {
      return (addr >= start && addr <= end);
    } else {
      return (addr > start && addr < end);
    }
  }

  vector<Range> Range::getBlockedRange(int32_t block_size, bool aligned) {
    vector<Range> ranges;
    if (aligned ) { 
      Address initial_block_start = start - (uint64_t) start % block_size;
      Address final_block_end;
      if ((end - start) > block_size) {
        final_block_end = end - (uint64_t) end % block_size;
      } else {
        final_block_end = initial_block_start + block_size;
      }

      Address start_ptr = initial_block_start;
      Address end_ptr;
      while (end_ptr != final_block_end) {
        end_ptr = start_ptr + block_size;
        Range r;
        r.start = start_ptr;
        r.end = end_ptr;

        ranges.push_back(r);
      }
    } else {
      Address ptr = start;
      while (ptr < end) {
        Address next = ptr + block_size;
        Range r = (next < end) ? Range(ptr, next) : Range(ptr, end);
        ptr += block_size;

        ranges.push_back(r);
      }
    }

    return ranges;
  }

  int64_t Range::distanceFromRange(defs::Address addr) {
    if ((withinRange(addr, true))) {
      return 0;
    } else if (addr < start) {
      return (int64_t) addr - (int64_t) start;
    } else {
      return (int64_t) addr - (int64_t) end;
    }
  }

  bool Range::overlapsWith(Range r) {
    if (r.withinRange(start, true) ||
        r.withinRange(end, true) || 
        (start < r.start && end > r.end)) {
      return true;
    } 
    return false;
  }

  bool Range::operator<(const Range& r) {
    return (this->end < r.start) ? true : false;
  }

  bool Range::operator>(const Range& r) {
    return (this->start > r.end) ? true : false;
  }

  bool Range::operator==(const Range& r) {
    return (this->start == r.start && this->end == r.end) ? true : false;
  }

  bool Range::operator!=(const Range& r) {
    return !(*this == r) ? true : false;
  }

  bool Range::operator<=(const Range& r) {
    return !(*this > r) ? true : false;
  }

  bool Range::operator>=(const Range& r) {
    return !(*this < r) ? true : false;
  }

}
