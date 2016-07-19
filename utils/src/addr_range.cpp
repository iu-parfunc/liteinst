
#include "addr_range.hpp"

namespace utils {
namespace range {

using std::vector;
using utils::Address;

const uint8_t Range::START = 0x01;
const uint8_t Range::END = 0x02;
const uint8_t Range::EXCLUSIVE = 0x00;
const uint8_t Range::INCLUSIVE = 0x04;

Range::Range() : start(nullptr), end(nullptr) {

}

Range::Range(Address start_addr, Address end_addr) : 
  start(start_addr), end(end_addr) {

}

bool Range::withinRange(Address addr, uint8_t inclusive) {
  bool start_inclusive = false;
  bool end_inclusive = false;

  if (inclusive & START) {
    start_inclusive = true;
  } else if (inclusive & END) {
    end_inclusive = true;
  } else if (inclusive & INCLUSIVE) {
    start_inclusive = true;
    end_inclusive = true;
  }

  return (start_inclusive ? addr >= start : addr > start)
    && (end_inclusive ? addr <= end : addr < end);
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

int64_t Range::distanceFromRange(Address addr) {
  if ((withinRange(addr, INCLUSIVE))) {
    return 0;
  } else if (addr < start) {
    return (int64_t) addr - (int64_t) start;
  } else {
    return (int64_t) addr - (int64_t) end;
  }
}

bool Range::overlapsWith(Range r, uint8_t inclusive) {
  if (withinRange(r.start, inclusive) ||
      withinRange(r.end, inclusive) || 
      (start < r.start && end > r.end) ||
      (start > r.start && end < r.end)) {
    return true;
  } 
  return false;
}

void Range::unionRange(const Range r) {
  this->start = (this->start <= r.start) ? this->start : r.start;
  this->end = (this->end >= r.start) ? this->end : r.end;
}

int64_t Range::size() {
  return (this->end - this->start);
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
  return (*this < r) || (*this == r) ? true : false;
}

bool Range::operator>=(const Range& r) {
  return (*this > r) || (*this == r) ? true : false;
}

} // End liteprobes 
} // End liteinst 
