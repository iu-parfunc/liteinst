
#include <cstdint>
#include <string>
#include <iostream>

#include "concurrency.hpp"

using std::to_string;

class RangeKey {
  public:
    int start;
    int end; 

    static const uint8_t START = 0x01;
    static const uint8_t END = 0x02;
    static const uint8_t EXCLUSIVE = 0x00;
    static const uint8_t INCLUSIVE = 0x04;

    RangeKey(int start, int end) : start(start), end(end) {

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

    bool overlapsWith(const RangeKey& r) const {
      if (r.withinRange(start, INCLUSIVE) ||
          r.withinRange(end, INCLUSIVE) || 
          (start < r.start && end > r.end)) {
        return true;
      } 
      return false;
    }

};

class RangeKeyComparator {
  public:
    bool operator()(const RangeKey& a, const RangeKey& b) const {
      if (a.overlapsWith(b)) {
        return true;
      } else {
        return a.start < b.start;
      }
    }
};

class Key {
  public:
    int start;
    int end;

    static const uint8_t START = 0x01;
    static const uint8_t END = 0x02;
    static const uint8_t EXCLUSIVE = 0x00;
    static const uint8_t INCLUSIVE = 0x04;


    Key() {

    }

    Key(int start_addr, int end_addr) : start(start_addr), 
    end(end_addr) {

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

    bool overlapsWith(const Key& r) const {
      if (r.withinRange(start, INCLUSIVE) ||
          r.withinRange(end, INCLUSIVE) || 
          (start < r.start && end > r.end)) {
        return true;
      } 
      return false;
    }

    bool operator<(const Key& r) const {
      return (this->end < r.start) ? true : false;
    }

    bool operator>(const Key& r) const {
      return (this->start > r.end) ? true : false;
    }

    bool operator==(const Key& r) const {
      return (this->start == r.start && this->end == r.end) ? true : false;
    }

    bool operator!=(const Key& r) const {
      return !(*this == r) ? true : false;
    }

    bool operator<=(const Key& r) const {
      return (*this < r) || (*this == r) ? true : false;
    }

    bool operator>=(const Key& r) const {
      return (*this > r) || (*this == r) ? true : false;
    }
};

class Value {
  public:
    int value;

    Value() {

    }

    Value(int val) : value(val) {
    }
};

class Comparator {
  public:
    bool operator()(const Key& a, const Key& b) const {
      if (a == b || a.overlapsWith(b)) {
        return true;
      } else {
        return a < b; 
      }
    }
};


