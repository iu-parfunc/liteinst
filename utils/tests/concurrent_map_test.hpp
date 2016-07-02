
#include <cstdint>
#include <string>
#include <iostream>

#include "concurrency.hpp"

using std::to_string;

class RangeKey {
  public:
    int start;
    int end; 

    RangeKey(int start, int end) : start(start), end(end) {

    }

    bool withinRange(int value, bool inclusive) const {
      if (inclusive) {
        return (value >= start && value <= end);
      } else {
        return (value > start && value < end);
      }
    }

    bool overlapsWith(const RangeKey& r) const {
      if (r.withinRange(start, true) ||
          r.withinRange(end, true) || 
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

    Key() {

    }

    Key(int start_addr, int end_addr) : start(start_addr), 
      end(end_addr) {
  
    }

    bool withinRange(int addr, bool inclusive) const {
      if (inclusive) {
        return (addr >= start && addr <= end);
      } else {
        return (addr > start && addr < end);
      }
    }

    bool overlapsWith(const Key& r) const {
      if (r.withinRange(start, true) ||
          r.withinRange(end, true) || 
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


