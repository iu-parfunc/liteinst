
// #include "concurrent_map_test.hpp"

#include <map>
#include <iostream>

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


int main() {
  std::map<RangeKey, int, RangeKeyComparator> m;
  m.insert(std::pair<RangeKey, int>(RangeKey(1, 2), 1));
  auto it = m.find(RangeKey(1, 2));

  std::cout << it->first.start << "\n";
  std::cout << it->first.end << "\n";
  std::cout << it->second << "\n";

  return 0;
}
