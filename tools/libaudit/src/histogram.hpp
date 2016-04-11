
#include "math.h"

#include <cstdint>
#include <map>
#include <list>

typedef struct {
  uint64_t start;
  uint64_t end;
} Range;

typedef struct {
  uint64_t index;
  Range bin_range;
  uint64_t count;
  double contribution; // # of items in this bin out of total number of items 
                       // in all bins (as a %)
} Bin;

// TODO: Make this a template class
class Histogram {

  private:
    Range range;
    uint64_t bin_size;
    uint64_t num_bins;
    uint64_t total_items;

    std::map<uint64_t, Bin> bins;

  public:

    Histogram(uint64_t range_start, uint64_t range_end, uint64_t size) { 
      range.start = range_start;
      range.end = range_end;
      bin_size = size;

      num_bins = (uint64_t)ceil(((double) (range_end - range_start)) / size);
    }

    Histogram(uint64_t range_start, uint64_t size) { 
      range.start = range_start;
      bin_size = size;

      num_bins = 0;
    }

    void addItem(uint64_t item);

    std::list<Bin> getBins();

    uint64_t getOccupiedBins() {
      return bins.size();
    }

    uint64_t getNumberOfBins() {
      return num_bins;
    }

};
