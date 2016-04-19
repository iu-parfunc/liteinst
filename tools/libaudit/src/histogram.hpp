
#include "math.h"
#include "defs.hpp"

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
class Histogram : public defs::Show {

  private:
    Range range;
    uint64_t bin_size;
    uint64_t num_bins;
    uint64_t total_items;
    uint64_t zero_count;
    bool zeroAbin;

    std::map<uint64_t, Bin*> bins;

  public:

    Histogram(uint64_t range_start, uint64_t range_end, uint64_t size, 
        bool isBin) { 
      range.start = range_start;
      range.end = range_end;
      bin_size = size;
      zeroAbin = isBin; 
      total_items = 0;

      num_bins = (uint64_t)ceil(((double) (range_end - range_start)) / size); 

      /*
      if (zeroAbin && bin_size > 1) {
        Range r;
        r.start = 0;
        r.end = 0;

        uint64_t bin_index = 0;
        Bin* bin = new Bin;
        bin->count = 1;
        bin->bin_range = r;
        bin->index = bin_index;

        bins.insert(std::pair<uint64_t, Bin*>(bin_index, bin));

        num_bins = (uint64_t)ceil(((double) (range_end - range_start)) / size) 
        + 1;
      } else {
        num_bins = (uint64_t)ceil(((double) (range_end - range_start)) / size); 
      }
      */
    }

    Histogram(uint64_t range_start, uint64_t size, bool isBin) { 
      range.start = range_start;
      bin_size = size;
      zeroAbin = isBin;
      total_items = 0;

      /*
      if (zeroAbin && bin_size > 1) {
        Range r;
        r.start = 0;
        r.end = 1;

        uint64_t bin_index = 0;
        Bin* bin = new Bin;
        bin->count = 1;
        bin->bin_range = r;
        bin->index = bin_index;

        bins.insert(std::pair<uint64_t, Bin*>(bin_index, bin));

        num_bins = 1;
      } else {
        num_bins = 0;
      }
      */
    }

    void addItem(uint64_t item);

    std::list<Bin*> getBins();

    uint64_t getOccupiedBins() {
      return bins.size();
    }

    uint64_t getNumberOfBins() {
      return num_bins;
    }

    void show(FILE* fp, int nspaces) {
      std::string left_pad = getPadding(nspaces);
      int PRINT_PRECISION = 2;
      std::list<Bin*> bins = getBins();
      for (Bin* bin : bins) {
        fprintf(fp, "%s[%2lu-%2lu) ", left_pad.c_str(), bin->bin_range.start,
            bin->bin_range.end);
        uint64_t num_bars = (uint64_t) ceil(bin->contribution/ PRINT_PRECISION);
        for (uint64_t j = 0; j < num_bars; j++) {
          fprintf(fp, "@");
        }

        // Max num bars is 50 with PRINT_PRECISION 2. i.e. one bar represents 2%
        // of the total
        int64_t padding = 100/PRINT_PRECISION - num_bars; 
        if (padding > 0) {
          for (int64_t j = 0; j < padding; j++) {
            fprintf(fp, " ");
          }
        }

        fprintf(fp, " %.2f\n", bin->contribution);
      } 

      if (zero_count > 0) {
        fprintf(fp, "\n%sZero valued items : %lu/%lu (%.2f%%)\n", 
            left_pad.c_str(), zero_count, total_items, 
            (double) zero_count / total_items * 100); 
      }
    }


};
