
#include "histogram.hpp"

using namespace std;

void Histogram::addItem(uint64_t item) {
  uint64_t bin_index = item / bin_size;
  auto it = bins.find(bin_index);
  if (it != bins.end()) {
    it->second.count++;
  } else {
    Range r;
    r.start = bin_index * bin_size;
    r.end = r.start + bin_size;

    Bin bin;
    bin.count = 1;
    bin.bin_range = r;
    bin.index = bin_index;

    bins.insert(pair<uint64_t, Bin>(bin_index, bin));
    num_bins++;
  }

  total_items++;
}

list<Bin> Histogram::getBins() {
  list<Bin> bl; 
  for (auto it : bins) {
    it.second.contribution = (double) it.second.count / total_items;
    bl.push_back(it.second);
  }

  return bl;
}
