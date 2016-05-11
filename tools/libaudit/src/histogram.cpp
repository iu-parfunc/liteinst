
#include "assert.h"
#include "histogram.hpp"

using namespace std;

void Histogram::addItem(uint64_t item) {

  uint64_t bin_index = item / bin_size;
  auto it = bins.find(bin_index);

  if (it != bins.end()) {
    it->second->count++;
  } else {
    BinRange r;
    r.start = bin_index * bin_size;
    r.end = r.start + bin_size;

    Bin* bin = new Bin;
    bin->count = 1;
    bin->bin_range = r;
    bin->index = bin_index;

    bins.insert(pair<uint64_t, Bin*>(bin_index, bin));
    num_bins++;
  }

  total_items++;
  if (item == 0) {
    zero_count++;
  }

}

/*
void Histogram::addItem(uint64_t item) {
  uint64_t prev_count = 0;
  uint64_t prev_total_count = total_items;
  for (auto it: bins) {
    prev_count += it.second->count;
  }

  if (item == 0 && zeroAbin) {
    auto it = bins.find(0);
    if (it != bins.end()) {
      it->second->count++;
    } else {
      Range r;
      r.start = 0;
      r.end = 1;

      Bin* bin = new Bin;
      bin->count = 1;
      bin->bin_range = r;

      bins.insert(pair<uint64_t, Bin*>(0, bin));
      num_bins++;
    }

    total_items++;

    uint64_t cur_count = 0;
    for (auto it: bins) {
      cur_count += it.second->count;
    }

    assert(cur_count == prev_count + 1);
    assert(total_items == prev_total_count + 1);

    return;
  } 

  uint64_t bin_index = item / bin_size;
  map<uint64_t, Bin*>::iterator it;
  if (zeroAbin && bin_size > 1) {
    bin_index++;
    it = bins.find(bin_index);
  } else {
    it = bins.find(bin_index);
  }

  if (it != bins.end()) {
    it->second->count++;
  } else {
    Range r;
    if (bin_size > 1) {
      if (zeroAbin && bin_index == 1) { 
        r.start = (bin_index - 1) * bin_size + 1;
        r.end = r.start + bin_size - 1;
      } else if (zeroAbin && bin_index > 1) {
        r.start = (bin_index - 1) * bin_size;
        r.end = r.start + bin_size;
      } else {
        r.start = bin_index * bin_size;
        r.end = r.start + bin_size;
      }
    } else {
      r.start = bin_index * bin_size;
      r.end = r.start + bin_size;
    }

    Bin* bin = new Bin;
    bin->count = 1;
    bin->bin_range = r;
    bin->index = bin_index;

    bins.insert(pair<uint64_t, Bin*>(bin_index, bin));
    num_bins++;
  }

  total_items++;

  uint64_t cur_count = 0;
  for (auto it: bins) {
    cur_count += it.second->count;
  }

  assert(cur_count == prev_count + 1);
  assert(prev_total_count + 1 == total_items);

}
*/

list<Bin*> Histogram::getBins() {
  list<Bin*> bl; 
  for (auto it: bins) {
    it.second->contribution = ((double) it.second->count / total_items) * 100;
    bl.push_back(it.second);
  }

  return bl;
}
