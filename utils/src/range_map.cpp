
#include <string>
#include <cstdio>
#include <unordered_set>
#include <cassert>

#include "range_map.hpp"

namespace utils { 
namespace range {

using std::string;
using std::vector;
using std::pair;
using utils::Address;
using std::unordered_set;

// RangeEntries RangeMap::entries;
// UpdateEntriesCallback RangeMap::cb;

BlockRangeMap::BlockRangeMap(int32_t block_size) : block_size(block_size), 
  entries() { 
  // fprintf(stderr, "[rangemap] BLOCK SIZE : %d\n", block_size);
}

void BlockRangeMap::show(FILE* fp, int nspaces) {
  unordered_set<Range, RangeHash> ranges;
  for (auto it : entries) {
    ranges.insert(it.second->entry_range);
  }

  string left_pad = getPadding(nspaces);
  fprintf(fp, "%sBlock size : %ld\n", left_pad.c_str(), block_size);
  fprintf(fp, "%sEntries : %ld\n", left_pad.c_str(), entries.size());
  fprintf(fp, "%sBlocks : %ld\n", left_pad.c_str(), ranges.size());
  fprintf(fp, "%sAllocted : %ld KB\n", left_pad.c_str(), ranges.size() * 
      block_size / 1024);
}

BlockStatistics BlockRangeMap::getBlockStatistics() {
  unordered_set<Range, RangeHash> ranges;
  for (auto it : entries) {
    ranges.insert(it.second->entry_range);
  }

  BlockStatistics stats;
  stats.n_entries = entries.size();
  stats.n_blocks = ranges.size();
  stats.kbs = ranges.size() * block_size / 1024;

  return stats;
}

int32_t BlockRangeMap::getBlockSize() {
  return block_size;
}

BlockEntries BlockRangeMap::getEntries() {
  return entries;
}

BlockRangeMap::~BlockRangeMap() {
  for (auto it : entries) {
    delete it.second;
  }
  // Let RAII take care of the map itself
}

bool BlockRangeMap::updateRangeEntries(Range r, RangeMetaData* m, 
    UpdateEntriesCallback cb) {
  vector<BlockEntry*> block_entries = lockRange(r);
  bool result = false;
  if (block_entries.size() > 0) {
    result = cb(block_entries, r, m);
  }

  bool unlocked = unlockRange(r);
  if (!unlocked) {
    // throw std::runtime_error;
  }

  return result;
  // return (block_entries.size() > 0) ? true : false;
}   

std::vector<BlockRange> BlockRangeMap::getBlockedRangeMetaData(Range r) {
  vector<Range> partitions = r.getBlockedRange(block_size, true);

  vector<BlockRange> blocks;
  blocks.reserve(partitions.size());
  for (Range partition : partitions) {
    BlockRange rb;
    rb.range = partition;
    rb.entry_present = false;

    blocks.push_back(rb);
  }

  return blocks;
}

vector<BlockEntry*> BlockRangeMap::lockRange(Range r) {
  std::vector<BlockRange> range_blocks = getBlockedRangeMetaData(r);

  assert(range_blocks.size() > 0);

  // Find the non existent range blocks corresponding to the given range
  // We are using the ordered nature of stl map to avoid multiple finds here.
  auto it = entries.lower_bound(range_blocks[0].range.start);
  uint32_t block_ptr = 0;
  for (; it != entries.end(); it++) {
    BlockEntry* entry = it->second;
    while (block_ptr < range_blocks.size() && range_blocks[block_ptr].range != entry->entry_range ) {
      block_ptr++;
    }

    if (block_ptr < range_blocks.size()) {
      range_blocks[block_ptr].entry_present = true;
    } else {
      break;
    }
  }

  // Now try to lock the range also adding non existent range entry blocks 
  // corresponding to the given range.
  // Note that we are acquiring range locks in the increasing order of range
  // blocks within the range. This is so that we can release those range 
  // locks in order of acquisition when releasing them. This is important for 
  // avoiding deadlocks.
  vector<BlockEntry*> block_entries;
  for (BlockRange rb : range_blocks) {
    if (!rb.entry_present) {
      global_lock.lock();
      auto it = entries.find(rb.range.start);
      if (it != entries.end()) {
        // Somebody else sneaked in on us and inserted the entry. Try to grab
        // the lock of this newly added entry. 
        global_lock.unlock();
        it->second->lock.lock();
        block_entries.push_back(it->second);
      } else {
        BlockEntry * re = new BlockEntry;
        re->entry_range = rb.range;
        re->lock.lock();
        re->metadata = NULL;

        entries.insert(pair<Address, BlockEntry*>(rb.range.start, re));
        global_lock.unlock();
        block_entries.push_back(re);
      }
    } else {
      auto it = entries.find(rb.range.start);
      if (it != entries.end()) {
        it->second->lock.lock();
        block_entries.push_back(it->second);
      } else { 
        // This is wierd. Entry just vanished after we last checked. 
        // This cannot happen since an entry is not deleted once we insert 
        // it.
        // throw int;
      }
    }
  } 

  return block_entries;
}

bool BlockRangeMap::unlockRange(Range r) {
  std::vector<BlockRange> range_blocks = getBlockedRangeMetaData(r);

  assert(range_blocks.size() > 0);

  BlockRange last_block = range_blocks.back();

  // Global lock is unnecesary here since all the range block entries are 
  // present due to the prior invocation of lockRange. Hence calling 
  // unlockRange prior to lockRange for a given range would result
  // in an undefined behavior. Releasing locks in the order of acquisition to
  // avoid deadlocks.
  auto it = entries.lower_bound(range_blocks[0].range.start);
  for (; it != entries.end(); ++it) {
    BlockEntry* entry = it->second;
    entry->lock.unlock();

    if (entry->entry_range == last_block.range) {
      break;
    }
  }

  return true;
}

} // End liteprobes 
} // End liteinst 

