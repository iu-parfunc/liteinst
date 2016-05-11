
#include <cstdio>
#include <cassert>

#include "range_map.hpp"

namespace range {

  using namespace defs;

  using std::vector;
  using std::pair;

  // RangeEntries RangeMap::entries;
  // UpdateEntriesCallback RangeMap::cb;

  RangeMap::RangeMap(int32_t block_size) : entries() { 
  }

  RangeMap::~RangeMap() {
    for (auto it : entries) {
      delete it.second;
    }
    // Let RAII take care of the map itself
  }

  bool RangeMap::updateRangeEntries(Range r, UpdateEntriesCallback cb) {
    vector<RangeEntry*> block_entries = lockRange(r);
    if (block_entries.size() > 0) {
      cb(block_entries, r);
    }

    bool unlocked = unlockRange(r);
    if (!unlocked) {
      // throw std::runtime_error;
    }

    return (block_entries.size() > 0) ? true : false;
  }   

  std::vector<RangeBlock> RangeMap::getRangeBlocks(Range r) {
    vector<Range> partitions = r.getRangePartitions(block_size, true);

    vector<RangeBlock> blocks;
    for (Range partition : partitions) {
      RangeBlock rb;
      rb.range = partition;;

      blocks.push_back(rb);
    }

    return blocks;
  }

  vector<RangeEntry*> RangeMap::lockRange(Range r) {
    std::vector<RangeBlock> range_blocks = getRangeBlocks(r);

    assert(range_blocks.size() > 0);

    // Find the non existent range blocks corresponding to the given range
    // We are using the ordered nature of stl map to avoid multiple finds here.
    auto it = entries.lower_bound(range_blocks[0].range.start);
    uint32_t block_ptr = 0;
    for (; it != entries.end(); ++it) {
      RangeEntry* entry = it->second;
      while (range_blocks[block_ptr].range != entry->entry_range &&
          block_ptr < range_blocks.size()) {
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
    vector<RangeEntry*> block_entries;
    for (RangeBlock rb : range_blocks) {
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
          RangeEntry* re = new RangeEntry;
          re->entry_range = rb.range;
          re->lock.lock();

          entries.insert(pair<Address, RangeEntry*>(rb.range.start, re));
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

  bool RangeMap::unlockRange(Range r) {
    std::vector<RangeBlock> range_blocks = getRangeBlocks(r);

    assert(range_blocks.size() > 0);

    RangeBlock last_block = range_blocks.back();

    // Global lock is unnecesary here since all the range block entries are 
    // present due to the prior invocation of lockRange. Hence calling 
    // unlockRange prior to lockRange for a given range would result
    // in an undefined behavior. Releasing locks in the order of acquisition to
    // avoid deadlocks.
    auto it = entries.lower_bound(range_blocks[0].range.start);
    for (; it != entries.end(); ++it) {
      RangeEntry* entry = it->second;
      entry->lock.unlock();

      if (entry->entry_range == last_block.range) {
        break;
      }
    }

    return true;
  }

}
