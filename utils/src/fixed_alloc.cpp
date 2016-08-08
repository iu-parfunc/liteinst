
#include <unistd.h> // sysconf
#include <sys/mman.h>

#include <string>
#include <cassert>

#include "alloc.hpp"
#include "fixed_alloc.hpp"
#include "range.hpp"
#include "defs.hpp"

namespace utils {
namespace alloc {

using namespace utils::range;

using std::string;
using std::vector;
using utils::Address;

BlockRangeMap FixedAllocator::allocations(sysconf(_SC_PAGESIZE));

FixedAllocator::FixedAllocator() : Allocator() {
  // printf("CREATING FIXED ALLOCATOR.\n");
}

FixedAllocator::~FixedAllocator() {
  // printf("DESTROYING FIXED ALLOCATOR.\n");
  // allocations.show(stderr, 0);
}

/*
   void FixedAllocator::initialize() {
   callback_lock.lock();
   if (!callback_inited) {
   allocations.registerCallback(
   [this](vector<RangeEntry*> entries, int32_t size) 
   { return this->allocationCallback(entries, size); });
   callback_inited = true;
   }
   callback_lock.unlock();
   }*/

Address FixedAllocator::getAllocation(Address at, int32_t size) {
  /*
     if (!callback_inited) {
     initialize();
     }*/

  Range r = Range(at, at+size);
  bool success = allocations.updateRangeEntries(
      r, [this](vector<BlockEntry*> entries, Range range) 
      { return this->allocationCallback(entries, range); });

  return success ? at : NULL;
}

bool FixedAllocator::removeAllocation(Address address) {
  /*
     if (!callback_inited) {
     initialize();
     }*/

  return false;
  // throw std::error
}

bool FixedAllocator::allocationCallback(std::vector<BlockEntry*> entries, 
    Range range) {

  // Get the range partitioned to constituent blocks.
  vector<Range> blocks = range.getBlockedRange(sysconf(_SC_PAGESIZE), 
      false);

  // Number of blocks and the number of meta data entries about those should
  // match
  assert(blocks.size() == entries.size());

  // Since we can assume both range blocks are block meta data entries are 
  // in the increasing address order we can linearly check thorugh them.

  // Check if the blocks constituting this given range occupies are free to be
  // allocated from.
  vector<Range> toBeAllocated;
  for (unsigned int i=0; i < blocks.size(); i++) {
    Range block = blocks[i];
    BlockEntry* be = entries[i];

    // Strictly not necessary since blocks and meta should iterate in 
    // lockstep. Just a sanity check
    if (be->entry_range.overlapsWith(block, Range::INCLUSIVE)) {
      PageMetaData* meta = (PageMetaData*) be->metadata;

      if (meta != NULL && meta->allocated) {
        // Now check the occupied sub ranges within this block
        for (Range occupiedRange : meta->occupied ) {
          // If any of the occupied sub ranges overlaps with this part of the
          // range that we want to allocate we cannot do this allocation since
          // it would conflict with an existing allocation. Fail fast.
          if (occupiedRange.overlapsWith(block, Range::INCLUSIVE)) {
            return false;
          }
        }
      } else {
        // This page is yet to be mapped. Deal with it later when we are  
        // doing the allocations.
      }
    } else {
      assert(false);
    }
  }

  // Now handle mapping of non existent pages. This is done first since if
  // any one of those fails we need to fail fast without adding allocation 
  // meta data for already existing page entries.
  for (unsigned int i=0; i < blocks.size(); i++) {
    BlockEntry* be = entries[i];
    PageMetaData* meta = (PageMetaData*) be->metadata;

    if (meta == NULL || !meta->allocated) {
      // The page is yet to be mapped. Map it now.
      Address addr = (Address) mmap(be->entry_range.start, 
          (be->entry_range.end - be->entry_range.start),
          PROT_READ | PROT_WRITE | PROT_EXEC, 
          MAP_PRIVATE | MAP_ANONYMOUS, -1,0);

      if (addr == MAP_FAILED) {
        // perror(string("[mmap]").c_str());
        /*
        fprintf(stderr, "[mmap] Failed allocating range at : [%p-%p]\n", 
            be->entry_range.start, be->entry_range.end); 
            */
        return false;
      } else {
        if (addr != be->entry_range.start) {
          /*
          fprintf(stderr, "[mmap] Got different stub address requested: %p  " 
              " recieved : %p\n", be->entry_range.start, addr);
              */

          // Rollback. 
          int ret = munmap(addr, 
              (be->entry_range.end - be->entry_range.end));
          if (ret == -1) {
            // perror(string("[munmap]").c_str());
            /*
            fprintf(stderr, "[munmap] Failed rolling back faulty allocation "
                "at : [%p-%p]\n", be->entry_range.start, be->entry_range.end);
                */
          }
          return false;
        }
      }

      // Page mapping was successful. Initialize and add page meta data entry.
      meta = new PageMetaData;
      meta->allocated = true;
      be->metadata = meta;
    }
  }

  // Finally, add allocation meta data to page meta data entries.
  for (unsigned int i=0; i < blocks.size(); i++) {
    BlockEntry* be = entries[i];
    PageMetaData* meta = (PageMetaData*) be->metadata;

    if (blocks[i].overlapsWith(be->entry_range, Range::INCLUSIVE)) {
      meta->occupied.push_back(blocks[i]);
    } else {
      assert(false);
    }
  }

  return true;
}

void FixedAllocator::show(FILE* fp, int nspaces) {
  allocations.show(stderr, nspaces);
}

MemStatistics FixedAllocator::getAllocationStatistics() {
  BlockStatistics stats = allocations.getBlockStatistics();

  MemStatistics mem;
  mem.n_pages = stats.n_blocks;
  mem.kbs = stats.kbs;

  return mem;
}

} // End alloc 
} // End liteinst 
