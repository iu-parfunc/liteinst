
#include <unistd.h> // sysconf
#include <sys/mman.h>

#include <string>
#include <array>
#include <list>
#include <algorithm>
#include <cassert>
#include <vector>
#include <set>
#include <unordered_map>

#include "alloc.hpp"
#include "fixed_alloc.hpp"
#include "range.hpp"
#include "defs.hpp"
#include "process.hpp"

namespace utils {
namespace alloc {

using namespace utils::process;
using namespace utils::range;

using std::string;
using std::list;
using std::array;
using std::vector;
using utils::Address;
using std::sort;
using std::pair;
using std::set;
using std::unordered_map;

const array<uint8_t, 14> invalid_opcodes = {0x06, 0x07, 0x0E, 0x16, 0x17,
          0x1E, 0x1F, 0x27, 0x2F, 0x37, 0x3F, 0x60, 0x61, 0x62};

BlockRangeMap FixedAllocator::allocations(sysconf(_SC_PAGESIZE));
unordered_map<Address, set<BlockEntry*>> FixedAllocator::page_map; 
int64_t FixedAllocator::n_allocations = 0;
long FixedAllocator::zone_size = sysconf(_SC_PAGESIZE);

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

const int A = 0;
const int B = 1;
const int C = 2;
const int D = 3;
const int E = 4;

list<Range> getUnoccupiedRangesWithSize(Range r, 
    set<Range, RangeSort> ranges, int size) {
  list<Range> unoccupied;
  Address ptr = r.start;
  for (Range range : ranges) {
    if (ptr < r.end && ptr < range.start) {
      if (range.start - ptr >= size) {
        unoccupied.push_back(Range(ptr, range.start-1));
      } 
      ptr = range.end;
    }
  }

  return unoccupied;
}

Address FixedAllocator::findAllocationWithinExisting(Address from, 
    set<BlockEntry*>& entries, enum Constraints c[], int32_t size) {
  // printf("Allocating for : %p\n", from);

  if (from == (Address) 0x42e987) {
    printf("DEBUG me..\n");
  }

  for (BlockEntry* be : entries) {
    // printf("[BLOCK] Trying block [%p - %p]\n", be->entry_range.start, 
    //    be->entry_range.end);
    list<Range> unoccupied = getUnoccupiedRangesWithSize(be->entry_range, 
        ((PageMetaData*)be->metadata)->occupied, size);
    Range r = be->entry_range;
    int32_t displacement = (int32_t) (r.start - from);
    Address displacement_ptr = (Address) &displacement;
    int32_t new_disp = 0;
    Address new_disp_ptr = (Address) &new_disp;
    new_disp_ptr[2] = ((uint8_t*)&((PageMetaData*) be->metadata)->prefix)[0];
    new_disp_ptr[3] = ((uint8_t*)&((PageMetaData*) be->metadata)->prefix)[1];

    if (c[B] == Constraints::UNCONSTRAINED  && 
        c[C] == Constraints::UNCONSTRAINED) {
      for (Range u : unoccupied) {
        int64_t diff = (int64_t) (u.start - (from + new_disp));
        if (u.includes(from + new_disp, from + new_disp + size)) {
           return getAllocation(from + new_disp, size);
        } else {
          if (diff > 0 && diff < (1LL << 16)) {
            new_disp += diff;
            if ((new_disp_ptr[2] == 
                ((uint8_t*)&((PageMetaData*) be->metadata)->prefix)[0]) &&
                (new_disp_ptr[3] ==  
                ((uint8_t*)&((PageMetaData*) be->metadata)->prefix)[1])) {
              Address addr = getAllocation(from + new_disp, size);
              if (addr) {
                return addr;
              }
            }
            // disp_ptr[0] = ((uint8_t*)(&diff))[0];
            // disp_ptr[1] = ((uint8_t*)(&diff))[1];
          } 
        }
      }
    } else if (c[B] == Constraints::ILLOP && 
        c[C] == Constraints::UNCONSTRAINED) {
      for (unsigned int i=0; i < invalid_opcodes.size(); i++) {
        new_disp_ptr[0] = invalid_opcodes[i];
        for (int j=0; j < 16; j++) {
          new_disp_ptr[1]  = (uint8_t) j;
          Address new_addr = from + new_disp;
          Range r(new_addr, new_addr + size); 
          // printf("\nTrying address %p with opcode %X\n", new_addr,
          //  invalid_opcodes[i]);
          for (Range u : unoccupied) {
            if (u.start > new_addr) {
              break;
            }

            if (u.includes(new_addr, new_addr + size)) {
              // printf(" Selected unoccupied range [%p - %p]\n", 
              //    u.start, u.end);
              return getAllocation(new_addr, size);
            }

            // printf(" Failed unoccupied range [%p - %p]\n", 
            //    u.start, u.end);
          }
        }
      }
    } else if (c[B] == Constraints::UNCONSTRAINED &&
        c[C] == Constraints::ILLOP) {
      for (unsigned int i=0; i < invalid_opcodes.size(); i++) {
        new_disp_ptr[1] = invalid_opcodes[i];
        for (Range u : unoccupied) {
          // printf("\nTrying address %p with opcode %X\n", u.start, invalid_opcodes[i]);
          Address new_addr = from + new_disp;
          if (u.start > new_addr) {
            break;
          }

          if (u.includes(new_addr, new_addr + size)) {
            // printf(" Selected unoccupied range [%p - %p]\n", 
            //    u.start, u.end);
            return getAllocation(new_addr, size);
          }

          // printf(" Failed unoccupied range [%p - %p]\n", 
          //     u.start, u.end); 

          /*
          int64_t diff = (int64_t) (u.start - (from + new_disp));
          assert(diff < (1LL << 16));
          return getAllocation(u.start, size);
          printf(" Failed unoccupied range [%p - %p]\n", 
             u.start, u.end);
             */
        }
      }
    } else {
      for (unsigned int i=0; i < invalid_opcodes.size(); i++) {
        for (unsigned int j=0; j < invalid_opcodes.size(); j++) {
          new_disp_ptr[0] = invalid_opcodes[i];
          new_disp_ptr[1] = invalid_opcodes[j];
          Address new_addr = from + new_disp;
          Range r(new_addr, new_addr + size); 
          for (Range u : unoccupied) {
            if (u.includes(new_addr, new_addr + size)) {
              return getAllocation(new_addr, size);
            }
          }
        }
      }
    }
  }

  return nullptr;
}

bool FixedAllocator::ifAllocatableRange(Address to, int alloc_sz) {

  Process p;
  Range stack = p.getStack();
  Range heap = p.getHeap();
  Range text = p.getText();

  Address start = to - (long) to % zone_size; // Rounding to first zone start
  Address end   = to + alloc_sz + (zone_size - 
    ((long) to + alloc_sz) % zone_size); // Rounding up to next zone
  Range r(start, end);
  if ((int64_t) start > 0 && start > text.end) {
    if (!(stack.overlapsWith(r, Range::INCLUSIVE)
      || heap.overlapsWith(r, Range::INCLUSIVE))) {
      return true;
    }
  }

  return false;
}


Address FixedAllocator::allocate(Address to, Address page, int alloc_sz) {
  if (ifAllocatableRange(to, alloc_sz)) {
    Address allocd = getAllocationFor(to, page, alloc_sz);
    if (allocd) {
      assert(allocd <= to);
      assert(allocd + zone_size > to);
      return allocd;
    } 
  }
  return nullptr;
}

int g_fail_count = 0;
int g_new_fails  = 0;
int g_existing_fails = 0;

Address FixedAllocator::newAllocationForPage(Address page, Address from, 
    enum Constraints c[], int size) {

  long offset    = (long) from % zone_size;
  int alloc_sz   = size;
  for (unsigned int i=0; i < invalid_opcodes.size(); i++) {
    for (int j=(int)invalid_opcodes.size() - 1; j >= 0; j--) {
      int64_t displacement      = 0xFFFF0FFF; // This is a special default value
      // int64_t displacement      = 0; 
      Address disp_ptr          = (Address) &displacement;
      disp_ptr[2]               = invalid_opcodes[i]; 
      disp_ptr[3]               = invalid_opcodes[j];

      Address to                = nullptr;
      switch (c[B]) {
        case Constraints::UNCONSTRAINED:
        {
          if (c[C] == Constraints::UNCONSTRAINED) {

            to             = from + displacement;
            Address addr   = allocate(to, page, alloc_sz);
            if (addr) {
              return addr;
            }
          } else {
            for (unsigned int i = 0; i < invalid_opcodes.size(); i++) {

              disp_ptr[1]  = invalid_opcodes[i]; 
              to           = from + displacement;
              Address addr = allocate(to, page, alloc_sz);
              if (addr) {
                return addr;
              }
            }
          }
          break;
        }
        case Constraints::ILLOP:
        {
          if (c[C] == Constraints::UNCONSTRAINED) {

            disp_ptr[0]    = invalid_opcodes[i];
            for (unsigned int i = 0; i < 16; i++) {

              disp_ptr[1]  = (uint8_t) i; // Vary the high nibble of C 
              to           = from + displacement;
              Address addr = allocate(to, page, alloc_sz);
              if (addr) {
                return addr;
              }
            }
          } else {

            disp_ptr[0]    = invalid_opcodes[0];
            for (unsigned int i = 0; i < invalid_opcodes.size(); i--) {

              disp_ptr[1]  = invalid_opcodes[i];
              to           = from + displacement;
              Address addr = allocate(to, page, alloc_sz);
              if (addr) {
                return addr;
              }
            }
         }
         break;
       }
       default:
        assert(false);
      }
    }
  }

  return nullptr;
}

Address FixedAllocator::searchAndAllocate(Address addr, enum Constraints c[],
    int32_t size) {
  Address from = addr + 5;
  Address page = from - (uint64_t) from % zone_size; 
  set<BlockEntry*> entries;
  set<BlockEntry*> prev_entries;
  bool entries_present = false;
  bool prev_entries_present = false;
  
  // Look for allocations for previous page
  auto it = page_map.find(page);
  if (it != page_map.end()) {
    entries = it->second;
    entries_present = true;
  }

  it = page_map.find(page - zone_size); 
  if (it != page_map.end()) {
    prev_entries = it->second;
    prev_entries_present = true;
  } 

  if (addr == (Address) 0x40a9f0) {
    printf("DEBUG ME..\n");
  }

  // No allocations for current or the previous page exists. Allocate for the
  // current page
  Address alloc = nullptr;
  if (!entries_present  && !prev_entries_present) {
    // printf("0: BRAND NEW ALLOCATION..\n");
    alloc = newAllocationForPage(page, from, c, size);
    if (!alloc) {
      g_new_fails++;
    }
  } else {
    if (entries_present) {
      alloc = findAllocationWithinExisting(from, entries, c, size);
      // Cannot find availble space for allocations for the current page. Try
      // allocations for previous page.
      if (alloc == nullptr) {
        g_existing_fails++;
        // printf("EXISTING CURRENT FAILED..\n");
        alloc = findAllocationWithinExisting(from, prev_entries, c, size);
        // Cannot find space within previous page allocations either. Try
        // allocating new for the current page
        if (alloc == nullptr) {
          g_existing_fails++;
          // printf("0: EXISTING PREVIOUS FAILED\n");
          // printf("0: NEW ALLOCATION\n");
          alloc = newAllocationForPage(page, from, c, size);
          if (!alloc) {
            g_new_fails++;
          }
        } 
      }
    } else if (prev_entries_present) {

      alloc = findAllocationWithinExisting(from, prev_entries, c, size);
      if (alloc == nullptr) {
        g_existing_fails++;
        // printf("1: EXISTING PREVIOUS FAILED\n");
        // printf("1: NEW ALLOCATION\n");
        alloc = newAllocationForPage(page, from, c, size);
        if (!alloc) {
          g_new_fails++;
        }
      }
    } else {
      // printf("1: BRAND NEW ALLOCATION..\n");
      alloc = newAllocationForPage(page, from, c, size);
      if (!alloc) {
        g_new_fails++;
      }
    }
  }

  if (alloc == nullptr) {
    g_fail_count++;
    printf("SERACH AND ALLOCATE FAILED..\n");
    printf("Current failure count : %lu\n\n", g_fail_count);
  }

  return alloc;
}

Address FixedAllocator::getAllocation(Address at, int32_t size) {
  return getAllocationFor(at, nullptr, size);
}

Address FixedAllocator::getAllocationFor(Address at, Address for_page,  
    int32_t size) {
  /*
     if (!callback_inited) {
     initialize();
     }*/

  bool success = false;
  if (for_page) {
    FixedAllocMetaData* m = new FixedAllocMetaData;
    m->for_page = for_page;
    int32_t displacement = at - for_page;
    ((uint8_t*)&m->prefix)[0] = ((uint8_t*) &displacement)[2];
    ((uint8_t*)&m->prefix)[1] = ((uint8_t*) &displacement)[3];

    Range r = Range(at, at+size);
    success = allocations.updateRangeEntries(
        r, m, [this](vector<BlockEntry*> entries, Range range, RangeMetaData* m) 
        { return this->allocationCallback(entries, range, m); });
    delete m;
  } else {
    Range r = Range(at, at+size);
    success = allocations.updateRangeEntries(
        r, nullptr, [this](vector<BlockEntry*> entries, Range range, 
          RangeMetaData* m) 
        { return this->allocationCallback(entries, range, m); });
  }

#ifdef AUDIT
  if (success) {
    n_allocations++;
  }
#endif

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
    Range range, RangeMetaData* m) {

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
    if (be->entry_range.overlapsWith(block, Range::START)) {
      PageMetaData* meta = (PageMetaData*) be->metadata;

      if (meta != NULL && meta->allocated) {
        // Now check the occupied sub ranges within this block
        for (Range occupiedRange : meta->occupied ) {
          // If any of the occupied sub ranges overlaps with this part of the
          // range that we want to allocate we cannot do this allocation since
          // it would conflict with an existing allocation. Fail fast.
          if (occupiedRange.overlapsWith(block, Range::START)) {
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

      if (m) {
        // Page mapping was successful. Initialize and add page meta data entry.
        meta = new PageMetaData;
        meta->allocated = true;
        meta->prefix = ((FixedAllocMetaData*) m)->prefix; 
        be->metadata = meta;
      } 
    }
  }

  if (m) {
    // Finally, add allocation meta data to page meta data entries.
    auto it = page_map.find(((FixedAllocMetaData*) m)->for_page);
    if (it == page_map.end()) {
      set<BlockEntry*> s;
      page_map.insert(pair<Address, set<BlockEntry*>>(
            ((FixedAllocMetaData*) m)->for_page, s));
    }    

    set<BlockEntry*>& st = page_map[((FixedAllocMetaData*) m)->for_page];

    for (unsigned int i=0; i < blocks.size(); i++) {
      BlockEntry* be = entries[i];
      PageMetaData* meta = (PageMetaData*) be->metadata;

      // Add page map meta data
      st.insert(be);

      if (blocks[i].overlapsWith(be->entry_range, Range::INCLUSIVE)) {
        meta->occupied.insert(blocks[i]);
      } else {
        assert(false);
      }
    }
  } else {
    for (unsigned int i=0; i < blocks.size(); i++) {
      BlockEntry* be = entries[i];
      PageMetaData* meta = (PageMetaData*) be->metadata;

      if (blocks[i].overlapsWith(be->entry_range, Range::INCLUSIVE)) {
        meta->occupied.insert(blocks[i]);
      } else {
        assert(false);
      }
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

#ifdef AUDIT
  BlockEntries entries = allocations.getEntries();
  int32_t block_size = allocations.getBlockSize();
  int64_t total_alloc_sz = 0;
  int64_t occupied_sz = 0;
  int64_t pages = 0;
  for (auto it : entries) {
    BlockEntry* be = it.second;
    PageMetaData* meta = (PageMetaData*) be->metadata;
    if (meta != nullptr && meta->allocated) {
      for (Range r : meta->occupied) {
        occupied_sz += (r.end - r.start);
      }
      total_alloc_sz += block_size;
      pages++;
    }
  }

  mem.n_pages = pages;
  mem.kbs = total_alloc_sz / 1024;
  mem.allocations = n_allocations;
  if (total_alloc_sz > 0) {
    mem.utilization = (double) occupied_sz / total_alloc_sz;
  } else {
    mem.utilization = 0;
  }

  printf("FAIL COUNT : %d\n", g_fail_count);
  printf("NEW FAILS  : %d\n", g_new_fails);
  printf("EXISTING FAILS  : %d\n", g_existing_fails);
#endif

  return mem;
}

} // End alloc 
} // End liteinst 
