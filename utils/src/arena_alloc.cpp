
#include <unistd.h> // sysconf
#include <sys/mman.h>

#include <string>
#include <cassert>
#include <new>
#include <stdexcept> // std::invalid_argument

#include "arena_alloc.hpp"
#include "range.hpp"
#include "process.hpp"

namespace utils {
namespace alloc {

using namespace utils::range;
using namespace utils::process;

using std::string;
using std::vector;
using std::deque;
using std::shared_ptr;
using std::make_shared;
using std::invalid_argument;
using utils::Address;

utils::concurrency::ConcurrentMap<utils::Address, std::shared_ptr<ArenaPool>> 
  ArenaAllocator::pools;

#ifdef AUDIT
int64_t ArenaAllocator::allocations = 0;
int64_t ArenaAllocator::allocation_sz = 0;
#endif

/*** ArenaAllocator Implementation **/

ArenaAllocator::ArenaAllocator(int prot) : Allocator(), prot(prot) {
}

Address ArenaAllocator::getAllocation(Address addr, int32_t size) {
#ifdef AUDIT
  allocations++;
  allocation_sz += size;
#endif

  Address mem_chunk = (Address)((uint64_t)addr >> 32);
  auto it = pools.find(mem_chunk);  
  if (it != pools.end()) {
    return (*it).second->getFreeMemorySlot(size);
  } else {
    pools.acquireUpdateLock();
    it = pools.find(mem_chunk);
    if (it != pools.end()) {
      pools.releaseUpdateLock();
      return (*it).second->getFreeMemorySlot(size);
    }

    pools.emplace(mem_chunk, 
      make_shared<ArenaPool>(mem_chunk, prot));

    it = pools.find(mem_chunk);
    if (it != pools.end()) {
      pools.releaseUpdateLock();
      return (*it).second->getFreeMemorySlot(size);
    } else {
#ifdef AUDIT
      allocations--;
      allocation_sz -= size;
#endif
      pools.releaseUpdateLock();
      return NULL;
    }
  }
}

Address ArenaAllocator::getAllocationFor(Address address, 
        utils::Address for_page, int32_t size) {
  return nullptr;
}

Address ArenaAllocator::searchAndAllocate(Address from, enum Constraints c[], 
    int32_t size) {
  return nullptr;
}

bool ArenaAllocator::removeAllocation(Address address) {
  throw invalid_argument("Explicit removal of allocated memory not supported "
      "arena allocator.\n");
}

void ArenaAllocator::show(FILE* fp, int nspaces) {
  int64_t total_alloc = 0;
  for (const auto& it : pools) {
    shared_ptr<ArenaPool> pool = it.second;
    deque<shared_ptr<Arena>> arenas = pool->arenas;

    for (auto it : arenas) {
      total_alloc += it->size;
    }
  }

  string left_pad = getPadding(nspaces);
  fprintf(fp, "%sAllocated : %ld KB\n", left_pad.c_str(), total_alloc/ 1024);
  fprintf(fp, "%sPages     : %ld \n", left_pad.c_str(), total_alloc/ 
      sysconf(_SC_PAGE_SIZE)); 
}

MemStatistics ArenaAllocator::getAllocationStatistics() {
  int64_t total_alloc = 0;
  for (const auto& it : pools) {
    shared_ptr<ArenaPool> pool = it.second;
    deque<shared_ptr<Arena>> arenas = pool->arenas;

    for (auto it : arenas) {
      total_alloc += it->size;
    }
  }

  MemStatistics stats;
  stats.n_pages = total_alloc / sysconf(_SC_PAGE_SIZE);
  stats.kbs = total_alloc / 1024;
  stats.allocations = allocations;
  stats.utilization = (double) allocation_sz / total_alloc;

  return stats;
}

/** ArenaPool Implementation **/

ArenaPool::ArenaPool(Address addr, int prot) : base(addr), prot(prot) {

}

Address ArenaPool::getFreeMemorySlot(int32_t size) {
  lock.lock();
  if (arenas.size() == 0) {
    shared_ptr<Arena> arena = make_shared<Arena>(base, prot);
    arenas.push_front(arena);
  }

  if (arenas[0]->remaining_size < size) {
    shared_ptr<Arena> arena = make_shared<Arena>(base, prot);
    arenas.push_front(arena);
  }

  Address addr  = arenas[0]->alloc_ptr;
  arenas[0]->alloc_ptr += size;
  arenas[0]->remaining_size -= size;

  lock.unlock();

  return addr;
}

/** Arena Implementation **/

Arena::Arena(Address addr, int prot) {
  int num_funcs = Process().getNumberOfFunctions();
  // Allocate with an estimate of 2 trampolines per function (for function 
  // prolog and epilog).
  size_t arena_size = num_funcs * 2 * TRAMPOLINE_SIZE;
  // Round the estimated arena size to a page size alignment
  int page_size = sysconf(_SC_PAGE_SIZE); 
  arena_size = (arena_size / page_size) * page_size + page_size; 
  // Calculate a approximate allocation target address
  Address mid = addr + (1LL << 32) / 2;
  start = (Address) mmap(mid, arena_size, prot, MAP_PRIVATE | MAP_ANONYMOUS, 
      -1, 0);

  if (start == MAP_FAILED) {
    throw std::bad_alloc();
  }

  Range r = Range(addr, addr + (1LL << 32));
  if (!r.withinRange(start, Range::INCLUSIVE)) {
    munmap(start, arena_size);
    throw std::bad_alloc();
  } 

  alloc_ptr = start;
  size = (int64_t) arena_size;
  remaining_size = size;
}

Arena::~Arena() {
  munmap(start, size);
}

} // End liteprobes 
} // End liteinst 
