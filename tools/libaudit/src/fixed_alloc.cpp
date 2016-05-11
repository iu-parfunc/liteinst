
#include <unistd.h> // sysconf
#include <sys/mman.h>

#include <string>

#include "alloc.hpp"
#include "fixed_alloc.hpp"
#include "range.hpp"

namespace alloc {

  using std::string;
  using std::vector;

  using namespace range;
  using namespace lock;
  using namespace defs;

  void handle_error(const char* msg) {
    perror(msg);
  }

  RangeMap FixedAllocator::allocations = RangeMap(sysconf(_SC_PAGESIZE));

  FixedAllocator::FixedAllocator() : Allocator() {
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

    Address stub_address = (Address)mmap(at, size,
        PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS, -1,0);

    if (stub_address == MAP_FAILED) {
      handle_error(string("mmap").c_str());
      fprintf(stderr, "[mmap] Failed allocating stub at : %p\n", at); 
      return NULL;
    } else {
      if (stub_address != at) {
        fprintf(stderr, "[mmap] Got different stub address requested: %p  " 
            " recieved : %p\n", at, stub_address);
        return NULL;
      }
    }

    return stub_address;

    /*
    if (!callback_inited) {
      initialize();
    }*/

    /*
    Range r = Range(at, at+size);
    bool success = allocations.updateRangeEntries(
        r, [this](vector<RangeEntry*> entries, Range range) 
           { return this->allocationCallback(entries, range); });

    return success ? at : NULL;
    */
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

    /*
    vector<Range> partitions = range.getRangePartitions(sysconf(_SC_PAGESIZE));
    for (Range p : partitions) {
      for (RangeEntry* re : entries) {

      }
    }
    return false;
    */
  }

}
