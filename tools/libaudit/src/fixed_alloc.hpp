
#ifndef FIXED_ALLOC_H
#define FIXED_ALLOC_H 

#include <vector>

#include "alloc.hpp"
#include "range.hpp"
#include "lock.hpp"

namespace alloc {

  class PageMetaData : public range::EntryMetaData {
    public:
      bool allocated;
      std::vector<range::Range> occupied;
  };
  
  /// Allocates memory at fixed addresses. 
  class FixedAllocator : public Allocator {
    friend AllocatorFactory;

    private:
      static range::RangeMap allocations; 

      FixedAllocator();

    public:
      defs::Address getAllocation(defs::Address address, int32_t size);
      bool removeAllocation(defs::Address address);

      /// Internal callback used to reserve memory and allocate it.
      bool allocationCallback(std::vector<range::RangeEntry*> entries, 
          range::Range range); 
  };

}

#endif /*FIXED_ALLOC_H*/
