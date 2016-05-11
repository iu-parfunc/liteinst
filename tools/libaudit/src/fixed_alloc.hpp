
#ifndef FIXED_ALLOC_H
#define FIXED_ALLOC_H 

#include <vector>

#include "alloc.hpp"
#include "range.hpp"
#include "lock.hpp"

namespace alloc {

  /// Memory page allocation meta data
  class PageMetaData : public range::BlockMetaData {
    public:
      bool allocated;
      std::vector<range::Range> occupied; ///< Occupied sub ranges within the
                                          ///< memory page
  };
  
  /// Allocates memory at fixed addresses. 
  class FixedAllocator : public Allocator {
    friend AllocatorFactory;

    private:
      static range::RangeMap allocations; 

      FixedAllocator();

    public:
      /// Gets memory chunk of given size allocated at the given address.
      /// May fail and return null if the memory address requested has already 
      /// been allocated.
      /* \param address Fixed address to allocate memory at
       * \param size    The size of the memory chunk to be allocated
       */
      defs::Address getAllocation(defs::Address address, int32_t size);

      /// Free the alllocated memory. The real mapping.   
      /* \param address Address to release the previously allocated memory from.
       */
      bool removeAllocation(defs::Address address);

      /// Internal callback used to reserve memory and allocate it.
      bool allocationCallback(std::vector<range::BlockEntry*> entries, 
          range::Range range); 
  };

}

#endif /*FIXED_ALLOC_H*/
