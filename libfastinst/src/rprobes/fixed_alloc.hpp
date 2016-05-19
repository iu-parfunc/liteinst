
#ifndef FIXED_ALLOC_H
#define FIXED_ALLOC_H 

#include <vector>

#include "alloc.hpp"
#include "range.hpp"

namespace fastinst { 
namespace rprobes {

/// Memory page allocation meta data
class PageMetaData : public BlockMetaData {
  public:
    bool allocated; ///< If this page has been mapped yet
    std::vector<Range> occupied; /**< Occupied sub ranges within the
                                         *   memory page*/
};

/// Allocates memory at fixed addresses. 
class FixedAllocator : public Allocator {
  friend AllocatorFactory;

  private:
  static RangeMap allocations; 

  FixedAllocator();

  public:
  /** /brief Gets memory chunk of given size allocated at the given address.
   *  /param address Fixed address to allocate memory at
   *  /param size    The size of the memory chunk to be allocated
   *  /return        The address at which memory was allocated or null in 
   *  case of allocation failure.
   *
   * May fail and return null if the memory address requested has already 
   * been allocated.
   */
  Address getAllocation(Address address, int32_t size);

  /** \brief Free the alllocated memory.    
   *  \param address Address to release the previously allocated memory 
   *  from.
   */
  bool removeAllocation(Address address);

  /** \brief Internal callback used to reserve memory and allocate it.
   *  \param entries Memory page blocks the given range occupies
   *  \param range   The memory address range to be allocated
   */ 
  bool allocationCallback(std::vector<BlockEntry*> entries, 
      Range range); 
};

} // End rprobes
} // End fastinst

#endif /*FIXED_ALLOC_H*/
