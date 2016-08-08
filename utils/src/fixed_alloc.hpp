
#ifndef FIXED_ALLOC_H
#define FIXED_ALLOC_H 

#include <vector>

#include "alloc.hpp"
#include "range.hpp"

namespace utils { 
namespace alloc {

/// Memory page allocation meta data
class PageMetaData : public utils::range::BlockMetaData {
  public:
    bool allocated; ///< If this page has been mapped yet
    std::vector<utils::range::Range> occupied; 
                                        /**< Occupied sub ranges within the
                                         *   memory page*/
};

/// Allocates memory at fixed addresses. 
class FixedAllocator : public Allocator {
  friend AllocatorFactory;

  private:
  static utils::range::BlockRangeMap allocations; 

  FixedAllocator();
  ~FixedAllocator();

  public:
  /** \brief Gets memory chunk of given size allocated at the given address.
   *  \param address Fixed address to allocate memory at
   *  \param size    The size of the memory chunk to be allocated
   *  \return        The address at which memory was allocated or null in 
   *  case of allocation failure.
   *
   * May fail and return null if the memory address requested has already 
   * been allocated.
   */
  utils::Address getAllocation(utils::Address address, int32_t size);

  /** \brief Free the alllocated memory.    
   *  \param address Address to release the previously allocated memory 
   *  from.
   */
  bool removeAllocation(utils::Address address);

  /** \brief Internal callback used to reserve memory and allocate it.
   *  \param entries Memory page blocks the given range occupies
   *  \param range   The memory address range to be allocated
   */ 
  bool allocationCallback(std::vector<utils::range::BlockEntry*> entries, 
      utils::range::Range range); 

  void show(FILE* fp, int nspaces);

  MemStatistics getAllocationStatistics();

};

} // End alloc 
} // End utils 

#endif /*FIXED_ALLOC_H*/
