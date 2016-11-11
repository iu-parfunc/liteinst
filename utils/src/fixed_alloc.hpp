
#ifndef FIXED_ALLOC_H
#define FIXED_ALLOC_H 

#include <vector>
#include <unordered_map>
#include <set>

#include "alloc.hpp"
#include "range.hpp"

namespace utils { 
namespace alloc {

class FixedAllocMetaData : public utils::range::RangeMetaData {
  public:
    Address for_page;
    uint16_t prefix;
};

struct RangeSort {
  inline bool operator() (const utils::range::Range& r1, 
      const utils::range::Range& r2) {
    return (r1.start < r2.start);
  }
};

/// Memory page allocation meta data
class PageMetaData : public utils::range::BlockMetaData {
  public:
    bool allocated;  ///< If this page has been mapped yet
    uint16_t prefix; ///< Invalid opcode prefix used to calculate the 
                     ///< displacement while allocating this page.
    std::set<utils::range::Range, RangeSort> occupied; 
                                        /**< Occupied sub ranges within the
                                         *   memory page*/
};

/// Allocates memory at fixed addresses. 
class FixedAllocator : public Allocator {
  friend AllocatorFactory;

  private:
  static utils::range::BlockRangeMap allocations; 
  static std::unordered_map<Address, std::set<utils::range::BlockEntry*>> page_map;
  static long zone_size;

  utils::Address allocate(utils::Address to, utils::Address page, int alloc_sz);
  bool ifAllocatableRange(Address to, int alloc_sz);
  utils::Address newAllocationForPage(utils::Address page, utils::Address from,
      enum Constraints c[], int size);
  utils::Address findAllocationWithinExisting(utils::Address from, 
      std::set<utils::range::BlockEntry*>& entries, enum Constraints c[], 
      int32_t size);


  FixedAllocator();
  ~FixedAllocator();

#ifdef AUDIT
  static int64_t n_allocations;
#endif

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

  utils::Address getAllocationFor(utils::Address address, 
        utils::Address for_page, int32_t size);

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
      utils::range::Range range, utils::range::RangeMetaData* m); 

  Address searchAndAllocate(Address from, enum Constraints c[], int32_t size);

  void show(FILE* fp, int nspaces);

  MemStatistics getAllocationStatistics();

};

} // End alloc 
} // End utils 

#endif /*FIXED_ALLOC_H*/
