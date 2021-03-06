
#ifndef ALLOC_H
#define ALLOC_H 

#include <cstdint>
#include <memory>

#include "defs.hpp"

namespace utils {
namespace alloc {

/// Different allocator types available
enum class AllocatorType {
  FIXED, ///< Fixed address allocator
  ARENA  ///< Arena allocator
};

/// Describes the constraints on a given punnable byte
enum class Constraints {
  UNCONSTRAINED,  ///< Any value can be used for this byte
  ILLOP           ///< The byte must be an illegal instruction
};

struct MemStatistics {
  int64_t allocations;
  int64_t n_pages;
  int64_t kbs; 
  double utilization;
};

/// A memory allocation strategy.
class Allocator : public Show {
  friend class AllocatorFactory;

  public:
    /** \brief Allocates memory chunk with a given size at/near given address.
     *  \param address Address to allocate memory at.
     *  \param size    Size of the allocation in bytes
     *  \return The address which the memory was allocated or NULL in case of
     *  allocation failure.
     *  
     * Depending on the allocator the address can either be a hint or fixed
     * (i.e: Not being able to allocate at given address amounting to faliure
     * for allocation).
     */
    virtual utils::Address getAllocation(utils::Address address, 
        int32_t size) = 0;

    virtual utils::Address getAllocationFor(utils::Address address, 
        utils::Address for_page, int32_t size) = 0;

    /** \brief Frees allocated memory. 
     *  \param address Address to release the previously allocated memory 
     *   from.
     *  \return If the removal was successful
     */
    virtual bool removeAllocation(utils::Address address) = 0;

    virtual void show(FILE* fp, int nspaces) = 0;

    virtual Address searchAndAllocate(Address from, enum Constraints c[], 
        int32_t size) = 0;

    virtual MemStatistics getAllocationStatistics() = 0;
};

/// Creates memory allocators
class AllocatorFactory {
  public:
    /** \brief Gets a memory allocator of given type.
     *  \param type The type of allocator to be returned.
     *  \return Allocator A reference to the allocator instance.
     */
    static std::unique_ptr<Allocator> getAllocator(AllocatorType type);
};

} // End liteprobes 
} // End liteinst 

#endif /*ALLOC_H*/
