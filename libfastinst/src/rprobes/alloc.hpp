
#ifndef ALLOC_H
#define ALLOC_H 

#include <cstdint>
#include <memory>

#include "utils.hpp"

namespace fastinst {
namespace rprobes {

/// Different allocator types available
enum class AllocatorType {
  FIXED, ///< Fixed address allocator
  ARENA  ///< Arena allocator
};

/// A memory allocation strategy.
class Allocator {
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
    virtual Address getAllocation(Address address, int32_t size) = 0;

    /** \brief Frees allocated memory. 
     *  \param address Address to release the previously allocated memory 
     *   from.
     *  \return If the removal was successful
     */
    virtual bool removeAllocation(Address address) = 0;

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

} // End rprobes
} // End fastinst

#endif /*ALLOC_H*/
