
#ifndef ALLOC_H
#define ALLOC_H 

#include <cstdint>
#include <memory>

#include "defs.hpp"

namespace alloc {

  /// Different allocator types available
  enum class AllocatorType {
    FIXED, ///< Fixed address allocator
    ARENA  ///< Arena allocator
  };

  /// A memory allocation strategy.
  class Allocator {
    friend class AllocatorFactory;

    public:
      /// Allocates memory chunk with a given size at/near given address.
      /// Depending on the allocator the address can either be a hint or 
      //mandatory.
      virtual defs::Address getAllocation(defs::Address address, 
          int32_t size) = 0;

      /// Free the alllocated memory.  
      /* \param address Address to release the previously allocated memory from.
       */
      virtual bool removeAllocation(defs::Address address) = 0;

  };

  /// Creates a memory allocators
  class AllocatorFactory {
    public:
      std::unique_ptr<Allocator> getAllocator(AllocatorType type);
  };

}

#endif /*ALLOC_H*/
