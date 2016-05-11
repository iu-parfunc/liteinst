
#ifndef ALLOC_H
#define ALLOC_H 

#include <cstdint>
#include <memory>

#include "defs.hpp"

namespace alloc {

  enum class AllocatorType {
    FIXED,
    ARENA
  };

  class Allocator {
    friend class AllocatorFactory;

    public:
      /// Gets memory chunk of given allocated at the given address.
      /// May fail and return null if the memory requested has already been 
      /// allocated.
      /* \param address Fixed address to allocate memory at
       * \param size    The size of the memory chunk to be allocated
       */
      virtual defs::Address getAllocation(defs::Address address, int32_t size) = 0;

      /// Free the alllocated memory.  
      /* \param address Address to release the previously allocated memory from.
       */
      virtual bool removeAllocation(defs::Address address) = 0;

  };

  class AllocatorFactory {
    public:
      std::unique_ptr<Allocator> getAllocator(AllocatorType type);
  };

}

#endif /*ALLOC_H*/
