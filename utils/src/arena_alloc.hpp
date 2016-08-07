
#ifndef ARENA_ALLOC_H
#define ARENA_ALLOC_H 

#include <vector>
#include <memory>
#include <deque>
#include <sys/mman.h>

#include "alloc.hpp"
#include "range.hpp"
#include "concurrency.hpp"
#include "defs.hpp"

namespace utils { 
namespace alloc {

// Estimate of an average trampoline size
const int TRAMPOLINE_SIZE = 90; 

class Arena {
  public:
    utils::Address alloc_ptr;
    utils::Address start;
    int64_t size;
    int64_t remaining_size;

    Arena(utils::Address addr, int prot);

    ~Arena(); 

};

class ArenaPool {
  public:
    /** \brief Creates an ArenaPool for the given memory region starting from 
     *    given address spanning length bytes which contains memory arenas with 
     *    given protection level.
     *  \param addr Start of the memory region
     *  \param prot Protection level of allocated memory arenas 
     */
    ArenaPool(utils::Address addr, int prot);

    /** \brief Gets a free memory slot of given size from arenas contained
     *    in the current pool.
     *  \param size The size of allocation
     *
     *  If no space is found in existing arenas a new arena will be added and
     *  space from it will be obtained.
     */
    utils::Address getFreeMemorySlot(int32_t size);

  private:
    utils::Address base; ///< Base address of memory chunk within which arenas 
                  ///< belonging to this pool will be allocated. 
    int prot;     ///< The protection level of allocated arenas of this pool 
    utils::concurrency::SpinLock lock;
    std::deque<std::shared_ptr<Arena>> arenas; ///< Arena at front is the 
                               ///< current arena in use. All allocations will
                               ///< happen from it. 
};

/// Allocates memory from an arena. 
class ArenaAllocator : public Allocator {
  friend AllocatorFactory;

  public:

    /** \brief Creates an arena allocator
     *  \param prot The protection level of allocated memory from this allocator
     */
    ArenaAllocator(int prot);

   /** \brief Gets memory chunk of given size allocated within 2^32 distance from
    *    given address.
    *  \param address Address from which the 2^32 byte distance is to be measured
    *  \param size    The size of the memory chunk to be allocated
    *  \return        The address at which memory was allocated or null in 
    *  case of allocation failure.
    *
    */
    utils::Address getAllocation(utils::Address address, int32_t size);

   /** \brief Singly removal of an allocated memory is not supported. 
    *  
    *  This will throw an expection if invoked since explicit removal of an 
    *  allocated memory region is not support with arena allocation. All the 
    *  allocated memory will be deallocated at destructionof the allocator.
    */
   bool removeAllocation(utils::Address address);

   void showStatistics(FILE* fp, int nspaces);

  private:
    static utils::concurrency::ConcurrentMap<utils::Address,
      std::shared_ptr<ArenaPool>> pools;  // putIfAbsent
    int prot;

};

} // End alloc 
} // End utils 

#endif /*ARENA_ALLOC_H*/
