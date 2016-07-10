
#include <utility>
#include <stdexcept> // std::invalid_argument

#include "alloc.hpp"
#include "fixed_alloc.hpp"
#include "arena_alloc.hpp"

namespace utils {
namespace alloc {

using std::unique_ptr;
using std::invalid_argument;

Allocator* AllocatorFactory::getAllocator(AllocatorType type) {
  switch(type) {
    case AllocatorType::FIXED:
      return new FixedAllocator;
    case AllocatorType::ARENA:
      return new ArenaAllocator(PROT_READ | PROT_WRITE | PROT_EXEC);
    default:
      throw invalid_argument("Invalid allocator type provided..\n");
  }
}

} // End liteprobes 
} // End liteinst 
