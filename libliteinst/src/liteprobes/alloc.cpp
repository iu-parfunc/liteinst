
#include <utility>

#include "alloc.hpp"
#include "fixed_alloc.hpp"

namespace liteinst {
namespace rprobes {

using std::unique_ptr;

unique_ptr<Allocator> AllocatorFactory::getAllocator(AllocatorType type) {
  return std::unique_ptr<Allocator>(new FixedAllocator);
}

} // End rprobes
} // End liteinst 
