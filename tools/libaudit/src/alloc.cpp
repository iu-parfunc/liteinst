
#include <utility>

#include "alloc.hpp"
#include "fixed_alloc.hpp"

namespace alloc {

  using std::unique_ptr;
  // using std::make_unique;

  // From SO : http://stackoverflow.com/a/24609331/481724
  // Apparently make_unqiue is only available from c++14
  template<typename T, typename... Args>
    std::unique_ptr<T> make_unique(Args&&... args) {
    return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
  }

  unique_ptr<Allocator> AllocatorFactory::getAllocator(AllocatorType type) {
    return std::unique_ptr<Allocator>(new FixedAllocator);
    // return make_unique<FixedAllocator>();
  }

}
