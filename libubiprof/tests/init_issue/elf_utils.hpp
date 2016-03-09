
#include <map>
#include <list>

namespace elfutils {

  typedef std::map<uint32_t, std::list<uint32_t>*> mem_alloc_table;

  extern mem_alloc_table mem_allocations;
}
