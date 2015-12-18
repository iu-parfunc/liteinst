
#include "zca_types.hpp"

namespace elfutils {

  typedef std::map<uint32_t, std::list<mem_island*>*> mem_alloc_table;

  extern mem_alloc_table mem_allocations;

  void readZCAELFMetaData(ProbeVec* pmdVec);
}
