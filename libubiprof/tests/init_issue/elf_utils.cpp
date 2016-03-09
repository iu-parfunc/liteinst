
#include "elf_utils.hpp"

namespace elfutils {

  using namespace std;

  mem_alloc_table mem_allocations;

  __attribute__((constructor, no_instrument_function))
    void init() {
      fprintf(stderr, "Inside init function..\n");
      uint32_t key = 0;
      list<uint32_t>* mem_list = new list<uint32_t>;
      mem_allocations.insert(make_pair(key,mem_list)); 
    }
}
