
#ifndef _PATCH_UTILS_HPP_
#define _PATCH_UTILS_HPP_

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <sys/mman.h>
#include <errno.h>
#include <unistd.h>

#include "logger.hpp"

inline bool modify_page_permissions(uint8_t* addr) {

  long page_size = sysconf(_SC_PAGESIZE);
  int code = mprotect((void*)(addr - (((uint64_t)addr)%page_size)), page_size,
      PROT_READ | PROT_WRITE | PROT_EXEC);

  if (code) {
    LOG_ERROR("mprotect was not successfull! code %d\n", code);
    LOG_ERROR("errno value is : %d\n", errno);
    return false;
  }

  if (page_size - ((uint64_t)addr)%page_size < PROBESIZE) {
    code = mprotect((void*)(addr-((uint64_t)addr)%page_size+ page_size) , page_size,
        PROT_READ | PROT_WRITE | PROT_EXEC);
    if (code) {
      LOG_ERROR("mprotect was not successfull! code %d\n", code);
      LOG_ERROR("errno value is : %d\n", errno);
      return false;;
    }
  }

  return true;
}

inline bool patch_with_jmp(uint64_t* addr, uint8_t start_idx, uint32_t distance) {

  if (start_idx > 3) {
    LOG_ERROR("[Finstrumentor] Not enough space to patch the jmp within 64 bits space given..\n");
    return false;
  }

  uint64_t old_val = *addr;

  uint64_t mask = 0xFFFFFFFFFFFFFFFF;
  int8_t shift_size = start_idx * 8;

  mask = (mask << shift_size);
  uint64_t new_val = (old_val & ~mask);
  uint8_t* new_val_ptr = (uint8_t*) &new_val;

  /* Patch with the temporary jump to skip executing next instruction byte until it is written with proper call site information */
  new_val_ptr[start_idx] = 0xE9;

  *(uint32_t*)(new_val_ptr+start_idx+1) = (uint32_t) distance;

  return __sync_bool_compare_and_swap(addr, old_val, new_val);

}

#endif /* _PATCH_UTILS_HPP_ */
