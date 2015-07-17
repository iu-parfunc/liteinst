#ifndef _UTILS_HPP_
#define _UTILS_HPP_

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <sys/mman.h>
#include <errno.h>
#include <unistd.h>

#include "logger.hpp"

#define PROBESIZE 16

inline void clflush(volatile void *p) {
    asm volatile ("clflush (%0)" :: "r"(p));
}

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

#endif /* _PATCH_UTILS_HPP_ */
