
#ifndef __PATCH_UTILS_H_
#define __PATCH_UTILS_H_

#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <unistd.h>
#include <assert.h>

#ifdef __cplusplus
extern "C" {
#endif

// Globals 
extern size_t g_cache_lvl3_line_size;
extern long g_page_size; 

  extern bool set_page_rwe(void *addr,size_t nbytes);

  extern uint64_t get_msb_mask_64(int nbytes);

  extern uint64_t get_lsb_mask_64(int nbytes);

  extern uint32_t get_msb_mask_32(int nbytes);

  extern uint32_t get_lsb_mask_32(int nbytes);

#ifdef __cplusplus
} /* End Of Extern */
#endif

#endif
