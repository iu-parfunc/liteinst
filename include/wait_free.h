
#ifndef __WAIT_FREE_H_
#define __WAIT_FREE_H_


#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

#include "patch_utils.h"

#ifdef __cplusplus
extern "C" {
#endif

bool patch_call_64(void* addr, uint64_t sequence);

bool init_patch_site(void *addr, size_t nbytes);

bool patch_call_32(void* addr, uint32_t sequence);

#ifdef __cplusplus
} /* End Of Extern */
#endif

#endif
