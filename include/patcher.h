/* 
  
  libpointpatch
  
  Authors: Buddhika Chamith,  Bo Joel Svensson 
  year: 2015

  Should compile with: 
    icpc [-std=c++11]
    icc 
    gcc -std=gnu99   (because of inline assembly, stdbool) 
    g++ [-std=c++11]
  
*/ 


#ifndef __PATCHER_H_
#define __PATCHER_H_

#include <stdio.h> 
#include <stdint.h> 
#include <stdbool.h> 

#include <sys/mman.h> 

#include <errno.h> 
#include <unistd.h> 

#include <limits.h> 
#include <assert.h> 

#include "distorm.h" 
#include "mnemonics.h" 

/* -----------------------------------------------------------------
   Types 
   ----------------------------------------------------------------- */ 
 
typedef struct { 
  _DInst *decoded_instructions; 
  unsigned int n_instructions; 
} Decoded; 


#ifdef __cplusplus
extern "C" {
#endif


#define DECODED_OK(d)  ((d).decoded_instructions != NULL && (d).n_instructions != 0)

/* ----------------------------------------------------------------- 
   Interface 
   ----------------------------------------------------------------- */ 

/* Allow READ, WRITE and EXEC on pages touched by patch site */ 
bool init_patch_site(void*, size_t);

/* see what the wait setting is (DEBUG) */ 
long patch_get_wait();

/* does the patch point straddle a cache line */
bool is_straddler_64(void *addr);

/* returns 0 on non-straddlers, otherwise an offset into the patch */
int straddle_point_64(void *addr);

/* Patch up to 8 byte instructions in a straddler safe way */ 
bool patch_64(void*, uint64_t); 
/* Patch up to 4 byte instructions in a straddler safe way */
bool patch_32(void*, uint32_t); 

Decoded decode_range(void*, void*);
void destroy_decoded(Decoded);

int64_t find_reg_setter(_RegisterType, Decoded);

/* -----------------------------------------------------------------
   unsafe replacements for patch code  
   ----------------------------------------------------------------- */ 
#define PATCH_64_UNSAFE(addr,x)  (((uint64_t*)addr)[0]) = x 
#define PATCH_32_UNSAFE(addr,x)  (((uint32_t*)addr)[0]) = x
#define PATCH_16_UNSAFE(addr,x)  (((uint16_t*)addr)[0]) = x 

/* this one is always safe */ 
inline void patch_8(void *addr, uint8_t patch){ 
  ((uint8_t*)addr)[0] = patch; 
}

#ifdef __cplusplus
} /* End Of Extern */
#endif

#endif 
