/* 
  
  Breaking out the patching tool-set. Make it completely standalone in 
  relation to finstrumentor.

  Authors: Buddhika Chamith,  Bo Joel Svensson 

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

#include "distorm/distorm.h" 
#include "distorm/mnemonics.h" 

/* -----------------------------------------------------------------
   Types 
   ----------------------------------------------------------------- */ 
 
typedef struct { 
  _DInst *decoded_instructions; 
  unsigned int n_instructions; 
} Decoded; 



#define DECODED_OK(d)  ((d).decoded_instructions != NULL && (d).n_instructions != 0)

/* ----------------------------------------------------------------- 
   Interface 
   ----------------------------------------------------------------- */ 

/* Allow READ, WRITE and EXEC on pages touched by patch site */ 
extern bool init_patch_site(void*, size_t);

/* write 8 bytes to a location.. */ 
extern void patch_64(void*, uint64_t); 

Decoded decode_range(void*, void*);
void destroy_decoded(Decoded);

int64_t find_reg_setter(_RegisterType, Decoded);

/* -----------------------------------------------------------------
   unsafe replacements for patch code 
   ----------------------------------------------------------------- */ 
#define PATCH_64_UNSAFE(addr,x) = (((uint64_t*)addr)[0]) = x) 


#endif 
