#ifndef __ASYNCPATCH_H_
#define __ASYNCPATCH_H_

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
   Data 
   ----------------------------------------------------------------- */

typedef struct { 
  _DInst *decoded_instructions; 
  unsigned int n_instructions; 
} Decoded; 


/* Switch to a address -> patch map */
struct Patch { 
  uint64_t address; /* address to be patched */ 
  uint64_t front;   /* part of patch before straddle location */
  uint64_t back;    /* part of patch behind straddle location */ 
  uint64_t timestamp; /* when was this patch initiated */ 
  struct Patch *next; /* linked list (for now) */ 
}; 

typedef struct  { 
  struct Patch *head; 
  unsigned long length; 
} PatchList;


/* -----------------------------------------------------------------
   PROTOTYPES 
   ----------------------------------------------------------------- */ 

#ifdef __cplusplus
extern "C" {
#endif


#define DECODED_OK(d)  ((d).decoded_instructions != NULL && (d).n_instructions != 0)

/* ----------------------------------------------------------------- 
   Interface 
   ----------------------------------------------------------------- */ 
/* temp exposed */
void cons_patch(struct Patch *p, PatchList *pl);
struct Patch *remove_patch(uint64_t find_addr, PatchList *pl);
/* print some info about cache hierarchy */ 
void print_cache_info();

/* Allow READ, WRITE and EXEC on pages touched by patch site */ 
bool init_patch_site(void*, size_t);

/* see what the wait setting is (DEBUG) */ 
long patch_get_wait();

/* does the patch point straddle a cache line */
bool is_straddler_64(void *addr);

/* returns 0 on non-straddlers, otherwise an offset into the patch */
int straddle_point_64(void *addr);

/* Patch up to 8 byte instructions in a straddler safe way */ 
bool async_patch_64(void*, uint64_t); 


Decoded decode_range(void*, void*);
void destroy_decoded(Decoded);

int64_t find_reg_setter(_RegisterType, Decoded);

#ifdef __cplusplus
} /* End Of Extern */
#endif

#endif 
