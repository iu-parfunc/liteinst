#ifndef __PATCHLIST_H 
#define __PATCHLIST_H 

#include <stdio.h> 
#include <stdlib.h> 
#include <stdint.h> 
#include <string.h>


/* ----------------------------------------------------------------- 
   Datastructures and types related to async patching 
   ----------------------------------------------------------------- */ 

typedef uint64_t lock_t;
typedef enum {FIRST_WAIT, SECOND_WAIT, FINISHED} patch_state_t; 


struct Patch { 
  uint64_t addr;      /* address to be patched */ 
  uint64_t front;     /* part of patch before straddle location */
  uint64_t back;      /* part of patch behind straddle location */ 
  uint64_t timestamp; /* when was this patch's state changed */

  patch_state_t state; 
  lock_t lock; 
  struct Patch *next; /* linked list (for now) */ 
}; 
  
typedef struct { 
  uint64_t lock;        /* Lock for adding patches to this list */ 
  uint64_t length;      /* Number of patches in this list */
  struct Patch *head;   /* First element of list (or NULL) */ 
} PatchList; 

#define ADDR_KEY_BITS 8 
#define ADDR_KEY_MAX  256 // why not 2**8 

typedef PatchList LSPatchList[ADDR_KEY_MAX]; 


/* -----------------------------------------------------------------
   proto
   ----------------------------------------------------------------- */
int patch_list_key(uint64_t);
bool cons_patch(struct Patch *p, PatchList *pl);
struct Patch *peek_patch(uint64_t find_addr, PatchList *pl);

/* -----------------------------------------------------------------
   Operations 
   ----------------------------------------------------------------- */
void initialize_LSPatchList(LSPatchList *pl) { 

  memset(pl,0,ADDR_KEY_MAX*sizeof(PatchList)); 
  
}

/* Addres to key */ 
inline int patch_list_key(uint64_t addr) { 
  
  /* use some low bits of address */ 
  return (int)(addr & (ADDR_KEY_MAX-1));
  
}


/* add patch to LSPatchList */  
bool add_patch( uint64_t addr,
	        uint64_t front,
		uint64_t back,
		uint64_t timestamp,
		LSPatchList pl) { 
 
  struct Patch *p = NULL; 
  
  /* acquire lock here */ 
  int key = patch_list_key(addr); 
  while (!__sync_bool_compare_and_swap(&pl[key].lock,0,1));

  p = peek_patch(addr, &pl[key]); 
    
  if (p) { 

    /* Interesting property (that doesnt hold!) */ 
    //assert(p->state == FINISHED);

    if (p->state != FINISHED) { 
      pl[key].lock = 0; 
      return false; 
    }
    /* update patch data */ 
    while (!__sync_bool_compare_and_swap(&p->lock,0,1));

    p->addr = addr; 
    p->front = front; 
    p->back = back; 
    p->timestamp = timestamp; 
    p->state = FIRST_WAIT; 
    p->lock = 0; 
	
  } else { 
    /* invent and insert patch */ 

    struct Patch *p = (struct Patch*)malloc(sizeof(struct Patch)); 
    p->addr      = addr;
    p->front     = front; 
    p->back      = back; 
    p->timestamp = timestamp; 
    p->state     = FIRST_WAIT; 
    p->lock      = 0; 
    p->next = NULL; 
    cons_patch(p, &pl[key]); 
  }
  
  pl[key].lock=0; 
  return true; 
}		 
		
bool cons_patch(struct Patch *p, PatchList *pl){ 
  
  struct Patch *old_hd = pl->head; 
  p->next  = old_hd;
  pl->head = p;
  pl->length++; 
  return true; 
}

/* Does not acquire lock  */ 
struct Patch *peek_patch(uint64_t find_addr, PatchList *pl){ 
  
  struct Patch *p = pl->head; 
  if (p == NULL) { 
    return NULL; 
  }
  
  if (p->addr == find_addr) { 
    return p; 
  }
  
  struct Patch *curr = p->next; 
  while (curr != NULL && curr->addr != find_addr) { 
    curr = curr->next;
  }
 
  return curr;
}

/* 
#define PATCH_REMOVED          0  
#define PATCH_NOT_FOUND        1 
#define PATCH_LIST_CONTENTION  2

int remove_patch(uint64_t find_addr, PatchList *pl, struct Patch **res){ 
  
  if (__sync_bool_compare_and_swap(&pl->lock,0,1)){ 
  
    struct Patch *p = pl->head; 
    if (p == NULL) { 
      pl->lock = 0; 
      return PATCH_NOT_FOUND; 
    }
    
    if (p->addr == find_addr) { 
      pl->head = p->next; 
      pl->length--;
      pl->lock = 0; 
      *res = p;
      return PATCH_REMOVED;
    }
  
    struct Patch *curr = p->next; 
    struct Patch *prev = p;
    while (curr != NULL && curr->addr != find_addr) { 
      prev = curr; 
      curr = curr->next;
    }
    if (curr == NULL) { 
      pl->lock = 0; 
      return PATCH_NOT_FOUND; 
    }

    prev->next = curr->next; 
    curr->next = NULL; 
    pl->length--;
    *res = curr;  // user is responsible for storage freeing of these.  
    pl->lock = 0; 
    return PATCH_REMOVED;
  }
  
  return PATCH_LIST_CONTENTION; 
}
*/ 
#endif /* __PATCHLIST_H */
