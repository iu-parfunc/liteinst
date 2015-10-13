#include <stdio.h> 
#include <stdlib.h> 
#include <stdint.h> 

/* ----------------------------------------------------------------- 
   Datastructures related to async patching 
   ----------------------------------------------------------------- */ 

struct Patch { 
  uint64_t addr;      /* address to be patched */ 
  uint64_t front;     /* part of patch before straddle location */
  uint64_t back;      /* part of patch behind straddle location */ 
  uint64_t timestamp; /* when was this patch initiated */ 
  struct Patch *next; /* linked list (for now) */ 
}; 
  
typedef struct { 
  uint64_t lock;        /* Lock for adding patches to this list */ 
  uint64_t length;      /* Number of patches in this list */
  struct Patch *head;   /* First element of list (or NULL) */ 
} PatchList; 


/* -----------------------------------------------------------------
   Operations 
   ----------------------------------------------------------------- */ 
bool cons_patch(struct Patch *p, PatchList *pl){ 
  
  if (__sync_bool_compare_and_swap(&pl->lock,0,1)) {

    struct Patch *old_hd = pl->head; 
    p->next  = old_hd;
    pl->head = p;
    pl->length++; 
    pl->lock = 0; // release lock 
    return true; 
  } 
  return false; 
}

#define PATCH_REMOVED          0  
#define PATCH_NOT_FOUND        1 
#define PATCH_LIST_CONTENTION  2

int remove_patch(uint64_t find_addr, PatchList *pl, struct Patch **res){ 
  
  if (__sync_bool_compare_and_swap(&pl->lock,0,1)){ 
  
    struct Patch *p = pl->head; 
    if (p == NULL) return PATCH_NOT_FOUND; 
    
    if (p->addr == find_addr) { 
      pl->head = p->next; 
      pl->length--;
      return p;
    }
  
    struct Patch *curr = p->next; 
    struct Patch *prev = p;
    while (curr != NULL && curr->addr != find_addr) { 
      prev = curr; 
      curr = curr->next;
    }
    if (curr == NULL) return PATCH_NOT_FOUND; 

    prev->next = curr->next; 
    curr->next = NULL; 
    pl->length--;
    pl->lock = 0; 
    *res = curr;  /* user is responsible for storage freeing of these.  */
    return PATCH_REMOVED;
  }
  
  return PATCH_LIST_CONTENTION; 
}

