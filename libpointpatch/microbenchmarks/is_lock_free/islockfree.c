



#include <stdio.h>
#include <stdint.h>
#include <stdbool.h> 
#include <stdlib.h>

#define __USE_GNU
#include <mmintrin.h>

int main(void) { 
  
  uint8_t *array; 

  /* array containing a few straddlers */ 
  array = (uint8_t*)malloc(128); 
  
  bool lock[120] = {true};
  
 
  bool always_lock_free = true; 
 
  for (int i=0; i < 120; i ++) { 
    bool l =  __atomic_always_lock_free(sizeof(uint64_t), (void*)&array[i]);
    
    always_lock_free = always_lock_free && l;

    if (! l) { 
      uint64_t addr = (uint64_t)&array[i]; 
      //uint8_t  cache_line_offset = (uint8_t)(addr % 64);
      lock[i] = false;
    }
  }

  if (always_lock_free) { 
    printf("IS ALWAYS LOCK FREE\n");
  } else { 
    printf("IS NOT ALWAYS LOCK FREE\n");
  }
  
  for (int i = 0; i < 120; i ++) { 
    
    printf("%d: %s \n", i, lock[i] ? "True" : "False");
    
  }
  

  return 0;
} 
