/* 
   Test desc: 
   
   Tests patching functions for correctness of value. 
   exercises both the straddler patch and the "normal" path of the patch function. 
   
   an array of 10 elements all containing zero is updated at index 2 
   using the patch_32 function. 
   
   The array is moved around in memory so that all variants of cache line boundary 
   straddlers occur (that is it is moved in a window 64 bytes larger than the array). 
   
   Test is considered a success if all writes into the array using patch_32 
   results in the correct value manifesting in memory. 

   # info: 
    - should add condition that no other element of array changes its value. 
   

*/ 


#include <stdio.h> 
#include <memory.h>
#include <stdint.h> 
#include <stdlib.h>

#include <patcher.h>

int main(void) {
 
  uint32_t *data; 
  uint32_t *dat; 

  printf("Testing patch writes and straddling writes for correctness of value\n"); 

  data = (uint32_t*)malloc((sizeof(uint32_t) * 10) + 64); 
  memset(data, 0, (sizeof(uint32_t) * 10) + 64); 
  
  if (!init_patch_site((void*)data,10 * 4 + 64)) { 
    printf("Test failed! (initialization)\n");
    exit(EXIT_FAILURE); 
  }
  
  
  for (int it = 0; it < 64; it ++) { 
    memset(data, 0, (sizeof(uint32_t) * 10) + 64); 
   
    dat = (uint32_t*)((uint8_t*)data + it);
   
    patch_32((void*)(dat + 2), 0xF0F1F2F3);
  
    /* test if the patching actually left the correct value in memory */
    if (dat[2] != 0xF0F1F2F3) { 
      printf("Test failied\n"); 
      exit(EXIT_FAILURE); 
    }

  }
  
  

  printf("Success\n");
  return 0;  
}
