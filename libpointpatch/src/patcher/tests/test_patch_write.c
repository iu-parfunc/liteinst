/* 
   Test desc: 
   
   Tests patching functions for correctness of value. 
   exercises both the straddler patch and the "normal" path of the patch function. 
   
   an array of 10 elements all containing zero is updated at index 2 
   using the patch_64 function. 
   
   The array is moved around in memory so that all variants of cache line boundary 
   straddlers occur (that is it is moved in a window 64 bytes larger than the array). 
   
   Test is considered a success if all writes into the array using patch_64 
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
 
  uint64_t *data; 
  uint64_t *dat; 

  printf("Testing patch writes and straddling writes for correctness of value\n"); 

  data = (uint64_t*)malloc((sizeof(uint64_t) * 10) + 64); 
  memset(data, 0, (sizeof(uint64_t) * 10) + 64); 
  
  if (!init_patch_site((void*)data,8)) { 
    printf("Test failed! (initialization)\n");
    exit(EXIT_FAILURE); 
  }
  
  
  for (int it = 0; it < 64; it ++) { 
    memset(data, 0, (sizeof(uint64_t) * 10) + 64); 
   
    dat = (uint64_t*)((uint8_t*)data + it);
   
    patch_64((void*)(dat + 2), 0xFFFEFDFCFBFAF9F8);
  
    /* test if the patching actually left the correct value in memory */
    if (dat[2] != 0xFFFEFDFCFBFAF9F8) { 
      printf("Test failied\n"); 
      exit(EXIT_FAILURE); 
    }

  }
  
  

  printf("Success\n");
  return 0;  
}
