
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
