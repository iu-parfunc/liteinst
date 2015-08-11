
#include <stdio.h> 
#include <memory.h>
#include <stdint.h> 
#include <stdlib.h>

#include <patcher.h>

int main(void) {
 
  uint32_t *data; 
  uint32_t *dat; 

  printf("Testing patch writes and straddling writes for correctness of value\n"); 

  data = (uint64_t*)malloc((sizeof(uint32_t) * 10) + 64); 
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
