
#include <stdio.h> 
#include <memory.h>
#include <stdint.h> 
#include <stdlib.h>

#include "patcher.h"  

int main(void) {
  int i=0; 
  bool ok=false;

  uint64_t *data; 
  uint64_t *dat; 
  
  data = (uint64_t*)malloc((sizeof(uint64_t) * 10) + 64); 
  memset(data, 0, (sizeof(uint64_t) * 10) + 64); 
  ok = init_patch_site((void*)data,8); 
  printf("\n%s\n",ok ? "Initialization Worked" : "Initialization Failed");


  for (int it = 0; it < 64; it ++) { 
    memset(data, 0, (sizeof(uint64_t) * 10) + 64); 
    printf("iteration %d\n",it); 
    dat = (uint64_t*)((uint8_t*)data + it);
  
  
    for (i = 0; i < 10; i ++) {
      printf(" %lx ", dat[i]);
    }
    
    printf("\n\n");
    
    patch_64((void*)(dat + 2), 0xFFFEFDFCFBFAF9F8);
    
    for (i = 0; i < 10; i ++) {
      printf(" %lx ", dat[i]);
    }
    printf("\n\n");
  }
  
  


  return 0;  
}
