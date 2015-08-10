
#include <stdio.h> 
#include <memory.h>
#include <stdint.h> 
#include <stdlib.h>

#include "patcher.h"  

int g_foo_val = 0; 

void bar(int apa){ 

  printf("Running bar %d\n", apa); 

}



void foo(int apa) { 
  
  printf("Running foo %d\n", apa); 
  
  if (g_foo_val >= 5) {
    /* this is the address that foo returns too, 
       so the call_site is a few bytes before that */ 
    uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
    uint8_t* call_addr = ((uint8_t*)addr - 5);
    
    uint64_t  orig_call = *(uint64_t*)call_addr; // ((uint8_t*)addr - 5);
    
    uint64_t call_bar_patch = 0x00000000000000e8;
   
    uint64_t bar_addr = ((uint64_t)bar - (uint64_t)addr); 
   

    uint64_t keep_mask = 0xFFFFFF0000000000; 
    
    call_bar_patch = (call_bar_patch | (bar_addr << 8)) & ~keep_mask;

    printf("%lx  \n", call_bar_patch);

    
    
    if (!init_patch_site((void*)call_addr, 8)) { 
      printf("ERROR init_patch_site\n"); 
      exit(EXIT_FAILURE);
    }
    
    uint64_t patch = (orig_call & keep_mask) | call_bar_patch; 
    
    printf("%lx  \n", patch);
    patch_64((void*)call_addr, patch); 
    
    return; 
  }  

  g_foo_val++;
} 



int main(void) {
 
  int i = 0; 

  printf("Testing to replace a callsite with a call to another function\n"); 
  
  
  for (i = 0; i < 10; i ++){ 
    foo(i);     
    printf("Loop %d\n",i);
  }
  

  printf("Test is a %s\n", (i == 10 && g_foo_val == 5) ? "Success" : "Failure");

  

  return 0;  
}
