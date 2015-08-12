/* 
   Test desc: 
   Single threaded code patching test. 
   
   A call site is replaced with a sequence of 5 nops. 
   (In a single threaded setting patching with 5 nops is ok, 
    since there is no other that that could potentially be in the 
    middle of this sequence as a change occurs) 
   
   Function foo is executed in a loop 10 times. 
   On the 5th run it patches its own call site with nops. 
   foo increments a foo_val. 
   
   Test is considererd a success if after running foo 10 times in a loop foo_val = 5
   
 */


#include <stdio.h> 
#include <memory.h>
#include <stdint.h> 
#include <stdlib.h>

#include <patcher.h>

int g_foo_val = 0; 

void foo(int apa) { 
  
  printf("Running foo %d\n", apa); 
  
  if (g_foo_val >= 4) {
    /* this is the address that foo returns too, 
       so the call_site is a few bytes before that */ 
    uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
    uint8_t* call_addr = ((uint8_t*)addr - 5);
    
    uint64_t  orig_call = *(uint64_t*)call_addr; // ((uint8_t*)addr - 5);
    
    uint64_t nop_patch = 0x0000009090909090;
    uint64_t keep_mask = 0xFFFFFF0000000000; 
    
    init_patch_site((void*)call_addr, 8);
    
    uint64_t patch = (orig_call & keep_mask) | nop_patch; 
    
    patch_64((void*)call_addr, patch); 
    
    /* to ensure its not a false positive */ 
    g_foo_val++;
    return; 
  }  

  g_foo_val++;
} 

int main(void) {
 
  int i = 0; 
  
  printf("Testing to patch a callsite with nops\n"); 
  
  
  for (i = 0; i < 10; i ++){ 
    foo(i);     
    printf("Loop %d\n",i);
  }
  

  if (i == 10 && g_foo_val == 5) { 
    printf("Success\n");
    return 0;
  }
  printf("Test failed!\n"); 
  return 1;  
}
