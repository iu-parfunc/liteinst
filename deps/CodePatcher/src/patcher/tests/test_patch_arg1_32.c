
#include <stdio.h> 
#include <memory.h>
#include <stdint.h> 
#include <stdlib.h>

#include <patcher.h>

int main(void);

int g_foo_val = 0; 

#define AFTER_ARG_PATCH 1337
#define PATCH_HEX       0x00000539
#define BEFORE_ARG_PATCH 10

void foo(int apa) { 
  
  printf("Running foo %d\n", apa); 
  
  /* this is the correctness test for arg patching */
  if (g_foo_val > 5 && apa != AFTER_ARG_PATCH)  { 
    /* arg patching has failed */
    exit(EXIT_FAILURE); 
  }
  if (g_foo_val <= 5 && apa != BEFORE_ARG_PATCH) {
    exit(EXIT_FAILURE);
  }
  
  /* if everything is all right so far and we are in the 5th iteration
     patch the argument */ 
  if (g_foo_val == 5) {
    /* this is the address that foo returns too, 
       so the call_site is a few bytes before that */ 
    uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
    uint8_t* call_addr = ((uint8_t*)addr - 5);

    Decoded d = decode_range((void*)main,(void*)call_addr);
    
    if (!DECODED_OK(d)) {
      printf("decode_range did not work\n");
    }
      
    
    bool is_edi = false; 
    /* Looking for setter of EDI or RDI */ 
    int64_t setter = find_reg_setter(R_EDI,d);
    if (setter == -1) { 
      printf("Did not find an EDI setter. Not looking for RDI setter in this test\n"); 
      exit(EXIT_FAILURE); 
    } else is_edi = true; 
    printf("setter found at offset %ld\n",setter);
    
    /* plus one byt to step past the mov inst */ 
    void *patch_site = (void*)((uint64_t)call_addr - setter + 1);
 
    uint32_t patch = PATCH_HEX;
    
    init_patch_site(patch_site, 8);
    PATCH_32_UNSAFE(patch_site, patch);


    destroy_decoded(d); 
    
  }  

  
  
  g_foo_val++;
} 

int main(void) {
 
  int i = 0; 
  
  printf("Testing to patch an argument\n"); 
  
  
  for (i = 0; i < 10; i ++){ 


    foo(BEFORE_ARG_PATCH);     
    
    printf("Loop %d\n",i);
  }

  if (i == 10 && g_foo_val == 10) { 
    printf("Success\n");
    return 0;
  }
  printf("Test failed!\n"); 
  return 1;  
}
