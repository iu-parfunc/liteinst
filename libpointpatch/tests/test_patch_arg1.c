/* 
   Test desc: 
   
   Test of the decoding and setter finding functionality: 
      - decode_range
      - find_reg_setter
   as well as the patching functionality. 
 
   Function "void foo(int apa)" is executed 10 times in a loop. 
   Argument BEFORE_ARG_PATCH is always passed to foo. 
   
   when foo runs for the 5th time, it patches its own argument setter site 
   with with AFTER_ARG_PATCH. 

   Test is a success if for iteration <= 5 arg to foo is BEFORE_ARG_PATCH 
   and at iteration > 5 arg to foo is AFTER_ARG_PATCH 
   and foo has been in total executed 10 times. 
    
*/ 


#include <stdio.h> 
#include <memory.h>
#include <stdint.h> 
#include <stdlib.h>

#include <patcher.h>

int main(void);

int g_foo_val = 0; 

#define AFTER_ARG_PATCH 1337            
#define PATCH_HEX       0x0000000000053900 /*first byte should be OPCODE */
#define BEFORE_ARG_PATCH 10

__attribute__((noinline))
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
      exit(EXIT_FAILURE);
    }
      
    
    bool is_edi = false; 
    /* Looking for setter of EDI or RDI */ 
    int64_t setter = find_reg_setter(R_EDI,d);
    if (setter == -1) { 
      setter = find_reg_setter(R_RDI,d);
      if (setter == -1) {
	printf("error finding setter\n");
	exit(EXIT_FAILURE);
      }
    } else is_edi = true; 
    printf("setter found at offset %ld\n",setter);
    printf("setter of %s was found.\n",is_edi ? "edi" : "rdi"); 
    
    
    void *patch_site = (void*)((uint64_t)call_addr - setter);
    uint64_t orig =  *(uint64_t*)patch_site;
    /* Also keep the opcode, its not always mov-edi */
    uint64_t keep_mask = 0xFFFFFF00000000FF; 

    uint64_t patch = (orig & keep_mask) | PATCH_HEX;
    
    init_patch_site(patch_site, 8);
    patch_64(patch_site, patch);


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
