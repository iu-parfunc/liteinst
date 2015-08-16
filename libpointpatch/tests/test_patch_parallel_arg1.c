/* 
    Test desc: 
    tests parallel updates of a argument setter site. 
     - One thread repeatedly writes ARG1. 
     - One thread repeatedly writes ARG2.

    test is successful if loop runs to completion 
    and only ARG1 and ARG2 are ever observed as arguments to foo. 
    
     

*/ 
#include <stdio.h> 
#include <memory.h>
#include <stdint.h> 
#include <stdlib.h>

#include <patcher.h>

#if defined(__ICC) || defined(__INTEL_COMPILER) 

#else 
#ifndef __USE_GNU 
#define __USE_GNU   
#endif 
#endif 
#include <pthread.h> 

#define ITERS 1000000

unsigned long g_foo_val = 0; 
unsigned long  g_bar_val = 0; 

/* control */
volatile int g_running = true; 
volatile bool g_first_run = true;


/* patch info */
uint64_t g_setter_addr = 0; 
uint64_t g_orig_arg_setter = 0;  /* original instr */
uint64_t g_alt_arg_setter = 0; 

/* test related */ 
unsigned long g_arg1_counter = 0; 
unsigned long g_arg2_counter = 0; 

/* different possible arguments */ 
#define ARG1 10
/* This one is set at call site and copied into g_orig_arg_setter at init */

#define ARG2       1337
#define ARG2_PATCH 0x00000000000539bf 
/* arg2 is 1337  bf is the mov opcode*/

/* proto */ 
int main(void);

/* ----------------------------------------------------------------- */
void foo(int apa) { 
  
  /* Test for correctness */ 

  if (!(apa == ARG1 || apa == ARG2) ){ 
    printf("Incorrect arg: %d\n",apa);
    exit(EXIT_FAILURE); 
  }

  /* collect statistics for further sanity checks */
  if (apa == ARG1) g_arg1_counter ++; 
  if (apa == ARG2) g_arg2_counter ++; 
  
  
  if (g_first_run) { 
    /* do init stuff */ 
    uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
    uint8_t* call_addr = (uint8_t*)addr - 5;
    
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
    g_setter_addr = (uint64_t)patch_site;
    g_orig_arg_setter = *(uint64_t*)patch_site;
    uint64_t keep_mask = 0xFFFFFF0000000000; 

    g_alt_arg_setter = (g_orig_arg_setter & keep_mask) | ARG2_PATCH;
    
    init_patch_site(patch_site, 8);
    

    g_first_run = false; 
  }
  

  g_foo_val++;
} 



void activator(int *arg) { 
  
  while (g_first_run); /* wait until setup phase is done */ 
  
  while(g_running) { 
    
    patch_64((void*)g_setter_addr, g_orig_arg_setter);
    
    pthread_yield(); 
  }  
} 

void deactivator(int *arg) { 

  while (g_first_run); /* wait until setup phase is done */ 
  
  while (g_running) { 

    patch_64((void*)g_setter_addr, g_alt_arg_setter);
    
    pthread_yield(); 
  }
} 


int main(void) {

  pthread_t thread1, thread2; 
  int r1,r2; 
  
  unsigned long it = 0; 
  
  pthread_create(&thread1,
		 NULL, 
		 (void *) activator, 
		 (void *) &r1);

  pthread_create(&thread2,
		 NULL, 
		 (void *) deactivator, 
		 (void *) &r2); 

   
  printf("Testing parallel updates to an argument setter site as it is being executed.\n"); 
  
  for (it = 0; it < ITERS; it ++) { 
    foo(ARG1);   
  }
  g_running = false; 
  
  pthread_join(thread1, NULL); 
  pthread_join(thread2, NULL); 
  
  printf("function foo executed %ld times.\n",g_foo_val); 


  printf("arg1: %ld, arg2: %ld\nsum: %ld, Targ: %d\n",
	 g_arg1_counter,
	 g_arg2_counter,
	 g_arg1_counter + g_arg2_counter,
	 ITERS);
  
  if ( g_arg1_counter + g_arg2_counter == ITERS) { 
    if (g_arg1_counter == 0 || g_arg2_counter == 0) { 
      printf("*** WARNING ***\nOne of the possible args was never observed\n*** WARNING ***\n");
    }
    printf("Success\n"); 
    return 0; 
  } else { 
    printf("Failure\n");
    return 1; 
  }

  
  /* if test reaches this point without crashing it is 
     considered a success */ 

 

}
