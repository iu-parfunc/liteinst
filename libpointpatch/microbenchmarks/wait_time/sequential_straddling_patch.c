/* 
    microbench desc: 

*/ 
#include <stdio.h> 
#include <memory.h>
#include <stdint.h> 
#include <stdlib.h>
#include <limits.h>
#include <time.h>
#include <patcher.h>
#include <inttypes.h>

#ifndef ITERS
#define ITERS 10000000
#endif 
#define NS_PER_SEC 1000000000

unsigned long g_foo_val = 0; 
unsigned long g_bar_val = 0; 

/* control */
volatile int g_running = true; 
volatile bool g_first_run = true;


/* patch info */
uint64_t g_call_addr = 0; 
uint64_t g_orig_call = 0;  /* original instr */
uint64_t g_call_bar_patch = 0;      /* replacement isntr sequence */

/* globals */ 
uint8_t* fun = NULL; 
unsigned int start_addr = 0;


uint64_t ellapsed_ns(struct timespec *t1, struct timespec *t2) { 
  uint64_t diff = (t2->tv_sec * NS_PER_SEC + t2->tv_nsec) - 
                  (t1->tv_sec * NS_PER_SEC + t1->tv_nsec); 
  
  return diff; 
} 

typedef unsigned long long ticks;

static __inline__ ticks getticks(void)
{
     unsigned a, d;
     asm("cpuid");
     asm volatile("rdtsc" : "=a" (a), "=d" (d));

     return (((ticks)a) | (((ticks)d) << 32));
}

void bar(void)  {
  
  g_bar_val++; 
} 

void foo(void) { 
  
  if (g_first_run) { 
    /* do init stuff */ 
    uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
    g_call_addr = (uint64_t)((uint8_t*)addr - 5);
    
    init_patch_site((void*)g_call_addr, 8);
    
    g_orig_call = *(uint64_t*)g_call_addr;
    
       
    uint64_t tmp = 0x00000000000000e8;
	
    uint64_t bar_addr = ((uint64_t)bar - (uint64_t)addr); 
   
    uint64_t keep_mask = 0xFFFFFF0000000000; 

    /*printf("call_instr: %lx\n",tmp);*/
    /*printf("bar_addr:   %lx\n",bar_addr)*/; 
	   
    tmp = (tmp | (bar_addr << 8)) & ~keep_mask;
    /*printf("call_bar: %lx\n",tmp);*/
	   
    
    g_call_bar_patch = (g_orig_call & keep_mask) | tmp;
    /*printf("call_bar_patch: %lx\n",g_call_bar_patch);*/
    

    g_first_run = false; 
  }
  
  //printf("Running foo %d\n", apa); 

  g_foo_val++;
}


int main(int argc, char** argv) {
  struct timespec time1, time2; 
  
  FILE *fp; 
  
  if (argc < 3) { 
    printf("Provide 2 arguments Outfile and straddler-location\n"); 
    exit(EXIT_FAILURE); 
  }
  
  fp = fopen(argv[1], "a");
  
  fun=(uint8_t*)malloc(1024); 
  memset(fun,0x90,1024); 

  /* where within the call should the straddler occur */ 
  int call_straddler_point = 1;
  
  call_straddler_point = atoi(argv[2]);

  /*printf("Setting straddler point at %d (distance in byte into the patch site)\n",call_straddler_point);*/ 

  /* find a straddling position within fun */
  uint64_t fun_address = (uint64_t)fun; 
  size_t cache_line_size=sysconf(_SC_LEVEL3_CACHE_LINESIZE);

  /* how many bytes into a cache line does the fun block begin */ 
  unsigned int fun_offset = fun_address % cache_line_size; 
  /*printf("FUN_OFFSET = %d\n",fun_offset);*/
  
  unsigned int closest_straddler_offset = (cache_line_size - fun_offset);

  start_addr = (closest_straddler_offset - 4) - call_straddler_point;
  /*printf("start_addr = %d\n",start_addr);*/
  /*printf("call_instr_offset = %d\n",start_addr + fun_offset + 4);*/
  /*printf("cache_line_offset = %d\n",start_addr + fun_offset); */
   

  uint32_t* addr = (uint32_t*)(&fun[start_addr+5]); 

  addr[0] = (uint32_t)((uint64_t)( (uint64_t)foo - (uint64_t)&fun[start_addr + 9]) );
  

  /* generate some code containing a call at a straddling location */ 
  fun[start_addr] = 0x55;     /* push %rbp */
  fun[start_addr + 1] = 0x48; /* mov %rsp, %rbp */
  fun[start_addr + 2] = 0x89; 
  fun[start_addr + 3] = 0xe5; 
  fun[start_addr + 4] = 0xe8; /* Call instr */ 
  /* Address */
  fun[start_addr + 9] = 0x5d; /* pop %rbp */
  fun[start_addr + 10] = 0xc3; 
  

  init_patch_site((void*)&fun[start_addr],1024); 
  
  /* the call site that will be patched */
  ((void (*)(void ))&fun[start_addr])(); 
  
  /* patch and time */
  
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID,&time1);
  ticks t1 = getticks(); 
  for (long i = 0; i < ITERS; i ++) { 
    
    patch_64((void*)g_call_addr, g_orig_call);
    patch_64((void*)g_call_addr, g_call_bar_patch);
    
  }
  ticks t2 = getticks(); 
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID,&time2);
 
  /*printf("patchings: %d\n", ITERS*2); */
  /*printf("RDTSC: %lld\n",t2 - t1); */
  fprintf(fp,"%d, %d, %lld, %f, %f\n", patch_get_wait()
	  ,ITERS*2
	  , t2 - t1
	  , (double)(t2-t1)/(ITERS*2)
	  , (double)ellapsed_ns(&time1,&time2) / NS_PER_SEC);
	  
  
  /* if test reaches this point without crashing it is 
     considered a success */ 
  
  fclose(fp);
 

}
