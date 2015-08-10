
#include "patcher.h" 

/* Just skip this part if ICC  */
#if defined(__ICC) || defined(__INTEL_COMPILER) 
/* Something else may be required here */ 
#else 
  #ifndef __USE_GNU 
    #define __USE_GNU   
  #endif 
#endif 
#include <signal.h>



/* -----------------------------------------------------------------
   Globals 
   ----------------------------------------------------------------- */

long g_page_size = 0; 
size_t g_cache_lvl3_line_size = 0; 
uint64_t g_int3_interrupt_count = 0; 

const uint64_t g_int3 = 0xcc;

/* -----------------------------------------------------------------
   MACROES 
   ----------------------------------------------------------------- */ 

/* You can try this out with normal writes to memory by defined NO_CAS */
#ifdef NO_CAS 
/* A WRITE is just a regular assingment */ 
#define WRITE(addr,value)  (addr)[0] = (value) 
#else 
/* An atomic write implemented via CAS. I'm not sure this comment makes sense */
#define WRITE(addr,value)  __sync_val_compare_and_swap((addr), *(addr), (value));
#endif 

/* internally used min/max macros */ 
#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))


/* -----------------------------------------------------------------
   local helpers
   ----------------------------------------------------------------- */
bool set_page_rwe(void *addr);
uint64_t get_msb_mask(int nbytes);


/* -----------------------------------------------------------------
   CODE Internal 
   ----------------------------------------------------------------- */
 

/* Constructor function that intializes constants */ 
__attribute__((constructor))
void init_patcher() {
  /* Should be able to just #define these values 
     by investigating what arch/os we are running on. 
     Then no need for this init phase */ 
  g_page_size=sysconf(_SC_PAGESIZE);
  g_cache_lvl3_line_size=sysconf(_SC_LEVEL3_CACHE_LINESIZE); 
}


  
/* Should be even stricter about use of compiler.. 
   Not much of this will be __STRICT_ANSI__ and maybe 
   in that case we should just say "No, You cannot compile this code */

/* Invalidate cacheline containing address p */ 
/* #if defined(__ICC) || defined(__INTEL_COMPILER) || defined(__GNUG__) */
/* inline void clflush(volatile void *p) { */
/*   asm volatile ("clflush (%0)" :: "r"(p)); */
/* } */

 
/* #elif defined(__STRICT_ANSI__) */
/* #pragma message("cflush is a nop") */
/* inline void cflush(void *p) {  */
/* return;  */
/* } */
 
/* #elif defined(__GNUC__)  */
/* inline void clflush(volatile void *p) { */
/*   asm volatile ("clflush (%0)" :: "r"(p)); */
/* } */

/* #else  */
/* #pragma message("cflush is a nop") */
/* inline void cflush(void *p) {  */
/* return;  */
/* } */
/* #endif */


/* This needs another name, when I understand what its for */ 
inline bool reg_equal(_RegisterType reg1, _RegisterType reg2) {
  _RegisterType a1;
  _RegisterType a2;

  if (reg1 == reg2) return true; 

  /* I think you can subtract 16 from max(reg1,reg2) and 
     then redo the simple check above.. to replace all of the below.
     But it is a bit of a hack. */
  
  a1 = MAX(reg1,reg2);
  a2 = MIN(reg1,reg2);
  if (a1 - 16 == a2) return true;

  return false; 

}

/* -----------------------------------------------------------------
   Interrupt handlers 
   ----------------------------------------------------------------- */ 
static void empty_int3_handler(int signo, siginfo_t *inf, void* ptr) {
  return;
}


static void int3_handler(int signo, siginfo_t *inf, void* ptr) {

  //ticks start = getticks();
  ucontext_t *ucontext = (ucontext_t*)ptr;

  /* Resuming the thread after skipping the call instruction. */ 
  ucontext->uc_mcontext.gregs[REG_RIP] = (greg_t)ucontext->uc_mcontext.gregs[REG_RIP] + 4;
  /* REG_RIP is another machine and compiler specific define */ 


  g_int3_interrupt_count++;
  //ticks end = getticks();
  //g_finstrumentor_overhead += (g_int3_interrupt_overhead + end - start);

}


/* -----------------------------------------------------------------
   CODE         
   ----------------------------------------------------------------- */ 

/* Inline this function when possible */ 
inline bool set_page_rwe(void *addr) { 

  int r = mprotect((void*)((uint64_t)addr - (((uint64_t)addr)%g_page_size)), g_page_size, 
		   PROT_READ | PROT_WRITE | PROT_EXEC);
  /* need to be more sophisticated here */ 
  if (r == 0) return true; 
  else return false; 
}

/* initialize a patch site, make pages read/write/exec.
   if addr + nbytes touches more than one page, all of those are modified */ 
bool init_patch_site(void *addr, size_t nbytes){ 
  uint64_t start = 0; 
  long nb = (long)nbytes;
  bool status = true; 

  while(nb > 0) {
    status = status && set_page_rwe((void*)((uint64_t)addr + start));
    
    start = start+4096; 
    nb = nb-4096; 
  }
  return status; 
}
 
/* patch 8 bytes (64 bits) in a safe way. 
   automatically applying patcher protocol in patch_site is a straddler. */
void patch_64(void *addr, uint64_t patch_value){  
  
  int offset = (uint64_t)addr % g_cache_lvl3_line_size; 

  /* Is this a straddler patch ? */ 
  if (offset > g_cache_lvl3_line_size - 8) { 
    fprintf(stderr,"Straddler update\n");
    /* Here the patch site straddles a cache line and all atomicity 
       guarantees in relation to instruction fetch seems to go out the window */ 

    unsigned int cutoff_point = g_cache_lvl3_line_size - offset; 
    uint64_t* straddle_point = (uint64_t*)((uint8_t*)addr + cutoff_point);
    
    // uint64_t lsb_mask = get_lsb_mask(cutoff_point);
    uint64_t msb_mask = get_msb_mask(cutoff_point); 

    /* read in 8 bytes before and after the straddle point */
    uint64_t before = *(straddle_point - 1);
    uint64_t after  = *straddle_point;

    int shift_size = 8 * (8 - cutoff_point); 

    uint64_t ormask = 0xFF << shift_size;  
    uint64_t int3mask = g_int3 << shift_size; 
    
    uint64_t int3_sequence = (before & ~ormask) | int3mask; 

    /* this is the parts to keep from what was originally in memory */
    uint64_t patch_keep_before = before & (~msb_mask);
    uint64_t patch_keep_after  = after & msb_mask;
    
                                                    /*  & lsb_mask */ 
    uint64_t patch_before = patch_keep_before | ((patch_value) << shift_size);
                                                   /*  & ~lsb_mask */ 
    uint64_t patch_after =  patch_keep_after | ((patch_value) >> (8 * cutoff_point));
    /* commented out masking that becomes "shifted out" */ 

    /* implement the straddler protocol */ 
    WRITE((straddle_point - 1), int3_sequence);
    
    /* An empty delay loop that is unlikely to be optimized out 
       due to the magic asm inside */ 
    for(long i = 0; i < 1000; i++) { asm(""); }
    WRITE(straddle_point,patch_after); 
    WRITE(straddle_point-1, patch_before); 
    
  } else 
    
  /* if not a straddler perform a single write */
  WRITE((uint64_t*)addr,patch_value);
}




/* ----------------------------------------------------------------- 
   This could be replaced by a few multiplications. (6 in worst case)
   But switching a loop for a case statement is probably a zero win. 
   Also, if needed, get_lsb_mask can be obtained by a ~ from the get_msb_mask
   (although not at the same index) . 
   ----------------------------------------------------------------- */    

/* Inline this when possible */ 
inline uint64_t get_msb_mask(int nbytes) {
  switch (nbytes) {
    case 1:
      return 0xFF00000000000000;
    case 2:
      return 0xFFFF000000000000;
    case 3:
      return 0xFFFFFF0000000000;
    case 4:
      return 0xFFFFFFFF00000000;
    case 5:
      return 0xFFFFFFFFFF000000;
    case 6:
      return 0xFFFFFFFFFFFF0000;
    case 7:
      return 0xFFFFFFFFFFFFFF00;
    default:
      printf("ERROR : Invalid input to get_msb_mask\n");
      return 0;
  }
}
