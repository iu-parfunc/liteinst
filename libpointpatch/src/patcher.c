/*

  libpointpatch

  Authors: Buddhika Chamith,  Bo Joel Svensson
  year: 2015

  Should compile with:
    icpc [-std=c++11]
    icc
    gcc -std=gnu99   (because of inline assembly, stdbool)
    g++ [-std=c++11]

*/

#include "patcher.h"

#include "patchlist.h"
#include "patch_utils.h"

// #include "assembly.hpp"

#include <stdlib.h>
#include <memory.h>
#include <time.h>

#define __USE_GNU
#include <signal.h>

#if defined(PATCH_SCHED_YIELD) || defined(WAIT_SPIN_RDTSC_YIELD)
#include <sched.h>
#endif

#if defined(PATCH_TRANSACTION_XBEGIN) || defined(PATCH_TRANSACTION_XBEGIN2) || defined(PATCH_TRANSACTION_XBEGIN3)
#include <immintrin.h>
#endif



/* -----------------------------------------------------------------
   Constants
   ----------------------------------------------------------------- */

const uint8_t int3 = 0xCC;

/* -----------------------------------------------------------------
   Globals
   ----------------------------------------------------------------- */

uint64_t g_int3_interrupt_count = 0;
int g_wait_iters = 2000;

LSPatchList g_patch_list;

struct sigaction g_newact;
struct sigaction g_oldact;

#if defined(WAIT_NANOSLEEP)
struct timespec g_wait_time;
#endif


/* -----------------------------------------------------------------
   WAIT
   ----------------------------------------------------------------- */

/* Switching to reading this from an env_var */
/* #ifndef WAIT_ITERS
   #define WAIT_ITERS 400
   #endif */

#if defined(NO_WAIT)
#define WAIT()
#elif defined(WAIT_NANOSLEEP)
#define WAIT() clock_nanosleep(CLOCK_MONOTONIC,0, &g_wait_time,NULL)
#elif defined(WAIT_CPUID)
#define WAIT() asm volatile ( "cpuid" )
#elif defined(WAIT_SPIN_RDTSC)
#define WAIT() wait_spin_rdtsc()
#elif defined(WAIT_SPIN_RDTSC_YIELD)
#define WAIT() wait_spin_rdtsc_yield()
#else
#define WAIT() for(long i = 0; i < g_wait_iters; i++) { asm (""); }
#endif



/* -----------------------------------------------------------------
   MACROES
   ----------------------------------------------------------------- */

#if defined(CAS_WRITE) 
#warning "CAS WRITE" 
#define WRITE(addr,value) assert(__sync_bool_compare_and_swap((addr), *(addr), (value)))
#elif defined(ATOMIC_WRITE)
#warning "ATOMIC WRITE" 
#define WRITE(addr,value) __atomic_store_n((addr),(value),__ATOMIC_SEQ_CST)

#elif defined(LUKE_ATOMIC_WRITE) 
#warning "LUKE's atomic write" 
#define WRITE(addr, val) do {              \
    __asm volatile ("":::"memory");                 \
    *(addr) = val;                                  \
  } while (0)

#else
#warning "NONATOMIC WRITE"
#define WRITE(addr,value)  (addr)[0] = (value)
#endif  

/* internally used min/max macros */
#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

#define IS_IMMEDIATE(t)  ((t) == O_IMM || (t) == O_IMM1 || (t) == O_IMM2)


/* -----------------------------------------------------------------
   internally used
   ----------------------------------------------------------------- */
// bool set_page_rwe(void *addr, size_t nBytes);
// uint64_t get_msb_mask_64(int nbyte);
// uint64_t get_lsb_mask_64(int nbytes);
// uint32_t get_msb_mask_32(int nbytes);
// uint32_t get_lsb_mask_32(int nbytes);
bool reg_equal(_RegisterType reg1, _RegisterType reg2);


static void int3_handler(int signo, siginfo_t *inf, void* ptr);
void clflush(volatile void *p);

/* -----------------------------------------------------------------
   CODE Internal
   ----------------------------------------------------------------- */

/* Constructor function that intializes constants */
__attribute__((constructor))
void init_point_patcher() {
  /* Should be able to just #define these values
     by investigating what arch/os we are running on.
     Then no need for this init phase */
  g_page_size=sysconf(_SC_PAGESIZE);
  g_cache_lvl3_line_size=sysconf(_SC_LEVEL3_CACHE_LINESIZE);

  char *wait_str = getenv("PATCH_WAIT_TIME");
  if (wait_str != NULL) {
    g_wait_iters = atoi(wait_str);

  } /* otherwise use default */

  /* setup signal handling for straddler protocol */
  memset( &g_newact, 0, sizeof g_newact);
  g_newact.sa_sigaction = & int3_handler;
  g_newact.sa_flags = SA_SIGINFO;
  sigemptyset(& (g_newact.sa_mask));

  sigaction(SIGTRAP, &g_newact, &g_oldact);

  /* zero out patchlists */ 
  initialize_LSPatchList(&g_patch_list); 

#ifndef NDEBUG
#ifdef NO_CAS
  printf("NO_CAS VERSION OF PATCHER CODE\n");
#endif
#ifdef NO_WAIT
  printf("NO_WAIT VERSION OF PATCHER CODE\n");
#endif
#ifdef WAIT_SPIN_RDTSC
  printf("WAIT_SPIN_RDTSC VERSION OF PATCHER CODE\n");
#endif
#ifdef WAIT_SPIN_RDTSC_YIELD
  printf("WAIT_SPIN_RDTSC_YIELD VERSION OF PATCHER CODE\n");
#endif
#ifdef PATCH_FLUSH_AFTER_INT3
  printf("FLUSHES CACHE AFTER WRITING INT3\n");
#endif
#ifdef PATCH_FLUSH_CACHE
  printf("FLUSH_CACHE VERSION OF PATCHER CODE\n");
#endif
#ifdef PATCH_SCHED_YIELD
  printf("SCHED_YIELD VERSION OF PATCHER CODE\n");
#endif
#ifdef PATCH_CLEAR_CACHE
  printf("\"__builtin___clear_cache\" VERSION OF PATCHER CODE\n");
#endif
#ifdef PATCH_TRANSACTION_XBEGIN
  printf("TRANSACTION VERSION OF PATCHER CODE\n");
#endif
#ifdef PATCH_TRANSACTION_XBEGIN2
  printf("TRANSACTION2 VERSION OF PATCHER CODE\n");
#endif
#ifdef PATCH_TRANSACTION_XBEGIN3
  printf("TRANSACTION3 VERSION OF PATCHER CODE\n");
#endif
#ifdef PATCH_ATOMIC_EXCHANGE
  printf("ATOMIC_EXCHANGE VERSION OF PATCHER CODE\n");
#endif
#ifdef PATCH_ATOMIC_EXCHANGE2
  printf("ATOMIC_EXCHANGE2 VERSION OF PATCHER CODE\n");
#endif
#ifdef WAIT_NANOSLEEP
  printf("Using NANOSLEEP for wait\n");
#endif
  printf("WAIT SETTING: %d\n", g_wait_iters);
#endif  /* NDEBUG */

#ifdef WAIT_NANOSLEEP
  struct timespec res;
  clock_getres(CLOCK_MONOTONIC,&res);
  if (res.tv_sec > 0) {
    printf("BAD!\n");
    exit(EXIT_FAILURE);
  }
  printf("Clock resolution ns: %ld\n", res.tv_nsec);

  g_wait_time.tv_nsec = res.tv_nsec * g_wait_iters;
  g_wait_time.tv_sec = 0;

  printf("Wait time is set to: %ld ns\n", g_wait_time.tv_nsec);
#endif
}

__attribute__((destructor))
void destroy_point_patcher() {

  /* potentially do something here */


  return;
}

/* flush cacheline */
inline void
clflush(volatile void *p)
{
    asm volatile ("clflush (%0)" :: "r"(p));
}

/* RDTSC */
static inline uint64_t rdtsc() {
  uint64_t a, d;
  __asm__ volatile ("rdtsc" : "=a" (a), "=d" (d));
  return (d<<32) | a;
}

#ifdef WAIT_SPIN_RDTSC
#warning "Patcher: using RDTSC variant"
static inline void wait_spin_rdtsc() {
  uint64_t start = rdtsc();

  while (rdtsc() < start + g_wait_iters);

  return;
}
#endif

#ifdef WAIT_SPIN_RDTSC_YIELD
#warning "Patcher: using RDTSC_yield variant"
static inline void wait_spin_rdtsc_yield() {
  uint64_t start = rdtsc();

  while (rdtsc() < start + g_wait_iters) {
    sched_yield();
  }

  return;
}
#endif

/* -----------------------------------------------------------------
   Interrupt handlers
   ----------------------------------------------------------------- */

static void int3_handler(int signo, siginfo_t *inf, void* ptr) {
  //ticks start = getticks();
  g_int3_interrupt_count++;
  ucontext_t *ucontext = (ucontext_t*)ptr;

  uint64_t addr = (uint64_t)(ucontext->uc_mcontext.gregs[REG_RIP]-1);
  
/* #ifndef NDEBUG */
/*   fprintf(stdout,"try_finish from int3_handler\n"); */
/*   fflush(stdout); */

/*   int key = patch_list_key(addr);  */
/*   if (peek_patch(addr,&g_patch_list[key]) == NULL ||  */
/*       peek_patch(addr,&g_patch_list[key])->state == FINISHED) {  */
    
/*   fprintf(stdout,"NO PATCH TO APPLY HERE \n"); */
/*   fflush(stdout); */
/*   } */
/* #endif    */

  try_finish_patch_64((void*)addr); 
  // printf("Inside signal handler..\n");

  //while(*(uint8_t*)(ucontext->uc_mcontext.gregs[REG_RIP]-1) == 0xCC); 
  
  ucontext->uc_mcontext.gregs[REG_RIP] = addr;
  //    (greg_t)ucontext->uc_mcontext.gregs[REG_RIP] - 1;

}


/* -----------------------------------------------------------------
   CODE
   ----------------------------------------------------------------- */

/* Get the set wait time for the patching protocol */
long patch_get_wait(){
#if defined(WAIT_NANOSLEEP)
  return(g_wait_time.tv_nsec * g_wait_iters);
#elif defined (WAIT_CPUID)
  return -1;
#else
  return g_wait_iters;
#endif
}

/* is this location a straddler ? */
inline bool is_straddler_64(void *addr){

  if (!g_cache_lvl3_line_size) {
    init_point_patcher();
  }

  int offset = (uint64_t)addr % g_cache_lvl3_line_size;

  return (offset > g_cache_lvl3_line_size - 8);
}

/* If it is a straddler, where does it straddle */
/* TODO Optimize */
inline int straddle_point_64(void *addr){
  if (is_straddler_64(addr))
    return  g_cache_lvl3_line_size - (uint64_t)addr % g_cache_lvl3_line_size;
  else
    return 0;
}

bool patch_64_plus(void *addr, uint64_t patch_value) {
  int offset = (uint64_t)addr % g_cache_lvl3_line_size;

  /* Is this a straddler patch ? */
  if (offset > g_cache_lvl3_line_size - 8) {
    // fprintf(stderr,"Straddler update\n");
    /* Here the patch site straddles a cache line and all atomicity
       guarantees in relation to instruction fetch seems to go out the window */

    unsigned int cutoff_point = g_cache_lvl3_line_size - offset;
    uint64_t* straddle_point = (uint64_t*)((uint8_t*)addr + cutoff_point);
    assert((uint64_t)straddle_point % g_cache_lvl3_line_size == 0);

    uint64_t* before_ptr = straddle_point - 1;
    uint64_t start_offset = (uint8_t*)addr - (uint8_t*)before_ptr;
    uint64_t heads_marked = *before_ptr;

    // Make sure we decode some what past the 8 bytes that we are going to patch
    // in order to make sure we get disassembly right for the 8 bytes that we 
    // are interested in 
    Decoded d = decode_range(addr, (uint8_t*)addr + 20);
    _DInst* decoded  = (_DInst*) d.decoded_instructions;
    int head_offset = 0;
    for (int i=0; i < d.n_instructions; i++) {
      if (head_offset <= cutoff_point) {
        int offset = start_offset + head_offset;
        if (offset < 8) {
          ((uint8_t*)&heads_marked)[offset] = int3;
        } else {
          break;
        }
        head_offset += decoded[i].size;
      }
    }

    destroy_decoded(d);

    /* read in 8 bytes before and after the straddle point */
    uint64_t before = *before_ptr;
    uint64_t after  = *straddle_point;


    uint64_t lsb_mask = get_lsb_mask_64(cutoff_point);
    uint64_t msb_mask = get_msb_mask_64(cutoff_point);


    int shift_size = 8 * (8 - cutoff_point);

    // fprintf(stderr, "straddle_point : %X\n", *straddle_point);
    // fprintf(stderr, "after: %X\n", after);

    /* These are the parts to keep from what was originally in memory.
     * These calculations handle little endianness in x86. Raw memory
     * when stored as int types will get converted to the little endian 
     * format where leading byte becomes LSB. 
     *
     * toy example :
     * char bytes[] = {1, 0, 0, 0};
     * int n = *(int*)bytes;
     * printf("%X\n", n);
     *
     * On x86 this would print 0x00000001 though in raw memory it is stored
     * as 0x10000000 */
    uint64_t patch_keep_before = before & (~msb_mask);
    uint64_t patch_keep_after  = after & msb_mask;

    uint64_t patch_before = patch_keep_before | ((patch_value  & lsb_mask) << shift_size);
    uint64_t patch_after =  patch_keep_after | ((patch_value  & ~lsb_mask) >> (8 * cutoff_point));

    uint8_t oldFR = ((uint8_t*)addr)[0];

    if (oldFR == int3) return false;
    else if (__sync_bool_compare_and_swap(before_ptr, before, heads_marked))     {
      // printf("Cutoff point : %d\n", cutoff_point);
      // printf("Location : %p Original : %llx  Heads marked : %llx\n", before_ptr, before, heads_marked);
      //else if (__atomic_compare_exchange((uint8_t*)addr,&oldFR, &int3,false,__ATOMIC_SEQ_CST,__ATOMIC_SEQ_CST)) { 
#ifdef PATCH_FLUSH_AFTER_INT3
      clflush(addr);
#endif
      
      WAIT();

      WRITE(straddle_point,patch_after);
      WAIT();
      WRITE((straddle_point-1), patch_before);
      //WRITE((uint64_t*)addr,patch_value);
      return true;
    }
  }

  /* if not a straddler perform a single write */
  WRITE((uint64_t*)addr,patch_value);
  return true;
}

/* patch 8 bytes (64 bits) in a safe way.
   automatically applying patcher protocol in patch_site is a straddler. */
bool patch_64(void *addr, uint64_t patch_value){

  int offset = (uint64_t)addr % g_cache_lvl3_line_size;

  /* Is this a straddler patch ? */
  if (offset > g_cache_lvl3_line_size - 8) {
    // fprintf(stderr,"Straddler update\n");
    /* Here the patch site straddles a cache line and all atomicity
       guarantees in relation to instruction fetch seems to go out the window */

    unsigned int cutoff_point = g_cache_lvl3_line_size - offset;
    uint64_t* straddle_point = (uint64_t*)((uint8_t*)addr + cutoff_point);
    assert((uint64_t)straddle_point % g_cache_lvl3_line_size == 0);

    uint64_t lsb_mask = get_lsb_mask_64(cutoff_point);
    uint64_t msb_mask = get_msb_mask_64(cutoff_point);

    /* read in 8 bytes before and after the straddle point */
    uint64_t before = *(straddle_point - 1);
    uint64_t after  = *straddle_point;

    int shift_size = 8 * (8 - cutoff_point);

    // fprintf(stderr, "straddle_point : %X\n", *straddle_point);
    // fprintf(stderr, "after: %X\n", after);

    /* These are the parts to keep from what was originally in memory.
     * These calculations handle little endianness in x86. Raw memory
     * when stored as int types will get converted to the little endian 
     * format where leading byte becomes LSB. 
     *
     * toy example :
     * char bytes[] = {1, 0, 0, 0};
     * int n = *(int*)bytes;
     * printf("%X\n", n);
     *
     * On x86 this would print 0x00000001 though in raw memory it is stored
     * as 0x10000000 */
    uint64_t patch_keep_before = before & (~msb_mask);
    uint64_t patch_keep_after  = after & msb_mask;

    uint64_t patch_before = patch_keep_before | ((patch_value  & lsb_mask) << shift_size);
    uint64_t patch_after =  patch_keep_after | ((patch_value  & ~lsb_mask) >> (8 * cutoff_point));
    

    /* -----------------------------------------------------------------
       NON_THREADSAFE
       ----------------------------------------------------------------- */
#if defined(NON_THREADSAFE_PATCHING) /* racy patching */
    /* implement the straddler protocol */
    ((uint8_t*)addr)[0] = int3;

    WAIT();
    WRITE(straddle_point,patch_after);
    WRITE((straddle_point-1), patch_before);
    return true;

    /* -----------------------------------------------------------------
       CLFLUSH BASED
       ----------------------------------------------------------------- */
#elif defined(PATCH_FLUSH_CACHE)
    uint8_t oldFR = ((uint8_t*)addr)[0];

    if (oldFR == int3) return false;
    else if (__sync_bool_compare_and_swap((uint8_t*)addr, oldFR, int3)) {

      clflush((void*)addr);
      WRITE(straddle_point,patch_after);
      clflush((void*)straddle_point);
      WRITE((straddle_point-1), patch_before);
      clflush((void*)(straddle_point-1));

      return true;
    }
    else return false;

    /* -----------------------------------------------------------------
       SCHED_YIELD
       ----------------------------------------------------------------- */
#elif defined(PATCH_SCHED_YIELD)
    uint8_t oldFR = ((uint8_t*)addr)[0];

    if (oldFR == int3) return false;
    else if (__sync_bool_compare_and_swap((uint8_t*)addr, oldFR, int3)) {

      sched_yield();

      WRITE(straddle_point,patch_after);
      WRITE((straddle_point-1), patch_before);

      return true;
    }
    else return false;

    /* -----------------------------------------------------------------
       CLEAR_CACHE
       ----------------------------------------------------------------- */
#elif defined(PATCH_CLEAR_CACHE)
uint8_t oldFR = ((uint8_t*)addr)[0];

    if (oldFR == int3) return false;
    else if (__sync_bool_compare_and_swap((uint8_t*)addr, oldFR, int3)) {

      __builtin___clear_cache((char*)addr,((char*)addr)+8);

      WRITE(straddle_point,patch_after);
      WRITE((straddle_point-1), patch_before);

      return true;
    }
    else return false;

   /* -----------------------------------------------------------------
       TRANSACTION
       ----------------------------------------------------------------- */
#elif defined(PATCH_TRANSACTION_XBEGIN)
    if (_xbegin() == _XBEGIN_STARTED) {
      ((uint64_t*)addr)[0] = patch_value;
	// straddle_point[0] = patch_after;
	// (straddle_point-1)[0] = patch_before;
      _xend();
      return true;
    } else {
      return false;
    }

   /* -----------------------------------------------------------------
       TRANSACTION2
      ----------------------------------------------------------------- */
#elif defined(PATCH_TRANSACTION_XBEGIN2)
    uint8_t oldFR = ((uint8_t*)addr)[0];

    if (oldFR == int3) return false;

    else {
      if (_xbegin() == _XBEGIN_STARTED) {
	((uint8_t*)addr)[0] = int3;
	_xend();

	((uint64_t*)addr)[0] = patch_value;

	  // WRITE(straddle_point,patch_after);
	  // WRITE((straddle_point-1), patch_before);

	return true;


      } else {
	return false;
      }
    }
   /* -----------------------------------------------------------------
       TRANSACTION3
      ----------------------------------------------------------------- */
#elif defined(PATCH_TRANSACTION_XBEGIN3)
    if (_xbegin() == _XBEGIN_STARTED) {
      uint8_t oldFR = ((uint8_t*)addr)[0];

      if (oldFR == int3) {
	_xend();
	return false;
      }
      ((uint8_t*)addr)[0] = int3;
      _xend();

      WRITE(straddle_point,patch_after);
      WRITE((straddle_point-1), patch_before);
      return true;
    } else {
      return false;
    }

    /* -----------------------------------------------------------------
       atomic-exchange
       ----------------------------------------------------------------- */
#elif defined(PATCH_ATOMIC_EXCHANGE)
    __sync_lock_test_and_set((uint64_t*)addr,patch_value);

    return true;
#elif defined(PATCH_ATOMIC_EXCHANGE2)

    uint8_t oldfr = __sync_lock_test_and_set((uint8_t*)addr,int3);

    if (oldfr == int3) {
      /* someone already initiated patching here */
      return false;
    } else {
      WRITE(straddle_point,patch_after);
      WRITE((straddle_point-1), patch_before);
      return true;
    }

    /* -----------------------------------------------------------------
       WAIT BASED THREADSAFE PATCHER
       ----------------------------------------------------------------- */
#else /* Threadsafe patching */
#warning "Running DEFAULT VERSION OF PATCHER"
    uint8_t oldFR = ((uint8_t*)addr)[0];

    if (oldFR == int3) return false;
    else if (__sync_bool_compare_and_swap((uint8_t*)addr, oldFR, int3))     {
      //else if (__atomic_compare_exchange((uint8_t*)addr,&oldFR, &int3,false,__ATOMIC_SEQ_CST,__ATOMIC_SEQ_CST)) { 
#ifdef PATCH_FLUSH_AFTER_INT3
      clflush(addr);
#endif
      
      WAIT();
      //__atomic_thread_fence(__ATOMIC_SEQ_CST);

      //__sync_lock_test_and_set((uint64_t*)straddle_point,patch_after);
      //__sync_lock_test_and_set((uint64_t*)(straddle_point-1),patch_before);
      WRITE(straddle_point,patch_after);
      WAIT();
      WRITE((straddle_point-1), patch_before);
      //WRITE((uint64_t*)addr,patch_value);
      return true;
    }
    else return false;
#endif
  }

  /* if not a straddler perform a single write */
  WRITE((uint64_t*)addr,patch_value);
  return true;
}


/* -----------------------------------------------------------------
   Patching
   ----------------------------------------------------------------- */

/* patch 4 bytes (32 bits) in a safe way.
       automatically applying patcher protocol in patch_site is a straddler. */
bool patch_32(void *addr, uint32_t patch_value){

  int offset = (uint64_t)addr % g_cache_lvl3_line_size;

  /* Is this a straddler patch ? */
  if (offset > g_cache_lvl3_line_size - 4) {
    /* fprintf(stderr,"Straddler update\n"); */
    /* Here the patch site straddles a cache line and all atomicity
       guarantees in relation to instruction fetch seems to go out the window */

    unsigned int cutoff_point = g_cache_lvl3_line_size - offset;
    uint32_t* straddle_point = (uint32_t*)((uint8_t*)addr + cutoff_point);

    uint32_t lsb_mask = get_lsb_mask_32(cutoff_point);
    uint32_t msb_mask = get_msb_mask_32(cutoff_point);

    /* read in 8 bytes before and after the straddle point */
    uint32_t before = *(straddle_point - 1);
    uint32_t after  = *straddle_point;

    int shift_size = 8 * (4 - cutoff_point);

    /* this is the parts to keep from what was originally in memory */
    uint32_t patch_keep_before = before & (~msb_mask);
    uint32_t patch_keep_after  = after & msb_mask;

    uint32_t patch_before = patch_keep_before | ((patch_value  & lsb_mask) << shift_size);
    uint32_t patch_after =  patch_keep_after | ((patch_value & ~lsb_mask) >> (8 * cutoff_point));


    /* implement the straddler protocol */
#if defined(NON_THREADSAFE_PATCHING)
    ((uint8_t*)addr)[0] = int3;
    /* An empty delay loop that is unlikely to be optimized out
       due to the magic asm inside */
  /* #ifndef NO_WAIT  */
  /*   for(long i = 0; i < WAIT_ITERS; i++) { asm (""); } */
  /* #endif */
    WAIT();
    WRITE(straddle_point,patch_after);
    WRITE(straddle_point-1, patch_before);
    return true;
#elif defined(PATCH_SIGILL)
    /* Threadsafe variant using illegal instruction */

#else
    /* Threadsafe variant using trap*/
    uint8_t oldFR = ((uint8_t*)addr)[0];

    if (oldFR == int3) return false;
    else if (__sync_bool_compare_and_swap((uint8_t*)addr, oldFR, int3))     {
      WAIT();
      WRITE(straddle_point,patch_after);
      WRITE((straddle_point-1), patch_before);
      return true;
    }
    else return false;

#endif
  }

  /* if not a straddler perform a single write */
  WRITE((uint32_t*)addr,patch_value);
  return true;
}

/* ----------------------------------------------------------------- 
   Async Patching
   ----------------------------------------------------------------- */

bool async_patch_64(void *addr, uint64_t patch_value) { 
  // printf("[patcher] Asynchronous patching probe site %p\n", (char*) addr);
  
  int offset = (uint64_t)addr % g_cache_lvl3_line_size; 
  
  /* Is this a straddler patch ? */ 
  if (offset > g_cache_lvl3_line_size - 8) { 
    /* fprintf(stderr,"Straddler update\n"); */ 
    /* Here the patch site straddles a cache line and all atomicity 
       guarantees in relation to instruction fetch seems to go out the window */ 
    
    unsigned int cutoff_point = g_cache_lvl3_line_size - offset; 
    uint64_t* straddle_point = (uint64_t*)((uint8_t*)addr + cutoff_point);
    
    uint64_t lsb_mask = get_lsb_mask_64(cutoff_point);
    uint64_t msb_mask = get_msb_mask_64(cutoff_point); 
     
    /* read in 8 bytes before and after the straddle point */
    uint64_t before = *(straddle_point - 1);
    uint64_t after  = *straddle_point;
    
    int shift_size = 8 * (8 - cutoff_point); 
      
    /* this is the parts to keep from what was originally in memory */
    uint64_t patch_keep_before = before & (~msb_mask);
    uint64_t patch_keep_after  = after & msb_mask;
    
    uint64_t patch_before = patch_keep_before | ((patch_value  & lsb_mask) << shift_size);
    uint64_t patch_after =  patch_keep_after | ((patch_value  & ~lsb_mask) >> (8 * cutoff_point));
    
    
    uint8_t oldFR = ((uint8_t*)addr)[0]; 
    

    
    /* Create async patch job */
    if (oldFR == int3) return false; 
    else if (__sync_bool_compare_and_swap((uint8_t*)addr, oldFR, int3)) {     

      uint64_t timestamp = rdtsc(); 
    
      add_patch((uint64_t)addr,patch_before,patch_after,timestamp,g_patch_list); 
      return true; 
    }        
    else {       
      return false; 
    }

  } 

  /* if not a straddler perform a single write */
  WRITE((uint32_t*)addr,patch_value);
  return true;

}

void try_finish_patch_64(void *addr){ 
  
  struct Patch *p; 

  /* Clean this up! */
  int key = patch_list_key((uint64_t)addr); 
  p = peek_patch((uint64_t)addr,&g_patch_list[key]); 

  if (p == NULL) { 
    return; 
  }
  
  if (__sync_bool_compare_and_swap(&p->lock,0,1)) { 
    /* got the patch lock. attempt to finish it */ 

    int offset = (uint64_t)addr % g_cache_lvl3_line_size;
    unsigned int cutoff_point = g_cache_lvl3_line_size - offset; 
    uint64_t* straddle_point = (uint64_t*)((uint8_t*)addr + cutoff_point);
    
    if (p->state == FINISHED) { 
      /* already finished, return */

      p->lock = 0; 
      return; 
    }
  
    if (p->state == FIRST_WAIT) { 
      /* the int3 has been written and we wait for enough 
	 time to pass to allow the write of BACK */
      
      if (rdtsc() > (p->timestamp + g_wait_iters)) { 
    
	/* write back part and update timestamp */ 
	WRITE(straddle_point,p->back); 
	p->timestamp = rdtsc(); 
	p->state = SECOND_WAIT; 
	p->lock = 0; 
	return; 
      }  
      
      /* There is still more time to wait. return */ 

      p->lock = 0; 
      return; 
      
    }
    if (p->state == SECOND_WAIT) { 
      
      /* int3 and BACK has been written, wait to write FRONT */ 
      
      if (rdtsc() > (p->timestamp + g_wait_iters)) { 	
	/* write front part and update timestamp */ 
	
	p->timestamp = 0; 
	p->state = FINISHED; 
	p->lock = 0; 
	WRITE((straddle_point-1),p->front); 
	return; 
      }
      /* There is still more time to wait. return */ 
      p->lock = 0; 
      return;      
      
    }
  
    assert(false);
  
  }
  return; 
}
    
     

void finish_patch_64(void *addr){ 
  assert(false); 
  /* 
  int key = patch_list_key(addr); 

#ifndef NDEBUG 
  printf("finish_patch_64: Enter\n");
#endif 


  struct Patch *p; 
  int retval;
  while ((retval = remove_patch((uint64_t)addr,&g_patch_list[key],&p)) == 
    PATCH_LIST_CONTENTION) { 
#ifndef NDEBUG 
    printf("finish_patch_64: contention\n");
#endif 
    
  
  }
  
  if (retval == PATCH_NOT_FOUND) {
#ifndef NDEBUG 
    printf("finish_patch: patch not found\n"); 
#endif 
    return; // patch already applied 
    }
// we got a patch to apply
  if (retval == PATCH_REMOVED) { 
#ifndef NDEBUG 
    printf("finish_patch_64: found patch data\n");
#endif 
    
    int offset = (uint64_t)addr % g_cache_lvl3_line_size;
    unsigned int cutoff_point = g_cache_lvl3_line_size - offset; 
    uint64_t* straddle_point = (uint64_t*)((uint8_t*)addr + cutoff_point);
      

    
    
    while (rdtsc() < (p->timestamp + g_wait_iters));
    WRITE(straddle_point,p->back); 
    WRITE((straddle_point-1),p->front); 
    free(p); 
    return; 
  }
  */ 
  
}



/* -----------------------------------------------------------------
   Parameter patching.
   ----------------------------------------------------------------- */

/* Maybe should take a pointer to a Decoded */
void destroy_decoded(Decoded d) {
  free(d.decoded_instructions);
}

Decoded decode_range(void* start_addr, void* end_addr){
  uint64_t n_decode_bytes = (uint64_t)((uint64_t)end_addr - (uint64_t)start_addr);

  Decoded d;

  /* why * 2 * n_decode_bytes */
  _DInst* result = (_DInst*)malloc(sizeof(_DInst) * 2 * n_decode_bytes);
  unsigned int instruction_count = 0;

  _CodeInfo ci = {0}; /* another modernity */
  ci.code = (uint8_t*)start_addr;
  ci.codeLen = n_decode_bytes;
  ci.dt = Decode64Bits;   /* here we make this AMD64 specific ? */
  ci.codeOffset = 0x100000;

  _DecodeResult res = distorm_decompose(&ci, result, n_decode_bytes, &instruction_count);
  if (res != DECRES_SUCCESS) {
    free(result);
    result = NULL;

    d.decoded_instructions = NULL;
    d.n_instructions = 0;

    return d;
  }

  d.decoded_instructions = result;
  d.n_instructions = instruction_count;
  return d;

}

/* Check if one register is a sub-part of another (EAX/RAX for example) */

inline bool reg_equal(_RegisterType reg1, _RegisterType reg2) {
  _RegisterType a1;
  _RegisterType a2;

  if (reg1 == reg2) return true;

  // I think you can subtract 16 from max(reg1,reg2) and
  //   then redo the simple check above.. to replace all of the below.
  //   But it is a bit of a hack.

  a1 = MAX(reg1,reg2);
  a2 = MIN(reg1,reg2);
  if (a1 - 16 == a2) return true;

  return false;
}

/* find the closest point to "end_addr" where the register reg is set
   via a mov instruction or return -1 if no such point found.
   returning the distance if a setter is found.
   This distance should be subtracted from the return address of the call
   to arrive at the setter. */
int64_t find_reg_setter(_RegisterType reg, Decoded d){


  _DInst* decoded = d.decoded_instructions;
  unsigned int instruction_count = d.n_instructions;

  uint64_t ptr_size = 0;
  int64_t setter_offset = 0; /* very unlikely that this is a 64bit quantity */
  _RegisterType intermediate_reg = 0;

  bool setter_found = false;
  bool set_by_intermediate = false;


  /* Search through instructions from the end */
  for (int i = instruction_count - 1; i >= 0; i --) {
    /* skip non-decodable */
    if (decoded[i].flags == FLAG_NOT_DECODABLE) continue;

    /* increment a ptr, the size of the current instruction in memory */
    ptr_size += decoded[i].size;

    if (decoded[i].opcode == I_MOV) {

      if (!set_by_intermediate &&
	        decoded[i].ops[0].type == O_REG &&
	        decoded[i].ops[0].index == reg) {
	        if (IS_IMMEDIATE(decoded[i].ops[1].type)) {
	          /* an mov-immediate to reg was found here */
	          setter_offset = ptr_size;
	          setter_found = true;
	          break;
	        } else if (decoded[i].ops[1].type == O_REG) {
	          /* reg is set to the contents of another register */
	          /* DANGER ZONE!!!! */
	          set_by_intermediate = true;
	          intermediate_reg = decoded[i].ops[1].index;
	        }

      } else if (set_by_intermediate &&
		    decoded[i].ops[0].type == O_REG &&
		    (reg_equal(intermediate_reg,decoded[i].ops[0].index))) {
	      if (IS_IMMEDIATE(decoded[i].ops[1].type)) {
	        setter_offset = ptr_size;
	        setter_found = true;
	        break;
	      }  else if (decoded[i].ops[1].type == O_REG) {
	        /* yet another level of intermediate register assignment */
	        /* MEGA DANGER ZONE */
	        intermediate_reg = decoded[i].ops[1].index;
	      }
      }
    }
  }

  if (setter_found)
    return setter_offset;
  else
    return -1;


}
