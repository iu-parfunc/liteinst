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
   Globals
   ----------------------------------------------------------------- */

long g_page_size = 0;
size_t g_cache_lvl3_line_size = 0;
uint64_t g_int3_interrupt_count = 0;
int g_wait_iters = 2000;

struct sigaction g_newact;
struct sigaction g_oldact;

#if defined(WAIT_NANOSLEEP)
struct timespec g_wait_time;
#endif

/* -----------------------------------------------------------------
   Constants
   ----------------------------------------------------------------- */

const uint8_t int3 = 0xCC;

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
#elif defined(NONATOMIC_WRITE)
#warning "NONATOMIC WRITE" 
#define WRITE(addr,value)  (addr)[0] = (value)

#elif defined(LUKE_ATOMIC_WRITE) 
#warning "LUKE's atomic write" 
#define WRITE(addr, val) do {              \
    __asm volatile ("":::"memory");                 \
    *(addr) = val;                                  \
  } while (0)

#else
#warning "ATOMIC WRITE"
#define WRITE(addr,value) __atomic_store_n((addr),(value),__ATOMIC_SEQ_CST)
#endif  

/* internally used min/max macros */
#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

#define IS_IMMEDIATE(t)  ((t) == O_IMM || (t) == O_IMM1 || (t) == O_IMM2)


/* -----------------------------------------------------------------
   internally used
   ----------------------------------------------------------------- */
bool set_page_rwe(void *addr, size_t nBytes);
uint64_t get_msb_mask_64(int nbytes);
uint64_t get_lsb_mask_64(int nbytes);
uint32_t get_msb_mask_32(int nbytes);
uint32_t get_lsb_mask_32(int nbytes);
bool reg_equal(_RegisterType reg1, _RegisterType reg2);

static void int3_handler(int signo, siginfo_t *inf, void* ptr);
void clflush(volatile void *p);

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
void destroy_patcher() {

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

  // fprintf(stdout, "INT3 HNDLR RET ADDR: %p\n", (void*)ucontext->uc_mcontext.gregs[REG_RIP]);

  /* Resuming the thread after skipping the call instruction. */
  //ucontext->uc_mcontext.gregs[REG_RIP] = (greg_t)ucontext->uc_mcontext.gregs[REG_RIP] + 4;
  

  //  uint64_t word=*(uint64_t*)(ucontext->uc_mcontext.gregs[REG_RIP]-1);
  //while (((uint8_t*)&word)[0] == 0xCC) { 
  //  word=*(uint64_t*)(ucontext->uc_mcontext.gregs[REG_RIP]-1);
    //  fprintf(stderr,"IT HAPPEND\n");
      
    //fflush(stderr);
  //}
  while(*(uint8_t*)(ucontext->uc_mcontext.gregs[REG_RIP]-1) == 0xCC); 
  
  ucontext->uc_mcontext.gregs[REG_RIP] = 
    (greg_t)ucontext->uc_mcontext.gregs[REG_RIP] - 1;
  //asm volatile ( "cpuid" );

  /* REG_RIP is another machine and compiler specific define */
  
  //ticks end = getticks();
  //g_finstrumentor_overhead += (g_int3_interrupt_overhead + end - start);
}


/* -----------------------------------------------------------------
   CODE
   ----------------------------------------------------------------- */

/* Inline this function when possible */
inline bool set_page_rwe(void *addr,size_t nbytes) {

  uint64_t start = (uint64_t)addr - (((uint64_t)addr)%g_page_size);


  uint64_t offset_into_page = (((uint64_t)addr)%g_page_size);
  size_t bytes = offset_into_page + nbytes; /* too touch page 2 or n...  */
  //printf("Offset into page %ld \n", offset_into_page);
  //printf("Setting prot for %d bytes\n", bytes);

  int r = mprotect((void*)start, bytes,
		   PROT_READ | PROT_WRITE | PROT_EXEC);
  /* need to be more sophisticated here */
  if (r == 0) return true;
  else return false;
}

/* initialize a patch site, make pages read/write/exec.
   if addr + nbytes touches more than one page, all of those are modified */
bool init_patch_site(void *addr, size_t nbytes){
  /* uint64_t start = 0;  */
  /* long nb = (long)nbytes; */
  bool status = false;

  status = set_page_rwe(addr,nbytes);

  return status;
}

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

/* patch 8 bytes (64 bits) in a safe way.
   automatically applying patcher protocol in patch_site is a straddler. */
bool patch_64(void *addr, uint64_t patch_value){

  int offset = (uint64_t)addr % g_cache_lvl3_line_size;

  /* Is this a straddler patch ? */
  if (offset > g_cache_lvl3_line_size - 8) {
    /* fprintf(stderr,"Straddler update\n"); */
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

    /* this is the parts to keep from what was originally in memory */
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


/* -----------------------------------------------------------------
   This could be replaced by a few multiplications. (6 in worst case)
   But switching a loop for a case statement is probably a zero win.
   Also, if needed, get_lsb_mask can be obtained by a ~ from the get_msb_mask
   (although not at the same index) .
   ----------------------------------------------------------------- */

/* Inline this when possible */
inline uint64_t get_msb_mask_64(int nbytes) {
  assert(nbytes > 0 && nbytes < 8); 
  return((uint64_t)0xFFFFFFFFFFFFFFFF << (64 - (nbytes*8)));
  
  /*
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
    }*/
}

inline uint64_t get_lsb_mask_64(int nbytes) {
  assert(nbytes > 0 && nbytes < 8); 
  return((uint64_t)0xFFFFFFFFFFFFFFFF >> (64 - (nbytes*8)));

  /*
  switch (nbytes) {
    case 1:
      return 0xFF;     
    case 2:
      return 0xFFFF;
    case 3:
      return 0xFFFFFF;
    case 4:
      return 0xFFFFFFFF;
    case 5:
      return 0xFFFFFFFFFF;
    case 6:
      return 0xFFFFFFFFFFFF;
    case 7:
      return 0xFFFFFFFFFFFFFF;
    default:
      printf("ERROR : Invalid input to get_lsb_mask\n");
      return 0;
      } */
}



inline uint32_t get_msb_mask_32(int nbytes) {
  switch (nbytes) {
  case 1:
    return 0xFF000000;
  case 2:
    return 0xFFFF0000;
  case 3:
    return 0xFFFFFF00;
  default:
    printf("ERROR : Invalid input to get_msb_mask\n");
    return 0;
  }
}

inline uint32_t get_lsb_mask_32(int nbytes) {
  switch (nbytes) {
    case 1:
      return 0xFF;
    case 2:
      return 0xFFFF;
    case 3:
      return 0xFFFFFF;
    default:
      printf("ERROR : Invalid input to get_lsb_mask_32\n");
      return 0;
  }
}
