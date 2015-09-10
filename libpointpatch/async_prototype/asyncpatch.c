
#include "asyncpatch.h"

#include <stdlib.h> 
#include <memory.h> 
#include <time.h> 

#define __USE_GNU  
#include <signal.h>

/* -----------------------------------------------------------------
   Globals 
   ----------------------------------------------------------------- */

long g_page_size = 0; 
size_t g_cache_lvl3_line_size = 0; 
uint64_t g_int3_interrupt_count = 0; 

struct sigaction g_newact; 
struct sigaction g_oldact; 


PatchList g_patchlist = {0}; 
volatile uint64_t g_patchlock = 0; 


void cons_patch(struct Patch *p, PatchList *pl){ 
  struct Patch *old_hd = pl->head; 
  p->next  = old_hd;
  pl->head = p;
  pl->length++; 
}

struct Patch *remove_patch(uint64_t find_addr, PatchList *pl){ 
  
  struct Patch *p = pl->head; 
  if (p == NULL) return NULL; 
  
  if (p->address == find_addr) { 
    pl->head = p->next; 
    pl->length--;
    return p;
  }
  
  struct Patch *curr = p->next; 
  struct Patch *prev = p;
  while (curr != NULL && curr->address != find_addr) { 
    prev = curr; 
    curr = curr->next;
  }
  if (curr == NULL) return NULL; 

  prev->next = curr->next; 
  curr->next = NULL; 
  pl->length--;
  return curr;
}


/* -----------------------------------------------------------------
   Constants 
   ----------------------------------------------------------------- */

const uint8_t int3 = 0xCC;

/* -----------------------------------------------------------------
   WAIT
   ----------------------------------------------------------------- */

#ifndef WAIT_ITERS
#define WAIT_ITERS 10
#endif 

//#define WAIT() for(long i = 0; i < WAIT_ITERS; i++) { asm (""); } 
#define WAIT() {}

/* -----------------------------------------------------------------
   MACROES 
   ----------------------------------------------------------------- */ 

/* You can try this out with normal writes to memory by defined NO_CAS */
#ifdef NO_CAS 
/* A WRITE is just a regular assingment */ 
#define WRITE(addr,value)  (addr)[0] = (value) 
#else 
/* An atomic write implemented via CAS. I'm not sure this comment makes sense */
#define WRITE(addr,value)  __sync_val_compare_and_swap((addr), *(addr), (value))
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
   Print some info
   ----------------------------------------------------------------- */
void print_cache_info() {
  printf("L1 ICACHE size: %ld\n", sysconf(_SC_LEVEL1_ICACHE_SIZE));
  printf("L1 ICACHE assoc: %ld\n", sysconf(_SC_LEVEL1_ICACHE_ASSOC));
  printf("L1 ICACHE linesize: %ld\n", sysconf(_SC_LEVEL1_ICACHE_LINESIZE));
  printf("\n");
  printf("L1 DCACHE size: %ld\n", sysconf(_SC_LEVEL1_DCACHE_SIZE));
  printf("L1 DCACHE assoc: %ld\n", sysconf(_SC_LEVEL1_DCACHE_ASSOC));
  printf("L1 DCACHE linesize: %ld\n", sysconf(_SC_LEVEL1_DCACHE_LINESIZE));
  printf("\n");
  printf("L2 CACHE size: %ld\n", sysconf(_SC_LEVEL2_CACHE_SIZE));
  printf("L2 CACHE assoc: %ld\n", sysconf(_SC_LEVEL2_CACHE_ASSOC));
  printf("L2 CACHE linesize: %ld\n", sysconf(_SC_LEVEL2_CACHE_LINESIZE));
  printf("\n");
  printf("L3 CACHE size: %ld\n", sysconf(_SC_LEVEL3_CACHE_SIZE));
  printf("L3 CACHE assoc: %ld\n", sysconf(_SC_LEVEL3_CACHE_ASSOC));
  printf("L3 CACHE linesize: %ld\n", sysconf(_SC_LEVEL3_CACHE_LINESIZE));

} 

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


  /* setup signal handling for straddler protocol */ 
  memset( &g_newact, 0, sizeof g_newact);
  g_newact.sa_sigaction = & int3_handler;
  g_newact.sa_flags = SA_SIGINFO;
  sigemptyset(& (g_newact.sa_mask));

  
  sigaction(SIGTRAP, &g_newact, &g_oldact);

#ifdef NO_CAS 
  printf("NO_CAS VERSION OF PATCHER CODE\n"); 
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

   
/* -----------------------------------------------------------------
   Interrupt handlers 
   ----------------------------------------------------------------- */ 

static void int3_handler(int signo, siginfo_t *inf, void* ptr) {

  g_int3_interrupt_count++;
  ucontext_t *ucontext = (ucontext_t*)ptr;  
  
  //uint64_t addr = (uint64_t)inf->si_addr; 
  uint64_t addr = (uint64_t)ucontext->uc_mcontext.gregs[REG_RIP] -1;
  
  
  /* printf("list: %lu \n",g_patchlist.length); */ 
  
  /* need locks around this one */ 
  bool i_got_it = __sync_bool_compare_and_swap(&g_patchlock, 0, 1); 
  if (i_got_it) { 
  
    struct Patch *p = remove_patch(addr,&g_patchlist); 
    //printf("INT3_HANDLER: looking up addr: %lu\n",addr); 
  
    if (p == NULL) { 
      //printf("INT3_HANDLER: addr %lu not found in list\n",addr); 
      g_patchlock=0; 
    } else { 
      //printf("PATCHING\n");
      int offset = (uint64_t)addr % g_cache_lvl3_line_size;
      unsigned int cutoff_point = g_cache_lvl3_line_size - offset; 
      uint64_t* straddle_point = (uint64_t*)((uint8_t*)addr + cutoff_point);
      
      WAIT();
      WRITE(straddle_point,p->back); 
      WRITE((straddle_point-1),p->front); 
      
      g_patchlock=0; 
    }
    ucontext->uc_mcontext.gregs[REG_RIP] = 
      (greg_t)ucontext->uc_mcontext.gregs[REG_RIP] + 4;
    return; 
 
  } 
  ucontext->uc_mcontext.gregs[REG_RIP] = 
      (greg_t)ucontext->uc_mcontext.gregs[REG_RIP] + 4;
  //ticks start = getticks();


  /* Resuming the thread after skipping the call instruction. */ 
  // ucontext->uc_mcontext.gregs[REG_RIP] = (greg_t)ucontext->uc_mcontext.gregs[REG_RIP] + 4;
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
  return WAIT_ITERS;
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
bool async_patch_64(void *addr, uint64_t patch_value){  
  
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
    
    
    
    /* ----------------------------------------------------------------- 
       WAIT BASED THREADSAFE PATCHER
       ----------------------------------------------------------------- */ 
    uint8_t oldFR = ((uint8_t*)addr)[0]; 
    
    if (oldFR == int3) return false; 
    else if (__sync_bool_compare_and_swap((uint8_t*)addr, oldFR, int3)) {
      
      struct Patch *p = (struct Patch*)malloc(sizeof(struct Patch)); 
      p->address = (uint64_t)addr;
      p->front   = patch_before; 
      p->back    = patch_after; 
      p->timestamp = 0; /* fixme */ 
      p->next = NULL; 
      
      /* need to lock access to this list */ 
      while (!__sync_bool_compare_and_swap(&g_patchlock, 0, 1)) {};  
      // printf("ASYNC_PATCH: Adding address %lu to list\n",p->address);
      cons_patch(p,&g_patchlist); 
      g_patchlock=0; 

      
      //WAIT();
      //WRITE(straddle_point,patch_after); 
      //WRITE((straddle_point-1), patch_before); 
      return true; 
    }
    else return false; 
  } 
  
  
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

inline uint64_t get_lsb_mask_64(int nbytes) {
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
  }
}
