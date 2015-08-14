
#include "patcher.h" 

#include <stdlib.h> 
#include <memory.h> 

/* Just skip this part if ICC  */
#if defined(__ICC) || defined(__INTEL_COMPILER) 
/* Something else may be required here */ 
#else 
/* think this part also rules out __STRICT_ANSI__ */ 
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

struct sigaction g_newact; 
struct sigaction g_oldact; 


/* -----------------------------------------------------------------
   Constants 
   ----------------------------------------------------------------- */

const uint64_t int3 = 0xcc;

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

#define IS_IMMEDIATE(t)  ((t) == O_IMM || (t) == O_IMM1 || (t) == O_IMM2)


/* -----------------------------------------------------------------
   internally used 
   ----------------------------------------------------------------- */
bool set_page_rwe(void *addr, size_t nBytes);
uint64_t get_msb_mask_64(int nbytes);
uint64_t get_lsb_mask_64(int nbytes);
uint32_t get_msb_mask_32(int nbytes); 

static void int3_handler(int signo, siginfo_t *inf, void* ptr);


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
   
/* -----------------------------------------------------------------
   Interrupt handlers 
   ----------------------------------------------------------------- */ 

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
  uint64_t start = 0; 
  long nb = (long)nbytes;
  bool status = false; 
  
  status = set_page_rwe(addr,nbytes); 
 
  return status; 
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
    
    uint64_t lsb_mask = get_lsb_mask_64(cutoff_point);
    uint64_t msb_mask = get_msb_mask_64(cutoff_point); 
    
    /* read in 8 bytes before and after the straddle point */
    uint64_t before = *(straddle_point - 1);
    uint64_t after  = *straddle_point;
    
    int shift_size = 8 * (8 - cutoff_point); 
      
    /* this is the parts to keep from what was originally in memory */
    uint64_t patch_keep_before = before & (~msb_mask);
    uint64_t patch_keep_after  = after & msb_mask;
    
    /*  */ 
    uint64_t patch_before = patch_keep_before | ((patch_value  & lsb_mask) << shift_size);
    /*  */ 
    uint64_t patch_after =  patch_keep_after | ((patch_value  & ~lsb_mask) >> (8 * cutoff_point));


#ifdef THREADSAFE_PATCHING 
    uint8_t oldFR = ((uint8_t*)addr)[0]; 

    if (oldFR == int3) return false; 
    else if (__sync_bool_compare_and_swap((uint8_t*)addr, oldFR, int3)) {
      for(long i = 0; i < 1000; i++) {__asm__ __volatile__(""); } 
      WRITE(straddle_point,patch_after); 
      WRITE((straddle_point-1), patch_before); 
    }
    else return false; 
    
#else /* racy patching */      
    /* implement the straddler protocol */ 
    ((uint8_t*)addr)[0] = int3; 
    /*WRITE((straddle_point - 1), int3_sequence); */
    
    /* An empty delay loop that is unlikely to be optimized out 
       due to the magic asm inside */ 
    for(long i = 0; i < 1000; i++) {__asm__ __volatile__(""); }
    WRITE(straddle_point,patch_after); 
    WRITE((straddle_point-1), patch_before); 
    return true; 
#endif     
  } 
  
  /* if not a straddler perform a single write */
  WRITE((uint64_t*)addr,patch_value);
  return true; 
  
}
    
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
    
    // uint64_t lsb_mask = get_lsb_mask(cutoff_point);
    uint32_t msb_mask = get_msb_mask_32(cutoff_point); 

    /* read in 8 bytes before and after the straddle point */
    uint32_t before = *(straddle_point - 1);
    uint32_t after  = *straddle_point;

    int shift_size = 8 * (4 - cutoff_point); 

    /* this is the parts to keep from what was originally in memory */
    uint32_t patch_keep_before = before & (~msb_mask);
    uint32_t patch_keep_after  = after & msb_mask;
    
                                                    /*  & lsb_mask */ 
    uint32_t patch_before = patch_keep_before | ((patch_value) << shift_size);
                                                   /*  & ~lsb_mask */ 
    uint32_t patch_after =  patch_keep_after | ((patch_value) >> (8 * cutoff_point));
    /* commented out masking that becomes "shifted out" */ 

    /* implement the straddler protocol */ 
#ifdef THREADSAFE_PATCHING 
    uint8_t oldFR = ((uint8_t*)addr)[0]; 

    if (oldFR == int3) return false; 
    else if (__sync_bool_compare_and_swap((uint8_t*)addr, oldFR, int3)) {
      for(long i = 0; i < 1000; i++) {__asm__ __volatile__(""); } 
      WRITE(straddle_point,patch_after); 
      WRITE((straddle_point-1), patch_before); 
    }
    else return false; 
    
#else
    ((uint8_t*)addr)[0] = int3;     
    /* An empty delay loop that is unlikely to be optimized out 
       due to the magic asm inside */ 
    for(long i = 0; i < 1000; i++) {__asm__ __volatile__(""); }

    WRITE(straddle_point,patch_after); 
    WRITE(straddle_point-1, patch_before); 
    return true; 
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
		 intermediate_reg == decoded[i].ops[0].index) { 
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
