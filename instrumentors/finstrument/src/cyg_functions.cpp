
#include "cyg_functions.hpp"
#include "patch_utils.hpp"
#include "finstrumentor.hpp"

static uint8_t mov_encodings[20] = {MOV_REG_8, MOV_REG_16, MOV_MEM_8, MOV_MEM_16,
  MOV_IMM_8_RAX, MOV_IMM_8_RCX, MOV_IMM_8_RDX, MOV_IMM_8_RBX,
  MOV_IMM_8_RSP, MOV_IMM_8_RBP, MOV_IMM_8_RSI, MOV_IMM_8_RDI,
  MOV_IMM_16_RAX, MOV_IMM_16_RCX, MOV_IMM_16_RDX, MOV_IMM_16_RBX,
  MOV_IMM_16_RSP, MOV_IMM_16_RBP, MOV_IMM_16_RSI,  MOV_IMM_16_RDI};

inline bool check_if_mov(uint8_t progbit) {
  for (int i=0; i<20; i++) {
    if (mov_encodings[i] == progbit) {
      return true;
    } 
  }

  return false;
}

int counter = 0;

extern "C" 
{
  void print_fn(short t);
}

inline void print_fn(short t) {
   // fprintf(stderr, "Do or die trying %d..\n", (int)t);
}

typedef void (*FuncPtr)(short);

void __cyg_profile_func_enter(void* func, void* caller) {

  LOG_DEBUG("Executing cyg_enter for %d time at function %p..\n", counter++, func);

  fprintf(stderr, "Parameter 1 is : %lu Parameter 2 is : %lu\n", (uint64_t) func & 0xFFFFFFFF, (uint64_t) caller);

  if ( (uint64_t) caller == 0) {
    return;
  }

  uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));

// #ifdef __INTEL_COMPILER
  uint64_t* ptr = addr-2; // 8 * 2 = 16 bytes
  int8_t probe_start_idx = -1; 

  uint8_t* probe_start = (uint8_t*) ptr;
  for (int i=0; i<4; i++) { // Probe site should start at most 4 bytes in
    bool is_mov = check_if_mov(*(probe_start+i)); 
    if (is_mov) {
      if (*(probe_start+i-1) == REX_PREFIX_0 || *(probe_start+i-1) == REX_PREFIX_1 ) {
        probe_start_idx = i-1;
      } else {
        probe_start_idx = i;
      }
    }
  }

  /*
  if (probe_start_idx != 0) {
    LOG_ERROR("Minimum size (15 bytes) required for probesite not available at %p\n. Skipping..\n", ptr);
    return;
  }
  */

  bool status = modify_page_permissions(probe_start);
  if (!status) {
    LOG_ERROR("Patching the probesite failed at %p. Skipping..\n", ptr);
    return;
  }

  /* Negative 5 deducts the size of the jump itself. Jump distance calculated from next address after jmp */
  uint8_t relative = 16 - probe_start_idx - 5;

  /* Patch with the temporary jump to skip executing next instructions within probe site until it is written with proper call site information */
  status = patch_with_jmp(ptr, probe_start_idx, relative);

  status = patch_params(ptr, probe_start_idx, 230);

  // LOG_DEBUG("CAS status for jmp is :%d..\n", status);

  // status = patch_with_call(addr, (uint64_t)print_fn);
  // LOG_DEBUG("CAS status for call is :%d..\n", status);

// #endif

  LOG_INFO("print_fn is at %p\n", &print_fn);

  /*
  FuncPtr hello_func = print_fn;
  hello_func(3);
  */


  // uint64_t addr = (uint64_t)__builtin_extract_return_addr(__builtin_return_address(0));
  // uint64_t distance = ((uint64_t)print_fn - addr);
  
  // fprintf(stderr, "print_fn is : %p addr is at : %p \n", &print_fn, (unsigned char*) addr);
  // fprintf(stderr, "Distance is : %lu with %lu 2^32 segments away\n", (unsigned long) distance, (unsigned long) distance >> 32);

  // print_fn(1);

  // instrumentation_func epilog = (instrumentation_func) INSTRUMENTOR_INSTANCE->get_profiler_epilog();

}

void __cyg_profile_func_exit(void* func, void* caller) {


  /*
  FuncPtr hello_func = print_fn;
  hello_func(3);
  */

  // print_fn(1);

}