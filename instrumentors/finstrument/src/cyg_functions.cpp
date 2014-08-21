
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

void __cyg_profile_func_enter(void* func, void* caller) {

  LOG_DEBUG("Executing cyg_enter for %d time at function %p..\n", counter++, func);

  uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));

  uint64_t* ptr = addr-2; // 8 * 2 = 16 bytes
  int8_t probe_start_idx = -1; // Cannot use -1 since it's a legitimate value for a start_idx

  uint8_t* probe_start = (uint8_t*) ptr;
  for (int i=0; i<4; i++) { // Probe site should start at most 4 bytes in
    bool is_mov = check_if_mov(*(probe_start+i)); 
    if (is_mov) {
      if (*(probe_start+i-1) == REX_PREFIX) {
        if (i > 0) {
          probe_start_idx = i-1;
        } else {
          probe_start_idx = i;
        }
      } else {
        if (i < 3) {
          probe_start_idx = i;
        }        
      }
    }
  }

  if (probe_start_idx == -1) {
    LOG_ERROR("Minimum size (13 bytes) required for probesite not available at %p\n. Skipping..\n", ptr);
    return;
  }

  bool status = modify_page_permissions(probe_start);
  if (!status) {
    LOG_ERROR("Patching the probesite failed at %p. Skipping..\n", ptr);
    return;
  }

  /* Negative 5 deducts the size of the jump itself. Jump distance calculated from next address after jmp */
  uint8_t relative = 16 - probe_start_idx - 5;

  /* Patch with the temporary jump to skip executing next instructions within probe site until it is written with proper call site information */
  status = patch_with_jmp(ptr, probe_start_idx, relative);

  LOG_DEBUG("CAS status is :%d..\n", status);

  /*

  uint8_t* probe_site_start = ptr + probe_start_idx;
  uint64_t probe_hi_byte_val = *((uint64_t*) ptr);
  uint8_t* probe_hi_byte_val_ptr = (uint8_t*) &probe_hi_byte_val;

  uint64_t mask = 0xFFFFFFFFFFFFFFFF;
  int8_t shift_size = probe_start_idx * 8;

  mask = (mask << shift_size);  
  probe_hi_byte_val = (probe_hi_byte_val & ~mask);
  */

  /* Patch with the temporary jump to skip executing next instruction byte until it is written with proper call site information */
  /*
  probe_hi_byte_val_ptr[probe_start_idx] = 0xE9;
  */

  /* Negative 5 deducts the size of the jump itself. Jump distance calculated from next address after jmp */
  /*
  uint8_t relative = 16 - probe_start_idx - 5;
  *(uint32_t*)(probe_hi_byte_val_ptr+probe_start_idx+1) = (uint32_t) relative;

  status = __sync_bool_compare_and_swap((uint64_t*)probe_boundary_start,
             *((uint64_t*)probe_boundary_start), probe_hi_byte_val);
             */


  // instrumentation_func epilog = (instrumentation_func) INSTRUMENTOR_INSTANCE->get_profiler_epilog();

}

void __cyg_profile_func_exit(void* func, void* caller) {

}
