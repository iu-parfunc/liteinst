
#include "cyg_functions.hpp"
#include "patcher.h"

#include <stdint.h>
#include <assert.h>

void __cyg_profile_func_enter(void* func, void* caller) {

  uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(
      __builtin_return_address(0));
  uint8_t* call_addr = (uint8_t*) addr - 5;

  uint64_t original = *(uint64_t*)call_addr;
  uint64_t mask = 0xFFFFFF0000000000; // Mask out the CALL instruction
  uint64_t call_masked = (uint64_t) (original & mask); // CALL masked out
  uint64_t nop_mask = 0x0000000000441F0F; // Mask with a 5 byte NOP

  uint64_t inactive_seq = (call_masked | nop_mask);

  init_patch_site((void*) call_addr, 8);
  bool b = patch_64((void*) call_addr, inactive_seq);
  assert(b);

}

void __cyg_profile_func_exit(void* func, void* caller) {

  uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(
      __builtin_return_address(0));
  uint8_t* call_addr = (uint8_t*) addr - 5;

  uint64_t original = *(uint64_t*)call_addr;
  uint64_t mask = 0xFFFFFF0000000000; // Mask out the CALL instruction
  uint64_t call_masked = (uint64_t) (original & mask); // CALL masked out
  uint64_t nop_mask = 0x0000000000441F0F; // Mask with a 5 byte NOP

  uint64_t inactive_seq = (call_masked | nop_mask);

  init_patch_site((void*) call_addr, 8);
  bool b = patch_64((void*) call_addr, inactive_seq);
   assert(b);

}
