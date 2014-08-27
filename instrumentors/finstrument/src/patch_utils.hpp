
#ifndef _PATCH_UTILS_HPP_
#define _PATCH_UTILS_HPP_

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <sys/mman.h>
#include <errno.h>
#include <unistd.h>
#include <assert.h>

#include "logger.hpp"

inline uint64_t convert_to_little_endian(uint64_t num) {

  uint64_t res = 0;
  uint64_t b0,b1,b2,b3,b4,b5,b6,b7;

  b0 = (num & 0xff) << 56;     
  b1 = (num & 0xff00) << 40;  
  b2 = (num & 0xff0000) << 24; 
  b3 = (num & 0xff000000) << 8;
  b4 = (num & 0xff00000000) >> 8; 
  b5 = (num & 0xff0000000000) >> 24;
  b6 = (num & 0xff000000000000) >> 40;
  b7 = (num & 0xff00000000000000) >> 56;

  res = b0 | b1 | b2 | b3 | b4 | b5 | b6 | b7 ;
  return res;
}

inline uint32_t convert_to_little_endian_32(uint32_t num) {

  uint16_t res = 0;
  uint16_t b0,b1,b2,b3;

  b0 = (num & 0xFF) << 24;     
  b1 = (num & 0xFF00) << 8;     
  b2 = (num & 0xFF0000) >> 8;     
  b3 = (num & 0xFF00000000) >> 24;  

  res = b0 | b1 | b2 | b3;
  return res;
}

inline bool modify_page_permissions(uint8_t* addr) {

  long page_size = sysconf(_SC_PAGESIZE);
  int code = mprotect((void*)(addr - (((uint64_t)addr)%page_size)), page_size,
      PROT_READ | PROT_WRITE | PROT_EXEC);

  if (code) {
    LOG_ERROR("mprotect was not successfull! code %d\n", code);
    LOG_ERROR("errno value is : %d\n", errno);
    return false;
  }

  if (page_size - ((uint64_t)addr)%page_size < PROBESIZE) {
    code = mprotect((void*)(addr-((uint64_t)addr)%page_size+ page_size) , page_size,
        PROT_READ | PROT_WRITE | PROT_EXEC);
    if (code) {
      LOG_ERROR("mprotect was not successfull! code %d\n", code);
      LOG_ERROR("errno value is : %d\n", errno);
      return false;;
    }
  }

  return true;
}

inline bool patch_with_call(uint64_t* addr, uint64_t fn) {

  uint64_t old_val = *(addr-1); 
  uint64_t fn_addr = 0; // convert_to_big_endian(fn);
  fn_addr = (fn_addr >> 16);
  uint64_t call = 0xFFD7000000000000;// CALL *%rdi
  uint64_t new_val = fn_addr | call;

  bool status = __sync_bool_compare_and_swap(addr-1, old_val, new_val);

  fprintf(stderr, "CALL CAS status : %d\n", status);

  old_val = *(addr-2);
  fn_addr = 0;// convert_to_big_endian(fn);

  uint64_t mask = 0xFFFF000000000000;
  fn_addr = (fn_addr & mask);
  // fn_addr = (fn_addr >> 48);

  uint64_t mov = 0x48BF;
  mov = (mov << 48);

  fn_addr = (fn_addr | mov);

  uint16_t func_id = 0; // convert_to_big_endian_16(2);
  mov = 0xBE0000;
  uint64_t temp = (func_id | mov);
  temp = (temp<< 32);

  fn_addr = (fn_addr | temp);

  uint64_t nop = 0x90;
  new_val = fn_addr | nop; 
  
  status = __sync_bool_compare_and_swap(addr-2, old_val, new_val);

  return status;
}

inline bool patch_params(uint64_t* addr, uint8_t start_idx, uint32_t func_id) {

  if (start_idx > 3) {
    LOG_ERROR("[Finstrumentor] Not enough space to patch the parameters within the probesite..\n");
    return false;
  }

  uint64_t low_qword_old_val = *addr;

  uint64_t mask = 0xFFFFFFFFFFFFFFFF;
  int8_t shift_size = start_idx * 8;

  mask = (mask << shift_size);
  uint64_t low_qword_new_val = (low_qword_old_val & ~mask);
  uint8_t* low_qword_new_val_ptr = (uint8_t*) &low_qword_new_val;

  /* Patch with the a XOR to clear out RSI (param 2). This will be used to detect if this function has been already been
   * initlized. */
  low_qword_new_val_ptr[start_idx] = 0x48;
  low_qword_new_val_ptr[start_idx + 1] = 0x33;
  low_qword_new_val_ptr[start_idx + 2] = 0xF6;

  /* Patch RDI (param 2) with func id. Need to handle little endianness of the integer */
  low_qword_new_val_ptr[start_idx + 3] = 0xBF;

  int func_id_low_bytes = 7 - start_idx - 3;

  uint32_t func_id_reversed = convert_to_little_endian_32(func_id);
  uint8_t* func_id_ptr = (uint8_t*) &func_id;

  switch(func_id_low_bytes) {
    case 4:
      *(uint32_t*)(low_qword_new_val_ptr+start_idx+4) = (uint32_t) func_id;
      break;
    case 3:
      *(uint16_t*)(low_qword_new_val_ptr+start_idx+4) = (uint16_t) func_id;
      *(uint8_t*)(low_qword_new_val_ptr+start_idx+7) = func_id_ptr[2];
      break;
    case 2:
      *(uint16_t*)(low_qword_new_val_ptr+start_idx+4) = (uint16_t) func_id;
      break;
    case 1:
      *(uint8_t*)(low_qword_new_val_ptr+start_idx+4) = (uint8_t) func_id;
      break;
    default:
      assert(0);
      break;
  }

  uint64_t high_qword_old_val = *(addr+1);
  mask = 0xFFFFFFFFFF000000; // Mask out except the call 
  uint64_t high_qword_new_val = (high_qword_old_val & mask);

  mask = 0x0000000000909090; //  And then NOP out lower bytes
  high_qword_new_val = high_qword_new_val | mask;


  uint8_t* high_qword_new_val_ptr = (uint8_t*) &high_qword_new_val;
  
  int func_id_hi_bytes = 4 - func_id_low_bytes;

  switch(func_id_hi_bytes) {
    case 3:
      *(uint16_t*)(high_qword_new_val_ptr) = (uint16_t) (func_id >> 8);
      *(uint8_t*)(high_qword_new_val_ptr + 2) = (uint8_t) (func_id >> 16);
      break;
    case 2:
      *(uint16_t*)(high_qword_new_val_ptr) = (uint16_t) (func_id >> 16);
      break;
    case 1:
      *(uint8_t*)(high_qword_new_val_ptr) = (uint8_t) (func_id >> 24);
      break;
    case 0:
      break;
    default:
      assert(0);
      break;
  }


  bool status = __sync_bool_compare_and_swap(addr + 1, high_qword_old_val, high_qword_new_val);
  fprintf(stderr, "First CAS status is : %d\n", status);

  status = __sync_bool_compare_and_swap(addr, low_qword_old_val, low_qword_new_val);
  fprintf(stderr, "Second CAS status is : %d\n", status);

  return false;

}

inline bool patch_with_jmp(uint64_t* addr, uint8_t start_idx, uint32_t distance) {

  if (start_idx > 3) {
    LOG_ERROR("[Finstrumentor] Not enough space to patch the jmp within 64 bits space given..\n");
    return false;
  }

  uint64_t old_val = *addr;

  uint64_t mask = 0xFFFFFFFFFFFFFFFF;
  int8_t shift_size = start_idx * 8;

  mask = (mask << shift_size);
  uint64_t new_val = (old_val & ~mask);
  uint8_t* new_val_ptr = (uint8_t*) &new_val;

  /* Patch with the temporary jump to skip executing next instruction byte until it is written with proper call site information */
  new_val_ptr[start_idx] = 0xE9;

  *(uint32_t*)(new_val_ptr+start_idx+1) = (uint32_t) distance;

  return __sync_bool_compare_and_swap(addr, old_val, new_val);
}

inline bool patch_with_call(uint64_t* addr, uint32_t* fn, uint32_t param) {
  return false;
}

inline bool patch_with_call_param(uint64_t*addr, uint32_t, uint8_t index) {
  return false;
}

#endif /* _PATCH_UTILS_HPP_ */
