#ifndef _UTILS_HPP_
#define _UTILS_HPP_

#include <string>
#include <vector>

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <sys/mman.h>
#include <errno.h>
#include <unistd.h>
#include <limits.h>
#include <assert.h> 

#include "finstrumentor.hpp"
#include "distorm.h"
#include "mnemonics.h"
#include "logger.hpp"
#include "bitmap.hpp"

#define PROBESIZE 16

inline int tokenize(const std::string& str, std::vector<std::string>& tokens, const std::string& delimiters = " ") {
  std::string::size_type lastPos = str.find_first_not_of(delimiters, 0);
  std::string::size_type pos = str.find_first_of(delimiters, lastPos);

  int count = 0;
  while (std::string::npos != pos || std::string::npos != lastPos)
  {
    tokens.push_back(str.substr(lastPos, pos - lastPos));
    lastPos = str.find_first_not_of(delimiters, pos);
    pos = str.find_first_of(delimiters, lastPos);
    count++;
  }
  return count;
}

inline void clflush(volatile void *p) {
    asm volatile ("clflush (%0)" :: "r"(p));
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

/* Checks if register type is equal (for 32 and 64 bit variants) for all general purpose registers */
// 64bit - R_RAX, R_RCX, R_RDX, R_RBX, R_RSP, R_RBP, R_RSI, R_RDI, R_R8, R_R9, R_R10, R_R11, R_R12, R_R13, R_R14, R_R15,
// 32 bit - R_EAX, R_ECX, R_EDX, R_EBX, R_ESP, R_EBP, R_ESI, R_EDI, R_R8D, R_R9D, R_R10D, R_R11D, R_R12D, R_R13D, R_R14D, R_R15D,
inline bool reg_equal(uint8_t reg1, uint8_t reg2) {

  switch (reg1) {
    case R_EAX:
    case R_RAX:
      if (reg2 == R_EAX || reg2 == R_RAX) {
        return true;
      }
      break;
    case R_ECX:
    case R_RCX:
      if (reg2 == R_ECX || reg2 == R_RCX) {
        return true;
      } 
      break;
    case R_EDX:
    case R_RDX:
      if (reg2 == R_EDX || reg2 == R_RDX) {
        return true;
      } 
      break;
    case R_EBX:
    case R_RBX:
      if (reg2 == R_EBX || reg2 == R_RBX) {
        return true;
      } 
      break;
    case R_ESP:
    case R_RSP:
      if (reg2 == R_ESP || reg2 == R_RSP) {
        return true;
      }
      break;
    case R_EBP:
    case R_RBP:
      if (reg2 == R_ESP || reg2 == R_RSP) {
        return true;
      }
      break;
    case R_ESI:
    case R_RSI:
      if (reg2 == R_ESI || reg2 == R_RSI) {
        return true;
      }
      break;
    case R_EDI:
    case R_RDI:
      if (reg2 == R_EDI || reg2 == R_RDI) {
        return true;
      }
      break;
    case R_R8:
    case R_R8D:
      if (reg2 == R_R8 || reg2 == R_R8D) {
        return true;
      }
      break;
    case R_R9:
    case R_R9D:
      if (reg2 == R_R9 || reg2 == R_R9D) {
        return true;
      }
      break;
    case R_R10:
    case R_R10D:
      if (reg2 == R_R10 || reg2 == R_R10D) {
        return true;
      }
      break;
    case R_R11:
    case R_R11D:
      if (reg2 == R_R11 || reg2 == R_R11D) {
        return true;
      }
      break;
    case R_R12:
    case R_R12D:
      if (reg2 == R_R12 || reg2 == R_R12D) {
        return true;
      }
      break;
    case R_R13:
    case R_R13D:
      if (reg2 == R_R13 || reg2 == R_R13D) {
        return true;
      }
      break;
    case R_R14:
    case R_R14D:
      if (reg2 == R_R14 || reg2 == R_R14D) {
        return true;
      }
      break;
    case R_R15:
    case R_R15D:
      if (reg2 == R_R15 || reg2 == R_R15D) {
        return true;
      }
      break;
    default:
      fprintf(stderr, "Unrecoginzed non general purpose register %d\n", reg2);
      break;
  }

  return false;

}

inline uint64_t get_lsb_mask(int nbytes) {
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

inline bool patch_with_value(uint8_t* addr, uint16_t func_id) {

  if (addr == NULL) return false; 
  
  bool status = modify_page_permissions(addr);
  if (!status) {
    LOG_ERROR("Patching the probesite failed at %p. Skipping..\n", addr);
    return status;
  }

  uint64_t sequence = *((uint64_t*)addr);
  uint64_t mask = 0xFFFFFF0000000000;
  uint64_t masked_sequence = (uint64_t) (sequence & mask); 
  uint8_t* sequence_ptr = (uint8_t*) (&masked_sequence);
  sequence_ptr[0] = addr[0]; // MOV REG opcode
  *(uint32_t*)(sequence_ptr+1) = (uint32_t)func_id;

  /*
  size_t cache_line_size = sysconf(_SC_LEVEL3_CACHE_LINESIZE); 
  int offset = (uint64_t) addr % cache_line_size;
  if (offset >= 57) { // If this is a cache line straddler
    int cutoff_point = 64 - offset;

    uint64_t* straddle_point = (uint64_t*)((uint8_t*)addr + cutoff_point);
    uint64_t* straddle_part_1_start = straddle_point - 1;
    uint64_t* straddle_part_2_start = straddle_point;
    uint64_t front_sequence = *straddle_part_1_start;
    uint64_t back_sequence = *straddle_part_2_start;

    // sequence_ptr[0] = 0xCC; // Overwrite INT3 

    int shift_size = 8 * (8 - cutoff_point);

    uint64_t int3mask = 0xCC;
    uint64_t ormask = 0xFF;

    int3mask = (int3mask << shift_size);
    ormask = (ormask << shift_size);

    uint64_t straddle_int3_sequence = (*straddle_part_1_start & ~ormask) | int3mask;
      
    uint64_t temp0 = masked_sequence & get_lsb_mask(cutoff_point);
    shift_size = (8 * (8-cutoff_point)); 
    temp0 = (temp0 << shift_size);
    uint64_t temp1 = front_sequence & (~get_msb_mask(cutoff_point));
    uint64_t new_front_sequence = temp0 | temp1;

    temp0 = masked_sequence & (~get_lsb_mask(cutoff_point));
    shift_size = (8 * cutoff_point);
    temp0 = (temp0 >> shift_size);
    temp1 = back_sequence & get_msb_mask(cutoff_point); 
    uint64_t new_back_sequence = temp0 | temp1;

    __sync_val_compare_and_swap((uint64_t*) straddle_part_1_start,
             *((uint64_t*)straddle_part_1_start), straddle_int3_sequence);
    __sync_synchronize(); 
    clflush(straddle_part_1_start);
    __sync_val_compare_and_swap((uint64_t*) straddle_part_2_start,
            *((uint64_t*)straddle_part_2_start), new_back_sequence);
    status = __sync_bool_compare_and_swap((uint64_t*) straddle_part_1_start,
            *((uint64_t*)straddle_part_1_start), new_front_sequence);
    __sync_synchronize(); 
    clflush(straddle_part_1_start);

    return status;

  }
  */

  status = __sync_bool_compare_and_swap((uint64_t*)addr, *((uint64_t*)addr), masked_sequence);
  __sync_synchronize(); 
  clflush(addr);

  return status;

}

inline void print_decoded_output(uint8_t* start, uint64_t length) {

  _DecodeResult res;
  _DecodedInst *disassembled = new _DecodedInst [length];
  unsigned int decodedInstructionsCount = 0;
  _OffsetType offset = 0;

  res = distorm_decode(offset,
      (const unsigned char*)start,
      length,
      Decode64Bits,
      disassembled,
      length,
      &decodedInstructionsCount);

  if ( res != DECRES_SUCCESS) {
    printf("Decoding failed\n");
    delete(disassembled);
    return;
  }
     
  // CHECK res here... 

  for (unsigned int i = 0; i < decodedInstructionsCount; i++) {
    printf("(%02d) %-24s %s%s%s\r\n",
        // disassembled[i].offset,
        disassembled[i].size,
        (char*)disassembled[i].instructionHex.p,
        (char*)disassembled[i].mnemonic.p,
        disassembled[i].operands.length != 0 ? " " : "",
        (char*)disassembled[i].operands.p);
  }
  
  delete(disassembled);
}


inline uint8_t* get_edi_set_addr(uint8_t* call_return_addr, uint8_t* start_addr) {
  uint8_t* call_addr = (uint8_t*) call_return_addr - 5;
  uint64_t offset = (uint8_t*)call_addr - (uint8_t*)start_addr;

  /*
    Diagnostics
    print_decoded_output((uint8_t*)start_addr, offset);
  */
  
  // Address where EDI/RDI is set last before the call
  uint8_t* edi_set_addr = 0;
  _DInst* result = (_DInst*) malloc(sizeof(_DInst) * 2 * offset);
  unsigned int instructions_count = 0;
  
  _DecodedInst inst;
  
  _CodeInfo ci  = {0};
  ci.code = (uint8_t*)start_addr;
  ci.codeLen = offset;
  ci.dt = Decode64Bits;
  ci.codeOffset = 0x100000;
  
  distorm_decompose(&ci, result, offset, &instructions_count);
  uint64_t ptr_size = 0;
  uint64_t edi_offset = 0;
  uint8_t intermediate_reg = 0;
  
  if (instructions_count > offset) {
    fprintf(stderr, "[DEBUG] Instructions decoded : %d Offset : %lu\n", instructions_count, offset);
    free(result);
    
    //res->success = false;
    //res->conflict = false;
    
    //while(!__sync_bool_compare_and_swap(lock, 1 , 0));
    //return res;
    return NULL;
  }
  
  bool edi_setter_found = false;
  int instruction_offset = 0;
  for (int i = instructions_count - 1; i >= 0; i--) {
    if (result[i].flags == FLAG_NOT_DECODABLE) {
      printf("Bad decode attempt.. Call address : %p \n", call_addr);
      free(result);
      
      //res->success = false;
      //res->conflict = false;
      
      //while(!__sync_bool_compare_and_swap(lock, 1 , 0));
      //return res;
      return NULL;
    }
    
    distorm_format(&ci, &result[i], &inst);
    
    ptr_size += result[i].size;
    instruction_offset++;
    
    if (result[i].opcode == I_MOV) {
      if (!edi_setter_found && result[i].ops[0].type == O_REG && 
	  (result[i].ops[0].index == R_EDI || result[i].ops[0].index == R_RDI)) {
	if (result[i].ops[1].type == O_IMM || result[i].ops[1].type == O_IMM1 || result[i].ops[1].type == O_IMM2) {
	  edi_offset = ptr_size;
	  break;
	} else if (result[i].ops[1].type == O_REG) {
	  edi_setter_found = true;
	  intermediate_reg  = result[i].ops[1].index;
	}
      } else if(edi_setter_found && result[i].ops[0].type == O_REG && 
		(reg_equal(intermediate_reg, result[i].ops[0].index))) {
	if (result[i].ops[1].type == O_IMM || result[i].ops[1].type == O_IMM1 || result[i].ops[1].type == O_IMM2) {
	  edi_offset = ptr_size;
	  break;
	} else if (result[i].ops[1].type == O_REG) {
	  intermediate_reg  = result[i].ops[1].index;
	}
      }
    }
  }
  free(result);
  
  edi_set_addr = call_addr - edi_offset;
  return edi_set_addr;
}
  
      // fprintf(stderr, "[DEBUG] Call address : %p EDI offset : %lu EDI set address : %p \n", call_addr, edi_offset, edi_set_addr);

  // Hack : If the edi setter and the call site are adjacent and edi setter straddles a cache line
  // we handle it specifically to escape patching it
  /*
  if (instruction_offset == 1) {
    size_t cache_line_size = sysconf(_SC_LEVEL3_CACHE_LINESIZE); 
    int edi_setter_cache_line_offset = (uint64_t) edi_set_addr % cache_line_size;
    int call_instruction_cache_line_offset = ((uint64_t) call_addr) % cache_line_size;
    if (edi_setter_cache_line_offset >= 57 ||
       call_instruction_cache_line_offset >= 57) {
      free(result);
      res->success = false;
      res->conflict = true;
      return res;
    }
  }
  */
//}

#define PARAMETER_PATCH_ERROR (-1)
#define PARAMETER_PATCH_OK    1 

inline int patch_first_parameter(FinsProbeInfo* probe_info,uint64_t* call_return_addr, uint64_t* start_addr, uint16_t func_id) {

  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;
  uint64_t* lock = ins->getLock((uint64_t) start_addr);
  int spin_counter = 0;
  //PatchResult* res = new PatchResult;


  // It seems that lock == NULL should be an error ? 
  assert(lock != NULL);


  if (__sync_bool_compare_and_swap(lock, 0 , 1)) {
    
    uint8_t *edi_set_addr = get_edi_set_addr((uint8_t*)call_return_addr, (uint8_t*) start_addr);
    
    bool status = patch_with_value(edi_set_addr, func_id);

    if (!status) {
      //res->success = false;
      //res->conflict = false;
      

      ins->addProbeInfo_((uint64_t)start_addr, (uint8_t*)call_return_addr, true);
      probe_info = ins->getProbeInfo((uint64_t)start_addr, (uint8_t*) call_return_addr);
      set_index(g_straddlers_bitmap, func_id);
      while(!__sync_bool_compare_and_swap(lock, 1 , 0));
      return PARAMETER_PATCH_OK;
    }
    
    //res->success = true;
    //res->conflict = false;
    //res->edi_set_addr = edi_set_addr;

    ins->addProbeInfo_((uint64_t)start_addr, (uint8_t*)call_return_addr, false);
    
    while(!__sync_bool_compare_and_swap(lock, 1 , 0));
    return PARAMETER_PATCH_OK;
    
  } else { // We failed. Wait until the other thread finish and just return
    
#ifndef NDEBUG
    ticks t0 = getticks(); 
#endif 
    while (*lock) {
      if (spin_counter == INT_MAX) {
	fprintf(stderr, "RESETTING the counter\n");
	fprintf(stderr, "Returning without adding the probe since lock is : %lu\n", *lock);
	break;
      }
      spin_counter += 1;
      pthread_yield();
    }
#ifndef NDEBUG
    pthread_t tid = pthread_self();
    ticks t1 = getticks();
    fprintf( stderr
	     , "[utils.hpp ***NEW*** patch_first_parameter] ThreadID: %lu was busy-waiting for %d iterations.\nWait took %llu ticks while attempting to patch function %d.\n"
	     , (unsigned long int)tid,spin_counter,(t1-t0),func_id);

#endif 

    // maybe here, let thread actually check what kind of patching took place 
    // by the successful thread. But it may just be unnecessary. 


    // as it is now it seems this function really has no "fail-mode" 
    return PARAMETER_PATCH_OK;
  }
}

inline PatchResult* patch_first_parameter_old(uint64_t* call_return_addr, uint64_t* start_addr, uint16_t func_id) {

  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;
  uint64_t* lock = ins->getLock((uint64_t) start_addr);
  int spin_counter = 0;
  PatchResult* res = new PatchResult;
  if (lock != NULL) {
    if (__sync_bool_compare_and_swap(lock, 0 , 1)) {

      uint8_t* call_addr = (uint8_t*) call_return_addr - 5;
      uint64_t offset = (uint8_t*)call_addr - (uint8_t*)start_addr;

  /*
  Diagnostics
    print_decoded_output((uint8_t*)start_addr, offset);
  */

  // Address where EDI/RDI is set last before the call
      uint8_t* edi_set_addr = 0;
      _DInst* result = (_DInst*) malloc(sizeof(_DInst) * 2 * offset);
      unsigned int instructions_count = 0;

      _DecodedInst inst;

      _CodeInfo ci  = {0};
      ci.code = (uint8_t*)start_addr;
      ci.codeLen = offset;
      ci.dt = Decode64Bits;
      ci.codeOffset = 0x100000;

      distorm_decompose(&ci, result, offset, &instructions_count);
      uint64_t ptr_size = 0;
      uint64_t edi_offset = 0;
      uint8_t intermediate_reg = 0;

      if (instructions_count > offset) {
        fprintf(stderr, "[DEBUG] Instructions decoded : %d Offset : %lu\n", instructions_count, offset);
        free(result);

        res->success = false;
        res->conflict = false;

        while(!__sync_bool_compare_and_swap(lock, 1 , 0));
        return res;
      }

      bool edi_setter_found = false;
      int instruction_offset = 0;
      for (int i = instructions_count - 1; i >= 0; i--) {
        if (result[i].flags == FLAG_NOT_DECODABLE) {
          printf("Bad decode attempt.. Call address : %p \n", call_addr);
          free(result);

          res->success = false;
          res->conflict = false;

          while(!__sync_bool_compare_and_swap(lock, 1 , 0));
          return res;
        }

        distorm_format(&ci, &result[i], &inst);

        ptr_size += result[i].size;
        instruction_offset++;

        if (result[i].opcode == I_MOV) {
          if (!edi_setter_found && result[i].ops[0].type == O_REG && 
              (result[i].ops[0].index == R_EDI || result[i].ops[0].index == R_RDI)) {
            if (result[i].ops[1].type == O_IMM || result[i].ops[1].type == O_IMM1 || result[i].ops[1].type == O_IMM2) {
              edi_offset = ptr_size;
              break;
            } else if (result[i].ops[1].type == O_REG) {
              edi_setter_found = true;
              intermediate_reg  = result[i].ops[1].index;
            }
          } else if(edi_setter_found && result[i].ops[0].type == O_REG && 
              (reg_equal(intermediate_reg, result[i].ops[0].index))) {
            if (result[i].ops[1].type == O_IMM || result[i].ops[1].type == O_IMM1 || result[i].ops[1].type == O_IMM2) {
              edi_offset = ptr_size;
              break;
            } else if (result[i].ops[1].type == O_REG) {
              intermediate_reg  = result[i].ops[1].index;
            }
          }
        }
      }

      edi_set_addr = call_addr - edi_offset;

      // fprintf(stderr, "[DEBUG] Call address : %p EDI offset : %lu EDI set address : %p \n", call_addr, edi_offset, edi_set_addr);

  // Hack : If the edi setter and the call site are adjacent and edi setter straddles a cache line
  // we handle it specifically to escape patching it
  /*
  if (instruction_offset == 1) {
    size_t cache_line_size = sysconf(_SC_LEVEL3_CACHE_LINESIZE); 
    int edi_setter_cache_line_offset = (uint64_t) edi_set_addr % cache_line_size;
    int call_instruction_cache_line_offset = ((uint64_t) call_addr) % cache_line_size;
    if (edi_setter_cache_line_offset >= 57 ||
       call_instruction_cache_line_offset >= 57) {
      free(result);
      res->success = false;
      res->conflict = true;
      return res;
    }
  }
  */

      bool status = patch_with_value(edi_set_addr, func_id);
 
      free(result);

      if (!status) {
        res->success = false;
        res->conflict = false;
        return res;
        while(!__sync_bool_compare_and_swap(lock, 1 , 0));
       }

       res->success = true;
       res->conflict = false;
       res->edi_set_addr = edi_set_addr;

       while(!__sync_bool_compare_and_swap(lock, 1 , 0));
       return res;

    } else { // We failed. Wait until the other thread finish and just return
      
      //BJS: I dont understand this. 
      
#ifndef NDEBUG
      ticks t0 = getticks(); 
#endif 
      while (*lock) {
        if (spin_counter == INT_MAX) {
          fprintf(stderr, "RESETTING the counter\n");
          fprintf(stderr, "Returning without adding the probe since lock is : %lu\n", *lock);
          break;
        }
        spin_counter += 1;
      }
#ifndef NDEBUG
      pthread_t tid = pthread_self();
      ticks t1 = getticks();
      fprintf( stderr
	     , "[utils.hpp ***OLD*** patch_first_parameter] ThreadID: %lu was busy-waiting for %d iterations.\nWait took %llu ticks while attempting to patch function %d.\n"
	       , (unsigned long int)tid,spin_counter,(t1-t0),func_id);

#endif 



      res->success = true;
      res->conflict = false;

      return res;
    }
  }

  res->success = false;
  res->conflict = false;

  return res;

}


#endif /* _UTILS_HPP_ */
