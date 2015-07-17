
#include <unistd.h>
#include <inttypes.h>
#include <stdlib.h>
#include <alloca.h>
#include <setjmp.h>
#include <limits.h>

#include "cyg_functions.hpp"
#include "patch_utils.hpp"

#include "finstrumentor.hpp"
#include "../../../common/include/cycle.h"
#include "bitmap.hpp"

#include "distorm.h"
#include "mnemonics.h"

// How many instructions to allocate on stack.
#define MAX_INSTRUCTIONS 32

extern uint64_t g_TicksPerNanoSec;

static uint8_t mov_encodings[20] = {MOV_REG_8, MOV_REG_16, MOV_MEM_8, MOV_MEM_16,
  MOV_IMM_8_RAX, MOV_IMM_8_RCX, MOV_IMM_8_RDX, MOV_IMM_8_RBX,
  MOV_IMM_8_RSP, MOV_IMM_8_RBP, MOV_IMM_8_RSI, MOV_IMM_8_RDI,
  MOV_IMM_16_RAX, MOV_IMM_16_RCX, MOV_IMM_16_RDX, MOV_IMM_16_RBX,
  MOV_IMM_16_RSP, MOV_IMM_16_RBP, MOV_IMM_16_RSI,  MOV_IMM_16_RDI};

/* Checks if register type is equal (for 32 and 64 bit variants) for all general purpose registers */
// 64bit - R_RAX, R_RCX, R_RDX, R_RBX, R_RSP, R_RBP, R_RSI, R_RDI, R_R8, R_R9, R_R10, R_R11, R_R12, R_R13, R_R14, R_R15,
// 32 bit - R_EAX, R_ECX, R_EDX, R_EBX, R_ESP, R_EBP, R_ESI, R_EDI, R_R8D, R_R9D, R_R10D, R_R11D, R_R12D, R_R13D, R_R14D, R_R15D,
bool reg_equal(uint8_t reg1, uint8_t reg2) {

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

inline bool check_if_mov(uint8_t progbit) {
  for (int i=0; i<20; i++) {
    if (mov_encodings[i] == progbit) {
      return true;
    } 
  }

  return false;
}

int counter = 0;

// BJS: This was unused
//static bool starts_with(const char *str, const char *pre) {
//  size_t lenpre = strlen(pre), lenstr = strlen(str);
//  return lenstr < lenpre ? false : strncmp(pre, str, lenpre) == 0;
//}

/*
bool disassemble_sequence(uint8_t* addr, uint8_t size, _DInst* result, uint8_t result_size) {

  unsigned int instructions_count = 0;

  _DecodedInst inst;

  _CodeInfo ci = {0};
  ci.code = (uint8_t*)addr;
  ci.codeLen = size;
  ci.dt = Decode64Bits;
  ci.codeOffset = 0x100000;

  distorm_decompose(&ci, result, result_size, &instructions_count);

  printf("--------- Probe site : %p ---------\n", addr);
  for (unsigned int i = 0; i < instructions_count; i++) {
    if (result[i].flags == FLAG_NOT_DECODABLE) {
      fprintf(stderr, "Error while decoding instructions at %p\n", addr);
      return false;
    }

    distorm_format(&ci, &result[i], &inst);
    printf("%s %s\n", inst.mnemonic.p, inst.operands.p);
  }

}
*/

static void print_string_hex(char *comment, unsigned char *str, int len)
{
  unsigned char *c;

  printf("%s", comment);
  for (c = str; c < str + len; c++) {
    printf("0x%02x ", *c & 0xff);
  }

  printf("\n");
}

static uint64_t get_lsb_mask(int nbytes) {
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

static uint64_t get_msb_mask(int nbytes) {
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

static void print_decoded_output(uint8_t* start, uint64_t length) {

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

inline PatchResult* patch_first_parameter(uint64_t* call_return_addr, uint64_t* start_addr, uint16_t func_id) {

  uint8_t* call_addr = (uint8_t*) call_return_addr - 5;

  uint64_t offset = (uint8_t*)call_addr - (uint8_t*)start_addr;

  /*
  Diagnostics
    print_decoded_output((uint8_t*)start_addr, offset);
  */

  // Address where EDI/RDI is set last before the call
  uint8_t* edi_set_addr = 0;

  // _DInst result[2*offset];
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

  PatchResult* res = new PatchResult;
  if (instructions_count > offset) {
    fprintf(stderr, "[DEBUG] Instructions decoded : %d Offset : %lu\n", instructions_count, offset);
    free(result);

    res->success = false;
    res->conflict = false;
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
  }

  res->success = true;
  res->conflict = false;
  res->edi_set_addr = edi_set_addr;
  return res;

}

// TODO : 
// 1. Assuming uint64_t function addresses not portable I guess. Change it to be portable
// 2. This needs to be protected by a lock
inline uint16_t get_func_id(uint64_t func_addr) {
  /*
  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;
  if(ins->functions->find(func_addr) == ins->functions->end()) {
    ins->functions->insert(func_table::value_type(func_addr, 
          __sync_fetch_and_add(&func_id_counter, 1)));
    ins->function_ids->insert(func_id_table::value_type(ins->functions->find(func_addr)->second, func_addr));
  }

  return ins->functions->find(func_addr)->second;
  */

  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;
  return ins->getFunctionId(func_addr);

}

static bool has_probe_info(uint64_t func_addr) {

  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;
  if (ins->probe_info->find(func_addr) == ins->probe_info->end()) {
    return false;
  } else {
    return true;
  }
}

static FinsProbeInfo* get_probe_info(uint64_t func_addr, uint8_t* addr) {
  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;
  if(ins->probe_info->find(func_addr) != ins->probe_info->end()) {
    std::list<FinsProbeInfo*>* probe_list = ins->probe_info->find(func_addr)->second;
    for(std::list<FinsProbeInfo*>::iterator iter = probe_list->begin(); 
      iter != probe_list->end(); iter++) {
      FinsProbeInfo* probeInfo= *iter;
      if (probeInfo->probeStartAddr == addr-5) {
        return probeInfo;
      }
    }
  }
  return NULL;
}

static bool is_prolog_initialized(uint64_t func_addr) {
  return has_probe_info(func_addr);
}

static FinsProbeInfo* populate_probe_info(uint8_t* probe_addr, bool unpatched){
  uint64_t* probe_start = (uint64_t*)(probe_addr - 5);

  FinsProbeInfo* probeInfo = new FinsProbeInfo;
  probeInfo->probeStartAddr = probe_addr-5;
  probeInfo->isActive = 1;
  probeInfo->unpatched = unpatched;

  uint64_t sequence = *probe_start;
  uint64_t mask = 0xFFFFFF0000000000;
  uint64_t deactive = (uint64_t) (sequence & mask); 
  mask = 0x0000000000441F0F; // Mask with a 5 byte NOP

  //uint64_t activeSequence = sequence;
  uint64_t deactiveSequence = deactive | mask;

  size_t cache_line_size = sysconf(_SC_LEVEL3_CACHE_LINESIZE); 
  int offset = (uint64_t) probe_start % cache_line_size;
  if (offset >= 57) { // If this is a cache line straddler
    int cutoff_point = 64 - offset;

    g_straddler_count++;
    printf("CUTOFF POINT : %d\n", cutoff_point);

    probeInfo->straddler = true;
   
    uint64_t* straddle_point = (uint64_t*)((uint8_t*)probe_start + cutoff_point);
    probeInfo->straddle_part_1_start = straddle_point - 1;
    probeInfo->straddle_part_2_start = straddle_point;
    probeInfo->activation_sequence_1 = *(probeInfo->straddle_part_1_start); 
    probeInfo->activation_sequence_2 = *(probeInfo->straddle_part_2_start); 

    int shift_size = 8 * (8 - cutoff_point);
    uint64_t int3mask = 0xCC;
    uint64_t ormask = 0xFF;

    int3mask = (int3mask << shift_size);
    ormask = (ormask << shift_size);

    probeInfo->straddle_int3_sequence = (*(probeInfo->straddle_part_1_start) & ~ormask) | int3mask;
      
    uint64_t temp0 = deactiveSequence & get_lsb_mask(cutoff_point);
    shift_size = (8 * (8-cutoff_point)); 
    temp0 = (temp0 << shift_size);
    uint64_t temp1 = probeInfo->activation_sequence_1 & (~get_msb_mask(cutoff_point));
    probeInfo->deactivation_sequence_1 = temp0 | temp1;

    temp0 = deactiveSequence & (~get_lsb_mask(cutoff_point));
    shift_size = (8 * cutoff_point);
    temp0 = (temp0 >> shift_size);
    temp1 = probeInfo->activation_sequence_2 & get_msb_mask(cutoff_point); 
    probeInfo->deactivation_sequence_2 = temp0 | temp1;
  } else {
    probeInfo->straddler = false;
  }

  probeInfo->activeSequence = sequence;
  probeInfo->deactiveSequence = deactiveSequence;

  return probeInfo;

}

// TODO: Make this thread safe
inline void init_probe_info(uint64_t func_addr, uint8_t* probe_addr, bool unpatched) {


  // fprintf(stderr, "Function address at init probe : %p\n", func_addr);
  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;

  uint64_t* lock = ins->getLock(func_addr);
  int spin_counter = 0;
  if (lock != NULL) {
    if (__sync_bool_compare_and_swap(lock, 0 , 1)) {
      // fprintf(stderr, "LOCKED %p : %lu\n", lock, *lock);
      // fprintf(stderr, "probe_info address at init probe : %p\n", ins->probe_info);
      if(ins->probe_info->find(func_addr) == ins->probe_info->end()) {
        std::list<FinsProbeInfo*>* probe_list = new std::list<FinsProbeInfo*>;
        // fprintf(stderr, "Probe list address for func id %d : %p\n", func_addr, probe_list);
        /*
        FinsProbeInfo* probeInfo = new FinsProbeInfo;
        probeInfo->probeStartAddr = (probe_addr-8);
        probeInfo->isActive = 1;

        uint64_t sequence = *probe_start;
        uint64_t mask = 0xFFFFFF0000000000;
        uint64_t deactive = (uint64_t) (sequence & mask); 
        mask = 0x0000000000441F0F; // Mask with a 5 byte NOP

        uint64_t activeSequence = sequence;
        uint64_t deactiveSequence = deactive | mask;

        size_t cache_line_size = sysconf(_SC_LEVEL3_CACHE_LINESIZE); 
        int offset = (uint64_t) probe_start % cache_line_size;
        if (offset >= 57) { // If this is a cache line straddler
          int cutoff_point = 64 - offset;
          printf("CUTOFF POINT : %d\n", cutoff_point);

          probeInfo->straddler = true;
   
          uint64_t* straddle_point = (uint64_t*)((uint8_t*)probe_start + cutoff_point);
          probeInfo->straddle_part_1_start = straddle_point - 1;
          probeInfo->straddle_part_2_start = straddle_point;
          probeInfo->activation_sequence_1 = *straddle_part_1_start; 
          probeInfo->activation_sequence_2 = *straddle_part_2_start; 

          int shift_size = 8 * (8 - cutoff_point);
          uint64_t int3mask = 0xCC;
          uint64_t ormask = 0xFF;

          int3mask = (int3mask << shift_size);
          ormask = (ormask << shift_size);

          probeInfo->straddle_int3_sequence = (*straddle_part_1_start & ~ormask) | int3mask;
      
          uint64_t temp0 = deactiveSequence & get_lsb_mask(cutoff_point);
          shift_size = (8 * (8-cutoff_point)); 
          temp0 = (temp0 << shift_size);
          uint64_t temp1 = probeInfo->activation_sequence_1 & (~get_msb_mask(cutoff_point));
          probeInfo->deactivation_sequence_1 = temp0 | temp1;

          temp0 = deactiveSequence & (~get_lsb_mask(cutoff_point));
          shift_size = (8 * cutoff_point);
          temp0 = (temp0 >> shift_size);
          temp1 = probeInfo->activation_sequence_2 & get_msb_mask(cutoff_point); 
          probeInfo->deactivation_sequence_2 = temp0 | temp1;
        }

        probeInfo->activeSequence = sequence;
        probeInfo->deactiveSequence = deactiveSequence;
        */

        FinsProbeInfo* probeInfo = populate_probe_info(probe_addr, unpatched);
        probe_list->push_back(probeInfo);
        // fprintf(stderr, "Adding probe %p for function %p\n", probe_addr, (uint64_t*) func_addr);
        ins->probe_info->insert(make_pair(func_addr, probe_list));
      } else {
        std::list<FinsProbeInfo*>* probe_list = ins->probe_info->find(func_addr)->second;
        for(std::list<FinsProbeInfo*>::iterator iter = probe_list->begin(); iter != probe_list->end(); iter++) {
          FinsProbeInfo* probeInfo= *iter;
          if (probeInfo->probeStartAddr == (probe_addr-8)) {
            while(!__sync_bool_compare_and_swap(lock, 1 , 0));
            // assert(*lock == 0);
            // fprintf(stderr, "UNLOCKING %p : %lu\n", lock, *lock);
            return; // Probe already initialized. Nothing to do.
          }
        }

        /*
        FinsProbeInfo* probeInfo= new FinsProbeInfo;
        probeInfo->probeStartAddr = (probe_addr - 8);
        probeInfo->isActive = 1;

        uint64_t sequence = *((uint64_t*)(probe_addr-8));
        uint64_t mask = 0x0000000000FFFFFF; // Little endianness
        uint64_t deactiveSequence = (uint64_t) (sequence & mask); 
        mask = 0x0000441F0F000000;

        probeInfo->activeSequence = sequence;
        probeInfo->deactiveSequence = deactiveSequence | mask;
        */

        FinsProbeInfo* probeInfo = populate_probe_info(probe_addr, unpatched);
        probe_list->push_back(probeInfo);
      }
    } else { // We failed. Wait until the other thread finish and just return
      // assert(*lock == 0);
      
      //BJS: I dont understand this. 
      
      while (*lock) {
        if (spin_counter == INT_MAX) {
          fprintf(stderr, "RESETTING the counter\n");
          fprintf(stderr, "Returning without adding the probe since lock is : %lu\n", *lock);
          break;
        }
        spin_counter += 1;
        /*
        if (counter++ % 100000 == 0) { // Try for a while and then give up
          // fprintf(stderr, "SPINNING at func : %lu\n", func_addr); 
          return;
          // goto loop;
        }
        */
      }

      return;
    }
  }

  while(!__sync_bool_compare_and_swap(lock, 1 , 0));

}

static void print_probe_info() {

  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;
  // fprintf(stderr, "Map size : %d\n", ins->probe_info->size());
  for(auto iter = ins->probe_info->begin(); iter != ins->probe_info->end(); iter++) {
    std::list<FinsProbeInfo*>* probe_list = iter->second;
    /// / fprintf(stderr, "Function address : %p\n", iter->first);

    for (std::list<FinsProbeInfo*>::iterator it = probe_list->begin(); it!= probe_list->end(); it++) {
      FinsProbeInfo* probeData = *it;

      fprintf(stderr, "Probe start address : %p\n", probeData->probeStartAddr);
    }

  }
}

#ifdef PROBE_HIST_ON
char* md = getenv("MONITOR_DEAC");

// Note : Calculations within this method is not thread safe
void update_overhead_histograms(TLStatistics* ts, uint64_t overhead, int type) {
  int bin;
  if (overhead > PROBE_HIST_MAX_VALUE) {
    bin = g_num_bins - 1;
  } else {
    bin = overhead / BIN_SIZE;
  }

  if (type) {
    g_prolog_timings[bin]++; 
    // Online calculation of mean and standara deviation
    // if (overhead < 1000000) { // Trying skip context switches
      g_prolog_count++;
      double delta = overhead - g_prolog_mean;
      g_prolog_mean = g_prolog_mean + delta/g_prolog_count;
      g_prolog_variance = g_prolog_variance + delta * (overhead - g_prolog_mean);
    // }
  } else {
    g_epilog_timings[bin]++;

    // Online calculation of mean and standara deviation
    // if (overhead < 1000000) { // Trying skip context switches
      g_epilog_count++;
      double delta = overhead - g_epilog_mean;
      g_epilog_mean = g_epilog_mean + delta/g_epilog_count;
      g_epilog_variance = g_epilog_variance + delta * (overhead - g_epilog_mean);
    // }

    uint64_t total_probe_overhead = ts->prolog_overhead + overhead;
    if (total_probe_overhead > PROBE_HIST_MAX_VALUE) {
      bin = g_num_bins - 1;
    } else {
      bin = total_probe_overhead / BIN_SIZE;
    }
    g_probe_timings[bin]++;

    // Online calculation of mean and standara deviation
    // if (total_probe_overhead < 1000000) { // Trying skip context switches
      g_total_probe_count++;
      delta = total_probe_overhead - g_probe_mean;
      g_probe_mean = g_probe_mean + delta/g_total_probe_count;
      g_probe_variance = g_probe_variance + delta * (total_probe_overhead - g_probe_mean);
    // }
  }
}
#endif

#ifdef PROBE_TRUE_EMPTY_ON

void update_empty_overheads(uint64_t overhead, int type) {
  int bin;
  if (overhead > PROBE_HIST_MAX_VALUE) {
    bin = g_num_bins - 1;
  } else {
    bin = overhead / BIN_SIZE;
  }

  if (type) {
    g_prolog_timings[bin]++; 
  } else {
    g_epilog_timings[bin]++;
  }
}

#endif

void __cyg_profile_func_enter(void* func, void* caller) {

#ifdef PROBE_TRUE_EMPTY_ON
  #ifdef PROBE_CPU_TIME
    struct timespec ts0;
    struct timespec ts1;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts0);
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts1);

    ticks start = (ticks)((ts0.tv_sec * 1000000000LL + ts0.tv_nsec) * g_TicksPerNanoSec);
    ticks end = (ticks)((ts1.tv_sec * 1000000000LL + ts1.tv_nsec) * g_TicksPerNanoSec);
  #else
    ticks start = getticks();
    ticks end = getticks();
  #endif

  update_empty_overheads(end - start, PROLOG); 
  return;
#else 
  #ifdef PROBE_CPU_TIME
    struct timespec ts0;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts0);
    ticks start = (ticks)((ts0.tv_sec * 1000000000LL + ts0.tv_nsec) * g_TicksPerNanoSec);
  #else
    ticks start = getticks();
  #endif


  // Experimental parameter patching code
  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;
  // uint64_t funcAddr = *((uint64_t*) func);

  // If the Ubiprof library has not yet been properly initialized return.
  // But caller parameter being -1 signals a special explicit invocation
  // of the instrumentation which is done for calibration purposes at 
  // the library init time. If that's the case we atually want to continue
  // executing.
  int64_t flag = (int64_t) caller;
  if (!g_ubiprof_initialized && flag != -1) {
    // fprintf(stderr, "Returning from func addr : %p\n", func);
    return;
  }

  // Explicitly set the function id for calibrate_cache_effects
  if (flag == -1) {
    func = (void*) (ins->getFunctionCount() - 1);
  }

  TLStatistics* ts;
  if ((uint64_t) func < 0x400200) {
    // fprintf(stderr, "\n[cyg_enter] Low function address  : %lu\n", ((uint64_t)func));
    // ts = prologFunction((uint16_t)func);
    ts = prologFunction((uint64_t)func);

  #ifdef PROBE_CPU_TIME
    struct timespec ts1;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts1);
    ticks end= (ticks)((ts1.tv_sec * 1000000000LL + ts1.tv_nsec) * g_TicksPerNanoSec);
  #else 
    ticks end = getticks();
  #endif

    uint64_t prolog_overhead = (end - start);
    ts->thread_local_overhead += prolog_overhead;
    ts->prolog_overhead = prolog_overhead;

#ifdef PROBE_HIST_ON
    update_overhead_histograms(ts, prolog_overhead, PROLOG); 
    /*
    if (!ts->deactivated) {
      if (md == NULL) {
        // fprintf(stderr, "Updating REGULAR flow\n");
        update_overhead_histograms(ts, prolog_overhead, PROLOG); 
      }
    } else {
      if (md != NULL) {
        fprintf(stderr, "Updating DEACTIVATION flow\n");
        update_overhead_histograms(ts, prolog_overhead, PROLOG); 
      }
      ts->deactivated = false;
    }
    */
#endif

    return;
    // abort();
  }

  uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));

  uint16_t func_id = get_func_id((uint64_t)func);

  // This is a straddler. Return without patching.
  // if (get_index(g_straddlers_bitmap, func_id)) {
    // fprintf(stderr, "RETURNING FROM STRADDLER\n");
    // return;
  // }

  // /*
  /* Newly commented ..
   *
  uint64_t* probe_start = (uint64_t*)((uint8_t*) addr - 5);
  size_t cache_line_size = sysconf(_SC_LEVEL3_CACHE_LINESIZE); 

  // bool word_aligned = (uint64_t) probe_start % 8 == 0 ? 1 : 0;
  // bool int_aligned = (uint64_t) probe_start % 32 == 0 ? 1 : 0;
  bool cache_aligned = (uint64_t) probe_start % cache_line_size == 0 ? 1 : 0;

  // FinsProbeMetaData* pmd = new FinsProbeMetaData;
  // pmd->addr = (uint64_t) probe_start;
  // pmd->word_aligned = word_aligned;
  // pmd->int_aligned = int_aligned;
  // pmd->cache_aligned = cache_aligned;

  // g_probe_meta_data->push_front(pmd);

  // Add to probe statistics
  
  int offset = (uint64_t) probe_start % cache_line_size;
  if (offset >= 57) {

  }

  if (!cache_aligned) {
    // return;
    if (offset >= 57) {
      // fprintf(stderr, "[cyg_enter] Disabling function: %s\n", ins->getFunctionName(func_id).c_str());
      // set_index(g_straddlers_bitmap, func_id);

      // if (func_id == 408) {
      //   fprintf(stderr, "SETTING bit map for enter : %d\n", func_id);
      // }
      // std::list<FinsProbeInfo*>* ls = ins->getProbes((void*)&func_id);
      
      // int before = -1;
      // if (ls != NULL && !ls->empty()) {
      //  before = ls->size();
      // }

      // int lock = *(ins->getLock((uint64_t) func));
      // if (lock != 0) {
      //  fprintf(stderr, "Func id before is : %d\n", func_id);
      //  fprintf(stderr, "Func name : %s\n", ins->getFunctionName(func_id).c_str());
      // }

      // assert(*(ins->getLock((uint64_t) func)) == 0);
      init_probe_info((uint64_t)func, (uint8_t*)addr);

      // assert(*(ins->getLock((uint64_t) func)) == 0);

      // int res = ins->deactivateProbe((void*)&func_id, FUNC);
      // fprintf(stderr, "Deactivation result : %d Before : %d After : %d\n", res, before, after);
      return;
    }


  }
  */

  if (addr < func) {
    // fprintf(stderr, "Function start is great than the cyg_enter return address.. Function address: %p Call address : %p \n", func, addr);
  #ifdef PROBE_CPU_TIME
    struct timespec ts1;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts1);
    ticks end= (ticks)((ts1.tv_sec * 1000000000LL + ts1.tv_nsec) * g_TicksPerNanoSec);
  #else 
    ticks end = getticks();
  #endif

    uint64_t prolog_overhead = (end - start);
    ts->thread_local_overhead += prolog_overhead;

  #ifdef PROBE_HIST_ON
    update_overhead_histograms(ts, prolog_overhead, PROLOG); 
  #endif

    return;
  }

  // If the function id mappings are not properly initialized fail gracefully
  if (!func_id && flag != -1) { 
    fprintf(stderr, "Returning from func addr : %p\n", func);
    return;
  }

  // Dirty hack to get around seg faults when accessing the STL meta data map at
  // initialization time
  /*
  if (flag == -1) {
    func_id = ins->getFunctionCount();
    func_id++;
  }
  */

  FinsProbeInfo* probe_info = get_probe_info((uint64_t) func, (uint8_t*) addr);
  if (probe_info != NULL && probe_info->unpatched) {
    ; // Escape to just executing prolog function
  } else {
    PatchResult* res  = patch_first_parameter(addr, (uint64_t*) func, func_id);

    if (!res->success) {
      if (res->conflict) {
        fprintf(stderr, "[Finstrumentor] Detected straddler conflict at %p ..\n", (void*)addr);
      }
        
      init_probe_info((uint64_t)func, (uint8_t*)addr, true);
      fprintf(stderr, "[Finstrumentor] Adding straddler conflict at %p  function %p with probe info unpatched at %p ..\n", func, (void*)addr, (void*)probe_info);
      probe_info = get_probe_info((uint64_t) func, (uint8_t*) addr);
      fprintf(stderr, "[Finstrumentor] After adding conflict at %p function %p : %p\n", (void*)addr, func, (void*)probe_info->unpatched);

      // Mark this as a function to escape patching
      set_index(g_straddlers_bitmap, func_id);

    } else {
      init_probe_info((uint64_t)func, (uint8_t*)addr, false);
    }

    delete res;
  }

  ts = prologFunction(func_id);
  ticks end = getticks();
  ts->thread_local_overhead += (end - start);
  uint64_t prolog_overhead = (end - start);
  ts->prolog_overhead = prolog_overhead;

#ifdef PROBE_HIST_ON
    update_overhead_histograms(ts, prolog_overhead, PROLOG); 
#endif

  // fprintf(stderr, "\n[cyg_enter] Function address : %p\n", ((uint8_t*)func));
  // fprintf(stderr, "[cyg_enter] Call address : %p\n", ((uint8_t*)addr - 5));
  // fprintf(stderr, "[cyg_enter] EDI set addresss : %p\n", edi_set_addr);
  // fprintf(stderr, "[cyg_enter] Call site size : %lu\n", (uint8_t*)addr - edi_set_addr);

  /*
     uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
     uint16_t func_id = get_func_id((uint64_t)func);

     init_probe_info((uint64_t)func, (uint8_t*)addr);

  // Delegates to actual profiler code
  prologFunction(func_id);
  */

#endif
}

void __cyg_profile_func_exit(void* func, void* caller) {

#ifdef PROBE_TRUE_EMPTY_ON
  #ifdef PROBE_CPU_TIME
    struct timespec ts0;
    struct timespec ts1;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts0);
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts1);

    ticks start = (ticks)((ts0.tv_sec * 1000000000LL + ts0.tv_nsec) * g_TicksPerNanoSec);
    ticks end = (ticks)((ts1.tv_sec * 1000000000LL + ts1.tv_nsec) * g_TicksPerNanoSec);
  #else
    ticks start = getticks();
    ticks end = getticks();
  #endif

  update_empty_overheads(end - start, EPILOG); 
  return;
#else 
  #ifdef PROBE_CPU_TIME
    struct timespec ts0;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts0);
    ticks start = (ticks)((ts0.tv_sec * 1000000000LL + ts0.tv_nsec) * g_TicksPerNanoSec);
  #else
    ticks start = getticks();
  #endif

  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;
  // uint64_t funcAddr = *((uint64_t*) func);

  // If the Ubiprof library has not yet been properly initialized return.
  // But caller parameter being -1 signals a special explicit invocation
  // of the instrumentation which is done for calibration purposes at 
  // the library init time. If that's the case we atually want to continue
  // executing.
  int64_t flag = (int64_t) caller;
  if (!g_ubiprof_initialized && flag != -1) {
    // fprintf(stderr, "Returning from func addr : %p\n", func);
    return;
  }

  // Explicitly set the function id for calibrate_cache_effects
  if (flag == -1) {
    func = (void*) (ins->getFunctionCount() - 1);
  }

  TLStatistics* ts;
  if ((uint64_t) func < 0x400200) {
    // fprintf(stderr, "\n[cyg_exit] Low function address  : %lu\n", ((uint64_t)func));
    // ts = epilogFunction((uint16_t)func);
    ts = epilogFunction((uint64_t)func);

  #ifdef PROBE_CPU_TIME
    struct timespec ts1;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts1);
    ticks end= (ticks)((ts1.tv_sec * 1000000000LL + ts1.tv_nsec) * g_TicksPerNanoSec);
  #else 
    ticks end = getticks();
  #endif

    uint64_t epilog_overhead = (end - start);
    ts->thread_local_overhead += epilog_overhead;

  #ifdef PROBE_HIST_ON
    update_overhead_histograms(ts, epilog_overhead, EPILOG); 
    /*
    if (!ts->deactivated) {
      if (md == NULL) {
        // fprintf(stderr, "Updating REGULAR flow\n");
        update_overhead_histograms(ts, epilog_overhead, EPILOG); 
      }
    } else {
      if (md != NULL) {
        fprintf(stderr, "Updating DEACTIVATION flow\n");
        update_overhead_histograms(ts, epilog_overhead, EPILOG); 
      }
      ts->deactivated = false;
    }
    */
  #endif

    return;
  }

  uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));

  uint16_t func_id = get_func_id((uint64_t)func);

  // This is a straddler. Return without patching.
  // /*
  /*
  if (get_index(g_straddlers_bitmap, func_id)) {
    fprintf(stderr, "[cyg_exit] Disabling function trivial: %s\n", ins->getFunctionName(func_id).c_str());
    init_probe_info((uint64_t)func, (uint8_t*)addr);
    int res = ins->deactivateProbe((void*)&func_id, FUNC);
    return;
  }
  */

  /* Newly commented..
   *
  uint64_t* probe_start = (uint64_t*)((uint8_t*) addr - 8);
  size_t cache_line_size = sysconf(_SC_LEVEL3_CACHE_LINESIZE); 

  // bool word_aligned = (uint64_t) probe_start % 8 == 0 ? 1 : 0;
  // bool int_aligned = (uint64_t) probe_start % 32 == 0 ? 1 : 0;
  bool cache_aligned = (uint64_t) probe_start % cache_line_size == 0 ? 1 : 0;

  // FinsProbeMetaData* pmd = new FinsProbeMetaData;
  // pmd->addr = (uint64_t) probe_start;
  // pmd->word_aligned = word_aligned;
  // pmd->int_aligned = int_aligned;
  // pmd->cache_aligned = cache_aligned;

  // g_probe_meta_data->push_front(pmd);

  // Add to probe statistics
  
  int offset = (uint64_t) probe_start % cache_line_size;
  if (!cache_aligned) {
    if (offset >= 57) {
      fprintf(stderr, "[cyg_exit] Disabling function: %s\n", ins->getFunctionName(func_id).c_str());
      g_straddler_count++;

      // if (func_id == 408) {
      //   fprintf(stderr, "SETTING bit map for exit : %d\n", func_id);
      // }

      // set_index(g_straddlers_bitmap, func_id);
      // std::list<FinsProbeInfo*>* ls = ins->getProbes((void*)&func_id);

      // int before;
      // if (ls != NULL) {
      //   before = ls->size();
      // }

      // assert(*(ins->getLock((uint64_t) func)) == 0);
      init_probe_info((uint64_t)func, (uint8_t*)addr);
      // assert(*(ins->getLock((uint64_t) func)) == 0);

      int res = ins->deactivateProbe((void*)&func_id, FUNC);
      // fprintf(stderr, "Deactivation result : %d Before : %d After : %d\n", res, before, after);
      return;
    }

  }
  */

  if (addr < func) {
    // fprintf(stderr, "Function start is great than the cyg_exit return address.. Function address: %p Call address : %p \n", func, addr);
  #ifdef PROBE_CPU_TIME
    struct timespec ts1;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts1);
    ticks end= (ticks)((ts1.tv_sec * 1000000000LL + ts1.tv_nsec) * g_TicksPerNanoSec);
  #else 
    ticks end = getticks();
  #endif

    uint64_t epilog_overhead = (end - start);
    ts->thread_local_overhead += epilog_overhead;

  #ifdef PROBE_HIST_ON
    update_overhead_histograms(ts, epilog_overhead, EPILOG); 
  #endif

    return;
  }

  // If the function id mappings are not properly initialized fail gracefully
  if (!func_id && flag != -1) { 
    fprintf(stderr, "Returning from func addr : %p\n", func);
    return;
  }

  // Dirty hack to get around seg faults when accessing the STL meta data map at
  // initialization time
  /*
  if (flag == -1) {
    func_id = ins->getFunctionCount();
    func_id++;
  }
  */

  // For some reason (mostly compiler scrweing things up at prolog) the prolog has not been properly initialized. 
  if (!is_prolog_initialized((uint64_t)func)) {
    return;
  }

  FinsProbeInfo* probe_info = get_probe_info((uint64_t) func, (uint8_t*) addr);
  if (probe_info != NULL && probe_info->unpatched) {
    ; // Escape to just executing epilog function
  } else {
    PatchResult* res  = patch_first_parameter(addr, (uint64_t*) func, func_id);

    if (!res->success) {
      if (res->conflict) {
        fprintf(stderr, "[Finstrumentor] Detected straddler conflict at %p ..\n", (void*)addr);
      }
        
      init_probe_info((uint64_t)func, (uint8_t*)addr, true);
      fprintf(stderr, "[Finstrumentor] Adding straddler conflict at %p function %p  with probe info unpatched at %p ..\n", (void*)addr, func, (void*)probe_info);
      probe_info = get_probe_info((uint64_t) func, (uint8_t*) addr);
      fprintf(stderr, "[Finstrumentor] After adding conflict at %p function %p : %d\n", (void *)addr, func, probe_info->unpatched);
      
      // Mark this as a function to escape patching
      set_index(g_straddlers_bitmap, func_id);
    } else {
      init_probe_info((uint64_t)func, (uint8_t*)addr, false);
    }

    delete res;
  }

  ts = epilogFunction(func_id);
  ticks end = getticks();
  ticks epilog_overhead = (end - start);
  ts->thread_local_overhead += epilog_overhead;

#ifdef PROBE_HIST_ON
    update_overhead_histograms(ts, epilog_overhead, EPILOG); 
#endif

  /*
  // Experimental parameter patching code
  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;

  if ((uint64_t) func < 0x400200) {
    // call the prolog function directly
    // fprintf(stderr, "\n[cyg_exit] Low function address  : %lu\n", ((uint64_t)func));
  
    return;
    // abort();
  }

  uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));

  if (addr < func) {
    fprintf(stderr, "Function start is great than the cyg_exit return address.. Function address: %p Call address : %p \n", func, addr);
    return;
  }

  uint8_t* edi_set_addr = patch_first_parameter(addr, (uint64_t*)func,  527);

  // fprintf(stderr, "\n[cyg_exit] Function address : %p\n", ((uint8_t*)func));
  // fprintf(stderr, "[cyg_exit] Call address : %p\n", ((uint8_t*)addr - 5));
  // fprintf(stderr, "[cyg_exit] EDI set addresss : %p\n", edi_set_addr);
  // fprintf(stderr, "[cyg_exit] Call site size : %lu\n", (uint8_t*)addr - edi_set_addr);

  if (edi_set_addr == 0) {
    abort();
  }
  */

  /*
     uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));

     uint16_t func_id = get_func_id((uint64_t)func);

     init_probe_info((uint64_t)func, (uint8_t*)addr);

  // Delegates to actual profiler code
  epilogFunction(func_id);
  */

#endif

}

/*


inline uint8_t* patch_first_parameter_1(uint64_t* call_return_addr, uint64_t* start_addr, uint16_t func_id) {

  uint8_t* call_addr = (uint8_t*) call_return_addr - 5;

  // Initially assuming first function parameter is set somewhere within 8 bytes before the call
  // We keep increasing this limit until we find the parameter set loation
  int call_site_start_offset = 8;

  // Address where EDI/RDI is set last before the call
  uint8_t* edi_set_addr = 0;

  while (call_site_start_offset < 512) {
    uint8_t* probe_start = call_addr - call_site_start_offset; 

    // Get to the first MOV instruction to start decoding the sequence
    uint8_t idx;
    for (idx = 0; idx < call_site_start_offset; idx++) { 
      bool is_mov = check_if_mov(*(probe_start+idx)); 
      if (is_mov) {
        break;
      }
    }

    probe_start += idx;

    bool rex_prefix_set = false;

    // Include the REX prefix if there is a one
    if (*(probe_start-1) == REX_PREFIX_0 || *(probe_start-1) == REX_PREFIX_1 ) {
      probe_start -= 1;
      rex_prefix_set = true;
    }

    uint8_t probe_site_size = call_addr - probe_start;

    // printf("++++++++++ Return address : %p ++++++++++ \n", caller);
    // printf("++++++++++ Call address : %p ++++++++++ \n", call_addr);

    do {
      // Dissasemble the call site
      _DInst result[call_site_start_offset];
      unsigned int instructions_count = 0;

      _DecodedInst inst;

      _CodeInfo ci = {0};
      ci.code = (uint8_t*)probe_start;
      ci.codeLen = probe_site_size;
      ci.dt = Decode64Bits;
      ci.codeOffset = 0x100000;

      distorm_decompose(&ci, result, call_site_start_offset, &instructions_count);

      bool bad_data = false;

      if (result[0].opcode != I_MOV) { // If the first instruction is not a MOV as we expected
        bad_data = true;
        printf("Bad decode attempt 2.. Call address : %p \
            Call site start offset : %d Probe start : %p \n", call_addr, call_site_start_offset, probe_start);
      } else {
        uint8_t ptr = 0;
        for (int i = 0; i < instructions_count; i++) {
          if (result[i].flags == FLAG_NOT_DECODABLE) {
            bad_data = true;
            printf("Bad decode attempt.. Call address : %p \
                Call site start offset : %d Probe start : %p \n", call_addr, call_site_start_offset, probe_start);
            break;
          }

          distorm_format(&ci, &result[i], &inst);
          // printf("%s %s\n", inst.mnemonic.p, inst.operands.p);

          if (result[i].opcode == I_MOV) {
            if (result[i].ops[0].type == O_REG && 
                (result[i].ops[0].index == R_EDI || result[i].ops[0].index == R_RDI)) {
              edi_set_addr = probe_start + ptr;
            }
          }

          ptr += result[i].size;

        }
      }

      if (!bad_data && edi_set_addr != 0) {
        goto end_outer_loop;
      } else {
        uint8_t resume_idx = idx;
        for ( ;idx < call_site_start_offset; ) { 
          if (rex_prefix_set) {
            idx += 2; // Skip REX prefix and increment pointer beyond current MOV instruction
          } else {
            idx++;
          }

          bool is_mov = check_if_mov(*(probe_start+idx-resume_idx)); 
          if (is_mov) {
            break;
          }
        }

        if (idx == call_site_start_offset) {
          break;
        }

        probe_start += (idx - resume_idx);

        // Include the REX prefix if there is a one
        if (*(probe_start-1) == REX_PREFIX_0 || *(probe_start-1) == REX_PREFIX_1 ) {
          probe_start -= 1;
        }

        probe_site_size = call_addr - probe_start;

      }

    } while (idx < call_site_start_offset);

    call_site_start_offset = call_site_start_offset << 1; // *2

  }

end_outer_loop:

  if (edi_set_addr == 0) {
    LOG_ERROR("Probesite doesn't seem to follow SysV ABI. Call address: %p. Skipping..\n",  call_addr);
    return 0;
  }

  bool status = patch_with_value(edi_set_addr, func_id);

  if (!status) {
    return 0;
  }

  return edi_set_addr;

}


   bool decode_instructions(void *caller) {

   uint8_t* call_addr = (uint8_t*) caller - 5;
   uint8_t* probe_start = call_addr - 32;

   uint8_t idx;
   for (idx = 0; idx < 32; idx++) { 
   bool is_mov = check_if_mov(*(probe_start+idx)); 
   if (is_mov) {
   break;
   }
   }

   probe_start += idx;

// Include the REX prefix if there is a one
if (*(probe_start-1) == REX_PREFIX_0 || *(probe_start-1) == REX_PREFIX_1 ) {
probe_start -= 1;
}

uint8_t probe_site_size = call_addr - probe_start;
// printf("++++++++++ Return address : %p ++++++++++ \n", caller);
// printf("++++++++++ Call address : %p ++++++++++ \n", call_addr);

do {

// printf("---------- Probe site : %p   ----------\n", probe_start);

// Dissasemble the probe site
_DInst result[32];
unsigned int instructions_count = 0;

_DecodedInst inst;

_CodeInfo ci = {0};
ci.code = (uint8_t*)probe_start;
ci.codeLen = probe_site_size;
ci.dt = Decode64Bits;
ci.codeOffset = 0x100000;

distorm_decompose(&ci, result, 32, &instructions_count);

bool bad_data = false;
uint8_t ptr = 0;
for (unsigned int i = 0; i < instructions_count; i++) {
if (result[i].flags == FLAG_NOT_DECODABLE) {
bad_data = true;
printf("Bad decode attempt..\n");
break;
}

distorm_format(&ci, &result[i], &inst);
// printf("%s %s\n", inst.mnemonic.p, inst.operands.p);

if (i == 0) {
if (result[i].opcode != I_MOV) {
bad_data = true;
printf("Bad decode attempt..\n");
break;
}
}

if (result[i].opcode == I_MOV) {
if (result[i].ops[0].type == O_REG && 
(result[i].ops[0].index == R_EDI || result[i].ops[0].index == R_RDI ||
result[i].ops[0].index == R_ESI || result[i].ops[0].index == R_RSI)) {
break;
}
}

ptr += result[i].size;
}

if (!bad_data) {
  probe_start += ptr;
  break;
} else {
  uint8_t resume_idx = idx;
  for (; idx < 8; idx++) { 
    idx++;
    bool is_mov = check_if_mov(*(probe_start+idx-resume_idx)); 
    if (is_mov) {
      break;
    }
  }

  if (idx >= 8) {
    LOG_ERROR("Probesite is too small (<8). Size : %d Location : %p. Skipping..\n", probe_site_size,  probe_start);
    return false;
  }

  probe_start += (idx - resume_idx);

  // Include the REX prefix if there is a one
  if (*(probe_start-1) == REX_PREFIX_0 || *(probe_start-1) == REX_PREFIX_1 ) {
    probe_start -= 1;
  }

  probe_site_size = call_addr - probe_start;

}

} while (idx < 8);

if (idx >= 8) {

}

probe_site_size = call_addr - probe_start;
// printf("\n Probe site size : %d\n", probe_site_size);

if (probe_site_size >= 10) {
  // Direct call patching strategy
}

if (probe_site_size >= 8) {

} else {

}

return true;

}
*/

/*
   void __cyg_profile_func_enter_1(void* func, void* caller) {

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

bool status = modify_page_permissions(probe_start);
if (!status) {
LOG_ERROR("Patching the probesite failed at %p. Skipping..\n", ptr);
return;
}

// Negative 5 deducts the size of the jump itself. Jump distance calculated from next address after jmp 
uint8_t relative = 16 - probe_start_idx - 5;

//  Patch with the temporary jump to skip executing next instructions within probe site until it is written with proper call site information 
status = patch_with_jmp(ptr, probe_start_idx, relative);

status = patch_params(ptr, probe_start_idx, 230);

// LOG_DEBUG("CAS status for jmp is :%d..\n", status);

// status = patch_with_call(addr, (uint64_t)print_fn);
// LOG_DEBUG("CAS status for call is :%d..\n", status);

// #endif

LOG_INFO("print_fn is at %p\n", &print_fn);

// uint64_t addr = (uint64_t)__builtin_extract_return_addr(__builtin_return_address(0));
// uint64_t distance = ((uint64_t)print_fn - addr);

// fprintf(stderr, "print_fn is : %p addr is at : %p \n", &print_fn, (unsigned char*) addr);
// fprintf(stderr, "Distance is : %lu with %lu 2^32 segments away\n", (unsigned long) distance, (unsigned long) distance >> 32);

// print_fn(1);

// instrumentation_func epilog = (instrumentation_func) INSTRUMENTOR_INSTANCE->get_profiler_epilog();

}

void __cyg_profile_func_enter_0(void* func, void* caller) {

uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
fprintf(stderr, "Function Address is : %p\n", (unsigned char*) func);
fprintf(stderr, "Call site Address is : %p\n", (unsigned char*) addr);

unsigned long distance = (unsigned long)addr - (unsigned long)func;
fprintf(stderr, "Distance is : %lu\n", distance);

fprintf(stderr, "@@@@@@@@@@@ Function Prolog @@@@@@@@@@@@@\n");
//   disassemble_sequence((uint8_t*)addr);

}
*/

    /*
    switch (offset) {
      case 57:
      case 58:
      case 59:
      case 60:
      case 61:
      case 62:
      case 63:
        fprintf(stderr, "SETTING bit map for : %d\n", func_id);
        set_index(g_straddlers_bitmap, func_id);
        // Check if this straddler was already encountered
        if (!(ins->probe_info->find((uint64_t)func) == ins->probe_info->end())) {
          std::list<FinsProbeInfo*>* probe_list = ins->probe_info->find((uint64_t)func)->second;
          for(std::list<FinsProbeInfo*>::iterator iter = probe_list->begin(); iter != probe_list->end(); iter++) {
            FinsProbeInfo* probeInfo= *iter;
            if (probeInfo->probeStartAddr == (uint8_t*)probe_start) {
                return;
            }
          } 
        }
        FinsCacheStraddler* fcs = new FinsCacheStraddler;
        fcs->addr = (uint64_t) probe_start;
        fcs->cutoff = cache_line_size - offset;
        g_cache_straddlers->push_front(fcs);

        FILE* fp = fopen("straddlers.out", "a");
        fprintf(fp, "%llx %lu\n", probe_start, cache_line_size - offset);
        fclose(fp);
        return;
      default:
        ;
    }
    */

    /*
    switch (offset) {
      case 57:
      case 58:
      case 59:
      case 60:
      case 61:
      case 62:
      case 63:
        fprintf(stderr, "SETTING bit map for : %d\n", func_id);
        set_index(g_straddlers_bitmap, func_id);
        // Check if this straddler was already encountered
        if (!(ins->probe_info->find((uint64_t)func) == ins->probe_info->end())) {
          std::list<FinsProbeInfo*>* probe_list = ins->probe_info->find((uint64_t)func)->second;
          for(std::list<FinsProbeInfo*>::iterator iter = probe_list->begin(); iter != probe_list->end(); iter++) {
            FinsProbeInfo* probeInfo= *iter;
            if (probeInfo->probeStartAddr == (uint8_t*)probe_start) {
                return;
            }
          } 
        }
        FinsCacheStraddler* fcs = new FinsCacheStraddler;
        fcs->addr = (uint64_t) probe_start;
        fcs->cutoff = cache_line_size - offset;
        g_cache_straddlers->push_front(fcs);

        FILE* fp = fopen("straddlers.out", "a");
        fprintf(fp, "%llx %lu\n", probe_start, cache_line_size - offset);
        fclose(fp);
        return;
      default:
        ;
    }
    */
