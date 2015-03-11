
#include <inttypes.h>
#include <stdlib.h>
#include <alloca.h>
#include <setjmp.h>

#include "cyg_functions.hpp"
#include "patch_utils.hpp"
#include "finstrumentor.hpp"
#include "../../../common/include/cycle.h"

#include "distorm.h"
#include "mnemonics.h"

// How many instructions to allocate on stack.
#define MAX_INSTRUCTIONS 32

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

bool starts_with(const char *str, const char *pre) {
  size_t lenpre = strlen(pre), lenstr = strlen(str);
  return lenstr < lenpre ? false : strncmp(pre, str, lenpre) == 0;
}

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

  status = __sync_bool_compare_and_swap((uint64_t*)addr, *((uint64_t*)addr), masked_sequence);

  return status;

}

void print_decoded_output(uint8_t* start, uint64_t length) {

  _DecodeResult res;
  _DecodedInst disassembled[length];
  unsigned int decodedInstructionsCount = 0;
  _OffsetType offset = 0;

  res = distorm_decode(offset,
      (const unsigned char*)start,
      length,
      Decode64Bits,
      disassembled,
      length,
      &decodedInstructionsCount);

  for (int i = 0; i < decodedInstructionsCount; i++) {
    printf("(%02d) %-24s %s%s%s\r\n",
        // disassembled[i].offset,
        disassembled[i].size,
        (char*)disassembled[i].instructionHex.p,
        (char*)disassembled[i].mnemonic.p,
        disassembled[i].operands.length != 0 ? " " : "",
        (char*)disassembled[i].operands.p);
  }
}

inline uint8_t* patch_first_parameter(uint64_t* call_return_addr, uint64_t* start_addr, uint16_t func_id) {

  uint8_t* call_addr = (uint8_t*) call_return_addr - 5;

  uint64_t offset = (uint8_t*)call_addr - (uint8_t*)start_addr;

  /*
  Diagnostics

  bool is_print = true;

  if (is_print) {
    print_decoded_output((uint8_t*)start_addr, offset);
  }
  */

  // Address where EDI/RDI is set last before the call
  uint8_t* edi_set_addr = 0;

  _DInst result[offset];
  unsigned int instructions_count = 0;

  _DecodedInst inst;

  _CodeInfo ci = {0};
  ci.code = (uint8_t*)start_addr;
  ci.codeLen = offset;
  ci.dt = Decode64Bits;
  ci.codeOffset = 0x100000;

  distorm_decompose(&ci, result, offset, &instructions_count);
  uint64_t ptr_size = 0;
  uint64_t edi_offset = 0;
  uint8_t intermediate_reg = 0;

  bool edi_setter_found = false;
  for (int i = instructions_count - 1; i >= 0; i--) {
    if (result[i].flags == FLAG_NOT_DECODABLE) {
      printf("Bad decode attempt.. Call address : %p \n", call_addr);
      return 0;
    }

    distorm_format(&ci, &result[i], &inst);

    ptr_size += result[i].size;

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

  bool status = patch_with_value(edi_set_addr, func_id);

  if (!status) {
    return 0;
  }

  return edi_set_addr;

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

// TODO: Make this thread safe
inline void init_probe_info(uint64_t func_addr, uint8_t* probe_addr) {

  // fprintf(stderr, "Function address at init probe : %p\n", func_addr);
  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;

  // fprintf(stderr, "probe_info address at init probe : %p\n", ins->probe_info);
  if(ins->probe_info->find(func_addr) == ins->probe_info->end()) {
    std::list<FinsProbeInfo*>* probe_list = new std::list<FinsProbeInfo*>;
    // fprintf(stderr, "Probe list address for func id %d : %p\n", func_addr, probe_list);
    FinsProbeInfo* probeInfo = new FinsProbeInfo;
    probeInfo->probeStartAddr = (probe_addr-8);
    // probeInfo->activeSequence = (uint64_t)(probe_addr - 8); // Should be return address - 8
    probeInfo->isActive = 1;

    uint64_t sequence = *((uint64_t*)(probe_addr-8));
    uint64_t mask = 0x0000000000FFFFFF;
    uint64_t deactiveSequence = (uint64_t) (sequence & mask); 
    mask = 0x0000441F0F000000; // Mask with a 5 byte NOP

    probeInfo->activeSequence = sequence;
    probeInfo->deactiveSequence = deactiveSequence | mask;

    probe_list->push_back(probeInfo);
    // fprintf(stderr, "Adding probe %p for function %p\n", probe_addr, (uint64_t*) func_addr);
    ins->probe_info->insert(make_pair(func_addr, probe_list));
  } else {
    std::list<FinsProbeInfo*>* probe_list = ins->probe_info->find(func_addr)->second;
    for(std::list<FinsProbeInfo*>::iterator iter = probe_list->begin(); iter != probe_list->end(); iter++) {
      FinsProbeInfo* probeInfo= *iter;
      if (probeInfo->probeStartAddr == (probe_addr-8)) {
        return; // Probe already initialized. Nothing to do.
      }
    }

    FinsProbeInfo* probeInfo= new FinsProbeInfo;
    probeInfo->probeStartAddr = (probe_addr - 8);
    probeInfo->isActive = 1;

    uint64_t sequence = *((uint64_t*)(probe_addr-8));
    uint64_t mask = 0x0000000000FFFFFF; // Little endianness
    uint64_t deactiveSequence = (uint64_t) (sequence & mask); 
    mask = 0x0000441F0F000000;

    probeInfo->activeSequence = sequence;
    probeInfo->deactiveSequence = deactiveSequence | mask;

    probe_list->push_back(probeInfo);
  }
}

void print_probe_info() {

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
void update_overhead_histograms(TLStatistics* ts, uint64_t overhead, int type) {
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
    uint64_t total_probe_overhead = ts->prolog_overhead + overhead;
    if (total_probe_overhead > PROBE_HIST_MAX_VALUE) {
      bin = g_num_bins - 1;
    } else {
      bin = total_probe_overhead / BIN_SIZE;
    }
    g_probe_timings[bin]++;
  }
}
#endif

void __cyg_profile_func_enter(void* func, void* caller) {

  ticks start = getticks();
  // Experimental parameter patching code
  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;

  TLStatistics* ts;
  if ((uint64_t) func < 0x400200) {
    // fprintf(stderr, "\n[cyg_enter] Low function address  : %lu\n", ((uint64_t)func));
    ts = prologFunction((uint16_t)func);
    ticks end = getticks();
    uint64_t prolog_overhead = (end - start);
    ts->thread_local_overhead += prolog_overhead;
    ts->prolog_overhead = prolog_overhead;

#ifdef PROBE_HIST_ON
    update_overhead_histograms(ts, prolog_overhead, PROLOG); 
#endif

    return;
    // abort();
  }

  uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));

  if (addr < func) {
    // fprintf(stderr, "Function start is great than the cyg_enter return address.. Function address: %p Call address : %p \n", func, addr);
    ticks end = getticks();
    uint64_t prolog_overhead = (end - start);
    ts->thread_local_overhead += prolog_overhead;

#ifdef PROBE_HIST_ON
    update_overhead_histograms(ts, prolog_overhead, PROLOG); 
#endif

    return;
  }

  uint16_t func_id = get_func_id((uint64_t)func);
  // If the function id mappings are not properly initialized fail gracefully
  if (!func_id) { 
    return;
  }

  uint8_t* edi_set_addr = patch_first_parameter(addr, (uint64_t*) func, func_id);

  init_probe_info((uint64_t)func, (uint8_t*)addr);

  if (edi_set_addr == 0) {
    abort();
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

}

void __cyg_profile_func_exit(void* func, void* caller) {

  ticks start = getticks();
  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;

  TLStatistics* ts;
  if ((uint64_t) func < 0x400200) {
    // fprintf(stderr, "\n[cyg_exit] Low function address  : %lu\n", ((uint64_t)func));
    ts = epilogFunction((uint16_t)func);
    ticks end = getticks();
    uint64_t epilog_overhead = (end - start);
    ts->thread_local_overhead += epilog_overhead;

#ifdef PROBE_HIST_ON
    update_overhead_histograms(ts, epilog_overhead, EPILOG); 
#endif

    return;
  }

  /*
  if((uint64_t) func == 0x00000000004a2af0) {
    fprintf(stderr, "In Perl_pp_const\n");
  }
  */

  uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));

  if (addr < func) {
    // fprintf(stderr, "Function start is great than the cyg_exit return address.. Function address: %p Call address : %p \n", func, addr);
    ticks end = getticks();
    uint64_t epilog_overhead = (end - start);
    ts->thread_local_overhead += epilog_overhead;

#ifdef PROBE_HIST_ON
    update_overhead_histograms(ts, epilog_overhead, EPILOG); 
#endif

    return;
  }

  uint16_t func_id = get_func_id((uint64_t)func);
  // If the function id mappings are not properly initialized fail gracefully
  if (!func_id) {
    return;
  }

  uint8_t* edi_set_addr = patch_first_parameter(addr, (uint64_t*) func, func_id);

  init_probe_info((uint64_t)func, (uint8_t*)addr);

  if (edi_set_addr == 0) {
    abort();
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
