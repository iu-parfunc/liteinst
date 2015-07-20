
#include <unistd.h>
#include <inttypes.h>
#include <stdlib.h>
#include <alloca.h>
#include <setjmp.h>
#include <limits.h>

#include "cyg_functions.hpp"
#include "utils.hpp"

#include "finstrumentor.hpp"
#include "../../../common/include/cycle.h"
#include "bitmap.hpp"

#include "distorm.h"
#include "mnemonics.h"

// How many instructions to allocate on stack.
#define MAX_INSTRUCTIONS 32

extern uint64_t g_TicksPerNanoSec;

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
    // printf("CUTOFF POINT : %d\n", cutoff_point);

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

        FinsProbeInfo* probeInfo = populate_probe_info(probe_addr, unpatched);
        probe_list->push_back(probeInfo);
      }
    } else { // We failed. Wait until the other thread finish and just return
      
      //BJS: I dont understand this. 
      
      while (*lock) {
        if (spin_counter == INT_MAX) {
          fprintf(stderr, "RESETTING the counter\n");
          fprintf(stderr, "Returning without adding the probe since lock is : %lu\n", *lock);
          break;
        }
        spin_counter += 1;
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
  }

  uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
  uint16_t func_id = ins->getFunctionId((uint64_t)func);
  // uint16_t func_id = get_func_id((uint64_t)func);

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
  uint16_t func_id = ins->getFunctionId((uint64_t)func);
  // uint16_t func_id = get_func_id((uint64_t)func);

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

#endif

}
