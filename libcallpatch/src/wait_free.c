
#include <wait_free.h> // #include "patch_utils.h"

#include <sys/mman.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h>
#include <execinfo.h>

/* -----------------------------------------------------------------
   MACROES
   ----------------------------------------------------------------- */

#if defined(CAS_WRITE) 
#warning "CAS WRITE" 
#define WRITE(addr,value) __sync_bool_compare_and_swap((addr), *(addr), (value))
#elif defined(NONATOMIC_WRITE)
#warning "NONATOMIC WRITE" 
#define WRITE(addr,value)  (addr)[0] = (value)

#elif defined(LUKE_ATOMIC_WRITE) 
#warning "LUKE's atomic write" 
#define WRITE(addr, val) do {              \
    __asm volatile ("":::"memory");                 \
    *(addr) = val;                                  \
  } while (0)

#else
#warning "ATOMIC WRITE"
#define WRITE(addr,value) __atomic_store_n((addr),(value),__ATOMIC_SEQ_CST)
#endif  

/* -----------------------------------------------------------------
   DEFINES 
   ----------------------------------------------------------------- */

#define TABLE_SIZE 64 

/* -----------------------------------------------------------------
 *    Globals
 * ----------------------------------------------------------------- */

uint8_t* g_trampoline_table[TABLE_SIZE];

/* -----------------------------------------------------------------
   internally used
   ----------------------------------------------------------------- */

bool set_page_rwe(void *addr,size_t nbytes);
uint64_t get_msb_mask_64(int nbytes);
uint64_t get_lsb_mask_64(int nbytes);
uint32_t get_msb_mask_32(int nbytes);
uint32_t get_lsb_mask_32(int nbytes);

bool handle_1_4_split(void* addr, uint64_t patch_value);
bool handle_2_3_split(void* addr, uint64_t patch_value);
bool handle_3_2_split(void* addr, uint64_t patch_value);
bool handle_4_1_split(void* addr, uint64_t patch_value);

// Checks if the patch is for a probe deactivation
bool is_a_deactivation(uint64_t patch_value);

uint8_t* allocate_degenerate_trampoline(void* addr);

static void dump_trace() {
  void * buffer[255];
  const int calls = backtrace(buffer,
        sizeof(buffer) / sizeof(void *));
  backtrace_symbols_fd(buffer, calls, 1);
}

/* -----------------------------------------------------------------
   Interface Implementation 
   ----------------------------------------------------------------- */

bool patch_call_64(void* addr, uint64_t patch_value) {
  // dump_trace();
  int offset = (uint64_t)addr % g_cache_lvl3_line_size;
  /* Is this a straddler patch ? */
  if (offset > g_cache_lvl3_line_size - 8) {
    unsigned int cutoff_point = g_cache_lvl3_line_size - offset;
    uint64_t* straddle_point = (uint64_t*)((uint8_t*)addr + cutoff_point);
    assert((uint64_t)straddle_point % g_cache_lvl3_line_size == 0);

    switch(cutoff_point) {
      case 1:
        return handle_1_4_split(addr, patch_value);
      case 2:
        return handle_2_3_split(addr, patch_value);
      case 3:
        return handle_3_2_split(addr, patch_value);
      case 4:
        return handle_4_1_split(addr, patch_value);
      default:
        // This is bit iffy. But we are yet to see failures at 
        // straddlers greater than 5 when patching a call instruction.
        // Alternatively treat these similar to 2_3 or 3_2 split.
        WRITE((uint64_t*)addr,patch_value);
        return true;
    }
  }

  /* if not a straddler perform a single write */
  WRITE((uint64_t*)addr,patch_value);

  // printf("[callpatch] Patched probe site %p\n", (char*) addr);
  return true;
}

/* -----------------------------------------------------------------
   CODE Internal
   ----------------------------------------------------------------- */

/* Constructor function that intializes constants */
__attribute__((constructor))
void init_call_patcher() {
  /* Should be able to just #define these values
     by investigating what arch/os we are running on.
     Then no need for this init phase */
  g_page_size=sysconf(_SC_PAGESIZE);
  g_cache_lvl3_line_size=sysconf(_SC_LEVEL3_CACHE_LINESIZE);

}

__attribute__((destructor))
void destroy_call_patcher() {
  /* Should be able to just #define these values
     by investigating what arch/os we are running on.
     Then no need for this init phase */
  int i, num_regions = 0;
  for (i=0; i< TABLE_SIZE; i++) {
    if (g_trampoline_table[i]) { 
      num_regions++;
    }
  }

  printf("Number of regions : %d\n", num_regions);

}

/* -----------------------------------------------------------------
 *    Private Helpers
 * ---------------------------------------------------------------- */

uint8_t* allocate_degenerate_trampoline(void* addr) {
  // Try to allocate half way between given address and 2^32 range from it
  uint8_t* alloc_addr = (uint8_t*)(((uint64_t) addr + (1LL<<32)) / 2); 
  // printf("Allocation address : %p\n", alloc_addr);
  uint8_t* tramp_addr = (uint8_t*)mmap(alloc_addr, sizeof(uint8_t),
                      PROT_READ|PROT_WRITE|PROT_EXEC, 
                      MAP_SHARED|MAP_ANONYMOUS, -1,0);

  if (tramp_addr == MAP_FAILED) {
    perror("mmap failed..\n");
  } 

  // TODO: Implement some kind of retry mechanism in case initial faliure
  assert(tramp_addr != MAP_FAILED);
  assert((uint64_t)tramp_addr < (uint64_t) addr + (1LL<<32));

  tramp_addr[0] = 0xC3; // x86 ret
  return tramp_addr;
}

uint8_t* get_trampoline(void* addr) {
  int i;
  for (i=0; i< TABLE_SIZE; i++) {
    if (!g_trampoline_table[i]) { 
      WRITE(&g_trampoline_table[i], allocate_degenerate_trampoline(addr));
      return g_trampoline_table[i];
    }

    if (abs(g_trampoline_table[i] - (uint8_t*)addr) < (1LL<<32)) {
      return g_trampoline_table[i];
    }
  }

  // This shouldn't happen. We have overflowed the trampoline table.
  assert(i < TABLE_SIZE);
  return 0;
}

bool is_a_deactivation(uint64_t patch_value) {
  uint64_t val = patch_value & get_lsb_mask_64(5); // Extract out 5 LSB
  // printf("patch value inside is_a_deactivation : %X\n", patch_value);
  // printf("val inside is_a_deactivation : %X\n", val);
  if (val == 0x0000000000441F0F) { // Check if we have NOOP
    return true;
  }  
  return false;
}

bool handle_1_4_split(void* addr, uint64_t patch_value) {
  // printf("Handling 1_4 split..\n");
  uint64_t* straddle_point = (uint64_t*)((uint8_t*) addr + 1);
  // The word after straddle point
  uint64_t after = *straddle_point;
  // New value to be patched in to 'after'
  uint64_t patch_after = 0;

  if (is_a_deactivation(patch_value)) {
    // printf("Handling 1_4 deactivation..\n");
    uint64_t tramp_addr = (uint64_t) get_trampoline(addr);
    uint32_t jump_distance = (uint32_t) (tramp_addr - ((uint64_t) addr + 5));

    // Mask out the call instruction bytes
    after &= get_msb_mask_64(4);

    // Calculate the new word to be patched for 'after'
    (&patch_after)[0] = jump_distance;
    patch_after |= after;
  } else {
    // Retain the last byte to be plugged in to patch_after for the void
    // left by shifting e8 out
    after &= get_msb_mask_64(1); 
    // Shift out call instruction opcode (e8) out since it is not in this part 
    // of the straddler
    uint64_t patch_after = patch_value >> 8;
    uint64_t tmp = patch_after | after;
    WRITE(straddle_point, tmp);
    // patch_after |= after;
    return true;
  }

  WRITE(straddle_point, patch_after);
  return true;
}

bool handle_2_3_split(void* addr, uint64_t patch_value) {
  uint64_t* straddle_point = (uint64_t*)((uint8_t*) addr + 2);
  // The word before straddle point
  uint64_t before = *(straddle_point-1);
  // New value to be patched in to 'before'
  uint64_t patch_before = 0;

  if (is_a_deactivation(patch_value)) {
    // Mask out two straddling bytes of the CALL instruction
    before &= get_lsb_mask_64(6);
    // Relative short jump to skip call instruction
    // Jump distance is taken from after the jump instruction itself hence 03
    // EB JMP rel8 = EB03 (03EB little endian)
    patch_before = 0x03EB; 
    patch_before <<= (8 * 6);

    patch_before |= before;
  } else {
    // Mask out two straddling bytes of the CALL instruction
    before &= get_lsb_mask_64(6);
    // Extract first two bytes of the CALL instruction
    patch_before = (patch_value & get_lsb_mask_64(2)) << (8 * 6);
    patch_before |= before;
  }

  WRITE(straddle_point-1,patch_before);
  return true;
}

bool handle_3_2_split(void* addr, uint64_t patch_value) {
  uint64_t* straddle_point = (uint64_t*)((uint8_t*) addr + 3);
  // The word before straddle point
  uint64_t before = *(straddle_point-1);
  // New value to be patched in to 'before'
  uint64_t patch_before = 0;

  if (is_a_deactivation(patch_value)) {
    // Mask out three straddling bytes of the CALL instruction
    before &= get_lsb_mask_64(5);
    // Relative short jump to skip call instruction
    // Jump distance is taken from after the jump instruction itself hence 03
    // EB JMP rel8 = EB03 (03EB little endian)
    patch_before = 0x03EB; 
    patch_before <<= (8 * 5);

    patch_before |= before;
  } else {
    // Mask out three straddling bytes of the CALL instruction
    before &= get_lsb_mask_64(5);
    // Extract first three bytes of the CALL instruction
    patch_before = (patch_value & get_lsb_mask_64(3)) << (8 * 5);
    patch_before |= before;
  }

  WRITE(straddle_point-1,patch_before);
  return true;
}

bool handle_4_1_split(void* addr, uint64_t patch_value) {
  uint64_t* straddle_point = (uint64_t*)((uint8_t*) addr + 4);
  // The word before straddle point
  uint64_t before = *(straddle_point-1);
  // New value to be patched in to 'before'
  uint64_t patch_before = 0;

  if (is_a_deactivation(patch_value)) {
    // Mask out four straddling bytes of the CALL instruction
    before &= get_lsb_mask_64(4);
    // Relative short jump to skip call instruction
    // Jump distance is taken from after the jump instruction itself hence 03
    // EB JMP rel8 = EB03 (03EB little endian)
    patch_before = 0x03EB; 
    patch_before <<= (8 * 4);

    patch_before |= before;
  } else {
    // Mask out four straddling bytes of the CALL instruction
    before &= get_lsb_mask_64(4);
    // Extract first four bytes of the CALL instruction
    patch_before = (patch_value & get_lsb_mask_64(4)) << (8 * 4);
    patch_before |= before;
  }

  WRITE(straddle_point-1,patch_before);
  return true;
}
