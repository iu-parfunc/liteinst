
#include "probe_utils.hpp"

#include <stdlib.h>
#include <cassert>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <sys/mman.h>
#include <errno.h>
#include <unistd.h>
#include <map>
#include <list>

#define IS_IMMEDIATE(t)  ((t) == O_IMM || (t) == O_IMM1 || (t) == O_IMM2)

#define PROBE_SIZE 5

#define CHUNK_SIZE 4294967296 // 2^32

// Count how many islands we have allocated:
static uint64_t num_islands = 0; 
  
// The most recently allocated island
static MemIsland* current_alloc_unit;

// Memory island allocation table for each 2^32 memory region 
static MemAllocTable* mem_allocations = NULL;

// Retry attempts for memory allocation
static uint64_t mmap_retry_attempts = 0;

// The stub will do the following.
// 1. Save the context (registers).
// 2. Push argument to the register (RDI)
// 3. Call the trampoline function.
// 4. Restore the context (registers)
// 5. Jump to the next instruction at probe site
// -------------------------------------------------------------------
const uint8_t stub[] = 
 { 0x56, /* push %rsi */
   0x57, /* push %rdi */
   0x50, /* push %rax */
   0x51, /* push %rcx */
   0x52, /* push %rdx */
   0x41, 0x50, /* push %r8 */
   0x41, 0x51, /* push %r9 */
   0x41, 0x52, /* push %r10 */
   0x41, 0x53, /* push %r11 */
   0x41, 0x54, /* push %r12 */
   0x41, 0x55, /* push %r13 */
   0x41, 0x56, /* push %r14 */
   0x41, 0x57, /* push %r15 */
   // 0x48, 0x33, 0xff, /* xor %rdi,%rdi */
   // 0x48, 0xc7, 0xc7, 0x00, 0x00, 0x00, 0x00, /* mov $00,%rdi */
   0xff, 0x15, 0x1b, 0x00, 0x00, 0x00,       /* callq *0x1b(%rip) */
   0x41, 0x5f, /* pop %r15 */
   0x41, 0x5e, /* pop %r14 */
   0x41, 0x5d, /* pop %r13 */
   0x41, 0x5c, /* pop %r12 */
   0x41, 0x5b, /* pop %r11 */
   0x41, 0x5a, /* pop %r10 */
   0x41, 0x59, /* pop %r9 */
   0x41, 0x58, /* pop %r8 */
   0x5a, /* pop %rdx */
   0x59, /* pop %rcx */
   0x58, /* pop %rax */
   0x5f, /* pop %rdi */
   0x5e, /* pop %rsi */
   0x40, 0xe9, 0x00, 0x00, 0x00, 0x00, /* rex jmpq xx*/
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 /* trampoline fn address */};

const int stub_base_size = sizeof(stub);
const int mov_offset = 27;
const int jmp_offset = 50;
const int call_addr_offset = 54;

void genStubCode(Address probe_address, Address stub_address, 
    Address trampoline_fn_address, PICInstruction ins) {
  memcpy(stub_address, (const void*) ins.addr, ins.size); 
  memcpy(stub_address+ins.size, (const void*) stub, stub_base_size); 

  Address stub_common_base = stub_address + ins.size;
  Address jmp_address = stub_common_base + jmp_offset;
  int64_t relative = (int64_t) ((int64_t)(probe_address + ins.size) 
      - (int64_t)(jmp_address + 6 - 2));
  *(uint32_t*)(jmp_address) = (int32_t)relative;

  Address call_address = stub_common_base + call_addr_offset;

  init_patch_site((void*) call_address, 8);
  patch_64((void*) call_address, (uint64_t) trampoline_fn_address);
}

PICInstruction fillStub(Address probe_address, Address stub_address,
  Address trampoline_fn_address, PICInstruction ins) {
  // LOG_DEBUG("Probe address is : %p\n", (unsigned char*)probe_address);
  // LOG_DEBUG("Stub address is : %p\n", stub_address);

  long page_size = sysconf(_SC_PAGESIZE); 
  int code = mprotect(
      (void*)(probe_address - (((uint64_t)probe_address)%page_size)), 
      page_size,
      PROT_READ | PROT_WRITE | PROT_EXEC);

  if (code) {
    /* current code page is now writable and code from it is allowed for 
     * execution */
    fprintf(stderr, "[Messit] mprotect was not successfull! code "
        "%d\n", code);
    fprintf(stderr, "[Messit] errno value is : %d\n", errno);
    assert(0);
  }

  // If this probesite crosses a page boundary change the permissions of 
  // adjacent page too.
  if (page_size - ((uint64_t)probe_address)%page_size < PROBE_SIZE) {
    code = mprotect(
        (void*)(probe_address -((uint64_t)probe_address)%page_size+ page_size),
        page_size,
        PROT_READ | PROT_WRITE | PROT_EXEC);
    if (code) {
      /* current code page is now writable and code from it is allowed 
       * for execution */
      fprintf(stderr, "[Messit] mprotect was not successful! code " 
          " %d\n", code);
      fprintf(stderr, "[Messit] errno value is : %d\n", errno);
      assert(0);
      // return -1;
    } 
  }

  genStubCode(probe_address, stub_address, trampoline_fn_address, ins);
  // Plug in the relative jump
  // Size of the JMP we insert is 5. So +5 points to the following instruction.
  long relative = (long)(stub_address - (uint64_t)probe_address - 5);
  //printf ("[Modify Probe site] Jump distance is : %lu\n", relative);

  uint64_t original = ins.original;
  uint64_t mask = 0xFFFFFFFFFFFFFFFF << (ins.size * 8);
  uint64_t patched = original & mask; 
  uint8_t* patched_ptr = (uint8_t*) &patched;
  patched_ptr[0] = 0xE9;
  *(uint32_t*)(patched_ptr+1) = (int)relative;

  ins.patched = patched;

  return ins;
}

int retry_allocation(Address start_addr, uint64_t size,
    Address* stub_address) {

  // Try with decreasing sizes until we get space to fit an available memory 
  // hole
  uint64_t new_size = size / 2;
  while (*stub_address == MAP_FAILED && new_size >= 4096) {
    mmap_retry_attempts++;
    *stub_address = (Address)mmap(start_addr, new_size,
        PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_FIXED|
        MAP_ANONYMOUS, -1,0);
    new_size = new_size / 2;
  }

  if (*stub_address == MAP_FAILED) {
    fprintf(stderr, "[Messit] MMAP failed!!\n");
    return -1; // We give up. Cannot allocate memory inside this memory region.
  } else {
    // fprintf(stderr, "[Zca Probe Provider] MMAP_RETRIES: %ld\n", mmap_retry_attempts);
    return new_size;
  }
}

int allocate_memory_for_stub(Address probe_address, uint8_t reloc_instruction_size, 
    Address* stub_address) {
  int32_t mem_chunk = ((unsigned long)probe_address) >> 32;
  int stub_size = stub_base_size + reloc_instruction_size; 

  // If we are in the same memory region avoid the hash table lookup and 
  // just use the previously used memory island. Assumes the probes are 
  // processed in linearly increasing order of their addresses
  // This also deals with the memory island allocations for a given region 
  // after the first allocation
  if (current_alloc_unit != NULL && 
      mem_chunk == current_alloc_unit->mem_chunk) {
    if (current_alloc_unit->remaining_size >= stub_size) {
      *stub_address = current_alloc_unit->insertion_ptr;

      current_alloc_unit->insertion_ptr = 
        current_alloc_unit->insertion_ptr + stub_size;
        current_alloc_unit->remaining_size -= stub_size;

      // This is the last probe that can be allocated from this memory island.
      // Keep its address as reference. So the next memory island address can 
      // be calculated using this as a reference point. This assumes probes 
      // are processed in increasing order by ther addresses within a memory 
      // region
      if (current_alloc_unit->remaining_size < stub_size) {
        current_alloc_unit->last_probe_address = probe_address;
      }

      // LOG_DEBUG("Stub starting at %p \n", *stub_address);
    } else {

      // Allocate a second memory island for stubs in this memory region since
      // we have run out of space in the already allocated ones
      uint64_t region_size = (1LL<<32);
      Address new_island_start_addr = 
        (Address)((((uint64_t)current_alloc_unit->last_probe_address) + 
            region_size) / 2); 
      // Take the middle address
       
      uint64_t new_island_size = current_alloc_unit->unallocated_size;

      // fprintf(stderr, "[Zca Probe Provider] MMAP_RETRIES: %ld\n", 
      // mmap_retry_attempts);
      *stub_address = (Address)mmap(new_island_start_addr, 
            new_island_size, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE |
            MAP_FIXED| MAP_ANONYMOUS, -1,0);

       // Retry with smaller allocation size in case of faliure
       if (*stub_address == MAP_FAILED) {
         int alloc_size = retry_allocation(new_island_start_addr, 
           new_island_size, stub_address);

         if (alloc_size == -1) {
            int err = errno;
            fprintf(stderr, "[Messit] Unable to allocate memory " 
                " within the region : %d. Functions within this memory " 
                "region will not be messed with. Bummer...\n",
                mem_chunk);
            return -1;
         } else {
           // This needs to be reallocated again at some place else
           current_alloc_unit->unallocated_size = new_island_size - alloc_size;
           current_alloc_unit->start_addr = *stub_address;
           current_alloc_unit->insertion_ptr = *stub_address + stub_size;
           current_alloc_unit->remaining_size = alloc_size - stub_size;
         }
       }

       // All's well. We got the amount we asked for.
       current_alloc_unit->start_addr = *stub_address;
       current_alloc_unit->mem_chunk = mem_chunk;
       current_alloc_unit->insertion_ptr += stub_size;
       current_alloc_unit->size = new_island_size;
       current_alloc_unit->remaining_size = new_island_size - stub_size;
    }

    return 0;
  }

  // This deals with initial memory island allocation for a given memory 
  // region. First add the descriptor for memory island if it is not there.
  MemIsland* first_mem;
  if (mem_allocations == NULL) {
    mem_allocations = new MemAllocTable;
  }

  auto it =  mem_allocations->find(mem_chunk);
  if (it == mem_allocations->end()) {
    uint64_t chunk_start = (uint64_t)probe_address & 0xFFFFFFFF00000000; // Get 32 high order bits

    // This is the first allocation of a memory island for this memory 
    // region.
    std::list<MemIsland*>* mem_list = new std::list<MemIsland*>;
	  first_mem = new MemIsland;
    // We initially set the start address to the middle of the 2^32 chunk
	  first_mem->start_addr = (Address)((chunk_start + CHUNK_SIZE) / 2); 
	  first_mem->size = 4096; // For now hard code the memory island size
	  first_mem->mem_chunk = mem_chunk;

    mem_list->push_back(first_mem);
  	mem_allocations->insert(
        std::map<uint32_t, std::list<MemIsland*>*>::value_type(
          mem_chunk, mem_list));
  }

  // Then allocate the memory according to the memory island descriptor
  if (first_mem != NULL) {
    *stub_address = (Address)mmap(first_mem->start_addr, first_mem->size,
        PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_FIXED| 
        MAP_ANONYMOUS, -1,0);

    // Retry with smaller allocation size in case of faliure
    if (*stub_address == MAP_FAILED) {
      int alloc_size = retry_allocation(first_mem->start_addr, 
          first_mem->size, stub_address);
      if (alloc_size == -1) {
        int err = errno;
        fprintf(stderr, "[Messit] Unable to allocate memory " 
              " within chunk: %d. Functions within this memory region will "
              " not be profiled. Bummer..\n", mem_chunk);
        return -1;
      } else {
        first_mem->unallocated_size = first_mem->size - alloc_size;
        // This needs to be reallocated again at some place else
        first_mem->start_addr = *stub_address;
        first_mem->insertion_ptr = *stub_address + stub_size;
        first_mem->remaining_size = alloc_size - stub_size;
      }
    }

    // All's well. We got the amount we asked for.
    first_mem->start_addr = *stub_address;
    first_mem->insertion_ptr = *stub_address + stub_size;
    first_mem->remaining_size = first_mem->size - stub_size;
    // first_mem->allocated = true;
    first_mem->unallocated_size = 0;

    num_islands ++;
    // We just print this message every time and take the last one:
    // fprintf(stderr, "[Zca Probe Provider] NUM_ISLANDS: %ld\n", num_islands);

    current_alloc_unit = first_mem;
  } else {
    // No memory island information available.
    return -1;
  }

  return 0;
}

// Finds a position indpendent instruction large enough to hold a relative jump
// The chosen position indpendent instruction is mov imm reg
PICInstruction findPICInstruction(Address fn_addr) {
  // We hard code the decode to 1024 bytes maximum. In future be more smart with
  // retries on failure to find a suitable instruction
  uint64_t n_decode_bytes = 1024; // (uint64_t)((uint64_t)end_addr - (uint64_t)fn_addr);

  uint64_t ptr_size = 0;

  /* why * 2 * n_decode_bytes */
  _DInst* decoded = (_DInst*)malloc(sizeof(_DInst) * 2 * n_decode_bytes);
  unsigned int instruction_count = 0;

  _CodeInfo ci = {0}; /* another modernity */
  ci.code = fn_addr;
  ci.codeLen = n_decode_bytes;
  ci.dt = Decode64Bits;   /* here we make this AMD64 specific ? */
  ci.codeOffset = 0x100000;

  _DecodeResult res = distorm_decompose(&ci, decoded, n_decode_bytes, &instruction_count);
  if (res != DECRES_SUCCESS) {
    PICInstruction ins;
    ins.addr = (Address) -1;
    return ins;
  }

  /* Search through instructions from the start */
  for (unsigned int i = 0; i < instruction_count; i++) {
    /* skip non-decodable */
    if (decoded[i].flags == FLAG_NOT_DECODABLE) continue;

    if (decoded[i].opcode == I_MOV && decoded[i].size == PROBE_SIZE) {
      if (decoded[i].ops[0].type == O_REG &&
	        decoded[i].ops[0].index != R_RIP &&
          IS_IMMEDIATE(decoded[i].ops[1].type)) {

          PICInstruction ins;
          ins.addr = (Address) (fn_addr + ptr_size);
          ins.size = decoded[i].size;
          ins.original = (uint64_t)*((uint64_t*)(fn_addr + ptr_size)); 

          // Initializes the probe site for patching
          init_patch_site((void*) ins.addr, 8);

          return ins;
      }
    }

     /* increment a ptr, the size of the current instruction in memory */
    ptr_size += decoded[i].size;
  }

  abort();
  PICInstruction ins;
  ins.addr = (Address) -1;
  return ins;

}

Probe setupProbe(Address fn_addr, Address trampoline_fn_addr) {
  Address stub_addr;
  Probe p;

  PICInstruction ins = findPICInstruction(fn_addr);
  
  if (ins.addr < 0) {
    fprintf(stderr, "[Messit] Unable to setup stub for function : %p\n", 
        fn_addr);
    p.fn_addr = (Address) -1;
    return p;
  }

  int status = allocate_memory_for_stub(ins.addr, ins.size, &stub_addr);

  if (status != -1) {
    ins = fillStub(ins.addr, stub_addr, trampoline_fn_addr, ins);
    p.ins = ins;
    p.fn_addr = fn_addr;
  } else {
    fprintf(stderr, "[Messit] Stub allocation failed for %p\n", 
       stub_addr);
    p.fn_addr = (Address) -1;
    return p;
  }

  return p; 
}
