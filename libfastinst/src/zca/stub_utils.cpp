
#include "zca_probe_provider.hpp"
#include "elf_utils.hpp"
#include "stub_utils.hpp"

#include <cassert>
#include <cstdio>
#include <iostream>
#include <sys/mman.h>
#include <errno.h>
#include <unistd.h>
#include <link.h>

namespace StubUtils {

  // Count how many islands we have allocated:
  static uint64_t num_islands = 0; 
  
  // The most recently allocated island
  static mem_island* current_alloc_unit;

  void print_fn(FuncId func_id) {
    printf("[DEFAULT] Func id is : %lu\n", func_id);
  }

  int genStubCode(Address stub_adress, Address probe_address,
      ZCAProbeMetaData* pmd) {
    using namespace asmjit;
    using namespace asmjit::x86;
 
    FuncId func_id = pmd->func_id;

    // printf("Function id for function %s at insertion is %lu \n", func_name, func_id);

    // This aims to make the same one-way jump as manual_jmp_there, except 
    // from JITed code.
    // -------------------------------------------------------------------
    JitRuntime runtime;
    X86Assembler a(&runtime);

#if LOGLEVEL >= DEBUG_LEVEL
    FileLogger logger(stderr);
    a.setLogger(&logger);
    a2.setLogger(&logger);
#endif

    // Push all volatile registers:
    a.push(rsi); a.push(rdi);a.push(rax); a.push(rcx); a.push(rdx);
    a.push(r8); a.push(r9); a.push(r10); a.push(r11);
    a.push(r12); a.push(r13); a.push(r14); a.push(r15);

    a.xor_(rdi,rdi);
    a.mov(rdx, imm((sysint_t)func_id));

    pmd->probe_offset = a.getCodeSize();

    a.call(imm((size_t)print_fn));
  
    a.pop(r15); a.pop(r14); a.pop(r13); a.pop(r12);
    a.pop(r11); a.pop(r10); a.pop(r9); a.pop(r8);
    a.pop(rdx); a.pop(rcx); a.pop(rax); a.pop(rdi); a.pop(rsi); 
    // This fixes the wierd seg fault which happens when -O3 is enabled

    a.jmp(imm((size_t)(void*)(probe_address + PROBESIZE)));

    int codesz = a.getCodeSize();
    // This works just as well, don't need the function_cast magic:
    size_t code = a.relocCode(stub_address);

    // pmd->probe_offset = codesz; // TODO: This is incorrect. But we don't
                                // use it at the moment

    // Copy over the displaced probe bytes:
    // TODO: Recopy from archived version and see what's wrong with it
 
    return (codesz);
  }


  void fillStub(Address probe_address, Address stub_address, 
      ZCAProbeMetaData* pmd) {
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
      fprintf(stderr "[Zca Probe Provider] mprotect was not successfull! code "
          "%d\n", code);
      fprintf(stderr, "[Zca Probe Provider] errno value is : %d\n", errno);
      assert(0);
      // return -1;
    }

    // If this probesite crosses a page boundary change the permissions of 
    // adjacent page too.
    if (page_size - ((uint64_t)probe_address)%page_size < PROBESIZE) {
      code = mprotect(
          (void*)(probe_address -((uint64_t)probe_address)%page_size+ page_size),
          page_size,
          PROT_READ | PROT_WRITE | PROT_EXEC);
      if (code) {
        /* current code page is now writable and code from it is allowed 
         * for execution */
        fprintf(stderr, "[Zca Probe Provider] mprotect was not successfull! code " 
            " %d\n", code);
        fprintf(stderr, "[Zca Probe Provider] errno value is : %d\n", errno);
        assert(0);
        // return -1;
      } 
    }

    int stub_size = genStubCode(stub_address, probe_address, fun, pmd);
    // Plug in the relative jump
    // Size of the JMP we insert is 5. So +5 points to the following instruction.
    long relative = (long)(stub_address - (uint64_t)probe_address - 5);
    //printf ("[Modify Probe site] Jump distance is : %lu\n", relative);

    fprintf(stderr, "[Zca Probe Provider] Relative offset of dest from " 
        "starting addr: %p %p, 32 bit: %d\n", probe_address, stub_address, 
        (int)relative);

    // TODO : Safe patch this using pointpatch
    probe_address[0] = 0xE9;
    *(uint32_t*)(probe_address+1) = (int)relative;
    probe_address[5] = 0x90;
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
      fprintf(stderr, "[Zca Probe Provider] MMAP failed!!\n");
      return -1; // We give up. Cannot allocate memory inside this memory region.
    } else {
      fprintf(stderr, "[Zca Probe Provider] MMAP_RETRIES: %ld\n", mmap_retry_attempts);
      return new_size;
    }
  }

  int allocate_memory_for_stub(Address probe_address, Address* stub_address) {
    int32_t mem_chunk = ((unsigned long)probe_address) >> 32;

    // If we are in the same memory region avoid the hash table lookup and 
    // just use the previously used memory island. Assumes the probes are 
    // processed in linearly increasing order of their addresses
    // This also deals with the memory island allocations for a given region 
    // after the first allocation
    if (current_alloc_unit != NULL && 
        mem_chunk == current_alloc_unit->mem_chunk) {
      if (current_alloc_unit->remaining_size >= STUB_SIZE) {
        *stub_address = current_alloc_unit->insertion_ptr;

        current_alloc_unit->insertion_ptr = 
          current_alloc_unit->insertion_ptr + STUB_SIZE;
        current_alloc_unit->remaining_size -= STUB_SIZE;

        // This is the last probe that can be allocated from this memory island.
        // Keep its address as reference. So the next memory island address can 
        // be calculated using this as a reference point. This assumes probes 
        // are processed in increasing order by ther addresses within a memory 
        // region
        if (current_alloc_unit->remaining_size < STUB_SIZE) {
          current_alloc_unit->last_probe_address = probe_address;
        }

        // LOG_DEBUG("Stub starting at %p \n", *stub_address);
      } else {

        // Allocate a second memory island for stubs in this memory region since
        // we have run out of space in the already allocated ones
        uint64_t region_size = (1LL<<32);
        Address new_island_start_addr = 
          (Address)(*((uint64_t*)current_alloc_unit->last_probe_address) + 
              region_size) / 2; 
        // Take the middle address
        
        uint64_t new_island_size = current_alloc_unit->unallocated_size;

        fprintf(stderr, "[Zca Probe Provider] MMAP_RETRIES: %ld\n", 
            mmap_retry_attempts);
        *stub_address = (Adress)mmap(&new_island_start_addr, 
            new_island_size, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE |
            MAP_FIXED| MAP_ANONYMOUS, -1,0);

        // Retry with smaller allocation size in case of faliure
        if (*stub_address == MAP_FAILED) {
          int alloc_size = retry_allocation(&new_island_start_addr, 
              new_island_size, stub_address);

          if (alloc_size == -1) {
            int err = errno;
            fprintf(stderr, "[Zca Probe Provider] Unable to allocate memory " 
                " within the region : %d. Annotations within this memory " 
                "region will not be profiled..\n",
                mem_chunk);
            return -1;
          } else {
            // This needs to be reallocated again at some place else
            current_alloc_unit->unallocated_size = new_island_size - alloc_size;
            current_alloc_unit->start_addr = *stub_address;
            current_alloc_unit->insertion_ptr = *stub_address + STUB_SIZE;
            current_alloc_unit->remaining_size = alloc_size - STUB_SIZE;
          }
        }

        // All's well. We got the amount we asked for.
        current_alloc_unit->start_addr = *stub_address;
        current_alloc_unit->mem_chunk = mem_chunk;
        current_alloc_unit->insertion_ptr += STUB_SIZE;
        current_alloc_unit->size = new_island_size;
        current_alloc_unit->remaining_size = new_island_size - STUB_SIZE;
      }

      return 0;
    }

    // This deals with initial memory island allocation for a given memory 
    // region
    auto it =  mem_allocations.find(mem_chunk);
    if (it != mem_allocations.end()) {
      // This is the first allocation of a memory island for this memory 
      // region.
      std::list<mem_island*>* mem_list = it->second;
      // Check first memory island
      mem_island* first_mem = mem_list->front();

      if (first_mem != NULL) {

        fprintf(stderr, "MMAP_RETRIES: %ld\n", mmap_retry_attempts);
        *stub_address = (Address)mmap(first_mem->start_addr, first_mem->size,
            PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_FIXED| 
            MAP_ANONYMOUS, -1,0);

        // Retry with smaller allocation size in case of faliure
        if (*stub_address == MAP_FAILED) {
          int alloc_size = retry_allocation(first_mem->start_addr, 
              first_mem->size, stub_address);
          if (alloc_size == -1) {
            int err = errno;
            fprintf(stderr, "[ZCA Probe Provider] Unable to allocate memory " 
                " within chunk: %d. Probe sites within this memory region will "
                " not be profiled..\n", mem_chunk);
            return -1;
          } else {
            first_mem->unallocated_size = first_mem->size - alloc_size;
            // This needs to be reallocated again at some place else
            first_mem->start_addr = *stub_address;
            first_mem->insertion_ptr = *stub_address + STUB_SIZE;
            first_mem->remaining_size = alloc_size - STUB_SIZE;
          }
        }

        // All's well. We got the amount we asked for.
        first_mem->start_addr = *stub_address;
        first_mem->insertion_ptr = *stub_address + STUB_SIZE;
        first_mem->remaining_size = first_mem->size - STUB_SIZE;
        // first_mem->allocated = true;
        first_mem->unallocated_size = 0;

        current_alloc_unit = first_mem;
        num_islands ++;
        // We just print this message every time and take the last one:
        fprintf(stderr, "[Zca Probe Provider] NUM_ISLANDS: %ld\n", num_islands);
      } else {
        return -1;
        // Error. Log and return
      }
    } else {
      // No memory island information available.
      return -1;
    }

    return 0;
  }

  ProbeVec setupStubs() {
    ZCAProbeProvider* ins = (ZCAProbeProvider*) PROBE_PROVIDER;
    ProbeVec pmdVec = readZCAELFMetaData(); // func_name, probe_id, probe_addr
    Address stub_address;

    for (auto it = pmdVec->begin(); it != pmdVec->begin(); ++it) {
      ZCAProbeMetaData* pmd = (ZCAProbeMetaData*) *it;
      int status = allocate_memory_for_stub(pmd->probe_addr, &stub_address);

      if (status != -1) {
        fillStub(pmd->probe_addr, stub_address, pmd);

        pmd->stub_address = stub_address;
        pmd->type = ProbeType::ZCA;
        pmd->state = ProbeState::UNINITIALIZED; 
        pmd->provider = ins;

        ins->registerProbe(pmd);
      } else {
        fprintf("[ZCA Probe Provider] Stub allocation failed for %p\n", 
            pmd->probe_addr);
      }
    }
  }

}
