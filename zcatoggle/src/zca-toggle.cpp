
/** 
 * @brief test file descr
 * @details detailed descr
 */

#include <stdio.h>

#include <iostream>
#include "zca-toggle.h"
#include "elf-provider.h"
#include "logger.h"   // LOG_DEBUG
#include "cycle.h"    // ticks
// #include "zca-utils.h"
#include "zca-types.hpp"
#include <sys/mman.h>

#include "AsmJit/AsmJit.h"
// #include "zca-utils.h"
#include <errno.h>

// ann_data* annotations;

#define PROBESIZE 6
#define DWORD_SIZE 8

int gen_stub_code(unsigned char* addr, unsigned char* probe_loc, void* target_fn)
{

  using namespace AsmJit;
    
  // This aims to make the same one-way jump as manual_jmp_there, except from JITed code.
  // --------------------------------------------------------------------------------
  Assembler a, a2;

#if LOGLEVEL >= DEBUG_LEVEL

  FileLogger logger(stderr);
  a.setLogger(&logger);
  a2.setLogger(&logger);

#endif

  // Push all volatile registers:
  a.push(rax); a.push(rcx); a.push(rdx);
  a.push(r8); a.push(r9); a.push(r10); a.push(r11);
  a.call(imm((sysint_t)target_fn));
  // Restore all volatile registers:
  a.pop(r11); a.pop(r10); a.pop(r9); a.pop(r8);
  a.pop(rdx); a.pop(rcx); a.pop(rax);

  int codesz = a.getCodeSize();
  // This works just as well, don't need the function_cast magic:
  sysuint_t code = a.relocCode(addr);

  // Copy over the displaced probe bytes:
  for(int i=0; i<PROBESIZE; i++)
    addr[codesz + i] = probe_loc[i];

  // Next generate the jump back home:
  a2.jmp(imm((sysint_t)(void*)(probe_loc + PROBESIZE)));
  int sz2 = a2.getCodeSize();
  a2.relocCode(addr + codesz + PROBESIZE);

  // TEMP: Fill with NOOPS:
  /*
    for(int i=0; i<1000; i++)
    addr[codesz + PROBESIZE + sz2 + i] = 0x90;
  */

  //#if LOGLEVEL >= DEBUG_LEVEL
  char buf[1024];
  for(int i=0; i<codesz + PROBESIZE + sz2; i++) {

    sprintf(&buf[5 * i], " %p", addr[i]);

  }

  LOG_DEBUG("%s\n", buf);
  //#endif

  return (codesz+ PROBESIZE + sz2);
}

void get_allocated_stub_memory_for_probe(unsigned char* probe_address, unsigned long** stub_address) {

	int32_t mem_chunk = ((unsigned long)probe_address) >> 32;

    if (mem_allocations.find(mem_chunk) == mem_allocations.end()) {
       // Error. Log and return
    } else {
    	std::list<mem_island*>* mem_list = mem_allocations.find(mem_chunk)->second;
    	// Check first memory island
    	mem_island* first_mem = mem_list->front();

    	if (first_mem != NULL) {
    		if (first_mem->allocated) {
    			if (first_mem->remaining_size > STUB_SIZE) {
    				*stub_address = first_mem->insertion_ptr;
    				first_mem->insertion_ptr = (unsigned long*)((byte*) first_mem->insertion_ptr + STUB_SIZE);
    				first_mem->remaining_size -= STUB_SIZE;

    				LOG_DEBUG("Stub starting at %p \n", *stub_address);
    			} else {
    				// allocate a second chunk
    			}
    		} else {
    			// unsigned long base = align_to_page_boundary(first_mem->start_addr);
    			// size_t size = align_to_page_boundary(first_mem->size);
    			*stub_address = (unsigned long*)mmap(first_mem->start_addr, first_mem->size,
    					PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_FIXED| MAP_ANONYMOUS, -1,0);

    			if (*stub_address == MAP_FAILED) {
    				printf("Map failed..\n");
    			    int err = errno;
    			    LOG_ERROR("Got error on mmap: %s\n", strerror(err));
    			    return; //exit(1);
    			}

    			first_mem->start_addr = *stub_address;
    			first_mem->insertion_ptr = (unsigned long*)((byte*) *stub_address + STUB_SIZE);
    			first_mem->remaining_size -= STUB_SIZE;
    			first_mem->allocated = true;
    		}
    	} else {
    		// Error. Log and return
    	}
    }
}

void setupStubs()
{
  // Retrieve annotation data
  int probe_count=read_self_zca_probes();

  // Calculate memory requirement and allocate memory for the stubs
  //------------------------------------------------------------
  // Second, create a 32-bit addressable scratch area.
  //------------------------------------------------------------
/*  unsigned long* base = (unsigned long*)0x01230000;// TODO : Figure out how to make sure this memory island is
  //        reachable with short jmp without hard coding addresses

  LOG_DEBUG("Base address is : %p\n\n", base);
  base = (unsigned long*)mmap(base, 40960, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_FIXED| MAP_ANONYMOUS, -1,0);
  if (base == MAP_FAILED) {
    int err = errno;
    LOG_ERROR("Got error on mmap: %s\n", strerror(err));
    return; //exit(1);
  }*/

  // LOG_DEBUG("Mmap'd scratch region starting at %p\n", base);

  unsigned long* stub_address;

  int i = 0;
  for (auto iter = annotations.begin(); iter != annotations.end(); ++iter, i++) {

    std::pair<zca_row_11_t*, unsigned long*> data = iter->second;

    unsigned char* probe_address = (unsigned char*) (data.first->anchor);

    get_allocated_stub_memory_for_probe(probe_address, &stub_address);

    LOG_DEBUG("Probe address is : %p\n", (unsigned char*)probe_address);
    LOG_DEBUG("Stub address is : %p\n", stub_address);

    int page_size = 4096;
    int code = mprotect((void*)(probe_address - (((unsigned long)probe_address)%4096)), page_size,
    		PROT_READ | PROT_WRITE | PROT_EXEC);

    if (code) {
      /* current code page is now writable and code from it is allowed for execution */
      LOG_ERROR("mprotect was not successfull! code %d\n", code);
      LOG_ERROR("errno value is : %d\n", errno);
      // return -1;
    }

    int stub_size = gen_stub_code((unsigned char*)(stub_address), probe_address, print_fn/*(&annotations[i])->fun*/);
    // ((&annotations[i])->fun)(); This works
    // Plug in the relative jump
    // This does a relative jump:
    probe_address[0] = 0xE9;
    // Size of the JMP we insert is 5.  So +5 points to the following instruction.
    long relative = (long)(((unsigned char*)(void*) stub_address) - probe_address - 5);

    LOG_DEBUG("  Relative offset of dest from starting addr: %p %p, 32 bit: %d\n", probe_address, stub_address, (int)relative);

    *(uint32_t*)(probe_address+1) = (int)relative;
    probe_address[5] = 0x0;

    // Next stub address
    // stub_address = (unsigned long*) ((byte*) stub_address + (stub_size + 1));
  }
  // Generate stubs for each of them and modify the probe sites to jump to them
  // Initialize the book keeping data structure mapping probe site to generated stub address for
  // subsequent activations after a deactivation
}


int activateProbe(const probe_t* label, probe_callable_t callback)
{
	return 0;
}

int deactivateProbe(const probe_t* label) {
	return 0;
}


/* This function is called automatically when the library is loaded */
// __attribute__((constructor)) 
// FIXME: trying to get this attribute((constructor)) business to work even in a statically linked library.
// I think it actually only works for shared libraries.
void initZCAService() {
	/* Read all annotations here and setup stubs. How best to do it (sync or async) needs to be emperically determined */
#ifdef PROFILE
	ticks start;
	ticks end;
	ticks elapsed_time;

	start = getticks();
#endif

	setupStubs();

#ifdef PROFILE
	end = getticks();
	elapsed_time = elapsed(end, start);
	printf("\n--- Init time (cycles): %llu \n", elapsed_time);
#endif

	LOG_DEBUG("This text is printed before reaching \"main\".\n");
	return;
}



