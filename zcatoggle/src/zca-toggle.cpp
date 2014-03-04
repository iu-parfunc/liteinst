
/** 
 * @brief test file descr
 * @details detailed descr
 */

#include <stdio.h>

#include <iostream>
#include "zca-toggle.hpp"
#include "elf-provider.h"
#include "logger.h"   // LOG_DEBUG
#include "cycle.h"    // ticks
#include <sys/time.h>
// #include "zca-utils.h"
#include "zca-types.hpp"
#include <sys/mman.h>

#include "AsmJit/AsmJit.h"
// #include "zca-utils.h"
#include <errno.h>
#include <unistd.h>

// ann_data* annotations;

#define PROBESIZE 6
#define DWORD_SIZE 8

#ifdef PROFILE
static ticks stub_gen_elapsed_time;
static struct timeval stub_gen_tvDiff;
#endif

static int probe_count;
static mem_island* current_alloc_unit;

inline int gen_stub_code(unsigned char* addr, unsigned char* probe_loc, void* target_fn)
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

  //#if LOGLEVEL >= DEBUG_LEVEL
  char buf[1024];
  for(int i=0; i<codesz + PROBESIZE + sz2; i++) {

    sprintf(&buf[5 * i], " %p", addr[i]);

  }

  LOG_DEBUG("%s\n", buf);
  //#endif

  return (codesz+ PROBESIZE + sz2);
}

inline int retry_allocation(unsigned long* start_addr, unsigned long size, unsigned long** stub_address) {
  // Try with decreasing sizes until we get space to fit an available memory hole
  unsigned long new_size = size / 2;
  while (*stub_address == MAP_FAILED && new_size >= 4096) {
    *stub_address = (unsigned long*)mmap(start_addr, new_size,
					 PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_FIXED| MAP_ANONYMOUS, -1,0);
    new_size = new_size / 2;
  }

  if (*stub_address == MAP_FAILED) {
    return -1; // We give up. Cannot allocate memory inside this memory region.
  } else {
    return new_size;
  }
}

inline int get_allocated_stub_memory_for_probe(unsigned char* probe_address, unsigned long** stub_address) {

  int32_t mem_chunk = ((unsigned long)probe_address) >> 32;

  // If we are in the same memory region avoid the hash table lookup and just use the previously used memory island
  // Assumes the probes are processed in linearly increasing order of their addresses
  // This also deals with the memory island allocations for a given region after the first allocation
  if (current_alloc_unit != NULL && mem_chunk == current_alloc_unit->mem_chunk) {
    if (current_alloc_unit->remaining_size >= STUB_SIZE) {
      *stub_address = current_alloc_unit->insertion_ptr;

      // This is the last probe that can be allocated from this memory island. Keep its address as reference
      // So the next memory island address can be calculated using this as a reference point
      // This assumes probes are processed in increasing order by ther addresses within a memory region
      if (current_alloc_unit->remaining_size == STUB_SIZE) {
	current_alloc_unit->last_probe_address = (unsigned long*)probe_address;
      }

      current_alloc_unit->insertion_ptr = (unsigned long*)((byte*) current_alloc_unit->insertion_ptr + STUB_SIZE);
      current_alloc_unit->remaining_size -= STUB_SIZE;

      LOG_DEBUG("Stub starting at %p \n", *stub_address);
    } else {

      // Allocate a second memory island for stubs in this memory region since we have run out of space
      // in the already allocated ones
      uint64_t region_size = (1LL<<32);
      unsigned long new_island_start_addr = (*(current_alloc_unit->last_probe_address) + region_size) / 2; // Take the middle address
      unsigned long new_island_size = current_alloc_unit->unallocated_size;

      *stub_address = (unsigned long*)mmap(&new_island_start_addr, new_island_size,
					   PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_FIXED| MAP_ANONYMOUS, -1,0);

      // Retry with smaller allocation size in case of faliure
      if (*stub_address == MAP_FAILED) {
	int alloc_size = retry_allocation(&new_island_start_addr, new_island_size, stub_address);

	if (alloc_size == -1) {
	  int err = errno;
	  LOG_ERROR("Unable to allocate memory within the region : %d. "
		    "Annotations within this memory region will not be profiled..\n", mem_chunk);
	  return -1;
	} else {
	  // This needs to be reallocated again at some place else
	  current_alloc_unit->unallocated_size = new_island_size - alloc_size;
	  current_alloc_unit->start_addr = *stub_address;
	  current_alloc_unit->insertion_ptr = (unsigned long*)((byte*) *stub_address + STUB_SIZE);
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

  // This deals with initial memory island allocation for a given memory region
  auto it =  mem_allocations.find(mem_chunk);
  if (it != mem_allocations.end()) {
    // This is the first allocation of a memory island for this memory region.
    std::list<mem_island*>* mem_list = it->second;
    // Check first memory island
    mem_island* first_mem = mem_list->front();

    if (first_mem != NULL) {

      *stub_address = (unsigned long*)mmap(first_mem->start_addr, first_mem->size,
					   PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_FIXED| MAP_ANONYMOUS, -1,0);

      // Retry with smaller allocation size in case of faliure
      if (*stub_address == MAP_FAILED) {
	int alloc_size = retry_allocation(first_mem->start_addr, first_mem->size, stub_address);
	if (alloc_size == -1) {
	  int err = errno;
	  LOG_ERROR("Unable to allocate memory within chunk: %d. "
		    "Annotations within this chunk will not be profiled..\n", mem_chunk);
	  return -1;
	} else {
	  first_mem->unallocated_size = first_mem->size - alloc_size;
	  // This needs to be reallocated again at some place else
	  first_mem->start_addr = *stub_address;
	  first_mem->insertion_ptr = (unsigned long*)((byte*) *stub_address + STUB_SIZE);
	  first_mem->remaining_size = alloc_size - STUB_SIZE;
	}
      }

      // All's well. We got the amount we asked for.
      first_mem->start_addr = *stub_address;
      first_mem->insertion_ptr = (unsigned long*)((byte*) *stub_address + STUB_SIZE);
      first_mem->remaining_size = first_mem->size - STUB_SIZE;
      // first_mem->allocated = true;
      first_mem->unallocated_size = 0;

      current_alloc_unit = first_mem;
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

inline void modify_probe_site(unsigned char* probe_address, unsigned long* stub_address)
{
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
  // Plug in the relative jump
  // This does a relative jump:
  probe_address[0] = 0xE9;
  // Size of the JMP we insert is 5.  So +5 points to the following instruction.
  long relative = (long)(((unsigned char*)(void*) stub_address) - probe_address - 5);
  
  LOG_DEBUG("  Relative offset of dest from starting addr: %p %p, 32 bit: %d\n", probe_address, stub_address, (int)relative);

  *(uint32_t*)(probe_address+1) = (int)relative;
  probe_address[5] = 0x0;
}

int timeval_subtract(struct timeval *result, struct timeval *t2, struct timeval *t1)
{
	long int diff = (t2->tv_usec + 1000000 * t2->tv_sec) - (t1->tv_usec + 1000000 * t1->tv_sec);
	result->tv_sec = diff / 1000000;
	result->tv_usec = diff % 1000000;

	return (diff<0);
}

void setupStubs()
{
  // Retrieve annotation data
  probe_count=read_self_zca_probes();
  unsigned long* stub_address;
  
#ifdef PROFILE
  ticks start;
  ticks end;
  ticks elapsed_time;
  
  struct timeval tvBegin, tvEnd, tvDiff;
  gettimeofday(&tvBegin, NULL);
  start = getticks();
#endif

  int i = 0;
  for (auto iter = annotations.begin(); iter != annotations.end(); iter++, i++) {

    std::pair<zca_row_11_t*, unsigned long*> data = iter->second;
    unsigned char* probe_address = (unsigned char*) (data.first->anchor);

    int status = get_allocated_stub_memory_for_probe(probe_address, &stub_address);

    printf("stub address: %p\n", stub_address);
    annotations[iter->first].second = stub_address;
    
    if (status != -1) {
      modify_probe_site(probe_address, stub_address);
    }
  }
#ifdef PROFILE
  gettimeofday(&tvEnd, NULL);
  end = getticks();
  timeval_subtract(&stub_gen_tvDiff, &tvEnd, &tvBegin);
  stub_gen_elapsed_time = elapsed(end, start);
#endif

}


int activateProbe(const probe_t* label, probe_callable_t callback)
{
  return 0;
}

int deactivateProbe(const char* ann)
{
  std::pair<zca_row_11_t*, unsigned long*> data = annotations[ann];
  
  uint32_t probespace = data.first->probespace;
  byte* loc = (byte*) (data.second + 4 + probespace);
  printf("deactivate location: %p\n", data.second);
  byte* ip = (byte*) data.first->anchor;

  for (int i = 0; i < probespace; i++)
      ip[i] = loc[i];

  return 1;
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

	struct timeval tvBegin, tvEnd, tvDiff;
	gettimeofday(&tvBegin, NULL);
	start = getticks();

	//start = getticks();
#endif

	setupStubs();

#ifdef PROFILE
	gettimeofday(&tvEnd, NULL);
	end = getticks();
	timeval_subtract(&tvDiff, &tvEnd, &tvBegin);
	elapsed_time = elapsed(end, start);

	printf("\n--- Stub gen time for %d probes (cycles) : %llu \n", probe_count, stub_gen_elapsed_time);
	printf("--- Stub gen time for %d probes (seconds): %ld.%06ld\n", probe_count, stub_gen_tvDiff.tv_sec, stub_gen_tvDiff.tv_usec);

	printf("\n--- Total init time for %d probes (cycles) : %llu \n", probe_count, elapsed_time);
	printf("--- Total init time for %d probes (seconds): %ld.%06ld\n", probe_count, tvDiff.tv_sec, tvDiff.tv_usec);
#endif

	LOG_DEBUG("This text is printed before reaching \"main\".\n");
	return;
}



