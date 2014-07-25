
/** 
 * @brief test file descr
 * @details detailed descr
 */

#include <stdio.h>

#include <string>
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
#include <link.h>
#include "utils.hpp"

// ann_data* annotations;

#define PROBESIZE 6
#define DWORD_SIZE 8

#ifdef PROFILE
static ticks stub_gen_elapsed_time;
static struct timeval stub_gen_tvDiff;
#endif

void calibrateTicks();

int ZCA_OVERHEAD = 600; // Assuming 1000 cycles overhead per function call. Need to measure this at runtime
int ZCA_INIT_OVERHEAD = 0;
const int NANO_SECONDS_IN_SEC = 1000000000;

static int probe_count = 0;
static mem_island* current_alloc_unit;

inline int gen_stub_code(unsigned char* addr, unsigned char* probe_loc, void (*target_fn)(), ann_data** ann_info)
{

  using namespace AsmJit;

  /*  int foo = 10, bar = 15;
      __asm__ __volatile__("addl  %%ebx,%%eax"
      :"=a"(foo)
      :"a"(foo), "b"(bar)
      );
      printf("foo+bar=%d\n", foo);*/


  /*  char* annotation;
      __asm__ __volatile__ ("movl:0x10(%%rbp),%%ecx"
      :"=c" (annotation)
      );*/

  /*  volatile intptr_t new_stack_ptr = 0;
      volatile intptr_t old_stack_ptr = 0;
      asm __volatile__("movl %%esp, %0\n\t"
      "movl %1, %%esp"
      : "=r"(old_stack_ptr)  output
      : "r"(new_stack_ptr)  input
      );*/

  // printf("Annotation is : %s\n", annotation);
  
  char* tok;
  char *temp = strdup((*ann_info)->expr);
  char* func_name = strtok_r(temp, ":", &tok);
 
  long func_id = 0;
  if(functions->find(std::string(func_name)) != functions->end()) {
    func_id = functions->find(std::string(func_name))->second;
  }

  // printf("Function id for function %s at insertion is %lu \n", func_name, func_id);

  // This aims to make the same one-way jump as manual_jmp_there, except from JITed code.
  // --------------------------------------------------------------------------------
  Assembler a, a2;
  Mem m;

  Compiler c;

#if LOGLEVEL >= DEBUG_LEVEL

  FileLogger logger(stderr);
  a.setLogger(&logger);
  a2.setLogger(&logger);

#endif

  // Push all volatile registers:

  a.push(rsi); a.push(rdi);a.push(rax); a.push(rcx); a.push(rdx);
  a.push(r8); a.push(r9); a.push(r10); a.push(r11);
  a.push(r12); a.push(r13); a.push(r14); a.push(r15);

  /*
  a.push(rsi); a.push(rdi); a.push(rcx); a.push(rdx);
  a.push(r8); a.push(r9); 
  */

  // printf("ann_info->expr : %p\n", (*ann_info)->expr);
  // printf("ann_info->expr : %s\n", ((*ann_info)->expr));

  a.xor_(rdx,rdx);
  a.mov(rdx, imm((sysint_t)func_id));

  // a.push(ecx);
  // a.mov(eax, imm((sysint_t)func_id));

  // a.mov(rax,imm((sysint_t)func_name));
  // a.push(rax);

  a.call(imm((sysint_t)target_fn));
  
  // Restore all volatile registers:
  // a.pop(rax);
 
  a.pop(r15); a.pop(r14); a.pop(r13); a.pop(r12);
  a.pop(r11); a.pop(r10); a.pop(r9); a.pop(r8);
  a.pop(rdx); a.pop(rcx); a.pop(rax); a.pop(rdi); a.pop(rsi); // This fixes the wierd seg fault which happens when -O3 is enabled

  /*
  a.pop(r9); a.pop(r8);
  a.pop(rdx); a.pop(rcx); a.pop(rdi); a.pop(rsi);
  */

  a.jmp(imm((sysint_t)(void*)(probe_loc + PROBESIZE)));

  int codesz = a.getCodeSize();
  // This works just as well, don't need the function_cast magic:
  sysuint_t code = a.relocCode(addr);

  // printf("[Gen Stub]Probe address is : %p\n", (unsigned char*)probe_loc);
  //printf("[Gen Stub]Original probe content is : %016llx\n", *((uint64_t*)probe_loc));
  // printf("[Gen Stub]Stub address is : %p\n", addr);

  (*ann_info)->probe_offset = codesz;
  // printf("[Gen Stub]Probe offset is : %d\n", (*ann_info)->probe_offset);

  // Copy over the displaced probe bytes:
  
  /*
  char b[8];
  for(int i=0; i<PROBESIZE; i++) {
    addr[codesz + i] = probe_loc[i];
  }
  */
 

  /*	for (int i=0; i<PROBESIZE; i++) {
  // b[i] = addr[codesz + i];
  b[i] = probe_loc[i];
  printf("%p ", addr[codesz+i]);
  }
  printf("\n");*/


  /*
  b[6] = 0;
  b[7] = 0;
  */

  // printf("[Gen Stub]Buffer value is : %016llx\n", b);

  // Next generate the jump back home:
  /*
  a2.jmp(imm((sysint_t)(void*)(probe_loc + PROBESIZE)));
  int sz2 = a2.getCodeSize();
  a2.relocCode(addr + codesz + PROBESIZE);
  */
  // a2.relocCode(addr + codesz);

  //printf("[Gen Stub]Stub content is :%016llx\n", *((uint64_t*)((byte*)addr+(*ann_info)->probe_offset)));
  //printf("[Gen Stub]Stub content direct :%016llx\n", *((uint64_t*)((byte*)addr + codesz + 1)));
  //printf("[Gen Stub]One word after the stub is : %016llx\n", *((uint64_t*)(addr + codesz + PROBESIZE)));
  //#if LOGLEVEL >= DEBUG_LEVEL

  /*
  char buf[1024];
  for(int i=0; i<codesz + PROBESIZE + sz2; i++) {

    sprintf(&buf[5 * i], " %p", addr[i]);

  }
  */


  //printf("%s\n", buf);
  //#endif

  return (codesz);
  // return (codesz+ PROBESIZE + sz2);
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

inline void modify_probe_site(unsigned char* probe_address, unsigned long* stub_address, ann_data** ann_info, void (*fun)()) {
  LOG_DEBUG("Probe address is : %p\n", (unsigned char*)probe_address);
  LOG_DEBUG("Stub address is : %p\n", stub_address);


  long page_size = sysconf(_SC_PAGESIZE); 
  int code = mprotect((void*)(probe_address - (((unsigned long)probe_address)%4096)), page_size,
      PROT_READ | PROT_WRITE | PROT_EXEC);

  if (code) {
    /* current code page is now writable and code from it is allowed for execution */
    LOG_ERROR("mprotect was not successfull! code %d\n", code);
    LOG_ERROR("errno value is : %d\n", errno);
    // return -1;
  }

  // If this probesite crosses a page boundary change the permissions of adjacent page too.
  if (page_size - ((unsigned long)probe_address)%4096 < PROBESIZE) {
    code = mprotect((void*)(probe_address -((unsigned long)probe_address)%4096 + 4096) , page_size,
        PROT_READ | PROT_WRITE | PROT_EXEC);
    if (code) {
      /* current code page is now writable and code from it is allowed for execution */
      LOG_ERROR("mprotect was not successfull! code %d\n", code);
      LOG_ERROR("errno value is : %d\n", errno);
      // return -1;
    } 
  }

  int stub_size = gen_stub_code((unsigned char*)(stub_address), probe_address, fun, ann_info/*(&annotations[i])->fun*/);
  // Plug in the relative jump
  // This does a relative jump:
  // Size of the JMP we insert is 5.  So +5 points to the following instruction.
  long relative = (long)(((unsigned char*)(void*) stub_address) - probe_address - 5);
  //printf ("[Modify Probe site] Jump distance is : %lu\n", relative);

  LOG_DEBUG("  Relative offset of dest from starting addr: %p %p, 32 bit: %d\n", probe_address, stub_address, (int)relative);

  probe_address[0] = 0xE9;
  *(uint32_t*)(probe_address+1) = (int)relative;
  probe_address[5] = 0x90;
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
    std::list<ann_data*>* ann_list = iter->second;
    // printf("String %d: %s\n", i, iter->first);

    for (std::list<ann_data*>::iterator it =  ann_list->begin(); it != ann_list->end(); ++it) {
      ann_data* data = *it;
      unsigned char* probe_address = (unsigned char*) (data->anchor);
      // ann_data* ann_info = data.second;

      int status = get_allocated_stub_memory_for_probe(probe_address, &stub_address);

      if (status != -1) {
        // Fill annotation information for later use in probe activation/ deactivation
        data->stubLocation = stub_address;

        // printf("Probe address %d : %p  Stub address : %p\n", i, probe_address, stub_address);
        modify_probe_site(probe_address, stub_address, &data, print_fn2);
        data->active = true;
      } else {
        printf("Memory allocation failed..\n");
      }
    }
  }
#ifdef PROFILE
  gettimeofday(&tvEnd, NULL);
  end = getticks();
  timeval_subtract(&stub_gen_tvDiff, &tvEnd, &tvBegin);
  stub_gen_elapsed_time = elapsed(end, start);
#endif

}

// Activate the probe by copying the jump to the stub
// Little bit of bitmasking trickery is needed if the probe size is less than 8 bytes since
// CAS can only deal with in integer sizes
int activateProbe(std::string label, void (*fun)())
{
  if (annotations.find(label) != annotations.end()) {

    std::list<ann_data*>* ann_list = annotations.find(label)->second;
    //zca_row_11_t* probe_info = data.first;
    // ann_data* ann_info = data.second;
    for (std::list<ann_data*>::iterator it =  ann_list->begin(); it != ann_list->end(); ++it) {
      ann_data* data = *it;

      if (data != NULL && data != NULL && data->active == false) {
        uint64_t* stub_address = data->stubLocation;
        uint64_t* probe_address = (uint64_t*) data->anchor;

        if (data->fun != fun) {
          // printf("Current function : %p New function : %p\n", ann_info->fun, fun);
          unsigned char* probe_address = (unsigned char*) (data->anchor);
          modify_probe_site(probe_address, data->stubLocation, &data, fun);
          data->active = true;
          return 0;
        }

        //printf("[Activate Probe]The probe content is : %016llx\n", *probe_address);
        //printf("[Activate Probe]The stub content is : %016llx\n", ((byte*)stub_address + ann_info->probe_offset));

        if (stub_address != NULL && probe_address != NULL) {
          uint64_t old_val = *probe_address;

          if (PROBESIZE < sizeof(uint64_t)) {
            uint64_t mask = 0x0FFFFFFFFFFFFFFF;

            int shift_size = (sizeof(uint64_t) - PROBESIZE) * 8 - 4;
            mask = (mask >> shift_size);

            uint64_t msb = (old_val & ~mask);

            //printf("[Activate Probe]MSB is : %016llx\n", msb);

            uint64_t new_val = 0;
            byte* new_val_ptr;
            new_val_ptr = (byte*) &new_val;
            new_val_ptr[0] = 0xE9;

            long relative = (long)(((unsigned char*)(void*)stub_address) - (unsigned char*)probe_address - 5);
            //printf("[Activate Probe]Jump distance is : %lu\n", relative);
            *(uint32_t*)(new_val_ptr+1) = (int)relative;
            new_val_ptr[5] = 0x90;

            new_val = (new_val | msb);

            //printf("[Activate Probe]Modfied probe content is : %016llx\n", new_val);

            // Now atomically swap the jump to the stub at the probe site
            int status = __sync_bool_compare_and_swap(probe_address, old_val, new_val);
            // printf("[Activate Probe]Changed probe content is : %016llx\n", *probe_address);

            data->active = true;
            return status;
          }

        }

      }
    }
  } else {
    printf("Couldn't find the annotation : %s \n", label);
  }
  return -1;
}

// Deactivate the probe by copying original probe sequence back
// Little bit of bitmasking trickery is needed if the probe size is less than 8 bytes since
// CAS can only deal with in integer sizes
int deactivateProbe(std::string label) {

  if (annotations.find(label) != annotations.end()) {

    std::list<ann_data*>* ann_list = annotations.find(label)->second;
    // zca_row_11_t* probe_info = data.first;
    // ann_data* ann_info = data.second;
    for (std::list<ann_data*>::iterator it =  ann_list->begin(); it != ann_list->end(); ++it) {
      ann_data* data = *it;
      if (data != NULL && data != NULL && data->active == true) {
        uint64_t* probe = (uint64_t*) data->anchor;
        uint64_t old_val = *probe;

        // fprintf(stderr, "Label is : %s\n", label);

        /*         if (count == 0) {
                   printf("Label is : %s\n", label);
                   printf("Probe location is : %p\n", data->anchor);
                   printf("Old val is : %016llx\n", old_val);

                   printf("Stub location is : %p\n", data->stubLocation);
                   } */
        // Commented
        // uint64_t* probespace = (uint64_t*) ((byte*)data->stubLocation + data->probe_offset);
        
        //printf("Probespace is : %p\n", probespace);
        //printf("Probe offset now is : %d\n", ann_info->probe_offset);
        /*          if (count == 0) {
                    printf("Probespace content is : %016llx\n", *probe);
                    } */

        // Commented
        // uint64_t new_val = *probespace;
        uint64_t new_val = 0x000000441f0f66; // 6 byte NOP 

        if (PROBESIZE < sizeof(uint64_t)) {
          uint64_t mask = 0x0FFFFFFFFFFFFFFF;

          // printf("Mask before shifting is %016llx\n", mask);
          int shift_size = (sizeof(uint64_t) - PROBESIZE) * 8 - 4;
          mask = (mask >> shift_size);
          //printf("Mask is %016llx\n", mask);

          new_val = (new_val & mask); // Shave off (2) least significant bytes
          //printf("New val after shaving off 2 msb : %016llx\n", new_val);
          uint64_t msb = (old_val & ~mask);
          //printf("LSB is : %016llx\n", msb);

          new_val = (new_val | msb);

          int page_size = 4096;
          //printf("Probe is %04x\n", probe);
          /*				int code = mprotect((void*)(probe - (((unsigned long)probe)%4096)), page_size,
                    PROT_READ | PROT_WRITE | PROT_EXEC);

                    if (code) {
                    current code page is now writable and code from it is allowed for execution
                    LOG_ERROR("mprotect was not successfull! code %d\n", code);
                    LOG_ERROR("errno value is : %d\n", errno);
          // return -1;
          }*/

          // Now atomically swap the displaced probe site sequence to its original location
          int status = __sync_bool_compare_and_swap(probe, old_val, new_val);

          data->active = false;
          //printf("Swapped the stuff..\n");

          return status;
        }

      }
    }
  }

  return -1;
}

static int
callback(struct dl_phdr_info *info, size_t size, void *data)
{
    int j;

   printf("name=%s (%d segments)\n", info->dlpi_name,
               info->dlpi_phnum);

   for (j = 0; j < info->dlpi_phnum; j++)
         printf("\t\t header %2d: address=%10p\n", j,
                          (void *) (info->dlpi_addr + info->dlpi_phdr[j].p_vaddr));
    return 0;
}

void test_function() {
    dl_iterate_phdr(callback, NULL);
}

/* This function is called automatically when the library is loaded */
// __attribute__((constructor)) 
// FIXME: trying to get this attribute((constructor)) business to work even in a statically linked library.
// I think it actually only works for shared libraries.
void initZCAService() {
  /* Read all annotations here and setup stubs. How best to do it (sync or async) needs to be emperically determined */
#ifdef PROFILE
  long start = gettime_millis();
#endif

  setupStubs();
  calibrateTicks();

#ifdef PROFILE
  long end = gettime_millis();

  // fprintf(stderr, "\n--- Stub gen time for %d probes (cycles) : %llu \n", probe_count, stub_gen_elapsed_time);
  // fprintf(stderr, "--- Stub gen time for %d probes (seconds): %ld.%06ld\n", probe_count, stub_gen_tvDiff.tv_sec, stub_gen_tvDiff.tv_usec);

  // fprintf(stderr, "Total init time for %d probes (cycles) : %llu \n", probe_count, elapsed_time);
  // fprintf(stderr, "--- Total init time for %d probes (seconds): %ld.%06ld\n", probe_count, tvDiff.tv_sec, tvDiff.tv_usec);
  
  ZCA_INIT_OVERHEAD = end - start;  
  fprintf(stderr, "INIT_TIME %.03f\n", (double) ZCA_INIT_OVERHEAD / 1000);
#endif

  LOG_DEBUG("This text is printed before reaching \"main\".\n");

  // Measure trampoline overhead
  //start = getticks();
  // empty_function();
  // end = getticks();

  // ticks overhead = end - start;
  // fprintf(stderr, "OVERHEAD %llu\n", overhead);

  // test_function();

  return;
}

/*
void empty_function() {

  __notify_intrinsic((void*)"empty_function:start", (void *)&global_x);

  __notify_intrinsic((void*)"empty_function:end", (void *)&global_x);

}
*/



long getInitOverhead() {
  return ZCA_INIT_OVERHEAD;
}

long gettime_millis() {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (ts.tv_sec * 1000 + ts.tv_nsec / 1000000);
}

long getThreadCPUTime() {
  struct timespec ts;
  clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts);
  return (ts.tv_sec * 1000000000LL + ts.tv_nsec);
}

long getProcessCPUTime() {
  struct timespec ts;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);
  return (ts.tv_sec * 1000000000LL + ts.tv_nsec);
}

double g_TicksPerMilliSec;
double getTicksPerMilliSec() {
  return g_TicksPerMilliSec; 
}

double g_TicksPerNanoSec;
double getTicksPerNanoSec() {
  return g_TicksPerNanoSec;
}

double getZCAOverhead() {
  return ZCA_OVERHEAD/g_TicksPerNanoSec;
}

double getZCAOverheadTicks() {  
  return ZCA_OVERHEAD;
}

struct timespec *timeSpecDiff(struct timespec *ts1, struct timespec *ts2)
{
  static struct timespec ts;
  ts.tv_sec = ts1->tv_sec - ts2->tv_sec;
  ts.tv_nsec = ts1->tv_nsec - ts2->tv_nsec;
  if (ts.tv_nsec < 0) {
      ts.tv_sec--;
      ts.tv_nsec += NANO_SECONDS_IN_SEC;
    }
  return &ts;
}

static void calibrateTicks()
{
  struct timespec begints, endts, diff;
  uint64_t begin = 0, end = 0;
  clock_gettime(CLOCK_MONOTONIC, &begints);
  begin = getticks();
  uint64_t i,result = 0;
  for (i = 0; i < 1000000; i++) { /* must be CPU intensive */
    result += i;
  }
  end = getticks();
  clock_gettime(CLOCK_MONOTONIC, &endts);
  struct timespec *tmpts = timeSpecDiff(&endts, &begints);
  uint64_t millisecElapsed = tmpts->tv_sec * 1000L + tmpts->tv_nsec / 1000000L;
  g_TicksPerMilliSec = (double)(end - begin)/(double)millisecElapsed;
  uint64_t nanosecElapsed = tmpts->tv_sec * 1000000000LL + tmpts->tv_nsec;
  g_TicksPerNanoSec = (double)(end - begin)/(double)nanosecElapsed;
}



