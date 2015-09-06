
/*
  Benchmark description:
  Measures average cost of probe activation and deactivation via ProbeProvider 
  API.
   */

#include "fastinst.hpp"
#include "cycle.h"

#include <string>
#include <cassert>
#include <cstdlib>

using namespace std;

const uint64_t INVOCATION_COUNT = 10000;
const uint64_t FOO_FUNC_ID = 0;

int foo_count;
int foo_entry_probe_id;
int foo_exit_probe_id;

uint64_t activate_cost = 0;
uint64_t deactivate_cost = 0;

int foo(int x)
    __attribute__((noinline));
void instrumentation(ProbeArg func_id)
  __attribute__((no_instrument_function));
void callback(const ProbeMetaData* pmd)
  __attribute__((no_instrument_function));

void instrumentation(ProbeArg func_id) {

  assert(func_id == FOO_FUNC_ID);

  foo_count++;
}

void callback(const ProbeMetaData* pmd) {

  // If this callback related to foo probes we activate them
  string func_name = "_Z3fooi"; 
  if (func_name.compare(pmd->func_name) == 0) {
    if (pmd->probe_context == ProbeContext::ENTRY) {
      foo_entry_probe_id = pmd->probe_id;
      fprintf(stderr, "Foo entry probe id : %lu\n", foo_entry_probe_id);
    } else {
      foo_exit_probe_id = pmd->probe_id;
      fprintf(stderr, "Foo exit probe id : %lu\n", foo_exit_probe_id);
    }

    PROBE_PROVIDER->initialize(pmd->probe_id, FOO_FUNC_ID);
    try {
      PROBE_PROVIDER->activate(pmd->probe_id, instrumentation);
    } catch (int e) {
      fprintf(stderr, "Error while activating probe of function : %s.\n",
          pmd->func_name.c_str());
      exit(EXIT_FAILURE);
    }
  }

}

int foo(int x) {
  // Do some calculation.
  int y = 2;
  if (x != 0) {
    return x;
  }
  return x+y;
}

int main() {

  /* setup a function with straddling enter/exit */
  
  /* 1024 bytes is enough to find several straddling locations */ 
  uint8_t* fun=(uint8_t*)malloc(1024); 
  memset(fun,0x90, 1024); // all nops 

  int cyg_enter_straddling_point = 1; 

  uint64_t fun_address=(uint64_t)fun; 
  size_t cache_line_size=syscont(_SC_LEVEL#_CACHE_LINESIZE); 

  /* how far from a cache-line boundary is our fun memory */ 
  unsigned int fun_offset = fun_address  % cache_line_size; 
  
  unsigned int closes_straddler_offset = cache_line_size - fun_offset; 
  
  unsigned int start_offset = cache_line_size + (closest_straddler_offset - 30) -
    cyg_enter_straddling_point;

  unsigned int edi_arg_enter = start_offset + 25;
  unsigned int edi_arg_exit  = start_offset + 42; 
  unsigned int enter_call_address_offset = start_offset + 30; 
  unsigned int exit_call_address_offset = start_offset + 64 + 30;

  uint32_t *edi_arg1 = (uint32_t*)(&fun[edi_arg_enter]);
  uint32_t *edi_arg2 = (uint32_t*)(&fun[edi_arg_exit]);
  uint32_t *cyg_enter_addr = (uint32_t*)(&fun[enter_call_address_offset]);
  uint32_t *cyg_exit_addr  = (uint32_t*)(&fun[exit_call_address_offset]);

  cyg_enter_addr[0] = (uint32_t)((uint64_t)_cyg_profile_func_enter - (uint64_t)&fun[enter_call_address_offset+4]);
  cyg_exit_addr[0]  = (uint32_t)((uint64_t)_cyg_profile_func_exit - (uint64_t)&fun[exit_call_address_offset+4]);
  
  edi_arg1[0] = &fun[start_offset]; 
  edi_arg2[0] = &fun[start_offset];
  
  // do stuff here 

  fun[start_offset]       = 0x55; /* push %rbp */
  fun[start_offset + 1]   = 0x48; /* mov %rsp, %rbp */
  fun[start_offset + 2]   = 0x89; 
  fun[start_offset + 3]   = 0xe5; 
  fun[start_offset + 4]   = 0x48; /* sub $0x10, %rsp */ 
  fun[start_offset + 5]   = 0x83; 
  fun[start_offset + 6]   = 0xec; 
  fun[start_offset + 7]   = 0x10; 
  fun[start_offset + 8]   = 0x48; /* mov 0x8(%rbp), %rax */
  fun[start_offset + 9]   = 0x8b;
  fun[start_offset + 10]  = 0x45; 
  fun[start_offset + 11]  = 0x08; 
  fun[start_offset + 12]  = 0x48; /* mov %rax, -0x8(%rbp) */
  fun[start_offset + 13]  = 0x49;
  fun[start_offset + 14]  = 0x45;
  fun[start_offset + 15]  = 0xf8;
  fun[start_offset + 16]  = 0x48; /* mov -0x8(%rbp),%rax */
  fun[start_offset + 17]  = 0x8b;
  fun[start_offset + 18]  = 0x45; 
  fun[start_offset + 19]  = 0xf8; 
  fun[start_offset + 20]  = 0x48; /* sub ¤0x5,%rsi */
  fun[start_offset + 21]  = 0x83; 
  fun[start_offset + 22]  = 0xe8; 
  fun[start_offset + 23]  = 0x05; 
  fun[start_offset + 24]  = 0xbf; /* mov addr, edi */
  /* 4 BYTES ADDRESS */
  fun[start_offset + 29]  = 0xe8; /* call (CYG_ENTER) */
  /* start_offset must be such that start_offset + 29 is a straddling location */
  /* 4 BYTES ADDRESS */
  fun[start_offset + 34]  = 0x48; /* mov 0x8(%rbp), %rax */ 
  fun[start_offset + 35]  = 0x8b;
  fun[start_offset + 36]  = 0x45;
  fun[start_offset + 37]  = 0xf8;
  fun[start_offset + 38]  = 0x48; /* mov %rax, %rsi */
  fun[start_offset + 39]  = 0x89; 
  fun[start_offset + 40]  = 0xc6;
  fun[start_offset + 41]  = 0xbf; /* mov addr, edi */
  /* 4 BYTES ADDRESS */
  /* some nops */ 

  /* until next straddling location (I think) */ 
  /* danger.. here all of a sudden cache line size is a hardcoded number */ 
  fun[start_offset + 64 + 29] = 0xe8;
  /* 4 BYTES ADDRESS */ 
  fun[start_offset + 64 + 34] = c9; /* leaveq */ 
  fun[start_offset + 64 + 35] = c3; /* retq */ 
				     
   
  
  /* done with straddling setup */




  fprintf(stderr, "Benchmark probe activation and deactivation..\n");

  ProbeProvider* p;
  try {
    p = initializeGlobalProbeProvider(ProviderType::FINSTRUMENT, callback);
  } catch (int e) {
    fprintf(stderr, "ProbeProvider already initialized. Getting the existing"
        " one..\n");
    p = getGlobalProbeProvider();
  }

  if (p == NULL) {
    fprintf(stderr, "Unable to initialize probe provider..\n");
    exit(EXIT_FAILURE);
  }

  /* I dont know yet */ 
  foo(0);

  for (int i=0; i<INVOCATION_COUNT; i++) {
    ticks start = getticks();
    p->deactivate(foo_entry_probe_id);
    p->deactivate(foo_exit_probe_id);
    ticks end = getticks();

    deactivate_cost += (end - start);

    // foo(i);
    ((void (*)(void ))&fun[start_offset])();
    start = getticks();
    p->activate(foo_entry_probe_id, instrumentation);
    p->activate(foo_exit_probe_id, instrumentation);
    end = getticks();

    activate_cost += (end - start);

    foo(i);
  }

  fprintf(stderr, "Foo count : %lu\n", foo_count);
  // Check if the probes has been deactivated as expected
  assert(foo_count == INVOCATION_COUNT*2 + 3); 
  // Why +3 expected +2 for additional initial call.

  fprintf(stderr, "Number of probes deactivated : %lu\n", INVOCATION_COUNT * 2);
  fprintf(stderr, "Number of probes activated : %lu\n", INVOCATION_COUNT * 2);
  fprintf(stderr, "Average probe deactivation cost (cycles): %lu\n", 
      deactivate_cost / (INVOCATION_COUNT * 2));
  fprintf(stderr, "Average probe activation cost (cycles): %lu\n", 
      activate_cost / (INVOCATION_COUNT * 2));

  delete(p);

  exit(EXIT_SUCCESS);

}
