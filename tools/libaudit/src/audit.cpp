
#include <list>
#include <map>
#include <sstream>
#include <fstream>
#include <iostream>
#include <vector>
#include <cstdio>
#include <string> 
#include <algorithm>

#include <string.h>  // memcpy
#include <unistd.h>  // sysconf
#include <assert.h>
#include <sys/mman.h> // mprotect

#include "disassembly.hpp"
#include "analysis.hpp"
#include "statistics.hpp"
#include "utils.hpp"
#include "elf.hpp"

using namespace disassembly;
using namespace analysis;
using namespace statistics;
using namespace defs;
using namespace elf;
using namespace utils;

using std::map;
using std::list;
using std::vector;
using std::string;
using std::find;
using std::make_pair;

typedef map<Address, FunctionSymbolInfo> FunctionSymbols;

/*
   typedef std::map<std:string, uint64_t> Sequences;

   Sequences bb_start_sequences;
   Sequences bb_end_sequences;
   Sequences fn_start_sequences;
   Sequences fn_end_sequences; */

FunctionSymbols* func_symbols;

vector<string> fn_black_list = {"_init"}; // Functions not to be touched


list<Function> auditFunctions(vector<string> fns) {
  auto it = func_symbols->begin();
  Address next_fn_start = NULL;
  if (it != func_symbols->end()) {
    next_fn_start = it->first;
  }

  list<Function> functions;
  while (it != func_symbols->end()) {
    Address start = next_fn_start;
    FunctionSymbolInfo fsi = it->second;
    string func_name = fsi.name;
    ++it;

    if (it != func_symbols->end()) {
      next_fn_start = it->first;
    } else {
      fprintf(stderr, "Reached the last function at %p\n",
          start);
      break;
    }

    if (start != (Address) NULL && start != fsi.end && 
        find(fns.begin(), fns.end() , func_name) != fns.end()) { 
      Decoded d = disassembleRange(start, fsi.end);

      BlockStructure bs = getBlockStructure(start, fsi.end, next_fn_start, d);

      Function fn = generateMetaDataForFunction(bs, d);
      fn.name = func_name;

      functions.push_back(fn);
    }
  }

  return functions;
}

list<Function> auditFunctions() {
  auto it = func_symbols->begin();
  Address next_fn_start = NULL;
  if (it != func_symbols->end()) {
    next_fn_start = it->first;
  }

  list<Function> fns;
  while (it != func_symbols->end()) {
    Address start = next_fn_start;
    FunctionSymbolInfo fsi = it->second;
    string func_name = fsi.name;
    ++it;

    if (it != func_symbols->end()) {
      next_fn_start = it->first;
    } else {
      fprintf(stderr, "Reached the last function at %p\n",
          start);
      break;
    }

    /*
       fprintf(stderr, "Decoding function %s start : %p end : %p\n", 
       func_name.c_str(), start, fsi.end); */
    /*
       bool is_black_listed = (find(fn_black_list.begin(), fn_black_list.end() , 
       func_name) != fn_black_list.end());*/
    if (start != (Address) NULL && start != fsi.end) {
      Decoded d = disassembleRange(start, fsi.end);

      BlockStructure bs = getBlockStructure(start, fsi.end, next_fn_start, d);

      Function fn = generateMetaDataForFunction(bs, d);
      fn.name = func_name;

      fns.push_back(fn);
    }
  }

  return fns;
}

void readFunctionInfo() {

  func_symbols = new FunctionSymbols;

  vector<FunctionSymbolInfo> fsis = readFunctionSymbols();

  for (FunctionSymbolInfo fsi : fsis) {
    func_symbols->insert(make_pair(fsi.start, fsi));
  }
}

// The stub will do the following.
// 1. Save the context (registers).
// 2. Push argument to the register (RDI)
// 3. Call the trampoline function.
// 4. Restore the context (registers)
// 5. Jump to back to the original context given by return address 
// -------------------------------------------------------------------
uint8_t g_stub[] = 
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
  0xff, 0x15, 0x23, 0x00, 0x00, 0x00,       /* callq *0x23(%rip) */
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
  0xff, 0x25, 0x00, 0x00, 0x00, 0x00, /* jumpq *%rip */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* return address */
  // 0x40, 0xe9, 0x00, 0x00, 0x00, 0x00, /* rex jmpq xx*/
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 /* trampoline fn address */};

uint8_t rip_indirect_jump[] =
{ 0xff, 0x25, 0x00, 0x00, 0x00, 0x00,       /* jumpq *%rip */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 /* jump target */};

uint8_t g_saved_main_prolog[sizeof(rip_indirect_jump)/
  sizeof(rip_indirect_jump[0])];

  void* g_main_ptr; // Address of the main function

  extern "C" void audit();
  // extern "C" int main();

  void audit() {

    fprintf(stderr, "[audit] Audting..\n");
    
    // Restore the prolog of main before we do any kind of analysis on binary
    memcpy(g_main_ptr, (const void*) g_saved_main_prolog, 
        sizeof(rip_indirect_jump)/sizeof(rip_indirect_jump[0])); 

    readFunctionInfo();

    list<Function> fns;
    vector<string> audit_fns;
    char *audit_fns_str = getenv("AUDIT_FUNCTIONS");
    if (audit_fns_str != NULL) {
      tokenize(string(audit_fns_str), audit_fns, ",");
      fns = auditFunctions(audit_fns);
    } else {
      fns = auditFunctions();
    }

    Statistics stats = generateStatistics(&fns);

    FILE* fp1 = fopen("block_info.txt", "a");
    FILE* fp2 = fopen("probes.txt", "a");

    bool verbose = true;
    printBlockInfo(fns, fp1);
    printStatistics(stats, fp2, verbose);

    fclose(fp1);
    fclose(fp2);

    // fprintf(stderr, "[main] EXITING PROGRM..\n");
    // For this we don't want to execute the application
    // exit(0);
  }

extern "C" {
  void infectMain() {
    // g_main_ptr = (void*) main;

    readFunctionInfo();
    for (auto it : *func_symbols) {
      if (!it.second.name.compare("main")) {
        g_main_ptr = (void*) it.second.start;
      }
    }

    // Allocate space for the stub
    long page_size = sysconf(_SC_PAGESIZE); 
    void* stub = mmap(NULL, page_size,
        PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS, -1,0);

    // Make it executable. 
    int code = mprotect(
        (void*)((uint8_t*)stub - ((uint64_t)stub)%page_size), 
        page_size,
        PROT_READ | PROT_WRITE | PROT_EXEC);
    // The stub occupies the next page as well. Change its permissions too.
    if (((uint64_t)((uint8_t*) stub + page_size) / page_size) != 
        ((uint64_t)stub / page_size)) {
      code &= mprotect(
        (void*)(((uint8_t*)stub + page_size) - 
          ((uint64_t)((uint8_t*)stub + page_size))%page_size), 
        page_size,
        PROT_READ | PROT_WRITE | PROT_EXEC);
    }

    if (code) {
      perror("[mprotect]");
      fprintf(stderr, "[premain] Failed generating stub..\n");
      assert(false);
    }

    // Populate stub content
    memcpy(stub, (const void*) g_stub, 
        sizeof(g_stub)/sizeof(g_stub[0])); 

    fprintf(stderr, "[premain] Inject trampoline fn call address to the stub..\n");
    // Inject trampoline fn call address to the stub
    int8_t trampoline_fn_offset = (sizeof(g_stub) / sizeof(g_stub[0])) - 8; 
    fprintf(stderr, "[premain] Trampoline function offset : %d\n", trampoline_fn_offset);
    *(uint64_t*)&((uint8_t*)stub)[trampoline_fn_offset] = (uint64_t) audit;

    fprintf(stderr, "[premain] Inject return jump address to the stub..\n");
    // Inject return jump address to the stub
    int8_t return_addr_offset = (sizeof(g_stub) / sizeof(g_stub[0])) - 16; 
    *(uint64_t*)&((uint8_t*)stub)[return_addr_offset] = (uint64_t) g_main_ptr;

    fprintf(stderr, "[premain] Setup the call to stub..\n");
    // Setup the call to stub 
    *(uint64_t*)&((uint8_t*)rip_indirect_jump)[6] = (uint64_t) stub;

    int8_t patch_site_length = 
      sizeof(rip_indirect_jump) / sizeof(rip_indirect_jump[0]);
    // Change permissions for main since we are going to patch it shortly
    code = mprotect(
        (void*)((uint8_t*)g_main_ptr - ((uint64_t)g_main_ptr)%page_size), 
        page_size,
        PROT_READ | PROT_WRITE | PROT_EXEC);
    // The patch site occupies the next page as well. Change its permissions 
    // too.
    if (((uint64_t)((uint8_t*) g_main_ptr + patch_site_length) / page_size) != 
      ((uint64_t) g_main_ptr / page_size)) {
      code &= mprotect(
        (void*)(((uint8_t*)g_main_ptr + patch_site_length) - 
          ((uint64_t)((uint8_t*)g_main_ptr + patch_site_length))%page_size), 
        page_size,
        PROT_READ | PROT_WRITE | PROT_EXEC);
    }

    if (code) {
      perror("[mprotect]");
      fprintf(stderr, "[rprobes] Failed hooking main function..\n");
      assert(false);
    }

    fprintf(stderr, "[premain] Done setting up..\n");
    // Save prolog of main before overwriting it
    memcpy(g_saved_main_prolog, (const void*) g_main_ptr, 
        sizeof(rip_indirect_jump)/sizeof(rip_indirect_jump[0])); 

    // Overwrite the prolog with a jump to instrumentation initialization stub
    memcpy(g_main_ptr, (const void*) rip_indirect_jump, 
        sizeof(rip_indirect_jump)/sizeof(rip_indirect_jump[0])); 

    fprintf(stderr, "[premain] Done patching..\n");
  }

   __attribute__((constructor))
    void premain() {
      fprintf(stderr, "[premain] In premain..\n");
      infectMain();
    }
}
