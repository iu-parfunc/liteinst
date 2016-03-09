
#include "instrument.h"

#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>
#include <link.h>
#include <asmjit.h>

unsigned int PROBE_SIZE = 6;

void printfn(int arg) {
  printf("Function called with arg : %d\n", arg);
}

void genStubCode(Address stub_address, Address probe_address) {
    using namespace asmjit;
    using namespace asmjit::x86;
 
    int arg = 42;

    // The stub will do the following.
    // 1. Save the context (registers).
    // 2. Push argument to the register (RDI)
    // 3. Call the trampoline function.
    // 4. Restore the context (registers)
    // 5. Jump to the next instruction at probe site
    // -------------------------------------------------------------------
    JitRuntime runtime;
    X86Assembler a(&runtime);

#if LOGLEVEL >= DEBUG_LEVEL
    FileLogger logger(stderr);
    a.setLogger(&logger);
    // a2.setLogger(&logger);
#endif

    // Push all volatile registers:
    a.push(rsi); a.push(rdi);a.push(rax); a.push(rcx); a.push(rdx);
    a.push(r8); a.push(r9); a.push(r10); a.push(r11);
    a.push(r12); a.push(r13); a.push(r14); a.push(r15);

    // Push the argument
    a.xor_(rdi,rdi);
    a.mov(rdi, imm((size_t)arg));

    size_t func = (size_t) printfn;

    fprintf(stderr, "[Stub Utils] Trampoline function : %p\n", 
        (Address) func);

    int sz = a.getCodeSize();

    // Call the trampoline function
    a.call(imm(func));
    
    fprintf(stderr, "[Stub Utils] CALL instruction size : %ld\n",
        a.getCodeSize() - sz);

    a.pop(r15); a.pop(r14); a.pop(r13); a.pop(r12);
    a.pop(r11); a.pop(r10); a.pop(r9); a.pop(r8);
    a.pop(rdx); a.pop(rcx); a.pop(rax); a.pop(rdi); a.pop(rsi); 

    a.jmp(imm((size_t)(void*)(probe_address + PROBE_SIZE)));

    a.relocCode(stub_address);
}

void fillStub(Address probe_address, Address stub_address) {

  long page_size = sysconf(_SC_PAGESIZE); 
  int code = mprotect(
      (void*)(probe_address - (((long)probe_address)%page_size)), 
      page_size,
      PROT_READ | PROT_WRITE | PROT_EXEC);

  if (code) {
    throw -1;
  } 

  // If this probesite crosses a page boundary change the permissions of 
  // adjacent page too.
  if (page_size - ((uint64_t)probe_address)%page_size < PROBE_SIZE) {
    code = mprotect(
        (void*)(probe_address -((long)probe_address)%page_size+ page_size),
        page_size,
        PROT_READ | PROT_WRITE | PROT_EXEC);
    if (code) {
      throw -1;
    } 
  }

  genStubCode(stub_address, probe_address);
  // Plug in the relative jump
  // Size of the JMP we insert is 5. So +5 points to the following instruction.
  long relative = (long)(stub_address - (uint64_t)probe_address - 5);

  probe_address[0] = 0xE9;
  *(int*)(probe_address+1) = (int)relative;
  probe_address[5] = 0x90;
}

Address allocateStub() {
  // Allocate within first 2^32 bytes where the text segment should be
  // and within which the probe site is located. 
  Address hint = (Address) ((1LL<<32) / 2); 
  Address stub_address = (Address)mmap(hint, 1024,
          PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_FIXED|
          MAP_ANONYMOUS, -1,0);
  /*
  if ((int)stub_address == MAP_FAILED) {
    throw -1; 
  }
  */

  return stub_address;
}

Address createStub(Address probe_address) {
  Address stub_address = allocateStub();
  fillStub(probe_address, stub_address);

  return stub_address;
}


