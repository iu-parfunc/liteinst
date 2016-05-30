#include <stdio.h>
#include <stdlib.h>
#include <asmjit.h>
#include <sys/mman.h>

#include <errno.h>
#include <unistd.h>



void print_fn() {

}

void gen_stub_code_1(unsigned char* addr, unsigned char* probe_loc, void* target_fn)
{
    using namespace asmjit;
    using namespace asmjit::x86;

    // This aims to make the same one-way jump as manual_jmp_there, except from JITed code.
    // --------------------------------------------------------------------------------
    JitRuntime runtime;
    X86Assembler a(&runtime), a2(&runtime);
    // Assembler a, a2;
    // FileLogger logger(stderr);
    // a.setLogger(&logger);
    // a2.setLogger(&logger);

    // Push all volatile registers:
    a.push(rax); a.push(rcx); a.push(rdx);
    a.push(r8); a.push(r9); a.push(r10); a.push(r11);
    a.call(imm((uint64_t)target_fn));
    // Restore all volatile registers:
    a.pop(r11); a.pop(r10); a.pop(r9); a.pop(r8);
    a.pop(rdx); a.pop(rcx); a.pop(rax);

    int codesz = a.getCodeSize();
    // This works just as well, don't need the function_cast magic:
    size_t code = a.relocCode(addr);


    printf("Finished generating the code..\n");

}

// This doesn't work. Notice the constructor priority
__attribute__ ((constructor)) void premain()
{
  printf("Inside initializer..\n");
  unsigned long* stub_address = (unsigned long*)mmap((void*) 0x01230000, 1024,
        PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_FIXED| MAP_ANONYMOUS, -1,0);
  long page_size = sysconf(_SC_PAGESIZE); 
  int code = mprotect((void*)(stub_address - (((unsigned long)stub_address)%page_size)), page_size,
      PROT_READ | PROT_WRITE | PROT_EXEC);

  if (code) {
    /* current code page is now writable and code from it is allowed for execution */
    printf("mprotect was not successfull! code %d\n", code);
    printf("errno value is : %d\n", errno);
    // return -1;
  }

  // If this probesite crosses a page boundary change the permissions of adjacent page too.
  if (page_size - ((unsigned long)stub_address)%page_size < 1024) {
    code = mprotect((void*)(stub_address -((unsigned long)stub_address)%page_size+ page_size) , page_size,
        PROT_READ | PROT_WRITE | PROT_EXEC);
    if (code) {
      /* current code page is now writable and code from it is allowed for execution */
      printf("mprotect was not successfull! code %d\n", code);
      printf("errno value is : %d\n", errno);
      // return -1;
    } 
  }

	gen_stub_code_1((unsigned char*) stub_address, NULL, (void*) print_fn);// This doesn't work.
}

void dummy() {

}


// This works... Finally
/*void __attribute__ ((constructor)) premain()
{
	gen_stub_code_1(NULL, NULL, print_fn);// This doesn't work.
}*/
