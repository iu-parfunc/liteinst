#include <stdio.h>
#include <stdlib.h>
#include <AsmJit/AsmJit.h>

void print_fn() {

}

void gen_stub_code_1(unsigned char* addr, unsigned char* probe_loc, void* target_fn)
{
    using namespace AsmJit;

    // This aims to make the same one-way jump as manual_jmp_there, except from JITed code.
    // --------------------------------------------------------------------------------
    Assembler a, a2;
    FileLogger logger(stderr);
    a.setLogger(&logger);
    a2.setLogger(&logger);

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

}

// This doesn't work. Notice the constructor priority
/*void __attribute__ ((constructor(10))) premain()
{
	gen_stub_code_1(NULL, NULL, print_fn);// This doesn't work.
}*/


// This works... Finally
void __attribute__ ((constructor)) premain()
{
	gen_stub_code_1(NULL, NULL, print_fn);// This doesn't work.
}

int main() {

	// gen_stub_code_1(NULL, NULL, NULL); // This works too
	printf("Main is here..\n");

}
