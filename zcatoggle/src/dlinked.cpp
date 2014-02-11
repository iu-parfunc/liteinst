

#include <stdio.h>
#include "elf-provider.h"
#include <sys/mman.h>
#include "AsmJit/AsmJit.h"
#include <errno.h>

#define PROBESIZE 6

int gen_stub_code(unsigned char* addr, unsigned char* probe_loc, void* target_fn)
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

	// Copy over the displaced probe bytes:
/*	for(int i=0; i<PROBESIZE; i++)
		addr[codesz + i] = probe_loc[i];*/

	// Next generate the jump back home:
	a2.jmp(imm((sysint_t)(void*)(probe_loc + PROBESIZE)));
	int sz2 = a2.getCodeSize();
	a2.relocCode(addr + codesz);

	// TEMP: Fill with NOOPS:
	for(int i=0; i<1000; i++)
		addr[codesz + PROBESIZE + sz2 + i] = 0x90;

	probe_loc[0] = 0xE9;
	// Size of the JMP we insert is 5.  So +5 points to the following instruction.
	long relative = (long)(((unsigned char*)(void*) addr) - probe_loc - 5);
	printf("  Relative offset of dest from starting addr: %p %p, 32 bit: %lu\n", probe_loc, addr, (int)relative);
	*(uint32_t*)(probe_loc+1) = (int)relative;
	probe_loc[5] = 0x0;

	printf("  Size of return jmp %d, total size %d\n", sz2, codesz + PROBESIZE + sz2);
	for(int i=0; i<codesz + PROBESIZE + sz2; i++)
		printf("  %p", addr[i]);
	printf("\n");
	return (codesz+ PROBESIZE + sz2);
}

void print_this() {
	printf("This shit better work..\n");
}

int main () {
	printf("[test] Simple program to read the ELF table of hello_notify.exe\n");
	// read_zca_probes("hello_notify.exe");
	int probes = read_self_zca_probes();
	printf("***** Probe count : %d\n", probes);

	unsigned long* base = (unsigned long*)0x01230000;// TODO : Figure out how to make sure this memory island is
	//        reachable with short jmp without hard coding addresses
	printf("Base address is : %p\n\n", base);
	base = (unsigned long*)mmap(base, 4096, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS, -1,0);
	if (base == MAP_FAILED) {
		int err = errno;
		printf("Got error on mmap: %s\n", strerror(err));
		return -1;
	}
	printf("Mmap'd scratch region starting at %p\n", base);

	unsigned long* stub_address = base + 4;
	printf("First stub starting at %p\n", stub_address);

	printf("Annotation table points to : %p\n", annotations);
	printf("Address of print_this function : %lu\n", print_this);

	int i;
	for (i=0; i < probes; i++) {
		unsigned char* probe_address = (unsigned char*)((ann_data*)&annotations[i])->ip;

		printf("Probe address is : %lu\n", (unsigned char*)(&annotations[i])->ip);

		int page_size = 4096;
		int code = mprotect((void*)(probe_address - (((unsigned long)probe_address)%4096)), page_size, PROT_READ | PROT_WRITE | PROT_EXEC);

		if (code) {
			/* current code page is now writable and code from it is allowed for execution */
			fprintf(stderr,"mprotect was not successfull! code %d\n", code);
			printf("errno value is : %d\n", errno);
			// return 1;
		}

		// int stub_size = gen_stub_code((unsigned char*)(stub_address), probe_address, (&annotations[i])->fun);
		int stub_size = gen_stub_code((unsigned char*)(stub_address), probe_address, &print_this);
		// ((&annotations[i])->fun)(); This works
		// Plug in the relative jump
		// This does a relative jump:
		probe_address[0] = 0xE9;
		// Size of the JMP we insert is 5.  So +5 points to the following instruction.
		long relative = (long)(((unsigned char*)(void*) stub_address) - probe_address - 5);
		printf("  Relative offset of dest from starting addr: %lu %lu, 32 bit: %d\n", probe_address, stub_address, (int)relative);
		*(uint32_t*)(probe_address+1) = (int)relative;
		probe_address[5] = 0x0;

		// Next stub address
		stub_address += (stub_size + 1);
		printf("Stub %d starting at %lu \n", i+1, stub_address);

	}

	printf("Print dummy..\n");
	__notify_intrinsic((void*)"notify01",(void*)99);
	printf("I should be sandwiched here.. \n");
// 	__notify_intrinsic((void*)"notify02",(void*)99);

	// read_zca_probes("/nfs/nfs3/home/rrnewton/working_copies/onlineProfiling/onlineProfiling/niknatar_prototype/test5.exe");

	printf("[test] Done reading probes!\n");
}
