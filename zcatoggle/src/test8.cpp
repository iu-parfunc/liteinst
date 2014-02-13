#include <stdio.h>
#include <stdlib.h>
// #include "zca-toggle.h"
#include <sys/mman.h>
#include <fcntl.h>  // O_RDONLY
#include <errno.h>
#include <err.h>     // err
#include <libelf.h>
#include <unistd.h>  // getcwd
#include <sys/param.h>
#include <AsmJit/AsmJit.h>
#include "zca-types.h"

#define PROBESIZE 6

extern const char *__progname;

ann_data* annotations;

#define dbgprint printf


void __attribute__ ((constructor(10))) premain()
{
	 // initZCAService();// Still get this without dynamically or statically linking when called from constructor
}

int gen_stub_code_1(unsigned char* addr, unsigned char* probe_loc, void* target_fn)
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
    for(int i=0; i<PROBESIZE; i++)
      addr[codesz + i] = probe_loc[i];

    // Next generate the jump back home:
    a2.jmp(imm((sysint_t)(void*)(probe_loc + PROBESIZE)));
    int sz2 = a2.getCodeSize();
    a2.relocCode(addr + codesz + PROBESIZE);

    // TEMP: Fill with NOOPS:
/*     for(int i=0; i<1000; i++)
       addr[codesz + PROBESIZE + sz2 + i] = 0x90;*/

    printf("  Size of return jmp %d, total size %d\n", sz2, codesz + PROBESIZE + sz2);
    for(int i=0; i<codesz + PROBESIZE + sz2; i++)
      printf("  %p", addr[i]);
    printf("\n");
    return (codesz + PROBESIZE + sz2);
}

/*const char* getAnnotation(zca_header_11_t *table, zca_row_11_t *row)
{
  return
    (const char*) ((byte*) table + table->strings + row->annotation);
}*/

void print_fn() {
	printf("[Successful] We are the borg\n");

}

void print_fn2() {
	printf ("[Successful] May the Force be with you\n");
}

int read_zca_probes(const char* path)
{
	int fd;       // File descriptor for the executable ELF file
	char *section_name;
	size_t shstrndx;
	zca_row_11_t* rows;

	int probe_count; // Number of probes discovered

	Elf *e;           // ELF struct
	Elf_Scn *scn;     // Section index struct
	Elf_Data *data;     // Section index struct
	Elf64_Shdr *shdr;  // Section strkkkuct

	if(elf_version(EV_CURRENT)==EV_NONE)
		errx(EXIT_FAILURE, "ELF library iinitialization failed: %s", elf_errmsg(-1));

	if((fd = open(path, O_RDONLY, 0))<0)
		err(EXIT_FAILURE, "open file \"%s\" failed", path);

	if((e = elf_begin(fd, ELF_C_READ, NULL))==NULL)
		errx(EXIT_FAILURE, "elf_begin() failed: %s.", elf_errmsg(-1));

	// Retrieve the section index of the ELF section containing the string table of section names
	if(elf_getshdrstrndx(e, &shstrndx)!=0)
		errx(EXIT_FAILURE, "elf_getshdrstrndx() failed: %s.", elf_errmsg(-1));

	scn = NULL;

	dbgprint(" LOADING ELF HEADER FOR FILE %s\n", path);

	// Loop over all sections in the ELF object
	while((scn = elf_nextscn(e, scn))!=NULL) {
		// Given a Elf Scn pointer, retrieve the associated section header
		if((shdr = elf64_getshdr(scn))!=shdr)
			errx(EXIT_FAILURE, "getshdr() failed: %s.", elf_errmsg(-1));

		// Retrieve the name of the section name
		if((section_name = elf_strptr(e, shstrndx, shdr->sh_name))==NULL)
			errx(EXIT_FAILURE, "elf_strptr() failed: %s.", elf_errmsg(-1));

		dbgprint("(section %s) ", section_name);

		// If the section is the one we want... (in my case, it is one of the main file sections)
		if(!strcmp(section_name, ".itt_notify_tab")) {

			data = elf_getdata(scn, NULL);

			// We can use the section adress as a pointer, since it corresponds to the actual
			// adress where the section is placed in the virtual memory
			// struct data_t * section_data = (struct data_t *) shdr->sh_addr; // This seems bogus!
			dbgprint("\n [read-zca] got itt_notify... section data is at addr %p, header at %p.\n", data, shdr);

			// Cast section data
			zca_header_11_t* header  = (zca_header_11_t*) data->d_buf;
			char* ptr = (char*)header;
			int i = 0;
			while(1) {
				header = (zca_header_11_t*)ptr;
				char* str = (char*)header->magic;
				printf(" [read-zca] check (%d) for magic value at loc %p : %d %d %d %d \n", i, header,
						str[0], str[1], str[2], str[3]);
				if (!strcmp(str, ".itt_notify_tab")) {
					// table->magic[0] == '.' && table->magic[1] == 'i' &&
					// table->magic[2] == 't' && table->magic[3] == 't') {
					printf(" magic number MATCHED 16 bytes!\n");
					break;
				}
				printf(" should be %d %d %d %d\n", '.','i','t','t');
				ptr++;
				i++;
			}
			uint8_t* ver = (uint8_t*) & (header->version);
			printf("Now that we've found the magic number, version num is: %d / %d\n", ver[0], ver[1]);

			// Here we skip the header and move on to the actual rows:
			rows = (zca_row_11_t*) ((byte*) header + sizeof(*header));
			// const unsigned char *expr = (const unsigned char *) ((byte*) table + table->exprs + row->expr);
			dbgprint(" [read-zca] found first row at %p, offset %lu\n", rows, ((long)rows - (long)header));

			const char *str = getAnnotation(header,rows);
			dbgprint("  -> Got the annotation for this row: %s\n", str);

			// const unsigned char *expr = getExpr(table,row);

			unsigned int reg = 200;
			int32_t offset = 200;

			annotations = (ann_data*)malloc(sizeof(ann_data) * (header->entry_count));
			probe_count = header->entry_count;
			printf("\n\nEntry count : %d\n", header->entry_count);
			printf("Annotation table resides at : %p\n", annotations);
			for (int i = 0; i < header->entry_count; i++)
			{
				ann_data* annotation = &annotations[i];
				zca_row_11_t* row = (zca_row_11_t*)((byte*)rows + sizeof(*row) * i);

				printf ("Row[%d] address is : %p\n", i, row);

				annotation->ip = row->anchor;
				annotation->probespace = row->probespace;

				if (i%2 == 0) {
				    annotation->fun = &print_fn;
				} else {
					annotation->fun = &print_fn2;
				}
				annotation->expr = getAnnotation(header, row);

				//probe_count++;

				printf ("\n------------ Annotation [%d] --------------\n", i);
				printf ("annotation-ip : %lu\n", (unsigned char*)annotation->ip);
				printf ("annotation-probespace : %d\n", annotation->probespace);
				printf ("annotation-func : %p \n", (unsigned char*)annotation->fun);
				printf ("annotation-expr : %s \n\n", annotation->expr);
			}
			break;
		}
	}

	elf_end(e);
	close(fd);

	return probe_count;
}

void get_working_path(char* buf)
{
	getcwd(buf, MAXPATHLEN);
	strcat(buf, "/");
	strcat(buf, __progname);
	// return ( getcwd(temp, MAXPATHLEN) ? std::string( temp ) : std::string("") );
}

int read_self_zca_probes()
{
	char path[MAXPATHLEN];
	get_working_path(path);
	return read_zca_probes(path);

}

void setupStub() {

	int probe_count=read_self_zca_probes();
	// probe_count = 1092;
	printf ("Probe count returned by read_self_zca_probes : %d\n", probe_count);

	unsigned long* base = (unsigned long*)0x01230000;// TODO : Figure out how to make sure this memory island is
	                                                 //        reachable with short jmp without hard coding addresses
	printf("Base address is : %p\n\n", base);
	base = (unsigned long*)mmap(base, 4096, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_FIXED| MAP_ANONYMOUS, -1,0);
	if (base == MAP_FAILED) {
		int err = errno;
		printf("Got error on mmap: %s\n", strerror(err));
		return; //exit(1);
	}
	printf("Mmap'd scratch region starting at %p\n", base);

	unsigned long* stub_address = base + 4;
	printf("First stub starting at %p\n", stub_address);

	printf("Annotation table points to : %p\n", annotations);

	int i;
	for (i=0; i < probe_count; i++) {
		unsigned char* probe_address = (unsigned char*)((ann_data*)&annotations[i])->ip;

		printf("Probe address is : %p\n", (unsigned char*)(&annotations[i])->ip);

		int page_size = 4096;
		int code = mprotect((void*)(probe_address - (((unsigned long)probe_address)%4096)), page_size, PROT_READ | PROT_WRITE | PROT_EXEC);

		if (code) {
			/* current code page is now writable and code from it is allowed for execution */
			fprintf(stderr,"mprotect was not successfull! code %d\n", code);
			printf("errno value is : %d\n", errno);
			// return 1;
		}

        int stub_size = gen_stub_code_1((unsigned char*)(stub_address), probe_address, (&annotations[i])->fun);
        // ((&annotations[i])->fun)(); This works
        // Plug in the relative jump
        // This does a relative jump:
        probe_address[0] = 0xE9;
        // Size of the JMP we insert is 5.  So +5 points to the following instruction.
        long relative = (long)(((unsigned char*)(void*) stub_address) - probe_address - 5);
        printf("  Relative offset of dest from starting addr: %p %p, 32 bit: %d\n", probe_address, stub_address, (int)relative);
        *(uint32_t*)(probe_address+1) = (int)relative;
        probe_address[5] = 0x0;

        // Next stub address
        stub_address += (stub_size + 1);
        printf("Stub %d starting at %p \n", i+1, stub_address);

	}
	// Generate stubs for each of them and modify the probe sites to jump to them
	// Initialize the book keeping data structure mapping probe site to generated stub address for
	// subsequent activations after a deactivation

}

int main () {
  // initZCAService(); // Still seg faults
  setupStub();

  printf("Ultimate destination function lives at.");
  printf("JITed stub lives at.\n" );
  printf("Finished mutating ourselves... enter the danger zone.\n");
  int x=2;
  // printf("[test] Simple program to read the ELF table of hello_notify.exe\n");
  // read_zca_probes("hello_notify.exe");
  __notify_intrinsic((void*)"notify01",(void*)&x);
  printf("Print something..\n");
  // int probes = read_self_zca_probes();
  // printf("***** Probe count : %d\n", probes);
  __notify_intrinsic((void*)"notify01",(void*)&x);

  printf("[test] Done reading probes!\n");
}

