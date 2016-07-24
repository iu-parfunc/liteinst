
#include <cstdint>
#include <map>

#include <string.h>  // memcpy
#include <unistd.h>  // sysconf
#include <sys/mman.h> // mprotect
#include <sys/param.h> // MAXPATHLEN
#include <assert.h>  

#include "elf64.h"
#include "sym_tab.h"
#include "elf64_shr.h"

#include "defs.hpp"

// #include "control_flow_router.hpp"
// #include "process.hpp"

namespace liteinst {
namespace liteprobes {

// using namespace utils::process;

using std::map;
using utils::Address;

void* g_main_ptr = nullptr; // Start address of the main function

// The stub will do the following.
// 1. Save the context (registers).
// 2. Push argument to the register (RDI)
// 3. Call the trampoline function.
// 4. Restore the context (registers)
// 5. Jump to back to the original context given by return address 
// -------------------------------------------------------------------
uint8_t g_stub[] = 
{ 0x9c, /* pushfq */
  0x56, /* push %rsi */
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
  0xff, 0x15, 0x24, 0x00, 0x00, 0x00,       /* callq *0x23(%rip) */
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
  0x9d, /* popfq */
  0xff, 0x25, 0x00, 0x00, 0x00, 0x00, /* jumpq *%rip */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* return address */
  // 0x40, 0xe9, 0x00, 0x00, 0x00, 0x00, /* rex jmpq xx*/
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 /* trampoline fn address */};

uint8_t g_rip_indirect_jump[] =
{ 0xff, 0x25, 0x00, 0x00, 0x00, 0x00,       /* jumpq *%rip */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 /* jump target */};

uint8_t g_saved_main_prolog[sizeof(g_rip_indirect_jump)/
  sizeof(g_rip_indirect_jump[0])];

extern void init();

void initializeLiteprobes() {

  // Restore the prolog of main
  memcpy(g_main_ptr, (const void*) g_saved_main_prolog,
     sizeof(g_rip_indirect_jump)/sizeof(g_rip_indirect_jump[0]));

  init();
}

void liteprobesInfectMain() {

  char* c_program_path = (char*) malloc(sizeof(char) * MAXPATHLEN);
  ssize_t len = 0;
  if (c_program_path != NULL) {
    if ((len = readlink("/proc/self/exe", c_program_path, 
            sizeof(char) * MAXPATHLEN)) == -1) {
      free(c_program_path);
      assert(false);
    }
  }
  c_program_path[len] = '\0';


  ELF *bin = elf64_read(c_program_path);
  unsigned int nb_sym = bin->symtab_num;
  Elf64_Sym **tab = bin->symtab;

  Elf64_Half strtab_idx = get_section_idx(bin, ".strtab");
  Elf64_Off strtab_offset = bin->shr[strtab_idx]->sh_offset;

  for (unsigned int i=0; i < nb_sym; i++) {
    if ((tab[i]->st_info & 0x0F) == STT_FUNC) {
      char* s_name = get_sym_name(bin->file, tab[i], strtab_offset);

      if (!strcmp(s_name, "main")) {
        g_main_ptr = (Address) tab[i]->st_value; 
        break;
      }
    }
  }

  // free(bin->file);
  free(bin);

  assert(g_main_ptr != nullptr);

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

  fprintf(stderr, "[premain] Inject trampoline fn call address to the "
      "stub..\n");
  // Inject trampoline fn call address to the stub
  int8_t trampoline_fn_offset = (sizeof(g_stub) /
      sizeof(g_stub[0])) - 8; 
  fprintf(stderr, "[premain] Trampoline function offset : %d\n",
      trampoline_fn_offset);
  *(uint64_t*)&((uint8_t*)stub)[trampoline_fn_offset] =
    (uint64_t) initializeLiteprobes;

  fprintf(stderr, "[premain] Inject return jump address to the stub..\n");
  // Inject return jump address to the stub
  int8_t return_addr_offset = (sizeof(g_stub) / sizeof(g_stub[0])) - 16; 
  *(uint64_t*)&((uint8_t*)stub)[return_addr_offset] = (uint64_t) g_main_ptr;

  fprintf(stderr, "[premain] Setup the call to stub..\n");
  // Setup the call to stub 
  *(uint64_t*)&((uint8_t*)g_rip_indirect_jump)[6] = (uint64_t) stub;

  int8_t patch_site_length = 
    sizeof(g_rip_indirect_jump) / sizeof(g_rip_indirect_jump[0]);
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
      sizeof(g_rip_indirect_jump)/sizeof(g_rip_indirect_jump[0])); 

  // Overwrite the prolog with a jump to instrumentation initialization stub
  memcpy(g_main_ptr, (const void*) g_rip_indirect_jump, 
      sizeof(g_rip_indirect_jump)/sizeof(g_rip_indirect_jump[0])); 

  fprintf(stderr, "[premain] Done patching..\n");
}



} // End liteprobes 
} // End liteinst 

extern "C" void boostrap();

// __attribute__((visibility("default")))
__attribute__((constructor))
void boostrap() {
  printf("Inside boostrap\n");
  liteinst::liteprobes::liteprobesInfectMain();
}

extern "C" void dummy() {
  printf("Dummy..\n");
}
