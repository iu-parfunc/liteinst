#include <stdio.h>
#include <iostream>
#include <cstddef>
#include <string.h>
#include <err.h>
#include <fcntl.h>
#include <libelf.h> 
#include <stdlib.h>
#include <sysexits.h>
#include <unistd.h>
#include <cstdio>
#include <unordered_map>
#include <stack>
#include <utility>
#include <errno.h>

#include <sys/mman.h>

#include <AsmJit/AsmJit.h>

using namespace std;

/* ZCA table stuff */
typedef unsigned char byte;

/*
 * Header for a table of V1.1 annotations.
 *
 * Structures derived from Low Cost ITT Notify document and the data
 * actually generated by the compiler.
 */
struct zca_header_11_t
{
    static const std::size_t magic_sz = 16;

    uint8_t     magic[magic_sz];// Magic value - ".itt_notify_tab"
    uint16_t    version;        // Major/Minor version number
    uint16_t    entry_count;    // Count of entries that follow
    uint32_t    strings;        // Offset in bytes to string table
    uint32_t    strings_len;    // Size of string table (bytes)
    uint32_t    exprs;          // Offset in bytes to expression table
    uint32_t    exprs_len;      // Size of expression table (bytes)
};

const uint16_t ZCA_INVALID_VERSION=0xffff;
const uint16_t ZCA_1_1_VERSION=0x0101;

// Zero-padded magic value in an itt_notify annotation group
const char itt_notify_magic[zca_header_11_t::magic_sz] = ".itt_notify_tab";

/*
 * Annotation within a V1.1 table of metadata
 */
#if defined(_MSC_VER)
#pragma pack(push, 4)
#endif
struct zca_row_11_t
{
    uint64_t    anchor;     // Instruction pointer of entry
    uint32_t    probespace; // Bytes of instruction to be copied by probe
    uint32_t    annotation; // Offset of annotation string in strings table
    uint32_t    expr;       // How to compute tag parameter in intrinsic
}
#if defined(_MSC_VER)
;
#pragma pack(pop)
#else
__attribute__((packed));
#endif

/*
 * Header for a table of V1.2 annotations.
 *
 * Structures derived from Low Cost ITT Notify document and the data
 * actually generated by the compiler.
 */
struct zca_header_12_t
{
    static const std::size_t magic_sz = 16;
    
    uint8_t     magic[magic_sz];// Magic value - ".itt_notify_tab"
    uint16_t    version;        // Major/Minor version number
    uint16_t    header_size;    // Size of this structure in bytes
    uint32_t    entry_count;    // Count of entries that follow
    uint32_t    strings;        // Offset in bytes to string table
    uint32_t    strings_len;    // Size of string table (bytes)
    uint32_t    exprs;          // Offset in bytes to expression table
    uint32_t    exprs_len;      // Size of expression table (bytes)
    uint64_t    flags;          // Flags
};

const uint16_t ZCA_1_2_VERSION=0x0102;

const uint64_t zca_c_anchors_pcrel = 0x01;      // Anchor address is PC-
                                                // relative.  If not present,
                                                // anchor address is absolute

const uint64_t zca_c_have_probe_region = 0x02;  // ZCA table row has field
                                                // specifying bytes that can
                                                // be used for a probe

const uint64_t zca_c_32bit_anchor=0x04;         // The anchor address is 32bits

struct ann_data
{
  unsigned long* stubLocation;
  void (*func)();
  byte* const ip;
  const uint32_t probespace;
  const unsigned char* expr;
  int stubSize;
  
  ann_data(unsigned long int* l, void (*f)(), byte* const i, const uint32_t ps,
	   const unsigned char* e, int ss): stubLocation(l), func(f), ip(i), probespace(ps), expr(e), stubSize(ss) {}
  /*
  {
    location = l;
    func = f;
    ip = i;
    probespace = ps;
    expr = e;
  }
  */
};

// Create hashtable<annotation, row>
typedef unordered_map<string, ann_data*> ann_table;
ann_table globalAnnTable;
zca_header_11_t* globalZCATable;

//-----------------------------------------------------------------------------------
/* DWARF stuff */
#if ! defined(_MSC_VER) || (_MSC_VER >= 1600)
#include <stdint.h>
#else
#ifndef __MS_STDINT_TYPES_DEFINED__
#define __MS_STDINT_TYPES_DEFINED__
typedef signed char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef __int64 int64_t;

typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned __int64 uint64_t;
#endif  /* __MS_STDINT_TYPES_DEFINED__ */
#endif  /* ! defined(_MSC_VER) || (_MSC_VER >= 1600) */

#define LIBZCA_ASSERT(expr)                            \
    do {                                               \
      if (! (expr)) {                                  \
        std::fprintf(stderr, "warning: %s\n", #expr);  \
      }                                                \
    } while (0)

/*
 * reg_vals: array of possible registers for storing data for Pin to pick up.
 * high_register: the highest possible index into reg_vals.
 *
 * These two variables are specific to x86 vs. x86-64, but are consistent across
 * Linux and Windows.
 */

#if defined _WIN64 || defined __x86_64__
/*
static REG reg_vals[16] =
{
    REG_RAX,
    REG_RDX,
    REG_RCX,
    REG_RBX,
    REG_RSI,
    REG_RDI,
    REG_RBP,
    REG_STACK_PTR,
    REG_R8,
    REG_R9,
    REG_R10,
    REG_R11,
    REG_R12,
    REG_R13,
    REG_R14,
    REG_R15
};
*/
static int high_register = 15;
#else
/*
static REG reg_vals[8] =
{
    REG_EAX,
    REG_ECX,
    REG_EDX,
    REG_EBX,
    REG_STACK_PTR,
    REG_EBP,
    REG_ESI,
    REG_EDI
};
*/
static int high_register = 7;
#endif

/*
 * decode_LEB128
 *
 * Decode a signed Little Endian Base 128 number.  Based on the DWARF spec and
 * the Wikipedia article at http://en.wikipedia.org/wiki/LEB128
 *
 * Note that this routine assumes that all decoded values will fit in a signed
 * 32-bit word.
 */

int REG_INVALID()
{
  return 99;
}

unsigned int REG_NONE = 100;

static
int decode_LEB128(const uint8_t *p, int32_t *value, size_t *len)
{
    int32_t result = 0;
    uint32_t shift = 0;
    const uint8_t *start = p;
    uint32_t size = sizeof(result) * 8;
    uint8_t byte;

    while(1)
    {
        byte = *p++;
        result |= (byte & 0x7f) << shift;
        shift += 7;
        /* sign bit of byte is second high order bit (0x40) */
        if ((byte & 0x80) == 0)
            break;
    }

    if ((shift < size) && (byte & 0x40))
        /* sign extend */
        result |= - (1 << shift);

    *len = p - start;
    *value = (int32_t)result;

    return 0;
}

/*
 * decode_ULEB128
 *
 * Decode an unsigned Little Endian Base 128 number.  Based on the DWARF spec
 * and the Wikipedia article at http://en.wikipedia.org/wiki/LEB128 .
 *
 * Note that this routine assumes that all values will fit in an
 * unsigned 32-bit word.
 */

static
int decode_ULEB128(const uint8_t *p, uint32_t *value, size_t *len)
{
    uint32_t result = 0;
    uint32_t shift = 0;
    const uint8_t *start = p;
    uint8_t byte;

    while(1)
    {
        byte = *p++;
        result |= (byte & 0x7f) << shift;
        if (0 == (byte & 0x80))
        {
            *len = p - start;
            *value = result;
            return 0;
        }
        shift += 7;
    }
}

/*
 * dwarf_expr_to_pin
 *
 * Convert a DWARF expression from the ZCA annotation to a PIN register and
 * offset. Supported DWARF expressions:
 *
 *  DW_OP_lit* - Simple small integers.  reg is set to REG_NONE.
 *  DW_OP_reg* - Simple registers.  offset is set to 0.
 *  DW_OP_breg* - A register and an offset
 *
 * Returns:
 * 0 - success
 * 1 - failure
 */

int dwarf_expr_to_pin(const unsigned char *expression,
                      unsigned int *reg,
                      int32_t *offset)
{
    // The format is a leading "ULEB128" value specifying length,
    // followed by a number of "Dwarf location atoms".  The expressions
    // I've seen have been (in hex):
    //  - "01 30" 1 byte, literal 0, or not specified,
    //  - "01 54" 1 byte, register 4.  For x86, that would be ESP
    //
    // Full DWARF specification: http://www.dwarfstd.org/doc/DWARF4.pdf
    // DWARF expr specification: http://dwarfstd.org/doc/040408.1.html
    // The complete enum dwarf_location_atom for the Dwarf codes can be found
    // in subversion at cilk/trunk/eng/prod/intel/gcc/gcc/dwarf2.h

    // The expression starts with a length as an unsigned LEB128 value
    size_t leb128_bytes;
    uint32_t expression_length;
    decode_ULEB128(expression, &expression_length, &leb128_bytes);
    expression += leb128_bytes;

    // Subset of dwarf opcodes known to occur in our ZCA entries
    enum dwarf_opcode
    {
        DW_OP_lit0  = 0x30,     // Literal values
        DW_OP_lit31 = 0x4f,
        DW_OP_reg0  = 0x50,     // Simple registers
        DW_OP_reg31 = 0x6f,
        DW_OP_breg0 = 0x70,     // Registers + offset as a signed LEB128 value
        DW_OP_breg31 = 0x8f,
    };

    // Extract the opcode
    unsigned char opcode = *expression;
    printf("Opcode: %u\n", opcode);
    // If this is a simple register, decode it
    if ((opcode >= DW_OP_reg0) && (opcode <= DW_OP_reg31))
    {
        // Length should have been 1 since the expression consists of a single
        // byte opcode
        LIBZCA_ASSERT(1 == expression_length);

        unsigned reg_number = opcode - DW_OP_reg0;
        if (reg_number <= high_register)
        {
            *reg = reg_number;
            *offset = 0;
            return 0;
        }
        else
        {
            *reg = REG_INVALID();
            *offset = 0;
            LIBZCA_ASSERT(! "Unexpected DWARF register code");
            return 1;
        }
    }

    // If this is a simple literal value, decode it
    if ((opcode >= DW_OP_lit0) && (opcode <= DW_OP_lit31))
    {
        // Length should have been 1 since the expression consists of a single
        // byte opcode
        LIBZCA_ASSERT(1 == expression_length);

        *reg = REG_NONE;
        *offset = opcode - DW_OP_lit0;
        return 0;
    }

    // If this is a register and offset, decode it
    if ((opcode >= DW_OP_breg0) && (opcode <= DW_OP_breg31))
    {
        // The opcode specifies the register
        unsigned reg_number = opcode - DW_OP_breg0;
        if (reg_number <= high_register)
        {
            *reg = reg_number;
        }
        else
        {
            *reg = REG_INVALID();
            *offset = 0;
            LIBZCA_ASSERT(! "Unexpected DWARF register code");
            return 1;
        }

        // Advance past the opcode
        expression++;
        expression_length--;

        // The offset is a signed LEB128 value
        decode_LEB128(expression, offset, &leb128_bytes);

        return 0;
    }

    LIBZCA_ASSERT(! "Unexpected DWARF register code");
    return 1;
}

// ---------------------------------------------------
// Convenience functions for dealing with rows
// REMINDER: CONVERT TO INLINE FUNCTIONS
uint32_t getProbespace(zca_row_11_t* row)
{
  return
    row->probespace;
}

byte* getIP(zca_row_11_t* row)
{
  return
    (byte*) row->anchor;
}

const unsigned char* getExpr(zca_row_11_t* row)
{
  return
    (const unsigned char*) ((byte*) globalZCATable + globalZCATable->exprs + row->expr);
}

const char* getAnnotation(zca_row_11_t *row)
{
  return
    (const char*) ((byte*) globalZCATable + globalZCATable->strings + row->annotation);
}

// --------------------------------------------------------------------------------
// A global variable which stores the executable file name
// This code is taken from:
// http://stackoverflow.com/questions/12159595/how-to-get-a-pointer-to-an-specific-section-of-a-program-from-within-itself-ma

void populateHT(const char* progname)
{
  int fd;       // File descriptor for the executable ELF file
  char *section_name, path[256];
  size_t shstrndx;
  zca_row_11_t* row;
  
  Elf *e;           // ELF struct
  Elf_Scn *scn;     // Section index struct
  Elf64_Shdr *shdr;     // Section struct

  // Create the full path of the executable
  getcwd(path, 255);
  strcat(path, "/");
  strcat(path, progname);

  if(elf_version(EV_CURRENT)==EV_NONE)
    errx(EXIT_FAILURE, "ELF library iinitialization failed: %s", elf_errmsg(-1));

  if((fd = open(path, O_RDONLY, 0))<0)
    err(EXIT_FAILURE, "open \"%s\" failed", path);

  if((e = elf_begin(fd, ELF_C_READ, NULL))==NULL)
    errx(EXIT_FAILURE, "elf_begin() failed: %s.", elf_errmsg(-1));

  // Retrieve the section index of the ELF section containing the string table of section names
  if(elf_getshdrstrndx(e, &shstrndx)!=0)
    errx(EXIT_FAILURE, "elf_getshdrstrndx() failed: %s.", elf_errmsg(-1));

  scn = NULL;

  // Loop over all sections in the ELF object
  while((scn = elf_nextscn(e, scn))!=NULL) {
    // Given a Elf Scn pointer, retrieve the associated section header
    if((shdr = elf64_getshdr(scn))!=shdr)
      errx(EXIT_FAILURE, "getshdr() failed: %s.", elf_errmsg(-1));

    // Retrieve the name of the section name
    if((section_name = elf_strptr(e, shstrndx, shdr->sh_name))==NULL)
      errx(EXIT_FAILURE, "elf_strptr() failed: %s.", elf_errmsg(-1));

    printf("Found section! %s\n", section_name);

    // If the section is the one we want... (in my case, it is one of the main file sections)
    if(!strcmp(section_name, ".itt_notify_tab")) {

      // We can use the section adress as a pointer, since it corresponds to the actual
      // adress where the section is placed in the virtual memory
      struct data_t * section_data = (struct data_t *) shdr->sh_addr;

      printf("Yep, got itt_notify... data is at addr %p.  Now to parse it!\n", section_data);
      
      // Cast section data
      zca_header_11_t *table  = (zca_header_11_t*) section_data;
      globalZCATable = table;
      row = (zca_row_11_t*) ((byte*) table + sizeof(*table));
      const char *str = (const char *) ((byte*) table + table->strings + row->annotation);
      const unsigned char *expr = (const unsigned char *) ((byte*) table + table->exprs + row->expr);      
      unsigned int reg = 200;
      int32_t offset = 200;
      
      // Put tag parameter regester and offset data into &reg and &offset
      dwarf_expr_to_pin(expr, &reg, &offset);
      
      for (int i = 0; i < table->entry_count; i++)
	{
	  cout << getAnnotation(row) << endl;
	  printf("%p\n", getIP(row));
	  ann_data* ad = new ann_data(nullptr, nullptr, getIP(row), getProbespace(row), getExpr(row), NULL);
          const char* str = getAnnotation(row);
	  
	  globalAnnTable.insert(ann_table::value_type(string(str), ad));
	  // Move to next row
	  row = (zca_row_11_t*) ((byte*) row + sizeof(*row));
	}
      row = (zca_row_11_t*) ((byte*) table + sizeof(*table));

      /*
      cout that shit
      cout << "  reg/offset: " << reg << ", " << offset << endl;
      cout << "  table " << table << ", row " << row << ", row byteoffset " << (byte*) row - (byte*) table << ", table_size "
      	   << sizeof(*table) << endl;
      cout << "  row "<< row << ": label \"" << str << "\", str offset " << (byte*) str - (byte*) table << ", strings "
	   << table->strings << ", annotation " << row->annotation 
           << ", probespace " << row->probespace
           << ", IP " << (void*)(row->anchor)
           << endl;
      */
      
      ann_data* tstann = globalAnnTable["probe1"];
      printf("Populating table, annotation 'probe1': struct is at addr %p, reading IP %p (from %p)\n", 
	     tstann, tstann->ip, &(tstann->ip));

      cout << "probespace: " << globalAnnTable["probe1"]->probespace << endl;
      // cout << "expr: " << getExpr(table, row) << endl;
      
      // End the loop (if we only need this section)
      break;
    }
  }

  elf_end(e);
  close(fd);
}


static void print_fn() { 
  printf("MADE IT -- to the print function!\n");
  return; 
}


// void write_abs_jump(unsigned char *opcodes, const void *jmpdest)
// {
//     opcodes[0] = 0xFF; 
//     opcodes[1] = 0x25;

//     // Whoa... does this stomp on 
//     *reinterpret_cast<DWORD *>(opcodes + 2) = reinterpret_cast<DWORD>(opcodes + 6);
//     *reinterpret_cast<DWORD *>(opcodes + 6) = reinterpret_cast<DWORD>(jmpdest);
// }


typedef void (*MyFn)();

void* gen_stub_code(const char* ann, unsigned char* addr, unsigned char* probe_loc, void* target_fn, uint32_t probespace)
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
    globalAnnTable[ann]->stubSize = codesz;
    // This works just as well, don't need the function_cast magic:
    sysuint_t code = a.relocCode(addr);
    
    // Copy over the displaced probe bytes:
    for(int i=0; i<probespace; i++)
      addr[codesz + i] = probe_loc[i];
    
    // Next generate the jump back home:
    a2.jmp(imm((sysint_t)(void*)(probe_loc + probespace)));
    int sz2 = a2.getCodeSize();
    a2.relocCode(addr + codesz + probespace);
    
    // TEMP: Fill with NOOPS:
    for(int i=0; i<1000; i++)
      addr[codesz + probespace + sz2 + i] = 0x90;

    printf("  Size of return jmp %d, total size %d\n", sz2, codesz + probespace + sz2);
    for(int i=0; i<codesz + probespace + sz2; i++) 
      printf("  %p", addr[i]);
    printf("\n");
    return addr;
}

int activateProbe(const char* ann)
{
  cout << "activateProbe(" << ann << ")" << endl;
  ann_data* tstann = globalAnnTable[string(ann)];
  printf("Activating probe: test annotation 'probe1': struct is at addr %p, reading IP %p (from %p)\n", tstann, tstann->ip, &(tstann->ip));
  
  byte* ip = tstann->ip;
  unsigned long ipn = (unsigned long)ip;
  int page = 4096;   /* size of a page */
  
  // I think we need to mprotect a page-aligned address:
  int code = mprotect((void*)(ipn - (ipn%4096)), page, PROT_READ | PROT_WRITE | PROT_EXEC);
  if (code)
    {
      /* current code page is now writable and code from it is allowed for execution */
      fprintf(stderr,"mprotect was not successfull! code %d\n", code);
      return 1;
    }

  // Assign memory for stub code
  unsigned long* base;
  if (tstann->stubLocation == nullptr)
    {
      base = (unsigned long*)0x01230000;
      base = (unsigned long*)mmap(base, 4096, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS, -1,0);

      if (base == MAP_FAILED)
	{
	  int err = errno;
	  printf("Got error on mmap: %s\n", strerror(err));
	  exit(1);
	}
      
      tstann->stubLocation = base;
      printf("Mmap'd scratch region starting at %p\n", base);
    }
  else
    base = tstann->stubLocation;
  
  // Here we repoint the probe site to the function print_fn, but any other function would work too!
  void* dst = gen_stub_code(ann, (byte*)(base + 4), ip, &print_fn, tstann->probespace);
  printf("Done generating stub code at %p\n", dst);
  
  // This does a relative jump:
  ip[0] = 0xE9;
  // Size of the JMP we insert is 5.  So +5 points to the following instruction.
  long relative = (long)(((unsigned char*)(void*) dst) - ip - 5);
  printf("  Relative offset of dest from starting addr: %p %d, 32 bit: %p\n", relative, relative, (int)relative);
  *(uint32_t*)(ip+1) = (int)relative;
  ip[5] = 0x0;
  
  for(int i=-4; i<12; i++) 
    printf("   Byte %d of probe space = %d = %p\n", i, ip[i], ip[i]);

  printf("Ultimate destination function lives at %p.\n", &print_fn);
  printf("JITed stub lives at %p.\n", dst);
  printf("Finished mutating ourselves... enter the danger zone.\n");

  return 0;
}

int deactivateProbe(const char* ann)
{
  ann_data* ad = globalAnnTable[ann];
  
  byte* loc = (byte*) (ad->stubLocation + 4 + ad->stubSize);
  byte* ip = ad->ip;
  uint32_t probespace = ad->probespace;

  for (int i = 0; i < probespace; i++)
      ip[i] = loc[i];

  return 1;
}

void test()
{
  int x = 5;
  __notify_intrinsic((void*)"probe1", (void*)&x);
  printf("[app] We are the borg.\n");
  __notify_intrinsic((void*)"probe2", (void*)&x);
  printf("[app] Done.\n");  
}

int main(int argc, char *argv[])
{
#if 0
  // This works as expected:
   asm("jmp *%0"::"r"(print_fn):);

   asm("jmp 7");

   asm("jmp 0x4020fa");
   // This generates:   401ce9:	e9 0c 04 00 00  jmpq   4020fa <itt_notify_magic+0x54>
   // Notice that it still uses four bytes.  
   // And I don't know what the "E9 cw" opcode is supposed to mean, I
   // don't see where the "cw" shows up or has any effect.

   asm("jmp *7(%rip)");

   //  asm(".intel_syntax;" "JMP rel32 7");
   // 
#else

  printf("Calling populateHT(argv[0]) ... \n");
  populateHT(argv[0]);
  //  cout << globalAnnTable["probe1"] << endl;
  printf("  Done .");

  /*
  printf("Now to self-modify... call activateProbe(\"probe1\")\n");
  int status = activateProbe("probe1");
  test();
  printf("Now to deactivate... call deactivateProbe(\"probe1\")\n");
  deactivateProbe("probe1");
  test();
  */
  
  printf("Activate next probe... call activateProbe(\"probe2\")\n");
  int status = activateProbe("probe2");
  test();
  printf("Now to deactivate... call deactivateProbe(\"probe2\")\n");
  deactivateProbe("probe2");
  test();
#endif
  
  return 1;
}
