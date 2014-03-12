

/** 
 * @brief Inspect an ELF binary and recover the probe locations from its headers.
 * 
 * 
 */

#include "elf-provider.h"

#include <stdio.h>
#include <stdlib.h> // EXIT_FAILURE
#include <fcntl.h>  // O_RDONLY
#include <string.h>
#include <err.h>     // err
#include <unistd.h>  // getcwd
#include <libelf.h>

#include "logger.h"   // LOG_DEBUG
#include "zca-types.hpp"
// #include "zca-utils.h"

#include <sys/param.h> // MAXPATHLEN
#include <string>
#include <unordered_map>

// Temp:
#include <iostream>
/* #include <cstddef> */
// #include <sysexits.h> 

/* #include <cstdio> */
/* #include <stack> */
/* #include <utility> */
// #include <errno.h>

/* #include <sys/mman.h> */
/* #include <AsmJit/AsmJit.h> */

using namespace std;

//! A table mapping probe names onto the runtime metadata.
// typedef unordered_map<string, ann_data*> ann_table;
// FIXME: this needs to be a concurrent data structure ultimately.

// typedef unordered_map<string, pair<zca_row_11_t*, unsigned long*>> ann_table;

ann_table annotations;
mem_alloc_table mem_allocations;

unsigned long probe_start;
unsigned long probe_end;
// zca_header_11_t* globalZCATable;

#define dbgprint printf

static int high_register = 15;
int REG_INVALID()
{
  return 99;
}

unsigned int REG_NONE = 100;

static int decode_LEB128(const uint8_t *p, int32_t *value, size_t *len)
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
        // LIBZCA_ASSERT(1 == expression_length);

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
	    //            LIBZCA_ASSERT(! "Unexpected DWARF register code");
            return 1;
        }
    }

    // If this is a simple literal value, decode it
    if ((opcode >= DW_OP_lit0) && (opcode <= DW_OP_lit31))
    {
        // Length should have been 1 since the expression consists of a single
        // byte opcode
      //        LIBZCA_ASSERT(1 == expression_length);

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
	    //            LIBZCA_ASSERT(! "Unexpected DWARF register code");
            return 1;
        }

        // Advance past the opcode
        expression++;
        expression_length--;

        // The offset is a signed LEB128 value
        decode_LEB128(expression, offset, &leb128_bytes);

        return 0;
    }

    //    LIBZCA_ASSERT(! "Unexpected DWARF register code");
    return 1;
}

//--------------------------------------------------------------------------------

/** Read the probes available in an ELF binary.
 *
 * This examines data section headers to retrieve the ZCA probe
 * information (section .itt_notify_tab).  It returns a freshly
 * allocated table containing the metadata.
 */
int read_zca_probes(const char* path)
{
  int fd;       // File descriptor for the executable ELF file
  char *section_name;
  size_t shstrndx;
  zca_row_11_t* rows;

  int probe_count = 0; // Number of probes discovered

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
  
  // Loop over all sections in the ELF object
  while((scn = elf_nextscn(e, scn))!=NULL) {
    // Given a Elf Scn pointer, retrieve the associated section header
    if((shdr = elf64_getshdr(scn))!=shdr)
      errx(EXIT_FAILURE, "getshdr() failed: %s.", elf_errmsg(-1));

    // Retrieve the name of the section name
    if((section_name = elf_strptr(e, shstrndx, shdr->sh_name))==NULL)
      errx(EXIT_FAILURE, "elf_strptr() failed: %s.", elf_errmsg(-1));

    LOG_DEBUG("(section %s) ", section_name);

    // If the section is the one we want... (in my case, it is one of the main file sections)
    if(!strcmp(section_name, ".itt_notify_tab")) {

      data = elf_getdata(scn, NULL);

      // We can use the section adress as a pointer, since it corresponds to the actual
      // adress where the section is placed in the virtual memory
      // struct data_t * section_data = (struct data_t *) shdr->sh_addr; // This seems bogus!
      LOG_DEBUG("\n [read-zca] got itt_notify... section data is at addr %p, header at %p.\n", data, shdr);

      // Cast section data
      zca_header_11_t* header  = (zca_header_11_t*) data->d_buf;
      char* ptr = (char*)header;
      int i = 0;
      while(1) {
	header = (zca_header_11_t*)ptr;
	char* str = (char*)header->magic;

	LOG_DEBUG(" [read-zca] check (%d) for magic value at loc %p : %d %d %d %d \n", i, header,
		  str[0], str[1], str[2], str[3]);
	if (!strcmp(str, ".itt_notify_tab")) {
	  // table->magic[0] == '.' && table->magic[1] == 'i' &&
	  // table->magic[2] == 't' && table->magic[3] == 't') {

	  LOG_DEBUG(" magic number MATCHED 16 bytes!\n");
	  break;
	}

	LOG_DEBUG(" should be %d %d %d %d\n", '.','i','t','t');
	ptr++;
	i++;
      }
      uint8_t* ver = (uint8_t*) & (header->version);

      LOG_DEBUG("Now that we've found the magic number, version num is: %d / %d\n", ver[0], ver[1]);

      // Here we skip the header and move on to the actual rows:
      zca_row_11_t* row = (zca_row_11_t*) ((byte*) header + sizeof(*header));

      LOG_DEBUG(" [read-zca] found first row at %p, offset %lu\n", rows, ((long)rows - (long)header));
      LOG_DEBUG("\n\nAnnotation entry count : %d\n", header->entry_count);
      LOG_DEBUG("Annotation table resides at : %p\n", annotations);

      probe_count = header->entry_count;
      ann_data* ann_info = (ann_data*)malloc(sizeof(ann_data) * (header->entry_count));
      // printf("Probe count : %d\n", probe_count);

      for (int i = 0; i < header->entry_count; i++)
	{
	  if (i == 0) {
		  probe_start = row->anchor;
	  } else if (i == (header->entry_count - 1)) {
		  probe_end = row->anchor;
	  }

	  //	  LOG_DEBUG("\n------------ Annotation [%d] --------------\n", i);
	  //	  LOG_DEBUG("annotation-ip : %lu\n", (unsigned char*)annotation->ip);
	  //	  LOG_DEBUG("annotation-probespace : %d\n", annotation->probespace);
	  //	  LOG_DEBUG("annotation-func : %p \n", (unsigned char*)annotation->fun);
	  //	  LOG_DEBUG("annotation-expr : %s \n\n", annotation->expr);
	  
	  // ann_data ad = ann_data(NULL, NULL, fun, getIP(row), getProbespace(row), getExpr(header, row));

	  const char* str = getAnnotation(header, row);

	  /*
	  const byte* expr = getExpr(header, row);
	  unsigned int reg = 99;
	  int32_t offset = 0;
	  dwarf_expr_to_pin(expr, &reg, &offset);
	  printf("\nregister: %u\noffset: %d\n", reg, offset);
	  
	  register int* in asm("rax");
	  printf("RAX: %d\n", *(in + 1));
	  */
	  
	  // printf("The annotation string  is : %s\n", str);
	  
	  // printf("Row %d address : %p\n", i, row);
	  // TODO : Decode in annotation's expression from zca_row exp and store in the ann_data
	  // ann_info[i].exp = <<>>
	  annotations.insert(ann_table::value_type(string(str), pair<zca_row_11_t*, ann_data*>(row, &(ann_info[i]))));
	  ann_info[i].fun = print_fn;
	  
	  uint64_t probe_adddress = row->anchor;
	  uint32_t mem_chunk = ((uint64_t)probe_adddress) >> 32;
	  uint64_t chunk_start = probe_adddress & 0xFFFF0000; // Get 32 high order bits
	  // Calculate memory requirements for the stubs to be allocated related to probe spaces,
	  // later during the JIT code generation phase
      mem_island* mem; // int counter=0;
	  if (mem_allocations.find(mem_chunk) == mem_allocations.end()) {
		  list<mem_island*>* mem_list = new list<mem_island*>;
		  mem = new mem_island;
		  mem->start_addr = (unsigned long*)((chunk_start + CHUNK_SIZE) / 2); // We initially set this to the middle of the 2^32 chunk
		  mem->size = STUB_SIZE;
		  mem->mem_chunk = mem_chunk;

		  mem_list->push_back(mem);
		  mem_allocations.insert(make_pair(mem_chunk, mem_list));
		  // counter += 1;
	  } else {
		  // counter += 1;
		  list<mem_island*>* mem_list = mem_allocations.find(mem_chunk)->second;
		  mem = mem_list->front(); // At this stage we only have one memory island in the list
		  if (mem != NULL) {
			  mem->size += STUB_SIZE; // Another stub for this memory island
		  } else {
			  // log error. This shouldn't happen
		  }
	  }

	  // printf("Mem chunk : %lu   Counter : %d\n", mem_chunk, counter);
	  // Move to next row
	  row = (zca_row_11_t*) ((byte*) row + sizeof(*row));
	}

      // End the loop (if we only need this section)
      break;
    }
  }

  // printf("Number of processors :%d", get_nprocs());

  // elf_end(e);
  close(fd);
  
  return probe_count;
  // dbgprint("\n [read-zca] Returning out table %p\n", out_table);
  // return out_table;
}
// ^^ This code is taken from:
// http://stackoverflow.com/questions/12159595/how-to-get-a-pointer-to-an-specific-section-of-a-program-from-within-itself-ma



/** Read the probes available in the CURRENT process.
 *
 * This attempts to locate the currently executing program on disk and
 * read its ELF headers, looking for a compiler-inserted section
 * containing a ZCA table.
 */
int read_self_zca_probes()
{

	char path[MAXPATHLEN];
	get_working_path(path);
	return read_zca_probes(path);

}

/** Return the current working path or an empty string upon failure.
 *
 */
void get_working_path(char* buf)
{
	getcwd(buf, MAXPATHLEN);
	strcat(buf, "/");
	strcat(buf, __progname);
	// return ( getcwd(temp, MAXPATHLEN) ? std::string( temp ) : std::string("") );
}

//* Temporary functions to test
// Probe function to test
void print_fn() {
  printf("[Successful] We are the borg\n");
}

void print_fn2() {
  printf("[Successful] Resistence is futile...\n");
}
