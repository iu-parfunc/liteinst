

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
#include "zca-types.h"
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

typedef unordered_map<string, pair<zca_row_11_t*, unsigned long*>> ann_table;

// ann_data* annotations;

// ann_table globalAnnTable;
// zca_header_11_t* globalZCATable;

#define dbgprint printf

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
			rows = (zca_row_11_t*) ((byte*) header + sizeof(*header));

			LOG_DEBUG(" [read-zca] found first row at %p, offset %lu\n", rows, ((long)rows - (long)header));

			// For now allocate memory for annotations as an array in the heap
			annotations = (ann_data*)malloc(sizeof(ann_data) * (header->entry_count));

			probe_count = header->entry_count;

			LOG_DEBUG("\n\nAnnotation entry count : %d\n", header->entry_count);
			LOG_DEBUG("Annotation table resides at : %p\n", annotations);

			for (int i = 0; i < header->entry_count; i++)
			{
				ann_data* annotation = &annotations[i];
				zca_row_11_t* row = (zca_row_11_t*)((byte*)rows + sizeof(*row) * i);

				annotation->ip = row->anchor;
				annotation->probespace = row->probespace;

				if (i%2 == 0) {
				    annotation->fun = &print_fn;
				} else {
					annotation->fun = &print_fn2;
				}
				annotation->expr = getAnnotation(header, row);

				//probe_count++;

				LOG_DEBUG("\n------------ Annotation [%d] --------------\n", i);
				LOG_DEBUG("annotation-ip : %lu\n", (unsigned char*)annotation->ip);
				LOG_DEBUG("annotation-probespace : %d\n", annotation->probespace);
				LOG_DEBUG("annotation-func : %p \n", (unsigned char*)annotation->fun);
				LOG_DEBUG("annotation-expr : %s \n\n", annotation->expr);

				/*
  	  dbgprint("%p\n", getIP(row));
  	  //	  ann_data ad = ann_data(NULL, NULL, getIP(row), getProbespace(row), getExpr(table, row));
          const char* str = getAnnotation(row);

  	  globalAnnTable.insert(ann_table::value_type(string(str), pair<zca_row_11_t*, unsigned long*>(row, nullptr)));
  	  // Move to next row
  	  row = (zca_row_11_t*) ((byte*) row + sizeof(*row));
				 */
			}

			//     /*
			//     cout << "  reg/offset: " << reg << ", " << offset << endl;
			//     cout << "  table " << table << ", row " << row << ", row byteoffset " << (byte*) row - (byte*) table << ", table_size "
			//     	   << sizeof(*table) << endl;
			//     cout << "  row "<< row << ": label \"" << str << "\", str offset " << (byte*) str - (byte*) table << ", strings "
			// 	   << table->strings << ", annotation " << row->annotation
			//          << ", probespace " << row->probespace
			//          << ", IP " << (void*)(row->anchor)
			//          << endl;

			//     ann_data* tstann = globalAnnTable["probe1"];
			//     dbgprint("Populating table, annotation 'probe1': struct is at addr %p, reading IP %p (from %p)\n",
			// 	     tstann, tstann->ip, &(tstann->ip));

			//     cout << "probespace: " << globalAnnTable["probe1"]->probespace << endl;
			//     cout << "expr: " << getExpr(table, row) << endl;
			//     */

			//     cout << "this is location of the zca row for 'probe1' obtained from the HT during populateHT(): " << globalAnnTable["probe1"].first << endl;

			// End the loop (if we only need this section)
			break;
		}
	}

	elf_end(e);
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
	LOG_DEBUG("[Successful] We are the borg\n");

}

void print_fn2() {
	LOG_DEBUG("[Successful] Resistence is futile...\n");
}
