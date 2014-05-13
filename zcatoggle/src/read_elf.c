#include <stdio.h>
#include <stdlib.h> // EXIT_FAILURE
#include <fcntl.h>  // O_RDONL
#include <string.h>
#include <err.h>     // err
#include <unistd.h>  // getcwd
#include <libelf.h>

#include "logger.h"   // LOG_DEBUG
#include "zca-types.hpp"

int main(int argc, const char*argv[]) {

	const char* path = argv[1];

	printf("===========================\n");
	printf("Reading file %s\n", path);

	int fd;       // File descriptor for the executable ELF file
	char *section_name;
	size_t shstrndx;
	zca_row_11_t* rows;

	int notify= 0;
	int probe_count = 0; // Number of probes discovered

	Elf *e;           // ELF struct
	Elf_Scn *scn;     // Section index struct
	Elf_Data *data = NULL;     // Section index struct
	Elf64_Shdr *shdr;  // Section strkkkuct

	int n=0;

	if(elf_version(EV_CURRENT)==EV_NONE)
		errx(EXIT_FAILURE, "ELF library initialization failed: %s", elf_errmsg(-1));

	if((fd = open(path, O_RDONLY, 0))<0)
		err(EXIT_FAILURE, "open file \"%s\" failed", path);

	if((e = elf_begin(fd, ELF_C_READ, NULL))==NULL)
		errx(EXIT_FAILURE, "elf_begin() failed: %s.", elf_errmsg(-1));

	// Retrieve the section index of the ELF section containing the string table of section names
	if(elf_getshdrstrndx(e, &shstrndx)!=0)
		errx(EXIT_FAILURE, "elf_getshdrstrndx() failed: %s.", elf_errmsg(-1));

	scn = NULL;

	int sec_count = 0;
	// Loop over all sections in the ELF object
	while((scn = elf_nextscn(e, scn))!=NULL) {
		sec_count++;
		// Given a Elf Scn pointer, retrieve the associated section header
		if((shdr = elf64_getshdr(scn))!=shdr)
			errx(EXIT_FAILURE, "getshdr() failed: %s.", elf_errmsg(-1));

		// Retrieve the name of the section name
		if((section_name = elf_strptr(e, shstrndx, shdr->sh_name))==NULL)
			errx(EXIT_FAILURE, "elf_strptr() failed: %s.", elf_errmsg(-1));

		// If the section is the one we want... (in my case, it is one of the main file sections)
		if(!strcmp(section_name, ".itt_notify_tab")) {

			printf("Header size : %lu\n", shdr->sh_size);
			while (n < shdr->sh_size && ((data = elf_getdata(scn, data)) != NULL )) {

			char* ptr;
			// data = elf_getdata(scn, data);
			ptr = (char*) data->d_buf;
			long int offset = 0;

      printf("Data size : %lu\n", data->d_size);

			int tables = 0;
			while (offset <= data->d_size) {
				tables++;
				// n++;
				// printf("Data size : %lu\n", data->d_size);

				// We can use the section adress as a pointer, since it corresponds to the actual
				// adress where the section is placed in the virtual memory

				// Cast section data
				zca_header_11_t* header  = (zca_header_11_t*) ptr;
				while(offset <= data->d_size) {
					header = (zca_header_11_t*)ptr;
					char* str = (char*)header->magic;
					if (str == NULL) {
						printf ("NULL detected..\n");
					}

					if (!strcmp(str, ".itt_notify_tab")) {
						// table->magic[0] == '.' && table->magic[1] == 'i' &&
						// table->magic[2] == 't' && table->magic[3] == 't') {
						break;
					}

					ptr++;
					offset++;
				}

          if (offset > data->d_size) {
            break; 
          }

				uint8_t* ver = (uint8_t*) & (header->version);

				// Here we skip the header and move on to the actual rows:
				zca_row_11_t* row = (zca_row_11_t*) ((byte*) header + sizeof(*header));

				printf("===========================\n");
				printf("Annotation entry count : %d\n\n", header->entry_count);
				printf ("Starting offset is : %lu\n", offset);

				probe_count = header->entry_count;
				// printf("Probe count : %d\n", probe_count);

				for (int i = 0; i < header->entry_count; i++)
				{
					const char* str = getAnnotation(header, row);
					printf("Annotation %d : %s\n", i, str);
					row = (zca_row_11_t*) ((byte*) row + sizeof(*row));
				}

				// Get to the end of this table
				ptr = (char*)((byte*) row + sizeof(*row));
				offset = ptr - (char*)data->d_buf;

				printf ("Ending offset is : %lu\n", offset);
				printf("Current table count is : %d\n", tables);
				printf("Data buffer size is : %lu\n", data->d_size);
				printf("===========================\n");
			}
			// }
		}
	}

	printf("##########################\n\n");
	printf("Total section count : %d\n", sec_count);
}
