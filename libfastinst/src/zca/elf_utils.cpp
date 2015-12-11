
#include "zca_probe_provider.hpp"
#include "elf_utils.hpp"
#include "zca_types.hpp"

#include <stdio.h>
#include <stdlib.h> // EXIT_FAILURE
#include <fcntl.h>  // O_RDONLY
#include <string.h>
#include <err.h>     // err
#include <unistd.h>  // getcwd
#include <libelf.h>

#include <sys/param.h> // MAXPATHLEN
#include <string>

namespace ElfUtils {

  using namespace std;

  mem_alloc_table mem_allocations;

  ProbeVec* readZCAELFMetaData() {
    // Get current directory
    char patch[MAXPATHLEN];
    getcwd(buf, MAXPATHLEN);
    strcat(buf, "/");
    strcat(buf, __progname);

  	int fd;       // File descriptor for the executable ELF file
  	char *section_name;
  	size_t shstrndx;
  	zca_row_11_t* rows;

  	int notify= 0;
  	int probe_count = 0;     // Number of probes discovered

  	Elf *e;                  // ELF struct
  	Elf_Scn *scn = NULL;     // Section index struct
  	Elf_Data *data = NULL;   // Section index struct
  	Elf64_Shdr *shdr;        // Section strkkkuct

  	if(elf_version(EV_CURRENT)==EV_NONE)
  		errx(EXIT_FAILURE, "ELF library iinitialization failed: %s", elf_errmsg(-1));

  	if((fd = open(path, O_RDONLY, 0))<0)
  		err(EXIT_FAILURE, "open file \"%s\" failed", path);

  	if((e = elf_begin(fd, ELF_C_READ, NULL))==NULL)
  		errx(EXIT_FAILURE, "elf_begin() failed: %s.", elf_errmsg(-1));

  	// Retrieve the section index of the ELF section containing the string table 
    // of section names
  	if(elf_getshdrstrndx(e, &shstrndx)!=0)
  		errx(EXIT_FAILURE, "elf_getshdrstrndx() failed: %s.", elf_errmsg(-1));

    uint64_t probe_start;
    uint64_t probe_end;

	  // Loop over all sections in the ELF object
	  while((scn = elf_nextscn(e, scn))!=NULL) {
	  	// Given a Elf Scn pointer, retrieve the associated section header
	  	if((shdr = elf64_getshdr(scn))!=shdr)
	  		errx(EXIT_FAILURE, "getshdr() failed: %s.", elf_errmsg(-1));

	  	// Retrieve the name of the section name
	  	if((section_name = elf_strptr(e, shstrndx, shdr->sh_name))==NULL)
	  		errx(EXIT_FAILURE, "elf_strptr() failed: %s.", elf_errmsg(-1));

	  	// If the section is the one we want... (in my case, it is one of the main
      // file sections)
	  	if(!strcmp(section_name, ".itt_notify_tab")) {

	  	  char* ptr;
        while ((data = elf_getdata(scn,data)) != NULL) {
		      ptr = (char*) data->d_buf;
		  	  long int offset = 0;

			    // We can use the section adress as a pointer, since it corresponds to 
          // the actual adress where the section is placed in the virtual memory
			    // struct data_t * section_data = (struct data_t *) shdr->sh_addr; 
          // This seems bogus!
			    fprintf(stderr, "\n [ZCA Probe Provider] got itt_notify... section " 
              "data is at addr %p, header at %p.\n", data, shdr);

			    zca_header_11_t* header;
			    // Cast section data
			    while (offset < data->d_size) {
				    // Loop until we get a valid zca notify table header until the end of 
            // this section
				    while(offset < data->d_size) {
				 	    header = (zca_header_11_t*)ptr;
					    char* str = (char*)header->magic;

					    if (!strcmp(str, ".itt_notify_tab")) {
					    	break;
					    }

					    ptr++;
			  		  offset++;
			  	  }

            if (offset >= data->d_size) {
              break;
            }

				    uint8_t* ver = (uint8_t*) & (header->version);

				    // Here we skip the header and move on to the actual rows:
				    zca_row_11_t* row = (zca_row_11_t*) ((byte*) header +
                sizeof(*header));

				    fprintf(stderr, " [ZCA Probe Provider] Found first row at %p, offset " 
                "%lu\n", row, ((long)row - (long)header));
				    fprintf(stderr, " [ZCA Probe Provider] Annotation entry count : %d\n", 
                header->entry_count);
				    fprintf(stderr, " [ZCA Probe Provider] Annotation table resides at :" 
                " %p\n", annotations);

				    ProbeVec pmdVec; 
            pmdVec.reserve(header->entry_count);

				    for (int i = 0; i < header->entry_count; i++) {
					    probe_count++;
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

              ZCAProbeMetaData* pmd = new ZCAProbeMetaData;
					    const char* str = getAnnotation(header, row);
              char* tok, char *suffix;
              char* temp = strdup(str);
              pmdVec[i]->probe_id = i; 
              pmdVec[i]->func_name = strtok_r(temp, ":", &tok);
						  pmdVec[i]->instrumentation_func = NULL; // TODO: This is set from profiler activate_function 
					    pmdVec[i]->expr = strdup(str);
              pmdVec[i]->probe_addr = row->anchor;
              suffix= strtok_r(NULL, ":", &tok);

              if (strcmp(suffix, "entry") == 0) {
                pmd[i]->probe_context = ProbeContext::ENTRY;
              } else if (strcmp(suffix, "exit") == 0) {
                pmd[i]->probe_context = ProbeContext::EXIT;
              } else {
                pmd[i]->probe_context = ProbeContext:OTHER;
              }

              /*
					    if (annotations.find(string(str)) == annotations.end()) {
					    	list<ann_data*>* ann_list = new list<ann_data*>;
					    	ann_list->push_back(&ann_info[i]);

					    	annotations.insert(ann_table::value_type(string(str), ann_list));
					    } else {
					    	list<ann_data*>* ann_list = annotations.find(string(str))->second;
					    	ann_list->push_back(&ann_info[i]);
					    }
              */

					    // annotations.insert(ann_table::value_type(string(str), &(ann_info[i])));

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

  				  ptr = (char*)((byte*) row + sizeof(*row));
  			  	offset = ptr - (char*)data->d_buf;
  		  	}
        } // END

			  // End the loop (if we only need this section)
			  // break;
		  }
	  }

	  // elf_end(e);
	  close(fd);

	  return probe_count;
  }

}
