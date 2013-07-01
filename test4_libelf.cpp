
#include <stdio.h>
#include <iostream>
#include <cstddef>

#include <string.h>

#include <err.h>
#include <fcntl.h>
#include <libelf.h> 
#include <stdio.h>
#include <stdlib.h>
#include <sysexits.h>
#include <unistd.h>

using namespace std;

// A global variable which stores the executable file name
const char *__progname;

// This code is taken from:
// http://stackoverflow.com/questions/12159595/how-to-get-a-pointer-to-an-specific-section-of-a-program-from-within-itself-ma

void retrieve_data() {
  int fd;       // File descriptor for the executable ELF file
  char *section_name, path[256];
  size_t shstrndx;

  Elf *e;           // ELF struct
  Elf_Scn *scn;     // Section index struct
  Elf64_Shdr *shdr;     // Section struct

  // Create the full path of the executable
  getcwd(path, 255);
  strcat(path, "/");
  strcat(path, __progname);

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

      // Do whatever we want

      // End the loop (if we only need this section)
      break;
    }
  }

  elf_end(e);
  close(fd);
}


int main(int argc, char *argv[])
{
  int x = 5;
  __notify_intrinsic((void*)"entered region", (void*)&x);
  printf("[app] We are the borg.\n");
  __notify_intrinsic((void*)"exited", &x);

  printf("[app] Done.\n");

  // Instrumentation comes here 
  typedef unsigned char BYTE;
  FILE *file = NULL;
  FILE *out = NULL;
  BYTE *fileBuf;
  unsigned long fileSize;

  __progname = argv[0];

  printf("Calling retrieve_data ... \n");
  retrieve_data();
  printf("Done \n");
    
  // Need to open in r+b
  /*
  if ((file = fopen(argv[0], "rb")) == NULL)
    cout << "Could not open binary" << endl;
  else
    cout << "Binary opened successfully" << endl;

  if ((out = fopen("out.exe", "wb")) == NULL)
    cout << "Could not open output file" << endl;
  else
    cout << "Output file opened successfully" << endl;
  return 0;
  
  fclose(file);
  fclose(out);

  delete[] fileBuf;
  */    
}

