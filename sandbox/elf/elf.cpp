
#include "elf64.h"
#include "sym_tab.h"
#include "elf64_shr.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>    // readlink
#include <sys/param.h> // MAXPATHLEN
#include <stdint.h>



typedef uint8_t* Address;

__attribute__((constructor))
void read_elf() {

  // char c_program_path[] = "h264ref";
  char* c_program_path = (char*) malloc(sizeof(char) * MAXPATHLEN);
  ssize_t len = 0;
  if (c_program_path != NULL) {
    if ((len = readlink("/proc/self/exe", c_program_path, 
            sizeof(char) * MAXPATHLEN)) == -1) {
      free(c_program_path);
      return;
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

      printf("%s %p %p\n", s_name, (Address) tab[i]->st_value,
          (Address) tab[i]->st_value + tab[i]->st_size);
    }
  }

  fclose(bin->file);
  free(bin);
}

int main() {
  read_elf();
}
