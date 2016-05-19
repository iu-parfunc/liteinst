
#include "analysis.hpp"
#include "elf64.h"
#include "sym_tab.h"
#include "elf64_shr.h"

#include <unistd.h>    // readlink
#include <sys/param.h> // MAXPATHLEN

#include <cstring>
#include <cstdio>
#include <algorithm>
#include <cstdint>

namespace fastinst {
namespace rprobes {

using std::map;
using std::pair;
using std::string;
using std::sort;

std::map<Address, Function> ProcessAnalyzer::getFunctions() {
  ELF *bin = elf64_read((char*)getProgramPath().c_str());
  unsigned int nb_sym = bin->symtab_num;
  Elf64_Sym **tab = bin->symtab;
  Elf64_Half strtab_idx = get_section_idx(bin, ".strtab");
  Elf64_Off strtab_offset = bin->shr[strtab_idx]->sh_offset;

  map<Address, Function> fns;

  for (unsigned int i=0; i < nb_sym; i++) {
    if ((tab[i]->st_info & 0x0F) == STT_FUNC) {
      char* s_name = get_sym_name(bin->file, tab[i], strtab_offset);

      Function fn;
      fn.start = (Address) tab[i]->st_value;
      fn.end = (Address) fn.start + tab[i]->st_size;
      fn.name = string(s_name);

      fns.insert(pair<Address, Function>(fn.start, fn));
    }
  }

  fclose(bin->file);
  free(bin);
  return fns;
}

std::map<Address, MappedRegion> ProcessAnalyzer::getMappedRegions() {
  unsigned long long addr, endaddr, offset, inode;
  char permissions[8], device[8];
  char* filename = (char*) malloc(sizeof(char) * MAXPATHLEN);

  FILE* fp;
  if ((fp = fopen ("/proc/self/maps", "r")) == NULL) {
    fprintf(stderr, "Could not open %s\n", "/proc/self/maps");
  }

  char* line = NULL;
  size_t len = 0;
  ssize_t read;
  map<Address, MappedRegion> regions;
  while ((read = getline(&line, &len, fp)) != -1) {
    MappedRegion mr;
    sscanf (line,  "%llx-%llx %s %llx %s %llx %s", 
        &addr, &endaddr, permissions, &offset, device, &inode, filename);
    mr.start = (Address) addr;
    mr.end = (Address) endaddr;
    mr.file = string(filename);
    regions.insert(pair<Address, MappedRegion>(mr.start, mr));
  }

  free(line);

  return regions;
}

string ProcessAnalyzer::getProgramPath() {
  if (program_path.empty()) {
    char* c_program_path = (char*) malloc(sizeof(char) * MAXPATHLEN);
    ssize_t len = 0;
    if (c_program_path != NULL) {
      if ((len = readlink("/proc/self/exe", c_program_path, 
              sizeof(char) * MAXPATHLEN)) == -1) {
        free(c_program_path);
        return NULL;
      }
    }
    c_program_path[len] = '\0';
    program_path = string(c_program_path);
  }
  return program_path;
}

} // End rprobes
} // End fastinst
