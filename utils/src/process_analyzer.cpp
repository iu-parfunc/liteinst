
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

namespace utils {
namespace process {

using std::map;
using std::pair;
using std::string;
using std::unique_ptr;
using std::shared_ptr;
using std::weak_ptr;
using std::sort;
using utils::Address;

void ProcessAnalyzer::populateFunctions(FunctionsByAddress& fn_by_addr,
    FunctionsByName& fn_by_name) {
  ELF *bin = elf64_read((char*)getProgramPath().c_str());
  unsigned int nb_sym = bin->symtab_num;
  Elf64_Sym **tab = bin->symtab;
  Elf64_Half strtab_idx = get_section_idx(bin, ".strtab");
  Elf64_Off strtab_offset = bin->shr[strtab_idx]->sh_offset;

  for (unsigned int i=0; i < nb_sym; i++) {
    if ((tab[i]->st_info & 0x0F) == STT_FUNC) {
      char* s_name = get_sym_name(bin->file, tab[i], strtab_offset);

      Function* fn = new Function();
      fn->start = (Address) tab[i]->st_value;
      fn->end = (Address) fn->start + tab[i]->st_size;
      fn->name = string(s_name);

      unique_ptr<Function> fn_ptr(fn);
      fn_by_name.insert(pair<string, unique_ptr<Function>>(fn->name, move(fn_ptr)));
      // range_map.insert(pair<Range, unique_ptr<SpinLock>>(r, move(lock)));

      // fn_by_name.emplace(fn->name, unique_ptr<Function>(fn));
      fn_by_addr.emplace(fn->start, fn);
    }
  }

  fclose(bin->file);
  free(bin);
}

void ProcessAnalyzer::populateMappedRegions(MappedRegionsByAddress& mapped) {
  unsigned long long addr, endaddr, offset, inode;
  char permissions[8], device[8];
  char* filename = new char[MAXPATHLEN];

  FILE* fp;
  if ((fp = fopen ("/proc/self/maps", "r")) == NULL) {
    fprintf(stderr, "Could not open %s\n", "/proc/self/maps");
  }

  char* line = NULL;
  size_t len = 0;
  ssize_t read;
  while ((read = getline(&line, &len, fp)) != -1) {
    MappedRegion* mr = new MappedRegion;
    sscanf (line,  "%llx-%llx %s %llx %s %llx %s", 
        &addr, &endaddr, permissions, &offset, device, &inode, filename);
    mr->start = (Address) addr;
    mr->end = (Address) endaddr;
    mr->file = string(filename);

    unique_ptr<MappedRegion> mr_ptr(mr);
    mapped.insert(pair<Address, unique_ptr<MappedRegion>>(mr->start, move(mr_ptr)));

    // mapped.emplace(mr->start, unique_ptr<MappedRegion>(mr));
  }

  delete[] filename;
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

} // End process 
} // End utils 
