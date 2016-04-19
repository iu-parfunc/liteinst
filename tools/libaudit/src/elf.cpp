
#include "elf.hpp"
#include "utils.hpp"


#include <cstring>
#include <cstdio>
#include <string>
#include <algorithm>
#include <cstdint>

namespace elf {

  using std::string;
  using std::vector;
  using std::sort;

  using namespace defs;
  using namespace utils;

  // Get the path of the executable
  /*
  char* get_working_path(char* buf) {
    getcwd(buf, MAXPATHLEN);
    strcat(buf, "/");
    strcat(buf, __progname);
    return buf;
  }
  */

  /*
  char* getProgramPath() {
    char *path = (char*) malloc(MAXPATHLEN);
    if (path != NULL) {
      if (readlink("/proc/self/exe", path, MAXPATHLEN) == -1) {
        free(path);
        path = NULL;
      }
    }
    return path;
  }
  */

  vector<FunctionSymbolInfo> readFunctionSymbols() {

    ELF *bin = elf64_read(getProgramPath());
    unsigned int nb_sym = bin->symtab_num;
    Elf64_Sym **tab = bin->symtab;
    Elf64_Half strtab_idx = get_section_idx(bin, ".strtab");
    Elf64_Off strtab_offset = bin->shr[strtab_idx]->sh_offset;

    vector<FunctionSymbolInfo> fns;

    for (unsigned int i=0; i < nb_sym; i++) {
      if ((tab[i]->st_info & 0x0F) == STT_FUNC) {
        char* s_name = get_sym_name(bin->file, tab[i], strtab_offset);

        FunctionSymbolInfo fn;
        fn.start = (Address) tab[i]->st_value;
        fn.end = (Address) fn.start + tab[i]->st_size;
        fn.name = string(s_name);

        fns.push_back(fn);
      }
    }

    sort(fns.begin(), fns.end());

    fclose(bin->file);
    return fns;

  }

}
