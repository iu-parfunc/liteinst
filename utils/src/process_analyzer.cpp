
#include "analysis.hpp"
#include "elf64.h"
#include "sym_tab.h"
#include "elf64_shr.h"

#include <unistd.h>    // readlink
#include <sys/param.h> // MAXPATHLEN
#include <assert.h>

#include <cstring>
#include <cstdio>
#include <algorithm>
#include <iostream>
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

/*
struct Foo {
  long a;
  long b;

  ~Foo() {
   printf("FOO DESTROYED!!\n");
  }
};

struct Bar {
 static std::map<int, Function*> foo_by_id;
 static std::map<std::string, std::unique_ptr<Function>> foo_by_name;
};

std::map<int, Function*> Bar::foo_by_id;
std::map<std::string, std::unique_ptr<Function>> Bar::foo_by_name;

void boo() {
  for (int i=0; i<1000; i++) {
    //Function* f = new Function;
    Function * f = new Function;
    f->start = (Address)10;
    f->end = (Address) 20;

    std::string s = std::to_string(i);
    Bar::foo_by_name.insert(std::pair<std::string,std::unique_ptr<Function>>(s,
      std::move(std::unique_ptr<Function>(f))));

    Address old_a = f->start;
    Address old_b = f->end;
    // std::cout << "Before - f->a : " << (long)f->start << " f->b : " << (long) f->end << "\n";
    // printf("Before - f->start : %ld f->end : %ld\n", f->start, f->end);

    Bar::foo_by_id.insert(std::pair<int, Function*>(i, f));

    Address new_a = f->start;
    Address new_b = f->end;
    // printf("Before - f->start : %ld f->end : %ld\n", f->start, f->end);

    auto it = Bar::foo_by_name.find(s);

    assert(it != Bar::foo_by_name.end());
    // printf("Name : %s Start : %p End %p\n", it->second->name, it->second->start, it->second->end);
    
    assert(old_a == new_a && old_b == new_b);
  }

  for(int i = 0 ; i < 100; i++){
     Function* f = Bar::foo_by_id[i];
     printf("Before - f->start : %ld f->end : %ld\n", f->start, f->end);
  }
}

*/

void ProcessAnalyzer::populateFunctions(FunctionsByAddress& fn_by_addr,
    FunctionsByName& fn_by_name) {

  /*
  printf("Calling BOO!\n");
  boo();
  printf("Called BOO!\n");
  */

  ELF *bin = elf64_read((char*)getProgramPath().c_str());
  unsigned int nb_sym = bin->symtab_num;
  Elf64_Sym **tab = bin->symtab;
  Elf64_Half strtab_idx = get_section_idx(bin, ".strtab");
  Elf64_Off strtab_offset = bin->shr[strtab_idx]->sh_offset;

  for (unsigned int i=0; i < nb_sym; i++) {
    if ((tab[i]->st_info & 0x0F) == STT_FUNC) {
      string name = string(get_sym_name(bin->file, tab[i], strtab_offset));

      fn_by_name.insert(pair<string, unique_ptr<Function>>(
        name, unique_ptr<Function>(new Function())));
      auto it = fn_by_name.find(name);

      assert(it != fn_by_name.end());

      Function* fn = it->second.get();
      fn->start = (Address) tab[i]->st_value;
      fn->end = (Address) fn->start + tab[i]->st_size;
      fn->name = name;

      fn_by_addr.insert(pair<Address, Function*>(fn->start, fn));
    }
  }

  fclose(bin->file);
  free(bin);
}

void ProcessAnalyzer::populateMappedRegions(MappedRegionsByAddress* mapped) {
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
    mapped->insert(pair<Address, unique_ptr<MappedRegion>>(mr->start, move(mr_ptr)));

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
