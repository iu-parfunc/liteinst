
#ifndef _ELF_HPP_
#define _ELF_HPP_

extern "C" {
#include "elf64.h"
#include "sym_tab.h"
#include "elf64_shr.h"
}

#include "defs.hpp"

#include <vector>

namespace elf { 

  class FunctionSymbolInfo {
    public:
      defs::Address start;
      defs::Address end;
      std::string name;

      // Custom comparator to sort using function start address
      bool operator < (const FunctionSymbolInfo& fsi) const {
        return (start < fsi.start);
      }
  };

  std::vector<FunctionSymbolInfo> readFunctionSymbols();

}

#endif /* _ELF_HPP_ */
