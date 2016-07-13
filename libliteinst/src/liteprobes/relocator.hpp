#ifndef _RELOCATOR_HPP_
#define _RELOCATOR_HPP_

#include "liteprobes.hpp"
#include "defs.hpp"

namespace liteinst {
namespace liteprobes {

class Relocations {
  public:
    int n_instructions;
    int* relocation_offsets;
};
  
class Relocator {
  public:
    Relocations relocate(utils::Address start, utils::Address end,
        utils::Address target);

};

} // End liteprobes
} // End liteinst

#endif /* _RELOCATOR_HPP_ */
