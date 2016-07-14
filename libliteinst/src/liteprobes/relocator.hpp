#ifndef _RELOCATOR_HPP_
#define _RELOCATOR_HPP_

#include "liteprobes.hpp"
#include "defs.hpp"

namespace liteinst {
namespace liteprobes {

class Relocations {
  public:
    int n_instructions; ///< Number of instructions relocated
    int* relocation_offsets; ///< Offsets of the start address of each 
                             ///< relocated instruction from the start of the
                             ///< relocation buffer
};
  
class Relocator {
  public:

    /** /brief Relocates the region of code from start to end (exclusive) to 
     *    target address      
     *  /param start Start address of the region
     *  /param end   End address of the region (exclusive)
     *  /return      Relocation information 
     */
    Relocations relocate(utils::Address start, utils::Address end,
        utils::Address target);

};

} // End liteprobes
} // End liteinst

#endif /* _RELOCATOR_HPP_ */
