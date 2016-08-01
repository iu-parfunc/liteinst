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
     *  /param start   Start address of the region
     *  /param end     End address of the region (exclusive)
     *  /param target  Relocation buffer
     *  /return        Relocation information 
     */
    Relocations relocate(utils::Address start, utils::Address end,
        utils::Address target);

    /** /brief Gets the size of the relocated code for the given target address
     *
     *  /param start   Start address of the region
     *  /param end     End address of the region (exclusive)
     *  /param target  Relocation buffer address
     *  /return        Size of the relocated code
     *
     *  This does not relocate the code. Just gives the size of the relocated 
     *  code if it were to be relocated to the given target address. The
     *  relocated code may occupy more space than the original code due to 
     *  instruction transformations which may happen during the relocation. 
     *  This method is useful to get the size of the relocation buffer prior to
     *  its allocation by the caller of this API. The target could be null in 
     *  which case this would return a conservative result which would consider
     *  all transformations due to unrechability of relative addressing based 
     *  instructions will happen.
     */
    int64_t getRelocationSize(utils::Address start, utils::Address end, 
        utils::Address target);

};

} // End liteprobes
} // End liteinst

#endif /* _RELOCATOR_HPP_ */
