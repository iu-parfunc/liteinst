
#ifndef _DISASSEMBLY_HPP_
#define _DISASSEMBLY_HPP_

#include <cstdint>

#include "defs.hpp"
#include "distorm.h"
#include "mnemonics.h" 

namespace disassembly {

  typedef struct { 
    _DInst *decoded_instructions; 
    unsigned int n_instructions; 
  } Decoded; 

  Decoded disassembleRange(Address start, Address end);

}

#endif /* _DISASSEMBLY_HPP_ */
