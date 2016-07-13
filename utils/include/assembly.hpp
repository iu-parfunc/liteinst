
#ifndef ASSEMBLY_H
#define ASSEMBLY_H 

#include <cstdint>
#include <vector>

#include "defs.hpp"
#include "distorm.h"
#include "mnemonics.h"

namespace utils {
namespace assembly {

/// An encoded instruction sequence
class Code {
  public:
    int n_bytes;
    uint8_t* bytes;
};

/// A decoded sequence of instructions
class Sequence {
  public:
    int n_instructions;
    void* instructions;
    utils::Address start;
    utils::Address end;

    Sequence(){};

    ~Sequence(){ 
      free(instructions); 
    }
};

/// Encodes a given sequence of instructions
class Assembler {
  public:
    virtual Code assemble(Sequence seq);

};

/// Decodes a sequence of encoded instructions
class Disassembler {
  public:
    virtual const Sequence* disassemble(utils::Address start,
        utils::Address end);
    virtual int findInstructionIndex(utils::Address instruction, 
        const Sequence* seq);

};

} // End assembly
} // End utils

#endif /*ASSEMBLY_H*/
