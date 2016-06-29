
#ifndef ASSEMBLY_H
#define ASSEMBLY_H 

#include <cstdint>
#include <vector>

#include "utils.hpp"

namespace liteinst {

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
    virtual Sequence disassemble(utils::Address start, utils::Address end);

};

} // End liteinst 

#endif /*ASSEMBLY_H*/