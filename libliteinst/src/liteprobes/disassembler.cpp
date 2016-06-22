
#include "assembly.hpp"
#include "distorm.h"
#include "mnemonics.h"

namespace liteinst {

using utils::Address;

Sequence Disassembler::disassemble(Address start, Address end) {
  uint64_t n_decode_bytes = (uint64_t)((uint64_t)end - (uint64_t)start);

  Sequence seq;

  // malloc(sizeof(_DInst) * n_decode_bytes);
  _DInst* result = new _DInst[n_decode_bytes]; 
  unsigned int instruction_count = 0;

  _CodeInfo ci = {0}; 
  ci.code = (uint8_t*)start;
  ci.codeLen = n_decode_bytes;
  ci.dt = Decode64Bits;   /* here we make this AMD64 specific ? : Yes. For now 
                             we are only concerned about 64 bit applications. */
  ci.codeOffset = 0x100000;

  _DecodeResult res = distorm_decompose(&ci, result, n_decode_bytes, 
      &instruction_count);
  if (res != DECRES_SUCCESS) {
    seq.n_instructions = 0;
    delete[] result;

    return seq;
  }

  seq.n_instructions = instruction_count;
  seq.instructions = static_cast<void*>(result);

  return seq;
}

} // End liteinst 
