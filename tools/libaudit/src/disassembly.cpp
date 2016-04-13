
#include <cstdlib>

#include "disassembly.hpp"

namespace disassembly {

  using namespace defs;

  Decoded disassembleRange(Address start, Address end) {
    uint64_t n_decode_bytes = (uint64_t)((uint64_t)end - (uint64_t)start);

    Decoded d;

    /* why * 2 * n_decode_bytes */
    _DInst* result = (_DInst*)malloc(sizeof(_DInst) * 2 * n_decode_bytes);
    unsigned int instruction_count = 0;

    _CodeInfo ci = {0}; /* another modernity */
    ci.code = (uint8_t*)start;
    ci.codeLen = n_decode_bytes;
    ci.dt = Decode64Bits;   /* here we make this AMD64 specific ? */
    ci.codeOffset = 0x100000;

    _DecodeResult res = distorm_decompose(&ci, result, n_decode_bytes, &instruction_count);
    if (res != DECRES_SUCCESS) {
      free(result);
      result = NULL;

      d.decoded_instructions = NULL;
      d.n_instructions = 0;

      return d;
    }

    d.decoded_instructions = result;
    d.n_instructions = instruction_count;
    return d;
  }

}
