
#include "assembly.hpp"

#include <cassert>

namespace utils {
namespace assembly {

using utils::Address;

inline bool isCall(_DInst i) {
  return (i.opcode == I_CALL || i.opcode == I_CALL_FAR);
}

inline bool isRelativeJump(_DInst i) {
  return (i.opcode == I_JA || i.opcode == I_JAE || 
    i.opcode == I_JB || i.opcode == I_JBE || i.opcode == I_JCXZ || 
    i.opcode == I_JECXZ || i.opcode == I_JG || i.opcode == I_JGE || 
    i.opcode == I_JL || i.opcode == I_JLE || i.opcode == I_JMP || 
    i.opcode == I_JNO || i.opcode == I_JNP || 
    i.opcode == I_JNS || i.opcode == I_JNZ || i.opcode == I_JO || 
    i.opcode == I_JP || i.opcode == I_JRCXZ || i.opcode == I_JS || 
    i.opcode == I_JZ);
}

inline bool isFarJump(_DInst i) {
  return (i.opcode == I_JMP_FAR);
}

inline bool isJump(_DInst i) {
  return isRelativeJump(i) || isFarJump(i);
}

inline bool isHalt(_DInst i) {
  return (i.opcode == I_HLT);
}

inline bool isReturn(_DInst i) {
 return (i.opcode == I_RET);
} 

inline bool isControlTransfer(_DInst i) {
  return isJump(i) || isReturn(i) || isHalt(i);
}

const Sequence* Disassembler::disassemble(Address start, Address end) {
  uint64_t n_decode_bytes = (uint64_t)((uint64_t)end - (uint64_t)start);

  Sequence* seq = new Sequence;

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
    seq->n_instructions = 0;
    delete[] result;

    return seq;
  }

  seq->n_instructions = instruction_count;
  seq->instructions = static_cast<void*>(result);
  seq->start = start;
  seq->end = end;

  return seq;
}

int Disassembler::findInstructionIndex(Address addr, const Sequence* seq) {
  _DInst* decoded = static_cast<_DInst*>(seq->instructions);
  Address ip = seq->start;
  int index = 0;
  while (ip < addr) {
    ip += decoded[index++].size;
  }

  assert(ip == addr);
  return index;
}


int Disassembler::isControlTransferInstruction(const Sequence* seq, int index) {
  _DInst* decoded = static_cast<_DInst*>(seq->instructions);
  _DInst i = decoded[index];

  return isControlTransfer(i);
}

  // malloc(sizeof(_DInst) * n_decode_bytes);

} // End assembly
} // End liteinst 
