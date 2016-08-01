
#include <memory.h> 
#include "assembly.hpp"

#include <stdexcept>
#include <cassert>

namespace utils {
namespace assembly {

using std::pair;
using std::unordered_map;
using std::invalid_argument;

struct OpCode {
  uint32_t op_byte;  // 8 bit operand version
  uint32_t op_word;  // 16 bit operand version
  uint32_t op_dword; // 32 bit operand version
  uint32_t op_qword; // 64 bit operand version

  uint8_t op_byte_sz;    // 8 bit operand version op code size;
  uint8_t op_word_sz;    // 8 bit operand version op code size;
  uint8_t op_dword_sz;   // 8 bit operand version op code size;
  uint8_t op_qword_sz;   // 8 bit operand version op code size;
};

static unordered_map<uint16_t, OpCode> opcode_map;

int getOffsetSize(_DInst ins) {
  int offset_size = 0;
  for (int i= 0; i < OPERANDS_NO; i++) {
    if (ins.ops[i].type == O_PC) {
      offset_size = ins.ops[i].size / 8;
      break;
     }
  }
  return offset_size;
}

OpCode lookupOpcodeBytes(_DInst ins) {
  if (ins.opcode == I_JCXZ) {
    ins.opcode = I_JECXZ;
  }

  auto it = opcode_map.find(ins.opcode);
  if (it != opcode_map.end()) {
    return (*it).second;
  }

  assert(false);
}

int emitConditionalJump(_DInst ins, Address target) {
  int offset_size = getOffsetSize(ins);

  OpCode opcode = lookupOpcodeBytes(ins);
  uint8_t* bytes;
  if (offset_size == 1) { // Jcc rel8
    bytes = new uint8_t[Assembler::COND_JMP8_SZ]();
    // opcode is one byte when the relative offset is 8 bits
    memcpy(bytes, &opcode.op_byte, opcode.op_byte_sz);

    memcpy(target, bytes, Assembler::COND_JMP8_SZ);
    delete[] bytes;

    return Assembler::COND_JMP8_SZ;
  } else if (offset_size == 4) { // Jcc rel32
    bytes = new uint8_t[Assembler::COND_JMP32_SZ]();
    // opcode is two bytes when the relative offset is 32 bits
    memcpy(bytes, &opcode.op_dword, opcode.op_dword_sz);

    memcpy(target, bytes, Assembler::COND_JMP32_SZ);
    delete[] bytes;

    return Assembler::COND_JMP32_SZ;
  } else {
    throw invalid_argument("[Assembler] Invalid offset size for a conditional " 
        "jmp  instruction..\n");
  }
}

int emitJump(_DInst ins, Address target) {
  int offset_size = getOffsetSize(ins);

  OpCode opcode = lookupOpcodeBytes(ins);
  uint8_t* bytes;
  if (offset_size == 1) { // JMP rel8
    bytes = new uint8_t[Assembler::JMP_REL8_SZ]();
    memcpy(bytes, &opcode.op_byte, opcode.op_byte_sz);

    memcpy(target, bytes, Assembler::JMP_REL8_SZ);
    delete[] bytes;

    return Assembler::JMP_REL8_SZ;
  } else if(offset_size == 4) { // JMP rel32
    bytes = new uint8_t[Assembler::JMP_REL32_SZ]();
    memcpy(bytes, &opcode.op_dword, opcode.op_dword_sz);
        
    memcpy(target, bytes, Assembler::JMP_REL32_SZ);
    delete[] bytes;

    return Assembler::JMP_REL32_SZ;
  } else {
    throw invalid_argument("[Assembler] Invalid offset size for JMP instruction..\n");
  }
}

int Assembler::emitInstruction(_DInst ins, Address target) {
  switch(ins.opcode) {
    case I_JMP:
      return emitJump(ins, target);
    case I_JA:
    case I_JAE:
    case I_JB:
    case I_JBE:
    case I_JCXZ:
    case I_JECXZ:
    case I_JG:
    case I_JGE:
    case I_JL:
    case I_JLE:
    case I_JNO:
    case I_JNP:
    case I_JNS:
    case I_JNZ:
    case I_JO:
    case I_JP:
    case I_JRCXZ:
    case I_JS:
    case I_JZ:
    case I_LOOP:
    case I_LOOPZ:
    case I_LOOPNZ:
      return emitConditionalJump(ins, target);
    default:
      throw invalid_argument("[Assembler] Unsupported instruction..\n");
  }

  return -1;
}

Code Assembler::assemble(Sequence seq) {
 // To be implemented
}

Assembler::Assembler() {
  opcode_map[I_JA]     = {0x77, 0x0, 0x0f87, 0x0, 1, 0, 2, 0};
  opcode_map[I_JAE]    = {0x73, 0x0, 0x0f83, 0x0, 1, 0, 2, 0};
  opcode_map[I_JB]     = {0x72, 0x0, 0x0f82, 0x0, 1, 0, 2, 0};
  opcode_map[I_JBE]    = {0x76, 0x0, 0x0f86, 0x0, 1, 0, 2, 0};
  opcode_map[I_JECXZ]  = {0xe3};
  opcode_map[I_JG]     = {0x7f, 0x0, 0x0f8f, 0x0, 1, 0, 2, 0};
  opcode_map[I_JGE]    = {0x7d, 0x0, 0x0f8d, 0x0, 1, 0, 2, 0};
  opcode_map[I_JL]     = {0x7c, 0x0, 0x0f8c, 0x0, 1, 0, 2, 0};
  opcode_map[I_JLE]    = {0x7e, 0x0, 0x0f8e, 0x0, 1, 0, 2, 0};
  opcode_map[I_JNO]    = {0x71, 0x0, 0x0f81, 0x0, 1, 0, 2, 0};
  opcode_map[I_JNP]    = {0x7b, 0x0, 0x0f8b, 0x0, 1, 0, 2, 0};
  opcode_map[I_JNS]    = {0x79, 0x0, 0x0f89, 0x0, 1, 0, 2, 0};
  opcode_map[I_JNZ]    = {0x75, 0x0, 0x0f85, 0x0, 1, 0, 2, 0};
  opcode_map[I_JO]     = {0x70, 0x0, 0x0f80, 0x0, 1, 0, 2, 0};
  opcode_map[I_JP]     = {0x7a, 0x0, 0x0f8a, 0x0, 1, 0, 2, 0};
  // opcode_map[I_JRCXZ]  = {0x77, 0x0f87};
  opcode_map[I_JS]     = {0x78, 0x0, 0x0f88, 0x0, 1, 0, 2, 0};
  opcode_map[I_JZ]     = {0x74, 0x0, 0x0f84, 0x0, 1, 0, 2, 0};
  opcode_map[I_JMP]    = {0xeb, 0x0, 0xe9, 0x0, 1, 0, 1, 0};

  /*
  opcode_map.emplace(I_JAE,   0x73);
  opcode_map.emplace(I_JB,    0x72);
  opcode_map.emplace(I_JBE,   0x76);
  opcode_map.emplace(I_JCXZ,  0xe3);
  opcode_map.emplace(I_JECXZ, 0xe3);
  opcode_map.emplace(I_JG,    0x74);
  opcode_map.emplace(I_JGE,   0x7d);
  opcode_map.emplace(I_JL,    0x7c);
  opcode_map.emplace(I_JLE,   0x7e);
  opcode_map.emplace(I_JNO,   0x71);
  opcode_map.emplace(I_JNP,   0x7b);
  opcode_map.emplace(I_JNS,   0x79);
  opcode_map.emplace(I_JNZ,   0x75);
  */
}

Assembler::~Assembler() {

}

} /* End assembly */
} /* End utils */
