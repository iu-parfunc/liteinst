
#include <memory.h> 
#include "assembly.hpp"

#include <stdexcept>
#include <cassert>

namespace utils {
namespace assembly {

using std::invalid_argument;

int Assembler::emitInstruction(_DInst ins, Address target) {
  switch(ins.opcode) {
    case I_JMP:
    {
      int offset_size = 0;
      for (int i= 0; i < OPERANDS_NO; i++) {
        if (ins.ops[i].type == O_PC) {
          offset_size = ins.ops[i].size / 8;
          break;
        }
      }

      uint8_t* bytes;
      if (offset_size == 1) {
        bytes = new uint8_t[JMP_REL8_SZ]();
        bytes[0] = JMP_REL8_OPCODE; // JMP rel8 

        memcpy(target, bytes, JMP_REL8_SZ);
        delete[] bytes;

        return JMP_REL8_SZ;
      } else if(offset_size == 4) {
        bytes = new uint8_t[JMP_REL32_SZ]();
        bytes[0] = JMP_REL32_OPCODE; // JMP rel32
        
        memcpy(target, bytes, JMP_REL32_SZ);
        delete[] bytes;

        return JMP_REL32_SZ;
      } else {
        throw invalid_argument("[Assembler] Invalid offset size for JMP instruction..\n");
      }
      break;
    }
    default:
      throw invalid_argument("[Assembler] Unsupported instruction..\n");
  }

  return -1;
}

Code Assembler::assemble(Sequence seq) {
 // To be implemented
}

} /* End assembly */
} /* End utils */
