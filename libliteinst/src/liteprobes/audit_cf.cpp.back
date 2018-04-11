
#include <iostream>
#include <cstdio>
#include <string>
#include <algorithm>
#include <cstdint>
#include <vector>
#include <set>
#include <map>
#include "liteinst.hpp"
#include "process.hpp"
#include "assembly.hpp"
#include "control_flow_router.hpp"
#include "signals.hpp"

namespace liteinst {
namespace liteprobes {

using namespace std;
using namespace utils::signals;

uint8_t get_register_index(uint8_t reg) {
  switch (reg) {
    case R_RAX: case R_EAX: case R_AX: case R_AL: 
      return 0;
    case R_RCX: case R_ECX: case R_CX: case R_CL:
      return 1;
    case R_RDX: case R_EDX: case R_DX: case R_DL:
      return 2;
    case R_RBX: case R_EBX: case R_BX: case R_BL:
      return 3;
    case R_RSP: case R_ESP: case R_SP: case R_AH:
      return 4;
    case R_RBP: case R_EBP: case R_BP: case R_CH:
      return 5;
    case R_RSI: case R_ESI: case R_SI: case R_DH:
      return 6;
    case R_RDI: case R_EDI: case R_DI: case R_BH:
      return 7;
    case R_R8: case R_R8D: case R_R8W: case R_R8B:
      return 8;
    case R_R9: case R_R9D: case R_R9W: case R_R9B:
      return 9;
    case R_R10: case R_R10D: case R_R10W: case R_R10B:
      return 10;
    case R_R11: case R_R11D: case R_R11W: case R_R11B:
      return 11;
    case R_R12: case R_R12D: case R_R12W: case R_R12B:
      return 12;
    case R_R13: case R_R13D: case R_R13W: case R_R13B:
      return 13;
    case R_R14: case R_R14D: case R_R14W: case R_R14B:
      return 14;
    case R_R15: case R_R15D: case R_R15W: case R_R15B:
      return 15;
    default:
      return reg;
  }
}

string get_register_name(uint8_t reg) {
  switch (reg) {
    case R_RAX: case R_EAX: case R_AX: case R_AL: 
      return "RAX";
    case R_RCX: case R_ECX: case R_CX: case R_CL:
      return "RCX";
    case R_RDX: case R_EDX: case R_DX: case R_DL:
      return "RDX";
    case R_RBX: case R_EBX: case R_BX: case R_BL:
      return "RBX";
    case R_RSP: case R_ESP: case R_SP: case R_AH:
      return "RSP";
    case R_RBP: case R_EBP: case R_BP: case R_CH:
      return "RBP";
    case R_RSI: case R_ESI: case R_SI: case R_DH:
      return "RSI";
    case R_RDI: case R_EDI: case R_DI: case R_BH:
      return "RDI";
    case R_R8: case R_R8D: case R_R8W: case R_R8B:
      return "R8";
    case R_R9: case R_R9D: case R_R9W: case R_R9B:
      return "R9";
    case R_R10: case R_R10D: case R_R10W: case R_R10B:
      return "R10";
    case R_R11: case R_R11D: case R_R11W: case R_R11B:
      return "R11";
    case R_R12: case R_R12D: case R_R12W: case R_R12B:
      return "R12";
    case R_R13: case R_R13D: case R_R13W: case R_R13B:
      return "R13";
    case R_R14: case R_R14D: case R_R14W: case R_R14B:
      return "R14";
    case R_R15: case R_R15D: case R_R15W: case R_R15B:
      return "R15";
    default:
      return string((const char*)((const _WString*)&_REGISTERS[reg])->p);
  }
}

void premain() {

  using namespace utils::process;
  using namespace utils::assembly;
  using namespace std;

  long n_indirect_jumps = 0;
  long n_indirect_calls = 0;
  long n_instructions   = 0;
  long n_returns        = 0;

  Process p;
  vector<utils::process::Function*> fns = p.getFunctions();
  Disassembler disas;

  set<uint8_t> all_regs;
  for (uint8_t i=0; i < R_DR7; i++) {
    if (i < 15 || i > 60) {
      all_regs.insert(i);
    }
  }

  printf("\nALL : \n");
  for (auto const& u: all_regs) {
      printf("%s ", get_register_name(u).c_str());
  }

  map<string, set<uint8_t>> fn_ins;

  for (utils::process::Function* fn : fns) {
    const Sequence* seq = disas.disassemble(fn->start, fn->end);
    _DInst *decoded = static_cast<_DInst*>(seq->instructions);

    set<uint8_t> ins;  
    for (int i=0; i < seq->n_instructions; i++) {
      _DInst* di = &decoded[i];

      if (disas.isBranch(*di)) {
  	    for (int j = 0; ((j < OPERANDS_NO) && (di->ops[j].type != O_NONE)); j++) {
  		    switch (di->ops[j].type) {
  			    case O_REG:
  			    case O_SMEM:
			      case O_MEM:
              n_indirect_jumps++;
              break;
            default:
              continue;
          }
        }
      } else if (disas.isNearCall(*di)) {
  	    for (int j = 0; ((j < OPERANDS_NO) && (di->ops[j].type != O_NONE)); j++) {
  		    switch (di->ops[j].type) {
  			    case O_REG:
  			    case O_SMEM:
			      case O_MEM:
              n_indirect_calls++;
              break;
            default:
              continue;
          }
        }
      } else if (di->opcode == I_RET) {
        n_returns++;
      }
    }

    n_instructions += seq->n_instructions;
  }

  printf("\n Statistics : \n\n");
  printf("   Indirect Calls : %ld\n", n_indirect_calls); 
  printf("   Indirect Jumps : %ld\n", n_indirect_jumps); 
  printf("   Returns        : %ld\n", n_returns);
  printf("   Indirect Call % : %ld\n", (n_indirect_calls/ n_instructions)*100); 
  printf("   Indirect Jump % : %ld\n", (n_indirect_jumps/ n_instructions)*100); 
  printf("   Returns       % : %ld\n", (n_returns / n_instructions)*100);
  printf("   Total indirect control flow : %ld\n", 
      n_indirect_calls + n_indirect_jumps + n_returns);
  printf("   Total indirect control flow % : %ld\n",
      ((n_indirect_calls + n_indirect_jumps + n_returns) / n_instructions) *100);

}

}
}
