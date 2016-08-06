
#include <stdexcept>
#include <vector>
#include <limits>
#include <assert.h>
#include <memory.h> 

#include "relocator.hpp"
#include "assembly.hpp" 
#include "addr_range.hpp"

#include "distorm.h"
#include "mnemonics.h" 

namespace liteinst { 
namespace liteprobes {

using namespace utils::assembly;
using namespace utils::range;

using std::vector;
using std::invalid_argument;
using utils::Address;

static const int MAXINT8 = std::numeric_limits<int8_t>::max();
static const int MININT8 = std::numeric_limits<int8_t>::min();
static const int MAXINT32 = std::numeric_limits<int32_t>::max();
static const int MININT32 = std::numeric_limits<int32_t>::min();

struct Rewrite {
  Address src;
  Address target;
  _DInst src_op;
  vector<_DInst> target_ops;
  int size;
  bool transformed;
  bool opnd_changed;
  int index;
};

int bytesNeeded(int64_t v) { 
    
  uint64_t vabs = abs(v); 
    
  uint64_t mask8 = 0xFFFFFFFF00000000; 
  uint64_t mask4 = 0x00000000FFFF0000; 
  uint64_t mask2 = 0x000000000000FF00; 
  uint64_t mask1 = 0x00000000000000FF; 
    
  if ( vabs & mask8 ) return 8; 
  if ( vabs & mask4 ) return 4; 
  if ( vabs & mask2 ) return 2; 
  if ( vabs & mask1 ) return 1; 
   
  return 0; 
}

Rewrite getRewriteRule(const _DInst& ins, Address address, 
    Range relocation_bounds) {
  
  Disassembler disas;
  if (disas.isConditionalBranch(ins) || disas.isShortJump(ins)) {
    int offset = ins.imm.addr;
    Address jmp_target = address + ins.size + offset;

    bool is_short_jump = false;
    int offset_size = 0;
    for (int i= 0; i < OPERANDS_NO; i++) {
      if (ins.ops[i].type == O_PC) {
        offset_size = ins.ops[i].size / 8;

        if (offset_size == 1) {
          is_short_jump = true;
        }
      }
    }

    if (is_short_jump) {
      if (!relocation_bounds.withinRange(jmp_target, Range::INCLUSIVE)) {
        Rewrite r;
        r.src = address;

        // Original instruction
        r.src_op = ins;

        _DInst short_jmp;
        short_jmp.opcode = I_JMP;

        _Operand opnd;
        opnd.size = 8; 
        opnd.type = O_PC;
        short_jmp.ops[0] = opnd;
        short_jmp.size = Assembler::JMP_REL8_SZ;

        r.target_ops.push_back(short_jmp);     

        // Then follow up with an unconditional jmp
        _DInst jmp;
        jmp.opcode = I_JMP;

        opnd.size = 32; 
        opnd.type = O_PC;
        jmp.ops[0] = opnd;
        jmp.size = Assembler::JMP_REL32_SZ;

        r.target_ops.push_back(jmp);     

        // Now rewrite the original with following instructions
        // Starts off with the original short JMP instruction
        r.target_ops.push_back(ins);

        int size = 0;
        for (_DInst op : r.target_ops) {
          size += op.size;
        }

        r.size = size;
        r.transformed = true;
        return r;
      }
    }
  } 

  Rewrite r;
  r.src = address;
  r.src_op = ins;
  r.transformed = false;
  r.size = ins.size;
  return r;
}

bool validateRewrite(const Sequence* seq, const vector<Rewrite>& rewrites,
    Rewrite& r) {
  Disassembler disas;
  Range relocation_bounds(seq->start, seq->end);

  if (disas.isConditionalBranch(r.src_op) || disas.isShortJump(r.src_op)) {
    // Already marked for transformation. Nothing to do here.
    if (r.transformed) {
      return false;
    }

    // Find relative offset size (can be either 8 bits or 32 bits)
    int offset_size = 0;
    for (int i= 0; i < OPERANDS_NO; i++) {
      if (r.src_op.ops[i].type == O_PC) {
        offset_size = r.src_op.ops[i].size / 8;
        break;
      }
    }

    assert(offset_size > 0);

    int32_t offset = r.src_op.imm.addr;
    Address jmp_target = r.src + r.src_op.size + offset;

    // If the offset size is one byte we might have to do a rewrite 
    if (offset_size == 1) {
      if (relocation_bounds.withinRange(jmp_target, Range::INCLUSIVE)) {
        int target_index = disas.findInstructionIndex(jmp_target, seq);
        int low = (target_index < r.index) ? target_index : r.index;
        int high = (target_index >= r.index) ? target_index : r.index;

        bool changed_in_middle = false;
        // Now let's find if any instructions got transformed between 
        // this instruction and it's target
        int32_t new_offset = 0;
        for (int i=low; i <= high; i++) {
          new_offset += rewrites[i].size;
          if (rewrites[i].transformed) {
            // If the jmp target is higher and if it changed as well we don't 
            // have a problem with it since that would not change the target
            // offset of the current instruction
            if (i == target_index && jmp_target > r.src) {
              continue;
            }
            changed_in_middle = true;
          }
        }

        if (r.src < jmp_target) {
          new_offset -= rewrites[target_index].size;
        } else {
          new_offset *= -1;
        }

        if (changed_in_middle) {
          assert(r.target_ops.size() == 0);
 
          if (new_offset >= MININT8 && new_offset < MAXINT8) {
            // Offset is still within -127 and 127. So we can still get away 
            // without transforming this short jump. We just need to change 
            // the offset
            r.opnd_changed = true;
          } else {
            // Out of luck. Now we have to rewrite this short jump.
            // Starts off with the original short JMP instruction
            r.target_ops.push_back(r.src_op);

            // Then follow up with an unconditional jmp
            _DInst jmp;
            jmp.opcode = I_JMP;

            _Operand opnd;
            opnd.size = 32; 
            opnd.type = O_PC;
            jmp.ops[0] = opnd;
            jmp.size = Assembler::JMP_REL32_SZ;

            r.target_ops.push_back(jmp);     

            int size = 0;
            for (_DInst op : r.target_ops) {
              size += op.size;
            }

            r.size = size;
            r.transformed = true;
            r.opnd_changed = false; // Just clear flag in case this jump was 
                                    // marked for an operand change earlier 
             return true;
          }
        }
      }
    } 
  }

  return false;
}

vector<Rewrite> rewriteSequence(const Sequence* seq, Address target) {
  _DInst *decoded = static_cast<_DInst*>(seq->instructions);
  vector<Rewrite> rewrites;
  rewrites.reserve(seq->n_instructions);

  Range relocation_bounds(seq->start, seq->end);

  // Do a first pass to get the rewrites. Don't write out things yet
  Address src_ip = seq->start;
  for (int i=0; i < seq->n_instructions; i++) {
    Rewrite r = getRewriteRule(decoded[i], src_ip, relocation_bounds); 
    r.index = i;
    rewrites.push_back(r);
    src_ip += decoded[i].size;
  }

  // Now keep transforming until it stabilizes without more transformations
  bool newly_transformed;
  do {
    newly_transformed = false; // reset
    for (Rewrite& r : rewrites) {
      newly_transformed |= validateRewrite(seq, rewrites, r);
    }
  } while (newly_transformed);

  // Do another pass to fix the relocated instruction addresses
  Address ip = target;
  for (Rewrite& r : rewrites) {
    r.target = ip;
    ip += r.size;
  }

  // Finally we can do the actual rewriting
  Assembler a;
  src_ip = seq->start;
  for (const Rewrite& r : rewrites) {
    Address target_ip = r.target;
    if (r.transformed) {
      for (_DInst target_op : r.target_ops) {
        int size = a.emitInstruction(target_op, target_ip);
        assert(size == target_op.size);
        target_ip += size;
      }
    } else {
      // Copy the original instruction unchanged
      memcpy(target_ip, src_ip, r.src_op.size);   
    }
    src_ip += r.src_op.size;
  }

  return rewrites;
}

bool fixupCall(const Sequence* seq, const Rewrite& r) {
  int offset_bits = r.src_op.ops[0].size; 
  int type = r.src_op.ops[0].type; 

  Address src = r.src;
  Address target = r.target; 

  Range relocation_bounds(seq->start, seq->end);
  int64_t reloc_distance = src - target;

  switch (type) {
    case O_PC: // Relative call
    {
      int64_t new_offset; 
      int32_t offset = r.src_op.imm.addr; 

      Address call_target = src + r.src_op.size + offset;

      if (!relocation_bounds.withinRange(call_target, Range::INCLUSIVE)) {
        // Compute new relative address.
        // Is this approach ok for "both direction" ? 
        new_offset = static_cast<int32_t>(reloc_distance + offset); 

        if (new_offset <= MININT32 || new_offset > MAXINT32) {
          printf("Int Min Value : %d Int Max Value : %d\n", MININT32, MAXINT32);
          printf("New offset : %d Overflow : %d\n", new_offset, new_offset - MAXINT32);
          assert(false);
          return false;
        }
	  
        // Prepare to access the bytes of the 32bit new_offset
        uint8_t *np = reinterpret_cast<uint8_t*>(&new_offset); 
	  
        // Create a new call instruction with new_offset 
        uint8_t newcall[5] = {0xe8,np[0],np[1],np[2],np[3]};

        // assert(target[0] == 0x00);

        // TODO: Check that the decodedInstr.size really is 5 bytes. 
        memcpy(target, newcall, r.src_op.size);
      } else {
        throw invalid_argument("[Relocator] Relocation crosses a function "
            " boundary..\n");
      }

      break;
    }
    case O_MEM: // Indirect call  
    case O_REG: 
    case O_SMEM:
      if (r.src_op.flags & FLAG_RIP_RELATIVE) {
        int64_t new_displacement;
        int32_t displacement = r.src_op.disp;

        Address call_target = r.src + r.src_op.size + displacement;

        if (relocation_bounds.withinRange(call_target, Range::INCLUSIVE)) {
          throw invalid_argument("[Relocator] Relocation crosses a function "
              " boundary..\n");
        }

        new_displacement = reloc_distance + displacement;

        if (new_displacement <= MININT32 || new_displacement > MAXINT32) {
          assert(false);
          return false;
        }

        // assert(target[0] == 0x00);

        // Copy the original instruction to the target
        memcpy(target, src, r.src_op.size);

        // Now fix the displacement

        // Find the size of the immediate, if one exists
        uint8_t imm_size = 0;
        for (int i = 0; i < OPERANDS_NO; i++) {
          if (r.src_op.ops[i].type == O_IMM) {
            imm_size = r.src_op.ops[i].size / 8;
            break;
          }
        }

        // Patch the address of relative displacement obtained relative to end 
        // of the instruction (target + ins.size) and subtracing imm_size and 
        // displacement (4), to contain the new value  
        *reinterpret_cast<int32_t*>(target + r.src_op.size - imm_size - 4) = 
           static_cast<int32_t>(new_displacement);
      } else { 
        // Some other register than RIP
        // Just copy it since rest of the register state remains unchanged at 
        // relocated site
        // assert(target[0] == 0x00);

        memcpy(target, src, r.src_op.size);
      }

      break;
    default:
      // TODO: Make a show instance of _DInst and print the instruction info to the
      // exception string 
      throw invalid_argument("[Relocator] Unknown near CALL instruction..\n");
  }

  return true;
}

bool fixupBranch(const Sequence* seq, const vector<Rewrite>& rewrites,
    const Rewrite& r) {
  int type   = r.src_op.ops[0].type;

  Address src = r.src;
  Address target = r.target;
  int64_t reloc_distance = src - target;

  switch (type) {
    case O_PC: // Relative jump
    {
      int32_t offset = r.src_op.imm.addr;
      int offset_bits = r.src_op.ops[0].size; 

	    if (offset_bits == 32) {  // JMP/Jcc rel32 (Near jmp)
	      int64_t new_offset = reloc_distance + offset; 

        if (new_offset <= MININT32 || new_offset > MAXINT32) {
          assert(false);
        }

	      uint8_t *np = reinterpret_cast<uint8_t*>(&new_offset); 
	      uint8_t newoff[4] = {np[0],np[1],np[2],np[3]}; 

        // assert(target[0] == 0x00);

        // Skips over the opcode (1/2 byte(s)) to write to the offset
        // 32 bit offset version of conditional jumps features a two byte opcode
        // 32 bit offset version of unconditional jumps has only one byte for 
        // opcode
        if (META_GET_FC(r.src_op.meta) == FC_CND_BRANCH) {
	        memcpy(target + 2, newoff, 4);
        } else if (META_GET_FC(r.src_op.meta) == FC_UNC_BRANCH) {
	        memcpy(target + 1, newoff, 4);
        } else {
          assert(false);
        }
      } else if (offset_bits == 8) {  // JMP/Jcc rel8 (Short jmp)
	  
        Address orig_jmp_target = src + r.src_op.size + offset;

        if (r.transformed) {
          // Skip over the short jump. It points to the near jump 
          // which immediately follows
          Address jmp_addr = r.target + Assembler::JMP_REL8_SZ; 

          *(r.target + 1) = 0x5; // SKips the near jump

          // Then fixup the near jump target to point to the instruction
          // being pointed to  by the original short jump
          int32_t new_offset = orig_jmp_target - jmp_addr - 
            Assembler::JMP_REL32_SZ;
  	      uint8_t *np = reinterpret_cast<uint8_t*>(&new_offset); 
  	      uint8_t newjmp[5] = {0xe9,np[0],np[1],np[2],np[3]}; 

          // assert(target[0] == 0x00);

	        memcpy(jmp_addr, newjmp, Assembler::JMP_REL32_SZ); 

          Address short_jmp = jmp_addr + Assembler::JMP_REL32_SZ;
          *reinterpret_cast<int8_t*>(short_jmp + 1) = -7; // Jumps back to near
            // jmp
        } else if (r.opnd_changed) {
          Disassembler disas;
          const int target_index = disas.findInstructionIndex(orig_jmp_target, seq);
          Address new_target = rewrites[target_index].target; 
          int32_t new_offset = new_target - r.target - r.src_op.size;

          if (new_offset <= MININT8 || new_offset > MAXINT8) {
            assert(false);
          }

          uint8_t newoff = static_cast<uint8_t>(new_offset);

          // Skips over the opcode (1 byte) to write to the offset
          memcpy(r.target + 1, &newoff, 1);
        } else {
          // Just copy it since no changes are necessary
 	        memcpy(target, src, r.src_op.size);
        }
      }

      break;
    }
    case O_MEM: // Indirect jump
    case O_REG:
    case O_SMEM:
      if (r.src_op.flags & FLAG_RIP_RELATIVE) {
        int64_t new_displacement;
        int32_t displacement = r.src_op.disp;

        new_displacement = reloc_distance + displacement;

        if (new_displacement <= MININT32 || new_displacement > MAXINT32) {
          assert(false);
          return false;
        }

        // assert(target[0] == 0x00);

        // Copy the original instruction to the target
        memcpy(target, src, r.src_op.size);

        // Now fix the displacement

        // Find the size of the immediate, if one exists
        uint8_t imm_size = 0;
        for (int i = 0; i < OPERANDS_NO; i++) {
          if (r.src_op.ops[i].type == O_IMM) {
            imm_size = r.src_op.ops[i].size / 8;
            break;
          }
        }

        // Patch the address of relative displacement obtained relative to end of 
        // the instruction (target + ins.size) and subtracing imm_size and 
        // displacement (4), to contain the new value  
        *reinterpret_cast<int32_t*>(target + r.src_op.size - imm_size - 4) = 
          static_cast<int32_t>(new_displacement);
      } else { 

        // assert(target[0] == 0x00);

        // Some other register than RIP
        // Just copy it since rest of the register state remains unchanged at 
        // relocated site
        memcpy(target, src, r.src_op.size);
      }

      break;
    default:
      // TODO: Make a show instance of _DInst and print the instruction info to the
      // exception string 
      throw invalid_argument("Unknown near JMP/Jcc instruction..\n");
  }

  return true;
}

bool fixupGeneric(const Sequence* seq, const Rewrite& r) {  
  Address src = r.src;
  Address target = r.target; 

  int64_t reloc_distance = src - target; 

  if (r.src_op.flags & FLAG_RIP_RELATIVE) { // RIP relative
    int64_t new_displacement;
    int32_t displacement = r.src_op.disp;

    new_displacement = reloc_distance + displacement;

    if (new_displacement <= MININT32 || new_displacement > MAXINT32) {
      assert(false);
      return false;
    }

    // assert(target[0] == 0x00);

    // Copy the original instruction to the target
    memcpy(target, src, r.src_op.size);

    // Now fix the displacement

    // Find the size of the immediate, if one exists
    uint8_t imm_size = 0;
    for (int i = 0; i < OPERANDS_NO; i++) {
      if (r.src_op.ops[i].type == O_IMM) {
        imm_size = r.src_op.ops[i].size / 8;

        if (r.src_op.size == 10) {
          assert(imm_size == 4);
          // imm_size = 4;
        }
        break;
      }
    }

    // Patch the address of relative displacement obtained relative to end of 
    // the instruction (target + ins.size) and subtracing imm_size and 
    // displacement (4), to contain the new value  
    *reinterpret_cast<int32_t*>(target + r.src_op.size - imm_size - 4) = 
      static_cast<int32_t>(new_displacement);
  } else {

    // assert(target[0] == 0x00);

    // Just copy it. Hoping it would not fire missles unintentionally.
	  memcpy(target, src, r.src_op.size);
  }

  return true;
}
 
bool fixup(const Sequence* seq, const vector<Rewrite>& rewrites) {

  assert(seq->n_instructions == rewrites.size());

  Disassembler disas;
  Address src_ip = seq->start;
  bool success = true;
  for (Rewrite r : rewrites) { 
    if (disas.isBranch(r.src_op)) {
      success = fixupBranch(seq, rewrites, r);
    } else if (disas.isNearCall(r.src_op)) {
      success = fixupCall(seq, r);
    } else {
      success = fixupGeneric(seq, r);
    }
    
    // Fail fast if we cannot fixup an instruction of the given sequence
    if (!success) {
      return false;
    }
  } 

  return success;
}

Relocations Relocator::relocate(Address start, Address end, Address target) { 
    
  Disassembler disas;
 
  //disassemble the range of interest.     
  const Sequence *seq = disas.disassemble(start, end); 

  vector<Rewrite> rewrites = rewriteSequence(seq, target);
  bool success = fixup(seq, rewrites);

  Relocations relocations; 
  if (success) {
    relocations.n_instructions = seq->n_instructions;
    relocations.relocation_offsets = new int [seq->n_instructions];

    int index = 0;
    int offset = 0;
    relocations.relocation_size = 0;
    for (const Rewrite& r : rewrites) {
      relocations.relocation_offsets[index] = offset;
      offset += r.size;
      relocations.relocation_size += r.size;
      index++;
    }
  } else {
    // Return a result that indicates failure to relocate 
    relocations.n_instructions = 0; 
    relocations.relocation_offsets = NULL; 
    relocations.relocation_size = 0;
  }

  return relocations;
} // end relocate 

int64_t Relocator::getRelocationSize(utils::Address start, utils::Address end, 
        utils::Address target) {
  Disassembler disas;
 
  //disassemble the range of interest.     
  const Sequence *seq = disas.disassemble(start, end); 

  _DInst *decoded = static_cast<_DInst*>(seq->instructions);
  vector<Rewrite> rewrites;
  rewrites.reserve(seq->n_instructions);

  Range relocation_bounds(seq->start, seq->end);

  // Do a first pass to get the rewrites
  Address src_ip = seq->start;
  for (int i=0; i < seq->n_instructions; i++) {
    Rewrite r = getRewriteRule(decoded[i], src_ip, relocation_bounds); 
    r.index = i;
    rewrites.push_back(r);
    src_ip += decoded[i].size;
  }

  // Now keep transforming until it stabilizes without more transformations
  bool newly_transformed;
  do {
    newly_transformed = false; // reset
    for (Rewrite& r : rewrites) {
      newly_transformed |= validateRewrite(seq, rewrites, r);
    }
  } while (newly_transformed);

  // Now get the total size of the relocated code
  int64_t size = 0;
  for (const Rewrite& r : rewrites) {
    size += r.size;
  }

  return size;
}
 
} // end liteprobes 
} // end liteinst 
