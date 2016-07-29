
#include <stdexcept>
#include <limits>
#include <assert.h>
#include <memory.h> 

#include "relocator.hpp"
#include "assembly.hpp" 

#include "distorm.h"
#include "mnemonics.h" 

namespace liteinst { 
namespace liteprobes {

using std::invalid_argument;
using utils::Address;

struct RelocationArgs {
  Address start;
  Address end;
  Address target;
};

using namespace utils::assembly;

static const int MAXINT32 = std::numeric_limits<int32_t>::max();
static const int MININT32 = std::numeric_limits<int32_t>::min();

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

bool rewrite() {

}
  
bool relocateNearCall(_DInst ins, const RelocationArgs& args, 
    unsigned int& dst_offset, unsigned int& src_offset) {
 int offset_bits = ins.ops[0].size; 
 int type = ins.ops[0].type; 

 Address src = args.start + src_offset;
 Address target = args.target + dst_offset; 

 // Find the distance between the source and destination addresses
 // Maybe this should be an Int64_t ? 
 int64_t distance = args.start - args.target; 

 // Relative call
 if (type == O_PC && offset_bits == 32) { 
   // This is an 0xE8 call instruction  
   int32_t new_offset; 
   int32_t offset = ins.imm.addr; 

   // Compute new relative address.
   // Is this approach ok for "both direction" ? 
   new_offset = static_cast<int32_t>(distance + offset); 

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

   assert(target[0] == 0x00);

   // TODO: Check that the decodedInstr.size really is 5 bytes. 
   memcpy(target, newcall, ins.size);

   // update dst and src pointers 
   dst_offset += ins.size;
   src_offset += ins.size; 

  } else if (type == O_REG) { // Indirect call  
    if (ins.flags & FLAG_RIP_RELATIVE) {
      int64_t new_displacement;
      int32_t displacement = ins.disp;

      new_displacement = distance + displacement;

      if (new_displacement <= MININT32 || new_displacement > MAXINT32) {
        assert(false);
        return false;
      }

      assert(target[0] == 0x00);

      // Copy the original instruction to the target
      memcpy(target, src, ins.size);

      // Now fix the displacement

      // Find the size of the immediate, if one exists
      uint8_t imm_size = 0;
      for (int i = 0; i < OPERANDS_NO; i++) {
        if (ins.ops[i].type == O_IMM) {
          imm_size = ins.ops[i].size / 8;
          break;
        }
      }

      // Patch the address of relative displacement obtained relative to end of 
      // the instruction (target + ins.size) and subtracing imm_size and 
      // displacement (4), to contain the new value  
      *reinterpret_cast<int32_t*>(target + ins.size - imm_size - 4) = 
        static_cast<int32_t>(new_displacement);

      // update dst and src pointers 
      dst_offset += ins.size;
      src_offset += ins.size; 

    } else { 
      // Some other register than RIP
      // Just copy it since rest of the register state remains unchanged at 
      // relocated site
      assert(target[0] == 0x00);

      memcpy(target, src, ins.size);

      // update dst and src pointers 
      dst_offset += ins.size;
      src_offset += ins.size; 
    }
  } else {
    // TODO: Make a show instance of _DInst and print the instruction info to the
    // exception string 
    throw invalid_argument("Unknown near CALL instruction..\n");
  }

  return true;
}

bool relocateNearJump(_DInst ins, const RelocationArgs& args,  
    unsigned int& dst_offset, unsigned int& src_offset) {
	int offset_bits = ins.ops[0].size; 
	int type   = ins.ops[0].type;

   Address src = args.start + src_offset;
   Address target = args.target + dst_offset; 

  // Find the distance between the source and destination addresses
  // Maybe this should be an Int64_t ? 
  int distance = args.start - args.target; 

	uint32_t offset; 
	uint32_t new_offset; 
	// 32 bit displacement jmp ? 
	if ( type == O_PC && offset_bits == 32) {  // Near jump
	  offset = ins.imm.addr; 
	  new_offset = distance + offset; 
	  uint8_t *np = reinterpret_cast<uint8_t*>(&new_offset); 
	  uint8_t newjmp[5] = {0xe9,np[0],np[1],np[2],np[3]}; 

    assert(target[0] == 0x00);

	  memcpy(target, newjmp, ins.size);

	  dst_offset += ins.size;
	  src_offset += ins.size;

	} else if (type == O_PC && offset_bits == 8) {  // Short jump
	  
	  //	  fprintf(stderr,"8 bit displacement JMP detected\nNew displacement size needed: %d Bytes", displacement_size);

	  offset = ins.imm.addr; 
	  new_offset = distance + offset; 

	  // double check if this is correct 
	  uint64_t jmp_target = reinterpret_cast<uint64_t>(src) + offset;
	  
	  if (jmp_target >= reinterpret_cast<uint64_t>(args.start) &&
        jmp_target < reinterpret_cast<uint64_t>(args.end)) { 
	    // jmp is within relocation, do nothing special 

      assert(target[0] == 0x00);

	    memcpy(target, src, ins.size);

	    dst_offset += ins.size;
	    src_offset += ins.size;
	  } else { 
	    // jmp is to outside of relocation.
	    // This needs to be improved if displacement is huge (or small) 
	    int displacement_size = bytesNeeded(new_offset);   
	    // if (displacement_size == 1) { 
	    //   r.n_instructions = 0; 
	    //   delete(r.relocation_offsets); 
	    //   r.relocation_offsets = NULL; 
	    //   return r;	      
	    // } else if (displacement_size == 2) { 
	    //   r.n_instructions = 0; 
	    //   delete(r.relocation_offsets); 
	    //   r.relocation_offsets = NULL; 
	    //   return r;	      
	    
	    // For now rejit as a size 4Bytes offset.  
	    if (displacement_size < 8) { 
	      unsigned char *np = (unsigned char*)&new_offset; 
	      unsigned char newjmp[5] = {0xe9,np[0],np[1],np[2],np[3]}; 

        assert(target[0] == 0x00);

	      memcpy(target, newjmp, ins.size);
	      
	      dst_offset += 5; 
	      src_offset += ins.size;
	    } else if (displacement_size == 8) { 
        return false;
	    }
	  }
	  
	} else if (type == O_REG) { // Indirect jump
    if (ins.flags & FLAG_RIP_RELATIVE) {
      int64_t new_displacement;
      int32_t displacement = ins.disp;

      new_displacement = distance + displacement;

      if (new_displacement <= MININT32 || new_displacement > MAXINT32) {
        assert(false);
        return false;
      }

      assert(target[0] == 0x00);

      // Copy the original instruction to the target
      memcpy(target, src, ins.size);

      // Now fix the displacement

      // Find the size of the immediate, if one exists
      uint8_t imm_size = 0;
      for (int i = 0; i < OPERANDS_NO; i++) {
        if (ins.ops[i].type == O_IMM) {
          imm_size = ins.ops[i].size / 8;
          break;
        }
      }

      // Patch the address of relative displacement obtained relative to end of 
      // the instruction (target + ins.size) and subtracing imm_size and 
      // displacement (4), to contain the new value  
      *reinterpret_cast<int32_t*>(target + ins.size - imm_size - 4) = 
        static_cast<int32_t>(new_displacement);

      // update dst and src pointers 
      dst_offset += ins.size;
      src_offset += ins.size; 

    } else { 

      assert(target[0] == 0x00);

      // Some other register than RIP
      // Just copy it since rest of the register state remains unchanged at 
      // relocated site
      memcpy(target, src, ins.size);

      // update dst and src pointers 
      dst_offset += ins.size;
      src_offset += ins.size; 
    }
  } else {
    // TODO: Make a show instance of _DInst and print the instruction info to the
    // exception string 
    throw invalid_argument("Unknown near JMP instruction..\n");
  }

  return true;
}

bool relocateGeneric(_DInst ins, const RelocationArgs& args,  
    unsigned int& dst_offset, unsigned int& src_offset) {

	int type   = ins.ops[0].type;

  Address src = args.start + src_offset;
  Address target = args.target + dst_offset; 

  // Find the distance between the source and destination addresses
  // Maybe this should be an Int64_t ? 
  int distance = args.start - args.target; 

  if (src == (Address) 0x0000000000403b80) {
    printf("INSTRUCTION RELATIVE AT %p with operand type of %d : %d\n", src, type, 
        ins.flags & FLAG_RIP_RELATIVE);
    // assert(false);
  }

  if (ins.flags & FLAG_RIP_RELATIVE) { // RIP relative
    if (src == (Address) 0x0000000000401fc5) {
      printf("INSTRUCTION RELATIVE AT %p : %d\n", src, ins.flags & FLAG_RIP_RELATIVE);
      assert(false);
    }
    
    int64_t new_displacement;
    int32_t displacement = ins.disp;

    new_displacement = distance + displacement;

    if (new_displacement <= MININT32 || new_displacement > MAXINT32) {
      assert(false);
      return false;
    }

    assert(target[0] == 0x00);

    // Copy the original instruction to the target
    memcpy(target, src, ins.size);

    // Now fix the displacement

    // Find the size of the immediate, if one exists
    uint8_t imm_size = 0;
    for (int i = 0; i < OPERANDS_NO; i++) {
      if (ins.ops[i].type == O_IMM) {
        imm_size = ins.ops[i].size / 8;

        // Hack around distorm bug
        if (ins.size == 10) {
          imm_size = 4;
        }
        break;
      }
    }

    // Patch the address of relative displacement obtained relative to end of 
    // the instruction (target + ins.size) and subtracing imm_size and 
    // displacement (4), to contain the new value  
    *reinterpret_cast<int32_t*>(target + ins.size - imm_size - 4) = 
      static_cast<int32_t>(new_displacement);

    // update dst and src pointers 
    dst_offset += ins.size;
    src_offset += ins.size; 
  } else {

    assert(target[0] == 0x00);

    // Just copy it. Hoping it would not fire missles unintentionally.
	  memcpy(target ,src, ins.size);

	  dst_offset += ins.size;
	  src_offset += ins.size;
  }

  return true;
}
  
  Relocations  Relocator::relocate(Address start, Address end, Address target) { 
    
    Disassembler disas;
 
    //disassemble the range of interest.     
    const Sequence *seq = disas.disassemble(start, end); 

    Relocations r; 
    r.relocation_offsets = new int [seq->n_instructions];
    
    
    // break the abstraction and get at the _DInst representation 
    _DInst *decodedInstructions = static_cast<_DInst*>(seq->instructions);
      
    unsigned int dst_offset = 0; 
    unsigned int src_offset = 0; 
    
    // We may need to check the magnitude of this distance 
    // as it will influence how to jit new jmps.
    // For example some jmps will will need to be converted
    // from 8 bit offset -> 16 bit -> 32 bit ???


    // ///////////////////////////////////////////////////////
    // Relocator loop 
    //  Copy instructions from src to dst as much as possible. 
    //  Some instructions that need changes in displacements. 
    //  These are handled by specific cases. 
    
    int curr_instr_offset = 0; 

    bool success = true;
    RelocationArgs args = {start, end, target};

    for (int i = 0; i < seq->n_instructions; i ++) { 
      
      r.relocation_offsets[i] = curr_instr_offset; 
      curr_instr_offset += decodedInstructions[i].size; 
    
      switch (decodedInstructions[i].opcode) { 
	
	// //////////////////////////////////////////////////
	// CALL 
	// //////////////////////////////////////////////////
      case I_CALL: 
        success = relocateNearCall(decodedInstructions[i], args, 
            dst_offset, src_offset);
      	break; 

	// //////////////////////////////////////////////////
	// JMP
	// //////////////////////////////////////////////////
      case I_JMP:
       success = relocateNearJump(decodedInstructions[i], args,
           dst_offset, src_offset);
	      break; 
      default: 
        success = relocateGeneric(decodedInstructions[i], args,
           dst_offset, src_offset);
      	break; 
      } // end opcode switch 

      // Fail fast if we cannot relocate an instruction of the given sequence
      if (!success) {
        // Return a result that indicates failure to relocate 
	      r.n_instructions = 0; 
	      delete(r.relocation_offsets); 
	      r.relocation_offsets = NULL; 
	      return r;
      }
    } // end for 
    
    // this is somewhat cheating. 
    // if it is ok for the relocator to just relocate a 
    // sub-part of the range, it should be reflected in the 
    // return value here. 
    r.n_instructions = seq->n_instructions; 
    return r; 
  } // end relocate 
 
} // end liteprobes 
} // end liteinst 
