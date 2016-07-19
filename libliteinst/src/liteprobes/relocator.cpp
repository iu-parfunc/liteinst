
#include <memory.h> 

#include "relocator.hpp"
#include "assembly.hpp" 

#include "distorm.h"
#include "mnemonics.h" 


using namespace utils::assembly;

namespace liteinst { 
namespace liteprobes {

  
  int  bytesNeeded(int64_t v) { 
    
    
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
  
  


  
  Relocations  Relocator::relocate( utils::Address start,
				    utils::Address end, 
				    utils::Address target) { 
    
    Disassembler disas;
 
    //disassemble the range of interest.     
    const Sequence *seq = disas.disassemble(start, end); 
    
    Relocations r; 
    r.relocation_offsets = new int [seq->n_instructions];
    
    
    // break the abstraction and get at the _DInst representation 
    _DInst *decodedInstructions = static_cast<_DInst*>(seq->instructions);
      
    unsigned int dst_offset = 0; 
    unsigned int src_offset = 0; 
    
    // Find the distance between the source and destination addresses
    // Maybe this should be an Int64_t ? 
    int64_t distance = start - target; 
    // We may need to check the magnitude of this distance 
    // as it will influence how to jit new jmps.
    // For example some jmps will will need to be converted
    // from 8 bit offset -> 16 bit -> 32 bit ???


    // ///////////////////////////////////////////////////////
    // Relocator loop 
    //  Copy instructions from src to dst as much as possible. 
    //  Some instructions that need changes in displacements. 
    //  These are handled by specific cases. 
    
    int type; 
    int offset_bits; 
    
    int curr_instr_offset = 0; 

    for (int i = 0; i < seq->n_instructions; i ++) { 
      
      r.relocation_offsets[i] = curr_instr_offset; 
      curr_instr_offset += decodedInstructions[i].size; 
      
      switch (decodedInstructions[i].opcode) { 
	
	// //////////////////////////////////////////////////
	// CALL 
	// //////////////////////////////////////////////////
      case I_CALL: 
	offset_bits = decodedInstructions[i].ops[0].size; 
	type   = decodedInstructions[i].ops[0].type; 

	if ( type == O_PC && offset_bits == 32) { 
	  // This is an 0xE8 call instruction  
	  uint32_t offset; 
	  uint32_t new_offset; 
	  offset = decodedInstructions[i].imm.addr; 

	  // Compute new relative address.
	  // Is this approach ok for "both direction" ? 
	  new_offset = distance + offset; 
	  
	  // Prepare to access the bytes of the 32bit new_offset
	  unsigned char *np = (unsigned char *)&new_offset; 
	  
	  // Create a new call instruction with new_offset 
	  unsigned char newcall[5] = {0xe8,np[0],np[1],np[2],np[3]};

	  // TODO: Check that the decodedInstr.size really is 5 bytes. 
	  memcpy(target + dst_offset,newcall,decodedInstructions[i].size);

	  // update dst and src pointers 
	  dst_offset += decodedInstructions[i].size;
	  src_offset += decodedInstructions[i].size; 

	} else { 
	  // Return a result that indicates failure to relocate 
	  r.n_instructions = 0; 
	  delete(r.relocation_offsets); 
	  r.relocation_offsets = NULL; 
	  return r;
	 
	}
	break; 

	// //////////////////////////////////////////////////
	// JMP
	// //////////////////////////////////////////////////
      case I_JMP:
	offset_bits = decodedInstructions[i].ops[0].size; 
	type   = decodedInstructions[i].ops[0].type;
	uint32_t offset; 
	uint32_t new_offset; 
	// 32 bit displacement jmp ? 
	if ( type == O_PC && offset_bits == 32) { 
	  offset = decodedInstructions[i].imm.addr; 
	  new_offset = distance + offset; 
	  unsigned char *np = (unsigned char*)&new_offset; 
	  unsigned char newjmp[5] = {0xe9,np[0],np[1],np[2],np[3]}; 
	  memcpy(target + dst_offset,newjmp,decodedInstructions[i].size);

	  dst_offset += decodedInstructions[i].size;
	  src_offset += decodedInstructions[i].size;

	  // 8 bit displacement jmp ? 
	} else if (type == O_PC && offset_bits == 8) { 
	  
	  //	  fprintf(stderr,"8 bit displacement JMP detected\nNew displacement size needed: %d Bytes", displacement_size);

	  offset = decodedInstructions[i].imm.addr; 
	  new_offset = distance + offset; 

	  // double check if this is correct 
	  uint64_t jmp_target = (uint64_t)start + src_offset + offset;
	  
	  if ( jmp_target >= (uint64_t)start && jmp_target < (uint64_t)end) { 
	    // jmp is within relocation, do nothing special 
	    memcpy(target + dst_offset,
		   start+src_offset,
		   decodedInstructions[i].size);

	    dst_offset += decodedInstructions[i].size;
	    src_offset += decodedInstructions[i].size;
	    break; 
	  } else { 
	    // jmp is to outside of relocation.
	    // This needs to be improved if displacement is huge 
	    int displacement_size = bytesNeeded(new_offset);   
	    if (displacement_size == 1) { 
	      r.n_instructions = 0; 
	      delete(r.relocation_offsets); 
	      r.relocation_offsets = NULL; 
	      return r;	      
	    } else if (displacement_size == 2) { 
	      r.n_instructions = 0; 
	      delete(r.relocation_offsets); 
	      r.relocation_offsets = NULL; 
	      return r;	      
	    } else if (displacement_size == 4) { 
	      unsigned char *np = (unsigned char*)&new_offset; 
	      unsigned char newjmp[5] = {0xe9,np[0],np[1],np[2],np[3]}; 
	      memcpy(target + dst_offset,newjmp,decodedInstructions[i].size);
	      
	      dst_offset += 5; 
	      src_offset += decodedInstructions[i].size;
	    } else if (displacement_size == 8) { 
	      r.n_instructions = 0; 
	      delete(r.relocation_offsets); 
	      r.relocation_offsets = NULL; 
	      return r;
	    }
	  }
	  
	} else { 
	  r.n_instructions = 0; 
	  delete(r.relocation_offsets); 
	  r.relocation_offsets = NULL; 
	  return r;
	}
	break; 

	//case I_RET: 
	// No special treatment 
	 
      default: 
	memcpy(target + dst_offset,start+src_offset,decodedInstructions[i].size);

	dst_offset += decodedInstructions[i].size;
	src_offset += decodedInstructions[i].size;
	break; 
	
      } // end opcode switch 
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
