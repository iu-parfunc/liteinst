
#include <memory.h> 

#include "relocator.hpp"
#include "assembly.hpp" 

#include "distorm.h"
#include "mnemonics.h" 


using namespace utils::assembly;

namespace liteinst { 
namespace liteprobes {
  
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
    
    // Find the distance between the source and destination addresses
    uint64_t distance = start - target; 
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
	  dst_offset += decodedInstructions[i].size;
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

	if ( type == O_PC && offset_bits == 32) { 
	  uint32_t offset; 
	  uint32_t new_offset; 
	  offset = decodedInstructions[i].imm.addr; 
	  new_offset = distance + offset; 
	  unsigned char *np = (unsigned char*)&new_offset; 
	  unsigned char newjmp[5] = {0xe9,np[0],np[1],np[2],np[3]}; 
	  memcpy(target + dst_offset,newjmp,decodedInstructions[i].size);
	  dst_offset += decodedInstructions[i].size;
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
	memcpy(target + dst_offset,start+dst_offset,decodedInstructions[i].size);
	dst_offset += decodedInstructions[i].size;
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
