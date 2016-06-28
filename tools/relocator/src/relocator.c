
#include <stdio.h> 
#include <stdlib.h> 
#include <memory.h>
#include <stdint.h> 

#include "relocator.h"
#include "distorm.h"
#include "mnemonics.h" 


void relocate_info() { 
  unsigned int dver = distorm_version();
  printf("diStorm version: %u.%u.%u\n", (dver >> 16), ((dver) >> 8) & 0xff, dver & 0xff);

} 


/* For printing */ 
char *op_types[] = {"O_NONE", 
		    "O_REG", 
		    "O_IMM", 
		    "O_IMM1", 
		    "O_IMM2", 
		    "O_DISP", 
		    "O_SMEM", 
		    "O_OMEM", 
		    "O_PC",
		    "O_PTR"};


/* TODO: 
     - What to do about jumps: 
       A relative jump with target addr within the relocated are 
       does not need to have that address tweaked. 
       A relative jump to the outside of the relocated code 
       does need ot have the address altered. 
     - The above goes for functions as well. 
       But most relocations are going to be on a sub function-sized part of code.  


     - Test cases: 
       * Perform small relocations within function. Move a small number of 
         instructions to elsewhere, put a jmp at old place to new place and 
         affix a jmp at the end of relocated code back to end of old code. 
       * Function pointers to get indirect jmps/calls
      
 */ 
int relocate(unsigned char *dst, 
             unsigned char *src,
             unsigned char *epilogue,
	     size_t epilogue_size,
             size_t nRelocateInstr) { 

  _DecodeResult res; 
  _DInst *decodedInstructions = (_DInst*)malloc(nRelocateInstr * sizeof(_DecodedInst)); 
  
  unsigned int decodedInstructionsCount = 0;
  unsigned int nDecodeBytes = nRelocateInstr * 8; // Assume 8 bytes per instr

  _CodeInfo ci = {0}; 
  ci.code = (uint8_t*)src; 
  ci.codeLen = nDecodeBytes; 
  ci.dt = Decode64Bits;   
  ci.codeOffset = 0x0; 
  
  res = distorm_decompose(&ci, 
			  decodedInstructions, 
			  nDecodeBytes,  
			  &decodedInstructionsCount);
  
  /* Check for decode error */ 
  if (res == DECRES_INPUTERR) {	      
    // fprintf(stderr,"Instruction decode error\n");
    return -1;
  }

  /* decoded way too many instructions ?*/ 
  if (decodedInstructionsCount >= nRelocateInstr) { 
    /* if too many, just repair by truncating */ 
    decodedInstructionsCount = nRelocateInstr; 
  } 
  /* Or way too few ? */ 
  else { 
    /* did not manage to decode as many instructions 
       as the user asked for */
    return -1; 
  }

  // printf("decoded %d instructions\n", decodedInstructionsCount); 

  unsigned int dst_offset = 0; 
  
  /* What is the distance between src and dst */ 

  /* need to consider what types make sense here! */ 
  int64_t distance =  (uint64_t)src - (uint64_t)dst; 
  /* and may need to check the magnitude of this distance 
     as it will influence how to jit new jmps. 
     For example some jmps will will need to be converted 
     from 8 bit offset -> 16 bit -> 32 bit ???  
  */

  /* copy instructions from src to dest 

     Add cases here for each instruction that needs 
     special relocation treatment. 
     call/jmp/jcc/mov
     
  */   
  int call_type;
  int offset_bits; 

  for (int i = 0; i < decodedInstructionsCount; i++) { 
    switch (decodedInstructions[i].opcode) {
    case I_CALL: 
      offset_bits = decodedInstructions[i].ops[0].size; 
      call_type   = decodedInstructions[i].ops[0].type;

      /*
      printf("Call instruction: \n");
      for (int j = 0; j < 4; j++) {
      	printf("operand type %d: %s\n",j,
          op_types[decodedInstructions[i].ops[j].type]);
      	printf("operand size %d: %d bits\n",j,
      	       decodedInstructions[i].ops[j].size);
      }
     
      
      exit(-1);
      */


      
      /* is this a PC relative call with a 32 bit displacement? */       
      if ( call_type == O_PC && offset_bits == 32) { 
	/* this is an E8 call */ 
	uint32_t offset; 
	uint32_t new_offset; 
	offset = decodedInstructions[i].imm.addr;
	//printf("%d offset %d \n", i, offset); 
	//printf("%d distance %d \n", i, (uint32_t)distance); 

	/* compute new relative address */ 
	new_offset = distance + offset; 
	
        /* prepare to access the bytes of the 32bit new_offset */ 
	unsigned char *np = (unsigned char *)&new_offset; 

	/* create a new call instruction with new_offset */ 
	unsigned char newcall[5] = {0xe8,np[0],np[1],np[2],np[3]}; 
	
	/* copy generated call to dst and update src location pointers */ 
	memcpy(dst + dst_offset,newcall,decodedInstructions[i].size);
	dst_offset += decodedInstructions[i].size;

	
      } else { 
	/* TODO: Figure out how to handle more kinds of calls */ 
	fprintf(stderr,"Error: unsupported Call instr"); 	
      }
      break; 
      
      /* Call will need to be modified in the dst */ 
     
    case I_RET: 
      /* no special treatment! Just relocate */ 
      
      
    default: 
      memcpy(dst + dst_offset,src+dst_offset,decodedInstructions[i].size);
      dst_offset += decodedInstructions[i].size;
      break; 
    }
    
  }

  /* Attach epilogue to end of relocated instr stream */ 
  if (epilogue && epilogue_size > 0) { 
    memcpy(dst + dst_offset,epilogue,epilogue_size);
  } 
  


  return decodedInstructionsCount; 

}


static int position_independent(_DInst *instr ) { 
  
  switch(instr->opcode) { 
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
  case I_JMP:
  case I_JMP_FAR: /* this one may be fine */ 
  case I_JNO:
  case I_JNP:
  case I_JNS:
  case I_JNZ:
  case I_JO:
  case I_JP:
  case I_JRCXZ:
  case I_JS:
  case I_JZ:
  case I_CALL:
    return 0; 
  /* Many instructions can use RIP relative addressing... */ 
  

  default: 
    return 1;
  }
} 


/* A very pessimistic count_relocatable function 
   Answers in bytes*/ 
unsigned int count_relocatable(unsigned char *addr, size_t nMax) { 
  
  _DecodeResult res; 
  _DInst *decodedInstructions = (_DInst*)malloc(nMax * sizeof(_DecodedInst)); 
  
  unsigned int decodedInstructionsCount = 0;
  unsigned int nDecodeBytes = nMax * 8; // Assume 8 bytes per instr

  _CodeInfo ci = {0}; 
  ci.code = (uint8_t*)addr; 
  ci.codeLen = nDecodeBytes; 
  ci.dt = Decode64Bits;   
  ci.codeOffset = 0x0; 
       
  res = distorm_decompose(&ci, 
			  decodedInstructions, 
			  nMax,  
			  &decodedInstructionsCount);

  /* Check for decode error */ 
  if (res == DECRES_INPUTERR) {	      
    // fprintf(stderr,"Instruction decode error\n");
    return -1;
  }

  /* decoded way too many instructions ?*/ 
  if (decodedInstructionsCount >= nMax) { 
    /* if too many, just repair by truncating */ 
    decodedInstructionsCount = nMax; 
  } 
  /* Or way too few ? */ 
  else { 
    /* did not manage to decode as many instructions 
       as the user asked for */
    return -1; 
  }

  
  unsigned int relocatableBytes = 0; 

  
  for (int i = 0; i < decodedInstructionsCount; i++) { 

    if ( position_independent(&decodedInstructions[i]) ) { 
      relocatableBytes += decodedInstructions[i].size;
    } else { 
      break; 
    }
  }
  
  return relocatableBytes;

} 
