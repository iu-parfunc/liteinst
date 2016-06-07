
#include <stdio.h> 
#include <stdlib.h> 
#include <memory.h>

#include "relocator.h"
#include "distorm.h"
#include "mnemonics.h" 


void relocate_info() { 
  unsigned int dver = distorm_version();
  printf("diStorm version: %u.%u.%u\n", (dver >> 16), ((dver) >> 8) & 0xff, dver & 0xff);

} 


int relocate(unsigned char *dst, unsigned char *src,size_t nRelocateInstr) { 

  _DecodeResult res; 
  _DInst *decodedInstructions = (_DInst*)malloc(nRelocateInstr * sizeof(_DecodedInst)); 
  
  unsigned int decodedInstructionsCount = 0;
  unsigned int nDecodeBytes = nRelocateInstr * 8; // Assume 8 bytes per instr

  _CodeInfo ci = {0}; 
  ci.code = (uint8_t*)src; 
  ci.codeLen = nDecodeBytes; 
  ci.dt = Decode64Bits;   
  ci.codeOffset = 0x0; 
  
  /* decode a block  
   
     TODO: Later repeat the procedure 
     * decode a block 
     * relocate that block 
     * keep track of jumps ahead (to find how far to decode) 
     * break out of loop when finding a "return" after the 
       maximum jump ahead distance. 
   
  */
     
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


  printf("decoded %d instructions\n", decodedInstructionsCount); 

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

  for (int i = 0; i < decodedInstructionsCount; i++) { 
    switch (decodedInstructions[i].opcode) {
    case I_CALL: 
      /* Call will need to be modified in the dst */ 
     
    case I_RET: 
      /* check if this return is located after all forward jmp targets.
         If yes, copy to dst and exit */ 
      
    default: 
      memcpy(dst + dst_offset,src+dst_offset,decodedInstructions[i].size);
      dst_offset += decodedInstructions[i].size;
      break; 
    }
    
  }
  


  return 0; 

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
