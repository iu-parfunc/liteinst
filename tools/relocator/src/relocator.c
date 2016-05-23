


#include <stdio.h> 
#include <stdlib.h> 
#include <memory.h>

#include "relocator.h"
#include "distorm.h"
#include "mnemonics.h" 

#define DECODE_BLOCK_SIZE 64 /* instructions */ 

void relocate_info() { 
  unsigned int dver = distorm_version();
  printf("diStorm version: %u.%u.%u\n", (dver >> 16), ((dver) >> 8) & 0xff, dver & 0xff);

} 


void relocate(unsigned char *dst, unsigned char *src,size_t n) { 
  /* TODO: Implement this 
   *   - Currently have no idea how, so attacking the simpler 
   *     problem of relocating an entire function. 
   */ 
  
  printf("relocate\n"); 

}


/* Relocate an entire function. 
   Implement this to better understand the problem  */ 

int relocate_function(unsigned char *dst, unsigned char *src,size_t n) {  
   
  _DecodeResult res; 
  _DInst *decodedInstructions = (_DInst*)malloc(n * sizeof(_DecodedInst)); 
  
  unsigned int decodedInstructionsCount = 0;
  
  _CodeInfo ci = {0}; 
  ci.code = (uint8_t*)src; 
  /* assume up to 8 bytes per instr, be more rigorous later  */ 
  ci.codeLen = DECODE_BLOCK_SIZE*8; 
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
			  DECODE_BLOCK_SIZE,  
			  &decodedInstructionsCount);

  if (res == DECRES_INPUTERR) {	      
    fprintf(stderr,"Instruction decode error\n");
    return -1;
  }
  printf("decoded %d instructions\n", decodedInstructionsCount); 

  unsigned int dst_offset = 0; 

  /* copy instructions from src to dest 

     Add cases here for each instruction that needs 
     special relocation treatment. 
     
   */   
  for (int i = 0; i < decodedInstructionsCount; i++) { 
    switch (decodedInstructions[i].opcode) {
    case I_CALL: 
      /* Call will need to be modified in the dst */ 
     
      
      
    default: 
      memcpy(dst + dst_offset,src+dst_offset,decodedInstructions[i].size);
      dst_offset += decodedInstructions[i].size;
      break; 
    }
    
  }
  


  return 0; 
} 
