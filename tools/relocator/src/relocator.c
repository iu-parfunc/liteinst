
#include <stdio.h> 
#include <stdlib.h> 
#include <memory.h>
#include <stdint.h> 

#include "relocator.h"
#include "distorm.h"
#include "mnemonics.h" 


const char *opcode_to_str(uint16_t instr); 

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
  ci.codeOffset = 0x100000; 
  
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
    return 0; 
  /* Many instructions can use RIP relative addressing... */ 
  
  case I_CALL:
    return 1; /* should be possible to handle now */
    
  default:
    return 1;
  }
} 


/* A very pessimistic count_relocatable function 
   Answers in bytes*/ 
int count_relocatable(unsigned char *addr, size_t nMax) { 
  
  _DecodeResult res; 
  _DInst *decodedInstructions = (_DInst*)malloc(nMax * sizeof(_DecodedInst)); 
  
  unsigned int decodedInstructionsCount = 0;
  unsigned int nDecodeBytes = nMax * 8; // Assume 8 bytes per instr

  _CodeInfo ci = {0}; 
  ci.code = (uint8_t*)addr; 
  ci.codeLen = nDecodeBytes; 
  ci.dt = Decode64Bits;   
  ci.codeOffset = 0x100000; 
       
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


int relocatable(unsigned char *addr, 
		size_t nMax, 
		unsigned int  *n_relocatable, 
		unsigned int  *n_relocatable_bytes
		) { 

  _DecodeResult res; 
  _DInst *decodedInstructions = (_DInst*)malloc(nMax * sizeof(_DecodedInst)); 
  
  unsigned int decodedInstructionsCount = 0;
  unsigned int nDecodeBytes = nMax * 8; // Assume 8 bytes per instr

  _CodeInfo ci = {0}; 
  ci.code = (uint8_t*)addr; 
  ci.codeLen = nDecodeBytes; 
  ci.dt = Decode64Bits;   
  ci.codeOffset = 0x100000; 
       
  res = distorm_decompose(&ci, 
			  decodedInstructions, 
			  nMax,  
			  &decodedInstructionsCount);

  /* Check for decode error */ 
  if (res == DECRES_INPUTERR) {	      
    // fprintf(stderr,"Instruction decode error\n");
    return 0;
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
    return 0; 
  }

  
  unsigned int relocatableBytes = 0; 
  /* think about answering in instrs */ 
  unsigned int relocatableInst = 0; 

  
  for (int i = 0; i < decodedInstructionsCount; i++) { 

    if ( position_independent(&decodedInstructions[i]) ) { 
      relocatableInst ++; 
      relocatableBytes += decodedInstructions[i].size;
    } else { 
      break; 
    }
  }
  
  
  *n_relocatable = relocatableInst; 
  *n_relocatable_bytes = relocatableBytes;

  return 1;
 
}




/* Collect information about instruction start addresses  
   
   It seems distorm and gdb does not fully agree on 
   what the start addresses of instructions are. 
   TODO: Investigate the source of this problem. 
    (test_small1.exe is an indicator)
*/ 


int instruction_offsets(unsigned char *addr, 
                        uint32_t *offs, 
			size_t nMax) { 
  
  _DecodeResult res; 
  _DInst *decodedInstructions = (_DInst*)malloc(nMax * sizeof(_DecodedInst)); 
  
  unsigned int decodedInstructionsCount = 0;
  unsigned int nDecodeBytes = nMax * 8; // Assume 8 bytes per instr

  _CodeInfo ci = {0}; 
  ci.code = (uint8_t*)addr; 
  ci.codeLen = nDecodeBytes; 
  ci.dt = Decode64Bits;   
  ci.codeOffset = 0x100000; 
       
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
  
  unsigned int offset = 0; 
  
  for (int i = 0; i < decodedInstructionsCount; i++) { 
    offs[i] = offset; 
    
    offset += decodedInstructions[i].size;  
    //printf("%d | %s\n", offs[i], op_to_str(decodedInstructions[i].opcode));
    
  }
  /* set the last offs[i] here as well */
  
  return decodedInstructionsCount;

} 







const char *opcode_to_str(uint16_t instr) { 
switch(instr) {
case 0 : return "I_UNDEFINED";
case 66 : return "I_AAA";
case 389 : return "I_AAD";
case 384 : return "I_AAM";
case 76 : return "I_AAS";
case 31 : return "I_ADC";
case 11 : return "I_ADD";
case 3132 : return "I_ADDPD";
case 3125 : return "I_ADDPS";
case 3146 : return "I_ADDSD";
case 3139 : return "I_ADDSS";
case 6416 : return "I_ADDSUBPD";
case 6426 : return "I_ADDSUBPS";
case 9231 : return "I_AESDEC";
case 9248 : return "I_AESDECLAST";
case 9189 : return "I_AESENC";
case 9206 : return "I_AESENCLAST";
case 9172 : return "I_AESIMC";
case 9817 : return "I_AESKEYGENASSIST";
case 41 : return "I_AND";
case 3043 : return "I_ANDNPD";
case 3035 : return "I_ANDNPS";
case 3012 : return "I_ANDPD";
case 3005 : return "I_ANDPS";
case 111 : return "I_ARPL";
case 9394 : return "I_BLENDPD";
case 9375 : return "I_BLENDPS";
case 7641 : return "I_BLENDVPD";
case 7631 : return "I_BLENDVPS";
case 104 : return "I_BOUND";
case 4368 : return "I_BSF";
case 4380 : return "I_BSR";
case 960 : return "I_BSWAP";
case 872 : return "I_BT";
case 934 : return "I_BTC";
case 912 : return "I_BTR";
case 887 : return "I_BTS";
case 456 : return "I_CALL";
case 260 : return "I_CALL_FAR";
case 228 : return "I_CBW";
case 250 : return "I_CDQ";
case 239 : return "I_CDQE";
case 492 : return "I_CLC";
case 512 : return "I_CLD";
case 4351 : return "I_CLFLUSH";
case 1855 : return "I_CLGI";
case 502 : return "I_CLI";
case 541 : return "I_CLTS";
case 487 : return "I_CMC";
case 694 : return "I_CMOVA";
case 663 : return "I_CMOVAE";
case 656 : return "I_CMOVB";
case 686 : return "I_CMOVBE";
case 754 : return "I_CMOVG";
case 738 : return "I_CMOVGE";
case 731 : return "I_CMOVL";
case 746 : return "I_CMOVLE";
case 648 : return "I_CMOVNO";
case 723 : return "I_CMOVNP";
case 708 : return "I_CMOVNS";
case 678 : return "I_CMOVNZ";
case 641 : return "I_CMOVO";
case 716 : return "I_CMOVP";
case 701 : return "I_CMOVS";
case 671 : return "I_CMOVZ";
case 71 : return "I_CMP";
case 4471 : return "I_CMPEQPD";
case 4392 : return "I_CMPEQPS";
case 4629 : return "I_CMPEQSD";
case 4550 : return "I_CMPEQSS";
case 4489 : return "I_CMPLEPD";
case 4410 : return "I_CMPLEPS";
case 4647 : return "I_CMPLESD";
case 4568 : return "I_CMPLESS";
case 4480 : return "I_CMPLTPD";
case 4401 : return "I_CMPLTPS";
case 4638 : return "I_CMPLTSD";
case 4559 : return "I_CMPLTSS";
case 4510 : return "I_CMPNEQPD";
case 4431 : return "I_CMPNEQPS";
case 4668 : return "I_CMPNEQSD";
case 4589 : return "I_CMPNEQSS";
case 4530 : return "I_CMPNLEPD";
case 4451 : return "I_CMPNLEPS";
case 4688 : return "I_CMPNLESD";
case 4609 : return "I_CMPNLESS";
case 4520 : return "I_CMPNLTPD";
case 4441 : return "I_CMPNLTPS";
case 4678 : return "I_CMPNLTSD";
case 4599 : return "I_CMPNLTSS";
case 4540 : return "I_CMPORDPD";
case 4461 : return "I_CMPORDPS";
case 4698 : return "I_CMPORDSD";
case 4619 : return "I_CMPORDSS";
case 301 : return "I_CMPS";
case 4498 : return "I_CMPUNORDPD";
case 4419 : return "I_CMPUNORDPS";
case 4656 : return "I_CMPUNORDSD";
case 4577 : return "I_CMPUNORDSS";
case 898 : return "I_CMPXCHG";
case 6395 : return "I_CMPXCHG16B";
case 6384 : return "I_CMPXCHG8B";
case 2801 : return "I_COMISD";
case 2793 : return "I_COMISS";
case 865 : return "I_CPUID";
case 255 : return "I_CQO";
case 9280 : return "I_CRC32";
case 6809 : return "I_CVTDQ2PD";
case 3329 : return "I_CVTDQ2PS";
case 6819 : return "I_CVTPD2DQ";
case 2703 : return "I_CVTPD2PI";
case 3255 : return "I_CVTPD2PS";
case 4183 : return "I_CVTPH2PS";
case 2517 : return "I_CVTPI2PD";
case 2507 : return "I_CVTPI2PS";
case 3339 : return "I_CVTPS2DQ";
case 3245 : return "I_CVTPS2PD";
case 4193 : return "I_CVTPS2PH";
case 2693 : return "I_CVTPS2PI";
case 2723 : return "I_CVTSD2SI";
case 3275 : return "I_CVTSD2SS";
case 2537 : return "I_CVTSI2SD";
case 2527 : return "I_CVTSI2SS";
case 3265 : return "I_CVTSS2SD";
case 2713 : return "I_CVTSS2SI";
case 6798 : return "I_CVTTPD2DQ";
case 2636 : return "I_CVTTPD2PI";
case 3349 : return "I_CVTTPS2DQ";
case 2625 : return "I_CVTTPS2PI";
case 2658 : return "I_CVTTSD2SI";
case 2647 : return "I_CVTTSS2SI";
case 245 : return "I_CWD";
case 233 : return "I_CWDE";
case 46 : return "I_DAA";
case 56 : return "I_DAS";
case 86 : return "I_DEC";
case 1646 : return "I_DIV";
case 3521 : return "I_DIVPD";
case 3514 : return "I_DIVPS";
case 3535 : return "I_DIVSD";
case 3528 : return "I_DIVSS";
case 9637 : return "I_DPPD";
case 9624 : return "I_DPPS";
case 4122 : return "I_EMMS";
case 340 : return "I_ENTER";
case 9502 : return "I_EXTRACTPS";
case 4158 : return "I_EXTRQ";
case 1192 : return "I_F2XM1";
case 1123 : return "I_FABS";
case 1023 : return "I_FADD";
case 1549 : return "I_FADDP";
case 1601 : return "I_FBLD";
case 1607 : return "I_FBSTP";
case 1117 : return "I_FCHS";
case 7311 : return "I_FCLEX";
case 1376 : return "I_FCMOVB";
case 1392 : return "I_FCMOVBE";
case 1384 : return "I_FCMOVE";
case 1445 : return "I_FCMOVNB";
case 1463 : return "I_FCMOVNBE";
case 1454 : return "I_FCMOVNE";
case 1473 : return "I_FCMOVNU";
case 1401 : return "I_FCMOVU";
case 1035 : return "I_FCOM";
case 1512 : return "I_FCOMI";
case 1623 : return "I_FCOMIP";
case 1041 : return "I_FCOMP";
case 1563 : return "I_FCOMPP";
case 1311 : return "I_FCOS";
case 1238 : return "I_FDECSTP";
case 1061 : return "I_FDIV";
case 1594 : return "I_FDIVP";
case 1067 : return "I_FDIVR";
case 1586 : return "I_FDIVRP";
case 1488 : return "I_FEDISI";
case 574 : return "I_FEMMS";
case 1482 : return "I_FENI";
case 1527 : return "I_FFREE";
case 1317 : return "I_FIADD";
case 1331 : return "I_FICOM";
case 1338 : return "I_FICOMP";
case 1361 : return "I_FIDIV";
case 1368 : return "I_FIDIVR";
case 1418 : return "I_FILD";
case 1324 : return "I_FIMUL";
case 1247 : return "I_FINCSTP";
case 7326 : return "I_FINIT";
case 1432 : return "I_FIST";
case 1438 : return "I_FISTP";
case 1424 : return "I_FISTTP";
case 1346 : return "I_FISUB";
case 1353 : return "I_FISUBR";
case 1074 : return "I_FLD";
case 1141 : return "I_FLD1";
case 1098 : return "I_FLDCW";
case 1090 : return "I_FLDENV";
case 1155 : return "I_FLDL2E";
case 1147 : return "I_FLDL2T";
case 1170 : return "I_FLDLG2";
case 1178 : return "I_FLDLN2";
case 1163 : return "I_FLDPI";
case 1186 : return "I_FLDZ";
case 1029 : return "I_FMUL";
case 1556 : return "I_FMULP";
case 7303 : return "I_FNCLEX";
case 7318 : return "I_FNINIT";
case 1111 : return "I_FNOP";
case 7333 : return "I_FNSAVE";
case 7288 : return "I_FNSTCW";
case 7271 : return "I_FNSTENV";
case 7348 : return "I_FNSTSW";
case 1213 : return "I_FPATAN";
case 1256 : return "I_FPREM";
case 1230 : return "I_FPREM1";
case 1206 : return "I_FPTAN";
case 1288 : return "I_FRNDINT";
case 1519 : return "I_FRSTOR";
case 7341 : return "I_FSAVE";
case 1297 : return "I_FSCALE";
case 1496 : return "I_FSETPM";
case 1305 : return "I_FSIN";
case 1279 : return "I_FSINCOS";
case 1272 : return "I_FSQRT";
case 1079 : return "I_FST";
case 7296 : return "I_FSTCW";
case 7280 : return "I_FSTENV";
case 1084 : return "I_FSTP";
case 7356 : return "I_FSTSW";
case 1048 : return "I_FSUB";
case 1579 : return "I_FSUBP";
case 1054 : return "I_FSUBR";
case 1571 : return "I_FSUBRP";
case 1129 : return "I_FTST";
case 1534 : return "I_FUCOM";
case 1504 : return "I_FUCOMI";
case 1614 : return "I_FUCOMIP";
case 1541 : return "I_FUCOMP";
case 1409 : return "I_FUCOMPP";
case 1135 : return "I_FXAM";
case 1105 : return "I_FXCH";
case 9914 : return "I_FXRSTOR";
case 9923 : return "I_FXRSTOR64";
case 9886 : return "I_FXSAVE";
case 9894 : return "I_FXSAVE64";
case 1221 : return "I_FXTRACT";
case 1199 : return "I_FYL2X";
case 1263 : return "I_FYL2XP1";
case 633 : return "I_GETSEC";
case 4203 : return "I_HADDPD";
case 4211 : return "I_HADDPS";
case 482 : return "I_HLT";
case 4237 : return "I_HSUBPD";
case 4245 : return "I_HSUBPS";
case 1651 : return "I_IDIV";
case 117 : return "I_IMUL";
case 447 : return "I_IN";
case 81 : return "I_INC";
case 123 : return "I_INS";
case 9569 : return "I_INSERTPS";
case 4165 : return "I_INSERTQ";
case 367 : return "I_INT";
case 360 : return "I_INT_3";
case 476 : return "I_INT1";
case 372 : return "I_INTO";
case 555 : return "I_INVD";
case 8306 : return "I_INVEPT";
case 1727 : return "I_INVLPG";
case 1869 : return "I_INVLPGA";
case 8323 : return "I_INVPCID";
case 8314 : return "I_INVVPID";
case 378 : return "I_IRET";
case 166 : return "I_JA";
case 147 : return "I_JAE";
case 143 : return "I_JB";
case 161 : return "I_JBE";
case 427 : return "I_JCXZ";
case 433 : return "I_JECXZ";
case 202 : return "I_JG";
case 192 : return "I_JGE";
case 188 : return "I_JL";
case 197 : return "I_JLE";
case 462 : return "I_JMP";
case 467 : return "I_JMP_FAR";
case 138 : return "I_JNO";
case 183 : return "I_JNP";
case 174 : return "I_JNS";
case 156 : return "I_JNZ";
case 134 : return "I_JO";
case 179 : return "I_JP";
case 440 : return "I_JRCXZ";
case 170 : return "I_JS";
case 152 : return "I_JZ";
case 289 : return "I_LAHF";
case 522 : return "I_LAR";
case 7016 : return "I_LDDQU";
case 9944 : return "I_LDMXCSR";
case 335 : return "I_LDS";
case 223 : return "I_LEA";
case 347 : return "I_LEAVE";
case 330 : return "I_LES";
case 4287 : return "I_LFENCE";
case 917 : return "I_LFS";
case 1703 : return "I_LGDT";
case 922 : return "I_LGS";
case 1709 : return "I_LIDT";
case 1668 : return "I_LLDT";
case 1721 : return "I_LMSW";
case 313 : return "I_LODS";
case 421 : return "I_LOOP";
case 406 : return "I_LOOPNZ";
case 414 : return "I_LOOPZ";
case 527 : return "I_LSL";
case 907 : return "I_LSS";
case 1674 : return "I_LTR";
case 4385 : return "I_LZCNT";
case 7141 : return "I_MASKMOVDQU";
case 7131 : return "I_MASKMOVQ";
case 3581 : return "I_MAXPD";
case 3574 : return "I_MAXPS";
case 3595 : return "I_MAXSD";
case 3588 : return "I_MAXSS";
case 4313 : return "I_MFENCE";
case 3461 : return "I_MINPD";
case 3454 : return "I_MINPS";
case 3475 : return "I_MINSD";
case 3468 : return "I_MINSS";
case 1771 : return "I_MONITOR";
case 218 : return "I_MOV";
case 2481 : return "I_MOVAPD";
case 2473 : return "I_MOVAPS";
case 9273 : return "I_MOVBE";
case 3942 : return "I_MOVD";
case 2208 : return "I_MOVDDUP";
case 6544 : return "I_MOVDQ2Q";
case 3968 : return "I_MOVDQA";
case 3976 : return "I_MOVDQU";
case 2173 : return "I_MOVHLPS";
case 2367 : return "I_MOVHPD";
case 2359 : return "I_MOVHPS";
case 2350 : return "I_MOVLHPS";
case 2190 : return "I_MOVLPD";
case 2182 : return "I_MOVLPS";
case 2837 : return "I_MOVMSKPD";
case 2827 : return "I_MOVMSKPS";
case 6871 : return "I_MOVNTDQ";
case 7917 : return "I_MOVNTDQA";
case 952 : return "I_MOVNTI";
case 2578 : return "I_MOVNTPD";
case 2569 : return "I_MOVNTPS";
case 6863 : return "I_MOVNTQ";
case 2596 : return "I_MOVNTSD";
case 2587 : return "I_MOVNTSS";
case 3948 : return "I_MOVQ";
case 6535 : return "I_MOVQ2DQ";
case 295 : return "I_MOVS";
case 2132 : return "I_MOVSD";
case 2375 : return "I_MOVSHDUP";
case 2198 : return "I_MOVSLDUP";
case 2125 : return "I_MOVSS";
case 939 : return "I_MOVSX";
case 10027 : return "I_MOVSXD";
case 2117 : return "I_MOVUPD";
case 2109 : return "I_MOVUPS";
case 927 : return "I_MOVZX";
case 9650 : return "I_MPSADBW";
case 1641 : return "I_MUL";
case 3192 : return "I_MULPD";
case 3185 : return "I_MULPS";
case 3206 : return "I_MULSD";
case 3199 : return "I_MULSS";
case 1780 : return "I_MWAIT";
case 1636 : return "I_NEG";
case 581 : return "I_NOP";
case 1631 : return "I_NOT";
case 27 : return "I_OR";
case 3075 : return "I_ORPD";
case 3069 : return "I_ORPS";
case 451 : return "I_OUT";
case 128 : return "I_OUTS";
case 7710 : return "I_PABSB";
case 7740 : return "I_PABSD";
case 7725 : return "I_PABSW";
case 3871 : return "I_PACKSSDW";
case 3703 : return "I_PACKSSWB";
case 7938 : return "I_PACKUSDW";
case 3781 : return "I_PACKUSWB";
case 7226 : return "I_PADDB";
case 7256 : return "I_PADDD";
case 6503 : return "I_PADDQ";
case 6952 : return "I_PADDSB";
case 6969 : return "I_PADDSW";
case 6642 : return "I_PADDUSB";
case 6661 : return "I_PADDUSW";
case 7241 : return "I_PADDW";
case 9432 : return "I_PALIGNR";
case 6629 : return "I_PAND";
case 6687 : return "I_PANDN";
case 10035 : return "I_PAUSE";
case 6702 : return "I_PAVGB";
case 2100 : return "I_PAVGUSB";
case 6747 : return "I_PAVGW";
case 7621 : return "I_PBLENDVB";
case 9413 : return "I_PBLENDW";
case 9669 : return "I_PCLMULQDQ";
case 4065 : return "I_PCMPEQB";
case 4103 : return "I_PCMPEQD";
case 7898 : return "I_PCMPEQQ";
case 4084 : return "I_PCMPEQW";
case 9748 : return "I_PCMPESTRI";
case 9725 : return "I_PCMPESTRM";
case 3724 : return "I_PCMPGTB";
case 3762 : return "I_PCMPGTD";
case 8109 : return "I_PCMPGTQ";
case 3743 : return "I_PCMPGTW";
case 9794 : return "I_PCMPISTRI";
case 9771 : return "I_PCMPISTRM";
case 9451 : return "I_PEXTRB";
case 9468 : return "I_PEXTRD";
case 9476 : return "I_PEXTRQ";
case 6333 : return "I_PEXTRW";
case 1936 : return "I_PF2ID";
case 1929 : return "I_PF2IW";
case 2050 : return "I_PFACC";
case 1999 : return "I_PFADD";
case 2057 : return "I_PFCMPEQ";
case 1960 : return "I_PFCMPGE";
case 2006 : return "I_PFCMPGT";
case 2015 : return "I_PFMAX";
case 1969 : return "I_PFMIN";
case 2066 : return "I_PFMUL";
case 1943 : return "I_PFNACC";
case 1951 : return "I_PFPNACC";
case 1976 : return "I_PFRCP";
case 2022 : return "I_PFRCPIT1";
case 2073 : return "I_PFRCPIT2";
case 2032 : return "I_PFRSQIT1";
case 1983 : return "I_PFRSQRT";
case 1992 : return "I_PFSUB";
case 2042 : return "I_PFSUBR";
case 7397 : return "I_PHADDD";
case 7414 : return "I_PHADDSW";
case 7380 : return "I_PHADDW";
case 8281 : return "I_PHMINPOSUW";
case 7473 : return "I_PHSUBD";
case 7490 : return "I_PHSUBSW";
case 7456 : return "I_PHSUBW";
case 1922 : return "I_PI2FD";
case 1915 : return "I_PI2FW";
case 9552 : return "I_PINSRB";
case 9590 : return "I_PINSRD";
case 9598 : return "I_PINSRQ";
case 6316 : return "I_PINSRW";
case 7433 : return "I_PMADDUBSW";
case 7095 : return "I_PMADDWD";
case 8196 : return "I_PMAXSB";
case 8213 : return "I_PMAXSD";
case 6986 : return "I_PMAXSW";
case 6670 : return "I_PMAXUB";
case 8247 : return "I_PMAXUD";
case 8230 : return "I_PMAXUW";
case 8128 : return "I_PMINSB";
case 8145 : return "I_PMINSD";
case 6924 : return "I_PMINSW";
case 6612 : return "I_PMINUB";
case 8179 : return "I_PMINUD";
case 8162 : return "I_PMINUW";
case 6553 : return "I_PMOVMSKB";
case 7776 : return "I_PMOVSXBD";
case 7797 : return "I_PMOVSXBQ";
case 7755 : return "I_PMOVSXBW";
case 7860 : return "I_PMOVSXDQ";
case 7818 : return "I_PMOVSXWD";
case 7839 : return "I_PMOVSXWQ";
case 8004 : return "I_PMOVZXBD";
case 8025 : return "I_PMOVZXBQ";
case 7983 : return "I_PMOVZXBW";
case 8088 : return "I_PMOVZXDQ";
case 8046 : return "I_PMOVZXWD";
case 8067 : return "I_PMOVZXWQ";
case 7881 : return "I_PMULDQ";
case 7560 : return "I_PMULHRSW";
case 2083 : return "I_PMULHRW";
case 6762 : return "I_PMULHUW";
case 6781 : return "I_PMULHW";
case 8264 : return "I_PMULLD";
case 6518 : return "I_PMULLW";
case 7076 : return "I_PMULUDQ";
case 22 : return "I_POP";
case 98 : return "I_POPA";
case 4360 : return "I_POPCNT";
case 277 : return "I_POPF";
case 6941 : return "I_POR";
case 1894 : return "I_PREFETCH";
case 2424 : return "I_PREFETCHNTA";
case 2437 : return "I_PREFETCHT0";
case 2449 : return "I_PREFETCHT1";
case 2461 : return "I_PREFETCHT2";
case 1904 : return "I_PREFETCHW";
case 7114 : return "I_PSADBW";
case 7363 : return "I_PSHUFB";
case 4010 : return "I_PSHUFD";
case 4018 : return "I_PSHUFHW";
case 4027 : return "I_PSHUFLW";
case 4002 : return "I_PSHUFW";
case 7509 : return "I_PSIGNB";
case 7543 : return "I_PSIGND";
case 7526 : return "I_PSIGNW";
case 7046 : return "I_PSLLD";
case 9869 : return "I_PSLLDQ";
case 7061 : return "I_PSLLQ";
case 7031 : return "I_PSLLW";
case 6732 : return "I_PSRAD";
case 6717 : return "I_PSRAW";
case 6473 : return "I_PSRLD";
case 9852 : return "I_PSRLDQ";
case 6488 : return "I_PSRLQ";
case 6458 : return "I_PSRLW";
case 7166 : return "I_PSUBB";
case 7196 : return "I_PSUBD";
case 7211 : return "I_PSUBQ";
case 6890 : return "I_PSUBSB";
case 6907 : return "I_PSUBSW";
case 6574 : return "I_PSUBUSB";
case 6593 : return "I_PSUBUSW";
case 7181 : return "I_PSUBW";
case 2092 : return "I_PSWAPD";
case 7651 : return "I_PTEST";
case 3802 : return "I_PUNPCKHBW";
case 3848 : return "I_PUNPCKHDQ";
case 3917 : return "I_PUNPCKHQDQ";
case 3825 : return "I_PUNPCKHWD";
case 3634 : return "I_PUNPCKLBW";
case 3680 : return "I_PUNPCKLDQ";
case 3892 : return "I_PUNPCKLQDQ";
case 3657 : return "I_PUNPCKLWD";
case 16 : return "I_PUSH";
case 91 : return "I_PUSHA";
case 270 : return "I_PUSHF";
case 7003 : return "I_PXOR";
case 977 : return "I_RCL";
case 2975 : return "I_RCPPS";
case 2982 : return "I_RCPSS";
case 982 : return "I_RCR";
case 9904 : return "I_RDFSBASE";
case 9934 : return "I_RDGSBASE";
case 600 : return "I_RDMSR";
case 607 : return "I_RDPMC";
case 10048 : return "I_RDRAND";
case 593 : return "I_RDTSC";
case 1886 : return "I_RDTSCP";
case 325 : return "I_RET";
case 354 : return "I_RETF";
case 967 : return "I_ROL";
case 972 : return "I_ROR";
case 9318 : return "I_ROUNDPD";
case 9299 : return "I_ROUNDPS";
case 9356 : return "I_ROUNDSD";
case 9337 : return "I_ROUNDSS";
case 882 : return "I_RSM";
case 2937 : return "I_RSQRTPS";
case 2946 : return "I_RSQRTSS";
case 283 : return "I_SAHF";
case 997 : return "I_SAL";
case 394 : return "I_SALC";
case 1002 : return "I_SAR";
case 36 : return "I_SBB";
case 319 : return "I_SCAS";
case 807 : return "I_SETA";
case 780 : return "I_SETAE";
case 774 : return "I_SETB";
case 800 : return "I_SETBE";
case 859 : return "I_SETG";
case 845 : return "I_SETGE";
case 839 : return "I_SETL";
case 852 : return "I_SETLE";
case 767 : return "I_SETNO";
case 832 : return "I_SETNP";
case 819 : return "I_SETNS";
case 793 : return "I_SETNZ";
case 761 : return "I_SETO";
case 826 : return "I_SETP";
case 813 : return "I_SETS";
case 787 : return "I_SETZ";
case 4343 : return "I_SFENCE";
case 1691 : return "I_SGDT";
case 987 : return "I_SHL";
case 876 : return "I_SHLD";
case 992 : return "I_SHR";
case 892 : return "I_SHRD";
case 6358 : return "I_SHUFPD";
case 6350 : return "I_SHUFPS";
case 1697 : return "I_SIDT";
case 1861 : return "I_SKINIT";
case 1657 : return "I_SLDT";
case 1715 : return "I_SMSW";
case 2877 : return "I_SQRTPD";
case 2869 : return "I_SQRTPS";
case 2893 : return "I_SQRTSD";
case 2885 : return "I_SQRTSS";
case 497 : return "I_STC";
case 517 : return "I_STD";
case 1849 : return "I_STGI";
case 507 : return "I_STI";
case 9973 : return "I_STMXCSR";
case 307 : return "I_STOS";
case 1663 : return "I_STR";
case 51 : return "I_SUB";
case 3401 : return "I_SUBPD";
case 3394 : return "I_SUBPS";
case 3415 : return "I_SUBSD";
case 3408 : return "I_SUBSS";
case 1878 : return "I_SWAPGS";
case 532 : return "I_SYSCALL";
case 614 : return "I_SYSENTER";
case 624 : return "I_SYSEXIT";
case 547 : return "I_SYSRET";
case 206 : return "I_TEST";
case 4373 : return "I_TZCNT";
case 2764 : return "I_UCOMISD";
case 2755 : return "I_UCOMISS";
case 569 : return "I_UD2";
case 2318 : return "I_UNPCKHPD";
case 2308 : return "I_UNPCKHPS";
case 2276 : return "I_UNPCKLPD";
case 2266 : return "I_UNPCKLPS";
case 3161 : return "I_VADDPD";
case 3153 : return "I_VADDPS";
case 3177 : return "I_VADDSD";
case 3169 : return "I_VADDSS";
case 6436 : return "I_VADDSUBPD";
case 6447 : return "I_VADDSUBPS";
case 9239 : return "I_VAESDEC";
case 9260 : return "I_VAESDECLAST";
case 9197 : return "I_VAESENC";
case 9218 : return "I_VAESENCLAST";
case 9180 : return "I_VAESIMC";
case 9834 : return "I_VAESKEYGENASSIST";
case 3060 : return "I_VANDNPD";
case 3051 : return "I_VANDNPS";
case 3027 : return "I_VANDPD";
case 3019 : return "I_VANDPS";
case 9403 : return "I_VBLENDPD";
case 9384 : return "I_VBLENDPS";
case 9703 : return "I_VBLENDVPD";
case 9692 : return "I_VBLENDVPS";
case 7694 : return "I_VBROADCASTF128";
case 7680 : return "I_VBROADCASTSD";
case 7666 : return "I_VBROADCASTSS";
case 5110 : return "I_VCMPEQPD";
case 4708 : return "I_VCMPEQPS";
case 5914 : return "I_VCMPEQSD";
case 5512 : return "I_VCMPEQSS";
case 5291 : return "I_VCMPEQ_OSPD";
case 4889 : return "I_VCMPEQ_OSPS";
case 6095 : return "I_VCMPEQ_OSSD";
case 5693 : return "I_VCMPEQ_OSSS";
case 5197 : return "I_VCMPEQ_UQPD";
case 4795 : return "I_VCMPEQ_UQPS";
case 6001 : return "I_VCMPEQ_UQSD";
case 5599 : return "I_VCMPEQ_UQSS";
case 5400 : return "I_VCMPEQ_USPD";
case 4998 : return "I_VCMPEQ_USPS";
case 6204 : return "I_VCMPEQ_USSD";
case 5802 : return "I_VCMPEQ_USSS";
case 5232 : return "I_VCMPFALSEPD";
case 4830 : return "I_VCMPFALSEPS";
case 6036 : return "I_VCMPFALSESD";
case 5634 : return "I_VCMPFALSESS";
case 5441 : return "I_VCMPFALSE_OSPD";
case 5039 : return "I_VCMPFALSE_OSPS";
case 6245 : return "I_VCMPFALSE_OSSD";
case 5843 : return "I_VCMPFALSE_OSSS";
case 5259 : return "I_VCMPGEPD";
case 4857 : return "I_VCMPGEPS";
case 6063 : return "I_VCMPGESD";
case 5661 : return "I_VCMPGESS";
case 5471 : return "I_VCMPGE_OQPD";
case 5069 : return "I_VCMPGE_OQPS";
case 6275 : return "I_VCMPGE_OQSD";
case 5873 : return "I_VCMPGE_OQSS";
case 5269 : return "I_VCMPGTPD";
case 4867 : return "I_VCMPGTPS";
case 6073 : return "I_VCMPGTSD";
case 5671 : return "I_VCMPGTSS";
case 5484 : return "I_VCMPGT_OQPD";
case 5082 : return "I_VCMPGT_OQPS";
case 6288 : return "I_VCMPGT_OQSD";
case 5886 : return "I_VCMPGT_OQSS";
case 5130 : return "I_VCMPLEPD";
case 4728 : return "I_VCMPLEPS";
case 5934 : return "I_VCMPLESD";
case 5532 : return "I_VCMPLESS";
case 5317 : return "I_VCMPLE_OQPD";
case 4915 : return "I_VCMPLE_OQPS";
case 6121 : return "I_VCMPLE_OQSD";
case 5719 : return "I_VCMPLE_OQSS";
case 5120 : return "I_VCMPLTPD";
case 4718 : return "I_VCMPLTPS";
case 5924 : return "I_VCMPLTSD";
case 5522 : return "I_VCMPLTSS";
case 5304 : return "I_VCMPLT_OQPD";
case 4902 : return "I_VCMPLT_OQPS";
case 6108 : return "I_VCMPLT_OQSD";
case 5706 : return "I_VCMPLT_OQSS";
case 5153 : return "I_VCMPNEQPD";
case 4751 : return "I_VCMPNEQPS";
case 5957 : return "I_VCMPNEQSD";
case 5555 : return "I_VCMPNEQSS";
case 5245 : return "I_VCMPNEQ_OQPD";
case 4843 : return "I_VCMPNEQ_OQPS";
case 6049 : return "I_VCMPNEQ_OQSD";
case 5647 : return "I_VCMPNEQ_OQSS";
case 5457 : return "I_VCMPNEQ_OSPD";
case 5055 : return "I_VCMPNEQ_OSPS";
case 6261 : return "I_VCMPNEQ_OSSD";
case 5859 : return "I_VCMPNEQ_OSSS";
case 5345 : return "I_VCMPNEQ_USPD";
case 4943 : return "I_VCMPNEQ_USPS";
case 6149 : return "I_VCMPNEQ_USSD";
case 5747 : return "I_VCMPNEQ_USSS";
case 5210 : return "I_VCMPNGEPD";
case 4808 : return "I_VCMPNGEPS";
case 6014 : return "I_VCMPNGESD";
case 5612 : return "I_VCMPNGESS";
case 5413 : return "I_VCMPNGE_UQPD";
case 5011 : return "I_VCMPNGE_UQPS";
case 6217 : return "I_VCMPNGE_UQSD";
case 5815 : return "I_VCMPNGE_UQSS";
case 5221 : return "I_VCMPNGTPD";
case 4819 : return "I_VCMPNGTPS";
case 6025 : return "I_VCMPNGTSD";
case 5623 : return "I_VCMPNGTSS";
case 5427 : return "I_VCMPNGT_UQPD";
case 5025 : return "I_VCMPNGT_UQPS";
case 6231 : return "I_VCMPNGT_UQSD";
case 5829 : return "I_VCMPNGT_UQSS";
case 5175 : return "I_VCMPNLEPD";
case 4773 : return "I_VCMPNLEPS";
case 5979 : return "I_VCMPNLESD";
case 5577 : return "I_VCMPNLESS";
case 5373 : return "I_VCMPNLE_UQPD";
case 4971 : return "I_VCMPNLE_UQPS";
case 6177 : return "I_VCMPNLE_UQSD";
case 5775 : return "I_VCMPNLE_UQSS";
case 5164 : return "I_VCMPNLTPD";
case 4762 : return "I_VCMPNLTPS";
case 5968 : return "I_VCMPNLTSD";
case 5566 : return "I_VCMPNLTSS";
case 5359 : return "I_VCMPNLT_UQPD";
case 4957 : return "I_VCMPNLT_UQPS";
case 6163 : return "I_VCMPNLT_UQSD";
case 5761 : return "I_VCMPNLT_UQSS";
case 5186 : return "I_VCMPORDPD";
case 4784 : return "I_VCMPORDPS";
case 5990 : return "I_VCMPORDSD";
case 5588 : return "I_VCMPORDSS";
case 5387 : return "I_VCMPORD_SPD";
case 4985 : return "I_VCMPORD_SPS";
case 6191 : return "I_VCMPORD_SSD";
case 5789 : return "I_VCMPORD_SSS";
case 5279 : return "I_VCMPTRUEPD";
case 4877 : return "I_VCMPTRUEPS";
case 6083 : return "I_VCMPTRUESD";
case 5681 : return "I_VCMPTRUESS";
case 5497 : return "I_VCMPTRUE_USPD";
case 5095 : return "I_VCMPTRUE_USPS";
case 6301 : return "I_VCMPTRUE_USSD";
case 5899 : return "I_VCMPTRUE_USSS";
case 5140 : return "I_VCMPUNORDPD";
case 4738 : return "I_VCMPUNORDPS";
case 5944 : return "I_VCMPUNORDSD";
case 5542 : return "I_VCMPUNORDSS";
case 5330 : return "I_VCMPUNORD_SPD";
case 4928 : return "I_VCMPUNORD_SPS";
case 6134 : return "I_VCMPUNORD_SSD";
case 5732 : return "I_VCMPUNORD_SSS";
case 2818 : return "I_VCOMISD";
case 2809 : return "I_VCOMISS";
case 6841 : return "I_VCVTDQ2PD";
case 3360 : return "I_VCVTDQ2PS";
case 6852 : return "I_VCVTPD2DQ";
case 3296 : return "I_VCVTPD2PS";
case 3371 : return "I_VCVTPS2DQ";
case 3285 : return "I_VCVTPS2PD";
case 2744 : return "I_VCVTSD2SI";
case 3318 : return "I_VCVTSD2SS";
case 2558 : return "I_VCVTSI2SD";
case 2547 : return "I_VCVTSI2SS";
case 3307 : return "I_VCVTSS2SD";
case 2733 : return "I_VCVTSS2SI";
case 6829 : return "I_VCVTTPD2DQ";
case 3382 : return "I_VCVTTPS2DQ";
case 2681 : return "I_VCVTTSD2SI";
case 2669 : return "I_VCVTTSS2SI";
case 3550 : return "I_VDIVPD";
case 3542 : return "I_VDIVPS";
case 3566 : return "I_VDIVSD";
case 3558 : return "I_VDIVSS";
case 9643 : return "I_VDPPD";
case 9630 : return "I_VDPPS";
case 1679 : return "I_VERR";
case 1685 : return "I_VERW";
case 9538 : return "I_VEXTRACTF128";
case 9513 : return "I_VEXTRACTPS";
case 8409 : return "I_VFMADD132PD";
case 8396 : return "I_VFMADD132PS";
case 8435 : return "I_VFMADD132SD";
case 8422 : return "I_VFMADD132SS";
case 8689 : return "I_VFMADD213PD";
case 8676 : return "I_VFMADD213PS";
case 8715 : return "I_VFMADD213SD";
case 8702 : return "I_VFMADD213SS";
case 8969 : return "I_VFMADD231PD";
case 8956 : return "I_VFMADD231PS";
case 8995 : return "I_VFMADD231SD";
case 8982 : return "I_VFMADD231SS";
case 8348 : return "I_VFMADDSUB132PD";
case 8332 : return "I_VFMADDSUB132PS";
case 8628 : return "I_VFMADDSUB213PD";
case 8612 : return "I_VFMADDSUB213PS";
case 8908 : return "I_VFMADDSUB231PD";
case 8892 : return "I_VFMADDSUB231PS";
case 8461 : return "I_VFMSUB132PD";
case 8448 : return "I_VFMSUB132PS";
case 8487 : return "I_VFMSUB132SD";
case 8474 : return "I_VFMSUB132SS";
case 8741 : return "I_VFMSUB213PD";
case 8728 : return "I_VFMSUB213PS";
case 8767 : return "I_VFMSUB213SD";
case 8754 : return "I_VFMSUB213SS";
case 9021 : return "I_VFMSUB231PD";
case 9008 : return "I_VFMSUB231PS";
case 9047 : return "I_VFMSUB231SD";
case 9034 : return "I_VFMSUB231SS";
case 8380 : return "I_VFMSUBADD132PD";
case 8364 : return "I_VFMSUBADD132PS";
case 8660 : return "I_VFMSUBADD213PD";
case 8644 : return "I_VFMSUBADD213PS";
case 8940 : return "I_VFMSUBADD231PD";
case 8924 : return "I_VFMSUBADD231PS";
case 8514 : return "I_VFNMADD132PD";
case 8500 : return "I_VFNMADD132PS";
case 8542 : return "I_VFNMADD132SD";
case 8528 : return "I_VFNMADD132SS";
case 8794 : return "I_VFNMADD213PD";
case 8780 : return "I_VFNMADD213PS";
case 8822 : return "I_VFNMADD213SD";
case 8808 : return "I_VFNMADD213SS";
case 9074 : return "I_VFNMADD231PD";
case 9060 : return "I_VFNMADD231PS";
case 9102 : return "I_VFNMADD231SD";
case 9088 : return "I_VFNMADD231SS";
case 8570 : return "I_VFNMSUB132PD";
case 8556 : return "I_VFNMSUB132PS";
case 8598 : return "I_VFNMSUB132SD";
case 8584 : return "I_VFNMSUB132SS";
case 8850 : return "I_VFNMSUB213PD";
case 8836 : return "I_VFNMSUB213PS";
case 8878 : return "I_VFNMSUB213SD";
case 8864 : return "I_VFNMSUB213SS";
case 9130 : return "I_VFNMSUB231PD";
case 9116 : return "I_VFNMSUB231PS";
case 9158 : return "I_VFNMSUB231SD";
case 9144 : return "I_VFNMSUB231SS";
case 4219 : return "I_VHADDPD";
case 4228 : return "I_VHADDPS";
case 4253 : return "I_VHSUBPD";
case 4262 : return "I_VHSUBPS";
case 9525 : return "I_VINSERTF128";
case 9579 : return "I_VINSERTPS";
case 7023 : return "I_VLDDQU";
case 9963 : return "I_VLDMXCSR";
case 7153 : return "I_VMASKMOVDQU";
case 7971 : return "I_VMASKMOVPD";
case 7959 : return "I_VMASKMOVPS";
case 3610 : return "I_VMAXPD";
case 3602 : return "I_VMAXPS";
case 3626 : return "I_VMAXSD";
case 3618 : return "I_VMAXSS";
case 1735 : return "I_VMCALL";
case 10011 : return "I_VMCLEAR";
case 1803 : return "I_VMFUNC";
case 3490 : return "I_VMINPD";
case 3482 : return "I_VMINPS";
case 3506 : return "I_VMINSD";
case 3498 : return "I_VMINSS";
case 1743 : return "I_VMLAUNCH";
case 1833 : return "I_VMLOAD";
case 1824 : return "I_VMMCALL";
case 2498 : return "I_VMOVAPD";
case 2489 : return "I_VMOVAPS";
case 3954 : return "I_VMOVD";
case 2256 : return "I_VMOVDDUP";
case 3984 : return "I_VMOVDQA";
case 3993 : return "I_VMOVDQU";
case 2217 : return "I_VMOVHLPS";
case 2404 : return "I_VMOVHPD";
case 2395 : return "I_VMOVHPS";
case 2385 : return "I_VMOVLHPS";
case 2236 : return "I_VMOVLPD";
case 2227 : return "I_VMOVLPS";
case 2858 : return "I_VMOVMSKPD";
case 2847 : return "I_VMOVMSKPS";
case 6880 : return "I_VMOVNTDQ";
case 7927 : return "I_VMOVNTDQA";
case 2615 : return "I_VMOVNTPD";
case 2605 : return "I_VMOVNTPS";
case 3961 : return "I_VMOVQ";
case 2165 : return "I_VMOVSD";
case 2413 : return "I_VMOVSHDUP";
case 2245 : return "I_VMOVSLDUP";
case 2157 : return "I_VMOVSS";
case 2148 : return "I_VMOVUPD";
case 2139 : return "I_VMOVUPS";
case 9659 : return "I_VMPSADBW";
case 10002 : return "I_VMPTRLD";
case 6407 : return "I_VMPTRST";
case 4150 : return "I_VMREAD";
case 1753 : return "I_VMRESUME";
case 1817 : return "I_VMRUN";
case 1841 : return "I_VMSAVE";
case 3221 : return "I_VMULPD";
case 3213 : return "I_VMULPS";
case 3237 : return "I_VMULSD";
case 3229 : return "I_VMULSS";
case 4174 : return "I_VMWRITE";
case 1763 : return "I_VMXOFF";
case 10020 : return "I_VMXON";
case 3088 : return "I_VORPD";
case 3081 : return "I_VORPS";
case 7717 : return "I_VPABSB";
case 7747 : return "I_VPABSD";
case 7732 : return "I_VPABSW";
case 3881 : return "I_VPACKSSDW";
case 3713 : return "I_VPACKSSWB";
case 7948 : return "I_VPACKUSDW";
case 3791 : return "I_VPACKUSWB";
case 7233 : return "I_VPADDB";
case 7263 : return "I_VPADDD";
case 6510 : return "I_VPADDQ";
case 6960 : return "I_VPADDSB";
case 6977 : return "I_VPADDSW";
case 6651 : return "I_VPADDUSW";
case 7248 : return "I_VPADDW";
case 9441 : return "I_VPALIGNR";
case 6635 : return "I_VPAND";
case 6694 : return "I_VPANDN";
case 6709 : return "I_VPAVGB";
case 6754 : return "I_VPAVGW";
case 9714 : return "I_VPBLENDVB";
case 9422 : return "I_VPBLENDW";
case 9680 : return "I_VPCLMULQDQ";
case 4074 : return "I_VPCMPEQB";
case 4112 : return "I_VPCMPEQD";
case 7907 : return "I_VPCMPEQQ";
case 4093 : return "I_VPCMPEQW";
case 9759 : return "I_VPCMPESTRI";
case 9736 : return "I_VPCMPESTRM";
case 3733 : return "I_VPCMPGTB";
case 3771 : return "I_VPCMPGTD";
case 8118 : return "I_VPCMPGTQ";
case 3752 : return "I_VPCMPGTW";
case 9805 : return "I_VPCMPISTRI";
case 9782 : return "I_VPCMPISTRM";
case 9287 : return "I_VPERM2F128";
case 7592 : return "I_VPERMILPD";
case 7581 : return "I_VPERMILPS";
case 9459 : return "I_VPEXTRB";
case 9484 : return "I_VPEXTRD";
case 9493 : return "I_VPEXTRQ";
case 6341 : return "I_VPEXTRW";
case 7405 : return "I_VPHADDD";
case 7423 : return "I_VPHADDSW";
case 7388 : return "I_VPHADDW";
case 8293 : return "I_VPHMINPOSUW";
case 7481 : return "I_VPHSUBD";
case 7499 : return "I_VPHSUBSW";
case 7464 : return "I_VPHSUBW";
case 9560 : return "I_VPINSRB";
case 9606 : return "I_VPINSRD";
case 9615 : return "I_VPINSRQ";
case 6324 : return "I_VPINSRW";
case 7444 : return "I_VPMADDUBSW";
case 7104 : return "I_VPMADDWD";
case 8204 : return "I_VPMAXSB";
case 8221 : return "I_VPMAXSD";
case 6994 : return "I_VPMAXSW";
case 6678 : return "I_VPMAXUB";
case 8255 : return "I_VPMAXUD";
case 8238 : return "I_VPMAXUW";
case 8136 : return "I_VPMINSB";
case 8153 : return "I_VPMINSD";
case 6932 : return "I_VPMINSW";
case 6620 : return "I_VPMINUB";
case 8187 : return "I_VPMINUD";
case 8170 : return "I_VPMINUW";
case 6563 : return "I_VPMOVMSKB";
case 7786 : return "I_VPMOVSXBD";
case 7807 : return "I_VPMOVSXBQ";
case 7765 : return "I_VPMOVSXBW";
case 7870 : return "I_VPMOVSXDQ";
case 7828 : return "I_VPMOVSXWD";
case 7849 : return "I_VPMOVSXWQ";
case 8014 : return "I_VPMOVZXBD";
case 8035 : return "I_VPMOVZXBQ";
case 7993 : return "I_VPMOVZXBW";
case 8098 : return "I_VPMOVZXDQ";
case 8056 : return "I_VPMOVZXWD";
case 8077 : return "I_VPMOVZXWQ";
case 7889 : return "I_VPMULDQ";
case 7570 : return "I_VPMULHRSW";
case 6771 : return "I_VPMULHUW";
case 6789 : return "I_VPMULHW";
case 8272 : return "I_VPMULLD";
case 6526 : return "I_VPMULLW";
case 7085 : return "I_VPMULUDQ";
case 6946 : return "I_VPOR";
case 7122 : return "I_VPSADBW";
case 7371 : return "I_VPSHUFB";
case 4036 : return "I_VPSHUFD";
case 4045 : return "I_VPSHUFHW";
case 4055 : return "I_VPSHUFLW";
case 7517 : return "I_VPSIGNB";
case 7551 : return "I_VPSIGND";
case 7534 : return "I_VPSIGNW";
case 7053 : return "I_VPSLLD";
case 9877 : return "I_VPSLLDQ";
case 7068 : return "I_VPSLLQ";
case 7038 : return "I_VPSLLW";
case 6739 : return "I_VPSRAD";
case 6724 : return "I_VPSRAW";
case 6480 : return "I_VPSRLD";
case 9860 : return "I_VPSRLDQ";
case 6495 : return "I_VPSRLQ";
case 6465 : return "I_VPSRLW";
case 7173 : return "I_VPSUBB";
case 7203 : return "I_VPSUBD";
case 7218 : return "I_VPSUBQ";
case 6898 : return "I_VPSUBSB";
case 6915 : return "I_VPSUBSW";
case 6583 : return "I_VPSUBUSB";
case 6602 : return "I_VPSUBUSW";
case 7188 : return "I_VPSUBW";
case 7658 : return "I_VPTEST";
case 3813 : return "I_VPUNPCKHBW";
case 3859 : return "I_VPUNPCKHDQ";
case 3929 : return "I_VPUNPCKHQDQ";
case 3836 : return "I_VPUNPCKHWD";
case 3645 : return "I_VPUNPCKLBW";
case 3691 : return "I_VPUNPCKLDQ";
case 3904 : return "I_VPUNPCKLQDQ";
case 3668 : return "I_VPUNPCKLWD";
case 7009 : return "I_VPXOR";
case 2989 : return "I_VRCPPS";
case 2997 : return "I_VRCPSS";
case 9327 : return "I_VROUNDPD";
case 9308 : return "I_VROUNDPS";
case 9365 : return "I_VROUNDSD";
case 9346 : return "I_VROUNDSS";
case 2955 : return "I_VRSQRTPS";
case 2965 : return "I_VRSQRTSS";
case 6375 : return "I_VSHUFPD";
case 6366 : return "I_VSHUFPS";
case 2910 : return "I_VSQRTPD";
case 2901 : return "I_VSQRTPS";
case 2928 : return "I_VSQRTSD";
case 2919 : return "I_VSQRTSS";
case 9992 : return "I_VSTMXCSR";
case 3430 : return "I_VSUBPD";
case 3422 : return "I_VSUBPS";
case 3446 : return "I_VSUBSD";
case 3438 : return "I_VSUBSS";
case 7612 : return "I_VTESTPD";
case 7603 : return "I_VTESTPS";
case 2783 : return "I_VUCOMISD";
case 2773 : return "I_VUCOMISS";
case 2339 : return "I_VUNPCKHPD";
case 2328 : return "I_VUNPCKHPS";
case 2297 : return "I_VUNPCKLPD";
case 2286 : return "I_VUNPCKLPS";
case 3117 : return "I_VXORPD";
case 3109 : return "I_VXORPS";
case 4140 : return "I_VZEROALL";
case 4128 : return "I_VZEROUPPER";
case 10042 : return "I_WAIT";
case 561 : return "I_WBINVD";
case 9953 : return "I_WRFSBASE";
case 9982 : return "I_WRGSBASE";
case 586 : return "I_WRMSR";
case 1007 : return "I_XABORT";
case 946 : return "I_XADD";
case 1015 : return "I_XBEGIN";
case 212 : return "I_XCHG";
case 1811 : return "I_XEND";
case 1787 : return "I_XGETBV";
case 400 : return "I_XLAT";
case 61 : return "I_XOR";
case 3102 : return "I_XORPD";
case 3095 : return "I_XORPS";
case 4295 : return "I_XRSTOR";
case 4303 : return "I_XRSTOR64";
case 4271 : return "I_XSAVE";
case 4278 : return "I_XSAVE64";
case 4321 : return "I_XSAVEOPT";
case 4331 : return "I_XSAVEOPT64";
case 1795 : return "I_XSETBV";
case 10056 : return "I__3DNOW";
}
 return "UNKNOWN";
}  
