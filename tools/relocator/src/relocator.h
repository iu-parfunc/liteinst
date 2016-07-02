#ifndef __RELOCATOR__INCLUDED
#define __RELOCATOR__INCLUDED

#include <stdio.h> 
#include <stdlib.h> 
#include <memory.h>
#include <stdint.h>

extern int relocate(unsigned char *dst, 
		    unsigned char *src,
		    unsigned char *epilogue, 
		    size_t epilogue_size, 
		    size_t n);

extern int count_relocatable(unsigned char *addr,size_t nMax); 
extern int relocatable(unsigned char *addr, 
		       size_t nMax, 
		       unsigned int  *n_relocatable, 
		       unsigned int  *n_relocatable_bytes
		       );

extern int instruction_offsets(unsigned char *addr, 
                                        uint32_t *offs, 
                                        size_t nMax);


extern void relocate_info();

#endif 
