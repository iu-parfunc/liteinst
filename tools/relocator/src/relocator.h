#ifndef __RELOCATOR__INCLUDED
#define __RELOCATOR__INCLUDED

#include <stdio.h> 
#include <stdlib.h> 
#include <memory.h>

extern int relocate(unsigned char *dst, unsigned char *src,size_t n);
extern unsigned int count_relocatable(unsigned char *addr,size_t nMax); 
extern void relocate_info();

#endif 
