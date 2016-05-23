#ifndef __RELOCATOR__INCLUDED
#define __RELOCATOR__INCLUDED

#include <stdio.h> 
#include <stdlib.h> 
#include <memory.h>

extern void relocate(unsigned char *dst, unsigned char *src,size_t n);
extern int relocate_function(unsigned char *dst, unsigned char *src,size_t n);
extern void relocate_info();

#endif 
