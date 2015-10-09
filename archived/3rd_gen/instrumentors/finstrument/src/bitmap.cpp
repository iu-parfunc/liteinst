/**
 * @file bitmap.cpp
 * @author Buddhika Chamith
 * @brief
 *
 */
#include "bitmap.hpp" 

#include <stdlib.h> 

Bitmap create_bitmap(unsigned int num_bits){ 
  /* number of whole bytes to allocate */ 
  unsigned int num_bytes = num_bits / 8; 
  /* do we need a few extra bits ?, if so add another byte */ 
  num_bytes += num_bits % 8 == 0 ? 0 : 1; 

  return ((Bitmap)malloc(num_bytes)); 
}

void destroy_bitmap(Bitmap b){
  free(b); 
}

void set_index(Bitmap bitmap, int idx) {
  int int_offset = idx / 8;
  int bit_offset = idx - int_offset * 8;

  bitmap[int_offset] |= (0x80 >> bit_offset);
}

int get_index(Bitmap bitmap, int idx) {
  int int_offset = idx / 8;
  int bit_offset = idx - int_offset * 8;

  return(((bitmap[int_offset] << bit_offset) & 0x80) >> 7);
}
