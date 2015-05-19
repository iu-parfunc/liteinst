
#include "bitmap.hpp" 

void set_index(uint8_t* bitmap, int idx) {
  int int_offset = idx / 8;
  int bit_offset = idx - int_offset * 8;

  bitmap[int_offset] |= (0x80 >> bit_offset);
}

int get_index(uint8_t* bitmap, int idx) {
  int int_offset = idx / 8;
  int bit_offset = idx - int_offset * 8;

  return(((bitmap[int_offset] << bit_offset) & 0x80) >> 7);
}
