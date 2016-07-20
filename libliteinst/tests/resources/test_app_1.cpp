
#include <cstdio>
#include "tests.hpp"

extern void dummy();

void six_bytes_func() {
  __asm("nop");
  __asm("nop");
  __asm("nop");
  __asm("nop");
  __asm("nop");
  return;
}

void five_bytes_func() {
  __asm("nop");
  __asm("nop");
  __asm("nop");
  __asm("nop");
  return;
}

int main() {

  // Do nothing.. 
  printf("Inside main..\n");

  dummy();

  five_bytes_func();
  six_bytes_func();

  // boostrap();
}
