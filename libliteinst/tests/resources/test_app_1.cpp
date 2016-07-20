
#include <cstdio>
#include "tests.hpp"

extern void dummy();

void five_bytes_func() {
  __asm(".int 0x00401f0f");
  return;
}

void six_bytes_func() {
  __asm(".int 0x00401f0f");
  __asm(".byte 0x90");
  return;
}

void seven_bytes_func() {
  __asm(".int 0x00401f0f");
  __asm(".short 0x9066");
  return;
}

void eight_bytes_func() {
  __asm(".int 0x00401f0f");
  __asm(".short 0x9066");
  __asm(".byte 0x90");
  return;
}

void nine_bytes_func() {
  __asm(".int 0x00401f0f");
  __asm(".int 0x00401f0f");
  return;
}

void ten_bytes_func() {
  __asm(".int 0x00401f0f");
  __asm(".int 0x00401f0f");
  __asm(".byte 0x90");
  return;
}

void eleven_bytes_func() {
  __asm(".int 0x00401f0f");
  __asm(".int 0x00401f0f");
  __asm(".short 0x9066");
  return;
}

void tweleve_bytes_func() {
  __asm(".int 0x00401f0f");
  __asm(".int 0x00401f0f");
  __asm(".short 0x9066");
  __asm(".byte 0x90");
  return;
}

int main() {

  // Do nothing.. 
  printf("Inside main..\n");

  dummy();

  eight_bytes_func();
  // five_bytes_func();
  // six_bytes_func();

  // boostrap();
}
