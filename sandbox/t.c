
#include <stdio.h>
#include <stdlib.h>

long foo() {
  return 234;
}

int main() {

  asm("movq $0x0000000000400763, %r10");
  asm("cmp %r10, %rax");
  asm(".long 0xc35d0274");
  //  asm(".int 0xc35d");

  printf("Hit me..\n");
  if (foo() < 1) {
    return -1;
  } else {
    exit(0);
  }
  return 0;
}
