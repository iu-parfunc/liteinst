
#include <stdio.h>

int foo() {
  int x=1;
  int y = x + x << 1;
  return y;
}

int main() {
  int y = foo();
  printf("y is %d\n", y);
  
  return 0;
}
