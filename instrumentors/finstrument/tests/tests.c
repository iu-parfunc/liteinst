
#include <stdio.h>

int call_count = 0;

void foo() {
  printf("foo : %d\n", call_count++);
}

// __attribute__((always_inline))
int inc (int *a){
  (*a)++;
  printf("a is %d\n", *a);
  return *a;
}

int main() {

  /*
  int x = 0;
  inc(&x);
  inc(&x);
  inc(&x);
  inc(&x);
  */
  foo();
  foo();
  foo();
  foo();

  return 0;
}
