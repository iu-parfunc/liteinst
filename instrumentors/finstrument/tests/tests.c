
#include <stdio.h>

int call_count = 0;

void print_fn(short);

void foo() {
  printf("foo : %d\n", call_count++);
}

__attribute__((always_inline))
int inc (int *a){
  (*a)++;
  printf("a is %d\n", *a);
  return *a;
}

int main() {

  int x = 0;
  // inc(&x);
  // inc(&x);

  foo();
  foo();

  // inc(&x);
  // inc(&x);

  foo();
  foo();

  // print_fn(5554);
  // print_fn(5554);

  return 0;
}
