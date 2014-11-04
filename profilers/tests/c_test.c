
#include <stdio.h>
#include <setjmp.h>
#include <alloca.h>

int call_count = 0;

void print_fn(short);

void foo(int i) {
  i= (i + call_count>> 2);
  printf("foo : %d\n", call_count++);
  // alloca(1);

  // jmp_buf env;

  /*
  int x;
  static int* p;
  p = &x;
  */

  // setjmp(env);
  // return i;
}

/*
__attribute__((always_inline))
int inc (int *a){
  (*a)++;
  printf("a is %d\n", *a);
  return *a;
}
*/

int main() {

  fprintf(stderr, "Entering main....\n");

  int x = 0;
  //inc(&x);
  // inc(&x);

  // foo(2);
  // foo(4);

  // inc(&x);
  // inc(&x);

  // foo(2);
  // foo(2);
  // foo(2);

  // foo(2);
  int i=0;
  for (i=0; i< 100000; i++) {
    foo(2);
  }

  // print_fn(5554);
  // print_fn(5554);

  return 0;
}
