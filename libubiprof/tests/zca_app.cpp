
#include <stdio.h>

int global_x = 0;

extern void initProfiler();

void foo() {
  __notify_intrinsic((void*)"foo:entry", (void *)&global_x);
  fprintf(stderr, "Inside foo..\n");
  __notify_intrinsic((void*)"foo:exit", (void *)&global_x);
}

int main() {

  initProfiler();
  __notify_intrinsic((void*)"main:entry", (void *)&global_x);
  fprintf(stderr, "Inside main..\n");
  __notify_intrinsic((void*)"main:exit", (void *)&global_x);
  foo();

}
