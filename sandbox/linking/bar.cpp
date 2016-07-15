
#include <stdio.h>

__attribute__((constructor))
void bar() {
  printf("Inside bar..\n");
}
