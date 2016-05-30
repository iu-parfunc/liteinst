
#include <stdio.h>

__attribute__ ((constructor)) void premain(void) {
  printf("Before main..\n");
}

void test() {

}
