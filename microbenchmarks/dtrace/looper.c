#include <strings.h>

int func(char* input) {
  // return (strlen(input)+1);
  return 99;
}

int main() {
  int i,j,sum = 0;
  for (i=0; i<1000000; i++) {
    for (j=0; j<100; j++) {
      sum +=func("hello Dtrace");
    }
  }

  return (0);
}
