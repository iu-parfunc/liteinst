

#include <stdio.h>

int main () {
  printf("hello world with a notify instruction\n");
  __notify_intrinsic("notify01",(void*)99);
  printf("Filler instructions");
  __notify_intrinsic("notify02",(void*)3299);
}
