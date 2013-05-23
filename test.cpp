#include <stdio.h>
#include <zca.h>

int main(void)
{
  int x = 5;
  __notify_intrinsic((void*)"entered region", (void*)&x);
  printf("We are the borg.\n");
  __notify_intrinsic((void*)"exited", &x);

  printf("Done.\n");
}
