#include <stdio.h>
#include <zca.h>

void testInsert()
{
  printf("Dyninst insert worked.\n");
}

int main(void)
{
  int x = 5;
  __notify_intrinsic((void*)"entered region", (void*)&x);
  printf("[app] We are the borg.\n");
  __notify_intrinsic((void*)"exited", &x);

  printf("[app] Done.\n");
}
