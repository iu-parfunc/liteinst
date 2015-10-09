#include <stdio.h>

int main()
{
  __int128 a = 1, b;
  b = __sync_val_compare_and_swap(&a, 1, 2);
  printf("Returned value = %d, New value = %d\n", (int)b, (int)a);
  return 0;
}
