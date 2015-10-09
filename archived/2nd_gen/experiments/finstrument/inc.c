#include <stdio.h>

inline int fetch_and_add( int * variable, int value ) {
     asm volatile("lock; xaddl %%eax, %2;"
                  :"=a" (value)                  //Output
                  :"a" (value), "m" (*variable)  //Input
                  :"memory");
     return value;
}

int main() {
  int x = 0; 
  fetch_and_add(&x, 2);
  printf("X : %d\n", x);
}
