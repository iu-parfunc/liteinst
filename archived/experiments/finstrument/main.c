#include <stdio.h>
 
inline void foo() {
 printf("foo\n");
}
 
__attribute__((always_inline))
int inc (int *a){
 (*a)++;
}

int main() {
 
 // foo();
 //foo();
 int x = 0;
 inc(&x); 
 inc(&x); 
 inc(&x); 

 return 0;
}
