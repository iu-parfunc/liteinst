#include <stdio.h>

typedef void (*FuncPtr)(void);
void _Func(void){
  printf("Hello\n");
}

int main(int argc, char *argv[]){
  register FuncPtr func asm ("rdi") = _Func;
  func();

  return 0;
} 
