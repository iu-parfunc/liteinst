#include <stdio.h>
#include <zca.h>
#include <iostream>
#include <cstddef>

int main(int argc, char *argv[])
{
  int x = 5;
  __notify_intrinsic((void*)"entered region", (void*)&x);
  printf("[app] We are the borg.\n");
  __notify_intrinsic((void*)"exited", &x);

  printf("[app] Done.\n");

  FILE *file = NULL;
  
  if ((file = fopen(argv[0], "rb")) == NULL)
    cout << "Could not open specified file" << endl;
  else
    cout << "File opened successfully" << endl;

  fseek(file, 0, SEEK_END);
  cout << ftell(file) << endl;

  fseek(file, 0, 0x400ab0);
  cout << ftell(file) << endl;
}
