#include <stdio.h>
#include <iostream>
#include <cstddef>

using namespace std;

/* Test program to get file opening working */
int main(int argc, char *argv[])
{
  FILE *file = NULL;
  
  if ((file = fopen(argv[0], "rb")) == NULL)
    cout << "Could not open specified file" << endl;
  else
    cout << "File opened successfully" << endl;

  fseek(file, 0, SEEK_END);
  cout << ftell(file) << endl;  
}
