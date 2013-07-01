
// The idea here is to link enough libraries to run libZCA's
// GetMetadata and retrieve the ZCA table.

#include <stdio.h>
#include <zca.h>
#include <iostream>
#include <cstddef>

using namespace ZCA;

int main(int argc, char *argv[])
{
  int x = 5;
  __notify_intrinsic((void*)"entered region", (void*)&x);
  printf("[app] We are the borg.\n");
  __notify_intrinsic((void*)"exited", &x);

  LEVEL_PINCLIENT::IMG img; 
  METADATA met = GetMetadata(img); 
  PrintMetadata(met);     

  printf("[app] Done.\n");

/*

  // Instrumentation comes here 
  typedef unsigned char BYTE;
  FILE *file = NULL;
  FILE *out = NULL;
  BYTE *fileBuf;
  unsigned long fileSize;
    
  // Need to open in r+b
  if ((file = fopen(argv[0], "rb")) == NULL)
    cout << "Could not open binary" << endl;
  else
    cout << "Binary opened successfully" << endl;

  if ((out = fopen("out.exe", "wb")) == NULL)
    cout << "Could not open output file" << endl;
  else
    cout << "Output file opened successfully" << endl;
  
  fseek(file, 0, SEEK_END);
  fileSize = ftell(file);
  cout << fileSize << endl;
  fseek(file, 0, SEEK_SET);
  
  //fseek(file, 0, 0x400ab0);
  //cout << ftell(file) << endl;

  fileBuf = new BYTE[fileSize];
  fread(fileBuf, 1, fileSize, file);
  
  fwrite(fileBuf, 1, fileSize, out);
  
  fclose(file);
  fclose(out);
    
  delete[] fileBuf;
*/
}
