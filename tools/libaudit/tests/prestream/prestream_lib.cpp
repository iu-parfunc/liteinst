
#include <cstdio>
#include <iostream>

using namespace std;

__attribute__((constructor))
void prestream() {
  fprintf(stderr, "Hello from pre stream library..\n");
  // cout<< "Hello from pre stream library..\n";
}
