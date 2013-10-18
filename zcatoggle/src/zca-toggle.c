
#include <stdio.h>
#include "zca-toggle.h"
#include "zca-types.h"

int activateProbe(const char* ann, void (*fptr)(const char*, void*)) {
  return 0;
}

int deactivateProbe(const char* ann) {
  return 0;
}


/* This function is called automatically when the library is loaded */
void initZCAService() {
    printf("This text is printed before reaching \"main\".\n");
    return;
}
