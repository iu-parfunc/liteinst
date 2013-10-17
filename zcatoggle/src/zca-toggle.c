#include <stdio.h>
#include "zca-toggle.h"

/* This is a normal function */
void myFunc() {
    printf("And finally, this text is printed from a regular function in the library.\n");
    return;
}

int activateProbe(const char* ann) {
  return 0;
}

/* This function is called automatically when the library is loaded */
void initZCAService() {
    printf("This text is printed before reaching \"main\".\n");
    return;
}
