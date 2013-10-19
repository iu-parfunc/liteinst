
/** 
 * @brief test file descr
 * @details detailed descr
 */

#include <stdio.h>
#include "zca-toggle.h"
#include "zca-types.h"

extern int activateProbe(const probe_t* label, probe_callable_t callback)
{
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
