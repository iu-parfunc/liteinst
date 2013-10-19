

/** 
 * @brief Inspect an ELF binary and recover the probe locations from its headers.
 * 
 * 
 */

#include <stdio.h>
#include "zca-types.h"

/* #include <stdio.h> */
/* #include <iostream> */
/* #include <cstddef> */
/* #include <string.h> */
/* #include <err.h> */
/* #include <fcntl.h> */
/* #include <libelf.h>  */
/* #include <stdlib.h> */
/* #include <sysexits.h> */
/* #include <unistd.h> */
/* #include <cstdio> */
/* #include <unordered_map> */
/* #include <stack> */
/* #include <utility> */
/* #include <errno.h> */

/* #include <sys/mman.h> */
/* #include <AsmJit/AsmJit.h> */

using namespace std;

typedef unordered_map<string, pair<zca_row_11_t*, unsigned long*>> ann_table;
// ann_table globalAnnTable;
// zca_header_11_t* globalZCATable;

void hello() 
{
   printf("hello\n");
}
