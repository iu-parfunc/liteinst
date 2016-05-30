
#include "patch_utils.h"

// Globals
size_t g_cache_lvl3_line_size;
long g_page_size; 

/* initialize a patch site, make pages read/write/exec.
   if addr + nbytes touches more than one page, all of those are modified */
bool init_patch_site(void *addr, size_t nbytes){
  /* uint64_t start = 0;  */
  /* long nb = (long)nbytes; */
  bool status = false;

  status = set_page_rwe(addr,nbytes);

  return status;
}
