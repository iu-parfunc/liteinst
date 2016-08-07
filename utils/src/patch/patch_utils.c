
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


bool set_page_rwe(void *addr,size_t nbytes) {

  uint64_t start = (uint64_t)addr - (((uint64_t)addr)%g_page_size);


  uint64_t offset_into_page = (((uint64_t)addr)%g_page_size);
  size_t bytes = offset_into_page + nbytes; /* too touch page 2 or n...  */
  //printf("Offset into page %ld \n", offset_into_page);
  //printf("Setting prot for %d bytes\n", bytes);

  int r = mprotect((void*)start, bytes,
		   PROT_READ | PROT_WRITE | PROT_EXEC);
  /* need to be more sophisticated here */
  if (r == 0) return true;
  else return false;
}

uint64_t get_msb_mask_64(int nbytes) {
  assert(nbytes > 0 && nbytes < 8); 
  return((uint64_t)0xFFFFFFFFFFFFFFFF << (64 - (nbytes*8)));
}

uint64_t get_lsb_mask_64(int nbytes) {
  assert(nbytes > 0 && nbytes < 8); 
  return((uint64_t)0xFFFFFFFFFFFFFFFF >> (64 - (nbytes*8)));
}

uint32_t get_msb_mask_32(int nbytes) {
  switch (nbytes) {
  case 1:
    return 0xFF000000;
  case 2:
    return 0xFFFF0000;
  case 3:
    return 0xFFFFFF00;
  default:
    printf("ERROR : Invalid input to get_msb_mask\n");
    return 0;
  }
}

uint32_t get_lsb_mask_32(int nbytes) {
  switch (nbytes) {
    case 1:
      return 0xFF;
    case 2:
      return 0xFFFF;
    case 3:
      return 0xFFFFFF;
    default:
      printf("ERROR : Invalid input to get_lsb_mask_32\n");
      return 0;
  }
}

