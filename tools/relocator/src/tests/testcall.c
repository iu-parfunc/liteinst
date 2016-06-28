
#include "relocator.h" 
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#include <stdint.h>
#include <stdbool.h>
#include <memory.h>
#include <stdio.h> 


long g_page_size;


int foo() { 

  int sum = 0; 
  
  for (int i = 0; i < 10; i ++) { 
   printf(".");    
   sum += i; 
  }  
  printf("\n"); 

  
  return sum; 
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

int main() { 

  printf("Testing relocation of entire function containing loop and call\n"); 

  g_page_size = sysconf(_SC_PAGESIZE);

  relocate_info(); 
  
  unsigned char *fun_data = (unsigned char*)malloc(100); 

  set_page_rwe(foo, 1024); 
  set_page_rwe(&fun_data[0], 1024); 

  int a = foo(); 
 
  unsigned int count = count_relocatable((unsigned char *)foo, 64); 
  
  //  printf("count_relocatable: %d\n", count); 

  relocate(fun_data, (unsigned char*)foo, NULL, 0, count); 

  int b = ((int (*)(void))&fun_data[0])();

  // printf("value computed by relocated fun: %d \n", b); 
  
  
  free(fun_data); 
  if (a == b) {  
    printf("SUCCESS\n"); 
    return 1; 
  } else { 
    return 0; 
  } 
  
} 
