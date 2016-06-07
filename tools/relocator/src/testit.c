
#include "relocator.h" 
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#include <stdint.h>
#include <stdbool.h>
#include <memory.h>
#include <stdio.h> 

unsigned char fun[1000] = { 
  0x55,          
  0x48, 0x89, 0xe5,             	        //mov    %rsp,%rbp
  0xc7, 0x45, 0xf4, 0x05, 0x00, 0x00, 0x00, 	//movl   $0x5,-0xc(%rbp)
  0xc7, 0x45, 0xf8, 0x02, 0x00, 0x00, 0x00, 	//movl   $0x2,-0x8(%rbp)
  0x8b, 0x55, 0xf4,             	        //mov    -0xc(%rbp),%edx
  0x8b, 0x45, 0xf8,             	        //mov    -0x8(%rbp),%eax
  0x01, 0xd0,                	                //add    %edx,%eax
  0x89, 0x45, 0xfc,             	        //mov    %eax,-0x4(%rbp)
  0x8b, 0x45, 0xfc,             	        //mov    -0x4(%rbp),%eax
  0x5d,                   	                //pop    %rbp
  0xc3,                   	                //retq   
  0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 	//nopl   0x0(%rax,%rax,1)
  0x00, 
};

long g_page_size;


int foo() { 
  int a = 3; 
  int b = 1; 
  int c = a + b; 
  return c; 
} 


inline bool set_page_rwe(void *addr,size_t nbytes) { 

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

void main() { 
  g_page_size = sysconf(_SC_PAGESIZE);
  int i; 

  relocate_info(); 
  
  unsigned char *fun_data = (unsigned char*)malloc(100); 
  //memcpy(fun_data, fun, 90); 
  set_page_rwe(&fun[0], 1024); 
  set_page_rwe(&fun_data[0], 1024); 

  //  for (i = 0; i < 90; i ++) { 
  //  printf("%x ", fun[i]); 
  //}
  //printf("\n"); 
  

  int a = ((int (*)(void))&fun[0])();

  printf("value of \"a\" computed by fun: %d \n", a); 
 
  unsigned int count = count_relocatable(fun, 64); 
  
  printf("count_relocatable: %d\n", count); 

  relocate(fun_data, fun, 64); 

  a = ((int (*)(void))&fun_data[0])();

  printf("value of \"a\" computed by relocated fun: %d \n", a); 
  


} 
