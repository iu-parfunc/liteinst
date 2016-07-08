
#include "relocator.h" 
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#include <stdint.h>
#include <stdbool.h>
#include <memory.h>
#include <stdio.h> 
#include <stdint.h> 

long g_page_size;


int foo() { 

  int sum = 0; 
  
  printf("%d\n",sum); 
  sum += 1; 
  printf("%d\n",sum); 
  sum += 1; 
  printf("%d\n",sum); 
  sum += 1; 
  printf("%d\n",sum); 
  sum += 1; 
  printf("%d\n",sum); 
  sum += 1; 
  printf("%d\n",sum);  
  sum += 1; 
  printf("%d\n",sum);  
  sum += 1; 
  printf("%d\n",sum);  
  sum += 1; 
  printf("%d\n",sum);  
  sum += 1; 
  printf("%d\n",sum);  
  sum += 1; 

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

  printf("Testing relocation of a part of a function\n"); 

  g_page_size = sysconf(_SC_PAGESIZE);

  relocate_info(); 
  
  unsigned char *fun_data = (unsigned char*)malloc(390); 
  memset(fun_data,0,390); 

  uint32_t *addresses = (uint32_t*)malloc(100*sizeof(uint32_t)); 
  
  unsigned int count = instruction_offsets((unsigned char*)foo, addresses, 100); 

  printf("Count: %d\n", count); 
  printf("foo Base address: %llx\n", (uint64_t)foo);
  printf("fun_data Base address: %llx\n", (uint64_t)fun_data); 
  for (int i = 0; i < count; i ++) { 
    
    printf("%x\n", addresses[i]);     
  }					
				
  /* pick out an instruction to start relocating from */ 
  unsigned int instr_offset = addresses[8]; 
  unsigned int num_relocate = 100; 

  /* printf("Instr offset: %x\n", instr_offset); */ 

  set_page_rwe(foo, 1024);  
  set_page_rwe(&fun_data[0], 1024); 

  int a = foo(); 
 
  
  unsigned int relocatable_instr; 
  unsigned int relocatable_bytes;

  relocatable((unsigned char *)(foo + instr_offset), 
	      num_relocate, 
	      &relocatable_instr, 
	      &relocatable_bytes);  

  
  printf("%d bytes relocatable\n", relocatable_bytes); 
  printf("%d instructions relocatable\n",relocatable_instr); 

  /* //  printf("count_relocatable: %d\n", count);  */
 
  unsigned char ret = 0xc3; 

  /* jmp back from relocated code */ 
  uint32_t jmp_addr = (((uint64_t)foo) + addresses[19]) - 
                      (((uint64_t)fun_data) + addresses[20]) + 5;
  //  unsigned char *jmp_addr_ = (unsigned char*)&jmp_addr;
  unsigned char jmp_back[5] = {0xe9,0,0,0,0};
  *(uint32_t*)(&jmp_back[1]) = jmp_addr; 
  

  /* jump into relocated code */ 
  uint32_t jmp_reloc = ((uint64_t)fun_data) - 
                       (((uint64_t)foo) + instr_offset) - 5; 
  //unsigned char *jmp_reloc_ = (unsigned char*)&jmp_reloc; 
  unsigned char jmp_reloc_instr[5] = {0xe9,0,0,0,0};
  *(uint32_t*)(&jmp_reloc_instr[1]) = jmp_reloc; 
 
  printf("Jump-back address: %lx\n", jmp_addr); 
  printf("jump-forward addr: %lx\n", jmp_reloc); 

  count = relocate(fun_data, 
		   ((unsigned char*)foo)+instr_offset, 
		   jmp_back, 
		   5, 
		   10);  


  /* write jmp_reloc_instr into foo */ 
  memcpy(((unsigned char *)foo)+instr_offset,jmp_reloc_instr,5); 
  
  printf("Relocated %d instructions\n", count); 

  int b = foo(); // ((int (*)(void))&fun_data[0])(); 

  /* // printf("value computed by relocated fun: %d \n", b);  */
    
  
  printf("RESULT:%d\n", a); 
  printf("RESULT:%d\n", b); 

  free(fun_data); 
  if (a == b) {   
    printf("SUCCESS\n");  
    return 1;  
  } else {  
    return 0;  
  }  
  
} 
