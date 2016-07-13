
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
  
  int r = mprotect((void*)start, bytes, 
		   PROT_READ | PROT_WRITE | PROT_EXEC);
  /* need to be more sophisticated here */ 
  if (r == 0) return true; 
  else return false; 
}

int main(int argc, char **argv) { 

  int start_ix; 
  int end_ix; 
  
  if (argc == 3) { 
    start_ix = atoi(argv[1]); 
    end_ix = start_ix + atoi(argv[2]); 
  } else { 
    printf("No arguments provided. Using defaults\n"); 
    int start_ix = 20; 
    int end_ix   = start_ix + 10; 
  } 
  printf("Relocating %d instructions starting at instruction %d in foo\n",
	 end_ix - start_ix, start_ix); 


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
				
  /* pick out an instruction to start relocating from */ 

  unsigned int instr_offset = addresses[start_ix]; 
  unsigned int num_relocate = 10; 

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
 
  unsigned char ret[4] = {0x55,0x44,0xc9,0xc3}; 

  /* jmp back from relocated code */ 
  uint32_t jmp_addr = - ((((uint64_t)fun_data) + 
			  addresses[end_ix+1] - 
			  addresses[start_ix]) - 
			 (((uint64_t)foo) + addresses[end_ix]));

  unsigned char jmp_back[5] = {0xe9,0,0,0,0};
  *(uint32_t*)(&jmp_back[1]) = jmp_addr; 
  

  /* jump into relocated code */ 
  uint32_t jmp_reloc = ((uint64_t)fun_data) - 
                       (((uint64_t)foo) + instr_offset) - 5; 

  unsigned char jmp_reloc_instr[5] = {0xe9,0,0,0,0};
  *(uint32_t*)(&jmp_reloc_instr[1]) = jmp_reloc; 
 
  printf("Jump-back address: %x, %d \n", jmp_addr, jmp_addr); 
  printf("jump-forward addr: %x, %d \n", jmp_reloc,jmp_reloc); 

  count = relocate(fun_data, 
		   ((unsigned char*)foo)+instr_offset, 
		   jmp_back, // ret, //jmp_back, 
		   5, // 4, // 5
		   (end_ix - start_ix));  


  /* write jmp_reloc_instr into foo */ 
  memcpy(((unsigned char *)foo)+instr_offset,jmp_reloc_instr,5); 
  
  printf("Relocated %d instructions\n", count); 

  /* Run foo again after relocating a part of it */ 
  
  int b = foo(); 

  
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
