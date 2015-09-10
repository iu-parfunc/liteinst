#include "asyncpatch.h" 

PatchList pl = {0}; 

void print_list(PatchList *pl){ 
  struct Patch *p = pl->head; 

  while (p != NULL) { 
    printf("%lu, ",p->address); 
    p = p->next; 
  }
  printf("\n"); 
    
}

int main(void) { 

  pl.head = NULL;

  struct Patch p[10] = {0}; 
  p[0].address = 1; 
  p[1].address = 2;
  p[2].address = 3; 
  p[3].address = 4;
  p[4].address = 5;
  p[5].address = 6; 
  p[6].address = 7;
  p[7].address = 8;
  p[8].address = 9; 
  p[9].address = 10; 
  
  printf("PatchList: %p\n",pl.head);

  for (int i = 0; i < 10; i ++) { 
    cons_patch(&p[i], &pl); 
  }
  
  printf("PatchList: %p\n",pl.head);
  print_list(&pl); 
  
  struct Patch *p1 = remove_patch(4, &pl); 
  printf("%lu\n",p1->address);
  print_list(&pl); 
  
  struct Patch *p2 = remove_patch(8, &pl); 
  printf("%lu\n",p2->address);
  print_list(&pl); 
  
  struct Patch *p3 = remove_patch(10, &pl); 
  printf("%lu\n",p3->address);
  print_list(&pl); 
  
  cons_patch(p2,&pl); 
  print_list(&pl); 

  struct Patch *p4 = remove_patch(710, &pl); 
  if (p4 != NULL) {
    printf("%lu\n",p4->address);
  } else { 
    printf("patch not found\n");
  }
  print_list(&pl); 

  cons_patch(p1,&pl); 
  print_list(&pl); 

  
  
  // print_cache_info();

  return 0; 
}
