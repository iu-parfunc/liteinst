#include <stdio.h>
#include <inttypes.h>
#include <time.h>
#include <pthread.h>
#include <sys/mman.h>
#include <errno.h>
#include <unistd.h>

volatile int active = 1; // Whether the function is toggled on or off. Not used currently
uint64_t* funcAddr = 0; // Address where function call happens which we need to toggle on/off
uint64_t activeSequence = 0; // Byte sequence for toggling on the function CALL
uint64_t deactiveSequence = 0; // NOP byte sequence for toggling off the function CALL
int64_t counter = 0;
//int64_t invocations = 1000000000;
int64_t invocations = 10000; 
volatile int ready_to_go_foo = 0;
volatile int done_foo = 0;
volatile int initial = 0;

volatile uint64_t lock = 0; 




volatile int active_apa = 1; // Whether the function is toggled on or off. Not used currently
uint64_t* funcAddr_apa = 0; // Address where function call happens which we need to toggle on/off
uint64_t activeSequence_apa = 0; // Byte sequence for toggling on the function CALL
uint64_t deactiveSequence_apa = 0; // NOP byte sequence for toggling off the function CALL
int64_t counter_apa = 0;

volatile int ready_to_go_apa = 0;
volatile int done_apa = 0; 
volatile int initial_apa = 0;






int NUM_THREADS = 5;

inline int modify_page_permissions(uint8_t* addr) {

    long page_size = sysconf(_SC_PAGESIZE);
    int code = mprotect((void*)(addr - (((uint64_t)addr)%page_size)), page_size,
            PROT_READ | PROT_WRITE | PROT_EXEC);

    if (code) {
        fprintf(stderr, "mprotect was not successfull! code %d\n", code);
        fprintf(stderr, "errno value is : %d\n", errno);
        return 0;
    }

    // If the 8 bytes we need to modify straddles a page boundary make the next page writable too
    if (page_size - ((uint64_t)addr)%page_size < 8) {
        code = mprotect((void*)(addr-((uint64_t)addr)%page_size+ page_size) , page_size,
                PROT_READ | PROT_WRITE | PROT_EXEC);
        if (code) {
            fprintf(stderr, "mprotect was not successfull! code %d\n", code);
            fprintf(stderr, "errno value is : %d\n", errno);
            return 0;;
        }
    }

    return 1;
}

void add_call() {
    if (activeSequence != 0) {
      
      /* while (! ( __sync_bool_compare_and_swap((uint64_t*)&lock, */
      /* 					      0, 1))){  */
      /* 	//fprintf(stderr,"+"); */
      /* }  */

      
        uint64_t* start_addr = funcAddr - 1;

        fprintf(stderr, "Activating foo..\n");


        uint64_t res = 0; 
	//while (res != activeSequence) {
	res = __sync_val_compare_and_swap((uint64_t*) start_addr,
					    *((uint64_t*)start_addr), activeSequence);
	//}
        // *(uint64_t*) start_addr = activeSequence;
        active = 1;

	
	/* __sync_bool_compare_and_swap((uint64_t*)&lock, */
	/* 			     1, 0); */
		
	
    } else {
        fprintf(stderr, "Active sequence not initialized..\n");
    }

}

int remove_call() {

    if (deactiveSequence != 0) {

        /* while (! ( __sync_bool_compare_and_swap((uint64_t*)&lock, */
	/* 					0, 1))) { */
	/*   //fprintf(stderr,"-"); */

	/* } */
     
	
      
        uint64_t* start_addr = funcAddr - 1;

        fprintf(stderr, "Deactivating foo..\n");

        uint64_t res = 0; 
	//while (res != deactiveSequence) {
	  res = __sync_val_compare_and_swap((uint64_t*)start_addr,
					    *((uint64_t*)start_addr), deactiveSequence);
	  //}
        // *(uint64_t*) start_addr = deactiveSequence;
        active = 0;
	
	/* __sync_bool_compare_and_swap((uint64_t*)&lock, */
	/* 			     1, 0); */
    } else {
        fprintf(stderr, "Active sequence not initialized..\n");
    }

}



void add_call_apa() {
    if (activeSequence_apa != 0) {
      
      /* while (! ( __sync_bool_compare_and_swap((uint64_t*)&lock, */
      /* 					      0, 1))){  */
      /* 	//fprintf(stderr,"+"); */
      /* }  */

      
        uint64_t* start_addr = funcAddr_apa - 1;

        fprintf(stderr, "Activating APA..\n");


        uint64_t res = 0; 
	//while (res != activeSequence) {
	res = __sync_val_compare_and_swap((uint64_t*) start_addr,
					    *((uint64_t*)start_addr), activeSequence_apa);
	//}
        // *(uint64_t*) start_addr = activeSequence;
        active_apa = 1;

	
	/* __sync_bool_compare_and_swap((uint64_t*)&lock, */
	/* 			     1, 0); */
		
	
    } else {
        fprintf(stderr, "Active sequence not initialized..\n");
    }

}

int remove_call_apa() {

    if (deactiveSequence_apa != 0) {

        /* while (! ( __sync_bool_compare_and_swap((uint64_t*)&lock, */
	/* 					0, 1))) { */
	/*   //fprintf(stderr,"-"); */

	/* } */
     
	
      
        uint64_t* start_addr = funcAddr_apa - 1;

        fprintf(stderr, "Deactivating APA..\n");

        uint64_t res = 0; 
	//while (res != deactiveSequence) {
	  res = __sync_val_compare_and_swap((uint64_t*)start_addr,
					    *((uint64_t*)start_addr), deactiveSequence_apa);
	  //}
        // *(uint64_t*) start_addr = deactiveSequence;
        active_apa = 0;
	
	/* __sync_bool_compare_and_swap((uint64_t*)&lock, */
	/* 			     1, 0); */
    } else {
        fprintf(stderr, "Active sequence not initialized..\n");
    }

}



// __attribute__((noinline))
void foo(int i) {

    if (!initial) {
        uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
	funcAddr = (uint64_t*)addr; // Save the call site address to a global variable so that it is visible to other threads
	//__sync_synchronize();


        uint64_t sequence =  *((uint64_t*)(addr-1));
        uint64_t mask = 0x0000000000FFFFFF;
        uint64_t deactive = (uint64_t) (sequence & mask);
        mask = 0x0000441F0F000000; // We NOP 5 bytes of CALL instruction and leave rest of the 3 bytes as it is

        activeSequence = sequence; // Saves the active 
        deactiveSequence = deactive |  mask;


        fprintf(stderr, "INITIAL CALL\n");

        int status = modify_page_permissions((uint8_t*)funcAddr);
        if (!status) {
            fprintf(stderr, "ERROR : Failed to modify page permissions\n");
            return;
        }

        initial = 1;
    }

    counter++;

    for (long j = 0; j < counter * 100; j++) { 
      
      if ( i + j == 10) printf("Hello foo\n"); 
      
      
    }

    // fprintf(stderr, "Foo counter : %d\n", counter);
}

// __attribute__((noinline))
void apa(uint64_t* a, uint64_t *b) { 
  
  if (!initial_apa) {
    uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
    funcAddr_apa = (uint64_t*)addr; // Save the call site address to a global variable so that it is visible to other threads
    //__sync_synchronize();
    
    
    uint64_t sequence =  *((uint64_t*)(addr-1));
    uint64_t mask = 0x0000000000FFFFFF;
    uint64_t deactive = (uint64_t) (sequence & mask);
    mask = 0x0000441F0F000000; // We NOP 5 bytes of CALL instruction and leave rest of the 3 bytes as it is
    
    activeSequence_apa = sequence; // Saves the active 
    deactiveSequence_apa = deactive |  mask;

    
    fprintf(stderr, "INITIAL CALL\n");
    
    int status = modify_page_permissions((uint8_t*)funcAddr_apa);
    if (!status) {
      fprintf(stderr, "ERROR : Failed to modify page permissions\n");
      return;
    }
    
    initial_apa = 1;
  }
  
  counter_apa++;

  for (long i = 0; i < counter * 1000; i++) { 
    
    if ( a + i == b) printf("Hello apa\n"); 
    

  } 

    

     
  return;
}



void* stress_add(void* param) {
  while(!(ready_to_go_apa && ready_to_go_foo)) {
        ;
    }

  while(!(done_apa && done_foo)) {
        add_call();
	add_call_apa();
    }
}

void* stress_remove(void* param) {
  while(!(ready_to_go_apa && ready_to_go_foo)) {
        ;
    }

  while (!(done_apa && done_foo)) {
        remove_call();
	remove_call_apa();
    }
}


void* apa_thread(void* param) {

  long i;
  for (i=0; i<invocations; i++) {
 
    apa((uint64_t*) 9999, (uint64_t*) i );

    if (i==0) { 
      fprintf(stderr, "Initial call done..\n");
      ready_to_go_apa =1;
    }
    

  }
  done_apa=1;
} 


void* foo_thread(void* param) {

  long i;
  for (i=0; i<invocations; i++) {  
    foo(5); // This is the call site that we patch

    if (i==0) { 
    	fprintf(stderr, "Initial call done..\n");
	ready_to_go_foo =1;
      }

  }
  
  done_foo=1;
}


/* void validate_patching() { */
/*     long invocs = 0; */

/* loop: */
/*     for (long i=0; i<invocations; i++) { */
/*         foo(5); // This is the call site that we patch */
/*         invocs++; */
/*     } */

/*     if (done != 0) { */
/*       goto incr; */
/*     } else { */
/*       goto remove; */
/*     } */

/* check: */
/*     if (done == 0) { */
/*       goto loop; */
/* remove: */
/*       remove_call(); */
/*     } else if (done == 1) { */
/*       goto loop; */
/*     } else { */
/*       add_call(); */
/*       goto loop; */
/*     } */

/* incr: */
/*    if (done < 2) { */
/*      done++; */
/*      goto loop;  */
/*    } */

/*     // Printing diff to make sure that there indeed have been some deactivations */
/*     fprintf(stderr, "Final count : %ld Invocations : %ld Diff : %ld..\n\n\n", counter, invocs, invocs - counter); */
/* } */

int main() {
 
    // validate_patching();

    pthread_t threads[2*NUM_THREADS];
    int rc;

    pthread_t app_threads[2]; 

    // Initialize the globals used for patching
    // foo(3);
    

    
    for (int i=0; i < 2*NUM_THREADS; i+=2) {
        int j=i;
        int k=i+1;

        rc = pthread_create(&threads[i], NULL, stress_add, (void*)j);
        if (rc) {
            printf("ERROR: thread creation failed with error %d\n", rc);
        }

        rc = pthread_create(&threads[i+1], NULL, stress_remove, (void*)k);
        if (rc) {
            printf("ERROR: thread creation failed with error %d\n", rc);
        }
    }
    
    for (int i=0; i < 2; i+=2) {
      
      rc = pthread_create(&threads[i], NULL, apa_thread, (void*)0);
      if (rc) {
	printf("ERROR: thread creation failed with error %d\n", rc);
      }
      
      rc = pthread_create(&threads[i+1], NULL, foo_thread, (void*)0);
      if (rc) {
	printf("ERROR: thread creation failed with error %d\n", rc);
      }
    }


    
      


    /* long i; */
    /* for (i=0; i<invocations; i++) { */
      
    /*   /\* while (! ( __sync_bool_compare_and_swap((uint64_t*)&lock, *\/ */
    /*   /\* 				      0, 1))){  *\/ */
    /*   /\*   //fprintf(stderr,"*"); *\/ */
    /*   /\* }  *\/ */
      
      
    /*   /\* __sync_bool_compare_and_swap((uint64_t*)&lock, *\/ */
    /*   /\* 1, 0); *\/ */
      
    /*   if (i==0) { */
    /* 	fprintf(stderr, "Initial call done..\n"); */
    /* 	ready_to_go =1; */
    /*   } */
   
    /* } */
    
    
    int *k = NULL;
    // Wait for all threads to finish
    for (int i=0; i<2*NUM_THREADS; i++) {
        pthread_join(threads[i], (void**)&k);
    }


    // Printing diff to make sure that there indeed have been some deactivations
    fprintf(stderr, "FOO: Final count : %ld Invocations : %ld Diff : %ld..\n\n\n", counter, invocations, (long)invocations - counter);
    fprintf(stderr, "APA: Final count : %ld Invocations : %ld Diff : %ld..\n\n\n", counter_apa, invocations, (long)invocations - counter_apa);

    return 0;
}



/*
   0x0000000000400f36 <+230>:	7e 26	jle    0x400f5e <main+270>
   0x0000000000400f38 <+232>:	bf 05 00 00 00	mov    $0x5,%edi
   0x0000000000400f3d <+237>:	e8 ee 00 00 00	callq  0x401030 <foo(int)>
   0x0000000000400f42 <+242>:	4d 85 f6	test   %r14,%r14
   0x0000000000400f45 <+245>:	0f 84 bf 00 00 00	je     0x40100a <main+442>
   0x0000000000400f4b <+251>:	49 ff c6	inc    %r14
   0x0000000000400f4e <+254>:	4c 3b 35 fb 37 20 00	cmp    0x2037fb(%rip),%r14        # 0x604750 <invocations>



0x0000000000400f36 <+230>:	7e 26	jle    0x400f5e <main+270>
   0x0000000000400f38 <+232>:	bf 05 ff 85 c0	mov    $0xc085ff05,%edi
   0x0000000000400f3d <+237>:	0f 1f 44 00 00	nopl   0x0(%rax,%rax,1)
   0x0000000000400f42 <+242>:	4d 85 f6	test   %r14,%r14
   0x0000000000400f45 <+245>:	0f 84 bf 00 00 00	je     0x40100a <main+442>
   0x0000000000400f4b <+251>:	49 ff c6	inc    %r14
   0x0000000000400f4e <+254>:	4c 3b 35 fb 37 20 00	cmp    0x2037fb(%rip),%r14        # 0x604750 <invocations>

0F 1F 44 00 00H
 */ 