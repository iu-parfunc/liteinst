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
int64_t invocations = 10000000;
volatile int ready_to_go = 0;
volatile int done = 0;
volatile int initial = 0;

const int NUM_THREADS = 5;

#define asm0 "call _Z3foov;"
#define asm1 "nop; call _Z3foov;" 
#define asm2 "nop; nop; call _Z3foov;" 
#define asm3 "nop; nop; nop; call _Z3foov;" 
#define asm4 "nop; nop; nop; nop; call _Z3foov;" 
#define asm5 "nop; nop; nop; nop; nop; call _Z3foov;" 
#define asm6 "nop; nop; nop; nop; nop; nop; call _Z3foov;" 
#define asm7 "nop; nop; nop; nop; nop; nop; nop; call _Z3foov;" 
#define asm8 "nop; nop; nop; nop; nop; nop; nop; nop; call _Z3foov;" 
#define asm9 "nop; nop; nop; nop; nop; nop; nop; nop; nop; call _Z3foov;" 


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

        uint64_t* start_addr = funcAddr - 1;

        // fprintf(stderr, "Activating foo..\n");
	
        /* uint64_t res = __sync_val_compare_and_swap((uint64_t*) start_addr, */
        /*         *((uint64_t*)start_addr), activeSequence); */
	__sync_val_compare_and_swap((uint64_t*) start_addr,
                *((uint64_t*)start_addr), activeSequence);
        // *(uint64_t*) start_addr = activeSequence;
        active = 1;
    } else {
        fprintf(stderr, "Active sequence not initialized..\n");
    }

}

void remove_call() {

    if (deactiveSequence != 0) {
        /*
        int status = modify_page_permissions(funcAddr);
        if (!status) {
            return -1;
        }
        */

        uint64_t* start_addr = funcAddr - 1;

        //fprintf(stderr, "Deactivating foo..\n");
        /* uint64_t res = __sync_val_compare_and_swap((uint64_t*)start_addr, */
        /*         *((uint64_t*)start_addr), deactiveSequence); */
        __sync_val_compare_and_swap((uint64_t*)start_addr,
                *((uint64_t*)start_addr), deactiveSequence);
        // *(uint64_t*) start_addr = deactiveSequence;
        active = 0;
    } else {
        fprintf(stderr, "Active sequence not initialized..\n");
    }

}


__attribute__((noinline))
void foo() {

  if (!initial) {
    uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));

printf("address of call instr: %lx\n", (unsigned long)addr);
    
    for (int apa = 1; apa < 17; apa*=2) {  
        if ((unsigned long)addr % apa == 0)
        printf("address is %dbytes aligned\n",apa);
    } 
 

    uint64_t sequence =  *((uint64_t*)(addr-1));
    uint64_t mask = 0x0000000000FFFFFF;
    uint64_t deactive = (uint64_t) (sequence & mask);
    mask = 0x0000441F0F000000; // We NOP 5 bytes of CALL instruction and leave rest of the 3 bytes as it is
    
    activeSequence = sequence; // Saves the active 
    deactiveSequence = deactive |  mask;
    
    funcAddr = (uint64_t*)addr; // Save the call site address to a global variable so that it is visible to other threads
    fprintf(stderr, "INITIAL CALL\n");
    
    int status = modify_page_permissions((uint8_t*)funcAddr);
    if (!status) {
      fprintf(stderr, "ERROR : Failed to modify page permissions\n");
      return;
    }
    
    initial = 1;
  }
  
  if ( counter % 400000 == 0)  
    printf("call of Cthu... I mean foo()\n");
  counter++;
    // fprintf(stderr, "Foo counter : %d\n", counter++);
}

void* stress_add(void*param) {
    while(!ready_to_go) {
        ;
    }

    while(!done) {
        add_call();
    }
    // I don't know what this return value means. 
    return NULL;
}

void* stress_remove(void* param) {
    while(!ready_to_go) {
        ;
    }

    while (!done) {
        remove_call();
    }
    return NULL;
}

/* void validate_patching() { */
/*     long invocs = 0; */

/* loop: */
/*     for (long i=0; i<invocations; i++) { */
/*         foo(); // This is the call site that we patch */
/*         invocs++; */
/*     } */

/*     if (done != 0) { */
/*       goto incr; */
/*     } else { */
/*       goto remove; */
/*     } */
/*     // No Jump to check  */
/*     //check: */
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

    // Printing diff to make sure that there indeed have been some deactivations
//    fprintf(stderr, "Final count : %ld Invocations : %ld Diff : %ld..\n\n\n", counter, invocs, invocs - counter);
//}

int main() {
 
    // validate_patching();

    pthread_t threads[2*NUM_THREADS];
    int rc;

    // Initialize the globals used for patching
    // foo(3);

    for (int i=0; i < 2*NUM_THREADS; i+=2) {
      //int j=i;
      //int k=i+1;

        rc = pthread_create(&threads[i], NULL, stress_add, (void*)0);
        if (rc) {
            printf("ERROR: thread creation failed with error %d\n", rc);
        }

	rc = pthread_create(&threads[i+1], NULL, stress_remove, (void*)0);
        if (rc) {
           printf("ERROR: thread creation failed with error %d\n", rc);
        }
    }

    long i;
    for (i=0; i<invocations; i++) {
      //__asm__ ("call foo");

#ifdef ASM0 
      __asm__(asm0);
#endif 
#ifdef ASM1 
      __asm__(asm1);
#endif 
#ifdef ASM2
      __asm__(asm2);
#endif 
#ifdef ASM3 
      __asm__(asm3);
#endif 
#ifdef ASM4 
      __asm__(asm4);
#endif 
#ifdef ASM5 
      __asm__(asm5);
#endif 
#ifdef ASM6 
      __asm__(asm6);
#endif 
#ifdef ASM7 
      __asm__(asm7);
#endif 
#ifdef ASM8 
      __asm__(asm8);
#endif 
#ifdef ASM9 
      __asm__(asm9);
#endif 


      
      //foo(5); // This is the call site that we patch
        if (i==0) {
            fprintf(stderr, "Initial call done..\n");
            ready_to_go =1;
        }
    }

    done =1; 
    int *k = NULL;
    // Wait for all threads to finish
    for (int i=0; i<2*NUM_THREADS; i++) {
        pthread_join(threads[i], (void**)&k);
    }


    // Printing diff to make sure that there indeed have been some deactivations
    fprintf(stderr, "Final count : %ld Invocations : %lu Diff : %ld..\n\n\n", counter, i, (long)i - counter);
}
