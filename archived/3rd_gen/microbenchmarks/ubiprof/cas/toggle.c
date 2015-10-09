#include <stdio.h>
#include <inttypes.h>
#include <time.h>
#include <pthread.h>
#include <sys/mman.h>
#include <errno.h>
#include <unistd.h>

volatile int active = 1; // Whether the function is toggled on or off
uint8_t* funcAddr = 0; // Address where function call happens which we need to toggle on/off
uint64_t activeSequence = 0; // Byte sequence for toggling on the function CALL
uint64_t deactiveSequence = 0; // NOP byte sequence for toggling off the function CALL
volatile int ready_to_go = 0;
volatile int done = 0;

int NUM_THREADS = 10;

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

void* stress_add(void* param) {
    // while(!ready_to_go) {
    //     ;
    // }
    struct timespec ts;
    ts.tv_sec = 0;
    ts.tv_nsec = 50000;
    
    while (done < NUM_THREADS/2) {
        if (!active) {
            if (activeSequence != 0) {
                int status = modify_page_permissions(funcAddr);
                if (!status) {
                    return 0;
                }
                
                uint8_t* start_addr = funcAddr - 8;
                
                fprintf(stderr, "Activating foo..\n");
                uint64_t res = __sync_val_compare_and_swap((uint64_t*) start_addr,
                                                          *((uint64_t*)start_addr), activeSequence);
                // *(uint64_t*) start_addr = activeSequence;
                active = 1;
            } else {
                fprintf(stderr, "Active sequence not initialized..\n");
            }
        }
        
       // nanosleep(&ts, NULL);
    }
    
}

int remove_call(uint8_t* addr) {
    
    // if (active) {
        // Remove gets called first before add so we initialize active and deactive state byte sequences during the first call the remove
        if (deactiveSequence == 0) {
            uint64_t sequence =  *((uint64_t*)(addr-8));
            uint64_t mask = 0x0000000000FFFFFF;
            uint64_t deactive = (uint64_t) (sequence & mask);
            mask = 0x0000441F0F000000; // We NOP 5 bytes of CALL instruction and leave rest of the 3 bytes as it is
            
            activeSequence = sequence;
            deactiveSequence = deactive |  mask;
            funcAddr = addr;
        }
        
        int status = modify_page_permissions(addr);
        if (!status) {
            return -1;
        }
        
        uint8_t* start_addr = addr - 8;
        
        fprintf(stderr, "Deactivating foo..\n");
        uint64_t res = __sync_val_compare_and_swap((uint64_t*)start_addr,
                                                  *((uint64_t*)start_addr), deactiveSequence);
        // *(uint64_t*) start_addr = deactiveSequence;
        active = 0;
        // fprintf(stderr, "Result : %p\n", res);
    // }
}

volatile int counter = 0;

void foo(int i) {
    
    // Use the return address to determine where we need to patch foo CALL instruction (5 bytes)
    uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
    
    fprintf(stderr, "Foo counter : %d\n", counter++);
    remove_call((uint8_t*)addr);
}

// This thread periodically checks if the method is inactive and if so reactivates it
void spawn_add_call_thread() {
    pthread_t tid;
    pthread_create(&tid, NULL, stress_add, (void*)NULL);
}

void* stress_remove(void* param) {
  // while (!ready_to_go) {
  //    ;
  // }
  int i=0;
  for (i=0; i<1000000; i++) {
    // fprintf(stderr, "i : %d..\n", i);
    foo(i);
  }

  done++;
}

int main() {
    
    // spawn_add_call_thread();

    pthread_t threads[2*NUM_THREADS];
    int rc;

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

    ready_to_go = 1;

    int *k = NULL;
    // Wait for all threads to finish
    for (int i=0; i<2*NUM_THREADS; i++) {
      pthread_join(threads[i], (void**)&k);
    }
    
    
    fprintf(stderr, "Final count : %d..\n\n\n", counter);
}