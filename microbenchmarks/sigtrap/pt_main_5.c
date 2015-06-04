
// Signal handling test 

#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <memory.h>
#include <pthread.h>
#include <inttypes.h>
#include <sys/mman.h>
#include <ucontext.h>
#include <errno.h>
#include <unistd.h>

uint8_t* call_addr;
int call_count = 0;
uint64_t call_sequence;
uint64_t int3_sequence;

// Makes the code pages writeable 
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

// Initial call to foo writes the int3 sequence at this callsite at exit.
// Second call to foo hits the int3 and signal handler gets invoked which
// then restore the callsite to its original state and restart the thread
// at callsite
void foo(int param) {
    // Call address = Return address - 5 (Assuming 5 byte call instruction)
    call_addr = ((uint8_t*)__builtin_extract_return_addr(__builtin_return_address(0))) - 5;

    if (call_count == 0) {
       call_sequence = *((uint64_t*) call_addr);
       uint64_t mask = 0xFFFFFFFFFFFFFF00; // Little endian
       int3_sequence = (call_sequence & mask) | 0xCC;

      int status = modify_page_permissions((uint8_t*)call_addr);
      if (!status) {
        fprintf(stderr, "ERROR : Failed to modify page permissions\n");
        return;
      }

       // write the sequence beginning with int3 at call site
       __sync_val_compare_and_swap((uint64_t*) call_addr,
               *((uint64_t*) call_addr), int3_sequence);
    }

    call_count++;
    printf("Inside FOO\n");
}

void catchit(int signo, siginfo_t * inf, void * ptr) { 
// void catchit(int signo) { 
  ucontext_t *ucontext = (ucontext_t*)ptr;
  printf("Caught signal, num %d..\n",signo);

  // Restoring call site 
  __sync_val_compare_and_swap((uint64_t*) call_addr,
           *((uint64_t*) call_addr), call_sequence);

  printf("Thread resume IP is : %p\n", (void*)ucontext->uc_mcontext.gregs[REG_RIP]);
  printf("Calling foo from signal handler\n");
  ucontext->uc_mcontext.gregs[REG_RIP] = (uint8_t*)ucontext->uc_mcontext.gregs[REG_RIP] + 4;
  foo(5);
}

int main () {
  // Set int3 signal handler
  struct sigaction newact; 
  struct sigaction oldact; 
  memset( &newact, 0, sizeof newact);
  newact.sa_sigaction = & catchit;
  newact.sa_flags = SA_SIGINFO;
  sigemptyset(& (newact.sa_mask));

  sigaction(SIGTRAP, &newact, &oldact);
  printf("Sigaction set, old funptr %p\n", oldact.sa_handler);

call:
  foo(5); 
  if (call_count == 1) {
      goto call;
  }
 
  return 0;
}
