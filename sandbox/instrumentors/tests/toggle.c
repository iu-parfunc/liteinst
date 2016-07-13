
#include <stdio.h>
#include <inttypes.h>
#include <time.h>
#include <pthread.h>
#include <sys/mman.h>
#include <errno.h>
#include <unistd.h>

int active = 1;
uint8_t* funcAddr = 0;
uint64_t activeSequence = 0;
uint64_t deactiveSequence = 0;

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

void* add_call(void* param) {

  struct timespec ts;
  ts.tv_sec = 0;
  ts.tv_nsec = 50000;

  while (1) {
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
        // fprintf(stderr, "Result : %p\n", res);
        active = 1;
      } else {
        fprintf(stderr, "Active sequence not initialized..\n");
      }
    }

    nanosleep(&ts, NULL);
  }

}

int remove_call(uint8_t* addr) {

  if (active) {
    if (deactiveSequence == 0) {
      uint64_t sequence =  *((uint64_t*)(addr-8));
      uint64_t mask = 0x0000000000FFFFFF;
      uint64_t deactive = (uint64_t) (sequence & mask);
      mask = 0x9090909090000000; // We NOP 5 bytes of CALL instruction and leave rest of the 3 bytes as it is

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
    active = 0;
    // fprintf(stderr, "Result : %p\n", res);
  }

}

int counter = 0;

void foo(int i) {

  // Use the return address to determine where we need to patch foo CALL instruction (5 bytes)
  uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));

  fprintf(stderr, "Foo counter : %d\n", counter++);
  remove_call((uint8_t*)addr);
}

void spawn_add_call_thread() {
  pthread_t tid;
  pthread_create(&tid, NULL, add_call, (void*)NULL);
}

int main() {

  spawn_add_call_thread();

  int i=0;
  for (i=0; i<1000000; i++) {
    // fprintf(stderr, "i : %d..\n", i);
    foo(i);
  }

  fprintf(stderr, "Final count : %d..\n\n\n", counter);

}
