#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>
#include <errno.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>
#include <inttypes.h>

#define PROBESIZE 16
 
// namespace types
// {
    typedef struct uint128_t
    {
        uint64_t lo;
        uint64_t hi;
    }  uint128_t __attribute__ (( __aligned__( 16 ) ));
//}

// template< class T > inline bool cas( volatile T * src, T cmp, T with );

inline bool dwcas( volatile uint128_t * src, uint128_t *cmp, uint128_t with )
{
    bool result;
    __asm__ __volatile__
    (
        "lock cmpxchg16b %1\n\t"
        "setz %0"
        : "=q" ( result )
        , "+m" ( *src )
        , "+d" ( cmp->hi )
        , "+a" ( cmp->lo )
        : "c" ( with.hi )
        , "b" ( with.lo )
        : "cc"
    );
    return result;
}

inline bool cas(volatile uint64_t *src, uint64_t cmp, uint64_t with) {

  return  __sync_val_compare_and_swap(src, cmp, with);

}

static FILE *fp_trace;

int counter = 0;
 
void
__attribute__ ((constructor))
trace_begin (void)
{
 fp_trace = fopen("trace.out", "w");
}
 
void
__attribute__ ((destructor))
trace_end (void)
{
 if(fp_trace != NULL) {
 fclose(fp_trace);
 }
}

void print_fn() {
  printf("Hello world!!\n");
}
 
void
__cyg_profile_func_enter (void *func,  void *caller)
{
/*
 if(fp_trace != NULL) {
 fprintf(fp_trace, "e %p %p %lu\n", func, caller, time(NULL) );
 }
*/
  // uint128_t with = { 0x55555555, 0xaaaaaaaa };
  unsigned char* addr = __builtin_extract_return_addr(__builtin_return_address(0)); 
  addr = addr - 8;
   
  // addr = addr - 18;

  if (((uintptr_t)&addr & 0x0F) == 0) {
    printf("Address %p is 16 byte aligned..\n", addr);
  }

    long page_size = sysconf(_SC_PAGESIZE); 
    int code = mprotect((void*)(addr - (((unsigned long)addr)%page_size)), page_size,
        PROT_READ | PROT_WRITE | PROT_EXEC);

    if (code) {
      fprintf(stderr, "mprotect was not successfull! code %d\n", code);
      fprintf(stderr, "errno value is : %d\n", errno);
    }

    if (page_size - ((unsigned long)addr)%page_size < PROBESIZE) {
      code = mprotect((void*)(addr-((unsigned long)addr)%page_size+ page_size) , page_size,
          PROT_READ | PROT_WRITE | PROT_EXEC);
      if (code) {
        fprintf(stderr, "mprotect was not successfull! code %d\n", code);
        fprintf(stderr, "errno value is : %d\n", errno);
      } 
    }

    /*
    uint128_t* b = (uint128_t*)addr;
    uint128_t c = {b->lo, b->hi};
    bool result = dwcas(b, &c, with); 
    printf("Result is : %d\n", result);
    */
    uint64_t* b = (uint64_t*)addr;
    uint64_t c = *b;
    // uint64_t c = 1;
    uint64_t wi = 0x9090909090909090; // 8 NO-Ops
     
    fprintf(stderr, "b : %" PRIu64 " c : %" PRIu64 " wi : %" PRIu64 "\n", *b, c, wi); 
    bool result = 0;
    result = __sync_bool_compare_and_swap(b, c, wi);
    fprintf(stderr, "b after CAS : %" PRIu64 "\n", *b); 
    // bool result = cas(b, c, wi); 

    printf("Result is : %d\n", result);

/*
  unsigned long offset = (unsigned long) addr % 16;
  unsigned char* cas_start = 0;
  if (offset < 12) {
    cas_start = addr - offset; 
  } else {
    fprintf(stderr, "Probe straddles a 16 bit boundary\n");
  }

  if (cas_start != 0) {
    long page_size = sysconf(_SC_PAGESIZE); 
    int code = mprotect((void*)(addr - (((unsigned long)addr)%page_size)), page_size,
        PROT_READ | PROT_WRITE | PROT_EXEC);

    if (code) {
      fprintf(stderr, "mprotect was not successfull! code %d\n", code);
      fprintf(stderr, "errno value is : %d\n", errno);
    }

    if (page_size - ((unsigned long)addr)%page_size < PROBESIZE) {
      code = mprotect((void*)(addr-((unsigned long)addr)%page_size+ page_size) , page_size,
          PROT_READ | PROT_WRITE | PROT_EXEC);
      if (code) {
        fprintf(stderr, "mprotect was not successfull! code %d\n", code);
        fprintf(stderr, "errno value is : %d\n", errno);
      } 
    }

    uint128_t* b = (uint128_t*)addr;
    uint128_t c = {b->lo, b->hi};
    bool result = cas(b, &c, with); 
    printf("Result is : %d\n", result);
  }

*/


  // uint128_t c = add;
  // uint128_t cmp = {(uint64_t)*add, (uint64_t)*(add+8)};

/*
  __int128* b = (__int128*)addr;
  __sync_val_compare_and_swap(b, *b, 2);
*/
  // printf("Result : %d\n", result);


  // printf("%p\n", add-16);
  // (unsigned char*)(caller-5);
}
 
void
__cyg_profile_func_exit (void *func, void *caller)
{
/*
 if(fp_trace != NULL) {
 fprintf(fp_trace, "x %p %p %lu\n", func, caller, time(NULL));
 }
*/
  unsigned char* add = __builtin_extract_return_addr(__builtin_return_address(0)); 
  // printf("%p\n", add-5);
}
