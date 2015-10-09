
#include <unistd.h> 
#include <stdlib.h>
#include <stdio.h>
#include <sys/mman.h>
#include <errno.h> 
#include <stdint.h> 
#include <stdbool.h> 

typedef struct uint128_t
    {
        uint64_t lo;
        uint64_t hi;
    }  uint128_t __attribute__ (( __aligned__( 16 ) ));

inline bool dwcas( volatile uint128_t * src, uint128_t *cmp, uint128_t* with )
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
        : "c" ( with->hi )
        , "b" ( with->lo )
        : "cc"
    );
    return result;
}

int main() {

 uint128_t* a = malloc(sizeof(uint128_t));
 uint128_t* b = malloc(sizeof(uint128_t));
 uint128_t* c = malloc(sizeof(uint128_t));

 long page_size = sysconf(_SC_PAGESIZE);
 int code = mprotect((void*)(a - (((unsigned long)a)%page_size)), page_size,
      PROT_READ | PROT_EXEC);

 a->hi = 1;
 a->lo = 2;
 b->hi = a->hi;
 b->lo = a->lo; 
 c->hi = 0;
 c->lo = 0;

 bool result = dwcas(a, b, c);
 printf("Result %d\n", result);

 return 0;

}

