#include <stdint.h>
#include <stdio.h>

namespace types
{
    struct uint128_t
    {
        uint64_t lo;
        uint64_t hi;
    }
    __attribute__ (( __aligned__( 16 ) ));
}

template< class T > inline bool cas( volatile T * src, T cmp, T with );

template<> inline bool cas( volatile types::uint128_t * src, types::uint128_t cmp, types::uint128_t with )
{
    bool result;
    __asm__ __volatile__
    (
        "lock cmpxchg16b %1\n\t"
        "setz %0"
        : "=q" ( result )
        , "+m" ( *src )
        , "+d" ( cmp.hi )
        , "+a" ( cmp.lo )
        : "c" ( with.hi )
        , "b" ( with.lo )
        : "cc"
    );
    return result;
}

void print_dword(char* address) {

  char* byte_array = address;
  int i = 0;
  while (i < 4) {
     if (i%8 == 0) {
       printf("\n");
     }
     printf("%02X",(int)byte_array[i]);
     i++;
  }
  
  printf("\n");
  printf("\n");

}

int main()
{
    using namespace types;
    uint128_t test = { 0xdecafbad, 0xfeedbeef };
    uint128_t cmp = test;
    uint128_t with = { 0x55555555, 0xaaaaaaaa };

    print_dword((char*)&test);

    // printf("%02x%02x:%02x%02x\n", (unsigned int*) test,(unsigned int*) *(&test+4), 
    //          (unsigned int*) *(&test+8),(unsigned int*) *(&test+16));
    // printf("%p:%p\n", &test, &test+8); 
    bool result = cas( & test, cmp, with );
    // printf("%02x%02x:%02x%02x\n", (unsigned int*) test,(unsigned int*) *(&test+4), 
    //          (unsigned int*) *(&test+8),(unsigned int*) *(&test+16));
    
    print_dword((char*)&test);

    return result;
}
