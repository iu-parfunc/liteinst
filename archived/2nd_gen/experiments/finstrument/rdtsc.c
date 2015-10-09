
#include <stdio.h>

static __inline__ void  getticks(void)
{
    unsigned long long int x;
    __asm__ volatile (".byte 0x0f, 0x31" : "=A" (x));
}

int main() {

  getticks();

}


