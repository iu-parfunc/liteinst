#include <stdio.h>
#include <stdint.h>

int lock_inc(uint64_t *value) {

 __asm__ __volatile__
 (
   "lock incq %0;\n"
   : "+m"(*value)
   : 
   : "memory"
 );
}

uint64_t value = 0;
uint64_t* value_ptr = &value;

int main() {

 __asm__ __volatile__
 (
   "lock incq %0;\n"
   : "+m"(value)
   : 
   : "memory"
 );

 __asm__ __volatile__
 (
   "lock incq %0;\n"
   : "+m"(value)
   : 
   : "memory"
 );

 // lock_inc(&value);
 // lock_inc(&value);
 printf("Value : %lu\n", value);

}
