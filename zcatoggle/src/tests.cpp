#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string>
#include "zca-toggle.hpp"
#include "cycle.h"

int x;

/******************* Probe Function ****************/
void probe_function() {
	printf("Hello my little world!!!\n");
}

/******************* Test Functions ********************/
long foo() {
	__notify_intrinsic((void*)"foo:start",(void*)&x);

  long sum;
  int i;
	for (i=0; i < 100; i++) {
		srand(time(NULL));
	  sum += rand();
	}

	__notify_intrinsic((void*)"foo:end",(void*)&x);

  return sum;
}

inline long inline_foo(double a, double b, double c, double d, double e, double f, double g, double h, double x, double j, double k) {
	__notify_intrinsic((void*)"inline_foo:start",(void*)&x);

  long sum;
  int i;
	for (i=0; i < 100; i++) {
		srand(time(NULL));
	  sum += rand();
	}

	__notify_intrinsic((void*)"inline_foo:end",(void*)&x);

  return sum + a + b + c + d + e + f + g + h + x + j + k;
}



void empty_func() {

}

long call_emulate_func() {

// #pragma noinline recursive
  empty_func();

  long sum;
  int i;
	for (i=0; i < 100; i++) {
		srand(time(NULL));
	  sum += rand();
	}

// #pragma noinline recursive
  empty_func();
  return sum;

}

/******************* Tests *************************/
void test_inline_function() {
	initZCAService();
  inline_foo(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0);
}

void test_overhead() {

  ticks start = getticks();
  int i;
  for (i=0; i<1000000; i++) {
    foo();
  } 
  ticks end = getticks();
  ticks elapsed_1 = end - start;

	initZCAService();

  start = getticks();
  for (i=0; i<1000000; i++) {
    foo();
  } 
  end = getticks();
  ticks elapsed = end - start;
  printf("Elapsed time without zcatoggle : %lu\n", end-start);
  printf("Elapsed time with zcatoggle : %lu\n", end-start);
  printf("Overhead : %f%\n", ((double)elapsed / elapsed_1) * 100);
}

void test_probe_activation() {

	initZCAService();

	printf("Executing foo after initial probe activation..\n");
	foo();
	printf("Done executing foo after initial probe activation..\n");

	deactivateProbe("foo:start");
	deactivateProbe("foo:end");

	printf("Executing foo after probe deactivation..\n");
	foo();
	printf("Done executing foo after probe deactivation..\n");

	activateProbe("foo:start", probe_function);
	activateProbe("foo:end", probe_function);

	printf("Executing foo after probe reactivation..\n");
	foo();
	printf("Done executing foo after probe reactivation..\n");

	deactivateProbe("foo:start");
	deactivateProbe("foo:end");

	printf("Executing foo after second probe deactivation..\n");
	foo();
	printf("Done executing foo after second probe deactivation..\n");

}

void read_elf() {

}

void test_multithreaded_probe_activation() {

}

int main () {
  // test_inline_function();
  // test_overhead();
	test_probe_activation();

}
