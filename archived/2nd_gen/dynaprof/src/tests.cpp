#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string>
#include "dynaprof.h"
#include "cycle.h"

int x = 0;

/******************* Probe Function ****************/
void probe_function() {
  x++;
	printf("x : %d \n", x);
}

/******************* Test Functions ********************/
long foo(int count) {
	__notify_intrinsic((void*)"foo:start",(void*)&x);

  if (count != 0) {
    foo(--count);
  }

  int i;
  long sum = 0;
	for (i=0; i < 100; i++) {
		srand(time(NULL));
		sum += rand();
	}

	__notify_intrinsic((void*)"foo:end",(void*)&x);
  return sum;
}

void if_conditions(int a) {
  __notify_intrinsic((void*)"if_conditions:start",(void*)&x);
  if(a == 1){
    __notify_intrinsic((void*)"if_conditions:end",(void*)&x);
    return;
  } else if (a == 2){
    __notify_intrinsic((void*)"if_conditions:end",(void*)&x);
    return;
  }
    
  __notify_intrinsic((void*)"if_conditions:end",(void*)&x);
  return;
}


/******************* Tests *************************/
void test_overhead_recursive() {

  ticks start = getticks();
  int i;
  foo(100000);
  ticks end = getticks();
  ticks elapsed_1 = end - start;

  start_profiler();

  start = getticks();
  foo(100000);

  end = getticks();
  ticks elapsed = end - start;
  printf("Elapsed time without zcatoggle : %lu\n", elapsed_1);
  printf("Elapsed time with zcatoggle : %lu\n", end-start);
  printf("Overhead : %f%\n", ((double)elapsed / elapsed_1) * 100);
}

void test_overhead() {

  ticks start = getticks();
  int i;
  for (i=0; i<1000000; i++) {
      foo(0);
  }
  ticks end = getticks();
  ticks elapsed_1 = end - start;

  start_profiler();

  start = getticks();
  for (i=0; i<1000000; i++) {
      foo(0);
  }

  end = getticks();
  ticks elapsed = end - start;
  printf("Elapsed time without zcatoggle : %lu\n", elapsed_1);
  printf("Elapsed time with zcatoggle : %lu\n", end-start);
  printf("Overhead : %f%\n", ((double)elapsed / elapsed_1) * 100);
}


void test_probe_activation() {

  start_profiler();

  activate_method_profiling("foo", probe_function, NULL);
  foo(0);

  printf("Deactivate - Start..\n");
  deactivate_method_profiling("foo");
  foo(0);
  printf("Deactivate - End..\n");

  activate_method_profiling("foo", probe_function, NULL);
  foo(0);
  printf("Deactivate - Start..\n");
  deactivate_method_profiling("foo");
  foo(0);
  printf("Deactivate - End..\n");

  activate_method_profiling("foo", probe_function, NULL);
  foo(0);
  printf("Deactivate - Start..\n");
  deactivate_method_profiling("foo");
  foo(0);
  printf("Deactivate - End..\n");

	// printf("x : %d expected : 6 \n", x);

}

void test_if_conditions() {
  start_profiler();
  if_conditions(1);
  if_conditions(2);
  if_conditions(3);
}


void read_elf() {

}

void test_multithreaded_probe_activation() {

}

int main () {
  // test_overhead();
	// test_probe_activation();
  test_if_conditions();
}
