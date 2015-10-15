
#include <stdio.h>
#include <stdlib.h>
#include "zca-toggle.hpp"
#include <sys/mman.h>
#include <errno.h>
#include "cycle.h"
#include <pthread.h>
#include <unistd.h>

#if PROFILE == 1
#define PROBE_ON
#endif

void empty_func(int i);
void test_probe_loop();
void test_single_probe();
void test_large_probe_count();
void test_setup_times();
void test_probe_deactivation();
void annotated_func();
ticks find_median(ticks* values, int items);

void __attribute__ ((constructor(10))) premain()
{
	// initZCAService();
}

int main () {
	initZCAService();
	int x = 20;
	// __notify_intrinsic((void*)"notify01",(void*)&x);
	// __notify_intrinsic((void*)"notify02",(void*)&x);

#ifdef SINGLE_PROBE
	 test_single_probe();
#endif

#ifdef PROBE_LOOP
	test_probe_loop();
#endif

#ifdef PROBE_LARGE
	test_large_probe_count();
#endif

#ifdef PROBE_ACTIVATION
	 test_probe_deactivation();
#endif

}

/**
 * Test functions
 */

void print_fn3() {
  printf("[Successful] Resistence is futile.. Hmm.. really???\n");
}

void *probe_activation_func(void* tid) {
	// printf("\nDeactivating probe..\n");
	int status = 0;
	status = deactivateProbe("emptyFunc");

	if(status == 1) {
	   empty_func((int)tid);
	}

	// sleep(1);

	status = 0;
	status = activateProbe("emptyFunc", print_fn3);

	if (status == 1) {
	   empty_func((int)tid);
	}

	pthread_exit(NULL);
}

void test_probe_deactivation() {

	int i = 0;

	printf("\nExecuting the annotated method..\n");
	empty_func(i);

	printf("\nDeactivating probe..\n");
	deactivateProbe("emptyFunc");
	printf("Executing the annotated method after deactivation..\n");
	empty_func(i);

	printf("\nReactivating probe..\n");
	activateProbe("emptyFunc", print_fn3);
	printf("Executing the annotated method after reactivation..\n");
	empty_func(i);

	pthread_t threads[100];

	int rc;
	for(i=0; i<100; i++) {
		// printf("Creating threads..\n");
	      rc = pthread_create(&threads[i], NULL, probe_activation_func, (void *)i);
	      if (rc){
	         printf("ERROR; return code from pthread_create() is %d\n", rc);
	         exit(-1);
	      }

	      if(pthread_join(threads[i], NULL)) {
	    	  printf("Error joining thread\n");
	    	  exit(-1);
	      }
	}

}

/**
 * This tests notify intrinsic timings in a loop
 */
#ifdef PROBE_LOOP
void test_probe_loop() {
	ticks start;
	ticks end;
	ticks elapsed_time;

	int rounds = 10;
	ticks timings[rounds];

	int j;
	for (j=0; j<rounds; j++) {
		int i=0;

		start = getticks();
		for (i=0; i<10000; i++) {
			empty_func(i);
		}

		end = getticks();

		timings[j] = elapsed(end, start);
		// printf("timings[%d] is : %llu\n", j, timings[j]);

	}


#ifdef PROBE_ON
	printf("--- Median time with annotations (cycles): %llu \n \n", find_median(timings, rounds));
	return;
#endif

	printf("--- Median time without annotations (cycles) : %llu\n", find_median(timings, rounds));

}
#endif

/**
 * This tests notify intrinsic timings in a loop
 */
void test_large_probe_count() {
	ticks start;
	ticks end;
	ticks elapsed_time;

	int rounds = 10;
	ticks timings[rounds];

	int j;
	for (j=0; j<rounds; j++) {
		int i=0;

		start = gettime();
		annotated_func();
		end = gettime();

		timings[j] = end - start ; // elapsed(end, start)
		// printf("timings[%d] is : %llu\n", j, timings[j]);

	}


#ifdef PROBE_ON
	printf("--- Median time with annotations (cycles): %llu \n \n", find_median(timings, rounds));
	return;
#endif

	printf("--- Median time without annotations (cycles): %llu \n", find_median(timings, rounds));

}

#ifdef SINGLE_PROBE
void test_single_probe() {
	ticks start;
	ticks end;
	ticks elapsed_time;

	int i = 0;

	start = getticks();
	empty_func(i);
	end = getticks();

	elapsed_time = elapsed(end, start);

	printf("--- Single probe elapsed time: %llu\n", elapsed_time);

}
#endif

/**
 * Utility functions
 */

void empty_func(int i) {
#ifdef PROBE_ON
	__notify_intrinsic((void*)"emptyFunc",(void*)&i);
#endif
}

/**
 * This tests a function having a large number of notify annotations. Annotations will be injected externally.
 */
void annotated_func() {
	// $?*!$
}

void parameterized_annotated_func(int i) {
	// #$@$%
}


int compare(const void* a, const void* b) {
	return (*(ticks*)a - *(ticks*)b);
}

ticks find_median(ticks* values, int items) {
	qsort(values, items, sizeof(double), compare);
	return values[items/2];
}