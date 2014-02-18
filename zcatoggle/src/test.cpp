
#include <stdio.h>
#include <stdlib.h>
#include "zca-toggle.h"
#include <sys/mman.h>
#include <errno.h>
#include "cycle.h"

#if PROFILE == 1
#define PROBE_ON
#endif

void empty_func(int i);
void test_probe_loop();
void test_single_probe();
void test_large_probe_count();
void large_probe_count();
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

	// test_probe_loop();
	test_large_probe_count();
	// test_single_probe();

}

/**
 * Test functions
 */

/**
 * This tests notify intrinsic timings in a loop
 */
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
	printf("\n[Probe Loop] Median time with annotations : %llu\n", find_median(timings, rounds));
	return;
#endif

	printf("[Probe Loop] Median time without annotations : %llu", find_median(timings, rounds));

}

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

		start = getticks();
		large_probe_count();
		end = getticks();

		timings[j] = elapsed(end, start);
		// printf("timings[%d] is : %llu\n", j, timings[j]);

	}


#ifdef PROBE_ON
	printf("\n[Large Probe] Median time with annotations : %llu\n", find_median(timings, rounds));
	return;
#endif

	printf("[Large Probe] Median time without annotations : %llu\n", find_median(timings, rounds));

}

void test_single_probe() {
	ticks start;
	ticks end;
	ticks elapsed_time;

	int i = 0;

	start = getticks();
#ifdef PROBE_ON
	__notify_intrinsic((void*)"singleProbe",(void*)&i);
#endif
	end = getticks();

	elapsed_time = elapsed(end, start);

	printf("[Single Probe] Single probe elapsed time: %llu\n", elapsed_time);

}

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
void large_probe_count() {
	// $?*!$
}


int compare(const void* a, const void* b) {
	return (*(ticks*)a - *(ticks*)b);
}

ticks find_median(ticks* values, int items) {
	qsort(values, items, sizeof(double), compare);
	return values[items/2];
}
