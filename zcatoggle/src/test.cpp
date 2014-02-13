

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
void test_large_probe_count();
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

	test_probe_loop();

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
		for (i=0; i<100; i++) {
			empty_func(i);
		}

		end = getticks();

		timings[j] = elapsed(end, start);
		// printf("timings[%d] is : %llu\n", j, timings[j]);

	}


#ifdef PROBE_ON
	printf("\nMedian time with annotations : %llu\n", find_median(timings, rounds));
	return;
#endif

	printf("Median time without annotations : %llu", find_median(timings, rounds));

}

/**
 * This tests a function having a large number of notify annotations. Annotations will be injected externally.
 */
void test_large_probe_count() {
	// $?*!$
}

/**
 * Utility functions
 */

void empty_func(int i) {
#ifdef PROBE_ON
	__notify_intrinsic((void*)"emptyFunc",(void*)&i);
#endif
}

int compare(const void* a, const void* b) {
	return (*(ticks*)a - *(ticks*)b);
}

ticks find_median(ticks* values, int items) {
	qsort(values, items, sizeof(double), compare);
	return values[items/2];
}
