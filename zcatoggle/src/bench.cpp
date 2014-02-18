
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
	// test_large_probe_count();
	// test_single_probe();

}

/**
 * Test functions
 */	ticks start;
	ticks end;
	ticks elapsed_time;

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
int i=0;

__notify_intrinsic((void*)"large_probe_count_func_0",(void*)&i);

i=1;
__notify_intrinsic((void*)"large_probe_count_func_1",(void*)&i);

i=2;
__notify_intrinsic((void*)"large_probe_count_func_2",(void*)&i);

i=3;
__notify_intrinsic((void*)"large_probe_count_func_3",(void*)&i);

i=4;
__notify_intrinsic((void*)"large_probe_count_func_4",(void*)&i);

i=5;
__notify_intrinsic((void*)"large_probe_count_func_5",(void*)&i);

i=6;
__notify_intrinsic((void*)"large_probe_count_func_6",(void*)&i);

i=7;
__notify_intrinsic((void*)"large_probe_count_func_7",(void*)&i);

i=8;
__notify_intrinsic((void*)"large_probe_count_func_8",(void*)&i);

i=9;
__notify_intrinsic((void*)"large_probe_count_func_9",(void*)&i);

i=10;
__notify_intrinsic((void*)"large_probe_count_func_10",(void*)&i);

i=11;
__notify_intrinsic((void*)"large_probe_count_func_11",(void*)&i);

i=12;
__notify_intrinsic((void*)"large_probe_count_func_12",(void*)&i);

i=13;
__notify_intrinsic((void*)"large_probe_count_func_13",(void*)&i);

i=14;
__notify_intrinsic((void*)"large_probe_count_func_14",(void*)&i);

i=15;
__notify_intrinsic((void*)"large_probe_count_func_15",(void*)&i);

i=16;
__notify_intrinsic((void*)"large_probe_count_func_16",(void*)&i);

i=17;
__notify_intrinsic((void*)"large_probe_count_func_17",(void*)&i);

i=18;
__notify_intrinsic((void*)"large_probe_count_func_18",(void*)&i);

i=19;
__notify_intrinsic((void*)"large_probe_count_func_19",(void*)&i);

i=20;
__notify_intrinsic((void*)"large_probe_count_func_20",(void*)&i);

i=21;
__notify_intrinsic((void*)"large_probe_count_func_21",(void*)&i);

i=22;
__notify_intrinsic((void*)"large_probe_count_func_22",(void*)&i);

i=23;
__notify_intrinsic((void*)"large_probe_count_func_23",(void*)&i);

i=24;
__notify_intrinsic((void*)"large_probe_count_func_24",(void*)&i);

i=25;
__notify_intrinsic((void*)"large_probe_count_func_25",(void*)&i);

i=26;
__notify_intrinsic((void*)"large_probe_count_func_26",(void*)&i);

i=27;
__notify_intrinsic((void*)"large_probe_count_func_27",(void*)&i);

i=28;
__notify_intrinsic((void*)"large_probe_count_func_28",(void*)&i);

i=29;
__notify_intrinsic((void*)"large_probe_count_func_29",(void*)&i);

i=30;
__notify_intrinsic((void*)"large_probe_count_func_30",(void*)&i);

i=31;
__notify_intrinsic((void*)"large_probe_count_func_31",(void*)&i);

i=32;
__notify_intrinsic((void*)"large_probe_count_func_32",(void*)&i);

i=33;
__notify_intrinsic((void*)"large_probe_count_func_33",(void*)&i);

i=34;
__notify_intrinsic((void*)"large_probe_count_func_34",(void*)&i);

i=35;
__notify_intrinsic((void*)"large_probe_count_func_35",(void*)&i);

i=36;
__notify_intrinsic((void*)"large_probe_count_func_36",(void*)&i);

i=37;
__notify_intrinsic((void*)"large_probe_count_func_37",(void*)&i);

i=38;
__notify_intrinsic((void*)"large_probe_count_func_38",(void*)&i);

i=39;
__notify_intrinsic((void*)"large_probe_count_func_39",(void*)&i);

i=40;
__notify_intrinsic((void*)"large_probe_count_func_40",(void*)&i);

i=41;
__notify_intrinsic((void*)"large_probe_count_func_41",(void*)&i);

i=42;
__notify_intrinsic((void*)"large_probe_count_func_42",(void*)&i);

i=43;
__notify_intrinsic((void*)"large_probe_count_func_43",(void*)&i);

i=44;
__notify_intrinsic((void*)"large_probe_count_func_44",(void*)&i);

i=45;
__notify_intrinsic((void*)"large_probe_count_func_45",(void*)&i);

i=46;
__notify_intrinsic((void*)"large_probe_count_func_46",(void*)&i);

i=47;
__notify_intrinsic((void*)"large_probe_count_func_47",(void*)&i);

i=48;
__notify_intrinsic((void*)"large_probe_count_func_48",(void*)&i);

i=49;
__notify_intrinsic((void*)"large_probe_count_func_49",(void*)&i);

i=50;
__notify_intrinsic((void*)"large_probe_count_func_50",(void*)&i);

i=51;
__notify_intrinsic((void*)"large_probe_count_func_51",(void*)&i);

i=52;
__notify_intrinsic((void*)"large_probe_count_func_52",(void*)&i);

i=53;
__notify_intrinsic((void*)"large_probe_count_func_53",(void*)&i);

i=54;
__notify_intrinsic((void*)"large_probe_count_func_54",(void*)&i);

i=55;
__notify_intrinsic((void*)"large_probe_count_func_55",(void*)&i);

i=56;
__notify_intrinsic((void*)"large_probe_count_func_56",(void*)&i);

i=57;
__notify_intrinsic((void*)"large_probe_count_func_57",(void*)&i);

i=58;
__notify_intrinsic((void*)"large_probe_count_func_58",(void*)&i);

i=59;
__notify_intrinsic((void*)"large_probe_count_func_59",(void*)&i);

i=60;
__notify_intrinsic((void*)"large_probe_count_func_60",(void*)&i);

i=61;
__notify_intrinsic((void*)"large_probe_count_func_61",(void*)&i);

i=62;
__notify_intrinsic((void*)"large_probe_count_func_62",(void*)&i);

i=63;
__notify_intrinsic((void*)"large_probe_count_func_63",(void*)&i);

i=64;
__notify_intrinsic((void*)"large_probe_count_func_64",(void*)&i);

i=65;
__notify_intrinsic((void*)"large_probe_count_func_65",(void*)&i);

i=66;
__notify_intrinsic((void*)"large_probe_count_func_66",(void*)&i);

i=67;
__notify_intrinsic((void*)"large_probe_count_func_67",(void*)&i);

i=68;
__notify_intrinsic((void*)"large_probe_count_func_68",(void*)&i);

i=69;
__notify_intrinsic((void*)"large_probe_count_func_69",(void*)&i);

i=70;
__notify_intrinsic((void*)"large_probe_count_func_70",(void*)&i);

i=71;
__notify_intrinsic((void*)"large_probe_count_func_71",(void*)&i);

i=72;
__notify_intrinsic((void*)"large_probe_count_func_72",(void*)&i);

i=73;
__notify_intrinsic((void*)"large_probe_count_func_73",(void*)&i);

i=74;
__notify_intrinsic((void*)"large_probe_count_func_74",(void*)&i);

i=75;
__notify_intrinsic((void*)"large_probe_count_func_75",(void*)&i);

i=76;
__notify_intrinsic((void*)"large_probe_count_func_76",(void*)&i);

i=77;
__notify_intrinsic((void*)"large_probe_count_func_77",(void*)&i);

i=78;
__notify_intrinsic((void*)"large_probe_count_func_78",(void*)&i);

i=79;
__notify_intrinsic((void*)"large_probe_count_func_79",(void*)&i);

i=80;
__notify_intrinsic((void*)"large_probe_count_func_80",(void*)&i);

i=81;
__notify_intrinsic((void*)"large_probe_count_func_81",(void*)&i);

i=82;
__notify_intrinsic((void*)"large_probe_count_func_82",(void*)&i);

i=83;
__notify_intrinsic((void*)"large_probe_count_func_83",(void*)&i);

i=84;
__notify_intrinsic((void*)"large_probe_count_func_84",(void*)&i);

i=85;
__notify_intrinsic((void*)"large_probe_count_func_85",(void*)&i);

i=86;
__notify_intrinsic((void*)"large_probe_count_func_86",(void*)&i);

i=87;
__notify_intrinsic((void*)"large_probe_count_func_87",(void*)&i);

i=88;
__notify_intrinsic((void*)"large_probe_count_func_88",(void*)&i);

i=89;
__notify_intrinsic((void*)"large_probe_count_func_89",(void*)&i);

i=90;
__notify_intrinsic((void*)"large_probe_count_func_90",(void*)&i);

i=91;
__notify_intrinsic((void*)"large_probe_count_func_91",(void*)&i);

i=92;
__notify_intrinsic((void*)"large_probe_count_func_92",(void*)&i);

i=93;
__notify_intrinsic((void*)"large_probe_count_func_93",(void*)&i);

i=94;
__notify_intrinsic((void*)"large_probe_count_func_94",(void*)&i);

i=95;
__notify_intrinsic((void*)"large_probe_count_func_95",(void*)&i);

i=96;
__notify_intrinsic((void*)"large_probe_count_func_96",(void*)&i);

i=97;
__notify_intrinsic((void*)"large_probe_count_func_97",(void*)&i);

i=98;
__notify_intrinsic((void*)"large_probe_count_func_98",(void*)&i);

i=99;
__notify_intrinsic((void*)"large_probe_count_func_99",(void*)&i);

i=100;
}


int compare(const void* a, const void* b) {
	return (*(ticks*)a - *(ticks*)b);
}

ticks find_median(ticks* values, int items) {
	qsort(values, items, sizeof(double), compare);
	return values[items/2];
}
