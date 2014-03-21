#include "zca-toggle.hpp"
#include "profiler.hpp"
#include <pthread.h>
#include <time.h>
#include <stdlib.h>
#include <unistd.h>
#include "cycle.h"

void func2();

void start_profile(char* method, void* fun) {
	if (fun != NULL) {
		activateProbe(method, fun);
	} else {
		activateProbe(method, profiler_func);
	}
}

void stop_profile(char* method) {
	deactivateProbe(method);
}

void profile_all() {

}

void turn_off_profiler() {

}

/*uint64_t gettid() {
	pthread_t ptid = pthread_self();
	uint64_t threadId = 0;
	memcpy(&threadId, &ptid, std::min(sizeof(threadId), sizeof(ptid)));
	return threadId;
}*/

void profiler_func() {

/*	__thread static prof_data* data;

	if (data == NULL) {
		data = (prof_data*)malloc(sizeof(prof_data));
		data->start = -1;
	}

	if (data->start == -1) {
		data->start = getticks();
	} else {
		ticks end = getticks();
		ticks elapsed = end - data->start;

		if (elapsed < data->min || data->min == 0) {
			data->min = elapsed;
		}

		if (elapsed > data->max) {
			data->max = elapsed;
		}

		data->sum = data->sum + elapsed;
		data->count += 1;

		data->start = -1;
	}

	if (data->count == 1000) {
		printf("Min : %lu", data->min);
		printf("Max : %lu", data->max);
		printf("Avg: %lu", data->sum / 1000);
	}*/

	printf("Inside profile function..\n");
	return;

}

ticks func1_min = 0;
ticks func1_max = 0;
ticks func1_sum = 0;
int func1_count = 0;
int spin_lock_1 = 0;


void func1(){
	int x;
	ticks start = getticks();
	__notify_intrinsic((void*)"func1_start",(void*)&x);

	// func2();

	uint64_t i;
	for (i=0; i < 1000; i++) {
		srand(time(NULL));
		int r = rand();

		// func2();
	}

	__notify_intrinsic((void*)"func1_end",(void*)&x);
	ticks end = getticks();
    ticks elapsed = (end - start);

	while (!(__sync_bool_compare_and_swap(&spin_lock_1, 0 , 1)));

	if (elapsed < func1_min || func1_min == 0) {
		func1_min = elapsed;
	}

	if (elapsed > func1_max) {
		func1_max = elapsed;
	}

	func1_sum += elapsed;
	func1_count++;

    __sync_bool_compare_and_swap(&spin_lock_1, 1 , 0);

	if (func1_count == 2000) {
	    printf("\nMin elapsed time (total : func + prof overhead) in func1 : %lu\n", func1_min);
	    printf("Max elapsed time (total : func + prof overhead) in func1 : %lu\n", func1_max);
	    printf("Avg elapsed time (total : func + prof overhead) in func1 : %lu\n", (func1_sum / func1_count));
	}
}

ticks func2_min = 0;
ticks func2_max = 0;
ticks func2_sum = 0;
int func2_count = 0;
int spin_lock_2 = 0;

void func2() {
	int x;
	ticks start = getticks();
	__notify_intrinsic((void*)"func2_start",(void*)&x);

/*	uint64_t i;
	for (i=0; i < 1000; i++) {
		srand(time(NULL));
		int r = rand();
	}*/

	__notify_intrinsic((void*)"func2_end",(void*)&x);
	ticks end = getticks();
    ticks elapsed = (end - start);

	while (!(__sync_bool_compare_and_swap(&spin_lock_2, 0 , 1)));

	if (elapsed < func2_min || func2_min == 0) {
		func2_min = elapsed;
	}

	if (elapsed > func2_max) {
		func2_max = elapsed;
	}

	func2_sum += elapsed;
	func2_count++;

	__sync_bool_compare_and_swap(&spin_lock_2, 1 , 0);

	if (func2_count == 2000) {
	    printf("\nMin elapsed time (total : func + prof overhead) in func2 : %lu\n", func2_min);
	    printf("Max elapsed time (total : func + prof overhead) in func2 : %lu\n", func2_max);
	    printf("Avg elapsed time (total : func + prof overhead) in func2 : %lu\n", (func2_sum / func2_count));
	}
}

void* func1_loop(void* tid) {

	// start_profile("func1_start", NULL);
	// start_profile("func1_end", NULL);
	int i;
#pragma noinline recursive
	for (i=0; i < 1000; i++) {
		func1();
		func2();
	}

	// stop_profile("func1_start");
	// stop_profile("func1_end");

	return NULL;
}

void* func2_loop(void* tid) {

	// start_profile("func2_start", NULL);
	// start_profile("func2_end", NULL);

	int i;
	for (i=0; i < 1000; i++) {
		func2();
		func1();
	}

	// stop_profile("func2_start");
	// stop_profile("func2_end");

	return NULL;
}

int main() {

	initZCAService();
	pthread_t func1_t;
	int rc = pthread_create(&func1_t, NULL, func1_loop, (void*)"Function_1");
	if (rc){
		printf("ERROR; return code from pthread_create() is %d\n", rc);
		exit(-1);
	}

	if(pthread_join(func1_t, NULL)) {
		printf("Error joining thread\n");
		exit(-1);
	}

	pthread_t func2_t;
	rc = pthread_create(&func2_t, NULL, func2_loop, (void*)"Function_2");
	if (rc){
		printf("ERROR; return code from pthread_create() is %d\n", rc);
		exit(-1);
	}

	if(pthread_join(func1_t, NULL)) {
		printf("Error joining thread\n");
		exit(-1);
	}
}


