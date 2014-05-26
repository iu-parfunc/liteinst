#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string>
#include "zca-toggle.hpp"

int x;

/******************* Probe Function ****************/
void probe_function() {
	printf("Hello my little world!!!\n");
}

/******************* Test Functions ********************/
void foo() {
	__notify_intrinsic((void*)"foo:start",(void*)&x);

        int i;
	for (i=0; i < 100; i++) {
		srand(time(NULL));
		int r = rand();
	}

	__notify_intrinsic((void*)"foo:end",(void*)&x);
}

/******************* Tests *************************/
void test_probe_activation() {

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
	initZCAService();

	test_probe_activation();

}
