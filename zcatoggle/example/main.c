#include "zca-toggle.h"

int main () {

	printf("And this text is printed from the top of \"main\".\n");

	printf("hello world with a notify instruction\n");
	__notify_intrinsic("notify01",(void*)99);
	printf("Filler instructions");
	__notify_intrinsic("notify02",(void*)3299);

	// myFunc();
	return 0;
}
