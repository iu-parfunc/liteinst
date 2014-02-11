

#include <stdio.h>
#include "zca-toggle.h"
#include <sys/mman.h>
#include <errno.h>

void __attribute__ ((constructor)) premain()
{
	initZCAService();
}


int main () {
  // initZCAService();
  printf("[test] Simple program to read the ELF table of hello_notify.exe\n");
  // read_zca_probes("hello_notify.exe");
  __notify_intrinsic((void*)"notify01",(void*)99);
  // int probes = read_self_zca_probes();
  // printf("***** Probe count : %d\n", probes);
  __notify_intrinsic((void*)"notify01",(void*)99);

  printf("[test] Done reading probes!\n");
}
