

#include <stdio.h>
#include "elf-provider.h"

int main () {
  printf("[test] Simple program to read the ELF table of hello_notify.exe\n");
  read_zca_probes("hello_notify.exe");
  // read_zca_probes("/nfs/nfs3/home/rrnewton/working_copies/onlineProfiling/onlineProfiling/niknatar_prototype/test5.exe");

  printf("[test] Done reading probes!\n");
}