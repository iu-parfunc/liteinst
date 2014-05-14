#include <stdio.h>
#include <pin.H>
#include <zca_table.h>

static ZCA::zca_table zca;

void fun1()
{
  printf("[tool] Lower your shields and surrender your ships.\n");
}

void fun2()
{
  printf("[tool] Resistance is futile.\n");
}

int main (int argc, char *argv[])
{
  printf(" [pintool] In main...\n");
  // PIN_InitSymbols();
  if (PIN_Init(argc, argv)) {
    printf("pin couldn't parse args\n");
  }
  printf(" [pintool] about to call zca.initialize...\n");
  zca.initialize();
  
  zca.insert_annotation_calls("entered region",
  			      (AFUNPTR)fun1,
  			      IARG_END);
  
  zca.insert_annotation_calls("exited",
			      (AFUNPTR)fun2,
  			      IARG_END);
  
  PIN_StartProgram();
  return 0;
}

