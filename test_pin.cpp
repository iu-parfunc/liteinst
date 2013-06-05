#include <stdio.h>
#include <pin.H>
#include <zca_table.h>

static ZCA::zca_table zca;

void fun1()
{
  printf("Lower your shields and surrender your ships.\n");
}

void fun2()
{
  printf("Resistance is futile.\n");
}

int main (int argc, char *argv[])
{
  PIN_InitSymbols();
  if (PIN_Init(argc, argv)) {
    printf("pin couldn't parse args\n");
  }

  zca.initialize();

  zca.insert_annotation_calls("cilk_detach_end",
			      (AFUNPTR)fun1,
			      IARG_END);

  zca.insert_annotation_calls("exited",
			      (AFUNPTR)fun2,
			      IARG_END);
    
  PIN_StartProgram();
  return 0;
}
