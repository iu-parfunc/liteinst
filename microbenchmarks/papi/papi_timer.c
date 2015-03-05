#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <memory.h>
#include <malloc.h>
#include <papi.h>

#define INDEX 100

static void test_fail(char* file, int line, char* call, int interval);

int main(int argc, char** argv) {

  extern void dummy(void *);

  float matrixa[INDEX][INDEX], matrixb[INDEX][INDEX], mresult[INDEX][INDEX];
  float real_time, proc_time, mflops;
  long long flpins;
  int retval;
  int i,j,k;

  for (i=0; i<INDEX*INDEX; i++) {
    mresult[0][i] = 0.0;
    matrixa[0][i] = matrixb[0][i] = rand()*(float) 1.1;
  }

  if ((retval=PAPI_flops(&real_time, &proc_time, &flpins, &mflops)) < PAPI_OK)
    test_fail(__FILE__, __LINE__, "PAPI_flops", retval);

  for (i=0; i<INDEX; i++) 
    for (j=0; j<INDEX; j++)
      for (k=0; k<INDEX; k++)
        mresult[i][j] = mresult[i][j] + matrixa[i][k]*matrixb[k][j];

  if ((retval = PAPI_flops(&real_time, &proc_time, &flpins, &mflops)) < PAPI_OK) {
    test_fail(__FILE__, __LINE__, "PAPI_flops", retval);
  }

  printf("Real_time:\t%f\nProc_time:\t%f\nTotal flpins:\t%lld\nMFLOPS:\t\t%f\n",
      real_time, proc_time, flpins, mflops);
  printf("%s\tPASSED\n", __FILE__);
  PAPI_shutdown();
  exit(0);

}

static void test_fail(char* file, int line, char* call, int retval) {
  printf("%s\tFAILED\nLine # %d\n", file, line);
  if (retval == PAPI_ESYS) {
    char buf[128];
    memset(buf, '\0', sizeof(buf));
    sprintf(buf, "System error in %s:", call);
    perror(buf);
  } else if (retval > 0) {
    printf("Error calculating: %s\n", call);
  } else {
    printf("Error in %s: %s\n", call, PAPI_strerror(retval));
  }
  printf("\n");
  exit(1);

}
