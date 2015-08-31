#!/bin/bash 

#set -v
#set -x

USE_CC=$1
USE_CXX=$2

#MAX_WAIT=1500
STEP_SIZE=10
N_TESTS=10
N_RUNS=5

OUTFILE=out.csv

echo "Iters, Patches, RDTSC, RDTSC_Per_Patch, Sec_Total" > $OUTFILE

EXEC=./sequential_straddling_patch.exe

compileit() { 
  ITERS=$1; 
  echo "Compiling LIBRARY" 
  echo "Wait iterations:  $ITERS"
  
  make cleanLib
  make installLib CC=$USE_CC CXX=$USE_CXX CFLAGS=-DWAIT_ITERS=$ITERS

} 

for (( i=0;i<=N_TESTS;i++ )); do 
   
    it=$(( i * STEP_SIZE ));
    
    stats[$i]=0; 
    echo $it
    compileit $it
    # There is static linking going on ! 
    make clean
    make seq_straddle CC=$USE_CC CXX=$USE_CXX 
    
    
    fail=0; 
    
    $EXEC $OUTFILE 2
    if [ $? -eq 0 ]; then 
	echo "OK"
    else 
	fail=1;
	echo "FAILURE"
    fi; 
done;  




