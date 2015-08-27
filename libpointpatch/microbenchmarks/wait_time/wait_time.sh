#!/bin/bash 

#set -v
#set -x

USE_CC=$1
USE_CXX=$2

#MAX_WAIT=1500
STEP_SIZE=10
N_TESTS=10
N_RUNS=5

declare -a stats

EXEC=../../tests/test_patch_parallel5.exe 



compileit() { 
  ITERS=$1; 
  echo "Compiling" 
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
    make CC=$USE_CC CXX=$USE_CXX CFLAGS=-DITERS=1000000
    
    for straddle_point in {1..7}; do 
	fail=0; 
	
	for (( run=1;run<=N_RUNS;run++ )) ; do 
	    echo "RUN NUM: $run"
	    $EXEC $straddle_point
	    if [ $? -eq 0 ]; then 
		echo "OK"
	    else 
		fail=1;
		echo "FAILURE"
	    fi; 
	done; 
	if [ $fail -eq 1 ]; then 
	    (( stats[$i]++ )); 
	fi; 
    done; 
done;  

echo ${stats[@]}

echo "Test, WaitIters, NumFailures"
for (( i=0;i<=N_TESTS;i++ )); do 
    
    it=$(( i * STEP_SIZE )); 
    
    echo "$i $it ${stats[$i]}"
    
done; 


