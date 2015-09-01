#!/bin/bash 

#set -v
#set -x

USE_CC=$1
USE_CXX=$2

#MAX_WAIT=1500
STEP_SIZE=10
N_TESTS=30
N_RUNS=5

declare -a stats

EXEC=./test_patch_parallel5_modified.exe 



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
    make CC=$USE_CC CXX=$USE_CXX 
    
  
    echo "**********************"
    echo "wait time iters: $it"
    echo "Straddle point: 2"
    echo "**********************"
	 
    for (( run=1;run<=N_RUNS;run++ )) ; do 
	echo "RUN NUM: $run"
	$EXEC 10000000 8 2
	if [ $? -eq 0 ]; then 
	    echo "OK"
	else 
	    (( stats[$i]++ )); 
	    echo "FAILURE"
	fi; 
    done; 
    
done; 

echo ${stats[@]}

echo "Test, WaitIters, NumFailures"
for (( i=0;i<=N_TESTS;i++ )); do 
    
    it=$(( i * STEP_SIZE )); 
    
    echo "$i $it ${stats[$i]}"
    
done; 


