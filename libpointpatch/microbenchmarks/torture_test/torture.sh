#!/bin/bash 

#set -v
#set -x

USE_CC=$1
USE_CXX=$2

STEP_SIZE=100
N_TESTS=20

declare -a stats

EXEC=./infinite.exe 


compileit() { 
  ITERS=$1; 
  echo "Compiling" 
  echo "Wait iterations:  $ITERS"
  
   make cleanLib
   make installLib CC=$USE_CC CXX=$USE_CXX CFLAGS=-DWAIT_ITERS=$ITERS

} 


for (( i=1;i<=N_TESTS;i++ )); do 
   
    it=$(( i * STEP_SIZE ));
   
    echo $it
    compileit $it
    # There is static linking going on ! 
    make clean
    make all CC=$USE_CC CXX=$USE_CXX 
    	 
    time timeout 4h $EXEC 2
    if [ $? -eq 124 ]; then 
	echo "OK"
	echo "Safe wait setting: $i"
	exit 0

    else 
	echo "FAILURE: $i"
    fi; 
done;  


