#!/bin/bash 

#set -v
#set -x

USE_CC=$1
USE_CXX=$2

STEP_SIZE=100
N_TESTS=20

declare -a stats

EXEC=./infinite.exe 


echo "Compiling" 
  
make cleanLib
make installLib CC=$USE_CC CXX=$USE_CXX EXTRA_FLAGS=-DWAIT_SPIN_RDTSC

make clean
make all CC=$USE_CC CXX=$USE_CXX

echo "Running tests" 

for (( i=1;i<=N_TESTS;i++ )); do 
   
    it=$(( i * STEP_SIZE ));
   
    echo $it
    #compileit $it
    # There is static linking going on ! 
    #make clean
    #make all CC=$USE_CC CXX=$USE_CXX 
    
    export PATCH_WAIT_TIME=$it
    echo "Patch wait: $PATCH_WAIT_ITERS"
#    time timeout 4h $EXEC 2
    time timeout 1m $EXEC 2
    if [ $? -eq 124 ]; then 
	echo "OK"
	echo "Safe wait setting: $i"
	exit 0

    else 
	echo "FAILURE: $i"
    fi; 
done;  


