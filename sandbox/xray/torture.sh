#!/bin/bash 

#set -v
#set -x

N_TESTS=10

declare -a stats

EXEC=./xray.exe 


compileit() { 
  ITERS=$1; 
  echo "Compiling" 
  echo "Wait iterations:  $ITERS"
  
   make cleanLib
   make installLib CC=$USE_CC CXX=$USE_CXX CFLAGS=-DWAIT_ITERS=$ITERS

} 


for (( i=2;i<=N_TESTS;i++ )); do 
   
    echo -e "\nTesting Straddler Position "$i
    echo ""

    # There is static linking going on ! 
    make clean
    make all 
    	 
    time timeout 4h $EXEC $i
    if [ $? -eq 124 ]; then 
	echo "OK"
	echo "Safe wait setting: $i"
	exit 0

    else 
	echo "FAILURE: $i"
    fi; 
done;  


