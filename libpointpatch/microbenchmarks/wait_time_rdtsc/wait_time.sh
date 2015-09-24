#!/bin/bash 

#set -v
#set -x

USE_CC=$1
USE_CXX=$2

#MAX_WAIT=1500
STEP_SIZE=100
N_TESTS=$3
START_POS=$4


compileLib() { 
    ITERS=$1; 
    echo "Compiling Library" 
    echo "Wait iterations:  $ITERS"
    
    make cleanLib
    make installLib CC=$USE_CC CXX=$USE_CXX CFLAGS=-DWAIT_ITERS=$ITERS
} 

# Do we really need to recompile these ?
# I think we do since we statically link against library.. (for some compilers?)
compileTests() {
    echo "Compiling Tests" 
    
    make cleanTests
    make buildTests CC=$USE_CC CXX=$USE_CXX
    
}

for (( i=0;i<=N_TESTS;i++ )); do 

    echo "WOHOO" 
    
    it=$(( i * STEP_SIZE + START_POS )); 
    
    echo $i $it
    
    compileLib $it; 
    compileTests;
    
    (cd ../../tests && ./testall.sh $it data.out || true) 

done; 

# for (( i=0;i<=N_TESTS;i++ )); do 
   
#     it=$(( i * STEP_SIZE ));
    
#     stats[$i]=0; 
#     echo $it
#       compileit $it
#     # There is static linking going on ! 
#     make clean
#     make CC=$USE_CC CXX=$USE_CXX CFLAGS=-DITERS=1000000
    
#     for straddle_point in {1..7}; do 
# 	fail=0; 
# 	echo "**********************"
# 	echo "wait time iters: $it"
# 	echo "Straddle point: $straddle_point"
# 	echo "**********************"
	 
# 	for (( run=1;run<=N_RUNS;run++ )) ; do 
# 	    echo "RUN NUM: $run"
# 	    $EXEC $straddle_point
# 	    if [ $? -eq 0 ]; then 
# 		echo "OK"
# 	    else 
# 		fail=1;
# 		echo "FAILURE"
# 	    fi; 
# 	done; 
# 	if [ $fail -eq 1 ]; then 
# 	    (( stats[$i]++ )); 
# 	fi; 
#     done; 
# done;  

# echo ${stats[@]}

# echo "Test, WaitIters, NumFailures"
# for (( i=0;i<=N_TESTS;i++ )); do 
    
#     it=$(( i * STEP_SIZE )); 
    
#     echo "$i $it ${stats[$i]}"
    
# done; 


