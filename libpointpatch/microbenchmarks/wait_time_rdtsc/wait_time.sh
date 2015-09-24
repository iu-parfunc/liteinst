#!/bin/bash 

#set -v
#set -x

if [ ! $# -eq 6 ]; then 
    echo "INCORRECT ARGS: CC CXX N_TESTS START_POS STEP_SIZE outfile" 
    exit 1;
fi 

USE_CC=$1
USE_CXX=$2
N_TESTS=$3
START_POS=$4
STEP_SIZE=$5
outfile=$6

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
    
    it=$(( i * STEP_SIZE + START_POS )); 
    
    echo "Running test: $i $it"
    
    compileLib $it; 
    compileTests;

    # dir=$(pwd); 

    (cd ../../tests && ./paralleltests.sh 1 14 $outfile 5 $it || true) 

done; 



