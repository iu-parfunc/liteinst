#!/bin/bash 

#set -v
#set -x

USE_CC=$1
USE_CXX=$2

#MAX_WAIT=1500
STEP_SIZE=100
N_TESTS=20


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

    echo "RUNNING TEST:" 
    
    it=$(( i * STEP_SIZE )); 
    
    echo $i $it
    
    compileLib $it; 
    compileTests;
    
    (cd ../../tests && ./testall.sh $it iter_fail.out || true) 

done; 

