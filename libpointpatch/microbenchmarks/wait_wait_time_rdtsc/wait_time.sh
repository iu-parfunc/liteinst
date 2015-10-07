#!/bin/bash 

#set -v
#set -x

if [ ! $# -eq 5 ]; then 
    echo "INCORRECT ARGS: N_TESTS START_POS STEP_SIZE N_RUNS outfile" 
    exit 1;
fi 

N_TESTS=$1
START_POS=$2
STEP_SIZE=$3
N_RUNS=$4
outfile=$5

# The wait iters are now set as a env var "PATCH_WAIT_TIME" 
# so recompilation of the library and tests are no-longer needed! 

for (( i=0;i<N_TESTS;i++ )); do 
    
    it=$(( i * STEP_SIZE + START_POS )); 
    
    echo "Running test: $i $it"
   
    (cd ../../tests && 
	./test7.sh 2 6 $outfile $N_RUNS $it || true) 

done; 



