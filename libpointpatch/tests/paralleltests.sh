#!/bin/bash

success=0;
failed=0;

fails=""



if [ ! $# -eq 5 ]; then 
    echo "Provide args: #minthreads #maxthreads csvfile #runs waitsetting" 
    exit 1
fi 


minthreads=$1
maxthreads=$2 
csvout=$3
nruns=$4
wait=$5

# PARALLEL TESTS ONLY 

for f in test_patch_parallel1.exe; do
    for (( run=0;run<nruns;run++ )); do 
	echo ""
	echo ""
	
	echo '****************************'
	echo "RUNNING TEST $f"
	echo "RUN: $run" 
	echo '****************************'
	
	time PATCH_WAIT_TIME=$wait ./${f}
	if [ $? -eq 0 ]; then
	    echo '****************************'
	    echo "$f PASSED"
	    echo '****************************'
	    success=$((success+1))
	else
	    echo '****************************'
	    echo "$f FAILED! "
	    echo '****************************'
	    
	    fails="$fails $f"
	    failed=$((failed+1))
    fi
    done; 
done; 

#test all straddling points att varying numbers of threads 
# hitting the call site. 
for f in test_patch_parallel5.exe test_patch_parallel6.exe test_patch_parallel7.exe; do
    for  (( threads=minthreads;threads<=maxthreads;threads++ )); do 
	#only try those straddler locations that are likely to fail (save time)
	for i in 1 2 3 4; do # straddler location 
	    for (( run=0;run<nruns;run++ )); do 
		echo ""
		echo ""

		echo '****************************'
		echo "RUNNING TEST $f"
		echo "RUN: $run" 
		echo '****************************'

		time PATCH_WAIT_TIME=$wait ./${f} $i $threads
 		if [ $? -eq 0 ]; then
		    echo '****************************'
		    echo "$f PASSED"
		    echo '****************************'
		    success=$((success+1))
		else
		    echo '****************************'
		    echo "$f FAILED! "
		    echo '****************************'
		    
		    fails="$fails $f($i,$threads)"
		    failed=$((failed+1))
		fi	    
	    done;
	done;
    done;
done; 


echo ""
echo ""

echo "***** Test summary *****"
echo "Num passed tests: " $success
echo "Num failed tests: " $failed
echo "Failed tests: " $fails


echo "$wait, $failed" >> $csvout


if [ "$failed" == "0" ];
then
    exit 0;
else
    exit 1;
fi
