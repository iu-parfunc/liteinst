#!/bin/bash

success=0;
failed=0;

fails=""




#test all straddling points att varying numbers of threads 
# hitting the call site. 
for f in test_async_patch1.exe test_async_patch2.exe test_async_patch3.exe test_async_patch4.exe ; do
    for threads in 1 2 3 4 5 6 7; do 
	for i in 1 2 3 4 ; do   # one is tested above
	    echo ""
	    echo ""

	    echo '****************************'
	    echo "RUNNING TEST $f"
	    echo '****************************'

	    time ./${f} $i $threads
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

echo ""
echo ""

echo "***** Test summary *****"
echo "Num passed tests: " $success
echo "Num failed tests: " $failed
echo "Failed tests: " $fails

if [ $# -eq 2 ]; then 
    echo "Appending data to file $2"
    echo "$1, $failed" >> $2 
fi 


if [ "$failed" == "0" ];
then
    exit 0;
else
    exit 1;
fi
