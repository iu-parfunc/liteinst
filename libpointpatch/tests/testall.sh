#!/bin/bash

success=0;
failed=0;

fails=""

for f in *.exe ; do
    if [ $f  =  '*.exe' ] ;
	then echo "Build tests first"
	exit;
    fi;
    echo ""
    echo ""

    echo '****************************'
    echo 'RUNNING TEST'
    echo '****************************'


    ./${f}
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

# Parameterised tests:
# parameterised tests should have a default param that is run as part as the
# unparameterised tests above.

#test all straddling points
for f in test_patch_parallel4.exe test_patch_parallel5.exe; do
    for i in 2 3 4 5 6 7 ; do   # one is tested above
	echo ""
	echo ""

	echo '****************************'
	echo 'RUNNING TEST'
	echo '****************************'

	./${f} $i
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

echo ""
echo ""

echo "***** Test summary *****"
echo "Num passed tests: " $success
echo "Num failed tests: " $failed
echo "Failed tests: " $fails

if [ "$failed" == "0" ];
then
    exit 0;
else
    exit 1;
fi