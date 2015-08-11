
success=0; 
failed=0; 

FAILURES=

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
	#FAILURES+="$f"
	failed=$((failed+1))
    fi
done; 

echo "Passed tests: " $success
echo "Failed tests: " $failed
echo $FAILURES
