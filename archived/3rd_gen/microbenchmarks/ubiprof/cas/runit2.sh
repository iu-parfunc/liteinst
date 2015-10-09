#!/bin/bash 



FAILED=""

for i in {0..65}; do  
    NUMNOPS=-DASM$i make build_toggle4 

    
    ./toggle_4.exe
    if [ $? != 0 ]
    then
	echo $i "SEGFAULTS"
	FAILED+=": ${i}" 
    fi
done 

echo "The following indices failed"  
echo $FAILED
