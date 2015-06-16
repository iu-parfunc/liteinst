#!/bin/bash 



FAILED=""

for i in {0..65}; do  
    # if [ $i -eq 7 ] 
    # then
    #     echo "Escaping" $i
    #     continue
    # fi

    # if [ $i -eq 7 ]  || [ $i -eq 8 ] || [ $i -eq 9 ] || [ $i -eq 10 ] 
    # then
    #    echo "Escaping" $i
    #    continue
    # fi

    NUMNOPS=-DASM$i make brun6 
    
    ./pt_main_6.exe
    if [ $? != 0 ]
    then
	echo $i "SEGFAULTS"
	FAILED+=": ${i}" 
        exit
    fi
done 

echo "The following indices failed"  
echo $FAILED
