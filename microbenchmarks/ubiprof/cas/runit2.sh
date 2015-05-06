#!/bin/bash 




for i in {0..9}; do  
    NUMNOPS=-DASM$i make build_toggle4 

    
    ./toggle_4.exe
    if [ $? != 0 ]
    then
	echo $i "SEGFAULTS" 
    fi
	
done 
