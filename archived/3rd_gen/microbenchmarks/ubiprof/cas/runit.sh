#!/bin/bash 




for i in {0..1000}; do  
    ./toggle_$1.exe 
    if [ $? != 0 ]
    then
	echo "ERRROROROROOR" 
	break; 
    fi
	
done 
