#!/bin/bash 



for i in 0 10 100 1000 10000 100000 500000 1000000 ; do 

   echo "Extracting ${i}" 
   awk -v v1=${i} -F, '$2 == v1 { print $1","$2","$3","$4","$5 }' $1 > freq_${i}.csv 
    
   
done 

