#!/bin/bash

export KMP_AFFINITY=compact

for j in {1..32}
do
    export OMP_NUM_THREADS=$j
#    echo $j
    t1="$t1 `./06_ActivatedNotifyLoopParallel.exe | grep gettime | cut -d " " -f 3`"
    t2="$t2 `./06_ActivatedNotifyLoopParallel.exe | grep gettime | cut -d " " -f 3`"
    t3="$t3 `./06_ActivatedNotifyLoopParallel.exe | grep gettime | cut -d " " -f 3`"
    t4="$t4 `./06_ActivatedNotifyLoopParallel.exe | grep gettime | cut -d " " -f 3`"
    t5="$t5 `./06_ActivatedNotifyLoopParallel.exe | grep gettime | cut -d " " -f 3`"
done
echo $t1
echo $t2
echo $t3
echo $t4
echo $t5
