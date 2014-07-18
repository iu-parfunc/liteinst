#!/bin/bash

export KMP_AFFINITY=compact
export DYN_STRATEGY="NO_BACKOFF"

for j in {1..32}
do
    export OMP_NUM_THREADS=$j
#    echo $j
#    t1="$t1 `./01_emptyFunctionParallel.exe | grep gettime | cut -d " " -f 3`"
#    t2="$t2 `./01_emptyFunctionParallel.exe | grep gettime | cut -d " " -f 3`"
#    t3="$t3 `./01_emptyFunctionParallel.exe | grep gettime | cut -d " " -f 3`"
#    t4="$t4 `./01_emptyFunctionParallel.exe | grep gettime | cut -d " " -f 3`"
#    t5="$t5 `./01_emptyFunctionParallel.exe | grep gettime | cut -d " " -f 3`"

#    t1="$t1 `./02_notActivatedNotifyFunctionParallel.exe | grep gettime | cut -d " " -f 3`"
#    t2="$t2 `./02_notActivatedNotifyFunctionParallel.exe | grep gettime | cut -d " " -f 3`"
#    t3="$t3 `./02_notActivatedNotifyFunctionParallel.exe | grep gettime | cut -d " " -f 3`"
#    t4="$t4 `./02_notActivatedNotifyFunctionParallel.exe | grep gettime | cut -d " " -f 3`"
#    t5="$t5 `./02_notActivatedNotifyFunctionParallel.exe | grep gettime | cut -d " " -f 3`"

#    t1="$t1 `./03_ActivatedNotifyFunctionParallel.exe | grep gettime | cut -d " " -f 3`"
#    t2="$t2 `./03_ActivatedNotifyFunctionParallel.exe | grep gettime | cut -d " " -f 3`"
#    t3="$t3 `./03_ActivatedNotifyFunctionParallel.exe | grep gettime | cut -d " " -f 3`"
#    t4="$t4 `./03_ActivatedNotifyFunctionParallel.exe | grep gettime | cut -d " " -f 3`"
#    t5="$t5 `./03_ActivatedNotifyFunctionParallel.exe | grep gettime | cut -d " " -f 3`"

#    t1="$t1 `./04_emptyLoopParallel.exe | grep gettime | cut -d " " -f 3`"
#    t2="$t2 `./04_emptyLoopParallel.exe | grep gettime | cut -d " " -f 3`"
#    t3="$t3 `./04_emptyLoopParallel.exe | grep gettime | cut -d " " -f 3`"
#    t4="$t4 `./04_emptyLoopParallel.exe | grep gettime | cut -d " " -f 3`"
#    t5="$t5 `./04_emptyLoopParallel.exe | grep gettime | cut -d " " -f 3`"

#    t1="$t1 `./05_notActivatedNotifyLoopParallel.exe | grep gettime | cut -d " " -f 3`"
#    t2="$t2 `./05_notActivatedNotifyLoopParallel.exe | grep gettime | cut -d " " -f 3`"
#    t3="$t3 `./05_notActivatedNotifyLoopParallel.exe | grep gettime | cut -d " " -f 3`"
#    t4="$t4 `./05_notActivatedNotifyLoopParallel.exe | grep gettime | cut -d " " -f 3`"
#    t5="$t5 `./05_notActivatedNotifyLoopParallel.exe | grep gettime | cut -d " " -f 3`"

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
