#!/bin/bash

export KMP_AFFINITY=compact
export DYN_STRATEGY="NO_BACKOFF"
#export DYN_STRATEGY="FIXED_BACKOFF"
#export DYN_SAMPLE_SIZE="1000"

rm -f 31_StartProfilerOverhead.cpp
cat 31_StartProfilerOverhead.cp1 >>31_StartProfilerOverhead.cpp
for j in {1..128}
do
    echo "void myEmptyFunc$j( void )">>31_StartProfilerOverhead.cpp
    echo "{">>31_StartProfilerOverhead.cpp
    echo ' __notify_intrinsic((void*)"myEmptyFunc$j:start", (void *)&global_x);'>>31_StartProfilerOverhead.cpp
    echo ' __notify_intrinsic((void*)"myEmptyFunc$j:end", (void *)&global_x);'>>31_StartProfilerOverhead.cpp
    echo "return;">>31_StartProfilerOverhead.cpp
    echo "}">>31_StartProfilerOverhead.cpp
done
    echo "unsigned long int countTEST=$j;">>31_StartProfilerOverhead.cpp
cat 31_StartProfilerOverhead.cp2 >>31_StartProfilerOverhead.cpp

make clean
make

for j in {1..1}
do
    t1="$t1 `./31_StartProfilerOverhead.exe 2>/dev/null | grep 'init (RDTSC)' | cut -d " " -f 3`"
    t2="$t2 `./31_StartProfilerOverhead.exe 2>/dev/null | grep 'init (RDTSC)' | cut -d " " -f 3`"
    t3="$t3 `./31_StartProfilerOverhead.exe 2>/dev/null | grep 'init (RDTSC)' | cut -d " " -f 3`"
    t4="$t4 `./31_StartProfilerOverhead.exe 2>/dev/null | grep 'init (RDTSC)' | cut -d " " -f 3`"
    t5="$t5 `./31_StartProfilerOverhead.exe 2>/dev/null | grep 'init (RDTSC)' | cut -d " " -f 3`"

done
echo $t1
echo $t2
echo $t3
echo $t4
echo $t5
