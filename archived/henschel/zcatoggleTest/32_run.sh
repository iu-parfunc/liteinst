#!/bin/bash

# Environment variable
export DYN_STRATEGY="NO_BACKOFF"
fileStem=32_StartProfilerOverhead

# Remove old output files
rm $fileStem.csv $fileStem.csv.make_output

# Output important information
echo "$fileStem">>$fileStem.csv
t0="Functions with _notify_intrinsics"
echo "Output can be found in:"
echo "   - $fileStem.csv"
echo "   - $fileStem.csv.make_output"

# Iterate through function numbers
#for i in 1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384
for i in 1 2 4 8
do
    # Infrastructure work... remove old files, output info
    echo "Functions: $i"
    echo "   Creating source code"
    t0="$t0,$i"
    rm -f $fileStem.cpp

    # Put together the source code
    cat $fileStem.cp1 >>$fileStem.cpp
    for (( j=0; j<$i; j++ ))
    do
	echo "void myEmptyFunc$j( void )">>$fileStem.cpp
	echo "{">>$fileStem.cpp
	echo ' __notify_intrinsic((void*)"myEmptyFunc$j:start", (void *)&global_x);'>>$fileStem.cpp
	echo ' __notify_intrinsic((void*)"myEmptyFunc$j:end", (void *)&global_x);'>>$fileStem.cpp
	echo "return;">>$fileStem.cpp
	echo "}">>$fileStem.cpp
    done
    echo "unsigned long int countTEST=$j;">>$fileStem.cpp
    cat $fileStem.cp2 >>$fileStem.cpp

    # Compile source code, exit if make fails
    echo "   Compiling"
    make clean >>/dev/null
    make $fileStem.exe >>$fileStem.csv.make_output
    if [[ $? != 0 ]] ; then
	exit $rc
    fi

    # Run 5 times and capture the key metric
    t1="$t1,`./$fileStem.exe 2>/dev/null | grep 'init (RDTSC)' | cut -d " " -f 3`"
    echo "   Repetition 1"
    t2="$t2,`./$fileStem.exe 2>/dev/null | grep 'init (RDTSC)' | cut -d " " -f 3`"
    echo "   Repetition 2"
    t3="$t3,`./$fileStem.exe 2>/dev/null | grep 'init (RDTSC)' | cut -d " " -f 3`"
    echo "   Repetition 3"
    t4="$t4,`./$fileStem.exe 2>/dev/null | grep 'init (RDTSC)' | cut -d " " -f 3`"
    echo "   Repetition 4"
    t5="$t5,`./$fileStem.exe 2>/dev/null | grep 'init (RDTSC)' | cut -d " " -f 3`"
    echo "   Repetition 5"
done

# Output CSV info
echo $t0>>$fileStem.csv
echo "Cycle count">>$fileStem.csv
echo $t1>>$fileStem.csv
echo $t2>>$fileStem.csv
echo $t3>>$fileStem.csv
echo $t4>>$fileStem.csv
echo $t5>>$fileStem.csv
