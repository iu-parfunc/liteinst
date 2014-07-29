#!/bin/bash
set -e

# Environment variable
export DYN_STRATEGY="NO_BACKOFF"
fileStem=32_StartProfilerOverhead


# Infrastructure work... remove old files, output info
rm -f $fileStem.cpp
# Put together the source code
cat $fileStem.cp1 >>$fileStem.cpp
for (( j=0; j<$1; j++ ))
do
    echo "void myEmptyFunc$j( void ) {">>$fileStem.cpp
    echo ' __notify_intrinsic((void*)"myEmptyFunc$j:start", (void *)&global_x);'>>$fileStem.cpp
    echo ' __notify_intrinsic((void*)"myEmptyFunc$j:end", (void *)&global_x);'>>$fileStem.cpp
    echo "return; }">>$fileStem.cpp
done
echo "unsigned long int countTEST=$j;">>$fileStem.cpp
cat $fileStem.cp2 >>$fileStem.cpp
