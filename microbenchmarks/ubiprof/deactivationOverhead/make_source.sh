#!/bin/bash
set -e

# Environment variable
export DYN_STRATEGY="NO_BACKOFF"
fileStem=40_deactivate

ARG=$1

if [ "$ARG" == "" ]; 
then echo "make_source.sh needs one numeric argument"; exit 1
fi

# Infrastructure work... remove old files, output info
rm -f $fileStem.cpp
# Put together the source code
cat $fileStem.cp1 >>$fileStem.cpp
for (( j=0; j<$ARG; j++ ))
do
    echo "void myEmptyFunc$j( void ) {">>$fileStem.cpp
    echo -e "return; \n}\n">>$fileStem.cpp
done

echo "void invoke() { ">>$fileStem.cpp
for (( j=0; j<$ARG; j++ ))
do
    echo "myEmptyFunc$j();">>$fileStem.cpp
done
echo -e "\n}\n ">>$fileStem.cpp

echo "unsigned long int countTEST=$j;">>$fileStem.cpp
cat $fileStem.cp2 >>$fileStem.cpp

for (( j=0; j<$1; j++ ))
do
    echo "PROFILER_INSTANCE->deactivateFunctionByName((void*)myEmptyFunc$j);">>$fileStem.cpp
#    echo "deactivate_method_profiling(\"myEmptyFunc$j\");">>$fileStem.cpp
done


cat $fileStem.cp3 >>$fileStem.cpp
