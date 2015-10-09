#!/bin/bash
set -e

# Environment variable
export DYN_STRATEGY="NO_BACKOFF"
fileStem=31_StartProfilerOverhead

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
    echo " __notify_intrinsic((void*)\"myEmptyFunc$j:start\", (void *)&global_x);">>$fileStem.cpp
    echo " __notify_intrinsic((void*)\"myEmptyFunc$j:end\", (void *)&global_x);">>$fileStem.cpp
    echo "return; }">>$fileStem.cpp
done
echo "unsigned long int countTEST=$j;">>$fileStem.cpp

echo "typedef void (*voidfun)(void);" >>$fileStem.cpp
echo "voidfun funtable[$ARG];"        >>$fileStem.cpp
# echo "(void(*)(void)) funtable[$ARG];" >>$fileStem.cpp
echo "void init_funtable() {" >>$fileStem.cpp
for (( j=0; j<$ARG; j++ ))
do
    echo "  funtable[$j] = & myEmptyFunc$j; ">>$fileStem.cpp
done
echo "}">>$fileStem.cpp


cat $fileStem.cp2 >>$fileStem.cpp
