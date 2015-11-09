#!/bin/bash
set -e

# Environment variable
fileStem=funcs

# Infrastructure work... remove old files, output info
rm -f $fileStem.hpp
rm -f $fileStem.cpp

file=$fileStem.hpp
touch $file
echo "void emptyFunc( void ) __attribute__((noinline));">>$file
for (( j=0; j<$1; j++ ))
do
    echo "void emptyFunc$j( void ) __attribute__((noinline));">>$file
done

file=$fileStem.cpp
touch $file
echo "void emptyFunc( void ) {">>$file
echo "  return; }" >> $file
for (( j=0; j<$1; j++ ))
do
    echo "void emptyFunc$j( void ) {">>$file
    echo "  return; }" >> $file
done

fileStem=init_cost
# Put together the source code
cat $fileStem.cp1 >>$fileStem.cpp
for (( j=0; j<$1; j++ ))
do
    echo "myEmptyFunc$j();">>$fileStem.cpp
done
cat $fileStem.cp2 >>$fileStem.cpp
