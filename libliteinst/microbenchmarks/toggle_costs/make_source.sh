#!/bin/bash
set -e

clean_sources() {
  rm -f "funcs.cpp"
  rm -f "funcs.hpp"
  rm -f "toggle_cost.cpp"
}

if [ $1 == "--clean" ]
then
  clean_sources
  exit 0
fi 

# Environment variable
fileStem=funcs

# Infrastructure work... remove old files, output info
rm -f $fileStem.hpp
rm -f $fileStem.cpp

file=$fileStem.hpp
echo "Generating $file.."
touch $file
for (( j=0; j<$1; j++ ))
do
  echo "void emptyFunc$j( void ) __attribute__((noinline));">>$file
done

file=$fileStem.cpp
echo "Generating $file.."
touch $file
for (( j=0; j<$1; j++ ))
do
  echo "void emptyFunc$j( void ) {">>$file
  echo "  return; }" >> $file
done

fileStem=toggle_cost
rm -f $fileStem.cpp
echo "Generating $fileStem.cpp.."
# Put together the source code
cat $fileStem.cp1 >>$fileStem.cpp
for (( j=0; j<$1; j++ ))
do
  echo "  emptyFunc$j();">>$fileStem.cpp
done
cat $fileStem.cp2 >>$fileStem.cpp
