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

echo "typedef void (*fn_ptr)();">>$file
echo "extern fn_ptr* g_funcs;">>$file
echo "void init();">>$file

for (( j=0; j<$1; j++ ))
do
  echo "extern \"C\" void func$j( void ) __attribute__((noinline));">>$file
done

file=$fileStem.cpp
echo -e "Generating $file.."
touch $file

echo -e "#include <stdlib.h>\n">>$file
echo -e "#include \"funcs.hpp\"">>$file
echo -e "fn_ptr* g_funcs = (fn_ptr*) malloc(sizeof(fn_ptr) * $1);\n">>$file

for (( j=0; j<$1; j++ ))
do
 echo "void func$j( void ) {">>$file
 echo "  return; }" >> $file
done

echo -e "void init() {">>$file
for (( j=0; j<$1; j++ ))
do
  echo -e "g_funcs[$j] = func$j;">>$file
done
echo -e "}">>$file
