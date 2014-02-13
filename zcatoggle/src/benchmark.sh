#!/bin/bash

# Annotate the source
# Generate notify annotations
echo -e "int i=0;\n" > tmp.txt
export COUNTER=0
while [ $COUNTER -lt 1 ]; do
  echo -e "__notify_intrinsic((void*)\"large_probe_count_func_${COUNTER}\",(void*)&i);\n" >> tmp.txt
  let COUNTER=$COUNTER+1
  echo -e "i=${COUNTER};" >> tmp.txt
done

# Annotate the source 
sed '/\$\?\*\!\$/ r tmp.txt' test.cpp > bench.cpp

# Build the binaries
make clean
make test_prof_on 
make test_prof_off 

echo "================== Results ================="

# Run the tests
./app_prof_off.exe # > Results.txt
./app_prof_on.exe # >> Results.txt





