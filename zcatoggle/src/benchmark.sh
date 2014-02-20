#!/bin/bash

# Annotate the source
# Generate notify annotations

function annotate {

  COUNTER=0
  NUM_INTRINSICS=$1
  echo -e "int i=0;\n" > tmp.txt
  while [ $COUNTER -lt $NUM_INTRINSICS]; do
    echo -e "__notify_intrinsic((void*)\"large_probe_count_func_${COUNTER}\",(void*)&i);\n" >> tmp.txt
    let COUNTER=$COUNTER+1
    echo -e "i=${COUNTER};" >> tmp.txt
  done
  
  sed '/\$\?\*\!\$/ r tmp.txt' test.cpp > bench.cpp

}

echo -e "---------- Generating Binaries ----------\n"

make clean

echo -e " << Generating Single Probe Execution Test >>"
cp test.cpp bench.cpp
make test_prof_on_single_probe CFLAGS=-D=SINGLE_PROBE=1

echo -e " << Generating Probe Loop Execution Test >>"
make test_prof_on_probe_loop CFLAGS=-DPROBE_LOOP=1 
make test_prof_off_probe_loop CFLAGS=-DPROBE_LOOP=1 

echo -e "\n---------- Running Profiler Benchmarks ---------\n"

echo -e "[Single Probe Execution Test]"
./app_prof_on_single_probe.exe
echo -e "\n"

echo -e "[In Loop Probe Execution Test]"
./app_prof_on_probe_loop.exe
./app_prof_off_probe_loop.exe
echo -e "\n"

rm app_*.exe

# echo -e "int i=0;\n" > tmp.txt
# export COUNTER=0
# while [ $COUNTER -lt 100 ]; do
#   echo -e "__notify_intrinsic((void*)\"large_probe_count_func_${COUNTER}\",(void*)&i);\n" >> tmp.txt
#   let COUNTER=$COUNTER+1
#   echo -e "i=${COUNTER};" >> tmp.txt
# done
#
# # Annotate the source 
# sed '/\$\?\*\!\$/ r tmp.txt' test.cpp > bench.cpp
#
# # Build the binaries
# make clean
# make test_prof_on 
# make test_prof_off 
#
# echo "================== Results ================="
#
# # Run the tests
# ./app_prof_off.exe # > Results.txt
# ./app_prof_on.exe # >> Results.txt





