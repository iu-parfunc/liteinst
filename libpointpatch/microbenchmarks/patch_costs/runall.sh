#! /bin/bash

success=0;
failed=0;

fails="";

# CPU on which the benchmarks are to run
CPU=0;

EXES=`find . -name '*.exe' -a ! -name 'patch_cost_parallel.exe'`;

for f in $EXES ; do
  if [ $f  =  '*.exe' ] ;
  then echo "Build benchmarks first"
    exit;
  fi;
  echo ""
  echo ""

  echo '****************************'
  echo 'RUNNING BENCHMARK'
  echo '****************************'

  LD_LIBRARY_PATH=../../build/lib:$LD_LIBRARY_PATH taskset -c $CPU ./${f}
  if [ $? -eq 0 ]; then
    echo '**********************************'
    echo "$f COMPLETED"
    echo '**********************************'
    success=$((success+1))
  else
    echo '***********************************'
    echo "$f FAILED! "
    echo '***********************************'

    fails="$fails $f"
    failed=$((failed+1))
  fi
   
  rm -f functions.txt
done;

echo '****************************'
echo 'RUNNING BENCHMARK'
echo '****************************'

for i in {1..32} ; do
  LD_LIBRARY_PATH=../../build/lib:$LD_LIBRARY_PATH ./patch_cost_parallel.exe $i
  if [ $? -eq 0 ]; then
    echo '**********************************'
    echo "$f COMPLETED"
    echo '**********************************'
    success=$((success+1))

    rm -f functions.txt
  else
    echo '***********************************'
    echo "$f FAILED! "
    echo '***********************************'

    fails="$fails $f"
    failed=$((failed+1))
  fi
done;

echo -e "\n***** Benchmarks summary *****"
echo "Num completed bebchmarks: " $success
echo "Num failed benchmarks: " $failed
echo "Failed benchmarks: " $fails


if [ "$failed" == "0" ];
then
  exit 0;
else
  exit 1;
fi
