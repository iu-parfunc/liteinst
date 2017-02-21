#! /bin/bash

ROOT=../..
TRIALS=3

function run_bench() { 
  duration=3.0
#  runners=(1 2)
  runners=(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
#  rates=(0 10)
  rates=(0 10 100 1000 10000 100000 500000 1000000 10000000 100000000)
  mkdir -p $ROOT/results/Toggle_Throughput-Fig5
  mkdir -p $ROOT/results/Toggle_Throughput-Fig5/raw
  for runner in "${runners[@]}";
  do
    for rate in "${rates[@]}";
    do
      echo "Runners : "$runner" Rate : "$rate
      export NUM_THREADS=$runner; export FREQ=$rate; make run
    done
  done
  yes | cp -f "toggles.out" $ROOT/results/Toggle_Throughput-Fig5/raw/
}

run_bench
