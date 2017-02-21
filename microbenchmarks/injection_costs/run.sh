#! /bin/bash

ROOT=../..
TRIALS=3

function run_bench() {
  runners=(2 3 4 5)
  mkdir -p $ROOT/results/Injection_Costs-Fig4
  mkdir -p $ROOT/results/Injection_Costs-Fig4/raw
  cd $1 
  make build
  for r in "${runners[@]}";
  do
    for i in {1..3};
    do
      echo "Runners : "$r" Trial : "$i
      export NUM_RUNNERS=$r; make run
    done
  done
  yes | cp -f $1".out" $ROOT/../results/Injection_Costs-Fig4/raw/
}

# run_bench "liteinst"
run_bench "dyninst"
