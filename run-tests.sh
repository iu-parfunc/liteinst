#!/usr/bin/env bash

function run_microbenchs(){
make microbench
}

function run_tests(){
make tests
}

function run_bench(){
make bench
}

function run_all(){
run_microbench;
run_tests;
run_bench;
}

if ! options=$(getopt -o mbta -l micro,bench,tests,all: -- "$@")
then
  printf "./run-tests.sh [-m|--micro] [-b|--bench] [-t|--tests] [-a|--all]\n
        -m | --micro Runs microbenchmarks\n
        -b | --bench Runs application benchmarks\n
        -t | --tests Runs unit and integration tests\n
        -a | --all Runs microbenchmarks, tests and application benchmarks\n";
  exit
fi

set -- $options

if [[ $# -eq 1 ]]; then
  run_all;
  exit
fi

while [ $# -gt 0 ]
do
  case $1 in
  -m|--micro) run_microbenchs;;
  -b|--bench) run_bench;;
  -t|--tests) run_tests;; 
  -a|--all)   run_all;;
  (--) shift; break;;
  (-*) echo "$0: error - unrecognized option $1" 1>&2; exit 1;;
  (*) break;;
  esac
  shift
done
