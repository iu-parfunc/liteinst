#!/bin/bash

set -x
set -e

make clean

if [ "$USE_DOCKER" == "1" ]; then

   # Build the docker image:
   make docker
   # Run the tests inside the docker image:
   make testdocker
   # build run-benchmarks.exe via stack:
   make -f Make_runbench benchharnesses

 elif [ "$BENCH_RUNS" == "1" ]; then
   make lib

   # build run-benchmarks.exe via stack, just to make sure it builds:
   make -f Make_runbench benchharnesses
 else

   make lib
   make devdoc
   make test

   # build run-benchmarks.exe via stack, just to make sure it builds:
   make -f Make_runbench benchharnesses
fi
