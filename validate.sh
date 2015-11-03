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

 elif [ "$BENCH_RUNS" == "PATCHING" ]; then # Patching related application benchmarks
   make lib

   # build run-benchmarks.exe via stack, just to make sure it builds:
   make -f Make_runbench run-full 

 elif [ "$BENCH_RUNS" == "PROBES" ]; then 
   # Application benchmarks with various probe toggle configurations
    # - DISABLE_STRADDLERS
    # - DISABLE_ALL_PROBES
    # - ENABLE_ALL_PROBES
   make lib
   make test

   # build run-benchmarks.exe via stack, just to make sure it builds:
   make -f Make_runbench run-full 
 else # Default application benchmark runs

   # The default is to just run tests
   make lib
   make devdoc
   make test

   # build run-benchmarks.exe via stack, just to make sure it builds:
   make -f Make_runbench benchharnesses 
fi
