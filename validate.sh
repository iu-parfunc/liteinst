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

else

   make lib
   make devdoc
   make test

fi
