#!/bin/bash

set -x
set -e

make clean

# build run-benchmarks.exe via stack:
make -f Make_runbench run-benchmarks.exe

# Build the docker image:
make docker

# Run the tests inside the docker image:
make testdocker
