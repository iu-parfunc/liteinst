#!/bin/bash

set -x
set -e

make clean

# Build the docker image:
make docker

# TODO: Run the tests inside the docker image:
make testdocker

# build run-benchmarks.exe via stack:
make -f Make_runbench benchharnesses
