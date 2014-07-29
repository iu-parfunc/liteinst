#!/bin/bash

set -x
set -e

make clean
make all

# Running the full benchmark takes too long for our regression testing build:
# make bench
