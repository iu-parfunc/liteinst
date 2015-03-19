#!/bin/bash

set -x
set -e

module add intel 

make clean
make all

# Running the full benchmark takes too long for our regression testing build:
# make bench
