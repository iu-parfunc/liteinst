#!/bin/bash

set -x
set -e

# Designed to run on SOIC RHEL machines:
module add gcc/5.1.0

make clean
make all

# Running the full benchmark takes too long for our regression testing build:
# make bench
