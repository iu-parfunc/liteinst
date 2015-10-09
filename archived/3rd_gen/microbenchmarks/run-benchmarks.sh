#!/bin/bash

# Script used by Jenkins to run benchmarks.

root=`dirname $0`

export BENCHARGS=$*

cd $root
make bench
