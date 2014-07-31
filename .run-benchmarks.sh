#!/bin/bash

# Script used by Jenkins to run benchmarks.

echo "Run benchmarks script starting, located at: $0"
root=`dirname $0`

export BENCHARGS=$*

cd $root

echo "Switched to directory: "`pwd`
./.jenkins_script.sh
make bench
