#!/bin/bash

# Script used by Jenkins to run benchmarks.

echo "Run benchmarks script starting, located at: $0"

rootdir=$1
shift
export BENCHARGS=$*

cd $rootdir

echo "Switched to working-copy directory: "`pwd`
./.jenkins_script.sh
make bench
