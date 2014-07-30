#!/bin/bash

# Script used by Jenkins to run benchmarks.

root=`dirname $0`

export BENCHARGS=$*

cd $root
./.jenkins_script.sh
make bench
