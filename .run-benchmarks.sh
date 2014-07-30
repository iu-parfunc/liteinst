#!/bin/bash

# Script used by Jenkins to run benchmarks.

export BENCHARGS=$*

./.jenkins_script.sh
make bench
