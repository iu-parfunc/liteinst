#!/bin/bash

# Script used by Jenkins to run benchmarks.
# This expects to be run FROM the top of the repo, with ./scripts/run-benchmarks.sh

echo "Run benchmarks script starting, located at: $0"

rootdir=$1
shift
export BENCHARGS=$*
set -e

if [ "$rootdir" == "" ] || ! [ -d "$rootdir" ];
then echo "run-benchmarks, cannot proceed because rootdir ($rootdir) does not exist."
     exit 1
fi

cd $rootdir
echo "Switched to working-copy directory: "`pwd`

set -x
which -a git
git --version
git submodule update --init --recursive
set +x


echo "*** CHECK IF THESE PATHS ARE SANE ***"
echo "***"
echo "CPATH:"
echo $CPATH
echo "***"
echo "LD_LIBRARY_PATH"
echo $LD_LIBRARY_PATH
echo "***"
echo "LIBRARY_PATH"
echo $LIBRARY_PATH
echo "***"
echo "PATH"
echo $PATH

# (1) Piggy-back on the standard regression tests to do the build:
# This should get the rest of the params, right ???

# Make sure the module bash function is available:
source /etc/profile.d/modules-local.sh

#Fixes for CUTTER
module add gcc

export CC=gcc

# MAke the Library 
make clean
make lib CC=gcc CXX=g++


# Compiling Ubiprof is done.

# separate out the benchmarking concerns.
make all -f Make_runbench


# (2) Then benchmark:
export DYN_OUTPUT_TYPE=CSV

make -f Make_runbench run-full
