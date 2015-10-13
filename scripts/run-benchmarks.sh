#!/bin/bash

# Script used by Jenkins to run benchmarks.
# This expects to be run FROM the top of the repo, with ./scripts/run-benchmarks.sh

echo "Run benchmarks script starting, located at: $0"

rootdir=$1
shift
LIBCOMPILER=$1
shift
export BENCHARGS=$*
set -e

if [ "$rootdir" == "" ] || ! [ -d "$rootdir" ];
then echo "run-benchmarks, cannot proceed because rootdir ($rootdir) does not exist."
     exit 1
fi

if [ "$LIBCOMPILER" == "" ];
then echo "run-benchmarks, cannot proceed because library compiler ($LIBCOMPILER) not set."
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

#./.jenkins_script.sh
case $LIBCOMPILER in
    icc)
	# after hacks.. add intel libs
	module add intel

	make clean
	make lib CC=icc CXX=icpc
	;;
    gcc)
	module add gcc/4.9.2

	make clean
	make lib CC=gcc CXX=g++
	;;
    *)
	echo "No suitable compiler for the library chosen: ABORTING!"
        exit 1
esac

# Compiling Ubiprof is done.

# separate out the benchmarking concerns.
make all -f Make_runbench


# (2) Then benchmark:
export DYN_OUTPUT_TYPE=CSV

make -f Make_runbench run-full
