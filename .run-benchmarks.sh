#!/bin/bash

# Script used by Jenkins to run benchmarks.

echo "Run benchmarks script starting, located at: $0"

rootdir=$1
shift
export BENCHARGS=$*
set -e

if [ "$rootdir" == "" ] || ! [ -d "$rootdir" ]; 
then echo ".run-benchmarks, cannot proceed because rootdir ($rootdir) does not exist."
     exit 1 
fi 

cd $rootdir
echo "Switched to working-copy directory: "`pwd`


# ----------------------------------------
# Step (0): Hack-o-rama [2014.08.02]:
if ! [ "$USE_FIXED_LIBS_HACK" == "" ]; then 
  cd ./deps/
  wget http://html.soic.indiana.edu/~parfunc/misc/libelf_0_152_rhel65.tgz
  tar xzvf libelf_0_152_rhel65.tgz
  export LD_LIBRARY_PATH=`pwd`:$LD_LIBRARY_PATH
  export CPATH=`pwd`:$CPATH
  cd $rootdir/
fi
# ----------------------------------------

# (1) Piggy-back on teh standard regression tests to do the build:
./.jenkins_script.sh

# (2) Then benchmark:
make bench
