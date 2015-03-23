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

# check where we are and add module 
#if [ $(HOSTNAME) != swarm ]; then
module add intel 
#fi
 


cd $rootdir
echo "Switched to working-copy directory: "`pwd`

git submodule update --init --recursive 

# ----------------------------------------
# Step (0): Hack-o-rama [2014.08.02]:
echo "Responding to USE_FIXED_LIBS_HACK=$USE_FIXED_LIBS_HACK "
if ! [ "$USE_FIXED_LIBS_HACK" == "" ]; then 
  set -x
  cd ./deps/
  wget http://html.soic.indiana.edu/~parfunc/misc/libelf_0_152_rhel65.tgz
  tar xzvf libelf_0_152_rhel65.tgz
  export LD_LIBRARY_PATH=`pwd`:$LD_LIBRARY_PATH
  export CPATH=`pwd`:$CPATH
  cd $rootdir/
fi
# ----------------------------------------

# if [ $HOSTNAME = swarm ]; then 
# echo "WE ARE ON SWARM"
# source /etc/profile.d/modules-local.sh
# fi 

# module add intel 


echo $LD_LIBRARY_PATH 
echo $PATH

# -- MORE HACKS AND THESE ARE VERY UGLY!
# cd benchmarks/nbody/ubiprof
# make gen-data
# cd $rootdir

# cd benchmarks/hull/ubiprof
# make gen-data
# cd $rootdir

# cd benchmarks/blackscholes/src
# make gen-data
# cd $rootdir

# (1) Piggy-back on teh standard regression tests to do the build:
./.jenkins_script.sh

# (2) Then benchmark:
export DYN_OUTPUT_TYPE=CSV
make bench
