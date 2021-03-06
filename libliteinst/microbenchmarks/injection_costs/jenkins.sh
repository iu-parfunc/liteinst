#!/bin/bash

# module add jdk
# Cutter hack:

export PATH=/opt/jdk/1.8.0_60/bin/:$PATH


source /etc/profile.d/modules-local.sh
module load gcc

export CC=gcc
#alias cc=gcc

# we want a core file in case of core dump 
ulimit -c unlimited 

echo "COMPILER VERSIONS"
echo $(gcc --version)
echo $(g++ --version) 
echo $(cc --version) 

here=`pwd`


# this makes lib I guess.. but Make_runbench does that as well. 
#  cd ../../../
## make
## source env_vars

cd $here
make all -f Make_runbench
# (2) Then benchmark:
export DYN_OUTPUT_TYPE=CSV
make -f Make_runbench run
