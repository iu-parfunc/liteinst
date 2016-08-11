#!/bin/bash

# module add jdk
# Cutter hack:

export PATH=/opt/jdk/1.8.0_60/bin/:$PATH


source /etc/profile.d/modules-local.sh
module load gcc
alias cc=gcc

here=`pwd`

cd ../../../
make
source env_vars

cd $here
make all -f Make_runbench
# (2) Then benchmark:
export DYN_OUTPUT_TYPE=CSV
make -f Make_runbench run
