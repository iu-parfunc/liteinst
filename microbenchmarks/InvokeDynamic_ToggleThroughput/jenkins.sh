#!/bin/bash

# module add jdk
# Cutter hack:

export PATH=/opt/jdk/1.8.0_60/bin/:$PATH

make all -f Make_runbench
# (2) Then benchmark:
export DYN_OUTPUT_TYPE=CSV
make -f Make_runbench run
