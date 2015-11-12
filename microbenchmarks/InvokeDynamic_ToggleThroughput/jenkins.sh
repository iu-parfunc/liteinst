#!/bin/bash

module add jdk

make all -f Make_runbench
# (2) Then benchmark:
export DYN_OUTPUT_TYPE=CSV
make -f Make_runbench run
