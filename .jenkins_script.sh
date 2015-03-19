#!/bin/bash

set -x
set -e

if [ $HOSTNAME = swarm ]; then 
 PATH=$PATH:/opt/modules/Modules/3.2.10/bin
fi 

module add intel 

make clean
make all

# Running the full benchmark takes too long for our regression testing build:
# make bench
