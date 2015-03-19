#!/bin/bash

set -x
set -e

if [ $HOSTNAME = swarm ]; then 
 PATH=$PATH:/opt/modules/Modules/3.2.10/bin
 modulecmd bash add intel 
else 
 module add intel 
fi 



make clean
make all

# Running the full benchmark takes too long for our regression testing build:
# make bench
