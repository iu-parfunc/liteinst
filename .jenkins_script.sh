#!/bin/bash

set -x
set -e

if [ $HOSTNAME = swarm ]; then 
 PATH=/opt/intel/parallel_studio_xe/composer_xe_2015.1.133/bin/intel64:/opt/intel/parallel_studio_xe/composer_xe_2015.1.133/bin/intel64_mic:/opt/intel/parallel_studio_xe/composer_xe_2015.1.133/debugger/gui/intel64:$PATH
   
 LD_LIBRARY_PATH=/opt/intel/parallel_studio_xe/composer_xe_2015.1.133/tbb/lib/intel64:/opt/intel/parallel_studio_xe/composer_xe_2015.1.133/mkl/lib/intel64:/opt/intel/parallel_studio_xe/composer_xe_2015.1.133/compiler/lib/intel64:/opt/intel/parallel_studio_xe/composer_xe_2015.1.133/ipp/lib/intel64:$LD_LIBRARY_PATH
else 
 module add intel 
fi 



make clean
make all

# Running the full benchmark takes too long for our regression testing build:
# make bench
