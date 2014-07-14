#!/bin/bash

set -x
set -e

TOP=`pwd`

cd $TOP/zcatoggle/

make install
make docs

# cd zcatoggle/src
# make prof

cd $TOP/dynaprof/src
make

