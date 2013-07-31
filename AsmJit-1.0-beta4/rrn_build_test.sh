#!/bin/bash
set -e

# g++ -v -I. AsmJit/*.cpp Test/testjit.cpp -o testjit.exe

g++ -v -I. AsmJit/*.cpp rrn_test.cpp -o rrn_test.exe

# g++ -v -I. AsmJit/*.cpp
# g++ -v *.o rrn_test.cpp -o rrn_test.exe


