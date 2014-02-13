#!/bin/bash

# Build the binaries
make clean
make test_prof_on 
make test_prof_off 

echo "================== Results ================="

# Run the tests
./app_prof_off.exe # > Results.txt
./app_prof_on.exe # >> Results.txt





