#!/bin/bash 
set -xe

criterion-external ./invocation_cost.exe -- -o report_new.html --regress=cycles:iters --regress=cpuTime:iters -L25
