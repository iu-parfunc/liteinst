#!/bin/bash 
set -xe

REPORT=report_new_`hostname`.html
SECS=120
LOG=/tmp/invocation_regress_log.txt

time criterion-external ./invocation_cost.exe -- -o $REPORT --regress=cycles:iters --regress=cpuTime:iters -L$SECS &> $LOG

tail -n 50 $LOG 

tail -n 60 $LOG > report_new_`hostname`.txt

