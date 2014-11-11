#!/bin/bash

# Git Depth, etc.
GIT_DEPTH=$2
HOSTNAME=$1

# General FusionTable Configuration
SECRET=MQ72ZWDde_1e1ihI5YE9YlEi
ID=925399326325-6dir7re3ik7686p6v3kkfkf1kj0ec7ck.apps.googleusercontent.com
TABLE=Dynaprof_Benchmarks2


Q="./hsbencher do --secret=$SECRET --id=$ID --table=$TABLE --raw"

function fetchall 
{ 
    echo Fetching data

    echo Fetching unprofiled mediantimes 

    $Q --query="select HOSTNAME,PROGNAME,count(),AVERAGE(MEDIANTIME) as runtime
            from FT
            where GIT_DEPTH=$GIT_DEPTH
              and VARIANT='unprofiled' 
              and HOSTNAME like '$HOSTNAME'
            group by HOSTNAME, PROGNAME
            order by PROGNAME" > unprofiled_$HOSTNAME.csv


    echo Fetching all info to one csv! 
    $Q --query="select PROGNAME, VARIANT,count(),AVERAGE(MEDIANTIME) as runtime
            from FT
            where GIT_DEPTH=$GIT_DEPTH
              and HOSTNAME like '$HOSTNAME'
            group by VARIANT, PROGNAME
            order by PROGNAME" > all_$HOSTNAME.csv
 
}    


# function fetchall
# {
# 	echo Fetching all data.

# 	# We're going to need slightly more than gitdepth to uniquely
# 	# identify a run.
	
# 	# Figure 7 (static vs dynamic)
# 	echo Fetching Figure 7 data...
# 	echo
# 	$Q --query="select HOSTNAME,count(),THREADS,AVERAGE(MEDIANTIME) as runtime
#                 from FT
#                 where GIT_DEPTH=$GIT_DEPTH
#                   and VARIANT='cilk'
#                   and HOSTNAME like 'd0%'
#                   and PROGNAME='accelerate-blackscholes'
#                 group by HOSTNAME, THREADS" > static-vs-dynamic-blackscholes.dat

# 	$Q --query="select HOSTNAME,count(),THREADS,AVERAGE(MEDIANTIME) as runtime
#                 from FT
#                 where GIT_DEPTH=$GIT_DEPTH
#                   and VARIANT='cilk'
#                   and HOSTNAME like 'd0%'
#                   and PROGNAME='accelerate-nbody-float'
#                 group by HOSTNAME, THREADS" > static-vs-dynamic-nbody.dat

# 	echo
# 	echo

# 	# Figure 9 (Black Scholes Backend Configurations)
# 	echo Fetching Figure 9 Data...
# 	echo
# 	$Q --query="select HOSTNAME,count(),ARGS,AVERAGE(MEDIANTIME) as runtime
#                 from FT
#                 where GIT_DEPTH=$GIT_DEPTH
#                   and VARIANT='cilk'
#                   and HOSTNAME like 'd0%'
#                   and PROGNAME='accelerate-blackscholes'
#                 group by HOSTNAME, ARGS" > bs-variant-cilk.dat
# 	$Q --query="select HOSTNAME,count(),ARGS,AVERAGE(MEDIANTIME) as runtime
#                 from FT
#                 where GIT_DEPTH=$GIT_DEPTH
#                   and VARIANT='2gpu'
#                   and HOSTNAME like 'd0%'
#                   and PROGNAME='accelerate-blackscholes'
#                 group by HOSTNAME, ARGS" > bs-variant-2gpu.dat
# 	$Q --query="select HOSTNAME,count(),ARGS,AVERAGE(MEDIANTIME) as runtime
#                 from FT
#                 where GIT_DEPTH=$GIT_DEPTH
#                   and VARIANT='cpugpu'
#                   and HOSTNAME like 'd0%'
#                   and PROGNAME='accelerate-blackscholes'
#                 group by HOSTNAME, ARGS" > bs-variant-cpugpu.dat
# 	$Q --query="select HOSTNAME,count(),ARGS,AVERAGE(MEDIANTIME) as runtime
#                 from FT
#                 where GIT_DEPTH=$GIT_DEPTH
#                   and VARIANT='cuda'
#                   and HOSTNAME like 'd0%'
#                   and PROGNAME='accelerate-blackscholes'
#                 group by HOSTNAME, ARGS" > bs-variant-cuda.dat

# 	# Figure 8 (nbody and nbody++)
# 	echo
# 	echo
# 	echo Fetching Figure 8 Data...
# 	echo
# 	$Q --query="select HOSTNAME,count(),ARGS,AVERAGE(MEDIANTIME) as runtime
#                 from FT
#                 where GIT_DEPTH=$GIT_DEPTH
#                   and VARIANT='cilk'
#                   and HOSTNAME like 'd0%'
#                   and THREADS='12'
#                   and PROGNAME='accelerate-nbody-float'
#                 group by HOSTNAME, ARGS" > nbody-variant-cilk.dat
# 	$Q --query="select HOSTNAME,count(),ARGS,AVERAGE(MEDIANTIME) as runtime
#                 from FT
#                 where GIT_DEPTH=$GIT_DEPTH
#                   and VARIANT='cuda'
#                   and HOSTNAME like 'd0%'
#                   and PROGNAME='accelerate-nbody-float'
#                 group by HOSTNAME, ARGS" > nbody-variant-cuda.dat
# 	$Q --query="select HOSTNAME,count(),ARGS,AVERAGE(MEDIANTIME) as runtime
#                 from FT
#                 where GIT_DEPTH=$GIT_DEPTH
#                   and VARIANT='2gpu'
#                   and HOSTNAME like 'd0%'
#                   and PROGNAME='accelerate-nbody-float'
#                 group by HOSTNAME, ARGS" > nbody-variant-2gpu.dat
# 	$Q --query="select HOSTNAME,count(),ARGS,AVERAGE(MEDIANTIME) as runtime,
#                        AVERAGE(THREADS)
#                 from FT
#                 where GIT_DEPTH=$GIT_DEPTH
#                   and VARIANT='cpugpu'
#                   and HOSTNAME like 'd0%'
#                   and PROGNAME='accelerate-nbody-float'
#                 group by HOSTNAME, ARGS" > nbody-variant-cpugpu.dat
# }




fetchall

