#!/bin/bash


#pass two parameters hostname then gitdepth
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


fetchall

