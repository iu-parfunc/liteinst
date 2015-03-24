#!/bin/bash 


#build ubiprof with overhead time series output capability 
# make lib CFLAGS='-DOVERHEAD_TIME_SERIES' 



BENCHROOT=benchmarks 
DATADIR=overhead_series

if [ ! -d $DATADIR ] ; then 
    mkdir $DATADIR
fi 


function build_it { 
    BENCH=$1
    echo "BUILDING:"$BENCH

    (cd $BENCHROOT/$BENCH/ubiprof; make build)        
}

function run_it { 
    BENCH=$1
    ROUND=$2 

    (cd $BENCHROOT/$BENCH/ubiprof; make run) 
    # will not work for the parallel benches!! 
    # that use a different directory structure (My bad idea) 
    cp $BENCHROOT/$BENCH/instrumented/overhead.out $DATADIR/$BENCH$ROUND.out
}



#ubiprof parameters 
export PROFILER_TYPE=SAMPLING 
export TARGET_OVERHEAD=5 


for benchmark in h264ref-9.2 hmmer lbm perl-5.8.7 sjeng bzip-1.03; do 
    echo "processing bench:"$benchmark
  
    build_it $benchmark ;
     
    for round in 1 2 3 ; do 
	
	run_it $benchmark $round 
	
    done 
    
    

done 


