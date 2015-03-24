#!/bin/bash 


#build ubiprof with overhead time series output capability 
make lib CFLAGS='-DOVERHEAD_TIME_SERIES' 



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
    mv $BENCHROOT/$BENCH/instrumented/overhead.out $DATADIR/$HOSTNAME-$BENCH-$ROUND.out
    mv $BENCHROOT/$BENCH/instrumented/prof.out $DATADIR/prof-$HOSTNAME-$BENCH-$ROUND.out
}

run_it_par { 
    BENCH=$1
    EXECDIR=$2
    ROUND=$3
    for retry in 1 2 3 4 5; do 
	(cd $BENCHROOT/$BENCH/ubiprof; make run)
	if [ $? = 0 ] ; then break 
	else 
	    (cd $BENCHROOT/$BENCH/$EXECDIR; rm overhead.out) 
	fi 
    done 
    mv $BENCHROOT/$BENCH/$EXECDIR/overhead.out $DATADIR/$HOSTNAME-$BENCH-$ROUND.out
    mv $BENCHROOT/$BENCH/$EXECDIR/prof.out $DATADIR/prof-$HOSTNAME-$BENCH-$ROUND.out
}


#ubiprof parameters 
export PROFILER_TYPE=SAMPLING 
export TARGET_OVERHEAD=5 


for benchmark in h264ref-9.3 hmmer lbm perl-5.8.7 sjeng bzip-1.03; do 
    echo "processing bench:"$benchmark
  
    build_it $benchmark ;
     
    for round in 1 2 3 ; do 
	
	run_it $benchmark $round 
	
    done 
done 

#Those parallel benchmarks need special attention! 

#blackscholes 
build_it blackscholes ; 
 for round in 1 2 3 ; do 
	run_it blackscholes src $round 	
 done 

#fluid 
build_it fluid ; 
 for round in 1 2 3 ; do 
	run_it fluid src $round 	
 done 

#hull 
build_it hull ; 
 for round in 1 2 3 ; do 
	run_it hull quickHull $round 	
 done 

#nbody 
build_it nbody ; 
 for round in 1 2 3 ; do 
	run_it nbody BarnesHut $round 	
 done 


