
# PERL WONT COMPILE 
#perl-5.8.7
for bench in hmmer lbm  sjeng bzip-1.0.3 fluid blackscholes hull nbody ; do 
WHICHBENCH=$bench/unprofiled JENKINS_GHC=7.8.3 ./.run-benchmarks.sh $(pwd) --hostname=$HOSTNAME $WHICHBENCH
WHICHBENCH=$bench/ubiprof JENKINS_GHC=7.8.3 ./.run-benchmarks.sh $(pwd) --hostname=$HOSTNAME $WHICHBENCH
done
