
# PERL WONT COMPILE 
#perl-5.8.7

export MACHINECLASS=$HOSTNAME

# fluid blackscholes hull nbody

for bench in hmmer lbm  sjeng bzip-1.0.3  ; do 
WHICHBENCH=$bench/unprofiled JENKINS_GHC=7.8.3 ./.run-benchmarks.sh $(pwd)  $WHICHBENCH
WHICHBENCH=$bench/ubiprof JENKINS_GHC=7.8.3 ./.run-benchmarks.sh $(pwd)  $WHICHBENCH
done
