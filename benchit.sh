
# PERL WONT COMPILE 
#perl-5.8.7

export MACHINECLASS=$HOSTNAME

# fluid blackscholes hull nbody
# bzip-1.0.3
# nbody bzip-1.0.3 lbm sjeng fluid blackscholes hull  hmmer
# for bench in nbody bzip-1.0.3 lbm sjeng fluid blackscholes hull hmmer ; do 
for bench in nbody ; do 
WHICHBENCH=$bench/unprofiled JENKINS_GHC=7.8.3 ./.run-benchmarks.sh $(pwd)  $WHICHBENCH
WHICHBENCH=$bench/ubiprof JENKINS_GHC=7.8.3 ./.run-benchmarks.sh $(pwd)  $WHICHBENCH
done
