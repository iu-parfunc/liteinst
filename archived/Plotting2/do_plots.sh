#!/bin/bash

# An experimental plot script that uses hsbencher-graph.

HSBG=hsbencher-graph

BENCHES=$*

# dir=./gitdepth123-125/
idir=./downloaded/
odir=./build/
findir=./final_plots/

mkdir -p $idir $odir $findir

set -ex

XLABEL=""
YLABEL=""

function go() {
    bench=$1
    shift
    $HSBG --GPL $idir/${bench}.csv -o $odir/${bench}_plot.csv \
          --cleaned=$odir/${bench}_cleaned.csv \
	  --xlabel="$XLABEL" --ylabel="$YLABEL" \
	  --key=PROGNAME --key=VARIANT --lines -x ARGS -y MEDIANTIME \
	  --summary --latest=GIT_DEPTH $*
}

# Delete all previous output.  Lets not confuse things:
# rm -f $odir/${bench}*

for bench in $BENCHES; do
    case $bench in

      # ------------------------------------------------------------
#       n-body)
#         XLABEL="Number of bodies"
# 	YLABEL="Median time in seconds"
# 	go $bench --renames=./nbody_renames.txt --template=./nbody_template.gpl\
# 	   --filtEq=PROGNAME,n-body\
#            --filtEq=VARIANT,cuda,multi_one_device_fissed,multi_one_device_unfissed,multi_two_device_fissed,multi_two_device_unfissed
# #	   --filtEq=VARIANT,java,binary,compact,compact-striped
# 	cp -f $odir/${bench}_plot.pdf $findir/${bench}.pdf
# 	;;

      # ------------------------------------------------------------      
      *)
	echo "Error: bench not handled: $bench"
	exit 1
	;;
  esac
done

# ================================================================================

# go gentree-mem-with-baseline
