#!/bin/bash

# Writes a CSV file per benchmark
# =================================================================
#
#  Takes command line arguments for which benchmark names (prefixes
#  actually) to download.
# 
#  Also responds to optional environment variables:
#
#   * MINGITDEPTH - set a lower bound
#
# =================================================================

TABLE=
TABLEID=1DJJM9SI_N8En4-M6mSB67tSerL_laFJ3Dw1evNMW

# Git depth lower bound for fetched data:
if [ "$MINGITDEPTH" == "" ]; then
#  MINGITDEPTH=140
  MINGITDEPTH=79
fi


host=

HSBFF=hsbencher-fusion-fetch

DEST=./downloaded

CID=905767673358.apps.googleusercontent.com
SEC=2a2H57dBggubW1_rqglC7jtK


BENCHES=$*
if [ "$BENCHES" == "" ]; then
    echo "Error: no benchmarks selected to fetch.  $0 needs one or more command arguments."
    exit 1
fi

# [2015.01.29] Not sure if fusion tables supports enough SQL to grab the latest row
# only within each group defined by group-by:
# 
#   http://stackoverflow.com/questions/3800551/select-first-row-in-each-group-by-group

# MAX(GIT_DEPTH) as latest_git_depth
#     GROUP BY PROGNAME,VARIANT,ARGS \

function go() {
    bench=$1

    # Version 1: Do the aggregation/deduplication on the server side:
    # # TEMP: Unfortunately we don't have MEDIAN as an aggregate:    
    # QUERY="SELECT PROGNAME,VARIANT,ARGS,THREADS, \
    #           AVERAGE(MEDIANTIME) as MEDIANTIME, \
    #           MAXIMUM(GIT_DEPTH) as GIT_DEPTH \
    # 	   FROM FT WHERE \
    # 	     GIT_DEPTH >= $MINGITDEPTH and \
    # 	     PROGNAME LIKE '%$bench%' and \
    # 	     HOSTNAME LIKE '%$host%' \
    #        GROUP BY PROGNAME,VARIANT,ARGS,THREADS \
    #        "
    #    # 	     PROGNAME='$bench' \
    
    # Version 2: allow duplicate data and filter it out later:
    QUERY="SELECT PROGNAME,VARIANT,ARGS,THREADS,MINTIME,MEDIANTIME,MAXTIME,GIT_DEPTH \
	   FROM FT WHERE \
             MEDIANTIME > 0 and \
             PROGNAME LIKE '$1%' \
           "
    # Note, I temporarily turned these off to fetch data that was mis-uploaded:
    QUERY+="and HOSTNAME LIKE '%$host%'"
    QUERY+="and GIT_DEPTH >= $MINGITDEPTH"
    
    $HSBFF --id=$CID --secret=$SEC --table=$TABLE --query="$QUERY" --raw > $DEST/$bench.csv
    echo "Successfully wrote file: $DEST/$bench.csv"
}

set -xe
mkdir -p $DEST

for bench in $BENCHES; do
    go $bench
done 

