#!/bin/bash

BENCHES=$*

# Use recent data
export MINGITDEPTH=120

# Generate plots
./fetch_data.sh $BENCHES
./do_plots.sh $BENCHES
