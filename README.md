
How To Run
----------

Execute the following sequence of commands from the top level directory.

```
  make build  (Builds the library sources and application benchmarks)
  make run    (Runs application benchmarks and microbenchmarks)
  make plots  (Summarizes data and plot them)
```

Raw data from make run will be stored under results directory organized by
each plot. Raw data files ends with the file extension *.out* and are stored
with a subdirectory named *'raw'* within each such directory. Summarized data 
are also stored within the same directory. Summarized data are csv files.
Plots are generated inside *'plots'* directory at top level.

How To Build The Docker
-----------------------

Run the following from the top level directory.

```
  docker run -w /home/liteinst -it liteinst
```

To run the docker image run the following from the top level directory. The 
shell will be placed under directory '/home/liteinst' at startup.

```
  docker build -t liteinst .
```

Source Organiztion
------------------

 * libcallpatch  - wait free call instruction patching implementation
 * libpointpatch - wordpatch library sources
 * libliteinst   - liteinst protocol implementation
 * utils         - common sources used between above three libraries
 * libliteprof   - minimal profiler based liteinst
 * deps          - third party dependencies used by above libraries
 * include       - public interface of the libraries
 * microbenchmarks - general microbenchmarks on probe operations
 * apps          - full application benchmarks for liteprof
 * scripts       - various benchmark scripts
