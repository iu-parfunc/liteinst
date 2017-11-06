
How To Run
----------

Execute the following sequence of commands from the top level directory.

```
  make build  (Builds the library sources and application benchmarks)
  make run    (Runs application benchmarks and microbenchmarks)
  make plots  (Summarizes data and plot them)
```

Raw data from `make run` will be stored under results directory organized by
each plot. Raw data files ends with the file extension *.out* and are stored
with a subdirectory named *'raw'* within each such directory. Summarized data 
are also stored within the same directory. Summarized data are csv files.
Plots are generated inside *'plots'* directory at top level.

How To Build The Docker 
-----------------------

Run the following from the top level directory.

```
  docker build -t liteinst .
```

To run the docker image run the following from the top level directory. The 
shell will be placed under directory '/home/liteinst' at startup.

```
  docker run -w /home/liteinst -it liteinst
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

API Usage
---------

1. First you need to Initialize the probe provider. Currently we only provide
instruction punning based liteprobes probe provider.

```
ProbeProvider p = liteinst::ProbeProvider::initializeGlobalProbeProvider(
            ProviderType::LITEPROBES, probeDiscoveryCallback, initCallback);
```

The `probeDiscoveryCallback` will be called once for each discovered probe at the
beginning. It accepts a `ProbeInfo` which contains the probe information. 

The `initCallback` will be called right after probe provider initialization. It may be 
used to setup statistics gathering meta data and registering probes with the 
probe provider.

Both `probeDiscoveryCallback` and `initCallbck` are optional.

2. Then we need to setup an instrumentation provider. This will provide the instrumentation
functions the attached probes invoke. There can be multiple instrumentation providers.

```
InstrumentationProvider i_provider("IProvider", entryInstrumentation, 
              exitInstrumentation);
p->registerInstrumentationProvider(i_provider);
```

The `entryInstrumentation` will be invoked at before the probed location is 
executed. The `exitInstrumentation` will be invoked after.

3. Next we need to specify where we want to probe. Probes are described using coordinates.
Currently supported coordinates include functions only. In future we plan 
to support modules, basic blocks, loops and instruction addresses.

```
Coordinates coords;
coords.setFunction(liteinst::Function(".*~_ZnwmPv"));
coords.setProbePlacement(ProbePlacement::BOUNDARY);
```

Function names accepts wildcards. Exclusions come after the ~. For example
above instruments all functions except the `new` function. `_ZnwmPv` is the mangled name 
of the `new` function. ProbePlacement specifies whether if we need to instrument 
at function entry, exit or both. In this case ProbePlacement::BOUNDARY specifies that we
want both.

4. Finally we have to register the probes with the probe provider.

```
ProbeRegistration pr = p->registerProbes(coords, "IProvider"); 
```

Registration function accepts the probe coordinates and the instrumentation provider
which will be associated with these registered probes.

Returned `ProbeRegistration` will contain information about registered probes.

Example API usage can be found at libliteprof/src/profiler.cpp.


