### Aug 6 2015 ###

    Docker added:

    Usage:

    docker build -t iu-parfunc/ubiprof .

    docker run -it iu-parfunc/ubiprof bash


### Aug 3 2015 ###
   * New makefile setup.
     make lib : makes the library with default GXX=g++ setting
     make lib GXX=icpc : uses intel compiler

   * Makefile for library is now standalone from makefile for
     benchmarking.
     "make -f Make_runbench" does the benchmarking.
     The .run_benchmarks2.sh script has been updated to reflect this.

     TODO: the .run_benchmarks files need an overhaul, cleaning and polish.
           Much of it should probably go to archive




### Jul 30 2015 ###

  * The Makefile_GCC has two modes:
     make lib
     make libdebug
    These two modes of operation are not present in the ICC makefile.



### 6-29-13 Nikhil ###

#### Self-reading binary ####
Working on getting the program to its binary through fopen(). Was unable to open in r+ (read/write). Also attempted to use ios:out with fstream, but that did not work eiter. Going to copy contents to buffer, modify buffer, and write to a new binary until I can
figure out a solution.

Keep in mind: the location of the probe-ready site will change if code is inserted in front of
it, so all the instrumentation code must come afterwards.

#### ToDo ####
1. Get r+ working.
2. Determine why the location of the probesite is far higher than the total size of the binary.

------------------

### 6-5-13 ####

#### DWARF encoding ####
Address of tag argument is DWARF encoded. Looking into libdwarf to read this data.

#### Pintool ####
Previously created a pintool utilizing libzca to insert function calls into test program.
Working on fixing "undefined reference" compiler error.



[2013.07.31] {Ryan trying various asm/jit libraries}
----------------------------------------------------

I'm experimenting with libffi, asmjit, jitasm, LuaJit/DynAsm, and Xed.
Here are the rough outcomes from each:

  * libffi - has interpretive overhead on every call, doesn't really
    JIT a stub.

  * asmJit - beta release only; cmake build didn't work but very easy
             to directly embed sources.  The directly-supported
             behavior is to generate complete functions, not
             fragments.

  * jitAsm - a single header.

  * Xed - encodes ONE instruction at a time.  Nice.  Unfortunotely,
    closed source.


Other systems seem to have too much baggage or are overkill:

  * Pin - specific instrumentation methodology, don't see how
    we can use for precise pinpoint emission of x86 instructions.
  * Dynamo/RIO ? Seem to be in the same bucket as Pin.  Intercepts all
    basic blocks in the application.
  * DynInst - built to connect to a separate process.  How do you
    transparently self-instrument?

  * LLVM - how do you use it for direct emission of machine code in a
    specific spot?  I suppose constructing the LLVM IR would be ok for
    the function call stub, but in other places we need specific
    control---inserting a SINGLE direct jump at the probe-ready site.

[2014.02.04] {Back to debugging}
----------------------------------------

When I last touched in in the Fall, I was running into problems with
not finding the magic number in the right place in the ZCA table.  Our
elf header extraction code would work sometimes and sometimes fail,
and I hadn't gotten to the bottom of it.


[2015.11.11] {Notes on getting competitors working}
---------------------------------------------------

Earlier, I had tried:

 * LTTNG
 * perf probes
 * System Tap

And had install problems or kernel version problems with all of them.
DTrace is definitely the easiest to get working


[2016.08.12] {Micro-benchmarking the new libliteinst}
-----------------------------------------------------

I'm using `criterion-external` to run microbenhmarks (RRN), starting
with invocation cost.   

    time                 6.303 μs   (6.058 μs .. 6.752 μs)
                         0.961 R²   (0.947 R² .. 0.972 R²)
    mean                 1.538 ms   (1.079 ms .. 2.423 ms)
    std dev              4.332 ms   (2.721 ms .. 6.945 ms)
    cycles:              0.961 R²   (0.948 R² .. 0.972 R²)
      iters              16386.094  (15735.024 .. 17530.907)
      y                  2.296e8    (2.233e8 .. 2.355e8)
    cpuTime:             0.629 R²   (0.580 R² .. 0.687 R²)
      iters              5.902e-9   (5.157e-9 .. 7.152e-9)
      y                  4.519e-4   (4.426e-4 .. 4.605e-4)
    variance introduced by outliers: 99% (severely inflated)

Ah, that's actually the cost of an empty function call!  The actual
instrumentation overhead is:

     Finally, here is some human-readable output, not for HSBencher:
     Number of invocations : 5629000
     Cost per invocation (cycles) : 64

     Finally, here is some human-readable output, not for HSBencher:
     Number of invocations : 5629000
     Cost per invocation (cycles) : 43
     Inside cleanup
     time                 47.71 μs   (41.93 μs .. 53.64 μs)
                          0.749 R²   (0.628 R² .. 0.836 R²)
     mean                 3.838 ms   (2.655 ms .. 5.879 ms)
     std dev              8.818 ms   (5.639 ms .. 14.13 ms)
     cycles:              0.749 R²   (0.631 R² .. 0.823 R²)
       iters              124033.259 (107913.113 .. 138935.968)
       y                  3.778e8    (3.601e8 .. 3.974e8)
     cpuTime:             0.415 R²   (0.253 R² .. 0.556 R²)
       iters              6.087e-8   (4.895e-8 .. 7.139e-8)
       y                  6.018e-4   (5.841e-4 .. 6.221e-4)


Then divide those regression values by 2000 for the per-invocation
cost.  I've pumped it up by 1000, and it runs twice.

