
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





