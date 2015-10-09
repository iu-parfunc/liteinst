2013-10-17
==========
- Added measurement for a function call with a notify intrinsic
  - A basic function call costs 6 cylces
  - If you add two notify intrinsics, one for the enter and one for the leave, it increases 
    the time to 7 cycles.
  - So two notify intrinsics cost about 1 cycle.
- If you add only one notify intrinsic, it also increases the runtime by one cycle
  - So the second notify intrinsic is free (at least if you have them right next to each other)
- Also interesting, if you have an empty function, and include a #pragma noinline
  in the source, the compiler will still remove the empty function call.
  - However, if you have an empty function with a notify intrinsic inside, the compiler will
    actually create a function call
- Meassured time for call into an empty function located in a dynamic or static library
  - For the static library, there is no change, 6 cycles.
  - For the dynamic library, it goes up to 9 cycles.
  - The good thing is though, one can now really call an empty function, it is not
    required that the function returns a value just to trick the compiler into not optimizing it
- Here is the new master table
  - Function call (function in the same source file):      6 cycles
  - Function call (function in a static library):          6 cycles
  - Function call (function in a dynacmi library):         9 cycles
  - The following is all based on a function call into the same source file
  - Function call with two notify intrinsics:              7 cycles    (+1)
  - Function call with two additional function calls:     15 cycles    (+9)
  - Function call with VampirTrace, filtered:            196 cycles  (+190) 
  - Function call with VampirTrace, output to /dev/shm:  891 cycles  (+885) 
  - Function call with VampirTrace, output to Lustre:   1146 cycles (+1140)


2013-09-29
==========
- Things to keep in mind when doing measurements:
  - The compiler creates highly optimized binaries, eliminating all "empty" code parts
    - This makes benchmarking a call to an "empty" function really difficult
    - Always check the assembly output, to verify what code the compiler generated (or did not generate)
  - Dropping the compiler to -O0 will work, but then even the normal code does not get optimized any more
    - Comparing -O0 to -O3 code, shows lots of code gets optimized and removed as the optimization
      level increases. Thus, going down to -O0 will increase the runtime for all parts of the code!
    - Also, no-one builds production code with -O0, so this may not even be a valid test case.
  - Measuring with /usr/bin/time works, clock_gettime() does too
  - From an overhead perspective, a local function call is not equivalent to a function call into a library
  - In addition to the overhead of the actual jump, there is also indirect disturbance due to messing up the cache
- Preliminary overhead numbers:
  - Since the compiler will not translate an empty function call, one has to do something in the function and 
    even that will be eliminated, unless something is return to main, and used later on in the program.
  - Code:
    int myEmptyFunc(int i){return i + 1;}
    int main() {
      unsigned long int i,j;
      for (i=0; i < 400000000; i++) { j = myEmptyFunc(i); }
      return j;
    }
  - Running the above code shows that it takes 6 CPU cycles for one loop iteration
    - This includes incrementing the loop variable, jumping into the function, incrementing i, jumping back
      and the loop jump
  - Adding a similar "myEmptyFunc" call to the begin and end of myEmptyFunc, to simulate the call of a tracing
    function, adds an additional 9 cycles.
    - This includes similar things as above, jumping into the function, doing a silly math operation and returning
      its result when jumping back. (all of this twice, once at the beginning and once at the end of the function.
  - Adding tracing with VampirTrace for myEmptyFunc, but filtering the function, adds an additional 190 cycles
    - This is the best case with VampirTrace, nothing is recorded, control flow immediately returns
      - The only thing done in VampirTrace is a check that the function is to be filtered.
  - Tracing the function properly, and wirting the results out to /dev/shm at the end, adds 885 cycles to a function call
    - This is fully costed, meaning it contains everything needed to use VampirTrace!
  - Tracing the function properly, and writing the results out to Lustre at the end, adds 1140 cycles to a function call
    - Again, fully costed!
  - Findings:
    - At the very minimum, a function call gets blown up by an additional 9 cycles when tracing the entry and exit
      - It does not get any better than this, as this basically does nothing, just transfer control flow back and forth
    - Using VampirTrace, but recording nothing, blows up a function call by 190 cycles.
    - Using VampirTrace, and writing results to a memory file system blows up a function call by 885 cycles
    - Using VampirTrace, and writing results to a network file system blows up a function call by 1140 cycles
  - Summary
    - Function call:                                         6 cycles
    - Function call with two additional function calls:     15 cycles    (+9)
    - Function call with VampirTrace, filtered:            196 cycles  (+190) 
    - Function call with VampirTrace, output to /dev/shm:  891 cycles  (+885) 
    - Function call with VampirTrace, output to Lustre:   1146 cycles (+1140)

2013-09-28
==========
- Running an empty program, costs no time at all (according to /usr/bin/time)
  - Obviously, we need a better timer
- Running an empty program, that has been instrumented with VampirTrace 5.14.4, costs 0.22 seconds
  - With VT_UNIFY=no and VT_COMPRESSION=no
  - This includes the time for startup and cleanup for VampirTrace
  - Running it a few dozen times, to remove cash effects and the like
  - export VT_UNIFY=no; /usr/bin/time ./main_vt.exe 
    0.00user 0.01system 0:00.22elapsed 5%CPU (0avgtext+0avgdata 17360maxresident)k
    0inputs+40outputs (0major+1292minor)pagefaults 0swaps
  - export VT_UNIFY=no; export VT_COMPRESSION=no; /usr/bin/time ./main_vt.exe 
    0.00user 0.01system 0:00.22elapsed 5%CPU (0avgtext+0avgdata 17360maxresident)k
    0inputs+40outputs (0major+1292minor)pagefaults 0swaps
- Running a program that calls a function 20Mio times, still takes no time (again, using /usr/bin/time)
  - The compiler actually eliminates the empty function call
- Running a program that calls a function 20Mio times, that has been instrumented with VampirTrace 5.14.4, costs 12.50 seconds
  - With VT_UNIFY=no and VT_COMPRESSION=no
  - This includes the time for startup and cleanup for VampirTrace
  - Running it a few dozen times, taking the best time!!
  - export VT_UNIFY=no; /usr/bin/time ./main_vt.exe 
    8.21user 3.70system 0:12.50elapsed 95%CPU (0avgtext+0avgdata 3770624maxresident)k
    0inputs+3125032outputs (0major+3366minor)pagefaults 0swaps
