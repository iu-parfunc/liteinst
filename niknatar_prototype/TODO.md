


Option 1: Demonstrate incremental probe activation with Pin + invalidation.
---------------------------------------------------------------------------

  * expose a function to user activate_probe(char* name, AFUNPTR)
    (actually, you could separate this into a lookup_probe to get the
     address, followed by activate_probe)
  * implement it by:
  * (1) do insert_annotation_calls to register the function pointer
  * (2) use the probe address to invalidate the trace and re-JIT
  
  * Verify in a test program that the probes work.

Open questions:

 * Does PIN always check the code cache when jumping between basic blocks?

 * Brief empirical experimentation implies *yes*, that PIN does slow
   down the program even beyond the initial translation cost (when run
   with no tool).T


Option  2: Manually hack something together.
--------------------------------------------

 * Find the probe point (ask libzca)
 * Insert a jump at the probe point 
   (to a function pointer "foo", hardcoding 
    the address of foo in the assembly)
    
 * Figure out the format of a jump, insert a jump 

