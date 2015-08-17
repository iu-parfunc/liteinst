LibPointPatch: cross modifying a single instruction safely
==========================================================

This library deals with the problem of patching instructions which may
be concurrently executed by other processors and which also may cross
cache-line boundaries.





Info
---- 

flags (#define at compile time) : 
 * NO_WAIT - Disables the wait loop in patching protocol.
 * NON_THREADSAFE_PATCHING - Use non threadsafe patching protocol.