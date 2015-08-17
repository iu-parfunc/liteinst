LibPointPatch: cross modifying a single instruction safely
==========================================================

This library deals with the problem of patching instructions which may
be concurrently executed by other processors and which also may cross
cache-line boundaries.
