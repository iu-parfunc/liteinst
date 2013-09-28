2013-09-28
==========
- Running an empty file, costs no time at all (according to /usr/bin/time)
  - Obviously, we need a better timer
- Running an empty file, that has been instrumented with VampirTrace 5.14.4, costs 0.22 seconds
  - With VT_UNIFY=no
  - Running it a few dozen times, to remove cash effects and the like
