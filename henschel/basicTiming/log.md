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
  - Even though I specificed no inline... there is probably still something wrong with what the compiler creates
- Running a program that calls a function 20Mio times, that has been instrumented with VampirTrace 5.14.4, costs 12.50 seconds
  - With VT_UNIFY=no and VT_COMPRESSION=no
  - This includes the time for startup and cleanup for VampirTrace
  - Running it a few dozen times, taking the best time!!
  - export VT_UNIFY=no; /usr/bin/time ./main_vt.exe 
    8.21user 3.70system 0:12.50elapsed 95%CPU (0avgtext+0avgdata 3770624maxresident)k
    0inputs+3125032outputs (0major+3366minor)pagefaults 0swaps
