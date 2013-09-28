2013-09-28
==========
- Running an empty file, costs no time at all (according to /usr/bin/time)
  - Obviously, we need a better timer
- Running an empty file, that has been instrumented with VampirTrace 5.14.4, costs 0.22 seconds
  - With VT_UNIFY=no and VT_COMPRESSION=no
  - Running it a few dozen times, to remove cash effects and the like
  - export VT_UNIFY=no; /usr/bin/time ./main_vt.exe 
    0.00user 0.01system 0:00.22elapsed 5%CPU (0avgtext+0avgdata 17360maxresident)k
    0inputs+40outputs (0major+1292minor)pagefaults 0swaps
  - export VT_UNIFY=no; export VT_COMPRESSION=no; /usr/bin/time ./main_vt.exe 
    0.00user 0.01system 0:00.22elapsed 5%CPU (0avgtext+0avgdata 17360maxresident)k
    0inputs+40outputs (0major+1292minor)pagefaults 0swaps
