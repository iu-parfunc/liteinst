CC = icc
CFLAGS =-lrt -Fa -O0
CFLAGSZCA = -lelf libzca-toggle.a
CFLAGSDYN = libdynaprof.a
#all: 01_emptyFunction.exe 01_emptyFunctionParallel.exe 02_notActivatedNotifyFunction.exe 02_notActivatedNotifyFunctionParallel.exe 03_ActivatedNotifyFunction.exe 03_ActivatedNotifyFunctionParallel.exe 04_emptyLoop.exe 04_emptyLoopParallel.exe 05_notActivatedNotifyLoop.exe 05_notActivatedNotifyLoopParallel.exe 06_ActivatedNotifyLoop.exe 06_ActivatedNotifyLoopParallel.exe
#all : 13_ActivatedNotifyFunction.exe 13_ActivatedNotifyFunctionParallel.exe 14_ActivatedNotifyLoop.exe 14_ActivatedNotifyLoopParallel.exe
#all: 23_ActivatedNotifyFunction.exe 23_ActivatedNotifyFunctionParallel.exe
all: 31_StartProfilerOverhead.exe

01_emptyFunction.exe: 01_emptyFunction.cpp
	$(CC) $(CFLAGS) 01_emptyFunction.cpp -o 01_emptyFunction.exe

01_emptyFunctionParallel.exe: 01_emptyFunction.cpp
	$(CC) $(CFLAGS) -openmp 01_emptyFunction.cpp -o 01_emptyFunctionParallel.exe

02_notActivatedNotifyFunction.exe: 02_notActivatedNotifyFunction.cpp
	$(CC) $(CFLAGS) 02_notActivatedNotifyFunction.cpp -o 02_notActivatedNotifyFunction.exe

02_notActivatedNotifyFunctionParallel.exe: 02_notActivatedNotifyFunction.cpp
	$(CC) $(CFLAGS) -openmp 02_notActivatedNotifyFunction.cpp -o 02_notActivatedNotifyFunctionParallel.exe

03_ActivatedNotifyFunction.exe: 03_ActivatedNotifyFunction.cpp
	$(CC) $(CFLAGS) 03_ActivatedNotifyFunction.cpp $(CFLAGSZCA) -o 03_ActivatedNotifyFunction.exe

03_ActivatedNotifyFunctionParallel.exe: 03_ActivatedNotifyFunction.cpp
	$(CC) $(CFLAGS) -openmp 03_ActivatedNotifyFunction.cpp $(CFLAGSZCA) -o 03_ActivatedNotifyFunctionParallel.exe

04_emptyLoop.exe: 04_emptyLoop.cpp
	$(CC) $(CFLAGS) 04_emptyLoop.cpp -o 04_emptyLoop.exe

04_emptyLoopParallel.exe: 04_emptyLoop.cpp
	$(CC) $(CFLAGS) -openmp 04_emptyLoop.cpp -o 04_emptyLoopParallel.exe

05_notActivatedNotifyLoop.exe: 05_notActivatedNotifyLoop.cpp
	$(CC) $(CFLAGS) 05_notActivatedNotifyLoop.cpp -o 05_notActivatedNotifyLoop.exe

05_notActivatedNotifyLoopParallel.exe: 05_notActivatedNotifyLoop.cpp
	$(CC) $(CFLAGS) -openmp 05_notActivatedNotifyLoop.cpp -o 05_notActivatedNotifyLoopParallel.exe

06_ActivatedNotifyLoop.exe: 06_ActivatedNotifyLoop.cpp
	$(CC) $(CFLAGS) 06_ActivatedNotifyLoop.cpp $(CFLAGSZCA) -o 06_ActivatedNotifyLoop.exe

06_ActivatedNotifyLoopParallel.exe: 06_ActivatedNotifyLoop.cpp
	$(CC) $(CFLAGS) -openmp 06_ActivatedNotifyLoop.cpp $(CFLAGSZCA) -o 06_ActivatedNotifyLoopParallel.exe

13_ActivatedNotifyFunction.exe: 13_ActivatedNotifyFunction.cpp
	$(CC) $(CFLAGS) 13_ActivatedNotifyFunction.cpp $(CFLAGSDYN) $(CFLAGSZCA) -o 13_ActivatedNotifyFunction.exe

13_ActivatedNotifyFunctionParallel.exe: 13_ActivatedNotifyFunction.cpp
	$(CC) $(CFLAGS) -openmp 13_ActivatedNotifyFunction.cpp $(CFLAGSDYN) $(CFLAGSZCA) -o 13_ActivatedNotifyFunctionParallel.exe

14_ActivatedNotifyLoop.exe: 14_ActivatedNotifyLoop.cpp
	$(CC) $(CFLAGS) 14_ActivatedNotifyLoop.cpp $(CFLAGSDYN) $(CFLAGSZCA) -o 14_ActivatedNotifyLoop.exe

14_ActivatedNotifyLoopParallel.exe: 14_ActivatedNotifyLoop.cpp
	$(CC) $(CFLAGS) -openmp 14_ActivatedNotifyLoop.cpp $(CFLAGSDYN) $(CFLAGSZCA) -o 14_ActivatedNotifyLoopParallel.exe

23_ActivatedNotifyFunction.exe: 23_ActivatedNotifyFunction.cpp
	$(CC) $(CFLAGS) 23_ActivatedNotifyFunction.cpp $(CFLAGSDYN) $(CFLAGSZCA) -o 23_ActivatedNotifyFunction.exe

23_ActivatedNotifyFunctionParallel.exe: 23_ActivatedNotifyFunction.cpp
	$(CC) $(CFLAGS) -openmp 23_ActivatedNotifyFunction.cpp $(CFLAGSDYN) $(CFLAGSZCA) -o 23_ActivatedNotifyFunctionParallel.exe

31_StartProfilerOverhead.exe: 31_StartProfilerOverhead.cpp
	$(CC) $(CFLAGS) -openmp 31_StartProfilerOverhead.cpp $(CFLAGSDYN) $(CFLAGSZCA) -o 31_StartProfilerOverhead.exe

clean:
	rm -f *.exe *.o *.s prof.out
