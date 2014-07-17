CC = icc
CFLAGS =-lrt -Fa -O0
CFLAGSZCA = -lelf libzca-toggle.a
all: 01_emptyFunction.exe 01_emptyFunctionParallel.exe 02_notActivatedNotifyFunction.exe 02_notActivatedNotifyFunctionParallel.exe 03_ActivatedNotifyFunction.exe 03_ActivatedNotifyFunctionParallel.exe 04_emptyLoop.exe 04_emptyLoopParallel.exe 05_notActivatedNotifyLoop.exe 05_notActivatedNotifyLoopParallel.exe 06_ActivatedNotifyLoop.exe 06_ActivatedNotifyLoopParallel.exe

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

clean:
	rm -f *.exe *.o *.s 
