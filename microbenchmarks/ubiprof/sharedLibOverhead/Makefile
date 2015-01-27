ARGS= -g -Wall
LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared

# COMP = icc
CC = icc $(ARGS)
CPP = icpc -std=c++11 $(ARGS)
# COMP = gcc
# CC = gcc $(ARGS)
# CPP = g++ -std=c++0x $(ARGS)

AR = ar
RM = rm
OPTS =

CODE = app_static.cpp app_so.cpp 
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

.PHONY: all build

all: build

build: $(OBJ)

so: 
	$(CPP) $(CFLAGS) $(OPTS) cyg_functions.cpp -fPIC -c -Wall -o cyg_functions.o
	$(CPP) -shared -Wl,-soname,libtest.so -o libtest.so cyg_functions.o 

runstatic: so build
	$(CPP) app_static.o -o app_static.exe
	/usr/bin/time -f "SELFTIMED %e\nuser %U\nsys %S\n" ./app_static.exe

runso: so build
	$(CPP) -L. -ltest app_so.o -o app_so.exe
	LD_LIBRARY_PATH=.:$(LD_LIBRARY_PATH) /usr/bin/time -f "SELFTIMED %e\nuser %U\nsys %S\n" ./app_so.exe

runlib: build
	$(CPP) -L../../../build -lrt -lubiprof app_so.o -o app_lib.exe
	LD_LIBRARY_PATH=../../../build:$(LD_LIBRARY_PATH) /usr/bin/time -f "SELFTIMED %e\nuser %U\nsys %S\n" ./app_lib.exe

.cpp.o:
	$(CPP) $(CFLAGS) $(OPTS) -finstrument-functions -fno-optimize-sibling-calls -c -Wall -o $@ $<
.c.o:
	$(CC) $(CFLAGS) $(OPTS) -finstrument-functions -fno-optimize-sibling-calls -c -Wall -o $@ $<

clean:
	$(RM) -f $(OBJ) *.exe *.so
