
ARGS= -g -Wall
LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared

CC = icc $(ARGS)
CPP = icpc -std=c++11 $(ARGS)
# CC = gcc $(ARGS)
# CPP = g++ -std=c++0x $(ARGS)

AR = ar
RM = rm
OPTS =

CODE = tests.cpp
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

.PHONY: all build tests 

.cpp.o:
	    $(CPP) $(CFLAGS) $(OPTS) -finstrument-functions -g -c -o $@ $<
.c.o:
	    $(CC) $(CFLAGS) $(OPTS) -finstrument-functions -g -c -o $@ $<

all: tests

run_c:
	$(CC) tests.o ../src/cyg_functions.o -o tests.exe
	./tests.exe 

run_cpp:
	$(CPP) tests.o ../src/cyg_functions.o -o tests.exe
	./tests.exe 

runso_c:
	$(CC) -shared -Wl,-soname,libinst.so -o libinst.so ../src/cyg_functions.o -lc
	$(CC) -L. -linst tests.o -o tests.exe
	LD_LIBRARY_PATH=.:$(LD_LIBRARY_PATH) ./tests.exe

runso_cpp:
	$(CPP) -shared -Wl,-soname,libinst.so -o libinst.so ../src/cyg_functions.o -lc
	$(CPP) -L. -linst tests.o -o tests.exe
	LD_LIBRARY_PATH=.:$(LD_LIBRARY_PATH) ./tests.exe

tests: $(OBJ)  
	cd ../src;make clean;make
	@echo -e "\n\n========================================"
	@echo "Running directly linked to the binary (C)"
	@echo "========================================"
	make run_c
	@echo -e "\n\n========================================"
	@echo "Running as a linked shared library (C)"
	@echo "========================================"
	make runso_c
	@echo -e "\n\n========================================"
	@echo "Running directly linked to the binary (C++)"
	@echo "========================================"
	make run_cpp
	@echo -e "\n\n========================================"
	@echo "Running as a linked shared library (C++)"
	@echo "========================================"
	make runso_cpp

clean:
	$(RM) -f $(OBJ) tests.exe *.so *.o
