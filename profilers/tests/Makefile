
ARGS= -g -Wall
LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared
CFLAGS=-O2

CC = icc $(ARGS)
CPP = icpc -std=c++11 $(ARGS)
# CC = clang $(ARGS)
# CPP = clang -std=c++11 $(ARGS)
# CC = gcc $(ARGS)
# CPP = g++ -std=c++0x $(ARGS)

AR = ar
RM = rm
OPTS =

DEPS = ../../deps/distorm/distorm3.a

CODE = c_test.c cpp_test.cpp
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

INST_SRC_C := $(wildcard ../../instrumentors/finstrument/src/*.c)
INST_SRC_CPP:= $(wildcard ../../instrumentors/finstrument/src/*.cpp)
INST_SRC := $(INST_SRC_C)$(INST_SRC_CPP)

INST_OBJ0 = $(INST_SRC:.cpp=.o)
INST_OBJ = $(INST_OBJ0:.c=.o)

PROF_SRC_C := $(wildcard ../src/*.c)
PROF_SRC_CPP:= $(wildcard ../src/*.cpp)
PROF_SRC := $(PROF_SRC_C)$(PROF_SRC_CPP)

PROF_OBJ0 = $(PROF_SRC:.cpp=.o)
PROF_OBJ = $(PROF_OBJ0:.c=.o)

.PHONY: all build tests 

.cpp.o:
	    $(CPP) $(CFLAGS) $(OPTS) -finstrument-functions -fno-optimize-sibling-calls -g -c -o $@ $<
.c.o:
	    $(CC) $(CFLAGS) $(OPTS) -finstrument-functions -fno-optimize-sibling-calls -g -c -o $@ $<

all: tests

inst:
	cd ../../instrumentors/finstrument/src;make clean;make

run_c: $(OBJ) inst 
	cd ../src;make clean;make build
	$(CPP) c_test.o $(PROF_OBJ) $(INST_OBJ) $(DEPS) -o tests.exe
	./tests.exe 

run_cpp: $(OBJ) inst
	cd ../src;make clean;make build
	$(CPP) cpp_test.o $(PROF_OBJ) $(INST_OBJ) $(DEPS) -o tests.exe
	./tests.exe 

runso_c: $(OBJ) inst
	cd ../src;make clean;make install
	$(CC) -L../../build -lubiprof c_test.o -o tests.exe
	LD_LIBRARY_PATH=../../build:$(LD_LIBRARY_PATH) ./tests.exe

runso_cpp: $(OBJ) inst
	cd ../src;make clean;make install
#	$(CPP) -shared -Wl,-soname,libinst.so -o libinst.so ../src/cyg_functions.o ../src/finstrumentor.o ../../../deps/distorm/distorm3.a -lc
	$(CPP) -L../../build -lubiprof cpp_test.o -o tests.exe
	LD_LIBRARY_PATH=../../build:$(LD_LIBRARY_PATH) ./tests.exe

time_so:
	cd ../src;make clean;make
	# $(CC) -g -c -o time.o time.c
	# $(CC) -lrt time.o -o time.exe
	# ./time.exe
	$(CC) -finstrument-functions -g -c -o time.o time.c
	$(CC) -shared -Wl,-soname,libinst.so -o libinst.so ../src/cyg_functions.o ../../../deps/distorm/distorm3.a -lc
	$(CC) -L. -linst -lrt time.o -o time.exe
	LD_LIBRARY_PATH=.:$(LD_LIBRARY_PATH) ./time.exe

time:
	cd ../src;make clean;make
	$(CC) -finstrument-functions -g -c -o time.o time.c
	$(CC) -lrt time.o ../src/cyg_functions.o -o time.exe
	./time.exe

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

exp: $(OBJ)
	$(CC) -c -o funcs.o funcs.c
	$(CC) funcs.o tests.o -o tests.exe
	./tests.exe

exp_so: $(OBJ)
	$(CC) -fPIC -c -o funcs.o funcs.c
	$(CC) -shared -Wl,-soname,libex.so -o libex.so funcs.o -lc
	$(CC) -L. -lex tests.o -o tests.exe
	LD_LIBRARY_PATH=.:$(LD_LIBRARY_PATH) ./tests.exe

clean:
	cd ../../instrumentors/finstrument/src; $(RM) -f $(OBJ) *.exe *.so *.o
	cd ../src; $(RM) -f $(OBJ) *.exe *.so *.o
	$(RM) -f $(OBJ) *.exe *.so *.o 
