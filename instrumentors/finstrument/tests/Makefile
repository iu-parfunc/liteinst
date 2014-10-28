
ARGS= -g -Wall
LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared
CFLAGS=-O3

CC = icc $(ARGS)
CPP = icpc -std=c++11 $(ARGS)
# CC = clang $(ARGS)
# CPP = clang -std=c++11 $(ARGS)
# CC = gcc $(ARGS)
# CPP = g++ -std=c++0x $(ARGS)

AR = ar
RM = rm
OPTS =

CODE = c_test.c cpp_test.cpp
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

.PHONY: all build tests 

.cpp.o:
	    $(CPP) $(CFLAGS) $(OPTS) -finstrument-functions -g -c -o $@ $<
.c.o:
	    $(CC) $(CFLAGS) $(OPTS) -finstrument-functions -g -c -o $@ $<

all: tests

run_c: $(OBJ)
	cd ../src;make clean;make
	# $(CPP) $(CFLAGS) $(OPTS) -c -o tests.o tests.c 
	# $(CC) $(CFLAGS) $(OPTS) -finstrument-functions -g -c -o tests.o tests.c
	# $(CPP) $(CFLAGS) $(OPTS) -c -o finstrumentor.o ../src/finstrumentor.cpp
	# $(CPP) $(CFLAGS) $(OPTS) -c -o cyg_functions.o ../src/cyg_functions.cpp
	$(CPP) c_test.o ../src/cyg_functions.o ../src/finstrumentor.o ../../../deps/distorm/distorm3.a -o tests.exe
	# $(CPP) tests.o cyg_functions.o finstrumentor.o ../../../deps/capstone/libcapstone.a -o tests.exe
	./tests.exe 

run_cpp: $(OBJ)
	cd ../src;make clean;make
	# $(CPP) $(CFLAGS) $(OPTS) -finstrument-functions -g -c -o tests.o tests.cpp
	# $(CPP) $(CFLAGS) $(OPTS) -c -o cyg_functions.o ../src/cyg_functions.cpp
	$(CPP) cpp_test.o ../src/cyg_functions.o ../src/finstrumentor.o ../../../deps/distorm/distorm3.a -o tests.exe
	# $(CPP) tests.o ../src/cyg_functions.o -o tests.exe
	./tests.exe 

runso_c: $(OBJ)
	cd ../src;make clean;make
	#$(CPP) $(CFLAGS) $(OPTS) -finstrument-functions -g -c -o tests.o tests.cpp
	# $(CC) $(CFLAGS) $(OPTS) -finstrument-functions -g -c -o tests.o tests.c
	$(CC) -shared -Wl,-soname,libinst.so -o libinst.so ../src/cyg_functions.o ../src/finstrumentor.o ../../../deps/distorm/distorm3.a -lc
	$(CC) -L. -linst c_test.o -o tests.exe
	LD_LIBRARY_PATH=.:$(LD_LIBRARY_PATH) ./tests.exe

runso_cpp: $(OBJ)
	cd ../src;make clean;make
	$(CPP) -shared -Wl,-soname,libinst.so -o libinst.so ../src/cyg_functions.o ../src/finstrumentor.o ../../../deps/distorm/distorm3.a -lc
	$(CPP) -L. -linst cpp_test.o -o tests.exe
	LD_LIBRARY_PATH=.:$(LD_LIBRARY_PATH) ./tests.exe

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
	$(RM) -f $(OBJ) *.exe *.so *.o
