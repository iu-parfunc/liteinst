
# CFLAGS += -DOVERHEAD_TIME_SERIES 
# CFLAGS += -DNO_INIT 
ARGS= -g -Wall -fpermissive
LIBS = -lpthread
LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared

# CCC = icc
# CC = icc $(ARGS)
# CPP = icpc -std=c++11 $(ARGS)
# COMP = gcc
CCC = gcc
CC = gcc $(ARGS)
CPP = g++ -std=c++0x $(ARGS)

AR = ar
RM = rm
OPTS = 

DEPS = ../../deps/distorm/distorm3.a

CODE = fprofiler.cpp minimal_backoff_profiler.cpp sprofiler.cpp minimal_sampling_profiler.cpp adaptive_profiler.cpp eprofiler.cpp minimal_adaptive_profiler.cpp profiler.cpp initializer.cpp globals.cpp overhead_series.cpp 
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

INST_SRC_C := $(wildcard ../../instrumentors/finstrument/src/*.c)
INST_SRC_CPP:= $(wildcard ../../instrumentors/finstrument/src/*.cpp)
INST_SRC := $(INST_SRC_C)$(INST_SRC_CPP)

INST_SRC_NO_PROF_CPP = $(filter-out ../../instrumentors/finstrument/src/cyg_functions.cpp, $(wildcard ../../instrumentors/finstrument/src/*.cpp))
INST_SRC_NO_PROF_CYG = $(wildcard ../../instrumentors/finstrument/src/noprof/*.cpp)
INST_SRC_NO_PROF0 :=  $(INST_SRC_C)$(INST_SRC_NO_PROF_CPP)
INST_SRC_NO_PROF :=  $(INST_SRC_NO_PROF0) $(INST_SRC_NO_PROF_CYG) # space between two entries is required for some reason 

INST_OBJ0 = $(INST_SRC:.cpp=.o)
INST_OBJ = $(INST_OBJ0:.c=.o)

INST_OBJ0_NO_PROF = $(INST_SRC_NO_PROF:.cpp=.o)
INST_OBJ_NO_PROF = $(INST_OBJ0_NO_PROF:.c=.o)

INSTALL = ../../build

.PHONY: all build install

libubiprof.so: libubiprof.a 
	$(CC) $(LIBS) -shared -Wl,-soname,libubiprof.so -o libubiprof.so $(OBJ) $(INST_OBJ) $(DEPS) -lc -lrt -lm

noprof.so: noprof.a 
	if [ -d libubiprof.so ]; then rm -f libubiprof.so; fi	
	$(CC) $(LIBS) -shared -Wl,-soname,libubiprof.so -o libubiprof.so $(INST_OBJ_NO_PROF) $(DEPS) -lc -lrt -lm

# TODO: Include lpapi.a in the static library
libubiprof.a: build distorm
	if [ -d libubiprof.a ]; then rm -f libubiprof.a; fi	
	mkdir temp;cd temp; $(AR) -x ../$(DEPS)
	$(AR) rc libubiprof.a  $(OBJ) $(INST_OBJ) temp/*.o
	rm -rf temp

noprof.a: build
	if [ -d libubiprof.a ]; then rm -f libubiprof.a; fi	
	$(AR) rc libubiprof.a $(OBJ) $(INST_OBJ_NO_PROF) 

all: build

build: $(OBJ) 

install:
	-rm -r $(INSTALL)
	mkdir $(INSTALL)
	mkdir $(INSTALL)/noprof
	make libubiprof.so
	cp libubiprof.so $(INSTALL) 
	cp libubiprof.a $(INSTALL) 
	make noprof.so
	cp libubiprof.so $(INSTALL)/noprof
	cp libubiprof.a $(INSTALL)/noprof

distorm: 
	cd ../../deps/distorm/make/linux;make clean;make CC=$(CCC) CFLAGS='-fPIC -c -Wall $(CFLAGS)' 

.cpp.o:
	  $(CPP) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<
.c.o:
	  $(CC) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<

clean:
	$(RM) -rf $(OBJ) *.a *.so 
	$(RM) -rf $(INSTALL) temp*




