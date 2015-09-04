
ROOT = ../..
INSTALL= $(ROOT)/build/lib
# CFLAGS=-DPROBE_TRUE_EMPTY_ON -DPROBE_CPU_TIME
# CFLAGS=-DPROBE_HIST_ON -DPROBE_CPU_TIME

EXTRA_WARNS= -pedantic 
#-Wextra
## -Wformat-nonliteral -Wcast-align -Wpointer-arith -Wbad-function-cast -Wmissing-prototypes -Wstrict-prototypes -Wmissing-declarations -Winline -Wundef -Wnested-externs -Wcast-qual -Wshadow -Wwrite-strings -Wno-unused-parameter -Wfloat-equal

ARGS= -g -Wall $(EXTRA_WARNS) -I$(ROOT)/include -I$(ROOT)/common/include -I.
LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared
DEPS = $(INSTALL)/libfastinst.a
LIBS =

CC  ?= icc
CXX ?= icpc

CC+  = $(CC) $(ARGS)
CXX+ = $(CC) -std=c++11 $(ARGS)

AR = ar
RM = rm
OPTS =

COMMON = $(wildcard $(ROOT)/common/src/*.cpp)
COMMON_OBJ0 = $(COMMON:.cpp=.o)
COMMON_OBJ  = $(COMMON_OBJ0:.c=.o)

CODE = profiler.cpp backoff_profiler.cpp minimal_backoff_profiler.cpp sampling_profiler.cpp minimal_sampling_profiler.cpp initializer.cpp globals.cpp
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

.PHONY: all build install clean libubiprof.a libubiprof.so

all: build

build: $(OBJ) $(COMMON_OBJ)

libubiprof.a: build
	if [ -d libubiprof.a ]; then $(RM) -f libubiprof.a; fi
	mkdir temp;cd temp; $(AR) -x ../$(DEPS)
	$(AR) rc libubiprof.a  $(OBJ) $(COMMON_OBJ) temp/*.o
	$(RM) -rf temp

libubiprof.so: build
	$(CC+) $(LIBS) -shared -Wl,-soname,libubiprof.so -o libubiprof.so $(OBJ) $(COMMON_OBJ) $(DEPS)

install: libubiprof.a libubiprof.so
	(mv libubiprof.so $(INSTALL))
	(mv libubiprof.a $(INSTALL))

.cpp.o:
	$(CXX+) -std=c++11 $(ARGS) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<

clean:
	$(RM) -f $(OBJ) *.a *.so $(INSTALL)/libubiprof.*
