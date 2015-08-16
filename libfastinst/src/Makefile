
# CFLAGS=-DPROBE_TRUE_EMPTY_ON -DPROBE_CPU_TIME
# CFLAGS=-DPROBE_HIST_ON -DPROBE_CPU_TIME

EXTRA_WARNS=
#-Wextra 
## -Wformat-nonliteral -Wcast-align -Wpointer-arith -Wbad-function-cast -Wmissing-prototypes -Wstrict-prototypes -Wmissing-declarations -Winline -Wundef -Wnested-externs -Wcast-qual -Wshadow -Wwrite-strings -Wno-unused-parameter -Wfloat-equal

ARGS= -g -Wall -pedantic $(EXTRA_WARNS) -I../../include -I../../deps/distorm/include -I.
LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared 
LIBS = -L../../build -lpointpatch

COMP = icc
CC = icc $(ARGS)
CPP = icpc -std=c++11 $(ARGS)
CXX= icpc -std=c++11 $(ARGS)
# COMP = gcc
# CC = gcc $(ARGS)
# CXX ?= g++ 

AR = ar
RM = rm
OPTS = 

CODE = finstrument_probe_provider.cpp lock.cpp cyg_functions.cpp globals.cpp 
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

.PHONY: all build 

all: build

build: $(OBJ) 

libfastinst.so: build
	$(CXX) $(LIBS) -shared -Wl,-soname,libfastinst.so -o libfastinst.so $(OBJ) 

.cpp.o:
	$(CXX) -std=c++11 $(ARGS) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<
.c.o:
	$(CC) $(ARGS) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<

clean:
	$(RM) -f $(OBJ) 
