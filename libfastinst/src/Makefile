
ROOT = ../..
INSTALL= $(ROOT)/build/lib
# CFLAGS=-DPROBE_TRUE_EMPTY_ON -DPROBE_CPU_TIME
# CFLAGS=-DPROBE_HIST_ON -DPROBE_CPU_TIME

EXTRA_WARNS=
#-Wextra 
## -Wformat-nonliteral -Wcast-align -Wpointer-arith -Wbad-function-cast -Wmissing-prototypes -Wstrict-prototypes -Wmissing-declarations -Winline -Wundef -Wnested-externs -Wcast-qual -Wshadow -Wwrite-strings -Wno-unused-parameter -Wfloat-equal

ARGS= -g -Wall -pedantic $(EXTRA_WARNS) -I$(ROOT)/include -I$(ROOT)/deps/distorm/include -I.
LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared 
DEPS = $(INSTALL)/libpointpatch.a
LIBS = 

CC = icc $(ARGS)
CXX= icpc -std=c++11 $(ARGS)
# COMP = gcc
# CC = gcc $(ARGS)
# CXX ?= g++ 

AR = ar
RM = rm
OPTS = 

CODE = finstrument_probe_provider.cpp probe_provider.cpp lock.cpp cyg_functions.cpp globals.cpp 
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

.PHONY: all build install clean

all: build

build: $(OBJ) 

libfastinst.a: build
	if [ -d libfastinst.a ]; then $(RM) -f libfastinst.a; fi
	mkdir temp;cd temp; $(AR) -x ../$(DEPS)
	$(AR) rc libfastinst.a  $(OBJ) temp/*.o
	$(RM) -rf temp

libfastinst.so: build
	$(CC) $(LIBS) -shared -Wl,-soname,libfastinst.so -o libfastinst.so $(OBJ) $(DEPS)

install: libfastinst.a libfastinst.so
	(cp libfastinst.so $(INSTALL))
	(cp libfastinst.a $(INSTALL))

.cpp.o:
	$(CXX) -std=c++11 $(ARGS) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<

clean:
	$(RM) -f $(OBJ) *.so $(INSTALL)/libfastinst.*
