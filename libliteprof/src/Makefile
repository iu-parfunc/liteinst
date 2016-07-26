
ROOT = ../..
INSTALL= $(ROOT)/build/lib

EXTRA_WARNS= -pedantic 

ARGS= -g -Wall $(EXTRA_WARNS) -I$(ROOT)/include -I$(ROOT)/utils/include -I.
LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared
DEPS = $(INSTALL)/libliteinst.a
LIBS = 

CC+  = $(CC) $(ARGS)
CXX+ = $(CXX) -std=c++11 $(ARGS)

AR = ar
RM = rm
OPTS =

CODE = sampling_profiler.cpp
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

.PHONY: all build install clean libliteprof.a libliteprof.so

all: build

build: $(OBJ) 

libliteprof.a: build
	if [ -d libliteprof.a ]; then $(RM) -f libliteprof.a; fi
	if [ -d temp ]; then $(RM) -rf temp; fi
	mkdir temp;cd temp; $(AR) -x ../$(DEPS)
	$(AR) rc libliteprof.a  $(OBJ) $(COMMON_OBJ) temp/*.o
	$(RM) -rf temp

libliteprof.so: build
	$(CXX+) $(LIBS) -shared -Wl,-soname,libliteprof.so -o libliteprof.so $(OBJ) \
	 -Wl,--whole-archive $(DEPS) -Wl,--no-whole-archive	

install: libliteprof.a libliteprof.so
	(mv libliteprof.so $(INSTALL))
	(mv libliteprof.a $(INSTALL))

.cpp.o:
	$(CXX+) $(CFLAGS) $(ARGS) -fPIC -c -Wall -o $@ $<

clean:
	$(RM) -f *.o *.a *.so $(INSTALL)/libliteprof.*
