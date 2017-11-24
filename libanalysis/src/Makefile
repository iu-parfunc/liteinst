
ROOT= ../..
INSTALL= $(ROOT)/build/lib
DEPS_ROOT= $(ROOT)/deps

EXTRA_WARNS= -pedantic 

override CFLAGS += --std=c++11 -g -Wall $(EXTRA_WARNS) -DAUDIT 

ARGS= -I. -I$(ROOT)/include -I$(ROOT)/utils/include -I$(ROOT)/deps/distorm/include -I$(ROOT)/deps/elph/include

LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared
UTILS = $(INSTALL)/libutils.a 
DEPS = $(UTILS) $(ROOT)/deps/distorm/distorm3.a 
LIBS = -lstdc++ -pthread

CC+  = $(CC) $(ARGS)
CXX+ = $(CXX) $(ARGS) $(CFLAGS) 

AR = ar
RM = rm
OPTS =
FIND = find

CODE = $(wildcard *.cpp)

OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

.PHONY: all build install clean init

init:
	$(CXX) --version || echo OK

all: build

build: init $(OBJ) deps

deps: 
	if [ ! -d $(UTILS) ]; then (cd $(ROOT)/utils/src; make install); fi

libanalysis.a: build 
	if [ -d libanalysis.a ]; then $(RM) -f libanalysis.a; fi
	if [ -d temp ]; then $(RM) -rf temp; fi
	mkdir temp;cd temp; $(foreach dep,$(DEPS),$(AR) -x ../$(dep);) 
	$(AR) rc libanalysis.a  $(OBJ) temp/*.o
	$(RM) -rf temp

libanalysis.so: build 
	$(CXX+) $(LIBS) -shared -Wl,-soname,libanalysis.so  -Wl,-init -o libanalysis.so $(OBJ) $(DEPS)

install: libanalysis.a libanalysis.so
	(cp libanalysis.so $(INSTALL))
	(cp libanalysis.a $(INSTALL))

.cpp.o:
	$(CXX+) -fPIC -c -Wall -o $@ $<

clean:
	$(FIND) . \( -name '*.o' -o -name '*.a' -o -name '*.so' \) -type f -delete
	$(RM) -f $(INSTALL)/libanlysis.* app

app:
	$(CXX+) -g -o app app.cpp -L../../build/lib -lanalysis
