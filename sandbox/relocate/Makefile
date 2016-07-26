ROOT= ../../
INSTALL= $(ROOT)/build/lib
DEPS_ROOT= $(ROOT)/deps

TARG = relocate_tests.exe
EXTRA_WARNS= -pedantic 

CFLAGS = --std=c++11 -g -Wall $(EXTRA_WARNS) -I$(ROOT)/include 

all: $(TARG)

%.exe : %.cpp
	g++ $(CFLAGS) $< $(INSTALL)/libliteinst.a $(DEPS_ROOT)/distorm/distorm3.a -o $@ 
