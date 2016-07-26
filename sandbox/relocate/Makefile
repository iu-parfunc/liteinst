ROOT= ../../
INSTALL= $(ROOT)/build/lib
DEPS_ROOT= $(ROOT)/deps

EXTRA_WARNS= 
# -pedantic 

CFLAGS = --std=c++11 -fpermissive -g -Wall $(EXTRA_WARNS) -I$(ROOT)/include -I$(ROOT)/libliteinst/src/liteprobes -I$(ROOT)/utils/include 


TARG = relocate_tests.exe\
       testcall.exe


all: $(TARG)

%.exe : %.cpp
	g++ $(CFLAGS) $< $(INSTALL)/libliteinst.a $(DEPS_ROOT)/distorm/distorm3.a -o $@ 


clean: 
	rm *.exe 
