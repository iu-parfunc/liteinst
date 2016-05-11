.PHONY: clean all

ROOT = ../../..

SRCS = $(wildcard *.cpp) 

# CFLAGS = -O3

EXES = control_flows.exe 

ARGS = -I../src/ 

LIBS = -L$(ROOT)/build/lib -laudit # -lelf -lrt

all: $(EXES)

.cpp.o:
	  $(CXX+) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<

control_flows.exe: 
	$(CXX) --std=c++11 -g $(CFLAGS) $(ARGS) -I$(ROOT)/include -I$(ROOT)/common/include -I$(ROOT)/deps/distorm/include control_flows.cpp -o $(EXES) $(LIBS)


test: $(EXES)
	# LD_PRELOAD=$(ROOT)/build/lib/libaudit.so ./control_flows.exe
	# LD_LIBRARY_PATH=../../build/lib:$(LD_LIBRARY_PATH) LD_PRELOAD=$(ROOT)/build/lib/libaudit.so ./h264ref
	# LD_LIBRARY_PATH=../../build/lib:$(LD_LIBRARY_PATH) AUDIT_FUNCTIONS=_Z10while_funcv ./control_flows.exe
	LD_LIBRARY_PATH=../../../build/lib:$(LD_LIBRARY_PATH) ./control_flows.exe
	
clean:
	rm -f *.o *.exe *.txt *.out
