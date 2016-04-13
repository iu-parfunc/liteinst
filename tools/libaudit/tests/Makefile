.PHONY: clean all

ROOT = ../../..

SRCS = $(wildcard *.cpp)

EXES = $(patsubst %.cpp,%.exe,$(SRCS))

# CFLAGS = -O3

ARGS = -I../src/

LIBS = -L$(ROOT)/build/lib -laudit # -lelf -lrt

all: $(EXES)

%.exe: %.cpp

	$(CXX) --std=c++11 -g $(CFLAGS) $(ARGS) -I$(ROOT)/include -I$(ROOT)/common/include -I$(ROOT)/deps/distorm/include $< -o $@ $(LIBS)


test: $(EXES)
	readelf -s control_flows.exe |grep FUNC | awk -F '[[:space:]]+' '{print $$3 "," $$9}' | sort -t, -k1 > functions.txt
	LD_PRELOAD=$(ROOT)/build/lib/libaudit.so ./control_flows.exe
	# LD_PRELOAD=$(ROOT)/build/lib/libaudit.so AUDIT_FUNCTIONS=_Z10while_funcv ./control_flows.exe
	# LD_LIBRARY_PATH=../../build/lib:$(LD_LIBRARY_PATH) AUDIT_FUNCTIONS=_Z10while_funcv ./control_flows.exe
	
clean:
	rm -f *.o *.exe *.txt *.out
