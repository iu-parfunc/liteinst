.PHONY: clean all

SRCS = $(filter-out instrument_overhead.cpp, $(wildcard *.cpp))

EXES = $(patsubst %.cpp,%.exe,$(SRCS))

CFLAGS = -finstrument-functions -fno-inline -fno-optimize-sibling-calls

LIBS = -L../../build/lib -lfastinst -lrt -ldl

CXX=icpc

all: $(EXES) instrument_overhead.exe

# Handle this one separately since we don't use -finstrument in the microbench
instrument_overhead.exe: instrument_overhead.cpp
	$(CXX) --std=c++11 -g -I../../include -I../../common/include instrument_overhead.cpp -o instrument_overhead.exe $(LIBS)

#this does not work properly !!!
$(EXES): $(SRCS)
	$(CXX) --std=c++11 -g $(CFLAGS) -I../../include -I../../common/include $< -o $@ $(LIBS)


run: $(EXES) instrument_overhead.exe
	./runall.sh

clean:
	rm -f *.o *.exe


straddler: toggle_cost_straddlers.cpp
	$(CXX) --std=c++11 -g $(CFLAGS) -I../../include -I../../common/include toggle_cost_straddlers.cpp -o toggle_cost_straddlers.exe $(LIBS)
