.PHONY: clean all

SRCS = $(wildcard *.cpp)

EXES = $(patsubst %.cpp,%.exe,$(SRCS))

CFLAGS = -finstrument-functions -fno-inline -fno-optimize-sibling-calls

LIBS = -L../../build/lib -lfastinst -lpointpatch -ldl

CXX=icpc

all: $(EXES)

%.exe: %.cpp

	$(CXX) --std=c++11 -g $(CFLAGS) -I../../include $< -o $@ $(LIBS)


test: $(EXES)
	LD_LIBRARY_PATH=../../build/lib:$(LD_LIBRARY_PATH) ./test_probe_activation.exe

clean:
	rm -f *.o *.exe
