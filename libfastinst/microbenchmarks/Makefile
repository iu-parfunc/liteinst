.PHONY: clean all

SRCS = $(wildcard *.cpp)

EXES = $(patsubst %.cpp,%.exe,$(SRCS))

CFLAGS = 

LIBS = -L../../build/lib -lfastinst -lrt -ldl

CXX=icpc

all: $(EXES)

%.exe: %.cpp

	$(CXX) --std=c++11 -g $(CFLAGS) -I../../include -I../../common/include $< -o $@ $(LIBS)


run: $(EXES)
	./runall.sh
	
clean:
	rm -f *.o *.exe
