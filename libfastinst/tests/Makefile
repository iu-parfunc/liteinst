.PHONY: clean all

SRCS = $(wildcard *.cpp)

EXES = $(patsubst %.cpp,%.exe,$(SRCS))

LIBS = -L../../build/lib -lfastinst -lpointpatch -ldl

CXX=icpc

all: $(EXES)

%.exe: %.cpp

	$(CXX) --std=c++11 -g $(CFLAGS) -I../../include $< -o $@ $(LIBS)


test: $(EXES)
	./testall.sh

clean:
	rm -f *.o *.exe
