.PHONY: clean all

SRCS = $(wildcard *.cpp)

EXES = $(patsubst %.cpp,%.exe,$(SRCS))

CFLAGS = -finstrument-functions -fno-inline -fno-optimize-sibling-calls

LIBS = -L../../build/lib -lfastinst -ldl -lpthread

all: $(EXES)

%.exe: %.cpp

	$(CXX) --std=c++11 -g $(CFLAGS) -I../../include $< -o $@ $(LIBS)


test: $(EXES)
	./testall.sh
	
clean:
	rm -f *.o *.exe
