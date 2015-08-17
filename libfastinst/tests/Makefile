.PHONY: clean all

SRCS = $(wildcard *.cpp)

EXES = $(patsubst %.cpp,%.exe,$(SRCS))

LIBS = -L../../build/lib -lfastinst
 
all: $(EXES)

%.exe: %.cpp

	icpc --std=c++11 -g $(CFLAGS) -I../../include $< -o $@ $(LIBS) 


test: $(EXES)
	./testall.sh

clean: 
	rm *.o *.exe
