.PHONY: clean all

SRCS = $(wildcard *.cpp)

EXES = $(patsubst %.cpp,%.exe,$(SRCS))

DEPS = -L../../build -lfastinst
 
all: $(EXES)

%.exe: %.cpp

	icpc --std=c++11 -g $(CFLAGS) -I../../include $< -o $@ $(DEPS) 


test: $(EXES)
	./testall.sh

clean: 
	rm *.exe
