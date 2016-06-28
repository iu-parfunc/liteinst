CPP=g++

SRCS = $(wildcard *.cpp)

EXES = $(patsubst %.cpp,%.exe,$(SRCS))

# I guess distorm and libpointpatch should be on the rhs of this rule
all: $(EXES) 

run: $(EXES)
	./xray.exe

%.exe: %.cpp
	$(CPP)  -std=c++11 -g $(CFLAGS) $< -o $@  -pthread

clean:
	rm -f *.exe
