
ROOT=../..
UTILS=..
CFLAGS = --std=c++11 -g -Wall $(EXTRA_WARNS) 

ARGS= -I. -I$(UTILS)/include

CC+  = $(CC) $(ARGS)
CXX+ = $(CXX) $(ARGS)

AR = ar
RM = rm
OPTS =

CODE = $(wildcard *.cpp)
override CODE += $(wildcard $(ROOT)/utils/src/*.cpp)

OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

.PHONY: all build install clean

all: build

build: $(OBJ)
	$(CXX+) $(OBJ) -o tests -pthread

test: build
	./tests -s

.cpp.o:
	$(CXX+) $(CFLAGS) $(ARGS) $(OPTS) -c -Wall -o $@ $<

clean:
	$(RM) -f $(OBJ) *.o *.a *.so tests 
