
ROOT=../..
UTILS=..
CFLAGS = --std=c++11 -g -Wall $(EXTRA_WARNS) 

ARGS= -I. -I$(UTILS)/include -I$(ROOT)/deps/bandit

CC+  = $(CC) $(ARGS)
CXX+ = $(CXX) $(ARGS)

AR = ar
RM = rm
OPTS =

CODE = $(wildcard *.cpp)
# override CODE += $(wildcard $(ROOT)/common/src/concurrency/*.cpp)

OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

.PHONY: all build install clean

all: build

build: $(OBJ)
	$(CXX+) $(OBJ) -o maptest

test: build
	./maptest

.cpp.o:
	$(CXX+) $(CFLAGS) $(ARGS) $(OPTS) -c -Wall -o $@ $<

clean:
	$(RM) -f $(OBJ) *.a *.so 
