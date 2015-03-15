
CFLAGS=-DPROBE_HIST_ON
ARGS= -g -Wall
LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared

# COMP = icc
CC = icc $(ARGS)
CPP = icpc -std=c++11 $(ARGS)
# COMP = gcc
# CC = gcc $(ARGS)
# CPP = g++ -std=c++0x $(ARGS)

AR = ar
RM = rm
OPTS = 

CODE = finstrumentor.cpp cyg_functions.cpp instrumentor.cpp noprof/no_prof_cyg_functions.cpp
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

.PHONY: all build 

all: build

build: $(OBJ) 

distorm: 
	cd ../../../deps/distorm/make/linux;make clean;make CC=$(COMP)

.cpp.o:
	  $(CPP) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<
.c.o:
	  $(CC) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<

clean:
	$(RM) -f $(OBJ) 




