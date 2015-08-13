.PHONY: clean all

SRCS = $(wildcard *.c)

EXES = $(patsubst %.c,%.exe,$(SRCS))

DEPS = ../../../../../deps/distorm/distorm3.a -lpthread
 

all: $(EXES)

%.exe: %.c

	gcc --std=gnu99 -g $(CFLAGS) -I../ ../patcher.c $< -o $@ $(DEPS) 


test: $(EXES)
	./testall.sh

clean: 
	rm *.exe
