.PHONY: clean all
# TODO: Rewrite completely when more done 



SRCS = $(wildcard *.c)

EXES = $(patsubst %.c,%.exe,$(SRCS))

DEPS = ../../../../../deps/distorm/distorm3.a

all: $(EXES)

%.exe: %.c

       # gcc --verbose -std=gnu99 -pedantic -Wall -I../ ../patcher.c -o $@ $< 
	gcc --std=gnu99 -I../ ../patcher.c $< -o $@ $(DEPS)


test: $(EXES)
	./testall.sh

clean: 
	rm *.exe
