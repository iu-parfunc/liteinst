.PHONY: clean all
# TODO: Rewrite completely when more done 



SRCS = $(wildcard *.c)

EXES = $(patsubst %.c,%.exe,$(SRCS))

DEPS = ../../../../../deps/distorm/distorm3.a

all: $(EXES)

%.exe: %.c

	gcc --std=gnu99 -I../ ../patcher.c $< -o $@ $(DEPS)


test: $(EXES)
	./testall.sh

clean: 
	rm *.exe
