
ROOT = ../../../../
INSTALL= $(ROOT)/build/lib
DEPS_ROOT= $(ROOT)/deps

EXTRA_WARNS= 
#-pedantic 

CFLAGS = --std=c99 -g -Wall $(EXTRA_WARNS) -I../

TARG = testit.exe \
       testcall.exe \
       test_small1.exe

all: $(TARG)

%.exe : %.c
	gcc $(CFLAGS) $< ../relocator.o $(DEPS_ROOT)/distorm/distorm3.a -o $@ 


clean:
	$(RM) -f *.exe
