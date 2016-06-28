
ROOT = ../../..
INSTALL= $(ROOT)/build/lib
DEPS_ROOT= $(ROOT)/deps

EXTRA_WARNS= -pedantic 

CFLAGS = --std=c99 -g -Wall $(EXTRA_WARNS) 


ARGS= -I$(ROOT)/include -I$(ROOT)/common/include -I$(ROOT)/deps/distorm/include 


all: relocator.c 
	gcc $(CFLAGS) $(ARGS) -c relocator.c -o relocator.o 

# test: testit.c testcall.c
# 	gcc -g testit.c relocator.o  $(DEPS_ROOT)/distorm/distorm3.a -o testit.exe
# 	gcc -g testcall.c relocator.o  $(DEPS_ROOT)/distorm/distorm3.a -o testcall.exe	

clean:
	$(RM) -f *.a *.so 
