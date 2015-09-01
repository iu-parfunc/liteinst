.PHONY: clean all



SRCS = $(wildcard *.c)

EXES = $(patsubst %.c,%.exe,$(SRCS))

# hardcoded ??
UBIROOT=../../../

LIBDIR=$(UBIROOT)build/lib

#DEPS =  ../../deps/distorm/distorm3.a -lpointpatch -lpthread 
# USE .a for now (static) 
DEPS =  $(UBIROOT)/deps/distorm/distorm3.a $(LIBDIR)/libpointpatch.a -lpthread 



# Old
#-I../src ../src/patcher.c

# I guess distorm and libpointpatch should be on the rhs of this rule
all: $(EXES) 

%.exe: %.c
	$(CC)  -std=gnu99 -g $(CFLAGS) -I$(UBIROOT)deps/distorm/include -I$(UBIROOT)include  $< -o $@ -L$(LIBDIR) $(DEPS) 


test: $(EXES)
	./testall.sh

clean:
	rm -f *.exe




installLib: 
	(cd ../../src && make install CC=$(CC) CXX=$(CXX) CFLAGS=$(CFLAGS))  

cleanLib: 
	(cd ../../src && make clean)
