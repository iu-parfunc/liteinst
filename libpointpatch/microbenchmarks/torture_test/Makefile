# hardcoded ??
UBIROOT=../../../

LIBDIR=$(UBIROOT)build/lib

#DEPS =  ../../deps/distorm/distorm3.a -lpointpatch -lpthread 
# USE .a for now (static) 
DEPS =  $(UBIROOT)/deps/distorm/distorm3.a $(LIBDIR)/libpointpatch.a -lpthread 

#all: 	
#	(cd ../../tests && make CC=$(CC) CXX=$(CXX) CFLAGS=$(CFLAGS))  


installLib: 
	(cd ../../src && make install CC=$(CC) CXX=$(CXX) EXTRA_FLAGS=$(EXTRA_FLAGS))  

#clean: 
#	(cd ../../tests && make clean)  



cleanLib: 
	(cd ../../src && make clean)


SRCS = $(wildcard *.c)

EXES = $(patsubst %.c,%.exe,$(SRCS))


DEPS =  $(UBIROOT)/deps/distorm/distorm3.a $(LIBDIR)/libpointpatch.a -lpthread 

# I guess distorm and libpointpatch should be on the rhs of this rule
all: $(EXES) 

%.exe: %.c
	$(CC)  -std=gnu99 -g $(CFLAGS) -I$(UBIROOT)deps/distorm/include -I$(UBIROOT)include  $< -o $@ -L$(LIBDIR) $(DEPS) 

clean:
	rm -f *.exe
