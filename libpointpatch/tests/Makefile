.PHONY: clean all



SRCS = $(wildcard *.c)

EXES = $(patsubst %.c,%.exe,$(SRCS))

# hardcoded ??
UBIROOT=../../

LIBDIR=$(UBIROOT)build/lib

#DEPS =  ../../deps/distorm/distorm3.a -lpointpatch -lpthread 
# USE .a for now (static) 
DEPS =  $(UBIROOT)/deps/distorm/distorm3.a $(LIBDIR)/libpointpatch.a -lpthread 



# Old
#-I../src ../src/patcher.c

# I guess distorm and libpointpatch should be on the rhs of this rule
all: $(EXES) 

%.exe: %.c
	gcc  --std=gnu99 -g $(CFLAGS) -I../../deps/distorm/include -I../../include  $< -o $@ -L$(LIBDIR) $(DEPS) 


test: $(EXES)
	./testall.sh

clean:
	rm -f *.exe
