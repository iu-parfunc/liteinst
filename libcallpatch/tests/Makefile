
SRCS = $(wildcard *.c)

EXES = $(patsubst %.c,%.exe,$(SRCS))

# hardcoded ??
UBIROOT=../../

LIBDIR=$(UBIROOT)build/lib

#DEPS =  ../../deps/distorm/distorm3.a -lpointpatch -lpthread 
# USE .a for now (static) 
DEPS =  $(UBIROOT)/deps/distorm/distorm3.a $(LIBDIR)/libcallpatch.a -lpthread 

all: $(EXES) 

clean: 
	rm *.exe

%.exe: %.c
	$(CC)  -std=gnu99 -g $(CFLAGS) -I$(UBIROOT)deps/distorm/include -I$(UBIROOT)include -I$(UBIROOT)common/include $< -o $@ -L$(LIBDIR) $(DEPS) 

test: $(EXES) 
	./testall.sh
