ifeq ($(STRADDLE_POINT),)
	STRADDLE_POINT=1
endif

# hardcoded ??
UBIROOT=../../

LIBDIR=$(UBIROOT)build/lib

#DEPS =  ../../deps/distorm/distorm3.a -lpointpatch -lpthread 
# USE .a for now (static) 
DEPS =  $(UBIROOT)/deps/distorm/distorm3.a $(LIBDIR)/libcallpatch.a -lpthread 

all: straddle_wait_free 

clean: 
	rm test_straddle_wait_free.exe

straddle_wait_free: 
	$(CC)  -std=gnu99 -g $(CFLAGS) -I$(UBIROOT)deps/distorm/include -I$(UBIROOT)include test_straddle_wait_free.c -o test_straddle_wait_free.exe -L$(LIBDIR) $(DEPS) 

test: straddle_wait_free
	./test_straddle_wait_free.exe $(STRADDLE_POINT)
