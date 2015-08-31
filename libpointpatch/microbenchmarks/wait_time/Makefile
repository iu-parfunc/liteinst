# hardcoded ??
UBIROOT=../../../

LIBDIR=$(UBIROOT)build/lib

#DEPS =  ../../deps/distorm/distorm3.a -lpointpatch -lpthread 
# USE .a for now (static) 
DEPS =  $(UBIROOT)/deps/distorm/distorm3.a $(LIBDIR)/libpointpatch.a -lpthread 

all: 	
	(cd ../../tests && make CC=$(CC) CXX=$(CXX) CFLAGS=$(CFLAGS))  


installLib: 
	(cd ../../src && make install CC=$(CC) CXX=$(CXX) CFLAGS=$(CFLAGS))  

clean: 
	(cd ../../tests && make clean)  
	rm sequential_straddling_patch.exe


cleanLib: 
	(cd ../../src && make clean)

seq_straddle: 
	$(CC)  -std=gnu99 -g $(CFLAGS) -I../../../deps/distorm/include -I../../../include sequential_straddling_patch.c -o sequential_straddling_patch.exe -L$(LIBDIR) $(DEPS) 

