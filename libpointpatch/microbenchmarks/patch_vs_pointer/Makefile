

all: pointerswap.exe patcherswap.exe 

pointerswap.exe: pointerswap.c 	
	gcc --std=gnu99 pointerswap.c -o pointerswap.exe -lpthread -lrt


patcherswap.exe: patcherswap.c 
	gcc --std=gnu99 patcherswap.c -o patcherswap.exe -lpthread -lrt -I../../../include  -I../../../deps/distorm/include ../../../build/lib/libpointpatch.a


clean: 
	rm *.exe
