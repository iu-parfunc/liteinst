#CC = icpc
CC = g++
CFLAGS =-O2 -g -finstrument-functions -fno-optimize-sibling-calls -std=c++0x 
CFLAGS_TOGGLE =-O2 -g -fno-optimize-sibling-calls -std=c++0x 
LDFLAGS = -L../../../build -lubiprof -lrt -lpthread
LDFLAGS_TOGGLE = -lrt -lpthread

ifeq ($(N),)
	  N = 10 
endif

build:
	$(CC) $(CFLAGS)  cas.cpp $(LDFLAGS) -o cas.exe

run: build
	readelf -s cas.exe |grep FUNC | awk -F '[[:space:]]+' '{print $$3 "," $$9}' > functions.txt
	LD_LIBRARY_PATH=../../../build:$(LD_LIBRARY_PATH) ./cas.exe ${N}

build_toggle:
	$(CC) $(CFLAGS_TOGGLE)  toggle_1.c $(LDFLAGS_TOGGLE) -o toggle_1.exe

build_toggle2:
	$(CC) $(CFLAGS_TOGGLE)  toggle_2.c $(LDFLAGS_TOGGLE) -o toggle_2.exe

build_toggle3:
	$(CC) $(CFLAGS_TOGGLE)  toggle_3.c $(LDFLAGS_TOGGLE) -o toggle_3.exe

toggle: build_toggle
	./toggle_1.exe

toggle2: build_toggle2
	./toggle_2.exe	

toggle3: build_toggle3
	./toggle_3.exe	



clean:
	rm -f *.exe *.o *.s *.csv 40_deactivate.cpp libubiprof.so prof.out core

