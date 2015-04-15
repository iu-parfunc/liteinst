CC = icpc
CFLAGS =-O2 -g -finstrument-functions -fno-optimize-sibling-calls -std=c++0x 
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
	$(CC) $(CFLAGS)  toggle.c $(LDFLAGS_TOGGLE) -o toggle.exe

toggle: build_toggle
	./toggle.exe


clean:
	rm -f *.exe *.o *.s *.csv 40_deactivate.cpp libubiprof.so prof.out

