CC = icpc
CFLAGS =-O2 -g -finstrument-functions -fno-optimize-sibling-calls -std=c++0x 
LDFLAGS = -L../../../build -lubiprof -lrt

ifeq ($(N),)
	  N = 10 
endif

default:
	$(CC) $(CFLAGS)  cas.cpp $(LDFLAGS) -o cas.exe

run:
	readelf -s cas.exe |grep FUNC | awk -F '[[:space:]]+' '{print $$3 "," $$9}' > functions.txt
	LD_LIBRARY_PATH=../../../build:$(LD_LIBRARY_PATH) ./cas.exe ${N}

clean:
	rm -f *.exe *.o *.s *.csv 40_deactivate.cpp libubiprof.so prof.out

