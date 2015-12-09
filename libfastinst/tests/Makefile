.PHONY: clean all

SRCS = $(wildcard *.cpp)

EXES = $(patsubst %.cpp,%.exe,$(SRCS))

# CFLAGS = -finstrument-functions -fno-inline -fno-optimize-sibling-calls

LIBS = -L../../build/lib -lfastinst -ldl -lpthread
# LIBS = ../../build/lib/libfastinst.a
# LIBS = -L. -ltest 

all: $(EXES)

test.so:
	g++ -std=c++11 -fPIC -g -c -Wall -o test.o test.cpp
	g++ -fPIC -shared -Wl,-soname,libtest.so -o libtest.so test.o 

main: test.so
	g++ --std=c++11 -g $(CFLAGS) main.cpp -o main.exe $(LIBS)  

asmjit:
	$(CXX) --std=c++11 -g  $(LIBS)  $(CFLAGS) -I../../deps/asmjit/src/asmjit asmjit.cpp -o asmjit.exe

%.exe: %.cpp

	$(CXX) --std=c++11 -g $(CFLAGS) -I../../include $< -o $@ $(LIBS)


test: $(EXES)
	./testall.sh
	
clean:
	rm -f *.o *.exe 
