
DEPS= libbar.a

EXTRA_WARNS= -pedantic
CFLAGS += --std=c++11 -g -Wall $(EXTRA_WARNS)

CC+  = $(CC)
CXX+ = $(CXX) $(CFLAGS)

CODE = $(wildcard *.cpp)

OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

DEPS = $(wildcard ../../deps/elph/obj/*.o)

foo: $(OBJ)
	$(CXX+) -shared -Wl,-soname,libfoo.so -Wl,-init,foo -o libfoo.so elf.o $(DEPS)

build: $(OBJ) 
	$(CXX+) -o elf.exe elf.o $(DEPS) 

test: foo
	LD_PRELOAD=/u/budkahaw/Builds/liteprof/sandbox/elf/libfoo.so ./h264ref

.cpp.o:
	  $(CXX+) -I../../deps/elph/include -fPIC -c -Wall -o $@ $<

clean:
	rm -f *.o *.a *.so *.exe 
