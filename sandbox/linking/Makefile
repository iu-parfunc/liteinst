
LIBS = libbar.a

EXTRA_WARNS= -pedantic
CFLAGS += --std=c++11 -g -Wall $(EXTRA_WARNS)

CC+  = $(CC)
CXX+ = $(CXX) $(CFLAGS)

CODE = $(wildcard *.cpp)

OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

bar:
	ar rc libbar.a bar.o

foo: bar
	$(CXX+) $(LIBS) -shared -Wl,-soname,libfoo.so -Wl,-init,foo -o libfoo.so foo.o $(DEPS)

build: $(OBJ) foo
	$(CXX+) -o app.exe app.cpp  

test: build
	LD_PRELOAD=/u/budkahaw/Builds/liteprof/sandbox/linking/libfoo.so ./app.exe

.cpp.o:
	  $(CXX+) -fPIC -c -Wall -o $@ $<

clean:
	rm *.o *.a *.so *.exe 
