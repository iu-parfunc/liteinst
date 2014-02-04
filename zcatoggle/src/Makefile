
ARGS= -g -I./include/ -Wall
# CC  = gcc $(ARGS)
# CPP = g++ -std=c++11 $(ARGS)
# CC  = clang $(ARGS)
# CPP = clang $(ARGS)
CC  = icc $(ARGS)
CPP = icpc -std=c++11 $(ARGS)
AR = ar
RM = rm

INSTALLD=../build

HDRS = zca-types.h include/zca-toggle.h
CODE = zca-toggle.c elf-provider.cpp
OBJ0 = $(CODE:.c=.o)
OBJ  = $(OBJ0:.cpp=.o)
SRC = $(HDRS) $(CODE)

LIBS = -lelf

LIBSOUT = libzca-toggle.so libzca-toggle.a 

.PHONY: all build install 

all: build install

build:	$(LIBSOUT)
install: build
	if ! [ -d $(INSTALLD) ]; then mkdir -p $(INSTALLD); fi	
	cp $(LIBSOUT) $(INSTALLD)

test: build test.cpp hello_notify.exe elf-provider.o
	$(CPP) -g test.cpp libzca-toggle.a $(LIBS) -o test.exe
	./test.exe

hello_notify.exe: hello_notify.c
	$(CC) hello_notify.c -o hello_notify.exe

.c.o:
	$(CC) -fPIC -g -c -Wall -o $@ $<
.cpp.o:
	$(CPP) -fPIC -g -c -Wall -o $@ $<

libzca-toggle.so: $(OBJ)
	$(CC) $(LIBS) -shared -Wl,-soname,libzca-toggle.so -Wl,-init,initZCAService -o libzca-toggle.so $(OBJ) -lc

libzca-toggle.a: $(OBJ) 
	$(AR) rc libzca-toggle.a $(OBJ)

clean:
	$(RM) -f $(OBJ) $(LIBSOUT) $(INSTALLD)/libzca-toggle.so $(INSTALLD)/libzca-toggle.a hello_notify.exe
