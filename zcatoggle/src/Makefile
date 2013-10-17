
CC = icc -I./include/
AR = ar
RM = rm

INSTALLD=../build

.PHONY: all build install

all: build install
build:	zca-toggle.o libzca-toggle.so libzca-toggle.a 
install: 
	if ! [ -d $(INSTALLD) ]; then mkdir -p $(INSTALLD); fi	
	cp libzca-toggle.so libzca-toggle.a $(INSTALLD)

zca-toggle.o: zca-toggle.c
	$(CC) -fPIC -g -c -Wall zca-toggle.c

libzca-toggle.so: zca-toggle.o
	$(CC) -shared -Wl,-soname,libzca-toggle.so -Wl,-init,myInit -o libzca-toggle.so zca-toggle.o -lc

libzca-toggle.a: zca-toggle.o
	$(AR) rc libzca-toggle.a zca-toggle.o

clean:
	$(RM) -f zca-toggle.o libzca-toggle.so libzca-toggle.a $(INSTALLD)/libzca-toggle.so $(INSTALLD)/libzca-toggle.a 
