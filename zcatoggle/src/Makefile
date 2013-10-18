
CC = icc -I./include/
AR = ar
RM = rm

INSTALLD=../build

SRC= zca-toggle.c zca-types.h include/zca-toggle.h

.PHONY: all build install 

all: build install

build:	zca-toggle.o libzca-toggle.so libzca-toggle.a 
install: build
	if ! [ -d $(INSTALLD) ]; then mkdir -p $(INSTALLD); fi	
	cp libzca-toggle.so libzca-toggle.a $(INSTALLD)

zca-toggle.o: $(SRC)
	$(CC) -fPIC -g -c -Wall zca-toggle.c

libzca-toggle.so: zca-toggle.o
	$(CC) -shared -Wl,-soname,libzca-toggle.so -Wl,-init,initZCAService -o libzca-toggle.so zca-toggle.o -lc

libzca-toggle.a: zca-toggle.o
	$(AR) rc libzca-toggle.a zca-toggle.o

clean:
	$(RM) -f zca-toggle.o libzca-toggle.so libzca-toggle.a $(INSTALLD)/libzca-toggle.so $(INSTALLD)/libzca-toggle.a 
