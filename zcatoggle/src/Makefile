
CC = icc -I./include/ -Wall
AR = ar
RM = rm

INSTALLD=../build

HDRS = zca-types.h include/zca-toggle.h
CODE = zca-toggle.c elf-provider.c
OBJ = $(CODE:.c=.o)
SRC = $(HDRS) $(CODE)

LIBS = libzca-toggle.so libzca-toggle.a 

.PHONY: all build install 

all: build install

build:	$(LIBS)
install: build
	if ! [ -d $(INSTALLD) ]; then mkdir -p $(INSTALLD); fi	
	cp $(LIBS) $(INSTALLD)

# %.o: $(SRC)
# 	$(CC) -fPIC -g -c -Wall zca-toggle.c

.c.o:
	$(CC) -fPIC -g -c -Wall -o $@ $<
#        $(COMPILE) -DCHECKING -c -o $@ $<

libzca-toggle.so: $(OBJ)
	$(CC) -shared -Wl,-soname,libzca-toggle.so -Wl,-init,initZCAService -o libzca-toggle.so $(OBJ) -lc

libzca-toggle.a: $(OBJ) 
	$(AR) rc libzca-toggle.a $(OBJ)

clean:
	$(RM) -f $(OBJ) $(LIBS) $(INSTALLD)/libzca-toggle.so $(INSTALLD)/libzca-toggle.a 
