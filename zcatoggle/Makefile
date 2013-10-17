CC = icc
AR = ar
RM = rm

all: zca-toggle.o libzca-toggle.so libzca-toggle.a mainDynamic mainStatic

zca-toggle.o: zca-toggle.c
	$(CC) -fPIC -g -c -Wall zca-toggle.c

libzca-toggle.so: zca-toggle.o
	$(CC) -shared -Wl,-soname,libzca-toggle.so -Wl,-init,myInit -o libzca-toggle.so zca-toggle.o -lc

libzca-toggle.a: zca-toggle.o
	$(AR) rc libzca-toggle.a zca-toggle.o

mainDynamic: libzca-toggle.so main.c
	$(CC) main.c libzca-toggle.so -o mainDynamic

mainStatic: libzca-toggle.a main.c
	$(CC) main.c libzca-toggle.a -o mainStatic

clean:
	$(RM) -f zca-toggle.o libzca-toggle.so libzca-toggle.a mainStatic mainDynamic
