

CC=icpc
LIBS= -lelf -lpthread \
  ../../zcatoggle/src/libdynprof.a \
  ../../zcatoggle/src/libzca-toggle.a

all:
	$(CC) -O0  -I ../../zcatoggle/src/include/ main.c $(LIBS)
