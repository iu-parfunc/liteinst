CC=icc

run:
	$(CC) -finstrument-functions -g -c -o main.o main.c
	#gcc -g -c -o main.o main.c
	# gcc -mcx16 -g -c -o trace.o trace.c
	$(CC)  -g -c -o trace.o trace.c
	$(CC) main.o trace.o -o main
	./main
	#cat trace.out
	#./readtracelog.sh main trace.out

runso:
	# $(CC) -fPIC -g -c -Wall trace.c
	# $(CC) -shared -Wl,-soname,libprof.so -o libprof.so trace.o -lc
	$(CC) -finstrument-functions -g -c -o main.o main.c
	$(CC) -L. -lprof main.o -o main
	LD_LIBRARY_PATH=.:$(LD_LIBRARY_PATH) ./main

inc :
	gcc -g -o inc inc.c

dtest:
	gcc -g -o dtest dcas_test.c
	./dtest

dgcas :
	gcc -mcx16 dgcas.c -o dgcas 
	./dgcas

lockinc:
	gcc -g -o lockinc lockinc.c

ticks:
	gcc -g -o ticks rdtsc.c

ind:
	gcc -g -o ind asm_indirect_call.c

rel:
	gcc -g -o rel rel_call.c

clean:
	rm -f *.o main inc ticks lockinc dgcas dtest ind rel

