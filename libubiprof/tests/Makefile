CC=icc
CPP=icpc

stl.so:
	$(CPP) -std=c++11 -fPIC -I. -g -c -Wall -o init_issue/elf_utils.o init_issue/elf_utils.cpp 
	$(CPP) -fPIC -shared -Wl,-soname,libstl.so -o init_issue/libstl.so init_issue/elf_utils.o

stl: stl.so
	$(CPP) -std=c++11 -g -o init_issue/stl init_issue/stl_test.cpp -Linit_issue -lstl
	LD_LIBRARY_PATH=.:$(LD_LIBRARY_PATH) ./init_issue/stl

inst.so:
	$(CPP) -std=c++11 -fPIC -I. -I../../deps/asmjit/src/asmjit/ -g -c -Wall -o instrument.o instrument.cpp 
	$(CPP) -fPIC -shared -Wl,-soname,libinst.so -o libinst.so instrument.o ../../deps/asmjit/build/libasmjit.a

asmjit: inst.so
	$(CPP) -std=c++11 -g -I. -o main main.cpp -L. -linst
	LD_LIBRARY_PATH=.:$(LD_LIBRARY_PATH) ./main
			
all:
	$(CPP) -std=c++11 -g -c -o zca_app.o zca_app.cpp
	$(CPP) -o zca_app zca_app.o -L../../build/lib -lubiprof -lrt -lm -lelf
	LD_LIBRARY_PATH=../../build/lib:$(LD_LIBRARY_PATH) ./zca_app

clean:
	rm -f main zca_app *.out *.so *.o
