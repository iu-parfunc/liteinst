

installLib: 
	(cd ../../src && make install CC=$(CC) CXX=$(CXX) CFLAGS="-DWAIT_SPIN_RDTSC "$(CFLAGS))  

cleanLib: 
	(cd ../../src && make clean)


buildTests: 
	(cd ../../tests && make  CC=$(CC) CXX=$(CXX) CFLAGS=$(CFLAGS))  


cleanTests: 
	(cd ../../tests && make clean)
