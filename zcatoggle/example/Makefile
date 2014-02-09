TOP=..
INTEL_LIB=/opt/intel/composer_xe_2013_sp1.1.106/compiler/lib/intel64/

CC = icc -g 
AR = ar
RM = rm

CCARGS= -I$(TOP)/src/include

.PHONY: all test test1 test2 test3

all:  mainDynamic mainStatic test

mainDynamic: $(TOP)/build/libzca-toggle.so main.c
	$(CC) $(CCARGS) main.c $(TOP)/build/libzca-toggle.so -o mainDynamic

mainStatic: $(TOP)/build/libzca-toggle.a main.c
	$(CC) $(CCARGS) main.c $(TOP)/build/libzca-toggle.a -o mainStatic

# Linux specific:
test: test1 test2 test3
test1:
	@echo
	@echo "Test1: Static executable"
	@echo "================================================================================"
	./mainStatic
test2:
	@echo
	@echo "Test2: Dynamically linked executable"
	@echo "================================================================================"
	LD_LIBRARY_PATH=$(TOP)/build/:$(INTEL_LIB):$(LD_LIBRARY_PATH) ./mainDynamic

test3:
	@echo
	@echo "Test3: LD_PRELOAD method with preexisting binary"
	@echo "================================================================================"
	LD_LIBRARY_PATH=$(INTEL_LIB):$(LD_LIBRARY_PATH) LD_PRELOAD=$(TOP)/build/libzca-toggle.so /bin/ls

clean:
	rm -rf mainDynamic mainStatic
