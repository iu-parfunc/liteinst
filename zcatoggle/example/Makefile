TOP=..

CC = icc 
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
	LD_LIBRARY_PATH=$(TOP)/build/:$(LD_LIBRARY_PATH) ./mainDynamic

test3:
	@echo
	@echo "Test3: LD_PRELOAD method with preexisting binary"
	@echo "================================================================================"
	LD_PRELOAD=$(TOP)/build/libzca-toggle.so ls

clean:
	rm -rf mainDynamic mainStatic
