


PINVER=pin-2.12-58423-gcc.4.4.7-linux
PIN_TARGET_ARCH = intel64

PINDIR=./3rdparty/$(PINVER)
BUILDDIR=linux64
ZCADIR=./libzca-src-195/zca

INCLUDES= -I libzca-src-195/zca/include/ -I 3rdparty/pintool/source/include/pin/ -I 3rdparty/pintool/source/include/pin/gen/ -I 3rdparty/pintool/extras/components/include/util/ -I 3rdparty/pintool/extras/components/include/ -I 3rdparty/pintool/extras/xed2-intel64/include/

DEFS= -DTARGET_IA32E -DTARGET_LINUX -DFUND_TC_HOSTCPU=FUND_CPU_INTEL64

LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared
LDPATHS  = -L $(PINDIR)/$(PIN_TARGET_ARCH)/lib
# LDPATHS += -L $(PINDIR)/$(PIN_TARGET_ARCH)/lib-ext
LDPATHS += -L $(PINDIR)/extras/xed2-$(PIN_TARGET_ARCH)/lib
LDPATHS += -L $(ZCADIR)/src/$(BUILDDIR)
LDLIBS = ./3rdparty/pin-2.12-58423-gcc.4.4.7-linux/intel64/lib/libpin.a -lxed -ldwarf -lelf -ldl -lpthread -lzca
#LDLIBS = -lpin -lxed -ldwarf ./3rdparty/pin-2.12-58423-gcc.4.4.7-linux/intel64/runtime/libelf.so.0 -ldl -lpthread -lzca

# Uh, doing Make's job for it a bit here.  Setting
# "3rdparty/$(PINVER)" as a dependency didn't work.
all: test test3

test:
	if ! [ -d $(PINDIR) ]; then $(MAKE) $(PINDIR); fi
	icc test.cpp $(INCLUDES) $(DEFS) -o test.exe

test3:
	if ! [ -d $(PINDIR) ]; then $(MAKE) $(PINDIR); fi
	icc test3_print_own_zca.cpp $(INCLUDES) $(DEFS) $(LDPATHS) $(LDLIBS) -o test3.exe

test4:
	if ! [ -d $(PINDIR) ]; then $(MAKE) $(PINDIR); fi
	icc -g -IAsmJit-1.0-beta4/ AsmJit-1.0-beta4/AsmJit/*.cpp -O0 test4_libelf.cpp $(INCLUDES) $(DEFS) -lelf -o test4.exe

test5:
	if ! [ -d $(PINDIR) ]; then $(MAKE) $(PINDIR); fi
	icc -g -O0 test5.cpp $(INCLUDES) $(DEFS) -lelf -o test5.exe

tool:
	icc test_pin.cpp $(INCLUDES) $(DEFS) $(LDFLAGS) $(LDPATHS) $(LDLIBS) -o tool.so

run:
	./3rdparty/pintool/pin -t tool.so -- ./test.exe

3rdparty/$(PINVER): $(PINVER).tar.gz
	tar xzvf $(PINVER).tar.gz 
	mv $(PINVER) 3rdparty/

$(PINVER).tar.gz:
	wget http://software.intel.com/sites/landingpage/pintool/downloads/$(PINVER).tar.gz

#-----------------------------------------
# Dyninst:

dyninst:
	gcc mutator.cpp -std=c++0x -ldyninstAPI

#-----------------------------------------

pin-example:
#	source install_env_vars.sh
	(cd 3rdparty/pintool/source/tools/Insmix/; $(MAKE))
	(cd 3rdparty/pintool; ./pin -t source/tools/Insmix/obj-intel64/insmix.so -- /bin/ls)
	head 3rdparty/pintool/insmix.out

clean:
	rm -f *.o *.exe

# Example compile and link from InsMix:
# g++ -DBIGARRAY_MULTIPLIER=1 -DUSING_XED -Wall -Werror -Wno-unknown-pragmas -fno-stack-protector -DTARGET_IA32E -DHOST_IA32E -fPIC -DTARGET_LINUX  -I../../../source/include/pin -I../../../source/include/pin/gen -I../../../extras/components/include -I../../../extras/xed2-intel64/include -I../../../source/tools/InstLib -O3 -fomit-frame-pointer -fno-strict-aliasing   -c -o obj-intel64/insmix.o insmix.cpp
# g++ -shared -Wl,--hash-style=sysv -Wl,-rpath=../../../intel64/runtime/cpplibs -Wl,-Bsymbolic -Wl,--version-script=../../../source/include/pin/pintool.ver    -o obj-intel64/insmix.so obj-intel64/insmix.o  -L../../../intel64/runtime/cpplibs -L../../../intel64/lib -L../../../intel64/lib-ext -L../../../intel64/runtime/glibc -L../../../extras/xed2-intel64/lib -lpin -lxed -ldwarf -lelf -ldl
