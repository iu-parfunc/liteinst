



PINVER=pin-2.12-58423-gcc.4.4.7-linux

INCLUDES= -I libzca-src-195/zca/include/ -I 3rdparty/pintool/source/include/pin/ -I 3rdparty/pintool/source/include/pin/gen/ -I 3rdparty/pintool/extras/components/include/util/ -I 3rdparty/pintool/extras/components/include/ -I 3rdparty/pintool/extras/xed2-intel64/include/

DEFS= -DTARGET_IA32E -DTARGET_LINUX -DFUND_TC_HOSTCPU=FUND_CPU_INTEL64


all: 3rdparty/$(PINVER)
#	if [ -d 3rdparty/pin-2.12-58423-gcc.4.4.7-linux ] 
	icc test.cpp $(INCLUDES) $(DEFS)

3rdparty/$(PINVER): $(PINVER).tar.gz
	tar xzvf $(PINVER).tar.gz 
	mv $(PINVER) 3rdparty/

$(PINVER).tar.gz:
	wget http://software.intel.com/sites/landingpage/pintool/downloads/$(PINVER).tar.gz
