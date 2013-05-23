



PINVER=pin-2.12-58423-gcc.4.4.7-linux

INCLUDES= -I libzca-src-195/zca/include/ -I 3rdparty/pintool/source/include/pin/ -I 3rdparty/pintool/source/include/pin/gen/ -I 3rdparty/pintool/extras/components/include/util/ -I 3rdparty/pintool/extras/components/include/ -I 3rdparty/pintool/extras/xed2-intel64/include/

DEFS= -DTARGET_IA32E -DTARGET_LINUX -DFUND_TC_HOSTCPU=FUND_CPU_INTEL64


# Uh, doing Make's job for it a bit here.  Setting
# "3rdparty/$(PINVER)" as a dependency didn't work.
all: 
	if ! [ -d 3rdparty/$(PINVER) ]; then $(MAKE) 3rdparty/$(PINVER); fi
	icc test.cpp $(INCLUDES) $(DEFS)

3rdparty/$(PINVER): $(PINVER).tar.gz
	tar xzvf $(PINVER).tar.gz 
	mv $(PINVER) 3rdparty/

$(PINVER).tar.gz:
	wget http://software.intel.com/sites/landingpage/pintool/downloads/$(PINVER).tar.gz


#-----------------------------------------

pin-example:
#	source install_env_vars.sh
	(cd 3rdparty/pintool/source/tools/Insmix/; $(MAKE))
	(cd 3rdparty/pintool; ./pin -t source/tools/Insmix/obj-intel64/insmix.so -- /bin/ls)
	head 3rdparty/pintool/insmix.out
