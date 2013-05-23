



all:
	icc test.cpp -I libzca-src-195/zca/include/ -I 3rdparty/pintool/source/include/pin/ -I 3rdparty/pintool/source/include/pin/gen/ -I 3rdparty/pintool/extras/components/include/util/ -I 3rdparty/pintool/extras/components/include/ -I 3rdparty/pintool/extras/xed2-intel64/include/ -DTARGET_IA32E -DTARGET_LINUX



pin-2.12-58423-gcc.4.4.7-linux.tar.gz:
	wget http://software.intel.com/sites/landingpage/pintool/downloads/pin-2.12-58423-gcc.4.4.7-linux.tar.gz
