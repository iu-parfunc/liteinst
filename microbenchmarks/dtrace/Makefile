.PHONY: all build run1 run2 build2 clean

all: build build2

build:
	$(CC) -o looper.exe looper.c

run1:
	sudo time dtrace -n 'pid$target:a.out:func:entry { @[copyinstr(arg0)] = count(); }' -c ./looper.exe

#--------------------------------------------------------------------------------
# This one worked on Ryan's macbook:
run2:
	sudo time dtrace -n 'pid$$target::func:entry { @ = count(); }' -c ./looper.exe

# This produces this output:
# dtrace: description 'pid$target::func:entry ' matched 1 probe
# dtrace: pid 12698 has exited
#         100,000,000
#        46.54 real        21.35 user        24.95 sys
#
# Or about 465 nanoseconds per loop iteration.
#--------------------------------------------------------------------------------

build2: raw_libtrace.exe
# For Mac OS:
#  elf:  brew install libelf
#
raw_libtrace.exe:
	cc raw_libtrace.c -o raw_libtrace.exe -l dtrace
#	cc \
         -I /usr/src/cddl/compat/opensolaris/include \
         -I /usr/src/cddl/contrib/opensolaris/lib/libdtrace/common/ \
         -I /usr/src/sys/cddl/compat/opensolaris \
         -I /usr/src/sys/cddl/contrib/opensolaris/uts/common/ \
         raw_libtrace.c \
         -l dtrace -l proc -l elf -l z -l rtld_db -l pthread -l util \
         -o raw_libtrace.exe
# What is: -l ctf

clean:
		rm -f *.exe
