
ZCA_BUILD := $(firstword $(subst +, ,$(shell hg id --num)))
KIT_DIR = libzca-src-$(ZCA_BUILD)
KIT = $(KIT_DIR).tgz

ZCA_FILES = include/zca.h include/zca_table.h src/api.cpp src/api.h \
	src/dwarf-to-pin-reg.cpp src/get-image-offset.cpp \
	src/get-image-offset.h src/Makefile src/metadata.cpp \
	src/metadata.h src/spawn-depth.cpp src/zca_table.cpp README \
	pin_api.odt

CILKPROF_FILES = cilkprof.cpp log.h Makefile sprof.h util.cpp util.h README
CILKPROF_FILES += csv2xls.py

INPUT_FILES = Makefile CHANGES LICENSE $(addprefix zca/,$(ZCA_FILES)) \
	$(addprefix cilkprof/,$(CILKPROF_FILES))

.PHONY: all
all:
	make -C zca/src
	make -C cilkprof

.PHONY: kit
kit: $(KIT)

.PHONY: clean
clean:
	make -C zca/src clean
	make -C cilkprof clean
	rm -f libzca-src-*.tgz
	rm -rf libzca-src-*

$(KIT_DIR):
	mkdir $(KIT_DIR)

$(KIT): $(INPUT_FILES) $(KIT_DIR)
	tar -cf - $(INPUT_FILES) | (cd $(KIT_DIR); tar xfp -)
	tar -czf $(KIT) $(KIT_DIR)
