

all: install docs

install:
	(cd src; $(MAKE) install)

test: install docs 
	(cd example; $(MAKE))

docs:
	doxygen zca-toggle.cfg

clean:
	rm -rf html/ latex/
	(cd src;     $(MAKE) clean)
	(cd example; $(MAKE) clean)
