

default: test

all: install docs test

install:
	(cd src; $(MAKE) install)

test: install 
	(cd example; $(MAKE))

docs:
	doxygen zca-toggle.cfg

# IU specific:
docs-install: docs
	rsync -e ssh -vrplt ./html/ $(shell whoami)@tank.soic.indiana.edu:.hyplan/docs/zcatoggle/

clean:
	rm -rf html/ latex/
	(cd src;     $(MAKE) clean)
	(cd example; $(MAKE) clean)
