# Makefile for directory fdda

DEVTOP = ..
include ../configure.user
# Makefile for directory fdda
lib:
	@if [ $(FDDAGD) -gt 0 ]; then (cd grid; $(MAKE) all); fi;
	@if [ $(FDDAOBS) -gt 0 ]; then (cd obs; $(MAKE) all); fi;
	@if [ $(FDDAGD) -gt 0 -o $(FDDAOBS) -gt 0 ]; then \
	  (cd util; $(MAKE) all); \
	fi;
code:
	@if [ $(FDDAGD) -gt 0 ]; then (cd grid; $(MAKE) code); fi;
	@if [ $(FDDAOBS) -gt 0 ]; then (cd obs; $(MAKE) code); fi;
	@if [ $(FDDAGD) -gt 0 -o $(FDDAOBS) -gt 0 ]; then \
	  (cd util; $(MAKE) code); \
	fi;
little_f:
	@if [ $(FDDAGD) -gt 0 ]; then (cd grid; $(MAKE) little_f); fi;
	@if [ $(FDDAOBS) -gt 0 ]; then (cd obs; $(MAKE) little_f); fi;
	@if [ $(FDDAGD) -gt 0 -o $(FDDAOBS) -gt 0 ]; then \
	  (cd util; $(MAKE) little_f); \
	fi;
clean:
	(cd grid; $(MAKE) clean); \
	(cd obs; $(MAKE) clean); \
	(cd util; $(MAKE) clean)
