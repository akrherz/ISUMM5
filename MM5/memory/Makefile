# Makefile for directory memory

DEVTOP = ..
include ../configure.user

lib:
	@if [ $(MAXNES) -gt 1 ]; then (cd address; $(MAKE) maxnes);\
	else (cd address; $(MAKE) all); fi
code:
	@if [ $(MAXNES) -gt 1 ]; then (cd address; $(MAKE) maxnes_code);\
	else (cd address; $(MAKE) code); fi
little_f:
	@if [ $(MAXNES) -gt 1 ]; then (cd address; $(MAKE) maxnes_little_f);\
	else (cd address; $(MAKE) little_f); fi
clean::
	(cd address; $(MAKE) clean)
