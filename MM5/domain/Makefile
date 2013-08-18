# Makefile for directory domain

DEVTOP = ..
include ../configure.user

# Makefile for directory domain

lib:
	@if [ $(MAXNES) -gt 1 ]; then \
	(cd nest; $(MAKE) all); \
	(cd drivers; $(MAKE) all ); fi
	(cd boundary; $(MAKE) all); \
	(cd initial; $(MAKE) all ); \
	(cd io; $(MAKE) all ); \
	(cd util; $(MAKE) all);
code:
	@if [ $(MAXNES) -gt 1 ]; then \
	(cd nest; $(MAKE) code);\
	(cd drivers; $(MAKE) code ); fi
	(cd boundary; $(MAKE) code); \
	(cd initial; $(MAKE) code ); \
	(cd io; $(MAKE) code ); \
	(cd util; $(MAKE) code);

little_f:
	@if [ $(MAXNES) -gt 1 ]; then \
	(cd nest; $(MAKE) little_f);\
	(cd drivers; $(MAKE) little_f ); fi
	(cd boundary; $(MAKE) little_f); \
	(cd initial; $(MAKE) little_f ); \
	(cd io; $(MAKE) little_f ); \
	(cd util; $(MAKE) little_f);

clean:
	(cd boundary; $(MAKE) clean); \
	(cd drivers; $(MAKE) clean); \
	(cd initial; $(MAKE) clean); \
	(cd io; $(MAKE) clean); \
	(cd nest; $(MAKE) clean); \
	(cd util; $(MAKE) clean)



