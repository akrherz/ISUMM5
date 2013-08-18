# Makefile for directory physics/radiation

DEVTOP = ../..
include ../../configure.user

lib:
	@tmpfile='.tmpfile'; \
	echo $(FRAD) > $$tmpfile; \
	$(GREP) "1" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "FRAD = 1"; \
		(cd simple; $(MAKE) all); \
	else \
		echo "FRAD != 1"; \
	fi; \
	$(GREP) "2" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "FRAD = 2"; \
		(cd cloud; $(MAKE) all); \
	else \
		echo "FRAD != 2"; \
	fi; \
	$(GREP) "3" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "FRAD = 3"; \
		(cd ccm2; $(MAKE) all); \
	else \
		echo "FRAD != 3"; \
	fi; \
	$(GREP) "4" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "FRAD = 4"; \
		(cd rrtm; $(MAKE) all); \
		(cd cloud; $(MAKE) all); \
	else \
		echo "FRAD != 4"; \
	fi;
	@(cd sfc; $(MAKE) all);
	@(cd util; $(MAKE) all);

code:
	@tmpfile='.tmpfile'; \
	echo $(FRAD) > $$tmpfile; \
	$(GREP) "1" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "FRAD = 1"; \
		(cd simple; $(MAKE) code); \
	else \
		echo "FRAD != 1"; \
	fi; \
	$(GREP) "2" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "FRAD = 2"; \
		(cd cloud; $(MAKE) code); \
	else \
		echo "FRAD != 2"; \
	fi; \
	$(GREP) "3" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "FRAD = 3"; \
		(cd ccm2; $(MAKE) code); \
	else \
		echo "FRAD != 3"; \
	fi; \
	$(GREP) "4" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "FRAD = 4"; \
		(cd rrtm; $(MAKE) code); \
		(cd cloud; $(MAKE) code); \
	else \
		echo "FRAD != 4"; \
	fi;
	@(cd sfc; $(MAKE) code);
	@(cd util; $(MAKE) code);

little_f:
	@tmpfile='.tmpfile'; \
	echo $(FRAD) > $$tmpfile; \
	$(GREP) "1" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "FRAD = 1"; \
		(cd simple; $(MAKE) little_f); \
	else \
		echo "FRAD != 1"; \
	fi; \
	$(GREP) "2" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "FRAD = 2"; \
		(cd cloud; $(MAKE) little_f); \
	else \
		echo "FRAD != 2"; \
	fi; \
	$(GREP) "3" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "FRAD = 3"; \
		(cd ccm2; $(MAKE) little_f); \
	else \
		echo "FRAD != 3"; \
	fi; \
	$(GREP) "4" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "FRAD = 4"; \
		(cd rrtm; $(MAKE) little_f); \
		(cd cloud; $(MAKE) little_f); \
	else \
		echo "FRAD != 4"; \
	fi;
	@(cd sfc; $(MAKE) little_f);
	@(cd util; $(MAKE) little_f);

clean:
	(cd sfc; $(MAKE) clean); \
	(cd simple; $(MAKE) clean); \
	(cd cloud; $(MAKE) clean); \
	(cd ccm2; $(MAKE) clean); \
	(cd rrtm; $(MAKE) clean); \
	(cd util; $(MAKE) clean);
