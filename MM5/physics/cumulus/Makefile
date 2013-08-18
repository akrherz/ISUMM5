# Makefile for directory physics/cumulus
# Dan L. Hansen, NCAR 1996

DEVTOP = ../..
include ../../configure.user

# Makefile for directory physics/cumulus

lib:
	@tmpfile='.tmpfile'; \
	echo $(ICUPA) > $$tmpfile; \
	$(GREP) "2" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 2"; \
		(cd kuo; $(MAKE) all); \
	else \
		echo "ICUPA != 2"; \
	fi; \
	$(GREP) "3" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 3"; \
		(cd grell; $(MAKE) all); \
	else \
		echo "ICUPA != 3"; \
	fi; \
	$(GREP) "4" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 4"; \
		(cd as; $(MAKE) all); \
		(cd shared; $(MAKE) as_shallow); \
	else \
		echo "ICUPA != 4"; \
	fi; \
	$(GREP) "5" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 5"; \
		(cd fc; $(MAKE) all); \
		(cd ../pbl_sfc/dry; $(MAKE) all); \
	else \
		echo "ICUPA != 5"; \
	fi; \
	$(GREP) "6" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 6"; \
		(cd kf; $(MAKE) all); \
		(cd ../pbl_sfc/dry; $(MAKE) all); \
		(cd shared; $(MAKE) kf_bm; $(MAKE) kf); \
	else \
		echo "ICUPA != 6"; \
	fi; \
	$(GREP) "7" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 7"; \
		(cd bm; $(MAKE) all); \
		(cd shared; $(MAKE) kf_bm); \
	else \
		echo "ICUPA != 7"; \
	fi; \
	$(GREP) "8" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 8"; \
		(cd kf2; $(MAKE) all); \
		(cd ../pbl_sfc/dry; $(MAKE) all); \
		(cd shared; $(MAKE) kf_bm; $(MAKE) kf); \
	else \
		echo "ICUPA != 8"; \
	fi;
	@tmpfile='.tmpfile'; \
	echo $(ISHALLO) > $$tmpfile; \
	$(GREP) "1" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ISHALLO = 1"; \
		(cd shallow; $(MAKE) ishallo1); \
		(cd shared; $(MAKE) as_shallow); \
	else \
		echo "ISHALLO != 1"; \
	fi;

code:
	@tmpfile='.tmpfile'; \
	echo $(ICUPA) > $$tmpfile; \
	$(GREP) "2" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 2"; \
		(cd kuo; $(MAKE) code); \
	else \
		echo "ICUPA != 2"; \
	fi; \
	$(GREP) "3" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 3"; \
		(cd grell; $(MAKE) code); \
	else \
		echo "ICUPA != 3"; \
	fi; \
	$(GREP) "4" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 4"; \
		(cd as; $(MAKE) code); \
		(cd shared; $(MAKE) as_shallow_code); \
	else \
		echo "ICUPA != 4"; \
	fi; \
	$(GREP) "5" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 5"; \
		(cd fc; $(MAKE) code); \
		(cd ../pbl_sfc/dry; $(MAKE) code); \
	else \
		echo "ICUPA != 5"; \
	fi; \
	$(GREP) "6" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 6"; \
		(cd kf; $(MAKE) code); \
		(cd ../pbl_sfc/dry; $(MAKE) code); \
		(cd shared; $(MAKE) kf_bm_code; $(MAKE) kf_code); \
	else \
		echo "ICUPA != 6"; \
	fi; \
	$(GREP) "7" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 7"; \
		(cd bm; $(MAKE) code); \
		(cd shared; $(MAKE) kf_bm_code); \
	else \
		echo "ICUPA != 7"; \
	fi; \
	$(GREP) "8" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 8"; \
		(cd kf2; $(MAKE) code); \
		(cd ../pbl_sfc/dry; $(MAKE) code); \
		(cd shared; $(MAKE) kf_bm_code; $(MAKE) kf_code); \
	else \
		echo "ICUPA != 8"; \
	fi; 
	@tmpfile='.tmpfile'; \
	echo $(ISHALLO) > $$tmpfile; \
	$(GREP) "1" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ISHALLO = 1"; \
		(cd shallow; $(MAKE) ishallo1_code); \
		(cd shared; $(MAKE) as_shallow_code); \
	else \
		echo "ISHALLO != 1"; \
	fi;

little_f:
	@tmpfile='.tmpfile'; \
	echo $(ICUPA) > $$tmpfile; \
	$(GREP) "2" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 2"; \
		(cd kuo; $(MAKE) little_f); \
	else \
		echo "ICUPA != 2"; \
	fi; \
	$(GREP) "3" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 3"; \
		(cd grell; $(MAKE) little_f); \
	else \
		echo "ICUPA != 3"; \
	fi; \
	$(GREP) "4" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 4"; \
		(cd as; $(MAKE) little_f); \
		(cd shared; $(MAKE) as_shallow_little_f); \
	else \
		echo "ICUPA != 4"; \
	fi; \
	$(GREP) "5" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 5"; \
		(cd fc; $(MAKE) little_f); \
		(cd ../pbl_sfc/dry; $(MAKE) little_f); \
	else \
		echo "ICUPA != 5"; \
	fi; \
	$(GREP) "6" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 6"; \
		(cd kf; $(MAKE) little_f); \
		(cd ../pbl_sfc/dry; $(MAKE) little_f); \
		(cd shared; $(MAKE) kf_bm_little_f; $(MAKE) kf_little_f); \
	else \
		echo "ICUPA != 6"; \
	fi; \
	$(GREP) "7" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 7"; \
		(cd bm; $(MAKE) little_f); \
		(cd shared; $(MAKE) kf_bm_little_f); \
	else \
		echo "ICUPA != 7"; \
	fi; \
	$(GREP) "8" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ICUPA = 8"; \
		(cd kf2; $(MAKE) little_f); \
		(cd ../pbl_sfc/dry; $(MAKE) little_f); \
		(cd shared; $(MAKE) kf_bm_little_f; $(MAKE) kf_little_f); \
	else \
		echo "ICUPA != 8"; \
	fi; 
	@tmpfile='.tmpfile'; \
	echo $(ISHALLO) > $$tmpfile; \
	$(GREP) "1" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ISHALLO = 1"; \
		(cd shallow; $(MAKE) ishallo1_little_f); \
		(cd shared; $(MAKE) as_shallow_little_f); \
	else \
		echo "ISHALLO != 1"; \
	fi;


clean:
	(cd as; $(MAKE) clean); \
	(cd fc; $(MAKE) clean); \
	(cd kf; $(MAKE) clean); \
	(cd kf2; $(MAKE) clean); \
	(cd bm; $(MAKE) clean); \
	(cd grell; $(MAKE) clean); \
	(cd kuo; $(MAKE) clean); \
	(cd shared; $(MAKE) clean); \
	(cd shallow; $(MAKE) clean);
