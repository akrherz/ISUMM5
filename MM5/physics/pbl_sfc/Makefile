# Makefile for directory physics/pbl_sfc

DEVTOP = ../..
include ../../configure.user

# Makefile for directory physics/pbl_sfc

lib:
	@tmpfile='.tmpfile'; \
	echo $(IBLTYP) > $$tmpfile; \
	$(GREP) "0" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 0"; \
		(cd dry; $(MAKE) all); \
	else \
		echo "IBLTYP != 0"; \
	fi; \
	$(GREP) "1" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 1"; \
		(cd bulk; $(MAKE) all); \
		(cd dry; $(MAKE) all); \
	else \
		echo "IBLTYP != 1"; \
	fi; \
	$(GREP) "2" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 2"; \
		(cd hirpbl; $(MAKE) all); \
	else \
		echo "IBLTYP != 2"; \
	fi; \
	$(GREP) "3" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 3"; \
		(cd btpbl; $(MAKE) all); \
	else \
		echo "IBLTYP != 3"; \
	fi; \
	$(GREP) "4" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 4"; \
		(cd myepbl; $(MAKE) all); \
	else \
		echo "IBLTYP != 4"; \
	fi; \
	$(GREP) "5" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 5"; \
		(cd mrfpbl; $(MAKE) all); \
	else \
		echo "IBLTYP != 5"; \
	fi; \
	$(GREP) "6" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 6"; \
		(cd gspbl; $(MAKE) all); \
	else \
		echo "IBLTYP != 6"; \
	fi; \
	$(GREP) "7" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 7"; \
		(cd pxpbl; $(MAKE) all); \
	else \
		echo "IBLTYP != 7"; \
	fi; \
	echo $(ISOIL) > $$tmpfile; \
	$(GREP) "2" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ISOIL = 2"; \
		(cd noahlsm; $(MAKE) all); \
	else \
		echo "ISOIL != 2"; \
	fi; 
	@(cd util; $(MAKE) all);

code:
	@tmpfile='.tmpfile'; \
	echo $(IBLTYP) > $$tmpfile; \
	$(GREP) "1" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 1"; \
		(cd bulk; $(MAKE) code); \
		(cd dry; $(MAKE) code); \
	else \
		echo "IBLTYP != 1"; \
	fi; \
	$(GREP) "0" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 0"; \
		(cd dry; $(MAKE) code); \
	else \
		echo "IBLTYP != 0"; \
	fi; \
	$(GREP) "2" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 2"; \
		(cd hirpbl; $(MAKE) code); \
	else \
		echo "IBLTYP != 2"; \
	fi; \
	$(GREP) "3" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 3"; \
		(cd btpbl; $(MAKE) code); \
	else \
		echo "IBLTYP != 3"; \
	fi; \
	$(GREP) "4" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 4"; \
		(cd myepbl; $(MAKE) code); \
	else \
		echo "IBLTYP != 4"; \
	fi; \
	$(GREP) "5" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 5"; \
		(cd mrfpbl; $(MAKE) code); \
	else \
		echo "IBLTYP != 5"; \
	fi; \
	$(GREP) "6" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 6"; \
		(cd gspbl; $(MAKE) code); \
	else \
		echo "IBLTYP != 6"; \
	fi; \
	$(GREP) "7" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 7"; \
		(cd pxpbl; $(MAKE) code); \
	else \
		echo "IBLTYP != 7"; \
	fi; \
	echo $(ISOIL) > $$tmpfile; \
	$(GREP) "2" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ISOIL = 2"; \
		(cd noahlsm; $(MAKE) code); \
		(cd util; $(MAKE) code); \
	else \
		echo "ISOIL != 2"; \
	fi; 
	@(cd util; $(MAKE) code);

little_f:
	@tmpfile='.tmpfile'; \
	echo $(IBLTYP) > $$tmpfile; \
	$(GREP) "0" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 0"; \
		(cd dry; $(MAKE) little_f); \
	else \
		echo "IBLTYP != 0"; \
	fi; \
	$(GREP) "1" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 1"; \
		(cd bulk; $(MAKE) little_f); \
		(cd dry; $(MAKE) little_f); \
	else \
		echo "IBLTYP != 1"; \
	fi; \
	$(GREP) "2" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 2"; \
		(cd hirpbl; $(MAKE) little_f); \
	else \
		echo "IBLTYP != 2"; \
	fi; \
	$(GREP) "3" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 3"; \
		(cd btpbl; $(MAKE) little_f); \
	else \
		echo "IBLTYP != 3"; \
	fi; \
	$(GREP) "4" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 4"; \
		(cd myepbl; $(MAKE) little_f); \
	else \
		echo "IBLTYP != 4"; \
	fi; \
	$(GREP) "5" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 5"; \
		(cd mrfpbl; $(MAKE) little_f); \
	else \
		echo "IBLTYP != 5"; \
	fi; \
	$(GREP) "6" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 6"; \
		(cd gspbl; $(MAKE) little_f); \
	else \
		echo "IBLTYP != 6"; \
	fi; \
	$(GREP) "7" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IBLTYP = 7"; \
		(cd pxpbl; $(MAKE) little_f); \
	else \
		echo "IBLTYP != 7"; \
	fi; \
	echo $(ISOIL) > $$tmpfile; \
	$(GREP) "2" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "ISOIL = 2"; \
		(cd noahlsm; $(MAKE) little_f); \
		(cd util; $(MAKE) little_f); \
	else \
		echo "ISOIL != 2"; \
	fi; 
	@(cd util; $(MAKE) little_f);


clean:
	(cd btpbl; $(MAKE) clean); \
	(cd bulk; $(MAKE) clean); \
	(cd dry; $(MAKE) clean); \
	(cd gspbl; $(MAKE) clean); \
	(cd hirpbl; $(MAKE) clean); \
	(cd mrfpbl; $(MAKE) clean); \
	(cd myepbl; $(MAKE) clean); \
	(cd pxpbl; $(MAKE) clean); \
	(cd util; $(MAKE) clean); \
	(cd noahlsm; $(MAKE) clean)
