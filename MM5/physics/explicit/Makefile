# Makefile for directory physics/explicit

DEVTOP = ../..
include ../../configure.user

lib:
	@tmpfile='.tmpfile'; \
	echo $(IMPHYS) > $$tmpfile; \
        $(GREP) "2" $$tmpfile; \
        if [ $$? = 0 ]; then \
                echo "IMPHYS = 2"; \
		(cd nonconv; $(MAKE) all); \
        else \
                echo "IMPHYS != 2"; \
        fi; \
	$(GREP) "3" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IMPHYS = 3"; \
		if [ $(MPHYSTBL) = 0 ]; then (cd simple; $(MAKE) all); \
                fi; \
		if [ $(MPHYSTBL) = 2 ]; then (cd simple; $(MAKE) all); \
                fi; \
		if [ $(MPHYSTBL) = 1 ]; then \
		echo "use look up table version of exmoiss"; \
		(cd simple; $(MAKE) lex); \
		(cd shared; $(MAKE) table); fi; \
	else \
		echo "IMPHYS != 3"; \
	fi; \
	$(GREP) "4" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IMPHYS = 4"; \
		if [ $(MPHYSTBL) = 0 ]; then (cd simple; $(MAKE) all); \
		fi; \
		if [ $(MPHYSTBL) = 2 ]; then (cd simple; $(MAKE) all); \
		fi; \
		if [ $(MPHYSTBL) = 1 ]; then \
		echo "use look up table version of exmoiss"; \
		(cd simple; $(MAKE) lex); \
		(cd shared; $(MAKE) table); fi; \
	else \
		echo "IMPHYS != 4"; \
	fi; \
	$(GREP) "5" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IMPHYS = 5"; \
		if [ $(MPHYSTBL) = 0 ]; then (cd reisner1; $(MAKE) all); \
                fi;\
		if [ $(MPHYSTBL) = 2 ]; then (cd reisner1; $(MAKE) zex); \
                fi;\
		if [ $(MPHYSTBL) = 1 ]; then  \
		echo "use look up table version of exmoisr"; \
		(cd reisner1; $(MAKE) lex); \
		(cd shared; $(MAKE) table); \
                fi; \
	else \
		echo "IMPHYS != 5"; \
	fi; \
	$(GREP) "6" $$tmpfile; \
	if [ $$? = 0 ]; then echo "IMPHYS = 6"; \
		(cd gsfc; $(MAKE) all); \
	else \
		echo "IMPHYS != 6"; \
	fi; \
	$(GREP) "7" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IMPHYS = 7"; \
		(cd reisner2; $(MAKE) all); \
	else \
		echo "IMPHYS != 7"; \
	fi; \
	$(GREP) "8" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IMPHYS = 8"; \
		(cd schultz; $(MAKE) all); \
	else \
		echo "IMPHYS != 8"; \
	fi; \
	(cd shared; $(MAKE) all);

code:
	@tmpfile='.tmpfile'; \
	echo $(IMPHYS) > $$tmpfile; \
        $(GREP) "2" $$tmpfile; \
        if [ $$? = 0 ]; then \
                echo "IMPHYS = 2"; \
                (cd nonconv; $(MAKE) code); \
        else \
                echo "IMPHYS != 2"; \
        fi; \
	$(GREP) "3" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IMPHYS = 3"; \
                if [ $(MPHYSTBL) = 0 ]; then (cd simple; $(MAKE) code); \
                fi;  \
                if [ $(MPHYSTBL) = 2 ]; then (cd simple; $(MAKE) code); \
                fi;  \
                if [ $(MPHYSTBL) = 1 ]; then  \
                echo "use look up table version of exmoiss"; \
                (cd simple; $(MAKE) lex_code); \
                (cd shared; $(MAKE) code); fi; \
	else \
		echo "IMPHYS != 3"; \
	fi; \
	$(GREP) "4" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IMPHYS = 4"; \
                if [ $(MPHYSTBL) = 0 ]; then (cd simple; $(MAKE) code); \
                fi;  \
                if [ $(MPHYSTBL) = 2 ]; then (cd simple; $(MAKE) code); \
                fi;  \
                if [ $(MPHYSTBL) = 1 ]; then  \
                echo "use look up table version of exmoiss"; \
                (cd simple; $(MAKE) lex_code); \
                (cd shared; $(MAKE) code); fi; \
	else \
		echo "IMPHYS != 4"; \
	fi; \
	$(GREP) "5" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IMPHYS = 5"; \
		if [ $(MPHYSTBL) = 0 ]; then (cd reisner1; $(MAKE) code); \
                fi;\
		if [ $(MPHYSTBL) = 2 ]; then (cd reisner1; $(MAKE) zex_code); \
                fi;\
		if [ $(MPHYSTBL) = 1 ]; then  \
                echo "use look up table version of exmoisr"; \
                (cd reisner1; $(MAKE) lex_code); \
                (cd shared; $(MAKE) code); fi; \
	else \
		echo "IMPHYS != 5"; \
	fi; \
	$(GREP) "6" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IMPHYS = 6"; \
		(cd gsfc; $(MAKE) code); \
	else \
		echo "IMPHYS != 6"; \
	fi; \
	$(GREP) "7" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IMPHYS = 7"; \
		(cd reisner2; $(MAKE) code); \
	else \
		echo "IMPHYS != 7"; \
	fi; \
	$(GREP) "8" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IMPHYS = 8"; \
		(cd schultz; $(MAKE) code); \
	else \
		echo "IMPHYS != 8"; \
	fi; \
	(cd shared; $(MAKE) code);


little_f:
	@tmpfile='.tmpfile'; \
	echo $(IMPHYS) > $$tmpfile; \
	$(GREP) "3" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IMPHYS = 3"; \
		if [ $(MPHYSTBL) = 0 ]; then (cd simple; $(MAKE) little_f); \
		fi;  \
		if [ $(MPHYSTBL) = 2 ]; then (cd simple; $(MAKE) little_f); \
		fi;  \
		if [ $(MPHYSTBL) = 1 ]; then \
		echo "use look up table version of exmoiss"; \
		(cd simple; $(MAKE) lex_little_f); \
		(cd shared; $(MAKE) table_little_f); fi; \
	else \
		echo "IMPHYS != 3"; \
	fi; \
	$(GREP) "4" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IMPHYS = 4"; \
		if [ $(MPHYSTBL) = 0 ]; then (cd simple; $(MAKE) little_f); \
		fi;  \
		if [ $(MPHYSTBL) = 2 ]; then (cd simple; $(MAKE) little_f); \
		fi;  \
		if [ $(MPHYSTBL) = 1 ]; then \
		echo "use look up table version of exmoiss"; \
		(cd simple; $(MAKE) lex_little_f); \
		(cd shared; $(MAKE) table_little_f); fi; \
	else \
		echo "IMPHYS != 4"; \
	fi; \
	$(GREP) "5" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IMPHYS = 5"; \
		if [ $(MPHYSTBL) = 0 ]; then (cd reisner1; $(MAKE) littte_f); \
                fi;\
		if [ $(MPHYSTBL) = 2 ]; then (cd reisner1; $(MAKE) zex_littte_f); \
                fi;\
		if [ $(MPHYSTBL) = 1 ]; then  \
		echo "use look up table version of exmoisr"; \
		(cd reisner1; $(MAKE) lex_little_f); \
		(cd shared; $(MAKE) table_little_f); fi; \
	else \
		echo "IMPHYS != 5"; \
	fi; \
	$(GREP) "6" $$tmpfile; \
	if [ $$? = 0 ]; then echo "IMPHYS = 6"; \
		(cd gsfc; $(MAKE) little_f); \
	else \
		echo "IMPHYS != 6"; \
	fi; \
	$(GREP) "7" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IMPHYS = 7"; \
		(cd reisner2; $(MAKE) little_f); \
	else \
		echo "IMPHYS != 7"; \
	fi; \
	$(GREP) "8" $$tmpfile; \
	if [ $$? = 0 ]; then \
		echo "IMPHYS = 8"; \
		(cd schultz; $(MAKE) little_f); \
	else \
		echo "IMPHYS != 8"; \
	fi; \
	(cd shared; $(MAKE) little_f);

clean:
	(cd nonconv; $(MAKE) clean); \
	(cd simple; $(MAKE) clean); \
	(cd reisner1; $(MAKE) clean); \
	(cd gsfc; $(MAKE) clean); \
	(cd shared; $(MAKE) clean); \
	(cd reisner2; $(MAKE) clean); \
	(cd schultz; $(MAKE) clean);

# DO NOT DELETE THIS LINE -- make depend depends on it.
