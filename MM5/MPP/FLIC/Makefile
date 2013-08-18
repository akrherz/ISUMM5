.SUFFIXES: .o .c

TARCOMMAND= tar cf
UNTARCOMMAND= tar xf
COMPRESS= gzip
UNCOMPRESS= gunzip

all : bin things

things : bin/n32.scanner bin/n32.parser h/n32.h

bin/n32.scanner : h/n32.h
	cd Scanner ; make all

bin/n32.parser : bin/n32.scanner h/n32.h
	cd Parser ; make all

h/n32.h :
	cd h ; make all

bin :
	mkdir bin

clean :
	cd Parser ; make clean
	cd Scanner; make clean
	cd h ; make clean

tar :
	cd Parser ; make tar  TARCOMMAND="$(TARCOMMAND)" COMPRESS=$(COMPRESS)
	cd Scanner ; make tar  TARCOMMAND="$(TARCOMMAND)" COMPRESS=$(COMPRESS)
	cd h ; make tar  TARCOMMAND="$(TARCOMMAND)"
	tar cf  n32dist.tar Makefile ./Parser/parser.tar.* \
		./Scanner/scanner.tar.* ./h/headers.tar

untar :
	cd Parser ; $(UNCOMPRESS) parser.tar ; $(UNTARCOMMAND) parser.tar
	cd Scanner ; $(UNCOMPRESS) scanner.tar ; $(UNTARCOMMAND) scanner.tar
	cd h ; $(UNTARCOMMAND) headers.tar

