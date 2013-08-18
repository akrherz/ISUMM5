.SUFFIXES: .o .c

INCLUDES = defines.h h.h
SRC = error.c lexin.c ioque.c drive.c scanformat.c
OBJ = error.o lexin.o printtok.o card.o ioque.o scanformat.o
LIB =
BIN = ../bin
# pjj/cray X1 cross compiler CC set in top Makefile
#CC = cc

all : drive

drive : drive.o $(OBJ)
	/bin/rm -f drive
	$(CC) $(FLAGS) -o drive drive.o $(OBJ) $(LIB)
	/bin/mv drive $(BIN)/n32.scanner
	touch drive

dr : dr.o error.o card.o ioque.o
	$(CC) $(FLAGS) -o dr dr.o error.o card.o ioque.o $(LIB)

clean:
	/bin/rm -f *.o drive scanner.tar*

.c.o : $(INCLUDES)
	$(CC) $(FLAGS) -c $<

###

drive.o : tokens.h defines.h

lexin.o : tokens.h

card.o : card.c defines.h

printtok.o : printtok.c

error.o : error.c
