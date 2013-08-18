########################################################################
#     
#                              COPYRIGHT
#     
#     The following is a notice of limited availability of the code and 
#     Government license and disclaimer which must be included in the 
#     prologue of the code and in all source listings of the code.
#     
#     Copyright notice
#       (c) 1977  University of Chicago
#     
#     Permission is hereby granted to use, reproduce, prepare 
#     derivative works, and to redistribute to others at no charge.  If 
#     you distribute a copy or copies of the Software, or you modify a 
#     copy or copies of the Software or any portion of it, thus forming 
#     a work based on the Software and make and/or distribute copies of 
#     such work, you must meet the following conditions:
#     
#          a) If you make a copy of the Software (modified or verbatim) 
#             it must include the copyright notice and Government       
#             license and disclaimer.
#     
#          b) You must cause the modified Software to carry prominent   
#             notices stating that you changed specified portions of    
#             the Software.
#     
#     This software was authored by:
#     
#     Argonne National Laboratory
#     J. Michalakes: (630) 252-6646; email: michalak@mcs.anl.gov
#     Mathematics and Computer Science Division
#     Argonne National Laboratory, Argonne, IL  60439
#     
#     ARGONNE NATIONAL LABORATORY (ANL), WITH FACILITIES IN THE STATES 
#     OF ILLINOIS AND IDAHO, IS OWNED BY THE UNITED STATES GOVERNMENT, 
#     AND OPERATED BY THE UNIVERSITY OF CHICAGO UNDER PROVISION OF A 
#     CONTRACT WITH THE DEPARTMENT OF ENERGY.
#     
#                      GOVERNMENT LICENSE AND DISCLAIMER
#     
#     This computer code material was prepared, in part, as an account 
#     of work sponsored by an agency of the United States Government.
#     The Government is granted for itself and others acting on its 
#     behalf a paid-up, nonexclusive, irrevocable worldwide license in 
#     this data to reproduce, prepare derivative works, distribute 
#     copies to the public, perform publicly and display publicly, and 
#     to permit others to do so.  NEITHER THE UNITED STATES GOVERNMENT 
#     NOR ANY AGENCY THEREOF, NOR THE UNIVERSITY OF CHICAGO, NOR ANY OF 
#     THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR 
#     ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, 
#     COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, 
#     PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
#     NOT INFRINGE PRIVATELY OWNED RIGHTS.
#
############################################################################
.SUFFIXES: .o .c

# Uncomment this on machines that need ranlib
#RANLIB=ranlib

SHELL=/bin/sh
BIN= ../bin
# -J is needed on Sun3
# pjj/cray - for X1 cross compiler use local cc, set in top Makefile
#CC = cc
CFLAGSL= $(CFLAGS) -g -DYYMAXDEPTH=1500 -DYYDEBUG $(CC_TRADITIONAL)
LDFLAGS= -g
#CFLAGS= -O -J -DYYMAXDEPTH=1500 -DYYDEBUG
#LDFLAGS= -O
THISDIR= .
SCANNERDIR= ../Scanner
SYMTABDIR= .
HDIR= ../h
INCLUDES= -I$(THISDIR) -I$(SCANNERDIR) -I$(HDIR)
TOKENS=$(SCANNERDIR)/tokens.h
SEMANTICSHEADS=\
	f_definitions.h  f_expressions.h  f_main.h \
	f_executables.h  f_format.h
OBJ = parser.o y.tab.o scanner.o yyerror.o \
      pushback.o symbol_table.o symtab_gen.o \
      f_main.o f_executables.o f_definitions.o f_expressions.o \
      f_format.o node.o \
      printtree.o minor.o \
      mymalloc.o type.o traverse.o intrinsics.o \
      dflt_backend.o dflt_main.o printhelp.o user_help.o fix_backptrs.o \
      edface.o getstat.o ifold.o # prback.o
TAR= Makefile dflt_backend.c dflt_main.c drive edface.c edt.h \
     f_definitions.c f_definitions.h f_executables.c f_executables.h \
     f_expressions.c f_expressions.h f_format.c f_format.h f_main.c \
     f_main.h f_y.h fix_backptrs.c getstat.c ifold.c intrinsics.c macros.h \
     minor.c mymalloc.c n32 n32.h node.c node.h noderoutines parselib.a \
     parser.c printhelp.c printtree.c pushback.c pushback.h scanner.c \
     symbol_table.c symtab_gen.c traverse.c type.c type_rules \
     user_help.c y.tab.c y.tab.h yyerror.c

main :
	if [ `uname` = 'Linux' ] ; then \
          echo 'Linux' ; \
	  make it CC_TRADITIONAL="" CC=gcc ; \
	else \
          echo 'not Linux' ; \
	  make it ; \
	fi

it : parselib.a

all : drive parselib.a n32_t
	echo MPP_TARGET $(MPP_TARGET)

n32_t :
	cp n32 $(BIN)/n32
	chmod +wx $(BIN)/n32

# use default main for drive (4-24-90)
# for sequent compiles
#drive : parselib.a
# for sun compiles
drive : parselib.a
	/bin/rm -f drive
	$(CC) $(LDFLAGS) -o drive $(BIN)/parselib.a
	/bin/mv drive $(BIN)/n32.parser
	touch drive

parselib.a : $(OBJ)
	/bin/rm -f parselib.a
	ar ru parselib.a $(OBJ)
	if [ $(RANLIB) ] ; then $(RANLIB) parselib.a ; fi
	/bin/mv parselib.a $(BIN)/parselib.a
	touch parselib.a

tags :
	ctags *.c

symtab_gen.o : $(SYMTABDIR)/symtab_gen.c
	$(CC) $(CFLAGSL) $(INCLUDES) -c $(SYMTABDIR)/symtab_gen.c

clean :
	/bin/rm -f *.o  parser.tar*

tar :
	$(TARCOMMAND) parser.tar $(TAR)
	$(COMPRESS) parser.tar

.c.o  :
	$(CC) $(CFLAGSL) $(INCLUDES) -c $<

###

dflt_main.o : pushback.h node.h

parser.o : pushback.h node.h

scanner.o : $(SCANNERDIR)/defines.h \
	    y.tab.h node.h pushback.h

minor.o : minor.c

symbol_table.o : node.h 

pushback.o : pushback.h

#next line for sequent compiles
#y.tab.o : y.tab.h node.h

yyerror.o :  y.tab.h node.h

f_main.o : node.h f_y.h

f_executables.o : node.h f_y.h

f_definitions.o : node.h f_y.h $(TOKENS)

f_format.o : node.h f_y.h

node.o : node.h $(TOKENS)

printtree.o : node.h $(TOKENS) macros.h

type.o : node.h $(TOKENS)

traverse.o : node.h $(TOKENS)

intrinsics.o : node.h
