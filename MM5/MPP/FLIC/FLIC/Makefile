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
.SUFFIXES: .c .o

SHELL = /bin/sh
#CFLAGS = -w -g $(CC_TRADITIONAL)
# pjj/cray
CFLAGSL = $(CFLAGS) -w -g $(CC_TRADITIONAL)
# pjj/cray for  X1 cross compiler use local cc, set in top Makefile
#CC = cc
INCLDIR = ../h
LIBDIR = ../bin

TAR = dm.c new_module.c new_statement.c read_config.c handle_declarations.c handle_executable.c sym.c my_malloc.c depends_on.c parse_directives.c prback.c dm.h    sym.h   twst.h offset.c flic.csh Makefile


OBJ = dm.o new_module.o new_statement.o read_config.o handle_declarations.o handle_executable.o sym.o my_malloc.o depends_on.o parse_directives.o prback.o offset.o addargs.o

all :
	if [ `uname` = 'Linux' ] ; then \
          echo 'Linux' ; \
          make dm CC_TRADITIONAL="" CC=gcc ; \
          make reassemble CC_TRADITIONAL="" CC=gcc ; \
        else \
          echo 'not Linux' ; \
          make dm ; \
          make reassemble ; \
        fi


dm : $(OBJ)
	$(CC) -o dm $(OBJ) $(LIBDIR)/parselib.a

reassemble : reassemble.c
	$(CC) -o reassemble reassemble.c

clean :
	/bin/rm -f *.o

tar :
	tar cvf flic.tar $(TAR)

.c.o :
	$(CC) -c $(CFLAGSL) -I$(INCLDIR) $<
