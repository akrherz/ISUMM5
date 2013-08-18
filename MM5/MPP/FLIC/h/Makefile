# This makefile is designed to be run from the top level makefile that
# installs N32.  If it is run by itself, the environment variable n32home
# must have been set to the home directory of N32 to set up the links
# correctly.
.SUFFIXES: .o .c .h

all :
	/bin/csh buildlinks

clean :

super :
	/bin/rm -f defines.h edt.h f_definitions.h f_executables.h \
		   f_expressions.h f_format.h f_main.h f_y.h h.h \
		   macros.h n32.h node.h pushback.h tokens.h

tar :
	$(TARCOMMAND) headers.tar Makefile buildlinks

