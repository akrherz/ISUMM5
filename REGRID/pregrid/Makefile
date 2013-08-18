#	Mid level Makefile for pregrid programs.

include ../macros_pregrid

all:
		( $(CD) util; $(MAKE) all )
		( $(CD) util; $(MAKE) gribprint )
		( $(CD) on84; $(MAKE) all )
		( $(CD) grib.misc; $(MAKE) all )
		( $(CD) navysst; $(MAKE) all )
		( $(CD) ncep.grib; $(MAKE) all )
#KWM		( $(CD) cwbgm; $(MAKE) all )
		( $(CD) nise; $(MAKE) all )

nise:		libpgu.a
		( $(CD) nise; $(MAKE) all )

on84:		libpgu.a
		( $(CD) on84; $(MAKE) all )

grib.misc:	libpgu.a
		( $(CD) grib.misc; $(MAKE) all )

navysst:	libpgu.a
		( $(CD) navysst; $(MAKE) all )

ncep.grib:	libpgu.a
		( $(CD) ncep.grib; $(MAKE) all )

#KWMcwbgm:		libpgu.a
#KWM		( $(CD) ncep.grib; $(MAKE) all )

libpgu.a:
		( $(CD) util; $(MAKE) all )
clean:
		( $(CD) util; $(MAKE) clean )
		( $(CD) on84; $(MAKE) clean )
		( $(CD) grib.misc; $(MAKE) clean )
		( $(CD) ncep.grib; $(MAKE) clean )
		( $(CD) navysst; $(MAKE) clean )
		( $(CD) nise; $(MAKE) clean )
