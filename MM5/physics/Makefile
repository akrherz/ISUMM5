# Makefile for directory physics

DEVTOP = ..
include ../configure.user

lib:
	(cd advection; $(MAKE) lib ); \
	(cd explicit; $(MAKE) lib ); \
	(cd pbl_sfc; $(MAKE) lib ); \
	(cd cumulus; $(MAKE) lib ); \
	(cd hzdiffu; $(MAKE) lib ); \
	(cd radiation; $(MAKE) lib )

code:
	(cd advection; $(MAKE) code ); \
	(cd explicit; $(MAKE) code ); \
	(cd pbl_sfc; $(MAKE) code ); \
	(cd cumulus; $(MAKE) code ); \
	(cd hzdiffu; $(MAKE) code ); \
	(cd radiation; $(MAKE) code )


little_f:
	(cd advection; $(MAKE) little_f ); \
	(cd explicit; $(MAKE) little_f ); \
	(cd pbl_sfc; $(MAKE) little_f ); \
	(cd cumulus; $(MAKE) little_f ); \
	(cd hzdiffu; $(MAKE) little_f ); \
	(cd radiation; $(MAKE) little_f )

clean:
	(cd advection; $(MAKE) clean); \
	(cd explicit; $(MAKE) clean); \
	(cd pbl_sfc; $(MAKE) clean); \
	(cd cumulus; $(MAKE) clean); \
	(cd hzdiffu; $(MAKE) clean); \
	(cd radiation; $(MAKE) clean)
