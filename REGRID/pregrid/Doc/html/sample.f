      program sample_read
C     Fortran 77 version

C   This is a simple program to read data in the REGRID intermediate
C   format.  It is included mainly as an aid to understanding said 
C   format.

      implicit none

C Declarations:       

      integer IFV
      character*24 HDATE
      real XFCST
      character*9  FIELD
      character*25 UNITS
      character*46 DESC
      real XLVL
      integer NX
      integer NY
      integer IPROJ
      real STARTLAT
      real STARTLON
      real DELTALAT
      real DELTALON
      real DX
      real DY
      real XLONC
      real TRUELAT1
      real TRUELAT2
C
C So as not to need dynamic memory allocation, we dimension SLAB as
C large as we're likely to need.  In order to read the appropriate 
C size of SLAB, we use the read routine READSLAB.  If you want to do
C anything with the array SLAB, subroutine READSLAB is the place to 
C do it.
C
      integer MAXNX, MAXNY
      parameter (MAXNX=500, MAXNY=500)
      real SLAB(MAXNX,MAXNY)

      integer IUNIT
      integer ICOUNT

      IUNIT = 10
      ICOUNT = 0

 100  CONTINUE

C        Read the first record, the format version number:

         read (IUNIT, END=101) IFV

C        We expect format version number 3:
     
         if (IFV .ne. 3) THEN
            print*, 'Unrecognized format version IFV: IFV = ', IFV
            stop 00001
         endif

         icount = icount + 1

C        Read the second record, common to all projections:

         read (IUNIT) HDATE, XFCST, FIELD, UNITS, DESC, XLVL, NX, NY,
     &        IPROJ
         print*, HDATE//'  ', XLVL, FIELD

C        Read the third record, which depends on the projection:

         if (IPROJ .EQ. 0) then

C           This is the Cylindrical Equidistant (lat/lon) projection:
            read (IUNIT) STARTLAT, STARTLON, DELTALAT, DELTALON

         elseif (IPROJ .EQ. 1) then

C           This is the Mercator projection:
            read (IUNIT) STARTLAT, STARTLON, DX, DY, TRUELAT1

         elseif (IPROJ .EQ. 3) then

C           This is the Lambert Conformal projection:
            read (IUNIT) STARTLAT, STARTLON, DX, DY, XLONC, TRUELAT1,
     &           TRUELAT2

         elseif (IPROJ .EQ. 5) then

C           This is the Polar Stereographic projection:
            read (IUNIT) STARTLAT, STARTLON, DX, DY, XLONC, TRUELAT1

         else

            print*, 'Unrecoginized IPROJ: IPROJ = ', iproj
            stop 00002

         endif

C        Now we can read the field itself, the fourth record:
         call readslab(IUNIT, SLAB, NX, NY)

C        Loop back to read the next field.
         GO TO 100

 101  CONTINUE

      print*
      print*, 'End of read loop.  Program finished.'
      print*, 'Number of fields read: ', icount
      print*

      stop
      end

C======================================================================

      subroutine readslab(IUNIT, SLAB, NX, NY)
      implicit none
      integer iunit
      integer nx, ny
      real slab(nx,ny)

      read (iunit) SLAB

C If you want to do anything with SLAB, this is as good place as any 
C for it.

      return
      end
