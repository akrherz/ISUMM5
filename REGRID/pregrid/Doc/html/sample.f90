program sample_read
! Fortran 90 version.

!   This is a simple program to read data in the REGRID intermediate
!   format.  It is included mainly as an aid to understanding said 
!   format.

  implicit none

! Declarations:

  integer, parameter :: IUNIT = 10
  integer, parameter :: OUNIT = 11
  logical, parameter :: LWRIT = .FALSE.
  integer :: ierr
  integer :: icount = 0

  integer :: IFV
  character(len=24) :: HDATE
  real :: XFCST
  character(len=9) :: FIELD
  character(len=25) :: UNITS
  character(len=46) :: DESC
  real :: XLVL
  integer :: NX
  integer :: NY
  integer :: IPROJ
  real :: STARTLAT
  real :: STARTLON
  real :: DELTALAT
  real :: DELTALON
  real :: DX
  real :: DY
  real :: XLONC
  real :: TRUELAT1
  real :: TRUELAT2

! SLAB is an allocatable array, because we do not necessarily know in 
! advance the size of the array we need to read.
  real, allocatable, dimension(:,:) :: SLAB

  READLOOP : DO

     ! Read the first record, the format version number:

     read (IUNIT, IOSTAT=IERR) IFV

     if (IERR > 0) then
        write(*, '(/,"Trouble reading IFV.",/)')
        stop 00003
     endif
        
     if (IERR < 0) exit READLOOP
     icount = icount + 1

     ! We expect format version number 3:
     
     if (IFV /= 3) THEN
        print*, 'Unrecognized format version IFV: IFV = ', IFV
        stop 00001
     endif

     if ( LWRIT ) then
        write (OUNIT) IFV
     endif


        
     ! Read the second record, common to all projections:

     read (IUNIT) HDATE, XFCST, FIELD, UNITS, DESC, XLVL, NX, NY, IPROJ
     print*, HDATE//"  ", XLVL, FIELD

     if ( LWRIT ) then
        write (OUNIT) HDATE, XFCST, FIELD, UNITS, DESC, XLVL, NX, NY, IPROJ
     endif

     ! Read the third record, which depends on the projection:

     if (IPROJ == 0) then 

        !  This is the Cylindrical Equidistant (lat/lon) projection:
        read (IUNIT) STARTLAT, STARTLON, DELTALAT, DELTALON

        if ( LWRIT ) then
           write (OUNIT) STARTLAT, STARTLON, DELTALAT, DELTALON
        endif

     elseif (IPROJ == 1) then 

        ! This is the Mercator projection:
        read (IUNIT) STARTLAT, STARTLON, DX, DY, TRUELAT1

        if ( LWRIT ) then
           write (OUNIT) STARTLAT, STARTLON, DX, DY, TRUELAT1
        endif

     elseif (IPROJ == 3) then

        ! This is the Lambert Conformal projection:
        read (IUNIT) STARTLAT, STARTLON, DX, DY, XLONC, TRUELAT1, TRUELAT2
        
        if ( LWRIT ) then
           write (OUNIT) STARTLAT, STARTLON, DX, DY, XLONC, TRUELAT1, TRUELAT2
        endif

     elseif (IPROJ == 5) then

        ! This is the Polar Stereographic projection:
        read (IUNIT) STARTLAT, STARTLON, DX, DY, XLONC, TRUELAT1

        if ( LWRIT ) then
           write (OUNIT) STARTLAT, STARTLON, DX, DY, XLONC, TRUELAT1
        endif

     else

        print*, 'Unrecoginized IPROJ: IPROJ = ', iproj
        stop 00002

     endif

     ! Now that we know the size of the field to follow in the file, 
     ! we can allocate our SLAB array:

     allocate(slab(NX,NY))

     ! Now that we have allocated the SLAB array, we can read the field in 
     ! the fourth record:

     read (IUNIT) slab

!=============================================================================!
! If you want to do anything with SLAB, this is as good place as any to do it.!
!                                                                             !
!=============================================================================!

     if ( LWRIT ) then
        write (OUNIT) slab
     endif

     ! Now that we have done all that we want to with SLAB, we need to
     ! deallocate it:

     deallocate(slab)

     ! Loop back to read the next field.
  ENDDO READLOOP

  write(*,'(/,"End of read loop.  Program finished.")')
  write(*,'("Number of fields read: ", i8,/)') icount

end program sample_read
