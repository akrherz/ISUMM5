SUBROUTINE rd_grib(gribflnm, iunit,  iuarr, &
     level, field, hdate, DataArray, Alength, hemis, ierr)
! Recent changes:
!    2000-08-31:  Changed the print flag in the call to "copen" to 
!                 turn off the error print that was scaring folks.
  use table
  use gridinfo
  use storage_module
  implicit none

#if defined (BIT64) 
  integer, parameter :: wdsize = 64
#endif
#if defined (BIT32) 
  integer, parameter :: wdsize = 32
#endif

  integer :: iunit
  integer :: alength
  real, dimension(alength) :: DataArray
  integer, dimension(100) :: KSEC1
  integer, dimension(10) :: KSEC2 
  integer, dimension(40) :: infogrid
  real, dimension(40) :: ginfo
!
!-----------------------------------------------------------------------
  integer :: iparm, ktype
  logical :: lopen
  integer :: nunit

  integer :: ivals, iscan, ii, isb
  integer :: year, month, day, hour, minute, second, icc, iyy
  real :: level
  character(len=*) :: field
  integer hemis
  character(LEN=*) :: gribflnm
  integer, dimension(255) :: iuarr
  integer :: ierr
  integer :: i, lvl2, lvl1
  character(LEN=*) :: hdate
  integer :: igherr

  ierr = 0
  if (iuarr(iunit).eq.0) then
     call copen(iunit, nunit, gribflnm, 1, ierr, 0)
     if (ierr.ne.0) then
        return
     endif
     iuarr(iunit) = nunit
  endif

! Read a single GRIB record, but do no unpacking now:

  call gribget(iuarr(iunit), ierr)

  if (ierr.ne.0) then
     write(*,'("Ierr = ", I4, " in RD_GRIB.")') ierr
     return
  endif
!
! Unpack the header information:
!
  call gribheader(igherr)
  if (igherr /= 0) then
     call deallogrib
     field = "NULL"
     return
  endif

  call get_sec1(ksec1)
  call get_sec2(ksec2)
  call get_gridinfo(infogrid, ginfo)

  IPARM=KSEC1(7)            ! INDICATOR OF PARAMETER
  KTYPE=KSEC1(8)            ! INDICATOR OF TYPE OF LEVEL

  IF(KTYPE.EQ.101) THEN
     LVL1=FLOAT((KSEC1(9)+KSEC1(10))/2)
     LVL2=-99
  ELSEIF(KTYPE.EQ.105) THEN
     LVL1=KSEC1(9)
     LVL2=KSEC1(10)
  ELSEIF(KTYPE.EQ.1) THEN
     LVL1=KSEC1(9)
     LVL2=KSEC1(10)
  ELSEIF(KTYPE.EQ.111) THEN
     LVL1=KSEC1(9)
     LVL2=KSEC1(10)
  ELSEIF(KTYPE.EQ.112) THEN ! Layer between two depths below surface
     LVL1=KSEC1(9)
     LVL2=KSEC1(10)
  ELSEIF(KTYPE.EQ.100) THEN
     LVL1=FLOAT(KSEC1(9))  * 100. !Pa ! VALUE 1 OF LEVEL
     LVL2=KSEC1(10)
  ELSE
     LVL1=KSEC1(9)
     LVL2=KSEC1(10)
  ENDIF

  field = 'NULL'
  do i = 1, maxvar
     if (gcode(i).eq.iparm) then
        if (lcode(i).eq.ktype) then
           if ((level1(i).eq.lvl1) .or. (level1(i).eq.splatcode) ) then
              if ((level2(i).eq.lvl2) .or. (level2(i).eq.-99)) then
                 field=namvar(i)
                 if (ktype.eq.100) then
                    level=lvl1
                 elseif (ktype.eq.102) then
                    level=201300.
                 else if ((ktype.eq.105).or.(ktype.eq.111).or. &
                      (ktype.eq.112).or.(ktype.eq.1)) then
                    level=200100.
                 endif
              endif
           endif
        endif
     endif
  enddo

  NLON = infogrid(1)
  NLAT = infogrid(2)
  STARTLAT = ginfo(3)
  STARTLON = ginfo(4)
  DELTALON = ginfo(8)
  DELTALAT = ginfo(9)

  ICC=KSEC1(22)             ! CENTURY OF THE DATA
  IYY=KSEC1(11)             ! (TWO-DIGIT) YEAR OF THE DATA
  MONTH=KSEC1(12)           ! MONTH OF THE DATA
  DAY=KSEC1(13)             ! DAY OF THE DATA
  HOUR=KSEC1(14)            ! HOUR OF THE DATA
  MINUTE=KSEC1(15)          ! MINUTE OF THE DATA
  SECOND=0

  if (IYY.EQ.00) then
     YEAR = ICC*100
  else
     YEAR = (ICC-1)*100 + IYY
  endif

  call build_hdate(hdate,year,month,day,hour,minute,second)

  hemis = ksec1(5)
  if (hemis.ne.29.and.hemis.ne.30) then
     write(*,'("Unrecognized grid: ", I8)') hemis
     write(*,'("Expecting grid 29 or grid 30")')
     write(*,'("STOP in subroutine RD_GRIB")')
     stop
  endif

  call gribdata(DataArray,nlon*nlat)

! Deallocate a couple of arrays that may have been allocated by the 
! GRIB decoding routines.

  call deallogrib

END subroutine rd_grib
