!*****************************************************************************!
! Subroutine RD_GRIB                                                          !
!                                                                             !
! Part of:                                                                    !
!    REGRID/pregrid/grib.misc                                                 !
!                                                                             !
! Purpose:                                                                    !
!    Read one record from the input GRIB file.  Based on the information in   !
!    the GRIB header and the user-defined Vtable, decide whether the field in !
!    the GRIB record is one to process or to skip.  If the field is one we    !
!    want to keep, extract the data from the GRIB record, and pass the data   !
!    back to the calling routine.                                             !
!                                                                             !
! Argument list:                                                              !
!    Input:                                                                   !
!       IUNIT   : "Unit Number" to open and read from.  Not really a Fortran  !
!                 unit number, since we don't do Fortran I/O for the GRIB     !
!                 files.  Nor is it a UNIX File Descriptor returned from a C  !
!                 OPEN statement.  It's really just an array index to the     !
!                 array (IUARR) where the UNIX File Descriptor values are     !
!                 stored.                                                     !
!       GRIBFLNM: File name to open, if it is not already open.               !
!       ALENGTH : Length of an array to hold the 2d slab read from the GRIB   !
!                 record.  This is parameterized in PREGRID_GRIB.             !
!       IUARR   : Array to hold UNIX File descriptors retured from a C open   !
!                 statement.  If the value of IUARR(IUNIT) is zero, then the  !
!                 file GRIBFLNM must be opened, and the value of IUARR(IUNIT) !
!                 becomes the UNIX File descriptor to read from.              !
!       IPRINT  : Integer for various amounts of printout.                    !
!                                                                             !
!    Output:                                                                  !
!       LEVEL    : The pressure-level (Pa) of the field to process.           !
!       FIELD    : The field name of the field to process.  'NULL' is returned!
!                  if we don't want to process the field we've read.          !
!       HDATE    : The 19-character date of the field to process.             !
!       DataArray: The 2d slab of data to process.                            !
!       IERR     : Error flag: 0 - no error on read from GRIB file.           !
!                              1 - Hit the end of the GRIB file.              !
!                              2 - The file GRIBFLNM we tried to open does    !
!                                  not exist.                                 !
! Externals                                                                   !
!     Module TABLE                                                            !
!     Module GRIDINFO                                                         !
!     Subroutine COPEN                                                        !
!     Subroutine DEALLOGRIB                                                   !
!     Subroutine GRIBGET                                                      !
!     Subroutine GRIBHEADER                                                   !
!     Subroutine GET_SEC1                                                     !
!     Subroutine GET_SEC2                                                     !
!     Subroutine GET_GRIDINFO                                                 !
!     Subroutine BUILD_HDATE                                                  !
!     Subroutine GETH_NEWDATE                                                 !
!     Subroutine GRIBDATA                                                     !
!                                                                             !
! Side Effects                                                                !
!     File GRIBFLNM is opened, as necessary                                   !
!                                                                             !
!     Variable MAP from module GRIDINFO is filled in.                         !
!                                                                             !
!     Numerous side effects from the GRIB-processing routines.                !
!                                                                             !
! Kevin W. Manning                                                            !
! NCAR/MMM                                                                    !
! Summer, 1998, and continuing                                                !
! SDG                                                                         !
!                                                                             !
!*****************************************************************************!
!                                                                             !
! Recent changes:                                                             !
!                                                                             !
! 2003-01-24:                                                                 !
!               - Truelat of Polar Stereographic grid depends on hemisphere   !
! 2002-05-16:                                                                 !
!               - Handle the Mercator projection.                             !
!                 This change also required changes to rrpr.F, output.F,      !
!                 datint.F, gribcode.F                                        !
!                                                                             !
! 2000-08-10:                                                                 !
!           - Added the capability to deal with thinned grids.  The thinned-  !
!             grid capability was adapted from modifications provided by Oren !
!             Duer of the Hebrew University of Jerusalem.  Many thanks are    !
!             due to him.                                                     !
!                                                                             !
!*****************************************************************************!
SUBROUTINE rd_grib(IUNIT, gribflnm, level, field, hdate,  &
     DataArray, alength, ierr, iuarr, iprint)
  use table
  use gridinfo
  implicit none

  integer :: iprint
  integer :: iunit ! Array number in IUARR assigned to the C read pointer.
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

  integer :: icenter, iprocess, iscan, ii, isb
  integer year, month, day, hour, minute, second, icc, iyy
  integer :: fcst
  real :: level
  character(LEN=*) :: field
  character(LEN=*) :: gribflnm
  integer, dimension(255) :: iuarr
  integer :: ierr, iostat, nunit
  integer :: i, lvl2, lvl1
  character(LEN=*) :: hdate
  integer :: igherr

! Variables for thinned grids:
  logical :: lthinned = .FALSE.
  real, dimension(alength) :: thinnedDataArray
  integer, dimension(74) :: npoints_acc
  real :: mj, xmj
  integer :: np, ny, nx
  real :: Va, Vb, Vc, Vd
  real, external :: oned

  ierr = 0

! If the file GRIBFLNM has not been opened, then IUARR(IUNIT) should be Zero.
! In this case, open the file GRIBFLNM, and store the UNIX File descriptor
! in to IUARR(IUNIT).  This way, we'll know what UNIX File descriptor to use
! next time we call this RD_GRIB subroutine.
!
  if (iuarr(iunit).eq.0) then
     if (iprint.eq.1) then
        call copen(iunit, nunit, trim(gribflnm)//char(0), 1, ierr, iprint)
     else
        call copen(iunit, nunit, trim(gribflnm)//char(0), 1, ierr,-1)
     endif
     if (ierr.ne.0) then
        call deallogrib
        ierr = 2
        return
     endif
     iuarr(iunit) = nunit
  endif

! Read a single GRIB record, but do no unpacking now:

  call gribget(iuarr(iunit), ierr)

  if (ierr.ne.0) then
     if (iprint.gt.0) write(*,*) 'ierr = ', ierr, ' in RD_GRIB.'
     call deallogrib
     return
  endif
!
! Unpack the header information:
!
  call gribheader(igherr)
  if (igherr /= 0) then
     field = "NULL"
     call deallogrib
     return
  endif
!
! Copy header information to arrays KSEC1, KSEC2, INFOGRID, and GRIDINFO
!
  call get_sec1(ksec1)
  call get_sec2(ksec2)
  call get_gridinfo(infogrid, ginfo)

  icenter = KSEC1(3)        ! Indicator of the source (center) of the data.
  iprocess = KSEC1(4)       ! Indicator of model (or whatever) which generated the data.
  IPARM=KSEC1(7)            ! Indicator of parameter
  KTYPE=KSEC1(8)            ! Indicator of type of level

  IF(KTYPE.EQ.1) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.100) THEN
     LVL1=FLOAT(KSEC1(9))  * 100.
     LVL2=-99
  ELSEIF(KTYPE.EQ.101) THEN
     LVL1=KSEC1(9)
     LVL2=KSEC1(10)
  ELSEIF(KTYPE.EQ.102) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.103) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.105) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.107) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.109) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.111) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.112) THEN ! Layer between two depths below surface
     LVL1=KSEC1(9)
     LVL2=KSEC1(10)
  ELSEIF(KTYPE.EQ.113) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.115) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.117) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.119) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.125) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.160) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.200) THEN
     LVL1=0
     LVL2=-99
  ELSEIF(KTYPE.EQ.201) THEN
     LVL1=0
     LVL2=-99
  ELSE
     LVL1=KSEC1(9)
     LVL2=KSEC1(10)
  ENDIF

! Check to see that the combination of iparm, ktype, lvl1, and lvl2
! match what's been requested in the Vtable.  If not, set the field
! name to 'NULL', meaning that we don't want to process this one.

  field = 'NULL'
  do i = 1, maxvar
     if (gcode(i).eq.iparm) then
        if (lcode(i).eq.ktype) then
           if ((level1(i).eq.lvl1) .or. (level1(i) == splatcode) ) then
              if (level2(i).eq.lvl2) then
                 field=namvar(i)
                 if (ktype.eq.100) then ! Pressure-level
                    level=lvl1
                 elseif (ktype.eq.102) then
                    level=201300.
                 elseif ((ktype.eq.116.and.lvl1.le.50.and.lvl2.eq.0) .or. &
                      (ktype.eq.105).or.(ktype.eq.1) .or. &
                      (ktype.eq.111).or.(ktype.eq.112) ) then
                    ! level=200100.
                    level = float(200000+iprty(i))
                 elseif (ktype.eq.109.and.icenter.eq.98) then 
                    ! level=200100.
                    level = float(200000+iprty(i))
                 endif
              endif
           endif
        endif
     endif
  enddo

  if (field .eq. 'NULL') then
     call deallogrib
     return
  endif

  if (field.eq.'WEASD') then
     level = level + ksec1(19)+1
  endif

! Build the 19-character date string, based on GRIB header date and time
! information, including forecast time information:

  ICC=KSEC1(22)             ! CENTURY OF THE DATA
  IYY=KSEC1(11)             ! (TWO-DIGIT) YEAR OF THE DATA
  MONTH=KSEC1(12)           ! MONTH OF THE DATA
  DAY=KSEC1(13)             ! DAY OF THE DATA
  HOUR=KSEC1(14)            ! HOUR OF THE DATA
  MINUTE=KSEC1(15)          ! MINUTE OF THE DATA
  SECOND=0
  if (ksec1(19) == 3) then
     FCST = (KSEC1(17) + KSEC1(18))/2
  elseif (ksec1(19) == 4 .or. ksec1(19) == 5) then
     FCST = KSEC1(18)
  else
     FCST = KSEC1(17)
  endif

  if (IYY.EQ.00) then
     YEAR = ICC*100
  else
     YEAR = (ICC-1)*100 + IYY
  endif

  call build_hdate(hdate,year,month,day,hour,minute,second)

  call geth_newdate(hdate,hdate,3600*fcst)

! Store information about the grid on which the data is. 
! This stuff gets stored in the MAP variable, as defined in module GRIDINFO

  if (ksec2(4).eq.0) then ! Lat/Lon grid
     map%igrid = 0
     map%nx = infogrid(1)
     map%ny = infogrid(2)
     map%dx = ginfo(8)
     map%dy = ginfo(9)
     map%lat1 = ginfo(3)
     map%lon1 = ginfo(4)

  elseif (ksec2(4).eq.1) then ! Mercator grid
     map%igrid = 1
     map%nx = infogrid(1)
     map%ny = infogrid(2)
     map%dx = ginfo(8)
     map%dy = ginfo(9)
     map%lat1 = ginfo(3)
     map%lon1 = ginfo(4)
     map%truelat1 = ginfo(11)

  elseif (ksec2(4).eq.3) then ! Lambert Conformal Grid
     map%igrid = 3
     map%nx = infogrid(1)
     map%ny = infogrid(2)
     map%lov = ginfo(6)
     map%truelat1 = ginfo(11)
     map%truelat2 = ginfo(12)
     map%dx = ginfo(7)
     map%dy = ginfo(8)
     map%lat1 = ginfo(3)
     map%lon1 = ginfo(4)
         
  elseif(ksec2(4).eq.4) then ! Gaussian Grid; we'll call it lat/lon
     map%igrid = 0
     map%nx = infogrid(1)
     map%ny = infogrid(2)
     map%dx = ginfo(8)
     map%dy = ginfo(19)
     map%lon1 = ginfo(4)
     map%lat1 = ginfo(3)

  elseif (ksec2(4).eq.5) then ! Polar-Stereographic Grid.
     map%igrid = 5
     map%nx = infogrid(1)
     map%ny = infogrid(2)
     map%lov = ginfo(6)
     if (infogrid(9) > 127) then
        map%truelat1 = -60.
     else
        map%truelat1 = 60.
     endif
     map%truelat2 = 91.
     map%dx = ginfo(7)
     map%dy = ginfo(8)
     map%lat1 = ginfo(3)
     map%lon1 = ginfo(4)

  else
     print*, 'Unknown ksec2(4): ', ksec2(4)
  endif

111  format(' igrid      : ', i3, /, &
          ' nx, ny     : ', 2I4, /, &
          ' truelat1, 2: ', 2F10.4, /, &
          ' Center Lon : ', F10.4, /, &
          ' LatLon(1,1): ', 2F10.4, /, &
          ' DX, DY     : ', F10.4, F10.4)

! Special for NCEP/NCAR Reanalysis Project:
!      Throw out PSFC on lat/lon grid (save gaussian version)
  if (((icenter.eq.7).and.(iprocess.eq.80)) .or. &
      ((icenter.eq.7).and.(iprocess.eq.180))) then  ! Careful! These combination may refer 
                                                    ! to other products as well.
     if ((field.eq.'PSFC').and.(ksec2(4).eq.0)) then
        write(*,'(/)')
        write(*,'("**** I interpret this as the PSFC field from the NCEP/NCAR")')
        write(*,'("**** Reanalysis pgb files, so I am skipping this field. ")')
        write(*,'(/)')
        field='NULL'
        call deallogrib
        return
     endif
  endif

! If nx=65535, assume the grid is a thinned grid. 
! I will deal with it only if the grid ID is 37-44.
  if (map%nx.eq.65535) then
     if ((KSEC1(5).lt.37).or.(KSEC1(5).gt.44)) then
        write(*,'("GRID ID is not 37-44. Aborting.",//)')
        write(*,'(" ***** STOP in Subroutine RD_GRIB.",//)')
        stop
     endif
     lthinned = .TRUE.
     map%nx = 73
     map%dx = 1.25
  else
     lthinned = .FALSE.
  endif

! Check that the parameter ALENGTH is large enough to hold the data.

  if ((map%nx*map%ny).gt.alength) then
     write(*,'(" ***** ERROR in Subroutine RD_GRIB.",//)')
     write(*,'(10x,"Grid dimensions are larger than parameterized size.")')
     write(*,'(10x,"Grid size = ", I10, 5x,&
          &"Parameterized dimension = ", I10)') map%nx*map%ny, alength
     write(*,'(10x,"Increase dimension ALENGTH in program PREGRID_GRIB",//)')
     write(*,'(" ***** STOP in Subroutine RD_GRIB.",//)')
     stop
  endif

! Unpack the 2D slab from the GRIB record, and put it in array DataArray
  if (lthinned) then
    call gribdata(thinnedDataArray,3447)

    ! Calculate how many points for each latitude, and accumulate into array
    if ((KSEC1(5).ge.37).and.(KSEC1(5).le.40)) then
       ! Northern hemisphere:
       npoints_acc(1)=0
       npoints_acc(2)=73
       do i=1,72
          np = int(2.0+(90.0/1.25)*cos(i*1.25*3.1415926/180.0))
          npoints_acc(i+2)=npoints_acc(i+1)+np
       enddo
    else
       ! Southern Hemisphere:
       npoints_acc(1)=0
       npoints_acc(2)=2
       do i=1,71
          ii = 72-i
          np = int(2.0+(90.0/1.25)*cos(ii*1.25*3.1415926/180.0))
          npoints_acc(i+2)=npoints_acc(i+1)+np
       enddo
       npoints_acc(74) = npoints_acc(73) + 73
    endif
       
    ! for row number i (where i=1 is the southern edge of the grid)
    !   npoints_acc(i+1)-npoints_acc(i) = number of points in this line
    !   npoints_acc(i)+1 = index into thinned array for first point of line

    do ny=1,73
       np = npoints_acc(ny+1)-npoints_acc(ny) ! Number of points in this line.
       do nx=1,73
          ! Calulate the x index (mj) of thinned array (real value)
          mj = (nx-1.0)*(np-1.0)/(72.0)
          
          if (abs(mj - int(mj)) < 1.E-10) then
             DataArray((ny-1)*73+nx) = thinnedDataArray(npoints_acc(ny)+1+int(mj))
          else
             ! Get the 2 closest values from thinned array
             Vb = thinnedDataArray(npoints_acc(ny)+1+int(mj))
             Vc = thinnedDataArray(npoints_acc(ny)+1+int(mj)+1)
             ! Get the next two closest, if available:
             Va = -999999.
             Vd = -999999.
             if (mj > 1.0) then
                Va = thinnedDataArray(npoints_acc(ny)+1+int(mj)-1)
             endif
             if (mj < np-2) then
                Vd = thinnedDataArray(npoints_acc(ny)+1+int(mj)+2)
             endif

             if ((Va < -999998.) .or. (Vd < -999998.)) then
                ! Use 2-point linear interpolation.
                DataArray((ny-1)*73+nx) = Vb*(int(mj)+1.0-mj) + Vc*(mj-int(mj))
             else
                ! Use 4-point overlapping parabolic interpolation.
                xmj = mj - float(int(mj))
                DataArray((ny-1)*73+nx) = oned(xmj,Va,Vb,Vc,Vd)
             endif
          endif
       enddo
    enddo
  else
    call gribdata(DataArray,map%nx*map%ny)

  endif

! Deallocate a couple of arrays that may have been allocated by the 
! GRIB decoding routines.

  call deallogrib

END subroutine rd_grib

real function oned(x, a, b, c, d) Result (Answer)
  implicit none
  real :: x ! Proportion of the way between B and C.  Between 0.0 and 1.0
  real :: a, b, c, d

  if (abs(x) < 1.E-10) then
     Answer = B
     return
  endif
  IF(abs(x-1.) < 1.E-10) then
     Answer = C
     return
  endif
  Answer = (1.0-X)*(B+X*(0.5*(C-A)+X*(0.5*(C+A)-B)))+X*(C+(1.0-X)*(0.5 &
       *(B-D)+(1.0-X)*(0.5*(B+D)-C)))
end function oned
