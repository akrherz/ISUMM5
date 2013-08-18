program pregrid_on84
! A priori knowledge about the archived analyses which this program uses:
!  --  This is a global dataset.
!  --  The data are on a lat/lon grid, regularly spaced in lat/lon space.
!  --  The lat/lon grid spacing is 2.5 x 2.5 degrees.
!  --  The data are separated into Northern hemisphere and Southern hemisphere
!      grids.
!  --  Each hemisphere grid has 145 points in the longitude.
!  --  Each hemisphere grid has 37 points in the latitude.
!  --  The northern hemisphere grid starts at Lat = 0.0, Lon = 0.0
!  --  The southern hemisphere grid starts at Lat = -90, Lon = 0.0
!  
!  --  Data at the following levels levels: 
!         Sfc, 1000, 850, 700, 500, 400, 300, 250, 200, 150, 100, 70, 50.
!  --  Analyses available: 
!         U, V, Tv, RH, Hgt
!  --  Wind components not available at the surface.
!  --  RH not available at the surface or above 300 mb
!  --  

! Recent changes:                                                             !
!    2001-02-14:                                                              !
!                                                                             !
!               - Allow file names to have date stamps out to minutes or      !
!                 seconds, if the user requests a time interval (in seconds)  !
!                 that is not evenly divisible into hours or minutes.         !


  use table
  use gridinfo
  use storage_module
  implicit none

  integer :: nunit1 = 12
  integer, dimension(255) :: iuarr = 0
  integer :: iostat
  integer :: iprint

  integer, parameter :: alength = 60000
  real, dimension (alength) :: array, scratch

  character(len=19),  dimension(1000) :: sstdates, fuldates, snowdates
  integer :: nsst=0, nful=0, nsnow=0

  integer :: ndim
  integer :: idim
  integer :: jdim
  integer :: n2lat

  integer , parameter :: maxlvl = 100
  real , dimension(maxlvl) :: plvl
  integer :: iplvl

  integer :: nlvl
  integer :: nvar 

  integer :: year, month, day, hour, minute, second
  integer :: k, n
  real :: level, xplv
  character (LEN=9) ::  field

  logical :: readit = .TRUE.
  logical :: gdone = .FALSE.

  character (LEN=9) :: on84flnm = 'ON84FILE '

  character (LEN=19) :: HSTART, HEND, HDATE
  character (LEN=19) :: hsave  = '0000-00-00_00:00:00'
  character (LEN=8) :: datatype
  integer, parameter :: north=29
  integer, parameter :: south=30
  logical :: lnorth = .FALSE.
  logical :: lsouth = .FALSE.
  integer :: itime
  integer :: hemis
  integer :: ierr

  integer :: ntimes
  integer :: interval
  logical :: ordered
  logical :: debug_print

  call pregrid_version

  call read_pregrid_namelist(hstart, hend, interval, ntimes, &
       ordered, debug_print, iprint)

  on84flnm(9:9) = char(ichar('A')-1)

  call parse_table(debug_print)

  if (debug_print) then
     call setll(ivrb=1)
  endif

  LOOP2 : DO ! Cycle through the list of files.

     on84flnm(9:9) = char(ichar(on84flnm(9:9))+1)
     gdone = .FALSE.
     readit = .TRUE.

     LOOP1 : DO  ! Read through a given file.  Exit only when the end of file
                 ! is hit, or we've read past date hend.
        nlvl = 0
        plvl = -999.
        call clear_storage

        LOOP0 : DO ! Read through a given date.  Exit only when we've reached
                   ! the end of the date stored in hsave, and want to output.

           if (READIT) then
              CALL rd_on84(on84flnm, NUNIT1, iuarr, &
                   level, field, hdate, Array, hemis, ierr)
              if (ierr.eq.1) then ! We've hit the end of a file.
                 hsave = hdate
                 exit LOOP0
              endif
              if (ierr.eq.2) then ! We've hit the end of all the files
                 exit LOOP2
              endif
           endif
           READIT = .TRUE.

           NDIM = NLAT * NLON

           if (hdate.gt.hsave) then
              print*, hdate//' > '//hsave//', so exit LOOP0'
              READIT = .FALSE.
              exit LOOP0
           endif

           if (field.eq.'SST') then
              iplvl = int(level)
              call put_storage(iplvl, field, &
                   reshape(array(1:ndim),(/nlon,nlat/)), nlon, nlat, hemis)
           endif

           if ((field.eq.'SNOWCOVR').and.(hemis == 29)) then
              iplvl = int(level)
              call put_storage(iplvl, field, &
                   reshape(array(1:ndim),(/nlon,nlat/)), nlon, nlat, hemis)
           endif

           if (hdate.lt.hstart) cycle LOOP0
           if (FIELD.EQ.'NULL') cycle LOOP0

           if (field /= 'SST' .and. field /= 'SNOWCOVR') then
              iplvl = int(level)
              call put_storage(iplvl, field, &
                   reshape(array(1:ndim),(/nlon,nlat/)), nlon, nlat, hemis)
           endif
        enddo LOOP0

        if (gdone) exit LOOP1
        if (hsave(1:4).ne.'0000') then
           n2lat = (nlat*2) - 1

           call output(scratch, hsave, n2lat, nlvl, sstdates, nsst, &
                plvl, maxlvl, snowdates, nsnow, fuldates, nful, interval)

           if ( (is_there(200100, 'SNOWCOVR')) .and. &
                (hsave < hstart) .and. &
                (nsnow == 2) ) then
              call file_delete(snowdates(1), 1, "PSNOW:", interval)
              snowdates(1) = snowdates(nsnow)
              snowdates(nsnow) = "0000-00-00_00:00:00.0000"
              nsnow = 1
           endif


           if ( (is_there(200100, 'SST')) .and. &
                (hsave < hstart) .and. &
                (nsst == 2) ) then
              call file_delete(sstdates(1), 1, "PSST:", interval)
              sstdates(1) = sstdates(nsst)
              sstdates(nsst) = "0000-00-00_00:00:00.0000"
              nsst = 1
           endif


           if (hsave.gt.hend) then
              print*, 'gdone = .TRUE.'
              gdone=.TRUE.
           endif
           hsave = hdate
        else
           hsave = hdate
        endif

        if (ierr.eq.1) exit LOOP1
     
     enddo LOOP1

     if (hsave.ge.hend) then
        if (is_there(200100, 'SST     ')) then
           print*, 'hsave, hend: '//hsave//'  '//hend
           print*, 'EXIT LOOP2'
           exit loop2
        endif
     endif

     hsave=hdate

     call cclose(iuarr(nunit1), iostat, ierr)
     iuarr(nunit1) = 0
     hsave = '0000-00-00_00:00:00'
  enddo LOOP2

  if (maxout.gt.1) then
     call datint(fuldates, nful, hstart, ntimes, interval)
  endif

  do n = 1, maxout
     if (nameout(n).eq.'SST     ') then
        call sstint(hstart, ntimes, interval, sstdates, nsst)
     elseif (nameout(n).eq.'SNOWCOVR') then
        ! Propagate the snow-cover data forward in time
        call snowprop(hstart, ntimes, interval, snowdates, nsnow)
     endif
  enddo

! Now delete the temporary files:
  write(*, '(/,10("*"), /, &
       &    "Deleting temporary files created by PREGRID_ON84...",/, &
       &    10("*")/)')

  call file_delete(fuldates, nful, "PON84:", interval)
  call file_delete(sstdates, nsst, "PSST:", interval)
  call file_delete(snowdates, nsnow, "PSNOW:", interval)

  write(*, '(/,10("*"), /, &
       &    "Done deleting temporary files",/, &
       &    10("*")/)')

  write(*, '(/,10("*"), /, &
       &    "Normal termination of program PREGRID_ON84",/, &
       &    10("*")/)')
end program pregrid_on84

subroutine file_delete(hdates, ndates, root, delta_time)
! Recent changes:                                                             !
!    2001-02-14:                                                              !
!                                                                             !
!             - Allow file names to have date stamps out to minutes or        !
!               seconds, if the user requests a time interval (in seconds)    !
!               that is not evenly divisible into hours or minutes.  Argument !
!               DELTA_TIME is checked for divisibility into 3600 (for hours)  !
!               or 60 (for minutes).  The local variable DATELEN is set       !
!               to be the number of characters to use in our character        !
!               dates.  Valid values for DATELEN are 13 (for hours),          !
!               16 (for minutes), and 19 (for seconds).                       !
!                                                                             !
  implicit none
  integer :: ndates
  character(len=19), dimension(ndates) :: hdates
  character(len=*) :: root

  logical :: lexist
  integer :: idate
  character(len=120) :: flnm

  character(len=120) :: fmt

  integer :: delta_time
  integer :: datelen

  if (mod(delta_time, 3600) == 0) then
     datelen = 13
  else if (mod(delta_time, 60) == 0) then
     datelen = 16
  else
     datelen = 19
  endif

  do idate = 1, ndates
     flnm=trim(root)//hdates(idate)(1:datelen)
     fmt = '(10x,"Deleting file:  '//trim(flnm)//'")'
     write(*, fmt)

     inquire(file=flnm, exist = lexist)
     if (lexist) then
        open(10, file=flnm, status='old')
        close(10, status="DELETE")
     else
        write(*,'(10x, "File ",A," does not exist.",/)') flnm
     endif
  enddo

end subroutine file_delete
