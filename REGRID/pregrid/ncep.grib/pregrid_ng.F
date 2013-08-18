program pregrid_ng
! Recent changes:                                                             !
!                                                                             !
!   2001-02-14:                                                               !
!               - Allow file names to have date stamps out to minutes or      !
!                 seconds, if the user requests a time interval (in seconds)  !
!                 that is not evenly divisible into hours or minutes.         !
!                                                                             !
!   2001-02-14:                                                               !
!               - Propagate snow-cover data forward in time as necessary      !
!                                                                             !
!   2000-08-31:                                                               !
!               - Allow the program to recognize SKINTEMP as something to be  !
!                 used as a sea-surface temperature.                          !
  use table
  use gridinfo
  use storage_module

  implicit none

  integer :: nunit1 = 12
  integer, dimension(255) :: iuarr = 0
  integer :: iostat

  integer, parameter :: alength = 120000
  real, dimension (alength) :: array, scratch

  character(len=19),  dimension(1000) :: sstdates, snowdates, fuldates
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

  character (LEN=9) :: gribflnm = 'GRIBFILE '

  character (LEN=19) :: HSTART, HEND, HDATE
  character (LEN=19) :: hsave = '0000-00-00_00:00:00'
  character (LEN=8) :: datatype
  integer, parameter :: north=29
  integer, parameter :: south=30
  logical :: lnorth = .FALSE.
  logical :: lsouth = .FALSE.
  integer :: itime
  integer :: ntimes
  integer :: hemis
  integer :: ierr
  integer :: iprint

  integer :: interval
  logical :: ordered
  logical :: debug_print

  call pregrid_version

  call read_pregrid_namelist(hstart, hend, interval, ntimes, &
       ordered, debug_print, iprint)

  gribflnm(9:9) = char(ichar('A')-1)

  call parse_table(debug_print)

  LOOP2 : DO ! Cycle through the list of files.
     gdone = .FALSE.
     readit = .TRUE.

     gribflnm(9:9) = char(ichar(gribflnm(9:9))+1)

     LOOP1 : DO  ! Read through a given file.  Exit only when the end of file
                 ! is hit, or we've read past date hend.
        nlvl = 0
        plvl = -999.
        call clear_storage

        LOOP0 : DO ! Read through a given date.  Exit only when we've reached
                   ! the end of the date stored in hsave, and want to output.

           if (READIT) then
              call rd_grib(gribflnm, nunit1, iuarr, &
                   level, field, hdate, array, Alength, hemis, ierr)
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

           if ((field.eq.'SST     ').or.(field == "SKINTEMP")) then
              iplvl = int(level)
              call put_storage(iplvl, field, &
                   reshape(array(1:ndim), (/nlon,nlat/)), &
                   nlon, nlat, hemis)
           endif

           if (hdate.lt.hstart) cycle LOOP0
           if (FIELD.EQ.'NULL    ') cycle LOOP0

           if ((field.ne.'SST     ').and.(field /= "SKINTEMP")) then
              iplvl = int(level)
              call put_storage(iplvl, field, &
                   reshape(array(1:ndim), (/nlon,nlat/)), &
                   nlon, nlat, hemis)
           endif
        enddo LOOP0
        if (gdone) exit LOOP1
        if (hsave(1:4).ne.'0000') then
           n2lat = (nlat*2) - 1
           call output(scratch, hsave, n2lat, nlvl, plvl, maxlvl, sstdates, nsst, &
                snowdates, nsnow, fuldates, nful, interval)
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
        if ( (is_there(200100, 'SST     ')) .or. &
             (is_there(200100, "SKINTEMP")) ) then
           print*, 'hsave, hend: '//hsave//'  '//hend
           call cclose(iuarr(nunit1), iostat, ierr)
           iuarr(nunit1) = 0
           hsave = '0000-00-00_00:00:00'
           print*, 'CYCLE LOOP2'
           cycle loop2
        endif
     endif

     hsave=hdate

     call cclose(iuarr(nunit1), iostat, ierr)
     iuarr(nunit1) = 0
     hsave = '0000-00-00_00:00:00'
  enddo LOOP2

  do n = 1, maxout
     if ((nameout(n).eq.'SST     ').or.(nameout(n) == "SKINTEMP")) then
        call sstint(hstart, ntimes, interval, sstdates, nsst)
     else if (nameout(n).eq.'WEASD   ') then
        ! Propagate the snow-cover data forward in time
        call snowprop(hstart, ntimes, interval, snowdates, nsnow)
     endif
  enddo

  if (maxout.gt.1) then
     call datint(fuldates, nful, hstart, ntimes, interval)
  endif

! Now delete the temporary files:
  write(*, '(/,10("*"), /, &
       &    "Deleting temporary files created by PREGRID_NCEP...",/, &
       &    10("*")/)')

  call file_delete(fuldates, nful, "PNG:", interval)
  call file_delete(sstdates, nsst, "PSST:", interval)
  call file_delete(snowdates, nsnow, "PSNOW:", interval)

  write(*, '(/,10("*"), /, &
       &    "Done deleting temporary files",/, &
       &    10("*")/)')

  write(*, '(/,10("*"), /, &
       &    "Normal termination of program PREGRID_NCEP",/, &
       &    10("*")/)')
end program pregrid_ng

subroutine file_delete(hdates, ndates, root, interval)
! Recent changes:                                                             !
!                                                                             !
!    2001-02-14:                                                              !
!               - Allow file names to have date stamps out to minutes or      !
!                 seconds, if the user requests a time interval (in seconds)  !
!                 that is not evenly divisible into hours or minutes.         !

  implicit none
  integer :: ndates
  character(len=19), dimension(ndates) :: hdates
  character(len=*) :: root

  logical :: lexist
  integer :: idate
  character(len=120) :: flnm

  character(len=120) :: fmt
  integer :: interval
  integer :: datelen

  if (mod(interval, 3600) == 0) then
     datelen = 13
  else if (mod(interval, 60) == 0) then
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
