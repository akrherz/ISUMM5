subroutine datint(fuldates, nful, hstart, ntimes, interval)
!                                                                             !
!*****************************************************************************!
!                                                                             !
! Recent changes:                                                             !
!    2002-05-16:                                                              !
!               - Handle the Mercator projection.                             !
!                 This change also required changes to rrpr.F, rd_grib.F,     !
!                 output.F, gribcode.F                                        !
!                                                                             !
!    2001-02-14:                                                              !
!               - Changed the variable name IDTS to INTERVAL, just to make    !
!                 things a little more consistent with other routines.        !
!                                                                             !
!               - Allow file names to have date stamps out to minutes or      !
!                 seconds, if the user requests a time interval (in seconds)  !
!                 that is not evenly divisible into hours or minutes.         !
!                 INTERVAL is checked for divisibility into 3600 (for hours)  !
!                 or 60 (for minutes).  The local variable DATELEN is set     !
!                 to be the number of characters to use in our character      !
!                 dates.  Valid values for DATELEN are 13 (for hours),        !
!                 16 (for minutes), and 19 (for seconds).                     !
!                                                                             !
!                 This change also requires changes to pregrid_grib.F,        !
!                 output.F, rrpr.F, file_delete.F                             !
!                                                                             !
!*****************************************************************************!
  use gridinfo
  use storage_module
  implicit none
  integer :: nful
  integer :: interval
  character(len=*), dimension(nful) :: fuldates
  character(len=*) :: hstart
  integer :: ntimes

  character(len=24) :: hdate = "0000-00-00_00:00:00.0000"
  character(len=24) :: hdate_output, jdate
  character(len=9) :: field
  character(len=25) :: units
  character(len=46) :: desc
  real :: xfcst

  real :: level
  real, allocatable, dimension(:,:) :: scr2d, bfr2d
  integer :: iful, intervala, intervalb, ifv
  real :: awt
  integer :: itime

! DATELEN:  length of date strings to use for our output file names.
  integer :: datelen

! Decide the length of date strings to use for output file names.  
! DATELEN is 13 for hours, 16 for minutes, and 19 for hours.
  if (mod(interval,3600) == 0) then
     datelen = 13
  else if (mod(interval, 60) == 0) then
     datelen = 16
  else
     datelen = 19
  end if

  write(*, '(/,10("*"), /, "Subroutine DATINT:",/,2x,  &
       &    "Interpolating 3-d files to fill in any missing data...",/, &
       &    10("*")/)')

  TIMELOOP : do itime = 1, ntimes
     call geth_newdate(hdate(1:19), hstart(1:19), (itime-1)*interval)
     write(*, '(/,2x,"Looking for data at time ", A19)') hdate(1:datelen)//"      "
     do iful = 1, nful
        if (fuldates(iful).eq.hdate) then
           write(*, '(/, 10x, "Found file:      FILE:", A19)') hdate(1:datelen)//"      "
           cycle TIMELOOP
        else if ((fuldates(iful).lt.hdate) .and. &
             (fuldates(iful+1).gt.hdate) )then

           write(*,'(/, 10x,"Found surrounding files:      FILE:",& 
                &  A19,2x,"FILE:",A19)') fuldates(iful)(1:datelen)//"      ", &
                fuldates(iful+1)(1:datelen)//"      "
           write(*, '(10x, "Interpolating to create file:  FILE:", A19,/)') &
              &  hdate(1:datelen)//"      "
           call geth_idts(hdate(1:19), fuldates(iful)(1:19), intervalA)
           write(*,'(15x, "A Time Difference = ", F6.2, " hours.")') &
                float(intervalA) / 3600.
           call geth_idts(fuldates(iful+1)(1:19), hdate(1:19), intervalB)
           write(*,'(15x, "B Time Difference = ", F6.2, " hours.")') &
                float(intervalB) / 3600.
           AWT = 1. - (float(intervalA)/float(intervalA+intervalB))

           open(10, file='FILE:'//fuldates(iful)(1:datelen), form='unformatted', &
                status='old')
           call clear_storage
           READLOOP1 : do
              read(10, end=44) ifv
              read (10) jdate, xfcst, field, units, desc, level, map%nx, map%ny, map%igrid
              if (map%igrid == 0) then
                 read(10) map%lat1, map%lon1, map%dy, map%dx
              elseif (map%igrid == 1) then
                 read(10) map%lat1, map%lon1, map%dy, map%dx, map%truelat1
              elseif (map%igrid == 3) then
                 read (10) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                      map%truelat1, map%truelat2
              elseif (map%igrid == 5) then
                 read (10) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                      map%truelat1
              else
                 print*, 'Unrecognized map%igrid: ', map%igrid
                 stop "STOP IN DATINT"
              endif
              allocate(scr2d(map%nx, map%ny))
              read (10) scr2d
              call put_storage(nint(level), field, scr2d, map%nx, map%ny)
              deallocate(scr2d)
           enddo READLOOP1
44         close(10)

           open(10, file='FILE:'//fuldates(iful+1)(1:datelen), status='old', &
                form = 'unformatted')
           open(11, file='FILE:'//hdate(1:datelen), status='new', form='unformatted')
           READLOOP2 : do
              read (10,END=45) ifv
              read (10) jdate, xfcst, field, units, desc, level, map%nx, map%ny, map%igrid
              if (map%igrid == 0) then
                 read(10) map%lat1, map%lon1, map%dy, map%dx
              elseif (map%igrid == 1) then
                 read(10) map%lat1, map%lon1, map%dy, map%dx, map%truelat1
              elseif (map%igrid == 3) then
                 read (10) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                      map%truelat1, map%truelat2
              elseif (map%igrid == 5) then
                 read (10) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                      map%truelat1
              else
                 print*, 'Unrecognized map%igrid: ', map%igrid
                 stop "STOP IN DATINT"
              endif
              allocate(scr2d(map%nx, map%ny))
              read (10) scr2d
              if (is_there(nint(level), field)) then
                 allocate(bfr2d(map%nx,map%ny))
                 call get_storage(nint(level), field, bfr2d, map%nx, map%ny)
                 scr2d = bfr2d * (AWT) + scr2d * (1.-AWT)
                 write(11) ifv
                 hdate_output = hdate
                 write(11) hdate_output, xfcst, field, units, desc, level, &
                      map%nx, map%ny, map%igrid
                 if (map%igrid == 0) then
                    write(11) map%lat1, map%lon1, map%dy, map%dx
                 elseif (map%igrid == 1) then
                    write(11) map%lat1, map%lon1, map%dy, map%dx, map%truelat1
                 elseif (map%igrid == 3) then
                    write (11) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                         map%truelat1, map%truelat2
                 elseif (map%igrid == 5) then
                    write (11) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                         map%truelat1
                 else
                    print*, 'Unrecognized map%igrid: ', map%igrid
                    stop "STOP IN DATINT"
                 endif
                 write(11) scr2d
              else
                 print*, 'hdate = ', hdate
                 print*, 'Problem:  ', fuldates
                 print*, 'Field = ', field
                 stop
              endif
              deallocate(scr2d, bfr2d)
           enddo READLOOP2
45         close(10)
           close(11)
           cycle TIMELOOP
        endif
     enddo

     print*, 'Date not found: ', hdate
     stop
     
  enddo TIMELOOP

  write(*, '(/,10("*"), /, "End Subroutine DATINT.",/,  10("*")/)')

end subroutine datint
