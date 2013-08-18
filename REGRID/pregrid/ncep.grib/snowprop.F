subroutine snowprop(hstart, ntimes, delta_time, snowdates, nsnow)
! Recent changes:                                                             !
!    2001-02-14:                                                              !
!             - This routine is new to the ncep.grib directory.  It is based  !
!               on existing code in the on84 directory                        !
  implicit none
  integer :: ntimes, nsnow, delta_time
  character(len=19) :: hstart
  character(len=19), dimension(nsnow) :: snowdates

  integer, parameter :: idim=145
  integer, parameter :: jdim=73

  real :: startlat, startlon, deltalat, deltalon

  real, dimension(idim,jdim) :: snow

  character(len=19), dimension(ntimes) :: reqdate
  character(len=24) :: hdate
  character(len=9) :: field
  character(len=25) :: units
  character(len=46) :: Desc
  real :: xfcst
  integer :: ifv
  real :: level
  integer :: nlon, nlat, llflag
  integer :: n, isb

  integer :: datelen

  if (mod(delta_time, 3600) == 0) then
     datelen = 13
  else if (mod(delta_time, 60) == 0) then
     datelen = 16
  else
     datelen = 19
  endif


  close(15)

  write(*, '(/,10("*"), /, "Subroutine SNOWPROP:",/,2x,  &
       &    "Propagating SNOW files forward in time to fill in any missing data...",/, &
       &    10("*")/)')

  reqdate(1) = hstart
  do n = 2, ntimes
     call geth_newdate(reqdate(n), reqdate(n-1), delta_time)
  enddo

  isb = 1
  FULLOOP : do n = 1, ntimes
     write(*, '(2x,"Looking for SNOW at time ", A19,/)') reqdate(n)(1:datelen)//"      "
     SNOWLOOP : do
        write(*, '(5x,"snowdates = ", A19)') snowdates(isb)(1:datelen)//"      "
        if (snowdates(isb) == reqdate(n)) then
           open(15, file='PSNOW:'//snowdates(isb)(1:datelen), form='unformatted')
           read(15) ifv
           read(15) hdate, xfcst, field, units, Desc, level, nlon, nlat, llflag
           read(15) startlat, startlon, deltalat, deltalon
           read(15) SNOW
           close(15)
           write(*, '(/, 10x, "Found file:     PSNOW:", A19)') &
                snowdates(isb)(1:datelen)//"      "
           write(*, '(   10x, "Copying to file: SNOW:", A19,//)') &
                reqdate(n)(1:datelen)//"      "
           open(unit=13,file='SNOW:'//reqdate(n)(1:datelen), status='new', &
                &   position = 'rewind', form='unformatted')

           write (13) ifv
           write (13) hdate,xfcst,field, units, Desc, level, nlon, nlat, llflag
           write (13) startlat, startlon, deltalat, deltalon
           write (13) snow
           close (13)
           exit SNOWLOOP

        elseif ( (snowdates(isb) < reqdate(n)) .and. &
             ((isb == nsnow) .or. (snowdates(isb+1).gt.reqdate(n)))) then
           write(*,'(/, 10x,"Found appropriate SNOW file:  PSNOW:",A19)') &
                snowdates(isb)(1:datelen)//"      "
           write(*, '(10x, &
                &"Propagating snow-cover data forward in time to create file:  SNOW:",&
                & A19,/)') reqdate(n)(1:datelen)//"      "
           open(15, file='PSNOW:'//snowdates(isb)(1:datelen), form='unformatted')
           read(15) ifv
           read(15) hdate, xfcst, field, units, Desc, level, nlon, nlat, llflag
           read(15) startlat, startlon, deltalat, deltalon
           read(15) SNOW
           close(15)

! Write a new SNOW file.
           open(unit=13, file='SNOW:'//reqdate(n)(1:datelen), form='unformatted')
           write (13) ifv
           hdate = reqdate(n)
           write (13) hdate, xfcst, field, units, Desc, level, nlon, nlat, llflag
           write (13) startlat, startlon, deltalat, deltalon
           write (13) snow
           close (13)

           write(*, '(/,10x,"New file created:  SNOW:", A19,//)') &
                reqdate(n)(1:datelen)//"      "

           exit SNOWLOOP

        elseif ( snowdates(isb) > reqdate(n)) then
           write(*,'("No snow cover data found appropriate for date ", A19)')&
                reqdate(n)(1:datelen)//"      "
           exit SNOWLOOP

        else
           isb = isb + 1
           if (isb.gt.nsnow) exit SNOWLOOP
        endif
     enddo SNOWLOOP
  enddo FULLOOP

  write(*, '(/,10("*"), /, "End Subroutine SNOWPROP.",/,  10("*")/)')

end subroutine snowprop

