subroutine output(scr2d, hdate, n2lat, nlvl, sstdates, nsst,&
     plvl, maxlvl, snowdates, nsnow, fuldates, nful, delta_time)
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

  use table
  use gridinfo
  use storage_module
  implicit none
  integer :: iunit = 13
  integer :: n2lat
  real, dimension(nlon, n2lat) :: scr2d
  integer :: nsst, nful, nsnow
  character(len=19) :: hdate, sstdates(1000), fuldates(1000), snowdates(1000)
  character(len=24) :: hdate_output
  integer nlvl
  character (LEN=9) :: field
  character (LEN=25) :: units
  character (LEN=46) :: desc
  real :: level
  integer :: sunit = 14
  integer :: llflag = 0
  integer :: delta_time

  integer :: maxlvl
  real , dimension(maxlvl) :: plvl

! Local Miscellaneous
  integer :: k, n, m, ilev
  real :: xplv, xfcst

  integer :: datelen

  if (mod(delta_time, 3600) == 0) then
     datelen = 13
  else if (mod(delta_time, 60) == 0) then
     datelen = 16
  else
     datelen = 19
  endif

  call get_plvls(plvl, maxlvl, nlvl)

  startlat = -90.
  startlon = 0.

  if ((is_there(100000, 'RH      ') .and. &
      (.not.(is_there(200100, 'RH      '))))) then
     call get_storage(100000, 'RH      ', scr2d)
     call put_storage(200100, 'RH      ', scr2d, nlon, n2lat, 999)
  endif

  scr2d = 1.
  do k = 1, nlvl
     if (plvl(k).lt.30000.) then
        if (.not. is_there(nint(plvl(k)), 'RH      ')) then
           call put_storage(nint(plvl(k)), 'RH      ', scr2d, nlon, n2lat, 999)
        endif
     endif
  enddo

  if (nlvl.gt.2) then
     nful=nful+1
     fuldates(nful) = hdate
     call tv_to_t(plvl, nlvl)
  endif

  write(*,119) hdate(1:10), hdate(12:19)
119 format(/,79('#'),//,'Inventory for date = ', A10,1x,A8,/)

  write(*,120) (nameout(n), n = 1, maxout)
120 format('PRES', 3X, 20(1X,A9))
  write(*,FMT='(79("-"))')
  do k = 1, nlvl
     ilev = nint(plvl(k))
     write(*, advance='NO', FMT='(F6.1)') plvl(k)/100.
     do m = 1, maxout
        if (is_there(ilev,nameout(m))) then
           if (m.eq.maxout) then
              write(*, advance='YES', FMT='("    X ")')
           else
              write(*, advance='NO', FMT='("   X      ")')
           endif
        else
           if (m.eq.maxout) then
              write(*, advance='YES', FMT='(" ")')
           else
              write(*, advance='NO', FMT= '("          ")')
           endif
        endif
     enddo
  enddo
  write(*,FMT='(79("-"))')

  if (nlvl.gt.1) then
     open(iunit, file='PON84:'//HDATE(1:datelen), form='unformatted')
  endif

  do n = 1, nlvl
     do m = 1, maxout
        field = nameout(m)
        units = unitout(m)
        Desc  = descout(m)
        level = plvl(n)
        ilev = nint(level)
        if (is_there(ilev,field)) then 
           call get_storage(ilev, field, scr2d)
           if (field.eq.'SST     ') then
              open(sunit, file='PSST:'//HDATE(1:datelen), form='unformatted')
              nsst=nsst+1
              sstdates(nsst) = hdate
!  PREGRID Format Version 2, 3:
              xfcst = 0.
              write (sunit) 3
              hdate_output = hdate
              write (sunit) hdate_output, xfcst, field, units, Desc, level, nlon, n2lat, llflag
              write (sunit) startlat, startlon, deltalat, deltalon
              write (sunit) scr2d

              close(sunit)
           elseif (field.eq.'SNOWCOVR') then
              open(sunit, file='PSNOW:'//HDATE(1:datelen), form='unformatted')
              nsnow=nsnow+1
              snowdates(nsnow) = hdate
!  PREGRID Format Version 2, 3:
              xfcst = 0.
              write (sunit) 3
              hdate_output = hdate
              write (sunit) hdate_output, xfcst, field, units, Desc, level, &
                   nlon, n2lat, llflag
              write (sunit) startlat, startlon, deltalat, deltalon
              write (sunit) scr2d
              close(sunit)
           else


!  PREGRID Format Version 2, 3:
              xfcst = 0
              write (iunit) 3
              hdate_output = hdate
              write (iunit) hdate_output, xfcst, field, units, Desc, level, nlon, n2lat, llflag
              write (iunit) startlat, startlon, deltalat, deltalon
              write (iunit) scr2d

           endif
        endif
     enddo
  enddo

  close(iunit)

end subroutine output
