subroutine output(hdate, nlvl, maxlvl, plvl, interval, iflag)
!                                                                             !
!*****************************************************************************!
!                                                                             !
! Recent changes:                                                             !
!                                                                             !
!    2002-05-16:                                                              !
!               - Handle the Mercator projection.                             !
!                 This change also required changes to rrpr.F, rd_grib.F,     !
!                 datint.F, gribcode.F                                        !
!                                                                             !
!    2001-02-14:                                                              !
!               - Allow file names to have date stamps out to minutes or      !
!                 seconds, if the user requests a time interval (in seconds)  !
!                 that is evenly divisible into minutes or hours.  This       !
!                 change involved adding INTERVAL to the argument list.       !
!                 INTERVAL is checked for divisibility into 3600 (for hours)  !
!                 or 60 (for minutes).  The local variable DATELEN is set     !
!                 to be the number of characters to use in our character      !
!                 dates.  Valid values for DATELEN are 13 (for hours),        !
!                 16 (for minutes), and 19 (for seconds).                     !
!                                                                             !
!                 This change also requires changes to pregrid_grib.F,        !
!                 rrpr.F, datint.F, file_delete.F                             !
!                                                                             !
!*****************************************************************************!

  use table
  use gridinfo
  use storage_module
  use filelist
  implicit none
  character(LEN=19) :: hdate
  character(LEN=24) :: hdate_output
  integer :: iunit = 13

  real, pointer, dimension(:,:) :: scr2d

  integer :: maxlvl
  integer nlvl
  real , dimension(maxlvl) :: plvl
  character (LEN=9) :: field
  real :: level
  integer :: sunit = 14
  integer :: interval
  integer :: iflag
! Local Miscellaneous
  integer :: k, n, m, ilev
  real :: xplv
  real :: xfcst = 0.
  character (LEN=25) :: units
  character (LEN=46) :: Desc
  logical lopen

! DATELEN:  length of date strings to use for our output file names.
  integer :: datelen

! Decide the length of date strings to use for output file names.  
! DATELEN is 13 for hours, 16 for minutes, and 19 for hours.
  if (mod(interval,3600) == 0) then
     datelen = 13
  elseif (mod(interval,60) == 0) then
     datelen = 16
  else
     datelen = 19
  endif
 
  call get_plvls(plvl, maxlvl, nlvl)

  write(*,119) hdate(1:10), hdate(12:19)
119 format(/,79('#'),//,'Inventory for date = ', A10,1x,A8,/)

  write(*,advance='NO', fmt='("PRES", 2x)')
  WRTLOOP : do n = 1, maxvar
     do k = 1, n-1
        if (namvar(k).eq.namvar(n)) cycle WRTLOOP
     enddo
     write(*,advance='NO', fmt='(1x,A8)') namvar(n)
  enddo WRTLOOP
  write(*,advance='YES', fmt='(1x)')

  write(*,FMT='(79("-"))')
  KLOOP : do k = 1, nlvl
     if ((iflag.eq.2).and.(plvl(k).gt.200100) .and. (plvl(k).lt.200200)) then
        cycle KLOOP
     endif
     ilev = nint(plvl(k))
     write(*, advance='NO', FMT='(F6.1)') plvl(k)/100.
     MLOOP : do m = 1, maxvar
        do n = 1, m-1
           if (namvar(m).eq.namvar(n)) cycle MLOOP
        enddo
        if (is_there(ilev,namvar(m))) then
           write(*, advance='NO', FMT='("  X      ")')
        else
           write(*, advance='NO', FMT='("         ")')
        endif
     enddo MLOOP
     write(*,advance='YES', fmt='(1x)')
  enddo KLOOP
  write(*,FMT='(79("-"))')
  
  if (iflag.eq.1) then
     if (nfiles.eq.0) then
        open(iunit, file='PFILE:'//HDATE(1:datelen), form='unformatted', &
             position='REWIND')
        nfiles = nfiles + 1
        filedates(nfiles)(1:datelen) = hdate(1:datelen)
     else
        DOFILES : do k = 1, nfiles
           if (hdate(1:datelen).eq.filedates(k)(1:datelen)) then
              open(iunit, file='PFILE:'//HDATE(1:datelen), form='unformatted',&
                   position='APPEND')
           endif
        enddo DOFILES
        inquire (iunit, OPENED=LOPEN)
        if (.not. LOPEN) then
           open(iunit, file='PFILE:'//HDATE(1:datelen), form='unformatted', &
                position='REWIND')
           nfiles = nfiles + 1
           filedates(nfiles)(1:datelen) = hdate(1:datelen)
        endif
     endif
  else if (iflag.eq.2) then
     open(iunit, file='FILE:'//HDATE(1:datelen), form='unformatted', &
          position='REWIND')
  endif

  NLOOP : do n = 1, nlvl

     OUTLOOP : do m = 1, maxvar
        field = namvar(m)
        do k = 1, m-1
           if (field.eq.namvar(k)) cycle OUTLOOP
        enddo
        level = plvl(n)
        if ((iflag.eq.2).and.(level.gt.200100) .and. (level.lt.200200)) then
           cycle NLOOP
        endif
        ilev = nint(level)
        desc = ddesc(m)
        if (iflag.eq.2) then
           if (desc.eq.' ') cycle OUTLOOP
        endif
        units = dunits(m)
        if ((iflag.eq.1).or.(iflag.eq.2.and.desc(1:1).ne.' ')) then
           if (is_there(ilev,field)) then 
              call get_dims(ilev, field)

              call refr_storage(ilev, field, scr2d, map%nx, map%ny)

              write(iunit) 3
              hdate_output = hdate
              write (iunit) hdate_output, xfcst, field, units, Desc, level,&
                   map%nx, map%ny, map%igrid
              if (map%igrid.eq.3) then ! lamcon
                 write (iunit) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                      map%truelat1, map%truelat2
              elseif (map%igrid.eq.5) then ! Polar Stereographic
                 write (iunit) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                      map%truelat1
              elseif (map%igrid.eq.0)then ! lat/lon
                 write (iunit) map%lat1, map%lon1, map%dy, map%dx
              elseif (map%igrid.eq.1)then ! Mercator
                 write (iunit) map%lat1, map%lon1, map%dy, map%dx, map%truelat1
              else
                 write(*,'("Unrecognized map%igrid: ", I20)') map%igrid
                 stop
              endif
              write (iunit) scr2d

              nullify(scr2d)

           endif
        endif
     enddo OUTLOOP
  enddo NLOOP

  close(iunit)

end subroutine output

