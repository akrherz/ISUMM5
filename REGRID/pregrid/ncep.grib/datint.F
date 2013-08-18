subroutine datint(fuldates, nful, hstart, ntimes, delta_time)

! Recent changes:                                                             !
!    2001-02-14:                                                              !
!               - Allow file names to have date stamps out to minutes or      !
!                 seconds, if the user requests a time interval (in seconds)  !
!                 that is not evenly divisible into hours or minutes.         !

  use storage_module
  character(len=19), dimension(nful) :: fuldates
  character(len=19) :: hstart
  integer :: ntimes, delta_time

  integer :: ifv
  character(len=19) :: hdate
  character(len=24) :: hdate_output, jdate
  character(len=9) :: field
  character(len=25) :: units
  character(len=46) :: desc
  real :: level
  real, allocatable, dimension(:,:) :: scr2d, bfr2d

  integer :: datelen

  if (mod(delta_time, 3600) == 0) then
     datelen = 13
  else if (mod(delta_time, 60) == 0) then
     datelen = 16
  else
     datelen = 19
  endif

  write(*, '(/,10(''*''), /, ''Subroutine DATINT:'',/,2x,  &
       &    ''Interpolating files to fill in any missing data...'',/, &
       &    10(''*'')/)')

  TIMELOOP : do itime = 1, ntimes
     call geth_newdate(hdate, hstart, (itime-1)*delta_time)
     print*, 'hdate = ', hdate
     do iful = 1, nful
        if (fuldates(iful).eq.hdate) then

           open(10, file='PNG:'//hdate(1:datelen), status='old', &
                form='unformatted')
           call clear_storage
           open(11, file='NCEP:'//hdate(1:datelen), form='unformatted')

           READLOOP0 : do
              read(10,iostat=ierr) ifv
              if (ierr.ne.0) exit READLOOP0
              write(11) ifv
              read (10) jdate, xfcst, field, units, desc, level, idim,  &
                   jdim, llflag
              write(11) jdate, xfcst, field, units, desc, level, idim,  &
                   jdim, llflag
              read (10) startlat, startlon, deltalat, deltalon
              write(11) startlat, startlon, deltalat, deltalon
              allocate(scr2d(idim, jdim))
              read (10) scr2d
              write(11) scr2d
              deallocate(scr2d)
           enddo READLOOP0
           close(10)
           close(11)

           cycle TIMELOOP
        else if ((fuldates(iful).lt.hdate) .and. &
             (fuldates(iful+1).gt.hdate) )then

           call geth_idts(hdate, fuldates(iful), idtsA)
           print*, 'A Time Difference = ', float(idtsA) / 3600.
           call geth_idts(fuldates(iful+1), hdate, idtsB)
           print*, 'B Time Difference = ', float(idtsB) / 3600.
           AWT = 1. - (float(idtsA)/float(idtsA+idtsB))
           print*, 'AWT, BWT = ', AWT, (1.-AWT)

           open(11, file='NCEP:'//hdate(1:datelen), form='unformatted')
           call clear_storage
           open(10, file='PNG:'//fuldates(iful)(1:datelen), status='old', &
                form='unformatted')
           READLOOP1 : do
              read (10,end=44) ifv
              read (10) jdate, sfcst, field, units, desc, level, idim, jdim, llflag
              read (10) startlat, startlon, deltalat, deltalon
              allocate(scr2d(idim, jdim))
              read (10) scr2d
              call put_storage(nint(level), field, scr2d, idim, jdim, 999)
              deallocate(scr2d)
           enddo READLOOP1
44         close(10)

           open(10, file='PNG:'//fuldates(iful+1)(1:datelen), status='old', &
                form = 'unformatted')
           READLOOP2 : do
              read (10,end=45) ifv
              read (10) jdate, xfcst, field, units, desc, level, idim, jdim, llflag
              read (10) startlat, startlon, deltalat, deltalon
              allocate(scr2d(idim, jdim))
              read (10) scr2d
              if (is_there(nint(level), field)) then
                 allocate(bfr2d(idim,jdim))
                 call get_storage(nint(level), field, bfr2d, idim, jdim)
                 scr2d = bfr2d * (AWT) + scr2d * (1.-AWT)
                 write(11) ifv
                 hdate_output = hdate
                 write(11) hdate_output, xfcst, field, units, desc, level, idim, jdim, llflag
                 write(11) startlat, startlon, deltalat, deltalon
                 write(11) scr2d
              else
                 print*, 'hdate = ', hdate
                 print*, 'Problem:  ', fuldates
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

  write(*, '(/,10(''*''), /, ''End Subroutine DATINT.'',/,  10(''*'')/)')

end subroutine datint

