subroutine datint(fuldates, nful, hstart, ntimes, delta_time)
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
  use storage_module
  character(len=19), dimension(nful) :: fuldates
  character(len=19) :: hstart
  integer :: ntimes, delta_time

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
       &    ''Interpolating 3-d files to fill in any missing data...'',/, &
       &    10(''*'')/)')

  idts = delta_time
  TIMELOOP : do itime = 1, ntimes
     call geth_newdate(hdate, hstart, (itime-1)*idts)
     write(*, '(2x,''Looking for data at time '', A19,/)') hdate(1:datelen)//"      "
     do iful = 1, nful
        if (fuldates(iful).eq.hdate) then
           write(*, '(/, 10x, ''Found file:     PON84:'', A19)') &
                hdate(1:datelen)//"      "
           write(*, '(   10x, ''Copying to file: ON84:'', A19,//)') &
                hdate(1:datelen)//"      "

           open(10, file='PON84:'//hdate(1:datelen), form='unformatted', status='old')
           call clear_storage
           open(11, file='ON84:'//hdate(1:datelen), status='new', &
                form='unformatted')
           READLOOP0 : do
              read(10, end=43) ifv
              write(11)  ifv
              read (10) jdate, xfcst, field, units, desc, level, idim, jdim, llflag
              write(11) jdate, xfcst, field, units, desc, level, idim, jdim, llflag
              read (10) startlat, startlon, deltalat, deltalon
              write(11) startlat, startlon, deltalat, deltalon
              allocate(scr2d(idim, jdim))
              read (10) scr2d
              write(11) scr2d
              deallocate(scr2d)
           enddo READLOOP0
43         close(10)
           close(11)
           
           cycle TIMELOOP
        else if ((fuldates(iful).lt.hdate) .and. &
             (fuldates(iful+1).gt.hdate) )then

           write(*,'(/, 10x,''Found surrounding files:      PON84:'',& 
              &  A19,2x,''PON84:'',A19)') fuldates(iful)(1:datelen)//"      ",&
              fuldates(iful+1)(1:datelen)//"      "
           write(*, '(10x, ''Interpolating to create file:  ON84:'', A19,/)') &
              &  hdate(1:datelen)//"      "
           call geth_idts(hdate, fuldates(iful), idtsA)
           write(*,'(15x, ''A Time Difference = '', F6.2, '' hours.'')') &
                          float(idtsA) / 3600.
           call geth_idts(fuldates(iful+1), hdate, idtsB)
           write(*,'(15x, ''B Time Difference = '', F6.2, '' hours.'')') &
                          float(idtsB) / 3600.
           AWT = 1. - (float(idtsA)/float(idtsA+idtsB))

           open(10, file='PON84:'//fuldates(iful)(1:datelen), form='unformatted', &
                status='old')
           call clear_storage
           READLOOP1 : do
              read(10, end=44) ifv
              read (10) jdate, xfcst, field, units, desc, level, idim, jdim, llflag
              read (10) startlat, startlon, deltalat, deltalon
              allocate(scr2d(idim, jdim))
              read (10) scr2d
              call put_storage(nint(level), field, scr2d, idim, jdim, 999)
              deallocate(scr2d)
           enddo READLOOP1
44         close(10)

           open(10, file='PON84:'//fuldates(iful+1)(1:datelen), status='old', &
                form = 'unformatted')
           open(11, file='ON84:'//hdate(1:datelen), status='new', form='unformatted')
           READLOOP2 : do
              read (10,END=45) ifv
              read (10) jdate, xfcst, field, units, desc, level, idim, jdim, llflag
              read (10) startlat, startlon, deltalat, deltalon
              allocate(scr2d(idim, jdim))
              read (10) scr2d
              if (is_there(nint(level), field)) then
                 allocate(bfr2d(idim,jdim))
                 call get_storage(nint(level), field, bfr2d)
                 scr2d = bfr2d * (AWT) + scr2d * (1.-AWT)
                 write(11) ifv
                 hdate_output = hdate
                 write(11) hdate_output, xfcst, field, units, desc, level, idim, jdim, llflag
                 write(11) startlat, startlon, deltalat, deltalon
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

  write(*, '(/,10(''*''), /, ''End Subroutine DATINT.'',/,  10(''*'')/)')

end subroutine datint
