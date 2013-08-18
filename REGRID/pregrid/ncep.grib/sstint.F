subroutine sstint(hstart, ntimes, delta_time, sstdates, nsst)
! Recent changes:                                                             !
!   2001-02-14:  Allow output file names to have date stamps out to minutes   !
!                or seconds, if the user requests a time interval (in         !
!                seconds) that is not evenly divisible into hours or minutes. !
!                                                                             !
  implicit none
  integer ntimes, nsst, delta_time
  character(len=19) :: hstart
  character(len=19), dimension(nsst) :: sstdates

  integer, parameter :: idim=145
  integer, parameter :: jdim=73

  real :: startlat, startlon, deltalat, deltalon

  real, dimension(idim,jdim) :: sst, sstA, sstB

  character(len=19), dimension(ntimes) :: reqdate
  character(len=24) :: hdate_output, hdateA, hdateB
  character(len=9) :: field
  character(len=25) :: units
  character(len=46) :: Desc
  real :: xfcst
  integer :: ifv
  real :: level
  integer :: nlon, nlat, llflag
  integer :: isb
! Local, Miscellaneous: 
  integer :: n, idtsA, idtsB
  real :: awt

  integer :: datelen

  if (mod(delta_time, 3600) == 0) then
     datelen = 13
  else if (mod(delta_time, 60) == 0) then
     datelen = 16
  else
     datelen = 19
  endif

  close(15)

  write(*, '(/,10(''*''), /, ''Subroutine SSTINT:'',/,2x,  &
       &    ''Interpolating SST files to fill in any missing data...'',/, &
       &    10(''*'')/)')

  reqdate(1) = hstart
  do n = 2, ntimes
     call geth_newdate(reqdate(n), reqdate(n-1), delta_time)
  enddo

  hdateA = '0000-00-00:00:00:00.0000'
  isb = 1
  FULLOOP : do n = 1, ntimes
     write(*, '(2x,''Looking for SST at time '', A19,/)') reqdate(n)(1:datelen)//"      "
     SSTLOOP : do
        write(*, '(5x,''sstdates = '', A19)') sstdates(isb)(1:datelen)//"      "
        if (sstdates(isb).eq.reqdate(n)) then
           open(15, file='PSST:'//sstdates(isb)(1:datelen), form='unformatted')
           read(15) ifv
           read(15) hdateA, xfcst, field, units, Desc, level, nlon, nlat, llflag
           read(15) startlat, startlon, deltalat, deltalon
           read(15) SSTA
           close(15)
           write(*, '(/, 10x, ''Found file:     PSST:'', A19)')  &
                sstdates(isb)(1:datelen)//"      "
           write(*, '(   10x, ''Copying to file: SST:'', A19,//)') &
                reqdate(n)(1:datelen)//"      "
           open(unit=13,file='SST:'//reqdate(n)(1:datelen), status='new', &
                &   position = 'rewind', form='unformatted')

           write (13) ifv
           write (13) hdateA,xfcst,field, units, Desc, level, nlon, nlat, llflag
           write (13) startlat, startlon, deltalat, deltalon
           write (13) sstA
           close (13)
           exit SSTLOOP

        elseif ( (sstdates(isb).lt.reqdate(n)) .and. &
             (sstdates(isb+1).gt.reqdate(n))) then
           write(*,'(/, 10x,''Found surrounding SST files:  PSST:'',& 
              &  A19,2x,''PSST:'',A19)') sstdates(isb)(1:datelen)//"      ",&
              sstdates(isb+1)(1:datelen)//"      "
           write(*, '(10x, ''Interpolating to create file:  SST:'', A19,/)') &
              &  reqdate(n)(1:datelen)//"      "
           open(15, file='PSST:'//sstdates(isb)(1:datelen), form='unformatted')
           read(15) ifv
           read(15) hdateA, xfcst, field, units, Desc, level, nlon, nlat, llflag
           read(15) startlat, startlon, deltalat, deltalon
           read(15) SSTA
           close(15)
           open(15, file='PSST:'//sstdates(isb+1)(1:datelen), form='unformatted')
           read(15) ifv
           read(15) hdateB, xfcst, field, units, Desc, level, nlon, nlat, llflag
           read(15) startlat, startlon, deltalat, deltalon
           read(15) SSTB
           close(15)

           call geth_idts(reqdate(n), hdateA(1:19), idtsA)
           write(*,'(15x, ''A Time Difference = '', F6.2, '' hours.'')') &
                          float(idtsA) / 3600.
           call geth_idts(hdateB(1:19), reqdate(n), idtsB)
           write(*,'(15x, ''B Time Difference = '', F6.2, '' hours.'')') &
                          float(idtsB) / 3600.
           AWT = 1. - (float(idtsA)/float(idtsA+idtsB))
           SST = SSTA * (AWT) + SSTB * (1.-AWT)

! Write a new SST file.
           open(unit=13, file='SST:'//reqdate(n)(1:datelen), form='unformatted')
           write (13) ifv
           hdate_output = reqdate(n)//".0000"
           write (13) hdate_output,xfcst, field, units, Desc, level, nlon, nlat, llflag
           write (13) startlat, startlon, deltalat, deltalon
           write (13) sst
           close (13)

           write(*, '(/,10x,''New file created:  SST:'', A19,//)') &
                reqdate(n)(1:datelen)//"      "

           hdateA=hdateB
           sstA = sstB
           exit SSTLOOP

        else
           isb = isb + 1
           if (isb.gt.nsst) exit SSTLOOP
        endif
     enddo SSTLOOP
  enddo FULLOOP

  write(*, '(/,10(''*''), /, ''End Subroutine SSTINT.'',/,  10(''*'')/)')

end subroutine sstint


