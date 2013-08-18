subroutine sstint(hstart, ntimes, delta_time, sstdates, nsst)
  implicit none
  integer ntimes, nsst, delta_time
  character(len=19) :: hstart
  character(len=19), dimension(nsst) :: sstdates

  real :: startlat, startlon, deltalat, deltalon
  real :: lov, truelat

  real, allocatable, dimension(:,:) :: sst, sstA, sstB

  character(len=24) :: hdateA, hdateB, hdate_output
  character(len=19), dimension(ntimes) :: reqdate
  character(len=9) :: field
  character(len=25) :: units
  character(len=46) :: Desc
  real :: xfcst
  integer :: ifv
  real :: level
  integer :: idim, jdim, llflag
  integer :: isb
! Local, Miscellaneous: 
  integer :: n, idtsA, idtsB
  real :: awt

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
     write(*, '(2x,''Looking for SST at time '', A13,/)') reqdate(n)(1:13)
     SSTLOOP : do
        write(*, '(5x,''sstdates = '', A13)') sstdates(isb)(1:13)
        if (sstdates(isb).eq.reqdate(n)) then
           open(15, file='PSST:'//sstdates(isb)(1:13), form='unformatted')
           read(15) ifv
           read(15) hdateA, xfcst, field, units, Desc, level, idim, jdim, llflag
           read(15) startlat, startlon, deltalat, deltalon, lov, truelat
           if (.not. allocated(ssta)) allocate(ssta(idim, jdim))
           read(15) SSTA
           close(15)
           write(*, '(/, 10x, ''Found file:     PSST:'', A13)') sstdates(isb)(1:13)
           write(*, '(   10x, ''Copying to file: SST:'', A13,//)') reqdate(n)(1:13)
           open(unit=13,file='SST:'//reqdate(n)(1:13), status='new', &
                &   position = 'rewind', form='unformatted')

           write (13) ifv
           write (13) hdateA,xfcst,field, units, Desc, level, idim, jdim, llflag
           write (13) startlat, startlon, deltalat, deltalon, lov, truelat
           write (13) sstA
           close (13)
           exit SSTLOOP

        elseif ( (sstdates(isb).lt.reqdate(n)) .and. &
             (sstdates(isb+1).gt.reqdate(n))) then
           write(*,'(/, 10x,''Found surrounding SST files:  PSST:'',& 
              &  A13,2x,''PSST:'',A13)') sstdates(isb)(1:13), sstdates(isb+1)(1:13)
           write(*, '(10x, ''Interpolating to create file:  SST:'', A13,/)') &
              &  reqdate(n)(1:13)
           open(15, file='PSST:'//sstdates(isb)(1:13), form='unformatted')
           read(15) ifv
           read(15) hdateA, xfcst, field, units, Desc, level, idim, jdim, llflag
           read(15) startlat, startlon, deltalat, deltalon, lov, truelat
           if (.not. allocated(ssta)) allocate(ssta(idim,jdim))
           read(15) SSTA
           close(15)
           open(15, file='PSST:'//sstdates(isb+1)(1:13), form='unformatted')
           read(15) ifv
           read(15) hdateB, xfcst, field, units, Desc, level, idim, jdim, llflag
           read(15) startlat, startlon, deltalat, deltalon, lov, truelat
           if (.not. allocated(sstb)) allocate(sstb(idim,jdim))
           read(15) SSTB
           close(15)

           call geth_idts(reqdate(n), hdateA(1:19), idtsA)
           write(*,'(15x, ''A Time Difference = '', F6.2, '' hours.'')') &
                          float(idtsA) / 3600.
           call geth_idts(hdateB(1:19), reqdate(n), idtsB)
           write(*,'(15x, ''B Time Difference = '', F6.2, '' hours.'')') &
                          float(idtsB) / 3600.
           AWT = 1. - (float(idtsA)/float(idtsA+idtsB))
           if (.not. allocated(sst)) allocate(sst(idim,jdim))
           SST = SSTA * (AWT) + SSTB * (1.-AWT)

! Write a new SST file.
           open(unit=13, file='SST:'//reqdate(n)(1:13), form='unformatted')
           write (13) ifv
           hdate_output = reqdate(n)//".0000"
           write (13) hdate_output,xfcst, field, units, Desc, level, idim, jdim, llflag
           write (13) startlat, startlon, deltalat, deltalon, lov, truelat
           write (13) sst
           close (13)

           write(*, '(/,10x,''New file created:  SST:'', A13,//)') reqdate(n)(1:13)

           hdateA=hdateB
           sstA = sstB
           exit SSTLOOP

        else
           isb = isb + 1
           cycle SSTLOOP
        endif
     enddo SSTLOOP
  enddo FULLOOP

  if (allocated(sst)) deallocate(sst)
  if (allocated(ssta)) deallocate(ssta)
  if (allocated(sstb)) deallocate(sstb)


  write(*, '(/,10(''*''), /, ''End Subroutine SSTINT.'',/,  10(''*'')/)')

end subroutine sstint


