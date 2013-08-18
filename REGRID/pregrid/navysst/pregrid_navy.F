program pregrid_navy

  use table
  use gridinfo
  implicit none
  logical, external :: is_there

  integer :: nunit1 = 12

  integer, parameter :: alength = 60000
  real, dimension (alength) :: array, scratch

  character(len=19),  dimension(1000) :: sstdates
  integer :: nsst=0

  integer :: ndim

  integer :: iplvl
  integer , parameter :: maxlvl = 50
  real , dimension(maxlvl) :: plvl

  integer :: nlvl
  integer :: nvar 

  integer :: year, month, day, hour, minute, second
  integer :: k, n
  real :: level, xplv
  character (LEN=9) ::  field

  logical :: readit = .TRUE.
  logical :: gdone = .FALSE.

  character (LEN=19) :: HSTART, HEND, HDATE
  character (LEN=19) :: hsave = '0000-00-00_00:00:00'
  character (LEN=8) :: datatype
  integer, parameter :: north=29
  integer, parameter :: south=30
  logical :: lnorth = .FALSE.
  logical :: lsouth = .FALSE.
  integer :: itime
  integer :: iprint
  integer :: ierr

  character(LEN=9) :: FLNM = 'NAVYFILE '

  integer :: ntimes
  integer :: interval
  logical :: ordered
  logical :: debug_print

  call pregrid_version

  call read_pregrid_namelist(hstart, hend, interval, ntimes, &
       ordered, debug_print, iprint)

  call parse_table(debug_print)

  print*, 'namvar = ', namvar

  flnm = flnm(1:8)//char(ichar('A')-1)

  nlvl = 0
  plvl = -999.
  call clear_storage

  LOOP2 : DO

     flnm(9:9) = char(ichar(flnm(9:9)) + 1)
     print*, 'Updating flnm: flnm = '//flnm

     LOOP1 : DO

        call rd_navy(nunit1, flnm, hdate, hstart, hend, field, level, &
             array, ierr)
        if (ierr.eq.1) then
           cycle LOOP2
        else if (ierr.eq.2) then
           stop
        endif

        print*, 'field, hdate: ', field, hdate
        if ((field.eq.'PRESST').or.(field.eq.'POSTSST')) then
           iplvl = int(level)
           ndim = idim*jdim
           call put_storage(iplvl, field, array(1:ndim), idim, jdim, &
                hdate)
           call print_storage
        endif

        if (hdate.lt.hstart) cycle LOOP1
        if (hdate.gt.hend) exit loop2

        call writsst(Array, hdate, idim, jdim)
        nsst = nsst + 1
        sstdates(nsst) = hdate

     enddo LOOP1
  enddo LOOP2

  call output(scratch, sstdates, nsst, hstart, hend)

  call sstint(hstart, ntimes, interval, sstdates, nsst)

! Now delete the temporary files:
  write(*, '(/,10("*"), /, &
       &    "Deleting temporary files created by PREGRID_NAVY...",/, &
       &    10("*")/)')

  call file_delete(sstdates, nsst, "PSST:")

  write(*, '(/,10("*"), /, &
       &    "Done deleting temporary files",/, &
       &    10("*")/)')

  write(*, '(/,10("*"), /, &
       &    "Normal termination of program PREGRID_NAVY",/, &
       &    10("*")/)')

end program pregrid_navy

subroutine file_delete(hdates, ndates, root)
  implicit none
  integer :: ndates
  character(len=19), dimension(ndates) :: hdates
  character(len=*) :: root

  logical :: lexist
  integer :: idate
  character(len=120) :: flnm

  character(len=120) :: fmt

  do idate = 1, ndates
     flnm=trim(root)//hdates(idate)(1:13)
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

subroutine writsst(Array, hdate, idim, jdim)

  integer :: idim, jdim
  real, dimension(idim,jdim) :: Array
  character(len=19) :: hdate
  character (len=24) :: hdate_output
  integer :: sunit = 15
  character(LEN=9) :: field = 'SST'
  character(LEN=25) :: units = 'K               '
  character(LEN=46) :: desc = 'Sea-Surface Temperature'
  real :: plevel = 200100.
  integer :: llflag = 5

  real :: startlat, startlon, deltalat, deltalon, lov, truelat

  print*, 'Array = ', Array(1:10, 1:10)

  startlat = -19.11821
  startlon = 235.
  deltax = 381.
  deltay = 381.
  Lov = 280.
  truelat = 60.

  open(sunit, file='PSST:'//HDATE(1:13), form='unformatted')
!  PREGRID Format Version 3:
  xfcst = 0.
  write (sunit) 3
  hdate_output = hdate
  write (sunit) hdate_output, xfcst, field, units, Desc, plevel, idim, jdim, llflag
  write (sunit) startlat, startlon, deltax, deltay, Lov, truelat
  write (sunit) Array
  close(sunit)

end subroutine writsst

