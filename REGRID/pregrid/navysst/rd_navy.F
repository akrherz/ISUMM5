subroutine rd_navy(IUNIT, flnm, hdate, hstart, hend, field, xlvl, &
     Array, irderr)
  use table
  use gridinfo

  integer :: iunit
  integer :: hemis

  parameter (KLENP=60000)
  real , dimension(KLENP) :: Array
  character (LEN=19) :: hdate, hstart, hend

  integer krec(klenp)

  integer :: year, month, day, hour, minute, second
  character(len=*) :: field

  integer :: form, nyr, nmo, ndy, nhr, var, bias, scale
  real (kind(0.0D0)) :: base

  integer, dimension(255) :: iuarr = 0

  character(LEN=*) :: flnm

  integer, allocatable, dimension(:) :: iarr

  integer, parameter :: irecsize = 7976

  irderr = 0

  minute = 0
  second = 0
  field = 'NULL'

  if (iuarr(iunit).eq.0) then
     call COPEN(iunit, ifd, flnm, 1, ierr, 1)
     if (ierr.ne.0) then
        stop
     endif
     iuarr(iunit) = ifd
  endif

  call bnread(iuarr(iunit), krec, 24, isz, ist, 1) ! 24 Bytes fit into 6 
                                                   ! integers of KREC
#if defined (DEC) || defined (ALPHA) 
  call swap4(krec, 24)
#endif
  if (ist.eq.1) then
     irderr = ist
     call cclose(iuarr(iunit), ist, ierr)
     iuarr(iunit) = 0
     return
  endif
     
  call gbyte(krec, form, 0, 6)
  call gbyte(krec, nyr, 6, 7)
  call gbyte(krec, nmo, 13, 4)
  call gbyte(krec, ndy, 17, 5)
  call gbyte(krec, nhr, 22, 5)
  call gbyte(krec, var, 37, 9)
  call gbyte(krec, bias, 78, 16)
  call gbyte(krec, scale, 94, 16)
  call gbyte(krec, base, 120, 60) ! Total of 180 Bits for the header info.
                                  ! (22.5 bytes)
  print*, 'nyr, nmo, ndy, nhr = ', nyr, nmo, ndy, nhr

  if (form.eq.3) then
     idim = 63
     jdim = 63
     ibskp = 133- 6
     hemis = 29
     xlvl = 200100.
     iby = (idim*jdim*16)/8  ! 7938 bytes of data
  else
     write(*,'(''form = '', I3)') form
     write(*,'(''Is this a NAVY SST file?'')')
     write(*,'(''Has this NAVY SST file been converted from COS-Blocked format?'')') 
     stop
  endif

  do n = 1, maxvar
     if (var.eq.gcode(n)) then
        field = namvar(n)
     endif
  enddo

  year = nyr + 1900
  month = nmo
  day = ndy
  hour = nhr
  call build_hdate(hdate,year,month,day,hour,minute,second)
  call geth_idts(hdate, hstart, idts)

  if (hdate.lt.hstart) field = 'PRESST  '
  if (hdate.gt.hend)   field = 'POSTSST '
  
  if (idts.lt.-5*24*3600) field = 'NULL    '

  ibskip = irecsize-24


  if (field.ne.'NULL    ') then
     call bnseek(iuarr(iunit), -24, 0, 1)
     call bnread(iuarr(iunit), krec, irecsize, isz, ist, 1)
#if defined (DEC) || defined (ALPHA) 
     call swap4(krec, isz)
#endif

     if (ist.eq.1) then
        irderr = ist
        call cclose(iuarr(iunit), ist, ierr)
        iuarr(iunit) = 0
        print*, 'close 2:'
        return
     endif

     allocate(iarr(idim*jdim))
     call gbytes(krec, iarr, 180, 16, 0, idim*jdim)
     array(1:idim*jdim) = 273.15 + sngl(base) + &
          float(iarr(1:idim*jdim)-bias)*(2.**(scale-bias))
     print*, 'array(100) = ', array(100)
     deallocate(iarr)
  else
     call bnseek(iuarr(iunit), ibskip, 0, 1)
  endif

  end

