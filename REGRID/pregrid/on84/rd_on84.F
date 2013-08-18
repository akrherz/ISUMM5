subroutine rd_on84(on84flnm, IUNIT, iuarr, &
     level, field, hdate, Array, hemis, ierr)
  use table
  use gridinfo

  integer :: iunit
  integer hemis
  integer, dimension(255) :: iuarr

  parameter (KLENP=60000)
  real , dimension(KLENP) :: Array

  integer krec(klenp)

  character*19 hdate
  integer year, month, day, hour, minute, second
  real level
  character*9 field
  character (LEN=*) :: on84flnm

#include <con84.incl>

  call ron84_record(on84flnm, iuarr, iunit, krec, klenp, ierr)
  if (ierr.ne.0) return

      field = 'NULL'
      do n = 1, maxvar
         if (no.eq.gcode(n)) then
            if (ns1 .eq. lcode(n)) then
               field = namvar(n)
            endif
         endif
      enddo

      level = float(nsfc1)*100.  ! Put level in Pa
      nlon = nidim
      nlat = njdim
      ndim = nj
      hemis = nk
      if (nk.eq.29) then
         startlat = 0.0
         startlon = 0.0
         deltalat = 2.5
         deltalon = 2.5
      else if (nk.eq.30) then
         startlat = -90.0
         startlon = 0.0
         deltalat = 2.5
         deltalon = 2.5
      else
         print*, 'Unknown nk: ', nk
         field = 'NULL'
         return
         print*, 'Stop in rd_on84.'
         stop
      endif
         
      year = nyr + 1900
      month = nmo
      day = ndy
      hour = nhr

      minute = 0
      second = 0

      call build_hdate(hdate,year,month,day,hour,minute,second)

      if (ns1.eq.145) field  = 'NULL'
      if (ns1.eq.144) field  = 'NULL'
      if (ns1.eq.130) field  = 'NULL'

      if (field.ne.'NULL') then
         call unpack_on84(krec, klenp, Array, ndim, ierr)
      else
         write(25, *) 'Not using field number ', no, ns1, level
      endif

      if ((field.eq.'PRESSURE').or.(field.eq.'PSFC')) then  ! Convert pressure from mb to Pa
         do k = 1, ndim
            array(k) = array(k) * 1.E2
         enddo
      endif
      end

