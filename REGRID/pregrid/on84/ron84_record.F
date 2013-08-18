subroutine ron84_record(on84flnm, iuarr, iunit, krec, klenp, ierr)
  integer iunit
  parameter(ksiz=240000)
  character*(ksiz) krec
  character(LEN=*) :: on84flnm
  logical lopen

#include <con84.incl>

      DIMENSION IDIM(70),JDIM(70)
      DATA IDIM/192,192,192,53,17,53,47,0,116,143, &
           216,286,74,36,108,40,39,17,0,47,        &
           45,73,73,29,31,53,53,65,65,145,         &
           145,327,31,181,181,228,41,145,145,181,  &
           181,34,0,65,65,97,97,113,0,129,         &
           129,257,257,117,35,87,87,15,1,79,       &
           0,0,0,0,0,0,0,0,0,0/
      DATA JDIM/49,49,49,57,30,57,51,0,44,1,       &
           1,1,23,16,1,1,40,13,0,51,               &
           59,19,19,27,21,57,45,65,65,37,          &
           37,1,24,46,46,1,38,37,37,46,            &
           46,25,0,65,65,25,25,89,0,129,           &
           129,257,257,51,30,71,71,16,1,67,        &
           10*0/

      integer year, month, day, hour
      integer, dimension(255) :: iuarr

      if (iuarr(iunit).eq.0) then
         print*, 'Opening: '//on84flnm
         call COPEN(iunit, ifd, on84flnm, 1, ierr, -1)
         if (ierr.ne.0) then
            ierr = 2
            return
         endif
         iuarr(iunit) = ifd
      endif

      call bnread(iuarr(iunit), krec, 48, isz, ierr, 1)
#if defined (DEC) || defined (ALPHA) 
      call swap4(krec, 48)
#endif
      if (ierr.ne.0) return

      call gbyte(krec, no, 0, 12)

      call gbyte(krec, ns1, 12, 12)

      call gbyte(krec, nf1, 24, 8)

      call gbyte(krec, nt, (1*32)+0, 4)

      call gbyte(krec, nc1, (1*32)+4, 20)
      if (nc1.gt.524288) nc1 = 524288-nc1

      call gbyte(krec, ne1, (1*32)+24, 8)
      if (ne1.gt.128) ne1 = 128-ne1

      call gbyte(krec, nm, (2*32)+0, 4)

      call gbyte(krec, nx, (2*32)+4, 8)

      call gbyte(krec, ns2, (2*32)+12, 12)

      call gbyte(krec, nf2, (2*32)+24, 8)

      call gbyte(krec, ni, (3*32)+0, 4)

      call gbyte(krec, nc2, (3*32)+4, 20)
      if (nc2.gt.524288) nc2 = 524288-nc2

      call gbyte(krec, ne2, (3*32)+24, 8)
      if (ne2.gt.128) ne2 = 128-ne2

      call gbyte(krec, ncd, (4*32)+0, 8)

      call gbyte(krec, ncm, (4*32)+8, 8)

      call gbyte(krec, nks, (4*32)+16, 8)

      call gbyte(krec, nk, (4*32)+24, 8)

      call gbyte(krec, nun, (5*32)+0, 4)

      call gbyte(krec, nrw, (5*32)+4, 12)

      call gbyte(krec, nnw, (5*32)+16, 16)

      call gbyte(krec, nyr, (6*32)+0, 8)
      call gbyte(krec, nmo, (6*32)+8, 8)
      call gbyte(krec, ndy, (6*32)+16, 8)
      call gbyte(krec, nhr, (6*32)+24, 8)

      call gbyte(krec, nr, (7*32)+0, 8)
      call gbyte(krec, ng, (7*32)+8, 8)

      call gbyte(krec, nj, (7*32)+16, 16)

      call gbyte(krec, nbb, (8*32)+0, 16)

      call gbyte(krec, nz, (8*32)+16, 16)

      call gbyte(krec, nbsn, (9*32), 1)
      call gbyte(krec, nbex, (9*32)+1, 7)
      call gbyte(krec, nbfr, (9*32)+8, 24)

      call gbyte(krec, nun7, (10*32), 8)

      call gbyte(krec, nun8, (10*32)+8, 8)

      call gbyte(krec, NSCL, (10*32)+16, 16)

      sfc1 = float(nc1)*10.**NE1
      sfc2 = float(nc2)*10.**NE2

      SMUL=1.
      IF(SFC1 .LT. 1.) SMUL=1000000.
      NSFC1=SMUL*SFC1+.001
      NSFC2=SMUL*SFC2+.001

      IF (NS1.eq.128) THEN
         NSFC1 = 2013
      elseif (NS1.EQ.129) THEN
         NSFC1=2001
      endif

      BASE=FLOAT(NBFR)*16.**(NBEX-70)
      IF(NSCL .GE. 32768) NSCL=NSCL-65536
      IF(NBSN .NE. 0) BASE=-BASE

      NIDIM = IDIM(nk+1)
      NJDIM = JDIM(nk+1)

      if ((nyr > 83) .or. (nyr == 83 .and. nmo > 2)) then
         ! use 10784-byte record lengths
         call bnread(iuarr(iunit), krec(49:49), nbb+6-48, isz, ierr, 1)
      else
         ! use 10792-byte record lengths
         call bnread(iuarr(iunit), krec(49:49), nbb+14-48, isz, ierr, 1)
      endif
#if defined (DEC) || defined (ALPHA)
      call swap4(krec(49:49), isz)
#endif
      end
