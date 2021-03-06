program pltfmt
  implicit none
!
! Utility program to plot up the files created by pregrid.
! Uses NCAR graphics routines.  If you don't have NCAR Graphics, you're 
! out of luck.
!
  real, allocatable, dimension(:,:) :: scr2d

  character(len=19) :: hdate
  character(len=24) :: hdate_output

  character(len=8)  :: field2
  character(len=16) :: units2
  character(len=56) :: Desc2

  character(len=9)  :: field
  character(len=25) :: units
  character(len=46) :: Desc
  real :: level
  integer :: idim, jdim
  real :: lat1, lon1
  real :: deltalat, deltalon
  character(len=80) :: flnm

  integer :: idum, ifv, llflag, ilev, ierr
  real :: xfcst, dy, dx, lov, truelat1, truelat2, lon2

!
!   Set up the graceful stop (Sun, SGI, DEC).
!
  integer, external :: graceful_stop
  integer, external :: signal
  integer :: iii
  iii = signal(2, graceful_stop, -1)

  call pregrid_version

  call getarg(1,flnm)
  open(13, file=flnm, form='unformatted', status='old')

  call gopks(6,idum)
  call gopwk(1,55,1)
  call gopwk(2,56,3)
  call gacwk(1)
  call gacwk(2)
  call pcseti('FN', 21)
  call pcsetc('FC', '~')

  call gscr(1,0, 1.000, 1.000, 1.000)
  call gscr(1,1, 0.000, 0.000, 0.000)
  call gscr(1,2, 0.900, 0.600, 0.600)

! Find format version


  BIGLOOP : DO
     read(13, iostat=ierr) ifv
     if (ierr /= 0) exit BIGLOOP
     if (ifv.eq.1) then
        read(13) hdate_output, field2, units2, Desc2, level, idim, jdim, llflag
        field = field2
        units = units2
        desc = desc2
        hdate = hdate_output(1:19)
        read(13) lat1, lon1, deltalat, deltalon
        allocate(scr2d(idim,jdim))
        read(13) scr2d
     elseif (ifv.eq.2) then
        read(13) hdate_output, xfcst, field2, units2, Desc2, level,&
             idim, jdim, llflag
        field = field2
        units = units2
        desc = desc2
        hdate = hdate_output(1:19)
        select case (llflag)
        case (0)
           read(13) lat1, lon1, dy, dx
        case (3)
           read(13) lat1, lon1, dx, dy, lov, truelat1, truelat2
           dy = dx
        case (5)
           read(13) lat1, lon1, dx, dy, lov, truelat1
        case default
           print*, 'Unknown llflag: ', llflag
           stop
        end select
        allocate(scr2d(idim,jdim))
        read(13) scr2d
     elseif (ifv.eq.3) then
        read(13) hdate_output, xfcst, field, units, Desc, level,&
             idim, jdim, llflag
        hdate = hdate_output(1:19)
        select case (llflag)
        case (0)
           read(13) lat1, lon1, dy, dx
        case (1)
           read(13) lat1, lon1, dy, dx, truelat1
        case (3)
           read(13) lat1, lon1, dx, dy, lov, truelat1, truelat2
           dy = dx
        case (5)
           read(13) lat1, lon1, dx, dy, lov, truelat1
        case default
           print*, 'Unknown llflag: ', llflag
           stop
        end select
        allocate(scr2d(idim,jdim))
        read(13) scr2d
     else
        print*, 'Unknown ifv: ', ifv
         stop
     endif

     ilev = nint(level)

     call plt2d(scr2d, idim, jdim, llflag, &
          lat1, lon1, dx, dy, lov, lon2, truelat1, truelat2, &
          field, ilev, units, desc) 

     deallocate(scr2d)

  enddo BIGLOOP

  call stopit

end program pltfmt

subroutine plt2d(scr2d, ix, jx, llflag, &
     lat1, lon1, dx, dy, lov, lon2, truelat1, truelat2, &
     field, ilev, units, Desc)
  implicit none
  integer :: llflag
  integer :: ix, jx
  real, dimension(ix,jx) :: scr2d(ix,jx)
  real :: lat1, lon1, lov, truelat1, truelat2
  real :: dx, dy
  character(len=*) :: field
  character(len=*) ::  units
  character(len=*) :: Desc
  character(len=30) :: hunit

  integer :: iproj, ierr
  real :: pl1, pl2, pl3, pl4, plon, plat, rota, lon2, phic
  real :: xl, xr, xb, xt, wl, wr, wb, wt
  integer :: ml, ih, i

  integer, parameter :: lwrk = 20000, liwk = 50000
  real, dimension(lwrk) :: rwrk
  integer, dimension(liwk) :: iwrk

  integer :: ilev
  character(len=8) :: hlev

  select case (llflag)
  case (0)
     pl1 = lat1
     pl2 = lon1
     call fmtxyll(float(ix), float(jx), pl3, pl4, 'CE', pl1, pl2, &
          plon, truelat1, truelat2, dx, dy)
     plon = (pl2 + pl4) / 2.
     plat = 0.
     rota = 0.
     iproj=8
  case (1)
     pl1 = lat1
     pl2 = lon1
     plon = 0.
     call fmtxyll(float(ix), float(jx), pl3, pl4, 'ME', pl1, pl2, &
          plon, truelat1, truelat2, dx, dy)
     plat = 0.
     rota = 0
     iproj = 9
  case (3)
     pl1 = lat1
     pl2 = lon1
     plon = lov
     call fmtxyll(float(ix), float(jx), pl3, pl4, 'LC', pl1, pl2,&
          plon, truelat1, truelat2, dx, dy)
     plat = truelat1
     rota = truelat2
     iproj=3
! This never used to be a problem, but currently we seem to need
! truelat1 (in plat) differ from truelat2 (in rota) for the 
! NCAR-Graphics map routines to work.  Maybe it's just a compiler
! thing.  So if the truelats are the same, we add an epsilon:
     if (abs(plat - rota) < 1.E-8) then
        plat = plat + 1.E-8
        rota = rota - 1.E-8
     endif
  case (5)
     print*, 'ix, jx = ', ix, jx
     print*, 'lat1, lon1 = ', lat1, lon1
     pl1 = lat1
     pl2 = lon1
     plon = lov
     plat = 90.
     print*, 'plon, plat = ', plon, plat
     phic = 90.
     rota = 0.
     call fmtxyll(float(ix), float(jx), pl3, pl4, 'ST', pl1, pl2,&
          plon, truelat1, truelat2, dx, dy)
     iproj=1
     print*, pl1, pl2, pl3, pl4
  end select

! print *, iproj,plat,plon,rota,pl1,pl2,pl3,pl4
  call supmap(iproj,plat,plon,rota,pl1,pl2,pl3,pl4,2,30,4,0,ierr)
! call supmap(iproj,plat+0.001,plon,rota-0.001,pl1,pl2,pl3,pl4,2,30,4,0,ierr)
  if (ierr.ne.0) then
     print*, 'supmap ierr = ', ierr
         stop
!    stop
  endif
  call getset(xl,xr,xb,xt,wl,wr,wb,wt,ml)

  write(hlev,'(I8)') ilev

  call set(0., 1., 0., 1., 0., 1., 0., 1., 1)
  call pchiqu(0.1, xb-0.04, hlev//'  '//field, .020, 0.0, -1.0)
  print*, field//'#'//units//'#'//trim(Desc)
  call pchiqu(0.1, xb-0.12, Desc, .012, 0.0, -1.0)
  hunit = '                                      '
  ih = 0
  do i = 1, len(units)
     if (units(i:i).eq.'{') then
        hunit(ih+1:ih+3) = '~S~'
        ih = ih + 3
        elseif (units(i:i).eq.'}') then
        hunit(ih+1:ih+3) = '~N~'
        ih = ih + 3
     else
        ih = ih + 1
        hunit(ih:ih) = units(i:i)
     endif
  enddo
  call pchiqu(0.1, xb-0.08, hunit, .015, 0.0, -1.0)
  call pchiqu(0.1, xb-0.12, Desc, .012, 0.0, -1.0)


  call set(xl,xr,xb,xt,1.,float(ix),1.,float(jx),ml)

  call CPSETI ('SET - Do-SET-Call Flag', 0)
  call CPSETR ('SPV - Special Value', -1.E30)

  if (dy.lt.0.) then
     call array_flip(scr2d, ix, jx)
  endif

  call cpseti('LLP', 3)
  call cprect(scr2d,ix,ix,jx,rwrk,lwrk,iwrk,liwk)
  call cpcldr(scr2d,rwrk,iwrk)
  call cplbdr(scr2d,rwrk,iwrk)

  if (dy.lt.0.) then
     call array_flip(scr2d, ix, jx)
  endif

  call frame

end subroutine plt2d

subroutine stopit
  call graceful_stop
end

subroutine graceful_stop
  call gdawk(2)
  call gdawk(1)
  call gclwk(2)
  call gclwk(1)
  call gclks
  print*, 'Graceful Stop.'
  stop
end subroutine graceful_stop

subroutine fmtxyll(x, y, xlat, xlon, project, glat1, glon1, gclon,&
     gtrue1, gtrue2, gdx, gdy)
  implicit none

  real , intent(in) :: x, y, glat1, glon1, gtrue1, gtrue2, gdx, gdy, gclon
  character(len=2), intent(in) :: project
  real , intent(out) :: xlat, xlon

  real :: gx1, gy1, gkappa
  real :: grrth = 6370.949

  real :: r, y1
  integer :: iscan, jscan
  real, parameter :: pi = 3.1415926534
  real, parameter :: degran = pi/180.
  real, parameter :: raddeg = 180./pi
  real :: gt

  if (project.eq.'CE') then  ! Cylindrical Equidistant grid

     xlat = glat1 + gdy*(y-1.)
     xlon = glon1 + gdx*(x-1.)
     
  elseif (project == "ME") then

     gt = grrth * cos(gtrue1*degran)
     xlon = glon1 + (gdx*(x-1.)/gt)*raddeg
     y1 = gt*alog((1.+sin(glat1*degran))/cos(glat1*degran))/gdy
     xlat = 90. - 2. * atan(exp(-gdy*(y+y1-1.)/gt))*raddeg

  elseif (project.eq.'ST') then  ! Polar Stereographic grid

     r = grrth/gdx * tand((90.-glat1)/2.) * (1.+sind(gtrue1))
     gx1 = r * sind(glon1-gclon)
     gy1 = - r * cosd(glon1-gclon)

     r = sqrt((x-1.+gx1)**2 + (y-1+gy1)**2)
     xlat = 90. - 2.*atan2d((r*gdx),(grrth*(1.+sind(gtrue1))))
     xlon = atan2d((x-1.+gx1),-(y-1.+gy1)) + gclon

  elseif (project.eq.'LC') then  ! Lambert-conformal grid

     call glccone(gtrue1, gtrue2, 1, gkappa)

     r = grrth/(gdx*gkappa)*sind(90.-gtrue1) * &
          (tand(45.-glat1/2.)/tand(45.-gtrue1/2.)) ** gkappa
     gx1 =  r*sind(gkappa*(glon1-gclon))
     gy1 = -r*cosd(gkappa*(glon1-gclon))

     r = sqrt((x-1.+gx1)**2 + (y-1+gy1)**2)
     xlat = 90. - 2.*atand(tand(45.-gtrue1/2.)* &
          ((r*gkappa*gdx)/(grrth*sind(90.-gtrue1)))**(1./gkappa))
     xlon = atan2d((x-1.+gx1),-(y-1.+gy1))/gkappa + gclon

  else

     write(*,'("Unrecoginzed projection: ", A)') project
     write(*,'("Abort in FMTXYLL",/)')
     stop

  endif
contains
  real function sind(theta)
    real :: theta
    sind = sin(theta*degran)
  end function sind
  real function cosd(theta)
    real :: theta
    cosd = cos(theta*degran)
  end function cosd
  real function tand(theta)
    real :: theta
    tand = tan(theta*degran)
  end function tand
  real function atand(x)
    real :: x
    atand = atan(x)*raddeg
  end function atand
  real function atan2d(x,y)
    real :: x,y
    atan2d = atan2(x,y)*raddeg
  end function atan2d

  subroutine glccone (fsplat,ssplat,sign,confac)
    implicit none
    real, intent(in) :: fsplat,ssplat
    integer, intent(in) :: sign
    real, intent(out) :: confac
    if (abs(fsplat-ssplat).lt.1.E-3) then
       confac = sind(fsplat)
    else
       confac = log10(cosd(fsplat))-log10(cosd(ssplat))
       confac = confac/(log10(tand(45.-float(sign)*fsplat/2.))- &
            log10(tand(45.-float(sign)*ssplat/2.)))
    endif
  end subroutine glccone

end subroutine fmtxyll

subroutine array_flip(array, ix, jx)
  implicit none
  integer :: ix, jx
  real , dimension(ix,jx) :: array

  real, dimension(ix) :: hold
  integer :: i, j, jj

  do j = 1, jx/2
     jj = jx+1-j
     hold = array(1:ix, j)
     array(1:ix,j) = array(1:ix,jj)
     array(1:ix,jj) = hold
  enddo
end subroutine array_flip

