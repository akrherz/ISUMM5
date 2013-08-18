subroutine tv_to_t(plvl, nlvl)
! Changes:
! 2001-09-19 (KWM):
!    Slight change to the iterative algorithm which computes temperature
!    from virtual temperature.  We ran into a situation where it would
!    not converge.  The original algorithm updated t1 at the end of 
!    each iteration by "t1=t2".  In the particular situation we ran into, 
!    this was bouncing hither and yon and never converging.  Changing that
!    update to "t1 = 0.5*(t1+t2)" seems to control that, and we get 
!    convergence.
  use storage_module
  real , dimension(nlvl) :: plvl

  integer, parameter :: idim = 145
  integer, parameter :: jdim=73
  real , dimension(idim,jdim) :: T, RH, P

  real, parameter :: EPS   = 0.622
  real, parameter :: SVP1  = 611.2
  real, parameter :: SVP2  = 17.67
  real, parameter :: SVP3  = 29.65
  real, parameter :: SVPT0 = 273.15

  PLOOP : do n = 1, nlvl
     iplvl = nint(plvl(n))
     if (iplvl.lt.30000) cycle PLOOP
     if (iplvl.gt.200100) cycle PLOOP
     if (iplvl.eq.200100) then
        call get_storage(iplvl, 'PSFC    ', P)
     else
        P = plvl(n)
     endif

     call get_storage(iplvl, 'T       ', T)
     call get_storage(iplvl, 'RH      ', Rh)

     do i = 1, idim
        do j = 1, jdim
           es = svp1 * exp(svp2*(t(i,j)-svpt0)/(t(i,j)-svp3))
           qs = eps*es/(p(i,j)-es)
           w = amax1(0.01*rh(i,j)*qs, 0.0)
           t1 = t(i,j)/(1.+0.61*w)
           converge : do ll = 1, 100
              es=svp1*exp(svp2*(t1-svpt0)/(t1-svp3))
              ws=eps*es/(p(i,j)-es)
              w=amax1(0.01*rh(i,j)*ws,0.0)
              t2=t(i,j)/(1+0.61*w)
              dt=t1-t2
              if (abs(dt) .le. 0.005) exit converge
              t1=0.5*(t1+t2)
              if (ll == 100) stop "PROBLEM CONVERGING IN TV_TO_T."
           enddo converge
           t(i,j) = t2
        enddo
     enddo

     call put_storage(iplvl, 'T       ', t, idim, jdim, 999)

  enddo PLOOP

end subroutine tv_to_t
