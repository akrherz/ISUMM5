!*****************************************************************************!
! MODULE GRIDINFO                                                             !
!                                                                             !
! Part of:                                                                    !
!    REGRID/pregrid/grib.misc                                                 !
!                                                                             !
! Purpose:                                                                    !
!    Hold information about the map projection of data we've read from the    !
!    analysis files.  We pass around the grid information through this        !
!    module, rather than through argument lists.                              !
!                                                                             !
! Variable MAP is defined to be of type MAPINFO, where MAPINFO is defined     !
! within this module:                                                         !
!                                                                             !
!*****************************************************************************!
module gridinfo
  type mapinfo
     integer :: igrid  ! Integer referring to the type of map projection:
                       ! 0 = lat/lon
                       ! 3 = Lambert Conformal
                       ! 5 = Polar Stereographic grid.
     integer :: nx     ! Number of points in the X direction.
     integer :: ny     ! Number of points in the Y direction.
     real :: truelat1  ! First true latitude (for Polar Stereo. and Lam. Conf.
     real :: truelat2  ! Second true latitude (for Lambt. Conformal).
     real :: lov       ! Central longitude of projection (PS and LC).
     real :: lat1      ! Starting latitude
     real :: lon1      ! Starting longitude
     real :: dx        ! grid-spacing in the X direction (km or degrees)
     real :: dy        ! grid-spacing in the Y direction (km or degrees)
  end type mapinfo

  type (mapinfo) :: map

end module gridinfo

