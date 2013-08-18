! From Jim Bresch and Kevin Manning (NCAR/MMM)
! From John Cassano (CU/CIRES)
! Others below

MODULE nsidc

   INTEGER , PARAMETER :: resolution = 4
   REAL , DIMENSION(360*resolution+1,180*resolution+1) :: seaice_ll = 0.

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE seaice_gridded

      USE support
      IMPLICIT NONE

      INTEGER :: i , j , icount
      REAL :: sum , lat , lon , row , col

      seaice_ll = flag
      
      !  Northern hemisphere first .

      DO j = (90+25)*resolution+1 , 180*resolution+1
         DO i = 1 , 360*resolution+1
            lat = -90 + REAL(j-1)/(180*resolution) * 180
            lon = REAL(i-1)/(360*resolution) * 360
            CALL ll2rc ( 'Nl' , lat , lon , row , col ) 
            seaice_ll(i,j) = seaice(NINT(col),NINT(row),1)
         END DO
      END DO
      
      !  Then the Southern hemisphere.

      DO j = 1, (90-25)*resolution+1
         DO i = 1 , 360*resolution+1
            lat = -90 + REAL(j-1)/(180*resolution) * 180
            lon = REAL(i-1)/(360*resolution) * 360
            CALL ll2rc ( 'Sl' , lat , lon , row , col ) 
            seaice_ll(i,j) = seaice(NINT(col),NINT(row),2)
         END DO
      END DO

   END SUBROUTINE seaice_gridded

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE ll2rc ( grid , lat , lon , r , s )
      IMPLICIT NONE

      !  Convert lat/lon to EASE-grid row/column

      REAL :: lat,lon,r,s
      CHARACTER (LEN=2) :: grid
      INTEGER :: status

      CALL ezlh_convert ( status , grid , lat , lon , r , s )

      IF ( status .NE. 0) THEN
         print *,'lat/lon not on grid'
         stop
      END IF

      !  Need to add 1 to r and s so that (r,s) refers to array that starts with element 1

      r=r+1
      s=s+1

   END SUBROUTINE ll2rc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!==========================================================================
! ezlhconv.for - FORTRAN routines for conversion of azimuthal
!               equal area and equal area cylindrical grid coordinates
!
!       30-Jan.-1992 H.Maybee
!       20-Mar-1992 Ken Knowles  303-492-0644  knowles@kryos.colorado.edu
!       16-Dec-1993 MJ Brodzik   303-492-8263  brodzik@jokull.colorado.edu
!                   Copied from nsmconv.f, changed resolutions from
!                   40-20-10 km to 25-12.5 km
!       21-Dec-1993 MJ Brodzik   303-492-8263  brodzik@jokull.colorado.edu
!                   Fixed sign of Southern latitudes in ease_inverse.
!       12-Sep-1994 David Hoogstrate 303-492-4116 hoogstra@jokull.colorado.edu
!                   Changed grid cell size. Changed "c","f" to "l","h"
!       25-Oct-1994 David Hoogstrate 303-492-4116 hoogstra@jokull.colorado.edu
!                   Changed row size from 587 to 586 for Mercator projection
!                   Changed function names to "ezlh-.."
!$Log: module_nsidc.F,v $
!Revision 1.1  2002/11/11 22:57:54  gill
!NSIDC.org sea ice fraction data, output in intermediate format
!
!Revision 1.3  1994/11/01 23:40:43  brodzik
!Replaced all references to 'ease' with 'ezlh'
!
!==========================================================================

!--------------------------------------------------------------------------
        subroutine ezlh_convert (status, grid, lat, lon, r, s)
!
! subroutine provided by NSIDC (http://nsidc.org/NASA/GUIDE/EASE/tools.html)
!
        integer status
        character (len=2) grid
        real lat, lon, r, s
!
!       convert geographic coordinates (spherical earth) to
!       azimuthal equal area or equal area cylindrical grid coordinates
!
!       input : grid - projection name '[NSM][lh]'
!               where l = "low"  = 25km resolution
!                     h = "high" = 12.5km resolution
!               lat, lon - geo. coords. (decimal degrees)
!
!       output: r, s - column, row coordinates
!               status = 0 indicates normal successful completion
!                       -1 indicates error status (point not on grid)
!
!--------------------------------------------------------------------------
        integer cols, rows, scale
        real Rg, phi, lam, rho, r0, s0

!       radius of the earth (km), authalic sphere based on International datum
        real,parameter :: RE_km = 6371.228
!       nominal cell size in kilometers
        real , parameter :: CELL_km = 25.067525

!       scale factor for standard paralles at +/-30.00 degrees
        real , parameter :: COS_PHI1 = .866025403

        real , parameter :: PI = 3.141592653589793

        real :: rad , t
        rad(t) = t*PI/180.
!       deg(t) = t*180./PI

        status = -1

        if ((grid(1:1).eq.'N').or.(grid(1:1).eq.'S')) then
          cols = 721
          rows = 721
        else if (grid(1:1).eq.'M') then
          cols = 1383
          rows = 586
        else
          print *, 'ezlh_convert: unknown projection: ', grid
          return
        endif

        if (grid(2:2).eq.'l') then
          scale = 1
        else if (grid(2:2).eq.'h') then
          scale = 2
        else
          print *, 'ezlh_convert: unknown projection: ', grid
          return
        endif

        Rg = scale * RE_km/CELL_km

!
!       r0,s0 are defined such that cells at all scales
!       have coincident center points
!
        r0 = (cols-1)/2. * scale
        s0 = (rows-1)/2. * scale

        phi = rad(lat)
        lam = rad(lon)

        if (grid(1:1).eq.'N') then
          rho = 2 * Rg * sin(PI/4. - phi/2.)
          r = r0 + rho * sin(lam)
          s = s0 + rho * cos(lam)

        else if (grid(1:1).eq.'S') then
          rho = 2 * Rg * cos(PI/4. - phi/2.)
          r = r0 + rho * sin(lam)
          s = s0 - rho * cos(lam)

        else if (grid(1:1).eq.'M') then
          r = r0 + Rg * lam * COS_PHI1
          s = s0 - Rg * sin(phi) / COS_PHI1

        endif

        status = 0
        end subroutine ezlh_convert
!****************************************************************
END MODULE nsidc
