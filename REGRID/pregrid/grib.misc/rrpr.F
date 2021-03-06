subroutine rrpr(hstart, ntimes, interval, nlvl, maxlvl, plvl, debug_print)
!                                                                             !
! In case you are wondering, RRPR stands for "Read, ReProcess, and wRite"     !
!                                                                             !
!*****************************************************************************!
!                                                                             !
! Recent changes:                                                             !
!                                                                             !
!    2002-05-16:                                                              !
!               - Handle the Mercator projection.                             !
!                 This change also required changes to output.F, rd_grib.F,   !
!                 datint.F, gribcode.F                                        !
!                                                                             !
!    2002-02-13:                                                              !
!               - Added vertical interpolation in pressure in case of missing !
!                 U, V, T (the check for RH was already there)                !
!                                                                             !
!    2001-02-14:                                                              !
!               - Allow file names to have date stamps out to minutes or      !
!                 seconds, if the user requests a time interval (in seconds)  !
!                 that is not evenly divisible into hours or minutes.         !
!                 INTERVAL is checked for divisibility into 3600 (for hours)  !
!                 or 60 (for minutes).  The local variable DATELEN is set     !
!                 to be the number of characters to use in our character      !
!                 dates.  Valid values for DATELEN are 13 (for hours),        !
!                 16 (for minutes), and 19 (for seconds).                     !
!                                                                             !
!                 This change also requires changes to pregrid_grib.F,        !
!                 output.F, datint.F, file_delete.F                           !
!                                                                             !
!               - Do processing not just if the requested date matches one we !
!                 want, but if the requested date falls between the startdate !
!                 and the enddate.                                            !
!                                                                             !
!*****************************************************************************!

  use filelist
  use gridinfo
  use storage_module
  use table
  implicit none

!------------------------------------------------------------------------------
! Arguments:

! HSTART:  Starting date of times to process 
  character (LEN=19) :: hstart

! NTIMES:  Number of time periods to process
  integer :: ntimes

! INTERVAL:  Time inteval (seconds) of time periods to process.
  integer :: interval

! NLVL:  The number of levels in the stored data.
  integer :: nlvl

! MAXLVL: The parameterized maximum number of levels to allow.
  integer :: maxlvl

! PLVL:  Array of pressure levels (Pa) in the dataset
  real , dimension(maxlvl) :: plvl

! DEBUG_PRINT:  Flag to turn off or on debug printout.
  logical :: debug_print

!------------------------------------------------------------------------------

  character (LEN=25) :: units
  character (LEN=46) :: Desc
  real, allocatable, dimension(:,:) :: scr2d
  real, pointer, dimension(:,:) :: ptr2d

  integer :: k, kk, m, n, ierr, ifv
  integer :: iunit=13

  character(LEN=19) :: hdate, hend
  character(LEN=24) :: hdate_output
  real :: xfcst, level
  character(LEN=9) :: field

  integer :: ntime, idts

! DATELEN:  length of date strings to use for our output file names.
  integer :: datelen

! Decide the length of date strings to use for output file names.  
! DATELEN is 13 for hours, 16 for minutes, and 19 for hours.
  if (mod(interval,3600) == 0) then
     datelen = 13
  else if (mod(interval, 60) == 0) then
     datelen = 16
  else
     datelen = 19
  endif

  print*, 'nfiles = ', nfiles
  print*, 'ntimes = ', ntimes
  print*, 'filedates = ', (filedates(n), n = 1, nfiles)

! Compute the ending time:

  call geth_newdate(hend, hstart, interval*ntimes)

  call clear_storage

! Loop over the output file dates, and do stuff if the file date falls
! between the starting and ending times.

  FILELOOP : do n = 1, nfiles
     print*, 'hstart, hend = ', hstart//"  "//hend
     print*, 'filedates(n) = ', filedates(n)
     if (filedates(n)(1:datelen) < hstart(1:datelen)) cycle FILELOOP
     if (filedates(n)(1:datelen) >   hend(1:datelen)) cycle FILELOOP
     print*, "Doing it: ", filedates(n)(1:datelen)
     open(iunit, file='PFILE:'//filedates(n)(1:datelen),form='unformatted',&
          status='old')

! Read the file:

     rdloop: do 
        read (iunit, iostat=ierr) ifv
        if (ierr.ne.0) exit rdloop
        read (iunit) hdate_output, xfcst, field, units, Desc, &
             level, map%nx, map%ny, map%igrid
        hdate = hdate_output(1:19)
        if (map%igrid.eq.3) then ! lamcon
           read (iunit) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                map%truelat1, map%truelat2
        elseif (map%igrid.eq.5) then ! Polar Stereographic
           read (iunit) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                map%truelat1
        elseif (map%igrid.eq.0)then ! lat/lon
           read (iunit) map%lat1, map%lon1, map%dy, map%dx
        elseif (map%igrid.eq.1)then ! Mercator
           read (iunit) map%lat1, map%lon1, map%dy, map%dx, map%truelat1
        else
           write(*,'("Unrecognized map%igrid: ", I20)') map%igrid
           stop
        endif

        allocate(ptr2d(map%nx,map%ny))
        read (iunit) ptr2d
        call refw_storage(nint(level), field, ptr2d, map%nx, map%ny)
        nullify (ptr2d)
     enddo rdloop
!
! We've reached the end of file, so it's time to close it.
!
     close(iunit)
     if (debug_print) call print_storage
!
! By now the file has been read completely.  Now, see if we need to fill in 
! missing fields:
!

! Retrieve the number of levels in storage:
!
     call get_plvls(plvl, maxlvl, nlvl)
!
! Fill the surface level (code 200100) from higher 200100's, as necessary
!
     do k = 1, nlvl
        if ((plvl(k).gt.200100) .and. (plvl(k).lt.200200)) then
           ! We've found a level between 200100 and 200200, now find the field
           ! corresponding to that level.
           MLOOP : do m = 1, maxvar
              if (is_there(nint(plvl(k)), namvar(m))) then
                 INLOOP : do kk = 200101, nint(plvl(k))
                    if (is_there(kk, namvar(m))) then
                       write(*, &
'("Copying ", A9, " at level ", I6, " to level 200100.")') namvar(m), kk
                       call get_dims(kk, namvar(m))
                       allocate(scr2d(map%nx,map%ny))
                       call get_storage &
                            (kk, namvar(m), scr2d, map%nx, map%ny)
                       call put_storage &
                            (200100,namvar(m), scr2d,map%nx,map%ny)
                       deallocate(scr2d)
                       EXIT INLOOP
                    endif
                 enddo INLOOP
              endif
           enddo MLOOP
        endif
     enddo

!
! If upper-air U is missing, see if we can interpolate from surrounding levels.
! This is a simple vertical interpolation, linear in pressure.
! Currently, this simply fills in one missing level between two present levels. 
!

     do k = 2, nlvl-1, 1
        if (plvl(k-1) .lt. 200000.) then
           if ( (.not. is_there(nint(plvl(k)),'U')) .and. &
                ( is_there(nint(plvl(k-1)), 'U')) .and.&
                ( is_there(nint(plvl(k+1)), 'U')) ) then
              call get_dims(nint(plvl(k+1)), 'U')
              call vntrp(plvl, maxlvl, k, "U       ", map%nx, map%ny)
           endif
        endif
     enddo

!
! If upper-air V is missing, see if we can interpolate from surrounding levels.
! This is a simple vertical interpolation, linear in pressure.
! Currently, this simply fills in one missing level between two present levels. 
!

     do k = 2, nlvl-1, 1
        if (plvl(k-1) .lt. 200000.) then
           if ( (.not. is_there(nint(plvl(k)),'V')) .and. &
                ( is_there(nint(plvl(k-1)), 'V')) .and.&
                ( is_there(nint(plvl(k+1)), 'V')) ) then
              call get_dims(nint(plvl(k+1)), 'V')
              call vntrp(plvl, maxlvl, k, "V       ", map%nx, map%ny)
           endif
        endif
     enddo

!
! If upper-air T is missing, see if we can interpolate from surrounding levels.
! This is a simple vertical interpolation, linear in pressure.
! Currently, this simply fills in one missing level between two present levels. 
!

     do k = 2, nlvl-1, 1
        if (plvl(k-1) .lt. 200000.) then
           if ( (.not. is_there(nint(plvl(k)),'T')) .and. &
                ( is_there(nint(plvl(k-1)), 'T')) .and.&
                ( is_there(nint(plvl(k+1)), 'T')) ) then
              call get_dims(nint(plvl(k+1)), 'T')
              call vntrp(plvl, maxlvl, k, "T       ", map%nx, map%ny)
           endif
        endif
     enddo


!
! Check to see if we need to fill HGT from GEOPT.
!
     do k = 1, nlvl
        if (plvl(k).lt.200000.) then
           if (.not. is_there(nint(plvl(k)), 'HGT').and. &
                is_there(nint(plvl(k)), 'GEOPT')) then
              call get_dims(nint(plvl(k)), 'GEOPT')
              allocate(scr2d(map%nx,map%ny))
              call get_storage(nint(plvl(k)), 'GEOPT', scr2d, map%nx, map%ny)
              scr2d = scr2d / 9.81
              call put_storage(nint(plvl(k)), 'HGT',   scr2d, map%nx, map%ny)
              deallocate(scr2d)
           endif
        endif
     enddo

! If upper-air RH is missing, see if we can compute RH from Specific Humidity:

     do k = 1, nlvl
        if (plvl(k).lt.200000.) then
           if (.not. is_there(nint(plvl(k)), 'RH').and. &
                is_there(nint(plvl(k)), 'SPECHUMD')) then
              call get_dims(nint(plvl(k)), 'T')
              call compute_rh_spechumd_upa(map%nx, map%ny, plvl(k))
           endif
        endif
     enddo

! If upper-air RH is missing, see if we can compute RH from Vapor Pressure:
!   (Thanks to Bob Hart of PSU ESSC -- 1999-05-27.)

     do k = 1, nlvl
        if (plvl(k).lt.200000.) then
           if (.not. is_there(nint(plvl(k)),'RH').and. &
                is_there(nint(plvl(k)),'VAPP')) then
              call get_dims(nint(plvl(k)),'T')
              call compute_rh_vapp_upa(map%nx, map%ny, plvl(k))
           endif
        endif
     enddo

! If upper-air RH is missing, see if we can compute RH from Dewpoint Depression:

     do k = 1, nlvl
        if (plvl(k).lt.200000.) then
           if (.not. is_there(nint(plvl(k)),'RH').and. &
                is_there(nint(plvl(k)),'DEPR')) then
              call get_dims(nint(plvl(k)),'T')
              call compute_rh_depr(map%nx, map%ny, plvl(k))
           endif
        endif
     enddo

! If upper-air RH is missing, see if we can compute RH from RHFRAC:
!    (Luksa Kraljevic, Meteorological and Hydrological Service of Croatia)

     do k = 1, nlvl
        if (plvl(k).lt.200000.) then
           if (.not. is_there(nint(plvl(k)), 'RH').and. &
                is_there(nint(plvl(k)), 'RHFRAC')) then
              call get_dims(nint(plvl(k)), 'RHFRAC')
              allocate(scr2d(map%nx,map%ny))
              call get_storage(nint(plvl(k)), 'RH', scr2d, map%nx, map%ny)
              scr2d = scr2d * 100
              call put_storage(nint(plvl(k)), 'RH',   scr2d, map%nx, map%ny)
              deallocate(scr2d)

           endif
        endif
     enddo

! If upper-air RH is missing, see if we can interpolate from surrounding levels.
! This is a simple vertical interpolation, linear in pressure.
! Currently, this simply fills in one missing level between two present levels. 
! May expand this in the future to fill in additional levels.  May also expand 
! this in the future to vertically interpolate other variables.
!

     do k = 2, nlvl-1, 1
        if (plvl(k-1) .lt. 200000.) then
           if ( (.not. is_there(nint(plvl(k)),'RH')) .and. &
                ( is_there(nint(plvl(k-1)), 'RH')) .and.&
                ( is_there(nint(plvl(k+1)), 'RH')) ) then
              call get_dims(nint(plvl(k+1)), 'RH')
              call vntrp(plvl, maxlvl, k, "RH      ", map%nx, map%ny)
           endif
        endif
     enddo

!
! Check to see if we need to fill RH above 300 mb 
! Set to 1% for now - can be over written in regridder by user
!
     if (is_there(30000, 'RH')) then
        call get_dims(30000, 'RH')
        allocate(scr2d(map%nx,map%ny))
        scr2d = 1.

        do k = 1, nlvl
           if (plvl(k).lt.30000.) then
              if (.not. is_there(nint(plvl(k)), 'RH')) then
                 call put_storage(nint(plvl(k)),'RH',scr2d,map%nx,map%ny)
              endif
           endif
        enddo
        deallocate(scr2d)
     endif
!
! If surface RH is missing, see if we can compute RH from Specific Humidity 
! or Dewpoint or Dewpoint depression:
!
     if (.not. is_there (200100, 'RH')) then
        if (is_there(200100, 'T').and. &
             is_there(200100, 'PSFC'    )   .and. &
             is_there(200100, 'SPECHUMD')) then
           call get_dims(200100, 'T')
           call compute_rh_spechumd(map%nx, map%ny)
        elseif (is_there(200100, 'T'       ).and. &
             is_there(200100, 'DEWPT')) then
           call get_dims(200100, 'T')
           call compute_rh_dewpt(map%nx, map%ny)
        elseif (is_there(200100, 'T').and. &
             is_there(200100, 'DEPR')) then
           call get_dims(200100, 'T')
           call compute_rh_depr(map%nx, map%ny, 200100.)
        endif
     endif

! If we've got a SOILHGT field and it is from ECMWF, change units 

     if (is_there(200100, 'SOILHGT')) then
        call get_dims(200100, 'SOILHGT')
        call ecmwf_soil_hgt(map%nx, map%ny)
     endif

! If we've got a SEAICE field, make sure that it is all Zeros and Ones:

     if (is_there(200100, 'SEAICE')) then
        call get_dims(200100, 'SEAICE')
        call make_zero_or_one(map%nx, map%ny)
     endif

! Write everything back out:
     call output(hdate, nlvl, maxlvl, plvl, interval, 2)
     call clear_storage
  enddo FILELOOP
end subroutine rrpr

subroutine ecmwf_soil_hgt(ix, jx)
! If we've got a SOILHGT field and it is from ECMWF, change units 
  use storage_module
  implicit none
  integer :: ix, jx
  real, dimension(ix,jx) :: soilhgt
  real :: soil_tmp

  call get_storage(200100, 'SOILHGT',soilhgt, ix, jx)
  soil_tmp = maxval(soilhgt)
  if (soil_tmp .gt. 20000.) then
    print*,"Looks like we have a SOILHGT field from ECMWF"
    soilhgt = soilhgt/9.81
  endif
  call put_storage(200100, 'SOILHGT',soilhgt, ix, jx)
end subroutine ecmwf_soil_hgt

subroutine make_zero_or_one(ix, jx)
! Make sure the SEAICE field is zero or one.
  use storage_module
  implicit none
  integer :: ix, jx
  real, dimension(ix,jx) :: seaice

  call get_storage(200100, 'SEAICE',seaice, ix, jx)
  where(seaice > 0.5)
     seaice = 1.0
  elsewhere
     seaice = 0.0
  end where
  call put_storage(200100, 'SEAICE',seaice, ix, jx)
end subroutine make_zero_or_one

subroutine compute_rh_spechumd(ix, jx)
! Compute relative humidity from specific humidity.
  use storage_module
  implicit none
  integer :: ix, jx
  real :: lat1, lon1, dx, dy
  real, dimension(ix,jx) :: T, P, RH, Q

  real, parameter :: svp1=611.2
  real, parameter :: svp2=17.67
  real, parameter :: svp3=29.65
  real, parameter :: svpt0=273.15
  real, parameter :: eps = 0.622

  real startlat, startlon, deltalat, deltalon

  call get_storage(200100, 'T',        T, ix, jx)
  call get_storage(200100, 'PSFC',     P, ix, jx)
  call get_storage(200100, 'SPECHUMD', Q, ix, jx)

  rh = 1.E2 * (p*q/(q*(1.-eps) + eps))/(svp1*exp(svp2*(t-svpt0)/(T-svp3)))

  call put_storage(200100, 'RH', rh, ix, jx)

end subroutine compute_rh_spechumd

subroutine compute_rh_spechumd_upa(ix, jx, plvl)
! Compute relative humidity from specific humidity.
  use storage_module
  implicit none
  integer :: ix, jx
  real :: plvl
  real :: lat1, lon1, dx, dy
  real, dimension(ix,jx) :: T, P, RH, Q

  real, parameter :: svp1=611.2
  real, parameter :: svp2=17.67
  real, parameter :: svp3=29.65
  real, parameter :: svpt0=273.15
  real, parameter :: eps = 0.622

  real startlat, startlon, deltalat, deltalon

  P = plvl
  call get_storage(nint(plvl), 'T',        T, ix, jx)
  call get_storage(nint(plvl), 'SPECHUMD', Q, ix, jx)

  rh = 1.E2 * (p*q/(q*(1.-eps) + eps))/(svp1*exp(svp2*(t-svpt0)/(T-svp3)))
  
  call put_storage(nint(plvl), 'RH', rh, ix, jx)

end subroutine compute_rh_spechumd_upa

subroutine compute_rh_vapp_upa(ix, jx, plvl)
! Compute relative humidity from vapor pressure.
! Thanks to Bob Hart of PSU ESSC -- 1999-05-27.
  use storage_module
  implicit none
  integer :: ix, jx
  real :: plvl
  real :: lat1, lon1, dx, dy
  real, dimension(ix,jx) :: P, ES
  real, pointer, dimension(:,:) :: T, E, RH

  real, parameter :: svp1=611.2
  real, parameter :: svp2=17.67
  real, parameter :: svp3=29.65
  real, parameter :: svpt0=273.15
  real, parameter :: eps = 0.622

  real :: startlat, startlon, deltalat, deltalon

  allocate(RH(ix,jx))

  P = plvl
  call refr_storage(nint(plvl), 'T',    T, ix, jx)
  call refr_storage(nint(plvl), 'VAPP', E, ix, jx)

  ES=svp1*exp(svp2*(T-svpt0)/(T-svp3))
  rh=min(1.E2*(P-ES)*E/((P-E)*ES), 1.E2)

  call refw_storage(nint(plvl), 'RH', rh, ix, jx)

  nullify(T,E)

end subroutine compute_rh_vapp_upa

subroutine compute_rh_depr(ix, jx, plvl)
! Compute relative humidity from Dewpoint Depression
  use storage_module
  implicit none
  integer :: ix, jx
  real :: plvl
  real :: lat1, lon1, dx, dy
  real, dimension(ix,jx) :: t, depr, rh

  real, parameter :: Xlv = 2.5e6
  real, parameter :: Rv = 461.5

  integer :: i, j

  call get_storage(nint(plvl), 'T', T,  ix, jx)
  call get_storage(nint(plvl), 'DEPR', DEPR, ix, jx)

  where(DEPR < 100.)
     rh = exp(Xlv/Rv*(1./T - 1./(T-depr))) * 1.E2
  elsewhere
     rh = 0.0
  endwhere

  call put_storage(nint(plvl),'RH      ', rh, ix, jx)

end subroutine compute_rh_depr

subroutine compute_rh_dewpt(ix,jx)
! Compute relative humidity from Dewpoint
  use storage_module
  implicit none
  integer :: ix, jx
  real :: lat1, lon1, dx, dy
  real, dimension(ix,jx) :: t, dp, rh

  real, parameter :: Xlv = 2.5e6
  real, parameter :: Rv = 461.5

  call get_storage(200100, 'T       ', T,  ix, jx)
  call get_storage(200100, 'DEWPT   ', DP, ix, jx)

  rh = exp(Xlv/Rv*(1./T - 1./dp)) * 1.E2

  call put_storage(200100,'RH      ', rh, ix, jx)

end subroutine compute_rh_dewpt

subroutine vntrp(plvl, maxlvl, k, name, ix, jx)
  use storage_module
  implicit none
  integer :: ix, jx, k, maxlvl
  real, dimension(maxlvl) :: plvl
  character(len=8) :: name
  real, dimension(ix,jx) :: a, b, c
  real :: frc

  write(*,'("Interpolating to fill in ", A, " at level ", I8)') trim(name), nint(plvl(k))

  call  get_storage(nint(plvl(k-1)), name, a, ix, jx)
  call  get_storage(nint(plvl(k+1)), name, c, ix, jx)

  frc = (plvl(k) - plvl(k+1)) / ( plvl(k-1)-plvl(k+1))

  b = (1.-frc)*a + frc*c

  call  put_storage(nint(plvl(k)), name, b, ix, jx)

end subroutine vntrp

