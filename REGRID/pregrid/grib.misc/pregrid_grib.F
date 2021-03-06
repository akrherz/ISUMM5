!*****************************************************************************!
! program pregrid_grib                                                        !
!                                                                             !
! This program is part of the REGRID package of the                           !
!    Penn State / NCAR Mesoscale Modeling System.                             !
!                                                                             !
! Questions, comments, suggestions, even complaints should be directed to:    !
!                        mesouser@ucar.edu                                    !
! Externals:                                                                  !
!    Module TABLE                                                             !
!    Module GRIDINFO                                                          !
!    Module FILELIST                                                          !
!    Subroutine PREGRID_VERSION                                               !
!    Subroutine READ_PREGRID_NAMELIST                                         !
!    Subroutine PARSE_TABLE                                                   !
!    Subroutine CLEAR_STORAGE                                                 !
!    Subroutine RD_GRIB                                                       !
!    Subroutine PUT_STORAGE                                                   !
!    Subroutine OUTPUT                                                        !
!    Subroutine CCLOSE                                                        !
!    Subroutine RRPR                                                          !
!    Subroutine DATINT                                                        !
!    Subroutine FILE_DELETE                                                   !
!                                                                             !
! Kevin W. Manning                                                            !
! NCAR/MMM                                                                    !
! Summer 1998, and continuing                                                 !
! SDG                                                                         !
!                                                                             !
!*****************************************************************************!
!                                                                             !
! Recent changes:                                                             !
!   2001-02-14:  Allow output file names to have date stamps out to minutes   !
!                or seconds, if the user requests a time interval (in         !
!                seconds) that is not evenly divisible into hours.  For this  !
!                routine (pregrid_grib.F), this simply involves passing       !
!                the interval to routines output.F, rrpr.F, datint.F,         !
!                file_delete.F                                                !
!                                                                             !
!   2000-08-31:  Allow the program to recognize SKINTEMP as something to be   !
!                used as a sea-surface temperature.                           !
!                                                                             !
!*****************************************************************************!
!                                                                             !
! This program reads GRIB-formatted data and puts it into the REGRID format   !
! for passing data to program "regridder".  This format is described in file  !
! "REGRID/pregrid/Doc/Format".                                                !
!                                                                             !
! The program tries to read from files called "GRIBFILE.AA", "GRIBFILE.AB",   !
! "GRIBFILE.AC", ... "GRIBFILE.BA", "GRIBFILE.BB", ... "GRIBFILE.ZZ" until it !
! cannot find a file.  This naming format allows for up to 676 files, which   !
! should be enough for most applications.                                     !
!                                                                             !
! The program searches through those "GRIBFILE.**" files, and pulls out all   !
! the requested fields which fall between a starting date and an ending date. !
! It writes the fields from a given time period to a file named according to  !
! the date and hour, i.e., "FILE:YYYY-MM-DD_HH"                               !
!                                                                             !
!*****************************************************************************!
program pregrid_grib

  use table
  use gridinfo
  use storage_module
  use filelist
  implicit none

  integer :: nunit1 = 12
  character(LEN=11) :: gribflnm = 'GRIBFILE.AA'

  integer, parameter :: alength = 1200000
  real, dimension (alength) :: array

  integer :: iprint

  integer , parameter :: maxlvl = 100

  real , dimension(maxlvl) :: plvl
  integer :: iplvl

  integer :: nlvl

  real :: startlat, startlon, deltalat, deltalon
  real :: level
  character (LEN=9) ::  field

  logical :: readit

  integer, dimension(255) :: iuarr = 0

  character (LEN=19) :: HSTART, HEND, HDATE
  character(LEN=19) :: hsave  = '0000-00-00_00:00:00'
  integer :: itime
  integer :: ntimes
  integer :: interval
  integer :: ierr
  logical :: ordered_by_date
  logical :: debug_print

  call pregrid_version

! Read the pregrid namelist, and return the information we want:

  call read_pregrid_namelist(hstart, hend, interval, ntimes, &
       ordered_by_date, debug_print, iprint)

! Read the "Vtable" file, and put the information contained therein into 
! the module "table".

  call parse_table(debug_print)

! Initialize the input filename to 'GRIBFILE.A{character just before A}'
! That way, when we update the filename below for the first time, it will 
! have the correct value of 'GRIBFILE.AA'.

  gribflnm(11:11)=char(ichar(gribflnm(11:11))-1)

! LOOP2 cycles through the list of files named 'GRIBFILE.**', until it finds
! a non-existent file.  Then we exit

  LOOP2 : DO

     ! At the beginning of LOOP2 update the input filename.
     if (gribflnm(11:11).eq.'Z') then
        gribflnm(10:10) = char(ichar(gribflnm(10:10))+1)
        gribflnm(11:11) = 'A'
     else
        gribflnm(11:11) = char(ichar(gribflnm(11:11))+1)
     endif

     ! Set READIT to .TRUE., meaning that we have not read any records yet 
     ! from the file GRIBFLNM.

     readit = .TRUE.  ! i.e., "Yes, we want to read a record."

! LOOP1 reads through the file GRIBFLNM, exiting under two conditions:
!        1) We've hit the end-of-file
!        2) We've read past the ending date HEND.
!
! Condition 2 assumes that the data in file GRIBFLNM are ordered in time.

     LOOP1 : DO
        ! At the beginning of LOOP1, we're at a new time period.
        ! Clear the storage arrays and associated level information.
        nlvl = 0
        plvl = -999.
        call clear_storage

! LOOP0 reads through the file GRIBFLNM, looking for data of the current 
! date.  It exits under the following conditions.
!          1) We've hit the end-of-file
!          2) The GRIBFLNM variable has been updated to a nonexistent file.
!          3) We start reading a new date and the data are assumed to be 
!             ordered by date.
!          4) We have a valid record and the data are not assumed to be 
!             ordered by date.

        LOOP0 : DO

           ! If we need to read a new grib record, then read one.
           if (READIT) then

              call rd_grib(nunit1, gribflnm, level, field, &
                   hdate, array, alength, ierr, iuarr, iprint)
              if (ierr.eq.1) then 
                 ! We've hit the end of a file.  Exit LOOP0 so we can 
                 ! write output for date HDATE, and then exit LOOP1
                 ! to update the GRIBFLNM
                 hsave = hdate
                 exit LOOP0
              endif

              if (ierr.eq.2) then
                 ! We've hit the end of all the files.  We can exit LOOP2
                 ! because there are no more files to read.
                 exit LOOP2
              endif

              if (debug_print) print*, 'Read a record '//field//' with date ', hdate(1:13)

           endif

           if (debug_print) print*, "hdate, hsave = "//hdate(1:13)//"  "//hsave(1:13)

           if (hdate < hstart) then
              ! The data we've read has a date HDATE earlier than the starting
              ! date HSTART.  So cycle LOOP0 to read the the next GRIB record.
              READIT = .TRUE.
              cycle LOOP0
           endif

           if (FIELD.EQ.'NULL') then
              ! The data we've read does not match any fields we've requested
              ! in the Vtable.  So cycle LOOP0 to read the next GRIB record.
              READIT = .TRUE.
              cycle LOOP0
           endif

           if (ordered_by_date .and. (hdate > hsave)) then

              ! Exit LOOP0, because we've started to read data from another 
              ! date.

              if (debug_print) then
                 write(*, '(A19, " > ", A19, " so exit LOOP0")') hdate, hsave
              endif

              ! We set READIT to FALSE because we have not yet processed
              ! the data from this record, and we will want to process this
              ! data on the next pass of LOOP1 (referring to the next time
              ! period of interest.

              READIT = .FALSE.

              exit LOOP0

           endif

! When we've reached this point, we have a data array ARRAY which has 
! some data we want to save, with field name FIELD at pressure level 
! LEVEL (Pa).  Dimensions of this data are map%nx and map%ny.  Let's put
! this data into storage.

           if (((field == "SST").or.(field == "SKINTEMP")) .and. &
                (level /= 200100.)) level = 200100.
           iplvl = int(level)
           call put_storage(iplvl,field, &
                reshape(array(1:map%nx*map%ny),(/map%nx, map%ny/)),&
                map%nx,map%ny)

           ! Since we've processed the record we've just read, we set
           ! READIT to .TRUE. so that LOOP0 will read the next record.
           READIT = .TRUE.

           if (.not. ordered_by_date) then
              if (hdate >= hstart) then
                 hsave = hdate
              endif
              exit LOOP0
           endif

        enddo LOOP0

! When we've reached this point, we've either hit the end of file, or 
! we've hit the end of the current date.  Either way, we want to output
! the data we've found for this date.  This current date is in HSAVE.
! However, if (HSAVE == 0000-00-00_00:00:00), no output is necessary,
! because that 0000 date is the flag for the very opening of a file.

        if ((hsave(1:4).ne.'0000').and.(hsave.le.hend)) then
           if (debug_print) print*, 'Calling output: '//hsave(1:13)
           call output(hsave, nlvl, maxlvl, plvl, interval, 1)
           hsave=hdate

           ! If the next record we process has a date later than HEND,
           ! it's time to exit LOOP1.
           if ((ordered_by_date) .and. (hdate.gt.hend)) exit LOOP1

        else
           hsave = hdate
        endif

        ! If we've hit the end-of-file, it's time to exit LOOP1 so we can
        ! increment the GRIBFLNM to read the next file.
        if (ierr.eq.1) exit LOOP1

     enddo LOOP1

! When we've reached this point, we've read all the data we want to from 
! file GRIBFLNM (either because we've reached the end-of-file, or we've 
! read past the date HEND).  So we close the file and cycle LOOP2 to read 
! the next file.

     call cclose(iuarr(nunit1), iprint, ierr)
     iuarr(nunit1) = 0
     hsave = '0000-00-00_00:00:00'

  enddo LOOP2

! Now Reread, process, and reoutput.

  write(*,*) 'First pass done, doing a reprocess'

  call rrpr(hstart, ntimes, interval, nlvl, maxlvl, plvl, debug_print)

! Make sure the filedates are in order, with an inefficient sort:

  call sort_filedates

! Interpolate temporally to fill in missing times:

  call datint(filedates, nfiles, hstart, ntimes, interval)

! Now delete the temporary files:

  call file_delete(filedates, nfiles, "PFILE:", interval)

! And Now we're done:

  write(*,'(/,10("*"), /, &
       &"Normal termination of program pregrid_grib.",/, &
       &10("*")/)')

contains
  subroutine sort_filedates
    implicit none

    integer :: n
    logical :: done
    if (nfiles > 1) then
       done = .FALSE.
       do while ( .not. done)
          done = .TRUE.
          do n = 1, nfiles-1
             if (filedates(n) > filedates(n+1)) then
                filedates(size(filedates)) = filedates(n)
                filedates(n) = filedates(n+1)
                filedates(n+1) = filedates(size(filedates))
                filedates(size(filedates)) = '0000-00-00 00:00:00.0000'
                done = .FALSE.
             endif
          enddo
       enddo
    endif
  end subroutine sort_filedates

end program pregrid_grib
