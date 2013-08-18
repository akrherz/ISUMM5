! Right now, RIF_MODULE is packaged with the build_mosaic.F program.  If
! this module turns out to be handy for other purposes, we may pull it out 
! as a separate source-code file.

module rif_module_3_0
  ! A couple of version numbers, unused now, but may be used in the 
  ! future for backward compatibility.  Big plans .... Who knows?
  integer, parameter :: RIF_major = 3, RIF_minor = 0

  type rifdata_struct
     ! RIF Record 1: RIF Format version
     integer :: IFV

     ! RIF Record 2: Field and Grid information independent of projection     !
     character(len=24) :: HDATE !\                                            !
     real :: XFCST              ! \                                           !
     character(len=9)  :: NAME  !  \                                          !
     character(len=25) :: UNITS !   \ Record 2 of the RIF data.               !
     character(len=46) :: DESC  !   /                                         !
     real :: XLVL               !  /                                          !
     integer :: NX, NY          ! /                                           !
     integer :: IPROJ           !/                                            !

     ! RIF Record 3: Grid information depending on the projection
     real :: STARTLAT, STARTLON ! Used for IPROJ = 0, 1, 3, 5
     real :: DELTALAT, DELTALON ! Used for IPROJ = 0
     real :: DX, DY             ! Used for IPROJ = 1, 3, 5
     real :: XLONC              ! Used for IPROJ = 3, 5
     real :: TRUELAT1           ! Used for IPROJ = 1, 3, 5
     real :: TRUELAT2           ! Used for IPROJ = 3

     ! RIF Record 4: The data record
     real, pointer, dimension(:,:) :: data

  end type rifdata_struct

contains

  subroutine read_one_rif(iunit,fld,istatus)
! 
! Read a single set of header and field records (a total of four records)
! from a RIF file.  Fills the appropriate elements of the FLD data structure.
! Return status = 0 if the read is succesful.  Return status is 1 if
! end-of-data is hit upon trying to read record 1.  Program stops if any
! other read errors occur.
!
    implicit none
    integer, intent(in) :: iunit
    type(rifdata_struct), intent(out) :: fld

    integer, intent(out) :: istatus ! Status flag:  0 = successful read.
                                    !               1 = end-of-file.
    integer :: ierr

    ! Read the first record, the format version number:

    read (IUNIT, IOSTAT=IERR) fld%IFV

    if (IERR > 0) then
       write(*, '(/,"Trouble reading IFV.",/)')
       stop 00001
    endif

    if (IERR < 0) then
       ! No more data to read.  Set status flag to 1 and return:
       ISTATUS = 1
       return
    endif

    ! We expect format version number to match the software:

    if (fld%IFV /= RIF_major) THEN
       print*, 'Unrecognized format version IFV: IFV = ', fld%IFV
       stop 00002
    endif

    ! Read the second record, common to all projections:

    read (IUNIT, iostat=ierr) fld%HDATE, fld%XFCST, fld%NAME, &
         fld%UNITS, fld%DESC, fld%XLVL, fld%NX, fld%NY, fld%IPROJ

    if (IERR /= 0) then
       write(*, '(/,"Trouble reading RIF Record 2.",/)')
       stop 00003
    endif

    ! Read the third record, which depends on the projection:

    select case (fld%iproj)
    case (0)

       !  This is the Cylindrical Equidistant (lat/lon) projection:
       read (IUNIT, iostat=ierr) fld%STARTLAT, fld%STARTLON, fld%DELTALAT, &
            fld%DELTALON

    case (1)

       !  This is the Mercator projection:
       read (IUNIT, iostat=ierr) fld%STARTLAT, fld%STARTLON, fld%DX, fld%DY, &
            fld%TRUELAT1

    case (3)

       ! This is the Lambert Conformal projection:
       read (IUNIT, iostat=ierr) fld%STARTLAT, fld%STARTLON, fld%DX, fld%DY,&
            fld%XLONC, fld%TRUELAT1, fld%TRUELAT2

    case (5)

       ! This is the Polar Stereographic projection:
       read (IUNIT, iostat=ierr) fld%STARTLAT, fld%STARTLON, fld%DX, fld%DY,&
            fld%XLONC, fld%TRUELAT1

    case default

       print*, 'Unrecoginized IPROJ: IPROJ = ', fld%iproj
       stop 00004

    end select

    if (ierr /= 0) then
       write(*, '(/,"Trouble reading RIF Record 3.",/)')
       stop 00005
    endif

    ! Now that we know the size of the field to follow in the file, 
    ! we can allocate our SLAB pointer array:

    allocate(fld%data(fld%NX,fld%NY), stat=ierr)

    if (ierr /= 0) then
       write(*, '(/,"Trouble allocating space for RIF data.",/)')
       stop 00006
    endif

    ! Now that we have allocated the SLAB array, we can read the field in 
    ! the fourth record:

    read (IUNIT, iostat=ierr) fld%data

    if (ierr /= 0) then
       write(*, '(/,"Trouble reading RIF record 4 (Data record).",/)')
       stop 00007
    endif

    ! All done, and successful.
    ISTATUS = 0
  end subroutine read_one_rif

end module rif_module_3_0


module bm_storage_module
! Module for managing the storage of Regrid Intermediate Format data.
  use rif_module_3_0
  implicit none
  private
  public :: node2
!KWM  public :: get_storage
!KWM  public :: get_dims
!KWM  public :: get_plvls
  public :: bmstor_put
  public :: bmstor_next
!KWM  public :: llstor_start
!KWM  public :: clear_storage
!KWM  public :: refr_storage
!KWM  public :: is_there
!KWM  public :: print_storage
  public :: bmstor_set
!KWM  public :: bmstor_get

  integer, parameter :: idlen = 9
  integer :: verbose = 2  ! 0 = no prints; >0 = some prints.

  type node2
     type(rifdata_struct) :: fld
     type(node2), pointer :: next
  end type node2

  type node1
     integer :: id
     type(node1), pointer :: next
     type(node2), pointer :: first
  end type node1

  type(node1), target :: root
  type(node1), pointer :: nnode
  type(node2), pointer :: current

  integer, public :: iferr

contains

  subroutine bmstor_start(icode)
    implicit none
    integer, intent(in) :: icode
!
! First, check to see that the list ICODE has not already been started:
!
    nnode => root
    SEARCH : do while (associated(nnode%next))
       nnode => nnode%next
       if (nnode%id == icode) then
          if (verbose.gt.0) write(*,&
               '(/,"BMSTOR_START: NNODE EXISTS, not starting ", I8, /)') icode
          return
       endif
    enddo SEARCH
!
! Since it's a new ICODE, add it to the list of lists:
!
    allocate(nnode%next)
    nnode => nnode%next
    nnode%id = icode
    if (verbose.gt.0) write(*, '(/,"NNODE%ID = ", I8, /)') nnode%id
    allocate(nnode%first)
    nnode%first%fld%name = 'Root'
    nullify(nnode%first%next)
    nullify (nnode%next)
  end subroutine bmstor_start

!KWM  subroutine bmstor_clear
!KWM    implicit none
!KWM    type(node1), pointer :: holdnn
!KWM    type(node2), pointer :: hold
!KWM
!KWM    if (verbose > 0) then
!KWM       print*, 'Call bmstor_clear.'
!KWM    endif
!KWM
!KWM    SEARCH : do
!KWM
!KWM       nnode => root
!KWM       SCANF : do while (associated(nnode%next))
!KWM          holdnn => nnode
!KWM          nnode => nnode%next
!KWM       enddo SCANF
!KWM       if (nnode%id == 0) exit SEARCH
!KWM
!KWM       N2: do 
!KWM          current => nnode%first
!KWM          do while (associated(current%next))
!KWM             hold => current
!KWM             current => current%next
!KWM          enddo
!KWM          if (current%id /= "Root") then
!KWM             if (associated(current%data2d)) then
!KWM                if (verbose > 0) then
!KWM                   print*, 'Deallocating and nullifying 2d.', &
!KWM                        nnode%id, current%id
!KWM                endif
!KWM                deallocate(current%data2d)
!KWM                nullify(current%data2d)
!KWM             endif
!KWM          endif
!KWM          nullify(hold%next)
!KWM          if (current%id == nnode%first%id) then
!KWM             deallocate(current)
!KWM             nullify(current)
!KWM             exit N2
!KWM          endif
!KWM       enddo N2
!KWM       nullify(holdnn%next)
!KWM
!KWM    enddo SEARCH
!KWM
!KWM  end subroutine bmstor_clear

  subroutine bmstor_find_node1(inname)
    implicit none
    integer :: inname, name
    name = inname
    nnode => root
    SEARCH : do while (associated(nnode%next))
       nnode => nnode%next
       if (nnode%id == name) then
          iferr = 0
          return
       endif
    enddo SEARCH
    if (verbose > 0) then
       print '("BMSTOR_FIND_NODE1: Name not found:  ", I8)',  name
    endif
    iferr = 1
  end subroutine bmstor_find_node1


!KWM  subroutine get_plvls(plvl, maxlvl, nlvl)
!KWM    implicit none
!KWM    integer :: maxlvl, nlvl
!KWM    real, dimension(maxlvl) :: plvl
!KWM    integer :: nn
!KWM
!KWM    nnode => root
!KWM    nlvl = 0
!KWM    plvl = -99999
!KWM    SEARCH : do while (associated(nnode%next))
!KWM       nnode => nnode%next
!KWM       nlvl = nlvl + 1
!KWM       LEVLOOP : do nn = 1, nlvl
!KWM          if (nnode%id > plvl(nn)) then
!KWM             plvl(nn+1:maxlvl) = plvl(nn:maxlvl-1)
!KWM             plvl(nn) = float(nnode%id)
!KWM             exit LEVLOOP
!KWM          endif
!KWM       enddo LEVLOOP
!KWM    enddo SEARCH
!KWM  end subroutine get_plvls

  subroutine bmstor_put(icode, fld)
    implicit none
    type(rifdata_struct) :: fld
    integer :: icode
    integer :: ioffs, joffs

    if (verbose>0) print*, 'Put Storage: '

    call bmstor_find_node1(icode)
    if (iferr /= 0) then
       call bmstor_start(icode)
    endif
    current => nnode%first
    
    SEARCH : do while (associated(current%next))
       current => current%next
       if ( (current%fld%name == fld%name) .and. &
            (current%fld%xlvl == fld%xlvl)) then
          joffs = (fld%startlat+90.)/1.25
          ioffs = (fld%startlon+30.)/1.25
          if (ioffs < 0) ioffs = 288+ioffs
          current%fld%data(ioffs+1:ioffs+73, joffs+1:joffs+73) = fld%data
          current%fld%units = fld%units
          current%fld%desc = fld%desc
          current%fld%hdate = fld%hdate
          current%fld%xfcst = fld%xfcst
          current%fld%iproj = fld%iproj
          current%fld%nx = 289
          current%fld%ny = 145
          current%fld%startlat = -90.
          current%fld%startlon = -30.
          current%fld%deltalat = fld%deltalat
          current%fld%deltalon = fld%deltalon
          if (verbose.gt.0) write(*,'("BMSTOR_PUT: Overwriting ", A,&
               &" to ID ", I8, "   Value: ", F16.6)') current%fld%name, nnode%id,&
               fld%data(1,1)
          return
       endif
    enddo SEARCH
    allocate(current%next)
    current => current%next
    current%fld%name = fld%name
    ! Allocate for the full global dataset:
    allocate(current%fld%data(289,145))
    current%fld%data = -1.E30
    ! offsets for this set of data:
    joffs = (fld%startlat+90.)/1.25
    ioffs = (fld%startlon+30.)/1.25
    if (ioffs < 0) ioffs = 288+ioffs
    current%fld%data(ioffs+1:ioffs+73, joffs+1:joffs+73) = fld%data
    current%fld%name = fld%name
    current%fld%xlvl = fld%xlvl
    current%fld%units = fld%units
    current%fld%desc = fld%desc
    current%fld%hdate = fld%hdate
    current%fld%xfcst = fld%xfcst
    current%fld%iproj = fld%iproj
    current%fld%nx = 289
    current%fld%ny = 145
    current%fld%startlat = -90.
    current%fld%startlon = -30.
    current%fld%deltalat = fld%deltalat
    current%fld%deltalon = fld%deltalon
    nullify (current%next)
    if (verbose.gt.0) write(*,'("BMSTOR_PUT: Writing ", A,&
         &" to ID ", I8, "   Value: ", F16.6)') current%fld%name, nnode%id, fld%data(1,1)

  end subroutine bmstor_put

  integer function bmstor_next(icode, ptr) Result (IERR)
    integer :: icode
    type (node2), pointer :: ptr

    if (.not. associated (ptr)) then
       call bmstor_find_node1(icode)
       if (iferr /= 0) then
          print*, 'BMSTOR_NEXT: Cannot find code ', icode
          ierr = 1
          return
       endif
       ptr => nnode%first
    endif

    if (associated(ptr%next)) then
       current => ptr%next
       if (verbose.gt.0) write(*,'("BMSTOR_NEXT: Referencing ", A, " of ID ", I5)') &
            ptr%fld%name, icode

       nullify(ptr)
       ptr => current
       ierr = 0
    else
       ierr = 1
    endif
  end function bmstor_next

!KWM  subroutine bmstor_get(icode, name, data, idum, jdum)
!KWM    implicit none
!KWM    character(len=*) :: name
!KWM    integer :: icode
!KWM    integer :: idum, jdum
!KWM    real, dimension(:,:) :: data
!KWM
!KWM    call bmstor_find_node1(icode)
!KWM    if (iferr /= 0) then
!KWM       print*, 'Cannot find code ', icode, ' in routine GET_STORAGE.'
!KWM       stop 'GET_STORAGE_code'
!KWM    endif
!KWM    current => nnode%first
!KWM
!KWM    SEARCH : do while (associated(current%next))
!KWM       current => current%next
!KWM       if (current%id == name) then
!KWM          data = current%data2d
!KWM          map = current%data_map
!KWM          if (verbose.gt.0) write(*,'("GET_STORAGE: READING ", A,&
!KWM               &" at ID ", I8, "   Value: ", F16.6)') current%id, nnode%id,&
!KWM               & data(1,1)
!KWM          return
!KWM       endif
!KWM    enddo SEARCH
!KWM    write(*,'("GET_STORAGE : NAME not found: ", A)') name
!KWM
!KWM  end subroutine bmstor_get

!KWM  subroutine bmstor_refr(icode, name, Pdata, idum, jdum)
!KWM    implicit none
!KWM    character(len=*) :: name
!KWM    integer :: icode
!KWM    integer :: idum, jdum
!KWM    real, pointer, dimension(:,:) :: Pdata
!KWM
!KWM    call bmstor_find_node1(icode)
!KWM    if (iferr /= 0) then
!KWM       print*, 'Cannot find code ', icode, ' in routine REFR_STORAGE.'
!KWM       STOP 'REFR_STORAGE_code'
!KWM    endif
!KWM    current => nnode%first
!KWM
!KWM    SEARCH : do while (associated(current%next))
!KWM       current => current%next
!KWM       if (current%id == name) then
!KWM          Pdata => current%data2d
!KWM          map = current%data_map
!KWM          if (verbose.gt.0) write(*,'("REFR_STORAGE: Referencing ", A,&
!KWM               &" at ID ", I8, "   Value: ", F16.6)') current%id, nnode%id,&
!KWM               Pdata(1,1)
!KWM          return
!KWM       endif
!KWM    enddo SEARCH
!KWM    print '("REFR_STORAGE : NAME not found: ", A)', name
!KWM
!KWM  end subroutine bmstor_refr

!KWM  subroutine bmstor_remove(icode, name)
!KWM    implicit none
!KWM    character(len=*) :: name
!KWM    integer :: icode
!KWM
!KWM    call bmstor_find_node1(icode)
!KWM    if (iferr /= 0) then
!KWM       STOP 'bmstor_find_node1'
!KWM    endif
!KWM    current => nnode%first
!KWM
!KWM    do while (current%id /= name )
!KWM       if (.not. associated(current%next)) then
!KWM          print*, 'Not there : ', name
!KWM          return
!KWM       endif
!KWM       hold => current
!KWM       current => current%next
!KWM    enddo
!KWM
!KWM    if (associated(current%data2d)) then
!KWM       deallocate(current%data2d)
!KWM    endif
!KWM    nullify(hold%next)
!KWM    hold%next => current%next
!KWM    nullify(current%next)
!KWM    nullify(current)
!KWM
!KWM  end subroutine bmstor_remove

!KWM  subroutine get_dims(icode, name)
!KWM    implicit none
!KWM    character(len=*) :: name
!KWM    integer :: icode
!KWM
!KWM    call bmstor_find_node1(icode)
!KWM    if (iferr /= 0) then
!KWM       STOP 'get_dims'
!KWM    end if
!KWM    current => nnode%first
!KWM    
!KWM    SEARCH : do while (associated(current%next))
!KWM       current => current%next
!KWM       if (current%id == name) then
!KWM          map = current%data_map
!KWM          return
!KWM       endif
!KWM    enddo SEARCH
!KWM
!KWM  end subroutine get_dims

!KWM  subroutine print_storage(icode)
!KWM    implicit none
!KWM    integer :: isz
!KWM    integer, optional :: icode
!KWM
!KWM    if (present(icode)) then
!KWM       call bmstor_find_node1(icode)
!KWM       if (iferr /= 0) then
!KWM          STOP 'print_storage'
!KWM       end if
!KWM       print '("PRINT_NODE1: id = ", I8)' , nnode%id
!KWM       current => nnode%first
!KWM
!KWM       print*
!KWM       if (.not. associated(current)) then
!KWM          print '("Nothing there.")'
!KWM          return
!KWM       endif
!KWM       do while ( associated(current%next))
!KWM          if (current%id == 'Root') then
!KWM             print*, 'id = ', current%id
!KWM          elseif (current%id /= 'Root') then
!KWM
!KWM             if (associated(current%data2d)) then
!KWM                isz = size(current%data2d)
!KWM                print*, current%id, ' = ', current%data2d(1,1)
!KWM             endif
!KWM                
!KWM          endif
!KWM          current => current%next
!KWM       enddo
!KWM       if (current%id == 'Root') then
!KWM          print*, 'id = ', current%id
!KWM       elseif (current%id /= 'Root') then
!KWM          if (associated(current%data2d)) then
!KWM             isz = size(current%data2d)
!KWM             print*, current%id, ' = ', current%data2d(1,1)
!KWM          endif
!KWM       endif
!KWM       current => current%next
!KWM       print*
!KWM
!KWM    else
!KWM       nnode => root
!KWM       do while (associated(nnode%next))
!KWM          nnode => nnode%next
!KWM          print '("PRINT_NODE1: id = ", I8)' , nnode%id
!KWM
!KWM
!KWM          current => nnode%first
!KWM
!KWM          print*
!KWM          if (.not. associated(current)) then
!KWM             print '("Nothing there.")'
!KWM             return
!KWM          endif
!KWM          do while ( associated(current%next))
!KWM             if (current%id == 'Root') then
!KWM                print*, 'id = ', current%id
!KWM             elseif (current%id /= 'Root') then
!KWM                if (associated(current%data2d)) then
!KWM                   isz = size(current%data2d)
!KWM                   print*, current%id, ' = ', current%data2d(1,1), isz
!KWM                endif
!KWM             endif
!KWM             current => current%next
!KWM          enddo
!KWM          if (current%id == 'Root') then
!KWM             print*, 'id = ', current%id
!KWM          elseif (current%id /= 'Root') then
!KWM             if (associated(current%data2d)) then
!KWM                isz = size(current%data2d)
!KWM                print*, current%id, ' = ', current%data2d(1,1), isz
!KWM             endif
!KWM          endif
!KWM          current => current%next
!KWM          print*
!KWM
!KWM       enddo
!KWM    endif
!KWM  end subroutine print_storage

!KWM  logical function is_there(icode, name) RESULT(answer)
!KWM    implicit none
!KWM    character(len=*) :: name
!KWM    integer :: icode
!KWM
!KWM    answer = .FALSE.
!KWM
!KWM    if (verbose > 0) then
!KWM       write(*,'("Is there ",A," at ", i8, "?")', advance="NO") name, icode
!KWM    endif
!KWM
!KWM    call bmstor_find_node1(icode)
!KWM    if (iferr /= 0) go to 1000
!KWM    
!KWM    current => nnode%first
!KWM    
!KWM    SEARCH : do while (associated(current%next))
!KWM       current => current%next
!KWM       if (current%id == name) then
!KWM          answer = .TRUE.
!KWM          exit SEARCH
!KWM       endif
!KWM    enddo SEARCH
!KWM
!KWM1000 continue
!KWM
!KWM    if (verbose > 0) then
!KWM       write(*,*) answer
!KWM    endif
!KWM
!KWM
!KWM  end function is_there

  subroutine bmstor_set(ivrb)
    implicit none
    integer, optional :: ivrb
    if (present(ivrb)) verbose = ivrb
  end subroutine bmstor_set

end module bm_storage_module

!============================================================================
!============================================================================
!============================================================================
!============================================================================
!============================================================================
!============================================================================
!============================================================================
!============================================================================
!============================================================================
!============================================================================

program build_mosaic
  use rif_module_3_0
  use bm_storage_module
  implicit none

  integer, parameter :: IUNIT = 10, ICODE = 10
  integer, parameter :: OUNIT = 11
  integer :: istatus
  integer :: icount = 0

  character(len=120) :: flnm
  character(len=13) :: datestr = " "

  ! Turn off/on (0/1) printout of storage routines:
  call bmstor_set (ivrb=0)

  ARGLOOP : DO

     ! Process command-line arguments, until there are no more (istatus/=0).
     call handle_arguments(flnm, datestr, istatus)
     if (istatus /= 0) exit ARGLOOP

     ! Read the whole file and put it into BM (build_mosaic) storage:
     call bmstor_the_file(iunit, flnm, icode, icount)

  ENDDO ARGLOOP

  write(*,'(/,"End of read loop.")')
  write(*,'("Total number of fields read: ", i8,/)') icount
  write(*,'("Now write everything out to file ''",A,"''"/)') "MOSAIC:"//datestr

  ! Write everything out.
  call bm_dump_storage(ounit, icode, datestr)

end program build_mosaic

subroutine bm_dump_storage(OUNIT, ICODE, datestr)
  use bm_storage_module
  implicit none
  integer, intent(in) :: OUNIT, ICODE
  character(len=*), intent(in) :: datestr

  type(node2), pointer :: fldptr

! Write our mosaic data set.
  open(OUNIT, file='MOSAIC:'//datestr, form='unformatted', &
       status='unknown', action='write')
  nullify(fldptr)
  do while (bmstor_next(icode, fldptr) == 0)
     write (OUNIT) 3
     write (OUNIT) fldptr%fld%HDATE, fldptr%fld%XFCST, &
          fldptr%fld%NAME, fldptr%fld%UNITS, fldptr%fld%DESC, &
          fldptr%fld%XLVL, fldptr%fld%NX, fldptr%fld%NY, fldptr%fld%IPROJ
     write (OUNIT) fldptr%fld%STARTLAT, fldptr%fld%STARTLON, &
          fldptr%fld%DELTALAT, fldptr%fld%DELTALON
     write(OUNIT) fldptr%fld%data
  enddo
  close(OUNIT)

end subroutine bm_dump_storage

subroutine handle_arguments(harg, datestr, istatus)
  implicit none
  character(len=120), intent(out)  :: harg
  character(len=13), intent(inout) :: datestr
  integer, intent(out) :: istatus
  character(len=13) :: tempstr
  logical :: lexist
  integer, external :: iargc
  integer :: iarg = 0
  integer :: numarg = 0

  if (iarg == 0) numarg = iargc()

  if (numarg == 0) then
     write(*,'(/,"********** ERROR EXIT **********",/,"*****")')
     write(*,'("*****  Program expects command-line argument(s): ")')
     write(*,'("*****      the file names from which to build the mosaic.")')
     write(*,'("*****",/,"********** ERROR EXIT **********",/)')
     stop
  endif

  iarg = iarg + 1

  if (iarg > numarg) then
     istatus = 1
     return
  endif

  call getarg(iarg, harg)

  tempstr = harg(len_trim(harg)-12:len_trim(harg))
  if (datestr == " ") then
     datestr = tempstr
  else
     if (datestr /= tempstr) then
        write(*, '(/,"********** ERROR EXIT **********",/,"*****")')
        write(*, '("*****  Date strings on the file names do not match:")')
        write(*, '("*****        ",A, " != ", A)') datestr, tempstr
        write(*, '("*****",/,"********** ERROR EXIT **********",/)')
        stop
     endif
  endif
  harg = trim(harg)
  write(*, '("file = ",A,"  datestr = ", A)')  trim(harg), datestr

  ! Check to see if the file exists:
  inquire(file=trim(harg), exist=lexist)
  if (.not. lexist) then
     write(*,'(/,"********** ERROR EXIT **********",/,"*****")')
     write(*, '("*****  File does not exist: ",A)') trim(harg)
     write(*,'("*****",/,"********** ERROR EXIT **********",/)')
     stop
  endif

  istatus = 0

end subroutine handle_arguments

subroutine bmstor_the_file(iunit, flnm, icode, icount)
  use rif_module_3_0
  use bm_storage_module
  implicit none
  integer, intent(in)    :: iunit, icode
  character(len=*), intent(in) :: flnm
  integer, intent(inout) :: icount

  integer :: istatus
  type(rifdata_struct) :: fld
  
  ! Open the file for processing.
  open(iunit, file=flnm, form='unformatted', status='old', action='read')

  READLOOP : DO

     ! Read a single RIF (header/field) set:
     call read_one_rif(iunit, fld, istatus)
     if (istatus /= 0) exit READLOOP

     if (fld%IPROJ /= 0) then
        write(*,'(/,"********** ERROR EXIT **********",/,"*****")')
        write(*, '("*****  Program build_mosaic will only work with lat/lon grids.")')
        write(*,'("*****",/,"********** ERROR EXIT **********",/)')
        stop
     endif

     icount = icount + 1

     ! Put things into storage.
     call bmstor_put(icode, fld)

     ! Now that we've transferred our data to a global array in storage,
     ! we can deallocate and nullify our data array:

     deallocate(fld%data)
     nullify(fld%data)

     ! Loop back to read the next field.
  ENDDO READLOOP

end subroutine bmstor_the_file
