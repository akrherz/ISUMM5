!*****************************************************************************!
! Subroutine PARSE_TABLE                                                      !
!                                                                             !
! Part of:                                                                    !
!    REGRID/pregrid/util                                                      !
!                                                                             !
! Purpose:                                                                    !
!    Read the Vtable, and fill arrays in the TABLE module with the Vtable     !
!    information.  Broadly, the Vtable file is how the user tells the pregrid !
!    program what fields to extract from the archive files.  The contents and !
!    format of file Vtable is described in REGRID/pregrid/Doc/Vtable_Format.  !
!                                                                             !
! Argument list:                                                              !
!    Input: DEBUG_PRINT:  T for extra prints; F for little printout.          !
!                                                                             !
! Externals:                                                                  !
!    Module TABLE                                                             !
!    Subroutine ABORT                                                         !
!                                                                             !
! Side Effects:                                                               !
!                                                                             !
!    - File "Vtable" is opened, read, and closed as Fortran unit 10.          !
!                                                                             !
!    - Various prints, especially if DEBUG_PRINT = .TRUE.                     !
!                                                                             !
!    - Abort for some miscellaneous error conditions.                         !
!                                                                             !
!    - Variables in module TABLE are filled., specifically, variables         !
!        MAXVAR                                                               !
!        MAXOUT                                                               !
!                                                                             !
!    - Arrays in module TABLE are filled., specifically, arrays               !
!        NAMVAR                                                               !
!        NAMEOUT                                                              !
!        UNITOUT                                                              !
!        DESCOUT                                                              !
!        GCODE                                                                !
!        LCODE                                                                !
!        LEVEL1                                                               !
!        LEVEL2                                                               !
!        IPRTY                                                                !
!        DUNITS                                                               !
!        DDESC                                                                !
!                                                                             !
! Author: Kevin W. Manning                                                    !
!         NCAR/MMM                                                            !
!         Summer 1998, and continuing                                         !
!         SDG                                                                 !
!                                                                             !
!*****************************************************************************!

subroutine parse_table(debug_print)
  use Table
  implicit none
  logical :: debug_print ! T/F Flag for extra printout.

  character(LEN=255) :: string = ' '
  integer :: ierr
  integer :: istart, ibar, i, j, ipcount
  integer :: nstart, maxtmp
  logical :: lexist

! added for IBM
  blankcode = -99
  splatcode = -88
! end added for IBM

! Open the file called "Vtable"

  open(10, file='Vtable', status='old', form='formatted', iostat=ierr)

! Check to see that the OPEN worked without error.

  if (ierr.ne.0) then
     inquire(file='Vtable', exist=LEXIST)
     write(*,'(//," ***** ERROR in Subroutine PARSE_TABLE:",//)')
     if (.not.lexist) then
        write(*,'(10x,"Problem opening file ''Vtable''.",/,&
             &10x,"File ''Vtable'' does not exist.")')
     else
        write(*,'(10x,"Problem opening file ''Vtable''.",/,&
             &10x,"File ''Vtable'' exists, but Fortran OPEN statement &
             &failed ",/,10x,"with error ", I3)') ierr
     endif
     write(*,'(//" ***** Abort in Subroutine PARSE_TABLE",//)')
     stop
  endif

! First, read past the headers, i.e., skip lines until we hit the first
! line beginning with '-'
  do while (string(1:1).ne.'-')
     read(10,'(A255)', iostat=ierr) string
     if (ierr /= 0) then
        write(*, '("Read error 1 in PARSE_TABLE.")')
        stop "PARSE_TABLE"
     endif
  enddo
  string = ' '

! Now interpret everything from here to the next '-' line:
!
  RDLOOP : do while (string(1:1).ne.'-')
     read(10,'(A255)', iostat=ierr) string
     if (ierr /= 0) then
        write(*, '("Read error 2 in PARSE_TABLE.")')
        stop "PARSE_TABLE"
     endif
     if (string(1:1).eq.'#') cycle RDLOOP
     if (len_trim(string) == 0) cycle RDLOOP
     if (string(1:1).eq.'-') then
        ! Skip over internal header lines
        BLOOP : do
           read(10,'(A255)', iostat=ierr) string
           if (ierr /= 0) exit RDLOOP
           if (len_trim(string) == 0) then
              cycle BLOOP
           else
              exit BLOOP
           endif
        enddo BLOOP
        do while (string(1:1).ne.'-')
           read(10,'(A255)', iostat=ierr) string
           if (ierr /= 0) then
              write(*, '("Read error 3 in PARSE_TABLE .")')
              stop "PARSE_TABLE"
           endif
        enddo
        string(1:1) = ' '
        
     elseif (string(1:1).ne.'-') then
        ! This is a line we'll need to interpret.
        maxvar = maxvar + 1 ! increment the variable count

        istart = 1
        ! There are seven fields to each line.
        do i = 1, 7
           ! The fields are delimited by '|'
           ibar = index(string(istart:255),'|') + istart - 2

           if (i.eq.1) then
              ! The first field is the code number:
              if (string(istart:ibar) == ' ') then
                 Gcode(maxvar) = blankcode
              elseif (scan(string(istart:ibar),'*') /= 0) then
                 write(*,'(//," ***** ERROR in Subroutine PARSE_TABLE:",//)')
                 write(*,'("It is probably unwise to use the VTable to &
                      &request ALL variables.",/,"Please give a GRIB Code &
                      &number, rather than ''*'', in the first column ",/,&
                      &"of the Vtable.",//, A)') trim(string)
                 write(*,'(//" ***** Abort in Subroutine PARSE_TABLE",//)')
                 stop
              else
                 read(string(istart:ibar), '(I10)') Gcode(maxvar)
              endif

           elseif (i.eq.2) then
              ! The second field is the level code:

              if (string(istart:ibar) == ' ') then
                 if (Gcode(maxvar) /= blankcode) then
                    write(*,'(//," ***** ERROR in Subroutine PARSE_TABLE:",//)')
                    write(*,'("Please supply a level code in the Vtable:",/, A)')&
                         trim(string)
                    write(*,'(//" ***** Abort in Subroutine PARSE_TABLE",//)')
                    stop
                 else
                    lcode(maxvar) = blankcode
                 endif
              elseif (scan(string(istart:ibar),'*') /= 0) then
                 write(*,'(//," ***** ERROR in Subroutine PARSE_TABLE:",//)')
                 write(*,'("It is probably unwise to use the VTable to &
                      &request a variables at ALL types of levels.",/,&
                      &"Please give a Level Code &
                      &number, rather than ''*'', in the second column ",/,&
                      &"of the Vtable.",//, A)') trim(string)
                 write(*,'(//" ***** Abort in Subroutine PARSE_TABLE",//)')
                 stop
              else
                 read(string(istart:ibar), '(I10)') lcode(maxvar)
              endif

           elseif (i.eq.3) then
              ! The third field is the Level 1 value, which may be '*':
              if (string(istart:ibar) == ' ') then
                 level1(maxvar) = blankcode
              elseif (scan(string(istart:ibar),'*') == 0) then
                 read(string(istart:ibar), '(I10)') level1(maxvar)
              else
                 level1(maxvar) = splatcode
              endif

           elseif (i.eq.4) then
              ! The fourth field is the Level 2 value, which may be blank:
              if (string(istart:ibar) == ' ') then
                 if ( (lcode(maxvar) == 112) .or.&
                      (lcode(maxvar) == 116) ) then
                    write(*,'(//," ***** ERROR in Subroutine PARSE_TABLE:",//)')
                    write(*,'("Level Code ",I3," expects two Level values.",/,&
                         &"Please supply a Level 2 value in column 4 for &
                         &Vtable record:",/,A)')&
                         lcode(maxvar), trim(string)
                    write(*,'(//" ***** Abort in Subroutine PARSE_TABLE",//)')
                    stop
                 else
                    level2(maxvar) = blankcode
                 endif
              elseif (scan(string(istart:ibar),'*') /= 0) then
                 write(*,'(//," ***** ERROR in Subroutine PARSE_TABLE:",//)')
                 write(*,'("Please give a Level 2 value (or blank), rather &
                      &than ''*'', in the fourth column ",/,&
                      &"of the Vtable.",//, A)') trim(string)
                 write(*,'(//" ***** Abort in Subroutine PARSE_TABLE",//)')
                 stop
              else
                 read(string(istart:ibar), '(I10)') level2(maxvar)
              endif

           elseif (i.eq.5) then
              ! The fifth field is the field name:
              if (string(istart:ibar).ne.' ') then
                 nstart = 0
                 do while (string(istart+nstart:istart+nstart).eq.' ')
                    nstart = nstart + 1
                 enddo
                 namvar(maxvar) = string(istart+nstart:ibar)
              else
                 write(*,'("PARSE_TABLE : A field name is missing in &
                      &the Vtable.")')
                 stop
              endif
           elseif (i.eq.6) then
              ! The sixth field is the Units string, which may be blank:
              if (string(istart:ibar).ne.' ') then
                 nstart = 0
                 do while (string(istart+nstart:istart+nstart).eq.' ')
                    nstart = nstart + 1
                 enddo
                 Dunits(maxvar) = string(istart+nstart:ibar)
              else
                 Dunits(maxvar) = ' '
              endif

           elseif (i.eq.7) then
              ! The seventh field is the description string, which may be blank:
              if (string(istart:ibar).ne.' ') then
                 nstart = 0
                 do while (string(istart+nstart:istart+nstart).eq.' ')
                    nstart = nstart + 1
                 enddo
                 Ddesc(maxvar) = string(istart+nstart:ibar)

                 ! If the description string is not blank, this is a
                 ! field we want to output.  In that case, copy the
                 ! field name to the MAXOUT array:
                 maxout = maxout + 1
                 nameout(maxout) = namvar(maxvar)
                 unitout(maxout) = Dunits(maxvar)
                 descout(maxout) = Ddesc(maxvar)

              else
                 Ddesc(maxvar) = ' '
              endif

           endif

           istart = ibar + 2

        enddo
     endif
  enddo RDLOOP
! Now we've finished reading the file.  
  close(10)

! Now remove duplicates from the NAMEOUT array.  Duplicates may arise
! when we have the same name referred to by different level or parameter
! codes in some dataset.

  maxtmp = maxout
  do i = 1, maxtmp-1
     do j = i+1, maxtmp
        if ((nameout(i).eq.nameout(j)).and.(nameout(j).ne.' ')) then
           if (debug_print) then
              write(*,'("Duplicate name.  Removing ",A9," from output list.")') nameout(j)
           endif
           nameout(j:maxlines-1) = nameout(j+1:maxlines)
           unitout(j:maxlines-1) = unitout(j+1:maxlines)
           descout(j:maxlines-1) = descout(j+1:maxlines)
           maxout = maxout - 1
        endif
     enddo
  enddo

! Compute a priority level based on position in the table:
! This assumes GRIB.

! Priorities are used only for surface fields.  If it is not a
! surface fields, the priority is assigned a value of 100.

! For surface fields, priorities are assigned values of 100, 101,
! 102, etc. in the order the field names appear in the Vtable.

  ipcount = 99
  do i = 1, maxvar
     if (lcode(i).eq.105) then
        ipcount = ipcount + 1
        iprty(i) = ipcount
     elseif (lcode(i).eq.116.and.level1(i).le.50.and.level2(i).eq.0) then
        ipcount = ipcount + 1
        iprty(i) = ipcount
     else
        iprty(i) = 100
     endif
  enddo

  if (debug_print) then
     write(*,'(//"Read from file ''Vtable'' by subroutine PARSE_TABLE:")')
     do i = 1, maxvar
        write(*,'(4I6, 3x,A10)')&
             gcode(i), lcode(i), level1(i), level2(i), namvar(i)
     enddo
     write(*,'(//)')
  endif
        
end subroutine parse_table
