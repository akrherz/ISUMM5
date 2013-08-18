module storage_module
   integer, parameter :: idim = 63
   integer, parameter :: jdim = 63
   type data
      character (LEN=9) :: id
      character (LEN=19) :: date
      real, dimension(idim,jdim) :: data_array
      type(data), pointer :: next_data
   end type data
   type level
      integer :: p_level
      type(data), pointer :: datalev
      type(level), pointer :: next_level
   end type level

   type(level), pointer :: base
   logical :: first_call = .TRUE.
end module storage_module

subroutine clear_storage
  use storage_module

  type(data), pointer :: cdata, ldata
  type(level), pointer :: current, llevel


  if (first_call) then
     nullify(base)
     allocate(base)
     base%p_level = 300000
     nullify(base%next_level)
     nullify(base%datalev)
     first_call = .FALSE.
  else


     levloop : do

     ! Bop down to the deepest level, saving a pointer to the next highest level.
        current => base
        nullify(llevel)
        LLOOP : do while (associated(current%next_level))
           llevel => current
           current => current%next_level
        enddo LLOOP
        if (current%p_level .eq. 300000) exit levloop


        fieldloop : do
     ! Now bop over to the lowest field, saving a pointer to the next higher field.
           cdata => current%datalev
           nullify(ldata)
           CLOOP : do while (associated(cdata%next_data))
              ldata => cdata
              cdata => cdata%next_data
           enddo CLOOP
           if (associated(ldata)) then 
              ! This is not the last field for this level
              nullify(cdata%next_data)  
              deallocate(cdata)
              nullify(cdata)
              deallocate(ldata%next_data)
              nullify(ldata%next_data)
              cdata => ldata
              deallocate(ldata)
              nullify(ldata)
              cycle fieldloop
           else
              ! This is the last field for this level
              nullify(cdata%next_data)
              deallocate(cdata)
              nullify(cdata)
              nullify(current%next_level)
              deallocate(current%datalev)
              nullify(current%datalev)
              deallocate(current)
              nullify(current)
              deallocate(llevel%next_level)
              nullify(llevel%next_level)
              exit fieldloop
           endif
        enddo fieldloop
        if (llevel%p_level.eq.300000) exit levloop
     enddo levloop

     deallocate(base)
     nullify(base)
     allocate(base)
     nullify(base%next_level)
     nullify(base%datalev)
     base%p_level = 300000

  endif


end subroutine clear_storage


subroutine put_storage(pres, inname, scr2d, ix, jx, hdate)
  use storage_module

  character(len=19) :: hdate
  type(level), pointer :: current
  type(data), pointer  :: cdata

  integer :: pres
  real, dimension(ix,jx) :: scr2d
  character (len=*) :: inname
  character (len=9) :: name
  name = trim(inname)

  current => base

  LEVLLOOP : do

     if (current%p_level .eq. pres) then
        cdata => current%datalev
        DATALOOP : do
           if (cdata%id.eq.name) then
              print*, 'Overwriting ', name, ' at level ', pres
              exit LEVLLOOP
           endif
           if (associated(cdata%next_data)) then
              cdata => cdata%next_data
           else
              allocate(cdata%next_data)
              cdata => cdata%next_data
              nullify(cdata%next_data)
              exit LEVLLOOP
           endif
        enddo DATALOOP

     endif
     if (associated(current%next_level)) then
        current => current%next_level
     else
        allocate(current%next_level)
        current => current%next_level
        nullify(current%next_level)
        allocate(current%datalev)
        cdata => current%datalev
        nullify(cdata%next_data)
        exit LEVLLOOP
     endif
  enddo LEVLLOOP

  current%p_level = pres
  cdata%id = name
  cdata%data_array = scr2d
  cdata%date = hdate
end subroutine put_storage

subroutine get_storage(pres, inname, scr2d, hdate)
  use storage_module
  character(len=19) :: hdate
  type(level), pointer :: current
  type(data), pointer :: cdata
  integer :: pres
  character (len=*) :: inname
  character (len=9) :: name
  real, dimension(idim,jdim) :: scr2d

  name = trim(inname)

  current => base

  do while (associated(current))
     if (current%p_level.eq.pres) then
        cdata => current%datalev
        do while (associated(cdata))
           if (cdata%id.eq.name) then
              scr2d = cdata%data_array
              hdate = cdata%date
              return
           endif
           cdata => cdata%next_data
        enddo
        print*, 'Could not find ', name, ' at level ', current%p_level, pres
        print*, 'Subroutine GET_STORAGE'
        stop
     endif
     current => current%next_level
  enddo
  print*, 'Could not find ', pres
  print*, 'Subroutine GET_STORAGE'
  stop

end subroutine get_storage

subroutine print_storage
  use storage_module
  type(level), pointer :: current
  type(data), pointer :: cdata
  current => base

  do while (associated(current))
     print*, 'p_level = ', current%p_level
     cdata => current%datalev
     do while (associated(cdata))
        print*, '     data: ', cdata%id, &
                               cdata%data_array(1,1), cdata%date
        cdata => cdata%next_data
     enddo

     current => current%next_level
  enddo
end subroutine print_storage

subroutine get_plvls(plvl, maxlvl, nlvl)
  use storage_module
  type(level), pointer :: current
  real, dimension(maxlvl) :: plvl
  current => base
  nlvl = 0
  plvl = -999.
  do while (associated(current))
     if (current%p_level .ne. 300000) then
        nlvl = nlvl + 1
        plvl(nlvl) = float(current%p_level)
     endif
     current => current%next_level
  enddo
end subroutine get_plvls

logical function is_there(pres, inname)
  use storage_module
  type(level), pointer :: current
  type(data), pointer :: cdata
  integer :: pres
  character (len=*) :: inname
  character (len=9) :: name

  name = trim(inname)
  current => base

  do while (associated(current))
     if (current%p_level.eq.pres) then
        cdata => current%datalev
        do while (associated(cdata))
           if (cdata%id.eq.name) then
              is_there = .TRUE.
              return
           endif
           cdata => cdata%next_data
        enddo
        is_there = .FALSE.
        return
     endif
     current => current%next_level
  enddo
  is_there = .FALSE.
end function is_there
