subroutine output(scr2d, sstdates, nsst, hstart, hend)
  use table
  use gridinfo
  implicit none
  logical, external :: is_there
  real, dimension(idim,jdim) :: scr2d
  integer :: nsst
  character(len=19) :: hdate, hstart, hend, sstdates(1000)
  character (LEN=9) :: field

  if (sstdates(1).ne.hstart) then
     if (is_there(200100, 'PRESST  ')) then
        call get_storage(200100, 'PRESST  ', scr2d, hdate)
        call writsst(scr2d, hdate, idim, jdim)
        sstdates(2:nsst+1) = sstdates(1:nsst)
        sstdates(1) = hdate
        nsst = nsst+1
     endif
  endif

  if (sstdates(nsst).ne.hend) then
     if (is_there(200100, 'POSTSST  ')) then
        call get_storage(200100, 'POSTSST ', scr2d, hdate)
        call writsst(scr2d, hdate, idim, jdim)
        nsst = nsst + 1
        sstdates(nsst) = hdate
     endif
  endif

end subroutine output
