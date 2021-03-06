subroutine swap4(in,nn)
#if defined (DEC) || defined (ALPHA)
! swaps bytes in groups of 4 to compensate for byte swapping within
!    words which occurs on DEC machines.
  implicit none
  logical*1 , dimension(1) , intent(inout) :: in  ! Array to be swapped
  integer, intent(in) :: nn ! number of bytes to be swapped

  logical*1, dimension(4) :: ia
  integer :: i
  do i=1,nn,4
     ia = in(i+3:i:-1)
     in(i:i+3) = ia
  enddo
	
#endif
end
