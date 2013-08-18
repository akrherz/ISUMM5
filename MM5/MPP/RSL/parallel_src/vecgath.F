#include <defines.incl>
#ifdef VECTOR
      subroutine vecgath( s,v,is,ie,js,je,ke,mix,mjx,mkx,ip )
      implicit none
      integer is,ie,js,je,ke,mix,mjx,mkx,ip
      real s(mix,mjx,mkx), v( mix*mjx, mkx )
      integer i,j,k

c        write(0,*)'vecgath is, ie ',is,ie

C_FLIC_BEGIN_NOFLIC
      DO K=1,KE
        IP=0
        do j=js_x 0 ( JS ) , je_x 0 ( JE  )
#ifndef vpp
          do i=is_x 0 ( IS ) , ie_x 0 ( IE )
            V(IP+i-(IS+idif_x0)+1,K)=S(I,J,K)
#else
          do i=IS,IE
c              if ( k .eq. 1) then
c                write(0,*)i,IP+i-IS+1,S(I,J,K)
c              endif
            V(IP+i-IS+1,K)=S(I,J,K)
#endif
          enddo
          IP=IP+IE-IS+1
        enddo
      ENDDO
C_FLIC_END_NOFLIC
      return
      end

      subroutine vecscat( v,s,is,ie,js,je,ke,mix,mjx,mkx,ip )
      implicit none
      integer is,ie,js,je,ke,mix,mjx,mkx,ip
      real s(mix,mjx,mkx), v( mix*mjx, mkx )
      integer i,j,k
C_FLIC_BEGIN_NOFLIC
      DO K=1,KE
        IP=0
        do j=js_x 0 ( JS ) , je_x 0 ( JE  )
#ifndef vpp
          do i=is_x 0 ( IS ) , ie_x 0 ( IE )
            S(I,J,K) = V(IP+i-(IS+idif_x0)+1,K)
#else
          do i=IS,IE
            S(I,J,K) = V(IP+i-IS+1,K)
#endif
          enddo
          IP=IP+IE-IS+1
        enddo
      ENDDO
C_FLIC_END_NOFLIC
      return
      end
#else
C stubs
      subroutine vecgath( s,v,is,ie,js,je,ke,mix,mjx,mkx,ip )
      return
      end
      subroutine vecscat( v,s,is,ie,js,je,ke,mix,mjx,mkx,ip )
      return
      end
#endif
