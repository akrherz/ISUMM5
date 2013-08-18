      subroutine set_to_nan
C------------------------------Description------------------------------
C------------------------------Parameters-------------------------------
#include <parame.incl>
C------------------------------Commons----------------------------------
      COMMON /STATEVAR/PR1(MIX,MJX,MKX),RHO1(MIX,MJX,MKX)
C------------------------------Arguments--------------------------------
#include <dusolve1.incl>
#include <varia.incl>
#include <nhcnst.incl>
C Input arguments
C Output arguments
C Input/Output arguments
C---------------------------Local workspace-----------------------------
      real nn
      integer in, j
C------------------------------Externals--------------------------------
C------------------------------Executable-------------------------------
      nn = 1.0
      do in = 1, maxnes
        do j = 1, IRHUGE
          allarr(j,in) = nn
        enddo
      enddo
C need a different value on each level of these
      do k = 1, mkx
           do j = 1, mjx
                 do i = 1, mix
          PR0(i,j,k) = nn+k
          PR1(i,j,k) = nn+k
          RHO0(i,j,k) = nn+k
          RHO1(i,j,k) = nn+k
                 enddo
           enddo
          enddo

      HSCR1 = nn
      HSCR2 = nn
      HSCR3 = nn
      HSCR4 = nn
      IPTN = nn
      PHI = nn
      PSC = nn
      PT2TOT = nn
      PTEN = nn
      PTNTOT = nn
      QCC = nn
      QDOT = nn
      QGC = nn
      QIC = nn
      QNCC = nn
      QNIC = nn
      QRC = nn
      QVC = nn
      TC = nn
      UC = nn
      UCC = nn
      UCD = nn
      VC = nn
      VCC = nn
      VCD = nn

	  TD = nn
	  T = nn
	  QV = nn
	  QR = nn
	  QC = nn
	  QI = nn
	  QNI = nn
	  U = nn
	  V = nn
      return
      end

