
      SUBROUTINE ARRCOPY(SRC,DEST,N)
      REAL SRC(*),DEST(*)
      INTEGER N
      DO I = 1,N
	DEST(I) = SRC(I)
      ENDDO
      RETURN
      END

