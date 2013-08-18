C
C Given P, find M and N st. M*N=P and minimizing |M-N|.
C 
C Used for finding a reasonable number of processors in lat and
C lon, given the number of processors available.  This form
C favors wider partitions against taller ones (1xP stead Px1).
C To change, make the DO M loop go down stead up, and initialize
C MINM and MINN accordingly.
C
C Modified to respect teh PROCMIN limits in the parallel MM5;
C that is, because memory is shrunken, we cannot allow fewer
C than a certain number of processors in a given dimension.
C 
C JM 970424
C
#ifndef vpp
      SUBROUTINE MPASPECT( P, MINM, MINN, PROCMIN_M, PROCMIN_N )
      INTEGER P, M, N, MINI, MINM, MINN, PROCMIN_M, PROCMIN_N
      MINI = 2*P
      MINM = 1
      MINN = P
      DO M = 1, P
        IF ( MOD( P, M ) .EQ. 0 ) THEN
          N = P / M
          IF ( ABS(M-N) .LT. MINI 
     +         .AND. M .GE. PROCMIN_M
     +         .AND. N .GE. PROCMIN_N
     +       ) THEN
            MINI = ABS(M-N)
            MINM = M
            MINN = N
          ENDIF
        ENDIF
      ENDDO
      IF ( MINM .LT. PROCMIN_M .OR. MINN .LT. PROCMIN_N ) THEN
        WRITE(0,*)
     +  'MPASPECT: UNABLE TO GENERATE PROCESSOR MESH.  STOPPING.'
        WRITE(0,*)' PROCMIN_M ', PROCMIN_M
        WRITE(0,*)' PROCMIN_N ', PROCMIN_N
        WRITE(0,*)' P         ', P
        WRITE(0,*)' MINM      ', MINM
        WRITE(0,*)' MINN      ', MINN
        STOP
      ENDIF
      RETURN
      END
#else
      SUBROUTINE MPASPECT( P, MINM, MINN, PROCMIN_M, PROCMIN_N )
      INTEGER P, M, N, MINI, MINM, MINN, PROCMIN_M, PROCMIN_N
      MINM = 1
      MINN = P
      RETURN
      END
#endif
