C***********************************************************************         SMTHER.1
C***********************************************************************         SMTHER.2
C***********************************************************************         SMTHER.3
      SUBROUTINE SMTHER(SLAB,IS1,IS2,NPASS,IST,IE,JST,JE)                        SMTHER.4
C                                                                                SMTHER.5
C        PURPOSE: SPATIALLY SMOOTH DATA IN SLAB TO DAMPEN SHORT                  SMTHER.6
C                 WAVELENGTH COMPONENTS.                                         SMTHER.7
C                                                                                SMTHER.8
      DIMENSION SLAB(IS1,IS2),XNU(2)                                             SMTHER.9
      IF(NPASS.EQ.0)RETURN                                                       SMTHER.10
      XNU(1)=0.50                                                                SMTHER.11
      XNU(2)=-0.52                                                               SMTHER.12
      DO 50 K=1,NPASS                                                            SMTHER.13
        DO 50 KP=1,2                                                             SMTHER.14
C        FIRST, SMOOTH IN THE IS1 DIRECTION                                      SMTHER.15
          DO 1 I=IST,IE                                                          SMTHER.16
            ASV=SLAB(I,JST-1)                                                    SMTHER.17
            DO 1 J=JST,JE                                                        SMTHER.18
              APLUS=SLAB(I,J+1)                                                  SMTHER.19
              CELL=SLAB(I,J)                                                     SMTHER.20
              SLAB(I,J)=SLAB(I,J)+XNU(KP)*((ASV+APLUS)/2.0-SLAB(I,J))            SMTHER.21
              ASV=CELL                                                           SMTHER.22
    1       CONTINUE                                                             SMTHER.23
                                                                                 SMTHER.24
C        NOW, SMOOTH IN THE IS2 DIRECTION                                        SMTHER.25
          DO 2 J=JST,JE                                                          SMTHER.26
            ASV=SLAB(IST-1,J)                                                    SMTHER.27
            DO 2 I=IST,IE                                                        SMTHER.28
              APLUS=SLAB(I+1,J)                                                  SMTHER.29
              CELL=SLAB(I,J)                                                     SMTHER.30
              SLAB(I,J)=SLAB(I,J)+XNU(KP)*((ASV+APLUS)/2.0-SLAB(I,J))            SMTHER.31
              ASV=CELL                                                           SMTHER.32
    2       CONTINUE                                                             SMTHER.33
                                                                                 SMTHER.34
   40     CONTINUE                                                               SMTHER.35
   50   CONTINUE                                                                 SMTHER.36
                                                                                 SMTHER.37
      RETURN                                                                     SMTHER.38
      END                                                                        SMTHER.39
                                                                                 SMTHER.40
