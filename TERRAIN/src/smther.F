      SUBROUTINE SMTHER(SLAB,IS1,IS2,NPASS,POINT)                                SMTHER.1
C                                                                                SMTHER.2
C   PURPOSE :  SPATIALLY SMOOTHES THE 2-D FIELD SLAB TO DAMP THE                 SMTHER.3
C              SHORTER WAVELENGTH COMPONENTS.                                    SMTHER.4
C   ON ENTRY : SLAB(IS1,IS2): 2-D FIELD TO BE SMOOTHED.                          SMTHER.5
C                    IS1,IS2: THE DIMENSIONS OF THE FIELD SLAB.                  SMTHER.6
C                      NPASS: THE NUMBER OF SMOOTHER-DESMOOTHER PASSES.          SMTHER.7
C                      POINT: POINT = 5HCROSS MEANS THE CROSS                    SMTHER.8
C                             POINTS FIELD, OTHERWISE DOT POINTS FIELD.          SMTHER.9
C   ON EXIT :  SLAB(IS1,IS2): THE SMOOTHED FIELD.                                SMTHER.10
C   CALLS :    SUBROUTINE SMTHER CALLS NO OTHER SUBROUTINES.                     SMTHER.11
C   CALLED BY: SUBROUTINE SMTHTR.                                                SMTHER.12
C   COMMENTS :                                                                   SMTHER.13
C  SEE SECTION 3.4 FOR MORE DETAILS.                                             SMTHER.14
C                                                                                SMTHER.15
      DIMENSION SLAB(IS1,IS2),XNU(2)                                             SMTHER.16
      CHARACTER*5 POINT                                                          SMTHER.17
      ICROSS=0                                                                   SMTHER.18
      IF(POINT.EQ.'CROSS') ICROSS=1                                              SMTHER.19
      IE=IS1-1-ICROSS                                                            SMTHER.20
      JE=IS2-1-ICROSS                                                            SMTHER.21
      XNU(1)=0.5                                                                 SMTHER.22
      XNU(2)=-0.52                                                               SMTHER.23
      DO 50 K=1,NPASS                                                            SMTHER.24
      DO 50 KP=1,2                                                               SMTHER.25
C   FIRST SMOOTH IN THE IS1 DIRECTION                                            SMTHER.26
      DO 1 I=2,IE                                                                SMTHER.27
      ASV=SLAB(I,1)                                                              SMTHER.28
      DO 1 J=2,JE                                                                SMTHER.29
      APLUS=SLAB(I,J+1)                                                          SMTHER.30
      CELL=SLAB(I,J)                                                             SMTHER.31
      SLAB(I,J)=SLAB(I,J)+XNU(KP)*((ASV+APLUS)/2.0-SLAB(I,J))                    SMTHER.32
      ASV=CELL                                                                   SMTHER.33
    1 CONTINUE                                                                   SMTHER.34
C   SMOOTH IN THE IS2 DIRECTION                                                  SMTHER.35
      DO 2 J=2,JE                                                                SMTHER.36
      ASV=SLAB(1,J)                                                              SMTHER.37
      DO 2 I=2,IE                                                                SMTHER.38
      APLUS=SLAB(I+1,J)                                                          SMTHER.39
      CELL=SLAB(I,J)                                                             SMTHER.40
      SLAB(I,J)=SLAB(I,J)+XNU(KP)*((ASV+APLUS)/2.0-SLAB(I,J))                    SMTHER.41
      ASV=CELL                                                                   SMTHER.42
    2 CONTINUE                                                                   SMTHER.43
   40 CONTINUE                                                                   SMTHER.44
   50 CONTINUE                                                                   SMTHER.45
      RETURN                                                                     SMTHER.46
      END                                                                        SMTHER.47
