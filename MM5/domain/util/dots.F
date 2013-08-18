      SUBROUTINE DOTS(SLAB1,SLAB2,IS1,IS2,ID3,ID4)                               DOTS.1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          DOTS.2
C                                                                     C          DOTS.3
C                                                                     C          DOTS.4
C     THIS SUBROUTINE INTERPOLATES IN HORIZONTAL FROM CROSS TO DOT    C          DOTS.5
C     POINTS                                                          C          DOTS.6
C                                                                     C          DOTS.7
C                                                                     C          DOTS.8
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          DOTS.9
C                                                                                DOTS.10
      DIMENSION SLAB1(ID3,ID4),SLAB2(ID3,ID4)                                    DOTS.11
C                                                                                DOTS.12
      IE=IS1-1                                                                   DOTS.13
      JE=IS2-1                                                                   DOTS.14
C                                                                                DOTS.15
C-----INTERIOR POINTS:                                                           DOTS.16
C                                                                                DOTS.17
C_FLIC_RUNPAD(2)                                                                 DOTS.18
cmic$ do all autoscope                                                           DOTS.19
c$omp parallel do default(shared)                                                DOTS.20
c$omp&private(i,j)                                                               DOTS.21
      DO 10 I=2,IE                                                               DOTS.22
        DO 10 J=2,JE                                                             DOTS.23
          SLAB2(I,J)=0.25*(SLAB1(I,J)+SLAB1(I-1,J)+SLAB1(I,J-1)+                 DOTS.24
     +               SLAB1(I-1,J-1))                                             DOTS.25
   10   CONTINUE                                                                 DOTS.26
                                                                                 DOTS.27
C                                                                                DOTS.28
C-----EAST AND WEST BOUNDARIES:                                                  DOTS.29
C                                                                                DOTS.30
      DO 20 I=2,IE                                                               DOTS.31
        SLAB2(I,1)=0.5*(SLAB1(I,1)+SLAB1(I-1,1))                                 DOTS.32
        SLAB2(I,IS2)=0.5*(SLAB1(I,JE)+SLAB1(I-1,JE))                             DOTS.33
   20 CONTINUE                                                                   DOTS.34
C                                                                                DOTS.35
C-----NORTH AND SOUTH BOUNDARIES:                                                DOTS.36
C                                                                                DOTS.37
      DO 30 J=2,JE                                                               DOTS.38
        SLAB2(1,J)=0.5*(SLAB1(1,J)+SLAB1(1,J-1))                                 DOTS.39
        SLAB2(IS1,J)=0.5*(SLAB1(IE,J)+SLAB1(IE,J-1))                             DOTS.40
   30 CONTINUE                                                                   DOTS.41
C                                                                                DOTS.42
C-----CORNER POINTS:                                                             DOTS.43
C                                                                                DOTS.44
      SLAB2(1,1)=SLAB1(1,1)                                                      DOTS.45
      SLAB2(1,IS2)=SLAB1(1,JE)                                                   DOTS.46
      SLAB2(IS1,IS2)=SLAB1(IE,JE)                                                DOTS.47
      SLAB2(IS1,1)=SLAB1(IE,1)                                                   DOTS.48
C                                                                                DOTS.49
      RETURN                                                                     DOTS.50
      END                                                                        DOTS.51
                                                                                 DOTS.52
