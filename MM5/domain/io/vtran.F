      SUBROUTINE VTRAN(FYX,IMAX,JMAX)                                            VTRAN.1
#     include <parame.incl>                                                      VTRAN.2
C                                                                                VTRAN.3
C     PURPOSE  GET RID OF LIB=IMSLIB ON THE SEGLDR CARD, THIS DOES A             VTRAN.4
C              SIMPLE VECTOR SLAB TRANSPOSE.                                     VTRAN.5
C                                                                                VTRAN.6
      DIMENSION FXY(MIX*MJX)  ,FYX(IMAX*JMAX)                                    VTRAN.7
C                                                                                VTRAN.8
      ICOUNT=0                                                                   VTRAN.9
      DO 500 I=1,IMAX                                                            VTRAN.10
        DO 500 J=1,JMAX                                                          VTRAN.11
          ICOUNT=ICOUNT+1                                                        VTRAN.12
          FXY(ICOUNT)=FYX(I+IMAX*J-IMAX)                                         VTRAN.13
  500   CONTINUE                                                                 VTRAN.14
                                                                                 VTRAN.15
      DO 600 IJ=1,IMAX*JMAX                                                      VTRAN.16
        FYX(IJ)=FXY(IJ)                                                          VTRAN.17
  600 CONTINUE                                                                   VTRAN.18
      RETURN                                                                     VTRAN.19
      END                                                                        VTRAN.20
                                                                                 VTRAN.21
