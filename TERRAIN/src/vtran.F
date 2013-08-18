      SUBROUTINE VTRAN(FYX,IMAX,JMAX)                                            VTRAN.1
C                                                                                VTRAN.2
C   PURPOSE :   GETS RID OF LIB=IMSLIB ON THE SEGLDR CARD, DOING                 VTRAN.3
C               A SIMPLE VECTOR SLAB TRANSPOSE.                                  VTRAN.4
C   ON ENTRY :  FYX: THE ORIGINAL 2-D FIELD.                                     VTRAN.5
C         IMAX,JMAX: THE DIMENSIONS OF THE FIELD FYX.                            VTRAN.6
C   ON EXIT :   FYX: THE TRANSPOSE OF THE ORIGINAL FIELD.                        VTRAN.7
C   CALLS :     SUBROUTINE VTRAN CALLS NO OTHER SUBROUTINES.                     VTRAN.8
C   CALLED BY : SUBROUTINE OUTPT.                                                VTRAN.9
C   COMMENTS :                                                                   VTRAN.10
C  THE ARRAY IS DECLARED AS AN ONE-DIMENSIONAL ARRAY FYX(IMAX*JMAX).             VTRAN.11
C                                                                                VTRAN.12
# include <parame.incl>                                                          VTRAN.13
      DIMENSION FXY(IIMX*JJMX)                                                   VTRAN.14
      DIMENSION FYX(IMAX*JMAX)                                                   VTRAN.15
C                                                                                VTRAN.16
      ICOUNT=0                                                                   VTRAN.17
      DO 500 I=1,IMAX                                                            VTRAN.18
      DO 500 J=1,JMAX                                                            VTRAN.19
         ICOUNT=ICOUNT+1                                                         VTRAN.20
         FXY(ICOUNT)=FYX(I+IMAX*J-IMAX)                                          VTRAN.21
500   CONTINUE                                                                   VTRAN.22
      DO 600 IJ=1,IMAX*JMAX                                                      VTRAN.23
         FYX(IJ)=FXY(IJ)                                                         VTRAN.24
600   CONTINUE                                                                   VTRAN.25
      RETURN                                                                     VTRAN.26
      END                                                                        VTRAN.27
