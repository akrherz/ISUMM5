CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          XTDOT.1
      SUBROUTINE XTDOT( PX, PD, NI, NJ, NI1, NJ1 )                               XTDOT.2
#     include <parame.incl>                                                      XTDOT.3
      REAL PX(MIX,MJX), PD(MIX,MJX)                                              XTDOT.4
C                                                                                XTDOT.5
C  THIS ROUTINE DETERMINES P(.) FROM P(X) BY A 4-POINT INTERPOLATION.            XTDOT.6
C  ON THE X-GRID, A P(X) POINT OUTSIDE THE GRID DOMAIN IS ASSUMED TO             XTDOT.7
C  SATISFY P(0,J)=P(1,J); P(NI,J)=P(NI-1,J); AND SIMILARLY FOR THE I`S.          XTDOT.8
C                                                                                XTDOT.9
C                                                                                XTDOT.10
      DO 1 J=2,NJ1                                                               XTDOT.11
        DO 1 I=2,NI1                                                             XTDOT.12
    1   PD(I,J)=0.25*(PX(I,J)+PX(I-1,J)+PX(I,J-1)+PX(I-1,J-1))                   XTDOT.13
                                                                                 XTDOT.14
C                                                                                XTDOT.15
      DO 2 I=2,NI1                                                               XTDOT.16
        PD(I,1)=0.5*(PX(I,1)+PX(I-1,1))                                          XTDOT.17
    2 PD(I,NJ)=0.5*(PX(I,NJ1)+PX(I-1,NJ1))                                       XTDOT.18
C                                                                                XTDOT.19
      DO 3 J=2,NJ1                                                               XTDOT.20
        PD(1,J)=0.5*(PX(1,J)+PX(1,J-1))                                          XTDOT.21
    3 PD(NI,J)=0.5*(PX(NI1,J)+PX(NI1,J-1))                                       XTDOT.22
C                                                                                XTDOT.23
      PD(1,1)=PX(1,1)                                                            XTDOT.24
      PD(1,NJ)=PX(1,NJ1)                                                         XTDOT.25
      PD(NI,1)=PX(NI1,1)                                                         XTDOT.26
      PD(NI,NJ)=PX(NI1,NJ1)                                                      XTDOT.27
C                                                                                XTDOT.28
    5 CONTINUE                                                                   XTDOT.29
C                                                                                XTDOT.30
      RETURN                                                                     XTDOT.31
      END                                                                        XTDOT.32
                                                                                 XTDOT.33
