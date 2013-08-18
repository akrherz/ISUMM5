      SUBROUTINE CUPARA7(T3D,QV3D,PSB,PP3D,T3DTEN,QV3DTEN,                       CUPARA7.1
     &                   RAINC,CLDEFI,XLAND,J,INEST,IBEG,IEND,TBASE)             CUPARA7.2
                                                                                 07NOV00.1204
      IMPLICIT NONE                                                              07NOV00.1205
                                                                                 CUPARA7.3
#     include <parame.incl>                                                      CUPARA7.4
#     include <param3.incl>                                                      CUPARA7.5
#     include <various.incl>                                                     CUPARA7.6
                                                                                 CUPARA7.7
      INTEGER J,INEST,IBEG,IEND                                                  07NOV00.1206
      REAL T3D(MIX,MJX,MKX), QV3D(MIX,MJX,MKX), PP3D(MIX,MJX,MKX)                07NOV00.1207
      REAL T3DTEN(MIX,MJX,MKX), QV3DTEN(MIX,MJX,MKX)                             07NOV00.1208
      REAL TBASE(MIXNH,MJXNH,MKXNH),CLDEFI(MIX,MJX)                              07NOV00.1209
      REAL RAINC(MIX,MJX),PSB(MIX,MJX)                                           07NOV00.1210
      REAL TMOD(MIX,MKX),QMOD(MIX,MKX)                                           07NOV00.1211
      REAL XLAND(MIX,MJX)                                                        23DEC04.3040
C                                                                                07NOV00.1213
      INTEGER I,K                                                                07NOV00.1214
      INTEGER ICOND,ICONSS                                                       07NOV00.1215
C                                                                                CUPARA7.14
C     IF (J.EQ.2) THEN                                                           CUPARA7.15
      ICOND=0                                                                    CUPARA7.16
      ICONSS=0                                                                   CUPARA7.17
C     END IF                                                                     CUPARA7.18
C                                                                                CUPARA7.19
C--- CALL CUMULUS PARAMETERIZATION                                               CUPARA7.20
C                                                                                CUPARA7.21
      CALL BMPARA(T3D,QV3D,PSB,PP3D,TMOD,QMOD,RAINC,ICOND,ICONSS,J,PTOP,         CUPARA7.22
     +     A,DSIGMA,SIGMA,INEST,IBEG,IEND,TBASE,CLDEFI,XLAND)                    CUPARA7.23
C                                                                                CUPARA7.24
      DO 100 K=1,KL                                                              CUPARA7.25
        DO 100 I=IBEG,IEND                                                       CUPARA7.26
          T3DTEN(I,J,K)=T3DTEN(I,J,K)+TMOD(I,K)*PSB(I,J)                         CUPARA7.27
          QV3DTEN(I,J,K)=QV3DTEN(I,J,K)+QMOD(I,K)*PSB(I,J)                       CUPARA7.28
  100   CONTINUE                                                                 CUPARA7.29
                                                                                 CUPARA7.30
      RETURN                                                                     CUPARA7.31
      END                                                                        CUPARA7.32
                                                                                 CUPARA7.33
