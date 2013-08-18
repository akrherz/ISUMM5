       SUBROUTINE CUPARA5(PSB,T3DTEN,QV3DTEN,QR3DTEN,QC3DTEN,                    CUPARA5.1
     &                    QS3DTEN,QI3DTEN,RAINC,SCR1,U3D,V3D,T3D,QV3D,           07NOV00.1216
     &                    PP3D,J,INEST,IMPHYS,IBEG,IEND)                         CUPARA5.3
                                                                                 07NOV00.1217
      IMPLICIT NONE                                                              07NOV00.1218
                                                                                 CUPARA5.4
#     include <parame.incl>                                                      CUPARA5.5
#     include <various.incl>                                                     CUPARA5.6
#     include <param3.incl>                                                      CUPARA5.7
#     include <addrcu.incl>                                                      CUPARA5.8
#     include <pmoist.incl>                                                      CUPARA5.9
#     include <nhcnst.incl>                                                      CUPARA5.10
                                                                                 CUPARA5.11
      INTEGER J,INEST,IMPHYS,IBEG,IEND                                           07NOV00.1219
      REAL RAINC(MIX,MJX), PSB(MIX,MJX), SCR1(MIX,MKX)                           07NOV00.1220
      REAL T3DTEN(MIX,MJX,MKX), QV3DTEN(MIX,MJX,MKX)                             07NOV00.1221
      REAL QR3DTEN(MIXM,MJXM,MKXM), QC3DTEN(MIXM,MJXM,MKXM)                      07NOV00.1222
      REAL QS3DTEN(MIXM,MJXM,MKXM), QI3DTEN(MIXM,MJXM,MKXM)                      07NOV00.1223
      REAL T3D(MIX,MJX,MKX), QV3D(MIX,MJX,MKX)                                   07NOV00.1224
      REAL U3D(MIX,MJX,MKX), V3D(MIX,MJX,MKX)                                    07NOV00.1225
      REAL PP3D(MIX,MJX,MKX)                                                     07NOV00.1226
C                                                                                07NOV00.1227
      INTEGER I,K                                                                07NOV00.1228
      INTEGER ICLDCK                                                             07NOV00.1229
C                                                                                CUPARA5.19
C--- CALL CUMULUS PARAMETERIZATION                                               CUPARA5.20
C                                                                                CUPARA5.21
      ICLDCK=MOD(KTAU,10)                                                        CUPARA5.22
      IF(ICLDCK.EQ.0)CALL FCPARA(J,INEST,SCR1,IBEG,IEND,T0,U3D,V3D,PP3D,         CUPARA5.23
     +  T3D,QV3D,PSB)                                                            CUPARA5.24
      DO 465 I=IBEG,IEND                                                         CUPARA5.25
        IF(NCA(I,J).LE.0)GOTO 465                                                CUPARA5.26
        DO 460 K=1,KL                                                            CUPARA5.27
          IF(IMPHYS.GE.3)THEN                                                    CUPARA5.28
            QR3DTEN(I,J,K)=QR3DTEN(I,J,K)+DQRDT(I,J,K)*PSB(I,J)                  CUPARA5.29
            QC3DTEN(I,J,K)=QC3DTEN(I,J,K)+DQLDT(I,J,K)*PSB(I,J)                  CUPARA5.30
          ENDIF                                                                  25JAN00.469
          IF(IMPHYS.GE.5)THEN                                                    25JAN00.470
C......QSTEN STORES GRAUPEL TENDENCY IF IT EXISTS, OTHERISE SNOW (V2)            CUPARA5.32
            QS3DTEN(I,J,K)=QS3DTEN(I,J,K)+DQSDT(I,J,K)*PSB(I,J)                  CUPARA5.33
            QI3DTEN(I,J,K)=QI3DTEN(I,J,K)+DQIDT(I,J,K)*PSB(I,J)                  CUPARA5.34
          ENDIF                                                                  CUPARA5.35
          T3DTEN(I,J,K)=T3DTEN(I,J,K)+DTDT(I,J,K)*PSB(I,J)                       CUPARA5.36
          QV3DTEN(I,J,K)=QV3DTEN(I,J,K)+DQDT(I,J,K)*PSB(I,J)                     CUPARA5.37
  460   CONTINUE                                                                 CUPARA5.38
        RAINC(I,J)=RAINC(I,J)+RAINCV(I,J)                                        CUPARA5.39
  465 CONTINUE                                                                   CUPARA5.40
      RETURN                                                                     CUPARA5.41
      END                                                                        CUPARA5.42
                                                                                 CUPARA5.43
