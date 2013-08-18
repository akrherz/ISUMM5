      SUBROUTINE EDDYMM4(PSTAR,KZO,G,ILX,KL,KLM,J,DTPBL,                         EDDYMM4.1
     >           IMOISTX,IMVDIF,R)                                               EDDYMM4.2
      IMPLICIT NONE                                                              EDDYMM4.3
C--------------------------------------------------------------------------      EDDYMM4.4
C---- KZ COMPUTED AS FUNCTION OF LOCAL RICHARDSON # AND VERTICAL SHEAR           EDDYMM4.5
C---- NOTE THAT K-THEORY IS USED ONLY WHEN NOT FREE CONVECTIVE                   EDDYMM4.7
C                                                                                EDDYMM4.8
C  REVISION HISTORY:                                                             05DEC01.1264
C  JEP        3/01 - updated Kz scheme similar to Liu & Carroll (1996)           05DEC01.1265
C  JEP       12/99 - V3, implicit none                                           EDDYMM4.10
C  D. TIVEL   8/95 - Minor adjustments from MM4 to MM5                           EDDYMM4.11
C  JEP       10/89 - Initial version (modified from existing MM4 scheme)         EDDYMM4.12
C--------------------------------------------------------------------------      EDDYMM4.13
#     include <parame.incl>                                                      EDDYMM4.14
#     include <comicl.incl>                                                      EDDYMM4.15
C                                                                                EDDYMM4.16
C.......Arguments                                                                EDDYMM4.17
      REAL PSTAR(MIX,MJX)                                                        EDDYMM4.18
      REAL KZO(MKX)                                                              EDDYMM4.19
      REAL G,R,DTPBL                                                             EDDYMM4.20
      INTEGER ILX,KL,KLM,J                                                       EDDYMM4.21
      INTEGER IMOISTX,IMVDIF                                                     EDDYMM4.22
C.......Index variables                                                          EDDYMM4.23
      INTEGER K,I,KK                                                             EDDYMM4.24
C.......Local variables                                                          EDDYMM4.25
      REAL DZF,RC,SS,GOTH,RI                                                     EDDYMM4.26
      REAL QMEAN,TMEAN                                                           EDDYMM4.27
      REAL XLV,ALPH,CHI                                                          EDDYMM4.28
      REAL DENSF                                                                 EDDYMM4.29
      REAL SQL,EDYZ0,RV,RLAM,ZK                                                  05DEC01.1266
      DATA RV/461.5/,RC/0.25/,RLAM/80./                                          05DEC01.1267
      SAVE RV,RC,RLAM                                                            05DEC01.1268
C                                                                                EDDYMM4.33
      DO 20 K=1,KLM                                                              EDDYMM4.34
        KK = KL - K + 1                                                          EDDYMM4.35
        EDYZ0 = KZO(KK)   ! SEAMAN'S MINIMUM KZO = 0.005/DELSIG, MAX=0.5         EDDYMM4.36
        DO 10 I = 1,ILX                                                          EDDYMM4.37
C                                                                                EDDYMM4.38
          DZF = Z(I,K+1) - Z(I,K)                                                EDDYMM4.39
C          RC = 0.257 * DZF ** 0.175                                             05DEC01.1269
          SS = ((US(I,K+1) - US(I,K)) ** 2 + (VS(I,K+1) -                        EDDYMM4.41
     &         VS(I,K)) ** 2) / (DZF * DZF) + 1.E-9                              EDDYMM4.42
          GOTH = 2.0 * G / (THETAV(I,K+1) + THETAV(I,K))                         EDDYMM4.43
          RI = GOTH * (THETAV(I,K+1) - THETAV(I,K)) / (DZF * SS)                 EDDYMM4.44
                                                                                 EDDYMM4.45
C-- Adjustment to vert diff in Moist air from HIRPBL                             EDDYMM4.46
          IF(IMVDIF.EQ.1.AND.IMOISTX.EQ.2)THEN                                   EDDYMM4.47
            IF((QCS(I,K)+QIS(I,K)).GT.0.01E-3.AND.(QCS(I,K+1)+                   EDDYMM4.48
     +        QIS(I,K+1)).GT.0.01E-3)THEN                                        EDDYMM4.49
              QMEAN=0.5*(QVS(I,K)+QVS(I,K+1))                                    EDDYMM4.50
              TMEAN=0.5*(TT(I,K)+TT(I,K+1))                                      EDDYMM4.51
              XLV = (2.501 - 0.00237 * (TMEAN - 273.15)) * 1.E6                  EDDYMM4.52
              ALPH=XLV*QMEAN/R/TMEAN                                             EDDYMM4.53
              CHI=XLV*XLV*QMEAN/CPAIR(I)/RV/TMEAN/TMEAN                          EDDYMM4.54
              RI=(1.+ALPH)*(RI-G*G/SS/TMEAN/CPAIR(I)*                            EDDYMM4.55
     >           ((CHI-ALPH)/(1.+CHI)))                                          EDDYMM4.56
            ENDIF                                                                EDDYMM4.57
          ENDIF                                                                  EDDYMM4.58
C-----------------                                                               05DEC01.1270
          ZK = 0.4 * ZF(I,K)                                                     05DEC01.1271
          SQL = (ZK*RLAM/(RLAM+ZK))**2                                           05DEC01.1272
c         EDDYZ(I,K) = CVMGP(EDYZ0,EDYZ0 + SQRT(SS) * (RC - RI) *                EDDYMM4.60
c    &                 SZKM / RC,RI - RC)                                        EDDYMM4.61
          IF(RI .GE.RC)THEN                                                      05DEC01.1273
            EDDYZ(I,K) = EDYZ0                                                   EDDYMM4.63
          ELSE IF(RI.GE.0.0) THEN                                                05DEC01.1274
            EDDYZ(I,K) = EDYZ0 + SQRT(SS) * (1.- RI/RC)**2 * SQL                 05DEC01.1275
          ELSE                                                                   05DEC01.1276
            EDDYZ(I,K) = EDYZ0 + SQRT(SS*(1.-25.*RI)) * SQL                      05DEC01.1277
          ENDIF                                                                  EDDYMM4.67
C                                                                                EDDYMM4.68
          EDDYZ(I,K) = AMIN1(1000.0,EDDYZ(I,K))                                  05DEC01.1278
C         IF(I.EQ.53.AND.J.EQ.128.AND.EDDYZ(I,K).GT.100.)                        05DEC01.1279
C    >    PRINT *,' KZ,kpbl,noc=',EDDYZ(I,K),KLPBL(I),NOCONV(I),                 05DEC01.1280
C    >    ' SS,RI,SQL=',SS,RI,SQL,' K=',K                                        05DEC01.1281
          DENSF = 0.5 * (DENSX(I,K+1) + DENSX(I,K))                              EDDYMM4.70
          EDDYZ(I,K) = EDDYZ(I,K) * (DENSF * G / PSTAR(I,J)) ** 2 *              EDDYMM4.71
     &                 DTPBL * DSIGFI(K) * 1.0E-6                                EDDYMM4.72
10      CONTINUE                                                                 EDDYMM4.73
20    CONTINUE                                                                   EDDYMM4.74
C                                                                                EDDYMM4.75
      DO 30 I = 1,ILX                                                            EDDYMM4.76
        EDDYZ(I,KL) = EDDYZ(I,KLM)                                               EDDYMM4.77
30    CONTINUE                                                                   EDDYMM4.78
C                                                                                EDDYMM4.79
      RETURN                                                                     EDDYMM4.80
      END                                                                        EDDYMM4.81
