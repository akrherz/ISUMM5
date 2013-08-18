                                                                                 PRECIP.1
C*******************************************************************             PRECIP.2
C*                                                                 *             PRECIP.3
C*   CALCULATE PRECIPITATION STEPWISE:                             *             PRECIP.4
C*                                                                 *             PRECIP.5
C*     1. USE STEADY STATE PLUME EQUATION TO CALCULATE QC          *             PRECIP.6
C*                                                                 *             PRECIP.7
C*     2. WITH SATURATION CONDITION, CALCULATE HOW MUCH MOISTURE   *             PRECIP.8
C*        COULD BE IN UPDRAFT (QRC).                                             PRECIP.9
C*                                                                 *             PRECIP.10
C*     3. CALCULATE LIQUID WATER AND PRECIPITATION (PW) AT THAT    *             PRECIP.11
C*        THAT LEVEL (QC-QRC).                                                   PRECIP.12
C*                                                                 *             PRECIP.13
C*     4. DETERMINE NEW QC.                                        *             PRECIP.14
C*                                                                 *             PRECIP.15
C*     5. GO BACK TO 1..                                           *             PRECIP.16
C*                                                                 *             PRECIP.17
C*******************************************************************             PRECIP.18
C                                                                                PRECIP.19
C                                                                                PRECIP.20
           SUBROUTINE PRECIP(CD,KB,KBCON,KDIM,R,HC,HES,T,QE,                     PRECIP.21
     *           QES,PW,QC,QRC,Z,LP,P,QKB,PCUT,C0,ZU,KNUM,KTOP)                  PRECIP.22
      DIMENSION PW(KDIM,KNUM),QRC(KDIM,KNUM),QC(KDIM)                            PRECIP.23
      DIMENSION HC(KDIM),HES(KDIM),T(KDIM),QE(KDIM),                             PRECIP.24
     *          QES(KDIM),Z(KDIM),P(KDIM),CD(KDIM,KNUM),                         PRECIP.25
     *          ZU(KDIM,KNUM)                                                    PRECIP.26
      INTEGER KTOP(KNUM)                                                         PRECIP.27
C                                                                                PRECIP.28
C------- ERASE                                                                   PRECIP.29
C                                                                                PRECIP.30
      DO 50 K=1,KDIM                                                             PRECIP.31
        PW(K,LP)=0.                                                              PRECIP.32
        QC(K)=0.                                                                 PRECIP.33
   50 CONTINUE                                                                   PRECIP.34
C                                                                                PRECIP.35
C------- INITIALIZE                                                              PRECIP.36
C                                                                                PRECIP.37
      DO 60 K=1,KB                                                               PRECIP.38
        QRC(K,LP)=.5*(QE(K)+QE(K+1))                                             PRECIP.39
   60 CONTINUE                                                                   PRECIP.40
      DO 65 K=KB,KBCON                                                           PRECIP.41
        QRC(K,LP)=QKB                                                            PRECIP.42
   65 CONTINUE                                                                   PRECIP.43
      DO 70 K=KTOP(LP),KDIM                                                      PRECIP.44
        QRC(K,LP)=QES(K)                                                         PRECIP.45
   70 CONTINUE                                                                   PRECIP.46
C                                                                                PRECIP.47
C-------- CONSTANTS                                                              PRECIP.48
C                                                                                PRECIP.49
      RD=287.04                                                                  PRECIP.50
      RV=461.5                                                                   PRECIP.51
      G=9.81                                                                     PRECIP.52
      XL=2.5E06                                                                  PRECIP.53
      CP=1004.                                                                   PRECIP.54
C                                                                                PRECIP.55
C------- BOUNDARY CONDITION                                                      PRECIP.56
C                                                                                PRECIP.57
      QC(KBCON)=QKB                                                              PRECIP.58
      PW(KBCON,LP)=0.                                                            PRECIP.59
C                                                                                PRECIP.60
C                                                                                PRECIP.61
C**********************************************************************          PRECIP.62
C                            START LOOP                                          PRECIP.63
C                                                                                PRECIP.64
      DO 100 K=KBCON+1,KTOP(LP)                                                  PRECIP.65
        IF(K.EQ.KTOP(LP))THEN                                                    PRECIP.66
          DH=HC(K)-HES(K)                                                        PRECIP.67
          DZ=.5*(Z(K)-Z(K-1))                                                    PRECIP.68
        ELSE                                                                     PRECIP.69
          DH=HC(K)-.5*(HES(K)+HES(K+1))                                          PRECIP.70
          DZ=-.5*Z(K-1)+.5*Z(K+1)                                                PRECIP.71
        ENDIF                                                                    PRECIP.72
        GAMMA=(XL/CP)*(XL/(RV*(T(K)**2)))*QE(K)                                  PRECIP.73
C                                                                                PRECIP.74
C                                                                                PRECIP.75
C------    1.                                                                    PRECIP.76
C                                                                                PRECIP.77
C                                                                                PRECIP.78
        QC(K)=(QC(K-1)*(1.-R*DZ*.5)+R*DZ*QE(K))/(1.+.5*DZ*R)                     07NOV00.1481
C                                                                                PRECIP.81
C                                                                                PRECIP.82
C-------   2.                                                                    PRECIP.83
C                                                                                PRECIP.84
C                                                                                PRECIP.85
        QRCH=QES(K)+(1./XL)*(GAMMA/(1.+GAMMA))*DH                                PRECIP.86
C                                                                                PRECIP.87
C------- LIQUID WATER CONTENT+WATER VAPOR (QRC)                                  PRECIP.88
C                                                                                PRECIP.89
        QRC(K,LP)=(QC(K)-QRCH)/(1.+C0*DZ)+QRCH                                   PRECIP.90
C                                                                                PRECIP.91
C                                                                                PRECIP.92
C-------   3.PRECIP                                                              PRECIP.93
C                                                                                PRECIP.94
C                                                                                PRECIP.95
        PW(K,LP)=C0*DZ*(QRC(K,LP)-QRCH)*ZU(K,LP)                                 PRECIP.96
C                                                                                PRECIP.97
C------- IF P<PCUT, ALL WATER IS RAINED OUT!                                     PRECIP.98
C                                                                                PRECIP.99
        IF(K.EQ.KTOP(LP))THEN                                                    PRECIP.100
          QRC(K,LP)=(QC(K)-QES(K))/(1.+C0*DZ)+QES(K)                             PRECIP.101
          PW(K,LP)=(QRC(K,LP)-QES(K))*C0*DZ*ZU(K,LP)                             PRECIP.102
C                                                                                PRECIP.103
C                                                                                PRECIP.104
C                                                                                PRECIP.105
          IF(P(KTOP(LP)).LT.PCUT)THEN                                            PRECIP.106
            QRC(K,LP)=QES(K)                                                     PRECIP.107
            PW(K,LP)=(QC(K)-QRC(K,LP))*ZU(K,LP)                                  PRECIP.108
          ENDIF                                                                  PRECIP.109
        ENDIF                                                                    PRECIP.110
C                                                                                PRECIP.111
C                                                                                PRECIP.112
C                                                                                PRECIP.113
C                                                                                PRECIP.114
C-------   4.+5.                                                                 PRECIP.115
C                                                                                PRECIP.116
C                                                                                PRECIP.117
        QC(K)=QRC(K,LP)                                                          PRECIP.118
  100 CONTINUE                                                                   PRECIP.119
C                          END LOOP                                              PRECIP.120
C**********************************************************************          PRECIP.121
C                                                                                PRECIP.122
      RETURN                                                                     PRECIP.123
      END                                                                        PRECIP.124
                                                                                 PRECIP.125
