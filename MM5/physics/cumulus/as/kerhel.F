C*********************************************************************           KERHEL.1
C*                                                                   *           KERHEL.2
C*      SUBROUTINE KERHEL CALCULATES THE CHANGES A CLOUD WITH        *           KERHEL.3
C*      TOP HEIGHT LP, BASE KB AND ENTRAINMENT RATE R WOULD DO TO    *           KERHEL.4
C*      THE ENVIRONMENT PER UNIT MASS (DEL*DP/G).                    *           KERHEL.5
C*                                                                   *           KERHEL.6
C*********************************************************************           KERHEL.7
C                                                                                KERHEL.8
C                                                                                KERHEL.9
      SUBROUTINE KERHEL(VAR,R,LP,ZU,HKB,HC,DELLA,P,Z,KB,KDIM,                    KERHEL.10
     *    XVAR,MBDT,KBEG,XHKB,ICH,CD,HCD,ZD,ITEST,                               KERHEL.11
     *    KDET,EDT,PSU,KNUM,KTOP)                                                KERHEL.12
      REAL MBDT                                                                  KERHEL.13
      DIMENSION P(KDIM),Z(KDIM),DELLA(KDIM),VAR(KDIM),XVAR(KDIM)                 KERHEL.14
      DIMENSION HC(KDIM,KNUM),ZU(KDIM,KNUM),CD(KDIM,KNUM),                       KERHEL.15
     *          HCD(KDIM,KNUM),ZD(KDIM,KNUM),EDT(KNUM)                           KERHEL.16
      INTEGER ITEST(KDIM),KTOP(KNUM)                                             KERHEL.17
C                                                                                KERHEL.18
C                                                                                KERHEL.19
      KDET1=KDET+1                                                               KERHEL.20
      XL=2.5E06                                                                  KERHEL.21
      RV=461.5                                                                   KERHEL.22
      G=9.81                                                                     KERHEL.23
C                                                                                KERHEL.24
C                                                                                KERHEL.25
C------- INITIALIZE                                                              KERHEL.26
C                                                                                KERHEL.27
      DO 10 K=1,KDIM                                                             KERHEL.28
        XVAR(K)=VAR(K)                                                           KERHEL.29
        DELLA(K)=0.                                                              KERHEL.30
   10 CONTINUE                                                                   KERHEL.31
C                                                                                KERHEL.32
C------- CALCULATE CHANGES, FIRST SURFACE, LAST TOP                              KERHEL.33
C                                                                                KERHEL.34
      DR=0.                                                                      KERHEL.35
      DZ=.5*(Z(2)-Z(1))                                                          KERHEL.36
      ZD1=ZD(1,LP)*(1.+DR*DZ)                                                    KERHEL.37
      ZD2=ZD(1,LP)                                                               KERHEL.38
      DP=50.*(PSU-P(2))                                                          KERHEL.39
      DELLA(1)=EDT(LP)*(ZD2*HCD(1,LP)-ZD2*.5*(VAR(1)+VAR(2)))*G/DP               KERHEL.40
      IF(ITEST(LP).LT.0)DELLA(1)=0.                                              KERHEL.41
C                                                                                KERHEL.42
C                                                                                KERHEL.43
C                                                                                KERHEL.44
      DO 150 K=2,KTOP(LP)-1                                                      KERHEL.45
        ZU1=ZU(K,LP)                                                             KERHEL.46
        ZU2=ZU(K-1,LP)                                                           KERHEL.47
        ZD1=ZD(K,LP)                                                             KERHEL.48
        ZD2=ZD(K-1,LP)                                                           KERHEL.49
        IF(ITEST(LP).LT.0)THEN                                                   KERHEL.50
          ZD1=0.                                                                 KERHEL.51
          ZD2=0.                                                                 KERHEL.52
        ENDIF                                                                    KERHEL.53
        DV1=.5*(VAR(K)+VAR(K+1))                                                 KERHEL.54
        DV2=VAR(K)                                                               KERHEL.55
        DV3=.5*(VAR(K)+VAR(K-1))                                                 KERHEL.56
C                                                                                KERHEL.57
C--- SPECIFIY DETRAINMENT OF DOWNDRAFT, HAS TO BE CONSISTENT                     KERHEL.58
C--- WITH ZD CALCULATIONS IN SOUNDD.                                             KERHEL.59
C                                                                                KERHEL.60
        DZ=.5*(Z(K+1)-Z(K-1))                                                    KERHEL.61
        IF(K.LE.KDET1)THEN                                                       KERHEL.62
          DDKK=.25*ZD1*(1.+DR*DZ)                                                KERHEL.63
        ELSE                                                                     KERHEL.64
          DDKK=0                                                                 KERHEL.65
        ENDIF                                                                    KERHEL.66
C                                                                                KERHEL.67
C--- CHANGED DUE TO SUBSIDENCE AND ENTRAINMENT                                   KERHEL.68
C                                                                                KERHEL.69
        DETDO=DDKK*(.5*(HCD(K,LP)+HCD(K-1,LP))-DV2)                              KERHEL.70
        DETUP=(.5*(HC(K-1,LP)+HC(K,LP))-DV2)*R*CD(K,LP)*DZ*ZU2                   KERHEL.71
        IF(K.LE.KB)DETUP=0.                                                      KERHEL.72
        DP=+50.*(P(K-1)-P(K+1))                                                  KERHEL.73
        DELLA(K)=((ZU1-EDT(LP)*ZD1)*(DV1-DV2)+(ZU2-EDT(LP)*ZD2)*(DV2-DV3         KERHEL.74
     +           )+DETUP+DETDO*EDT(LP))*G/DP                                     KERHEL.75
  150 CONTINUE                                                                   KERHEL.76
C                                                                                KERHEL.77
C------- CLOUD TOP                                                               KERHEL.78
C                                                                                KERHEL.79
      LPT=KTOP(LP)                                                               KERHEL.80
      ZU1=ZU(LPT-1,LP)                                                           KERHEL.81
      DZ=.5*(Z(LPT)-Z(LPT-1))                                                    KERHEL.82
      DP=100.*(P(LPT-1)-P(LPT))                                                  KERHEL.83
      DV1=.5*(VAR(LPT)+VAR(LPT-1))                                               KERHEL.84
      DELLA(LPT)=(ZU1*((1.+R*DZ)*(HC(LPT,LP)-VAR(LPT))-DV1+VAR(LPT)+             KERHEL.85
     +           CD(LPT-1,LP)*R*DZ*(.5*(HC(LPT,LP)+HC(LPT-1,LP))-                KERHEL.86
     +           VAR(LPT))))*G/DP                                                KERHEL.87
C                                                                                KERHEL.88
C------- FINAL CHANGED VARIABLE PER UNIT MASS FLUX                               KERHEL.89
C                                                                                KERHEL.90
      DO 200 K=1,KTOP(LP)                                                        KERHEL.91
        XVAR(K)=DELLA(K)*MBDT+VAR(K)                                             KERHEL.92
        IF(XVAR(K).LE.0.)THEN                                                    KERHEL.93
          XVAR(K)=1.E-08                                                         KERHEL.94
          DELLA(K)=0.                                                            KERHEL.95
        ENDIF                                                                    KERHEL.96
  200 CONTINUE                                                                   KERHEL.97
      XHKB=DELLA(KBEG)*MBDT+HKB                                                  KERHEL.98
      IF(ICH.LE.0)THEN                                                           KERHEL.99
        IF(DELLA(KBEG).GE.0.)THEN                                                KERHEL.100
          DELLA(KBEG)=0.                                                         KERHEL.101
          XHKB=DELLA(KBEG)*MBDT+HKB                                              KERHEL.102
        ENDIF                                                                    KERHEL.103
      ENDIF                                                                      KERHEL.104
      RETURN                                                                     KERHEL.105
      END                                                                        KERHEL.106
                                                                                 KERHEL.107
