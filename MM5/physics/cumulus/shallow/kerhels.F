C*********************************************************************           KERHELS.1
C*                                                                   *           KERHELS.2
C*      SUBROUTINE KERHEL CALCULATES THE CHANGES A CLOUD WITH        *           KERHELS.3
C*      TOP HEIGHT LP, BASE KB AND ENTRAINMENT RATE R WOULD DO TO    *           KERHELS.4
C*      THE ENVIRONMENT PER UNIT MASS (DEL*DP/G).                    *           KERHELS.5
C*                                                                   *           KERHELS.6
C*********************************************************************           KERHELS.7
C                                                                                KERHELS.8
C                                                                                KERHELS.9
C                                                                                KERHELS.10
C                                                                                KERHELS.11
      SUBROUTINE KERHELS(VAR,R,LP,ZU,HKB,HC,DELLA,P,Z,KB,KDIM,                   KERHELS.12
     *    XVAR,MBDT,KBEG,XHKB,ICH,CD,                                            KERHELS.13
     *    KDET,PSU,KNUM,KTOP)                                                    KERHELS.14
      REAL MBDT                                                                  KERHELS.15
      DIMENSION P(KDIM),Z(KDIM),DELLA(KDIM),VAR(KDIM),XVAR(KDIM)                 KERHELS.16
      DIMENSION HC(KDIM,KNUM),ZU(KDIM,KNUM),CD(KDIM,KNUM)                        KERHELS.17
      INTEGER KTOP(KNUM)                                                         KERHELS.18
C                                                                                KERHELS.19
C                                                                                KERHELS.20
      XL=2.5E06                                                                  KERHELS.21
      RV=461.5                                                                   KERHELS.22
      G=9.81                                                                     KERHELS.23
C                                                                                KERHELS.24
C                                                                                KERHELS.25
C------- INITIALIZE                                                              KERHELS.26
C                                                                                KERHELS.27
      DO 10 K=1,KDIM                                                             KERHELS.28
        XVAR(K)=VAR(K)                                                           KERHELS.29
        DELLA(K)=0.                                                              KERHELS.30
   10 CONTINUE                                                                   KERHELS.31
C                                                                                KERHELS.32
C------- CALCULATE CHANGES, FIRST SURFACE, LAST TOP                              KERHELS.33
C                                                                                KERHELS.34
      IF(KBEG.EQ.1)THEN                                                          KERHELS.35
        DP=50.*(PSU-P(2))                                                        KERHELS.36
C*!*!                                                                            KERHELS.37
        DV1=.5*(VAR(1)+VAR(2))                                                   KERHELS.38
        DV2=VAR(1)                                                               KERHELS.39
        DELLA(KBEG)=(DV1-DV2)*G/DP                                               KERHELS.40
      ELSE                                                                       KERHELS.41
        K=KBEG                                                                   KERHELS.42
        DP=+50.*(P(K-1)-P(K+1))                                                  KERHELS.43
        DV1=.5*(VAR(K)+VAR(K+1))                                                 KERHELS.44
        DV2=VAR(K)                                                               KERHELS.45
        DELLA(KBEG)=(DV1-DV2)*G/DP                                               KERHELS.46
      ENDIF                                                                      KERHELS.47
      DELLA(KBEG)=0.                                                             KERHELS.48
C                                                                                KERHELS.49
C                                                                                KERHELS.50
C                                                                                KERHELS.51
      IF(KBEG+1.GE.KTOP(LP))GOTO 151                                             KERHELS.52
      DO 150 K=KBEG+1,KTOP(LP)-1                                                 KERHELS.53
        ZU1=ZU(K,LP)                                                             KERHELS.54
        ZU2=ZU(K-1,LP)                                                           KERHELS.55
        DV1=.5*(VAR(K)+VAR(K+1))                                                 KERHELS.56
        DV2=VAR(K)                                                               KERHELS.57
        DV3=.5*(VAR(K)+VAR(K-1))                                                 KERHELS.58
C                                                                                KERHELS.59
C--- SPECIFIY DETRAINMENT OF DOWNDRAFT, HAS TO BE CONSISTENT                     KERHELS.60
C--- WITH ZD CALCULATIONS IN SOUNDD.                                             KERHELS.61
C                                                                                KERHELS.62
        DZ=.5*(Z(K+1)-Z(K-1))                                                    KERHELS.63
C                                                                                KERHELS.64
C--- CHANGED DUE TO SUBSIDENCE AND ENTRAINMENT                                   KERHELS.65
C                                                                                KERHELS.66
        DETUP=(.5*(HC(K-1,LP)+HC(K,LP))-DV2)*R*CD(K,LP)*DZ*ZU2                   KERHELS.67
        IF(K.LE.KB)DETUP=0.                                                      KERHELS.68
        DP=+50.*(P(K-1)-P(K+1))                                                  KERHELS.69
        DELLA(K)=((ZU1)*(DV1-DV2)+(ZU2)*(DV2-DV3)+DETUP)*G/DP                    KERHELS.70
  150 CONTINUE                                                                   KERHELS.71
  151 CONTINUE                                                                   KERHELS.72
C                                                                                KERHELS.73
C------- CLOUD TOP                                                               KERHELS.74
C                                                                                KERHELS.75
      LPT=KTOP(LP)                                                               KERHELS.76
      ZU1=ZU(LPT-1,LP)                                                           KERHELS.77
      DZ=.5*(Z(LPT)-Z(LPT-1))                                                    KERHELS.78
      DP=100.*(P(LPT-1)-P(LPT))                                                  KERHELS.79
      DV1=(VAR(LPT))                                                             KERHELS.80
      DV1=.5*(VAR(LPT)+VAR(LPT-1))                                               KERHELS.81
      DELLA(LPT)=ZU1*(HC(LPT,LP)-DV1)*G/DP                                       KERHELS.82
C                                                                                KERHELS.83
C------- FINAL CHANGED VARIABLE PER UNIT MASS FLUX                               KERHELS.84
C                                                                                KERHELS.85
      DO 200 K=1,KTOP(LP)                                                        KERHELS.86
        XVAR(K)=DELLA(K)*MBDT+VAR(K)                                             KERHELS.87
        IF(XVAR(K).LE.0.)THEN                                                    KERHELS.88
          XVAR(K)=1.E-08                                                         KERHELS.89
          DELLA(K)=0.                                                            KERHELS.90
        ENDIF                                                                    KERHELS.91
  200 CONTINUE                                                                   KERHELS.92
      XHKB=DELLA(KBEG)*MBDT+HKB                                                  KERHELS.93
      IF(ICH.LE.0)THEN                                                           KERHELS.94
        IF(DELLA(KBEG).GE.0.)THEN                                                KERHELS.95
          DELLA(KBEG)=0.                                                         KERHELS.96
          XHKB=DELLA(KBEG)*MBDT+HKB                                              KERHELS.97
        ENDIF                                                                    KERHELS.98
      ENDIF                                                                      KERHELS.99
      RETURN                                                                     KERHELS.100
      END                                                                        KERHELS.101
                                                                                 KERHELS.102
