C*********************************************************************           ENTR.1
C*                                                                   *           ENTR.2
C*     THIS SUBROUTINE CALCULATES INCLOUD MOIST STATIC ENERGY        **          ENTR.3
C**                                                                  **          ENTR.4
C**********************************************************************          ENTR.5
C**********************************************************************          ENTR.6
C                                                                                ENTR.7
C                                                                                ENTR.8
        SUBROUTINE ENTR (CD,KBC,H,HC,HSAT,ENT,KDIM,                              ENTR.9
     *                   LP,KB,P,HKB,KNUM,KTOP)                                  ENTR.10
      DIMENSION H(KDIM),HSAT(KDIM),HC(KDIM),                                     ENTR.11
     *          P(KDIM),CD(KDIM,KNUM)                                            ENTR.12
      DIMENSION DBY(80)                                                          ENTR.13
      INTEGER KTOP(KNUM)                                                         ENTR.14
      IF(KDIM.GT.80)THEN                                                         ENTR.15
        PRINT *,'INCREASE DIMENSION OF DBY !'                                    ENTR.16
        STOP 1212                                                                ENTR.17
      ENDIF                                                                      ENTR.18
C                                                                                ENTR.19
C------- SPECIFY CONSTANTS                                                       ENTR.20
C                                                                                ENTR.21
      BU=0.                                                                      ENTR.22
      R=287.04                                                                   ENTR.23
      G=9.81                                                                     ENTR.24
      BH=5.E00                                                                   ENTR.25
      KTOP(LP)=2                                                                 ENTR.26
C------- INITIALIZE                                                              ENTR.27
C                                                                                ENTR.28
      DO 40 K=1,KBC                                                              ENTR.29
        HC(K)=HKB                                                                ENTR.30
   40 CONTINUE                                                                   ENTR.31
      DO 50 K=KBC,KDIM-1                                                         ENTR.32
        HC(K)=.5*(HSAT(K)+HSAT(K+1))                                             ENTR.33
   50 CONTINUE                                                                   ENTR.34
      HC(KDIM)=HSAT(KDIM)                                                        ENTR.35
      HC(KBC)=HKB                                                                ENTR.36
C                                                                                ENTR.37
C--- DO CALCULATIONS TWICE, THE FIRST TIME TOP HEIGHT IS NOT KNOWN!              ENTR.38
C                                                                                ENTR.39
      ITERAT=0                                                                   ENTR.40
   99 CONTINUE                                                                   ENTR.41
      ITERAT=ITERAT+1                                                            ENTR.42
      KEND=KDIM-1                                                                ENTR.43
      IF(ITERAT.EQ.2)KEND=KTOP(LP)-1                                             ENTR.44
      DO 100 K=KBC+1,KEND                                                        ENTR.45
        DZ=-.5*P(K-1)+.5*P(K+1)                                                  ENTR.46
        HC(K)=(HC(K-1)*(1.-CD(K,LP)*DZ*ENT*.5)+(1.+CD(K,LP))*ENT*DZ*H(K)         ENTR.47
     +        )/(1.+(1.+.5*CD(K,LP))*ENT*DZ)                                     ENTR.48
  100 CONTINUE                                                                   ENTR.49
      DZ=.5*(P(KEND+1)-P(KEND))                                                  ENTR.50
      HC(KEND+1)=(HC(KEND)*(1.-CD(KEND+1,LP)*DZ*ENT*.5)+(1.+                     ENTR.51
     +           CD(KEND+1,LP))*ENT*DZ*H(KEND+1))/(1.+(1.+.5*                    ENTR.52
     +           CD(KEND+1,LP))*ENT*DZ)                                          ENTR.53
C                                                                                ENTR.54
C--- DETERMINE CLOUD TOP LEVEL IN FIRST ROUND!                                   ENTR.55
C                                                                                ENTR.56
      IF(ITERAT.EQ.1)THEN                                                        ENTR.57
        DO 200 K=KBC+1,KDIM-1                                                    ENTR.58
          DBY(K)=HC(K)-.5*(HSAT(K)+HSAT(K+1))                                    ENTR.59
  200   CONTINUE                                                                 ENTR.60
        DBY(KDIM)=HC(KDIM)-HSAT(KDIM)                                            ENTR.61
        DO 300 K=1,KDIM-KBC-1                                                    ENTR.62
          KK=KDIM-K+1                                                            ENTR.63
          IF(DBY(KK).GE.BH)THEN                                                  ENTR.64
            KTOP(LP)=KK+1                                                        ENTR.65
            IF(KTOP(LP).GE.KDIM)KTOP(LP)=KDIM                                    ENTR.66
            GOTO 99                                                              ENTR.67
          ELSE                                                                   ENTR.68
            KTOP(LP)=2                                                           ENTR.69
          ENDIF                                                                  ENTR.70
  300   CONTINUE                                                                 ENTR.71
      ENDIF                                                                      ENTR.72
      RETURN                                                                     ENTR.73
      END                                                                        ENTR.74
                                                                                 ENTR.75
