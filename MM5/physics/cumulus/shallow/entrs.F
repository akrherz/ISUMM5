C                                                                                ENTRS.1
C*********************************************************************           ENTRS.2
C*                                                                   *           ENTRS.3
C*     THIS SUBROUTINE CALCULATES INCLOUD MOIST STATIC ENERGY        **          ENTRS.4
C**                                                                  **          ENTRS.5
C**********************************************************************          ENTRS.6
C**********************************************************************          ENTRS.7
C                                                                                ENTRS.8
C                                                                                ENTRS.9
        SUBROUTINE ENTRS(CD,KBC,H,HC,HSAT,ENT,KDIM,                              ENTRS.10
     *                   LP,KB,P,HKB,KNUM,KTOP)                                  ENTRS.11
      DIMENSION H(KDIM),HSAT(KDIM),HC(KDIM),                                     ENTRS.12
     *          P(KDIM),CD(KDIM,KNUM)                                            ENTRS.13
      DIMENSION DBY(80)                                                          ENTRS.14
      INTEGER KTOP(KNUM)                                                         ENTRS.15
      IF(KDIM.GT.80)THEN                                                         ENTRS.16
        PRINT *,'INCREASE DIMENSION OF DBY !'                                    ENTRS.17
        STOP 1212                                                                ENTRS.18
      ENDIF                                                                      ENTRS.19
C                                                                                ENTRS.20
C------- SPECIFY CONSTANTS                                                       ENTRS.21
C                                                                                ENTRS.22
      BU=0.                                                                      ENTRS.23
      R=287.04                                                                   ENTRS.24
      G=9.81                                                                     ENTRS.25
      BH=5.E00                                                                   ENTRS.26
      KTOP(LP)=1                                                                 ENTRS.27
C------- INITIALIZE                                                              ENTRS.28
C                                                                                ENTRS.29
      DO 40 K=1,KBC                                                              ENTRS.30
        HC(K)=HKB                                                                ENTRS.31
   40 CONTINUE                                                                   ENTRS.32
      DO 50 K=KBC+1,KDIM-1                                                       ENTRS.33
        HC(K)=.5*(HSAT(K)+HSAT(K+1))                                             ENTRS.34
   50 CONTINUE                                                                   ENTRS.35
      HC(KDIM)=HSAT(KDIM)                                                        ENTRS.36
C                                                                                ENTRS.37
C--- DO CALCULATIONS TWICE, THE FIRST TIME TOP HEIGHT IS NOT KNOWN!              ENTRS.38
C                                                                                ENTRS.39
      ITERAT=0                                                                   ENTRS.40
   99 CONTINUE                                                                   ENTRS.41
      KEND=KDIM-1                                                                ENTRS.42
      IF(KBC+1.GT.KEND)GOTO 101                                                  ENTRS.43
      DO 100 K=KBC+1,KEND                                                        ENTRS.44
        DZ=-.5*P(K-1)+.5*P(K+1)                                                  ENTRS.45
        HC(K)=(HC(K-1)*(1.-.5*DZ*ENT)+ENT*DZ*H(K))/(1.+.5*ENT*DZ)                07NOV00.1472
        DBY(K)=HC(K)-.5*(HSAT(K)+HSAT(K+1))                                      ENTRS.48
        DEPTH=P(K)-P(KBC+1)                                                      25JAN00.479
        IF(DBY(K).LT.0..OR.DEPTH.GT.3000.)THEN                                   25JAN00.480
          KTOP(LP)=K-1                                                           ENTRS.50
          KENDS=K-2                                                              ENTRS.51
          GOTO 101                                                               ENTRS.52
        ENDIF                                                                    ENTRS.53
  100 CONTINUE                                                                   ENTRS.54
  101 CONTINUE                                                                   ENTRS.55
      RETURN                                                                     ENTRS.56
      END                                                                        ENTRS.57
                                                                                 ENTRS.58
