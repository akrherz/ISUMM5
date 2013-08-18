C*********************************************************************           ZUNC.1
C*                                                                   *           ZUNC.2
C*      NORMALIZED UPDRAFT MASS FLUX FUNCTION                        *           ZUNC.3
C*                                                                   *           ZUNC.4
C*********************************************************************           ZUNC.5
C                                                                                ZUNC.6
C                                                                                ZUNC.7
          SUBROUTINE ZUNC (KBEG,ZU,KB,R,Z,KDIM,LP,KNUM,KTOP)                     ZUNC.8
      DIMENSION ZU(KDIM,KNUM),Z(KDIM)                                            ZUNC.9
      INTEGER KTOP(KNUM)                                                         ZUNC.10
C                                                                                ZUNC.11
C--- BELOW UPDRAFT AIR ORIGINATING LEVEL (ZU)                                    ZUNC.12
C                                                                                ZUNC.13
      DO 25 K=1,KBEG                                                             ZUNC.14
        ZU(K,LP)=0.                                                              ZUNC.15
   25 CONTINUE                                                                   ZUNC.16
C                                                                                ZUNC.17
C--- BETWEEN ZU AND LEVEL OF FREE CONVECTION                                     ZUNC.18
C                                                                                ZUNC.19
      DO 50 K=KBEG,KB                                                            ZUNC.20
        ZU(K,LP)=1.                                                              ZUNC.21
   50 CONTINUE                                                                   ZUNC.22
C                                                                                ZUNC.23
C--- BETWEEN LEVEL OF FREE CONVECTION AND CLOUD TOP                              ZUNC.24
C                                                                                ZUNC.25
      DO 100 K=KB+1,KTOP(LP)-1                                                   ZUNC.26
        DZ=.5*Z(K+1)-.5*Z(K-1)                                                   ZUNC.27
        ZU(K,LP)=ZU(K-1,LP)*(1.+R*DZ)                                            ZUNC.28
  100 CONTINUE                                                                   ZUNC.29
C                                                                                ZUNC.30
C--- CLOUD TOP                                                                   ZUNC.31
C                                                                                ZUNC.32
      LPT=KTOP(LP)                                                               ZUNC.33
      DZ=(Z(LPT)-Z(LPT-1))*.5                                                    ZUNC.34
      ZU(LPT,LP)=ZU(LPT-1,LP)*(1.+R*DZ)                                          ZUNC.35
      RETURN                                                                     ZUNC.36
      END                                                                        ZUNC.37
                                                                                 ZUNC.38
