        SUBROUTINE ARAOUT(XMC,XMB,ZU,DELT,DELQ,KDIM,                             ARAOUT.1
     *                    OUTTEM,OUTQ,IER,PW,PRE,PWD,ZD,EDT,KNUM)                ARAOUT.2
      DIMENSION XMB(KNUM),DELT(KDIM,KNUM),DELQ(KDIM,KNUM),                       ARAOUT.3
     *          ZU(KDIM,KNUM),PW(KDIM,KNUM),PWD(KDIM,KNUM),                      ARAOUT.4
     *          ZD(KDIM,KNUM),EDT(KNUM)                                          ARAOUT.5
      DIMENSION XMC(KDIM),OUTTEM(KDIM),OUTQ(KDIM)                                ARAOUT.6
C------- ERASE                                                                   ARAOUT.7
C                                                                                ARAOUT.8
      IER=0                                                                      ARAOUT.9
      IBCOUT=0                                                                   ARAOUT.10
      PRE=0.                                                                     ARAOUT.11
      DO 50 K=1,KDIM                                                             ARAOUT.12
        OUTTEM(K)=0.                                                             ARAOUT.13
        OUTQ(K)=0.                                                               ARAOUT.14
        XMC(K)=0.                                                                ARAOUT.15
   50 CONTINUE                                                                   ARAOUT.16
      DO 51 K=1,KNUM                                                             ARAOUT.17
        IF(XMB(K).GT.0.)IBCOUT=1                                                 ARAOUT.18
   51 CONTINUE                                                                   ARAOUT.19
      IF(IBCOUT.EQ.0)THEN                                                        ARAOUT.20
        IER=999                                                                  ARAOUT.21
        RETURN                                                                   ARAOUT.22
      ENDIF                                                                      ARAOUT.23
C                                                                                ARAOUT.24
C------- FIRST CALCULATE TEMPERATURE AND MOISTURE CHANGES!                       ARAOUT.25
C                                                                                ARAOUT.26
      DO 100 K=1,KDIM                                                            ARAOUT.27
        OUTTEM(K)=0.                                                             ARAOUT.28
        OUTQ(K)=0.                                                               ARAOUT.29
        DO 100 LP=1,KNUM                                                         ARAOUT.30
          OUTTES=DELT(K,LP)*XMB(LP)*86400.                                       ARAOUT.31
          IF((OUTTES.GT.500.).OR.(OUTTES.LT.-200.))XMB(LP)=0.                    ARAOUT.32
          OUTTEM(K)=OUTTEM(K)+DELT(K,LP)*XMB(LP)                                 ARAOUT.33
          OUTQ(K)=OUTQ(K)+DELQ(K,LP)*XMB(LP)                                     ARAOUT.34
  100   CONTINUE                                                                 ARAOUT.35
                                                                                 ARAOUT.36
C                                                                                ARAOUT.37
C------ HOW ABOUT THEM MASS FLUXES??                                             ARAOUT.38
C                                                                                ARAOUT.39
      DO 200 K=1,KDIM                                                            ARAOUT.40
        XMC(K)=0.                                                                ARAOUT.41
        DO 200 LP=1,KNUM                                                         ARAOUT.42
          XMC(K)=XMC(K)+(ZU(K,LP)-EDT(LP)*ZD(K,LP))*XMB(LP)                      ARAOUT.43
  200   CONTINUE                                                                 ARAOUT.44
                                                                                 ARAOUT.45
C                                                                                ARAOUT.46
C------- PRECIPITATION                                                           ARAOUT.47
C                                                                                ARAOUT.48
      DO 300 LP=1,KNUM                                                           ARAOUT.49
        PRE1=0.                                                                  ARAOUT.50
        DO 301 K=1,KDIM                                                          ARAOUT.51
          PRE1=PRE1+(PW(K,LP)+EDT(LP)*PWD(K,LP))*XMB(LP)                         ARAOUT.52
  301   CONTINUE                                                                 ARAOUT.53
        PRE=PRE+PRE1                                                             ARAOUT.54
  300 CONTINUE                                                                   ARAOUT.55
      RETURN                                                                     ARAOUT.56
      END                                                                        ARAOUT.57
                                                                                 ARAOUT.58
