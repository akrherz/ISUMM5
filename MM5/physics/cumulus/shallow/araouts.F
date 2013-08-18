        SUBROUTINE ARAOUTS(XMC,XMB,ZU,DELT,DELQ,KDIM,                            ARAOUTS.1
     *                    OUTTEM,OUTQ,IER,PRE,KNUM)                              ARAOUTS.2
      DIMENSION XMB(KNUM),DELT(KDIM,KNUM),DELQ(KDIM,KNUM),                       ARAOUTS.3
     *          ZU(KDIM,KNUM)                                                    ARAOUTS.4
      DIMENSION XMC(KDIM),OUTTEM(KDIM),OUTQ(KDIM)                                ARAOUTS.5
C------- ERASE                                                                   ARAOUTS.6
C                                                                                ARAOUTS.7
      IER=0                                                                      ARAOUTS.8
      IBCOUT=0                                                                   ARAOUTS.9
      PRE=0.                                                                     ARAOUTS.10
      DO 50 K=1,KDIM                                                             ARAOUTS.11
        OUTTEM(K)=0.                                                             ARAOUTS.12
        OUTQ(K)=0.                                                               ARAOUTS.13
        XMC(K)=0.                                                                ARAOUTS.14
   50 CONTINUE                                                                   ARAOUTS.15
      DO 51 K=1,KNUM                                                             ARAOUTS.16
        IF(XMB(K).GT.0.)IBCOUT=1                                                 ARAOUTS.17
   51 CONTINUE                                                                   ARAOUTS.18
      IF(IBCOUT.EQ.0)THEN                                                        ARAOUTS.19
        IER=888                                                                  ARAOUTS.20
        RETURN                                                                   ARAOUTS.21
      ENDIF                                                                      ARAOUTS.22
C                                                                                ARAOUTS.23
C------- FIRST CALCULATE TEMPERATURE AND MOISTURE CHANGES!                       ARAOUTS.24
C                                                                                ARAOUTS.25
      DO 100 K=1,KDIM                                                            ARAOUTS.26
        OUTTEM(K)=0.                                                             ARAOUTS.27
        OUTQ(K)=0.                                                               ARAOUTS.28
        DO 100 LP=1,KNUM                                                         ARAOUTS.29
          OUTTES=DELT(K,LP)*XMB(LP)*86400.                                       ARAOUTS.30
          IF((OUTTES.GT.500.).OR.(OUTTES.LT.-200.))THEN                          ARAOUTS.31
            XMB(LP)=0.                                                           ARAOUTS.32
            IER=777                                                              ARAOUTS.33
C          PRINT *,'IERR = 777 AT LEVEL K =',K                                   ARAOUTS.34
            RETURN                                                               ARAOUTS.35
          ENDIF                                                                  ARAOUTS.36
          OUTTEM(K)=OUTTEM(K)+DELT(K,LP)*XMB(LP)                                 ARAOUTS.37
          OUTQ(K)=OUTQ(K)+DELQ(K,LP)*XMB(LP)                                     ARAOUTS.38
  100   CONTINUE                                                                 ARAOUTS.39
                                                                                 ARAOUTS.40
C                                                                                ARAOUTS.41
C------ HOW ABOUT THEM MASS FLUXES??                                             ARAOUTS.42
C                                                                                ARAOUTS.43
      DO 200 K=1,KDIM                                                            ARAOUTS.44
        XMC(K)=0.                                                                ARAOUTS.45
        DO 200 LP=1,KNUM                                                         ARAOUTS.46
          XMC(K)=XMC(K)+ZU(K,LP)*XMB(LP)                                         ARAOUTS.47
  200   CONTINUE                                                                 ARAOUTS.48
                                                                                 ARAOUTS.49
      RETURN                                                                     ARAOUTS.50
      END                                                                        ARAOUTS.51
                                                                                 ARAOUTS.52
