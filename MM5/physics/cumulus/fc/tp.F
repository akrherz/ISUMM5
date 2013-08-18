      FUNCTION TP (PRESS,THETAE,TGS,D273,RL,QS,PI,XTIME,JFLAG)                   TP.1
C                                                                                TP.2
C...ITERATIVELY CONVERT TEMPERATURE FROM EQUIVALENT POTENTIAL                    TP.3
C...TEMPERATURE.                                                                 TP.4
C                                                                                TP.5
      ITCNT=0                                                                    TP.6
      RL461=RL/461.5                                                             TP.7
      RL1004=RL/1004.                                                            TP.8
      RP=THETAE/PI                                                               TP.9
      ES=611.*EXP(RL461*(D273-1./TGS))                                           TP.10
      QS=.622*ES/(PRESS-ES)                                                      TP.11
      FO=TGS*EXP(RL1004*QS/TGS)-RP                                               TP.12
      T1=TGS-.5*FO                                                               TP.13
      T1=AMAX1(T1,100.)                                                          TP.14
      TGUESS=TGS                                                                 TP.15
   10 CONTINUE                                                                   TP.16
      ITCNT=ITCNT+1                                                              TP.17
      IF(ITCNT.GT.10)THEN                                                        TP.18
        JFLAG=1                                                                  TP.19
        RETURN                                                                   TP.20
      ENDIF                                                                      TP.21
      ES=611.*EXP(RL461*(D273-1./T1))                                            TP.22
      QS=.622*ES/(PRESS-ES)                                                      TP.23
      F1=T1*EXP(RL1004*QS/T1)-RP                                                 TP.24
      IF(ABS(F1).LT..05)GOTO 20                                                  TP.25
      DT=F1*(T1-TGUESS)/(F1-FO)                                                  TP.26
      TGUESS=T1                                                                  TP.27
      FO=F1                                                                      TP.28
      T1=T1-DT                                                                   TP.29
      GOTO 10                                                                    TP.30
C                                                                                TP.31
   20 TP=T1                                                                      TP.32
      RETURN                                                                     TP.33
      END                                                                        TP.34
                                                                                 TP.35
