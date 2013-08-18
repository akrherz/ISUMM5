      FUNCTION TPFC(PRESS,THETAE,TGS,D273,RL,QS,PI)                              TPFC.1
C                                                                                TPFC.2
C...ITERATIVELY EXTRACT TEMPERATURE FROM EQUIVALENT POTENTIAL                    TPFC.3
C...TEMPERATURE.                                                                 TPFC.4
C                                                                                TPFC.5
      RL461=RL/461.5                                                             TPFC.6
      RL1004=RL/1004.                                                            TPFC.7
      RP=THETAE/PI                                                               TPFC.8
      ES=611.*EXP(RL461*(D273-1./TGS))                                           TPFC.9
      QS=.622*ES/(PRESS-ES)                                                      TPFC.10
      FO=TGS*EXP(RL1004*QS/TGS)-RP                                               TPFC.11
      T1=TGS-.5*FO                                                               TPFC.12
      TGUESS=TGS                                                                 TPFC.13
   10 ES=611.*EXP(RL461*(D273-1./T1))                                            TPFC.14
      QS=.622*ES/(PRESS-ES)                                                      TPFC.15
      F1=T1*EXP(RL1004*QS/T1)-RP                                                 TPFC.16
      IF(ABS(F1).LT..1)GOTO 20                                                   TPFC.17
      DT=F1*(T1-TGUESS)/(F1-FO)                                                  TPFC.18
      TGUESS=T1                                                                  TPFC.19
      FO=F1                                                                      TPFC.20
      T1=T1-DT                                                                   TPFC.21
      GOTO 10                                                                    TPFC.22
C                                                                                TPFC.23
   20 TPFC=T1                                                                    TPFC.24
      RETURN                                                                     TPFC.25
      END                                                                        TPFC.26
                                                                                 TPFC.27
