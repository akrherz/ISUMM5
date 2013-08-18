      SUBROUTINE ENVIRTHT(P1,T1,Q1,THT1,R1,RL,                                   ENVIRTHT.1
     *   ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)                                ENVIRTHT.2
      IMPLICIT NONE                                                              07NOV00.1398
C Declarations for Implicit None                                                 07NOV00.1399
      REAL    R1,EE,Q1,P1,TLOG,ALIQ,TDPT,CLIQ,DLIQ,BLIQ,TSAT,T00                 07NOV00.1400
      REAL    T1,THT,P00,THT1,C1,C2,AICE,TFPT,CICE,DICE,BICE                     07NOV00.1401
      REAL    C3,C4,TLOGIC,TSATLQ,TSATIC,RL,C5                                   07NOV00.1402
      DATA T00,P00,C1,C2,C3,C4,C5/273.16,1.E5,3374.6525,2.5403,3114.834,         ENVIRTHT.3
     *     0.278296,1.0723E-3/                                                   ENVIRTHT.4
C                                                                                ENVIRTHT.5
C  CALCULATE ENVIRONMENTAL EQUIVALENT POTENTIAL TEMPERATURE...                   ENVIRTHT.6
C                                                                                ENVIRTHT.7
      IF(R1.LT.1.E-6)THEN                                                        ENVIRTHT.8
        EE=Q1*P1/(0.622+Q1)                                                      ENVIRTHT.9
        TLOG=ALOG(EE/ALIQ)                                                       ENVIRTHT.10
        TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)                                        ENVIRTHT.11
        TSAT=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(T1-T00))*(T1-TDPT)          ENVIRTHT.12
        THT=T1*(P00/P1)**(0.2854*(1.-0.28*Q1))                                   ENVIRTHT.13
        THT1=THT*EXP((C1/TSAT-C2)*Q1*(1.+0.81*Q1))                               ENVIRTHT.14
      ELSEIF(ABS(R1-1.).LT.1.E-6)THEN                                            ENVIRTHT.15
        EE=Q1*P1/(0.622+Q1)                                                      ENVIRTHT.16
        TLOG=ALOG(EE/AICE)                                                       ENVIRTHT.17
        TFPT=(CICE-DICE*TLOG)/(BICE-TLOG)                                        ENVIRTHT.18
        THT=T1*(P00/P1)**(0.2854*(1.-0.28*Q1))                                   ENVIRTHT.19
        TSAT=TFPT-(.182+1.13E-3*(TFPT-T00)-3.58E-4*(T1-T00))*(T1-TFPT)           ENVIRTHT.20
        THT1=THT*EXP((C3/TSAT-C4)*Q1*(1.+0.81*Q1))                               ENVIRTHT.21
      ELSE                                                                       ENVIRTHT.22
        EE=Q1*P1/(0.622+Q1)                                                      ENVIRTHT.23
        TLOG=ALOG(EE/ALIQ)                                                       ENVIRTHT.24
        TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)                                        ENVIRTHT.25
        TLOGIC=ALOG(EE/AICE)                                                     ENVIRTHT.26
        TFPT=(CICE-DICE*TLOGIC)/(BICE-TLOGIC)                                    ENVIRTHT.27
        THT=T1*(P00/P1)**(0.2854*(1.-0.28*Q1))                                   ENVIRTHT.28
        TSATLQ=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(T1-T00))*(T1-TDPT         ENVIRTHT.29
     +         )                                                                 ENVIRTHT.30
        TSATIC=TFPT-(.182+1.13E-3*(TFPT-T00)-3.58E-4*(T1-T00))*(T1-TFPT)         ENVIRTHT.31
        TSAT=R1*TSATIC+(1.-R1)*TSATLQ                                            ENVIRTHT.32
        THT1=THT*EXP(RL*Q1*C5/TSAT*(1.+0.81*Q1))                                 ENVIRTHT.33
      ENDIF                                                                      ENVIRTHT.34
      RETURN                                                                     ENVIRTHT.35
      END                                                                        ENVIRTHT.36
                                                                                 ENVIRTHT.37
