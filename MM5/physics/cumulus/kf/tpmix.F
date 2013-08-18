       SUBROUTINE TPMIX(P,THTU,TU,QU,QLIQ,QICE,QNEWLQ,QNEWIC,RATIO2,RL,          TPMIX.1
     *                   XLV0,XLV1,XLS0,XLS1,                                    TPMIX.2
     *   ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)                                TPMIX.3
      IMPLICIT NONE                                                              07NOV00.1436
C Declarations for Implicit none                                                 07NOV00.1437
      REAL    C5,RV,RATIO2,ES,ALIQ,BLIQ,TU,CLIQ,DLIQ,QS,P,PI,THTGS,AICE          07NOV00.1438
      REAL    BICE,CICE,DICE,ESLIQ,ESICE,RL,F0,THTU,T1,T0,F1,DT,QU,QNEW          07NOV00.1439
      REAL    DQ,QTOT,QLIQ,QICE,DQICE,DQLIQ,RLL,XLV0,XLV1,XLS0,XLS1              07NOV00.1440
      REAL    CP,QNEWLQ,QNEWIC                                                   07NOV00.1441
      INTEGER ITCNT                                                              07NOV00.1442
                                                                                 07NOV00.1443
C                                                                                TPMIX.4
C...THIS SUBROUTINE ITERATIVELY EXTRACTS WET-BULB TEMPERATURE FROM EQUIV         TPMIX.5
C   POTENTIAL TEMPERATURE, THEN CHECKS TO SEE IF SUFFICIENT MOISTURE IS          TPMIX.6
C   AVAILABLE TO ACHIEVE SATURATION...IF NOT, TEMPERATURE IS ADJUSTED            TPMIX.7
C   ACCORDINGLY, IF SO, THE RESIDUAL LIQUID WATER/ICE CONCENTRATION IS           TPMIX.8
C   DETERMINED...                                                                TPMIX.9
C                                                                                TPMIX.10
      C5=1.0723E-3                                                               TPMIX.11
      RV=461.5                                                                   TPMIX.12
C                                                                                TPMIX.13
C   ITERATE TO FIND WET BULB TEMPERATURE AS A FUNCTION OF EQUIVALENT POT         TPMIX.14
C   TEMP AND PRS, ASSUMING SATURATION VAPOR PRESSURE...RATIO2 IS THE DEG         TPMIX.15
C   OF GLACIATION...                                                             TPMIX.16
C                                                                                TPMIX.17
      IF(RATIO2.LT.1.E-6)THEN                                                    TPMIX.18
        ES=ALIQ*EXP((BLIQ*TU-CLIQ)/(TU-DLIQ))                                    TPMIX.19
        QS=0.622*ES/(P-ES)                                                       TPMIX.20
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))                                       TPMIX.21
        THTGS=TU*PI*EXP((3374.6525/TU-2.5403)*QS*(1.+0.81*QS))                   TPMIX.22
      ELSEIF(ABS(RATIO2-1.).LT.1.E-6)THEN                                        TPMIX.23
        ES=AICE*EXP((BICE*TU-CICE)/(TU-DICE))                                    TPMIX.24
        QS=0.622*ES/(P-ES)                                                       TPMIX.25
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))                                       TPMIX.26
        THTGS=TU*PI*EXP((3114.834/TU-0.278296)*QS*(1.+0.81*QS))                  TPMIX.27
      ELSE                                                                       TPMIX.28
        ESLIQ=ALIQ*EXP((BLIQ*TU-CLIQ)/(TU-DLIQ))                                 TPMIX.29
        ESICE=AICE*EXP((BICE*TU-CICE)/(TU-DICE))                                 TPMIX.30
        ES=(1.-RATIO2)*ESLIQ+RATIO2*ESICE                                        TPMIX.31
        QS=0.622*ES/(P-ES)                                                       TPMIX.32
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))                                       TPMIX.33
        THTGS=TU*PI*EXP(RL*QS*C5/TU*(1.+0.81*QS))                                TPMIX.34
      ENDIF                                                                      TPMIX.35
      F0=THTGS-THTU                                                              TPMIX.36
      T1=TU-0.5*F0                                                               TPMIX.37
      T0=TU                                                                      TPMIX.38
      ITCNT=0                                                                    TPMIX.39
   90 IF(RATIO2.LT.1.E-6)THEN                                                    TPMIX.40
        ES=ALIQ*EXP((BLIQ*T1-CLIQ)/(T1-DLIQ))                                    TPMIX.41
        QS=0.622*ES/(P-ES)                                                       TPMIX.42
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))                                       TPMIX.43
        THTGS=T1*PI*EXP((3374.6525/T1-2.5403)*QS*(1.+0.81*QS))                   TPMIX.44
      ELSEIF(ABS(RATIO2-1.).LT.1.E-6)THEN                                        TPMIX.45
        ES=AICE*EXP((BICE*T1-CICE)/(T1-DICE))                                    TPMIX.46
        QS=0.622*ES/(P-ES)                                                       TPMIX.47
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))                                       TPMIX.48
        THTGS=T1*PI*EXP((3114.834/T1-0.278296)*QS*(1.+0.81*QS))                  TPMIX.49
      ELSE                                                                       TPMIX.50
        ESLIQ=ALIQ*EXP((BLIQ*T1-CLIQ)/(T1-DLIQ))                                 TPMIX.51
        ESICE=AICE*EXP((BICE*T1-CICE)/(T1-DICE))                                 TPMIX.52
        ES=(1.-RATIO2)*ESLIQ+RATIO2*ESICE                                        TPMIX.53
        QS=0.622*ES/(P-ES)                                                       TPMIX.54
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))                                       TPMIX.55
        THTGS=T1*PI*EXP(RL*QS*C5/T1*(1.+0.81*QS))                                TPMIX.56
      ENDIF                                                                      TPMIX.57
      F1=THTGS-THTU                                                              TPMIX.58
      IF(ABS(F1).LT.0.01)GOTO 50                                                 TPMIX.59
      ITCNT=ITCNT+1                                                              TPMIX.60
      IF(ITCNT.GT.10)GOTO 50                                                     TPMIX.61
      DT=F1*(T1-T0)/(F1-F0)                                                      TPMIX.62
      T0=T1                                                                      TPMIX.63
      F0=F1                                                                      TPMIX.64
      T1=T1-DT                                                                   TPMIX.65
      GOTO 90                                                                    TPMIX.66
C                                                                                TPMIX.67
C   IF THE PARCEL IS SUPERSATURATED, CALCULATE CONCENTRATION OF FRESH            TPMIX.68
C   CONDENSATE...                                                                TPMIX.69
C                                                                                TPMIX.70
   50 IF(QS.LE.QU)THEN                                                           TPMIX.71
        QNEW=QU-QS                                                               TPMIX.72
        QU=QS                                                                    TPMIX.73
        GOTO 96                                                                  TPMIX.74
      ENDIF                                                                      TPMIX.75
C                                                                                TPMIX.76
C   IF THE PARCEL IS SUBSATURATED, TEMPERATURE AND MIXING RATIO MUST BE          TPMIX.77
C   ADJUSTED...IF LIQUID WATER OR ICE IS PRESENT, IT IS ALLOWED TO EVAPO         TPMIX.78
C   SUBLIMATE.                                                                   TPMIX.79
C                                                                                TPMIX.80
      QNEW=0.                                                                    TPMIX.81
      DQ=QS-QU                                                                   TPMIX.82
      QTOT=QLIQ+QICE                                                             TPMIX.83
C                                                                                TPMIX.84
C   IF THERE IS ENOUGH LIQUID OR ICE TO SATURATE THE PARCEL, TEMP STAYS          TPMIX.85
C   WET BULB VALUE, VAPOR MIXING RATIO IS AT SATURATED LEVEL, AND THE MI         TPMIX.86
C   RATIOS OF LIQUID AND ICE ARE ADJUSTED TO MAKE UP THE ORIGINAL SATURA         TPMIX.87
C   DEFICIT... OTHERWISE, ANY AVAILABLE LIQ OR ICE VAPORIZES AND APPROPR         TPMIX.88
C   ADJUSTMENTS TO PARCEL TEMP; VAPOR, LIQUID, AND ICE MIXING RATIOS ARE         TPMIX.89
C                                                                                TPMIX.90
C...NOTE THAT THE LIQ AND ICE MAY BE PRESENT IN PROPORTIONS SLIGHTLY DIF         TPMIX.91
C   THAN SUGGESTED BY THE VALUE OF RATIO2...CHECK TO MAKE SURE THAT LIQ          TPMIX.92
C   ICE CONCENTRATIONS ARE NOT REDUCED TO BELOW ZERO WHEN EVAPORATION/           TPMIX.93
C   SUBLIMATION OCCURS...                                                        TPMIX.94
C                                                                                TPMIX.95
      IF(QTOT.GE.DQ)THEN                                                         TPMIX.96
        DQICE=0.0                                                                TPMIX.97
        DQLIQ=0.0                                                                TPMIX.98
        QLIQ=QLIQ-(1.-RATIO2)*DQ                                                 TPMIX.99
        IF(QLIQ.LT.0.)THEN                                                       TPMIX.100
          DQICE=0.0-QLIQ                                                         TPMIX.101
          QLIQ=0.0                                                               TPMIX.102
        ENDIF                                                                    TPMIX.103
        QICE=QICE-RATIO2*DQ+DQICE                                                TPMIX.104
        IF(QICE.LT.0.)THEN                                                       TPMIX.105
          DQLIQ=0.0-QICE                                                         TPMIX.106
          QICE=0.0                                                               TPMIX.107
        ENDIF                                                                    TPMIX.108
        QLIQ=QLIQ+DQLIQ                                                          TPMIX.109
        QU=QS                                                                    TPMIX.110
        GOTO 96                                                                  TPMIX.111
      ELSE                                                                       TPMIX.112
        IF(RATIO2.LT.1.E-6)THEN                                                  TPMIX.113
          RLL=XLV0-XLV1*T1                                                       TPMIX.114
        ELSEIF(ABS(RATIO2-1.).LT.1.E-6)THEN                                      TPMIX.115
          RLL=XLS0-XLS1*T1                                                       TPMIX.116
        ELSE                                                                     TPMIX.117
          RLL=RL                                                                 TPMIX.118
        ENDIF                                                                    TPMIX.119
        CP=1005.7*(1.+0.89*QU)                                                   TPMIX.120
        IF(QTOT.LT.1.E-10)THEN                                                   TPMIX.121
C                                                                                TPMIX.122
C...IF NO LIQUID WATER OR ICE IS AVAILABLE, TEMPERATURE IS GIVEN BY:             TPMIX.123
          T1=T1+RLL*(DQ/(1.+DQ))/CP                                              TPMIX.124
          GOTO 96                                                                TPMIX.125
        ELSE                                                                     TPMIX.126
C                                                                                TPMIX.127
C...IF SOME LIQ WATER/ICE IS AVAILABLE, BUT NOT ENOUGH TO ACHIEVE SATURA         TPMIX.128
C   THE TEMPERATURE IS GIVEN BY:                                                 TPMIX.129
          T1=T1+RLL*((DQ-QTOT)/(1+DQ-QTOT))/CP                                   TPMIX.130
          QU=QU+QTOT                                                             TPMIX.131
          QTOT=0.                                                                TPMIX.132
        ENDIF                                                                    TPMIX.133
        QLIQ=0                                                                   TPMIX.134
        QICE=0.                                                                  TPMIX.135
      ENDIF                                                                      TPMIX.136
   96 TU=T1                                                                      TPMIX.137
      QNEWLQ=(1.-RATIO2)*QNEW                                                    TPMIX.138
      QNEWIC=RATIO2*QNEW                                                         TPMIX.139
      IF(ITCNT.GT.10)PRINT*,'***** NUMBER OF ITERATIONS IN TPMIX =',             TPMIX.140
     +  ITCNT                                                                    TPMIX.141
      RETURN                                                                     TPMIX.142
      END                                                                        TPMIX.143
                                                                                 TPMIX.144
