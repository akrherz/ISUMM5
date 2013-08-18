      SUBROUTINE DTFRZNEW(TU,P,THTEU,QVAP,QLIQ,QICE,RATIO2,TTFRZ,TBFRZ,          DTFRZNEW.1
     *               QNWFRZ,RL,FRC1,EFFQ,IFLAG,XLV0,XLV1,XLS0,XLS1,              DTFRZNEW.2
     *   ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)                                DTFRZNEW.3
C                                                                                DTFRZNEW.4
C...ALLOW GLACIATION OF THE UPDRAFT TO OCCUR AS AN APPROXIMATELY LINEAR          DTFRZNEW.5
C   FUNCTION OF TEMPERATURE IN THE TEMPERATURE RANGE TTFRZ TO TBFRZ...           DTFRZNEW.6
C                                                                                DTFRZNEW.7
      RV=461.5                                                                   DTFRZNEW.8
      C5=1.0723E-3                                                               DTFRZNEW.9
C                                                                                DTFRZNEW.10
C...ADJUST THE LIQUID WATER CONCENTRATIONS FROM FRESH CONDENSATE AND THA         DTFRZNEW.11
C   BROUGHT UP FROM LOWER LEVELS TO AN AMOUNT THAT WOULD BE PRESENT IF N         DTFRZNEW.12
C   LIQUID WATER HAD FROZEN THUS FAR...THIS IS NECESSARY BECAUSE THE             DTFRZNEW.13
C   EXPRESSION FOR TEMP CHANGE IS MULTIPLIED BY THE FRACTION EQUAL TO TH         DTFRZNEW.14
C   PARCEL TEMP DECREASE SINCE THE LAST MODEL LEVEL DIVIDED BY THE TOTAL         DTFRZNEW.15
C   GLACIATION INTERVAL, SO THAT EFFECTIVELY THIS APPROXIMATELY ALLOWS A         DTFRZNEW.16
C   AMOUNT OF LIQUID WATER TO FREEZE WHICH IS EQUAL TO THIS SAME FRACTIO         DTFRZNEW.17
C   OF THE LIQUID WATER THAT WAS PRESENT BEFORE THE GLACIATION PROCESS W         DTFRZNEW.18
C   INITIATED...ALSO, TO ALLOW THETAU TO CONVERT APPROXIMATELY LINEARLY          DTFRZNEW.19
C   ITS VALUE WITH RESPECT TO ICE, WE NEED TO ALLOW A PORTION OF THE FRE         DTFRZNEW.20
C   CONDENSATE TO CONTRIBUTE TO THE GLACIATION PROCESS; THE FRACTIONAL           DTFRZNEW.21
C   AMOUNT THAT APPLIES TO THIS PORTION IS 1/2 OF THE FRACTIONAL AMOUNT          DTFRZNEW.22
C   FROZEN OF THE "OLD" CONDENSATE BECAUSE THIS FRESH CONDENSATE IS ONLY         DTFRZNEW.23
C   PRODUCED GRADUALLY OVER THE LAYER...NOTE THAT IN TERMS OF THE DYNAMI         DTFRZNEW.24
C   OF THE PRECIPITATION PROCESS, IE. PRECIPITATION FALLOUT, THIS FRACTI         DTFRZNEW.25
C   AMNT OF FRESH CONDENSATE HAS ALREADY BEEN INCLUDED IN THE ICE CATEGO         DTFRZNEW.26
C                                                                                DTFRZNEW.27
      QLQFRZ=QLIQ*EFFQ                                                           DTFRZNEW.28
      QNEW=QNWFRZ*EFFQ*0.5                                                       DTFRZNEW.29
      ESLIQ=ALIQ*EXP((BLIQ*TU-CLIQ)/(TU-DLIQ))                                   DTFRZNEW.30
      ESICE=AICE*EXP((BICE*TU-CICE)/(TU-DICE))                                   DTFRZNEW.31
      RLC=2.5E6-2369.276*(TU-273.16)                                             DTFRZNEW.32
      RLS=2833922.-259.532*(TU-273.16)                                           DTFRZNEW.33
      RLF=RLS-RLC                                                                DTFRZNEW.34
      CP=1005.7*(1.+0.89*QVAP)                                                   DTFRZNEW.35
C                                                                                DTFRZNEW.36
C  A = D(ES)/DT IS THAT CALCULATED FROM BUCK`S (1981) EMPIRICAL FORMULAS         DTFRZNEW.37
C  FOR SATURATION VAPOR PRESSURE...                                              DTFRZNEW.38
C                                                                                DTFRZNEW.39
      A=(CICE-BICE*DICE)/((TU-DICE)*(TU-DICE))                                   DTFRZNEW.40
      B=RLS*0.622/P                                                              DTFRZNEW.41
      C=A*B*ESICE/CP                                                             DTFRZNEW.42
      DQVAP=B*(ESLIQ-ESICE)/(RLS+RLS*C)-RLF*(QLQFRZ+QNEW)/(RLS+RLS/C)            DTFRZNEW.43
      DTFRZ=(RLF*(QLQFRZ+QNEW)+B*(ESLIQ-ESICE))/(CP+A*B*ESICE)                   DTFRZNEW.44
      TU1=TU                                                                     DTFRZNEW.45
      QVAP1=QVAP                                                                 DTFRZNEW.46
      TU=TU+FRC1*DTFRZ                                                           DTFRZNEW.47
      QVAP=QVAP-FRC1*DQVAP                                                       DTFRZNEW.48
      ES=QVAP*P/(0.622+QVAP)                                                     DTFRZNEW.49
      ESLIQ=ALIQ*EXP((BLIQ*TU-CLIQ)/(TU-DLIQ))                                   DTFRZNEW.50
      ESICE=AICE*EXP((BICE*TU-CICE)/(TU-DICE))                                   DTFRZNEW.51
      RATIO2=(ESLIQ-ES)/(ESLIQ-ESICE)                                            DTFRZNEW.52
C                                                                                DTFRZNEW.53
C  TYPICALLY, RATIO2 IS VERY CLOSE TO (TTFRZ-TU)/(TTFRZ-TBFRZ), USUALLY          DTFRZNEW.54
C  WITHIN 1% (USING TU BEFORE GALCIATION EFFECTS ARE APPLIED);  IF THE           DTFRZNEW.55
C  INITIAL UPDRAFT TEMP IS BELOW TBFRZ AND RATIO2 IS STILL LESS THAN 1,          DTFRZNEW.56
C  AN ADJUSTMENT TO FRC1 AND RATIO2 IS INTRODUCED SO THAT GLACIATION             DTFRZNEW.57
C  EFFECTS ARE NOT UNDERESTIMATED; CONVERSELY, IF RATIO2 IS GREATER THAN         DTFRZNEW.58
C  FRC1 IS ADJUSTED SO THAT GLACIATION EFFECTS ARE NOT OVERESTIMATED...          DTFRZNEW.59
C                                                                                DTFRZNEW.60
      IF(IFLAG.GT.0.AND.RATIO2.LT.1)THEN                                         DTFRZNEW.61
        FRC1=FRC1+(1.-RATIO2)                                                    DTFRZNEW.62
        TU=TU1+FRC1*DTFRZ                                                        DTFRZNEW.63
        QVAP=QVAP1-FRC1*DQVAP                                                    DTFRZNEW.64
        RATIO2=1.                                                                DTFRZNEW.65
        IFLAG=1                                                                  DTFRZNEW.66
        GOTO 20                                                                  DTFRZNEW.67
      ENDIF                                                                      DTFRZNEW.68
      IF(RATIO2.GT.1.)THEN                                                       DTFRZNEW.69
        FRC1=FRC1-(RATIO2-1.)                                                    DTFRZNEW.70
        FRC1=AMAX1(0.0,FRC1)                                                     DTFRZNEW.71
        TU=TU1+FRC1*DTFRZ                                                        DTFRZNEW.72
        QVAP=QVAP1-FRC1*DQVAP                                                    DTFRZNEW.73
        RATIO2=1.                                                                DTFRZNEW.74
        IFLAG=1                                                                  DTFRZNEW.75
      ENDIF                                                                      DTFRZNEW.76
C                                                                                DTFRZNEW.77
C  CALCULATE A HYBRID VALUE OF THETAU, ASSUMING THAT THE LATENT HEAT OF          DTFRZNEW.78
C  VAPORIZATION/SUBLIMATION CAN BE ESTIMATED USING THE SAME WEIGHTING            DTFRZNEW.79
C  FUNCTION AS THAT USED TO CALCULATE SATURATION VAPOR PRESSURE, CALCU-          DTFRZNEW.80
C  LATE NEW LIQUID WATER AND ICE CONCENTRATIONS...                               DTFRZNEW.81
C                                                                                DTFRZNEW.82
   20 RLC=XLV0-XLV1*TU                                                           DTFRZNEW.83
      RLS=XLS0-XLS1*TU                                                           DTFRZNEW.84
      RL=RATIO2*RLS+(1.-RATIO2)*RLC                                              DTFRZNEW.85
      PI=(1.E5/P)**(0.2854*(1.-0.28*QVAP))                                       DTFRZNEW.86
      THTEU=TU*PI*EXP(RL*QVAP*C5/TU*(1.+0.81*QVAP))                              DTFRZNEW.87
      IF(IFLAG.EQ.1)THEN                                                         DTFRZNEW.88
        QICE=QICE+FRC1*DQVAP+QLIQ                                                DTFRZNEW.89
        QLIQ=0.                                                                  DTFRZNEW.90
      ELSE                                                                       DTFRZNEW.91
        QICE=QICE+FRC1*(DQVAP+QLQFRZ)                                            DTFRZNEW.92
        QLIQ=QLIQ-FRC1*QLQFRZ                                                    DTFRZNEW.93
      ENDIF                                                                      DTFRZNEW.94
      QNWFRZ=0.                                                                  DTFRZNEW.95
      RETURN                                                                     DTFRZNEW.96
      END                                                                        DTFRZNEW.97
                                                                                 DTFRZNEW.98
