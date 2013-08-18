************************** TPDD.FOR ************************************         TPDD.1
*   THIS SUBROUTINE ITERATIVELY EXTRACTS TEMPERATURE FROM EQUIVALENT   *         TPDD.2
*   POTENTIAL TEMP.  IT IS DESIGNED FOR USE WITH DOWNDRAFT CALCULATIONS.         TPDD.3
*   IF RELATIVE HUMIDITY IS SPECIFIED TO BE LESS THAN 100%, PARCEL     *         TPDD.4
*   TEMP, SPECIFIC HUMIDITY, AND LIQUID WATER CONTENT ARE ITERATIVELY  *         TPDD.5
*   CALCULATED.                                                        *         TPDD.6
************************************************************************         TPDD.7
      FUNCTION TPDD(P,THTED,TGS,RS,RD,RH,XLV0,XLV1,                              TPDD.8
     *   ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)                                TPDD.9
      ES=ALIQ*EXP((BLIQ*TGS-CLIQ)/(TGS-DLIQ))                                    TPDD.10
      RS=0.622*ES/(P-ES)                                                         TPDD.11
      PI=(1.E5/P)**(0.2854*(1.-0.28*RS))                                         TPDD.12
      THTGS=TGS*PI*EXP((3374.6525/TGS-2.5403)*RS*(1.+0.81*RS))                   TPDD.13
      F0=THTGS-THTED                                                             TPDD.14
      T1=TGS-0.5*F0                                                              TPDD.15
      T0=TGS                                                                     TPDD.16
      CP=1005.7                                                                  TPDD.17
C                                                                                TPDD.18
C...ITERATE TO FIND WET-BULB TEMPERATURE...                                      TPDD.19
C                                                                                TPDD.20
      ITCNT=0                                                                    TPDD.21
   90 ES=ALIQ*EXP((BLIQ*T1-CLIQ)/(T1-DLIQ))                                      TPDD.22
      RS=0.622*ES/(P-ES)                                                         TPDD.23
      PI=(1.E5/P)**(0.2854*(1.-0.28*RS))                                         TPDD.24
      THTGS=T1*PI*EXP((3374.6525/T1-2.5403)*RS*(1.+0.81*RS))                     TPDD.25
      F1=THTGS-THTED                                                             TPDD.26
      IF(ABS(F1).LT.0.05)GOTO 50                                                 TPDD.27
      ITCNT=ITCNT+1                                                              TPDD.28
      IF(ITCNT.GT.10)GOTO 50                                                     TPDD.29
      DT=F1*(T1-T0)/(F1-F0)                                                      TPDD.30
      T0=T1                                                                      TPDD.31
      F0=F1                                                                      TPDD.32
      T1=T1-DT                                                                   TPDD.33
      GOTO 90                                                                    TPDD.34
   50 RL=XLV0-XLV1*T1                                                            TPDD.35
C                                                                                TPDD.36
C...IF RELATIVE HUMIDITY IS SPECIFIED TO BE LESS THAN 100%, ESTIMATE THE         TPDD.37
C   TEMPERATURE AND MIXING RATIO WHICH WILL YIELD THE APPROPRIATE VALUE.         TPDD.38
C                                                                                TPDD.39
      IF(RH.EQ.1.)GOTO 110                                                       TPDD.40
      DSSDT=(CLIQ-BLIQ*DLIQ)/((T1-DLIQ)*(T1-DLIQ))                               TPDD.41
      DT=RL*RS*(1.-RH)/(CP+RL*RH*RS*DSSDT)                                       TPDD.42
      T1RH=T1+DT                                                                 TPDD.43
      ES=RH*ALIQ*EXP((BLIQ*T1RH-CLIQ)/(T1RH-DLIQ))                               TPDD.44
      RSRH=0.622*ES/(P-ES)                                                       TPDD.45
C                                                                                TPDD.46
C...CHECK TO SEE IF MIXING RATIO AT SPECIFIED RH IS LESS THAN ACTUAL             TPDD.47
C...MIXING RATIO...IF SO, ADJUST TO GIVE ZERO EVAPORATION...                     TPDD.48
C                                                                                TPDD.49
      IF(RSRH.LT.RD)THEN                                                         TPDD.50
        RSRH=RD                                                                  TPDD.51
        T1RH=T1+(RS-RSRH)*RL/CP                                                  TPDD.52
      ENDIF                                                                      TPDD.53
      T1=T1RH                                                                    TPDD.54
      RS=RSRH                                                                    TPDD.55
  110 TPDD=T1                                                                    TPDD.56
      IF(ITCNT.GT.10)PRINT*,'***** NUMBER OF ITERATIONS IN TPDD = ',             TPDD.57
     +  ITCNT                                                                    TPDD.58
      RETURN                                                                     TPDD.59
      END                                                                        TPDD.60
                                                                                 TPDD.61
