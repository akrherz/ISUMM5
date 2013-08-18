      SUBROUTINE CONDLOAD(QLIQ,QICE,WTW,DZ,BOTERM,ENTERM,RATE,QNEWLQ,            CONDLOAD.1
     *                  QNEWIC,QLQOUT,QICOUT)                                    CONDLOAD.2
      IMPLICIT NONE                                                              07NOV00.1379
C  9/18/88...THIS PRECIPITATION FALLOUT SCHEME IS BASED ON THE SCHEME US         CONDLOAD.3
C  BY OGURA AND CHO (1973).  LIQUID WATER FALLOUT FROM A PARCEL IS CAL-          CONDLOAD.4
C  CULATED USING THE EQUATION DQ=-RATE*Q*DT, BUT TO SIMULATE A QUASI-            CONDLOAD.5
C  CONTINUOUS PROCESS, AND TO ELIMINATE A DEPENDENCY ON VERTICAL                 CONDLOAD.6
C  RESOLUTION THIS IS EXPRESSED AS Q=Q*EXP(-RATE*DZ).                            CONDLOAD.7
C DECLARATIONS FOR IMPLICIT NONE                                                 07NOV00.1380
      REAL QTOT,QLIQ,QICE,QNEW,QNEWLQ,QNEWIC,QEST,G1,WTW,BOTERM                  07NOV00.1381
      REAL ENTERM,G,DZ,WAVG,CONV,RATE,RATIO3,OLDQ,RATIO4,DQ,QLQOUT               07NOV00.1382
      REAL QICOUT,PPTDRG                                                         07NOV00.1383
      DATA G/9.81/                                                               CONDLOAD.8
      QTOT=QLIQ+QICE                                                             CONDLOAD.9
      QNEW=QNEWLQ+QNEWIC                                                         CONDLOAD.10
C                                                                                CONDLOAD.11
C  ESTIMATE THE VERTICAL VELOCITY SO THAT AN AVERAGE VERTICAL VELOCITY C         CONDLOAD.12
C  BE CALCULATED TO ESTIMATE THE TIME REQUIRED FOR ASCENT BETWEEN MODEL          CONDLOAD.13
C  LEVELS...                                                                     CONDLOAD.14
C                                                                                CONDLOAD.15
      QEST=0.5*(QTOT+QNEW)                                                       CONDLOAD.16
      G1=WTW+BOTERM-ENTERM-2.*G*DZ*QEST/1.5                                      CONDLOAD.17
      IF(G1.LT.0.0)G1=0.                                                         CONDLOAD.18
      WAVG=(SQRT(WTW)+SQRT(G1))/2.                                               CONDLOAD.19
      CONV=RATE*DZ/WAVG                                                          CONDLOAD.20
C                                                                                CONDLOAD.21
C  RATIO3 IS THE FRACTION OF LIQUID WATER IN FRESH CONDENSATE, RATIO4 IS         CONDLOAD.22
C  THE FRACTION OF LIQUID WATER IN THE TOTAL AMOUNT OF CONDENSATE INVOLV         CONDLOAD.23
C  IN THE PRECIPITATION PROCESS - NOTE THAT ONLY 60% OF THE FRESH CONDEN         CONDLOAD.24
C  SATE IS IS ALLOWED TO PARTICIPATE IN THE CONVERSION PROCESS...                CONDLOAD.25
C                                                                                CONDLOAD.26
      RATIO3=QNEWLQ/(QNEW+1.E-10)                                                CONDLOAD.27
C     OLDQ=QTOT                                                                  CONDLOAD.28
      QTOT=QTOT+0.6*QNEW                                                         CONDLOAD.29
      OLDQ=QTOT                                                                  CONDLOAD.30
      RATIO4=(0.6*QNEWLQ+QLIQ)/(QTOT+1.E-10)                                     CONDLOAD.31
      QTOT=QTOT*EXP(-CONV)                                                       CONDLOAD.32
C                                                                                CONDLOAD.33
C  DETERMINE THE AMOUNT OF PRECIPITATION THAT FALLS OUT OF THE UPDRAFT           CONDLOAD.34
C  PARCEL AT THIS LEVEL...                                                       CONDLOAD.35
C                                                                                CONDLOAD.36
      DQ=OLDQ-QTOT                                                               CONDLOAD.37
      QLQOUT=RATIO4*DQ                                                           CONDLOAD.38
      QICOUT=(1.-RATIO4)*DQ                                                      CONDLOAD.39
C                                                                                CONDLOAD.40
C  ESTIMATE THE MEAN LOAD OF CONDENSATE ON THE UPDRAFT IN THE LAYER, CAL         CONDLOAD.41
C  LATE VERTICAL VELOCITY                                                        CONDLOAD.42
C                                                                                CONDLOAD.43
      PPTDRG=0.5*(OLDQ+QTOT-0.2*QNEW)                                            CONDLOAD.44
      WTW=WTW+BOTERM-ENTERM-2.*G*DZ*PPTDRG/1.5                                   CONDLOAD.45
C                                                                                CONDLOAD.46
C  DETERMINE THE NEW LIQUID WATER AND ICE CONCENTRATIONS INCLUDING LOSSE         CONDLOAD.47
C  DUE TO PRECIPITATION AND GAINS FROM CONDENSATION...                           CONDLOAD.48
C                                                                                CONDLOAD.49
      QLIQ=RATIO4*QTOT+RATIO3*0.4*QNEW                                           CONDLOAD.50
      QICE=(1.-RATIO4)*QTOT+(1.-RATIO3)*0.4*QNEW                                 CONDLOAD.51
      QNEWLQ=0.                                                                  CONDLOAD.52
      QNEWIC=0.                                                                  CONDLOAD.53
      RETURN                                                                     CONDLOAD.54
      END                                                                        CONDLOAD.55
                                                                                 CONDLOAD.56
