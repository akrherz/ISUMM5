       SUBROUTINE NOPRO(MDATES,IDDATE,IDCHK,MCHA,MVAR,IDHK)                      NOPRO.1
C THIS SUBROUTINE SETS THE DECISION CHECK FOR NO-PROCESSING                      NOPRO.2
C BASED ON IDDATE AND IDCHK SET IN PARAM                                         NOPRO.3
C FOR NOW, 0 OR 12H DATA CANNOT BE FLAGGED FOR NO=PROCESSING                     NOPRO.4
      DIMENSION IDDATE(MCHA),IDCHK(MCHA,MVAR),IDHK(MVAR)                         NOPRO.5
      IFOUND=-999                                                                NOPRO.6
      DO 10 NOC=1,MCHA                                                           NOPRO.7
        IF(MDATES.EQ.IDDATE(NOC))THEN                                            NOPRO.8
          IFOUND=NOC                                                             NOPRO.9
          GOTO 15                                                                NOPRO.10
        ENDIF                                                                    NOPRO.11
   10 CONTINUE                                                                   NOPRO.12
   15 CONTINUE                                                                   NOPRO.13
      IF(IFOUND.LT.0)RETURN                                                      NOPRO.14
C CHECK SO THAT HOUR IS NOT 00 OR 12                                             NOPRO.15
      IYR=INT(MDATES/1000000.+.001)                                              NOPRO.16
      IDATE1=MDATES-IYR*1000000                                                  NOPRO.17
      IMO=INT(IDATE1/10000.+.001)                                                NOPRO.18
      IDY=INT((IDATE1-IMO*10000.)/100.+.001)                                     NOPRO.19
      IHR=IDATE1-IMO*10000-IDY*100                                               NOPRO.20
      IF(IHR.EQ.0.OR.IHR.EQ.12)THEN                                              NOPRO.21
        PRINT20,IHR                                                              NOPRO.22
20    FORMAT(1X,'STOPPING EXECUTION IN NOPRO: IHR = ',I2)                        NOPRO.23
        STOP 20                                                                  NOPRO.24
      ENDIF                                                                      NOPRO.25
C DATE IS FLAGGED FOR NO-PROCESSING                                              NOPRO.26
C SET IDHK(IVAR) ACCORDING TO IDCHK...                                           NOPRO.27
      DO 30 NV=1,MVAR                                                            NOPRO.28
        IDHK(NV)=IDCHK(IFOUND,NV)                                                NOPRO.29
   30 CONTINUE                                                                   NOPRO.30
      PRINT40,MDATES                                                             NOPRO.31
40    FORMAT(1X,'++++CALL NOPRO FOR MDATES = ',I8)                               NOPRO.32
      PRINT41,(IDHK(NN),NN=1,7)                                                  NOPRO.33
41    FORMAT(1X,'++++NOPRO SURFACE DATA INVENTORY: U=',I2,                       NOPRO.34
     1' V=',I2,' TEMP=',I2,'MIXR=',I2,' PSB=',I2,' RH=',I2,' SLP=',I2)           NOPRO.35
      RETURN                                                                     NOPRO.36
      END                                                                        NOPRO.37
                                                                                 NOPRO.38
