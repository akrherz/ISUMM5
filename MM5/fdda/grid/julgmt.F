C                                                                                JULGMT.1
C                                                                                JULGMT.2
      SUBROUTINE JULGMT(MDATE,JULGMTN,TIMANL,JULDAY,GMT,IND)                     JULGMT.3
C CONVERT MDATE YYMMDDHH TO JULGMT (JULIAN DAY * 100. +GMT)                      JULGMT.4
C AND TO TIMANL (TIME IN MINUTES WITH RESPECT TO MODEL TIME)                     JULGMT.5
C IF IND=0  INPUT MDATE, OUTPUT JULGMTN AND TIMANL                               JULGMT.6
C IF IND=1  INPUT TIMANL, OUTPUT JULGMTN                                         JULGMT.7
C IF IND=2  INPUT JULGMTN, OUTPUT TIMANL                                         JULGMT.8
      REAL JULGMTN                                                               JULGMT.9
      DIMENSION MO(12)                                                           JULGMT.10
      IF(IND.EQ.2)GOTO 150                                                       JULGMT.11
      IYR=INT(MDATE/1000000.+0.001)                                              JULGMT.12
      IDATE1=MDATE-IYR*1000000                                                   JULGMT.13
      IMO=INT(IDATE1/10000.+0.001)                                               JULGMT.14
      IDY=INT((IDATE1-IMO*10000.)/100.+0.001)                                    JULGMT.15
      IHR=IDATE1-IMO*10000-IDY*100                                               JULGMT.16
      MO(1)=31                                                                   JULGMT.17
      MO(2)=28                                                                   JULGMT.18
C IS THE YEAR A LEAP YEAR? (IN THIS CENTURY)                                     JULGMT.19
      IYR=IYR+1900                                                               JULGMT.20
      MY1=MOD(IYR,4)                                                             JULGMT.21
      MY2=MOD(IYR,100)                                                           JULGMT.22
      MY3=MOD(IYR,400)                                                           JULGMT.23
      ILEAP=0                                                                    JULGMT.24
      IF(MY1.EQ.0.AND.MY2.NE.0.OR.MY3.EQ.0)THEN                                  JULGMT.25
        ILEAP=1                                                                  JULGMT.26
        MO(2)=29                                                                 JULGMT.27
      ENDIF                                                                      JULGMT.28
      IF(IND.EQ.1)GOTO 200                                                       JULGMT.29
      MO(3)=31                                                                   JULGMT.30
      MO(4)=30                                                                   JULGMT.31
      MO(5)=31                                                                   JULGMT.32
      MO(6)=30                                                                   JULGMT.33
      MO(7)=31                                                                   JULGMT.34
      MO(8)=31                                                                   JULGMT.35
      MO(9)=30                                                                   JULGMT.36
      MO(10)=31                                                                  JULGMT.37
      MO(11)=30                                                                  JULGMT.38
      MO(12)=31                                                                  JULGMT.39
      JULDAYN=0                                                                  JULGMT.40
      DO 100 MM=1,IMO-1                                                          JULGMT.41
        JULDAYN=JULDAYN+MO(MM)                                                   JULGMT.42
  100 CONTINUE                                                                   JULGMT.43
      IF(IHR.GE.24)THEN                                                          JULGMT.44
        IDY=IDY+1                                                                JULGMT.45
        IHR=IHR-24                                                               JULGMT.46
      ENDIF                                                                      JULGMT.47
      JULGMTN=(JULDAYN+IDY)*100.+IHR                                             JULGMT.48
C CONVERT JULGMT TO TIMANL WRT MODEL TIME IN MINUTES (XTIME)                     JULGMT.49
  150 CONTINUE                                                                   JULGMT.50
      JULDANL=INT(JULGMTN/100.+0.000001)                                         JULGMT.51
      RJULANL=FLOAT(JULDANL)*100.                                                JULGMT.52
      HOURANL=JULGMTN-RJULANL                                                    JULGMT.53
      TIMANL=(FLOAT(JULDANL-JULDAY)*24.-GMT+HOURANL)*60.                         JULGMT.54
      RETURN                                                                     JULGMT.55
  200 CONTINUE                                                                   JULGMT.56
      RHR=GMT+TIMANL/60.+0.000001                                                JULGMT.57
      IDY=JULDAY                                                                 JULGMT.58
      IDYMAX=365+ILEAP                                                           JULGMT.59
  300 IF(RHR.GE.24.0)THEN                                                        JULGMT.60
        RHR=RHR-24.0                                                             JULGMT.61
        IDY=IDY+1                                                                JULGMT.62
        GOTO 300                                                                 JULGMT.63
      ENDIF                                                                      JULGMT.64
      IF(IDY.GT.IDYMAX)IDY=IDY-IDYMAX                                            JULGMT.65
      JULGMTN=FLOAT(IDY)*100.+RHR                                                JULGMT.66
      RETURN                                                                     JULGMT.67
      END                                                                        JULGMT.68
                                                                                 JULGMT.69
