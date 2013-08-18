        SUBROUTINE XYOBSLL(YOBS,XOBS,HT,NOBS,IX,JX,DS,                           XYOBSLL.1
     1                     ICN,JCN,IRATIO,INEST,ICROSS)                          XYOBSLL.2
C                                                                                XYOBSLL.3
C   PURPOSE :   CALCULATES THE INDICES (I,J) ON THE MESOSCALE GRID               XYOBSLL.4
C               FROM THE LATITUDES AND LONGITUDES (SEE SECTION 3.1).             XYOBSLL.5
C   ON ENTRY :  YOBS(NOBS): LATITUDES.                                           XYOBSLL.6
C               XOBS(NOBS): LONGITUDES.                                          XYOBSLL.7
C                 HT(NOBS): TERRAIN HEIGHT IN METERS.                            XYOBSLL.8
C                     NOBS: THE NUMBER OF THE DATA POINTS.                       XYOBSLL.9
C                    IX,JX: THE DIMENSIONS OF COARSE DOMAIN; THEY ARE            XYOBSLL.10
C                           THE EXPANDED GRID DIMENSIONS IF                      XYOBSLL.11
C                           IEXP = .TRUE..                                       XYOBSLL.12
C                       DS: THE COARSE DOMAIN GRID DISTANCE (KM).                XYOBSLL.13
C                  ICN,JCN: THE LOCATION OF THE LOWER LEFT CORNER OF             XYOBSLL.14
C                           THE NESTED DOMAIN IN THE COARSE DOMAIN.              XYOBSLL.15
C                           THEY ARE REAL NUMBERS.                               XYOBSLL.16
C                   IRATIO: THE RATIO OF THE COARSE GRID SIZE TO THE             XYOBSLL.17
C                           NEST GRID SIZE.                                      XYOBSLL.18
C                    INEST: EQUAL TO DOMAIN ID NUMBER - 1. FOR COARSE            XYOBSLL.19
C                           DOMAIN INEST=0, AND FOR NESTED DOMAIN                XYOBSLL.20
C                           INEST $>$ 0.                                         XYOBSLL.21
C                   ICROSS: =0 FOR DOT POINT FIELD, =1 FOR CROSS POINT           XYOBSLL.22
C                           FILED.                                               XYOBSLL.23
C   ON EXIT :   YOBS(NOBS): THE I INDICES IN Y-DIRECTION.                        XYOBSLL.24
C               XOBS(NOBS): THE J INDICES IN X-DIRECTION.                        XYOBSLL.25
C                 HT(NOBS): TERRAIN HEIGHTS IN 100 METERS.                       XYOBSLL.26
C   CALLS :       THE SUBROUTINE RFLP AND THE TRINSIC FUNCTION SIN               XYOBSLL.27
C                 AND COS.                                                       XYOBSLL.28
C   CALLED BY : SUBROUTINE TERDRV.                                               XYOBSLL.29
C   COMMENTS :                                                                   XYOBSLL.30
C  IN ADDITION TO THE LIST OF ARGUMENTS, SUBROUTINE XYOBS GETS THE               XYOBSLL.31
C  INFORMATION FROM THE COMMON BLOCKS /MAPS/ AND /OPTION/. NOTE THAT             XYOBSLL.32
C  THE ICN AND JCN ARE THE LOCATION OF NESTED GRID OINT (1,1) IN THE             XYOBSLL.33
C  COARSE DOMAIN, NOT IN THE MOTHER DOMAIN, SO THEY MAY BE REAL                  XYOBSLL.34
C  NUMBERS.                                                                      XYOBSLL.35
C                                                                                XYOBSLL.36
# include <maps.incl>                                                            XYOBSLL.37
# include <option.incl>                                                          XYOBSLL.38
                                                                                 XYOBSLL.39
      DIMENSION YOBS(NOBS),XOBS(NOBS),HT(NOBS)                                   XYOBSLL.40
      REAL ICN,JCN                                                               XYOBSLL.41
                                                                                 XYOBSLL.42
      IF (INEST.EQ.0 .AND. IPRINT.EQ.1) PRINT 200, IX,JX,DS                      06NOV00.804
200   FORMAT('**** COARSE MESH COORDINATES (X,Y): IX=',I3,                       XYOBSLL.44
     1       2X,'JX=',I3,2X,'DS=',E12.3)                                         XYOBSLL.45
                                                                                 XYOBSLL.46
      IF (INEST.GT.0 .AND. IPRINT.EQ.1)                                          06NOV00.805
     1    PRINT 201, IX,JX,DS,ICN,JCN,IRATIO                                     06NOV00.806
201   FORMAT('* COARSE MESH COORDINATES (X,Y): IX=',I3,2X,'JX=',                 XYOBSLL.48
     1  I3,2X,'DS=',E12.3/2X,'THE LOW-LEFT CORNER OF NEST DOMAIN:',              XYOBSLL.49
     2  ' ICN=',F7.2,2X,'JCN=',F7.2,2X,'IRATIO=',I3)                             XYOBSLL.50
                                                                                 XYOBSLL.51
      CNTRJ = CNTRJ0                                                             XYOBSLL.52
      CNTRI = CNTRI0                                                             XYOBSLL.53
C                                                                                XYOBSLL.54
      IF (IPROJ.EQ.NPROJ(3)) THEN                                                XYOBSLL.55
        XCORD = (CNTRJ-1)*DS                                                     XYOBSLL.56
        YCORD = YCNTR - (CNTRI-1.)*DS                                            XYOBSLL.57
      ELSE                                                                       XYOBSLL.58
        XCORD = (CNTRJ-1.)*DS -XCNTR                                             XYOBSLL.59
        YCORD = (CNTRI-1.)*DS -YCNTR                                             XYOBSLL.60
      ENDIF                                                                      XYOBSLL.61
C                                                                                XYOBSLL.62
        IF(IPRINT.EQ.1) PRINT 5,XCORD/DS,YCORD/DS,IPROJ                          XYOBSLL.63
  5     FORMAT(2X,'***X,Y COORDINATES OF THE CENTER:',2F10.2,2X,A6//)            XYOBSLL.64
C                                                                                XYOBSLL.65
C    CONVERT THE LAT. AND LON. TO THE X, Y COORDINATES:                          XYOBSLL.66
C                                                                                XYOBSLL.67
      DO 10 II = 1,NOBS                                                          XYOBSLL.68
C                                                                                XYOBSLL.69
        YLAT = YOBS(II)                                                          XYOBSLL.70
        YLON = XOBS(II)                                                          XYOBSLL.71
C                                                                                XYOBSLL.72
        CALL RFLP(YLAT,YLON,R,FLP)                                               XYOBSLL.73
C                                                                                XYOBSLL.74
      IF (IPROJ.EQ.NPROJ(3)) GOTO 82                                             XYOBSLL.75
      IF (PHIC.LT.0.0) THEN                                                      XYOBSLL.76
         XOBSD = (R*SIN(FLP) + XCORD)                                            XYOBSLL.77
         YOBSD = (R*COS(FLP) + YCORD)                                            XYOBSLL.78
      ELSE                                                                       XYOBSLL.79
         XOBSD = (- R*SIN(FLP) + XCORD)                                          XYOBSLL.80
         YOBSD = (R*COS(FLP) + YCORD)                                            XYOBSLL.81
      END IF                                                                     XYOBSLL.82
      GO TO 85                                                                   XYOBSLL.83
C                                                                                XYOBSLL.84
C  FOR MERCATOR PROJECTION TRUE AT PHI1                                          XYOBSLL.85
C                                                                                XYOBSLL.86
   82 CONTINUE                                                                   XYOBSLL.87
      IF(IPRINT.EQ.1) PRINT 83, FLP,R,XCORD,YCORD                                XYOBSLL.88
83    FORMAT(2X,'R=',F12.2,2X,'FLP=',F12.2,2X,'XCORD=',F12.2,                    XYOBSLL.89
     1                                        2X,',YCORD='F12.2)                 XYOBSLL.90
      XOBSD=(FLP+XCORD)                                                          XYOBSLL.91
      YOBSD=(R-YCORD)                                                            XYOBSLL.92
C                                                                                XYOBSLL.93
   85 CONTINUE                                                                   XYOBSLL.94
      XOBSC = XOBSD - .5*DS                                                      XYOBSLL.95
      YOBSC = YOBSD - .5*DS                                                      XYOBSLL.96
C        CONVERT THE X,Y LOCATIONS  TO FLOATING POINT GRID INDEX VALUES.         XYOBSLL.97
      XJD = XOBSD/DS + 1.                                                        XYOBSLL.98
      YID = YOBSD/DS + 1.                                                        XYOBSLL.99
      XJCS = XOBSC/DS + 1.                                                       XYOBSLL.100
      YICS = YOBSC/DS + 1.                                                       XYOBSLL.101
C                                                                                XYOBSLL.102
      IF (INEST.GT.0) THEN                                                       XYOBSLL.103
C                                                                                XYOBSLL.104
        YID = (YID-ICN)*FLOAT(IRATIO) + 1.0                                      XYOBSLL.105
        XJD = (XJD-JCN)*FLOAT(IRATIO) + 1.0                                      XYOBSLL.106
        YICS = YID - 0.5                                                         XYOBSLL.107
        XJCS = XJD - 0.5                                                         XYOBSLL.108
      END IF                                                                     XYOBSLL.109
C                                                                                XYOBSLL.110
      IF(ICROSS.EQ.1) THEN                                                       XYOBSLL.111
        YOBS(II) = YICS                                                          XYOBSLL.112
        XOBS(II) = XJCS                                                          XYOBSLL.113
      ELSE                                                                       XYOBSLL.114
        YOBS(II) = YID                                                           XYOBSLL.115
        XOBS(II) = XJD                                                           XYOBSLL.116
      ENDIF                                                                      XYOBSLL.117
C                                                                                XYOBSLL.118
      HT(II) = HT(II)/100.                                                       XYOBSLL.119
10    CONTINUE                                                                   XYOBSLL.120
C                                                                                XYOBSLL.121
      RETURN                                                                     XYOBSLL.122
      END                                                                        XYOBSLL.123
