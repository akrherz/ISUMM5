      SUBROUTINE ANAL2(A2,ASTA,XOBS,YOBS,IMAX,JMAX,NSTA,N,NI)                    ANAL2.1
C                                                                                ANAL2.2
C ANAL2(A2, ASTA, XOBS, YOBS, IMAX, JMAX, NSTA, N, NI)                           ANAL2.3
C                                                                                ANAL2.4
C   PURPOSE : CREATE THE TERRAIN HEIGHT FIELDS ON MESOSCALE GRIDS BY             ANAL2.5
C             USING THE CRESSMAN-TYPE OBJECTIVE ANALYSIS TECHNIQUE.              ANAL2.6
C                                                                                ANAL2.7
C   ASTA(NSTA): TERRAIN HEIGHTS FROM THE SOURCE DATA.                            ANAL2.8
C   XOBS(NSTA): J INDICES OF THE SOURCE DATA.                                    ANAL2.9
C   YOBS(NSTA): I INDICES OF THE SOURCE DATA.                                    ANAL2.10
C   NSTA      : NUMBER OF DATA POINTS.                                           ANAL2.11
C   IMAX,JMAX : THE DIMENSION OF THE MESOSCALE DOMAIN.                           ANAL2.12
C   N         : THE ID NUMBER OF THE MESOSCALE DOMAIN.                           ANAL2.13
C   NI        : THE ID NUMBER OF THE SOURCE DATA FILE.                           ANAL2.14
C                        NI = 1:  1 DEGREE RESOLUTION DATA                       ANAL2.15
C                        NI = 2:  30 MINUTES RESOLUTION DATA                     ANAL2.16
C                        NI = 3:  10 MINUTES RESOLUTION DATA                     ANAL2.17
C                        NI = 4:   5 MINUTES RESOLUTION DATA                     ANAL2.18
C                        NI = 5:  30 SECONDS RESOLUTION DATA                     ANAL2.19
C   A2(IMAX,JMAX): THE TERRAIN HEIGHT FIELD ON THE MESOSCALE DOMAIN N.           ANAL2.20
C                                                                                ANAL2.21
C   SUBROUTINE ANAL2 CALLS NO OTHER SUBROUTINES.                                 ANAL2.22
C   IT IS CALLED BY SUBROUTINE CRTER.                                            ANAL2.23
C                                                                                ANAL2.24
C COMMENTS :                                                                     ANAL2.25
C    NO FIRST-GUESS FIELDS ARE USED, AND A SINGLE SCAN ONLY IS                   ANAL2.26
C PERFORMED IN THE CRESSMAN-TYPE OBJECTIVE ANALYSIS. THE RADIUS OF               ANAL2.27
C INFLUENCE RID IS DEFINED THROUGH THE COMMON BLOCK /NESTDMN/.                   ANAL2.28
C BEFORE CALLING ANAL2, SUBROUTINE XYOBSLL MUST BE CALLED FIRST TO               ANAL2.29
C CONVERT THE LATITUDES AND LONGITUDES OF THE SOURCE DATA POINTS TO              ANAL2.30
C THE MESOSCALE INDICES XOBS AND YOBS.                                           ANAL2.31
# include <parame.incl>                                                          ANAL2.32
# include <paramed.incl>                                                         06NOV00.1
# include <io.incl>                                                              06NOV00.2
# include <nestdmn.incl>                                                         ANAL2.33
C                                                                                ANAL2.34
      DIMENSION A2(IMAX,JMAX),XOBS(NSTA),YOBS(NSTA),ASTA(NSTA)                   ANAL2.35
      DIMENSION COR  (IIMX,JJMX), SUM (IIMX,JJMX), NS  (IIMX,JJMX),              ANAL2.36
     -          WTMAX(IIMX,JJMX),HTSAV(IIMX,JJMX)                                ANAL2.37
C                                                                                ANAL2.38
      IF (IMAX.GT.IIMX) THEN                                                     ANAL2.39
        PRINT 2,IMAX,IIMX                                                        ANAL2.40
2       FORMAT(2X,'**> IN ANAL2, IMAX=',I3,' > IIMX=',I3,                        ANAL2.41
     -         ', INCREASE IIMX IN PARAMSZ.INCL, RE-RUN TERRAIN.')               ANAL2.42
        STOP                                                                     ANAL2.43
      ENDIF                                                                      ANAL2.44
      IF (JMAX.GT.JJMX) THEN                                                     ANAL2.45
        PRINT 3,JMAX,JJMX                                                        ANAL2.46
3       FORMAT(2X,'**> IN ANAL2, JMAX=',I3,' > JJMX=',I3,                        ANAL2.47
     -         ', INCREASE JJMX IN PARAMSZ.INCL, RE-RUN TERRAIN.')               ANAL2.48
        STOP                                                                     ANAL2.49
      ENDIF                                                                      ANAL2.50
C                                                                                ANAL2.51
C     OBJECTIVE ANALYSIS TO FILL A GRID BASED ON OBSERVATIONS                    ANAL2.52
C     XOBS AND YOBS ARE X AND Y POSITIONS ON OBSERVATIONS, NOT                   ANAL2.53
C     NECESSARILY GRID POINTS.                                                   ANAL2.54
C                                                                                ANAL2.55
      IE     = IMAX-1                                                            ANAL2.56
      JE     = JMAX-1                                                            ANAL2.57
      NSCAN  = 1                                                                 ANAL2.58
      DELTAS = DIS(N)                                                            ANAL2.59
      RIN = RID(N)                                                               ANAL2.60
      DELTAS = DELTAS*1000.                                                      ANAL2.61
   10 FORMAT(1X,'NEST ',I2,2X,'NOBS=',L8,3X,'RIN,DS(M) =',2E12.3)                ANAL2.62
C                                                                                ANAL2.63
C-----GRID LENGTHS IN X AND Y DIRECTIONS ARE UNITY.                              ANAL2.64
C                                                                                ANAL2.65
      RIS   = RIN**2                                                             ANAL2.66
C-----RIN IS RADIUS OF INFLUENCE IN GRID UNITS                                   ANAL2.67
C                                                                                ANAL2.68
      DO 30 I = 1,IMAX                                                           ANAL2.69
      DO 30 J = 1,JMAX                                                           ANAL2.70
      COR(I,J) = 0.0                                                             ANAL2.71
      SUM(I,J) = 0.0                                                             ANAL2.72
      NS(I,J)  = 0                                                               ANAL2.73
      WTMAX(I,J) = 0.0                                                           ANAL2.74
      HTSAV(I,J) = 0.0                                                           ANAL2.75
   30 CONTINUE                                                                   ANAL2.76
C                                                                                ANAL2.77
C-----BEGIN TO PROCESS THE NSTA OBSERVATIONS:                                    ANAL2.78
C                                                                                ANAL2.79
      DO 80 KK = 1,NSTA                                                          ANAL2.80
      IF (ASTA(KK) .GT. 400.) GO TO 80                                           ANAL2.81
C                                                                                ANAL2.82
C  MISSING VALUE = -4000/3.2804 M FOR 1/120 DEG. TERRAIN DATA:                   ANAL2.83
      IF (NI.EQ.max_type .AND. ASTA(KK) .EQ. -99.99) GO TO 80                    06NOV00.3
C                                                                                ANAL2.85
C-----DEFINE MAX AND MIN I AND J VALUES TO LIMIT THE NUMBER OF POINTS            ANAL2.86
C-----MUST BE CONSIDERED.                                                        ANAL2.87
C                                                                                ANAL2.88
      RIOBS = YOBS(KK)                                                           ANAL2.89
      RJOBS = XOBS(KK)                                                           ANAL2.90
C                                                                                ANAL2.91
      IF(RJOBS.GT.JMAX+RIN .OR. RJOBS.LT.-RIN .OR.                               ANAL2.92
     1   RIOBS.GT.IMAX+RIN .OR. RIOBS.LT.-RIN) GO TO 80                          ANAL2.93
C                                                                                ANAL2.94
      YMAXI = RIOBS + RIN                                                        ANAL2.95
      MAXI  = IFIX(YMAXI + 0.99)                                                 ANAL2.96
      MAXI  = MIN0(MAXI,IE)                                                      ANAL2.97
C                                                                                ANAL2.98
      YMINI = RIOBS - RIN                                                        ANAL2.99
      MINI  = IFIX(YMINI)                                                        ANAL2.100
      MINI  = MAX0(MINI,1)                                                       ANAL2.101
C                                                                                ANAL2.102
      XMAXJ = RJOBS + RIN                                                        ANAL2.103
      MAXJ  = IFIX(XMAXJ + 0.99)                                                 ANAL2.104
      MAXJ  = MIN0(MAXJ,JE)                                                      ANAL2.105
C                                                                                ANAL2.106
      XMINJ = RJOBS - RIN                                                        ANAL2.107
      MINJ  = IFIX(XMINJ)                                                        ANAL2.108
      MINJ  = MAX0(MINJ,1)                                                       ANAL2.109
C                                                                                ANAL2.110
      DO 70 I=MINI,MAXI                                                          ANAL2.111
      DO 70 J=MINJ,MAXJ                                                          ANAL2.112
C                                                                                ANAL2.113
      RX = FLOAT(J) - RJOBS                                                      ANAL2.114
      RY = FLOAT(I) - RIOBS                                                      ANAL2.115
      RSQ = RX**2+RY**2                                                          ANAL2.116
      IF (RSQ.GE.RIS) GOTO 70                                                    ANAL2.117
C                                                                                ANAL2.118
      WT = (RIS - RSQ)/(RIS + RSQ)                                               ANAL2.119
C                                                                                ANAL2.120
C-----SAVE MAX. WEIGHTING FACTOR AND TERRAIN HEIGHT TO CHECK IF GRID             ANAL2.121
C-----POINT SHOULD BE TREATED AS A LAND OR SEA POINT.                            ANAL2.122
C                                                                                ANAL2.123
      IF (WT.GT.0.0) THEN                                                        ANAL2.124
         WTMAX(I,J) = AMAX1(WT,WTMAX(I,J))                                       ANAL2.125
C        HTSAV(I,J) = CVMGN(HTSAV(I,J),ASTA(KK),(WT - WTMAX(I,J)))               ANAL2.126
         IF ((WT - WTMAX(I,J)).EQ.0.0) HTSAV(I,J) = ASTA(KK)                     ANAL2.127
         COR(I,J)   = COR(I,J) + WT*ASTA(KK)                                     ANAL2.128
         SUM(I,J)   = SUM(I,J) + WT                                              ANAL2.129
         NS(I,J)    = NS(I,J) + 1                                                ANAL2.130
      ENDIF                                                                      ANAL2.131
  70  CONTINUE                                                                   ANAL2.132
  80  CONTINUE                                                                   ANAL2.133
C                                                                                ANAL2.134
C-----NOW APPLY SUMMED WEIGHTS AND WEIGHTED OBSERVATIONS TO DETERMINE            ANAL2.135
C-----TERRAIN VALUE AT I,J POINTS                                                ANAL2.136
C                                                                                ANAL2.137
      DO 90 I = 1,IE                                                             ANAL2.138
      DO 90 J = 1,JE                                                             ANAL2.139
      IF (NS(I,J) .NE. 0) THEN                                                   ANAL2.140
         COR(I,J) = COR(I,J)/SUM(I,J)                                            ANAL2.141
         A2(I,J)  = COR(I,J)                                                     ANAL2.142
      ELSE IF (NI.EQ.max_type) THEN                                              06NOV00.4
C                                                                                ANAL2.144
C  FOR 1/120 DEG. DATA, ASSUMING NO OBS OVER WATER:                              ANAL2.145
        A2(I,J) = -0.00001                                                       ANAL2.146
      ELSE                                                                       ANAL2.147
        PRINT 26,RIN,I,J                                                         ANAL2.148
        STOP 26                                                                  ANAL2.149
      ENDIF                                                                      ANAL2.150
   90 CONTINUE                                                                   ANAL2.151
C                                                                                ANAL2.152
   26 FORMAT(' NO OBSERVATIONS ARE WITHIN RIN=',F7.2,                            ANAL2.153
     1 ' GRID LENGTHS OF I=',I3,' J=',I3)                                        ANAL2.154
C-----MAY WANT TO SMOOTH FINAL FIELD A2 HERE                                     ANAL2.155
      RETURN                                                                     ANAL2.156
      END                                                                        ANAL2.157
