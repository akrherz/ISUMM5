      SUBROUTINE NESTLL(IYY,JXX,YLAT,YLON,PHIC,                                  NESTLL.1
     1             XMAXLAT,XMINLAT,XMAXLON,XMINLON,XEXTND,IPOLE,INEST)           NESTLL.2
C                                                                                NESTLL.3
C   PURPOSE        : FINDS THE MAXIMUM AND MINIMUM LATITUDE AND                  NESTLL.4
C                    LONGITUDE OF THE SEARCH AREA FOR THE NEST                   NESTLL.5
C                    DOMAINS WHEN THE POLE IS INSIDE THE COARSE D                NESTLL.6
C                    OMAIN FOR POLAR STEREOGRAPHIC PROJECTION.                   NESTLL.7
C   ON ENTRY       :                                                             NESTLL.8
C   IYY,JXX        : THE DIMENSIONS OF THE DOMAIN INEST+1.                       NESTLL.9
C   YLAT(IYY,JXX)  : LATITUDES OF THE MESOSCALE GRIDS.                           NESTLL.10
C   YLON(IYY,JXX)  : LONGITUDES OF THE MESOSCALE GRIDS.                          NESTLL.11
C   PHIC           : THE CENTRAL LATITUDE OF THE COARSE DOMAIN.                  NESTLL.12
C   XEXTND         : THE INCREMENT TO EXPAND THE SEARCH AREA.                    NESTLL.13
C   INEST          : THE DOMAIN ID NUMBER - 1. FOR THE COARSE DOMAIN,            NESTLL.14
C                    INEST=0. FOR THE NEST DOMAIN, INSET > 0.                    NESTLL.15
C   ON EXIT        :                                                             NESTLL.16
C   XMAXLAT        : THE MAXIMUM LATITUDE OF THE SEARCH AREA.                    NESTLL.17
C   XMINLAT        : THE MINIMUM LATITUDE OF THE SEARCH AREA.                    NESTLL.18
C   XMAXLON        : THE MAXIMUM LONGITUDE OF THE SEARCH AREA.                   NESTLL.19
C   XMINLON        : THE MINIMUM LONGITUDE OF THE SEARCH AREA.                   NESTLL.20
C   IPOLE(INEST+1) : IPOLE(INEST+1) = 1: POLE IS INSIDE THE DOMAIN,              NESTLL.21
C                    IPOLE(INEST+1) = 0 : POLE IS OUTSIDE THE DOMAIN.            NESTLL.22
C                                                                                NESTLL.23
C   CALLS NO OTHER SUBROUTINES.                                                  NESTLL.24
C   CALLED BY SUBROUTINE MXMNLL.                                                 NESTLL.25
C                                                                                NESTLL.26
C   COMMENTS :                                                                   NESTLL.27
C THIS SUBROUTINE IS ONLY CALLED WHEN THE NORTH OR SOUTH POLES ARE               NESTLL.28
C INSIDE THE COARSE DOMAIN.                                                      NESTLL.29
# include <option.incl>                                                          NESTLL.30
C                                                                                NESTLL.31
C                                                                                NESTLL.32
      DIMENSION YLAT(IYY,JXX),YLON(IYY,JXX),SS(2),IPOLE(20)                      NESTLL.33
C                                                                                NESTLL.34
      PRINT 5                                                                    NESTLL.35
5     FORMAT(/2X,' ++ CALL NESTLL +++++++++++++++')                              NESTLL.36
C                                                                                NESTLL.37
      K = 1                                                                      NESTLL.38
      SS(1) = 0.0                                                                NESTLL.39
      SS(2) = 0.0                                                                NESTLL.40
C                                                                                NESTLL.41
      DO 10 I = IYY,2,-1                                                         NESTLL.42
      IF ((YLON(I,1)*YLON(I-1,1)) .LT. 0.0) THEN                                 NESTLL.43
        SS(K) = ABS(YLON(I,1)-YLON(I-1,1))                                       NESTLL.44
        IF(IPRINT.EQ.1) THEN                                                     NESTLL.45
        PRINT 11,I,1,YLON(I,1),YLON(I-1,1),K,SS(K)                               NESTLL.46
11      FORMAT(1X,'I=',I3,2X,'J=',I3,2X,'LON:',2F12.3,'  K=',I2,                 NESTLL.47
     1         2X,'SS=',F10.3)                                                   NESTLL.48
        ENDIF                                                                    NESTLL.49
        K = 2                                                                    NESTLL.50
      ENDIF                                                                      NESTLL.51
10    CONTINUE                                                                   NESTLL.52
C                                                                                NESTLL.53
      DO 20 J = 1,JXX-1                                                          NESTLL.54
      IF ((YLON(1,J)*YLON(1,J+1)) .LT. 0.0) THEN                                 NESTLL.55
        SS(K) = ABS(YLON(1,J)-YLON(1,J+1))                                       NESTLL.56
        IF(IPRINT.EQ.1) PRINT 11,1,J,YLON(1,J),YLON(1,J+1),K,SS(K)               NESTLL.57
        IF(K.EQ.2) GO TO 45                                                      NESTLL.58
        K = 2                                                                    NESTLL.59
      ENDIF                                                                      NESTLL.60
20    CONTINUE                                                                   NESTLL.61
C                                                                                NESTLL.62
      DO 30 I = 1,IYY-1                                                          NESTLL.63
      IF ((YLON(I,JXX)*YLON(I+1,JXX)) .LT. 0.0) THEN                             NESTLL.64
        SS(K) = ABS(YLON(I,JXX)-YLON(I+1,JXX))                                   NESTLL.65
        PRINT 11,I,JXX,YLON(I,JXX),YLON(I+1,JXX),K,SS(K)                         NESTLL.66
        IF(K.EQ.2) GO TO 45                                                      NESTLL.67
        K = 2                                                                    NESTLL.68
      ENDIF                                                                      NESTLL.69
30    CONTINUE                                                                   NESTLL.70
C                                                                                NESTLL.71
      DO 40 J = JXX,2,-1                                                         NESTLL.72
      IF ((YLON(IYY,J)*YLON(IYY,J-1)) .LT. 0.0) THEN                             NESTLL.73
        SS(K) = ABS(YLON(IYY,J)-YLON(IYY,J-1))                                   NESTLL.74
        IF(IPRINT.EQ.1) PRINT 11,IYY,J,YLON(IYY,J),YLON(IYY,J-1),K,SS(K)         NESTLL.75
      ENDIF                                                                      NESTLL.76
40    CONTINUE                                                                   NESTLL.77
                                                                                 NESTLL.78
C  NINDEX = 0:                                                                   NESTLL.79
C  IT MEANS THE GENERAL SITUATION;                                               NESTLL.80
C                                                                                NESTLL.81
C  NINDEX = 1:                                                                   NESTLL.82
C  IF ONE OF THE SS VALUES ARE MORE THAN 200.0, IT MEANS THE DOMAIN              NESTLL.83
C  CONTAINS THE POLE;                                                            NESTLL.84
C                                                                                NESTLL.85
C  NINDEX = 2:                                                                   NESTLL.86
C  IF BOTH OF THE SS VALUES ARE MORE THAN 200.0, IT MEANS THE DOMAIN             NESTLL.87
C  ACROSS 180 DEG.                                                               NESTLL.88
                                                                                 NESTLL.89
45    CONTINUE                                                                   NESTLL.90
      NINDEX = 0                                                                 NESTLL.91
      DO 50 L = 1,2                                                              NESTLL.92
      IF (SS(L) .GT. 200.0) THEN                                                 NESTLL.93
      NINDEX = NINDEX + 1                                                        NESTLL.94
      ENDIF                                                                      NESTLL.95
50    CONTINUE                                                                   NESTLL.96
C                                                                                NESTLL.97
      IF (NINDEX.EQ.1) THEN                                                      NESTLL.98
        IF (PHIC .GE. 0.0) THEN                                                  NESTLL.99
        PRINT *,'+++ NESTED DOMAIN CONTAINS THE NORTH POLE +++'                  NESTLL.100
           XMAXLAT =  90.                                                        NESTLL.101
           XMAXLON = 180.                                                        NESTLL.102
           XMINLON =-180.                                                        NESTLL.103
           XMINLAT = AMIN1(YLAT(1,1),YLAT(1,JXX))                                NESTLL.104
           XMINLAT = AMIN1(XMINLAT,YLAT(IYY,JXX))                                NESTLL.105
           XMINLAT = AMIN1(XMINLAT,YLAT(IYY,1))                                  NESTLL.106
           XMINLAT = XMINLAT - XEXTND                                            NESTLL.107
           IPOLE(INEST+1) = 1                                                    NESTLL.108
        ELSE                                                                     NESTLL.109
        PRINT *,'+++ NESTED DOMAIN CONTAINS THE SOUTH POLE +++'                  NESTLL.110
           XMINLAT = -90.                                                        NESTLL.111
           XMAXLON = 180.                                                        NESTLL.112
           XMINLON =-180.                                                        NESTLL.113
           XMAXLAT = AMAX1(YLAT(1,1),YLAT(1,JXX))                                NESTLL.114
           XMAXLAT = AMAX1(XMAXLAT,YLAT(IYY,JXX))                                NESTLL.115
           XMAXLAT = AMAX1(XMAXLAT,YLAT(IYY,1))                                  NESTLL.116
           IPOLE(INEST+1) = -1                                                   NESTLL.117
           XMAXLAT = XMAXLAT + XEXTND                                            NESTLL.118
        ENDIF                                                                    NESTLL.119
C                                                                                NESTLL.120
      PRINT 65,NINDEX,SS,XMAXLON,XMINLON,XMAXLAT,XMINLAT                         NESTLL.121
      RETURN                                                                     NESTLL.122
C                                                                                NESTLL.123
      ENDIF                                                                      NESTLL.124
C                                                                                NESTLL.125
      IF (NINDEX.EQ.2) PRINT *,'+ NESTED DOMAIN ACROSS THE 180 DEG. +'           NESTLL.126
C                                                                                NESTLL.127
      XMAXLAT = -200.                                                            NESTLL.128
      XMINLAT =  200.                                                            NESTLL.129
      XMAXLON = -1000.                                                           NESTLL.130
      XMINLON =  1000.                                                           NESTLL.131
      DO 60 I = 1,IYY                                                            NESTLL.132
      DO 60 J = 1,JXX                                                            NESTLL.133
      XMAXLAT = AMAX1(XMAXLAT,YLAT(I,J))                                         NESTLL.134
      XMINLAT = AMIN1(XMINLAT,YLAT(I,J))                                         NESTLL.135
      YY = YLON(I,J)                                                             NESTLL.136
      IF (NINDEX.EQ.2 .AND. YY.GT. 0.) YY = YY - 360.                            NESTLL.137
      XMAXLON = AMAX1(XMAXLON,YY)                                                NESTLL.138
      XX = YLON(I,J)                                                             NESTLL.139
      IF (NINDEX.EQ.2 .AND. XX.GT. 0.) XX = XX - 360.                            NESTLL.140
      XMINLON = AMIN1(XMINLON,XX)                                                NESTLL.141
60    CONTINUE                                                                   NESTLL.142
C                                                                                NESTLL.143
      PRINT 65,NINDEX,SS,XMAXLON,XMINLON,XMAXLAT,XMINLAT                         NESTLL.144
65    FORMAT(2X,'NINDEX=',I2,3X,'SS=',2F10.2/                                    NESTLL.145
     1       'XMAXLON,XMINLON,XMAXLAT,XMINLAT:',4F10.2)                          NESTLL.146
C                                                                                NESTLL.147
      YMAXLON = XMAXLON + XEXTND                                                 NESTLL.148
      IF(NINDEX.EQ.2) THEN                                                       NESTLL.149
      XMAXLON = YMAXLON                                                          NESTLL.150
      ELSE IF (YMAXLON.GT.180.0) THEN                                            NESTLL.151
      XMAXLON = YMAXLON - 360.                                                   NESTLL.152
      ELSE                                                                       NESTLL.153
      XMAXLON = YMAXLON                                                          NESTLL.154
      ENDIF                                                                      NESTLL.155
C                                                                                NESTLL.156
      XMINLON = XMINLON - XEXTND                                                 NESTLL.157
      IF (XMINLON .GT. XMAXLON) XMINLON = XMINLON - 360.                         NESTLL.158
      XMAXLAT = XMAXLAT + XEXTND                                                 NESTLL.159
      XMINLAT = XMINLAT - XEXTND                                                 NESTLL.160
      PRINT 66,XMAXLON,XMINLON,XMAXLAT,XMINLAT                                   NESTLL.161
66    FORMAT(2X,'AFTER EXTENDING..  XMAXLON,XMINLON,XMAXLAT,XMINLAT:',           NESTLL.162
     1       4F10.2)                                                             NESTLL.163
C                                                                                NESTLL.164
      RETURN                                                                     NESTLL.165
      END                                                                        NESTLL.166
