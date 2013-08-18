      SUBROUTINE MXMNLL(IYY,JXX,YLAT,YLON,XI1,XI2,XJ1,XJ2,                       MXMNLL.1
     1            XEXTND,XMAXLAT,XMINLAT,XMAXLON,XMINLON,IPOLE,INEST)            MXMNLL.2
C                                                                                MXMNLL.3
C   PURPOSE      : FINDS THE MAXIMUM AND MINIMUM LATITUDE AND LONGITUDE          MXMNLL.4
C                  OF THE SEARCH AREA FOR THE DOMAIN INEST+1.                    MXMNLL.5
C   ON ENTRY     :                                                               MXMNLL.6
C   IYY,JXX      : THE DIMENSIONS OF THE DOMAIN INEST+1.                         MXMNLL.7
C   YLAT(IYY,JXX): LATITUDES OF THE MESOSCALE GRIDS.                             MXMNLL.8
C   YLON(IYY,JXX): LONGITUDES OF THE MESOSCALE GRIDS.                            MXMNLL.9
C   XI1          : THE LOCATION OF THE SOUTHERN BOUNDARY OF THE                  MXMNLL.10
C                  DOMAIN INEST+1.                                               MXMNLL.11
C   XI2          : THE LOCATION OF THE NORTHERN BOUNDARY OF THE                  MXMNLL.12
C                  DOMAIN INEST+1.                                               MXMNLL.13
C   XJ1          : THE LOCATION OF THE WESTERN BOUNDARY OF THE                   MXMNLL.14
C                  DOMAIN INEST+1.                                               MXMNLL.15
C   XJ2          : THE LOCATION OF THE EASTERN BOUNDARY OF THE                   MXMNLL.16
C                  DOMAIN INEST+1.                                               MXMNLL.17
C   XEXTND       : THE INCREMENT TO EXPAND THE SEARCH AREA.                      MXMNLL.18
C   IPOLE(20)    : A FLAG ARRAY TO INDICATE THE POLE IS INSIDE (= 1)             MXMNLL.19
C                  OR OUTSIDE (=0) THE DOMAIN.                                   MXMNLL.20
C   INEST        : THE DOMAIN ID NUMBER - 1. FOR THE COARSE DOMAIN,              MXMNLL.21
C                  INEST=0. FOR THE NEST DOMAIN, INEST > 0.                      MXMNLL.22
C   ON EXIT        :                                                             MXMNLL.23
C   XMAXLAT        : THE MAXIMUM LATITUDE OF THE SEARCH AREA.                    MXMNLL.24
C   XMINLAT        : THE MINIMUM LATITUDE OF THE SEARCH AREA.                    MXMNLL.25
C   XMAXLON        : THE MAXIMUM LONGITUDE OF THE SEARCH AREA.                   MXMNLL.26
C   XMINLON        : THE MINIMUM LONGITUDE OF THE SEARCH AREA.                   MXMNLL.27
C   IPOLE(INEST+1) : IPOLE(INEST+1) = 1: POLE IS INSIDE THE DOMAIN,              MXMNLL.28
C                    IPOLE(INEST+1) = 0: POLE IS OUTSIDE THE DOMAIN.             MXMNLL.29
C                                                                                MXMNLL.30
C   CALLS THE SUBROUTINE NESTLL.                                                 MXMNLL.31
C   CALLED BY SUBROUTINE TERDRV.                                                 MXMNLL.32
C                                                                                MXMNLL.33
C   COMMENTS :                                                                   MXMNLL.34
C THE METHOD TO DETERMINE THE XMAXLAT, XMINLAT, XMAXLON, AND XMINLON IS          MXMNLL.35
C DESCRIBED IN SECTION 3.2. IN ADDITION TO THE LIST OF ARGUMENTS,                MXMNLL.36
C SUBROUTINE MXMNLL GETS THE CENTRAL LATITUDE PHIC, THE CENTRAL                  MXMNLL.37
C J-INDEX XJC, ETC., FROM THE COMMON BLOCK /MAPS/ AND THE SWITCH                 MXMNLL.38
C IPRINT FROM THE COMMON BLOCK /OPTION/.                                         MXMNLL.39
C                                                                                MXMNLL.40
C                                                                                MXMNLL.41
C  (1) IF IPROJ=6HPOLSTR, IT IS POSSIBLE THAT EITHER OF THE NORTH OR             MXMNLL.42
C    SOUTH POLE IS INSIDE THE DOMAIN.                                            MXMNLL.43
C                                                                                MXMNLL.44
C       IF THE NORTH POLE IS IN THE DOMAIN, SET IPOLE =  1 AND                   MXMNLL.45
C        XMAXLAT =  90.                                                          MXMNLL.46
C        XMINLAT =  MIN(YLAT(1,1),YLAT(1,JXX),YLAT(IYY,JXX),                     MXMNLL.47
C                       YLAT(IYY,1))                                             MXMNLL.48
C        XMINLON = -180.                                                         MXMNLL.49
C        XMAXLON =  180.                                                         MXMNLL.50
C                                                                                MXMNLL.51
C       IF THE SOUTH POLE IS IN THE DOMAIN, SET IPOLE = -1 AND                   MXMNLL.52
C        XMAXLAT =  MAX(YLAT(1,1),YLAT(1,JXX),YLAT(IYY,JXX),                     MXMNLL.53
C                       YLAT(IYY,1))                                             MXMNLL.54
C        XMINLAT = -90.                                                          MXMNLL.55
C        XMINLON = -180.                                                         MXMNLL.56
C        XMAXLON =  180.                                                         MXMNLL.57
C                                                                                MXMNLL.58
C                                                                                MXMNLL.59
C       IF THE POLE IS INSIDE THE COARSE DOMAIN, THERE IS A LITTLE               MXMNLL.60
C        BIT TRICK TO DETERMINE THE XMAXLON AND XMINLON.                         MXMNLL.61
C                                                                                MXMNLL.62
C  (2) IF NEITHER OF THE POLES IS WITHIN THE DOMAIN, DETERMINE THE               MXMNLL.63
C     XMAXLAT, XMINLAT, XMAXLON AND XMINLON FOR DIFFERENT HEMISPHERE.            MXMNLL.64
C                                                                                MXMNLL.65
C                                                                                MXMNLL.66
C                                                                                MXMNLL.67
# include <parame.incl>                                                          MXMNLL.68
# include <maps.incl>                                                            MXMNLL.69
# include <option.incl>                                                          MXMNLL.70
C                                                                                MXMNLL.71
      DIMENSION YLAT(IYY,JXX),YLON(IYY,JXX),IPOLE(20)                            MXMNLL.72
      DIMENSION XLON_OUT(IIMX,JJMX), XLON_IN(IIMX,JJMX)                          MXMNLL.73
C                                                                                MXMNLL.74
C  SET THE LONGITUDES IN EAST HEMISPHERE TO BE POSITIVE.                         MXMNLL.75
C                                                                                MXMNLL.76
      DO 10 I = 1,IYY                                                            MXMNLL.77
      DO 10 J = 1,JXX                                                            MXMNLL.78
        IF(YLON(I,J).LT.-180.) YLON(I,J) = 360. + YLON(I,J)                      MXMNLL.79
10    CONTINUE                                                                   MXMNLL.80
C                                                                                MXMNLL.81
      IPOLE(INEST+1) = 0                                                         MXMNLL.82
C                                                                                MXMNLL.83
       IF (PHIC .GE. 0.0) THEN                                                   MXMNLL.84
C                                                                                MXMNLL.85
C    NORTHERN HEMISPHERE:                                                        MXMNLL.86
C                                                                                MXMNLL.87
      IF (YLON(IYY,JXX).LT.YLON(IYY,1)) THEN                                     MXMNLL.88
        CC = -YLON(IYY,JXX) + YLON(IYY,1)                                        MXMNLL.89
      ELSE                                                                       MXMNLL.90
        CC = 360. - YLON(IYY,JXX) + YLON(IYY,1)                                  MXMNLL.91
      ENDIF                                                                      MXMNLL.92
      IF(IPRINT.EQ.1) THEN                                                       MXMNLL.93
      PRINT 104,XI1,XI2,XJ1,XJ2,XJC,CC                                           MXMNLL.94
      PRINT 1301,YLAT(1,1),YLAT(1,JXX),YLAT(IYY,JXX),YLAT(IYY,1)                 MXMNLL.95
      PRINT 1302,YLON(1,1),YLON(1,JXX),YLON(IYY,JXX),YLON(IYY,1)                 MXMNLL.96
      ENDIF                                                                      MXMNLL.97
C                                                                                MXMNLL.98
                                                                                 MXMNLL.99
        IF (INEST.EQ.0 .AND. CC.LE.180. .AND. IPROJ.EQ.NPROJ(2)) THEN            MXMNLL.100
C                                                                                MXMNLL.101
C    THE POLE IS INSIDE THE DOMAIN:                                              MXMNLL.102
           XMAXLAT =  90.                                                        MXMNLL.103
           XMAXLON = 180.                                                        MXMNLL.104
           XMINLON =-180.                                                        MXMNLL.105
           XMINLAT = AMIN1(YLAT(1,1),YLAT(1,JXX))                                MXMNLL.106
           XMINLAT = AMIN1(XMINLAT,YLAT(IYY,JXX))                                MXMNLL.107
           XMINLAT = AMIN1(XMINLAT,YLAT(IYY,JXX))                                MXMNLL.108
           XMINLAT = XMINLAT - XEXTND                                            MXMNLL.109
           IPOLE(1) = 1                                                          MXMNLL.110
           GO TO 105                                                             MXMNLL.111
C                                                                                MXMNLL.112
C    THE POLE IS OUTSIDE THE DOMAIN:                                             MXMNLL.113
C                                                                                MXMNLL.114
        ELSE IF (INEST.GE.1 .AND. IPOLE(1).NE.0) THEN                            MXMNLL.115
C                                                                                MXMNLL.116
C  IF THE NORTH POLE IS INSIDE THE LARGEST DOMAIN AND OUTSIDE THE NESTED         MXMNLL.117
C  DOMAIN, WE MUST DETERMINE IF THE NESTED DOMAIN CROSSES THE DATE LINE          MXMNLL.118
C  (180 E) OR NOT FIRST.                                                         MXMNLL.119
C                                                                                MXMNLL.120
         CALL NESTLL(IYY,JXX,YLAT,YLON,PHIC,                                     MXMNLL.121
     1         XMAXLAT,XMINLAT,XMAXLON,XMINLON,XEXTND,IPOLE,INEST)               MXMNLL.122
         GO TO 105                                                               MXMNLL.123
C                                                                                MXMNLL.124
        ELSE IF (XJ1.LE.XJC.AND.XJ2.GE.XJC) THEN                                 MXMNLL.125
          XMAXLAT = -200.                                                        MXMNLL.126
          DO 111 J = 1,JXX                                                       MXMNLL.127
          IF (YLAT(IYY,J).GT.XMAXLAT) XMAXLAT = YLAT(IYY,J)                      MXMNLL.128
111       CONTINUE                                                               MXMNLL.129
          XMAXLON = YLON(IYY,JXX)                                                MXMNLL.130
          XMINLON = YLON(IYY,1)                                                  MXMNLL.131
          XMINLAT = AMIN1(YLAT(1,1),YLAT(1,JXX))                                 MXMNLL.132
        ELSE IF (XJ2.LT.XJC) THEN                                                MXMNLL.133
          XMAXLAT = YLAT(IYY,JXX)                                                MXMNLL.134
          XMAXLON = YLON(1,JXX)                                                  MXMNLL.135
          XMINLON = YLON(IYY,1)                                                  MXMNLL.136
          XMINLAT = YLAT(1,1)                                                    MXMNLL.137
        ELSE                                                                     MXMNLL.138
          XMAXLAT = YLAT(IYY,1)                                                  MXMNLL.139
          XMAXLON = YLON(IYY,JXX)                                                MXMNLL.140
          XMINLON = YLON(1,1)                                                    MXMNLL.141
          XMINLAT = YLAT(1,JXX)                                                  MXMNLL.142
         ENDIF                                                                   MXMNLL.143
C                                                                                MXMNLL.144
       ELSE                                                                      MXMNLL.145
C                                                                                MXMNLL.146
C    SOUTH HEMISPHERE:                                                           MXMNLL.147
C                                                                                MXMNLL.148
      IF (YLON(1,1).LT.YLON(1,JXX)) THEN                                         MXMNLL.149
        CC = - YLON(1,1) + YLON(1,JXX)                                           MXMNLL.150
      ELSE                                                                       MXMNLL.151
        CC = 360. - YLON(1,1) + YLON(1,JXX)                                      MXMNLL.152
      ENDIF                                                                      MXMNLL.153
      IF(IPRINT.EQ.1) THEN                                                       MXMNLL.154
      PRINT 104,XI1,XI2,XJ1,XJ2,XJC,CC                                           MXMNLL.155
      PRINT 1301,YLAT(1,1),YLAT(1,JXX),YLAT(IYY,JXX),YLAT(IYY,JXX)               MXMNLL.156
      PRINT 1302,YLON(1,1),YLON(1,JXX),YLON(IYY,JXX),YLON(IYY,1)                 MXMNLL.157
      ENDIF                                                                      MXMNLL.158
C                                                                                MXMNLL.159
        IF (INEST.EQ.0 .AND. CC.GE.180. .AND. IPROJ.EQ.NPROJ(2)) THEN            MXMNLL.160
           XMINLAT = -90.                                                        MXMNLL.161
           XMAXLON = 180.                                                        MXMNLL.162
           XMINLON =-180.                                                        MXMNLL.163
           XMAXLAT = AMAX1(YLAT(1,1),YLAT(1,JXX))                                MXMNLL.164
           XMAXLAT = AMAX1(XMAXLAT,YLAT(IYY,JXX))                                MXMNLL.165
           XMAXLAT = AMAX1(XMAXLAT,YLAT(IYY,1))                                  MXMNLL.166
           IPOLE(1) = -1                                                         MXMNLL.167
           XMAXLAT = XMAXLAT + XEXTND                                            MXMNLL.168
           GO TO 105                                                             MXMNLL.169
C                                                                                MXMNLL.170
C    THE POLE IS OUTSIDE THE DOMAIN:                                             MXMNLL.171
C                                                                                MXMNLL.172
        ELSE IF (INEST.GE.1 .AND. IPOLE(1).NE.0) THEN                            MXMNLL.173
         CALL NESTLL(IYY,JXX,YLAT,YLON,PHIC,                                     MXMNLL.174
     1         XMAXLAT,XMINLAT,XMAXLON,XMINLON,XEXTND,IPOLE,INEST)               MXMNLL.175
         GO TO 105                                                               MXMNLL.176
C                                                                                MXMNLL.177
        ELSE IF (XJ1.LE.XJC.AND.XJ2.GE.XJC) THEN                                 MXMNLL.178
          XMAXLAT = AMAX1(YLAT(IYY,1),YLAT(IYY,JXX))                             MXMNLL.179
          XMAXLON = YLON(1,JXX)                                                  MXMNLL.180
          XMINLON = YLON(1,1)                                                    MXMNLL.181
          XMINLAT = 200.                                                         MXMNLL.182
          DO 112 J = 1,JXX                                                       MXMNLL.183
          IF (YLAT(1,J).LT.XMINLAT) XMINLAT = YLAT(1,J)                          MXMNLL.184
112       CONTINUE                                                               MXMNLL.185
        ELSE IF (XJ2.LT.XJC) THEN                                                MXMNLL.186
          XMAXLAT = YLAT(IYY,1)                                                  MXMNLL.187
          XMAXLON = YLON(IYY,JXX)                                                MXMNLL.188
          XMINLON = YLON(1,1)                                                    MXMNLL.189
          XMINLAT = YLAT(1,JXX)                                                  MXMNLL.190
        ELSE                                                                     MXMNLL.191
          XMAXLAT = YLAT(IYY,JXX)                                                MXMNLL.192
          XMAXLON = YLON(1,JXX)                                                  MXMNLL.193
          XMINLON = YLON(IYY,1)                                                  MXMNLL.194
          XMINLAT = YLAT(1,1)                                                    MXMNLL.195
        ENDIF                                                                    MXMNLL.196
C                                                                                MXMNLL.197
      ENDIF                                                                      MXMNLL.198
C                                                                                MXMNLL.199
        XMAXLAT = XMAXLAT + XEXTND                                               MXMNLL.200
        XMINLAT = XMINLAT - XEXTND                                               MXMNLL.201
        IF (PHIC .GE. 0.0) THEN                                                  MXMNLL.202
          CC2 = CC/2.0 - 0.10                                                    MXMNLL.203
        ELSE                                                                     MXMNLL.204
          CC2 = (360.-CC)/2. - 0.10                                              MXMNLL.205
        ENDIF                                                                    MXMNLL.206
        IF (CC2.LT.XEXTND) THEN                                                  MXMNLL.207
          PRINT *,'THE HALF RESIDAUL AREA CC2=',CC2,' < XEXTND=',XEXTND          MXMNLL.208
          XMAXLON = XMAXLON + CC2                                                MXMNLL.209
          XMINLON = XMINLON - CC2                                                MXMNLL.210
        ELSE                                                                     MXMNLL.211
          XMAXLON = XMAXLON + XEXTND                                             MXMNLL.212
          XMINLON = XMINLON - XEXTND                                             MXMNLL.213
        ENDIF                                                                    MXMNLL.214
C                                                                                MXMNLL.215
C   IF THE DOMAIN CROSSED THE DATE LINE (180 DEG.):                              MXMNLL.216
C                                                                                MXMNLL.217
      IF(XMINLON .GT. XMAXLON) PRINT 100                                         MXMNLL.218
100   FORMAT(1X,'###### DOMAIN CROSSED THE 180 DEG. ######')                     06NOV00.99
      IF(XMINLON .GT. XMAXLON) XMINLON = XMINLON - 360.                          MXMNLL.220
C                                                                                06NOV00.100
      IF (XMAXLON.GT.180.) THEN                                                  06NOV00.101
          XMAXLON = XMAXLON -360.                                                06NOV00.102
          XMINLON = XMINLON -360.                                                06NOV00.103
      ENDIF                                                                      06NOV00.104
C                                                                                06NOV00.105
105   CONTINUE                                                                   MXMNLL.222
C                                                                                MXMNLL.223
      IF (XMAXLAT.GT. 90.0) XMAXLAT =  90.0                                      MXMNLL.224
      IF (XMINLAT.LT.-90.0) XMINLAT = -90.0                                      MXMNLL.225
C                                                                                MXMNLL.226
      IF(IPOLE(INEST+1).EQ.1) PRINT 101,INEST                                    MXMNLL.227
101   FORMAT(1X,'$$$$$$ NORTH POLE IS INSIDE THE DOMAIN=',I3,' $$$$$$')          06NOV00.106
      IF(IPOLE(INEST+1).EQ.-1) PRINT 102,INEST                                   MXMNLL.229
102   FORMAT('  $$$$$$ SOUTH POLE IS INSIDE THE DOMAIN=',I3,' $$$$$$')           06NOV00.107
C                                                                                MXMNLL.231
104   FORMAT(/2X,'I1=',F5.1,2X,'I2=',F5.1,2X,'J1=',F5.1,2X,'J2=',F5.1,           MXMNLL.232
     1        2X,'JC=',F5.1,3X,'CC=',F8.2)                                       MXMNLL.233
1301  FORMAT(2X,'YLAT(1,1)=',F8.2,2X,'YLAT(1,JXX)=',F8.2,2X,                     MXMNLL.234
     1      'YLAT(IYY,JXX)=',F8.2,2X,'YLAT(IYY,1)=',F8.2)                        MXMNLL.235
1302  FORMAT(2X,'YLON(1,1)=',F8.2,2X,'YLON(1,JXX)=',F8.2,2X,                     MXMNLL.236
     1      'YLON(IYY,JXX)=',F8.2,2X,'YLON(IYY,1)=',F8.2)                        MXMNLL.237
C                                                                                MXMNLL.238
      RETURN                                                                     MXMNLL.239
      END                                                                        MXMNLL.240
                                                                                 MXMNLL.241
