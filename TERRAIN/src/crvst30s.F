      SUBROUTINE CRVST30S(VARNAME,NCAT,IX,JX,XLAT,XLON,YYY,ZZZ,LZZ,N,            CRVST30S.1
     1                 NOBS,GRDLNMN,GRDLTMN,ZLON,ZLAT,ZXXX,RESOL,IDW,IP)         06NOV00.61
C                                                                                CRVST30S.3
C   PURPOSE : TO CREAT THE VEGETATION, SOIL, VEGETATION FRACTION AND             CRVST30S.4
C             GROUND TEMPERATURE FIELDS FOR LAND-SURFACE MODEL.                  CRVST30S.5
C                                                                                CRVST30S.6
C   ON ENTRY: VARNAME    -- NAME OF THE FIELD                                    CRVST30S.7
C             NCAT       -- THE NUMBER OF CATEGORIES                             CRVST30S.8
C             IX,JX      -- DOMAIN SIZE                                          CRVST30S.9
C             XLAT, XLON -- LATITUDE AND LONGITUDE FOR MESOSCALE GRID            CRVST30S.10
C             N          -- DOMAIN ID NUMBER                                     CRVST30S.11
C             NOBS       -- NUMBER OF (LAT/LON) DATA POINTS                      CRVST30S.12
C             ZLON,ZLAT  -- LATITUDE AND LONGITUDE OF THE SOURCE DATA POINTS     CRVST30S.13
C             ZXXX       -- SOURCE DATA AT (ZLON,ZLAT)                           CRVST30S.14
C             RESOL      -- RESOULUTION OF THE SOURCE DATA                       CRVST30S.15
C             IDW        -- CATEGORY OF WATER, OTHERWISE SET IDW = 0             CRVST30S.16
C             GRDLTMN    -- MINIMUM LATITUDE IN THE SEARCH AREA                  CRVST30S.17
C             GRDLNMN    -- MINIMUM LONGITUDE IN THE SEARCH AREA                 CRVST30S.18
C    ON EXIT: YYY        -- THE PERCENTAGE FOR EACH OF CATEGORIES                CRVST30S.19
C                           ON MESOSCALE GRID                                    CRVST30S.20
C             ZZZ        -- THE PERCANTAGE CORRESPONDING TO THE DOMINATING       CRVST30S.21
C                           COTEGORY                                             CRVST30S.22
C             LZZ        -- THE DOMINATING COTEGORY AT THE GRID POINT            CRVST30S.23
C                                                                                CRVST30S.24
# include <parame.incl>                                                          CRVST30S.25
# include <paramed.incl>                                                         CRVST30S.26
# include <io.incl>                                                              06NOV00.62
# include <nestdmn.incl>                                                         CRVST30S.27
# include <maps.incl>                                                            CRVST30S.28
# include <ltdata.incl>                                                          CRVST30S.29
C                                                                                CRVST30S.30
      DIMENSION YYY(IX,JX,NCAT), XLAT(IX,JX), XLON(IX,JX)                        CRVST30S.31
      DIMENSION ZLON(NOBS), ZLAT(NOBS), ZXXX(NOBS)                               CRVST30S.32
      DIMENSION ZZZ(IX,JX), LZZ(IX,JX),                                          CRVST30S.33
     -          IBOX(NSIZE), JBOX(NSIZE)                                         CRVST30S.34
      CHARACTER*(*) VARNAME                                                      CRVST30S.35
C                                                                                CRVST30S.36
      DO I = 1,IX                                                                CRVST30S.37
        DO J = 1,JX                                                              CRVST30S.38
          ZZZ(I,J) = 0.                                                          CRVST30S.39
          LZZ(I,J) = 0                                                           CRVST30S.40
          DO NN = 1,NCAT                                                         CRVST30S.41
            YYY(I,J,NN) = 0.                                                     CRVST30S.42
          END DO                                                                 CRVST30S.43
        END DO                                                                   CRVST30S.44
      END DO                                                                     CRVST30S.45
C                                                                                CRVST30S.46
      DSGRID = RESOL                                                             CRVST30S.47
      INCR = 1./DSGRID+0.001                                                     CRVST30S.48
      IF(IP.EQ.1)PRINT 6, VARNAME,RESOL,INCR,IDW,NCAT                            06NOV00.63
6     FORMAT(' ==> CALL CRVST30S FOR ',A8,'  RESOLUTION =',                      CRVST30S.50
     1       F8.5,3X,'INCR=',I3,'   WATER CAT.=',I2,                             CRVST30S.51
     2       '  # OF CATEG.=',I2)                                                CRVST30S.52
      IF(IP.EQ.1)PRINT 7, NOBS,GRDLNMN, GRDLTMN                                  06NOV00.64
7     FORMAT(/2X,' NOBS=',I8,4X,' ==< GRDLNMN=',F10.2,                           CRVST30S.54
     1                            '   GRDLTMN=',F10.2,'>==')                     CRVST30S.55
C                                                                                CRVST30S.56
C-----CONVERT ZLON AND ZLAT FROM LON AND LAT TO X AND Y IN MESH                  CRVST30S.57
C                                                                                CRVST30S.58
      CALL XYOBSLL(ZLAT,ZLON,ZXXX,NOBS,IXEX,JXEX,DIS(1),                         CRVST30S.59
     1             XSOUTH(N),XWEST(N),NRATIO(N),N-1,1)                           CRVST30S.60
      NIX = 0                                                                    CRVST30S.61
      DO MM = 1,NOBS                                                             CRVST30S.62
        IF (ZLAT(MM).GE.0.0 .AND. ZLAT(MM).LE.IX .AND.                           CRVST30S.63
     1      ZLON(MM).GE.0.0 .AND. ZLON(MM).LE.JX) THEN                           CRVST30S.64
        NIX = NIX+1                                                              CRVST30S.65
C                                                                                CRVST30S.66
        ICAT = NINT(ZXXX(MM)*100.)                                               CRVST30S.67
        IF (ICAT.EQ.0) ICAT = NCAT                                               CRVST30S.68
C                                                                                CRVST30S.69
        IF (ICAT.LT.1 .OR. ICAT.GT.NCAT) THEN                                    CRVST30S.70
          PRINT 1002,NIX,MM,ZLAT(MM),ZLON(MM),ZXXX(MM),ICAT                      CRVST30S.71
 1002     FORMAT('NIX=',I6,2X,I6,' YIC,XIC,ZXX:',3F12.4,' ICAT=',I5)             CRVST30S.72
          STOP 1002                                                              CRVST30S.73
        ENDIF                                                                    CRVST30S.74
C                                                                                CRVST30S.75
        I0 = INT(ZLAT(MM)+0.5)                                                   CRVST30S.76
        J0 = INT(ZLON(MM)+0.5)                                                   CRVST30S.77
          IF (I0.GE.1 .AND. I0.LE.IX-1 .AND.                                     CRVST30S.78
     1        J0.GE.1 .AND. J0.LE.JX-1 ) THEN                                    CRVST30S.79
              YYY(I0,J0,ICAT) = YYY(I0,J0,ICAT)+1.0                              CRVST30S.80
          ENDIF                                                                  CRVST30S.81
        ENDIF                                                                    CRVST30S.82
      END DO                                                                     CRVST30S.83
C                                                                                CRVST30S.84
      NBOX = 0                                                                   CRVST30S.85
      DO I = 1,IX-1                                                              CRVST30S.86
      DO J = 1,JX-1                                                              CRVST30S.87
        POINTS = 0.0                                                             CRVST30S.88
        DO NC = 1,NCAT                                                           CRVST30S.89
          POINTS = POINTS + YYY(I,J,NC)                                          CRVST30S.90
        END DO                                                                   CRVST30S.91
        IF (POINTS.GT.0.0) THEN                                                  CRVST30S.92
          DO NC = 1, NCAT                                                        CRVST30S.93
            YYY(I,J,NC) = YYY(I,J,NC)/POINTS*100.                                CRVST30S.94
          END DO                                                                 CRVST30S.95
        ELSE                                                                     CRVST30S.96
          NBOX = NBOX+1                                                          CRVST30S.97
          IBOX(NBOX) = I                                                         CRVST30S.98
          JBOX(NBOX) = J                                                         CRVST30S.99
        ENDIF                                                                    CRVST30S.100
      END DO                                                                     CRVST30S.101
      END DO                                                                     CRVST30S.102
C                                                                                CRVST30S.103
      IF (NBOX.GT.0) THEN                                                        CRVST30S.104
        KK = 1.0/DIS(N)                                                          CRVST30S.105
        IF(IP.EQ.1) PRINT 11,N,DIS(N),KK,NBOX                                    06NOV00.65
 11     FORMAT(/3X,'DOMAIN ',I2,' DS=',F8.3,' KM. N-DIFF_GRID=',I4,              CRVST30S.107
     1     ' ===>',I5,' GRID-BOXES WITH NO DATA INSIDE.'/)                       CRVST30S.108
        RMAX = 0.0                                                               CRVST30S.109
        DO NB = 1,NBOX                                                           CRVST30S.110
          I0 = IBOX(NB)                                                          CRVST30S.111
          J0 = JBOX(NB)                                                          CRVST30S.112
CC          PRINT 1003,NB,I0,J0                                                  CRVST30S.113
 1003     FORMAT(/'N =',I4,' I0=',I3,' J0=',I3)                                  CRVST30S.114
C                                                                                CRVST30S.115
          RMIN = 1.E8                                                            CRVST30S.116
          MMIN = 0                                                               CRVST30S.117
          NPP = 0                                                                CRVST30S.118
          DO MM = 1,NOBS                                                         CRVST30S.119
            IOBS = INT(ZLAT(MM))                                                 CRVST30S.120
            JOBS = INT(ZLON(MM))                                                 CRVST30S.121
            IF (ABS(I0-IOBS).LE.KK .AND. ABS(J0-JOBS).LE.KK) THEN                CRVST30S.122
              NPP = NPP+1                                                        CRVST30S.123
              RXX = ZLON(MM) - J0                                                CRVST30S.124
              RYY = ZLAT(MM) - I0                                                CRVST30S.125
              RR = SQRT(RXX*RXX + RYY*RYY)                                       CRVST30S.126
              IF (RR.LT.RMIN) THEN                                               CRVST30S.127
                RMIN = RR                                                        CRVST30S.128
                MMIN = MM                                                        CRVST30S.129
              ENDIF                                                              CRVST30S.130
            ENDIF                                                                CRVST30S.131
          END DO                                                                 CRVST30S.132
C                                                                                CRVST30S.133
          IF (RMIN.GT.RMAX) RMAX = RMIN                                          CRVST30S.134
          DO NC = 1,NCAT                                                         CRVST30S.135
            YYY(I0,J0,NC) = 0.0                                                  CRVST30S.136
          END DO                                                                 CRVST30S.137
          ICAT = NINT(ZXXX(MMIN)*100.)                                           CRVST30S.138
          YYY(I0,J0,ICAT) = 100.                                                 CRVST30S.139
CC          PRINT 1004,NPP,MMIN,RMIN,                                            CRVST30S.140
CC     1               ZLAT(MMIN),ZLON(MMIN),ZXXX(MMIN),ICAT                     CRVST30S.141
 1004     FORMAT('SEARCH ',I4,' DATA POINTS. THE CLOSET POINT INDEX=',           CRVST30S.142
     1         I6,' DIST.=',F8.2,' ZLAT,ZLON,ZXXX=',3F8.3,' ICAT=',I3)           CRVST30S.143
        END DO                                                                   CRVST30S.144
        IF(IP.EQ.1) PRINT *,'THE MAXIMUM DISTANCE =',RMAX                        06NOV00.66
CC        STOP                                                                   CRVST30S.146
      ENDIF                                                                      CRVST30S.147
C                                                                                CRVST30S.148
C                                                                                CRVST30S.149
C     NOTE: IT IS DESIRABLE TO FORCE GRID BOXES WITH LESS                        CRVST30S.150
C           THAN 50 PERCENT WATER TO BE A LAND CATEGORY,                         CRVST30S.151
C           EVEN IF WATER IS THE LARGEST SINGLE CATEGORY.                        CRVST30S.152
C                                                                                CRVST30S.153
      DO II = 1,IX-1                                                             CRVST30S.154
      DO JJ = 1,JX-1                                                             CRVST30S.155
C                                                                                CRVST30S.156
        DO ILEV = 1,NCAT                                                         CRVST30S.157
C                                                                                CRVST30S.158
        IF (ILEV.EQ.IDW .AND. YYY(II,JJ,ILEV).LT.50.0) GOTO 30                   CRVST30S.159
C                                                                                CRVST30S.160
          IF (YYY(II,JJ,ILEV).GT.ZZZ(II,JJ)) THEN                                CRVST30S.161
            ZZZ(II,JJ) = YYY(II,JJ,ILEV)                                         CRVST30S.162
            LZZ(II,JJ) = ILEV                                                    CRVST30S.163
          ENDIF                                                                  CRVST30S.164
C                                                                                CRVST30S.165
 30     CONTINUE                                                                 CRVST30S.166
        END DO                                                                   CRVST30S.167
C                                                                                CRVST30S.168
      END DO                                                                     CRVST30S.169
      END DO                                                                     CRVST30S.170
C                                                                                CRVST30S.171
      RETURN                                                                     CRVST30S.172
      END                                                                        CRVST30S.173
