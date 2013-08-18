      SUBROUTINE RDLDTR(ID1,ID2,NREC,LREC,XLATI,XLONI,CENTER,                    RDLDTR.1
     1            GRDLTMN,GRDLNMN,XMAXLAT,XMINLAT,XMAXLON,XMINLON,               RDLDTR.2
     2             NI,NIOLD,LND,NUNIT,LNDNAME,TER,IUNIT,TERNAME,                 RDLDTR.3
     3             LANDOUT1,INHT1,LANDIN,INTER1,                                 RDLDTR.4
     4             LANDIN_CHR,INTER1_CHR,IP)                                     06NOV00.247
C                                                                                RDLDTR.6
C   PURPOSE         : READS IN THE PACKED TERRAIN HEIGHT AND LAND-USE            RDLDTR.7
C                     SOURCE DATA, UNPACKS AND STORES THE DATA IN THE            RDLDTR.8
C                     CORE MEMORY POSITIONED BY THE PTRD11, PTRD12,              RDLDTR.9
C                     PTRD13, AND PTRD21 POINTERS IN THE COMMON BLOCK            RDLDTR.10
C                     /SPACE/.                                                   RDLDTR.11
C   ON ENTRY :                                                                   RDLDTR.12
C   ID1              : THE NUMBER OF LAND-USE CATEGORIES (=13).                  RDLDTR.13
C   ID2              : THE NUMBER OF DATA IN A LATITUDE.                         RDLDTR.14
C   NREC             : THE NUMBER OF RECORDS IN THE DATASET, ONE RECORD          RDLDTR.15
C                      CONTAINS ONE LATITUDE LINE OF DATA.                       RDLDTR.16
C   XLATI            : THE FIRST INTEGER LATITUDE IN THE DATASET.                RDLDTR.17
C   XLONI            : THE FIRST INTEGER LONGITUDE IN EACH LATITUDE.             RDLDTR.18
C   CENTER           : THE DATA RESOLUTION (\DEG).                               RDLDTR.19
C   XMAXLAT          : THE MAXIMUM LATITUDE OF THE SEARCH AREA.                  RDLDTR.20
C   XMINLAT          : THE MAXIMUM LATITUDE OF THE SEARCH AREA.                  RDLDTR.21
C   XMAXLON          : THE MAXIMUM LONGITUDE OF THE SEARCH AREA.                 RDLDTR.22
C   XMINLON          : THE MAXIMUM LONGITUDE OF THE SEARCH AREA.                 RDLDTR.23
C   NI               : THE ID NUMBER OF THE SOURCE DATA FILE.                    RDLDTR.24
C   NIOLD            : THE PREVIOUS NI VALUE.                                    RDLDTR.25
C   LND              : A FLAG REQUESTING TO READ IN THE LAND-USE DATA            RDLDTR.26
C                      (=1) OR NOT (=0).                                         RDLDTR.27
C   NUNIT            : READ UNIT NUMBER FOR THE LAND-USE SOURCE DATA.            RDLDTR.28
C   LNDNAME          : THE LAND-USE SOURCE DATA MSS NAME.                        RDLDTR.29
C   TER              : A FLAG REQUESTING TO READ IN THE TERRAIN HEIGHT           RDLDTR.30
C                      DATA (=1) OR NOT (=0).                                    RDLDTR.31
C   IUNIT            : READ UNIT NUMBER FOR THE TERRAIN HEIGHT SOURCE            RDLDTR.32
C                      DATA.                                                     RDLDTR.33
C   TERNAME          : THE TERRAIN HEIGHT SOURCE DATA MSS NAME.                  RDLDTR.34
C                                                                                RDLDTR.35
C   ON EXIT :                                                                    RDLDTR.36
C   LREC             : THE TOTAL NUMBER OF THE SOURCE DATA POINTS                RDLDTR.37
C                      WITHIN THE SEARCH AREA.                                   RDLDTR.38
C   GRDLTMN          : THE MINIMUM LONGITUDE OF THE SOURCE DATA.                 RDLDTR.39
C   GRDLNMN          : THE MINIMUM LATITUDE OF THE SOURCE DATA.                  RDLDTR.40
C   ZLON(IBLOCK)     : LONGITUDES OF THE SOURCE DATA POINTS WITHIN THE           RDLDTR.41
C                      SEARCH AREA.                                              RDLDTR.42
C   ZLAT(IBLOCK)     : LATITUDES OF THE SOURCE DATA POINTS WITHIN THE            RDLDTR.43
C                      SEARCH AREA.                                              RDLDTR.44
C   HTOB(IBLOCK)     : TERRAIN HEIGHTS OF THE SOURCE DATA POINTS WITHIN          RDLDTR.45
C                      THE SEARCH AREA.                                          RDLDTR.46
C   ZLAND(13,IBLOCK) : LAND-USE OF THE SOURCE DATA POINTS WITHIN THE             RDLDTR.47
C                      SEARCH AREA.                                              RDLDTR.48
C                                                                                RDLDTR.49
C   CALLS THE SUBROUTINE NUNPACK.                                                RDLDTR.50
C   CALLED BY SUBROUTINE TERDRV.                                                 RDLDTR.51
C                                                                                RDLDTR.52
C   COMMENTS :                                                                   RDLDTR.53
C IN THE SUBROUTINE TERDRV, THE PROGRAM DYNAMICALLY ALLOCATES THE CORE           RDLDTR.54
C MEMORY FOR ARRAYS ZLON, ZLAT, HTOB, AND ZLAND FOR THE CURRENT                  RDLDTR.55
C WORKING DOMAIN BEFORE ENTERING THIS SUBROUTINE. IN THIS SUBROUTINE,            RDLDTR.56
C THE COMMON BLOCK /SPACE/ AND THE POINTER STATEMENT DEFINES THE FOUR            RDLDTR.57
C ARRAYS. AFTER EXITING FROM THIS SUBROUTINE, THESE FOUR ARRAYS HOLD             RDLDTR.58
C ALL THE NECESSARY SOURCE DATA TO CREATE THE TERRAIN HEIGHT AND                 RDLDTR.59
C LAND-USE FIELDS ON THE MESOSCALE GRIDS.                                        RDLDTR.60
C                                                                                RDLDTR.61
C THE PROGRAM READS IN ONE LATITUDE OF DATA AT A TIME AND CHECKS IF              RDLDTR.62
C THIS LATITUDE IS WITHIN THE SEARCH AREA. IF NOT, THE SUBROUTINE                RDLDTR.63
C READS IN THE NEXT LATITUDE OF DATA. IF YES, THE SUBROUTINE COMPARES            RDLDTR.64
C THE LONGITUDE WITH THE XMAXLON AND XMINLON TO DETERMINE WHETHER IT             RDLDTR.65
C SHOULD KEEP THE DATA. NOTE THAT IF THE DOMAIN IS ACROSS THE DATE LINE          RDLDTR.66
C (XMINLON < -180), THE CONVERSION MENTIONED MUST BE COMPLETED BEFORE            RDLDTR.67
C THE LONGITUDE COMPARISON. SINCE THE 1-DEGREE, 30-, AND 10-MINUTE               RDLDTR.68
C DATA ARE AVAILABLE FOR BOTH TERRAIN HEIGHT AND LAND-USE, THE LOCATIONS         RDLDTR.69
C OF THE DATA ARE AT THE CENTER OF LATITUDE-LONGITUDE BOX. A SHIFT EQUAL         RDLDTR.70
C TO HALF OF THE RESOLUTION CENTER IS INCORPORATED WITH IN THE LATITUDE          RDLDTR.71
C AND LONGITUDE BEFORE THE COMPARISON.                                           19DEC02.8
C                                                                                RDLDTR.76
C DURING THE PROCESS OF READING THE DATA, THE MINIMUM LATITUDE AND               RDLDTR.77
C LONGITUDE, GRDLTMN AND GRDLNMN, OF THE SOURCE DATA ARE FOUND.                  RDLDTR.78
C THERE ARE TWO INTERNAL MSREAD COMMANDS COMPLETED WITH CALL ISHELL IN           RDLDTR.79
C THIS SUBROUTINE TO ACQUIRE THE NECESSARY SOURCE DATA FILES FROM THE            RDLDTR.80
C MSS ONTO THE CRAY DISK. IF NI=NIOLD, IT MEANS THAT THE DATA FILES              RDLDTR.81
C HAVE ALREADY BEEN READ IN FROM THE MSS, AND THE MSREAD COMMANDS ARE            RDLDTR.82
C SKIPPED. WITH THIS INTERNAL MSREAD, WE MAY SAVE SOME DISK SPACE. TO            RDLDTR.83
C MAKE THE TERRAIN PROGRAM PORTABLE TO ANOTHER CRAY COMPUTER SYSTEM,             RDLDTR.84
C HOWEVER, THE NCAR INTERNAL MSREAD COMMANDS ARE NOT USED CURRENTLY AND          RDLDTR.85
C THE SOURCE DATA FILES ARE READ IN FROM THE JOB SCRIPT. THEREFORE,              RDLDTR.86
C ALL THE ARGUMENTS RELATED TO THE INTERNAL MSREAD, SUCH AS NIOLD,               RDLDTR.87
C NUNIT, LNDNAME, IUNIT, TERNAME, IN THIS SUBROUTINE ARE NOT BEING               RDLDTR.88
C USED WITH THIS VERSION OF THE TERRAIN CODE.                                    RDLDTR.89
C                                                                                RDLDTR.90
C IF THE USERS HAVE THEIR OWN SOURCE DATA AND WANT TO USE IT AS THE              RDLDTR.91
C INPUT TO TERRAIN, THE FIRST STEP IS TO RECONSTRUCT THE SOURCE DATA             RDLDTR.92
C TO THE FORMAT DESCRIBED IN TERRAIN CHAPTER OF TUTORIAL NOTES.  AFTER           19DEC02.9
C THE DATA IS IN THE PROPER FORMAT, ONLY MINOR MODIFICATIONS ARE                 RDLDTR.94
C NEEDED TO PROVIDE THE NECESSARY INFORMATION SUCH AS THE NUMBER OF              RDLDTR.95
C RECORDS, THE FIRST LATITUDE AND LONGITUDE OF THE DATA, THE DATA                RDLDTR.96
C RESOLUTION, ETC., TO THE PROGRAM (SEE THE EXAMPLE USING THE AIR                RDLDTR.97
C FORCE 5-MINUTE DATA IN APPENDIX C).                                            RDLDTR.98
                                                                                 RDLDTR.114
# include <paramed.incl>                                                         RDLDTR.115
# include <paramesv.incl>                                                        RDLDTR.116
# include <space.incl>                                                           RDLDTR.117
C                                                                                RDLDTR.118
#if (defined(SUN) || defined(HP))
      INTEGER*2 LANDIN(ID1,ID2)                                                  RDLDTR.120
#else                                                                            RDLDTR.121
      INTEGER*1 LANDIN(ID1,ID2)                                                  RDLDTR.122
#endif                                                                           RDLDTR.123
      INTEGER*2 INTER1(ID2)                                                      RDLDTR.124
C                                                                                RDLDTR.125
      CHARACTER*1 LANDIN_CHR(ID1,ID2)                                            RDLDTR.126
      CHARACTER*2 INTER1_CHR(ID2)                                                RDLDTR.127
C                                                                                RDLDTR.128
      INTEGER TER                                                                RDLDTR.129
      CHARACTER ACS*10, NAMEC*50                                                 05DEC01.16
      CHARACTER LNDNAME*(*), TERNAME*(*)                                         RDLDTR.135
C                                                                                RDLDTR.136
      ID = ID1*ID2                                                               RDLDTR.137
      CSHIFT = CENTER/2.                                                         RDLDTR.138
      LREC = 0                                                                   RDLDTR.145
      IF (IP.EQ.1) PRINT 15,ID1,ID2,XLATI,XLONI,CENTER                           06NOV00.250
 15   FORMAT('IN RDLDTR: ID1=',I2,' ID2=',I5,                                    RDLDTR.147
     -       ' LATI=',F12.4,' LONI=',F12.4,' RESOL=',F12.6)                      RDLDTR.148
C                                                                                RDLDTR.149
      GRDLTMN = XMINLAT + 2.                                                     RDLDTR.150
      GRDLNMN = XMINLON + 2.                                                     RDLDTR.151
C                                                                                RDLDTR.152
      N_START = NINT((XLATI - XMAXLAT)/CENTER) + 1                               RDLDTR.153
      IF (N_START.LT.1) N_START = 1                                              RDLDTR.154
      N_END   = NINT((XLATI - XMINLAT)/CENTER) + 1                               RDLDTR.155
      IF (N_END.GT. NREC) THEN                                                   RDLDTR.156
        PRINT 17,N_END,NREC                                                      RDLDTR.157
 17     FORMAT(/'!!! WARNING ==> N_END=',I6,' > NREC=',I6,' !!!'/)               RDLDTR.158
        N_END=NREC                                                               RDLDTR.159
CC        STOP 171                                                               RDLDTR.160
      ENDIF                                                                      RDLDTR.161
      IF (IP.EQ.1) PRINT 16 , N_START, N_END                                     06NOV00.251
 16   FORMAT(3X,'START_RECORD=',I6,'  END-RECORD=',I6)                           RDLDTR.163
C                                                                                06NOV00.252
      IF(TER.EQ.0) THEN                                                          05DEC01.23
      INQUIRE(UNIT=NUNIT,ACCESS= ACS,NAME=NAMEC,RECL=LENRC)                      06NOV00.253
      IF (IP.EQ.1) PRINT 18, NUNIT,NAMEC,ACS,LENRC                               06NOV00.255
      ELSE                                                                       05DEC01.24
      INQUIRE(UNIT=IUNIT,ACCESS= ACS,NAME=NAMEC,RECL=LENRC)                      05DEC01.25
      IF (IP.EQ.1) PRINT 18, IUNIT,NAMEC,ACS,LENRC                               05DEC01.26
      END IF                                                                     05DEC01.27
 18   FORMAT('<DATA FILE> fort.',I2.2,2X,A50,' ACCESS=',A10,' RECL=',I8)         05DEC01.28
C                                                                                06NOV00.256
      DO 40 IREC = N_START,N_END                                                 RDLDTR.165
                                                                                 RDLDTR.166
C   BUFFER IN LANUSE AND TERRAIN DATA IF LND AND TER NOT EQUAL TO 0              RDLDTR.167
      IF (LND .NE. 0) THEN                                                       RDLDTR.168
          READ(NUNIT,REC=IREC,ERR=100) LANDIN_CHR                                RDLDTR.169
          DO IID1 = 1,ID1                                                        RDLDTR.170
            DO IID2 = 1,ID2                                                      RDLDTR.171
              LANDIN(IID1,IID2) = ICHAR(LANDIN_CHR(IID1,IID2))                   RDLDTR.172
            END DO                                                               RDLDTR.173
          END DO                                                                 RDLDTR.174
      ENDIF                                                                      RDLDTR.175
                                                                                 RDLDTR.176
      IF (TER .NE. 0) THEN                                                       RDLDTR.177
          READ(IUNIT,REC=IREC,ERR=200) INTER1_CHR                                RDLDTR.178
          DO IID2 = 1,ID2                                                        RDLDTR.179
            IF (NI.GT.0) THEN                                                    RDLDTR.180
              INTER1(IID2) = IA(INTER1_CHR(IID2),2,-1)                           RDLDTR.181
            ELSE IF (NI.EQ.-4) THEN                                              RDLDTR.182
              INTER1(IID2) = IA(INTER1_CHR(IID2),2,0)                            RDLDTR.183
            ENDIF                                                                RDLDTR.184
          END DO                                                                 RDLDTR.185
      ENDIF                                                                      RDLDTR.186
                                                                                 RDLDTR.187
      RLAT = XLATI-(IREC-1)*CENTER                                               RDLDTR.188
      IF (RLAT .LT. XMINLAT) GO TO 45                                            RDLDTR.189
      IF ((RLAT-CENTER) .GT. XMAXLAT) GO TO 40                                   RDLDTR.190
      RLAT = RLAT - CSHIFT                                                       RDLDTR.191
                                                                                 06NOV00.257
C                                                                                RDLDTR.192
      DO 41 J = 1,ID2                                                            RDLDTR.193
      RLON = (J-1)*CENTER + CSHIFT + XLONI                                       RDLDTR.194
      IF (XMINLON .LT. -180.) THEN                                               RDLDTR.195
         IF (RLON .GT. XMAXLON .AND. (RLON-360.) .LT. XMINLON) GO TO 41          RDLDTR.196
         IF (RLON .GT. XMAXLON) RLON = RLON -360.                                RDLDTR.197
      ENDIF                                                                      RDLDTR.198
        IF (RLON .LT. XMINLON) GO TO 41                                          RDLDTR.199
        IF (RLON .GT. XMAXLON) GO TO 41                                          RDLDTR.200
        IF(RLON .LT. GRDLNMN) GRDLNMN=RLON                                       RDLDTR.201
        IF(RLAT .LT. GRDLTMN) GRDLTMN=RLAT                                       RDLDTR.202
                                                                                 RDLDTR.203
        LREC = LREC + 1                                                          RDLDTR.204
        IF(LREC.GT.NOBT) THEN                                                    RDLDTR.205
         PRINT 25,LREC,NOBT                                                      RDLDTR.206
 25      FORMAT(1X,'ARRAY SIZE LREC= ',I10,' EXCEEDED THE MAXIMUM SIZE'          RDLDTR.207
     1,' OF ',I10)                                                               RDLDTR.208
         PRINT *, 'TERRAIN PROGRAM STOP IN SUBROUTINE RDLDTR'                    RDLDTR.209
         STOP                                                                    RDLDTR.210
        ENDIF                                                                    RDLDTR.211
                                                                                 RDLDTR.212
        ZLAT(LREC) = RLAT                                                        RDLDTR.213
        ZLON(LREC) = RLON                                                        RDLDTR.214
        IF(TER.NE.0) THEN                                                        RDLDTR.215
           IF (NI.GT.0) THEN                                                     RDLDTR.216
              HTOB(LREC) = FLOAT(INTER1(J))                                      RDLDTR.217
           ELSE IF (NI.EQ.-4) THEN                                               RDLDTR.218
              TEMPGRD(LREC) = FLOAT(INTER1(J))/100.                              RDLDTR.219
              IF (INTER1(J).EQ.0) TEMPGRD(LREC) = -999.9                         RDLDTR.220
           ENDIF                                                                 RDLDTR.221
        ENDIF                                                                    RDLDTR.222
        IF(LND.NE.0) THEN                                                        RDLDTR.223
        DO 30 K=1,ID1                                                            RDLDTR.224
          IF (NI.GT.0) THEN                                                      RDLDTR.225
            ZLAND(K,LREC)=FLOAT(LANDIN(K,J))                                     RDLDTR.226
          ELSE IF (NI.EQ.-1) THEN                                                RDLDTR.227
            IF (NIOLD.EQ.1) THEN                                                 RDLDTR.228
C  ... VEGETATION DATA:                                                          05DEC01.29
              VEGECAT(K,LREC)=FLOAT(LANDIN(K,J))                                 RDLDTR.230
              IF (LANDIN(ID1,J).EQ.100 .AND. K.LT.ID1)                           RDLDTR.231
     -          VEGECAT(K,LREC)= -999.9                                          RDLDTR.232
            ELSE IF (NIOLD.EQ.2) THEN                                            RDLDTR.233
C  ... 30 SEC. VEGETATION DATA:                                                  RDLDTR.234
              VEGECAT30S(LREC) = FLOAT(LANDIN(K,J))                              RDLDTR.235
            ENDIF                                                                RDLDTR.236
          ELSE IF (NI.EQ.-2) THEN                                                RDLDTR.237
            IF (NIOLD.EQ.1) THEN                                                 RDLDTR.238
C  ... SOIL DATA:                                                                05DEC01.30
            SOILCAT(K,LREC)=FLOAT(LANDIN(K,J))                                   RDLDTR.240
            IF (LANDIN(ID1,J).EQ.100 .AND. K.LT.ID1)                             RDLDTR.241
     -          SOILCAT(K,LREC)= -999.9                                          RDLDTR.242
            ELSE IF (NIOLD.EQ.2) THEN                                            RDLDTR.243
C  ... 30 SEC. SOIL DATA:                                                        RDLDTR.244
              SOILCAT30S(LREC) = FLOAT(LANDIN(K,J))                              RDLDTR.245
            ENDIF                                                                RDLDTR.246
          ELSE IF (NI.EQ.-3) THEN                                                RDLDTR.247
            VEGEFRC(K,LREC)=FLOAT(LANDIN(K,J))                                   RDLDTR.248
          ENDIF                                                                  RDLDTR.249
30      CONTINUE                                                                 RDLDTR.250
        ENDIF                                                                    RDLDTR.251
C                                                                                RDLDTR.252
                                                                                 RDLDTR.253
41      CONTINUE                                                                 RDLDTR.254
40      CONTINUE                                                                 RDLDTR.255
C                                                                                RDLDTR.256
45    CONTINUE                                                                   RDLDTR.257
C                                                                                RDLDTR.258
      GO TO 400                                                                  RDLDTR.259
C                                                                                RDLDTR.260
100   PRINT 101,IREC,NUNIT                                                       RDLDTR.261
101   FORMAT(' RECORD =',I8,',  ERROR OCCURED ON UNIT: ',I3)                     RDLDTR.262
      STOP                                                                       RDLDTR.263
200   PRINT 101,IREC,IUNIT                                                       RDLDTR.264
201   STOP                                                                       RDLDTR.265
C                                                                                RDLDTR.266
400   CONTINUE                                                                   RDLDTR.267
      IF(IP.EQ.1)                                                                06NOV00.258
     >   PRINT 401,LND,NUNIT,TER,IUNIT, CENTER, CSHIFT, NI,NIOLD,LREC            06NOV00.259
 401  FORMAT('IN RDLDTR: *LND=',I2,'  UNIT=',I2,' *TER=',I2,'  UNIT=',I2,        RDLDTR.269
     >       ' RESOLUTION=',F8.5,' SHIFT=',F8.5/10X,                             RDLDTR.270
     >       ' NI=',I2,' NIOLD=',I2,'   LREC=',I10)                              RDLDTR.271
C                                                                                RDLDTR.272
      RETURN                                                                     RDLDTR.273
      END                                                                        RDLDTR.274
