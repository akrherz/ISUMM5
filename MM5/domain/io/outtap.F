      SUBROUTINE OUTTAP(IUTL,NESCOU,IX,JX,KZZ,IFWRTHDR)                          OUTTAP.1
       IMPLICIT NONE                                                             19DEC02.524
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          OUTTAP.3
C                                                                     C          OUTTAP.4
C                                                                     C          OUTTAP.5
C     THIS SUBROUTINE WRITES THE MODEL OUTPUT TO DISK FOR USE         C          OUTTAP.6
C     IN POST-PROCESSING THE FORECAST                                 C          OUTTAP.7
C                                                                     C          OUTTAP.8
C                                                                     C          OUTTAP.9
C     IUTL : THE OUTPUT UNIT NUMBER FOR THIS GRID, THIS VALUE         C          OUTTAP.10
C            IS INCREMENTED IN OUTPUT                                 C          OUTTAP.11
C                                                                     C          OUTTAP.12
C     NESCOU: UNIQUE INTEGER DOMAIN IDENTIFIER (THE COARSEST GRID     C          OUTTAP.13
C             NESCOU = 1)                                             C          OUTTAP.14
C                                                                     C          OUTTAP.15
C     IX,JX,KZZ: DIMENSIONS OF CURRENT DOMAIN                         C          OUTTAP.16
C                                                                     C          OUTTAP.17
C                                                                     C          OUTTAP.18
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          OUTTAP.19
#     include <parame.incl>                                                      OUTTAP.20
#     include <addr0.incl>                                                       OUTTAP.21
#     include <point3d.incl>                                                     OUTTAP.22
#     include <navypb.incl>                                                      OUTTAP.23
#     include <point2d.incl>                                                     OUTTAP.24
#     include <various.incl>                                                     OUTTAP.25
#     include <nestl.incl>                                                       OUTTAP.26
#     include <hdrv3.incl>                                                       OUTTAP.27
#     include <chardate.incl>                                                    OUTTAP.28
#     include <nonhyd.incl>                                                      OUTTAP.29
#     include <nhcnst.incl>                                                      OUTTAP.30
#     include <fddagd.incl>                                                      OUTTAP.31
#     include <pmoist.incl>                                                      OUTTAP.32
#     include <varia.incl>                                                       OUTTAP.33
#     include <param2.incl>                                                      OUTTAP.34
#     include <parfdda.incl>                                                     OUTTAP.35
#     include <param3.incl>                                                      OUTTAP.36
#     include <soil.incl>                                                        OUTTAP.37
#     include <radiat.incl>                                                      OUTTAP.38
#     include <soilp.incl>                                                       OUTTAP.39
#     include <paramveg_USGS.incl>                                               19DEC02.525
#     include <rpstar.incl>                                                      OUTTAP.40
#     include <pbltke.incl>                                                      OUTTAP.41
#     include <landuse.incl>                                                     OUTTAP.42
#     include <fogstuf.incl>                                                     OUTTAP.43
#     include <parpx.incl>                                                       07NOV00.368
#     include <addrcu.incl>                                                      05DEC01.225
C                                                                                OUTTAP.44
      INTEGER IUTL, NESCOU, IX, JX, KZZ                                          OUTTAP.45
      INTEGER ITMP                                                               19DEC02.526
#ifdef MPP1                                                                      OUTTAP.46
      INTEGER ISAVEDOMSTAT, IDUMMY                                               OUTTAP.48
#endif                                                                           OUTTAP.49
C                                                                                OUTTAP.50
      CHARACTER*8 NAME8                                                          OUTTAP.51
      CHARACTER*6 NAME6                                                          OUTTAP.52
      CHARACTER*5 NAME5                                                          OUTTAP.53
      CHARACTER*19 CDATENEW,CDATEIN                                              OUTTAP.54
                                                                                 OUTTAP.55
      CHARACTER*13 FILENM2                                                       OUTTAP.56
      CHARACTER*17 FILENM                                                        05DEC01.226
C                                                                                OUTTAP.58
      INTEGER BH_FLAG, SH_FLAG, EOT_FLAG                                         OUTTAP.59
      INTEGER D3, D2, D1                                                         OUTTAP.60
      INTEGER IFWRTHDR, HEADER                                                   OUTTAP.61
C                                                                                OUTTAP.62
      INTEGER I,J,K,II,L,NP,NIV,NRV,LEV                                          OUTTAP.63
      INTEGER MOTHER                                                             OUTTAP.64
      REAL YISTART, XJSTART, YIEND, XJEND                                        OUTTAP.65
      INTEGER NY,NM,ND,NH,NI,NS                                                  OUTTAP.66
      LOGICAL DM_IONODE                                                          OUTTAP.67
      EXTERNAL DM_IONODE                                                         OUTTAP.68
C                                                                                OUTTAP.69
C----- ADDITIONAL STACK MEMORY FOR OUTPUTTING CORRECT SLAB                       OUTTAP.70
C      AND 3D ARRAY SIZES TO DISK                                                OUTTAP.71
C                                                                                OUTTAP.72
      INTEGER IN, ID3, ID2                                                       OUTTAP.73
      REAL  HSCRL(MIX,MJX)                                                       OUTTAP.74
      LOGICAL OPENED, LEXIST                                                     05DEC01.227
#ifndef MPP1                                                                     OUTTAP.76
      REAL  HDOTH(MIX*MJX*(MKX+1))                                               OUTTAP.77
      REAL  HDOTH2(MIX*MJX*(MKX+1))                                              19DEC02.527
#else                                                                            OUTTAP.78
      INTEGER MIX2,MJX2,MIJX,NSPG2,SCRSZ,HDOTHSZ,IOSCRSZ                         07NOV00.369
      PARAMETER(MIX2=MAX(MIX,MIX_G))                                             07NOV00.370
      PARAMETER(MJX2=MAX(MJX,MJX_G))                                             07NOV00.371
      PARAMETER(MIJX=MAX(MIX2,MJX2))                                             07NOV00.372
      PARAMETER(NSPG2=MAX(NSPGD,NSPGX))                                          07NOV00.373
      PARAMETER(SCRSZ = MIJX*NSPG2*(MKX+1))                                      07NOV00.374
      PARAMETER(HDOTHSZ = MIX2*MJX2*(MKX+1))                                     07NOV00.375
      PARAMETER(IOSCRSZ = MAX(SCRSZ,HDOTHSZ))                                    07NOV00.376
      REAL SCR(IOSCRSZ), HDOTH(IOSCRSZ)                                          07NOV00.377
      COMMON /OUTSCRATCH/ SCR                                                    07NOV00.378
      EQUIVALENCE (SCR,HDOTH)                                                    07NOV00.379
      REAL SCR2(IOSCRSZ), HDOTH2(IOSCRSZ)                                        19DEC02.528
      COMMON /OUTSCRATCH/ SCR2                                                   19DEC02.529
      EQUIVALENCE (SCR2,HDOTH2)                                                  19DEC02.530
C      INTEGER MIX2,MJX2                                                         07NOV00.380
C      PARAMETER(MIX2=MAX(MIX,MIX_G))                                            07NOV00.381
C      PARAMETER(MJX2=MAX(MJX,MJX_G))                                            07NOV00.382
C      REAL HDOTH(MIX2*MJX2*(MKX+1))                                             07NOV00.383
#endif                                                                           OUTTAP.80
      CHARACTER*8 MONALBNAMECHAR                                                 19DEC02.531
      INTEGER MONTHLOOP                                                          19DEC02.532
      CHARACTER *9 MONTHNAME(12)                                                 19DEC02.533
                                                                                 19DEC02.534
#ifdef MPP1                                                                      OUTTAP.81
      CALL MP_DOMAINSTAT(ISAVEDOMSTAT,IDUMMY)                                    OUTTAP.82
      CALL MP_INITDOMAIN(NESCOU)                                                 OUTTAP.83
#endif                                                                           OUTTAP.84
C                                                                                19DEC02.535
C---- NAMES OF THE MONTHS FOR CHAR STRING BUILDS FOR MONTHLY ALBEDO              19DEC02.536
C                                                                                19DEC02.537
      MONTHNAME( 1)(1:9) = 'JANUARY  '                                           19DEC02.538
      MONTHNAME( 2)(1:9) = 'FEBRUARY '                                           19DEC02.539
      MONTHNAME( 3)(1:9) = 'MARCH    '                                           19DEC02.540
      MONTHNAME( 4)(1:9) = 'APRIL    '                                           19DEC02.541
      MONTHNAME( 5)(1:9) = 'MAY      '                                           19DEC02.542
      MONTHNAME( 6)(1:9) = 'JUNE     '                                           19DEC02.543
      MONTHNAME( 7)(1:9) = 'JULY     '                                           19DEC02.544
      MONTHNAME( 8)(1:9) = 'AUGUST   '                                           19DEC02.545
      MONTHNAME( 9)(1:9) = 'SEPTEMBER'                                           19DEC02.546
      MONTHNAME(10)(1:9) = 'OCTOBER  '                                           19DEC02.547
      MONTHNAME(11)(1:9) = 'NOVEMBER '                                           19DEC02.548
      MONTHNAME(12)(1:9) = 'DECEMBER '                                           19DEC02.549
                                                                                 19DEC02.550
C                                                                                OUTTAP.85
C----- OPEN NEW UNIT NUMBER IF NECESSARY                                         OUTTAP.86
C                                                                                OUTTAP.87
      HEADER=0                                                                   OUTTAP.88
      IF (DM_IONODE()) THEN                                                      OUTTAP.89
        INQUIRE(IUTL, OPENED=OPENED)                                             OUTTAP.90
        IF (.NOT. OPENED) THEN                                                   OUTTAP.91
C                                                                                OUTTAP.92
C----- CHECK TO SEE IF OUTPUT FILES NEED TO BE DIVIDED                           OUTTAP.93
C                                                                                OUTTAP.94
          IF (IFREST) THEN                                                       05DEC01.228
              ITAPCNT = ITAPCNT+1                                                05DEC01.229
C             PRINT *,' RESTART OUTPUT FILE SUFFIX = ', ITAPCNT                  05DEC01.230
          ELSE IF (BUFFRQ.GE.TAPFRQ) THEN                                        05DEC01.231
            ITMP = MAX(NINT(BUFFRQ/(TAPFRQ*INCTAP(NESCOU))),1)                   07NOV00.384
            IF (MOD(NINT(XTIME/(TAPFRQ*INCTAP(NESCOU))),ITMP).EQ.1               07NOV00.385
     *        .OR. (NINT(BUFFRQ).EQ.NINT(TAPFRQ) .AND. XTIME.GT.0.)              07NOV00.386
     *        .OR. (NINT(BUFFRQ).LE.NINT(TAPFRQ*INCTAP(NESCOU))                  07NOV00.387
     *        .AND. XTIME.GT.0.)) THEN                                           07NOV00.388
              ITAPCNT = ITAPCNT+1                                                OUTTAP.99
C             PRINT *,' OUTPUT FILE SUFFIX = ', ITAPCNT,XTIME                    OUTTAP.100
            END IF                                                               OUTTAP.101
          END IF                                                                 OUTTAP.105
                                                                                 OUTTAP.106
          IF (BUFFRQ.GE.TAPFRQ .OR. IFREST) THEN                                 OUTTAP.107
            FILENM2 = 'MMOUT_DOMAIN'//CHAR(NESCOU+ICHAR('0'))                    OUTTAP.108
            IF(ITAPCNT.LE.99) THEN                                               05DEC01.232
            WRITE (FILENM,90) FILENM2,ITAPCNT                                    OUTTAP.109
            ELSE                                                                 05DEC01.233
            WRITE (FILENM,91) FILENM2,ITAPCNT                                    05DEC01.234
            ENDIF                                                                05DEC01.235
90          FORMAT(A13,'_',I2.2)                                                 OUTTAP.110
91          FORMAT(A13,'_',I3.3)                                                 05DEC01.236
            INQUIRE (FILE=FILENM, EXIST=LEXIST)                                  05DEC01.237
            IF (LEXIST .AND. IFREST) ITAPCNT = ITAPCNT+1                         19DEC02.551
            IF(ITAPCNT.LE.99) THEN                                               19DEC02.552
            WRITE (FILENM,90) FILENM2,ITAPCNT                                    05DEC01.239
            ELSE                                                                 19DEC02.553
            WRITE (FILENM,91) FILENM2,ITAPCNT                                    19DEC02.554
            ENDIF                                                                19DEC02.555
            OPEN(IUTL, FILE=FILENM,                                              OUTTAP.111
     &           FORM='UNFORMATTED',STATUS='UNKNOWN')                            OUTTAP.112
            PRINT 920, FILENM                                                    OUTTAP.113
          ELSE IF(BUFFRQ.LT.TAPFRQ) THEN                                         OUTTAP.114
            FILENM2 = 'MMOUT_DOMAIN'//CHAR(NESCOU+ICHAR('0'))                    OUTTAP.115
            OPEN(IUTL, FILE=FILENM2,                                             OUTTAP.116
     &           FORM='UNFORMATTED',STATUS='UNKNOWN')                            OUTTAP.117
            PRINT 910, FILENM2                                                   OUTTAP.118
          END IF                                                                 OUTTAP.119
          HEADER=1                                                               OUTTAP.120
                                                                                 OUTTAP.121
          IF (KTAU.EQ.0) THEN                                                    OUTTAP.122
            PRINT 930, XTIME,NESCOU                                              25JAN00.111
          END IF                                                                 OUTTAP.124
        END IF                                                                   OUTTAP.125
910   FORMAT(/6X,'*** OPEN OUTPUT FILE ',A13)                                    OUTTAP.126
920   FORMAT(/6X,'*** OPEN OUTPUT FILE ',A16)                                    OUTTAP.127
930   FORMAT(6X,'--- MODEL OUTPUT IS WRITTEN AT TIME = ',F10.2,                  OUTTAP.128
     *    ' MINUTES FOR DOMAIN ',I2)                                             OUTTAP.129
      END IF                                                                     OUTTAP.130
C                                                                                OUTTAP.131
C----- DEFINE HEADER FLAGS, AND FIELD DIMENSIONS                                 OUTTAP.132
C                                                                                OUTTAP.133
      BH_FLAG = 0                                                                OUTTAP.134
      SH_FLAG = 1                                                                OUTTAP.135
      EOT_FLAG = 2                                                               OUTTAP.136
      D3 = 3                                                                     OUTTAP.137
      D2 = 2                                                                     OUTTAP.138
      D1 = 1                                                                     OUTTAP.139
C                                                                                OUTTAP.140
C----- INITIALIZE HEADER ARRAYS                                                  OUTTAP.141
C                                                                                OUTTAP.142
      STAGGERING   = '    '                                                      OUTTAP.143
      ORDERING     = '    '                                                      OUTTAP.144
C     CURRENT_DATE = '                        '                                  OUTTAP.145
      NAME         = '         '                                                 OUTTAP.146
      UNITS        = '                         '                                 OUTTAP.147
      DESCRIPTION  = '                                              '            OUTTAP.148
C                                                                                OUTTAP.149
C----- CHECK IF BIG HEADER SHOULD BE WRITTEN                                     OUTTAP.150
C                                                                                OUTTAP.151
      IF (IFWRTHDR.EQ.1) HEADER=1                                                OUTTAP.152
C                                                                                OUTTAP.153
C----- WRITE BIG HEADER ONCE WHEN EACH DOMAIN STARTS                             OUTTAP.154
C                                                                                OUTTAP.155
      IF (HEADER.EQ.1) THEN                                                      OUTTAP.156
C                                                                                OUTTAP.157
C----- LOCAL COPY OF VARIABLES                                                   OUTTAP.158
C                                                                                OUTTAP.159
      DO 1000 NP=1,NUMPROGS                                                      OUTTAP.160
        DO 1000 NIV=1,NUMINT                                                     OUTTAP.161
          JBHI(NIV,NP)=BHI(NIV,NP)                                               OUTTAP.162
          JBHIC(NIV,NP)=BHIC(NIV,NP)                                             OUTTAP.163
 1000   CONTINUE                                                                 OUTTAP.164
                                                                                 OUTTAP.165
      DO 1100 NP=1,NUMPROGS                                                      OUTTAP.166
        DO 1100 NRV=1,NUMREAL                                                    OUTTAP.167
          JBHR(NRV,NP)=BHR(NRV,NP)                                               OUTTAP.168
          JBHRC(NRV,NP)=BHRC(NRV,NP)                                             OUTTAP.169
 1100   CONTINUE                                                                 OUTTAP.170
                                                                                 07NOV00.389
      IF (NESCOU.NE.1) THEN                                                      07NOV00.390
         DO NP=6,7                                                               07NOV00.391
         DO NIV=1,NUMINT                                                         07NOV00.392
            JBHI(NIV,NP)=-999                                                    07NOV00.393
            JBHIC(NIV,NP)( 1:80)=                                                07NOV00.394
     *                   '                                        '              07NOV00.395
     *               //  '                                        '              07NOV00.396
         END DO                                                                  07NOV00.397
         END DO                                                                  07NOV00.398
                                                                                 07NOV00.399
         DO NP=6,7                                                               07NOV00.400
         DO NRV=1,NUMREAL                                                        07NOV00.401
            JBHR(NRV,NP)=-999.                                                   07NOV00.402
            JBHRC(NRV,NP)( 1:80)=                                                07NOV00.403
     *                   '                                        '              07NOV00.404
     *               //  '                                        '              07NOV00.405
         END DO                                                                  07NOV00.406
         END DO                                                                  07NOV00.407
      END IF                                                                     07NOV00.408
C                                                                                OUTTAP.171
C----- CHANGES TO THE RECORD HEADER FOR THIS DATASET                             OUTTAP.172
C      MM5 DATA START AT PROGRAM NUMBER LOCATION 11 IN V3                        OUTTAP.173
C                                                                                OUTTAP.174
      JBHI(1,1)=11                                                               OUTTAP.175
      JBHIC( 1, 1)( 1:80)='OUTPUT FROM PROGRAM MM5 V3              '             25JAN00.112
     *                //  '                                        '             OUTTAP.177
C                                                                                OUTTAP.178
C----- MODIFY TERRAIN PART OF THE HEADER FOR DOMAIN INFO                         OUTTAP.179
C                                                                                OUTTAP.180
      JBHI(13,1)=NESCOU+(BHI(13,1)-1)                                            OUTTAP.181
C                                                                                OUTTAP.182
      IF(LEVIDN(NESCOU).EQ.0)THEN                                                OUTTAP.183
        JBHI(14,1)=BHI(14,1)                                                     OUTTAP.184
      ELSE                                                                       OUTTAP.185
        JBHI(14,1)=NUMNC(NESCOU)+(BHI(13,1)-1)                                   OUTTAP.186
      ENDIF                                                                      OUTTAP.187
C                                                                                OUTTAP.188
      JBHI(15,1)=LEVIDN(NESCOU)+BHI(15,1)                                        OUTTAP.189
C                                                                                OUTTAP.190
      JBHI(16,1)=IL                                                              OUTTAP.191
      JBHI(17,1)=JL                                                              OUTTAP.192
C                                                                                OUTTAP.193
      IF(LEVIDN(NESCOU).EQ.0)THEN                                                OUTTAP.194
        JBHI(18,1)=BHI(18,1)                                                     OUTTAP.195
        JBHI(19,1)=BHI(19,1)                                                     OUTTAP.196
      ELSE                                                                       OUTTAP.197
        JBHI(18,1)=NESTI(NESCOU)                                                 OUTTAP.198
        JBHI(19,1)=NESTJ(NESCOU)                                                 OUTTAP.199
      ENDIF                                                                      OUTTAP.200
C                                                                                OUTTAP.201
      JBHI(20,1)=(3**(LEVIDN(NESCOU)-LEVIDN(1)))*BHI(20,1)                       OUTTAP.202
C                                                                                OUTTAP.203
      IF(LEVIDN(NESCOU).EQ.0)THEN                                                OUTTAP.204
        JBHI(21,1)=BHI(21,1)                                                     OUTTAP.205
      ELSE                                                                       OUTTAP.206
        JBHI(21,1)=3**(LEVIDN(NESCOU)-LEVIDN(NUMNC(NESCOU)))                     OUTTAP.207
        JBHI(24,1)=2                                                             07NOV00.409
      ENDIF                                                                      OUTTAP.208
C                                                                                OUTTAP.209
      JBHR(9,1)=DX                                                               OUTTAP.210
C                                                                                OUTTAP.211
      YISTART=1.                                                                 OUTTAP.212
      XJSTART=1.                                                                 OUTTAP.213
      MOTHER=NESCOU                                                              OUTTAP.214
      DO 1300 LEV=1,LEVIDN(NESCOU)                                               OUTTAP.215
        YISTART=YISTART+FLOAT(NESTI(MOTHER)-1)/3**(LEVIDN(MOTHER)-               OUTTAP.216
     +          LEVIDN(1)-1)                                                     OUTTAP.217
        XJSTART=XJSTART+FLOAT(NESTJ(MOTHER)-1)/3**(LEVIDN(MOTHER)-               OUTTAP.218
     +          LEVIDN(1)-1)                                                     OUTTAP.219
        MOTHER=NUMNC(MOTHER)                                                     OUTTAP.220
 1300 CONTINUE                                                                   OUTTAP.221
      YIEND=YISTART+FLOAT(IL-1)/3**(LEVIDN(NESCOU)-LEVIDN(1))                    OUTTAP.222
      XJEND=XJSTART+FLOAT(JL-1)/3**(LEVIDN(NESCOU)-LEVIDN(1))                    OUTTAP.223
      YISTART=BHR(10,1)+(YISTART-1)/FLOAT(BHI(20,1))                             OUTTAP.224
      XJSTART=BHR(11,1)+(XJSTART-1)/FLOAT(BHI(20,1))                             OUTTAP.225
      YIEND=BHR(10,1)+(YIEND-1)/FLOAT(BHI(20,1))                                 OUTTAP.226
      XJEND=BHR(11,1)+(XJEND-1)/FLOAT(BHI(20,1))                                 OUTTAP.227
      JBHR(10,1)=YISTART                                                         OUTTAP.228
      JBHR(11,1)=XJSTART                                                         OUTTAP.229
      JBHR(12,1)=YIEND                                                           OUTTAP.230
      JBHR(13,1)=XJEND                                                           OUTTAP.231
C                                                                                OUTTAP.232
C---- NEW RECORD HEADER STUFF                                                    OUTTAP.233
C                                                                                OUTTAP.234
C     1. (X,11): VERSIONS, DATES, AND COMPILE/MEMORY OPTIONS                     OUTTAP.235
C                                                                                OUTTAP.236
      JBHI (2,11)=1                                                              OUTTAP.237
      JBHIC(2,11)( 1:80)='MM5 Version 3 MM5 System Format Edition '              OUTTAP.238
     *               //  'Number                                  '              OUTTAP.239
      JBHI (3,11)=7                                                              23DEC04.231
      JBHIC(3,11)( 1:80)='MM5 Program Version Number              '              OUTTAP.241
     *               //  '                                        '              OUTTAP.242
      JBHI (4,11)=4                                                              OUTTAP.243
      JBHIC(4,11)( 1:80)='MM5 Program Minor Revision Number       '              OUTTAP.244
     *               //  '                                        '              OUTTAP.245
      IF(NESCOU.EQ.1.AND.DM_IONODE()) PRINT 950,JBHI(3,11),JBHI(4,11)            25JAN00.114
950   FORMAT(6X,'*** YOU ARE RUNNING MM5 VERSION 3-',I1,'-',I1/)                 25JAN00.115
C                                                                                OUTTAP.246
C---- STARTING DATE STUFF                                                        OUTTAP.247
C                                                                                OUTTAP.248
      CALL SPLIT_DATE_CHAR( CDATE, NY , NM , ND , NH , NI , NS )                 OUTTAP.249
                                                                                 OUTTAP.250
      JBHI(5,11)=NY                                                              OUTTAP.251
      JBHIC(5,11)( 1:80)='FOUR-DIGIT YEAR OF START TIME           '              OUTTAP.252
     *               //  '                                        '              OUTTAP.253
      JBHI(6,11)=NM                                                              OUTTAP.254
      JBHIC(6,11)( 1:80)='INTEGER MONTH OF START TIME             '              OUTTAP.255
     *               //  '                                        '              OUTTAP.256
      JBHI(7,11)=ND                                                              OUTTAP.257
      JBHIC(7,11)( 1:80)='DAY OF THE MONTH OF THE START TIME      '              OUTTAP.258
     *               //  '                                        '              24SEP99.94
      JBHI(8,11)=NH                                                              OUTTAP.260
      JBHIC(8,11)( 1:80)='HOUR OF THE START TIME                  '              OUTTAP.261
     *               //  '                                        '              OUTTAP.262
      JBHI(9,11)=NI                                                              OUTTAP.263
      JBHIC(9,11)( 1:80)='MINUTES OF THE START TIME               '              OUTTAP.264
     *               //  '                                        '              OUTTAP.265
      JBHI(10,11)=NS                                                             OUTTAP.266
      JBHIC(10,11)(1:80)='SECONDS OF THE START TIME               '              OUTTAP.267
     *               //  '                                        '              OUTTAP.268
      JBHI(11,11)=0                                                              OUTTAP.269
      JBHIC(11,11)(1:80)='TEN THOUSANDTHS OF A SECOND OF THE START'              OUTTAP.270
     *               //  ' TIME                                   '              OUTTAP.271
C                                                                                OUTTAP.272
      JBHI(12,11)=MKX                                                            OUTTAP.273
      JBHIC(12,11)(1:80)='MKX: NUMBER OF LAYERS IN MM5 OUTPUT     '              OUTTAP.274
     *               //  '                                        '              OUTTAP.275
C                                                                                OUTTAP.276
C---- MEMORY AVAILABLE AND COMPILE OPTIONS                                       OUTTAP.277
C                                                                                OUTTAP.278
      JBHI(13,11)=IFDDAG                                                         OUTTAP.279
      JBHIC(13,11)( 1:80)='IFDDAG: 1=GRIDDED FDDA OPTION COMPILED, '             OUTTAP.280
     *                //  '0=NOT COMPILED                          '             OUTTAP.281
      JBHI(14,11)=IFDDAO                                                         OUTTAP.282
      JBHIC(14,11)( 1:80)='IFDDAO: 1=OBS FDDA OPTION COMPILED, 0=NO'             OUTTAP.283
     *                //  'T COMPILED                              '             OUTTAP.284
      JBHI(15,11)=INAV                                                           OUTTAP.285
      JBHIC(15,11)( 1:80)='INAV:   1=TKE ARRAY PRESENT, 0=NOT PRESE'             OUTTAP.286
     *                //  'NT                                      '             OUTTAP.287
      JBHI(16,11)=INAV2                                                          OUTTAP.288
      JBHIC(16,11)( 1:80)='INAV2:  1=TKE ARRAY PRESENT, 0=NOT PRESE'             OUTTAP.289
     *                //  'NT                                      '             OUTTAP.290
      JBHI(17,11)=INAV3                                                          OUTTAP.291
      JBHIC(17,11)( 1:80)='INAV3:  1=TKE ARRAY PRESENT, 0=NOT PRESE'             OUTTAP.292
     *                //  'NT                                      '             OUTTAP.293
      JBHI(18,11)=IICE                                                           OUTTAP.294
      JBHIC(18,11)( 1:80)='IICE:   1=CLOUD ICE AND SNOW ARRAYS PRES'             OUTTAP.295
     *                //  'ENT, 0=NOT PRESENT                      '             OUTTAP.296
      JBHI(19,11)=IICEG                                                          OUTTAP.297
      JBHIC(19,11)( 1:80)='IICEG:  1=GRAUPEL AND NUMBER CONC ARRAYS'             OUTTAP.298
     *                //  'PRESENT, 0=NOT PRESENT                  '             OUTTAP.299
      JBHI(20,11)=IEXMS                                                          OUTTAP.300
      JBHIC(20,11)( 1:80)='IEXMS:  1=CLOUD WATER AND RAIN WATER ARR'             OUTTAP.301
     *                //  'AYS PRESENT, 0=NOT PRESENT              '             OUTTAP.302
      JBHI(21,11)=IKFFC                                                          OUTTAP.303
      JBHIC(21,11)( 1:80)='IKFFC:  1=KF AND/OR FC ARRAYS PRESENT, 0'             OUTTAP.304
     *                //  '=NOT PRESENT                            '             OUTTAP.305
      JBHI(22,11)=IARASC                                                         OUTTAP.306
      JBHIC(22,11)( 1:80)='IARASC: 1=ARAKAWA-SCHUBERT ARRAYS PRESEN'             OUTTAP.307
     *                //  'T, 0=NOT PRESENT                        '             OUTTAP.308
      JBHI(23,11)=IRDDIM                                                         OUTTAP.309
      JBHIC(23,11)( 1:80)='IRDDIM: 1=ATMOSPHERIC RADIATION TENDENCY'             OUTTAP.310
     *                //  ' ARRAY PRESENT, 0=NOT PRESENT           '             OUTTAP.311
      JBHI(24,11)=ISLDIM                                                         OUTTAP.312
      JBHIC(24,11)( 1:80)='ISLDIM: 1=5-LAYER SOIL MODEL ARRAYS PRES'             OUTTAP.313
     *                //  'ENT, 0=NOT PRESENT                      '             OUTTAP.314
      JBHI(25,11)=ILDDIM                                                         OUTTAP.315
      JBHIC(25,11)( 1:80)='ILDDIM: 1=LAND-SURFACE MODEL ARRAYS PRES'             OUTTAP.316
     *                //  'ENT, 0=NOT PRESENT                      '             OUTTAP.317
C                                                                                OUTTAP.318
      JBHR(1,11)=FLOAT(NINT(TAPFRQ*60.+.1))                                      OUTTAP.319
      JBHRC( 1,11)( 1:80)='INTTIM: TIME DIFFERENCE IN MODEL OUTPUT '             OUTTAP.320
     *                //  '                                        '             OUTTAP.321
C                                                                                OUTTAP.322
C     2. (X,12): VARIABLES FROM NAMELIST OPARAM                                  OUTTAP.323
C                                                                                OUTTAP.324
      JBHR(1,12)=TIMAX                                                           OUTTAP.325
C     JBHR(2,12)=BHR(2,12)                                                       OUTTAP.326
      JBHRC(1,12)='TIMAX:   SIMULATION END TIME (MINUTES)  '                     24SEP99.95
     *     //     '                                        '                     OUTTAP.328
      JBHRC(2,12)='TISTEP:  COARSE-DOMAIN TIME STEP IN SECO'                     OUTTAP.329
     *     //     'NDS                                     '                     OUTTAP.330
      IF(IFREST)THEN                                                             OUTTAP.331
        JBHI(1,12)=1                                                             OUTTAP.332
        JBHI(2,12)=IXTIMR                                                        OUTTAP.333
      ELSE                                                                       OUTTAP.334
        JBHI(1,12)=0                                                             OUTTAP.335
        JBHI(2,12)=0                                                             OUTTAP.336
      ENDIF                                                                      OUTTAP.337
      JBHIC(1,12)='IFREST:  1 = RESTARTED JOB;  0 = NOT A R'                     OUTTAP.338
     *      //    'ESTARTED JOB                            '                     OUTTAP.339
      JBHIC(2,12)='IXTIMR:  TIME OF RESTART                '                     OUTTAP.340
     *      //    '                                        '                     OUTTAP.341
      IF(IFSAVE)THEN                                                             OUTTAP.342
        JBHI(3,12)=1                                                             OUTTAP.343
        JBHR(3,12)=SAVFRQ                                                        OUTTAP.344
      ELSE                                                                       OUTTAP.345
        JBHI(3,12)=0                                                             OUTTAP.346
        JBHR(3,12)=0.                                                            OUTTAP.347
      ENDIF                                                                      OUTTAP.348
      JBHIC(3,12)='IFSAVE:  1 = DATA SAVED FOR RESTART;  0 '                     OUTTAP.349
     *     //     '= DATA NOT SAVED FOR RESTART            '                     OUTTAP.350
      JBHRC(3,12)='SAVFRQ:  TIME INTERVAL (MINUTES) THAT DA'                     OUTTAP.351
     *     //     'TA WERE SAVED FOR RESTART               '                     OUTTAP.352
      JBHI(4,12)=IFTAPE                                                          OUTTAP.353
      JBHIC(4,12)='IFTAPE:  1 = OUTPUT DATA SAVED FOR GRIN;'                     OUTTAP.354
     *     //     '  0 = NO OUTPUT FOR GRIN                '                     OUTTAP.355
      IF(IFTAPE.EQ.1)THEN                                                        OUTTAP.356
        JBHR(4,12)=TAPFRQ*INCTAP(NESCOU)                                         OUTTAP.357
      ELSE                                                                       OUTTAP.358
        JBHR(4,12)=0.                                                            OUTTAP.359
      ENDIF                                                                      OUTTAP.360
      JBHRC(4,12)='TAPFRQ:  TIME INTERVAL (MINUTES) THAT DA'                     OUTTAP.361
     *     //     'TA WERE SAVED FOR GRIN                  '                     OUTTAP.362
      JBHR(5,12)=BUFFRQ                                                          OUTTAP.363
      JBHRC(5,12)( 1:80)='BUFFRQ:  TIME FREQ USED TO SPLIT OUTPUT '              24SEP99.96
     *               //  'FILES (MINUTES). IGNORED IF < TAPFRQ    '              24SEP99.97
      JBHI(5,12)=MASCHK                                                          OUTTAP.366
      JBHIC(5,12)='MASCHK:  MASS CONSERVATION CHECK FREQUEN'                     OUTTAP.367
     *     //     'CY (MINUTES)                            '                     OUTTAP.368
C                                                                                OUTTAP.369
C---- (X,13): VARIABLES FROM NAMELIST LPARAM                                     OUTTAP.370
C                                                                                OUTTAP.371
      JBHI(1,13) =IFRAD                                                          OUTTAP.372
      JBHIC(1,13)='IFRAD:   0=NO RADIATIVE COOLING; 1=SIMPL'                     24SEP99.98
     *     //     'E; 2=CLOUD RADIATION; 3=CCM2; 4=RRTMLW  '                     25JAN00.116
      JBHR(1,13) =RADFRQ                                                         OUTTAP.375
      JBHRC(1,13)='RADFRQ:  FREQUENCY THAT SOLAR RADIATION '                     OUTTAP.376
     *     //     'IS COMPUTED (MINUTES)                   '                     OUTTAP.377
      JBHI(2,13) =ICUPA(NESCOU)                                                  OUTTAP.378
      JBHIC(2,13)='ICUPA:   1-8/NO/ANTHES-KUO/GRELL/A-S/F-C'                     05DEC01.241
     *     //     '/KAIN-FRITSCH/BETTS-MILLER/KAIN-FRITSCH2'                     05DEC01.242
      JBHI(3,13) =IMPHYS(NESCOU)                                                 OUTTAP.381
      JBHIC(3,13)='IMPHYS:  1-8 DRY/STABLE/WARM RAIN/SIMPLE'                     OUTTAP.382
     *     //     ' ICE/REISNER1/GODDARD/REISNER2/SCHULTZ  '                     OUTTAP.383
      JBHI(4,13) =IBLTYP(NESCOU)                                                 OUTTAP.384
      JBHIC(4,13)='IBLTYP:  0=FRICTIONLESS; 1=BULK;2=BLACKA'                     07NOV00.411
     *     //     'DAR;3=B-T;4=ETA M-Y;5=MRF;6=G-S;7=PX    '                     07NOV00.412
      JBHI(5,13) =ISOIL                                                          OUTTAP.387
      JBHIC(5,13)='ISOIL:   0=BLACKADAR SLAB MODEL, 1=MULTI'                     24SEP99.104
     *     //     '-LAYER, 2=LAND-SURFACE SCHEME           '                     24SEP99.105
      JBHI(6,13) =ISHALLO(NESCOU)                                                OUTTAP.390
      JBHIC(6,13)='ISHALLO: 1=SHALLOW CONVECTION SCHEME USE'                     24SEP99.106
     *     //     'D; 0=SHALLOW CONVECTION SCHEME NOT USED '                     24SEP99.107
      JBHI(7,13) =IMVDIF                                                         OUTTAP.393
      JBHIC(7,13)='IMVDIF:  1=MOIST-ADIABATIC VERTICAL DIFF'                     OUTTAP.394
     *     //     'USION IN CLOUDS INCLUDED; 0=NOT INCLUDED'                     OUTTAP.395
      JBHI(8,13) =IVQADV                                                         OUTTAP.396
      JBHIC(8,13)='IVQADV:  =0, LOG, =1, LINEAR INTERPOLATI'                     OUTTAP.397
     *     //     'ON OF MOISTURE IN VERTICAL ADVECTION    '                     OUTTAP.398
      JBHI(9,13) =IVTADV                                                         OUTTAP.399
      JBHIC(9,13)='IVTADV:  =0, THETA, =1, LINEAR INTERPOLA'                     OUTTAP.400
     *     //     'TION OF TEMPERATURE IN VERTICAL ADVECTN '                     OUTTAP.401
      JBHI(10,13) =ITHADV                                                        OUTTAP.402
      JBHIC(10,13)='ITHADV:  =0, STANDARD, = 1, USING POTENT'                    24SEP99.108
     *     //      'IAL TEMPERATURE IN TEMP ADVECTION       '                    24SEP99.109
      JBHI(11,13) =ITPDIF                                                        OUTTAP.405
      JBHIC(11,13)='ITPDIF:  =1, HORIZONTAL DIFFUSION OF PER'                    OUTTAP.406
     *     //      'TURBATION TEMPERATURE ONLY, =0,  FULL T '                    OUTTAP.407
      JBHI(12,13) =ICOR3D                                                        OUTTAP.408
      JBHIC(12,13)='ICOR3D:  1=FULL CORIOLIS WITH VERTICAL C'                    OUTTAP.409
     *     //      'OMPONENT; 0=VERTICAL COMPONENT NEGLECTED'                    OUTTAP.410
      JBHI(13,13) =IFUPR                                                         OUTTAP.411
      JBHIC(13,13)='IFUPR:   1= UPPER RADIATIVE BOUNDARY CON'                    24SEP99.110
     *     //      'DITION USED;  0= U.R.B.C. NOT USED      '                    24SEP99.111
      JBHI(14,13)=IFDRY                                                          OUTTAP.414
      JBHIC(14,13)='IFDRY:   1=FAKE DRY RUN;  2=NOT A FAKE D'                    24SEP99.112
     *      //     'RY RUN                                  '                    24SEP99.113
      JBHI(15,13)=IBOUDY(NESCOU)                                                 OUTTAP.417
      JBHIC(15,13)='IBOUDY:  SPECIFIED 0=FIXED, 2=TIME DEPEN'                    24SEP99.114
     *      //     'DENT, 3=RELAXATION ZONE/IO DEPENDENT    '                    24SEP99.115
      JBHI(16,13)=IFSNOW(NESCOU)                                                 OUTTAP.420
      JBHIC(16,13)='IFSNOW:  1=SNOW-COVER EFFECTS CONSIDERED'                    OUTTAP.421
     *      //     '0=NOT CONSIDERED                        '                    OUTTAP.422
      JBHI(17,13)=ISFFLX(NESCOU)                                                 OUTTAP.423
      JBHIC(17,13)='ISSFLX:  1=SURFACE HEAT AND MOISTURE FLU'                    OUTTAP.424
     *      //     'XES CALCULATED; 0=NOT CALCULATED        '                    OUTTAP.425
      JBHI(18,13)=ITGFLG(NESCOU)                                                 OUTTAP.426
      JBHIC(18,13)='ITGFLG:  1=TG CALCULATED FROM BUDGET; 2='                    OUTTAP.427
     *      //     'SINUSOIDAL FUNCTION; 3=SPECIFIED CONSTS '                    OUTTAP.428
      JBHI(19,13)=ISFPAR(NESCOU)                                                 OUTTAP.429
      JBHIC(19,13)='ISFPAR:  1=SFC/LAND-USE PARAMETERS VARIA'                    OUTTAP.430
     *      //     'BLE; 0=SFC/LAND-USE PARAMETERS CONSTANT '                    OUTTAP.431
      JBHI(20,13)=ICLOUD(NESCOU)                                                 OUTTAP.432
      JBHIC(20,13)='ICLOUD:  0=CLOUD EFFECTS NOT CONSIDERED;'                    05DEC01.243
     *      //     '1,2=CLOUD EFF THR CLOUD WATER/ICE OR RH '                    05DEC01.244
      JBHI(21,13)=ICDCON(NESCOU)                                                 OUTTAP.435
      JBHIC(21,13)='ICDCON:  1=DRAG COEFFS ARE CONSTANT F(TE'                    OUTTAP.436
     *      //     'R-ELEV) IN BULK PBL;  0=COEFFS VARIABLE '                    OUTTAP.437
      JBHI(22,13)=IVMIXM(NESCOU)                                                 OUTTAP.438
      JBHIC(22,13)='IVMIXM:  1=VERTICAL MIXING OF MOMENTUM C'                    OUTTAP.439
     *      //     'ONSIDERED; 0=NOT CONSIDERED             '                    OUTTAP.440
      JBHI(23,13)=IEVAP(NESCOU)                                                  OUTTAP.441
      JBHIC(23,13)='IEVAP:  -1=EVAP OF RAIN NOT CONSIDERED; '                    OUTTAP.442
     *      //     '0=EVAP NOT CONSIDERED; 1=EVAP CONSIDERED'                    OUTTAP.443
      JBHI(24,13)=ICUSTB                                                         OUTTAP.444
      JBHIC(24,13)='ICUSTB:  1=STABILITY CHECK IN CUMULUS SC'                    OUTTAP.445
     *      //     'HEME ACTIVATED; 0 = NOT ACTIVATED       '                    OUTTAP.446
      JBHI(25,13)=IMOIAV(NESCOU)                                                 OUTTAP.447
      JBHIC(25,13)='IMOIAV: 1=USE BUCKET MODEL W/O EXTRA INP'                    05DEC01.245
     *      //     'UT; 2=USE BUCKET MODEL W SOIL MOIS INPUT'                    05DEC01.246
      JBHI(26,13)=IBMOIST                                                        OUTTAP.450
      JBHIC(26,13)='IBMOIST: 1=BOUNDARY AND INITIAL WATER/IC'                    24SEP99.116
     *      //     'E SPECIFIED; 0=NOT SPECIFIED            '                    24SEP99.117
      JBHI(27,13)=IFOGSW(NESCOU)                                                 OUTTAP.453
      JBHIC(27,13)='IFOGMD:  1=FOG MODEL IS USED            '                    OUTTAP.454
     *     //      '0=FOG MODEL IS NOT USED                 '                    OUTTAP.455
      JBHI(28,13)=ISSTVAR                                                        07NOV00.413
      JBHIC(28,13)='ISSTVAR: 1=SST VARYING IN TIME          '                    07NOV00.414
     *     //      '0=SST DOES NOT VARY IN TIME             '                    07NOV00.415
      JBHI(28,14)=IZ0TOPT                                                        05DEC01.247
      JBHIC(28,14)='IZ0TOPT: 0=ORIG THERMAL ROUGHNESS FORM.;'                    05DEC01.248
     *     //      ' 1=GARRATT; 2=ZILITINKEVICH (IBLTYP=2,5)'                    05DEC01.249
      JBHR(2,13)=HYDPRE(NESCOU)                                                  OUTTAP.456
      JBHRC(2,13)='HYDPRE:  1.0 = WATER LOADING EFFECTS IN '                     OUTTAP.457
     *      //    'HYDROSTATIC EQN; 0.0=NO WATER LOADING   '                     OUTTAP.458
C                                                                                OUTTAP.459
C---- (X,14): VARIABLES FROM NAMELIST NPARAM (NEW IN V3)                         OUTTAP.460
C                                                                                OUTTAP.461
      JBHI(1,14) =IOVERW(NESCOU)                                                 OUTTAP.462
      JBHIC(1,14)='IOVERW:  1=NEST INITIAL CONDITIONS OVERW'                     OUTTAP.463
     *   //       'RITTEN WITH USER ANALYSIS; 0=INTERPOLATE'                     OUTTAP.464
      JBHI(2,14) =IFEED                                                          OUTTAP.465
      JBHIC(2,14)='IFEED:   0=NO FB; 1=9-PT FB; 1-PT FD/2=N'                     24SEP99.118
     *   //       'O SMOOTH; 3=LIGHT SMOOTH; 4=HEAVY SMOOTH'                     OUTTAP.467
      JBHI(3,14)=MAXMV                                                           OUTTAP.468
      JBHIC(3,14)='MAXMV:   MAXIMUM NUMBER OF MOVES ALLOWED'                     24SEP99.119
     *     //     '                                        '                     OUTTAP.470
      IF(MAXMV.GT.33)PRINT*,'INFORMATION ABOUT MOVES AFTER MOVE 33 ',            OUTTAP.471
     *  'ARE NOT INCLUDED IN THE HEADER OF THE OUTPUT FILE'                      OUTTAP.472
                                                                                 OUTTAP.473
      JBHI(4,14) =IMOVE(NESCOU)                                                  OUTTAP.474
      JBHIC(4,14)='IMOVE:   1=THIS DOMAIN MOVES;  0=THIS DO'                     24SEP99.120
     *   //       'MAIN DOES NOT MOVE                      '                     24SEP99.121
      JBHI(5,14) =IMOVCO(NESCOU)                                                 OUTTAP.477
      JBHIC(5,14)='IMOVCO:  MOVE NUMBER                    '                     OUTTAP.478
     *   //       '                                        '                     OUTTAP.479
      DO II=1,MIN(33,MAXMV)                                                      OUTTAP.480
        JBHI(5+II,14)=IMOVET(NESCOU,II)                                          OUTTAP.481
         JBHIC(5+II,14)='IMOVET:  TIME OF MOVE (MINUTES FROM STAR'               OUTTAP.482
     *   //             'T OF FORECAST)                          '               OUTTAP.483
        JBHI(5+MAXMV+II,14)=IMOVEI(NESCOU,II)                                    OUTTAP.484
         JBHIC(5+MAXMV+II,14)='IMOVEI:  I INCREMENT OF MOVE          '           OUTTAP.485
     *   //               '                                          '           OUTTAP.486
        JBHI(5+2*MAXMV+II,14)=IMOVEJ(NESCOU,II)                                  OUTTAP.487
         JBHIC(5+2*MAXMV+II,14)='IMOVEJ:  J INCREMENT OF MOVE        '           OUTTAP.488
     *   //             '                                            '           OUTTAP.489
      ENDDO                                                                      OUTTAP.490
                                                                                 OUTTAP.491
      JBHR(1,14) =XSTNES(NESCOU)                                                 OUTTAP.492
      JBHRC(1,14)='XSTNES:  STARTING TIME (MINUTES) OF DOMA'                     OUTTAP.493
     *   //       'IN                                      '                     OUTTAP.494
      JBHR(2,14) =XENNES(NESCOU)                                                 OUTTAP.495
      JBHRC(2,14)='XENNES:  ENDING TIME (MINUTES) OF DOMAIN'                     OUTTAP.496
     *   //       '                                        '                     OUTTAP.497
C                                                                                OUTTAP.498
C---- (X,15): VARIABLES FROM NAMELIST PPARAM                                     OUTTAP.499
C                                                                                OUTTAP.500
      JBHR(1,15)=ZZLND                                                           OUTTAP.501
      JBHRC(1,15)='ZZLND:   ROUGHNESS LENGTH (M) OVER LAND '                     24SEP99.122
     *     //     '(WHEN ISFPAR=1)                         '                     24SEP99.123
      JBHR(2,15)=ZZWTR                                                           OUTTAP.504
      JBHRC(2,15)='ZZWTR:   ROUGHNESS LENGTH (M) OVER WATER'                     24SEP99.124
     *     //     ' (WHEN ISFPAR=1)                        '                     24SEP99.125
      JBHR(3,15)=ALBLND                                                          OUTTAP.507
      JBHRC(3,15)='ALBLND:  ALBEDO OVER LAND (WHEN ISFPAR=1'                     OUTTAP.508
     *     //     ')                                       '                     OUTTAP.509
      JBHR(4,15)=THINLD                                                          OUTTAP.510
      JBHRC(4,15)='THINLD:  THERMAL INERTIA OVER LAND (WHEN'                     OUTTAP.511
     *     //     ' ISFPAR=1)                              '                     OUTTAP.512
      JBHR(5,15)=XMAVA                                                           OUTTAP.513
      JBHRC(5,15)='XMAVA:   MOISTURE AVAILABLITY OVER LAND '                     24SEP99.126
     *     //     'AS A FRACTION OF 1 (WHEN ISFPAR=1)      '                     24SEP99.127
      JBHR(6,15)=CONF                                                            OUTTAP.516
      JBHRC(6,15)='CONF:    CONDENSATION THRESHOLD         '                     24SEP99.128
     *     //     '                                        '                     OUTTAP.518
      JBHR(7,15)=1                                                               OUTTAP.519
      JBHRC(7,15)='other namelist variables                '                     OUTTAP.520
     *     //     '                                        '                     OUTTAP.521
C                                                                                OUTTAP.522
C---- (X,16): VARIABLES FROM NAMELIST FPARAM                                     OUTTAP.523
C                                                                                OUTTAP.524
      JBHR(1,16)=FDASTA(NESCOU)                                                  OUTTAP.525
      JBHRC(1,16)='FDASTA:  STARTING TIME FOR FDDA (MINUTES'                     OUTTAP.526
     *   //       ')                                       '                     OUTTAP.527
      JBHR(2,16)=FDAEND(NESCOU)                                                  OUTTAP.528
      JBHRC(2,16)='FDAEND:  ENDING TIME FOR FDDA (MINUTES) '                     OUTTAP.529
     *   //       '                                        '                     OUTTAP.530
      JBHR(3,16)=DIFTIM(NESCOU,1)                                                OUTTAP.531
      JBHRC(3,16)='DIFTIM(3D):  TIME INTERVAL (MINUTES) BET'                     OUTTAP.532
     *   //       'WEEN 3-D ANALYSES FOR NUDGING           '                     OUTTAP.533
      JBHR(4,16)=DIFTIM(NESCOU,2)                                                OUTTAP.534
      JBHRC(4,16)='DIFTIM(SFC): TIME INTERVAL (MINUTES) BET'                     OUTTAP.535
     *   //       'WEEN SURFACE ANALYSES FOR NUDGING       '                     OUTTAP.536
      JBHR(5,16)=GV(NESCOU,1)                                                    OUTTAP.537
      JBHRC(5,16)='GV(3D):  NUDGING COEFFICIENT FOR 3-D ANA'                     OUTTAP.538
     *   //       'LYSIS FDDA OF WINDS                     '                     OUTTAP.539
      JBHR(6,16)=GT(NESCOU,2)                                                    OUTTAP.540
      JBHRC(6,16)='GT(3D):  NUDGING COEFFICIENT FOR 3-D ANA'                     OUTTAP.541
     *   //       'LYSIS FDDA OF TEMP                      '                     OUTTAP.542
      JBHR(7,16)=GQ(NESCOU,1)                                                    OUTTAP.543
      JBHRC(7,16)='GQ(3D):  NUDGING COEFFICIENT FOR 3-D ANA'                     OUTTAP.544
     *   //       'LYSIS FDDA OF MIXING RATIO              '                     OUTTAP.545
      JBHR(8,16)=GR(NESCOU)                                                      OUTTAP.546
      JBHRC(8,16)='GR(3D):  NUDGING COEFFICIENT FOR 3-D ANA'                     OUTTAP.547
     *   //       'LYSIS FDDA OF ROTATIONAL WIND COMPONENT '                     OUTTAP.548
      JBHR(9,16)=GV(NESCOU,2)                                                    OUTTAP.549
      JBHRC(9,16)='GV(SFC):  NUDGING COEFFICIENT FOR SFC AN'                     OUTTAP.550
     *   //       'ALYSIS FDDA OF WINDS                    '                     OUTTAP.551
      JBHR(10,16)=GT(NESCOU,2)                                                   OUTTAP.552
      JBHRC(10,16)='GT(SFC):  NUDGING COEFFICIENT FOR SFC AN'                    OUTTAP.553
     *    //       'ALYSIS FDDA OF TEMP                     '                    OUTTAP.554
      JBHR(11,16)=GQ(NESCOU,2)                                                   OUTTAP.555
      JBHRC(11,16)='GQ(SFC):  NUDGING COEFFICIENT FOR SFC AN'                    OUTTAP.556
     *    //       'ALYSIS FDDA OF MIXING RATIO             '                    OUTTAP.557
      JBHI(1,16)=I4D(NESCOU,1)                                                   OUTTAP.558
      JBHIC(1,16)='I4D(3D):  1=3-D ANALYSIS NUDGING; 0=NO 3'                     OUTTAP.559
     *   //       '-D ANALYSIS NUDGING                     '                     OUTTAP.560
      JBHI(2,16)=IWIND(NESCOU,1)                                                 OUTTAP.561
      JBHIC(2,16)='IWIND(3D):  1=3-D ANALYSIS NUDGING OF WI'                     OUTTAP.562
     *   //       'ND; 0=NO 3-D ANALYSIS NUDGING OF WIND   '                     OUTTAP.563
      JBHI(3,16)=ITEMP(NESCOU,1)                                                 OUTTAP.564
      JBHIC(3,16)='ITEMP(3D):  1=3-D ANALYSIS NUDGING OF TE'                     OUTTAP.565
     *   //       'MP; 0=NO 3-D ANALYSIS NUDGING OF TEMP   '                     OUTTAP.566
      JBHI(4,16)=IMOIS(NESCOU,1)                                                 OUTTAP.567
      JBHIC(4,16)='IMOIS(3D):  1=3-D ANALYSIS NUDGING MOIST'                     OUTTAP.568
     *   //       'URE: 0=NO 3-D ANALYSIS NUDGING MOISTURE '                     OUTTAP.569
      JBHI(5,16)=IROT(NESCOU)                                                    OUTTAP.570
      JBHIC(5,16)='IROT:  1=3-D ANALYSIS NUDGING OF ROTATIO'                     OUTTAP.571
     *   //       'NAL WIND; 0 = ROT. WIND NOT NUDGED      '                     OUTTAP.572
      JBHI(6,16)=I4D(NESCOU,2)                                                   OUTTAP.573
      JBHIC(6,16)='I4D(SFC):  1=SFC ANALYSIS NUDGING; 0=NO '                     OUTTAP.574
     *   //       'SFC ANALYSIS NUDGING                    '                     OUTTAP.575
      JBHI(7,16)=IWIND(NESCOU,2)                                                 OUTTAP.576
      JBHIC(7,16)='IWIND(SFC): 1=SFC ANALYSIS NUDGING OF WI'                     OUTTAP.577
     *   //       'ND; 0=NO SFC ANALYSIS NUDGING OF WIND   '                     OUTTAP.578
      JBHI(8,16)=ITEMP(NESCOU,2)                                                 OUTTAP.579
      JBHIC(8,16)='ITEMP(SFC): 1=SFC ANALYSIS NUDGING OF TE'                     OUTTAP.580
     *   //       'MP; 0=NO SFC ANALYSIS NUDGING OF TEMP   '                     OUTTAP.581
      JBHI(9,16)=IMOIS(NESCOU,2)                                                 OUTTAP.582
      JBHIC(9,16)='IMOIS(SFC): 1=SFC ANALYSIS NUDGING MOIST'                     OUTTAP.583
     *   //       'URE: 0=NO SFC ANALYSIS NUDGING MOISTURE '                     OUTTAP.584
      JBHI(10,16)=INONBL(NESCOU,1)                                               OUTTAP.585
      JBHIC(10,16)='INONBL(U):  0 = B.L. NUDGING OF U INCLUD'                    OUTTAP.586
     *    //       'ED; 1 = B.L. NUDGING OF U EXCLUDED      '                    OUTTAP.587
      JBHI(11,16)=INONBL(NESCOU,2)                                               OUTTAP.588
      JBHIC(11,16)='INONBL(V):  0 = B.L. NUDGING OF V INCLUD'                    OUTTAP.589
     *    //       'ED; 1 = B.L. NUDGING OF V EXCLUDED      '                    OUTTAP.590
      JBHI(12,16)=INONBL(NESCOU,3)                                               OUTTAP.591
      JBHIC(12,16)='INONBL(T):  0 = B.L. NUDGING OF T INCLUD'                    OUTTAP.592
     *    //       'ED; 1 = B.L. NUDGING OF T EXCLUDED      '                    OUTTAP.593
      JBHI(13,16)=INONBL(NESCOU,4)                                               OUTTAP.594
      JBHIC(13,16)='INONBL(M.R.):  0 = B.L. NUDGING OF M.R. '                    OUTTAP.595
     *    //       'INCLUDED; 1 = B.L. NUDGING M.R. EXCLUDED'                    OUTTAP.596
      JBHR(12,16)=RINBLW                                                         OUTTAP.597
      JBHRC(12,16)='RINBLW:  RADIUS OF INFLUENCE FOR SURFACE'                    OUTTAP.598
     *    //       'ANALYSIS NUDGING                        '                    OUTTAP.599
      JBHI(14,16)=I4DI(NESCOU)                                                   OUTTAP.600
      JBHIC(14,16)='I4DI:  1=OBSERVATIONS NUDGING;  2=NO OBS'                    OUTTAP.601
     *    //       'ERVATIONS NUDGING                       '                    OUTTAP.602
      JBHI(15,16)=ISWIND(NESCOU)                                                 OUTTAP.603
      JBHIC(15,16)='ISWIND:  1=OBS NUDGING OF THE WIND FIELD'                    OUTTAP.604
     *    //       '; 2=NO OBS NUDGING OF THE WIND FIELD    '                    OUTTAP.605
      JBHR(13,16)=GIV(NESCOU)                                                    OUTTAP.606
      JBHRC(13,16)='GIV:  NUDGING COEFFICIENT FOR OBS NUDGIN'                    OUTTAP.607
     *    //       'G OF THE WIND FIELD                     '                    OUTTAP.608
      JBHI(16,16)=ISTEMP(NESCOU)                                                 OUTTAP.609
      JBHIC(16,16)='ISTEMP:  1=OBS NUDGING OF THE TEMP FIELD'                    OUTTAP.610
     *    //       '; 2=NO OBS NUDGING OF THE TEMP FIELD    '                    OUTTAP.611
      JBHR(14,16)=GIT(NESCOU)                                                    OUTTAP.612
      JBHRC(14,16)='GIT:  NUDGING COEFFICIENT FOR OBS NUDGIN'                    OUTTAP.613
     *    //       'G OF THE TEMP FIELD                     '                    OUTTAP.614
      JBHI(17,16)=ISMOIS(NESCOU)                                                 24SEP99.129
      JBHIC(17,16)='ISMOIS:  1=OBS NUDGING OF THE MIXING RAT'                    24SEP99.130
     *    //       'IO FIELD;  2=NO OBS NUDGING OF MIX. RAT.'                    OUTTAP.617
      JBHR(15,16)=GIQ(NESCOU)                                                    24SEP99.131
      JBHRC(15,16)='GIQ:  NUDGING COEFFICIENT FOR OBS NUDGIN'                    24SEP99.132
     *    //       'G OF THE MIXING RATIO FIELD             '                    OUTTAP.620
      JBHR(16,16)=RINXY                                                          24SEP99.133
      JBHRC(16,16)='RINXY:  OBS NUDGING RADIUS OF INFLUENCE '                    24SEP99.134
     *    //       '(KM) IN THE HORIZONTAL                  '                    OUTTAP.623
      JBHR(17,16)=RINSIG                                                         24SEP99.135
      JBHRC(17,16)='RINSIG:  OBS NUDGING RADIUS OF INFLUENCE'                    24SEP99.136
     *    //       ' (SIGMA UNITS) IN THE VERTICAL          '                    OUTTAP.626
      JBHR(18,16)=TWINDO                                                         24SEP99.137
      JBHRC(18,16)='TWINDO:  OBS NUDGING HALF PERIOD (MINUTE'                    24SEP99.138
     *    //       'S) OF THE TIME WINDOW                   '                    OUTTAP.629
      JBHI(18,16)=IONF                                                           24SEP99.139
      JBHIC(18,16)='IONF:  FREQUENCY (COARSE-GRID TIMESTEPS)'                    24SEP99.140
     *    //       ' TO COMPUTE OBS-NUDGING WEIGHTS         '                    OUTTAP.632
      JBHI(19,16)=IDYNIN                                                         24SEP99.141
      JBHIC(19,16)='IDYNIN: =1, USING RAMPING FUNCTION AT EN'                    24SEP99.142
     *    //       'D OF FDDA, =0, NO RAMP                  '                    OUTTAP.635
      JBHR(19,16)=DTRAMP                                                         24SEP99.143
      JBHRC(19,16)='DTRAMP: IF IDYNIN=1, RAMPING TIME IN MIN'                    24SEP99.144
     *    //       'UTES                                    '                    OUTTAP.638
C                                                                                OUTTAP.639
C----- THE HEADER FILE VALID FOR THIS DOMAIN                                     OUTTAP.640
C                                                                                OUTTAP.641
      CALL WRITE_FLAG( IUTL, BH_FLAG )                                           OUTTAP.642
      CALL WRITE_BIG_HEADER( iutl )                                              OUTTAP.643
                                                                                 OUTTAP.644
      END IF                                                                     OUTTAP.645
C                                                                                OUTTAP.646
C----- END OF BIG HEADER                                                         OUTTAP.647
C                                                                                OUTTAP.648
C----- GET NEW DATE                                                              OUTTAP.649
C                                                                                OUTTAP.650
      CALL GETH_NEWDATE( CDATENEW, CDATE, NINT(XTIME*60.) )                      OUTTAP.651
      WRITE (CURRENT_DATE(1:24),FMT=340) CDATENEW                                OUTTAP.652
340   FORMAT(A19,'.0000')                                                        OUTTAP.653
C                                                                                OUTTAP.654
C----- 3D FORECAST VARIABLES                                                     OUTTAP.655
C                                                                                OUTTAP.656
C         1) U COMPONENT WIND                                                    OUTTAP.657
C         2) V COMPONENT WIND                                                    OUTTAP.658
C         3) TEMPERATURE                                                         OUTTAP.659
C                                                                                OUTTAP.660
      CALL DECOUPLE(UA,MIX,MJX,MKX,PDOTA,HDOTH,IL,JL,KL)                         OUTTAP.661
      CALL WRITE_FIELDREC( iutl,D3,nescou,HDOTH,XTIME,                           OUTTAP.662
     +                     'U',CURRENT_DATE,                                     OUTTAP.663
     +                     'D','YXS','m/s',                                      OUTTAP.664
     +                     'U COMPONENT OF HORIZONTAL WIND',                     OUTTAP.665
     +                     IX,JX,KZZ,MIX,MJX,MKX )                               OUTTAP.666
                                                                                 OUTTAP.667
      CALL DECOUPLE(VA,MIX,MJX,MKX,PDOTA,HDOTH,IL,JL,KL)                         OUTTAP.668
      CALL WRITE_FIELDREC( iutl,D3,nescou,HDOTH,XTIME,                           OUTTAP.669
     +                     'V',CURRENT_DATE,                                     OUTTAP.670
     +                     'D','YXS','m/s',                                      OUTTAP.671
     +                     'V COMPONENT OF HORIZONTAL WIND',                     OUTTAP.672
     +                     IX,JX,KZZ,MIX,MJX,MKX )                               OUTTAP.673
C                                                                                OUTTAP.674
      CALL FILLCRS(PSA,MIX,MJX,1,IL,JL,1)                                        07NOV00.416
                                                                                 07NOV00.417
      CALL FILLCRS(TA,MIX,MJX,MKX,IL,JL,KL)                                      07NOV00.418
      CALL DECOUPLE(TA,MIX,MJX,MKX,PSA,HDOTH,IL,JL,KL)                           OUTTAP.681
      CALL WRITE_FIELDREC( iutl,D3,nescou,HDOTH,XTIME,                           OUTTAP.682
     +                     'T',CURRENT_DATE,                                     OUTTAP.683
     +                     'C','YXS','K',                                        OUTTAP.684
     +                     'TEMPERATURE',                                        OUTTAP.685
     +                     IX,JX,KZZ,MIX,MJX,MKX )                               OUTTAP.686
C                                                                                OUTTAP.687
C----- OPTIONAL 3D MOISTURE VARIABLES                                            OUTTAP.688
C                                                                                OUTTAP.689
C         1) QV                                                                  OUTTAP.690
C         2) CLOUD WATER                                                         OUTTAP.691
C         3) RAIN WATER                                                          OUTTAP.692
C         4) SNOW                                                                OUTTAP.693
C         5) ICE                                                                 OUTTAP.694
C         6) GRAUPEL                                                             OUTTAP.695
C         7) ICE NUMBER CONCENTRATION                                            OUTTAP.696
C                                                                                OUTTAP.697
      IF(IMPHYS(NESCOU).GE.2)THEN                                                OUTTAP.698
                                                                                 OUTTAP.699
         CALL FILLCRS(QVA,MIX,MJX,MKX,IL,JL,KL)                                  07NOV00.419
         CALL DECOUPLE(QVA,MIX,MJX,MKX,PSA,HDOTH,IL,JL,KL)                       OUTTAP.700
         CALL WRITE_FIELDREC( iutl,D3,nescou,HDOTH,XTIME,                        OUTTAP.701
     +                        'Q',CURRENT_DATE,                                  OUTTAP.702
     +                        'C','YXS','kg/kg',                                 OUTTAP.703
     +                        'MIXING RATIO',                                    OUTTAP.704
     +                        IX,JX,KZZ,MIX,MJX,MKX )                            OUTTAP.705
                                                                                 OUTTAP.706
C                                                                                OUTTAP.707
        IF(IMPHYS(NESCOU).GE.3)THEN                                              OUTTAP.708
C                                                                                OUTTAP.709
          CALL FILLCRS(QCA,MIX,MJX,MKX,IL,JL,KL)                                 07NOV00.420
          CALL DECOUPLE(QCA,MIX,MJX,MKX,PSA,HDOTH,IL,JL,KL)                      OUTTAP.710
          CALL WRITE_FIELDREC( iutl,D3,nescou,HDOTH,XTIME,                       OUTTAP.711
     +                         'CLW',CURRENT_DATE,                               OUTTAP.712
     +                         'C','YXS','kg/kg',                                OUTTAP.713
     +                         'CLOUD WATER MIXING RATIO',                       OUTTAP.714
     +                         IX,JX,KZZ,MIX,MJX,MKX )                           OUTTAP.715
                                                                                 OUTTAP.716
C                                                                                OUTTAP.717
          CALL FILLCRS(QRA,MIX,MJX,MKX,IL,JL,KL)                                 07NOV00.421
          CALL DECOUPLE(QRA,MIX,MJX,MKX,PSA,HDOTH,IL,JL,KL)                      OUTTAP.718
          CALL WRITE_FIELDREC( iutl,D3,nescou,HDOTH,XTIME,                       OUTTAP.719
     +                         'RNW',CURRENT_DATE,                               OUTTAP.720
     +                         'C','YXS','kg/kg',                                OUTTAP.721
     +                         'RAIN WATER MIXING RATIO',                        OUTTAP.722
     +                         IX,JX,KZZ,MIX,MJX,MKX )                           OUTTAP.723
                                                                                 OUTTAP.724
C                                                                                OUTTAP.725
          IF(IMPHYS(NESCOU).GE.5)THEN                                            OUTTAP.726
                                                                                 OUTTAP.727
            CALL FILLCRS(QIA,MIX,MJX,MKX,IL,JL,KL)                               07NOV00.422
            CALL DECOUPLE(QIA,MIX,MJX,MKX,PSA,HDOTH,IL,JL,KL)                    OUTTAP.728
            CALL WRITE_FIELDREC( iutl,D3,nescou,HDOTH,XTIME,                     OUTTAP.729
     +                           'ICE',CURRENT_DATE,                             OUTTAP.730
     +                           'C','YXS','kg/kg',                              OUTTAP.731
     +                           'CLOUD ICE MIXING RATIO',                       07NOV00.423
     +                           IX,JX,KZZ,MIX,MJX,MKX )                         OUTTAP.733
                                                                                 OUTTAP.734
C                                                                                OUTTAP.735
            CALL FILLCRS(QNIA,MIX,MJX,MKX,IL,JL,KL)                              07NOV00.424
            CALL DECOUPLE(QNIA,MIX,MJX,MKX,PSA,HDOTH,IL,JL,KL)                   OUTTAP.736
            CALL WRITE_FIELDREC( iutl,D3,nescou,HDOTH,XTIME,                     OUTTAP.737
     +                           'SNOW',CURRENT_DATE,                            OUTTAP.738
     +                           'C','YXS','kg/kg',                              OUTTAP.739
     +                           'SNOW MIXING RATIO',                            OUTTAP.740
     +                           IX,JX,KZZ,MIX,MJX,MKX )                         OUTTAP.741
                                                                                 OUTTAP.742
          ENDIF                                                                  OUTTAP.743
C                                                                                OUTTAP.744
          IF(IMPHYS(NESCOU).GE.6)THEN                                            OUTTAP.745
                                                                                 OUTTAP.746
            CALL FILLCRS(QGA,MIX,MJX,MKX,IL,JL,KL)                               07NOV00.425
            CALL DECOUPLE(QGA,MIX,MJX,MKX,PSA,HDOTH,IL,JL,KL)                    OUTTAP.747
            CALL WRITE_FIELDREC( iutl,D3,nescou,HDOTH,XTIME,                     OUTTAP.748
     +                           'GRAUPEL',CURRENT_DATE,                         OUTTAP.749
     +                           'C','YXS','kg/kg',                              OUTTAP.750
     +                           'GRAUPEL MIXING RATIO',                         OUTTAP.751
     +                           IX,JX,KZZ,MIX,MJX,MKX )                         OUTTAP.752
                                                                                 OUTTAP.753
C                                                                                OUTTAP.754
          IF(IMPHYS(NESCOU).EQ.7)THEN                                            19DEC02.557
            CALL FILLCRS(QNCA,MIX,MJX,MKX,IL,JL,KL)                              07NOV00.426
            CALL DECOUPLE(QNCA,MIX,MJX,MKX,PSA,HDOTH2,IL,JL,KL)                  19DEC02.558
            CALL RHO_MLT(HDOTH2,IL,JL,KL,PSB,HDOTH,IL,JL,KL)                     19DEC02.559
            CALL WRITE_FIELDREC( iutl,D3,nescou,HDOTH,XTIME,                     OUTTAP.756
     +                           'NCI',CURRENT_DATE,                             OUTTAP.757
     +                           'C','YXS','#/m^3',                              OUTTAP.758
     +                           'NUMBER CONCENTRATION OF ICE',                  OUTTAP.759
     +                           IX,JX,KZZ,MIX,MJX,MKX )                         OUTTAP.760
                                                                                 OUTTAP.761
          ENDIF                                                                  OUTTAP.762
          ENDIF                                                                  19DEC02.560
        ENDIF                                                                    OUTTAP.763
      ENDIF                                                                      OUTTAP.764
C                                                                                OUTTAP.765
C----- OPTIONAL NAVY PBL SCHEME 3D FORECAST VARIABLES                            OUTTAP.766
C                                                                                OUTTAP.767
C        1) TURBULENT KINETIC ENERGY                                             OUTTAP.768
C                                                                                OUTTAP.769
      IF(IBLTYP(NESCOU).EQ.3.OR.IBLTYP(NESCOU).EQ.4)THEN                         OUTTAP.770
                                                                                 OUTTAP.771
         CALL FILLCRS(TURKE,MIX,MJX,MKX,IL,JL,KL)                                07NOV00.427
         CALL EQUATO(TURKE,MIX,MJX,MKX,HDOTH,IL,JL,KL)                           24SEP99.145
         CALL WRITE_FIELDREC( iutl,D3,nescou,HDOTH,XTIME,                        OUTTAP.773
     +                        'TKE',CURRENT_DATE,                                OUTTAP.774
     +                        'C','YXS','J/kg',                                  OUTTAP.775
     +                        'TURBULENT KINETIC ENERGY',                        OUTTAP.776
     +                        IX,JX,KZZ,MIX,MJX,MKX )                            OUTTAP.777
      ENDIF                                                                      OUTTAP.778
                                                                                 OUTTAP.779
      IF(IBLTYP(NESCOU).EQ.6)THEN                                                OUTTAP.780
                                                                                 OUTTAP.781
         CALL FILLCRS(TKEA,MIX,MJX,MKX+1,IL,JL,KL+1)                             23DEC04.232
         CALL EQUATO(TKEA,MIX,MJX,MKX+1,HDOTH,IL,JL,KL+1)                        24SEP99.146
         CALL WRITE_FIELDREC( iutl,D3,nescou,HDOTH,XTIME,                        OUTTAP.783
     +                        'TKE',CURRENT_DATE,                                OUTTAP.784
     +                        'C','YXW','J/kg',                                  OUTTAP.785
     +                        'TURBULENT KINETIC ENERGY',                        OUTTAP.786
     +                        IX,JX,KZZ+1,MIX,MJX,MKX+1 )                        OUTTAP.787
      ENDIF                                                                      OUTTAP.788
C                                                                                OUTTAP.789
C----- OPTIONAL ATMOSPHERIC RADIATION VARIABLE                                   OUTTAP.790
C                                                                                OUTTAP.791
C        1) ATMOSPHERIC RADIATION TENDENCY                                       OUTTAP.792
C                                                                                OUTTAP.793
      IF(IFRAD.GE.2)THEN                                                         OUTTAP.794
         CALL FILLCRS(RTTEN,MIX,MJX,MKX,IL,JL,KL)                                07NOV00.429
         CALL EQUATO(RTTEN,MIX,MJX,MKX,HDOTH,IL,JL,KL)                           24SEP99.147
#ifndef MPP1                                                                     OUTTAP.796
         DO I=1,IL*JL*KL                                                         OUTTAP.797
            HDOTH(I)=HDOTH(I)*86400.                                             OUTTAP.798
         ENDDO                                                                   OUTTAP.799
#else                                                                            OUTTAP.800
         DO I=1,MIX*MJX*MKX                                                      OUTTAP.801
            HDOTH(I)=HDOTH(I)*86400.                                             OUTTAP.802
         ENDDO                                                                   OUTTAP.803
#endif                                                                           OUTTAP.804
         CALL WRITE_FIELDREC( iutl,D3,nescou,HDOTH,XTIME,                        OUTTAP.805
     +                        'RAD TEND',CURRENT_DATE,                           OUTTAP.806
     +                        'C','YXS','K/DAY',                                 OUTTAP.807
     +                        'ATMOSPHERIC RADIATION TENDENCY',                  OUTTAP.808
     +                        IX,JX,KZZ,MIX,MJX,MKX )                            OUTTAP.809
      ENDIF                                                                      OUTTAP.810
C                                                                                OUTTAP.811
C----- NON-HYDROSTATIC 3D FORECAST VARIABLES                                     OUTTAP.812
C                                                                                OUTTAP.813
C        1) VERTICAL WIND COMPONENT                                              OUTTAP.814
C        2) PRESSURE PERTURBATION                                                OUTTAP.815
C                                                                                OUTTAP.816
         CALL FILLCRS(WA,MIX,MJX,MKX+1,IL,JL,KL+1)                               07NOV00.430
         CALL DECOUPLE(WA,MIX,MJX,MKX+1,PSA,HDOTH,IL,JL,KL+1)                    OUTTAP.817
         CALL WRITE_FIELDREC( iutl,D3,nescou,HDOTH,XTIME,                        OUTTAP.818
     +                        'W',CURRENT_DATE,                                  OUTTAP.819
     +                        'C','YXW','m/s',                                   OUTTAP.820
     +                        'VERTICAL WIND COMPONENT',                         OUTTAP.821
     +                        IX,JX,KZZ+1,MIX,MJX,MKX+1 )                        OUTTAP.822
                                                                                 OUTTAP.823
C                                                                                OUTTAP.824
         CALL FILLCRS(PPA,MIX,MJX,MKX,IL,JL,KL)                                  07NOV00.431
         CALL DECOUPLE(PPA,MIX,MJX,MKX,PSA,HDOTH,IL,JL,KL)                       OUTTAP.825
         CALL WRITE_FIELDREC( iutl,D3,nescou,HDOTH,XTIME,                        OUTTAP.826
     +                        'PP',CURRENT_DATE,                                 OUTTAP.827
     +                        'C','YXS','Pa',                                    OUTTAP.828
     +                        'PRESSURE PERTURBATION',                           OUTTAP.829
     +                        IX,JX,KZZ,MIX,MJX,MKX )                            OUTTAP.830
C                                                                                OUTTAP.831
C----- 2D FIELDS                                                                 OUTTAP.832
C                                                                                OUTTAP.833
C         1) PSTAR (in Pascal)                                                   OUTTAP.834
C                                                                                OUTTAP.835
      DO I=1,ILX                                                                 OUTTAP.836
        DO J=1,JLX                                                               OUTTAP.837
          HSCRL(I,J)=PSA(I,J)*1000.                                              OUTTAP.838
        ENDDO                                                                    OUTTAP.839
      ENDDO                                                                      OUTTAP.840
      CALL EQUATO(HSCRL,MIX,MJX,1,HDOTH,IL,JL,1)                                 24SEP99.148
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.842
     +                     'PSTARCRS',CURRENT_DATE,                              OUTTAP.843
     +                     'C','YX','Pa',                                        OUTTAP.844
     +                     '(REFERENCE) SURFACE PRESSURE MINUS PTOP',            OUTTAP.845
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.846
C                                                                                OUTTAP.847
C----- 2D FORECAST VARIABLES                                                     OUTTAP.848
C                                                                                OUTTAP.849
C        1) GROUND TEMPERATURE                                                   OUTTAP.850
C                                                                                OUTTAP.851
      CALL EQUATO(TGA,MIX,MJX,1,HDOTH,IL,JL,1)                                   24SEP99.149
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.853
     +                     'GROUND T',CURRENT_DATE,                              OUTTAP.854
     +                     'C','YX','K',                                         OUTTAP.855
     +                     'GROUND TEMPERATURE',                                 OUTTAP.856
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.857
                                                                                 OUTTAP.858
C                                                                                OUTTAP.859
C----- 2D FORECAST VARIABLES                                                     OUTTAP.860
C                                                                                OUTTAP.861
C        1) ACCUMULATED CONVECTIVE PRECIPITATION                                 OUTTAP.862
C        2) ACCUMULATED RESOLVED PRECIPITATION                                   OUTTAP.863
C                                                                                OUTTAP.864
      CALL EQUATO(RAINC,MIX,MJX,1,HDOTH,IL,JL,1)                                 24SEP99.150
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.866
     +                     'RAIN CON',CURRENT_DATE,                              OUTTAP.867
     +                     'C','YX','cm',                                        OUTTAP.868
     +                     'ACCUMULATED CONVECTIVE PRECIPITATION',               OUTTAP.869
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.870
                                                                                 OUTTAP.871
C                                                                                OUTTAP.872
      CALL EQUATO(RAINNC,MIX,MJX,1,HDOTH,IL,JL,1)                                24SEP99.151
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.874
     +                     'RAIN NON',CURRENT_DATE,                              OUTTAP.875
     +                     'C','YX','cm',                                        OUTTAP.876
     +                     'ACCUMULATED NONCONVECTIVE PRECIPITATION',            OUTTAP.877
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.878
C                                                                                OUTTAP.879
C----- OPTIONAL 2D FORECAST VARIABLE                                             OUTTAP.880
C                                                                                OUTTAP.881
C        1) VISBILITY                                                            OUTTAP.882
C        2) FOG DEPTH                                                            OUTTAP.883
C                                                                                OUTTAP.884
      IF(IFOGSW(NESCOU).EQ.1)THEN                                                OUTTAP.885
         CALL EQUATO(VISB(NESCOU,1,1),MIX,MJX,1,HDOTH,IL,JL,1)                   24SEP99.152
         CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                        OUTTAP.887
     +                     'VISB',CURRENT_DATE,                                  OUTTAP.888
     +                     'C','YX','m',                                         OUTTAP.889
     +                     'VISIBILITY',                                         OUTTAP.890
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.891
                                                                                 OUTTAP.892
         CALL EQUATO(FOGDEP(NESCOU,1,1),MIX,MJX,1,HDOTH,IL,JL,1)                 24SEP99.153
         CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                        OUTTAP.894
     +                     'FOGDEPTH',CURRENT_DATE,                              OUTTAP.895
     +                     'C','YX','m',                                         OUTTAP.896
     +                     'FOG DEPTH',                                          OUTTAP.897
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.898
      ENDIF                                                                      OUTTAP.899
C                                                                                05DEC01.250
C  OPTIONAL KF-2 VARIABLES                                                       05DEC01.251
C                                                                                05DEC01.252
      IF(ICUPA(NESCOU).EQ.8)THEN                                                 05DEC01.253
         CALL EQUATO(SUMFB,MIX,MJX,1,HDOTH,IL,JL,1)                              05DEC01.254
         CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                        05DEC01.255
     +                     'SUMFB',CURRENT_DATE,                                 05DEC01.256
     +                     'C','YX','m',                                         05DEC01.257
     +                     'KF MASS FLUX UPDRAFT   MAX VALUE IN PREVIOUS         05DEC01.258
     & HOUR',                                                                    05DEC01.259
     +                     IX,JX,1,MIX,MJX,1 )                                   05DEC01.260
                                                                                 05DEC01.261
         CALL EQUATO(SPSRC,MIX,MJX,1,HDOTH,IL,JL,1)                              05DEC01.262
         CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                        05DEC01.263
     +                     'SPSRC',CURRENT_DATE,                                 05DEC01.264
     +                     'C','YX','m',                                         05DEC01.265
     +                     'KF SOURCE LYR UPDRAFT  MAX VALUE IN PREVIOUS         05DEC01.266
     & HOUR',                                                                    05DEC01.267
     +                     IX,JX,1,MIX,MJX,1 )                                   05DEC01.268
        DO 773 I = 1,MIX                                                         05DEC01.269
        DO 773 J = 1,MJX                                                         05DEC01.270
          SUMFB(I,J) = 0.                                                        05DEC01.271
          SPSRC(I,J) = 0.                                                        05DEC01.272
 773    CONTINUE                                                                 05DEC01.273
      ENDIF                                                                      05DEC01.274
C                                                                                OUTTAP.900
C----- NON-FORECAST (TERRESTRIAL) 2D ARRAYS                                      OUTTAP.901
C                                                                                OUTTAP.902
C         1) TERRAIN                                                             OUTTAP.903
C         2) MAP FACTOR CROSS                                                    OUTTAP.904
C         3) MAP FACTOR DOT                                                      OUTTAP.905
C         4) CORIOLIS                                                            OUTTAP.906
C         5) RESERVOIR TEMPERATURE                                               OUTTAP.907
C         6) LATITUDE                                                            OUTTAP.908
C         7) LONGITUDE                                                           OUTTAP.909
C         8) LAND USE CATEGORY                                                   OUTTAP.910
C         9) SNOW COVER                                                          OUTTAP.911
C        10) SEA ICE FRACTION                                                    19DEC02.561
C                                                                                OUTTAP.912
      DO I=1,ILX                                                                 OUTTAP.913
        DO J=1,JLX                                                               OUTTAP.914
          HSCRL(I,J)=HT(I,J)/G                                                   OUTTAP.915
        ENDDO                                                                    OUTTAP.916
      ENDDO                                                                      OUTTAP.917
      CALL EQUATO(HSCRL,MIX,MJX,1,HDOTH,IL,JL,1)                                 24SEP99.154
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.919
     +                     'TERRAIN',CURRENT_DATE,                               OUTTAP.920
     +                     'C','YX','m',                                         OUTTAP.921
     +                     'TERRAIN ELEVATION',                                  OUTTAP.922
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.923
                                                                                 OUTTAP.924
      DO I=1,ILX                                                                 OUTTAP.925
        DO J=1,JLX                                                               OUTTAP.926
          HSCRL(I,J)=1./MSFX(I,J)                                                OUTTAP.927
        ENDDO                                                                    OUTTAP.928
      ENDDO                                                                      OUTTAP.929
      CALL EQUATO(HSCRL,MIX,MJX,1,HDOTH,IL,JL,1)                                 24SEP99.155
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.931
     +                     'MAPFACCR',CURRENT_DATE,                              OUTTAP.932
     +                     'C','YX','(DIMENSIONLESS)',                           OUTTAP.933
     +                     'MAP SCALE FACTOR',                                   OUTTAP.934
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.935
                                                                                 OUTTAP.936
C                                                                                OUTTAP.937
      DO I=1,IL                                                                  OUTTAP.938
        DO J=1,JL                                                                OUTTAP.939
          HSCRL(I,J)=1./MSFD(I,J)                                                OUTTAP.940
        ENDDO                                                                    OUTTAP.941
      ENDDO                                                                      OUTTAP.942
      CALL EQUATO(HSCRL,MIX,MJX,1,HDOTH,IL,JL,1)                                 24SEP99.156
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.944
     +                     'MAPFACDT',CURRENT_DATE,                              OUTTAP.945
     +                     'D','YX','(DIMENSIONLESS)',                           OUTTAP.946
     +                     'MAP SCALE FACTOR',                                   OUTTAP.947
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.948
C                                                                                OUTTAP.949
      CALL EQUATO(F,MIX,MJX,1,HDOTH,IL,JL,1)                                     24SEP99.157
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.951
     +                     'CORIOLIS',CURRENT_DATE,                              OUTTAP.952
     +                     'D','YX','1/s',                                       OUTTAP.953
     +                     'CORIOLIS PARAMETER',                                 OUTTAP.954
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.955
C                                                                                OUTTAP.956
      CALL EQUATO(TMN,MIX,MJX,1,HDOTH,IL,JL,1)                                   24SEP99.158
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.958
     +                     'RES TEMP',CURRENT_DATE,                              OUTTAP.959
     +                     'C','YX','K',                                         OUTTAP.960
     +                     'INFINITE RESERVOIR SLAB TEMPERATURE',                OUTTAP.961
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.962
C                                                                                OUTTAP.963
      CALL EQUATO(XLAT,MIX,MJX,1,HDOTH,IL,JL,1)                                  24SEP99.159
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.965
     +                     'LATITCRS',CURRENT_DATE,                              OUTTAP.966
     +                     'C','YX','DEGREES',                                   OUTTAP.967
     +                     'LATITUDE (SOUTH NEGATIVE)',                          OUTTAP.968
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.969
C                                                                                OUTTAP.970
      CALL EQUATO(XLONG,MIX,MJX,1,HDOTH,IL,JL,1)                                 24SEP99.160
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.972
     +                     'LONGICRS',CURRENT_DATE,                              OUTTAP.973
     +                     'C','YX','DEGREES',                                   OUTTAP.974
     +                     'LONGITUDE (WEST NEGATIVE)',                          OUTTAP.975
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.976
C                                                                                OUTTAP.977
      CALL EQUATO(SATBRT,MIX,MJX,1,HDOTH,IL,JL,1)                                24SEP99.161
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.979
     +                     'LAND USE',CURRENT_DATE,                              OUTTAP.980
     +                     'C','YX','category',                                  OUTTAP.981
     +                     'LANDUSE CATEGORY',                                   OUTTAP.982
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.983
C                                                                                19DEC02.562
      IF(IFSNOW(NESCOU).EQ.1.AND.ISOIL.NE.2)THEN                                 19DEC02.563
        CALL EQUATO(SNOWC,MIX,MJX,1,HDOTH,IL,JL,1)                               05DEC01.276
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         05DEC01.277
     +                     'SNOWCOVR',CURRENT_DATE,                              OUTTAP.987
     +                     'C','YX','(DIMENSIONLESS)',                           OUTTAP.988
     +                     'SNOW COVER FLAGS',                                   OUTTAP.989
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.990
C                                                                                19DEC02.564
      ELSE IF(IFSNOW(NESCOU).EQ.2)THEN                                           05DEC01.278
        CALL EQUATO(SNOWC,MIX,MJX,1,HDOTH,IL,JL,1)                               05DEC01.279
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         05DEC01.280
     +                     'WEASD   ',CURRENT_DATE,                              19DEC02.565
     +                     'C','YX','mm',                                        05DEC01.282
     +                     'SNOW COVER LIQUID EQUIVALENT',                       05DEC01.283
     +                     IX,JX,1,MIX,MJX,1 )                                   05DEC01.284
      ENDIF                                                                      05DEC01.285
      IF(IPOLAR.EQ.1)THEN                                                        19DEC02.566
        CALL EQUATO(SEAICE,MIX,MJX,1,HDOTH,IL,JL,1)                              19DEC02.567
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         19DEC02.568
     +                     'SEAICEFR',CURRENT_DATE,                              19DEC02.569
     +                     'C','YX','(DIMENSIONLESS)',                           19DEC02.570
     +                     'SEA ICE FRACTION',                                   19DEC02.571
     +                     IX,JX,1,MIX,MJX,1 )                                   19DEC02.572
        CALL EQUATO(TGSI,MIX,MJX,1,HDOTH,IL,JL,1)                                23DEC04.233
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         23DEC04.234
     +                     'TGSI    ',CURRENT_DATE,                              23DEC04.235
     +                     'C','YX','K',                                         23DEC04.236
     +                     'SEA ICE TEMPERATURE',                                23DEC04.237
     +                     IX,JX,1,MIX,MJX,1 )                                   23DEC04.238
      ENDIF                                                                      19DEC02.573
C                                                                                24SEP99.163
      CALL EQUATO(TSS,MIX,MJX,1,HDOTH,IL,JL,1)                                   24SEP99.164
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           24SEP99.165
     +                     'TSEASFC ',CURRENT_DATE,                              24SEP99.166
     +                     'C','YX','K',                                         24SEP99.167
     +                     'SEA SURFACE TEMPERATURE',                            24SEP99.168
     +                     IX,JX,1,MIX,MJX,1 )                                   24SEP99.169
C                                                                                OUTTAP.991
C----- OPTIONAL 2D FORECAST VARIABLES (SINCE V2 RELEASE 2-7)                     OUTTAP.992
C                                                                                OUTTAP.993
C        1) PBL HEIGHT (M)                                                       OUTTAP.994
C        2) PBL REGIME (BULK RI (BLTYP=1) OR 1-4 (IBLTYP=2,5))                   OUTTAP.995
C     NOTE: ABOVE PBL VARIABLES ONLY DEFINED FOR IBLTYP=1,2,5                    OUTTAP.996
C        3) SURFACE SENSIBLE HEAT FLUX (W/M^2)                                   OUTTAP.997
C        4) SURFACE LATENT HEAT FLUX (W/M^2)                                     OUTTAP.998
C        5) FRICTIONAL VELOCITY (M/S)                                            OUTTAP.999
C        6) SHORT WAVE RADIATION (W/M^2)                                         OUTTAP.1000
C        7) LONG WAVE RADIATION (W/M^2)                                          OUTTAP.1001
C    8-MLX) SOIL TEMPERATURE: LAYERS 1-MLX                                       OUTTAP.1002
C                                                                                OUTTAP.1003
      CALL EQUATO(PBL,MIX,MJX,1,HDOTH,IL,JL,1)                                   24SEP99.170
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.1005
     +                     'PBL HGT',CURRENT_DATE,                               OUTTAP.1006
     +                     'C','YX','m',                                         OUTTAP.1007
     +                     'PBL HEIGHT',                                         OUTTAP.1008
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.1009
C                                                                                OUTTAP.1010
      CALL EQUATO(REGIME,MIX,MJX,1,HDOTH,IL,JL,1)                                24SEP99.171
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.1012
     +                     'REGIME',CURRENT_DATE,                                OUTTAP.1013
     +                     'C','YX','(DIMENSIONLESS)',                           OUTTAP.1014
     +                     'PBL REGIME',                                         OUTTAP.1015
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.1016
C                                                                                OUTTAP.1017
      CALL EQUATO(HFX,MIX,MJX,1,HDOTH,IL,JL,1)                                   24SEP99.172
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.1019
     +                     'SHFLUX',CURRENT_DATE,                                OUTTAP.1020
     +                     'C','YX','W/m^2',                                     OUTTAP.1021
     +                     'SENSIBLE HEAT FLUX',                                 OUTTAP.1022
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.1023
C                                                                                OUTTAP.1024
      DO I = 1,ILX                                                               OUTTAP.1025
        DO J = 1,JLX                                                             07NOV00.432
         IF(IBLTYP(NESCOU).EQ.7) THEN                                            07NOV00.433
            HSCRL(I,J) = QFX(I,J)   ! in PX QFX already includes Lv              07NOV00.434
         ELSE                                                                    07NOV00.435
            HSCRL(I,J) = QFX(I,J)*XLV                                            07NOV00.436
         ENDIF                                                                   07NOV00.437
        END DO                                                                   OUTTAP.1028
      END DO                                                                     OUTTAP.1029
      CALL EQUATO(HSCRL,MIX,MJX,1,HDOTH,IL,JL,1)                                 24SEP99.173
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.1031
     +                     'LHFLUX',CURRENT_DATE,                                OUTTAP.1032
     +                     'C','YX','W/m^2',                                     OUTTAP.1033
     +                     'LATENT HEAT FLUX',                                   OUTTAP.1034
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.1035
C                                                                                OUTTAP.1036
      CALL EQUATO(UST,MIX,MJX,1,HDOTH,IL,JL,1)                                   24SEP99.174
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.1038
     +                     'UST',CURRENT_DATE,                                   OUTTAP.1039
     +                     'C','YX','m/s',                                       OUTTAP.1040
     +                     'FRICTIONAL VELOCITY',                                OUTTAP.1041
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.1042
C                                                                                OUTTAP.1043
      DO I = 1,ILX                                                               OUTTAP.1044
        DO J = 1,JLX                                                             OUTTAP.1045
         IF(IBLTYP(NESCOU).EQ.7) THEN                                            07NOV00.438
            HSCRL(I,J) = GSW(I,J)/(1.-ALBEDO(I,J))                               07NOV00.439
         ELSE                                                                    07NOV00.440
            HSCRL(I,J) = GSW(I,J)/(1.-ALB(I,J))                                  07NOV00.441
         ENDIF                                                                   07NOV00.442
        END DO                                                                   OUTTAP.1047
      END DO                                                                     OUTTAP.1048
      CALL EQUATO(HSCRL,MIX,MJX,1,HDOTH,IL,JL,1)                                 24SEP99.175
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.1050
     +                     'SWDOWN',CURRENT_DATE,                                OUTTAP.1051
     +                     'C','YX','W/m^2',                                     OUTTAP.1052
     +                     'SURFACE DOWNWARD SHORTWAVE RADIATION',               OUTTAP.1053
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.1054
C                                                                                OUTTAP.1055
      CALL EQUATO(GLW,MIX,MJX,1,HDOTH,IL,JL,1)                                   24SEP99.176
      CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                           OUTTAP.1057
     +                     'LWDOWN',CURRENT_DATE,                                OUTTAP.1058
     +                     'C','YX','W/m^2',                                     OUTTAP.1059
     +                     'SURFACE DOWNWARD LONGWAVE RADIATION',                OUTTAP.1060
     +                     IX,JX,1,MIX,MJX,1 )                                   OUTTAP.1061
      IF(IFRAD.GE.2)THEN                                                         07NOV00.443
        CALL EQUATO(OSW,MIX,MJX,1,HDOTH,IL,JL,1)                                 07NOV00.444
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         07NOV00.445
     +                     'SWOUT',CURRENT_DATE,                                 07NOV00.446
     +                     'C','YX','W/m^2',                                     07NOV00.447
     +                     'TOP OUTGOING SHORTWAVE RADIATION',                   07NOV00.448
     +                     IX,JX,1,MIX,MJX,1 )                                   07NOV00.449
C                                                                                07NOV00.450
        CALL EQUATO(OLW,MIX,MJX,1,HDOTH,IL,JL,1)                                 07NOV00.451
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         07NOV00.452
     +                     'LWOUT',CURRENT_DATE,                                 07NOV00.453
     +                     'C','YX','W/m^2',                                     07NOV00.454
     +                     'TOP OUTGOING LONGWAVE RADIATION',                    07NOV00.455
     +                     IX,JX,1,MIX,MJX,1 )                                   07NOV00.456
      ENDIF                                                                      07NOV00.457
C                                                                                OUTTAP.1062
      IF(ISOIL.EQ.1) THEN                                                        OUTTAP.1063
        DO L = 1,MLX                                                             OUTTAP.1064
#ifndef MPP1                                                                     24SEP99.177
           CALL EQUATO(TSLA(1,1,L),MIX,MJX,1,HDOTH,IL,JL,1)                      24SEP99.178
#else                                                                            24SEP99.179
           CALL EQUATO(TSLA(NOFLIC(1),NOFLIC(1),L),                              24SEP99.180
     +                 MIX,MJX,1,HDOTH,IL,JL,1)                                  24SEP99.181
#endif                                                                           24SEP99.182
           WRITE (NAME(1:8),FMT=350)L                                            OUTTAP.1066
           WRITE (DESCRIPTION(1:28),FMT=360)L                                    OUTTAP.1067
           CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                      OUTTAP.1068
     +                          NAME,CURRENT_DATE,                               OUTTAP.1069
     +                          'C','YX','K',                                    OUTTAP.1070
     +                          DESCRIPTION,                                     OUTTAP.1071
     +                          IX,JX,1,MIX,MJX,1 )                              OUTTAP.1072
        END DO                                                                   OUTTAP.1073
      END IF                                                                     OUTTAP.1074
350   FORMAT('SOIL T',I2)                                                        OUTTAP.1075
360   FORMAT('SOIL TEMPERATURE IN LAYER ',I2)                                    OUTTAP.1076
                                                                                 OUTTAP.1077
      IF(ISOIL.GE.2) THEN                                                        07NOV00.458
        DO L = 1,MLX                                                             OUTTAP.1079
#ifndef MPP1                                                                     24SEP99.183
           CALL EQUATO(STCA(1,1,L),MIX,MJX,1,HDOTH,IL,JL,1)                      24SEP99.184
#else                                                                            24SEP99.185
           CALL EQUATO(STCA(NOFLIC(1),NOFLIC(1),L),                              24SEP99.186
     +                 MIX,MJX,1,HDOTH,IL,JL,1)                                  24SEP99.187
#endif                                                                           24SEP99.188
           WRITE (NAME(1:8),FMT=350)L                                            OUTTAP.1081
           WRITE (DESCRIPTION(1:28),FMT=360)L                                    OUTTAP.1082
           CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                      OUTTAP.1083
     +                          NAME,CURRENT_DATE,                               OUTTAP.1084
     +                          'C','YX','K',                                    OUTTAP.1085
     +                          DESCRIPTION,                                     OUTTAP.1086
     +                          IX,JX,1,MIX,MJX,1 )                              OUTTAP.1087
        END DO                                                                   OUTTAP.1088
C                                                                                OUTTAP.1089
        DO L = 1,MLX                                                             OUTTAP.1090
                                                                                 OUTTAP.1091
#ifndef MPP1                                                                     24SEP99.189
           CALL EQUATO(SMCA(1,1,L),MIX,MJX,1,HDOTH,IL,JL,1)                      24SEP99.190
#else                                                                            24SEP99.191
           CALL EQUATO(SMCA(NOFLIC(1),NOFLIC(1),L),                              24SEP99.192
     +                 MIX,MJX,1,HDOTH,IL,JL,1)                                  24SEP99.193
#endif                                                                           24SEP99.194
           WRITE (NAME(1:8),FMT=370)L                                            OUTTAP.1093
           WRITE (DESCRIPTION(1:25),FMT=380)L                                    OUTTAP.1094
           CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                      OUTTAP.1095
     +                          NAME,CURRENT_DATE,                               OUTTAP.1096
     +                          'C','YX','m^3/m^3',                              OUTTAP.1097
     +                          DESCRIPTION,                                     OUTTAP.1098
     +                          IX,JX,1,MIX,MJX,1 )                              OUTTAP.1099
                                                                                 OUTTAP.1100
        END DO                                                                   OUTTAP.1101
370   FORMAT('SOIL M',I2)                                                        OUTTAP.1102
380   FORMAT('TOTAL SOIL MOIS IN LYR ',I2)                                       19DEC02.574
C                                                                                19DEC02.575
        DO L = 1,MLX                                                             19DEC02.576
                                                                                 19DEC02.577
#ifndef MPP1                                                                     19DEC02.578
           CALL EQUATO(SWCA(1,1,L),MIX,MJX,1,HDOTH,IL,JL,1)                      19DEC02.579
#else                                                                            19DEC02.580
           CALL EQUATO(SWCA(NOFLIC(1),NOFLIC(1),L),                              19DEC02.581
     +                 MIX,MJX,1,HDOTH,IL,JL,1)                                  19DEC02.582
#endif                                                                           19DEC02.583
           WRITE (NAME(1:8),FMT=371)L                                            19DEC02.584
           WRITE (DESCRIPTION(1:25),FMT=381)L                                    19DEC02.585
           CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                      19DEC02.586
     +                          NAME,CURRENT_DATE,                               19DEC02.587
     +                          'C','YX','m^3/m^3',                              19DEC02.588
     +                          DESCRIPTION,                                     19DEC02.589
     +                          IX,JX,1,MIX,MJX,1 )                              19DEC02.590
                                                                                 19DEC02.591
        END DO                                                                   19DEC02.592
371     FORMAT('SOIL W',I2)                                                      19DEC02.593
381     FORMAT('SOIL LQD WATER IN LYR ',I2)                                      19DEC02.594
C                                                                                OUTTAP.1104
                                                                                 OUTTAP.1105
        CALL EQUATO(CMCA,MIX,MJX,1,HDOTH,IL,JL,1)                                24SEP99.195
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         OUTTAP.1107
     +                       'CANOPYM ',CURRENT_DATE,                            OUTTAP.1108
     +                       'C','YX','m',                                       OUTTAP.1109
     +                       'CANOPY MOISTUR E CONTENT',                         OUTTAP.1110
     +                       IX,JX,1,MIX,MJX,1 )                                 OUTTAP.1111
C                                                                                OUTTAP.1112
        IF(ISOIL.EQ.2) THEN                                                      07NOV00.459
        CALL EQUATO(SNOA,MIX,MJX,1,HDOTH,IL,JL,1)                                24SEP99.196
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         OUTTAP.1114
     +                       'WEASD   ',CURRENT_DATE,                            19DEC02.595
     +                       'C','YX','mm',                                      OUTTAP.1116
     +                       'WATER EQUIVALENT SNOW DEPTH',                      OUTTAP.1117
     +                       IX,JX,1,MIX,MJX,1 )                                 OUTTAP.1118
C                                                                                24SEP99.197
C CONVERT SNOWH TO METERS FOR OUTPUT                                             19DEC02.596
      DO I=1,ILX                                                                 19DEC02.597
        DO J=1,JLX                                                               19DEC02.598
          HSCRL(I,J)=SNOWHA(I,J)*0.001                                           19DEC02.599
        ENDDO                                                                    19DEC02.600
      ENDDO                                                                      19DEC02.601
        CALL EQUATO(HSCRL,MIX,MJX,1,HDOTH,IL,JL,1)                               19DEC02.602
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         19DEC02.603
     +                       'SNOWH   ',CURRENT_DATE,                            19DEC02.604
     +                       'C','YX','m',                                       19DEC02.605
     +                       'PHYSICAL SNOW DEPTH',                              19DEC02.606
     +                       IX,JX,1,MIX,MJX,1 )                                 19DEC02.607
C                                                                                19DEC02.608
        CALL EQUATO(SNOWC,MIX,MJX,1,HDOTH,IL,JL,1)                               19DEC02.609
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         19DEC02.610
     +                       'SNOWCOVR',CURRENT_DATE,                            19DEC02.611
     +                       'C','YX','fraction',                                19DEC02.612
     +                       'FRACTIONAL SNOW COVER',                            19DEC02.613
     +                       IX,JX,1,MIX,MJX,1 )                                 19DEC02.614
C                                                                                19DEC02.615
        CALL EQUATO(ALB,MIX,MJX,1,HDOTH,IL,JL,1)                                 19DEC02.616
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         19DEC02.617
     +                       'ALB     ',CURRENT_DATE,                            19DEC02.618
     +                       'C','YX','fraction',                                19DEC02.619
     +                       'ALBEDO',                                           19DEC02.620
     +                       IX,JX,1,MIX,MJX,1 )                                 19DEC02.621
C                                                                                19DEC02.622
        CALL EQUATO(GRNFLX,MIX,MJX,1,HDOTH,IL,JL,1)                              19DEC02.623
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         19DEC02.624
     +                       'GRNFLX  ',CURRENT_DATE,                            19DEC02.625
     +                       'C','YX','W m{-2}',                                 19DEC02.626
     +                       'GROUND HEAT FLUX',                                 19DEC02.627
     +                       IX,JX,1,MIX,MJX,1 )                                 19DEC02.628
C                                                                                19DEC02.629
        CALL EQUATO(VEGFRC,MIX,MJX,1,HDOTH,IL,JL,1)                              19DEC02.630
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         19DEC02.631
     +                     'VEGFRC',CURRENT_DATE,                                19DEC02.632
     +                     'C','YX','fraction',                                  19DEC02.633
     +                     'VEGETATION COVERAGE',                                19DEC02.634
     +                     IX,JX,1,MIX,MJX,1 )                                   19DEC02.635
                                                                                 19DEC02.636
C                                                                                19DEC02.637
        CALL EQUATO(XICE,MIX,MJX,1,HDOTH,IL,JL,1)                                24SEP99.198
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         24SEP99.199
     +                       'SEAICE  ',CURRENT_DATE,                            24SEP99.200
     +                       'C','YX','(DIMENSIONLESS)',                         24SEP99.201
     +                       'SEA ICE FLAG',                                     24SEP99.202
     +                       IX,JX,1,MIX,MJX,1 )                                 24SEP99.203
C                                                                                OUTTAP.1119
        CALL EQUATO(SSROFF,MIX,MJX,1,HDOTH,IL,JL,1)                              24SEP99.204
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         OUTTAP.1121
     +                       'SFCRNOFF ',CURRENT_DATE,                           OUTTAP.1122
     +                       'C','YX','mm',                                      OUTTAP.1123
     +                       'SURFACE RUNOFF',                                   OUTTAP.1124
     +                       IX,JX,1,MIX,MJX,1 )                                 OUTTAP.1125
C                                                                                OUTTAP.1126
        CALL EQUATO(BGROFF,MIX,MJX,1,HDOTH,IL,JL,1)                              24SEP99.205
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         OUTTAP.1128
     +                       'UGDRNOFF ',CURRENT_DATE,                           OUTTAP.1129
     +                       'C','YX','mm',                                      OUTTAP.1130
     +                       'UNDERGROUND RUNOFF',                               OUTTAP.1131
     +                       IX,JX,1,MIX,MJX,1 )                                 OUTTAP.1132
C                                                                                OUTTAP.1133
          IF(RDBRDALB)THEN                                                       19DEC02.638
        DO MONTHLOOP=1,12                                                        19DEC02.639
        WRITE(MONALBNAMECHAR,FMT='("MONALB",I2.2)') MONTHLOOP                    19DEC02.640
#ifndef MPP1                                                                     19DEC02.641
        CALL EQUATO(ALBMON(1,1,MONTHLOOP),MIX,MJX,1,HDOTH,IL,JL,1)               19DEC02.642
#else                                                                            19DEC02.643
        CALL EQUATO(ALBMON(NOFLIC(1),NOFLIC(1),MONTHLOOP),                       19DEC02.644
     +              MIX,MJX,1,HDOTH,IL,JL,1)                                     19DEC02.645
#endif                                                                           19DEC02.646
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         19DEC02.647
     +                       MONALBNAMECHAR,CURRENT_DATE,                        19DEC02.648
     +                       'C','YX','PERCENT',                                 19DEC02.649
     +            'MONTHLY ALBEDO FOR ' // MONTHNAME(MONTHLOOP),                 19DEC02.650
     +                       IX,JX,1,MIX,MJX,1 )                                 19DEC02.651
        END DO                                                                   19DEC02.652
        CALL EQUATO(ALBEDO,MIX,MJX,1,HDOTH,IL,JL,1)                              19DEC02.653
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         19DEC02.654
     +                     'ALBEDO   ',CURRENT_DATE,                             19DEC02.655
     +                     'C','YX','PERCENT',                                   19DEC02.656
     +                     'BACKGROUND ALBEDO',                                  19DEC02.657
     +                     IX,JX,1,MIX,MJX,1 )                                   19DEC02.658
C                                                                                19DEC02.659
                                                                                 19DEC02.660
          ENDIF                                                                  19DEC02.661
C                                                                                19DEC02.662
          IF(RDMAXALB)THEN                                                       19DEC02.663
        CALL EQUATO(ALBMAX,MIX,MJX,1,HDOTH,IL,JL,1)                              19DEC02.664
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         19DEC02.665
     +                       'ALBSNOMX ',CURRENT_DATE,                           19DEC02.666
     +                       'C','YX','PERCENT',                                 19DEC02.667
     +                       'MAXIMUM SNOW ALBEDO',                              19DEC02.668
     +                       IX,JX,1,MIX,MJX,1 )                                 19DEC02.669
          ENDIF                                                                  19DEC02.670
C                                                                                19DEC02.671
        ENDIF                                                                    07NOV00.460
      END IF                                                                     OUTTAP.1134
C                                                                                07NOV00.461
C----- EXTRA OUTPUT FOR PBL:  T2, Q2, U10, and V10:                              05DEC01.286
C                                                                                07NOV00.463
      IF (IBLTYP(NESCOU).EQ.2 .OR. IBLTYP(NESCOU).EQ.4 .OR.                      05DEC01.287
     +     IBLTYP(NESCOU) .EQ. 5) THEN                                           05DEC01.288
         CALL EQUATO(TA2,MIX,MJX,1,HDOTH,IL,JL,1)                                07NOV00.465
         CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                        07NOV00.466
     +                        'T2',CURRENT_DATE,                                 07NOV00.467
     +                        'C','YX','K',                                      07NOV00.468
     +                        '2-meter Temperature',                             07NOV00.469
     +                        IX,JX,1,MIX,MJX,1 )                                07NOV00.470
C                                                                                07NOV00.471
         CALL EQUATO(QA2,MIX,MJX,1,HDOTH,IL,JL,1)                                07NOV00.472
         CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                        07NOV00.473
     +                        'Q2',CURRENT_DATE,                                 07NOV00.474
     +                        'C','YX','kg kg{-1}',                              07NOV00.475
     +                        '2-meter Mixing Ratio',                            07NOV00.476
     +                        IX,JX,1,MIX,MJX,1 )                                07NOV00.477
C                                                                                07NOV00.478
C      MAKE USE OF FILLCRS ROUTINE TO FILL THE LAST ROW AND COLUMN               07NOV00.479
C           OF U10 AND V10                                                       07NOV00.480
C                                                                                07NOV00.481
         CALL FILLCRS(UA10,MIX,MJX,1,IL,JL,1)                                    07NOV00.482
         CALL EQUATO(UA10,MIX,MJX,1,HDOTH,IL,JL,1)                               07NOV00.483
         CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                        07NOV00.484
     +                        'U10',CURRENT_DATE,                                07NOV00.485
     +                        'C','YX','m s{-1}',                                07NOV00.486
     +                        '10-meter U Component',                            07NOV00.487
     +                        IX,JX,1,MIX,MJX,1 )                                07NOV00.488
C                                                                                07NOV00.489
         CALL FILLCRS(VA10,MIX,MJX,1,IL,JL,1)                                    07NOV00.490
         CALL EQUATO(VA10,MIX,MJX,1,HDOTH,IL,JL,1)                               07NOV00.491
         CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                        07NOV00.492
     +                        'V10',CURRENT_DATE,                                07NOV00.493
     +                        'C','YX','m s{-1}',                                07NOV00.494
     +                        '10-meter V Component',                            07NOV00.495
     +                        IX,JX,1,MIX,MJX,1 )                                07NOV00.496
      ENDIF                                                                      07NOV00.497
C                                                                                07NOV00.498
C     MOISTURE AVAILABILITY IF VARYING IN TIME                                   07NOV00.499
C                                                                                07NOV00.500
      IF (IMOIAV(NESCOU) .GE. 1) THEN                                            07NOV00.501
         CALL EQUATO(MAVAIL,MIX,MJX,1,HDOTH,IL,JL,1)                             07NOV00.502
         CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                        07NOV00.503
     +                        'MAVAIL',CURRENT_DATE,                             07NOV00.504
     +                        'C','YX','dimensionless',                          07NOV00.505
     +                        'MOISTURE AVAILABILITY',                           07NOV00.506
     +                        IX,JX,1,MIX,MJX,1 )                                07NOV00.507
      ENDIF                                                                      07NOV00.508
C                                                                                OUTTAP.1135
C----- PHYSICAL PARAMETERS FROM LANDUSE.TBL                                      OUTTAP.1136
C                                                                                OUTTAP.1137
      IF (ISFPAR(NESCOU).EQ.1) THEN                                              07NOV00.509
        CALL EQUATO(ALBD,NLU,NSEAS,1,HDOTH,LUN,NSEAS,1)                          24SEP99.206
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         OUTTAP.1139
     +                       'ALBD ',CURRENT_DATE,                               OUTTAP.1140
     +                       ' ','CA','PERCENT',                                 OUTTAP.1141
     +                       'SURFACE ALBEDO',                                   OUTTAP.1142
     +                       LUN,NSEAS,1,NLU,NSEAS,1 )                           07NOV00.510
C                                                                                OUTTAP.1144
        CALL EQUATO(SLMO,NLU,NSEAS,1,HDOTH,LUN,NSEAS,1)                          24SEP99.207
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         OUTTAP.1146
     +                       'SLMO ',CURRENT_DATE,                               OUTTAP.1147
     +                       ' ','CA','fraction',                                OUTTAP.1148
     +                       'SURFACE MOISTURE AVAILABILITY',                    OUTTAP.1149
     +                       LUN,NSEAS,1,NLU,NSEAS,1 )                           07NOV00.511
C                                                                                OUTTAP.1151
        CALL EQUATO(SFEM,NLU,NSEAS,1,HDOTH,LUN,NSEAS,1)                          24SEP99.208
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         OUTTAP.1153
     +                       'SFEM ',CURRENT_DATE,                               OUTTAP.1154
     +                       ' ','CA','fraction',                                OUTTAP.1155
     +                       'SURFACE EMISSIVITY AT 9 um',                       OUTTAP.1156
     +                       LUN,NSEAS,1,NLU,NSEAS,1 )                           07NOV00.512
C                                                                                OUTTAP.1158
        CALL EQUATO(SFZ0,NLU,NSEAS,1,HDOTH,LUN,NSEAS,1)                          24SEP99.209
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         OUTTAP.1160
     +                       'SFZ0 ',CURRENT_DATE,                               OUTTAP.1161
     +                       ' ','CA','cm',                                      OUTTAP.1162
     +                       'SURFACE ROUGHNESS LENGTH',                         OUTTAP.1163
     +                       LUN,NSEAS,1,NLU,NSEAS,1 )                           07NOV00.513
C                                                                                OUTTAP.1165
        CALL EQUATO(THERIN,NLU,NSEAS,1,HDOTH,LUN,NSEAS,1)                        24SEP99.210
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         OUTTAP.1167
     +                       'THERIN ',CURRENT_DATE,                             OUTTAP.1168
     +                       ' ','CA','100*cal cm^-2 K^-1 s^1/2',                OUTTAP.1169
     +                       'SURFACE THERMAL INERTIA',                          OUTTAP.1170
     +                       LUN,NSEAS,1,NLU,NSEAS,1 )                           07NOV00.514
C                                                                                OUTTAP.1172
        CALL EQUATO(SFHC,NLU,NSEAS,1,HDOTH,LUN,NSEAS,1)                          24SEP99.211
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         OUTTAP.1174
     +                       'SFHC ',CURRENT_DATE,                               OUTTAP.1175
     +                       ' ','CA','J m^-3 K^-1',                             OUTTAP.1176
     +                       'SOIL HEAT CAPACITY',                               OUTTAP.1177
     +                       LUN,NSEAS,1,NLU,NSEAS,1 )                           07NOV00.515
C                                                                                OUTTAP.1179
        CALL EQUATO(SCFX,NLU,1,1,HDOTH,LUN,1,1)                                  24SEP99.212
        CALL WRITE_FIELDREC( iutl,D1,nescou,HDOTH,XTIME,                         OUTTAP.1181
     +                       'SCFX ',CURRENT_DATE,                               OUTTAP.1182
     +                       ' ','CA','fraction',                                OUTTAP.1183
     +                       'SNOW COVER EFFECT',                                OUTTAP.1184
     +                       LUN,1,1,NLU,1,1 )                                   07NOV00.516
      END IF                                                                     07NOV00.517
C                                                                                07NOV00.518
C---- Output 2-d variables if using the PX land surface/PBL model                07NOV00.519
C                                                                                07NOV00.520
      IF (IBLTYP(NESCOU) .EQ. 7) THEN                                            07NOV00.521
C                                                                                07NOV00.522
         CALL EQUATO(TA2,MIX,MJX,1,HDOTH,IL,JL,1)                                19DEC02.672
         CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                        19DEC02.673
     +                        'T2',CURRENT_DATE,                                 19DEC02.674
     +                        'C','YX','K',                                      19DEC02.675
     +                        '1.5-meter Temperature',                           19DEC02.676
     +                        IX,JX,1,MIX,MJX,1 )                                19DEC02.677
C                                                                                19DEC02.678
        CALL EQUATO(MOL,MIX,MJX,1,HDOTH,IL,JL,1)                                 07NOV00.523
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         07NOV00.524
     +                     'M-O LENG',CURRENT_DATE,                              07NOV00.525
     +                     'C','YX','m',                                         07NOV00.526
     +                     'MONIN-OBUKOV LENGTH',                                07NOV00.527
     +                     IX,JX,1,MIX,MJX,1 )                                   07NOV00.528
C                                                                                07NOV00.529
        CALL EQUATO(RADNET,MIX,MJX,1,HDOTH,IL,JL,1)                              07NOV00.530
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         07NOV00.531
     +                     'NET RAD',CURRENT_DATE,                               07NOV00.532
     +                     'C','YX','W/m^2',                                     07NOV00.533
     +                     'SURFACE NET RADIATION',                              07NOV00.534
     +                     IX,JX,1,MIX,MJX,1 )                                   07NOV00.535
C                                                                                07NOV00.536
        CALL EQUATO(ALBEDO,MIX,MJX,1,HDOTH,IL,JL,1)                              07NOV00.537
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         07NOV00.538
     +                     'ALBEDO',CURRENT_DATE,                                07NOV00.539
     +                     'C','YX','fraction',                                  07NOV00.540
     +                     'SURFACE ALBEDO',                                     07NOV00.541
     +                     IX,JX,1,MIX,MJX,1 )                                   07NOV00.542
C                                                                                07NOV00.543
        CALL EQUATO(GRNFLX,MIX,MJX,1,HDOTH,IL,JL,1)                              07NOV00.544
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         07NOV00.545
     +                     'GRNFLX',CURRENT_DATE,                                07NOV00.546
     +                     'C','YX','W/m^2',                                     07NOV00.547
     +                     'GROUND HEAT FLUX',                                   07NOV00.548
     +                     IX,JX,1,MIX,MJX,1 )                                   07NOV00.549
C                                                                                07NOV00.550
        CALL EQUATO(RA,MIX,MJX,1,HDOTH,IL,JL,1)                                  07NOV00.551
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         07NOV00.552
     +                     'RA',CURRENT_DATE,                                    07NOV00.553
     +                     'C','YX','s/m',                                       07NOV00.554
     +                     'AERODYNAMIC RESISTANCE',                             07NOV00.555
     +                     IX,JX,1,MIX,MJX,1 )                                   07NOV00.556
C                                                                                07NOV00.557
        CALL EQUATO(RS,MIX,MJX,1,HDOTH,IL,JL,1)                                  07NOV00.558
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         07NOV00.559
     +                     'RS',CURRENT_DATE,                                    07NOV00.560
     +                     'C','YX','s/m',                                       07NOV00.561
     +                     'SURFACE RESISTANCE',                                 07NOV00.562
     +                     IX,JX,1,MIX,MJX,1 )                                   07NOV00.563
C                                                                                07NOV00.564
        IF(NUDGE.EQ.1) THEN                                                      07NOV00.565
        CALL EQUATO(THZ1OB,MIX,MJX,1,HDOTH,IL,JL,1)                              07NOV00.566
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         07NOV00.567
     +                     'TOB1',CURRENT_DATE,                                  07NOV00.568
     +                     'C','YX','K',                                         07NOV00.569
     +                     'SFC OBS TEMP SCALED TO LOWEST MODEL LEVEL',          07NOV00.570
     +                     IX,JX,1,MIX,MJX,1 )                                   07NOV00.571
C                                                                                07NOV00.572
        CALL EQUATO(RHOBS,MIX,MJX,1,HDOTH,IL,JL,1)                               07NOV00.573
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         07NOV00.574
     +                     'RHOBS',CURRENT_DATE,                                 07NOV00.575
     +                     'C','YX','percent',                                   07NOV00.576
     +                     'OBS. RELATIVE HUMIDITY',                             07NOV00.577
     +                     IX,JX,1,MIX,MJX,1 )                                   07NOV00.578
        ENDIF                                                                    07NOV00.579
                                                                                 07NOV00.580
        CALL EQUATO(LAI,MIX,MJX,1,HDOTH,IL,JL,1)                                 07NOV00.581
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         07NOV00.582
     +                     'LAI',CURRENT_DATE,                                   07NOV00.583
     +                     'C','YX','area/area',                                 07NOV00.584
     +                     'LEAF AREA INDEX',                                    07NOV00.585
     +                     IX,JX,1,MIX,MJX,1 )                                   07NOV00.586
                                                                                 07NOV00.587
        CALL EQUATO(VEGFRC,MIX,MJX,1,HDOTH,IL,JL,1)                              07NOV00.588
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         07NOV00.589
     +                     'VEGFRC',CURRENT_DATE,                                07NOV00.590
     +                     'C','YX','fraction',                                  07NOV00.591
     +                     'VEGETATION COVERAGE',                                07NOV00.592
     +                     IX,JX,1,MIX,MJX,1 )                                   07NOV00.593
                                                                                 07NOV00.594
        CALL EQUATO(ZNT,MIX,MJX,1,HDOTH,IL,JL,1)                                 07NOV00.595
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         07NOV00.596
     +                     'ZNT',CURRENT_DATE,                                   07NOV00.597
     +                     'C','YX','m',                                         07NOV00.598
     +                     'ROUGHNESS LENGTH',                                   07NOV00.599
     +                     IX,JX,1,MIX,MJX,1 )                                   07NOV00.600
                                                                                 07NOV00.601
      DO I=1,IL                                                                  07NOV00.602
        DO J=1,JL                                                                07NOV00.603
          HSCRL(I,J)=FLOAT(ISLTYP(I,J))                                          07NOV00.604
        ENDDO                                                                    07NOV00.605
      ENDDO                                                                      07NOV00.606
        CALL EQUATO(HSCRL,MIX,MJX,1,HDOTH,IL,JL,1)                               07NOV00.607
        CALL WRITE_FIELDREC( iutl,D2,nescou,HDOTH,XTIME,                         07NOV00.608
     +                     'ISLTYP',CURRENT_DATE,                                07NOV00.609
     +                     'C','YX','USDA cat',                                  07NOV00.610
     +                     'SOIL TEXTURE TYPE',                                  07NOV00.611
     +                     IX,JX,1,MIX,MJX,1 )                                   07NOV00.612
C                                                                                07NOV00.613
      ENDIF                                                                      07NOV00.614
C                                                                                OUTTAP.1186
C---- 1D FIELD                                                                   OUTTAP.1187
C                                                                                OUTTAP.1188
      CALL EQUATO(A,MKX,1,1,HDOTH,KL,1,1)                                        24SEP99.213
      CALL WRITE_FIELDREC( iutl,D1,nescou,HDOTH,XTIME,                           OUTTAP.1190
     +                     'SIGMAH',CURRENT_DATE,                                OUTTAP.1191
     +                     'H','S','sigma',                                      OUTTAP.1192
     +                     'VERTICAL COORDINATE',                                OUTTAP.1193
     +                     KZZ,1,1,MKX,1,1 )                                     OUTTAP.1194
C                                                                                OUTTAP.1195
C---- WRITE END OF FILE FLAG                                                     OUTTAP.1196
C                                                                                OUTTAP.1197
      CALL WRITE_FLAG(IUTL,EOT_FLAG)                                             OUTTAP.1198
#ifdef MPP1                                                                      OUTTAP.1199
      IF ( MPP_IO_NODE .EQ. 1 ) THEN                                             OUTTAP.1200
              CALL RSL_OUTPUT_BUFFER_WRITE                                       OUTTAP.1201
      ENDIF                                                                      OUTTAP.1202
#endif                                                                           OUTTAP.1203
C                                                                                OUTTAP.1204
C----- CHECK TO SEE IF OUTPUT FILES NEED TO BE CLOSED                            OUTTAP.1205
C                                                                                OUTTAP.1206
      IF (BUFFRQ.GE.TAPFRQ) THEN                                                 OUTTAP.1207
         IF (MOD(NINT(XTIME/TAPFRQ),NINT(BUFFRQ/TAPFRQ+0.01)).EQ.0               24SEP99.214
     *       .OR. (BUFFRQ.EQ.TAPFRQ .AND. XTIME.GT.0.)) THEN                     24SEP99.215
            IF ( DM_IONODE() )  CLOSE (IUTL)                                     OUTTAP.1210
C           PRINT 940, IUTL                                                      OUTTAP.1211
940         FORMAT(2X,'OUTPUT UNIT ',I2,' IS CLOSED.')                           OUTTAP.1212
         END IF                                                                  OUTTAP.1213
      END IF                                                                     OUTTAP.1214
      RETURN                                                                     OUTTAP.1215
      END                                                                        OUTTAP.1216
