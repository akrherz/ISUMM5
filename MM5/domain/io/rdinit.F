      SUBROUTINE RDINIT(IUNIT,INEST,IX,JX)                                       RDINIT.1
      IMPLICIT NONE                                                              RDINIT.2
C                                                                                RDINIT.3
C     ... THIS SUBROUTINE INPUTS THE INITIAL CONDITION DATA                      RDINIT.4
C                                                                                RDINIT.6
#     include <defines.incl>                                                     07NOV00.615
#     include <parame.incl>                                                      RDINIT.7
#     include <param2.incl>                                                      RDINIT.8
#     include <param3.incl>                                                      RDINIT.9
#     include <addr0.incl>                                                       RDINIT.10
#     include <point3d.incl>                                                     RDINIT.11
#     include <point2d.incl>                                                     RDINIT.12
#     include <various.incl>                                                     RDINIT.13
#     include <pmoist.incl>                                                      19DEC02.700
#     include <nestl.incl>                                                       RDINIT.14
#     include <varia.incl>                                                       RDINIT.15
#     include <dusolve1.incl>                                                    RDINIT.16
#     include <nonhyd.incl>                                                      RDINIT.17
#     include <nonhydb.incl>                                                     RDINIT.18
#     include <nhcnst.incl>                                                      RDINIT.19
#     include <hdrv3.incl>                                                       RDINIT.20
#     include <chardate.incl>                                                    RDINIT.21
#     include <soilp.incl>                                                       RDINIT.22
#     include <soilcnst.incl>                                                    RDINIT.23
#     include <soil.incl>                                                        24SEP99.216
#     include <landinp.incl>                                                     24SEP99.217
#     include <paramsoil_STAS.incl>                                              19DEC02.701
#     include <paramveg_USGS.incl>                                               19DEC02.702
#     include <paramgen_LSM.incl>                                                19DEC02.703
                                                                                 RDINIT.24
      CHARACTER*8 NAME8                                                          RDINIT.28
      CHARACTER*6 NAME6                                                          RDINIT.29
      CHARACTER*5 NAME5                                                          RDINIT.30
      CHARACTER*19 CDATENEW,CDATEIN                                              RDINIT.31
      REAL PDOTA(MIX,MJX)                                                        RDINIT.32
      INTEGER IUNIT,INEST,IX,JX                                                  RDINIT.33
C                                                                                RDINIT.34
C     LOCAL ARRAYS FOR LAND-SURFACE DATA INPUT                                   RDINIT.35
C     NVC=NUMBER OF VEGETATION CATEGORIES (USGS)                                 07NOV00.616
C     NSC=NUMBER OF SOIL CATEGORIES (STATSGO)                                    RDINIT.37
      INTEGER NVC,NSC                                                            05DEC01.292
      PARAMETER (NVC=25, NSC=17)                                                 05DEC01.293
      INTEGER I,J,L,K,ISML,JSML,NL                                               RDINIT.41
      INTEGER NY,NM,ND,MHOUR,MMIN,MSEC,M1,M2                                     RDINIT.42
      INTEGER ICOARSE,NEXTFLG,IER,IWARN,NESTICH,NESTJCH                          RDINIT.43
      INTEGER IXCHECK,JXCHECK,KXCHECK                                            RDINIT.44
      INTEGER NDIM,IDTS,LENDIM1,LENDIM2,LENDIM3,ICAT                             RDINIT.45
      INTEGER IMON,ILYR,ILEV                                                     RDINIT.46
      INTEGER ISTOP                                                              RDINIT.47
      REAL DXCHECK                                                               RDINIT.48
      REAL DUMMY                                                                 RDINIT.49
      REAL REALMON,FRCMON                                                        RDINIT.50
      REAL PCNTMX,TLAPSE,ZANN                                                    RDINIT.51
      REAL VEGCAT(MIXL,MJXL,NVC),SOLCAT(MIXL,MJXL,NSC)                           05DEC01.294
      REAL SOILM,SOILT,SOILW                                                     19DEC02.704
      DIMENSION SOILT(MIXL,MJXL,MLX),SOILM(MIXL,MJXL,MLX),                       19DEC02.705
     -     SOILW(MIXL,MJXL,MLX)                                                  19DEC02.706
      REAL SOILHGT                                                               24SEP99.220
      DIMENSION SOILHGT(MIXL,MJXL)                                               24SEP99.221
      REAL SMLEV,STLEV                                                           24SEP99.222
      DIMENSION STLEV(MLX),SMLEV(MLX)                                            24SEP99.223
      REAL BX,SMCMAX,PSISAT,BLIM,FK,HLICE,GRAV,FREE                              19DEC02.707
      LOGICAL FNDWEASD,FNDSEAIC,FNDTEMPG,FNDSOILI,FNDSOILH,FNDVEGFR,             RDINIT.54
     -        FNDSOILT(MLX),FNDSOILM(MLX),FNDLSMT,FNDLSMM,FNDVEGCT,              07NOV00.618
     -        FNDSOLCT,FNDSOILW(MLX),FNDLSMW,FNDSNOWH,FNDALBMO,FNDALBMX          19DEC02.708
      LOGICAL FNDSOILMA                                                          07NOV00.620
CJM DM_IONODE IS ALWAYS TRUE EXCEPT WHEN COMPILING FOR MPP                       RDINIT.56
      EXTERNAL DM_IONODE                                                         RDINIT.57
      LOGICAL DM_IONODE                                                          RDINIT.58
C                                                                                RDINIT.59
C                                                                                RDINIT.61
      REAL HDOTH(MIX*MJX*(MKX+1))                                                RDINIT.62
      REAL SCR(MIX,MJX)                                                          RDINIT.63
                                                                                 RDINIT.64
      IF (DM_IONODE()) THEN                                                      25JAN00.117
        OPEN(IUNIT,FILE='MMINPUT_DOMAIN'//CHAR(INEST+ICHAR('0')),                RDINIT.66
     &       FORM='UNFORMATTED',STATUS='OLD',ERR=9010)                           25JAN00.118
      ENDIF                                                                      25JAN00.119
      ICOARSE=0                                                                  RDINIT.69
      IF(INEST.EQ.1)ICOARSE=1                                                    RDINIT.70
      IF(IFSKIP)THEN                                                             RDINIT.71
        IF (DM_IONODE()) REWIND IUNIT                                            RDINIT.72
      ENDIF                                                                      RDINIT.73
                                                                                 RDINIT.74
      ISKF = 0                                                                   RDINIT.75
C_FLIC_BEGIN_NOFLIC                                                              RDINIT.76
C                                                                                RDINIT.77
C     always read header                                                         RDINIT.78
C     cdate is the start date of the run corresponding to xtime=0.               RDINIT.79
C     call date add utility (cdate + xtime*60. = cdatenew)                       RDINIT.80
C                                                                                RDINIT.81
      call geth_newdate(cdatenew, cdate, NINT(xtime*60.))                        RDINIT.82
      if (dm_ionode()) rewind iunit                                              RDINIT.83
      NEXTFLG=0                                                                  RDINIT.84
      FNDSOILMA=.FALSE.                                                          07NOV00.621
      IF(ISOIL.EQ.2)THEN                                                         RDINIT.85
C     SET LOGICALS TO CHECK LSM INPUT COMPLETENESS                               RDINIT.86
        FNDWEASD=.FALSE.                                                         RDINIT.87
        FNDSNOWH=.FALSE.                                                         19DEC02.709
        FNDSEAIC=.FALSE.                                                         RDINIT.88
        FNDTEMPG=.FALSE.                                                         RDINIT.89
        FNDSOILI=.FALSE.                                                         RDINIT.90
        FNDSOILH=.FALSE.                                                         RDINIT.91
        FNDVEGFR=.FALSE.                                                         RDINIT.92
        FNDALBMO=.FALSE.                                                         19DEC02.710
        FNDALBMX=.FALSE.                                                         19DEC02.711
        DO NL=1,NSTL                                                             24SEP99.225
          FNDSOILT(NL)=.FALSE.                                                   24SEP99.226
        ENDDO                                                                    24SEP99.227
        DO NL=1,NSML                                                             24SEP99.228
          FNDSOILM(NL)=.FALSE.                                                   24SEP99.229
          FNDSOILW(NL)=.FALSE.                                                   19DEC02.712
        ENDDO                                                                    24SEP99.230
        FNDLSMT=.FALSE.                                                          24SEP99.231
        FNDLSMM=.FALSE.                                                          24SEP99.232
        FNDLSMW=.FALSE.                                                          19DEC02.713
      ENDIF                                                                      RDINIT.95
      IF(ISOIL.EQ.3)THEN                                                         07NOV00.622
        FNDVEGFR=.FALSE.                                                         07NOV00.623
        FNDVEGCT=.FALSE.                                                         07NOV00.624
        FNDSOLCT=.FALSE.                                                         07NOV00.625
        FNDLSMT=.FALSE.                                                          07NOV00.626
        FNDLSMM=.FALSE.                                                          07NOV00.627
      ENDIF                                                                      07NOV00.628
   10 CONTINUE                                                                   RDINIT.96
                                                                                 RDINIT.97
      if (DM_IONODE()) read (iunit,err=9005,end=9006) flag                       RDINIT.98
                                                                                 RDINIT.99
#ifdef MPP1                                                                      RDINIT.100
      call dm_bcast_integers( flag, 1 )                                          RDINIT.101
#endif                                                                           RDINIT.102
                                                                                 RDINIT.103
      if (flag .eq. 0) then                                                      RDINIT.104
        if (dm_ionode()) then                                                    RDINIT.105
           read(iunit,iostat=ier) jbhi, jbhr, jbhic, jbhrc                       RDINIT.106
           if(ier.ne.0) then                                                     RDINIT.107
              write(*,'("Error reading big header")')                            RDINIT.108
              stop                                                               RDINIT.109
           endif                                                                 RDINIT.110
        endif                                                                    RDINIT.111
#ifdef MPP1                                                                      RDINIT.112
        call dm_bcast_integers( jbhi, NUMINT*NUMPROGS)                           RDINIT.113
        call dm_bcast_reals   ( jbhr, NUMREAL*NUMPROGS)                          25JAN00.120
#endif                                                                           RDINIT.115
        IWARN=0                                                                  RDINIT.116
        NESTICH=JBHI(18,1)                                                       RDINIT.117
        NESTJCH=JBHI(19,1)                                                       RDINIT.118
        DXCHECK=JBHR(9,1)                                                        RDINIT.119
        IF(NINT(DXCHECK).NE.NINT(DX))THEN                                        RDINIT.120
          PRINT *,'THE INPUT DATA DOES NOT MATCH EXPECTED GRID SIZE'             RDINIT.121
          PRINT *,'DX FROM MODEL =',DX                                           RDINIT.122
          PRINT *,'DX FROM HEADER=',DXCHECK                                      RDINIT.123
          IWARN=IWARN+1                                                          RDINIT.124
        ENDIF                                                                    RDINIT.125
        IF(ICOARSE.EQ.0)THEN                                                     RDINIT.126
          IF((NESTICH.NE.ISOUTH).OR.(NESTJCH.NE.JWEST))THEN                      RDINIT.127
            PRINT *,'THE INPUT DATA DOES NOT MATCH EXPECTED LOCATION'            RDINIT.128
            PRINT *,'NESTI NESTJ FROM NAMELIST=',ISOUTH,JWEST                    RDINIT.129
            PRINT *,'NESTI NESTJ FROM HEADER  =',NESTICH,NESTJCH                 RDINIT.130
            IWARN=IWARN+1                                                        RDINIT.131
          ENDIF                                                                  RDINIT.132
        ENDIF                                                                    RDINIT.133
        IF(IWARN.GT.0)STOP 'STOP IN RDINIT'                                      RDINIT.134
        IXCHECK=JBHI(16,1)                                                       RDINIT.135
        JXCHECK=JBHI(17,1)                                                       RDINIT.136
        KXCHECK=JBHI(12,5)                                                       RDINIT.137
        IF(KXCHECK.NE.MKX)THEN                                                   RDINIT.138
          PRINT *,'THE INPUT DATA DOES NOT MATCH THE EXPECTED LEVELS'            RDINIT.139
          PRINT *,'MKX FROM MODEL DIMENSION=',MKX                                RDINIT.140
          PRINT *,'MKX FROM INPUT FILE     =',KXCHECK                            RDINIT.141
          CALL SHUTDO                                                            RDINIT.142
          STOP 100                                                               RDINIT.143
        ENDIF                                                                    RDINIT.144
        IF((IXCHECK.NE.IX).OR.(JXCHECK.NE.JX))THEN                               RDINIT.145
          PRINT *,'THE INPUT DATA DOES NOT MATCH THE EXPECTED SIZE'              RDINIT.146
          PRINT *,'NESTIX NESTJX FROM NAMELIST=',IX,JX                           RDINIT.147
          PRINT *,'NESTIX NESTJX FROM HEADER  =',IXCHECK,JXCHECK                 RDINIT.148
          PRINT *,'ASSUMING THE DOMAINS ARE NOT ASSIGNED CORRECTLY'              RDINIT.149
          CALL SHUTDO                                                            RDINIT.150
          STOP 100                                                               RDINIT.151
        ENDIF                                                                    RDINIT.152
                                                                                 RDINIT.153
      goto 10                                                                    RDINIT.154
      elseif (flag .eq. 1) then                                                  RDINIT.155
        if (dm_ionode()) then                                                    RDINIT.156
          READ (iunit,iostat=ier) ndim, start_index, end_index, rtime,           RDINIT.157
     -       staggering, ordering, current_date, name, units,                    RDINIT.158
     -       description                                                         RDINIT.159
          if(ier.ne.0) then                                                      RDINIT.160
            write(*,'("Error reading subheader")')                               RDINIT.161
            stop                                                                 RDINIT.162
          endif                                                                  RDINIT.163
        endif                                                                    RDINIT.164
#ifdef MPP1                                                                      RDINIT.165
        call dm_bcast_integers (ndim,          1               )                 RDINIT.166
        call dm_bcast_integers (start_index,   LEN_START_INDEX )                 RDINIT.167
        call dm_bcast_integers (end_index,     LEN_END_INDEX   )                 RDINIT.168
        call dm_bcast_reals    (rtime,         1               )                 RDINIT.169
        call dm_bcast_string   (staggering,    LEN_STAGGERING  )                 RDINIT.170
        call dm_bcast_string   (ordering,      LEN_ORDERING    )                 RDINIT.171
        call dm_bcast_string   (current_date,  LEN_CURRENT_DATE)                 RDINIT.172
        call dm_bcast_string   (name,          LEN_NAME        )                 RDINIT.173
        call dm_bcast_string   (units,         LEN_UNITS       )                 RDINIT.174
        call dm_bcast_string   (description,   LEN_DESCRIPTION )                 RDINIT.175
#endif                                                                           RDINIT.176
        cdatein=current_date                                                     RDINIT.177
c call date diff utility (cdatein - cdatenew,  tolerance = 1 min)                RDINIT.178
        call geth_idts (cdatenew, cdatein, idts)                                 RDINIT.179
        IF(abs(idts) .gt. 60)THEN                                                RDINIT.180
          IF(NEXTFLG.EQ.0)THEN                                                   RDINIT.181
            PRINT *,'READ IN MODEL INPUT AT DATE = ',cdatein                     RDINIT.182
            PRINT *,'DATE OF MMINPUT DOES NOT MATCH CURRENT MODEL TIME'          RDINIT.183
            PRINT *,'DATE INPUT =',cdatein,' DATE MODEL =',cdatenew              RDINIT.184
          ENDIF                                                                  RDINIT.185
          NEXTFLG=1                                                              RDINIT.186
        ELSE                                                                     RDINIT.188
          IF(NEXTFLG.EQ.1)THEN                                                   RDINIT.189
            PRINT *,'DATE OF MMINPUT MATCHES MODEL START TIME ',cdatenew         RDINIT.190
          ENDIF                                                                  RDINIT.191
          NEXTFLG=0                                                              RDINIT.192
        ENDIF                                                                    RDINIT.193
        LENDIM1 = (END_INDEX(1)-START_INDEX(1)+1)                                RDINIT.194
        LENDIM2 = (END_INDEX(2)-START_INDEX(2)+1)                                RDINIT.195
        LENDIM3 = (END_INDEX(3)-START_INDEX(3)+1)                                RDINIT.196
        IF ( NDIM .GE. 2 ) THEN                                                  RDINIT.197
#ifndef MPP1                                                                     RDINIT.198
          IF ( LENDIM1*LENDIM2*LENDIM3 .LE. MIX*MJX*(MKX+1) ) THEN               RDINIT.199
            READ(IUNIT,ERR=9004)(HDOTH(I),I=1,LENDIM1*LENDIM2*LENDIM3)           RDINIT.200
          ENDIF                                                                  RDINIT.201
#else                                                                            RDINIT.202
          DO I = 1, MIX*MJX*(MKX+1)                                              RDINIT.203
            HDOTH(I) = 0.                                                        RDINIT.204
          ENDDO                                                                  RDINIT.205
          CALL DM_DIST_READ(IUNIT,INEST,HDOTH,NDIM,                              RDINIT.206
     +                      LENDIM1,LENDIM2,LENDIM3,MIX,MJX,MKX+1)               RDINIT.207
#endif                                                                           RDINIT.208
        ELSE                                                                     RDINIT.209
          IF (DM_IONODE()) READ(IUNIT)DUMMY                                      RDINIT.210
        ENDIF                                                                    RDINIT.211
      IF(NEXTFLG.EQ.0)THEN                                                       RDINIT.212
C     THE FOLLOWING READS ALL FIELDS IN ANY ORDER AND ONLY ASSIGNS               RDINIT.213
C       INPUT TO MEMORY IF THE FIELD IS RECOGNIZED AND DIMENSIONED IN            RDINIT.214
C       THE MODEL                                                                RDINIT.215
        NAME8=name(1:8)                                                          RDINIT.216
        NAME6=name(1:6)                                                          RDINIT.217
        NAME5=name(1:5)                                                          RDINIT.218
        IF(IFPRT.EQ.1)print *,'**',name8,'**'                                    RDINIT.219
        IF(NAME8.EQ.'U       ')THEN                                              RDINIT.220
          CALL EQUATE(HDOTH,IX,JX,MKX,UA,MIX,MJX,MKX)                            RDINIT.221
        ELSE IF(NAME8.EQ.'V       ')THEN                                         RDINIT.222
          CALL EQUATE(HDOTH,IX,JX,MKX,VA,MIX,MJX,MKX)                            RDINIT.223
        ELSE IF(NAME8.EQ.'T       ')THEN                                         RDINIT.224
          CALL EQUATE(HDOTH,IX,JX,MKX,TA,MIX,MJX,MKX)                            RDINIT.225
        ELSE IF(NAME8.EQ.'Q       ')THEN                                         RDINIT.226
          CALL EQUATE(HDOTH,IX,JX,MKX,QVA,MIX,MJX,MKX)                           RDINIT.227
        ELSE IF(NAME8.EQ.'W       '                                              RDINIT.228
     -          .AND.MIXNH.GT.1)THEN                                             RDINIT.229
          CALL EQUATE(HDOTH,IX,JX,MKXNH+1,WA,MIXNH,MJXNH,MKXNH+1)                RDINIT.230
        ELSE IF(NAME8.EQ.'PP      '                                              RDINIT.231
     -          .AND.MIXNH.GT.1)THEN                                             RDINIT.232
          CALL EQUATE(HDOTH,IX,JX,MKXNH,PPA,MIXNH,MJXNH,MKXNH)                   RDINIT.233
        ELSE IF(NAME8.EQ.'CLW     '                                              RDINIT.234
     -          .AND.MIXM.GT.1)THEN                                              RDINIT.235
          CALL EQUATE(HDOTH,IX,JX,MKXM,QCA,MIXM,MJXM,MKXM)                       RDINIT.236
        ELSE IF(NAME8.EQ.'RNW     '                                              RDINIT.237
     -          .AND.MIXM.GT.1)THEN                                              RDINIT.238
          CALL EQUATE(HDOTH,IX,JX,MKXM,QRA,MIXM,MJXM,MKXM)                       RDINIT.239
        ELSE IF(NAME8.EQ.'ICE     '                                              RDINIT.240
     -          .AND.MIXIC.GT.1)THEN                                             RDINIT.241
          CALL EQUATE(HDOTH,IX,JX,MKXIC,QIA,MIXIC,MJXIC,MKXIC)                   RDINIT.242
        ELSE IF(NAME8.EQ.'SNOW    '                                              RDINIT.243
     -          .AND.MIXIC.GT.1)THEN                                             RDINIT.244
          CALL EQUATE(HDOTH,IX,JX,MKXIC,QNIA,MIXIC,MJXIC,MKXIC)                  RDINIT.245
        ELSE IF(NAME8.EQ.'GRAUPEL '                                              RDINIT.246
     -          .AND.MIXIG.GT.1)THEN                                             RDINIT.247
          CALL EQUATE(HDOTH,IX,JX,MKXIG,QGA,MIXIG,MJXIG,MKXIG)                   RDINIT.248
        ELSE IF(NAME8.EQ.'NCI     '                                              RDINIT.249
     -          .AND.MIXIG.GT.1)THEN                                             RDINIT.250
          CALL EQUATE(HDOTH,IX,JX,MKXIG,QNCA,MIXIG,MJXIG,MKXIG)                  RDINIT.251
c   all 3d fields need to be coupled too                                         RDINIT.252
        ELSE IF(NAME8.EQ.'PSTARCRS')THEN                                         RDINIT.253
          CALL EQUATE(HDOTH,IX,JX,1,PSA,MIX,MJX,1)                               RDINIT.254
        ELSE IF(NAME8.EQ.'CORIOLIS')THEN                                         RDINIT.255
          CALL EQUATE(HDOTH,IX,JX,1,F,MIX,MJX,1)                                 RDINIT.256
        ELSE IF(NAME8.EQ.'MAPFACCR')THEN                                         RDINIT.257
          CALL EQUATE(HDOTH,IX,JX,1,MSFX,MIX,MJX,1)                              RDINIT.258
        ELSE IF(NAME8.EQ.'MAPFACDT')THEN                                         RDINIT.259
          CALL EQUATE(HDOTH,IX,JX,1,MSFD,MIX,MJX,1)                              RDINIT.260
        ELSE IF(NAME8.EQ.'LATITCRS')THEN                                         RDINIT.261
          CALL EQUATE(HDOTH,IX,JX,1,XLAT,MIX,MJX,1)                              RDINIT.262
        ELSE IF(NAME8.EQ.'LONGICRS')THEN                                         RDINIT.263
          CALL EQUATE(HDOTH,IX,JX,1,XLONG,MIX,MJX,1)                             RDINIT.264
        ELSE IF(NAME8.EQ.'GROUND T')THEN                                         RDINIT.265
          CALL EQUATE(HDOTH,IX,JX,1,TGA,MIX,MJX,1)                               RDINIT.266
          CALL EQUATE(TGA,MIX,MJX,1,TGB,MIX,MJX,1)                               RDINIT.267
        ELSE IF(NAME8.EQ.'TSEASFC ')THEN                                         07NOV00.629
          CALL EQUATE(HDOTH,IX,JX,1,TSS,MIX,MJX,1)                               07NOV00.630
        ELSE IF(NAME8.EQ.'TERRAIN ')THEN                                         RDINIT.268
          CALL EQUATE(HDOTH,IX,JX,1,HT,MIX,MJX,1)                                RDINIT.269
        ELSE IF(NAME8.EQ.'LAND USE')THEN                                         RDINIT.270
          CALL EQUATE(HDOTH,IX,JX,1,SATBRT,MIX,MJX,1)                            RDINIT.271
C_FLIC_END_NOFLIC                                                                RDINIT.272
          IF(ISOIL.EQ.2)THEN                                                     RDINIT.273
          DO I=1,IX                                                              RDINIT.274
          DO J=1,JX                                                              RDINIT.275
             IVGTYP(I,J)=NINT(SATBRT(I,J))                                       RDINIT.276
          ENDDO                                                                  RDINIT.277
          ENDDO                                                                  RDINIT.278
          ENDIF                                                                  RDINIT.279
C_FLIC_BEGIN_NOFLIC                                                              RDINIT.280
        ELSE IF(NAME8.EQ.'SNOWCOVR')THEN                                         RDINIT.281
          IF(IFSNOW(INEST).NE.2)THEN                                             05DEC01.295
            IF(.NOT.FNDWEASD)CALL EQUATE(HDOTH,IX,JX,1,SNOWC,MIX,MJX,1)          05DEC01.296
          ELSE                                                                   05DEC01.297
            IF(IFPRT.EQ.1)PRINT *,'FIELD ',NAME8,' SKIPPED'                      05DEC01.298
          ENDIF                                                                  05DEC01.299
        ELSE IF((NAME8.EQ.'WEASD   ')                                            19DEC02.714
     -          .AND.MIXL.GT.1)THEN                                              RDINIT.284
          FNDWEASD=.TRUE.                                                        RDINIT.285
          CALL EQUATE(HDOTH,IX,JX,1,SNOA,MIXL,MJXL,1)                            RDINIT.286
          CALL EQUATE(SNOA,MIXL,MJXL,1,SNOB,MIXL,MJXL,1)                         RDINIT.287
C_FLIC_END_NOFLIC                                                                07NOV00.632
          DO I=1,IX                                                              RDINIT.288
          DO J=1,JX                                                              RDINIT.289
C   SET SNOWC TO 1 FOR SNOA > 1.0 MM                                             24SEP99.236
             IF(SNOA(I,J).GT.1.0)THEN                                            24SEP99.237
               SNOWC(I,J)=1.0                                                    RDINIT.291
             ELSE                                                                RDINIT.292
               SNOWC(I,J)=0.0                                                    RDINIT.293
             ENDIF                                                               RDINIT.294
          ENDDO                                                                  RDINIT.295
          ENDDO                                                                  RDINIT.296
C_FLIC_BEGIN_NOFLIC                                                              05DEC01.300
        ELSE IF((NAME8.EQ.'WEASD   ')                                            19DEC02.715
     -          .AND.IFSNOW(INEST).EQ.2)THEN                                     05DEC01.302
          FNDWEASD=.TRUE.                                                        05DEC01.303
          CALL EQUATE(HDOTH,IX,JX,1,SNOWC,MIX,MJX,1)                             05DEC01.304
C_FLIC_END_NOCFLIC                                                               05DEC01.305
          DO J=1,JX                                                              05DEC01.306
          DO I=1,IX                                                              05DEC01.307
            SNOWC(I,J)=AMAX1(SNOWC(I,J),0.0)                                     05DEC01.308
            IF(SNOWC(I,J).LT.0.01)SNOWC(I,J)=0.                                  19DEC02.716
          ENDDO                                                                  05DEC01.309
          ENDDO                                                                  05DEC01.310
          IF(IFPRT.EQ.1)print *,'weasd found and put in snowc'                   05DEC01.311
C_FLIC_BEGIN_NOFLIC                                                              07NOV00.633
        ELSE IF(NAME8.EQ.'CANOPYM '                                              24SEP99.238
     -          .AND.MIXL.GT.1)THEN                                              24SEP99.239
          CALL EQUATE(HDOTH,IX,JX,1,CMCA,MIXL,MJXL,1)                            24SEP99.240
          CALL EQUATE(HDOTH,IX,JX,1,CMCB,MIXL,MJXL,1)                            24SEP99.241
        ELSE IF(NAME8.EQ.'SNOWH   '                                              19DEC02.717
     -          .AND.MIXL.GT.1)THEN                                              19DEC02.718
          FNDSNOWH=.TRUE.                                                        19DEC02.719
          CALL EQUATE(HDOTH,IX,JX,1,SNOWHA,MIXL,MJXL,1)                          19DEC02.720
          CALL EQUATE(HDOTH,IX,JX,1,SNOWHB,MIXL,MJXL,1)                          19DEC02.721
        ELSE IF(NAME8.EQ.'SEAICE  '                                              RDINIT.297
     -          .AND.MIXL.GT.1)THEN                                              RDINIT.298
          FNDSEAIC=.TRUE.                                                        RDINIT.299
          CALL EQUATE(HDOTH,IX,JX,1,XICE,MIXL,MJXL,1)                            RDINIT.300
        ELSE IF(NAME8.EQ.'TEMPGRD '.AND.ISOIL.EQ.2)THEN                          07NOV00.634
          CALL EQUATE(HDOTH,IX,JX,1,TMN,MIX,MJX,1)                               RDINIT.302
          FNDTEMPG=.TRUE.                                                        RDINIT.303
        ELSE IF(NAME8.EQ.'SOILINDX'.AND.MIXL.GT.1)THEN                           RDINIT.304
          FNDSOILI=.TRUE.                                                        RDINIT.305
          CALL EQUATE(HDOTH,IX,JX,1,SCR,MIX,MJX,1)                               RDINIT.306
C_FLIC_END_NOFLIC                                                                RDINIT.307
          DO I=1,IX                                                              RDINIT.308
          DO J=1,JX                                                              RDINIT.309
             ISLTYP(I,J)=NINT(SCR(I,J))                                          RDINIT.310
          ENDDO                                                                  RDINIT.311
          ENDDO                                                                  RDINIT.312
C_FLIC_BEGIN_NOFLIC                                                              RDINIT.313
        ELSE IF(NAME8.EQ.'SOILHGT '                                              RDINIT.314
     -          .AND.MIXL.GT.1)THEN                                              RDINIT.315
          FNDSOILH=.TRUE.                                                        RDINIT.316
          CALL EQUATE(HDOTH,IX,JX,1,SOILHGT,MIXL,MJXL,1)                         RDINIT.317
        ELSE IF(NAME8.EQ.'ALBSNOMX'                                              19DEC02.722
     -          .AND.MIXL.GT.1)THEN                                              19DEC02.723
          FNDALBMX=.TRUE.                                                        19DEC02.724
          CALL EQUATE(HDOTH,IX,JX,1,ALBMAX,MIXL,MJXL,1)                          19DEC02.725
        ELSE IF(NAME6.EQ.'VEGCAT'                                                RDINIT.318
     -          .AND.MIXL.GT.1)THEN                                              RDINIT.319
          READ(NAME8(7:8),'(I2)')ICAT                                            RDINIT.320
          FNDVEGCT=.TRUE.                                                        07NOV00.635
          CALL EQUATE(HDOTH,IX,JX,1,VEGCAT(1,1,ICAT),MIXL,MJXL,1)                RDINIT.321
        ELSE IF(NAME6.EQ.'SOICAT'                                                07NOV00.636
     -          .AND.MIXL.GT.1)THEN                                              RDINIT.323
          READ(NAME8(7:8),'(I2)')ICAT                                            RDINIT.324
          FNDSOLCT=.TRUE.                                                        07NOV00.637
          CALL EQUATE(HDOTH,IX,JX,1,SOLCAT(1,1,ICAT),MIXL,MJXL,1)                RDINIT.325
        ELSE IF(NAME6.EQ.'VEGFRC' .AND. NAME8(7:8).NE.'  '                       19DEC02.726
     -          .AND.MIXL.GT.1)THEN                                              RDINIT.327
          READ(NAME8(7:8),'(I2)')IMON                                            RDINIT.328
          FNDVEGFR=.TRUE.                                                        RDINIT.329
          CALL EQUATE(HDOTH,IX,JX,1,VEGFMO(1,1,IMON),MIXL,MJXL,1)                RDINIT.330
        ELSE IF(NAME6.EQ.'MONALB'                                                19DEC02.727
     -          .AND.MIXL.GT.1)THEN                                              19DEC02.728
          READ(NAME8(7:8),'(I2)')IMON                                            19DEC02.729
          FNDALBMO=.TRUE.                                                        19DEC02.730
          CALL EQUATE(HDOTH,IX,JX,1,ALBMON(1,1,IMON),MIXL,MJXL,1)                19DEC02.731
        ELSE IF(NAME5.EQ.'SOILT'                                                 RDINIT.331
     -          .AND.MIXL.GT.1)THEN                                              RDINIT.332
          READ(NAME8(6:8),'(I3)')ILYR                                            RDINIT.333
          ILEV=0                                                                 RDINIT.334
          DO NL=1,NSTL                                                           RDINIT.335
            IF(ILYR.EQ.ISTLYR(NL))ILEV=NL                                        RDINIT.336
          ENDDO                                                                  RDINIT.337
          IF(ILEV.NE.0)THEN                                                      RDINIT.338
            FNDSOILT(ILEV)=.TRUE.                                                24SEP99.242
            CALL EQUATE(HDOTH,IX,JX,1,SOILT(1,1,ILEV),MIXL,MJXL,1)               RDINIT.340
          ELSE                                                                   RDINIT.341
            PRINT *,NAME8,' LAYER NOT RECOGNIZED'                                RDINIT.342
          ENDIF                                                                  RDINIT.343
C       ELSE IF(NAME5.EQ.'SOILM'                                                 07NOV00.638
C    -          .AND.MIXL.GT.1)THEN                                              07NOV00.639
        ELSE IF(NAME5.EQ.'SOILM') THEN                                           07NOV00.640
          IF (MIXL.GT.1) THEN                                                    07NOV00.641
            READ(NAME8(6:8),'(I3)')ILYR                                          07NOV00.642
            ILEV=0                                                               07NOV00.643
            DO NL=1,NSML                                                         07NOV00.644
              IF(ILYR.EQ.ISMLYR(NL))ILEV=NL                                      07NOV00.645
            ENDDO                                                                07NOV00.646
            IF(ILEV.NE.0)THEN                                                    07NOV00.647
              FNDSOILM(ILEV)=.TRUE.                                              07NOV00.648
              CALL EQUATE(HDOTH,IX,JX,1,SOILM(1,1,ILEV),MIXL,MJXL,1)             07NOV00.649
            ELSE                                                                 07NOV00.650
              PRINT *,NAME8,' LAYER NOT RECOGNIZED'                              07NOV00.651
            ENDIF                                                                07NOV00.652
          ELSE IF(NAME8.EQ.'SOILM010' .AND. IMOIAV(INEST).EQ.2) THEN             07NOV00.653
C IF SOIL MOISTURE FIELD IS PRESENT AND IMOIAV = 2, USE IT TO DEFINE             07NOV00.654
C    MOISTURE AVAILABILITY IN THE MODEL, INSTEAD OF TABLE VALUE                  07NOV00.655
            FNDSOILMA=.TRUE.                                                     07NOV00.656
            CALL EQUATE(HDOTH,IX,JX,1,MAVAIL(1,1),MIX,MJX,1)                     07NOV00.657
C CONVERT SOIL MOISTURE TO MOISTURE AVAILIBILITY (FROM J. BRESCH)                07NOV00.658
C_FLIC_END_NOFLIC                                                                07NOV00.659
            DO I = 1,IX                                                          07NOV00.660
              DO J = 1,JX                                                        07NOV00.661
                IF (MAVAIL(I,J) .LT. 0.02) THEN                                  07NOV00.662
                  MAVAIL(I,J) = 0.02                                             07NOV00.663
                ELSE IF (MAVAIL(I,J) .GT. 0.35) THEN                             07NOV00.664
                  MAVAIL(I,J) = 0.7                                              07NOV00.665
                ELSE                                                             07NOV00.666
                  MAVAIL(I,J) = MAVAIL(I,J)*2. + (.5-MAVAIL(I,J))*.09            07NOV00.667
                END IF                                                           07NOV00.668
              END DO                                                             07NOV00.669
            END DO                                                               07NOV00.670
C_FLIC_BEGIN_NOFLIC                                                              07NOV00.671
          ENDIF                                                                  07NOV00.672
        ELSE IF(NAME5.EQ.'SOILW' .AND. MIXL.GT.1 ) THEN                          19DEC02.732
            READ(NAME8(6:8),'(I3)')ILYR                                          19DEC02.733
            ILEV=0                                                               19DEC02.734
            DO NL=1,NSML                                                         19DEC02.735
              IF(ILYR.EQ.ISMLYR(NL))ILEV=NL                                      19DEC02.736
            ENDDO                                                                19DEC02.737
            IF(ILEV.NE.0)THEN                                                    19DEC02.738
              FNDSOILW(ILEV)=.TRUE.                                              19DEC02.739
              CALL EQUATE(HDOTH,IX,JX,1,SOILW(1,1,ILEV),MIXL,MJXL,1)             19DEC02.740
            ELSE                                                                 19DEC02.741
              PRINT *,NAME8,' LAYER NOT RECOGNIZED'                              19DEC02.742
            ENDIF                                                                19DEC02.743
C CALCULATING MOISTURE AVAILABILITY FROM SOIL MOISTURE FIELD                     07NOV00.673
        ELSE IF(NAME6.EQ.'SOIL T'                                                24SEP99.244
     -          .AND.MIXL.GT.1)THEN                                              24SEP99.245
          READ(NAME(7:8),'(I2)')ILEV                                             24SEP99.246
          FNDLSMT=.TRUE.                                                         24SEP99.247
          IF(ILEV.GT.MLX)THEN                                                    24SEP99.248
            PRINT *,'ISOIL=2 CANNOT TAKE INPUT FROM ISOIL=1 RUN'                 24SEP99.249
            STOP                                                                 24SEP99.250
          ENDIF                                                                  24SEP99.251
          CALL EQUATE(HDOTH,IX,JX,1,STCA(1,1,ILEV),MIXL,MJXL,1)                  24SEP99.252
          CALL EQUATE(STCA(1,1,ILEV),MIXL,MJXL,1,STCB(1,1,ILEV),                 24SEP99.253
     -                MIXL,MJXL,1)                                               24SEP99.254
        ELSE IF(NAME6.EQ.'SOIL T'                                                24SEP99.255
     -          .AND.MIXS.GT.1)THEN                                              24SEP99.256
          READ(NAME(7:8),'(I2)')ILEV                                             24SEP99.257
          CALL EQUATE(HDOTH,IX,JX,1,TSLA(1,1,ILEV),MIXS,MJXS,1)                  24SEP99.258
          CALL EQUATE(TSLA(1,1,ILEV),MIXS,MJXS,1,TSLB(1,1,ILEV),                 24SEP99.259
     -                MIXS,MJXS,1)                                               24SEP99.260
        ELSE IF(NAME6.EQ.'SOIL M'                                                24SEP99.261
     -          .AND.MIXL.GT.1)THEN                                              24SEP99.262
          READ(NAME(7:8),'(I2)')ILEV                                             24SEP99.263
          FNDLSMM=.TRUE.                                                         24SEP99.264
          CALL EQUATE(HDOTH,IX,JX,1,SMCA(1,1,ILEV),MIXL,MJXL,1)                  24SEP99.265
          CALL EQUATE(SMCA(1,1,ILEV),MIXL,MJXL,1,SMCB(1,1,ILEV),                 24SEP99.266
     -                MIXL,MJXL,1)                                               24SEP99.267
        ELSE IF(NAME6.EQ.'SOIL W'                                                19DEC02.744
     -          .AND.MIXL.GT.1)THEN                                              19DEC02.745
          READ(NAME(7:8),'(I2)')ILEV                                             19DEC02.746
          FNDLSMW=.TRUE.                                                         19DEC02.747
          CALL EQUATE(HDOTH,IX,JX,1,SWCA(1,1,ILEV),MIXL,MJXL,1)                  19DEC02.748
          CALL EQUATE(SWCA(1,1,ILEV),MIXL,MJXL,1,SWCB(1,1,ILEV),                 19DEC02.749
     -                MIXL,MJXL,1)                                               19DEC02.750
        ELSE IF(NAME6.EQ.'SOIL M'                                                24SEP99.268
     -          .AND.MIXS.GT.1)THEN                                              24SEP99.269
          PRINT *,'ISOIL=1 CANNOT TAKE INPUT FROM ISOIL=2 RUN'                   24SEP99.270
          STOP                                                                   24SEP99.271
        ELSE                                                                     RDINIT.357
          IF(IFPRT.EQ.1)PRINT *,'FIELD ',NAME8,' SKIPPED'                        RDINIT.358
        ENDIF                                                                    RDINIT.359
      ENDIF                                                                      RDINIT.360
      goto 10                                                                    RDINIT.361
      elseif (flag .eq. 2) then                                                  RDINIT.362
          IF(NEXTFLG.EQ.1)THEN                                                   RDINIT.363
            PRINT *,'SKIPPING TO NEXT INPUT TIME'                                RDINIT.364
            ISKF = ISKF+1                                                        07NOV00.674
            GOTO 10                                                              RDINIT.365
          ELSE                                                                   RDINIT.366
            PRINT *,'END OF INPUT TIME'                                          RDINIT.367
          ENDIF                                                                  RDINIT.368
      else                                                                       RDINIT.369
          print *,'UNRECOGNIZED FLAG VALUE = ', FLAG                             RDINIT.370
          print *,'PROBABLY NOT VERSION 3 DATA'                                  RDINIT.371
          go to 9004                                                             RDINIT.372
      endif                                                                      RDINIT.373
C_FLIC_END_NOFLIC                                                                RDINIT.374
                                                                                 RDINIT.375
#ifdef MPP1                                                                      RDINIT.376
#  include <mpp_rdinit_10.incl>                                                  RDINIT.377
#endif                                                                           RDINIT.378
                                                                                 RDINIT.379
      ISTOP=0                                                                    RDINIT.380
      IF(IFSNOW(INEST).EQ.2.AND.(.NOT.FNDWEASD))THEN                             05DEC01.312
           PRINT *,                                                              05DEC01.313
     -     'NO WATER EQUIVALENT SNOW DEPTH FOUND: CHECK REGRID JOB'              05DEC01.314
           PRINT *,'IFSNOW=2 REQUIRES SNOW DEPTH FIELD IN INPUT FILE'            05DEC01.315
           ISTOP=ISTOP+1                                                         05DEC01.316
      ENDIF                                                                      05DEC01.317
      IF(ISOIL.EQ.2)THEN                                                         RDINIT.381
C     CHECK COMPLETENESS OF INPUT FOR LSM                                        RDINIT.382
        IF(.NOT.FNDWEASD)                                                        RDINIT.383
     -     PRINT *,'NO WATER EQUIVALENT SNOW DEPTH FOUND: ASSUMED ZERO'          RDINIT.384
        IF(.NOT.FNDSEAIC)                                                        RDINIT.385
     -     PRINT *,'NO SEA ICE FOUND: ASSUMED ZERO'                              RDINIT.386
        IF(.NOT.FNDTEMPG)THEN                                                    RDINIT.387
           PRINT *,'NO ANNUAL-MEAN TEMPERATURE FOUND: CHECK TERRAIN JOB'         RDINIT.388
           ISTOP=ISTOP+1                                                         RDINIT.389
        ENDIF                                                                    RDINIT.390
        IF(.NOT.FNDSOILI)THEN                                                    RDINIT.391
           PRINT *,'NO SOIL INDEX FOUND: CHECK TERRAIN JOB'                      RDINIT.392
           ISTOP=ISTOP+1                                                         RDINIT.393
        ENDIF                                                                    RDINIT.394
        IF(.NOT.FNDVEGFR)THEN                                                    RDINIT.399
           PRINT *,'NO VEG FRACTION FOUND: CHECK TERRAIN JOB'                    RDINIT.400
           ISTOP=ISTOP+1                                                         RDINIT.401
        ENDIF                                                                    RDINIT.402
        IF(RDMAXALB.AND..NOT.FNDALBMX)THEN                                       19DEC02.751
           PRINT *,'RDMAXALB=.TRUE. BUT ALBSNOMX NOT FOUND: '                    19DEC02.752
           PRINT *,'   CHECK REGRID JOB OR SET RDMAXALB=.FALSE.'                 19DEC02.753
           ISTOP=ISTOP+1                                                         19DEC02.754
        ENDIF                                                                    19DEC02.755
        IF(RDBRDALB.AND..NOT.FNDALBMO)THEN                                       19DEC02.756
           PRINT *,'RDBRDALB=.TRUE. BUT MONALBnn NOT FOUND: '                    19DEC02.757
           PRINT *,'   CHECK REGRID JOB OR SET RDBRDALB=.FALSE.'                 19DEC02.758
           ISTOP=ISTOP+1                                                         19DEC02.759
        ENDIF                                                                    19DEC02.760
        DO NL=1,NSTL                                                             24SEP99.273
        IF(.NOT.FNDSOILT(NL).AND..NOT.FNDLSMT)THEN                               24SEP99.274
           PRINT *,'SOIL TEMP NOT FOUND: CHECK REGRID JOB'                       24SEP99.275
           PRINT *,'SOILT LAYER = ',ISTLYR(NL)                                   24SEP99.276
           ISTOP=ISTOP+1                                                         RDINIT.405
        ENDIF                                                                    RDINIT.406
        ENDDO                                                                    24SEP99.277
        DO NL=1,NSML                                                             24SEP99.278
        IF(.NOT.FNDSOILM(NL).AND..NOT.FNDLSMM)THEN                               24SEP99.279
           PRINT *,'SOIL MOISTURE NOT FOUND: CHECK REGRID JOB'                   24SEP99.280
           PRINT *,'SOILM LAYER = ',ISMLYR(NL)                                   24SEP99.281
           ISTOP=ISTOP+1                                                         RDINIT.409
        ENDIF                                                                    RDINIT.410
        ENDDO                                                                    24SEP99.282
        IF(ISTOP.GT.0)THEN                                                       RDINIT.411
          PRINT *,'INPUT FILE MISSING FIELDS FOR ISOIL=2: SEE ABOVE'             19DEC02.761
          STOP                                                                   RDINIT.413
        ENDIF                                                                    RDINIT.414
      ENDIF                                                                      RDINIT.415
C FOR IMOIAV = 2                                                                 07NOV00.675
      IF (.NOT.FNDSOILMA .AND. IMOIAV(INEST).EQ.2) THEN                          07NOV00.676
         PRINT *, '10 CM SOIL MOISTURE NOT FOUND FOR DOMAIN ', INEST             07NOV00.677
         PRINT *, 'CHECK REGRID JOB OR RESET IMOIAV = 1 AND RUN AGAIN'           07NOV00.678
         STOP                                                                    07NOV00.679
      END IF                                                                     07NOV00.680
C                                                                                RDINIT.416
C     CALCULATE PS0 FROM PSA (INPUT PA) AND OTHER REF-STATE ARRAYS               RDINIT.417
C                                                                                RDINIT.418
cmic$ do all autoscope                                                           RDINIT.419
c$doacross                                                                       RDINIT.420
c$& local(i,j,k)                                                                 RDINIT.421
c$omp parallel do default(shared)                                                RDINIT.422
c$omp&private(i,j,k)                                                             RDINIT.423
        DO J=1,JLX                                                               RDINIT.424
          DO I=1,ILX                                                             RDINIT.425
            PS0(I,J)=PSA(I,J)                                                    RDINIT.426
          ENDDO                                                                  RDINIT.427
          DO K=1,KL                                                              RDINIT.428
            DO I=1,ILX                                                           RDINIT.429
               PR0(I,J,K)=PS0(I,J)*A(K)+(PTOP*1000.)                             RDINIT.430
                T0(I,J,K)=AMAX1(TS0+TLP*ALOG(PR0(I,J,K)/P0), TISO)               RDINIT.431
              RHO0(I,J,K)=PR0(I,J,K)/(R*T0(I,J,K))                               RDINIT.432
            ENDDO                                                                RDINIT.433
          ENDDO                                                                  RDINIT.434
        ENDDO                                                                    RDINIT.435
C                                                                                RDINIT.436
C     ... ADD PERTURBATION PRESSURE TO PSAINI IF NONHYDROSTATIC                  RDINIT.437
C         V3 INPUT IS DECOUPLED PPA AND MKS PSA                                  RDINIT.438
C                                                                                RDINIT.439
cmic$ do all autoscope                                                           RDINIT.440
c$doacross                                                                       RDINIT.441
c$& local (isml,jsml)                                                            RDINIT.442
c$omp parallel do default(shared)                                                RDINIT.443
c$omp&private(isml,jsml)                                                         RDINIT.444
        DO 23 JSML=1,JLX                                                         RDINIT.445
          DO 23 ISML=1,ILX                                                       RDINIT.446
            PSAINI(ISML,JSML)=(PSA(ISML,JSML)+PPA(ISML,JSML,KL))                 RDINIT.447
     +                        /1000.                                             RDINIT.448
C     CONVERT PSA TO CENTIBARS (KILOPASCALS)                                     RDINIT.449
            PSA(ISML,JSML)=PSA(ISML,JSML)/1000.                                  RDINIT.450
   23     CONTINUE                                                               RDINIT.451
                                                                                 RDINIT.452
C                                                                                RDINIT.453
C     PDOTA IS ONLY LOCAL TO THIS ROUTINE (RPSTAR.INCL NOT HERE)                 RDINIT.454
      CALL EQUATE(PSA,MIX,MJX,1,PSB,MIX,MJX,1)                                   RDINIT.455
#ifdef MPP1                                                                      RDINIT.456
#  include <mpp_rdinit_10.incl>                                                  RDINIT.457
#endif                                                                           RDINIT.458
      CALL DOTS(PSA,PDOTA,IL,JL,MIX,MJX)                                         RDINIT.459
C                                                                                RDINIT.460
C     COUPLE INPUT 3D ARRAYS AND SET ..B VALUES EQUAL TO ..A VALUES              RDINIT.461
C                                                                                RDINIT.462
          CALL COUPLE(UA,PDOTA,MIX,MJX,MKX,IL,JL,KL)                             RDINIT.463
          CALL EQUATE(UA,MIX,MJX,MKX,UB,MIX,MJX,MKX)                             RDINIT.464
C                                                                                RDINIT.465
          CALL COUPLE(VA,PDOTA,MIX,MJX,MKX,IL,JL,KL)                             RDINIT.466
          CALL EQUATE(VA,MIX,MJX,MKX,VB,MIX,MJX,MKX)                             RDINIT.467
C                                                                                RDINIT.468
          CALL COUPLE(TA,PSA,MIX,MJX,MKX,ILX,JLX,KL)                             RDINIT.469
          CALL EQUATE(TA,MIX,MJX,MKX,TB,MIX,MJX,MKX)                             RDINIT.470
C                                                                                RDINIT.471
          CALL COUPLE(QVA,PSA,MIX,MJX,MKX,ILX,JLX,KL)                            RDINIT.472
          CALL EQUATE(QVA,MIX,MJX,MKX,QVB,MIX,MJX,MKX)                           RDINIT.473
C                                                                                RDINIT.474
          CALL COUPLE(WA,PSA,MIXNH,MJXNH,KXP1,ILX,JLX,KXP1)                      RDINIT.475
          CALL EQUATE(WA,MIXNH,MJXNH,MKXNH+1,WB,MIXNH,MJXNH,MKXNH+1)             RDINIT.476
C                                                                                RDINIT.477
          CALL COUPLE(PPA,PSA,MIX,MJX,MKX,ILX,JLX,KL)                            RDINIT.478
          CALL EQUATE(PPA,MIXNH,MJXNH,MKXNH,PPB,MIXNH,MJXNH,MKXNH)               RDINIT.479
C                                                                                RDINIT.480
        IF(MIXM.GT.1)THEN                                                        RDINIT.481
          CALL COUPLE(QCA,PSA,MIX,MJX,MKX,ILX,JLX,KL)                            RDINIT.482
          CALL EQUATE(QCA,MIXM,MJXM,MKXM,QCB,MIXM,MJXM,MKXM)                     RDINIT.483
C                                                                                RDINIT.484
          CALL COUPLE(QRA,PSA,MIX,MJX,MKX,ILX,JLX,KL)                            RDINIT.485
          CALL EQUATE(QRA,MIXM,MJXM,MKXM,QRB,MIXM,MJXM,MKXM)                     RDINIT.486
        ENDIF                                                                    RDINIT.487
C                                                                                RDINIT.488
        IF(MIXIC.GT.1)THEN                                                       RDINIT.489
          CALL COUPLE(QIA,PSA,MIX,MJX,MKX,ILX,JLX,KL)                            RDINIT.490
          CALL EQUATE(QIA,MIXIC,MJXIC,MKXIC,QIB,MIXIC,MJXIC,MKXIC)               RDINIT.491
C                                                                                RDINIT.492
          CALL COUPLE(QNIA,PSA,MIX,MJX,MKX,ILX,JLX,KL)                           RDINIT.493
          CALL EQUATE(QNIA,MIXIC,MJXIC,MKXIC,QNIB,MIXIC,MJXIC,MKXIC)             RDINIT.494
        ENDIF                                                                    RDINIT.495
C                                                                                RDINIT.496
        IF(MIXIG.GT.1)THEN                                                       RDINIT.497
          CALL COUPLE(QGA,PSA,MIX,MJX,MKX,ILX,JLX,KL)                            RDINIT.498
          CALL EQUATE(QGA,MIXIG,MJXIG,MKXIG,QGB,MIXIG,MJXIG,MKXIG)               RDINIT.499
C                                                                                RDINIT.500
          CALL COUPLE(QNCA,PSA,MIX,MJX,MKX,ILX,JLX,KL)                           RDINIT.501
          CALL EQUATE(QNCA,MIXIG,MJXIG,MKXIG,QNCB,MIXIG,MJXIG,MKXIG)             RDINIT.502
        ENDIF                                                                    RDINIT.503
                                                                                 RDINIT.504
C******************************************************************************  19DEC02.762
C LSM INITIALIZATION FROM I/O STARTS HERE                                        19DEC02.763
C******************************************************************************  19DEC02.764
#ifdef SOIL2                                                                     19DEC02.765
      IF(ISOIL.EQ.2.AND.ILDDIM.EQ.1)THEN                                         RDINIT.505
C       INTERPOLATE VEGFRC IN TIME                                               05DEC01.318
C                                                                                RDINIT.508
        CALL SETVEGFR(ILX,JLX,XTIME,JULDAY,GMT)                                  19DEC02.766
C SET XVEGMN (MINIMUM ANNUAL VEGFRC IN NOAH LSM)                                 19DEC02.767
        DO J=1,JLX                                                               19DEC02.768
        DO I=1,ILX                                                               19DEC02.769
          XVEGMN(I,J)=100. 
          DO IMON=1,NMO                                                          19DEC02.771
            XVEGMN(I,J)=AMIN1(XVEGMN(I,J),VEGFMO(I,J,IMON))                      19DEC02.772
          ENDDO                                                                  19DEC02.773
        ENDDO                                                                    19DEC02.774
        ENDDO                                                                    19DEC02.775
C SKIP CODE TO DETERMINE DOMINANT TYPE FROM PERCENTAGES                          05DEC01.320
C       DO J=1,JLX                                                               05DEC01.321
C       DO I=1,ILX                                                               05DEC01.322
C         PCNTMX=-99.                                                            05DEC01.323
C      IF(VEGCAT(I,J,15).LT.50.)THEN                                             RDINIT.525
CC         LAND POINT                                                            RDINIT.526
C           XLAND(I,J)=1.                                                        RDINIT.527
C           DO IVC=1,NVC                                                         RDINIT.528
C             IF(VEGCAT(I,J,IVC).GT.PCNTMX.AND.IVC.NE.15)THEN                    RDINIT.529
C               PCNTMX=VEGCAT(I,J,IVC)                                           RDINIT.530
C               IVCMX=IVC                                                        RDINIT.531
C             ENDIF                                                              RDINIT.532
C           ENDDO                                                                RDINIT.533
C           IVGTYP(I,J)=IVCMX                                                    RDINIT.534
C           PCNTMX=-99.                                                          RDINIT.535
C           DO ISC=1,NSC                                                         RDINIT.536
C             IF(SOLCAT(I,J,ISC).GT.PCNTMX.AND.ISC.NE.14)THEN                    RDINIT.537
C               PCNTMX=SOLCAT(I,J,ISC)                                           RDINIT.538
C               ISCMX=ISC                                                        RDINIT.539
C             ENDIF                                                              RDINIT.540
C           ENDDO                                                                RDINIT.541
C           ISLTYP(I,J)=ISCMX                                                    RDINIT.542
C      ELSE                                                                      RDINIT.543
CC         WATER POINT                                                           RDINIT.544
C           XLAND(I,J)=2.                                                        RDINIT.545
C           IVGTYP(I,J)=15                                                       RDINIT.546
C           ISLTYP(I,J)=14                                                       RDINIT.547
C      ENDIF                                                                     RDINIT.548
C       ENDDO                                                                    05DEC01.324
C       ENDDO                                                                    05DEC01.325
C     SKIP THE FOLLOWING FOR 1-WAY NESTED INPUT IF LSM DATA WAS FOUND            07NOV00.682
       IF(.NOT.FNDLSMT.AND..NOT.FNDLSMM) THEN                                    07NOV00.683
         IF(FNDSOILH)THEN                                                        07NOV00.684
          PRINT *,'THIS USED A PRE-3.4 VERSION OF REGRID'                        07NOV00.685
          PRINT *,'SOIL HEIGHT ADJUSTMENT DONE IN MM5 INSTEAD OF REGRID'         07NOV00.686
C       USE SURFACE GEOPOTENTIAL FROM ANALYSIS TO ADJUST SOILT (6.5K/KM)         23DEC04.239
        TLAPSE=.0065                                                             23DEC04.240
        DO J=1,JLX                                                               RDINIT.553
        DO I=1,ILX                                                               RDINIT.554
          DO NL=1,NSTL                                                           RDINIT.555
            SOILT(I,J,NL)=SOILT(I,J,NL)+TLAPSE*(SOILHGT(I,J)-HT(I,J))            RDINIT.556
          ENDDO                                                                  RDINIT.557
        ENDDO                                                                    RDINIT.558
        ENDDO                                                                    RDINIT.559
         END IF                                                                  07NOV00.687
C     READ REGRID OUTPUT FOR STCA, SMCA, SNOW DEPTH, SEA-ICE                     24SEP99.285
C       ASSIGN 4-LAYER ARRAYS FROM 3-LAYER SOIL TEMP AND 2-LAYER SOIL MO         RDINIT.561
C       ADJUST ELEVATION BASED ON INPUT AND MODEL ELEVATION FOR SOIL TEM         RDINIT.562
C                                                                                RDINIT.563
C     ANNUAL CONSTANT DEPTH FOR SOIL TEMP                                        RDINIT.564
        ZANN=3.0                                                                 RDINIT.565
C     FIND MID-LEVELS OF INPUT SOIL DATA                                         RDINIT.566
        STLEV(1)=0.5*FLOAT(ISTLYR(1))*0.01                                       RDINIT.567
        DO NL=2,NSTL                                                             RDINIT.568
          STLEV(NL)=0.01*(FLOAT(ISTLYR(NL-1))+0.5*FLOAT(ISTLYR(NL)               RDINIT.569
     -              -ISTLYR(NL-1)))                                              RDINIT.570
        ENDDO                                                                    RDINIT.571
        DO J=1,JLX                                                               RDINIT.572
        DO I=1,ILX                                                               RDINIT.573
          IF(FNDSEAIC.AND.XICE(I,J).GT.0.5)TMN(I,J)=271.16                       25JAN00.121
          DO L=1,MLX                                                             RDINIT.574
            IF(ZS(L).LE.STLEV(1))THEN                                            RDINIT.575
C             MODEL LAYER SHALLOWER THAN OR EQUAL TO FIRST INPUT LAYER           RDINIT.576
              STCA(I,J,L)=SOILT(I,J,1)                                           RDINIT.577
              STCB(I,J,L)=STCA(I,J,L)                                            RDINIT.578
            ELSE IF(ZS(L).GT.STLEV(NSTL).AND.ZS(L).LT.ZANN)THEN                  RDINIT.579
C             MODEL LAYER DEEPER THAN LAST INPUT AND LESS THAN 3.0 M             RDINIT.580
              STCA(I,J,L)=((ZANN-ZS(L))*SOILT(I,J,NSTL)+                         24SEP99.286
     -                        (ZS(L)-STLEV(NSTL))*TMN(I,J))                      24SEP99.287
     -                        /(ZANN-STLEV(NSTL))                                24SEP99.288
              STCB(I,J,L)=STCA(I,J,L)                                            RDINIT.584
            ELSE IF(ZS(L).GT.STLEV(NSTL).AND.ZS(L).GE.ZANN)THEN                  RDINIT.585
C             MODEL LAYER DEEPER THAN LAST INPUT AND MORE THAN 3.0 M             RDINIT.586
              PRINT *,'IN CURRENT CONFIGURATION OF LAND-SURFACE MODEL'           RDINIT.587
              PRINT *,'LOWEST SOIL LAYER MUST BE LESS THAN 3 M (ZANN)'           RDINIT.588
              PRINT *,'ZS AT LEVEL ',L,' = ',ZS(L)                               RDINIT.589
              STOP 'IN RDINIT'                                                   RDINIT.590
            ELSE                                                                 RDINIT.591
C             MODEL LAYER BETWEEN INPUT LAYERS - INTERPOLATE                     RDINIT.592
              DO NL=1,NSTL-1                                                     RDINIT.593
                IF(ZS(L).GT.STLEV(NL).AND.ZS(L).LE.STLEV(NL+1))THEN              RDINIT.594
                  STCA(I,J,L)=((STLEV(NL+1)-ZS(L))*SOILT(I,J,NL)+                RDINIT.595
     -                        (ZS(L)-STLEV(NL))*SOILT(I,J,NL+1))                 RDINIT.596
     -                        /(STLEV(NL+1)-STLEV(NL))                           RDINIT.597
                  STCB(I,J,L)=STCA(I,J,L)                                        RDINIT.598
                ENDIF                                                            RDINIT.599
              ENDDO                                                              RDINIT.600
            ENDIF                                                                RDINIT.601
          ENDDO                                                                  RDINIT.602
        ENDDO                                                                    RDINIT.603
        ENDDO                                                                    RDINIT.604
C     FIND MID-LEVELS OF INPUT SOIL DATA                                         RDINIT.605
C     ASSUME SOILW COMES IN AT SAME LEVELS AS SOILM                              19DEC02.776
        SMLEV(1)=0.5*FLOAT(ISMLYR(1))*0.01                                       RDINIT.606
        DO NL=2,NSML                                                             RDINIT.607
          SMLEV(NL)=0.01*(FLOAT(ISMLYR(NL-1))+0.5*FLOAT(ISMLYR(NL)               RDINIT.608
     -              -ISMLYR(NL-1)))                                              RDINIT.609
        ENDDO                                                                    RDINIT.610
        DO J=1,JLX                                                               RDINIT.611
        DO I=1,ILX                                                               RDINIT.612
          DO L=1,MLX                                                             RDINIT.613
            IF(ZS(L).LE.SMLEV(1))THEN                                            RDINIT.614
C             MODEL LAYER SHALLOWER THAN OR EQUAL TO FIRST INPUT LAYER           RDINIT.615
              SMCA(I,J,L)=SOILM(I,J,1)                                           RDINIT.616
              SMCB(I,J,L)=SMCA(I,J,L)                                            RDINIT.617
              IF(FNDSOILW(1))THEN                                                23DEC04.241
                SWCA(I,J,L)=SOILW(I,J,1)                                         19DEC02.778
                SWCB(I,J,L)=SWCA(I,J,L)                                          19DEC02.779
              ENDIF                                                              19DEC02.780
            ELSE IF(ZS(L).GT.SMLEV(NSML))THEN                                    RDINIT.618
C             MODEL LAYER MORE THAN DEEPEST INPUT - SET TO DEEPEST INPUT         RDINIT.619
              SMCA(I,J,L)=SOILM(I,J,NSML)                                        RDINIT.620
              SMCB(I,J,L)=SMCA(I,J,L)                                            RDINIT.621
              IF(FNDSOILW(NSML))THEN                                             23DEC04.242
                SWCA(I,J,L)=SOILW(I,J,NSML)                                      19DEC02.782
                SWCB(I,J,L)=SWCA(I,J,L)                                          19DEC02.783
              ENDIF                                                              19DEC02.784
            ELSE                                                                 RDINIT.622
C             MODEL LAYER BETWEEN INPUT LAYERS - INTERPOLATE                     RDINIT.623
              DO NL=1,NSML-1                                                     RDINIT.624
                IF(ZS(L).GT.SMLEV(NL).AND.ZS(L).LE.SMLEV(NL+1))THEN              RDINIT.625
                  SMCA(I,J,L)=((SMLEV(NL+1)-ZS(L))*SOILM(I,J,NL)+                RDINIT.626
     -                        (ZS(L)-SMLEV(NL))*SOILM(I,J,NL+1))                 RDINIT.627
     -                        /(SMLEV(NL+1)-SMLEV(NL))                           RDINIT.628
                  SMCB(I,J,L)=SMCA(I,J,L)                                        RDINIT.629
                  IF(FNDSOILW(NL))THEN                                           23DEC04.243
                    SWCA(I,J,L)=((SMLEV(NL+1)-ZS(L))*SOILW(I,J,NL)+              19DEC02.786
     -                        (ZS(L)-SMLEV(NL))*SOILW(I,J,NL+1))                 19DEC02.787
     -                        /(SMLEV(NL+1)-SMLEV(NL))                           19DEC02.788
                    SWCB(I,J,L)=SWCA(I,J,L)                                      19DEC02.789
                  ENDIF                                                          19DEC02.790
                ENDIF                                                            RDINIT.630
              ENDDO                                                              RDINIT.631
            ENDIF                                                                RDINIT.632
          ENDDO                                                                  RDINIT.633
        ENDDO                                                                    RDINIT.634
        ENDDO                                                                    RDINIT.635
         IF(.NOT.FNDSOILW(1))THEN                                                19DEC02.791
C If no SWC, do the following                                                    19DEC02.792
          PRINT *,'SOIL WATER NOT FOUND - VALUE SET IN RDINIT'                   19DEC02.793
        GRAV=G                                                                   19DEC02.794
        HLICE=XLF0                                                               19DEC02.795
        BLIM=5.5                                                                 19DEC02.796
        DO J = 1, JLX                                                            19DEC02.797
        DO I = 1, ILX                                                            19DEC02.798
          BX = BB(ISLTYP(I,J))                                                   19DEC02.799
          SMCMAX = MAXSMC(ISLTYP(I,J))                                           19DEC02.800
          PSISAT = SATPSI(ISLTYP(I,J))                                           19DEC02.801
         if ((bx.gt.0.0).and.(smcmax.gt.0.0).and.(psisat.gt.0.0)) then           19DEC02.802
          DO L=1,MLX                                                             19DEC02.803
C ----------------------------------------------------------------------         19DEC02.805
C SWC <= SMC for T < 273.149K (-0.001C)                                          19DEC02.806
            IF (STCA(I,J,L) .LT. 273.149) THEN                                   19DEC02.807
C ----------------------------------------------------------------------         19DEC02.808
C first guess following explicit solution for Flerchinger Eqn from Koren         19DEC02.809
C et al, JGR, 1999, Eqn 17 (KCOUNT=0 in FUNCTION FRH2O).                         19DEC02.810
C ISLTPK is soil type                                                            19DEC02.811
              BX = BB(ISLTYP(I,J))                                               19DEC02.812
              SMCMAX = MAXSMC(ISLTYP(I,J))                                       19DEC02.813
              PSISAT = SATPSI(ISLTYP(I,J))                                       19DEC02.814
              IF ( BX .GT. BLIM ) BX = BLIM                                      19DEC02.815
              FK=(((HLICE/(GRAV*(-SATPSI(ISLTYP(I,J)))))*                        19DEC02.816
     .           ((STCA(I,J,L)-TO)/STCA(I,J,L)))**                               19DEC02.817
     .           (-1/BX))*MAXSMC(ISLTYP(I,J))                                    19DEC02.818
              IF (FK .LT. 0.02) FK = 0.02                                        19DEC02.819
              SWCA(I,J,L) = AMIN1( FK, SMCB(I,J,L) )                             19DEC02.820
C ----------------------------------------------------------------------         19DEC02.821
C now use iterative solution for liquid soil water content using                 19DEC02.822
C FUNCTION FRH2O with the initial guess for SH2O from above explicit             19DEC02.823
C first guess.                                                                   19DEC02.824
              CALL FRH2O (FREE,STCA(I,J,L),SMCB(I,J,L),SWCA(I,J,L),              19DEC02.825
     .           SMCMAX,BX,PSISAT)                                               19DEC02.826
              SWCA(I,J,L) = FREE                                                 19DEC02.827
              SWCB(I,J,L) = SWCA(I,J,L)                                          19DEC02.828
            ELSE                                                                 19DEC02.829
C ----------------------------------------------------------------------         19DEC02.830
C SWC = SMC for T => 273.149K (-0.001C)                                          19DEC02.831
              SWCA(I,J,L)=SMCB(I,J,L)                                            19DEC02.832
              SWCB(I,J,L)=SMCB(I,J,L)                                            19DEC02.833
C ----------------------------------------------------------------------         19DEC02.834
            ENDIF                                                                19DEC02.835
          END DO                                                                 19DEC02.837
         else                                                                    19DEC02.838
          DO L=1,MLX                                                             19DEC02.839
           SWCA(I,J,L)=SMCB(I,J,L)                                               19DEC02.840
           SWCB(I,J,L)=SMCB(I,J,L)                                               19DEC02.841
          END DO                                                                 19DEC02.842
         endif                                                                   19DEC02.843
        ENDDO                                                                    19DEC02.844
        ENDDO                                                                    19DEC02.845
         ENDIF                                                                   19DEC02.846
       ENDIF                                                                     24SEP99.289
        IF(.NOT.FNDSNOWH)THEN                                                    19DEC02.847
C If no SNOWH do the following                                                   19DEC02.848
          PRINT *,'SNOW HEIGHT NOT FOUND - VALUE DEFINED IN RDINIT'              19DEC02.849
          DO J = 1, JLX                                                          19DEC02.850
          DO I = 1, ILX                                                          19DEC02.851
            SNOWHA(I,J)=SNOA(I,J)*5.0                                            19DEC02.852
            SNOWHB(I,J)=SNOB(I,J)*5.0                                            19DEC02.853
          ENDDO                                                                  19DEC02.854
          ENDDO                                                                  19DEC02.855
        ELSE                                                                     19DEC02.856
C convert from m to mm - later outtap converts back                              19DEC02.857
          DO J = 1, JLX                                                          19DEC02.858
          DO I = 1, ILX                                                          19DEC02.859
            SNOWHA(I,J)=SNOWHA(I,J)*1000.0                                       19DEC02.860
            SNOWHB(I,J)=SNOWHB(I,J)*1000.0                                       19DEC02.861
          ENDDO                                                                  19DEC02.862
          ENDDO                                                                  19DEC02.863
        ENDIF                                                                    19DEC02.864
      ENDIF                                                                      RDINIT.636
#endif                                                                           19DEC02.865
C******************************************************************************  19DEC02.866
C LSM INITIALIZATION FROM I/O ENDS HERE                                          19DEC02.867
C******************************************************************************  19DEC02.868
C--------PX-VEGELAND if IBLTYP = 7                                               07NOV00.688
#ifdef IBLT7                                                                     07NOV00.689
      IF(IBLTYP(1).EQ.7) THEN                                                    07NOV00.690
C        PRINT *,' vcat(20,20,7),vf(06),scat(5)=',                               07NOV00.691
C     >   vegcat(20,20,7),vegfmo(20,20,6),solcat(20,20,5),                       07NOV00.692
C     >   ' JULDAY=',JULDAY                                                      07NOV00.693
           IF(FNDVEGCT.AND.FNDSOLCT.AND.FNDVEGFR)THEN                            07NOV00.694
             CALL VEGELAND(VEGCAT,VEGFMO,JULDAY,ILX,JLX,ZNT,RSTMIN,              07NOV00.695
     >                      XLAI,XVEG,XLAIMN,XVEGMN)                             07NOV00.696
             CALL SOILAV(SOLCAT,ILX,JLX,ISLTYP)                                  07NOV00.697
           ELSE                                                                  07NOV00.698
             PRINT *,'MISSING FIELD REQUIRED'                                    07NOV00.699
             IF(.NOT.FNDVEGCT)PRINT *,'VEGCAT: USE IEXTRA IN TERRAIN'            07NOV00.700
             IF(.NOT.FNDSOLCT)PRINT *,'SOLCAT: USE IEXTRA IN TERRAIN'            07NOV00.701
             IF(.NOT.FNDVEGFR)PRINT *,'VEGFMO'                                   07NOV00.702
             STOP 'IN RDINIT'                                                    07NOV00.703
           ENDIF                                                                 07NOV00.704
C     CHECK FOR 1-WAY NESTED OUTPUT LSM DATA ALREADY ON CORRECT LAYERS           07NOV00.705
       IF(.NOT.FNDLSMT.AND..NOT.FNDLSMM)THEN                                     07NOV00.706
C     FIND MID-LEVELS OF INPUT SOIL DATA                                         07NOV00.707
        SMLEV(1)=0.5*FLOAT(ISMLYR(1))*0.01                                       07NOV00.708
        DO NL=2,NSML                                                             07NOV00.709
          SMLEV(NL)=0.01*(FLOAT(ISMLYR(NL-1))+0.5*FLOAT(ISMLYR(NL)               07NOV00.710
     -              -ISMLYR(NL-1)))                                              07NOV00.711
        ENDDO                                                                    07NOV00.712
        DO J=1,JLX                                                               07NOV00.713
        DO I=1,ILX                                                               07NOV00.714
          STCA(I,J,1)=TGA(I,J)                                                   07NOV00.715
          DO L=1,MLX                                                             07NOV00.716
            IF(ZS(L).LE.SMLEV(1))THEN                                            07NOV00.717
C             MODEL LAYER SHALLOWER THAN OR EQUAL TO FIRST INPUT LAYER           07NOV00.718
              SMCA(I,J,L)=SOILM(I,J,1)                                           07NOV00.719
              SMCB(I,J,L)=SMCA(I,J,L)                                            07NOV00.720
            ELSE IF(ZS(L).GT.SMLEV(NSML))THEN                                    07NOV00.721
C             MODEL LAYER MORE THAN DEEPEST INPUT - SET TO DEEPEST INPUT         07NOV00.722
              SMCA(I,J,L)=SOILM(I,J,NSML)                                        07NOV00.723
              SMCB(I,J,L)=SMCA(I,J,L)                                            07NOV00.724
            ELSE                                                                 07NOV00.725
C             MODEL LAYER BETWEEN INPUT LAYERS - INTERPOLATE                     07NOV00.726
              DO NL=1,NSML-1                                                     07NOV00.727
                IF(ZS(L).GT.SMLEV(NL).AND.ZS(L).LE.SMLEV(NL+1))THEN              07NOV00.728
                  SMCA(I,J,L)=((SMLEV(NL+1)-ZS(L))*SOILM(I,J,NL)+                07NOV00.729
     -                        (ZS(L)-SMLEV(NL))*SOILM(I,J,NL+1))                 07NOV00.730
     -                        /(SMLEV(NL+1)-SMLEV(NL))                           07NOV00.731
                  SMCB(I,J,L)=SMCA(I,J,L)                                        07NOV00.732
                ENDIF                                                            07NOV00.733
              ENDDO                                                              07NOV00.734
            ENDIF                                                                07NOV00.735
          ENDDO                                                                  07NOV00.736
        ENDDO                                                                    07NOV00.737
        ENDDO                                                                    07NOV00.738
       ENDIF                                                                     07NOV00.739
      ENDIF                                                                      07NOV00.740
#endif                                                                           07NOV00.741
C---------------------------------                                               07NOV00.742
C                                                                                RDINIT.637
C     ... ALL DATA INPUT, CLOSE IT                                               RDINIT.638
C                                                                                RDINIT.639
      IF (DM_IONODE())THEN                                                       07NOV00.743
      CLOSE (IUNIT)                                                              RDINIT.640
      ENDIF                                                                      07NOV00.744
                                                                                 07NOV00.745
#ifdef MPP1                                                                      25JAN00.122
#  include <mpp_rdinit_10.incl>                                                  25JAN00.123
#endif                                                                           25JAN00.124
      RETURN                                                                     25JAN00.125
 9006 CONTINUE                                                                   RDINIT.645
      PRINT *,'END OF INPUT FILE, UNIT ',IUNIT                                   RDINIT.646
     *      ,' CHECK IF INPUT FILE EXISTS'                                       RDINIT.647
      STOP 'STOP 9006 IN RDINIT'                                                 RDINIT.648
 9005 PRINT *,'ERROR IN READING HEADER. CHECK INPUT FILE, UNIT ',IUNIT           RDINIT.649
      STOP 'STOP 9005 IN RDINIT'                                                 RDINIT.650
 9004 PRINT *,'ERROR IN READING INPUT FILE. CHECK THE FILE, UNIT ',IUNIT         RDINIT.651
      STOP 'STOP 9004 IN RDINIT'                                                 RDINIT.652
 9010 PRINT *,'ERROR OPENING INPUT FILE. CHECK THE FILE, UNIT ',IUNIT            25JAN00.126
      STOP 'STOP 9010 IN RDINIT'                                                 25JAN00.127
      END                                                                        RDINIT.653
