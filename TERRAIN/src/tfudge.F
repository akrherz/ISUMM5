      SUBROUTINE TFUDGE(TC,LC,VC,SC,VFC,TGC,WLMSKC,XLVC,XLSC,                    TFUDGE.1
     1                  TF,LF,VF,SF,VFF,TGF,WLMSKF,XLVF,XLSF,                    TFUDGE.2
     1                  TFSINT,IYY,JXX,IINC,IYYF,JXXF,IINF,                      TFUDGE.3
     2                  PFLAG,MIXX,MJXX,VEGESOIL,VEGONLY,IP)                     06NOV00.560
C                                                                                TFUDGE.5
C   PURPOSE :  RESTS THE BOUNDARY TERRAIN HEIGHT AND LAND-USE VALUES             TFUDGE.6
C              FOR THE NESTED DOMAINS (SECTION 3.5.1 AND 5.3.3).                 TFUDGE.7
C   ON ENTRY : IYY,JXX: THE DIMENSIONS OF THE MOTHER DOMAIN.                     TFUDGE.8
C                 IINC: THE MOTHER DOMAIN ID.                                    TFUDGE.9
C            IYYF,JXXF: THE DIMENSIONS OF THE NEST DOMAIN.                       TFUDGE.10
C                 IINF: THE NEST DOMAIN ID.                                      TFUDGE.11
C                PFLAG: THE PRINT SWITCH IF .TRUE., PRINT THE TERRAIN            TFUDGE.12
C                       HEIGHT FIELDS AFTER THE ADJUSTMENT. IF .FALSE.,          TFUDGE.13
C                       DO NOT PRINT.                                            TFUDGE.14
C            MIXX,MJXX: THE MAXIMUM DIMENSIONS OF ALL THE DOMAINS.               TFUDGE.15
C   ON EXIT :  THE ADJUSTED TERRAIN HEIGHT AND LAND-USE FIELDS                   TFUDGE.16
C              ARE WRITTEN OUT TO THE DISK AGAIN WITH THE SAME UNIT              TFUDGE.17
C              NUMBERS IVOL_mdl+N FOR THE DOMAIN N.                              06NOV00.561
C   CALLS :    THE SUBROUTINES EXAINT, OUTPT.                                    TFUDGE.19
C   CALLED BY :THE MAIN PROGRAM TERRAIN.                                         TFUDGE.20
C   COMMENTS :                                                                   TFUDGE.21
C  THERE IS A CHECK AFTER READING IN THE DATA FOR THE MOTHER AND                 TFUDGE.22
C  NESTED DOMAINS FROM DISK WHICH IS THE NECESSARY CONDITION TO PERFORM          TFUDGE.23
C  THE CALCULATION IN THIS SUBROUTINE. IF THE NEST IS TOO CLOSE TO THE           TFUDGE.24
C  BOUNDARIES (4 COLUMNS OR 5 ROWS) OF THE MOTHER DOMAIN, THE PROGRAM            TFUDGE.25
C  STOPS THE EXECUTION. THE MAXIMUM DIMENSIONS MIXX, MJXX ARE USED IN            TFUDGE.26
C  CALLING THE SUBROUTINE EXAINT FOR THE DECLARATION OF WORKING ARRAYS.          TFUDGE.27
C                                                                                TFUDGE.28
# include <parame.incl>                                                          TFUDGE.29
# include <paramed.incl>                                                         06NOV00.562
# include <io.incl>                                                              06NOV00.563
# include <paramesv.incl>                                                        TFUDGE.30
# include <vs_cmn1.incl>                                                         TFUDGE.31
# include <maps.incl>                                                            TFUDGE.32
# include <nestdmn.incl>                                                         TFUDGE.33
C                                                                                TFUDGE.34
      DIMENSION TC(IYY,JXX), LC(IYY,JXX), TF(IYYF,JXXF), LF(IYYF,JXXF),          TFUDGE.35
     -          TFSINT(IYYF,JXXF)                                                TFUDGE.36
      DIMENSION VC(IYY ,JXX ,NVC), SC(IYY ,JXX ,NSC), VFC(IYY ,JXX ,12),         TFUDGE.37
     -         TGC(IYY ,JXX ), WLMSKC(IYY ,JXX ), XLVC(IYY ,JXX ),               TFUDGE.38
     -        XLSC(IYY ,JXX ),                                                   TFUDGE.39
     -          VF(IYYF,JXXF,NVC), SF(IYYF,JXXF,NSC), VFF(IYYF,JXXF,12),         TFUDGE.40
     -         TGF(IYYF,JXXF), WLMSKF(IYYF,JXXF), XLVF(IYYF,JXXF),               TFUDGE.41
     -        XLSF(IYYF,JXXF)                                                    TFUDGE.42
      LOGICAL VEGESOIL, VEGONLY                                                  TFUDGE.43
C                                                                                TFUDGE.44
      DIMENSION WRK1(IIMX,JJMX),WRK2(IIMX,JJMX)                                  TFUDGE.45
      CHARACTER*10 NAME                                                          TFUDGE.46
      LOGICAL PFLAG                                                              TFUDGE.47
      REAL LC,LF                                                                 TFUDGE.48
C                                                                                TFUDGE.49
      PRINT *,'==> CALL TFUDGE <=='                                              06NOV00.564
C  .. DOMAIN ID:                                                                 TFUDGE.51
      NESTID = IINF - IVOL_mdl                                                   06NOV00.565
      NVW = IVW(NTYPE(NESTID))                                                   23DEC04.27
      NSW = ISW(NTYPE(NESTID))                                                   23DEC04.28
      IF(IP.EQ.1)PRINT *,'VEGE. WATER_C = ',NVW,'  SOIL WATER_C = ',NSW          06NOV00.568
      PRINT 910, NESTID, IINC-IVOL_mdl                                           06NOV00.569
910   FORMAT(1X,'RESET BOUNDARY VALUES FOR DOMAIN ',I2,                          06NOV00.570
     -       ' FROM DOMAIN ',I2)                                                 06NOV00.571
C                                                                                TFUDGE.56
      REWIND IINC                                                                TFUDGE.59
      REWIND IINF                                                                TFUDGE.60
C                                                                                TFUDGE.61
C   READ IN COARSE AND FINE MESH TERRESTIAL FIELDS:                              TFUDGE.62
C                                                                                TFUDGE.63
C   COARSE MESH:                                                                 TFUDGE.64
C                                                                                TFUDGE.65
C     READ(IINC) MIF,MRF,MIFC,MRFC                                               TFUDGE.66
      NC     = IINC - IVOL_mdl                                                   06NOV00.572
      IYY1 = IYY                                                                 TFUDGE.68
      JXX1 = JXX                                                                 TFUDGE.69
      IF(NC.EQ.1) THEN                                                           TFUDGE.70
        IYY1 = NESTIX(1)                                                         TFUDGE.71
        JXX1 = NESTJX(1)                                                         TFUDGE.72
      END IF                                                                     TFUDGE.73
                                                                                 TFUDGE.74
      READ(IINC) TC                                                              TFUDGE.75
      READ(IINC) LC                                                              TFUDGE.76
                                                                                 TFUDGE.77
      IF (VEGESOIL) THEN                                                         TFUDGE.78
        IF (.NOT. VEGONLY) THEN                                                  TFUDGE.79
          DO IC = 1,NVC                                                          TFUDGE.80
            READ(IINC) TGC                                                       TFUDGE.81
            CALL EQUATE(TGC,VC (1,1,IC),IYY ,JXX , 1,'VEGECATC')                 TFUDGE.82
          END DO                                                                 TFUDGE.83
          DO IC = 1,NSC                                                          TFUDGE.84
            READ(IINC) TGC                                                       TFUDGE.85
            CALL EQUATE(TGC,SC (1,1,IC),IYY ,JXX , 1,'SOILCATC')                 TFUDGE.86
          END DO                                                                 TFUDGE.87
          DO IC = 1,12                                                           TFUDGE.88
            READ(IINC) TGC                                                       TFUDGE.89
            CALL EQUATE(TGC,VFC(1,1,IC),IYY ,JXX , 1,'VEGEFRCC')                 TFUDGE.90
          END DO                                                                 TFUDGE.91
          READ(IINC) TGC                                                         TFUDGE.92
          READ(IINC) WLMSKC                                                      TFUDGE.93
C         READ(IINC) XLVC                                                        TFUDGE.94
          READ(IINC) XLSC                                                        TFUDGE.95
        ENDIF                                                                    TFUDGE.96
      ENDIF                                                                      TFUDGE.97
C                                                                                TFUDGE.98
C   FINE MESH:                                                                   TFUDGE.99
C                                                                                TFUDGE.100
C     READ(IINF) MIF,MRF,MIFC,MRFC                                               TFUDGE.101
      NFN    = IINF-IVOL_mdl                                                     06NOV00.573
      READ(IINF) TF                                                              TFUDGE.103
      READ(IINF) LF                                                              TFUDGE.104
      IF (VEGESOIL) THEN                                                         TFUDGE.105
        IF (.NOT. VEGONLY) THEN                                                  TFUDGE.106
          DO IC = 1,NVC                                                          TFUDGE.107
            READ(IINF) TGF                                                       TFUDGE.108
            CALL EQUATE(TGF,VF (1,1,IC),IYYF,JXXF, 1,'VEGECATF')                 TFUDGE.109
          END DO                                                                 TFUDGE.110
          DO IC = 1,NSC                                                          TFUDGE.111
            READ(IINF) TGF                                                       TFUDGE.112
            CALL EQUATE(TGF,SF (1,1,IC),IYYF,JXXF, 1,'SOILCATF')                 TFUDGE.113
          END DO                                                                 TFUDGE.114
          DO IC = 1,12                                                           TFUDGE.115
            READ(IINF) TGF                                                       TFUDGE.116
            CALL EQUATE(TGF,VFF(1,1,IC),IYYF,JXXF, 1,'VEGEFRCF')                 TFUDGE.117
          END DO                                                                 TFUDGE.118
          READ(IINF) TGF                                                         TFUDGE.119
          READ(IINF) WLMSKF                                                      TFUDGE.120
C         READ(IINF) XLVF                                                        TFUDGE.121
          READ(IINF) XLSF                                                        TFUDGE.122
        ENDIF                                                                    TFUDGE.123
      ENDIF                                                                      TFUDGE.124
C                                                                                TFUDGE.125
      ISOUTH = NESTI(NFN)                                                        TFUDGE.126
      JWEST  = NESTJ(NFN)                                                        TFUDGE.127
      IRATIOF= IRATIO(NFN)                                                       TFUDGE.128
C                                                                                TFUDGE.129
C NOTE: INORTH IS THE NORTHERNMOST ROW AND JEAST IS THE EASTERNMOST              TFUDGE.130
C       COLUMN OF THE DOT POINT GRID OF CGM AREA OVERLAPPING THE FGM             TFUDGE.131
C                       (TERRAIN IS ON CROSS POINTS)                             TFUDGE.132
C       THUS, ALL LOOPS MUST USE ISOUTH TO INORTH-1                              TFUDGE.133
C             AND JWEST TO JEAST-1                                               TFUDGE.134
C                                                                                TFUDGE.135
        INORTH = ISOUTH + (IYYF-1)/IRATIOF                                       TFUDGE.136
        JEAST  = JWEST  + (JXXF-1)/IRATIOF                                       TFUDGE.137
C                                                                                TFUDGE.138
        IF (ISOUTH.LT.6 .OR. INORTH.GE.IYY1-5 .OR.                               TFUDGE.139
     1       JWEST.LT.6 .OR.  JEAST.GE.JXX1-5) THEN                              TFUDGE.140
         PRINT *,'THE NEST:',NFN,' IS TOO CLOSE TO THE BOUNDARY OF ',            TFUDGE.141
     1           'THE DOMAIN: ',NC,'(IYY,JXX=',IYY1,JXX1,')'                     TFUDGE.142
         PRINT *,'SINT IS NOT WORKING FOR ISOUTH,INORTH,JWEST,JEAST:',           TFUDGE.143
     1            ISOUTH,INORTH,JWEST,JEAST                                      TFUDGE.144
         STOP                                                                    TFUDGE.145
        ENDIF                                                                    TFUDGE.146
C                                                                                TFUDGE.147
      IF (NC.EQ.1) THEN                                                          TFUDGE.148
        ISOUTH = ISOUTH + IOFFST                                                 TFUDGE.149
        JWEST  = JWEST  + JOFFST                                                 TFUDGE.150
        INORTH = INORTH + IOFFST                                                 TFUDGE.151
        JEAST  = JEAST  + JOFFST                                                 TFUDGE.152
      ENDIF                                                                      TFUDGE.153
                                                                                 TFUDGE.154
      IF(PFLAG) THEN                                                             TFUDGE.155
         NAME='TC_1'                                                             TFUDGE.156
         CALL OUTPT(TC, IYY, 1, IYY-1, 1, JXX, 1, JXX-1, 1,KSIGT,NAME)           TFUDGE.157
         NAME='TF_1'                                                             TFUDGE.158
         CALL OUTPT(TF,IYYF, 1,IYYF-1, 1, JXXF,1, JXXF-1,1,KSIGT,NAME)           TFUDGE.159
      ENDIF                                                                      TFUDGE.160
C                                                                                TFUDGE.161
C .. TERRAIN HEIGHT ADJUSTED ALONG THE BOUNDARY OF FINE MESH DOMAIN:             TFUDGE.162
C                                                                                TFUDGE.163
      CALL NESTBDY(TC,TF,TFSINT,IYY,JXX,IYYF,JXXF,                               TFUDGE.164
     >             ISOUTH,JWEST,IRATIOF,MIXX,MJXX,PFLAG, 0)                      TFUDGE.165
      IF(IP.EQ.1)PRINT *,'    $$ TERRAIN HEIGHT ADJUSTED ALONG BOUNDARY'         06NOV00.574
C                                                                                TFUDGE.167
C     IF (VEGESOIL) THEN                                                         TFUDGE.168
C       IF (.NOT. VEGONLY) THEN                                                  TFUDGE.169
C       DO IC = 1,NVC                                                            TFUDGE.170
C         CALL NESTBDY(VC(1,1,IC),VF(1,1,IC),TFSINT,IYY,JXX,IYYF,JXXF,           TFUDGE.171
C    >                 ISOUTH,JWEST,IRATIOF,MIXX,MJXX,PFLAG, 1)                  TFUDGE.172
C       END DO                                                                   TFUDGE.173
C       PRINT *,'    $$ VEGE. CATEGORY PERCNT. ADJUSTED ALONG BOUNDARY'          TFUDGE.174
C                                                                                TFUDGE.175
C       DO IC = 1,NSC                                                            TFUDGE.176
C         CALL NESTBDY(SC(1,1,IC),SF(1,1,IC),TFSINT,IYY,JXX,IYYF,JXXF,           TFUDGE.177
C    >                 ISOUTH,JWEST,IRATIOF,MIXX,MJXX,PFLAG, 1)                  TFUDGE.178
C       END DO                                                                   TFUDGE.179
C       PRINT *,'    $$ SOIL  CATEGORY PERCNT. ADJUSTED ALONG BOUNDARY'          TFUDGE.180
C                                                                                TFUDGE.181
C       DO IC = 1,12                                                             TFUDGE.182
C         CALL NESTBDY(VFC(1,1,IC),VFF(1,1,IC),TFSINT,IYY,JXX,IYYF,JXXF,         TFUDGE.183
C    >                 ISOUTH,JWEST,IRATIOF,MIXX,MJXX,PFLAG, 1)                  TFUDGE.184
C       END DO                                                                   TFUDGE.185
C       PRINT *,'    $$ VEGETATION FRACTION. ADJUSTED ALONG BOUNDARY'            TFUDGE.186
C                                                                                TFUDGE.187
C       CALL NESTBDY(TGC,TGF,TFSINT,IYY,JXX,IYYF,JXXF,                           TFUDGE.188
C    >             ISOUTH,JWEST,IRATIOF,MIXX,MJXX,PFLAG, 1)                      TFUDGE.189
C       PRINT *,'    $$ GOUND TEMPERATURE ADJUSTED ALONG BOUNDARY'               TFUDGE.190
C       ENDIF                                                                    TFUDGE.191
C                                                                                TFUDGE.192
C     ENDIF                                                                      TFUDGE.193
C                                                                                TFUDGE.194
C SMOOTHING IS PERFORMED IN TERDRV FOR ALL MESHES.                               TFUDGE.195
C                                                                                TFUDGE.196
      IF (PFLAG) THEN                                                            TFUDGE.197
         NAME='TF_1-2-1'                                                         TFUDGE.198
         CALL OUTPT(TF,IYYF,1,IYYF-1, 1,JXXF, 1, JXXF-1,1, KSIGT,NAME)           TFUDGE.199
      ENDIF                                                                      TFUDGE.200
C                                                                                TFUDGE.201
C  OVER THE OCEAN, SET LANDUSE = WATER CATEGORY:                                 TFUDGE.202
C                                                                                TFUDGE.203
      DO 2020 I=1,IYYF-1                                                         TFUDGE.204
      DO 2020 J=1,JXXF-1                                                         TFUDGE.205
          IF(TF(I,J).GE.0.0 .OR. TF(I,J).LE.-1.) GO TO 2020                      TFUDGE.206
          IF (VEGESOIL .AND. .NOT.VEGONLY) TGF(I,J) = 0.0                        TFUDGE.207
          IF (VEGESOIL) THEN                                                     TFUDGE.208
            LF(I,J) = NVW                                                        TFUDGE.209
          ELSE                                                                   TFUDGE.210
            LF(I,J) = 7                                                          TFUDGE.211
          ENDIF                                                                  TFUDGE.212
2020  CONTINUE                                                                   TFUDGE.213
C                                                                                TFUDGE.214
C    REPLACE THE 2 ROWS/COLUMNS ON THE EDGE OF FINE MESH WITH                    TFUDGE.215
C    COARSE MESH LANDUSE (DOMINANT VEGETATION), DOMINANT SOIL                    TFUDGE.216
C    CATEGORIES, AND LANDMASK:                                                   TFUDGE.217
C    ALSO DO SAME BOUNDARY REPLACE FOR VEG AND SOIL FRAC ARRAYS ---jp 4/00       06NOV00.575
C                                                                                TFUDGE.218
      DO 200 I = ISOUTH, INORTH-1                                                TFUDGE.219
        II = (I-ISOUTH)*IRATIOF                                                  06NOV00.576
        DO 200 L = 1, IRATIOF                                                    06NOV00.577
        DO 200 M = 1,2                                                           06NOV00.578
C   THE WEST BOUNDARY:                                                           TFUDGE.223
        LF(II+L,M) = LC(I,JWEST)                                                 TFUDGE.224
c                                                                                06NOV00.579
        IF (VEGESOIL) THEN                                                       06NOV00.580
        IF (.NOT. VEGONLY) THEN                                                  06NOV00.581
        XLSF(II+L,M) = XLSC(I,JWEST)                                             06NOV00.582
        WLMSKF(II+L,M) = WLMSKC(I,JWEST)                                         TFUDGE.226
        TGF(II+L,M) = TGC(I,JWEST)                                               06NOV00.583
        DO IC = 1,12
          VFF(II+L,M,IC) = VFC(I,JWEST,IC)
        END DO
        DO IC = 1,NVC                                                            06NOV00.584
          VF(II+L,M,IC) = VC(I,JWEST,IC)                                         06NOV00.585
        END DO                                                                   06NOV00.586
        DO IC = 1,NSC                                                            06NOV00.587
          SF(II+L,M,IC) = SC(I,JWEST,IC)                                         06NOV00.588
        END DO                                                                   06NOV00.589
        ENDIF                                                                    06NOV00.590
        ENDIF                                                                    06NOV00.591
c                                                                                06NOV00.592
C   THE EAST BOUNDARY:                                                           TFUDGE.227
        LF(II+L,JXXF-M) = LC(I,JEAST-1)                                          TFUDGE.228
c                                                                                06NOV00.593
        IF (VEGESOIL) THEN                                                       06NOV00.594
        IF (.NOT. VEGONLY) THEN                                                  06NOV00.595
        XLSF(II+L,JXXF-M) = XLSC(I,JEAST-1)                                      TFUDGE.229
        WLMSKF(II+L,JXXF-M) = WLMSKC(I,JEAST-1)                                  TFUDGE.230
        TGF(II+L,JXXF-M) = TGC(I,JEAST-1)                                        06NOV00.596
        DO IC = 1,12
          VFF(II+L,JXXF-M,IC) = VFC(I,JEAST-1,IC)
        END DO
        DO IC = 1,NVC                                                            06NOV00.597
          VF(II+L,JXXF-M,IC) = VC(I,JEAST-1,IC)                                  06NOV00.598
        END DO                                                                   06NOV00.599
        DO IC = 1,NSC                                                            06NOV00.600
          SF(II+L,JXXF-M,IC) = SC(I,JEAST-1,IC)                                  06NOV00.601
        END DO                                                                   06NOV00.602
        ENDIF                                                                    06NOV00.603
        ENDIF                                                                    06NOV00.604
c                                                                                06NOV00.605
200   CONTINUE                                                                   TFUDGE.231
                                                                                 TFUDGE.232
      DO 210 J = JWEST,JEAST-1                                                   TFUDGE.233
        JJ = (J-JWEST)*IRATIOF                                                   06NOV00.606
        DO 210 L = 1, 2                                                          06NOV00.607
        DO 210 M = 1, IRATIOF                                                    06NOV00.608
C   THE SOUTH BOUNDARY:                                                          TFUDGE.237
        LF(L,JJ+M) = LC(ISOUTH,J)                                                TFUDGE.238
c                                                                                06NOV00.609
        IF (VEGESOIL) THEN                                                       06NOV00.610
        IF (.NOT. VEGONLY) THEN                                                  06NOV00.611
        XLSF(L,JJ+M) = XLSC(ISOUTH,J)                                            TFUDGE.239
        WLMSKF(L,JJ+M) = WLMSKC(ISOUTH,J)                                        TFUDGE.240
        TGF(L,JJ+M) = TGC(ISOUTH,J)                                              06NOV00.612
        DO IC = 1,12
          VFF(L,JJ+M,IC) = VFC(ISOUTH,J,IC)
        END DO
        DO IC = 1,NVC                                                            06NOV00.613
          VF(L,JJ+M,IC) = VC(ISOUTH,J,IC)                                        06NOV00.614
        END DO                                                                   06NOV00.615
        DO IC = 1,NSC                                                            06NOV00.616
          SF(L,JJ+M,IC) = SC(ISOUTH,J,IC)                                        06NOV00.617
        END DO                                                                   06NOV00.618
        ENDIF                                                                    06NOV00.619
        ENDIF                                                                    06NOV00.620
c                                                                                06NOV00.621
C   THE NORTH BOUNDARY:                                                          TFUDGE.241
        LF(IYYF-L,JJ+M) = LC(INORTH-1,J)                                         TFUDGE.242
c                                                                                06NOV00.622
        IF (VEGESOIL) THEN                                                       06NOV00.623
        IF (.NOT. VEGONLY) THEN                                                  06NOV00.624
        XLSF(IYYF-L,JJ+M) = XLSC(INORTH-1,J)                                     TFUDGE.243
        WLMSKF(IYYF-L,JJ+M) = WLMSKC(INORTH-1,J)                                 TFUDGE.244
        TGF(IYYF-L,JJ+M) = TGC(INORTH-1,J)                                       06NOV00.625
        DO IC = 1,12
          VFF(IYYF-L,JJ+M,IC) = VFC(INORTH-1,J,IC)
        END DO
        DO IC = 1,NVC                                                            06NOV00.626
          VF(IYYF-L,JJ+M,IC) = VC(INORTH-1,J,IC)                                 06NOV00.627
        END DO                                                                   06NOV00.628
        DO IC = 1,NSC                                                            06NOV00.629
          SF(IYYF-L,JJ+M,IC) = SC(INORTH-1,J,IC)                                 06NOV00.630
        END DO                                                                   06NOV00.631
        ENDIF                                                                    06NOV00.632
        ENDIF                                                                    06NOV00.633
c                                                                                06NOV00.634
210   CONTINUE                                                                   TFUDGE.245
C                                                                                TFUDGE.246
C     IF (VEGESOIL) THEN                                                         TFUDGE.247
C                                                                                TFUDGE.248
C .. NORMALIZE THE PERCENTAGES OF VEGETATION AND SOIL CATEGORY:                  TFUDGE.249
C                                                                                TFUDGE.250
C      IF (.NOT. VEGONLY) THEN                                                   TFUDGE.251
C         DO I = 1,IYYF-1                                                        TFUDGE.252
C         DO J = 1,JXXF-1                                                        TFUDGE.253
C            SPCNTV = 0.0                                                        TFUDGE.254
C            DO ILEV = 1,NVC                                                     TFUDGE.255
C              SPCNTV = SPCNTV + VF(I,J,ILEV)                                    TFUDGE.256
C            END DO                                                              TFUDGE.257
C            PADJCV = 100./SPCNTV                                                TFUDGE.258
CC           IF (ABS(SPCNTV-100.0).GT.0.1)                                       TFUDGE.259
CC     -        PRINT *,'I.J:',I,J,'  PADJCV=',PADJCV,'  SPCNTV=',SPCNTV         TFUDGE.260
C            DO ILEV = 1,NVC                                                     TFUDGE.261
C               VF(I,J,ILEV) = VF(I,J,ILEV)*PADJCV                               TFUDGE.262
C            END DO                                                              TFUDGE.263
C            SPCNTS = 0.0                                                        TFUDGE.264
C            DO ILEV = 1,NSC                                                     TFUDGE.265
C              SPCNTS = SPCNTS + SF(I,J,ILEV)                                    TFUDGE.266
C            END DO                                                              TFUDGE.267
C            PADJCS = 100./SPCNTS                                                TFUDGE.268
CC           IF (ABS(SPCNTS-100.0).GT.0.1)                                       TFUDGE.269
CC     -        PRINT *,'I.J:',I,J,'  PADJCS=',PADJCS,'  SPCNTS=',SPCNTS         TFUDGE.270
C            DO ILEV = 1,NSC                                                     TFUDGE.271
C               SF(I,J,ILEV) = SF(I,J,ILEV)*PADJCS                               TFUDGE.272
C            END DO                                                              TFUDGE.273
C         END DO                                                                 TFUDGE.274
C         END DO                                                                 TFUDGE.275
C       PRINT *,'       COMPLETED NORMALIZATION OF THE PERCENTAGES.'             TFUDGE.276
C      ENDIF                                                                     TFUDGE.277
C                                                                                TFUDGE.278
C .. TO RESET THE DOMINATING CATEGORY:                                           TFUDGE.279
C                                                                                TFUDGE.280
C       IF (.NOT. VEGONLY) THEN                                                  TFUDGE.281
C         DO I = 1,IYYF-1                                                        TFUDGE.282
C         DO J = 1,JXXF-1                                                        TFUDGE.283
C            WRK1(I,J) = 0.                                                      TFUDGE.284
C            DO 300 ILEV = 1,NVC                                                 TFUDGE.285
C              IF (ILEV.EQ.NVW .AND. VF(I,J,ILEV).LT.50.0) GOTO 300              TFUDGE.286
C              IF (VF(I,J,ILEV).GT.WRK1(I,J)) THEN                               TFUDGE.287
C                WRK1(I,J) = VF(I,J,ILEV)                                        TFUDGE.288
C                XLVF(I,J) = ILEV                                                TFUDGE.289
C              ENDIF                                                             TFUDGE.290
C300         CONTINUE                                                            TFUDGE.291
C                                                                                TFUDGE.292
C            WRK2(I,J) = 0.                                                      TFUDGE.293
C            DO 310 ILEV = 1,NSC                                                 TFUDGE.294
C              IF (ILEV.EQ.NSW .AND. SF(I,J,ILEV).LT.50.0) GOTO 310              TFUDGE.295
C              IF (SF(I,J,ILEV).GT.WRK2(I,J)) THEN                               TFUDGE.296
C                WRK2(I,J) = SF(I,J,ILEV)                                        TFUDGE.297
C                XLSF(I,J) = ILEV                                                TFUDGE.298
C              ENDIF                                                             TFUDGE.299
C310         CONTINUE                                                            TFUDGE.300
C         END DO                                                                 TFUDGE.301
C         END DO                                                                 TFUDGE.302
C      PRINT *,'       THE DOMINATING CATEGORY WAS RESET.'                       TFUDGE.303
C      ENDIF                                                                     TFUDGE.304
                                                                                 TFUDGE.305
C     ENDIF                                                                      TFUDGE.306
C                                                                                TFUDGE.307
C ==================================================================             TFUDGE.308
C                                                                                TFUDGE.309
399   CONTINUE                                                                   TFUDGE.310
                                                                                 TFUDGE.311
      REWIND IINC                                                                TFUDGE.312
      REWIND IINF                                                                TFUDGE.313
C     WRITE(IINF) MIF,MRF,MIFC,MRFC                                              TFUDGE.314
      WRITE(IINF)TF                                                              TFUDGE.315
      WRITE(IINF)LF                                                              TFUDGE.316
      IF (VEGESOIL) THEN                                                         TFUDGE.317
        IF (.NOT. VEGONLY) THEN                                                  TFUDGE.318
        DO IC = 1,NVC                                                            TFUDGE.319
          WRITE(IINF) ((VF (I,J,IC),I=1,IYYF),J=1,JXXF)                          TFUDGE.320
        END DO                                                                   TFUDGE.321
        DO IC = 1,NSC                                                            TFUDGE.322
          WRITE(IINF) ((SF (I,J,IC),I=1,IYYF),J=1,JXXF)                          TFUDGE.323
        END DO                                                                   TFUDGE.324
        DO IC = 1,12                                                             TFUDGE.325
          WRITE(IINF) ((VFF(I,J,IC),I=1,IYYF),J=1,JXXF)                          TFUDGE.326
        END DO                                                                   TFUDGE.327
        WRITE(IINF) TGF                                                          TFUDGE.328
        WRITE(IINF) WLMSKF                                                       TFUDGE.329
C       WRITE(IINF) XLVF                                                         TFUDGE.330
        WRITE(IINF) XLSF                                                         TFUDGE.331
        ENDIF                                                                    TFUDGE.332
      ENDIF                                                                      TFUDGE.333
C                                                                                TFUDGE.334
      RETURN                                                                     TFUDGE.335
      END                                                                        TFUDGE.336
