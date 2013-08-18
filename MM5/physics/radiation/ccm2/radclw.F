CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC             RADCLW.1
C-----------------------------------------------------------------------         RADCLW.2
      SUBROUTINE RADCLW(JSLC    ,TG      ,PLOL    ,PLOS    ,TNM     ,            RADCLW.3
     $                  QNM     ,PMID    ,PINT    ,PMLN    ,PILN    ,            RADCLW.4
     $                  PLCO2   ,PLH2O   ,CLD     ,TCLRSF  ,QRL     ,            RADCLW.5
     $                  FRLA    ,FIRTP ,CLRLS ,CLRLT ,SLWD   )                   RADCLW.6
C                                                                                RADCLW.7
C COMPUTE LONGWAVE RADIATION HEATING RATES AND BOUNDARY FLUXES                   RADCLW.8
C                                                                                RADCLW.9
C USES BROAD BAND ABSORPTIVITY/EMISSIVITY METHOD TO COMPUTE CLEAR                RADCLW.10
C SKY; ASSUMES RANDOMLY OVERLAPPED CLOUDS WITH VARIABLE CLOUD                    RADCLW.11
C EMISSIVITY TO INCLUDE EFFECTS OF CLOUDS.                                       RADCLW.12
C                                                                                RADCLW.13
C COMPUTES CLEAR SKY ABSORPTIVITY/EMISSIVITY AT LOWER FREQUENCY                  RADCLW.14
C (IN GENERAL) THAN THE MODEL RADIATION FREQUENCY; USES PREVIOUSLY               RADCLW.15
C COMPUTED AND STORED VALUES FOR EFFICIENCY                                      RADCLW.16
C                                                                                RADCLW.17
C     INCLUDE 'PARAME'                                                           RADCLW.18
C     INCLUDE 'PARAM1.CB'                                                        RADCLW.19
C     INCLUDE 'RADCCM2.DATA'                                                     RADCLW.20
#     include <parame.incl>                                                      RADCLW.21
#     include <parccm2.incl>                                                     RADCLW.22
#     include <radccm2.incl>                                                     RADCLW.23
#     include <various.incl>                                                     RADCLW.24
C-----------------------------------------------------------------------         RADCLW.25
      INTEGER IPLEVP2,IPLEVP3,IPLEVP4                                            RADCLW.26
      PARAMETER (IPLEVP2=IPLEV+2,IPLEVP3=IPLEV+3,IPLEVP4=IPLEV+4)                RADCLW.27
C                                                                                RADCLW.28
C INPUT ARGUMENTS                                                                RADCLW.29
C                                                                                RADCLW.30
      REAL TG(IPLOND),             ! GROUND (SKIN) TEMPERATURE                   RADCLW.31
     $     PLOL(IPLOND,IPLEVP),     ! O3 PRESSURE WGHTED PATH LENGTH             RADCLW.32
     $     PLOS(IPLOND,IPLEVP)      ! O3 PATH LENGTH                             RADCLW.33
C                                                                                RADCLW.34
C INPUT ARGUMENTS WHICH ARE ONLY PASSED TO OTHER ROUTINES                        RADCLW.35
C                                                                                RADCLW.36
      REAL TNM(IPLOND,IPLEV),        ! LEVEL TEMPERATURE                         RADCLW.37
     $     QNM(IPLOND,IPLEV),        ! LEVEL MOISTURE FIELD                      RADCLW.38
     $     PMID(IPLOND,IPLEV),       ! LEVEL PRESSURE                            RADCLW.39
     $     PINT(IPLOND,IPLEVP),      ! MODEL INTERFACE PRESSURE                  RADCLW.40
     $     PMLN(IPLOND,IPLEV),       ! LN(PMID)                                  RADCLW.41
     $     PILN(IPLOND,IPLEVP),      ! LN(PINT)                                  RADCLW.42
     $     PLCO2(IPLOND,IPLEVP),     ! PATH LENGTH CO2                           RADCLW.43
     $     PLH2O(IPLOND,IPLEVP)      ! PATH LENGTH H2O                           RADCLW.44
C                                                                                RADCLW.45
C INPUT/OUTPUT ARGUMENTS                                                         RADCLW.46
C                                                                                RADCLW.47
      REAL CLD(IPLOND,IPLEVP),       ! CLOUD COVER                               RADCLW.48
     $      TCLRSF(IPLOND,IPLEVP)    ! CLEAR SKY FRACTION                        RADCLW.49
C                                                                                RADCLW.50
C OUTPUT ARGUMENTS                                                               RADCLW.51
C                                                                                RADCLW.52
      REAL                                                                       RADCLW.53
     $     QRL(IPLOND,IPLEV),        ! LONGWAVE HEATING RATE                     RADCLW.54
     $     FRLA(IPLOND),            ! SURFACE COOLING FLUX                       RADCLW.55
     $     FIRTP(IPLOND),           ! UPWARD LW FLX AT MODEL TOP                 07NOV00.2172
     $     CLRLS(IPLOND),           ! CLEAR SKY SURFACE COOING                   RADCLW.57
     $     CLRLT(IPLOND),           ! NET CLEAR SKY OUTGOING FLUX                RADCLW.58
     $     SLWD(IPLOND)             ! DOWN LONGWAVE FLUX AT SURFACE              RADCLW.59
C                                                                                RADCLW.60
C LOCAL WORKSPACE                                                                RADCLW.61
C                                                                                RADCLW.62
      INTEGER     I,   ! LONGITUDE INDEX                                         RADCLW.63
     $            K,   ! LEVEL INDEX                                             RADCLW.64
     $           K1,   ! LEVEL INDEX                                             RADCLW.65
     $           K2,   ! LEVEL INDEX                                             RADCLW.66
     $           K3,   ! LEVEL INDEX                                             RADCLW.67
     $           KM,   ! LEVEL INDEX                                             RADCLW.68
     $          KMM,   ! LEVEL INDEX                                             RADCLW.69
     $          KM1,   ! LEVEL INDEX                                             RADCLW.70
     $          KM2,   ! LEVEL INDEX                                             RADCLW.71
     $          KM3,   ! LEVEL INDEX                                             RADCLW.72
     $          KM4    ! LEVEL INDEX                                             RADCLW.73
C                                                                                RADCLW.74
      INTEGER  ITOP,   ! LEVEL INDEX                                             RADCLW.75
     $         ICLD,   ! LEVEL INDEX                                             RADCLW.76
     $          KLO,   ! LEVEL INDEX                                             RADCLW.77
     $          KHI,   ! LEVEL INDEX                                             RADCLW.78
     $          MP1,   ! LEVEL INDEX                                             RADCLW.79
     $         MP12,   ! LEVEL INDEX                                             RADCLW.80
     $          MM1,   ! LEVEL INDEX                                             RADCLW.81
     $        NPTSC    ! LEVEL INDEX                                             RADCLW.82
C                                                                                RADCLW.83
      REAL      SUM,   ! SUM                                                     RADCLW.84
     $         TMP1,   ! TEMPORARY 1                                             RADCLW.85
     $     ABSBT(IPLOND)                                                         RADCLW.86
C                                                                                RADCLW.87
      REAL CO2EM(IPLOND,IPLEVP),   ! LAYER CO2 NORMALIZED PLNCK FUNCTION         RADCLW.88
     $     CO2EML(IPLOND,IPLEV),   ! INTRFC CO2 NORMALIZED PLNCK FUNCTIO         RADCLW.89
     $     DELT(IPLOND),          ! DIFF T**4 MID LAYER TO TOP INTERFACE         RADCLW.90
     $     DELT1(IPLOND),         ! DIFF T**4 LOWER INTRFC TO MID LAYER          RADCLW.91
     $     BK1(IPLOND),           ! ABSRPTVTY FOR VERTICAL QUADRATURE            RADCLW.92
     $     BK2(IPLOND),           ! ABSRPTVTY FOR VERTICAL QUADRATURE            RADCLW.93
     $     FUL(IPLOND,IPLEVP),          ! TOTAL UPWARDS LONGWAVE FLUX            RADCLW.94
     $     FSUL(IPLOND,IPLEVP),         ! CLEAR SKY UPWARDS LONGWAVE FLU         RADCLW.95
     $     FDL(IPLOND,IPLEVP),          ! TOTAL DOWNWARDS LONGWAVE FLUX          RADCLW.96
     $     FSDL(IPLOND,IPLEVP),         ! CLEAR SKY DOWNWARDS LONGWAVE F         RADCLW.97
     $     FCLB4(IPLOND,IPLEV),         ! SIG T**4 FOR CLOUD BOTTOM INTE         RADCLW.98
     $     FCLT4(IPLOND,IPLEV),         ! SIG T**4 FOR CLOUD TOP INTERFA         RADCLW.99
     $     S(IPLOND,IPLEVP,IPLEVP)       ! FLX INTEGRAL SUM                      RADCLW.100
      REAL ABSNXT(IPLOND,IPLEV,4),      ! NEAREST LAYER ABSORPTIVITIES           RADCLW.101
     $     ABSTOT(IPLOND,IPLEVP,IPLEVP), ! NON-ADJACENT LAYER ABSORPTIVI         RADCLW.102
     $     EMSTOT(IPLOND,IPLEVP),       ! TOTAL EMISSIVITY                       RADCLW.103
     $     TPLNKA(IPLOND,IPLEVP),       ! PLANCK FNCTN TMP                       RADCLW.104
     $     S2C(IPLOND,IPLEVP),          ! H2O CONT AMOUNT                        RADCLW.105
     $     S2T(IPLOND,IPLEVP),          ! H2O CONT TMP                           RADCLW.106
     $     W(IPLOND,IPLEVP),            ! H2O PATH                               RADCLW.107
     $     TPLNKE(IPLOND)              ! PLANCK FNCTN TMP                        RADCLW.108
      REAL H2OTR(IPLOND,IPLEVP),        ! H2O TRNMSN FOR O3 OVERLAP              RADCLW.109
     $     CO2T(IPLOND,IPLEVP),         ! PRS WGHTED TMP PATH                    RADCLW.110
     $     TINT(IPLOND,IPLEVP),         ! INTERFACE TMP                          RADCLW.111
     $     TINT4(IPLOND,IPLEVP),        ! INTERFACE TMP**4                       RADCLW.112
     $     TLAYR(IPLOND,IPLEVP),        ! LEVEL TMP                              RADCLW.113
     $     TLAYR4(IPLOND,IPLEVP)        ! LEVEL TMP**4                           RADCLW.114
      INTEGER IPOS(IPLOND),            ! ARRAY FOR SPECIFIED CONDITION           RADCLW.115
     $        INDXC(IPLOND),           ! INDICES FOR CLOUD COVERED POINT         RADCLW.116
     $        KLOV(IPLOND),            ! CLOUD LOWEST LEVEL INDEX                RADCLW.117
     $        KHIV(IPLOND)             ! CLOUD HIGHEST LEVEL INDEX               RADCLW.118
C      REAL ABSEMS(LNGBUF)                                                       RADCLW.119
C      EQUIVALENCE (ABSTOT,ABSEMS(1                                   ))         RADCLW.120
C     $            (ABSNXT,ABSEMS(1 + IPLOND*IPLEVP*IPLEVP                       RADCLW.121
C     $            (EMSTOT,ABSEMS(1 + IPLOND*IPLEVP*IPLEVP + IPLOND*IPLE         RADCLW.122
C                                                                                RADCLW.123
C      EXTERNAL RADTPL,    ! COMPUTE PATH LENGTHS                                RADCLW.124
C     $         RADEMS,    ! H2O,CO2,O3 EMISSIVITY                               RADCLW.125
C     $         RADABS,    ! H2O,CO2,O3 ABSORPTIVITY                             RADCLW.126
C     $       WRITERIC,    ! WRITE FOR ABS/EMS                                   RADCLW.127
C     $        READRIC     ! READ  FOR ABS/EMS                                   RADCLW.128
C                                                                                RADCLW.129
C                                                                                RADCLW.130
C  INITIALIZE AND RECOMPUTE THE TCLRSF ARRAY                                     RADCLW.131
C      TCLRSF ARRAY WILL BE LOGS TO HANDLE SMALL NUMBERS BETTER                  25JAN00.683
C                                                                                RADCLW.132
#ifdef MPP1                                                                      RADCLW.133
#   define IPLON I  LX                                                           RADCLW.134
#   define IPLOND I  LX                                                          RADCLW.135
#endif                                                                           RADCLW.136
      ABEMH=3.0                                                                  RADCLW.137
      JX=MJX                                                                     RADCLW.138
      IFRABE=NINT(3600.*ABEMH/DT)                                                RADCLW.139
      DO 40 K=1,IPLEV                                                            RADCLW.140
        DO 30 I=1,IPLON                                                          RADCLW.141
          FCLB4(I,K)=0.                                                          RADCLW.142
          FCLT4(I,K)=0.                                                          RADCLW.143
          IF(K.EQ.1)TCLRSF(I,K)=ALOG(TCLRSF(I,K))                                25JAN00.684
          TCLRSF(I,K+1)=TCLRSF(I,K)+ALOG(1.-CLD(I,K+1))                          25JAN00.685
   30   CONTINUE                                                                 RADCLW.145
   40 CONTINUE                                                                   RADCLW.146
C                                                                                RADCLW.147
C CALCULATE SOME TEMPERATURES NEEDED TO DERIVE ABSORPTIVITY AND                  RADCLW.148
C EMISSIVITY, AS WELL AS SOME H2O PATH LENGTHS                                   RADCLW.149
C                                                                                RADCLW.150
      CALL RADTPL(TNM,TG,QNM,PMID,PINT,PLH2O,TPLNKA,S2C,S2T,W,TPLNKE,            RADCLW.151
     +     TINT,TINT4,TLAYR,TLAYR4,PMLN,PILN)                                    RADCLW.152
C                                                                                RADCLW.153
C DO EMISSIVITY AND ABSORPTIVITY CALCULATIONS                                    RADCLW.154
C ONLY IF ABS/EMS COMPUTATION                                                    RADCLW.155
C                                                                                RADCLW.156
C      IF(KTAU.EQ.0 .OR. (KTAU.EQ.KTAUR) .OR.                                    RADCLW.157
C    $   (MOD(KTAU+1,IFRABE).EQ.0) ) THEN                                        RADCLW.158
C                                                                                RADCLW.159
C COMPUTE TOTAL EMISSIVITY:                                                      RADCLW.160
C                                                                                RADCLW.161
      CALL RADEMS(S2C,S2T,W,TPLNKE,PLH2O,PINT,PLCO2,TINT,TINT4,TLAYR,            RADCLW.162
     +     TLAYR4,PLOL,PLOS,CO2EM,CO2EML,CO2T,H2OTR,EMSTOT)                      RADCLW.163
C                                                                                RADCLW.164
C COMPUTE TOTAL ABSORPTIVITY:                                                    RADCLW.165
C                                                                                RADCLW.166
      CALL RADABS(PMID,PINT,CO2EM,CO2EML,TPLNKA,S2C,S2T,W,H2OTR,PLCO2,           RADCLW.167
     +     PLH2O,CO2T,TINT,TLAYR,PLOL,PLOS,PMLN,PILN,ABSTOT,ABSNXT)              RADCLW.168
C                                                                                RADCLW.169
C HERE OUTPUT EMSTOT, ABSTOT, ABSNXT                                             RADCLW.170
C       CALL PUTABE(JSLC,JX-1,IPLOND,IPLEV,IPLEVP,ABSTOT,ABSNXT,EMSTOT)          RADCLW.171
C                                                                                RADCLW.172
C     ELSE                                                                       RADCLW.173
C                                                                                RADCLW.174
C HERE RETRIEVE EMSTOT, ABSTOT, ABSNXT                                           RADCLW.175
C LAST ABS/EMS COMPUTATION                                                       RADCLW.176
C                                                                                RADCLW.177
C       CALL GETABE(JSLC,JX-1,IPLOND,IPLEV,IPLEVP,ABSTOT,ABSNXT,EMSTOT)          RADCLW.178
C                                                                                RADCLW.179
C     ENDIF                                                                      RADCLW.180
C                                                                                RADCLW.181
C COMPUTE FLUXES AND COOLING RATES.                                              RADCLW.182
C INITIALIZE LONGITUDE INDEX SUBSET.                                             RADCLW.183
C                                                                                RADCLW.184
      DO 100 I=1,IPLON                                                           RADCLW.185
        IPOS(I)=1                                                                RADCLW.186
  100 CONTINUE                                                                   RADCLW.187
C                                                                                RADCLW.188
C FIND THE LOWEST AND HIGHEST LEVEL CLOUD FOR EACH GRID POINT                    RADCLW.189
C                                                                                RADCLW.190
      DO 120 I=1,IPLON                                                           RADCLW.191
        KLOV(I)=0                                                                RADCLW.192
        DO 110 K=1,IPLEV                                                         RADCLW.193
          IF(CLD(I,IPLEVP2-K).GT.0.0)THEN                                        RADCLW.194
            KLOV(I)=K                                                            RADCLW.195
            GOTO 120                                                             RADCLW.196
          ENDIF                                                                  RADCLW.197
  110   CONTINUE                                                                 RADCLW.198
  120 CONTINUE                                                                   RADCLW.199
C                                                                                RADCLW.200
      DO 140 I=1,IPLON                                                           RADCLW.201
        KHIV(I)=KLOV(I)                                                          RADCLW.202
        ITOP=KLOV(I)                                                             RADCLW.203
        IF(ITOP.EQ.0)ITOP=1                                                      RADCLW.204
        DO 130 K=IPLEV,ITOP,-1                                                   RADCLW.205
          IF(CLD(I,IPLEVP2-K).GT.0.0)THEN                                        RADCLW.206
            KHIV(I)=K                                                            RADCLW.207
            GOTO 140                                                             RADCLW.208
          ENDIF                                                                  RADCLW.209
  130   CONTINUE                                                                 RADCLW.210
  140 CONTINUE                                                                   RADCLW.211
C                                                                                RADCLW.212
      DO 160 I=1,IPLON                                                           RADCLW.213
        IF(KLOV(I).NE.0)THEN                                                     RADCLW.214
          DO 150 K=KLOV(I),KHIV(I)                                               RADCLW.215
            FCLT4(I,IPLEVP-K)=STEBOL*TINT4(I,IPLEVP2-K)                          RADCLW.216
            FCLB4(I,IPLEVP-K)=STEBOL*TINT4(I,IPLEVP3-K)                          RADCLW.217
  150     CONTINUE                                                               RADCLW.218
        ENDIF                                                                    RADCLW.219
  160 CONTINUE                                                                   RADCLW.220
C                                                                                RADCLW.221
C COMPUTE SUMS USED IN INTEGRALS (ALL LONGITUDE POINTS)                          RADCLW.222
C                                                                                RADCLW.223
C DEFINITION OF BK1 & BK2 DEPENDS ON FINITE DIFFERENCING.                        RADCLW.224
C FOR TRAPEZOIDAL RULE BK1=BK2. TRAPEZOIDAL RULE APPLIED FOR                     RADCLW.225
C NONADJACENT LAYERS ONLY.                                                       RADCLW.226
C                                                                                RADCLW.227
C DELT=T**4 IN LAYER ABOVE CURRENT SIGMA LEVEL KM.                               RADCLW.228
C DELT1=T**4 IN LAYER BELOW CURRENT SIGMA LEVEL KM.                              RADCLW.229
C                                                                                RADCLW.230
      DO 210 I=1,IPLON                                                           RADCLW.231
        DELT(I)=TINT4(I,IPLEV)-TLAYR4(I,IPLEVP)                                  RADCLW.232
        DELT1(I)=TLAYR4(I,IPLEVP)-TINT4(I,IPLEVP)                                RADCLW.233
        S(I,IPLEVP,IPLEVP)=STEBOL*(DELT1(I)*ABSNXT(I,IPLEV,1)+DELT(I)*           RADCLW.234
     +                     ABSNXT(I,IPLEV,4))                                    RADCLW.235
        S(I,IPLEV,IPLEVP)=STEBOL*(DELT(I)*ABSNXT(I,IPLEV,2)+DELT1(I)*            RADCLW.236
     +                    ABSNXT(I,IPLEV,3))                                     RADCLW.237
  210 CONTINUE                                                                   RADCLW.238
      DO 230 K=1,IPLEV-1                                                         RADCLW.239
        DO 220 I=1,IPLON                                                         RADCLW.240
          BK2(I)=(ABSTOT(I,K,IPLEV)+ABSTOT(I,K,IPLEVP))*0.5                      RADCLW.241
          BK1(I)=BK2(I)                                                          RADCLW.242
          S(I,K,IPLEVP)=STEBOL*(BK2(I)*DELT(I)+BK1(I)*DELT1(I))                  RADCLW.243
  220   CONTINUE                                                                 RADCLW.244
  230 CONTINUE                                                                   RADCLW.245
C                                                                                RADCLW.246
C ALL K, KM>1                                                                    RADCLW.247
C                                                                                RADCLW.248
      DO 300 KM=IPLEV,2,-1                                                       RADCLW.249
        DO 240 I=1,IPLON                                                         RADCLW.250
          DELT(I)=TINT4(I,KM-1)-TLAYR4(I,KM)                                     RADCLW.251
          DELT1(I)=TLAYR4(I,KM)-TINT4(I,KM)                                      RADCLW.252
  240   CONTINUE                                                                 RADCLW.253
        DO 290 K=IPLEVP,1,-1                                                     RADCLW.254
          IF(K.EQ.KM)THEN                                                        RADCLW.255
            DO 250 I=1,IPLON                                                     RADCLW.256
              BK2(I)=ABSNXT(I,KM-1,4)                                            RADCLW.257
              BK1(I)=ABSNXT(I,KM-1,1)                                            RADCLW.258
  250       CONTINUE                                                             RADCLW.259
          ELSEIF(K.EQ.KM-1)THEN                                                  RADCLW.260
            DO 260 I=1,IPLON                                                     RADCLW.261
              BK2(I)=ABSNXT(I,KM-1,2)                                            RADCLW.262
              BK1(I)=ABSNXT(I,KM-1,3)                                            RADCLW.263
  260       CONTINUE                                                             RADCLW.264
          ELSE                                                                   RADCLW.265
            DO 270 I=1,IPLON                                                     RADCLW.266
              BK2(I)=(ABSTOT(I,K,KM-1)+ABSTOT(I,K,KM))*0.5                       RADCLW.267
              BK1(I)=BK2(I)                                                      RADCLW.268
  270       CONTINUE                                                             RADCLW.269
          ENDIF                                                                  RADCLW.270
          DO 280 I=1,IPLON                                                       RADCLW.271
            S(I,K,KM)=S(I,K,KM+1)+STEBOL*(BK2(I)*DELT(I)+BK1(I)*DELT1(I)         RADCLW.272
     +                )                                                          RADCLW.273
  280     CONTINUE                                                               RADCLW.274
  290   CONTINUE                                                                 RADCLW.275
  300 CONTINUE                                                                   RADCLW.276
C                                                                                RADCLW.277
C COMPUTATION OF CLEAR SKY FLUXES                                                RADCLW.278
C ALWAYS SET FIRST LEVEL OF FSUL                                                 RADCLW.279
C                                                                                RADCLW.280
      DO 340 I=1,IPLON                                                           RADCLW.281
        FSUL(I,IPLEVP)=STEBOL*(TG(I)**4)                                         RADCLW.282
  340 CONTINUE                                                                   RADCLW.283
C                                                                                RADCLW.284
C ALL LONGITUDE POINTS                                                           RADCLW.285
C                                                                                RADCLW.286
      DO 360 K=1,IPLEV                                                           RADCLW.287
        DO 350 I=1,IPLON                                                         RADCLW.288
          TMP1=FSUL(I,IPLEVP)-STEBOL*TINT4(I,IPLEVP)                             RADCLW.289
          FSUL(I,K)=FSUL(I,IPLEVP)-ABSTOT(I,K,IPLEVP)*TMP1+S(I,K,K+1)            RADCLW.290
  350   CONTINUE                                                                 RADCLW.291
  360 CONTINUE                                                                   RADCLW.292
C                                                                                RADCLW.293
C DOWNWARD CLEAR SKY FLUXES                                                      RADCLW.294
C STORE INTERMEDIATE QUANTITIES IN DOWN FLUX                                     RADCLW.295
C                                                                                RADCLW.296
      DO 380 K=1,IPLEVP                                                          RADCLW.297
        DO 370 I=1,IPLON                                                         RADCLW.298
          FSDL(I,K)=STEBOL*(TPLNKE(I)**4)*EMSTOT(I,K)                            RADCLW.299
  370   CONTINUE                                                                 RADCLW.300
  380 CONTINUE                                                                   RADCLW.301
C                                                                                RADCLW.302
C STORE THE DOWNWARD EMISSION FROM LEVEL 1                                       RADCLW.303
C = TOTAL GAS EMISSION * SIGMA T**4.                                             RADCLW.304
C FSDL DOES NOT YET INCLUDE ALL TERMS                                            RADCLW.305
C                                                                                RADCLW.306
      DO 390 I=1,IPLON                                                           RADCLW.307
        ABSBT(I)=FSDL(I,IPLEVP)                                                  RADCLW.308
  390 CONTINUE                                                                   RADCLW.309
C                                                                                RADCLW.310
C FSDL(I,IPLEVP) ASSUMES ISOTHERMAL LAYER                                        RADCLW.311
C                                                                                RADCLW.312
      DO 420 K=2,IPLEV                                                           RADCLW.313
        DO 410 I=1,IPLON                                                         RADCLW.314
          FSDL(I,K)=FSDL(I,K)-(S(I,K,2)-S(I,K,K+1))                              RADCLW.315
  410   CONTINUE                                                                 RADCLW.316
  420 CONTINUE                                                                   RADCLW.317
      DO 430 I=1,IPLON                                                           RADCLW.318
        FSDL(I,IPLEVP)=ABSBT(I)-S(I,IPLEVP,2)                                    RADCLW.319
  430 CONTINUE                                                                   RADCLW.320
C                                                                                RADCLW.321
C COMPUTATION OF THE UPWARD FLUXES                                               RADCLW.322
C                                                                                RADCLW.323
C FIRST SET FLUXES TO CLEAR SKY VALUES, THEN MODIFY FOR CLOUDS                   RADCLW.324
C                                                                                RADCLW.325
      DO 450 K=1,IPLEVP                                                          RADCLW.326
        DO 440 I=1,IPLON                                                         RADCLW.327
          FUL(I,K)=FSUL(I,K)                                                     RADCLW.328
          FDL(I,K)=FSDL(I,K)                                                     RADCLW.329
  440   CONTINUE                                                                 RADCLW.330
  450 CONTINUE                                                                   RADCLW.331
C                                                                                RADCLW.332
C MODIFICATIONS FOR CLOUDS                                                       RADCLW.333
C                                                                                RADCLW.334
C FURTHER QUALIFY LONGITUDE SUBSET FOR COMPUTATIONS BY CHANGING                  RADCLW.335
C IPOS FROM 1 TO 2 WHERE THERE ARE CLOUDS                                        RADCLW.336
C (TOTAL CLOUD FRACTION <= 1.E-3 TREATED AS CLEAR)                               RADCLW.337
C                                                                                RADCLW.338
      DO 460 I=1,IPLON                                                           RADCLW.339
        IF((-TCLRSF(I,IPLEVP)).GT.1.E-3)THEN                                     25JAN00.686
          IPOS(I)=2                                                              RADCLW.341
        ENDIF                                                                    RADCLW.342
  460 CONTINUE                                                                   RADCLW.343
C                                                                                RADCLW.344
C GENERATE LONGITUDE INDEX LIST (INDXC) FOR CLOUD MODIFICATIONS                  RADCLW.345
C                                                                                RADCLW.346
      CALL WHENEQ(IPLON,IPOS,1,2,INDXC,NPTSC)                                    RADCLW.347
C                                                                                RADCLW.348
C COMPUTE DOWNFLUX AT LEVEL 1 FOR CLOUDY SKY                                     RADCLW.349
C                                                                                RADCLW.350
      DO 470 ICLD=1,NPTSC                                                        RADCLW.351
#ifndef MPP1                                                                     RADCLW.352
        I=INDXC(ICLD)                                                            RADCLW.353
#else                                                                            RADCLW.354
        I=INDXC(NOFLIC(ICLD))                                                    RADCLW.355
#endif                                                                           RADCLW.356
C                                                                                RADCLW.357
C FIRST CLEAR SKY FLUX PLUS FLUX FROM CLOUD AT LEVEL 1                           RADCLW.358
C                                                                                RADCLW.359
        FDL(I,IPLEVP)=FSDL(I,IPLEVP)*EXP(TCLRSF(I,IPLEV)-(                       25JAN00.687
     +                TCLRSF(I,IPLEVP-KHIV(I))))+FCLB4(I,IPLEV-1)*               25JAN00.688
     +                CLD(I,IPLEV)                                               RADCLW.362
  470 CONTINUE                                                                   RADCLW.363
C                                                                                RADCLW.364
C FLUX EMITTED BY OTHER LAYERS                                                   RADCLW.365
C                                                                                RADCLW.366
      DO 490 ICLD=1,NPTSC                                                        RADCLW.367
#ifndef MPP1                                                                     RADCLW.368
        I=INDXC(ICLD)                                                            RADCLW.369
        DO 480 KM=3,KHIV(I)                                                      RADCLW.370
#else                                                                            RADCLW.371
          I=INDXC(NOFLIC(ICLD))                                                  RADCLW.372
C FLIC SHOULD CONVERT THIS BUT IT DOES NOT (BUG)                                 RADCLW.373
          DO 480 KM=3,KHIV(FLIC_G2L_M(I,MIX))                                    RADCLW.374
#endif                                                                           RADCLW.375
            KM1=IPLEVP-KM                                                        RADCLW.376
            KM2=IPLEVP2-KM                                                       RADCLW.377
            KM4=IPLEVP4-KM                                                       RADCLW.378
            TMP1=CLD(I,KM2)*EXP(TCLRSF(I,IPLEV)-(TCLRSF(I,KM2)))                 25JAN00.689
            FDL(I,IPLEVP)=FDL(I,IPLEVP)+(FCLB4(I,KM1)-S(I,IPLEVP,KM4))*          RADCLW.380
     +                    TMP1                                                   RADCLW.381
  480     CONTINUE                                                               RADCLW.382
                                                                                 RADCLW.383
  490 CONTINUE                                                                   RADCLW.384
C                                                                                RADCLW.385
C BEGIN OUTER LONGITUDE LOOP FOR CLOUD MODIFICATIONS                             RADCLW.386
C (OPERATE ON POINTS WITH CLOUDS ONLY)                                           RADCLW.387
C                                                                                RADCLW.388
      DO 560 ICLD=1,NPTSC                                                        RADCLW.389
#ifndef MPP1                                                                     RADCLW.390
        I=INDXC(ICLD)                                                            RADCLW.391
#else                                                                            RADCLW.392
        I=INDXC(NOFLIC(ICLD))                                                    RADCLW.393
#endif                                                                           RADCLW.394
        KLO=KLOV(I)                                                              RADCLW.395
        KHI=KHIV(I)                                                              RADCLW.396
        MP1=KHI+1                                                                RADCLW.397
        MP12=IPLEVP2-MP1                                                         RADCLW.398
        MM1=KHI-1                                                                RADCLW.399
        DO 510 K=KLO,MM1                                                         RADCLW.400
          K1=IPLEVP-K                                                            RADCLW.401
          K2=IPLEVP2-K                                                           RADCLW.402
          K3=IPLEVP3-K                                                           RADCLW.403
          SUM=FSUL(I,K2)*EXP((TCLRSF(I,IPLEVP)-(TCLRSF(I,K1))))                  25JAN00.690
          DO 500 KM=KLO,K                                                        RADCLW.405
            KM1=IPLEVP-KM                                                        RADCLW.406
            KM2=IPLEVP2-KM                                                       RADCLW.407
            KM3=IPLEVP3-KM                                                       RADCLW.408
            SUM=SUM+(FCLT4(I,KM1)+S(I,K2,K3)-S(I,K2,KM3))*CLD(I,KM2)*            25JAN00.691
     +          EXP(TCLRSF(I,KM1)-(TCLRSF(I,K1)))                                25JAN00.692
  500     CONTINUE                                                               RADCLW.411
          FUL(I,K2)=SUM                                                          RADCLW.412
  510   CONTINUE                                                                 RADCLW.413
        DO 530 K=KHI,IPLEVP                                                      RADCLW.414
          K2=IPLEVP2-K                                                           RADCLW.415
          K3=IPLEVP3-K                                                           RADCLW.416
          SUM=FSUL(I,K2)*EXP(TCLRSF(I,IPLEVP)-(TCLRSF(I,MP12)))                  25JAN00.693
          DO 520 KM=KLO,KHI                                                      RADCLW.418
            KM1=IPLEVP-KM                                                        RADCLW.419
            KM2=IPLEVP2-KM                                                       RADCLW.420
            KM3=IPLEVP3-KM                                                       RADCLW.421
            SUM=SUM+(CLD(I,KM2)*EXP(TCLRSF(I,KM1)-(TCLRSF(I,MP12))))*(           25JAN00.694
     +          FCLT4(I,KM1)+S(I,K2,K3)-S(I,K2,KM3))                             RADCLW.423
  520     CONTINUE                                                               RADCLW.424
          FUL(I,K2)=SUM                                                          RADCLW.425
  530   CONTINUE                                                                 RADCLW.426
C                                                                                RADCLW.427
C COMPUTATION OF THE DOWNWARD FLUXES                                             RADCLW.428
C                                                                                RADCLW.429
        DO 550 K=2,MM1                                                           RADCLW.430
          K1=IPLEVP-K                                                            RADCLW.431
          K2=IPLEVP2-K                                                           RADCLW.432
          K3=IPLEVP3-K                                                           RADCLW.433
          SUM=0.                                                                 RADCLW.434
          KMM=MAX0(K+1,KLO)                                                      RADCLW.435
          DO 540 KM=KMM,KHI                                                      RADCLW.436
            KM1=IPLEVP-KM                                                        RADCLW.437
            KM2=IPLEVP2-KM                                                       RADCLW.438
            KM4=IPLEVP4-KM                                                       RADCLW.439
            SUM=SUM+(CLD(I,KM2)*EXP(TCLRSF(I,K1)-(TCLRSF(I,KM2))))*(             25JAN00.695
     +          FCLB4(I,KM1)-S(I,K2,KM4)+S(I,K2,K3))                             RADCLW.441
  540     CONTINUE                                                               RADCLW.442
          FDL(I,K2)=SUM+FSDL(I,K2)*EXP(TCLRSF(I,K1)-(TCLRSF(I,MP12))             25JAN00.696
     +              )                                                            RADCLW.444
  550   CONTINUE                                                                 RADCLW.445
C                                                                                RADCLW.446
C END CLOUD MODIFICATION LONGITUDE LOOP                                          RADCLW.447
C                                                                                RADCLW.448
  560 CONTINUE                                                                   RADCLW.449
C                                                                                RADCLW.450
C BACK TO ORIGINAL LONGITUDE SUBSET.                                             RADCLW.451
C PUT DOWN LONGWAVE FLUX IN LOCAL ARRAY.                                         RADCLW.452
C                                                                                RADCLW.453
      DO 570 I=1,IPLON                                                           RADCLW.454
        SLWD(I)=FDL(I,IPLEVP)                                                    RADCLW.455
        FRLA(I)=FUL(I,IPLEVP)-FDL(I,IPLEVP)                                      RADCLW.456
  570 CONTINUE                                                                   RADCLW.457
C                                                                                RADCLW.458
C ALL LONGITUDES: STORE HISTORY TAPE QUANTITIES                                  RADCLW.459
C                                                                                RADCLW.460
      DO 580 I=1,IPLON                                                           RADCLW.461
C                                                                                RADCLW.462
C NET FLUX                                                                       RADCLW.463
C                                                                                RADCLW.464
        FRLA(I)=FUL(I,IPLEVP)-FDL(I,IPLEVP)                                      RADCLW.465
C                                                                                RADCLW.466
C CLEAR SKY FLUX AT TOP OF ATMOSPHERE                                            RADCLW.467
C                                                                                RADCLW.468
        CLRLT(I)=FSUL(I,1)                                                       RADCLW.469
        CLRLS(I)=FSUL(I,IPLEVP)-FSDL(I,IPLEVP)                                   RADCLW.470
C                                                                                RADCLW.471
C OUTGOING IR                                                                    RADCLW.472
C                                                                                RADCLW.473
        FIRTP(I)=FUL(I,1)                                                        07NOV00.2173
  580 CONTINUE                                                                   RADCLW.475
C                                                                                RADCLW.476
C COMPUTATION OF LONGWAVE HEATING (K PER SEC)                                    RADCLW.477
C                                                                                RADCLW.478
      DO 600 K=1,IPLEV                                                           RADCLW.479
        DO 590 I=1,IPLON                                                         RADCLW.480
          QRL(I,K)=(FUL(I,K)-FDL(I,K)-FUL(I,K+1)+FDL(I,K+1))*GRAVIT/((           RADCLW.481
     +             PINT(I,K)-PINT(I,K+1))*CPAIR)                                 RADCLW.482
  590   CONTINUE                                                                 RADCLW.483
  600 CONTINUE                                                                   RADCLW.484
C                                                                                RADCLW.485
C DONE                                                                           RADCLW.486
C                                                                                RADCLW.487
      RETURN                                                                     RADCLW.488
      END                                                                        RADCLW.489
