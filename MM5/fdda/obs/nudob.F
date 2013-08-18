      SUBROUTINE NUDOB(J,IVAR,ATEN,IN,A,                                         NUDOB.1
     1ISOUTH,JWEST,UB,VB,TB,QVB,PSB,PSBD,KPBLT,PTOP)                             NUDOB.2
C                                                                                NUDOB.3
C THIS SUBROUTINE GENERATES NUDGING TENDENCIES FOR THE J-TH                      NUDOB.4
C VERTICAL SLICE (I-K PLANE) FOR FOUR-DIMENSIONAL DATA                           NUDOB.5
C ASSIMILATION FROM INDIVIDUAL OBSERVATIONS.  THE NUDGING                        NUDOB.6
C TENDENCIES ARE FOUND FROM A ONE-PASS CALCULATION OF                            NUDOB.7
C WEIGHTING FACTORS SIMILAR TO THE BENJAMIN-SEAMAN OBJECTIVE                     NUDOB.8
C ANALYSIS.  THIS SUBROUTINE IS DESIGNED FOR RAPID EXECUTION                     NUDOB.9
C AND MINIMAL STORAGE REQUIREMENTS.  ALGORITHMS SHOULD BE                        NUDOB.10
C VECTORIZED WHEREVER POSSIBLE.                                                  NUDOB.11
C                                                                                NUDOB.12
C NET WEIGHTING (WT) OF THE DIFFERENCE BETWEEN THE OBSERVATIONS                  NUDOB.13
C AND LOCAL FORECAST VALUES IS BASED ON THE MULTIPLE OF THREE                    NUDOB.14
C TYPES OF FACTORS:                                                              NUDOB.15
C       1) TIME WEIGHTING - ONLY OBSERVATIONS WITHIN A SELECTED                  NUDOB.16
C          TIME WINDOW (TWINDO) CENTERED AT THE CURRENT FORECAST                 NUDOB.17
C          TIME (XTIME) ARE USED.  OBSERVATIONS CLOSEST TO                       NUDOB.18
C          XTIME ARE TIME-WEIGHTED MOST HEAVILY (TIMEWT)                         NUDOB.19
C       2) VERTICAL WEIGHTING - NON-ZERO WEIGHTS (WTSIG) ARE                     NUDOB.20
C          CALCULATED WITHIN A VERTICAL REGION OF INFLUENCE                      NUDOB.21
C          (RINSIG).                                                             NUDOB.22
C       3) HORIZONTAL WEIGHTING - NON-ZERO WEIGHTS (WTIJ) ARE                    NUDOB.23
C          CALCULATED WITHIN A RADIUS OF INFLUENCE (RINXY).  THE                 NUDOB.24
C          VALUE OF RIN IS DEFINED IN KILOMETERS, AND CONVERTED                  NUDOB.25
C          TO GRID LENGTHS FOR THE APPROPRIATE MESH SIZE.                        NUDOB.26
C                                                                                NUDOB.27
C THE FIVE FORECAST VARIABLES ARE PROCESSED BY CHANGING THE                      NUDOB.28
C VALUE OF IVAR AS FOLLOWS:                                                      NUDOB.29
C             IVAR                     VARIABLE(TAU-1)                           NUDOB.30
C             ----                     ---------------                           NUDOB.31
C               1                             U                                  NUDOB.32
C               2                             V                                  NUDOB.33
C               3                             T                                  NUDOB.34
C               4                             QV                                 NUDOB.35
C               5                          PSB(CROSS)   REMOVED IN V3            NUDOB.36
C              (6)                           PSB(DOT)                            NUDOB.37
C                                                                                NUDOB.38
C                                                                                NUDOB.39
#     include <parame.incl>                                                      NUDOB.40
#     include <fddaob.incl>                                                      NUDOB.41
#     include <param2.incl>                                                      NUDOB.42
#     include <parfdda.incl>                                                     NUDOB.43
#     include <various.incl>                                                     NUDOB.44
      DIMENSION WTP(MIXFO,MKXFO),PWT2ER(MIXFO,MKXFO)                             NUDOB.45
      DIMENSION WTSIG(MKXFO),WT(MIXFO,MKXFO),WT2ERR(MIXFO,MKXFO),                NUDOB.46
     1RA(NIOBF),RB(NIOBF),                                                       NUDOB.47
C NOTE: IONF WAS ADDED TO PARAM2, AND WPBL IS ASSOCIATED WITH FDAMOD             NUDOB.48
     2ATEN(MIX,MJX,MKX),A(MKX),UB(MIX,MJX,MKX),                                  NUDOB.49
     3VB(MIX,MJX,MKX),TB(MIX,MJX,MKX),QVB(MIX,MJX,MKX),                          NUDOB.50
     4PSB(MIX,MJX)                                                               NUDOB.51
      DIMENSION SCRPR(MKXFO,MIXFO)                                               NUDOB.52
      CHARACTER*40 NAME                                                          NUDOB.53
      DIMENSION PSBD(MIX,MJX),                                                   NUDOB.54
     1 KPBLT(MIX),WPBL(MIX,MKX),                                                 NUDOB.55
     1          PSBO(NIOBF),PDOC(MIXFO,MJXFO),IA(NIOBF),IB(NIOBF)                NUDOB.56
      DIMENSION MM(MAXSES)                                                       NUDOB.57
      NSTA=NSTAT(IN)                                                             NUDOB.58
C COMPUTE IPL WHICH REPRESENTS IVAR FOR EACH MESH IN SAVWT MODS                  NUDOB.59
      ICUT=0                                                                     NUDOB.60
      IF(IN.GT.1)ICUT=1                                                          NUDOB.61
      ISOUTH=NESTI(IN)                                                           NUDOB.62
      JWEST=NESTJ(IN)                                                            NUDOB.63
      IPL=IVAR                                                                   NUDOB.64
C                                                                                NUDOB.65
C DEFINE GRID-TYPE OFFSET FACTORS, IGRID AND GRID                                NUDOB.66
C                                                                                NUDOB.67
      IF(IN.EQ.1)THEN                                                            NUDOB.68
        INPF=NPFI                                                                NUDOB.69
        INFR=IONF                                                                NUDOB.70
      ELSE                                                                       NUDOB.71
        IF(IRAX.NE.3)STOP 1                                                      NUDOB.72
        INPF=(3**LEVIDN(IN))*NPFI                                                NUDOB.73
        INFR=(3**LEVIDN(IN))*IONF                                                NUDOB.74
      ENDIF                                                                      NUDOB.75
      GRID=0.0                                                                   NUDOB.76
      IGRID=0                                                                    NUDOB.77
      IF(IVAR.GE.3)THEN                                                          NUDOB.78
        GRID=0.5                                                                 NUDOB.79
        IGRID=1                                                                  NUDOB.80
      ENDIF                                                                      NUDOB.81
C IWTSIG(IN) IS DEFINED IN PARAM AND STORED IN PARAM2                            NUDOB.82
      IF(IWTSIG(IN).EQ.1)RINFMX=1.0                                              NUDOB.83
C COMPUTE WPBL(I,K), THE WEIGHT USED TO RESTRICT OBS NUDGING FOR TEMP            NUDOB.84
C AND MOISTURE IN THE MODEL SIMULATED BOUNDARY LAYER                             NUDOB.85
      IF(IVAR.GT.2)THEN                                                          NUDOB.86
        DO 151 II=2,ILX-IGRID                                                    NUDOB.87
          IF(KPBLT(II).GT.KL.OR.KPBLT(II).LE.2)THEN                              NUDOB.88
            PRINT152,KPBLT(II),II,J                                              NUDOB.89
152   FORMAT(1X,'*** STOP152 IN NUDOB: KPBLT = ',I2,' AT I = ',I3,               NUDOB.90
     1' AND J = ',I3,' ***')                                                     NUDOB.91
            STOP 152                                                             NUDOB.92
          ENDIF                                                                  NUDOB.93
  151   CONTINUE                                                                 NUDOB.94
        DO 201 II=2,ILX-IGRID                                                    NUDOB.95
          DO 201 KK=KL,KPBLT(II),-1                                              NUDOB.96
            WPBL(II,KK)=0.0                                                      NUDOB.97
  201     CONTINUE                                                               NUDOB.98
                                                                                 NUDOB.99
        DO 202 II=2,ILX-IGRID                                                    NUDOB.100
          DO 202 KK=1,KPBLT(II)-2                                                NUDOB.101
            WPBL(II,KK)=1.0                                                      NUDOB.102
  202     CONTINUE                                                               NUDOB.103
                                                                                 NUDOB.104
        DO 203 II=2,ILX-IGRID                                                    NUDOB.105
          WPBL(II,KPBLT(II)-1)=0.1                                               NUDOB.106
  203   CONTINUE                                                                 NUDOB.107
      ELSE                                                                       NUDOB.108
        DO 204 KK=1,KL                                                           NUDOB.109
          DO 204 II=2,ILX-IGRID                                                  NUDOB.110
            WPBL(II,KK)=1.0                                                      NUDOB.111
  204     CONTINUE                                                               NUDOB.112
                                                                                 NUDOB.113
      ENDIF                                                                      NUDOB.114
C                                                                                NUDOB.115
      IF(J.EQ.10.AND.MOD(KTAU,INPF).EQ.0)THEN                                    NUDOB.116
        PRINT205,IN,IVAR,PTOP,WPBL(10,KL),WPBL(10,KL-1),WPBL(10,KL-2)            NUDOB.117
205   FORMAT(1X,'IN NUDOB: IN,IV,PTOP ',2(1X,I2),1X,F6.2,                        NUDOB.118
     1' WPBL(KL,KL-1,KL-2)',3(1X,F6.2))                                          NUDOB.119
        PRINT206,KPBLT(10),(WPBL(10,KK),KK=KL,1,-NPFV)                           NUDOB.120
206   FORMAT(1X,'IN NUDOB: KPBL,WPBL(KL,KL-NPFV...): ',                          NUDOB.121
     1I2,8(1X,F6.2))                                                             NUDOB.122
        PRINT207,RINXY*1000./DX,RINFMN,RINFMX,PFREE,DCON                         NUDOB.123
207   FORMAT(1X,'IN NUDOB: RINDX,RINFMN,RINFMX,PFREE,DCON: ',                    NUDOB.124
     15(1X,E11.3))                                                               NUDOB.125
      ENDIF                                                                      NUDOB.126
C                                                                                NUDOB.127
C INITIALIZE WEIGHTING ARRAYS TO ZERO                                            NUDOB.128
C                                                                                NUDOB.129
      DO 50 I=1,IL                                                               NUDOB.130
        DO 50 K=1,KL                                                             NUDOB.131
          WT(I,K)=1.0                                                            NUDOB.132
          WTP(I,K)=1.0                                                           NUDOB.133
          WT2ERR(I,K)=0.0                                                        NUDOB.134
          PWT2ER(I,K)=0.0                                                        NUDOB.135
   50   CONTINUE                                                                 NUDOB.136
                                                                                 NUDOB.137
C                                                                                NUDOB.138
C TRANSFORM THE HORIZONTAL RADIUS OF INFLUENCE, RINXY, FROM                      NUDOB.139
C KILOMETERS TO GRID LENGTHS, RINDX                                              NUDOB.140
C                                                                                NUDOB.141
      RINDX=RINXY*1000./DX                                                       NUDOB.142
      RIS=RINDX*RINDX                                                            NUDOB.143
      IF(IFREST.AND.KTAU.EQ.KTAUR)GOTO 5                                         NUDOB.144
      IF(MOD(KTAU,INFR).NE.0)GOTO 126                                            NUDOB.145
    5 CONTINUE                                                                   NUDOB.146
      IF(J.EQ.2)PRINT6,IN,J,KTAU,XTIME,IVAR,IPL                                  NUDOB.147
6     FORMAT(1X,'------COMPUTING OBS NUDGING FOR IN,J,KTAU,XTIME,',              NUDOB.148
     1'IVAR,IPL: ',I2,1X,I2,1X,I5,1X,F8.2,1X,I2,1X,I2)                           NUDOB.149
      IF(IN.NE.1)GOTO 20                                                         NUDOB.150
C FOR THE COARSE MESH...                                                         NUDOB.151
      DO 10 N=1,NSTA                                                             NUDOB.152
        RA(N)=RIO(N)-GRID                                                        NUDOB.153
        RB(N)=RJO(N)-GRID                                                        NUDOB.154
   10 CONTINUE                                                                   NUDOB.155
      GOTO 40                                                                    NUDOB.156
C FOR THE FINE MESH CASE...                                                      NUDOB.157
   20 DO 30 N=1,NSTA                                                             NUDOB.158
C COMPUTE THE OBS LOCATION WITH RESPECT TO THIS MESH (IN)...                     NUDOB.159
        NML=IN                                                                   NUDOB.160
        MM(LEVIDN(IN)+1)=IN                                                      NUDOB.161
C WORKING TOWARD COARSER MESHES, DETERMINE THE HIERARCHY OF MOTHER               NUDOB.162
C MESHES WITH RESPECT TO EACH MOTHER MESH STARTING AT MESH "IN"...               NUDOB.163
C THAT IS, DETERMINE ITS MOTHER, GRANDMOTHER, GREAT-GRANDMOTHER, ETC.            NUDOB.164
C OUT TO THE COARSE GRID MESH (IN=1).                                            NUDOB.165
C LEVIDN HOLDS THE NEST LEVEL AND NUMNC HOLDS THE MOTHER MESH FOR EACH           NUDOB.166
C GRID (E.G., FOR 3 MESHES AND 2 NEST LEVELS, IN=1 IS THE COARSE GRID            NUDOB.167
C MESH, IN=2 HAS LEVIDN(2)=1 AND NUMNC(2)=1, AND IN=3 HAS LEVIDN(3)=2            NUDOB.168
C AND NUMNC(3)=2...)                                                             NUDOB.169
        DO 21 NNL=LEVIDN(IN),1,-1                                                NUDOB.170
          MM(NNL)=NUMNC(NML)                                                     NUDOB.171
          NML=MM(NNL)                                                            NUDOB.172
   21   CONTINUE                                                                 NUDOB.173
C MM(1) MUST BE THE COARSE GRID MESH (IN=1)                                      NUDOB.174
        IF(MM(1).NE.1)STOP 21                                                    NUDOB.175
        RA(N)=RIO(N)                                                             NUDOB.176
        RB(N)=RJO(N)                                                             NUDOB.177
        DO 22 NNL=1,LEVIDN(IN)                                                   NUDOB.178
          GRFAC=0.                                                               NUDOB.179
C COMPUTE THE OBS LOCATION WITH RESPECT TO THE INNER GRID IN DOT-POINT           NUDOB.180
C SPACE (GRID=0.).  WHEN WE REACH MESH IN, THEN USE "GRID" TO GO TO              NUDOB.181
C CROSS OR DOT DEPENDING ON THE VARIABLE...                                      NUDOB.182
          IF(NNL.EQ.LEVIDN(IN))GRFAC=GRID                                        NUDOB.183
          RA(N)=(RA(N)-FLOAT(NESTI(MM(NNL+1))))*FLOAT(IRATIO)+1.-GRFAC           NUDOB.184
          RB(N)=(RB(N)-FLOAT(NESTJ(MM(NNL+1))))*FLOAT(IRATIO)+1.-GRFAC           NUDOB.185
   22   CONTINUE                                                                 NUDOB.186
   30 CONTINUE                                                                   NUDOB.187
   40 CONTINUE                                                                   NUDOB.188
C                                                                                NUDOB.189
C INITIALIZE WEIGHTING ARRAYS TO ZERO                                            NUDOB.190
C                                                                                NUDOB.191
      DO 45 I=1,IL                                                               NUDOB.192
        DO 45 K=1,KL                                                             NUDOB.193
          WT(I,K)=0.0                                                            NUDOB.194
          WTP(I,K)=0.0                                                           NUDOB.195
          WT2ERR(I,K)=0.0                                                        NUDOB.196
          PWT2ER(I,K)=0.0                                                        NUDOB.197
   45   CONTINUE                                                                 NUDOB.198
                                                                                 NUDOB.199
C                                                                                NUDOB.200
C DO P* COMPUTATIONS ON DOT POINTS FOR IVAR.LT.3 (U AND V)                       NUDOB.201
C AND CROSS POINTS FOR IVAR.GE.3 (T,Q,P*).                                       NUDOB.202
C                                                                                NUDOB.203
C COMPUTE P* AT OBS LOCATION (RA,RB).  DO THIS AS SEPARATE VECTOR LOOP H         NUDOB.204
C SO IT IS ALREADY AVAILABLE IN NSTA LOOP 120 BELOW                              NUDOB.205
C_FLIC_RUNPAD(2)                                                                 NUDOB.206
      IF(IVAR.LT.3)THEN                                                          NUDOB.207
C FOR WINDS, PSBD IS PSB ON DOT POINTS (RAINP FROM TEND)                         NUDOB.208
        DO 210 II=1,IL                                                           NUDOB.209
          DO 210 JJ=1,JL                                                         NUDOB.210
            PDOC(II,JJ)=PSBD(II,JJ)                                              NUDOB.211
  210     CONTINUE                                                               NUDOB.212
                                                                                 NUDOB.213
        IEND=IL                                                                  NUDOB.214
        JEND=JL                                                                  NUDOB.215
      ELSE                                                                       NUDOB.216
        DO 211 II=1,ILX                                                          NUDOB.217
          DO 211 JJ=1,JLX                                                        NUDOB.218
            PDOC(II,JJ)=PSB(II,JJ)                                               NUDOB.219
  211     CONTINUE                                                               NUDOB.220
                                                                                 NUDOB.221
        IEND=ILX                                                                 NUDOB.222
        JEND=JLX                                                                 NUDOB.223
      ENDIF                                                                      NUDOB.224
C_FLIC_RUNPAD(0)                                                                 NUDOB.225
      DO 220 N=1,NSTA                                                            NUDOB.226
        IA(N)=RA(N)                                                              NUDOB.227
        IB(N)=RB(N)                                                              NUDOB.228
        IOB=MAX0(1,IA(N))                                                        NUDOB.229
        IOB=MIN0(IOB,IEND-1)                                                     NUDOB.230
        JOB=MAX0(1,IB(N))                                                        NUDOB.231
        JOB=MIN0(JOB,JEND-1)                                                     NUDOB.232
        DXOB=RB(N)-FLOAT(IB(N))                                                  NUDOB.233
        DYOB=RA(N)-FLOAT(IA(N))                                                  NUDOB.234
#ifndef MPP1                                                                     NUDOB.235
C BILINEARLY INTERPOLATE PDOC (PSB ON DOT OR CROSS) TO THE OBS LOCATION          NUDOB.236
        PSBO(N)=((1.-DXOB)*((1.-DYOB)*PDOC(IOB,JOB)+DYOB*PDOC(IOB+1,JOB)         NUDOB.237
     +          )+DXOB*((1.-DYOB)*PDOC(IOB,JOB+1)+DYOB*PDOC(IOB+1,JOB+1)         NUDOB.238
     +          ))                                                               NUDOB.239
#else                                                                            NUDOB.240
C PSB IS NOT AVAILABLE GLOBALLY, THEREFORE, THE BILINEAR INTERPOLATION           NUDOB.241
C AROUND THE OBS POINT IS DONE IN ERROB() AND STORED IN ERRF([78],N) FOR         NUDOB.242
C THE POINT (7 = CROSS, 8=DOT).                                                  NUDOB.243
        IF(IVAR.LT.3)THEN                                                        NUDOB.244
          PSBO(N)=ERRF(8,N)                                                      NUDOB.245
        ELSE                                                                     NUDOB.246
          PSBO(N)=ERRF(7,N)                                                      NUDOB.247
        ENDIF                                                                    NUDOB.248
#endif                                                                           NUDOB.249
  220 CONTINUE                                                                   NUDOB.250
C                                                                                NUDOB.251
C DETERMINE THE LIMITS OF THE SEARCH REGION FOR THE CURRENT                      NUDOB.252
C J-STRIP                                                                        NUDOB.253
C                                                                                NUDOB.254
#ifndef MPP1                                                                     NUDOB.255
      MAXJ=J+IFIX(RINDX*RINFMX+0.99)                                             NUDOB.256
      MAXJ=MIN0(JL-IGRID,MAXJ)                                                   NUDOB.257
      MINJ=J-IFIX(RINDX*RINFMX+0.99)                                             NUDOB.258
      MINJ=MAX0(1,MINJ)                                                          NUDOB.259
#else                                                                            NUDOB.260
      MAXJ=FLIC_L2G_N(J)+IFIX(RINDX*RINFMX+0.99)                                 NUDOB.261
      MAXJ=MIN0(JL-IGRID,MAXJ)                                                   NUDOB.262
      MINJ=FLIC_L2G_N(J)-IFIX(RINDX*RINFMX+0.99)                                 NUDOB.263
      MINJ=MAX0(1,MINJ)                                                          NUDOB.264
#endif                                                                           NUDOB.265
C BEGIN OUTER LOOP FOR THE NSTA OBSERVATIONS                                     NUDOB.266
      DO 120 N=1,NSTA                                                            NUDOB.267
C CHECK TO SEE OF STATION N HAS DATA FOR VARIABLE IVAR                           NUDOB.268
C AND IF IT IS SUFFICIENTLY CLOSE TO THE J STRIP.  THIS                          NUDOB.269
C SHOULD ELIMINATE MOST STATIONS FROM FURTHER CONSIDER-                          NUDOB.270
C ATION.                                                                         NUDOB.271
#ifndef MPP1                                                                     NUDOB.272
        IF(MOD(KTAU,INPF).EQ.0.AND.J.EQ.3)PRINT1453,N,IN,MAXJ,MINJ,RA(N)         NUDOB.273
     +    ,RB(N),RKO(N),ERRF(IVAR,N),VAROBS(IVAR,N),IVAR                         NUDOB.274
1453  FORMAT(1X,'N NSTA,IN,MAXJ,MINJ,RA,RB,RK,ERRF,VAR,IVAR',I4,I2,              NUDOB.275
     12I3,3(1X,F6.2),2(1X,E11.4),I2)                                             NUDOB.276
#else                                                                            NUDOB.277
        IF(MOD(KTAU,INPF).EQ.0.AND.NOFLIC(J).EQ.8)PRINT1453,N,IN,MAXJ,           NUDOB.278
     +    MINJ,RA(N),RB(N),RKO(N),ERRF(IVAR,N),PSBO(N),VAROBS(IVAR,N),           NUDOB.279
     +    IVAR                                                                   NUDOB.280
1453  FORMAT(1X,'N NSTA,IN,MAXJ,MINJ,RA,RB,RK,ERRF,P,VAR,IVAR',I4,I2,            NUDOB.281
     12I3,3(1X,F6.2),3(1X,E11.4),I2)                                             NUDOB.282
#endif                                                                           NUDOB.283
        IF(ABS(ERRF(IVAR,N)).GT.9.E4)GOTO 120                                    NUDOB.284
        IF(RB(N).LT.FLOAT(MINJ).OR.RB(N).GT.FLOAT(MAXJ))GOTO 120                 NUDOB.285
C DETERMINE THE TIME-WEIGHT FACTOR FOR N                                         NUDOB.286
        FDTIM=XTIME-DTMIN                                                        NUDOB.287
C TWINDO IS IN MINUTES:                                                          NUDOB.288
        TW1=TWINDO/2.*60.                                                        NUDOB.289
        TW2=TWINDO*60.                                                           NUDOB.290
        TCONST=1./TW1                                                            NUDOB.291
        TIMEWT=0.0                                                               NUDOB.292
        TTIM=TIMEOB(N)*60.                                                       NUDOB.293
C***********TTIM=TARGET TIME IN MINUTES                                          NUDOB.294
        DIFT=ABS(FDTIM-TTIM)                                                     NUDOB.295
        IF(DIFT.LE.TW1)TIMEWT=1.0                                                NUDOB.296
        IF(DIFT.GT.TW1.AND.DIFT.LE.TW2)GOTO 11                                   NUDOB.297
        GOTO 51                                                                  NUDOB.298
   11   CONTINUE                                                                 NUDOB.299
        IF(FDTIM.LT.TTIM)TIMEWT=(FDTIM-(TTIM-TW2))*TCONST                        NUDOB.300
        IF(FDTIM.GT.TTIM)TIMEWT=((TTIM+TW2)-FDTIM)*TCONST                        NUDOB.301
   51   CONTINUE                                                                 NUDOB.302
C DETERMINE THE LIMITS OF APPLICATION OF THE OBS IN THE VERTICAL                 NUDOB.303
C FOR THE VERTICAL WEIGHTING, WTSIG                                              NUDOB.304
C                                                                                NUDOB.305
        IF(IWTSIG(IN).EQ.1)THEN                                                  NUDOB.306
          STOP 20                                                                NUDOB.307
C FIND LEVEL INDEX CLOSEST TO THE OBS, KCLOSE                                    NUDOB.308
          KCLOSE=IFIX(RKO(N)+0.5)                                                NUDOB.309
          KCLOSE=MIN0(KL,KCLOSE)                                                 NUDOB.310
C FIND SIGMA EQUIVALENT TO RKO(N)                                                NUDOB.311
          KM=IFIX(RKO(N))                                                        NUDOB.312
          KM=MIN0(KL-1,KM)                                                       NUDOB.313
          SIGOB=(RKO(N)-FLOAT(KM))*(A(KM+1)-A(KM))+A(KM)                         NUDOB.314
C DEFINE THE LIMITS OF THE SIGMA WINDOW ABOUT SIGOB                              NUDOB.315
          SIGOMX=SIGOB+RINSIG                                                    NUDOB.316
          SIGOMN=SIGOB-RINSIG                                                    NUDOB.317
C FIND MINIMUM INDEX CALCULATION LEVEL AND MAXIMUM INDEX                         NUDOB.318
C CALCULATION LEVEL WITHIN A SIGMA-WINDOW.                                       NUDOB.319
          KOMIN=1                                                                NUDOB.320
          DO 60 K=1,KL                                                           NUDOB.321
            IF(A(K).GT.SIGOMN)GOTO 65                                            NUDOB.322
            KOMIN=K+1                                                            NUDOB.323
   60     CONTINUE                                                               NUDOB.324
   65     CONTINUE                                                               NUDOB.325
          KOMIN=MIN0(KCLOSE,KOMIN)                                               NUDOB.326
          KOMAX=KOMIN                                                            NUDOB.327
          DO 70 K=KOMIN,KL                                                       NUDOB.328
            IF(A(K).GT.SIGOMX)GOTO 75                                            NUDOB.329
            KOMAX=K                                                              NUDOB.330
   70     CONTINUE                                                               NUDOB.331
   75     CONTINUE                                                               NUDOB.332
          KOMAX=MAX0(KCLOSE,KOMAX)                                               NUDOB.333
          KOMAX=MIN0(KL,KOMAX)                                                   NUDOB.334
C CALCULATE VERTICAL WEIGHTING FACTORS, WTSIG(K)                                 NUDOB.335
          DO 80 K=1,KL                                                           NUDOB.336
            WTSIG(K)=0.0                                                         NUDOB.337
   80     CONTINUE                                                               NUDOB.338
          DO 90 K=KOMIN,KOMAX                                                    NUDOB.339
            WTSIG(K)=1.0-ABS(SIGOB-A(K))/RINSIG                                  NUDOB.340
   90     WTSIG(K)=AMAX1(0.0,WTSIG(K))                                           NUDOB.341
        ELSE                                                                     NUDOB.342
C ASSIMILATE OBSERVATIONS ON PRESSURE LEVELS, EXCEPT FOR SURFACE                 NUDOB.343
          KOB=IFIX(RKO(N)+0.5)                                                   NUDOB.344
          KOB=MIN0(KL,KOB)                                                       NUDOB.345
          KOB=MAX0(1,KOB)                                                        NUDOB.346
C                                                                                NUDOB.347
C DO NOT PROCESS SURFACE-LAYER TEMPERATURE OR MOISTURE DATA                      NUDOB.348
C      (WPBL WEIGHTING WILL ZERO IT OUT LATER ANYWAY)                            NUDOB.349
          IF(KOB.EQ.KL.AND.IVAR.GT.2)GOTO 120                                    NUDOB.350
C                                                                                NUDOB.351
C ASSIMILATE SURFACE LAYER WIND DATA ON SIGMA                                    NUDOB.352
C NOTE: WPBL DOES NOT ZERO OUT SURFACE-LAYER WIND DATA                           NUDOB.353
          IF(KOB.EQ.KL.AND.IVAR.LE.2)THEN                                        NUDOB.354
            DO 280 K=1,KL                                                        NUDOB.355
              WTSIG(K)=0.0                                                       NUDOB.356
  280       CONTINUE                                                             NUDOB.357
C DEFINE WTSIG: (FOR SRP: SPREAD SURFACE DATA THROUGH LOWEST 200 M)              NUDOB.358
            WTSIG(KL)=1.0                                                        NUDOB.359
            WTSIG(KL-1)=0.67                                                     NUDOB.360
            WTSIG(KL-2)=0.33                                                     NUDOB.361
            KOMIN=KL-2                                                           NUDOB.362
            KOMAX=KL                                                             NUDOB.363
C DEFINE THE MAX AND MIN I VALUES FOR POSSIBLE NONZERO                           NUDOB.364
C WEIGHTS, BASED ON THE RADIUS OF INFLUENCE, RINDX (IN GRID LENGTHS).            NUDOB.365
            MAXI=IFIX(RA(N)+0.99+RINDX)                                          NUDOB.366
            MAXI=MIN0(IL-IGRID,MAXI)                                             NUDOB.367
            MINI=IFIX(RA(N)-RINDX-0.99)                                          NUDOB.368
            MINI=MAX0(1,MINI)                                                    NUDOB.369
C                                                                                NUDOB.370
C LOOP THROUGH THE NECESSARY GRID POINTS SURROUNDING                             NUDOB.371
C OBSERVATION N.  COMPUTE THE HORIZONTAL DISTANCE TO                             NUDOB.372
C THE OBS AND FIND THE WEIGHTING SUM OVER ALL OBS                                NUDOB.373
#ifndef MPP1                                                                     NUDOB.374
            RJ=FLOAT(J)                                                          NUDOB.375
#else                                                                            NUDOB.376
            RJ=FLOAT(FLIC_L2G_N(J))                                              NUDOB.377
#endif                                                                           NUDOB.378
            RX=RJ-RB(N)                                                          NUDOB.379
C WEIGHTS FOR THE 3-D VARIABLES                                                  NUDOB.380
            IF(ERRF(5,N)-9.E4.GE.0)THEN                                          NUDOB.381
              ERRF(5,N)=0.0                                                      NUDOB.382
            ENDIF                                                                NUDOB.383
            IF(ERRF(6,N)-9.E4.GE.0)THEN                                          NUDOB.384
              ERRF(6,N)=0.0                                                      NUDOB.385
            ENDIF                                                                NUDOB.386
            IF(2-IVAR.LT.0)THEN                                                  NUDOB.387
              PERRF=ERRF(5,N)                                                    NUDOB.388
            ELSE                                                                 NUDOB.389
              PERRF=ERRF(6,N)                                                    NUDOB.390
            ENDIF                                                                NUDOB.391
            IF(PERRF.NE.0)THEN                                                   NUDOB.392
              PWTFAC=1.0                                                         NUDOB.393
            ELSE                                                                 NUDOB.394
              PWTFAC=0.0                                                         NUDOB.395
            ENDIF                                                                NUDOB.396
            ERFIVR=ERRF(IVAR,N)                                                  NUDOB.397
C                                                                                NUDOB.398
            DO 230 K=KOMIN,KOMAX                                                 NUDOB.399
CJM I will be local, because it indexes into PDOC, WT, and others                NUDOB.400
              DO 230 I=MINI,MAXI                                                 NUDOB.401
#ifndef MPP1                                                                     NUDOB.402
                RI=FLOAT(I)                                                      NUDOB.403
#else                                                                            NUDOB.404
                RI=FLOAT(FLIC_L2G_M(I))                                          NUDOB.405
#endif                                                                           NUDOB.406
                RY=RI-RA(N)                                                      NUDOB.407
                RIS=RINDX*RINDX                                                  NUDOB.408
                RSQ=RX*RX+RY*RY                                                  NUDOB.409
                DPRIM=SQRT(RSQ)                                                  NUDOB.410
C THIS FUNCTION DECREASES WTIJ AS PSFC CHANGES WITHIN SEARCH RADIUS              NUDOB.411
                D=DPRIM+RINDX*DCON*ABS(PSBO(N)-PDOC(I,J))                        NUDOB.412
                DSQ=D*D                                                          NUDOB.413
                WTIJ=(RIS-DSQ)/(RIS+DSQ)                                         NUDOB.414
                WTIJ=AMAX1(0.0,WTIJ)                                             NUDOB.415
                WT(I,K)=WT(I,K)+TIMEWT*WTSIG(K)*WTIJ                             NUDOB.416
                WTP(I,K)=WTP(I,K)+TIMEWT*WTSIG(K)*WTIJ*PWTFAC                    NUDOB.417
                WT2ERR(I,K)=WT2ERR(I,K)+TIMEWT*TIMEWT*WTIJ*WTIJ*WTSIG(K)         NUDOB.418
     +                      *WTSIG(K)*ERFIVR                                     NUDOB.419
                PWT2ER(I,K)=PWT2ER(I,K)+TIMEWT*TIMEWT*WTIJ*WTIJ*PERRF            NUDOB.420
C PWT2ER IS BASED ON THE DIFFERENCE OF THE OBS AND FCST P*                       NUDOB.421
C IN ERRF(6,N)                                                                   NUDOB.422
  230         CONTINUE                                                           NUDOB.423
                                                                                 NUDOB.424
            GOTO 120                                                             NUDOB.425
          ENDIF                                                                  NUDOB.426
C END SURFACE-LAYER U OR V OBS NUDGING                                           NUDOB.427
C                                                                                NUDOB.428
C BEGIN CALCULATIONS TO SPREAD OBS INFLUENCE ALONG PRESSURE LEVELS               NUDOB.429
          SIGOB=A(KOB)                                                           NUDOB.430
C COMPUTE THE PRESSURE OF THE OBSERVATION                                        NUDOB.431
          POB=SIGOB*PSBO(N)+PTOP                                                 NUDOB.432
C DEFINE THE MAX AND MIN I VALUES FOR POSSIBLE NONZERO                           NUDOB.433
C WEIGHTS, BASED ON THE RADIUS OF INFLUENCE, RINDX, AND RINFAC.                  NUDOB.434
C RINFAC VARIES AS A LINEAR FUNCTION FROM FROM RINFMN AT P*+PTOP                 NUDOB.435
C TO RINFMX AT PFREE AND "ABOVE" (LOWER PRESSURE).                               NUDOB.436
          SLOPE=(RINFMN-RINFMX)/(PSBO(N)+PTOP-PFREE)                             NUDOB.437
          RINFAC=SLOPE*POB+RINFMX-SLOPE*PFREE                                    NUDOB.438
          RINFAC=AMAX1(RINFAC,RINFMN)                                            NUDOB.439
          RINFAC=AMIN1(RINFAC,RINFMX)                                            NUDOB.440
C                                                                                NUDOB.441
          MAXI=IFIX(RA(N)+0.99+RINDX*RINFAC)                                     NUDOB.442
          MAXI=MIN0(IL-IGRID,MAXI)                                               NUDOB.443
          MINI=IFIX(RA(N)-RINDX*RINFAC-0.99)                                     NUDOB.444
          MINI=MAX0(1,MINI)                                                      NUDOB.445
C                                                                                NUDOB.446
C LOOP THROUGH THE NECESSARY GRID POINTS SURROUNDING                             NUDOB.447
C OBSERVATION N.  COMPUTE THE HORIZONTAL DISTANCE TO                             NUDOB.448
C THE OBS AND FIND THE WEIGHTING SUM OVER ALL OBS                                NUDOB.449
#ifndef MPP1                                                                     NUDOB.450
          RJ=FLOAT(J)                                                            NUDOB.451
#else                                                                            NUDOB.452
          RJ=FLOAT(FLIC_L2G_N(J))                                                NUDOB.453
#endif                                                                           NUDOB.454
          RX=RJ-RB(N)                                                            NUDOB.455
C WEIGHTS FOR THE 3-D VARIABLES                                                  NUDOB.456
          IF(ERRF(5,N)-9.E4.GE.0)THEN                                            NUDOB.457
            ERRF(5,N)=0.0                                                        NUDOB.458
          ENDIF                                                                  NUDOB.459
          IF(ERRF(6,N)-9.E4.GE.0)THEN                                            NUDOB.460
            ERRF(6,N)=0.0                                                        NUDOB.461
          ENDIF                                                                  NUDOB.462
          IF(2-IVAR.LT.0)THEN                                                    NUDOB.463
            PERRF=ERRF(5,N)                                                      NUDOB.464
          ELSE                                                                   NUDOB.465
            PERRF=ERRF(6,N)                                                      NUDOB.466
          ENDIF                                                                  NUDOB.467
          IF(PERRF.NE.0)THEN                                                     NUDOB.468
            PWTFAC=1.0                                                           NUDOB.469
          ELSE                                                                   NUDOB.470
            PWTFAC=0.0                                                           NUDOB.471
          ENDIF                                                                  NUDOB.472
          ERFIVR=ERRF(IVAR,N)                                                    NUDOB.473
          DO 330 I=MINI,MAXI                                                     NUDOB.474
C DEFINE KPRS TO BE THAT SIGMA-LAYER AT I,J WHOSE PRESSURE IS CLOSEST TO         NUDOB.475
            SIGPRS=(POB-PTOP)/PDOC(I,J)                                          NUDOB.476
C DO NOT ASSIMILATE OBSERVATION IF ABOVE HIGHEST (IN HEIGHT) MODEL LAYER         NUDOB.477
            IF(SIGPRS.LT.A(1))GOTO 330                                           NUDOB.478
            IF(SIGPRS.GE.A(KL))THEN                                              NUDOB.479
C DO NOT ASSIMILATE OBSERVATION IF BELOW GROUND                                  NUDOB.480
              IF(SIGPRS.GT.1.0)GOTO 330                                          NUDOB.481
C OBSERVATION IS WITHIN SURFACE LAYER                                            NUDOB.482
              KPRS=KL                                                            NUDOB.483
              GOTO 313                                                           NUDOB.484
            ENDIF                                                                NUDOB.485
C DETERMINE KPRS AT THIS I,J                                                     NUDOB.486
            DSIG=SIGPRS-A(KOB)                                                   NUDOB.487
            IF(DSIG.LT.0.)THEN                                                   NUDOB.488
C SIGPRS IS LOWER THAN SIGOB A(KOB)                                              NUDOB.489
              KKL=KOB                                                            NUDOB.490
              IFOUND=0                                                           NUDOB.491
              DO 305 KKL=KOB,2,-1                                                NUDOB.492
                IF(SIGPRS.LT.A(KKL).AND.SIGPRS.GE.A(KKL-1))THEN                  NUDOB.493
                  IFOUND=1                                                       NUDOB.494
                  KHI=KKL                                                        NUDOB.495
                  KLO=KKL-1                                                      NUDOB.496
                  GOTO 306                                                       NUDOB.497
                ENDIF                                                            NUDOB.498
  305         CONTINUE                                                           NUDOB.499
            ELSE                                                                 NUDOB.500
              IFOUND=0                                                           NUDOB.501
              DO 307 KKL=KOB,KL-1,1                                              NUDOB.502
                IF(SIGPRS.GE.A(KKL).AND.SIGPRS.LT.A(KKL+1))THEN                  NUDOB.503
                  IFOUND=1                                                       NUDOB.504
                  KHI=KKL+1                                                      NUDOB.505
                  KLO=KKL                                                        NUDOB.506
                  GOTO 306                                                       NUDOB.507
                ENDIF                                                            NUDOB.508
  307         CONTINUE                                                           NUDOB.509
            ENDIF                                                                NUDOB.510
C                                                                                NUDOB.511
  306       CONTINUE                                                             NUDOB.512
            IF(IFOUND.NE.1)THEN                                                  NUDOB.513
              PRINT308,KOB,A(KOB),POB,SIGPRS,N                                   NUDOB.514
308   FORMAT(1X,'+-+- NOT FOUND WARNING: KOB,SIGOB,POB,SIGPRS,NSTA: ',           NUDOB.515
     1I2,3(1X,F10.4),1X,I5,' +-+-')                                              NUDOB.516
              STOP 111                                                           NUDOB.517
              GOTO 330                                                           NUDOB.518
            ENDIF                                                                NUDOB.519
C DEFINE KPRS TO BE THE CLOSEST K LAYER                                          NUDOB.520
            DA=A(KHI)-A(KLO)                                                     NUDOB.521
            KPRS=KLO                                                             NUDOB.522
            IF(SIGPRS.GT.A(KLO)+0.5*DA)KPRS=KHI                                  NUDOB.523
  313       CONTINUE                                                             NUDOB.524
C KPRS IS KNOWN AT THIS POINT                                                    NUDOB.525
            IF(KPRS.GT.KL.OR.KPRS.LT.1)THEN                                      NUDOB.526
              PRINT314,KPRS,SIGPRS,KHI,KLO,KOB,A(KOB),POB,N                      NUDOB.527
314   FORMAT(1X,'+-+- BAD KPRS WARNING: KPRS,SIGPRS,KHI,KLO: ',I2,1X,            NUDOB.528
     1F10.4,1X,I2,1X,I2,' KOB,SIGOB,POB,NSTA: ',I2,2(1X,F10.4),1X,I5)            NUDOB.529
C     STOP222                                                                    NUDOB.530
              GOTO 330                                                           NUDOB.531
            ENDIF                                                                NUDOB.532
#ifndef MPP1                                                                     NUDOB.533
            RI=FLOAT(I)                                                          NUDOB.534
#else                                                                            NUDOB.535
            RI=FLOAT(FLIC_L2G_M(I))                                              NUDOB.536
#endif                                                                           NUDOB.537
            RY=RI-RA(N)                                                          NUDOB.538
            RIS=RINDX*RINFAC*RINDX*RINFAC                                        NUDOB.539
            RSQ=RX*RX+RY*RY                                                      NUDOB.540
            WTIJ=(RIS-RSQ)/(RIS+RSQ)                                             NUDOB.541
            WTIJ=AMAX1(0.0,WTIJ)                                                 NUDOB.542
C THE VERTICAL WEIGHT IS SET TO UNITY IN THE FOLLOWING 4 EQS...                  NUDOB.543
            WT(I,KPRS)=WT(I,KPRS)+TIMEWT*WTIJ                                    NUDOB.544
            WTP(I,KPRS)=WTP(I,KPRS)+TIMEWT*WTIJ*PWTFAC                           NUDOB.545
            WT2ERR(I,KPRS)=WT2ERR(I,KPRS)+TIMEWT*TIMEWT*WTIJ*WTIJ*ERFIVR         NUDOB.546
            PWT2ER(I,KPRS)=PWT2ER(I,KPRS)+TIMEWT*TIMEWT*WTIJ*WTIJ*PERRF          NUDOB.547
C PWT2ER IS BASED ON THE DIFFERENCE OF THE OBS AND FCST P*                       NUDOB.548
C IN ERRF(6,N)                                                                   NUDOB.549
C                                                                                NUDOB.550
C END IF MINI,MAXI LOOP                                                          NUDOB.551
  330     CONTINUE                                                               NUDOB.552
          GOTO 120                                                               NUDOB.553
        ENDIF                                                                    NUDOB.554
C END OF NUDGING TO OBS ON PRESSURE LEVELS                                       NUDOB.555
  100   CONTINUE                                                                 NUDOB.556
C DEFINE THE MAX AND MIN I VALUES FOR POSSIBLE NONZERO                           NUDOB.557
C WEIGHTS, BASED ON THE RADIUS OF INFLUENCE, RINDX.                              NUDOB.558
        MAXI=IFIX(RA(N)+0.99+RINDX)                                              NUDOB.559
        MAXI=MIN0(IL-IGRID,MAXI)                                                 NUDOB.560
        MINI=IFIX(RA(N)-RINDX-0.99)                                              NUDOB.561
        MINI=MAX0(1,MINI)                                                        NUDOB.562
C                                                                                NUDOB.563
C LOOP THROUGH THE NECESSARY GRID POINTS SURROUNDING                             NUDOB.564
C OBSERVATION N.  COMPUTE THE HORIZONTAL DISTANCE TO                             NUDOB.565
C THE OBS AND FIND THE WEIGHTING SUM OVER ALL OBS                                NUDOB.566
#ifndef MPP1                                                                     NUDOB.567
        RJ=FLOAT(J)                                                              NUDOB.568
#else                                                                            NUDOB.569
        RJ=FLOAT(FLIC_L2G_N(J))                                                  NUDOB.570
#endif                                                                           NUDOB.571
        RX=RJ-RB(N)                                                              NUDOB.572
C WEIGHTS FOR THE 3-D VARIABLES                                                  NUDOB.573
        IF(ERRF(5,N)-9.E4.GE.0)THEN                                              NUDOB.574
          ERRF(5,N)=0.0                                                          NUDOB.575
        ENDIF                                                                    NUDOB.576
        IF(ERRF(6,N)-9.E4.GE.0)THEN                                              NUDOB.577
          ERRF(6,N)=0.0                                                          NUDOB.578
        ENDIF                                                                    NUDOB.579
        IF(2-IVAR.LT.0)THEN                                                      NUDOB.580
          PERRF=ERRF(5,N)                                                        NUDOB.581
        ELSE                                                                     NUDOB.582
          PERRF=ERRF(6,N)                                                        NUDOB.583
        ENDIF                                                                    NUDOB.584
        IF(PERRF.NE.0)THEN                                                       NUDOB.585
          PWTFAC=1.0                                                             NUDOB.586
        ELSE                                                                     NUDOB.587
          PWTFAC=0.0                                                             NUDOB.588
        ENDIF                                                                    NUDOB.589
        ERFIVR=ERRF(IVAR,N)                                                      NUDOB.590
        DO 130 K=KOMIN,KOMAX                                                     NUDOB.591
          DO 130 I=MINI,MAXI                                                     NUDOB.592
#ifndef MPP1                                                                     NUDOB.593
            RI=FLOAT(I)                                                          NUDOB.594
#else                                                                            NUDOB.595
            RI=FLOAT(FLIC_L2G_M(I))                                              NUDOB.596
#endif                                                                           NUDOB.597
            RY=RI-RA(N)                                                          NUDOB.598
            RSQ=RX*RX+RY*RY                                                      NUDOB.599
            WTIJ=(RIS-RSQ)/(RIS+RSQ)                                             NUDOB.600
            WTIJ=AMAX1(0.0,WTIJ)                                                 NUDOB.601
            WT(I,K)=WT(I,K)+TIMEWT*WTSIG(K)*WTIJ                                 NUDOB.602
            WTP(I,K)=WTP(I,K)+TIMEWT*WTSIG(K)*WTIJ*PWTFAC                        NUDOB.603
            WT2ERR(I,K)=WT2ERR(I,K)+TIMEWT*TIMEWT*WTIJ*WTIJ*WTSIG(K)*            NUDOB.604
     +                  WTSIG(K)*ERFIVR                                          NUDOB.605
            PWT2ER(I,K)=PWT2ER(I,K)+TIMEWT*TIMEWT*WTIJ*WTIJ*PERRF                NUDOB.606
C PWT2ER IS BASED ON THE DIFFERENCE OF THE OBS AND FCST P*                       NUDOB.607
C IN ERRF(6,N)                                                                   NUDOB.608
  130     CONTINUE                                                               NUDOB.609
                                                                                 NUDOB.610
        GOTO 120                                                                 NUDOB.611
C                                                                                NUDOB.612
  140   CONTINUE                                                                 NUDOB.613
C WEIGHTS FOR 2-D VARIABLE PRESSURE                                              NUDOB.614
        DO 150 I=MINI,MAXI                                                       NUDOB.615
#ifndef MPP1                                                                     NUDOB.616
          RI=FLOAT(I)                                                            NUDOB.617
#else                                                                            NUDOB.618
          RI=FLOAT(FLIC_L2G_M(I))                                                NUDOB.619
#endif                                                                           NUDOB.620
          RY=RI-RA(N)                                                            NUDOB.621
          RSQ=RX*RX+RY*RY                                                        NUDOB.622
          WTIJ=(RIS-RSQ)/(RIS+RSQ)                                               NUDOB.623
          WTIJ=AMAX1(0.0,WTIJ)                                                   NUDOB.624
          WT(I,KL)=WT(I,KL)+TIMEWT*WTIJ                                          NUDOB.625
          WT2ERR(I,KL)=WT2ERR(I,KL)+TIMEWT*TIMEWT*WTIJ*WTIJ*ERRF(IVAR,N)         NUDOB.626
  150   CONTINUE                                                                 NUDOB.627
  120 CONTINUE                                                                   NUDOB.628
C                                                                                NUDOB.629
C WEIGHTS AND WEIGHTED DIFFERENCES HAVE BEEN SUMMED.  NOW                        NUDOB.630
C APPLY THE NUDGING FACTOR AND THE RESULTANT TENDENCY TO                         NUDOB.631
C THE ATEN ARRAY                                                                 NUDOB.632
C ASSURE THAT WT(I,K) AND WTP(I,K) ARE NONZERO SINCE                             NUDOB.633
C THEY ARE USED BELOW IN THE DENOMINATOR.                                        NUDOB.634
      DO 125 K=1,KL                                                              NUDOB.635
        DO 125 I=1,IL                                                            NUDOB.636
          IF(WT(I,K).EQ.0)THEN                                                   NUDOB.637
            WT(I,K)=1.0                                                          NUDOB.638
          ENDIF                                                                  NUDOB.639
          IF(WTP(I,K).EQ.0)THEN                                                  NUDOB.640
            WTP(I,K)=1.0                                                         NUDOB.641
          ENDIF                                                                  NUDOB.642
          IF(WT(I,K).EQ.0)THEN                                                   NUDOB.643
            WT2ERR(I,K)=0.0                                                      NUDOB.644
          ENDIF                                                                  NUDOB.645
          IF(WTP(I,K).EQ.0)THEN                                                  NUDOB.646
            PWT2ER(I,K)=0.0                                                      NUDOB.647
          ENDIF                                                                  NUDOB.648
  125   CONTINUE                                                                 NUDOB.649
                                                                                 NUDOB.650
  126 CONTINUE                                                                   NUDOB.651
C                                                                                NUDOB.652
      IF(IVAR.GE.3)GOTO 170                                                      NUDOB.653
C 3-D DOT POINT TENDENCIES                                                       NUDOB.654
      DO 165 K=1,KL                                                              NUDOB.655
        DO 165 I=2+ICUT,ILX-ICUT                                                 NUDOB.656
          IF(1-IVAR.LT.0)THEN                                                    NUDOB.657
            UBORVB=VB(I,J,K)                                                     NUDOB.658
          ELSE                                                                   NUDOB.659
            UBORVB=UB(I,J,K)                                                     NUDOB.660
          ENDIF                                                                  NUDOB.661
          PSD=0.25*(PSB(I,J)+PSB(I,J-1)+PSB(I-1,J)+PSB(I-1,J-1))                 NUDOB.662
          IF(MOD(KTAU,INFR).EQ.0.OR.(IFREST.AND.KTAU.EQ.KTAUR))THEN              NUDOB.663
            W2EOWT=WT2ERR(I,K)/WT(I,K)                                           NUDOB.664
          ELSE                                                                   NUDOB.665
            W2EOWT=SAVWT(IPL,I,J,K)                                              NUDOB.666
          ENDIF                                                                  NUDOB.667
C     W2EOWT=CVMGT(WT2ERR(I,K)/WT(I,K),SAVWT(IPL,I,J,K),                         NUDOB.668
C    1MOD(KTAU,INFR).EQ.0.OR.(IFREST.AND.KTAU.EQ.KTAUR))                         NUDOB.669
          ATEN(I,J,K)=ATEN(I,J,K)+GIV(IN)*PSD*W2EOWT*WPBL(I,K)*TFACI(IN)         NUDOB.670
     +                *ISWIND(IN)+GIP(IN)*UBORVB/PSD*PWT2ER(I,K)/                NUDOB.671
     +                WTP(I,K)*TFACI(IN)*ISPSTR(IN)                              NUDOB.672
  165   CONTINUE                                                                 NUDOB.673
                                                                                 NUDOB.674
      IF(MOD(KTAU,INFR).EQ.0.OR.(IFREST.AND.KTAU.EQ.KTAUR))THEN                  NUDOB.675
        DO 166 K=1,KL                                                            NUDOB.676
          DO 166 I=1,IL                                                          NUDOB.677
            SAVWT(IPL,I,J,K)=WT2ERR(I,K)/WT(I,K)                                 NUDOB.678
  166     CONTINUE                                                               NUDOB.679
                                                                                 NUDOB.680
      ENDIF                                                                      NUDOB.681
      IF(IVAR.NE.1)RETURN                                                        NUDOB.682
#ifndef MPP1                                                                     NUDOB.683
      IF(MOD(J,15).NE.0)RETURN                                                   NUDOB.684
      INPRT=0                                                                    NUDOB.685
      IF(MOD(KTAU,INPF).EQ.0)INPRT=1                                             NUDOB.686
      IF(INPRT.EQ.0)RETURN                                                       NUDOB.687
      DO 650 K=1,KL                                                              NUDOB.688
        DO 650 I=1,IL                                                            NUDOB.689
          SCRPR(K,I)=WT(I,K)                                                     NUDOB.690
  650   CONTINUE                                                                 NUDOB.691
                                                                                 NUDOB.692
      IF(IVAR.EQ.1)PRINT666,J                                                    NUDOB.693
666   FORMAT(1H0,'WT(I,K) FOR J = ',I2)                                          NUDOB.694
      WRITE (NAME,FMT=761)                                                       NUDOB.695
C      ENCODE(40,761,NAME)                                                       NUDOB.696
761   FORMAT(' OBS FDDA WEIGHTING (N-S SLICE) FOR WIND')                         NUDOB.697
      CALL MAPSMP(SCRPR,MKX,MIX,1,KL,1,1,IL,1,0.,-1,NAME,XTIME)                  NUDOB.698
      DO 660 K=1,KL                                                              NUDOB.699
        DO 660 I=1,IL                                                            NUDOB.700
          SCRPR(K,I)=WT2ERR(I,K)                                                 NUDOB.701
  660   CONTINUE                                                                 NUDOB.702
                                                                                 NUDOB.703
      PRINT661,J                                                                 NUDOB.704
661   FORMAT(1H0,'WT2ERR(I,K) FOR J = ',I2)                                      NUDOB.705
      CALL MAPSMP(SCRPR,MKX,MIX,1,KL,1,1,IL,1,0.,-1,NAME,XTIME)                  NUDOB.706
      RETURN                                                                     NUDOB.707
      DO 680 K=1,KL                                                              NUDOB.708
        DO 680 I=1,IL                                                            NUDOB.709
          SCRPR(K,I)=WTP(I,K)                                                    NUDOB.710
  680   CONTINUE                                                                 NUDOB.711
                                                                                 NUDOB.712
      PRINT662,J                                                                 NUDOB.713
662   FORMAT(1H0,'WTP(I,K) FOR J = ',I2)                                         NUDOB.714
      CALL MAPSMP(SCRPR,MKX,MIX,1,KL,1,1,IL,1,0.,-1,NAME,XTIME)                  NUDOB.715
      DO 690 K=1,KL                                                              NUDOB.716
        DO 690 I=1,IL                                                            NUDOB.717
          SCRPR(K,I)=PWT2ER(I,K)                                                 NUDOB.718
  690   CONTINUE                                                                 NUDOB.719
                                                                                 NUDOB.720
      PRINT663,J                                                                 NUDOB.721
663   FORMAT(1H0,'PWT2ER(I,K) FOR J = ',I2)                                      NUDOB.722
      CALL MAPSMP(SCRPR,MKX,MIX,1,KL,1,1,IL,1,0.,-1,NAME,XTIME)                  NUDOB.723
#endif                                                                           NUDOB.724
      RETURN                                                                     NUDOB.725
C                                                                                NUDOB.726
C                                                                                NUDOB.727
  170 CONTINUE                                                                   NUDOB.728
C 3-D CROSS-POINT TENDENCIES                                                     NUDOB.729
      IF(3-IVAR.LT.0)THEN                                                        NUDOB.730
        GITQ=GIQ(IN)                                                             NUDOB.731
      ELSE                                                                       NUDOB.732
        GITQ=GIT(IN)                                                             NUDOB.733
      ENDIF                                                                      NUDOB.734
      IF(3-IVAR.LT.0)THEN                                                        NUDOB.735
        ISTQ=ISMOIS(IN)                                                          NUDOB.736
      ELSE                                                                       NUDOB.737
        ISTQ=ISTEMP(IN)                                                          NUDOB.738
      ENDIF                                                                      NUDOB.739
      DO 180 K=1,KL                                                              NUDOB.740
        DO 180 I=2+ICUT,ILXM-ICUT                                                NUDOB.741
          IF(3-IVAR.LT.0)THEN                                                    NUDOB.742
            TBORQB=QVB(I,J,K)                                                    NUDOB.743
          ELSE                                                                   NUDOB.744
            TBORQB=TB(I,J,K)                                                     NUDOB.745
          ENDIF                                                                  NUDOB.746
          IF(MOD(KTAU,INFR).EQ.0.OR.(IFREST.AND.KTAU.EQ.KTAUR))THEN              NUDOB.747
            W2EOWT=WT2ERR(I,K)/WT(I,K)                                           NUDOB.748
          ELSE                                                                   NUDOB.749
            W2EOWT=SAVWT(IPL,I,J,K)                                              NUDOB.750
          ENDIF                                                                  NUDOB.751
C     W2EOWT=CVMGT(WT2ERR(I,K)/WT(I,K),SAVWT(IPL,I,J,K),                         NUDOB.752
C    1MOD(KTAU,INFR).EQ.0.OR.(IFREST.AND.KTAU.EQ.KTAUR))                         NUDOB.753
          ATEN(I,J,K)=ATEN(I,J,K)+GITQ*PSB(I,J)*W2EOWT*WPBL(I,K)*                NUDOB.754
     +                TFACI(IN)*ISTQ+GIP(IN)*TBORQB/PSB(I,J)*PWT2ER(I,K)         NUDOB.755
     +                /WTP(I,K)*TFACI(IN)*ISPSTR(IN)                             NUDOB.756
  180   CONTINUE                                                                 NUDOB.757
                                                                                 NUDOB.758
      IF(MOD(KTAU,INFR).EQ.0.OR.(IFREST.AND.KTAU.EQ.KTAUR))THEN                  NUDOB.759
        DO 181 K=1,KL                                                            NUDOB.760
          DO 181 I=1,ILX                                                         NUDOB.761
            SAVWT(IPL,I,J,K)=WT2ERR(I,K)/WT(I,K)                                 NUDOB.762
  181     CONTINUE                                                               NUDOB.763
                                                                                 NUDOB.764
      ENDIF                                                                      NUDOB.765
      RETURN                                                                     NUDOB.766
C                                                                                NUDOB.767
C                                                                                NUDOB.768
      END                                                                        NUDOB.769
                                                                                 NUDOB.770
