C-----------------------------------------------------------------------         RADDED.1
C-----------------------------------------------------------------------         RADDED.2
      SUBROUTINE RADDED(COSZRS,  TRAYOSLP,PFLX    ,ABH2O   ,ABO3    ,            RADDED.3
     $                  ABCO2   ,ABO2    ,UH2O    ,UO3     ,UCO2    ,            RADDED.4
     $                  UO2     ,TAUEXC  ,WC      ,GC      ,FC      ,            RADDED.5
     $                  NLOOP   ,IS      ,IE      ,RDIR    ,RDIF    ,            RADDED.6
     $                  TDIR    ,TDIF    ,EXPLAY  ,EXPTDN  ,RDNDIF  ,            RADDED.7
     $                  TOTTRN, KTAUW, TAUEXI, WCI, GCI, FCI)                    05DEC01.1492
C                                                                                RADDED.9
C DELTA-EDDINGTON SOLUTION                                                       RADDED.10
C                                                                                RADDED.11
C COMPUTES LAYER REFLECTIVITIES AND TRANSMISSIVITIES, FROM THE                   RADDED.12
C TOP DOWN TO THE SURFACE USING THE DELTA-EDDINGTON SOLUTIONS FOR                RADDED.13
C EACH LAYER                                                                     RADDED.14
C                                                                                RADDED.15
C IF TOTAL TRANSMISSION TO THE INTERFACE ABOVE A PARTICULAR                      RADDED.16
C LAYER IS LESS THAN TRMIN, THEN NO FURTHER DELTA-EDDINGTON SOLUTIONS            RADDED.17
C ARE EVALUATED FOR LAYERS BELOW                                                 RADDED.18
C                                                                                RADDED.19
C FOR MORE DETAILS, SEE  BRIEGLEB, B.P.   DELTA-EDDINGTON APPROXIMATION          RADDED.20
C FOR SOLAR RADIATION IN THE NCAR COMMUNITY CLIMATE MODEL, SUBMITTED             RADDED.21
C TO JOURNAL OF GEOPHYSICAL RESEARCH                                             RADDED.22
C                                                                                RADDED.23
C     INCLUDE 'PARAME'                                                           RADDED.24
C     INCLUDE 'RADCCM2.DATA'                                                     RADDED.25
#     include <parame.incl>                                                      RADDED.26
#     include <parccm2.incl>                                                     RADDED.27
#     include <radccm2.incl>                                                     RADDED.28
#     include <various.incl>                                                     RADDED.29
C-----------------------------------------------------------------------         RADDED.30
C                                                                                RADDED.31
C INPUT ARGUMENTS                                                                RADDED.32
C                                                                                RADDED.33
      REAL COSZRS(IPLOND)         ! COSINE ZENITH ANGLE                          RADDED.34
      REAL   TRAYOSLP,              ! TRAY/SSLP                                  RADDED.35
     $     PFLX(IPLOND,0:IPLEVP),   ! INTERFACE PRESSURE                         RADDED.36
     $     ABH2O,                 ! ABSORPTION COEFFICIANT FOR H2O               RADDED.37
     $     ABO3 ,                 ! ABSORPTION COEFFICIANT FOR O3                RADDED.38
     $     ABCO2,                 ! ABSORPTION COEFFICIANT FOR CO2               RADDED.39
     $     ABO2 ,                 ! ABSORPTION COEFFICIANT FOR O2                RADDED.40
     $     UH2O(IPLOND,0:IPLEV),    ! LAYER ABSORBER AMOUNT OF H2O               RADDED.41
     $      UO3(IPLOND,0:IPLEV),    ! LAYER ABSORBER AMOUNT OF  O3               RADDED.42
     $     UCO2(IPLOND,0:IPLEV),    ! LAYER ABSORBER AMOUNT OF CO2               RADDED.43
     $      UO2(IPLOND,0:IPLEV)     ! LAYER ABSORBER AMOUNT OF  O2               RADDED.44
      REAL TAUEXC(IPLOND,0:IPLEV),  ! CLOUD EXTINCTION OPTICAL DEPTH             RADDED.45
     $         WC(IPLOND,0:IPLEV),  ! CLOUD SINGLE SCATTERING ALBEDO             RADDED.46
     $         GC(IPLOND,0:IPLEV),  ! CLOUD ASSYMETRY PARAMETER                  RADDED.47
     $         FC(IPLOND,0:IPLEV)   ! CLOUD FORWARD SCATTERED FRACTION           RADDED.48
      REAL TAUEXI(IPLOND,0:IPLEV),  ! CLOUD EXT OPTICAL DEPTH - ICE              05DEC01.1493
     $         WCI(IPLOND,0:IPLEV),  ! CLOUD SINGLE SCAT ALB - ICE               05DEC01.1494
     $         GCI(IPLOND,0:IPLEV),  ! CLOUD ASSYMETRY PARAM - ICE               05DEC01.1495
     $         FCI(IPLOND,0:IPLEV)   ! CLOUD FORWARD SCAT FRAC - ICE             05DEC01.1496
      INTEGER NLOOP,              ! NUMBER OF LOOPS (1 OR 2)                     RADDED.49
     $        IS(2),              ! STARTING INDEX FOR 1 OR 2 LOOPS              RADDED.50
     $        IE(2)               ! ENDING INDEX FOR 1 OR 2 LOOPS                RADDED.51
C                                                                                RADDED.52
C INPUT/OUTPUT ARGUMENTS                                                         RADDED.53
C                                                                                RADDED.54
C FOLLOWING VARIABLES ARE DEFINED FOR EACH LAYER; 0 REFERS TO EXTRAE             RADDED.55
C LAYER ABOVE TOP OF MODEL:                                                      RADDED.56
C                                                                                RADDED.57
      REAL RDIR(IPLOND,0:IPLEV),   ! LAYER REFLECTIVITY TO DIRECT RADIAT         RADDED.58
     $     RDIF(IPLOND,0:IPLEV),   ! LAYER REFFLECTIVITY TO DIFFUSE RADI         RADDED.59
     $     TDIR(IPLOND,0:IPLEV),   ! LAYER TRANSMISSION TO DIRECT RADIAT         RADDED.60
     $     TDIF(IPLOND,0:IPLEV),   ! LAYER TRANSMISSION TO DIFFUSE RADIA         RADDED.61
     $     EXPLAY(IPLOND,0:IPLEV)  ! SOLAR BEAM EXP TRANSMISSION FOR LAY         RADDED.62
C                                                                                RADDED.63
C (NOTE THAT THE FOLLOWING VARIABLES ARE DEFINED ON INTERFACES, WITH             RADDED.64
C THE INDEX K REFERRING TO THE TOP INTERFACE OF THE KTH LAYER:                   RADDED.65
C EXPTDN,RDNDIF,TOTTRN; FOR EXAMPLE, TOTTRN(K=5) REFERS TO THE TOTAL             RADDED.66
C TRANSMISSION TO THE TOP INTERFACE OF THE 5TH LAYER; IPLEVP REFERS TO T         RADDED.67
C EARTH SURFACE                                                                  RADDED.68
C                                                                                RADDED.69
      REAL RDNDIF(IPLOND,0:IPLEVP), ! ADDED DIF REF FOR LAYERS ABOVE             RADDED.70
     $ EXPTDN(IPLOND,0:IPLEVP), ! SOLAR BEAM EXP DOWN TRANSMISSION FRO           RADDED.71
     $  TOTTRN(IPLOND,0:IPLEVP)  ! TOTAL TRANSMISSION FOR LAYERS ABOVE           RADDED.72
C                                                                                RADDED.73
C      EXTERNAL  RESETR,     ! RESETS ARRAY ELEMENTS TO ZERO                     RADDED.74
C     $        WHENFGT     ! COLLECT INDICES FOR GREATER THAN CONDITIO           RADDED.75
C                                                                                RADDED.76
C LOCAL WORKSPACE                                                                RADDED.77
C                                                                                RADDED.78
      INTEGER          I,   ! LONGITUDE INDEX                                    RADDED.79
     $                 K,   ! LEVEL INDEX                                        RADDED.80
     $                NN,   ! INDEX OF LONGITUDE LOOPS (MAX=NLOOP)               RADDED.81
     $                II,   ! LONGITUDE INDEX                                    RADDED.82
     $            NVAL,   ! NUMBER OF LONGITUDE VALUES SATISFYING CRIT           RADDED.83
     $      INDEX(IPLOND)    ! ARRAY OF LONGITUDE INDICES                        RADDED.84
C                                                                                RADDED.85
      REAL TAUGAB(IPLOND), ! LAYER TOTAL GAS ABSORPTION OPTICAL DEPTH            RADDED.86
     $     TAURAY(IPLOND),   ! LAYER RAYLEIGH OPTICAL DEPTH                      RADDED.87
     $     TAUCSC       ,   ! LAYER CLOUD SCATTERING OPTICAL DEPTH               RADDED.88
     $     TAUTOT       ,   ! TOTAL LAYER OPTICAL DEPTH                          RADDED.89
     $       WTOT       ,   ! TOTAL LAYER SINGLE SCATTER ALBEDO                  RADDED.90
     $       GTOT       ,   ! TOTAL LAYER ASYMMETRY PARAMETER                    RADDED.91
     $       FTOT           ! TOTAL LAYER FORWARD SCATTER FRACTION               RADDED.92
C                                                                                RADDED.93
      REAL MU0          ,   ! COSINE SOLAR ZENITH ANGLE                          RADDED.94
     $     LLMM             ! LM*LM*MU0*MU0                                      RADDED.95
C                                                                                RADDED.96
C MINIMUM TOTAL TRANSMISSION BELOW WHICH NOLAYER COMPUTATION ARE DONE:           RADDED.97
C                                                                                RADDED.98
      REAL TRMIN,    ! TRANSMISSION CUTOFF                                       RADDED.99
     $      WRAY,    ! RAYLEIGH SINGLE SCATTER ALBEDO                            RADDED.100
     $      GRAY,    ! RAYLEIGH ASYMETRY PARAMETER                               RADDED.101
     $      FRAY,    ! RAYLEIGH FORWARD SCATTERED FRACTION                       RADDED.102
     $      EPS      ! ADJUSTMENT PARAMETER FOR DIVIDE BY ZERO                   RADDED.103
C                                                                                RADDED.104
      DATA TRMIN  /  1.E-3    /                                                  RADDED.105
      DATA  WRAY  /  0.999999 /                                                  RADDED.106
      DATA  GRAY  /  0.0      /                                                  RADDED.107
      DATA  FRAY  /  0.1      /                                                  RADDED.108
      DATA  EPS   /  1.E-5    /                                                  RADDED.109
C                                                                                RADDED.110
      REAL WTAU,    !  RAYLEIGH LAYER SCATTERING OPTICAL DEPTH                   RADDED.111
     $       WT,    !  LAYER TOTAL SINGLE SCATTERING ALBEDO                      RADDED.112
     $       TS,    !  LAYER SCALED EXTINCTION OPTICAL DEPTH                     RADDED.113
     $       WS,    !  LAYER SCALED SINGLE SCATTERING ALBEDO                     RADDED.114
     $       GS     !  LAYER SCALED ASYMMETRY PARAMETER                          RADDED.115
C                                                                                RADDED.116
      REAL RDENOM,    !  MULITIPLE SCATTERING TERM                               RADDED.117
     $    RDIREXP,    !  LAYER DIRECT REF TIMES EXP TRANSMISSION                 RADDED.118
     $    TDNMEXP     !  TOTAL TRANSMISSION MINUS EXP TRANSMISSION               RADDED.119
C                                                                                RADDED.120
C STATEMENT FUNCTIONS FOR DELTA-EDDINGTON SOLUTION FOR EACH LAYER                RADDED.121
C                                                                                RADDED.122
      REAL ALPHAX,GAMMAX,EL,TAUS,OMGS,ASYS,U,N,LM,NE                             RADDED.123
      REAL W,UU,G,E,F,T,ET                                                       RADDED.124
C                                                                                RADDED.125
C INTERMEDIATE TERMS FOR DELTA-EDDINGTON SOLUTION                                RADDED.126
C                                                                                RADDED.127
      REAL ALP,GAM,UE,ARG,EXTINS,AMG,APG                                         RADDED.128
C                                                                                RADDED.129
      ALPHAX(W,UU,G,E) = .75*W*UU*((1. + G*(1-W))/(1. - E*E*UU*UU))              RADDED.130
      GAMMAX(W,UU,G,E) = .50*W*((3.*G*(1.-W)*UU*UU + 1.)/(1.-E*E*UU*UU))         RADDED.131
      EL(W,G)         = SQRT(3.*(1-W)*(1. - W*G))                                RADDED.132
      TAUS(W,F,T)     = (1. - W*F)*T                                             RADDED.133
      OMGS(W,F)       = (1. - F)*W/(1. - W*F)                                    RADDED.134
      ASYS(G,F)       = (G - F)/(1. - F)                                         RADDED.135
      U(W,G,E)        = 1.5*(1. - W*G)/E                                         RADDED.136
      N(UU,ET)        = ((UU+1.)*(UU+1.)/ET ) - ((UU-1.)*(UU-1.)*ET)             RADDED.137
#ifdef MPP1                                                                      RADDED.138
#   define IPLON I  LX                                                           RADDED.139
#   define IPLOND I  LX                                                          RADDED.140
#endif                                                                           RADDED.141
C                                                                                RADDED.142
C                                                                                RADDED.143
C INITIALIZE ALL TOTAL TRANSMISSION VALUES TO 0, SO THAT NIGHTTIME VALUE         RADDED.144
C FROM PREVIOUS COMPUTATIONS ARE NOT USED:                                       RADDED.145
C                                                                                RADDED.146
#ifndef MPP1                                                                     RADDED.147
      CALL RESETR(TOTTRN,IPLOND*IPLEVP,0.)                                       RADDED.148
#else                                                                            RADDED.149
      CALL RESETR(TOTTRN,MIX*IPLEVP,0.)                                          RADDED.150
#endif                                                                           RADDED.151
C                                                                                RADDED.152
C COMPUTE TOTAL DIRECT BEAM TRANSMISSION, TOTAL TRANSMISSION, AND                RADDED.153
C REFLECTIVITY FOR DIFFUSE RADIATION (FROM BELOW) FOR ALL LAYERS                 RADDED.154
C ABOVE EACH INTERFACE BY STARTING FROM THE TOP AND ADDING LAYERS                RADDED.155
C DOWN:                                                                          RADDED.156
C                                                                                RADDED.157
C FOR THE EXTRAE LAYER ABOVE MODEL TOP:                                          RADDED.158
C                                                                                RADDED.159
      DO 10 NN=1,NLOOP                                                           RADDED.160
        DO 20 I=IS(NN),IE(NN)                                                    RADDED.161
C                                                                                RADDED.162
          TAURAY(I)=TRAYOSLP*(PFLX(I,1)-PFLX(I,0))                               RADDED.163
          TAUGAB(I)=ABH2O*UH2O(I,0)+ABO3*UO3(I,0)+ABCO2*UCO2(I,0)+ABO2*          RADDED.164
     +              UO2(I,0)                                                     RADDED.165
C                                                                                RADDED.166
          TAUTOT=TAUEXC(I,0)+TAUEXI(I,0)+TAURAY(I)+TAUGAB(I)                     05DEC01.1497
          TAUCSC=TAUEXC(I,0)*WC(I,0)+TAUEXI(I,0)*WCI(I,0)                        05DEC01.1498
          WTAU=WRAY*TAURAY(I)                                                    RADDED.169
          WT=WTAU+TAUCSC                                                         RADDED.170
          WTOT=WT/TAUTOT                                                         RADDED.171
          GTOT=(WTAU*GRAY+TAUEXC(I,0)*WC(I,0)*GC(I,0)+                           05DEC01.1499
     +          TAUEXI(I,0)*WCI(I,0)*GCI(I,0))/WT                                05DEC01.1500
          FTOT=(WTAU*FRAY+TAUEXC(I,0)*WC(I,0)*FC(I,0)+                           05DEC01.1501
     +          TAUEXI(I,0)*WCI(I,0)*FCI(I,0))/WT                                05DEC01.1502
C                                                                                RADDED.174
          TS=TAUS(WTOT,FTOT,TAUTOT)                                              RADDED.175
          WS=OMGS(WTOT,FTOT)                                                     RADDED.176
          GS=ASYS(GTOT,FTOT)                                                     RADDED.177
          LM=EL(WS,GS)                                                           RADDED.178
C                                                                                RADDED.179
C ADJUST COSZRS IF NECESSARY TO AVOID DIVIDE BY ZERO                             RADDED.180
C                                                                                RADDED.181
          MU0=AMAX1(COSZRS(I),EPS)                                               RADDED.182
          LLMM=LM*LM*MU0*MU0                                                     RADDED.183
C                                                                                RADDED.184
          IF(LLMM.GT.1.-EPS.AND.LLMM.LE.1.)THEN                                  RADDED.185
            MU0=MU0-EPS                                                          RADDED.186
          ENDIF                                                                  RADDED.187
          IF(LLMM.LT.1.+EPS.AND.LLMM.GT.1.)THEN                                  RADDED.188
            MU0=MU0+EPS                                                          RADDED.189
          ENDIF                                                                  RADDED.190
C                                                                                RADDED.191
          ALP=ALPHAX(WS,MU0,GS,LM)                                               RADDED.192
          GAM=GAMMAX(WS,MU0,GS,LM)                                               RADDED.193
          UE=U(WS,GS,LM)                                                         RADDED.194
C                                                                                RADDED.195
C LIMIT ARGUMENT OF EXPONENTIAL TO 25, INCASE LM*TS VERY LARGE:                  RADDED.196
C                                                                                RADDED.197
          ARG=AMIN1(LM*TS,25.)                                                   RADDED.198
          EXTINS=EXP(-ARG)                                                       RADDED.199
          NE=N(UE,EXTINS)                                                        RADDED.200
C                                                                                RADDED.201
          RDIF(I,0)=(UE+1.)*(UE-1.)*(1./EXTINS-EXTINS)/NE                        RADDED.202
          TDIF(I,0)=4.*UE/NE                                                     RADDED.203
C                                                                                RADDED.204
C LIMIT ARGUMENT OF EXPONENTIAL TO 25, INCASE COSZRS IS VERY SMALL:              RADDED.205
C                                                                                RADDED.206
          ARG=AMIN1(TS/MU0,25.)                                                  RADDED.207
          EXPLAY(I,0)=EXP(-ARG)                                                  RADDED.208
C                                                                                RADDED.209
          APG=ALP+GAM                                                            RADDED.210
          AMG=ALP-GAM                                                            RADDED.211
          RDIR(I,0)=AMG*(TDIF(I,0)*EXPLAY(I,0)-1.)+APG*RDIF(I,0)                 RADDED.212
          TDIR(I,0)=APG*TDIF(I,0)+(AMG*RDIF(I,0)-(APG-1.))*EXPLAY(I,0)           RADDED.213
C                                                                                RADDED.214
C UNDER RARE CONDITIONS, REFLECTIVIES AND TRANSMISSIVITIES CAN BE                RADDED.215
C NEGATIVE; ZERO OUT ANY NEGATIVE VALUES                                         RADDED.216
C                                                                                RADDED.217
          RDIR(I,0)=AMAX1(RDIR(I,0),0.0)                                         RADDED.218
          TDIR(I,0)=AMAX1(TDIR(I,0),0.0)                                         RADDED.219
          RDIF(I,0)=AMAX1(RDIF(I,0),0.0)                                         RADDED.220
          TDIF(I,0)=AMAX1(TDIF(I,0),0.0)                                         RADDED.221
C                                                                                RADDED.222
C INITIALIZE TOP INTERFACE OF EXTRAE LAYER:                                      RADDED.223
C                                                                                RADDED.224
          EXPTDN(I,0)=1.0                                                        RADDED.225
          RDNDIF(I,0)=0.0                                                        RADDED.226
          TOTTRN(I,0)=1.0                                                        RADDED.227
C                                                                                RADDED.228
          RDNDIF(I,1)=RDIF(I,0)                                                  RADDED.229
          TOTTRN(I,1)=TDIR(I,0)                                                  RADDED.230
C                                                                                RADDED.231
   20   CONTINUE                                                                 RADDED.232
   10 CONTINUE                                                                   RADDED.233
C                                                                                RADDED.234
C NOW, CONTINUE DOWN ONE LAYER AT A TIME; IF THE TOTAL TRANSMISSION              RADDED.235
C TO THE INTERFACE JUST ABOVE A GIVEN LAYER IS LESS THAN TRMIN, THEN             RADDED.236
C NO DELTA-EDDINGTON COMPUTATION FOR THAT LAYER IS DONE:                         RADDED.237
C                                                                                RADDED.238
      DO 30 K=1,IPLEV                                                            RADDED.239
C                                                                                RADDED.240
C INITIALIZE CURRENT LAYER PROPERTIES TO ZERO; ONLY IF TOTAL TRANSMISSIO         RADDED.241
C TO THE TOP INTERFACE OF THE CURRENT LAYER EXCEEDS THE MINIMUM, WILL            RADDED.242
C THESE VALUES BE COMPUTED BELOW:                                                RADDED.243
C                                                                                RADDED.244
        DO 40 NN=1,NLOOP                                                         RADDED.245
          DO 50 I=IS(NN),IE(NN)                                                  RADDED.246
C                                                                                RADDED.247
            RDIR(I,K)=0.0                                                        RADDED.248
            RDIF(I,K)=0.0                                                        RADDED.249
            TDIR(I,K)=0.0                                                        RADDED.250
            TDIF(I,K)=0.0                                                        RADDED.251
            EXPLAY(I,K)=0.0                                                      RADDED.252
C                                                                                RADDED.253
C CALCULATES THE SOLAR BEAM TRANSMISSION, TOTAL TRANSMISSION, AND                RADDED.254
C REFLECTIVITY FOR DIFFUSE RADIATION FROM BELOW AT THE TOP OF THE                RADDED.255
C CURRENT LAYER:                                                                 RADDED.256
C                                                                                RADDED.257
            EXPTDN(I,K)=EXPTDN(I,K-1)*EXPLAY(I,K-1)                              RADDED.258
            RDENOM=1./(1.-RDIF(I,K-1)*RDNDIF(I,K-1))                             RADDED.259
            RDIREXP=RDIR(I,K-1)*EXPTDN(I,K-1)                                    RADDED.260
            TDNMEXP=TOTTRN(I,K-1)-EXPTDN(I,K-1)                                  RADDED.261
            TOTTRN(I,K)=EXPTDN(I,K-1)*TDIR(I,K-1)+TDIF(I,K-1)*(TDNMEXP+          RADDED.262
     +                  RDNDIF(I,K-1)*RDIREXP)*RDENOM                            RADDED.263
            RDNDIF(I,K)=RDIF(I,K-1)+(RDNDIF(I,K-1)*TDIF(I,K-1))*(                RADDED.264
     +                  TDIF(I,K-1)*RDENOM)                                      RADDED.265
C                                                                                RADDED.266
   50     CONTINUE                                                               RADDED.267
   40   CONTINUE                                                                 RADDED.268
C                                                                                RADDED.269
C COMPUTE NEXT LAYER DELTA-EDDINGTON SOLUTION ONLY IF TOTAL TRANSMISSION         RADDED.270
C OF RADIATION TO THE INTERFACE JUST ABOVE THE LAYER EXCEEDS TRMIN.              RADDED.271
C                                                                                RADDED.272
#ifndef MPP1                                                                     RADDED.273
        CALL WHENFGT(IPLON,TOTTRN(1,K),1,TRMIN,INDEX,NVAL)                       RADDED.274
#else                                                                            RADDED.275
        CALL WHENFGT(IPLON,TOTTRN(NOFLIC(1),K),1,TRMIN,INDEX,NVAL)               RADDED.276
#endif                                                                           RADDED.277
        IF(NVAL.GT.0)THEN                                                        RADDED.278
          DO 60 II=1,NVAL                                                        RADDED.279
#ifndef MPP1                                                                     RADDED.280
            I=INDEX(II)                                                          RADDED.281
#else                                                                            RADDED.282
            I=INDEX(NOFLIC(II))                                                  RADDED.283
#endif                                                                           RADDED.284
C                                                                                RADDED.285
            TAURAY(I)=TRAYOSLP*(PFLX(I,K+1)-PFLX(I,K))                           RADDED.286
            TAUGAB(I)=ABH2O*UH2O(I,K)+ABO3*UO3(I,K)+ABCO2*UCO2(I,K)+ABO2         RADDED.287
     +                *UO2(I,K)                                                  RADDED.288
C                                                                                RADDED.289
            TAUTOT=TAUEXC(I,K)+TAUEXI(I,K)+TAURAY(I)+TAUGAB(I)                   05DEC01.1503
            TAUCSC=TAUEXC(I,K)*WC(I,K)+TAUEXI(I,K)*WCI(I,K)                      05DEC01.1504
            WTAU=WRAY*TAURAY(I)                                                  RADDED.292
            WT=WTAU+TAUCSC                                                       RADDED.293
            WTOT=WT/TAUTOT                                                       RADDED.294
            GTOT=(WTAU*GRAY+TAUEXC(I,K)*WC(I,K)*GC(I,K)+                         05DEC01.1505
     +            TAUEXI(I,K)*WCI(I,K)*GCI(I,K))/WT                              05DEC01.1506
            FTOT=(WTAU*FRAY+TAUEXC(I,K)*WC(I,K)*FC(I,K)+                         05DEC01.1507
     +            TAUEXI(I,K)*WCI(I,K)*FCI(I,K))/WT                              05DEC01.1508
C                                                                                RADDED.297
            TS=TAUS(WTOT,FTOT,TAUTOT)                                            RADDED.298
            WS=OMGS(WTOT,FTOT)                                                   RADDED.299
            GS=ASYS(GTOT,FTOT)                                                   RADDED.300
            LM=EL(WS,GS)                                                         RADDED.301
C                                                                                RADDED.302
C ADJUST COSZRS IF NECESSARY TO AVOID DIVIDE BY ZERO                             RADDED.303
C                                                                                RADDED.304
            MU0=AMAX1(COSZRS(I),EPS)                                             RADDED.305
            LLMM=LM*LM*MU0*MU0                                                   RADDED.306
C                                                                                RADDED.307
            IF(LLMM.GT.1.-EPS.AND.LLMM.LE.1.)THEN                                RADDED.308
              MU0=MU0-EPS                                                        RADDED.309
            ENDIF                                                                RADDED.310
            IF(LLMM.LT.1.+EPS.AND.LLMM.GT.1.)THEN                                RADDED.311
              MU0=MU0+EPS                                                        RADDED.312
            ENDIF                                                                RADDED.313
C                                                                                RADDED.314
            ALP=ALPHAX(WS,MU0,GS,LM)                                             RADDED.315
            GAM=GAMMAX(WS,MU0,GS,LM)                                             RADDED.316
            UE=U(WS,GS,LM)                                                       RADDED.317
C                                                                                RADDED.318
C LIMIT ARGUMENT OF EXPONENTIAL TO 25, INCASE LM VERY LARGE:                     RADDED.319
C                                                                                RADDED.320
            ARG=AMIN1(LM*TS,25.)                                                 RADDED.321
            EXTINS=EXP(-ARG)                                                     RADDED.322
            NE=N(UE,EXTINS)                                                      RADDED.323
C                                                                                RADDED.324
            RDIF(I,K)=(UE+1.)*(UE-1.)*(1./EXTINS-EXTINS)/NE                      RADDED.325
            TDIF(I,K)=4.*UE/NE                                                   RADDED.326
C                                                                                RADDED.327
C LIMIT ARGUMENT OF EXPONENTIAL TO 25, INCASE COSZRS IS VERY SMALL:              RADDED.328
C                                                                                RADDED.329
            ARG=AMIN1(TS/MU0,25.)                                                RADDED.330
            EXPLAY(I,K)=EXP(-ARG)                                                RADDED.331
C                                                                                RADDED.332
            APG=ALP+GAM                                                          RADDED.333
            AMG=ALP-GAM                                                          RADDED.334
            RDIR(I,K)=AMG*(TDIF(I,K)*EXPLAY(I,K)-1.)+APG*RDIF(I,K)               RADDED.335
            TDIR(I,K)=APG*TDIF(I,K)+(AMG*RDIF(I,K)-(APG-1.))*EXPLAY(I,K)         RADDED.336
C                                                                                RADDED.337
C UNDER RARE CONDITIONS, REFLECTIVIES AND TRANSMISSIVITIES CAN BE                RADDED.338
C NEGATIVE; ZERO OUT ANY NEGATIVE VALUES                                         RADDED.339
C                                                                                RADDED.340
            RDIR(I,K)=AMAX1(RDIR(I,K),0.0)                                       RADDED.341
            TDIR(I,K)=AMAX1(TDIR(I,K),0.0)                                       RADDED.342
            RDIF(I,K)=AMAX1(RDIF(I,K),0.0)                                       RADDED.343
            TDIF(I,K)=AMAX1(TDIF(I,K),0.0)                                       RADDED.344
   60     CONTINUE                                                               RADDED.345
        ENDIF                                                                    RADDED.346
C                                                                                RADDED.347
   30 CONTINUE                                                                   RADDED.348
C                                                                                RADDED.349
C COMPUTE TOTAL DIRECT BEAM TRANSMISSION, TOTAL TRANSMISSION, AND                RADDED.350
C REFLECTIVITY FOR DIFFUSE RADIATION (FROM BELOW) FOR ALL LAYERS                 RADDED.351
C ABOVE THE SURFACE:                                                             RADDED.352
C                                                                                RADDED.353
      K=IPLEVP                                                                   RADDED.354
      DO 70 NN=1,NLOOP                                                           RADDED.355
        DO 80 I=IS(NN),IE(NN)                                                    RADDED.356
          EXPTDN(I,K)=EXPTDN(I,K-1)*EXPLAY(I,K-1)                                RADDED.357
          RDENOM=1./(1.-RDIF(I,K-1)*RDNDIF(I,K-1))                               RADDED.358
          RDIREXP=RDIR(I,K-1)*EXPTDN(I,K-1)                                      RADDED.359
          TDNMEXP=TOTTRN(I,K-1)-EXPTDN(I,K-1)                                    RADDED.360
          TOTTRN(I,K)=EXPTDN(I,K-1)*TDIR(I,K-1)+TDIF(I,K-1)*(TDNMEXP+            RADDED.361
     +                RDNDIF(I,K-1)*RDIREXP)*RDENOM                              RADDED.362
          RDNDIF(I,K)=RDIF(I,K-1)+(RDNDIF(I,K-1)*TDIF(I,K-1))*(                  RADDED.363
     +                TDIF(I,K-1)*RDENOM)                                        RADDED.364
   80   CONTINUE                                                                 RADDED.365
   70 CONTINUE                                                                   RADDED.366
C                                                                                RADDED.367
C DONE                                                                           RADDED.368
C                                                                                RADDED.369
      RETURN                                                                     RADDED.370
      END                                                                        RADDED.371
                                                                                 RADDED.372
