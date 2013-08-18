C-----------------------------------------------------------------------         RADCLR.1
C                                                                                RADCLR.2
      SUBROUTINE RADCLR(COSZRS,  TRAYOSLP,PFLX    ,ABH2O   ,ABO3    ,            RADCLR.3
     $                  ABCO2   ,ABO2    ,UTH2O   ,UTO3    ,UTCO2   ,            RADCLR.4
     $                  UTO2    ,NLOOP   ,IS      ,IE      ,RDIR    ,            RADCLR.5
     $                  RDIF    ,TDIR    ,TDIF    ,EXPLAY  ,EXPTDN  ,            RADCLR.6
     $                  RDNDIF  ,TOTTRN  )                                       RADCLR.7
C                                                                                RADCLR.8
C DELTA-EDDINGTON SOLUTION FOR SPECIAL CLEAR SKY COMPUTATION                     RADCLR.9
C                                                                                RADCLR.10
C COMPUTES TOTAL REFLECTIVITIES AND TRANSMISSIVITIES FOR TWO ATMOSPHERIC         RADCLR.11
C LAYERS: AN OVERLYING PURELY OZONE ABSORBING LAYER, AND THE REST OF THE         RADCLR.12
C COLUMN BELOW.                                                                  RADCLR.13
C                                                                                RADCLR.14
C FOR MORE DETAILS, SEE  BRIEGLEB, B.P.   DELTA-EDDINGTON APPROXIMATION          RADCLR.15
C FOR SOLAR RADIATION IN THE NCAR COMMUNITY CLIMATE MODEL, SUBMITTED             RADCLR.16
C TO JOURNAL OF GEOPHYSICAL RESEARCH                                             RADCLR.17
C                                                                                RADCLR.18
CC    INCLUDE 'PARAME'                                                           RADCLR.19
CC    INCLUDE 'RADCCM2.DATA'                                                     RADCLR.20
#     include <parame.incl>                                                      RADCLR.21
#     include <parccm2.incl>                                                     RADCLR.22
#     include <radccm2.incl>                                                     RADCLR.23
#ifndef MPP1                                                                     RADCLR.24
#     include <various.incl>                                                     RADCLR.25
#endif                                                                           RADCLR.26
                                                                                 RADCLR.27
C-----------------------------------------------------------------------         RADCLR.28
C                                                                                RADCLR.29
C INPUT ARGUMENTS                                                                RADCLR.30
C                                                                                RADCLR.31
      REAL COSZRS(IPLOND)          ! COSINE ZENITH ANGLE                         RADCLR.32
      REAL     TRAYOSLP,              ! TRAY/SSLP                                RADCLR.33
     $     PFLX(IPLOND,0:IPLEVP),   ! INTERFACE PRESSURE                         RADCLR.34
     $     ABH2O,                 ! ABSORPTION COEFFICIANT FOR H2O               RADCLR.35
     $     ABO3 ,                 ! ABSORPTION COEFFICIANT FOR O3                RADCLR.36
     $     ABCO2,                 ! ABSORPTION COEFFICIANT FOR CO2               RADCLR.37
     $     ABO2 ,                 ! ABSORPTION COEFFICIANT FOR O2                RADCLR.38
     $     UTH2O(IPLOND),          ! TOTAL COLUMN ABSORBER AMOUNT OF H2O         RADCLR.39
     $     UTO3(IPLOND),           ! TOTAL COLUMN ABSORBER AMOUNT OF  O3         RADCLR.40
     $     UTCO2(IPLOND),          ! TOTAL COLUMN ABSORBER AMOUNT OF CO2         RADCLR.41
     $     UTO2(IPLOND)            ! TOTAL COLUMN ABSORBER AMOUNT OF  O2         RADCLR.42
      INTEGER NLOOP,              ! NUMBER OF LOOPS (1 OR 2)                     RADCLR.43
     $        IS(2),              ! STARTING INDEX FOR 1 OR 2 LOOPS              RADCLR.44
     $        IE(2)               ! ENDING INDEX FOR 1 OR 2 LOOPS                RADCLR.45
C                                                                                RADCLR.46
C INPUT/OUTPUT ARGUMENTS                                                         RADCLR.47
C                                                                                RADCLR.48
C FOLLOWING VARIABLES ARE DEFINED FOR EACH LAYER; NOTE, WE USE LAYER 0 T         RADCLR.49
C REFER TO THE ENTIRE ATMOSPHERIC COLUMN:                                        RADCLR.50
C                                                                                RADCLR.51
      REAL RDIR(IPLOND,0:IPLEV),! LAYER REFLECTIVITY TO DIRECT RADIATIO          RADCLR.52
     $     RDIF(IPLOND,0:IPLEV),! LAYER REFFLECTIVITY TO DIFFUSE RADIAT          RADCLR.53
     $     TDIR(IPLOND,0:IPLEV),! LAYER TRANSMISSION TO DIRECT RADIATIO          RADCLR.54
     $     TDIF(IPLOND,0:IPLEV),! LAYER TRANSMISSION TO DIFFUSE RADIATI          RADCLR.55
     $     EXPLAY(IPLOND,0:IPLEV)! SOLAR BEAM EXP TRANSMISSION FOR LAYER         RADCLR.56
C                                                                                RADCLR.57
C (NOTE THAT THE FOLLOWING VARIABLES ARE DEFINED ON INTERFACES, WITH             RADCLR.58
C THE INDEX K REFERRING TO THE TOP INTERFACE OF THE KTH LAYER:                   RADCLR.59
C EXPTDN,RDNDIF,TOTTRN; FOR EXAMPLE, TOTTRN(K=5) REFERS TO THE TOTAL             RADCLR.60
C TRANSMISSION TO THE TOP INTERFACE OF THE 5TH LAYER.                            RADCLR.61
C                                                                                RADCLR.62
      REAL EXPTDN(IPLOND,0:IPLEVP),!SOLAR BEAM EXP DOWN TRANSMISSION FRO         RADCLR.63
     $     RDNDIF(IPLOND,0:IPLEVP), ! ADDED DIF REF FOR LAYERS ABOVE             RADCLR.64
     $     TOTTRN(IPLOND,0:IPLEVP)!TOTAL TRANSMISSION FOR LAYERS ABOVE           RADCLR.65
C                                                                                RADCLR.66
C      EXTERNAL  RESETR,     ! RESETS ARRAY ELEMENTS TO ZERO                     RADCLR.67
C     $          WHENFGT     ! COLLECT INDICES FOR GREATER THAN CONDITIO         RADCLR.68
C                                                                                RADCLR.69
C LOCAL WORKSPACE                                                                RADCLR.70
C                                                                                RADCLR.71
      INTEGER          I,   ! LONGITUDE INDEX                                    RADCLR.72
     $                 K,   ! LEVEL INDEX                                        RADCLR.73
     $                NN,   ! INDEX OF LONGITUDE LOOPS (MAX=NLOOP)               RADCLR.74
     $                II,   ! LONGITUDE INDEX                                    RADCLR.75
     $              NVAL,   ! NUMBER OF LONGITUDE VALUES SATISFYING CRIT         RADCLR.76
     $      INDEX(IPLOND)    ! ARRAY OF LONGITUDE INDICES                        RADCLR.77
C                                                                                RADCLR.78
      REAL TAUGAB(IPLOND),   ! TOTAL COLUMN GAS ABSORPTION OPTICAL DEPTH         RADCLR.79
     $     TAURAY(IPLOND),   ! COLUMN RAYLEIGH OPTICAL DEPTH                     RADCLR.80
     $     TAUTOT       ,   ! TOTAL COLUMN OPTICAL DEPTH                         RADCLR.81
     $       WTOT       ,   ! TOTAL COLUMN SINGLE SCATTER ALBEDO                 RADCLR.82
     $       GTOT       ,   ! TOTAL COLUMN ASYMMETRY PARAMETER                   RADCLR.83
     $       FTOT           ! TOTAL COLUMN FORWARD SCATTER FRACTION              RADCLR.84
C                                                                                RADCLR.85
      REAL MU0          ,   ! COSINE SOLAR ZENITH ANGLE                          RADCLR.86
     $     LLMM             ! LM*LM*MU0*MU0                                      RADCLR.87
C                                                                                RADCLR.88
C MINIMUM TOTAL TRANSMISSION BELOW WHICH NO LAYER COMPUTATION ARE DONE:          RADCLR.89
C                                                                                RADCLR.90
      REAL  TRMIN,   ! MINIMUM TOTAL TRANSMISSION ALLOWED                        RADCLR.91
     $      WRAY,    ! RAYLEIGH SINGLE SCATTER ALBEDO                            RADCLR.92
     $      GRAY,    ! RAYLEIGH ASYMETRY PARAMETER                               RADCLR.93
     $      FRAY,    ! RAYLEIGH FORWARD SCATTERED FRACTION                       RADCLR.94
     $      EPS      ! ADJUSTMENT PARAMETER FOR DIVIDE BY ZERO                   RADCLR.95
C                                                                                RADCLR.96
      DATA TRMIN  /  1.E-3    /                                                  RADCLR.97
      DATA  WRAY  /  0.999999 /                                                  RADCLR.98
      DATA  GRAY  /  0.0      /                                                  RADCLR.99
      DATA  FRAY  /  0.1      /                                                  RADCLR.100
      DATA  EPS   /  1.E-5    /                                                  RADCLR.101
C                                                                                RADCLR.102
      REAL   TS,    !  COLUMN SCALED EXTINCTION OPTICAL DEPTH                    RADCLR.103
     $       WS,    !  COLUMN SCALED SINGLE SCATTERING ALBEDO                    RADCLR.104
     $       GS     !  COLUMN SCALED ASYMMETRY PARAMETER                         RADCLR.105
C                                                                                RADCLR.106
      REAL RDENOM,    !  MULITIPLE SCATTERING TERM                               RADCLR.107
     $    RDIREXP,    !  LAYER DIRECT REF TIMES EXP TRANSMISSION                 RADCLR.108
     $    TDNMEXP     !  TOTAL TRANSMISSION MINUS EXP TRANSMISSION               RADCLR.109
C                                                                                RADCLR.110
C STATEMENT FUNCTIONS FOR DELTA-EDDINGTON SOLUTION FOR ENTIRE COLUMN:            RADCLR.111
C                                                                                RADCLR.112
      REAL ALPHAX,GAMMAX,EL,TAUS,OMGS,ASYS,U,N,LM,NE                             RADCLR.113
      REAL W,UU,G,E,F,T,ET                                                       RADCLR.114
C                                                                                RADCLR.115
C INTERMEDIATE TERMS FOR DELTA-EDDINGTON SOLUTION                                RADCLR.116
C                                                                                RADCLR.117
      REAL ALP,GAM,UE,ARG,EXTINS,AMG,APG                                         RADCLR.118
C                                                                                RADCLR.119
      ALPHAX(W,UU,G,E) = .75*W*UU*((1. + G*(1-W))/(1. - E*E*UU*UU))              RADCLR.120
      GAMMAX(W,UU,G,E) = .50*W*((3.*G*(1.-W)*UU*UU + 1.)/(1.-E*E*UU*UU))         RADCLR.121
      EL(W,G)         = SQRT(3.*(1-W)*(1. - W*G))                                RADCLR.122
      TAUS(W,F,T)     = (1. - W*F)*T                                             RADCLR.123
      OMGS(W,F)       = (1. - F)*W/(1. - W*F)                                    RADCLR.124
      ASYS(G,F)       = (G - F)/(1. - F)                                         RADCLR.125
      U(W,G,E)        = 1.5*(1. - W*G)/E                                         RADCLR.126
      N(UU,ET)        = ((UU+1.)*(UU+1.)/ET ) - ((UU-1.)*(UU-1.)*ET)             RADCLR.127
                                                                                 RADCLR.128
#ifdef MPP1                                                                      RADCLR.129
#   define IPLON I  LX                                                           RADCLR.130
#   define IPLOND I  LX                                                          RADCLR.131
#endif                                                                           RADCLR.132
                                                                                 RADCLR.133
C                                                                                RADCLR.134
C                                                                                RADCLR.135
C INITIALIZE ALL TOTAL TRANSMISSION VALUES TO 0, SO THAT NIGHTTIME VALUE         RADCLR.136
C FROM PREVIOUS COMPUTATIONS ARE NOT USED:                                       RADCLR.137
C                                                                                RADCLR.138
#ifndef MPP1                                                                     RADCLR.139
      CALL RESETR(TOTTRN,IPLOND*2,0.)                                            RADCLR.140
#else                                                                            RADCLR.141
      CALL RESETR(TOTTRN,MIX*2,0.)                                               RADCLR.142
#endif                                                                           RADCLR.143
C                                                                                RADCLR.144
C COMPUTE TOTAL DIRECT BEAM TRANSMISSION, TOTAL TRANSMISSION, AND                RADCLR.145
C REFLECTIVITY FOR DIFFUSE RADIATION (FROM BELOW) FOR ALL LAYERS                 RADCLR.146
C ABOVE EACH INTERFACE BY STARTING FROM THE TOP AND ADDING LAYERS                RADCLR.147
C DOWN:                                                                          RADCLR.148
C                                                                                RADCLR.149
C THE TOP LAYER IS ASSUMED TO BE A PURELY ABSORBING OZONE LAYER, AND             RADCLR.150
C THAT THE MEAN DIFFUSIVITY FOR DIFFUSE TRANSMISSION IS 1.66:                    RADCLR.151
C                                                                                RADCLR.152
      DO 10 NN=1,NLOOP                                                           RADCLR.153
        DO 20 I=IS(NN),IE(NN)                                                    RADCLR.154
C                                                                                RADCLR.155
          TAUGAB(I)=ABO3*UTO3(I)                                                 RADCLR.156
C                                                                                RADCLR.157
C LIMIT ARGUMENT OF EXPONENTIAL TO 25, INCASE COSZRS IS VERY SMALL:              RADCLR.158
C                                                                                RADCLR.159
          MU0=AMAX1(COSZRS(I),EPS)                                               RADCLR.160
          ARG=AMIN1(TAUGAB(I)/MU0,25.)                                           RADCLR.161
          EXPLAY(I,0)=EXP(-ARG)                                                  RADCLR.162
          TDIR(I,0)=EXPLAY(I,0)                                                  RADCLR.163
C                                                                                RADCLR.164
C SAME LIMIT FOR DIFFUSE TRANSMISSION:                                           RADCLR.165
C                                                                                RADCLR.166
          ARG=AMIN1(1.66*TAUGAB(I),25.)                                          RADCLR.167
          TDIF(I,0)=EXP(-ARG)                                                    RADCLR.168
C                                                                                RADCLR.169
          RDIR(I,0)=0.0                                                          RADCLR.170
          RDIF(I,0)=0.0                                                          RADCLR.171
C                                                                                RADCLR.172
C INITIALIZE TOP INTERFACE OF EXTRAE LAYER:                                      RADCLR.173
C                                                                                RADCLR.174
          EXPTDN(I,0)=1.0                                                        RADCLR.175
          RDNDIF(I,0)=0.0                                                        RADCLR.176
          TOTTRN(I,0)=1.0                                                        RADCLR.177
C                                                                                RADCLR.178
          RDNDIF(I,1)=RDIF(I,0)                                                  RADCLR.179
          TOTTRN(I,1)=TDIR(I,0)                                                  RADCLR.180
C                                                                                RADCLR.181
   20   CONTINUE                                                                 RADCLR.182
   10 CONTINUE                                                                   RADCLR.183
C                                                                                RADCLR.184
C NOW, COMPLETE THE REST OF THE COLUMN; IF THE TOTAL TRANSMISSION                RADCLR.185
C THROUGH THE TOP OZONE LAYER IS LESS THAN TRMIN, THEN                           RADCLR.186
C NO DELTA-EDDINGTON COMPUTATION FOR THE UNDERLYING COLUMN IS DONE:              RADCLR.187
C                                                                                RADCLR.188
      DO 30 K=1,1                                                                RADCLR.189
C                                                                                RADCLR.190
C INITIALIZE CURRENT LAYER PROPERTIES TO ZERO; ONLY IF TOTAL TRANSMISSIO         RADCLR.191
C TO THE TOP INTERFACE OF THE CURRENT LAYER EXCEEDS THE MINIMUM, WILL            RADCLR.192
C THESE VALUES BE COMPUTED BELOW:                                                RADCLR.193
C                                                                                RADCLR.194
        DO 40 NN=1,NLOOP                                                         RADCLR.195
          DO 50 I=IS(NN),IE(NN)                                                  RADCLR.196
C                                                                                RADCLR.197
            RDIR(I,K)=0.0                                                        RADCLR.198
            RDIF(I,K)=0.0                                                        RADCLR.199
            TDIR(I,K)=0.0                                                        RADCLR.200
            TDIF(I,K)=0.0                                                        RADCLR.201
            EXPLAY(I,K)=0.0                                                      RADCLR.202
C                                                                                RADCLR.203
C CALCULATES THE SOLAR BEAM TRANSMISSION, TOTAL TRANSMISSION, AND                RADCLR.204
C REFLECTIVITY FOR DIFFUSE RADIATION FROM BELOW AT THE TOP OF THE                RADCLR.205
C CURRENT LAYER:                                                                 RADCLR.206
C                                                                                RADCLR.207
            EXPTDN(I,K)=EXPTDN(I,K-1)*EXPLAY(I,K-1)                              RADCLR.208
            RDENOM=1./(1.-RDIF(I,K-1)*RDNDIF(I,K-1))                             RADCLR.209
            RDIREXP=RDIR(I,K-1)*EXPTDN(I,K-1)                                    RADCLR.210
            TDNMEXP=TOTTRN(I,K-1)-EXPTDN(I,K-1)                                  RADCLR.211
            TOTTRN(I,K)=EXPTDN(I,K-1)*TDIR(I,K-1)+TDIF(I,K-1)*(TDNMEXP+          RADCLR.212
     +                  RDNDIF(I,K-1)*RDIREXP)*RDENOM                            RADCLR.213
            RDNDIF(I,K)=RDIF(I,K-1)+(RDNDIF(I,K-1)*TDIF(I,K-1))*(                RADCLR.214
     +                  TDIF(I,K-1)*RDENOM)                                      RADCLR.215
C                                                                                RADCLR.216
   50     CONTINUE                                                               RADCLR.217
   40   CONTINUE                                                                 RADCLR.218
C                                                                                RADCLR.219
C COMPUTE NEXT LAYER DELTA-EDDINGTON SOLUTION ONLY IF TOTAL TRANSMISSION         RADCLR.220
C OF RADIATION TO THE INTERFACE JUST ABOVE THE LAYER EXCEEDS TRMIN.              RADCLR.221
C                                                                                RADCLR.222
#ifndef MPP1                                                                     RADCLR.223
        CALL WHENFGT(IPLON,TOTTRN(1,K),1,TRMIN,INDEX,NVAL)                       RADCLR.224
#else                                                                            RADCLR.225
        CALL WHENFGT(IPLON,TOTTRN(NOFLIC(1),K),1,TRMIN,INDEX,NVAL)               RADCLR.226
#endif                                                                           RADCLR.227
        IF(NVAL.GT.0)THEN                                                        RADCLR.228
          DO 60 II=1,NVAL                                                        RADCLR.229
#ifndef MPP1                                                                     RADCLR.230
            I=INDEX(II)                                                          RADCLR.231
#else                                                                            RADCLR.232
            I=INDEX(NOFLIC(II))                                                  RADCLR.233
#endif                                                                           RADCLR.234
C                                                                                RADCLR.235
C REMEMBER, NO OZONE ABSORPTION IN THIS LAYER:                                   RADCLR.236
C                                                                                RADCLR.237
            TAURAY(I)=TRAYOSLP*PFLX(I,IPLEVP)                                    RADCLR.238
            TAUGAB(I)=ABH2O*UTH2O(I)+ABCO2*UTCO2(I)+ABO2*UTO2(I)                 RADCLR.239
C                                                                                RADCLR.240
            TAUTOT=TAURAY(I)+TAUGAB(I)                                           RADCLR.241
            WTOT=(WRAY*TAURAY(I))/TAUTOT                                         RADCLR.242
            GTOT=GRAY                                                            RADCLR.243
            FTOT=FRAY                                                            RADCLR.244
C                                                                                RADCLR.245
            TS=TAUS(WTOT,FTOT,TAUTOT)                                            RADCLR.246
            WS=OMGS(WTOT,FTOT)                                                   RADCLR.247
            GS=ASYS(GTOT,FTOT)                                                   RADCLR.248
            LM=EL(WS,GS)                                                         RADCLR.249
C                                                                                RADCLR.250
C ADJUST COSZRS IF NECESSARY TO AVOID DIVIDE BY ZERO                             RADCLR.251
C                                                                                RADCLR.252
            MU0=AMAX1(COSZRS(I),EPS)                                             RADCLR.253
            LLMM=LM*LM*MU0*MU0                                                   RADCLR.254
C                                                                                RADCLR.255
            IF(LLMM.GT.1.-EPS.AND.LLMM.LE.1.)THEN                                RADCLR.256
              MU0=MU0-EPS                                                        RADCLR.257
            ENDIF                                                                RADCLR.258
            IF(LLMM.LT.1.+EPS.AND.LLMM.GT.1.)THEN                                RADCLR.259
              MU0=MU0+EPS                                                        RADCLR.260
            ENDIF                                                                RADCLR.261
C                                                                                RADCLR.262
            ALP=ALPHAX(WS,MU0,GS,LM)                                             RADCLR.263
            GAM=GAMMAX(WS,MU0,GS,LM)                                             RADCLR.264
            UE=U(WS,GS,LM)                                                       RADCLR.265
C                                                                                RADCLR.266
C LIMIT ARGUMENT OF EXPONENTIAL TO 25, INCASE LM VERY LARGE:                     RADCLR.267
C                                                                                RADCLR.268
            ARG=AMIN1(LM*TS,25.)                                                 RADCLR.269
            EXTINS=EXP(-ARG)                                                     RADCLR.270
            NE=N(UE,EXTINS)                                                      RADCLR.271
C                                                                                RADCLR.272
            RDIF(I,K)=(UE+1.)*(UE-1.)*(1./EXTINS-EXTINS)/NE                      RADCLR.273
            TDIF(I,K)=4.*UE/NE                                                   RADCLR.274
C                                                                                RADCLR.275
C LIMIT ARGUMENT OF EXPONENTIAL TO 25, INCASE COSZRS IS VERY SMALL:              RADCLR.276
C                                                                                RADCLR.277
            ARG=AMIN1(TS/MU0,25.)                                                RADCLR.278
            EXPLAY(I,K)=EXP(-ARG)                                                RADCLR.279
C                                                                                RADCLR.280
            APG=ALP+GAM                                                          RADCLR.281
            AMG=ALP-GAM                                                          RADCLR.282
            RDIR(I,K)=AMG*(TDIF(I,K)*EXPLAY(I,K)-1.)+APG*RDIF(I,K)               RADCLR.283
            TDIR(I,K)=APG*TDIF(I,K)+(AMG*RDIF(I,K)-(APG-1.))*EXPLAY(I,K)         RADCLR.284
C                                                                                RADCLR.285
C UNDER RARE CONDITIONS, REFLECTIVIES AND TRANSMISSIVITIES CAN BE                RADCLR.286
C NEGATIVE; ZERO OUT ANY NEGATIVE VALUES                                         RADCLR.287
C                                                                                RADCLR.288
            RDIR(I,K)=AMAX1(RDIR(I,K),0.0)                                       RADCLR.289
            TDIR(I,K)=AMAX1(TDIR(I,K),0.0)                                       RADCLR.290
            RDIF(I,K)=AMAX1(RDIF(I,K),0.0)                                       RADCLR.291
            TDIF(I,K)=AMAX1(TDIF(I,K),0.0)                                       RADCLR.292
   60     CONTINUE                                                               RADCLR.293
        ENDIF                                                                    RADCLR.294
C                                                                                RADCLR.295
   30 CONTINUE                                                                   RADCLR.296
C                                                                                RADCLR.297
C COMPUTE TOTAL DIRECT BEAM TRANSMISSION, TOTAL TRANSMISSION, AND                RADCLR.298
C REFLECTIVITY FOR DIFFUSE RADIATION (FROM BELOW) FOR BOTH LAYERS                RADCLR.299
C ABOVE THE SURFACE:                                                             RADCLR.300
C                                                                                RADCLR.301
      K=2                                                                        RADCLR.302
      DO 70 NN=1,NLOOP                                                           RADCLR.303
        DO 80 I=IS(NN),IE(NN)                                                    RADCLR.304
          EXPTDN(I,K)=EXPTDN(I,K-1)*EXPLAY(I,K-1)                                RADCLR.305
          RDENOM=1./(1.-RDIF(I,K-1)*RDNDIF(I,K-1))                               RADCLR.306
          RDIREXP=RDIR(I,K-1)*EXPTDN(I,K-1)                                      RADCLR.307
          TDNMEXP=TOTTRN(I,K-1)-EXPTDN(I,K-1)                                    RADCLR.308
          TOTTRN(I,K)=EXPTDN(I,K-1)*TDIR(I,K-1)+TDIF(I,K-1)*(TDNMEXP+            RADCLR.309
     +                RDNDIF(I,K-1)*RDIREXP)*RDENOM                              RADCLR.310
          RDNDIF(I,K)=RDIF(I,K-1)+(RDNDIF(I,K-1)*TDIF(I,K-1))*(                  RADCLR.311
     +                TDIF(I,K-1)*RDENOM)                                        RADCLR.312
   80   CONTINUE                                                                 RADCLR.313
   70 CONTINUE                                                                   RADCLR.314
C                                                                                RADCLR.315
C DONE                                                                           RADCLR.316
C                                                                                RADCLR.317
      RETURN                                                                     RADCLR.318
      END                                                                        RADCLR.319
                                                                                 RADCLR.320
