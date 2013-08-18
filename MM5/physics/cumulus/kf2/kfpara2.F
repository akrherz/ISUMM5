      SUBROUTINE KFPARA2(NCUYES,ICUYES,J,LSB,NSHALL,INEST,TBASE,                 KFPARA2.1
     &      U3D,V3D,PP3D,T3D,QV3D,PSB)                                           KFPARA2.2
c                                                                                KFPARA2.3
c                                                                                KFPARA2.4
c...NCUYES:  The number of grid points to check on this J-slice                  KFPARA2.5
c...ICUYES:  Array containing the I-values for grid points to check              KFPARA2.6
c...     J:  J value                                                             KFPARA2.7
c...   LSB:  Array containing L values for each NCUYES point at which            KFPARA2.8
c...         checks for convective initaition should begin.                      KFPARA2.9
c...NSHALL:  Counter for number of shallow convection points activated           KFPARA2.10
c                                                                                KFPARA2.11
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         KFPARA2.12
C                                                                      C         KFPARA2.13
C        THIS SUBROUTINE COMPUTES THE EFFECTS OF DEEP CONVECTION USING C         KFPARA2.14
C   THE KAIN-FRITSCH CONVECTIVE PARAMETERIZATION SCHEME.               C         KFPARA2.15
C                                                 MKS UNITS.           C         KFPARA2.16
C                                                                      C         KFPARA2.17
C MM5 INPUT:   TEMPERTURE (T0, K) ;    SPECIFIC HUMIDITY (Q0, KG/KG) ; C         KFPARA2.18
C              HORIZONTAL WIND SPEED (U0 AND V0, M/S) ;                C         KFPARA2.19
C              PRESSURE (P0, PASCAL) ;  VERTICAL MOTION (W0AVG, M/S).  C         KFPARA2.20
C MM5 OUTPUT:  CONVECTIVE TEMPERATURE (DTDT), WATER VAPOR (DQDT),      C         KFPARA2.21
C              CLOUD WATER (DQLDT), CLOUD ICE (DQIDT),                 C         KFPARA2.22
C              RAIN WATER (DQRDT),  SNOW (DQSDT), AND                  C         KFPARA2.23
C              RAINFALL (RAINCV) TENDENCIES.                           C         KFPARA2.24
C                                                                      C         KFPARA2.25
C**********************************************************************C         KFPARA2.26
C  ETA interface coded originally by Dave Stensrud;                    C         KFPARA2.27
C      modified by Jack Kain                                           C         KFPARA2.28
C                                                                      C         KFPARA2.29
C ETA INPUT:   TEMPERATURE (T, K);    SPECIFIC HUMIDITY (Q, KG/KG)     C         KFPARA2.30
C              PD, RES, AND PT USED TO CALCULATE PRESSURE (PASCAL)     C         KFPARA2.31
C              HORIZONTAL WIND SPEED (U AND V,  M/S)                   C         KFPARA2.32
C              HORIZONTAL GRID SPACING (DX,  M)                        C         KFPARA2.33
C              MODEL TIME STEP (DT,  SECONDS)                          C         KFPARA2.34
C              NUMBER OF TIME STEPS INTEGRATED (NTSD,  NO UNITS)       C         KFPARA2.35
C              INSTANTANEOUS SURFACE SENSIBLE HEAT FLUX (TWBS, W/M^2)  C         KFPARA2.36
C              INSTANTANEOUS SURFACE LATENT HEAT FLUX (QWBS, W/M^2)    C         KFPARA2.37
C              ETA MODEL LEVELS (ETA,  NO UNITS)                       C         KFPARA2.38
C              HALF ETA MODEL LEVELS (AETA, NO UNITS)                  C         KFPARA2.39
C              MODEL LEVEL OF TOP OF PBL (PBLLEV)                      C         KFPARA2.40
C              DIMENSIONS OF MODEL GRID (IMX,JMX,KMX FOR IM,JM,LM)     C         KFPARA2.41
C              ARRAYS SPECIFYING WHERE H AND V ARRAYS ARE BELOW        C         KFPARA2.42
C                                GROUND (LMH AND VMH,  NO UNITS)       C         KFPARA2.43
C                                                                      C         KFPARA2.44
C    OUTPUT:   CONVECTIVE TENDENCIES OF TEMPERATURE (DTDT), WATER      C         KFPARA2.45
C              VAPOR (DQDT), CLOUD WATER (DQCDT), RAIN WATER (DQRDT)   C         KFPARA2.46
C   !!!!! NOTE:  cloud water and rainwater arrays (DQCDT and DQRDT)    C         KFPARA2.47
C                arrays removed for implementation in ETA model !!!!!!!C         KFPARA2.48
C              AND PRECIPITATION RATE (RAINCV)                         C         KFPARA2.49
C              HORIZ. LOCATION OF ACTIVE CONVECTION (NCA, NO UNITS)    C         KFPARA2.50
C                                                                      C         KFPARA2.51
C**********************************************************************C         KFPARA2.52
C                                                                      C         KFPARA2.53
C                               KF scheme documented by                C         KFPARA2.54
C                               Jack Kain                              C         KFPARA2.55
C                               NSSL                                   C         KFPARA2.56
C                               jack.kain@noaa.gov                     C         KFPARA2.57
C                               JUNE 1997                              C         KFPARA2.58
C                                                                      C         KFPARA2.59
C                              Original adaptation for Eta model:      C         KFPARA2.60
C                                                                      C         KFPARA2.61
C                                   MODIFIED TO USE IN THE NMC ETA     C         KFPARA2.62
C                                   MODEL BY DAVID STENSRUD, NSSL      C         KFPARA2.63
C                                   APRIL 20, 1995                     C         KFPARA2.64
C                                                                      C         KFPARA2.65
C                                   MODIFIED FOR USE IN THE "BUNDLE"   C         KFPARA2.66
C                                   BY GEOFF MANIKIN                   C         KFPARA2.67
C                                   DECEMBER 1996                      C         KFPARA2.68
C                                                                      C         KFPARA2.69
C     REFERENCES:                                                      C         KFPARA2.70
C                                                                      C         23DEC04.3156
C         KAIN (2004):  "The Kain-Fritsch Convective Parameterization: C         23DEC04.3157
C         An update.  Journ. Appl. Meteor., 170-181                    C         23DEC04.3158
C                                                                      C         KFPARA2.71
C         KAIN AND FRITSCH (1993):  "CONVECTIVE PARAMETERIZATION IN    C         KFPARA2.72
C         MESOSCALE MODELS:  THE KAIN-FRITSCH SCHEME" IN THE REPRESEN- C         KFPARA2.73
C         TATION OF CUMULUS CONVECTION IN NUMERICAL MODELS, A.M.S.     C         KFPARA2.74
C         MONOGRAPH, K.A. EMANUEL AND D.J. RAYMOND, EDS., 165-170.     C         KFPARA2.75
C                                                                      C         KFPARA2.76
C         FRITSCH AND KAIN (1993):  "CONVECTIVE PARAMETERIZATION IN    C         KFPARA2.77
C         MESOSCALE MODELS:  THE FRITSCH-CHAPPELL SCHEME" IN THE REP-  C         KFPARA2.78
C         RESENTATION OF CUMULUS CONVECTION IN NUMERICAL MODELS, A.M.S.C         KFPARA2.79
C         MONOGRAPH, K.A. EMANUEL AND D.J. RAYMOND, EDS., 165-170.     C         KFPARA2.80
C                                                                      C         KFPARA2.81
C         STENSRUD AND FRITSCH (1994), MON. WEA. REV., 2084-2104.      C         KFPARA2.82
C                                                                      C         KFPARA2.83
C         FRITSCH AND CHAPPELL (1980), J. ATMOS. SCI., 1722-1733.      C         KFPARA2.84
C                                                                      C         KFPARA2.85
C                                                                      C         KFPARA2.95
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         KFPARA2.96
C                                                                                KFPARA2.97
#     include <parame.incl>                                                      KFPARA2.98
#     include <various.incl>                                                     KFPARA2.99
#     include <param2.incl>                                                      KFPARA2.100
#     include <param3.incl>                                                      KFPARA2.101
#     include <pmoist.incl>                                                      KFPARA2.102
#     include <varia.incl>                                                       KFPARA2.103
#     include <addrcu.incl>                                                      KFPARA2.104
#     include <rpstar.incl>                                                      KFPARA2.105
#     include <parkfcu2.incl>                                                    KFPARA2.106
      COMMON/VAPPRS/ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE,XLS0,XLS1            KFPARA2.107
      INTEGER   ICUYES(MIX)                                                      KFPARA2.108
      DIMENSION LSB(MIX),QUER(KX),TKE(KX)                                        KFPARA2.109
      REAL      CLDHGT(KX),QSD(KX),DILFRC(KX),DDILFRC(KX)                        KFPARA2.110
      REAL      THTEEG(KX),TGU(KX),QGU(KX)                                       KFPARA2.111
C                                                                                KFPARA2.112
C...INPUT VARIABLES                                                              KFPARA2.113
C                                                                                KFPARA2.114
      REAL      U3D(MIX,MJX,MKX),  V3D(MIX,MJX,MKX),                             KFPARA2.115
     &          T3D(MIX,MJX,MKX), QV3D(MIX,MJX,MKX),                             KFPARA2.116
     &         PP3D(MIX,MJX,MKX),  PSB(MIX,MJX)                                  KFPARA2.117
C                                                                                KFPARA2.118
C                                                                                KFPARA2.119
C...DEFINE LOCAL VARIABLES...                                                    KFPARA2.120
C                                                                                KFPARA2.121
      REAL      P0(KXP1),Z00(KX),T0(KX),TV0(KX),Q0(KXP1),U0(KX),V0(KX),          KFPARA2.122
     2  TU(KX),TVU(KX),QU(KX),TZ(KX),TVD(KX),QD(KX),QES(KX),THTES(KX),           KFPARA2.123
     3  TG(KX),TVG(KX),QG(KX),WU(KX),WD(KX),EMS(KX),EMSD(KX),W0(KX),             KFPARA2.124
     3  UMF(KX) ,UER(KX) ,UDR(KX) ,DMF(KX) ,DER(KX) ,DDR(KX) ,DZQ(KX),           KFPARA2.125
     5  UMF2(KX),UER2(KX),UDR2(KX),DMF2(KX),DER2(KX),DDR2(KX),DZA(KX),           KFPARA2.126
     6  THTA0(KXP1),THETEE(KX),THTAU(KX),THETEU(KX),THTAD(KX),                   KFPARA2.127
     6  THETED(KX),                                                              KFPARA2.128
     7  QLIQ(KX),QICE(KX),QLQOUT(KX),QICOUT(KX),PPTLIQ(KX),PPTICE(KX),           KFPARA2.129
     8  DETLQ(KX),DETIC(KX),DETLQ2(KX),DETIC2(KX),RATIO(KX),RATIO2(KX)           KFPARA2.130
c!!REMEMBER!!! change Z0 to Z00 !!!                                              KFPARA2.131
                                                                                 KFPARA2.132
      REAL      DOMGDP(KX),EXN(KX),RHOE(KX),TVQU(KX),DP(KX),RH(KX),              KFPARA2.133
     2  EQFRC(KX),WSPD(KX),QDT(KX),FXM(KX),THTAG(KX),THTESG(KX),                 KFPARA2.134
     3  THPA(KX),THFXIN(KX),THFXOUT(KX),QPA(KX),QFXIN(KX),QFXOUT(KX),            KFPARA2.135
     4  QLPA(KX),QLFXIN(KX),QLFXOUT(KX),QIPA(KX),QIFXIN(KX),QIFXOUT(KX),         KFPARA2.136
     5  QRPA(KX),QRFXIN(KX),QRFXOUT(KX),QSPA(KX),QSFXIN(KX),QSFXOUT(KX),         KFPARA2.137
     6  QL0(KX),QLG(KX),QI0(KX),QIG(KX),QR0(KX),QRG(KX),QS0(KX),QSG(KX)          KFPARA2.138
      REAL      OMG(KXP1)                                                        KFPARA2.139
      REAL      RAINFB(KX),SNOWFB(KX)                      ! PPT FB MODS         KFPARA2.140
      LOGICAL IPRNT                                                              KFPARA2.141
      REAL      PPBS(MIX,MKX+1),TBASE(MIXNH,MJXNH,MKXNH)                         KFPARA2.142
#ifdef MPP1                                                                      KFPARA2.143
      integer ii,ic                                                              KFPARA2.144
#endif                                                                           KFPARA2.145
C                                                                                KFPARA2.146
      DATA P00,T00/1.E5,273.16/                                                  KFPARA2.147
      DATA CV,B61,RLF/717.,0.608,3.339E5/                                        KFPARA2.148
      DATA RHIC,RHBC/1.,0.90/                                                    KFPARA2.149
      DATA PIE,TTFRZ,TBFRZ,C5/3.141592654,268.16,248.16,1.0723E-3/               KFPARA2.150
      DATA RATE/0.01/                                                            KFPARA2.151
      GDRY=-G/CP                                                                 KFPARA2.152
C                                                                                KFPARA2.153
C...DEFINE CONSTANTS NEEDED FOR KFPARA SUBROUTINE                                KFPARA2.154
C                                                                                KFPARA2.155
C****************************************************************************    KFPARA2.156
C                                                      ! PPT FB MODS             KFPARA2.157
C...OPTION TO FEED CONVECTIVELY GENERATED RAINWATER    ! PPT FB MODS             KFPARA2.158
C...INTO GRID-RESOLVED RAINWATER (OR SNOW/GRAUPEL)     ! PPT FB MODS             KFPARA2.159
C...FIELD.  'FBFRC' IS THE FRACTION OF AVAILABLE       ! PPT FB MODS             KFPARA2.160
C...PRECIPITATION TO BE FED BACK (0.0 - 1.0)...        ! PPT FB MODS             KFPARA2.161
      FBFRC=0.0                                        ! PPT FB MODS             KFPARA2.162
C**********************************************************                      KFPARA2.163
C...LOOP OVER ALL POTENTIAL CONVECTIVE GRID POINTS...                            KFPARA2.164
C**********************************************************                      KFPARA2.165
C                                                                                KFPARA2.166
       NCCNT=0                                                                   KFPARA2.167
       NCLDCK = NINT(1200./DT2)                                                  KFPARA2.168
       IPRNT=.FALSE.                                                             KFPARA2.169
c      IPRNT=.TRUE.                                                              KFPARA2.170
c      write(98,*)'inside KFPARA; J, NCUYES =', j,ncuyes                         KFPARA2.171
c                                                                                KFPARA2.172
c...specify downdraft mass flux as fraction of updraft mass flux (DMFFRC):       KFPARA2.173
c                                                                                KFPARA2.174
       NLEV=0                                                                    KFPARA2.175
       DMFFRC=0.9                                                                KFPARA2.176
c      write(6,*)'inside KFPARA; NCUYES =',ncuyes                                KFPARA2.177
c      call flush(6)                                                             KFPARA2.178
       DO 325 NC = 1,NCUYES                                                      KFPARA2.179
c...mods to allow shallow convection...                                          KFPARA2.180
          NCHM = 0                                                               KFPARA2.181
          ISHALL = 0                                                             KFPARA2.182
          RAD = 1500.                                                            KFPARA2.183
          FBFRC = 0.                                                             KFPARA2.184
          DPMIN = 5.E3                                                           KFPARA2.185
c...                                                                             KFPARA2.186
#ifndef MPP1                                                                     KFPARA2.187
          I = ICUYES(NC)                                                         KFPARA2.188
#else                                                                            KFPARA2.189
                                                                                 KFPARA2.190
C_FLIC_BEGIN_NOFLIC                                                              KFPARA2.191
          ic = ICUYES(nc)                                                        KFPARA2.192
C_FLIC_END_NOFLIC                                                                KFPARA2.193
                                                                                 KFPARA2.194
          ii = FLIC_L2G_M(ic)                                                    KFPARA2.195
          do i = ii,ii                                                           KFPARA2.196
#endif                                                                           KFPARA2.197
c      IF(INEST.EQ.2 .and. I.EQ.18 .AND. J.EQ.35)THEN                            KFPARA2.198
c        IPRNT=.TRUE.                                                            KFPARA2.199
c      ELSE                                                                      KFPARA2.200
         IPRNT=.FALSE.                                                           KFPARA2.201
c      ENDIF                                                                     KFPARA2.202
   10  CONTINUE                                                                  KFPARA2.203
       P300=1000.*(PSB(I,J)*A(KL)+PTOP-30.)+PP3D(I,J,KL)                         KFPARA2.204
C                                                                                KFPARA2.205
C...PRESSURE PERTURBATION TERM IS ONLY DEFINED AT MID-POINT OF                   KFPARA2.206
C...VERTICAL LAYERS...SINCE TOTAL PRESSURE IS NEEDED AT THE TOP AND              KFPARA2.207
C...BOTTOM OF LAYERS BELOW, DO AN INTERPOLATION...                               KFPARA2.208
C                                                                                KFPARA2.209
          DO 5 K=2,KX                                                            KFPARA2.210
            PPBS(I,K)=(PP3D(I,J,K-1)*DSIGMA(K)+PP3D(I,J,K)*DSIGMA(K-1))/         KFPARA2.211
     +                (DSIGMA(K-1)+DSIGMA(K))                                    KFPARA2.212
    5     CONTINUE                                                               KFPARA2.213
          PPBS(I,1)=PP3D(I,J,1)+((PP3D(I,J,1)-PP3D(I,J,2))/(A(1)-A(2))*(         KFPARA2.214
     +              SIGMA(1)-A(1)))                                              KFPARA2.215
          PPBS(I,KX+1)=PP3D(I,J,KX)+((PP3D(I,J,KX-1)-PP3D(I,J,KX))/(             KFPARA2.216
     &                 A(KX-1)-A(KX))*(SIGMA(KX+1)-A(KX)))                       KFPARA2.217
c        P200 = PD(I,J)+PT-2.0E4                                                 KFPARA2.218
c        P300 = PD(I,J)+PT-3.0E4                                                 KFPARA2.219
c        P400 = PD(I,J)+PT-4.0E4                                                 KFPARA2.220
         ML = 0                                                                  KFPARA2.221
C                                                                                KFPARA2.222
C  INPUT A VERTICAL SOUNDING                                                     KFPARA2.223
C                                                                                KFPARA2.224
CDJS  DO LOOP 15 IS CRUCIAL.  THE KF SCHEME IT SET UP TO HAVE LEVELS             KFPARA2.225
CDJS  STARTING WITH 1 AT THE LOWEST MODEL LEVEL AND GOING UP, WHICH IS           KFPARA2.226
CDJS  INVERTED FROM WHAT SIGMA AND ETA COORDINATE MODELS TEND TO USE.            KFPARA2.227
CDJS  THUS, THE FIRST STEP IS TO SWITCH THE ORDER OF THE VARIABLES USED          KFPARA2.228
CDJS  WITHIN THE KF SCHEME.  NOTE THAT THE SCHEME NEEDS THE FOLLOWING            KFPARA2.229
CDJS  VARIABLES:                                                                 KFPARA2.230
C     INPUT:   TEMPERTURE (T0, K) ;    SPECIFIC HUMIDITY (Q0, KG/KG) ; C         KFPARA2.231
C              HORIZONTAL WIND SPEED (U0 AND V0, M/S) ;                C         KFPARA2.232
C              PRESSURE (P0, PASCAL) ;  HEIGHT (Z0, M);                C         KFPARA2.233
C              VERTICAL MOTION (W0, M/S).                              C         KFPARA2.234
CDJS                                                                             KFPARA2.235
        CELL=PTOP*RPSB(I,J)                                                      KFPARA2.236
        DO 15 K=1,KX                                                             KFPARA2.237
          NK=KX-K+1                                                              KFPARA2.238
          P0(K)=1.E3*(A(NK)*PSB(I,J)+PTOP)+PP3D(I,J,NK)                          KFPARA2.239
          T0(K)=T3D(I,J,NK)                                                      KFPARA2.240
          Q0(K)=QV3D(I,J,NK)                                                     KFPARA2.241
c         TKE(K) = Q2(I,J,NK)                                                    KFPARA2.242
C                                                                                KFPARA2.243
C...IF Q0 IS ABOVE SATURATION VALUE, REDUCE IT TO SATURATION LEVEL...            KFPARA2.244
C                                                                                KFPARA2.245
          ES=ALIQ*EXP((BLIQ*T0(K)-CLIQ)/(T0(K)-DLIQ))                            KFPARA2.246
          QES(K)=0.622*ES/(P0(K)-ES)                                             KFPARA2.247
          Q0(K)=AMIN1(QES(K),Q0(K))                                              KFPARA2.248
          IF(Q0(K).LE.0.1E-8) THEN                                               KFPARA2.249
             Q0(K)=0.1E-8                                                        KFPARA2.250
          END IF                                                                 KFPARA2.251
          RH(K) = Q0(K)/QES(K)                                                   KFPARA2.252
c                                                                                KFPARA2.253
          QL0(K)=0.                                                              KFPARA2.254
          QI0(K)=0.                                                              KFPARA2.255
          QR0(K)=0.                                                              KFPARA2.256
          QS0(K)=0.                                                              KFPARA2.257
          DILFRC(K) = 1.                                                         KFPARA2.258
C                                                                                KFPARA2.259
          U0(K)=.25*(U3D(I,J,NK)+U3D(I+1,J,NK)+U3D(I,J+1,NK)+                    KFPARA2.260
     &          U3D(I+1,J+1,NK))                                                 KFPARA2.261
          V0(K)=.25*(V3D(I,J,NK)+V3D(I+1,J,NK)+V3D(I,J+1,NK)+                    KFPARA2.262
     &          V3D(I+1,J+1,NK))                                                 KFPARA2.263
          TV0(K) = T0(K)*(1.+0.608*Q0(K))                                        KFPARA2.264
          RHOE(K) = P0(K)/(R*TV0(K))                                             KFPARA2.265
          W0(K) = W0AVG(I,J,K)                                                   KFPARA2.266
          DP(K)=(1.E3*(SIGMA(NK+1)*PSB(I,J)+PTOP)+PPBS(I,NK+1))-(1.E3*(          KFPARA2.267
     +          SIGMA(NK)*PSB(I,J)+PTOP)+PPBS(I,NK))                             KFPARA2.268
          DZQ(K)=ROVG*TBASE(I,J,NK)*                                             KFPARA2.269
     +           ALOG((SIGMA(NK+1)+CELL)/(SIGMA(NK)+CELL))                       KFPARA2.270
C                                                                                KFPARA2.271
C...DZQ IS DZ BETWEEN ETA SURFACES, DZA IS DZ BETWEEN MODEL HALF LEVEL           KFPARA2.272
C   DP IS THE PRESSURE INTERVAL BETWEEN FULL ETA LEVELS...                       KFPARA2.273
C                                                                                KFPARA2.274
C          IF (P0(K).GE.500E2) L5 = K 
           IF (P0(K).GE.0.5*P0(1)) L5 = K
           IF (P0(K).GE.400E2) L4 = K                                            KFPARA2.276
           IF (P0(K).GE.P300) LLFC = K         ! jsk 9/19/97                     KFPARA2.277
c          IF (P0(K).GE.P400) LLFC = K         ! jsk 9/19/97                     KFPARA2.278
c          IF (P0(K).GE.P200) LLFC = K         ! jsk 4/18/98                     KFPARA2.279
           IF (T0(K).GT.T00) ML=K                                                KFPARA2.280
           CLDHGT(K) = 0.                                                        KFPARA2.281
  15    CONTINUE                                                                 KFPARA2.282
C                                                                                KFPARA2.283
        Z00(1)=.5*DZQ(1)                                                         KFPARA2.284
        DO 20 K=2,KL                                                             KFPARA2.285
          Z00(K)=Z00(K-1)+.5*(DZQ(K)+DZQ(K-1))                                   KFPARA2.286
          DZA(K-1)=Z00(K)-Z00(K-1)                                               KFPARA2.287
 20     CONTINUE                                                                 KFPARA2.288
        DZA(KL)=0.                                                               KFPARA2.289
        nlooP = 0.                                                               KFPARA2.290
C                                                                                KFPARA2.291
        KMIX=LSB(I)                                                              KFPARA2.292
   25   LOW=KMIX                                                                 KFPARA2.293
        NLOOP = NLOOP+1                                                          KFPARA2.294
c       IF(NLOOP.GT.50)THEN                                                      KFPARA2.295
c         print *,'i, j, LOW, ISHALL, NCHM =',I,J,LOW,ISHALL,NCHM                KFPARA2.296
c         iF(NLOOP.GT.100)THEN                                                   KFPARA2.297
c            GOTO 265                                                            KFPARA2.298
cc            STOP 'NLOOP'                                                       KFPARA2.299
c         ENDIF                                                                  KFPARA2.300
c       ENDIF                                                                    KFPARA2.301
c       if(iprnt)then                                                            KFPARA2.302
c         write(98,*),'LOW, KMIX, LLFC, ISHALL, LSB =',                          KFPARA2.303
c    *                 LOW, KMIX, LLFC, ISHALL, LSB(I)                           KFPARA2.304
c         call flush(98)                                                         KFPARA2.305
c       endif                                                                    KFPARA2.306
c                                                                                KFPARA2.307
C...IF PARCEL ORIGINATES FROM 300 MB ABOVE SURFACE OR HIGHER THEN SCHEME         KFPARA2.308
C   IS NOT USED - CHECK NEXT GRID POINT                                          KFPARA2.309
C                                                                                KFPARA2.310
c...mods for shallow convection...                                               KFPARA2.311
c        IF(LOW.GT.LLFC)GOTO 325                                                 KFPARA2.312
        IF(LOW.GT.LLFC)THEN                                                      KFPARA2.313
          IF(ISHALL.EQ.1)THEN                                                    KFPARA2.314
c            KMIX = LSB(I)                                                       KFPARA2.315
             CHMAX = 0.                                                          KFPARA2.316
             NCHM = 0                                                            KFPARA2.317
             DO 527 NK = 1,LLFC                                                  KFPARA2.318
               IF(CLDHGT(NK).GT.CHMAX)THEN                                       KFPARA2.319
                 NCHM = NK                                                       KFPARA2.320
                 CHMAX = CLDHGT(NK)                                              KFPARA2.321
               ENDIF                                                             KFPARA2.322
 527         CONTINUE                                                            KFPARA2.323
             KMIX = NCHM                                                         KFPARA2.324
            GOTO 25                                                              KFPARA2.325
          ENDIF                                                                  KFPARA2.326
          GOTO 325                                                               KFPARA2.327
         ENDIF                                                                   KFPARA2.328
c...                                                                             KFPARA2.329
        LC = LOW                                                                 KFPARA2.330
        IF(ISHALL.EQ.1 .and. LC.EQ.NCHM)THEN                                     KFPARA2.331
c        RAD = 300.                                                              KFPARA2.332
c        RAD = 1500.                                                             KFPARA2.333
         FBFRC = 1.                                                              KFPARA2.334
c...3_24_99...comment out statemnet below so that shallow cloud draws            KFPARA2.335
c...          from same layer as deep cloud - to prevent shallow cloud           KFPARA2.336
c...          from becoming too deep!                                            KFPARA2.337
c                                                                                KFPARA2.338
c        DPMIN = 2.5E3                                                           KFPARA2.339
        ENDIF                                                                    KFPARA2.340
c       if(ishall.eq.1)print *,'LC, NCHM =',lc,nchm                              KFPARA2.341
C                                                                                KFPARA2.342
C...ASSUME THAT IN ORDER TO SUPPORT A DEEP UPDRAFT YOU NEED A LAYER OF           KFPARA2.343
C...UNSTABLE AIR 50 TO 100 mb DEEP...TO APPROXIMATE THIS, ISOLATE A              KFPARA2.344
C...GROUP OF ADJACENT INDIVIDUAL MODEL LAYERS, WITH THE BASE AT LEVEL            KFPARA2.345
C...LC, SUCH THAT THE COMBINED DEPTH OF THESE LAYERS IS AT LEAST 60 mb..         KFPARA2.346
C                                                                                KFPARA2.347
C                                                                                KFPARA2.348
        NLAYRS=0                                                                 KFPARA2.349
        DPTHMX=0.                                                                KFPARA2.350
        DO 63 NK=LC,KX                                                           KFPARA2.351
          DPTHMX=DPTHMX+DP(NK)                                                   KFPARA2.352
          NLAYRS=NLAYRS+1                                                        KFPARA2.353
c         IF(RH(NK).ge.0.75 .and. RH(NK).le.0.95)then                            KFPARA2.354
c           QEF = 0.25*(RH(NK)-0.75)                                             KFPARA2.355
c         ELSEIF(RH(NK).GT.0.95)THEN                                             KFPARA2.356
c           QEF = 1./RH(NK)-1.                                                   KFPARA2.357
c         ELSE                                                                   KFPARA2.358
            QEF = 0.                                                             KFPARA2.359
c         ENDIF                                                                  KFPARA2.360
          QUER(NK) = Q0(NK)*(1.+QEF)                                             KFPARA2.361
C         IF(DPTHMX.GT.6.E3)GOTO 64                                              KFPARA2.362
          IF(DPTHMX.GT.DPMIN)GOTO 64                                             KFPARA2.363
 63     CONTINUE                                                                 KFPARA2.364
        GOTO 325                                                                 KFPARA2.365
   64   KPBL=LC+NLAYRS-1                                                         KFPARA2.366
c                                                                                KFPARA2.367
c...go ahead and determine what level to start with for the                      KFPARA2.368
c...next mixture in case the current mixture, with base at                       KFPARA2.369
c...level LC, is not buoyant...                                                  KFPARA2.370
C...instead of checking mixtures using every single layer,                       KFPARA2.371
c...move up in increments of at least 25 mb...                                   KFPARA2.372
C...!!! make that 20 mb !!!!!! 9/15/97...                                        KFPARA2.373
c         KMIX=LC+1                                                              KFPARA2.374
c        PM25 = P0(LC)-25.E2                                                     KFPARA2.375
c        PM20 = P0(LC)-20.E2                                                     KFPARA2.376
         PM15 = P0(LC)-15.E2                                                     KFPARA2.377
         DO 66 NK = LC+1,KL                                                      KFPARA2.378
c           IF(P0(NK).LT.PM25)THEN                                               KFPARA2.379
c           IF(P0(NK).LT.PM20)THEN                                               KFPARA2.380
            IF(P0(NK).LT.PM15)THEN                                               KFPARA2.381
             KMIX = NK                                                           KFPARA2.382
             GOTO 267                                                            KFPARA2.383
           ENDIF                                                                 KFPARA2.384
 66      CONTINUE                                                                KFPARA2.385
         GOTO 325                                                                KFPARA2.386
 267     CONTINUE                                                                KFPARA2.387
c                                                                                KFPARA2.388
c...********************************************************                     KFPARA2.389
c...for computational simplicity without much loss in accuracy,                  KFPARA2.390
c...mix temperature instead of theta for evaluating convective                   KFPARA2.391
c...initiation (triggering) potential...                                         KFPARA2.392
c        THMIX=0.                                                                KFPARA2.393
        TMIX=0.                                                                  KFPARA2.394
        QMIX=0.                                                                  KFPARA2.395
        ZMIX=0.                                                                  KFPARA2.396
        PMIX=0.                                                                  KFPARA2.397
c        DPTHMX=0.                                                               KFPARA2.398
C                                                                                KFPARA2.399
C...FIND THE THERMODYNAMIC CHARACTERISTICS OF THE LAYER BY                       KFPARA2.400
C...MASS-WEIGHTING THE CHARACTERISTICS OF THE INDIVIDUAL MODEL                   KFPARA2.401
C...LAYERS...                                                                    KFPARA2.402
C                                                                                KFPARA2.403
        DO 17 NK=LC,KPBL                                                         KFPARA2.404
c          DPTHMX=DPTHMX+DP(NK)                                                  KFPARA2.405
c          ROCPQ=0.2854*(1.-0.28*Q0(NK))                                         KFPARA2.406
c          THMIX=THMIX+DP(NK)*T0(NK)*(P00/P0(NK))**ROCPQ                         KFPARA2.407
          TMIX=TMIX+DP(NK)*T0(NK)                                                KFPARA2.408
c          QMIX=QMIX+DP(NK)*Q0(NK)                                               KFPARA2.409
          QMIX=QMIX+DP(NK)*QUER(NK)                                              KFPARA2.410
          ZMIX=ZMIX+DP(NK)*Z00(NK)                                               KFPARA2.411
          PMIX=PMIX+DP(NK)*P0(NK)                                                KFPARA2.412
 17     CONTINUE                                                                 KFPARA2.413
c        THMIX=THMIX/DPTHMX                                                      KFPARA2.414
        TMIX=TMIX/DPTHMX                                                         KFPARA2.415
        QMIX=QMIX/DPTHMX                                                         KFPARA2.416
        ZMIX=ZMIX/DPTHMX                                                         KFPARA2.417
        PMIX=PMIX/DPTHMX                                                         KFPARA2.418
C        ROCPQ=0.2854*(1.-0.28*QMIX)                                             KFPARA2.419
C        TMIX=THMIX*(PMIX/P00)**ROCPQ                                            KFPARA2.420
        EMIX=QMIX*PMIX/(0.622+QMIX)                                              KFPARA2.421
C                                                                                KFPARA2.422
C...FIND THE TEMPERATURE OF THE MIXTURE AT ITS LCL...                            KFPARA2.423
C                                                                                KFPARA2.424
C        TLOG=ALOG(EMIX/ALIQ)                                                    KFPARA2.425
        astrt=1.e-3                                                              KFPARA2.426
        ainc=0.075                                                               KFPARA2.427
        a1=emix/aliq                                                             KFPARA2.428
        tp=(a1-astrt)/ainc                                                       KFPARA2.429
        indlu=int(tp)+1                                                          KFPARA2.430
        value=(indlu-1)*ainc+astrt                                               KFPARA2.431
        aintrp=(a1-value)/ainc                                                   KFPARA2.432
        tlog=aintrp*alu(indlu+1)+(1-aintrp)*alu(indlu)                           KFPARA2.433
        TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)                                        KFPARA2.434
        TLCL=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(TMIX-T00))*(TMIX-           KFPARA2.435
     +       TDPT)                                                               KFPARA2.436
        TLCL=AMIN1(TLCL,TMIX)                                                    KFPARA2.437
        TVLCL=TLCL*(1.+0.608*QMIX)                                               KFPARA2.438
C       CPORQ=1./ROCPQ                                                           KFPARA2.439
C       PLCL=P00*(TLCL/THMIX)**CPORQ                                             KFPARA2.440
        ZLCL = ZMIX+(TLCL-TMIX)/GDRY                                             KFPARA2.441
        DO 29 NK = LC,KL                                                         KFPARA2.442
          KLCL=NK                                                                KFPARA2.443
C         IF(PLCL.GE.P0(NK))GO TO 35                                             KFPARA2.444
          IF(ZLCL.LE.Z00(NK)) GOTO 35                                            KFPARA2.445
29      CONTINUE                                                                 KFPARA2.446
        GOTO 325                                                                 KFPARA2.447
35      K=KLCL-1                                                                 KFPARA2.448
C        DLP=ALOG(PLCL/P0(K))/ALOG(P0(KLCL)/P0(K))                               KFPARA2.449
C                                                                                KFPARA2.450
C...CALCULATE DLP USING Z INSTEAD OG LOG(P)...                                   KFPARA2.451
C       dlp=alog(plcl/p0(k))/alog(p0(klcl)/p0(k))                                KFPARA2.452
        DLP=(ZLCL-Z00(K))/(Z00(KLCL)-Z00(K))                                     KFPARA2.453
C                                                                                KFPARA2.454
C...ESTIMATE ENVIRONMENTAL TEMPERATURE AND MIXING RATIO AT THE LCL...            KFPARA2.455
C                                                                                KFPARA2.456
        TENV=T0(K)+(T0(KLCL)-T0(K))*DLP                                          KFPARA2.457
        QENV=Q0(K)+(Q0(KLCL)-Q0(K))*DLP                                          KFPARA2.458
        TVEN=TENV*(1.+0.608*QENV)                                                KFPARA2.459
c        TVBAR=0.5*(TV0(K)+TVEN)                                                 KFPARA2.460
C        ZLCL=Z00(K)+R*TVBAR*ALOG(P0(K)/PLCL)/G                                  KFPARA2.461
c        ZLCL=Z00(K)+(Z00(KLCL)-Z00(K))*DLP                                      KFPARA2.462
C        write(98,*)'k, klcl, DLP, ZLCL =',K,KLCL,DLP,ZLCL                       KFPARA2.463
C                                                                                KFPARA2.464
C...CHECK TO SEE IF CLOUD IS BUOYANT USING FRITSCH-CHAPPELL TRIGGER              KFPARA2.465
C...FUNCTION DESCRIBED IN KAIN AND FRITSCH (1992)...W0 IS AN                     KFPARA2.466
C...APROXIMATE VALUE FOR THE RUNNING-MEAN GRID-SCALE VERTICAL                    KFPARA2.467
C...VELOCITY, WHICH GIVES SMOOTHER FIELDS OF CONVECTIVE INITIATION               KFPARA2.468
C...THAN THE INSTANTANEOUS VALUE...FORMULA RELATING TEMPERATURE                  KFPARA2.469
C...PERTURBATION TO VERTICAL VELOCITY HAS BEEN USED WITH THE MOST                KFPARA2.470
C...SUCCESS AT GRID LENGTHS NEAR 25 km.  FOR DIFFERENT GRID-LENGTHS,             KFPARA2.471
C...ADJUST VERTICAL VELOCITY TO EQUIVALENT VALUE FOR 25 KM GRID                  KFPARA2.472
C...LENGTH, ASSUMING LINEAR DEPENDENCE OF W ON GRID LENGTH...                    KFPARA2.473
C                                                                                KFPARA2.474
        IF(ZLCL.LT.2.E3)THEN                                                     KFPARA2.475
          WKLCL=0.02*ZLCL/2.E3                                                   KFPARA2.476
        ELSE                                                                     KFPARA2.477
          WKLCL=0.02                                                             KFPARA2.478
        ENDIF                                                                    KFPARA2.479
c       WKL=(W0(K)+(W0(KLCL)-W0(K))*DLP)*DX(I,J)                                 KFPARA2.480
        WKL=(W0(K)+(W0(KLCL)-W0(K))*DLP)*DX                                      KFPARA2.481
     *      /25.E3-WKLCL                                                         KFPARA2.482
c        write(98,*)'lc, WKL =',LC,WKL                                           KFPARA2.483
        IF(WKL.LT.0.0001)THEN                                                    23DEC04.3159
          DTLCL=0.                                                               23DEC04.3160
        ELSE                                                                     23DEC04.3161
          DTLCL=4.64*WKL**0.33                                                   23DEC04.3162
        ENDIF                                                                    23DEC04.3163
c                                                                                KFPARA2.494
c...for ETA model, give parcel an extra temperature perturbation based           KFPARA2.495
c...the threshold RH for condensation...                                         KFPARA2.496
c                                                                                KFPARA2.497
c...for now, just assume U00=0.75...                                             KFPARA2.498
c...!!!!!! for MM5, SET DTRH = 0. !!!!!!!!                                       KFPARA2.499
c       U00 = 0.75                                                               KFPARA2.500
c       IF(U00.lt.1.)THEN                                                        KFPARA2.501
c         QSLCL=QES(K)+(QES(KLCL)-QES(K))*DLP                                    KFPARA2.502
c         RHLCL = QENV/QSLCL                                                     KFPARA2.503
c         DQSSDT = QMIX*(CLIQ-BLIQ*DLIQ)/((TLCL-DLIQ)*(TLCL-DLIQ))               KFPARA2.504
c         IF(RHLCL.ge.0.75 .and. RHLCL.le.0.95)then                              KFPARA2.505
c           DTRH = 0.25*(RHLCL-0.75)*QMIX/DQSSDT                                 KFPARA2.506
c         ELSEIF(RHLCL.GT.0.95)THEN                                              KFPARA2.507
c           DTRH = (1./RHLCL-1.)*QMIX/DQSSDT                                     KFPARA2.508
c         ELSE                                                                   KFPARA2.509
            DTRH = 0.                                                            KFPARA2.510
c         ENDIF                                                                  KFPARA2.511
c       ENDIF                                                                    KFPARA2.512
C       IF(ISHALL.EQ.1)IPRNT=.TRUE.                                              KFPARA2.513
c       IPRNT=.TRUE.                                                             KFPARA2.514
       IF(IPRNT)THEN                                                             KFPARA2.515
         write(98,*)                                                             KFPARA2.516
         write(98,*)'At t(min), I, J =',xtime,I,J                                KFPARA2.517
         write(98,*)'P(LC), DTP, WKL, WKLCL =',p0(LC)/100.,                      KFPARA2.518
     *                TLCL+DTLCL+dtrh-TENV,WKL,WKLCL                             KFPARA2.519
         write(98,*)'TLCL, DTLCL, DTRH, TENV =',TLCL,DTLCL,                      KFPARA2.520
     *                DTRH,TENV                                                  KFPARA2.521
         write(98,*)'LOW, KMIX, LLFC, ISHALL, LSB =',                            KFPARA2.522
     *                LOW, KMIX, LLFC, ISHALL, LSB(I)                            KFPARA2.523
c       call flush(98)                                                           KFPARA2.524
       ENDIF                                                                     KFPARA2.525
c       IF(TLCL+DTLCL.GT.TENV)GOTO 45                                            KFPARA2.526
        IF(TLCL+DTLCL+DTRH.GT.TENV)GOTO 45                                       KFPARA2.527
        IF(ISHALL.EQ.1 .and. LC.EQ.NCHM)GOTO 325                                 KFPARA2.528
        IF(KMIX.LE.LLFC)GOTO 25                                                  KFPARA2.529
c...shallow convection mods...                                                   KFPARA2.530
        IF(ISHALL.EQ.1)THEN                                                      KFPARA2.531
c          KMIX = LSB(I)                                                         KFPARA2.532
             CHMAX = 0.                                                          KFPARA2.533
             NCHM = 0                                                            KFPARA2.534
             DO 627 NK = 1,LLFC                                                  KFPARA2.535
               IF(CLDHGT(NK).GT.CHMAX)THEN                                       KFPARA2.536
                 NCHM = NK                                                       KFPARA2.537
                 CHMAX = CLDHGT(NK)                                              KFPARA2.538
               ENDIF                                                             KFPARA2.539
 627         CONTINUE                                                            KFPARA2.540
             KMIX = NCHM                                                         KFPARA2.541
          GOTO 25                                                                KFPARA2.542
        ENDIF                                                                    KFPARA2.543
c...                                                                             KFPARA2.544
        GOTO 325                                                                 KFPARA2.545
C                                                                                KFPARA2.546
C...CONVECTIVE TRIGGERING CRITERIA HAS BEEN SATISFIED...COMPUTE                  KFPARA2.547
C...EQUIVALENT POTENTIAL TEMPERATURE                                             KFPARA2.548
C...(THETEU) AND VERTICAL VELOCITY OF THE RISING PARCEL AT THE LCL...            KFPARA2.549
C                                                                                KFPARA2.550
   45   CONTINUE                                                                 KFPARA2.551
        THMIX=TMIX*(1.E5/PMIX)**(0.2854*(1.-0.28*QMIX))                          KFPARA2.552
        THETEU(K)=THMIX*                                                         KFPARA2.553
     +            EXP((3374.6525/TLCL-2.5403)*QMIX*(1.+0.81*QMIX))               KFPARA2.554
        ES=ALIQ*EXP((TENV*BLIQ-CLIQ)/(TENV-DLIQ))                                KFPARA2.555
        TVAVG=0.5*(TV0(KLCL)+TENV*(1.+0.608*QENV))                               KFPARA2.556
c...modify calculation of initial parcel vertical velocity...jsk 11/26/97        KFPARA2.557
        DTTOT = DTLCL+DTRH                                                       23DEC04.3164
        IF(DTTOT.GT.1.E-4)THEN                                                   23DEC04.3165
          GDT=2.*G*DTTOT*500./TVEN                                               23DEC04.3166
          WLCL=1.+0.5*SQRT(GDT)                                                  23DEC04.3167
          WLCL = AMIN1(WLCL,3.)                                                  23DEC04.3168
        ELSE                                                                     23DEC04.3169
          WLCL=1.                                                                23DEC04.3170
        ENDIF                                                                    23DEC04.3171
C        PLCL=P0(KLCL)*EXP(G/(R*TVAVG)*(Z00(KLCL)-ZLCL))                         KFPARA2.566
        PLCL=P0(K)+(P0(KLCL)-P0(K))*DLP                                          KFPARA2.567
c       QESE=0.622*ES/(PLCL-ES)                                                  KFPARA2.568
c       THTES(K)=TENV*(1.E5/PLCL)**(0.2854*(1.-0.28*QESE))*                      KFPARA2.569
c    +           EXP((3374.6525/TENV-2.5403)*QESE*(1.+0.81*QESE))                KFPARA2.570
        WTW=WLCL*WLCL                                                            KFPARA2.571
        IF(WLCL.LT.0.)GOTO 25                                                    KFPARA2.572
        TVLCL=TLCL*(1.+0.608*QMIX)                                               KFPARA2.573
        RHOLCL=PLCL/(R*TVLCL)                                                    KFPARA2.574
        PLCL0 = PLCL                                                             KFPARA2.575
C                                                                                KFPARA2.576
        LCL=KLCL                                                                 KFPARA2.577
        LET=LCL                                                                  KFPARA2.578
c       IF(LC.NE.NCHM)THEN                                                       KFPARA2.579
          IF(WKL.LT.0.)THEN                                                      KFPARA2.580
            RAD = 1000.                                                          KFPARA2.581
          ELSEIF(WKL.GT.0.1)THEN                                                 KFPARA2.582
            RAD = 2000.                                                          KFPARA2.583
          ELSE                                                                   KFPARA2.584
            RAD = 1000.+1000*WKL/0.1                                             KFPARA2.585
          ENDIF                                                                  KFPARA2.586
c       ENDIF                                                                    KFPARA2.587
C                                                                                KFPARA2.588
C*******************************************************************             KFPARA2.589
C                                                                  *             KFPARA2.590
C                 COMPUTE UPDRAFT PROPERTIES                       *             KFPARA2.591
C                                                                  *             KFPARA2.592
C*******************************************************************             KFPARA2.593
C                                                                                KFPARA2.594
C                                                                                KFPARA2.595
c...                                                                             KFPARA2.596
C...ESTIMATE INITIAL UPDRAFT MASS FLUX (UMF(K))...                               KFPARA2.597
C                                                                                KFPARA2.598
        WU(K)=WLCL                                                               KFPARA2.599
        AU0=0.01*DXSQ                                                            19DEC02.1363
        UMF(K)=RHOLCL*AU0                                                        KFPARA2.601
        VMFLCL=UMF(K)                                                            KFPARA2.602
        UPOLD=VMFLCL                                                             KFPARA2.603
        UPNEW=UPOLD                                                              KFPARA2.604
C                                                                                KFPARA2.605
C...RATIO2 IS THE DEGREE OF GLACIATION IN THE CLOUD (0 TO 1),                    KFPARA2.606
C...UER IS THE ENVIR ENTRAINMENT RATE, ABE IS AVAILABLE                          KFPARA2.607
C...BUOYANT ENERGY, TRPPT IS THE TOTAL RATE OF PRECIPITATION                     KFPARA2.608
C...PRODUCTION...                                                                KFPARA2.609
C                                                                                KFPARA2.610
        RATIO2(K)=0.                                                             KFPARA2.611
        UER(K)=0.                                                                KFPARA2.612
        ABE=0.                                                                   KFPARA2.613
        TRPPT=0.                                                                 KFPARA2.614
        TU(K)=TLCL                                                               KFPARA2.615
        TVU(K)=TVLCL                                                             KFPARA2.616
        QU(K)=QMIX                                                               KFPARA2.617
        EQFRC(K)=1.                                                              KFPARA2.618
        QLIQ(K)=0.                                                               KFPARA2.619
        QICE(K)=0.                                                               KFPARA2.620
        QLQOUT(K)=0.                                                             KFPARA2.621
        QICOUT(K)=0.                                                             KFPARA2.622
        DETLQ(K)=0.                                                              KFPARA2.623
        DETIC(K)=0.                                                              KFPARA2.624
        PPTLIQ(K)=0.                                                             KFPARA2.625
        PPTICE(K)=0.                                                             KFPARA2.626
        IFLAG=0                                                                  KFPARA2.627
        KFRZ=LC                                                                  KFPARA2.628
C                                                                                KFPARA2.629
C...THE AMOUNT OF CONV AVAIL POT ENERGY (CAPE) IS CALCULATED WITH                KFPARA2.630
C...RESPECT TO UNDILUTE PARCEL ASCENT; EQ POT TEMP OF UNDILUTE                   KFPARA2.631
C...PARCEL IS THTUDL, UNDILUTE TEMPERATURE IS GIVEN BY TUDL...                   KFPARA2.632
C                                                                                KFPARA2.633
        THTUDL=THETEU(K)                                                         KFPARA2.634
        TUDL=TLCL                                                                KFPARA2.635
C                                                                                KFPARA2.636
C...TTEMP IS USED DURING CALCULATION OF THE LINEAR GLACIATION                    KFPARA2.637
C...PROCESS; IT IS INITIALLY SET TO THE TEMPERATURE AT WHICH                     KFPARA2.638
C...FREEZING IS SPECIFIED TO BEGIN.  WITHIN THE GLACIATION                       KFPARA2.639
C...INTERVAL, IT IS SET EQUAL TO THE UPDRAFT TEMP AT THE                         KFPARA2.640
C...PREVIOUS MODEL LEVEL...                                                      KFPARA2.641
C                                                                                KFPARA2.642
        TTEMP=TTFRZ                                                              KFPARA2.643
C                                                                                KFPARA2.644
C...ENTER THE LOOP FOR UPDRAFT CALCULATIONS...CALCULATE UPDRAFT TEMP,            KFPARA2.645
C...MIXING RATIO, VERTICAL MASS FLUX, LATERAL DETRAINMENT OF MASS AND            KFPARA2.646
C...MOISTURE, PRECIPITATION RATES AT EACH MODEL LEVEL...                         KFPARA2.647
C                                                                                KFPARA2.648
C                                                                                KFPARA2.649
c...                                                                             KFPARA2.650
c...!!! diagnostic stuff !!!!                                                    KFPARA2.651
       PLCL0 = PLCL                                                              KFPARA2.652
       LFC = 0                                                                   KFPARA2.653
       PMIX0 = PMIX                                                              KFPARA2.654
       REI = 0.                                                                  KFPARA2.655
       DILBE = 0.                                                                KFPARA2.656
       DO 60 NK=K,KLM                                                            KFPARA2.657
         NK1=NK+1                                                                KFPARA2.658
         RATIO2(NK1)=RATIO2(NK)                                                  KFPARA2.659
         FRC1=0.                                                                 KFPARA2.660
         TU(NK1)=T0(NK1)                                                         KFPARA2.661
         THETEU(NK1)=THETEU(NK)                                                  KFPARA2.662
         QU(NK1)=QU(NK)                                                          KFPARA2.663
         QLIQ(NK1)=QLIQ(NK)                                                      KFPARA2.664
         QICE(NK1)=QICE(NK)                                                      KFPARA2.665
C         CALL TPMIX(P0(NK1),THETEU(NK1),TU(NK1),QU(NK1),QLIQ(NK1),              KFPARA2.666
C     *   QICE(NK1),QNEWLQ,QNEWIC,RATIO2(NK1),RL,XLV0,XLV1,XLS0,XLS1,            KFPARA2.667
C     *   ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)                               KFPARA2.668
         call tpmix2(p0(nk1),theteu(nk1),tu(nk1),qu(nk1),qliq(nk1),              KFPARA2.669
     1               qice(nk1),qnewlq,qnewic,ratio2(nk1),                        KFPARA2.670
     2               XLV1,XLV0)                                                  KFPARA2.671
C                                                                                KFPARA2.672
C...CHECK TO SEE IF UPDRAFT TEMP IS ABOVE THE TEMPERATURE AT WHICH               KFPARA2.673
C...GLACIATION IS ASSUMED TO INITIATE; IF IT IS, CALCULATE THE                   KFPARA2.674
C...FRACTION OF REMAINING LIQUID WATER TO FREEZE...TTFRZ IS THE                  KFPARA2.675
C...TEMP AT WHICH FREEZING BEGINS, TBFRZ THE TEMP BELOW WHICH ALL                KFPARA2.676
C...LIQUID WATER IS FROZEN AT EACH LEVEL...                                      KFPARA2.677
C                                                                                KFPARA2.678
         IF(TU(NK1).LE.TTFRZ)THEN                                                KFPARA2.679
           IF(TU(NK1).GT.TBFRZ)THEN                                              KFPARA2.680
             IF(TTEMP.GT.TTFRZ)TTEMP=TTFRZ                                       KFPARA2.681
             FRC1=(TTEMP-TU(NK1))/(TTEMP-TBFRZ)                                  KFPARA2.682
           ELSE                                                                  KFPARA2.683
             FRC1=1.                                                             KFPARA2.684
             IFLAG=1                                                             KFPARA2.685
           ENDIF                                                                 KFPARA2.686
           TTEMP=TU(NK1)                                                         KFPARA2.687
C                                                                                KFPARA2.688
C  DETERMINE THE EFFECTS OF LIQUID WATER FREEZING WHEN TEMPERATURE               KFPARA2.689
C...IS BELOW TTFRZ...                                                            KFPARA2.690
C                                                                                KFPARA2.691
           QFRZ = (QLIQ(NK1)+QNEWLQ)*FRC1                                        KFPARA2.692
           QNEWIC=QNEWIC+QNEWLQ*FRC1                                             KFPARA2.693
           QNEWLQ=QNEWLQ-QNEWLQ*FRC1                                             KFPARA2.694
           QICE(NK1) = QICE(NK1)+QLIQ(NK1)*FRC1                                  KFPARA2.695
           QLIQ(NK1) = QLIQ(NK1)-QLIQ(NK1)*FRC1                                  KFPARA2.696
           CALL DTFRZ2(TU(NK1),P0(NK1),THETEU(NK1),QU(NK1),QFRZ,                 KFPARA2.697
     *                 QICE(NK1),ALIQ,BLIQ,CLIQ,DLIQ)                            KFPARA2.698
         ENDIF                                                                   KFPARA2.699
         TVU(NK1)=TU(NK1)*(1.+0.608*QU(NK1))                                     KFPARA2.700
C                                                                                KFPARA2.701
C  CALCULATE UPDRAFT VERTICAL VELOCITY AND PRECIPITATION FALLOUT...              KFPARA2.702
C                                                                                KFPARA2.703
         IF(NK.EQ.K)THEN                                                         KFPARA2.704
           BE=(TVLCL+TVU(NK1))/(TVEN+TV0(NK1))-1.                                KFPARA2.705
           BOTERM=2.*(Z00(NK1)-ZLCL)*G*BE/1.5                                    KFPARA2.706
C          ENTERM=0.                                                             KFPARA2.707
           DZZ=Z00(NK1)-ZLCL                                                     KFPARA2.708
         ELSE                                                                    KFPARA2.709
           BE=(TVU(NK)+TVU(NK1))/(TV0(NK)+TV0(NK1))-1.                           KFPARA2.710
           BOTERM=2.*DZA(NK)*G*BE/1.5                                            KFPARA2.711
c          ENTERM=2.*UER(NK)*WTW/UPOLD                                           KFPARA2.712
           DZZ=DZA(NK)                                                           KFPARA2.713
         ENDIF                                                                   KFPARA2.714
         ENTERM=2.*REI*WTW/UPOLD                                                 KFPARA2.715
c...diagnostics...                                                               KFPARA2.716
         IF(TVU(NK1).GT.TV0(NK1))THEN                                            KFPARA2.717
           IF(TVU(NK).LT.TV0(NK) .or. NK1.EQ.KLCL)LFC=NK1                        KFPARA2.718
         ENDIF                                                                   KFPARA2.719
         WSQ=WTW                                                                 KFPARA2.720
         CALL CONDLOAD(QLIQ(NK1),QICE(NK1),WTW,DZZ,BOTERM,ENTERM,                KFPARA2.721
     *                 RATE,QNEWLQ,QNEWIC,QLQOUT(NK1),QICOUT(NK1))               KFPARA2.722
C                                                                                23DEC04.3172
C...IF VERT VELOCITY IS LESS THAN ZERO, EXIT THE UPDRAFT LOOP AND,               23DEC04.3173
C...IF CLOUD IS TALL ENOUGH, FINALIZE UPDRAFT CALCULATIONS...                    23DEC04.3174
C                                                                                23DEC04.3175
         IF(WTW.LT.1.E-3)THEN                                                    23DEC04.3176
           GOTO 65                                                               23DEC04.3177
         ELSE                                                                    23DEC04.3178
           WU(NK1)=SQRT(WTW)                                                     23DEC04.3179
         ENDIF                                                                   23DEC04.3180
C                                                                                KFPARA2.730
C  UPDATE THE ABE FOR UNDILUTE ASCENT...                                         KFPARA2.731
C                                                                                KFPARA2.732
c        THTES(NK1)=T0(NK1)*(1.E5/P0(NK1))**(0.2854*(1.-0.28*QES(NK1)))*         KFPARA2.733
c    *      EXP((3374.6525/T0(NK1)-2.5403)*QES(NK1)*(1.+0.81*QES(NK1)))          KFPARA2.734
c        UDLBE=((2.*THTUDL)/(THTES(NK)+THTES(NK1))-1.)*DZZ                       KFPARA2.735
c        IF(UDLBE.GT.0.)ABE=ABE+UDLBE*G                                          KFPARA2.736
C                                                                                KFPARA2.737
C...CALL SUBROUTINE TO CALCULATE ENVIRONMENTAL EQUIVALENT POTENTIAL              KFPARA2.738
C...TEMP...WITHIN GLACIATION INTERVAL, THETAE MUST BE CALCULATED                 KFPARA2.739
C...WITH RESPECT TO THE SAME DEGREE OF GLACIATION FOR ALL ENTRAINING             KFPARA2.740
C...AIR...                                                                       KFPARA2.741
C                                                                                KFPARA2.742
c...for lookup table version, calculate thetae with respect to                   KFPARA2.743
c...liquid water at all levels...                                                KFPARA2.744
c        CALL ENVIRTHT(P0(NK1),T0(NK1),Q0(NK1),THETEE(NK1),RATIO2(NK1),          KFPARA2.745
c    *                 RL,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)               KFPARA2.746
       if(nk1.le.kpbl)then                                                       KFPARA2.747
         CALL ENVIRTHT(P0(NK1),T0(NK1),QUER(NK1),THETEE(NK1),0.,                 KFPARA2.748
     *                 RL,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)               KFPARA2.749
       else                                                                      KFPARA2.750
         CALL ENVIRTHT(P0(NK1),T0(NK1),Q0(NK1),THETEE(NK1),0.,                   KFPARA2.751
     *                 RL,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)               KFPARA2.752
       endif                                                                     KFPARA2.753
C                                                                                KFPARA2.754
C...REI IS THE RATE OF ENVIRONMENTAL INFLOW...                                   KFPARA2.755
C                                                                                KFPARA2.756
         REI=VMFLCL*DP(NK1)*0.03/RAD                                             KFPARA2.757
         TVQU(NK1)=TU(NK1)*(1.+0.608*QU(NK1)-QLIQ(NK1)-QICE(NK1))                KFPARA2.758
         IF(NK.EQ.K)THEN                                                         KFPARA2.759
           DILBE=((TVLCL+TVQU(NK1))/(TVEN+TV0(NK1))-1.)*DZZ                      KFPARA2.760
         ELSE                                                                    KFPARA2.761
           DILBE=((TVQU(NK)+TVQU(NK1))/(TV0(NK)+TV0(NK1))-1.)*DZZ                KFPARA2.762
         ENDIF                                                                   KFPARA2.763
         IF(DILBE.GT.0.)ABE=ABE+DILBE*G                                          KFPARA2.764
C                                                                                KFPARA2.765
C...IF CLOUD PARCELS ARE VIRTUALLY COLDER THAN THE ENVIRONMENT, NO               KFPARA2.766
C   ENTRAINMENT IS ALLOWED AT THIS LEVEL...                                      KFPARA2.767
C                                                                                KFPARA2.768
         IF(TVQU(NK1).LE.TV0(NK1))THEN                                           KFPARA2.769
c          UER(NK1)=0.0                                                          KFPARA2.770
c          UDR(NK1)=REI                                                          KFPARA2.771
           UER(NK1)=0.5*REI                                                      KFPARA2.772
           UDR(NK1)=1.5*REI                                                      KFPARA2.773
c          EE2=0.                                                                KFPARA2.774
c          UD2=1.                                                                KFPARA2.775
           EE2=0.5                                                               KFPARA2.776
           UD2=1.                                                                KFPARA2.777
           EQFRC(NK1)=0.                                                         KFPARA2.778
           GOTO 55                                                               KFPARA2.779
         ENDIF                                                                   KFPARA2.780
         LET=NK1                                                                 KFPARA2.781
         TTMP=TVQU(NK1)                                                          KFPARA2.782
C                                                                                KFPARA2.783
C...DETERMINE THE CRITICAL MIXED FRACTION OF UPDRAFT AND ENVIRONMENTAL AIR...    KFPARA2.784
C                                                                                KFPARA2.785
        F1=0.95                                                                  KFPARA2.786
        F2=1.-F1                                                                 KFPARA2.787
        THTTMP=F1*THETEE(NK1)+F2*THETEU(NK1)                                     KFPARA2.788
        QTMP=F1*Q0(NK1)+F2*QU(NK1)                                               KFPARA2.789
        TMPLIQ=F2*QLIQ(NK1)                                                      KFPARA2.790
        TMPICE=F2*QICE(NK1)                                                      KFPARA2.791
C        CALL TPMIX(P0(NK1),THTTMP,TTMP,QTMP,TMPLIQ,TMPICE,QNEWLQ,               KFPARA2.792
C     *                QNEWIC,RATIO2(NK1),RL,XLV0,XLV1,XLS0,XLS1,                KFPARA2.793
C     *   ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)                               KFPARA2.794
         call tpmix2(p0(nk1),thttmp,ttmp,qtmp,tmpliq,tmpice,                     KFPARA2.795
     1               qnewlq,qnewic,ratio2(nk1),                                  KFPARA2.796
     2               XLV1,XLV0)                                                  KFPARA2.797
        TU95=TTMP*(1.+0.608*QTMP-TMPLIQ-TMPICE)                                  KFPARA2.798
        IF(TU95.GT.TV0(NK1))THEN                                                 KFPARA2.799
          EE2=1.                                                                 KFPARA2.800
          UD2=0.                                                                 KFPARA2.801
          EQFRC(NK1)=1.0                                                         KFPARA2.802
          GOTO 50                                                                KFPARA2.803
        ENDIF                                                                    KFPARA2.804
        F1=0.10                                                                  KFPARA2.805
        F2=1.-F1                                                                 KFPARA2.806
        THTTMP=F1*THETEE(NK1)+F2*THETEU(NK1)                                     KFPARA2.807
        QTMP=F1*Q0(NK1)+F2*QU(NK1)                                               KFPARA2.808
        TMPLIQ=F2*QLIQ(NK1)                                                      KFPARA2.809
        TMPICE=F2*QICE(NK1)                                                      KFPARA2.810
c        CALL TPMIX(P0(NK1),THTTMP,TTMP,QTMP,TMPLIQ,TMPICE,QNEWLQ,               KFPARA2.811
c     *            QNEWIC,RATIO2(NK1),RL,XLV0,XLV1,XLS0,XLS1,                    KFPARA2.812
c     *   ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)                               KFPARA2.813
         call tpmix2(p0(nk1),thttmp,ttmp,qtmp,tmpliq,tmpice,                     KFPARA2.814
     1               qnewlq,qnewic,ratio2(nk1),                                  KFPARA2.815
     2               XLV1,XLV0)                                                  KFPARA2.816
        TU10=TTMP*(1.+0.608*QTMP-TMPLIQ-TMPICE)                                  KFPARA2.817
        TVDIFF = ABS(TU10-TVQU(NK1))                                             23DEC04.3181
        IF(TVDIFF.LT.1.e-3)THEN                                                  23DEC04.3182
          EE2=1.                                                                 KFPARA2.819
          UD2=0.                                                                 KFPARA2.820
          EQFRC(NK1)=1.0                                                         KFPARA2.821
          GOTO 50                                                                KFPARA2.822
        ENDIF                                                                    KFPARA2.823
        EQFRC(NK1)=(TV0(NK1)-TVQU(NK1))*F1/(TU10-TVQU(NK1))                      KFPARA2.824
        EQFRC(NK1)=AMAX1(0.0,EQFRC(NK1))                                         KFPARA2.825
        EQFRC(NK1)=AMIN1(1.0,EQFRC(NK1))                                         KFPARA2.826
        IF(EQFRC(NK1).EQ.1)THEN                                                  KFPARA2.827
          EE2=1.                                                                 KFPARA2.828
          UD2=0.                                                                 KFPARA2.829
          GOTO 50                                                                KFPARA2.830
        ELSEIF(EQFRC(NK1).EQ.0.)THEN                                             KFPARA2.831
          EE2=0.                                                                 KFPARA2.832
          UD2=1.                                                                 KFPARA2.833
          GOTO 50                                                                KFPARA2.834
        ELSE                                                                     KFPARA2.835
C                                                                                KFPARA2.836
C...SUBROUTINE PROF5 INTEGRATES OVER THE GAUSSIAN DIST TO DETERMINE THE          KFPARA2.837
C   FRACTIONAL ENTRAINMENT AND DETRAINMENT RATES...                              KFPARA2.838
C                                                                                KFPARA2.839
          CALL PROF5(EQFRC(NK1),EE2,UD2)                                         KFPARA2.840
        ENDIF                                                                    KFPARA2.841
C                                                                                KFPARA2.842
50      IF(NK.EQ.K)THEN                                                          KFPARA2.843
           EE1=1.                                                                KFPARA2.844
           UD1=0.                                                                KFPARA2.845
         ENDIF                                                                   KFPARA2.846
C                                                                                KFPARA2.847
C...NET ENTRAINMENT AND DETRAINMENT RATES ARE GIVEN BY THE AVERAGE FRACTIONAL    KFPARA2.848
C   VALUES IN THE LAYER...                                                       KFPARA2.849
C                                                                                KFPARA2.850
         EE2 = AMAX1(EE2,0.5)                                                    KFPARA2.851
c        UD2 = UD2+EE2                                                           KFPARA2.852
         UD2 = 1.5*UD2                                                           KFPARA2.853
         UER(NK1)=0.5*REI*(EE1+EE2)                                              KFPARA2.854
         UDR(NK1)=0.5*REI*(UD1+UD2)                                              KFPARA2.855
C                                                                                KFPARA2.856
C...IF THE CALCULATED UPDRAFT DETRAINMENT RATE IS GREATER THAN THE TOTAL         KFPARA2.857
C   UPDRAFT MASS FLUX, ALL CLOUD MASS DETRAINS, EXIT UPDRAFT CALCULATIONS...     KFPARA2.858
C                                                                                KFPARA2.859
55      IF(UMF(NK)-UDR(NK1).LT.10.)THEN                                          KFPARA2.860
C                                                                                KFPARA2.861
C...IF THE CALCULATED DETRAINED MASS FLUX IS GREATER THAN THE TOTAL UPD MASS     KFPARA2.862
C   FLUX, IMPOSE TOTAL DETRAINMENT OF UPDRAFT MASS AT THE PREVIOUS MODEL LVL..   KFPARA2.863
C                                                                                KFPARA2.864
          IF(DILBE.GT.0.)ABE=ABE-DILBE*G                                         KFPARA2.865
          LET=NK                                                                 KFPARA2.866
c          WRITE(98,1015)P0(NK1)/100.                                            KFPARA2.867
          GOTO 65                                                                KFPARA2.868
        ENDIF                                                                    KFPARA2.869
         EE1=EE2                                                                 KFPARA2.870
         UD1=UD2                                                                 KFPARA2.871
         UPOLD=UMF(NK)-UDR(NK1)                                                  KFPARA2.872
         UPNEW=UPOLD+UER(NK1)                                                    KFPARA2.873
         UMF(NK1)=UPNEW                                                          KFPARA2.874
         DILFRC(NK1) = UPNEW/UPOLD                                               KFPARA2.875
C                                                                                KFPARA2.876
C...DETLQ AND DETIC ARE THE RATES OF DETRAINMENT OF LIQUID AND                   KFPARA2.877
C...ICE IN THE DETRAINING UPDRAFT MASS...                                        KFPARA2.878
C                                                                                KFPARA2.879
         DETLQ(NK1)=QLIQ(NK1)*UDR(NK1)                                           KFPARA2.880
         DETIC(NK1)=QICE(NK1)*UDR(NK1)                                           KFPARA2.881
         QDT(NK1)=QU(NK1)                                                        KFPARA2.882
       if(nk1.le.kpbl)then                                                       KFPARA2.883
         QU(NK1)=(UPOLD*QU(NK1)+UER(NK1)*QUER(NK1))/UPNEW                        KFPARA2.884
       else                                                                      KFPARA2.885
         QU(NK1)=(UPOLD*QU(NK1)+UER(NK1)*Q0(NK1))/UPNEW                          KFPARA2.886
       endif                                                                     KFPARA2.887
         THETEU(NK1)=(THETEU(NK1)*UPOLD+THETEE(NK1)*UER(NK1))/UPNEW              KFPARA2.888
         QLIQ(NK1)=QLIQ(NK1)*UPOLD/UPNEW                                         KFPARA2.889
         QICE(NK1)=QICE(NK1)*UPOLD/UPNEW                                         KFPARA2.890
C                                                                                KFPARA2.891
C...KFRZ IS THE HIGHEST MODEL LEVEL AT WHICH LIQUID CONDENSATE                   KFPARA2.892
C...IS GENERATED...PPTLIQ IS THE RATE OF GENERATION (FALLOUT) OF                 KFPARA2.893
C...LIQUID PRECIP AT A GIVEN MODEL LVL, PPTICE THE SAME FOR ICE,                 KFPARA2.894
C...TRPPT IS THE TOTAL RATE OF PRODUCTION OF PRECIP UP TO THE                    KFPARA2.895
C...CURRENT MODEL LEVEL...                                                       KFPARA2.896
C                                                                                KFPARA2.897
         IF(ABS(RATIO2(NK1)-1.).GT.1.E-6)KFRZ=NK1                                KFPARA2.898
c...reverse the mod that allows feedback of rain/snow that originates in         KFPARA2.899
c...detrained air...12/9/98...jsk                                                KFPARA2.900
c        PPTLIQ(NK1)=QLQOUT(NK1)*(UMF(NK)-UDR(NK1))                              KFPARA2.901
c        PPTICE(NK1)=QICOUT(NK1)*(UMF(NK)-UDR(NK1))                              KFPARA2.902
         PPTLIQ(NK1)=QLQOUT(NK1)*UMF(NK)                                         KFPARA2.903
         PPTICE(NK1)=QICOUT(NK1)*UMF(NK)                                         KFPARA2.904
c                                                                                KFPARA2.905
         TRPPT=TRPPT+PPTLIQ(NK1)+PPTICE(NK1)                                     KFPARA2.906
         IF(NK1.LE.KPBL)UER(NK1)=UER(NK1)+VMFLCL*DP(NK1)/DPTHMX                  KFPARA2.907
60     CONTINUE                                                                  KFPARA2.908
C...CHECK CLOUD DEPTH...IF CLOUD IS TALL ENOUGH, ESTIMATE THE EQUILIBRIU         KFPARA2.909
C   TEMPERATURE LEVEL (LET) AND ADJUST MASS FLUX PROFILE AT CLOUD TOP SO         KFPARA2.910
C   THAT MASS FLUX DECREASES TO ZERO AS A LINEAR FUNCTION OF PRESSURE BE         KFPARA2.911
C   THE LET AND CLOUD TOP...                                                     KFPARA2.912
C                                                                                KFPARA2.913
C...LTOP IS THE MODEL LEVEL JUST BELOW THE LEVEL AT WHICH VERTICAL VELOC         KFPARA2.914
C   FIRST BECOMES NEGATIVE...                                                    KFPARA2.915
C                                                                                KFPARA2.916
   65   LTOP=NK                                                                  KFPARA2.917
        CLDHGT(LC)=Z00(LTOP)-ZLCL                                                KFPARA2.918
C                                                                                KFPARA2.919
C...If cloud top height is less than the specified minimum for deep              KFPARA2.920
c...convection, save value to consider this level as source for                  KFPARA2.921
c...shallow convection, go back up to check next level...                        KFPARA2.922
C                                                                                KFPARA2.923
c...Try specifying minimum cloud depth as a function of TLCL...                  KFPARA2.924
c                                                                                KFPARA2.925
      IF(TLCL.GT.293.)THEN                                                       KFPARA2.926
        CHMIN = 4.E3                                                             KFPARA2.927
      ELSEIF(TLCL.LE.293. .and. TLCL.GE.273)THEN                                 KFPARA2.928
        CHMIN = 2.E3 + 100.*(TLCL-273.)                                          KFPARA2.929
      ELSEIF(TLCL.LT.273.)THEN                                                   KFPARA2.930
        CHMIN = 2.E3                                                             KFPARA2.931
      ENDIF                                                                      KFPARA2.932
c     print *,'CHMIN =',CHMIN                                                    KFPARA2.933
C     CHMIN = 3.E3                                                               KFPARA2.934
      KSTART=MAX0(KPBL,KLCL)                                                     KFPARA2.935
c                                                                                KFPARA2.936
c...DO NOT ALLOW ANY CLOUD FROM THIS LAYER IF:                                   KFPARA2.937
C                                                                                KFPARA2.938
C...		1.) if there is no CAPE, or                                                KFPARA2.939
c...		2.) cloud top is at model level just above LCL, or                         KFPARA2.940
c...            3.) cloud top is within updraft source layer, or                 KFPARA2.941
c...            4.) cloud-top detrainment layer begins within                    KFPARA2.942
c...                updraft source layer.                                        KFPARA2.943
c                                                                                KFPARA2.944
      IF(LTOP.LE.KLCL .or.                                                       KFPARA2.945
     *   LTOP.LE.KPBL .or. LET+1.LE.KPBL)THEN                                    KFPARA2.946
        CLDHGT(LC)=0.                                                            KFPARA2.947
c...if this is selected shallow source and still does not make a                 KFPARA2.948
c...significant cloud, go on to next grid point.                                 KFPARA2.949
        IF(LC.EQ.NCHM)GOTO 325                                                   KFPARA2.950
c...if all layers in the specified portion of lower atmosph have been            KFPARA2.951
c...checked, then...                                                             KFPARA2.952
        IF(KMIX.GT.LLFC)THEN                                                     KFPARA2.953
c...if no possible shallow cloud layers were found, goto next grid               KFPARA2.954
c...point...                                                                     KFPARA2.955
          IF(ISHALL.EQ.0)THEN                                                    KFPARA2.956
            GOTO 325                                                             KFPARA2.957
          ELSE                                                                   KFPARA2.958
c...if some potential shallow cloud source layers were found,                    KFPARA2.959
c...find the one that gives the tallest cloud...                                 KFPARA2.960
            GOTO 68                                                              KFPARA2.961
          ENDIF                                                                  KFPARA2.962
        ELSE                                                                     KFPARA2.963
c...if there are more layers to check, reset cloud characteristics,              KFPARA2.964
c...go to next level...                                                          KFPARA2.965
          GOTO 67                                                                KFPARA2.966
        ENDIF                                                                    KFPARA2.967
c...if this layer has been selected as a shallow convective source,              KFPARA2.968
c...allow shallow convection...(even if the shallow-cloud parameters             KFPARA2.969
c...allow CLDHGT to exceed CHMIN)                                                KFPARA2.970
      ELSEIF(LC.EQ.NCHM)THEN                                                     KFPARA2.971
        GOTO 69                                                                  KFPARA2.972
c...if cloud depth is greater than minimum depth criterion, and                  KFPARA2.973
c...this layer has not been marked as a shallow convective source layer,         KFPARA2.974
c...allow deep convection...                                                     KFPARA2.975
      ELSEIF(CLDHGT(LC).GT.CHMIN .and. ABE.GT.1)THEN                             KFPARA2.976
        ISHALL=0                                                                 KFPARA2.977
        GOTO 69                                                                  KFPARA2.978
      ENDIF                                                                      KFPARA2.979
c...at this point, we have a parcel that is able to maintain upward momentum     KFPARA2.980
C...for at least a short distance, but the cloud from this source layer (LC)     KFPARA2.981
c...is not deep enough for "deep" (precipitating) convection;                    KFPARA2.982
c...Set ISHALL=1 to save LC as a possible for source                             KFPARA2.983
c...layer for shallow convection...                                              KFPARA2.984
c...                                                                             KFPARA2.985
C...TO DISALLOW SHALLOW CONVECTION, COMMENT OUT NEXT LINE !!!!!!!!               KFPARA2.986
      ISHALL = 1                                                                 KFPARA2.987
      IF(KMIX.GT.LLFC)THEN                                                       KFPARA2.988
        GOTO 68                                                                  KFPARA2.989
      ELSE                                                                       KFPARA2.990
        GOTO 67                                                                  KFPARA2.991
      ENDIF                                                                      KFPARA2.992
 68   CONTINUE                                                                   KFPARA2.993
      IF(ISHALL.EQ.0)THEN                                                        KFPARA2.994
        GOTO 325                                                                 KFPARA2.995
      ELSE                                                                       KFPARA2.996
        CHMAX = 0.                                                               KFPARA2.997
        NCHM = 0                                                                 KFPARA2.998
        DO 427 NK = 1,LLFC                                                       KFPARA2.999
          IF(CLDHGT(NK).GT.CHMAX)THEN                                            KFPARA2.1000
            NCHM = NK                                                            KFPARA2.1001
            CHMAX = CLDHGT(NK)                                                   KFPARA2.1002
          ENDIF                                                                  KFPARA2.1003
 427    CONTINUE                                                                 KFPARA2.1004
        KMIX = NCHM                                                              KFPARA2.1005
      ENDIF                                                                      KFPARA2.1006
 67   CONTINUE                                                                   KFPARA2.1007
      DO 70 NK=K,LTOP                                                            KFPARA2.1008
        UMF(NK)=0.                                                               KFPARA2.1009
        UDR(NK)=0.                                                               KFPARA2.1010
        UER(NK)=0.                                                               KFPARA2.1011
        DETLQ(NK)=0.                                                             KFPARA2.1012
        DETIC(NK)=0.                                                             KFPARA2.1013
        PPTLIQ(NK)=0.                                                            KFPARA2.1014
        PPTICE(NK)=0.                                                            KFPARA2.1015
 70   CONTINUE                                                                   KFPARA2.1016
      GOTO 25                                                                    KFPARA2.1017
 69   CONTINUE                                                                   KFPARA2.1018
c     IF(ISHALL.EQ.1)LET=KLCL                                                    KFPARA2.1019
      IF(ISHALL.EQ.1)THEN                                                        KFPARA2.1020
        KSTART=MAX0(KPBL,KLCL)                                                   KFPARA2.1021
        LET=KSTART                                                               KFPARA2.1022
      endif                                                                      KFPARA2.1023
C                                                                                KFPARA2.1024
C...IF THE LET AND LTOP ARE THE SAME, DETRAIN ALL OF THE UPDRAFT MASS FL         KFPARA2.1025
C   THIS LEVEL...                                                                KFPARA2.1026
C                                                                                KFPARA2.1027
        IF(LET.EQ.LTOP)THEN                                                      KFPARA2.1028
          UDR(LTOP)=UMF(LTOP)+UDR(LTOP)-UER(LTOP)                                KFPARA2.1029
          DETLQ(LTOP)=QLIQ(LTOP)*UDR(LTOP)*UPNEW/UPOLD                           KFPARA2.1030
          DETIC(LTOP)=QICE(LTOP)*UDR(LTOP)*UPNEW/UPOLD                           KFPARA2.1031
                                                                                 KFPARA2.1032
c...reverse the mod that allows feedback of rain/snow that originates in         KFPARA2.1033
c...detrained air...12/9/98...jsk                                                KFPARA2.1034
c         TRPPT=TRPPT-(PPTLIQ(LTOP)+PPTICE(LTOP))                                KFPARA2.1035
c******************                                                              KFPARA2.1036
c         PPTLIQ(LTOP)=0.                                                        KFPARA2.1037
c         PPTICE(LTOP)=0.                                                        KFPARA2.1038
c******************                                                              KFPARA2.1039
c                                                                                KFPARA2.1040
          UER(LTOP)=0.                                                           KFPARA2.1041
          UMF(LTOP)=0.                                                           KFPARA2.1042
          GOTO 85                                                                KFPARA2.1043
        ENDIF                                                                    KFPARA2.1044
C                                                                                KFPARA2.1045
C   BEGIN TOTAL DETRAINMENT AT THE LEVEL ABOVE THE LET...                        KFPARA2.1046
C                                                                                KFPARA2.1047
        DPTT=0.                                                                  KFPARA2.1048
        DO 71 NJ=LET+1,LTOP                                                      KFPARA2.1049
   71   DPTT=DPTT+DP(NJ)                                                         KFPARA2.1050
        DUMFDP=UMF(LET)/DPTT                                                     KFPARA2.1051
C                                                                                KFPARA2.1052
C...ADJUST MASS FLUX PROFILES, DETRAINMENT RATES, AND PRECIPITATION FALL         KFPARA2.1053
C   RATES TO REFLECT THE LINEAR DECREASE IN MASS FLX BETWEEN THE LET AND         KFPARA2.1054
C                                                                                KFPARA2.1055
        DO 75 NK=LET+1,LTOP                                                      KFPARA2.1056
c                                                                                KFPARA2.1057
c...entrainment is allowed at every level except for LTOP, so disallow           KFPARA2.1058
c...entrainment at LTOP and adjust entrainment rates between LET and LTOP        KFPARA2.1059
c...so the the dilution factor due to entyrianment is not changed but            KFPARA2.1060
c...the actual entrainment rate will change due due forced total                 KFPARA2.1061
c...detrainment in this layer...                                                 KFPARA2.1062
c                                                                                KFPARA2.1063
          IF(NK.EQ.LTOP)THEN                                                     KFPARA2.1064
            UDR(NK) = UMF(NK-1)                                                  KFPARA2.1065
            UER(NK) = 0.                                                         KFPARA2.1066
            DETLQ(NK) = UDR(NK)*QLIQ(NK)*DILFRC(NK)                              KFPARA2.1067
            DETIC(NK) = UDR(NK)*QICE(NK)*DILFRC(NK)                              KFPARA2.1068
          ELSE                                                                   KFPARA2.1069
            UMF(NK)=UMF(NK-1)-DP(NK)*DUMFDP                                      KFPARA2.1070
            UER(NK)=UMF(NK)*(1.-1./DILFRC(NK))                                   KFPARA2.1071
            UDR(NK)=UMF(NK-1)-UMF(NK)+UER(NK)                                    KFPARA2.1072
            DETLQ(NK)=UDR(NK)*QLIQ(NK)*DILFRC(NK)                                KFPARA2.1073
            DETIC(NK)=UDR(NK)*QICE(NK)*DILFRC(NK)                                KFPARA2.1074
          ENDIF                                                                  KFPARA2.1075
c...reverse the mod that allows feedback of rain/snow that originates in         KFPARA2.1076
c...detrained air...12/9/98...jsk                                                KFPARA2.1077
c         TRPPT=TRPPT-PPTLIQ(NK)-PPTICE(NK)                                      KFPARA2.1078
c         PPTLIQ(NK)=(UMF(NK-1)-UDR(NK))*QLQOUT(NK)                              KFPARA2.1079
c         PPTICE(NK)=(UMF(NK-1)-UDR(NK))*QICOUT(NK)                              KFPARA2.1080
c         TRPPT=TRPPT+PPTLIQ(NK)+PPTICE(NK)                                      KFPARA2.1081
          IF(NK.GE.LET+2)THEN                                                    KFPARA2.1082
            TRPPT=TRPPT-PPTLIQ(NK)-PPTICE(NK)                                    KFPARA2.1083
            PPTLIQ(NK)=UMF(NK-1)*QLQOUT(NK)                                      KFPARA2.1084
            PPTICE(NK)=UMF(NK-1)*QICOUT(NK)                                      KFPARA2.1085
            TRPPT=TRPPT+PPTLIQ(NK)+PPTICE(NK)                                    KFPARA2.1086
          ENDIF                                                                  KFPARA2.1087
c                                                                                KFPARA2.1088
   75   CONTINUE                                                                 KFPARA2.1089
C                                                                                KFPARA2.1090
C...SEND UPDRAFT CHARACTERISTICS TO OUTPUT FILES...                              KFPARA2.1091
C                                                                                KFPARA2.1092
   85   CONTINUE                                                                 KFPARA2.1093
c...when moist perturbation is added to updraft and KLCL.le.KPBL,                KFPARA2.1094
c...reset THETEE so that moisture perturbation only affects updrafts...          KFPARA2.1095
        if(KLCL.LE.KPBL)THEN                                                     KFPARA2.1096
          do 77 nk1 = klcl,kpbl                                                  KFPARA2.1097
         CALL ENVIRTHT(P0(NK1),T0(NK1),Q0(NK1),THETEE(NK1),0.,                   KFPARA2.1098
     *                 RL,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)               KFPARA2.1099
 77       continue                                                               KFPARA2.1100
        endif                                                                    KFPARA2.1101
c      XTIME = NTSD*DT/60.                                                       KFPARA2.1102
c       IF(XTIME .LT. 10.) THEN                                                  KFPARA2.1103
c      WRITE(98,3001)                                                            KFPARA2.1104
c3001  FORMAT(' ')                                                               KFPARA2.1105
c      WRITE(98,3002)NTSD*DT,I,J                                                 KFPARA2.1106
c      call flush(98)                                                            KFPARA2.1107
c       ENDIF                                                                    KFPARA2.1108
 3002  FORMAT('XTIME =',F9.2,' GRID POINT: I =',I3,                              KFPARA2.1109
     * ' J =',I3)                                                                KFPARA2.1110
C      IF(MXLAYR.EQ.1)THEN                                                       KFPARA2.1111
C       PRINT *,'HFX, QFX, PBL, THGV =',HFX(I,J),QFX(I,J),PBL(I,J),THGV          KFPARA2.1112
C     PRINT *,'RHOE(1), WPTHP, WPQP, WPTHVP, WSTR =',RHOE(1),WPTHP,WPQP,         KFPARA2.1113
C     *WPTHVP,WSTR                                                               KFPARA2.1114
C      ENDIF                                                                     KFPARA2.1115
C     PRINT *,'PRS AT BASE OF UPDRAFT SOURCE LAYER =',P0(LC)/100.                KFPARA2.1116
C      PRINT *,'DEPTH OF UPDRAFT SOURCE MIXED LAYER =',DPTHMX/100.               KFPARA2.1117
C      PRINT 1000,'  P  ','  VMFU  ','  TU  ',' EQFRC',' CLDWAT ',               KFPARA2.1118
C     *' CLDICE ',' PPTLIQ ',' PPTICE ',' DETLQ ',' DETIC '                      KFPARA2.1119
C      DO 1020 NK=KLCL-1,LTOP                                                    KFPARA2.1120
C         IF(NK.EQ.KLCL-1)THEN                                                   KFPARA2.1121
C           PRS=PLCL/100.                                                        KFPARA2.1122
C         ELSE                                                                   KFPARA2.1123
C           PRS=P0(NK)/100.                                                      KFPARA2.1124
C         ENDIF                                                                  KFPARA2.1125
CC         PRS=CVMGT(PLCL/100.,P0(NK)/100.,NK.EQ.KLCL-1)                         KFPARA2.1126
C         PRINT 1005,PRS,UMF(NK)/VMFLCL,TU(NK)-273.16,EQFRC(NK),                 KFPARA2.1127
C     *   QLIQ(NK)*1.E3,QICE(NK)*1.E3,QLQOUT(NK)*1.E3,QICOUT(NK)*1.E3,           KFPARA2.1128
C     *   DETLQ(NK)/1.E3,DETIC(NK)/1.E3                                          KFPARA2.1129
C1020  CONTINUE                                                                  KFPARA2.1130
C      PRINT 1010,P0(NK1)/100.                                                   KFPARA2.1131
C                                                                                KFPARA2.1132
C...EXTEND THE UPDRAFT MASS FLUX PROFILE DOWN TO THE SOURCE LAYER FOR TH         KFPARA2.1133
C   UPDRAFT AIR...ALSO, DEFINE THETAE FOR LEVELS BELOW THE LCL...                KFPARA2.1134
C                                                                                KFPARA2.1135
        DO 90 NK=1,K                                                             KFPARA2.1136
          IF(NK.GE.LC)THEN                                                       KFPARA2.1137
            IF(NK.EQ.LC)THEN                                                     KFPARA2.1138
              UMF(NK)=VMFLCL*DP(NK)/DPTHMX                                       KFPARA2.1139
              UER(NK)=VMFLCL*DP(NK)/DPTHMX                                       KFPARA2.1140
            ELSEIF(NK.LE.KPBL)THEN                                               KFPARA2.1141
              UER(NK)=VMFLCL*DP(NK)/DPTHMX                                       KFPARA2.1142
              UMF(NK)=UMF(NK-1)+UER(NK)                                          KFPARA2.1143
            ELSE                                                                 KFPARA2.1144
              UMF(NK)=VMFLCL                                                     KFPARA2.1145
              UER(NK)=0.                                                         KFPARA2.1146
            ENDIF                                                                KFPARA2.1147
            TU(NK)=TMIX+(Z00(NK)-ZMIX)*GDRY                                      KFPARA2.1148
            QU(NK)=QMIX                                                          KFPARA2.1149
            WU(NK)=WLCL                                                          KFPARA2.1150
          ELSE                                                                   KFPARA2.1151
            TU(NK)=0.                                                            KFPARA2.1152
            QU(NK)=0.                                                            KFPARA2.1153
            UMF(NK)=0.                                                           KFPARA2.1154
            WU(NK)=0.                                                            KFPARA2.1155
            UER(NK)=0.                                                           KFPARA2.1156
          ENDIF                                                                  KFPARA2.1157
          UDR(NK)=0.                                                             KFPARA2.1158
          QDT(NK)=0.                                                             KFPARA2.1159
          QLIQ(NK)=0.                                                            KFPARA2.1160
          QICE(NK)=0.                                                            KFPARA2.1161
          QLQOUT(NK)=0.                                                          KFPARA2.1162
          QICOUT(NK)=0.                                                          KFPARA2.1163
          PPTLIQ(NK)=0.                                                          KFPARA2.1164
          PPTICE(NK)=0.                                                          KFPARA2.1165
          DETLQ(NK)=0.                                                           KFPARA2.1166
          DETIC(NK)=0.                                                           KFPARA2.1167
          RATIO2(NK)=0.                                                          KFPARA2.1168
          EE=Q0(NK)*P0(NK)/(0.622+Q0(NK))                                        KFPARA2.1169
          TLOG=ALOG(EE/ALIQ)                                                     KFPARA2.1170
          TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)                                      KFPARA2.1171
          TSAT=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(T0(NK)-T00))*(            KFPARA2.1172
     +         T0(NK)-TDPT)                                                      KFPARA2.1173
          THTA=T0(NK)*(1.E5/P0(NK))**(0.2854*(1.-0.28*Q0(NK)))                   KFPARA2.1174
          THETEE(NK)=THTA*                                                       KFPARA2.1175
     +               EXP((3374.6525/TSAT-2.5403)*Q0(NK)*(1.+0.81*Q0(NK))         KFPARA2.1176
     +               )                                                           KFPARA2.1177
          THTES(NK)=THTA*                                                        KFPARA2.1178
     +              EXP((3374.6525/T0(NK)-2.5403)*QES(NK)*(1.+0.81*              KFPARA2.1179
     +              QES(NK)))                                                    KFPARA2.1180
          EQFRC(NK)=1.0                                                          KFPARA2.1181
   90   CONTINUE                                                                 KFPARA2.1182
C...FIND HEIGHT OF LFC, CALCULATE CIN...                                         KFPARA2.1183
        if(LFC.gt.0)then                                                         KFPARA2.1184
        CIN1 = 0.                                                                KFPARA2.1185
        IF(LFC.EQ.KLCL)THEN                                                      KFPARA2.1186
          IF(TVLCL.GT.TVEN)THEN                                                  KFPARA2.1187
             CIN1 = 0.                                                           KFPARA2.1188
          ELSE                                                                   KFPARA2.1189
            DTUDZ = (TVU(LFC)-TVLCL)/(Z00(LFC)-ZLCL)                             KFPARA2.1190
            DTEDZ = (TV0(LFC)-TVEN )/(Z00(LFC)-ZLCL)                             KFPARA2.1191
            ZLFC = ZLCL + (TVEN - TVLCL)/(DTUDZ-DTEDZ)                           KFPARA2.1192
            TVLFC = TVEN+(ZLFC-ZLCL)*DTEDZ                                       KFPARA2.1193
            CIN1 = G*(ZLFC-ZLCL)*(TVLCL-TVEN)/(TVEN+TVLFC)                       KFPARA2.1194
          ENDIF                                                                  KFPARA2.1195
        ELSE                                                                     KFPARA2.1196
          CIN1 = CIN1 +                                                          KFPARA2.1197
     +         G*(Z00(klcl)-ZLCL)*((TVU(KLCL)+TVLCL)/                            KFPARA2.1198
     +         (TV0(KLCL)+TVEN)-1.)                                              KFPARA2.1199
          DO 91 NK = KLCL+1,LFC                                                  KFPARA2.1200
            IF(NK.EQ.LFC)THEN                                                    KFPARA2.1201
              DTUDZ = (TVU(LFC)-TVU(LFC-1))/(Z00(LFC)-Z00(LFC-1))                KFPARA2.1202
              DTEDZ = (TV0(LFC)-TV0(LFC-1))/(Z00(LFC)-Z00(LFC-1))                KFPARA2.1203
              ZLFC = Z00(LFC-1)+(TV0(LFC-1)-TV0(LFC-1))/(DTUDZ-DTEDZ)            KFPARA2.1204
              TVEN = TV0(LFC-1)+(ZLFC-Z00(LFC-1))*DTEDZ                          KFPARA2.1205
             CIN1 = CIN1+G*(ZLFC-Z00(LFC-1))*(TVU(LFC-1)-TV0(LFC-1))/            KFPARA2.1206
     +               (TVEN+TV0(LFC-1))                                           KFPARA2.1207
            ELSE                                                                 KFPARA2.1208
              CIN1 = CIN1+G*(Z00(NK)-Z00(NK-1))*                                 KFPARA2.1209
     +              ((TVU(NK)+TVU(NK-1))/(TV0(NK)+TV0(NK-1))-1.)                 KFPARA2.1210
            ENDIF                                                                KFPARA2.1211
 91       CONTINUE                                                               KFPARA2.1212
        ENDIF                                                                    KFPARA2.1213
c      PLFC = P0(LFC-1)*EXP(-G*(zlfc-z00(lfc-1))/(287.*tv0(lfc-1)))              KFPARA2.1214
c      print *,'PLFC, CIN =',PLFC,CIN                                            KFPARA2.1215
       endif                                                                     KFPARA2.1216
C                                                                                KFPARA2.1217
        LTOP1=LTOP+1                                                             KFPARA2.1218
        LTOPM1=LTOP-1                                                            KFPARA2.1219
C                                                                                KFPARA2.1220
C...DEFINE VARIABLES ABOVE CLOUD TOP...                                          KFPARA2.1221
C                                                                                KFPARA2.1222
        DO 95 NK=LTOP1,KX                                                        KFPARA2.1223
          UMF(NK)=0.                                                             KFPARA2.1224
          UDR(NK)=0.                                                             KFPARA2.1225
          UER(NK)=0.                                                             KFPARA2.1226
          QDT(NK)=0.                                                             KFPARA2.1227
          QLIQ(NK)=0.                                                            KFPARA2.1228
          QICE(NK)=0.                                                            KFPARA2.1229
          QLQOUT(NK)=0.                                                          KFPARA2.1230
          QICOUT(NK)=0.                                                          KFPARA2.1231
          DETLQ(NK)=0.                                                           KFPARA2.1232
          DETIC(NK)=0.                                                           KFPARA2.1233
          PPTLIQ(NK)=0.                                                          KFPARA2.1234
          PPTICE(NK)=0.                                                          KFPARA2.1235
          IF(NK.GT.LTOP1)THEN                                                    KFPARA2.1236
            TU(NK)=0.                                                            KFPARA2.1237
            QU(NK)=0.                                                            KFPARA2.1238
            WU(NK)=0.                                                            KFPARA2.1239
          ENDIF                                                                  KFPARA2.1240
          THTA0(NK)=0.                                                           KFPARA2.1241
          THTAU(NK)=0.                                                           KFPARA2.1242
          EMS(NK)=0.                                                             KFPARA2.1243
          EMSD(NK)=0.                                                            KFPARA2.1244
          TG(NK)=T0(NK)                                                          KFPARA2.1245
          QG(NK)=Q0(NK)                                                          KFPARA2.1246
          QLG(NK)=0.                                                             KFPARA2.1247
          QIG(NK)=0.                                                             KFPARA2.1248
          QRG(NK)=0.                                                             KFPARA2.1249
          QSG(NK)=0.                                                             KFPARA2.1250
   95   OMG(NK)=0.                                                               KFPARA2.1251
        OMG(KXP1)=0.                                                             KFPARA2.1252
        P150=P0(KLCL)-1.50E4                                                     KFPARA2.1253
        DO 100 NK=1,LTOP                                                         KFPARA2.1254
          EMS(NK)=DP(NK)*DXSQ/G                                                  KFPARA2.1255
          EMSD(NK)=1./EMS(NK)                                                    KFPARA2.1256
C                                                                                KFPARA2.1257
C...INITIALIZE SOME VARIABLES TO BE USED LATER IN THE VERT ADVECTION SCH         KFPARA2.1258
C                                                                                KFPARA2.1259
          EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*QDT(NK)))                       KFPARA2.1260
          THTAU(NK)=TU(NK)*EXN(NK)                                               KFPARA2.1261
          EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*Q0(NK)))                        KFPARA2.1262
          THTA0(NK)=T0(NK)*EXN(NK)                                               KFPARA2.1263
C                                                                                KFPARA2.1264
C...LVF IS THE LEVEL AT WHICH MOISTURE FLUX IS ESTIMATED AS THE BASIS FO         KFPARA2.1265
C...PRECIPITATION EFFICIENCY CALCULATIONS...                                     KFPARA2.1266
C                                                                                KFPARA2.1267
          IF(P0(NK).GT.P150)LVF=NK                                               KFPARA2.1268
          DDILFRC(NK) = 1./DILFRC(NK)                                            KFPARA2.1269
  100   OMG(NK)=0.                                                               KFPARA2.1270
        LVF=MIN0(LVF,LET)                                  ! JSK MODS            KFPARA2.1271
        USR=UMF(LVF+1)*(QU(LVF+1)+QLIQ(LVF+1)+QICE(LVF+1))                       KFPARA2.1272
        USR=AMIN1(USR,TRPPT)                                                     KFPARA2.1273
C!!!!! HERE !!!!!!!                                                              KFPARA2.1274
      IF(IPRNT)THEN                                                              KFPARA2.1275
        WRITE(98,*)'ISHALL =',ISHALL                                             KFPARA2.1276
        WRITE(98,1025)KLCL,ZLCL,DTLCL,LTOP,P0(LTOP),IFLAG,                       KFPARA2.1277
     *  TMIX-T00,PMIX,QMIX,ABE                                                   KFPARA2.1278
       WRITE(98,1030)P0(LET)/100.,P0(LTOP)/100.,VMFLCL,PLCL/100.,                KFPARA2.1279
     * WLCL,CLDHGT(LC)                                                           KFPARA2.1280
c      call flush(98)                                                            KFPARA2.1281
      ENDIF                                                                      KFPARA2.1282
c     IF (XTIME.LT.10.)THEN                                                      KFPARA2.1283
c      WRITE(98,1025)KLCL,ZLCL,DTLCL,LTOP,P0(LTOP),IFLAG,                        KFPARA2.1284
c    * TMIX-T00,PMIX,QMIX,ABE                                                    KFPARA2.1285
c      WRITE(98,1030)P0(LET)/100.,P0(LTOP)/100.,VMFLCL,PLCL/100.,                KFPARA2.1286
c    * WLCL,CLDHGT                                                               KFPARA2.1287
c      call flush(98)                                                            KFPARA2.1288
c     ENDIF                                                                      KFPARA2.1289
C                                                                                KFPARA2.1290
C...COMPUTE CONVECTIVE TIME SCALE(TIMEC). THE MEAN WIND AT THE LCL               KFPARA2.1291
C...AND MIDTROPOSPHERE IS USED.                                                  KFPARA2.1292
C                                                                                KFPARA2.1293
        WSPD(KLCL)=SQRT(U0(KLCL)*U0(KLCL)+V0(KLCL)*V0(KLCL))                     KFPARA2.1294
        WSPD(L5)=SQRT(U0(L5)*U0(L5)+V0(L5)*V0(L5))                               KFPARA2.1295
        WSPD(LTOP)=SQRT(U0(LTOP)*U0(LTOP)+V0(LTOP)*V0(LTOP))                     KFPARA2.1296
        VCONV=.5*(WSPD(KLCL)+WSPD(L5))                                           KFPARA2.1297
c...for ETA model, DX is a function of location...                               KFPARA2.1298
c       TIMEC=DX(I,J)/VCONV                                                      KFPARA2.1299
        TIMEC=DX/VCONV                                                           KFPARA2.1300
        TADVEC=TIMEC                                                             KFPARA2.1301
        TIMEC=AMAX1(1800.,TIMEC)                                                 KFPARA2.1302
        TIMEC=AMIN1(3600.,TIMEC)                                                 KFPARA2.1303
        IF(ISHALL.EQ.1)TIMEC=2400.                                               KFPARA2.1304
        NIC=NINT(TIMEC/(.5*DT2))                                                 KFPARA2.1305
        TIMEC=FLOAT(NIC)*.5*DT2                                                  KFPARA2.1306
C                                                                                KFPARA2.1307
C...COMPUTE WIND SHEAR AND PRECIPITATION EFFICIENCY.                             KFPARA2.1308
C                                                                                KFPARA2.1309
C        SHSIGN = CVMGT(1.,-1.,WSPD(LTOP).GT.WSPD(KLCL))                         KFPARA2.1310
        IF(WSPD(LTOP).GT.WSPD(KLCL))THEN                                         KFPARA2.1311
          SHSIGN=1.                                                              KFPARA2.1312
        ELSE                                                                     KFPARA2.1313
          SHSIGN=-1.                                                             KFPARA2.1314
        ENDIF                                                                    KFPARA2.1315
        VWS=(U0(LTOP)-U0(KLCL))*(U0(LTOP)-U0(KLCL))+(V0(LTOP)-V0(KLCL))*         KFPARA2.1316
     +      (V0(LTOP)-V0(KLCL))                                                  KFPARA2.1317
        VWS=1.E3*SHSIGN*SQRT(VWS)/(Z00(LTOP)-Z00(LCL))                           KFPARA2.1318
        PEF=1.591+VWS*(-.639+VWS*(9.53E-2-VWS*4.96E-3))                          KFPARA2.1319
        PEF=AMAX1(PEF,.2)                                                        KFPARA2.1320
        PEF=AMIN1(PEF,.9)                                                        KFPARA2.1321
C                                                                                KFPARA2.1322
C...PRECIPITATION EFFICIENCY IS A FUNCTION OF THE HEIGHT OF CLOUD BASE.          KFPARA2.1323
C                                                                                KFPARA2.1324
        CBH=(ZLCL-Z00(1))*3.281E-3                                               KFPARA2.1325
        IF(CBH.LT.3.)THEN                                                        KFPARA2.1326
          RCBH=.02                                                               KFPARA2.1327
        ELSE                                                                     KFPARA2.1328
          RCBH=.96729352+CBH*(-.70034167+CBH*(.162179896+CBH*(-                  KFPARA2.1329
     +         1.2569798E-2+CBH*(4.2772E-4-CBH*5.44E-6))))                       KFPARA2.1330
        ENDIF                                                                    KFPARA2.1331
        IF(CBH.GT.25)RCBH=2.4                                                    KFPARA2.1332
        PEFCBH=1./(1.+RCBH)                                                      KFPARA2.1333
        PEFCBH=AMIN1(PEFCBH,.9)                                                  KFPARA2.1334
C                                                                                KFPARA2.1335
C... MEAN PEF. IS USED TO COMPUTE RAINFALL.                                      KFPARA2.1336
C                                                                                KFPARA2.1337
        PEFF=.5*(PEF+PEFCBH)                                                     KFPARA2.1338
        PEFF2 = PEFF                                ! JSK MODS                   KFPARA2.1339
       IF(IPRNT)THEN                                                             KFPARA2.1340
         WRITE(98,1035)PEF,PEFCBH,LC,LET,WKL,VWS                                 KFPARA2.1341
c       call flush(98)                                                           KFPARA2.1342
       endif                                                                     KFPARA2.1343
c        WRITE(98,1035)PEF,PEFCBH,LC,LET,WKL,VWS                                 KFPARA2.1344
C*****************************************************************               KFPARA2.1345
C                                                                *               KFPARA2.1346
C                  COMPUTE DOWNDRAFT PROPERTIES                  *               KFPARA2.1347
C                                                                *               KFPARA2.1348
C*****************************************************************               KFPARA2.1349
C                                                                                KFPARA2.1350
C                                                                                KFPARA2.1351
       TDER=0.                                                                   KFPARA2.1352
       IF(ISHALL.EQ.1)THEN                                                       KFPARA2.1353
         LFS = 1                                                                 KFPARA2.1354
         GOTO 141                                                                KFPARA2.1355
       ENDIF                                                                     KFPARA2.1356
c                                                                                KFPARA2.1357
c...start downdraft about 150 mb above cloud base...                             KFPARA2.1358
c                                                                                KFPARA2.1359
c       KSTART=MAX0(KPBL,KLCL)                                                   KFPARA2.1360
c       KSTART=KPBL                                  ! Changed 7/23/99           KFPARA2.1361
        KSTART=KPBL+1                                ! Changed 7/23/99           KFPARA2.1362
        DO 683 NK = KSTART+1,KL                                                  KFPARA2.1363
          DPPP = P0(KSTART)-P0(NK)                                               KFPARA2.1364
c          IF(DPPP.GT.200.E2)THEN                                                KFPARA2.1365
          KLFS = LET-1
          IF(DPPP.GT.150.E2)THEN                                                 KFPARA2.1366
            KLFS = NK                                                            KFPARA2.1367
            GOTO 684                                                             KFPARA2.1368
          ENDIF                                                                  KFPARA2.1369
 683    CONTINUE                                                                 KFPARA2.1370
 684    CONTINUE                                                                 KFPARA2.1371
        KLFS = MIN0(KLFS,LET-1)                                                  KFPARA2.1372
        LFS = KLFS                                                               KFPARA2.1373
c       print *,'LFS =',LFS                                                      KFPARA2.1374
c                                                                                KFPARA2.1375
c...if LFS is not at least 50 mb above cloud base (implying that the             KFPARA2.1376
c...level of equil temp, LET, is just above cloud base) do not allow a           KFPARA2.1377
c...downdraft...                                                                 KFPARA2.1378
c                                                                                KFPARA2.1379
        IF((P0(KSTART)-P0(LFS)).LT.50.E2)THEN                                    KFPARA2.1380
c       IF((P0(KLCL)-P0(LFS)).LT.50.E2)THEN                                      KFPARA2.1381
          TDER = 0.                                                              KFPARA2.1382
c         write(6,*)'downdraft not allowed...too shallow'                        KFPARA2.1383
c         print *,'P0(KSTART), P0(LFS) =',P0(KSTART), P0(LFS)                    KFPARA2.1384
c         write(98,*)'downdraft not allowed...too shallow'                       KFPARA2.1385
          GOTO 141                                                               KFPARA2.1386
        ENDIF                                                                    KFPARA2.1387
c       write(98,*)'KLCL, KLFS, ML, KSTART =',klcl,klfs,ml,kstart                KFPARA2.1388
c     call flush(98)                                                             KFPARA2.1389
        THETED(LFS) = THETEE(LFS)                                                KFPARA2.1390
        QD(LFS) = Q0(LFS)                                                        KFPARA2.1391
c                                                                                KFPARA2.1392
c...call tpmix2dd to find wet-bulb temp, qv...                                   KFPARA2.1393
c                                                                                KFPARA2.1394
        call tpmix2dd(p0(lfs),theted(lfs),tz(lfs),qss)                           KFPARA2.1395
        THTAD(LFS)=TZ(LFS)*(P00/P0(LFS))**(0.2854*(1.-0.28*QSS))                 KFPARA2.1396
C                                                                                KFPARA2.1397
C...TAKE A FIRST GUESS AT THE INITIAL DOWNDRAFT MASS FLUX...                     KFPARA2.1398
C                                                                                KFPARA2.1399
        TVD(LFS)=TZ(LFS)*(1.+0.608*QSS)                                          KFPARA2.1400
        RDD=P0(LFS)/(R*TVD(LFS))                                                 KFPARA2.1401
        A1=(1.-PEFF)*AU0                                                         KFPARA2.1402
        DMF(LFS)=-A1*RDD                                                         KFPARA2.1403
        DER(LFS)=DMF(LFS)                                                        KFPARA2.1404
        DDR(LFS)=0.                                                              KFPARA2.1405
        RHBAR = RH(LFS)*DP(LFS)                                                  KFPARA2.1406
        DPTT = DP(LFS)                                                           KFPARA2.1407
        DO 137 ND = LFS-1,KSTART,-1                                              KFPARA2.1408
c       DO 137 ND = LFS-1,KLCL,-1                                                KFPARA2.1409
          ND1 = ND+1                                                             KFPARA2.1410
          DER(ND)=DER(LFS)*EMS(ND)/EMS(LFS)                                      KFPARA2.1411
          DDR(ND)=0.                                                             KFPARA2.1412
          DMF(ND)=DMF(ND1)+DER(ND)                                               KFPARA2.1413
          THETED(ND)=(THETED(ND1)*DMF(ND1)+THETEE(ND)*DER(ND))/DMF(ND)           KFPARA2.1414
          QD(ND)=(QD(ND1)*DMF(ND1)+Q0(ND)*DER(ND))/DMF(ND)                       KFPARA2.1415
          DPTT = DPTT+DP(ND)                                                     KFPARA2.1416
          RHBAR = RHBAR+RH(ND)*DP(ND)                                            KFPARA2.1417
 137    continue                                                                 KFPARA2.1418
        RHBAR = RHBAR/DPTT                                                       KFPARA2.1419
c       print *,' RHBAR =',rhbar                                                 KFPARA2.1420
        DMFFRC = 2.*(1.-RHBAR)                                                   KFPARA2.1421
        DPDD = 0.                                                                KFPARA2.1422
c        LDT = KLCL - 1                                                          KFPARA2.1423
c        DO 138 ND = KLCL-1,1,-1                                                 KFPARA2.1424
c...                                                                             KFPARA2.1425
c...3/2/98:  Calculate melting effect...                                         KFPARA2.1426
c... first, compute total frozen precipitation generated...                      KFPARA2.1427
c                                                                                KFPARA2.1428
        pptmlt = 0.                                                              KFPARA2.1429
        DO 38 NK = KLCL,LTOP                                                     KFPARA2.1430
          PPTMLT = PPTMLT+PPTICE(NK)                                             KFPARA2.1431
 38     CONTINUE                                                                 KFPARA2.1432
        if(lc.lt.ml)then                                                         KFPARA2.1433
c         DTMELT = RLF*PPTMLT/(CP*DMFFRC*UMF(KLCL))                              KFPARA2.1434
c...                                                                             KFPARA2.1435
c...For now, calculate melting effect as if DMF = -UMF at KLCL, i.e., as         KFPARA2.1436
c...if DMFFRC=1.  Otherwise, for small DMFFRC, DTMELT gets too large!            KFPARA2.1437
c...12/14/98 jsk...                                                              KFPARA2.1438
          DTMELT = RLF*PPTMLT/(CP*UMF(KLCL))                                     KFPARA2.1439
        else                                                                     KFPARA2.1440
          DTMELT = 0.                                                            KFPARA2.1441
        endif                                                                    KFPARA2.1442
c       print *,'DTMELT =',DTMELT                                                KFPARA2.1443
c       LDT = MIN0(LFS-1,KLCL-1)                                                 KFPARA2.1444
        LDT = MIN0(LFS-1,KSTART-1)                                               KFPARA2.1445
c       call tpmix2dd(p0(klcl),theted(klcl),tz(klcl),qss)                        KFPARA2.1446
        call tpmix2dd(p0(kstart),theted(kstart),tz(kstart),qss)                  KFPARA2.1447
c       print *,'p(klcl), tz before melt =',p0(klcl)/100.,tz(klcl)               KFPARA2.1448
c       tz(klcl) = tz(klcl)-dtmelt                                               KFPARA2.1449
        tz(kstart) = tz(kstart)-dtmelt                                           KFPARA2.1450
c       print *,'p(klcl), tz after melt =',p0(klcl)/100.,tz(klcl)                KFPARA2.1451
c       ES=ALIQ*EXP((BLIQ*TZ(KLCL)-CLIQ)/(TZ(KLCL)-DLIQ))                        KFPARA2.1452
c       QSS=0.622*ES/(P0(klcl)-ES)                                               KFPARA2.1453
        ES=ALIQ*EXP((BLIQ*TZ(KSTART)-CLIQ)/(TZ(KSTART)-DLIQ))                    KFPARA2.1454
        QSS=0.622*ES/(P0(KSTART)-ES)                                             KFPARA2.1455
c        qd(klcl)=qs                                                             KFPARA2.1456
c      THETED(KLCL)=TZ(KLCL)*(1.E5/P0(KLCL))**(0.2854*(1.-0.28*QSS))*            KFPARA2.1457
c    +           EXP((3374.6525/TZ(KLCL)-2.5403)*QSS*(1.+0.81*QSS))              KFPARA2.1458
       THETED(KSTART)=TZ(KSTART)*(1.E5/P0(KSTART))**                             KFPARA2.1459
     +           (0.2854*(1.-0.28*QSS))*                                         KFPARA2.1460
     +           EXP((3374.6525/TZ(KSTART)-2.5403)*QSS*(1.+0.81*QSS))            KFPARA2.1461
c....                                                                            KFPARA2.1462
c       LDT = MIN0(LFS-1,KLCL-1)                                                 KFPARA2.1463
        LDT = MIN0(LFS-1,KSTART-1)                                               KFPARA2.1464
        DO 138 ND = LDT,1,-1                                                     KFPARA2.1465
          DPDD = DPDD+DP(ND)                                                     KFPARA2.1466
c         THETED(ND) = THETED(KLCL)                                              KFPARA2.1467
c         QD(ND)     = QD(KLCL)                                                  KFPARA2.1468
          THETED(ND) = THETED(KSTART)                                            KFPARA2.1469
          QD(ND)     = QD(KSTART)                                                KFPARA2.1470
c                                                                                KFPARA2.1471
c...call tpmix2dd to find wet bulb temp, saturation mixing ratio...              KFPARA2.1472
c                                                                                KFPARA2.1473
         call tpmix2dd(p0(nd),theted(nd),tz(nd),qss)                             KFPARA2.1474
         qsd(nd) = qss                                                           KFPARA2.1475
c                                                                                KFPARA2.1476
c...specify RH decrease of 10%/km in downdraft...                                KFPARA2.1477
c                                                                                KFPARA2.1478
c        RHH = 1.-0.1/1000.*(Z00(KLCL)-Z00(ND))                                  KFPARA2.1479
c        RHH = 1.-0.1/1000.*(Z00(KSTART)-Z00(ND))                                KFPARA2.1480
         RHH = 1.-0.2/1000.*(Z00(KSTART)-Z00(ND))                                KFPARA2.1481
c       print *,'PRS, RHH =',P0(ND)/100.,RHH                                     KFPARA2.1482
c                                                                                KFPARA2.1483
c...adjust downdraft TEMP, Q to specified RH:                                    KFPARA2.1484
c                                                                                KFPARA2.1485
         IF(RHH.LT.1.)THEN                                                       KFPARA2.1486
           DSSDT=(CLIQ-BLIQ*DLIQ)/((TZ(ND)-DLIQ)*(TZ(ND)-DLIQ))                  KFPARA2.1487
           RL=XLV0-XLV1*TZ(ND)                                                   KFPARA2.1488
           DTMP=RL*QSS*(1.-RHH)/(CP+RL*RHH*QSS*DSSDT)                            KFPARA2.1489
           T1RH=TZ(ND)+DTMP                                                      KFPARA2.1490
           ES=RHH*ALIQ*EXP((BLIQ*T1RH-CLIQ)/(T1RH-DLIQ))                         KFPARA2.1491
           QSRH=0.622*ES/(P0(ND)-ES)                                             KFPARA2.1492
C                                                                                KFPARA2.1493
C...CHECK TO SEE IF MIXING RATIO AT SPECIFIED RH IS LESS THAN ACTUAL             KFPARA2.1494
C...MIXING RATIO...IF SO, ADJUST TO GIVE ZERO EVAPORATION...                     KFPARA2.1495
C                                                                                KFPARA2.1496
           IF(QSRH.LT.QD(ND))THEN                                                KFPARA2.1497
             QSRH=QD(ND)                                                         KFPARA2.1498
             T1RH=TZ(ND)+(QSS-QSRH)*RL/CP                                        KFPARA2.1499
c             T1RH=TZ(ND)                                                        KFPARA2.1500
           ENDIF                                                                 KFPARA2.1501
           TZ(ND)=T1RH                                                           KFPARA2.1502
           QSS=QSRH                                                              KFPARA2.1503
           QSD(ND) = QSS                                                         KFPARA2.1504
         ENDIF                                                                   KFPARA2.1505
         TVD(nd) = tz(nd)*(1.+0.608*qsd(nd))                                     KFPARA2.1506
         IF(TVD(ND).GT.TV0(ND).OR.ND.EQ.1)THEN                                   KFPARA2.1507
c        IF(TVD(ND).GT.TV0(ND).OR.ND.EQ.LC)THEN                                  KFPARA2.1508
            LDB=ND                                                               KFPARA2.1509
            GOTO 139                                                             KFPARA2.1510
          ENDIF                                                                  KFPARA2.1511
 138    CONTINUE                                                                 KFPARA2.1512
 139    CONTINUE                                                                 KFPARA2.1513
c         print *,'LFS, LDB =',P0(LFS),P0(LDB)                                   KFPARA2.1514
        IF((P0(LDB)-P0(LFS)) .lt. 50.E2)THEN   ! No Downdraft allowed!           KFPARA2.1515
          TDER = 0.                                                              KFPARA2.1516
c         write(98,*)'downdraft not allowed...too shallow'                       KFPARA2.1517
c         write(6,*)'downdraft not allowed...too shallow'                        KFPARA2.1518
c         print *,'P0(LDB), P0(LFS) =',P0(LDB), P0(LFS)                          KFPARA2.1519
c         call flush(98)                                                         KFPARA2.1520
          GOTO 141                                                               KFPARA2.1521
        ENDIF                                                                    KFPARA2.1522
        TDER=0.                                                                  KFPARA2.1523
C                                                                                KFPARA2.1524
C...CALCULATE AN EVAPORATION RATE FOR GIVEN MASS FLUX...                         KFPARA2.1525
C                                                                                KFPARA2.1526
c         print *,'LFS, LDB, LDT, I, J =',P0(LFS),P0(LDB),P0(LDT)                KFPARA2.1527
c    *,i,j                                                                       KFPARA2.1528
c     if(i.eq.96 .and. j.eq.44)then                                              KFPARA2.1529
c       write(15,4433) istid,rlat,rlon,idate(3)-1900,                            KFPARA2.1530
c    & idate(1),idate(2),ihrst,ifhr                                              KFPARA2.1531
c        write(15,4422) lmh+1                                                    KFPARA2.1532
c       write(96,4422) i,j,kl                                                    KFPARA2.1533
c       do 550 nl=1,kl                                                           KFPARA2.1534
c        l = kl-nl+1                                                             KFPARA2.1535
c       write(96,4455) p0(l)*0.01,t0(l)-273.16,q0(l)*1000.,                      KFPARA2.1536
c    *                  u0(l),v0(l),w0avg(i,j,nl),dp(l),q2(i,j,nl)               KFPARA2.1537
c 550   continue                                                                 KFPARA2.1538
c     endif                                                                      KFPARA2.1539
c4433  format(i6,2f8.2,i5,4i3)                                                   KFPARA2.1540
c4422  format(i6)                                                                KFPARA2.1541
c4455  format(8f12.3)                                                            KFPARA2.1542
        DO 135 ND=LDT,LDB,-1                                                     KFPARA2.1543
          ND1 = ND+1                                                             KFPARA2.1544
          DDR(ND) = -DMF(KSTART)*DP(ND)/DPDD                                     KFPARA2.1545
          DER(ND) = 0.                                                           KFPARA2.1546
          DMF(ND) = DMF(ND1)+DDR(ND)                                             KFPARA2.1547
         TDER=TDER+(QSD(nd)-QD(ND))*DDR(ND)                                      KFPARA2.1548
         QD(ND)=QSD(nd)                                                          KFPARA2.1549
         THTAD(ND)=TZ(ND)*(P00/P0(ND))**(0.2854*(1.-0.28*QD(ND)))                KFPARA2.1550
 135   CONTINUE                                                                  KFPARA2.1551
C                                                                                KFPARA2.1552
C...IF DOWNDRAFT DOES NOT EVAPORATE ANY WATER FOR SPECIFIED RELATIVE             KFPARA2.1553
C...HUMIDITY, NO DOWNDRAFT IS ALLOWED...                                         KFPARA2.1554
C                                                                                KFPARA2.1555
  141   IF(TDER.LT.1.)THEN                                                       KFPARA2.1556
c           WRITE(98,3004)I,J                                                    KFPARA2.1557
c           WRITE(6,3004)I,J,ISHALL                                              KFPARA2.1558
 3004       FORMAT(' ','No Downdraft!;  I=',I3,2X,'J=',I3,'ISHALL =',I2)         KFPARA2.1559
          PPTFLX=TRPPT                                                           KFPARA2.1560
          CPR=TRPPT                                                              KFPARA2.1561
          TDER=0.                                                                KFPARA2.1562
          CNDTNF=0.                                                              KFPARA2.1563
          UPDINC=1.                                                              KFPARA2.1564
          LDB=LFS                                                                KFPARA2.1565
          DO 117 NDK=1,LTOP                                                      KFPARA2.1566
            DMF(NDK)=0.                                                          KFPARA2.1567
            DER(NDK)=0.                                                          KFPARA2.1568
            DDR(NDK)=0.                                                          KFPARA2.1569
            THTAD(NDK)=0.                                                        KFPARA2.1570
            WD(NDK)=0.                                                           KFPARA2.1571
            TZ(NDK)=0.                                                           KFPARA2.1572
  117     QD(NDK)=0.                                                             KFPARA2.1573
          AINCM2=100.                                                            KFPARA2.1574
          GOTO 165                                                               KFPARA2.1575
        ENDIF                                                                    KFPARA2.1576
C                                                                                KFPARA2.1577
C...ADJUST DOWNDRAFT MASS FLUX SO THAT EVAPORATION RATE IN DOWNDRAFT IS          KFPARA2.1578
C...CONSISTENT WITH PRECIPITATION EFFICIENCY RELATIONSHIP...                     KFPARA2.1579
C                                                                                KFPARA2.1580
c       DEVDMF=TDER/DMF(LFS)                                                     KFPARA2.1581
c       PPR=0.                                                                   KFPARA2.1582
c       PPTFLX=PEFF*USR                                                          KFPARA2.1583
c       RCED=TRPPT-PPTFLX                                                        KFPARA2.1584
C                                                                                KFPARA2.1585
C...PPR IS THE TOTAL AMOUNT OF PRECIPITATION THAT FALLS  OUT OF THE UPDR         KFPARA2.1586
C...FROM CLOUD BASE TO THE LFS...UPDRAFT MASS FLUX WILL BE INCREASED UP          KFPARA2.1587
C...THE LFS TO ACCOUNT FOR UPDRAFT AIR MIXING WITH ENVIRONMENTAL AIR TO          KFPARA2.1588
C...THE UPDRAFT, SO PPR WILL INCREASE PROPORTIONATELY...                         KFPARA2.1589
C                                                                                KFPARA2.1590
c       DO 132 NM=KLCL,LFS                                                       KFPARA2.1591
c 132   PPR=PPR+PPTLIQ(NM)+PPTICE(NM)                                            KFPARA2.1592
c       IF(LFS.GE.KLCL)THEN                                                      KFPARA2.1593
c         DPPTDF=(1.-PEFF)*PPR*(1.-EQFRC(LFS))/UMF(LFS)                          KFPARA2.1594
c       ELSE                                                                     KFPARA2.1595
c         DPPTDF=0.                                                              KFPARA2.1596
c       ENDIF                                                                    KFPARA2.1597
C                                                                                KFPARA2.1598
C...CNDTNF IS THE AMOUNT OF CONDENSATE TRANSFERRED ALONG WITH UPDRAFT MA         KFPARA2.1599
C...THE DOWNDRAFT AT THE LFS...                                                  KFPARA2.1600
C                                                                                KFPARA2.1601
c       CNDTNF=(QLIQ(LFS)+QICE(LFS))*(1.-EQFRC(LFS))                             KFPARA2.1602
c       DMFLFS=RCED/(DEVDMF+DPPTDF+CNDTNF)                                       KFPARA2.1603
c       IF(DMFLFS.GT.0.)THEN                                                     KFPARA2.1604
c         TDER=0.                                                                KFPARA2.1605
c         GOTO 141                                                               KFPARA2.1606
c       ENDIF                                                                    KFPARA2.1607
C                                                                                KFPARA2.1608
C...DDINC IS THE FACTOR BY WHICH TO INCREASE THE FIRST-GUESS DOWNDRAFT M         KFPARA2.1609
C...FLUX TO SATISFY THE PRECIP EFFICIENCY RELATIONSHIP, UPDINC IS THE FA         KFPARA2.1610
C...WHICH TO INCREASE THE UPDRAFT MASS FLUX BELOW THE LFS TO ACCOUNT FOR         KFPARA2.1611
C...TRANSFER OF MASS FROM UPDRAFT TO DOWNDRAFT...                                KFPARA2.1612
C                                                                                KFPARA2.1613
c        DDINC=DMFLFS/DMF(LFS)                              ! JSK MODS           KFPARA2.1614
c      IF(DMF(KLCL).LT.0.)THEN                                                   KFPARA2.1615
c      IF(DMF(KSTART).LT.0.)THEN                                                 KFPARA2.1616
c         DDINC = -UMF(KLCL)/DMF(KLCL)                                           KFPARA2.1617
c        DDINC = -DMFFRC*UMF(KLCL)/DMF(KLCL)                                     KFPARA2.1618
         DDINC = -DMFFRC*UMF(KLCL)/DMF(KSTART)                                   KFPARA2.1619
c      ELSE                                                                      KFPARA2.1620
C        DDINC = -UMF(KLCL)/DMF(LFS)                                             KFPARA2.1621
c        DDINC = -DMFFRC*UMF(KLCL)/DMF(LFS)                                      KFPARA2.1622
c      ENDIF                                                                     KFPARA2.1623
       UPDINC=1.                                                                 KFPARA2.1624
c      IF(TDER*DDINC.GT.0.9*TRPPT)THEN                                           KFPARA2.1625
c        DDINC = 0.9*TRPPT/TDER                                                  KFPARA2.1626
       IF(TDER*DDINC.GT.TRPPT)THEN                                               KFPARA2.1627
         DDINC = TRPPT/TDER                                                      KFPARA2.1628
c        ISHALL=1                                                                KFPARA2.1629
c        NCHM = LC                                                               KFPARA2.1630
c        KMIX = NCHM                                                             KFPARA2.1631
c        DO 707 NK=K,LTOP                                                        KFPARA2.1632
c         UMF(NK)=0.                                                             KFPARA2.1633
c         UDR(NK)=0.                                                             KFPARA2.1634
c         UER(NK)=0.                                                             KFPARA2.1635
c         DETLQ(NK)=0.                                                           KFPARA2.1636
c         DETIC(NK)=0.                                                           KFPARA2.1637
c         PPTLIQ(NK)=0.                                                          KFPARA2.1638
c         PPTICE(NK)=0.                                                          KFPARA2.1639
c707     CONTINUE                                                                KFPARA2.1640
c        DO NKk = 0,NLAYRS-1                                                     KFPARA2.1641
c          nk = KPBL-NkK                                                         KFPARA2.1642
c          QUER(NK) = Q0(nk)                                                     KFPARA2.1643
c        ENDDO                                                                   KFPARA2.1644
c       PRINT *,' '                                                              KFPARA2.1645
c       print *,'I, J, NTSD =',I,J,NTSD                                          KFPARA2.1646
c       print *,'!!! FOR SOURCE LAYER CENTERED AT ',PMIX/100.,'mb,'              KFPARA2.1647
c       print *,'!!! DOWNDRAFT EVAPORATES ALL CONDENSATE...REVERT TO'            KFPARA2.1648
c       print *,'!!! SHALLOW CONVECTION !!!'                                     KFPARA2.1649
c       PRINT *,' '                                                              KFPARA2.1650
c        GOTO 25                                                                 KFPARA2.1651
       ENDIF                                                                     KFPARA2.1652
       TDER = TDER*DDINC                                                         KFPARA2.1653
c      print *,'TDER, TRPPT =',tder,trppt                                        KFPARA2.1654
C       IF(LFS.GE.KLCL)THEN                                                      KFPARA2.1655
c         UPDINC=(UMF(LFS)-(1.-EQFRC(LFS))*DMFLFS)/UMF(LFS)                      KFPARA2.1656
C                                                           ! JSK MODS           KFPARA2.1657
C...LIMIT UPDINC TO LESS THAN OR EQUAL TO 1.5...            ! JSK MODS           KFPARA2.1658
C                                                           ! JSK MODS           KFPARA2.1659
c         IF(UPDINC.GT.1.5)THEN                             ! JSK MODS           KFPARA2.1660
c           UPDINC = 1.5                                    ! JSK MODS           KFPARA2.1661
c           DMFLFS2 = UMF(LFS)*(UPDINC-1.)/(EQFRC(LFS)-1.)  ! JSK MODS           KFPARA2.1662
c           RCED2 = DMFLFS2*(DEVDMF+DPPTDF+CNDTNF)          ! JSK MODS           KFPARA2.1663
c           PPTFLX = PPTFLX+(RCED-RCED2)                    ! JSK MODS           KFPARA2.1664
c           PEFF2 = PPTFLX/USR                              ! JSK MODS           KFPARA2.1665
c           RCED = RCED2                                    ! JSK MODS           KFPARA2.1666
c           DMFLFS = DMFLFS2                                ! JSK MODS           KFPARA2.1667
c         ENDIF                                             ! JSK MODS           KFPARA2.1668
c       ELSE                                                                     KFPARA2.1669
c         UPDINC=1.                                                              KFPARA2.1670
c       ENDIF                                                                    KFPARA2.1671
c       DDINC=DMFLFS/DMF(LFS)                     ! JSK MODS                     KFPARA2.1672
        DO 149 NK=LDB,LFS                                                        KFPARA2.1673
          DMF(NK)=DMF(NK)*DDINC                                                  KFPARA2.1674
          DER(NK)=DER(NK)*DDINC                                                  KFPARA2.1675
          DDR(NK)=DDR(NK)*DDINC                                                  KFPARA2.1676
  149   CONTINUE                                                                 KFPARA2.1677
C      CPR=TRPPT+PPR*(UPDINC-1.)                                                 KFPARA2.1678
        CPR=TRPPT                                                                KFPARA2.1679
        PPTFLX = TRPPT-TDER                                                      KFPARA2.1680
c      print *,'PPTFLX = ',PPTFLX                                                KFPARA2.1681
C        PPTFLX=PPTFLX+PEFF*PPR*(UPDINC-1.)                                      KFPARA2.1682
c        PEFF=PEFF2                                ! JSK MODS                    KFPARA2.1683
        PEFF=PPTFLX/TRPPT                                                        KFPARA2.1684
c       write(*,*)'PRECIP EFFICIENCY =',PEFF                                     KFPARA2.1685
        IF(IPRNT)THEN                                                            KFPARA2.1686
          write(98,*)'PRECIP EFFICIENCY =',PEFF                                  KFPARA2.1687
c        call flush(98)                                                          KFPARA2.1688
        ENDIF                                                                    KFPARA2.1689
c        TDER=TDER*DDINC                                                         KFPARA2.1690
C                                                                                KFPARA2.1691
C...ADJUST UPDRAFT MASS FLUX, MASS DETRAINMENT RATE, AND LIQUID WATER AN         KFPARA2.1692
C   DETRAINMENT RATES TO BE CONSISTENT WITH THE TRANSFER OF THE ESTIMATE         KFPARA2.1693
C   FROM THE UPDRAFT TO THE DOWNDRAFT AT THE LFS...                              KFPARA2.1694
C                                                                                KFPARA2.1695
        DO 155 NK=LC,LFS                                                         KFPARA2.1696
          UMF(NK)=UMF(NK)*UPDINC                                                 KFPARA2.1697
          UDR(NK)=UDR(NK)*UPDINC                                                 KFPARA2.1698
          UER(NK)=UER(NK)*UPDINC                                                 KFPARA2.1699
          PPTLIQ(NK)=PPTLIQ(NK)*UPDINC                                           KFPARA2.1700
          PPTICE(NK)=PPTICE(NK)*UPDINC                                           KFPARA2.1701
          DETLQ(NK)=DETLQ(NK)*UPDINC                                             KFPARA2.1702
  155   DETIC(NK)=DETIC(NK)*UPDINC                                               KFPARA2.1703
C                                                                                KFPARA2.1704
C...ZERO OUT THE ARRAYS FOR DOWNDRAFT DATA AT LEVELS ABOVE AND BELOW THE         KFPARA2.1705
C...DOWNDRAFT...                                                                 KFPARA2.1706
C                                                                                KFPARA2.1707
        IF(LDB.GT.1)THEN                                                         KFPARA2.1708
          DO 156 NK=1,LDB-1                                                      KFPARA2.1709
            DMF(NK)=0.                                                           KFPARA2.1710
            DER(NK)=0.                                                           KFPARA2.1711
            DDR(NK)=0.                                                           KFPARA2.1712
            WD(NK)=0.                                                            KFPARA2.1713
            TZ(NK)=0.                                                            KFPARA2.1714
            QD(NK)=0.                                                            KFPARA2.1715
            THTAD(NK)=0.                                                         KFPARA2.1716
  156     CONTINUE                                                               KFPARA2.1717
        ENDIF                                                                    KFPARA2.1718
        DO 157 NK=LFS+1,KX                                                       KFPARA2.1719
          DMF(NK)=0.                                                             KFPARA2.1720
          DER(NK)=0.                                                             KFPARA2.1721
          DDR(NK)=0.                                                             KFPARA2.1722
          WD(NK)=0.                                                              KFPARA2.1723
          TZ(NK)=0.                                                              KFPARA2.1724
          QD(NK)=0.                                                              KFPARA2.1725
          THTAD(NK)=0.                                                           KFPARA2.1726
  157   CONTINUE                                                                 KFPARA2.1727
        DO 158 NK=LDT+1,LFS-1                                                    KFPARA2.1728
          TZ(NK)=0.                                                              KFPARA2.1729
          QD(NK)=0.                                                              KFPARA2.1730
          THTAD(NK)=0.                                   ! JSK MODS              KFPARA2.1731
  158   CONTINUE                                                                 KFPARA2.1732
C                                                                                KFPARA2.1733
C...SET LIMITS ON THE UPDRAFT AND DOWNDRAFT MASS FLUXES SO THAT THE INFL         KFPARA2.1734
C   INTO CONVECTIVE DRAFTS FROM A GIVEN LAYER IS NO MORE THAN IS AVAILAB         KFPARA2.1735
C   IN THAT LAYER INITIALLY...                                                   KFPARA2.1736
C                                                                                KFPARA2.1737
  165   AINCMX=1000.                                                             KFPARA2.1738
        LMAX=MAX0(KLCL,LFS)                                                      KFPARA2.1739
        DO 166 NK=LC,LMAX                                                        KFPARA2.1740
         IF((UER(NK)-DER(NK)).GT.1.e-3)THEN                                      23DEC04.3183
           AINCM1=EMS(NK)/((UER(NK)-DER(NK))*TIMEC)                              23DEC04.3184
           AINCMX=AMIN1(AINCMX,AINCM1)                                           23DEC04.3185
         ENDIF                                                                   23DEC04.3186
  166   CONTINUE                                                                 KFPARA2.1744
        AINC=1.                                                                  KFPARA2.1745
        IF(AINCMX.LT.AINC)AINC=AINCMX                                            KFPARA2.1746
C                                                                                KFPARA2.1747
C...SAVE THE RELEVENT VARIABLES FOR A UNIT UPDRFT AND DOWNDRFT...THEY WI         KFPARA2.1748
C...ITERATIVELY ADJUSTED BY THE FACTOR AINC TO SATISFY THE STABILIZATION         KFPARA2.1749
C...CLOSURE...                                                                   KFPARA2.1750
C                                                                                KFPARA2.1751
        NCOUNT=0                                                                 KFPARA2.1752
        TDER2=TDER                                                               KFPARA2.1753
        PPTFL2=PPTFLX                                                            KFPARA2.1754
        DO 170 NK=1,LTOP                                                         KFPARA2.1755
          DETLQ2(NK)=DETLQ(NK)                                                   KFPARA2.1756
          DETIC2(NK)=DETIC(NK)                                                   KFPARA2.1757
          UDR2(NK)=UDR(NK)                                                       KFPARA2.1758
          UER2(NK)=UER(NK)                                                       KFPARA2.1759
          DDR2(NK)=DDR(NK)                                                       KFPARA2.1760
          DER2(NK)=DER(NK)                                                       KFPARA2.1761
          UMF2(NK)=UMF(NK)                                                       KFPARA2.1762
          DMF2(NK)=DMF(NK)                                                       KFPARA2.1763
  170   CONTINUE                                                                 KFPARA2.1764
        FABE=1.                                                                  KFPARA2.1765
        STAB=0.95                                                                KFPARA2.1766
        NOITR=0                                                                  KFPARA2.1767
        ISTOP=0                                                                  KFPARA2.1768
        IF(AINC/AINCMX.GT.0.999)THEN                                             KFPARA2.1769
          NCOUNT=0                                                               KFPARA2.1770
          GOTO 255                                                               KFPARA2.1771
        ENDIF                                                                    KFPARA2.1772
c...Shallow convection mods...                                                   KFPARA2.1773
c!!!! for mm5, just use TKEMAX = 0.5 for now !!!!!                               KFPARA2.1774
        IF(ISHALL.EQ.1)THEN                                                      KFPARA2.1775
c                                                                                KFPARA2.1776
c...find the maximum TKE value between LC and KLCL...                            KFPARA2.1777
c         TKEMAX = 0.                                                            KFPARA2.1778
          TKEMAX = 5.                                                            KFPARA2.1779
c          DO 173 K = LC,KLCL                                                    KFPARA2.1780
c            NK = KX-K+1-NLEV                                                    KFPARA2.1781
c            TKEMAX = AMAX1(TKEMAX,Q2(I,J,NK))                                   KFPARA2.1782
c 173      CONTINUE                                                              KFPARA2.1783
c          TKEMAX = AMIN1(TKEMAX,10.)                                            KFPARA2.1784
c          TKEMAX = AMAX1(TKEMAX,5.)                                             KFPARA2.1785
cc         TKEMAX = 10.                                                          KFPARA2.1786
cc...3_24_99...DPMIN was changed for shallow convection so that it is the        KFPARA2.1787
cc...          the same as for deep convection (5.E3).  Since this doubles       KFPARA2.1788
cc...          (roughly) the value of DPTHMX, add a factor of 0.5 to calcu-      KFPARA2.1789
cc...          lation of EVAC...                                                 KFPARA2.1790
cc         EVAC  = TKEMAX*0.1                                                    KFPARA2.1791
          EVAC  = 0.5*TKEMAX*0.1                                                 KFPARA2.1792
c         AINC = 0.1*DPTHMX*DXIJ*DXIJ/(VMFLCL*G*TIMEC)                           KFPARA2.1793
c          AINC = EVAC*DPTHMX*DX(I,J)*DX(I,J)/(VMFLCL*G*TIMEC)                   KFPARA2.1794
          AINC = EVAC*DPTHMX*DXSQ/(VMFLCL*G*TIMEC)                               KFPARA2.1795
c         PRINT *,'Shallow convection AINC =',ainc                               KFPARA2.1796
c         if(evac.gt.0.3)print *,'I, J, EVAC, AINC =',I,J,EVAC,ainc              KFPARA2.1797
c         if(i.eq.66 .and. j.eq.117)print *,                                     KFPARA2.1798
c    *'OMAHA: XTIME, EVAC, AINC =',xtime,EVAC,ainc                               KFPARA2.1799
          GOTO 255                                                               KFPARA2.1800
        ENDIF                                                                    KFPARA2.1801
c...end of Shallow mods...                                                       KFPARA2.1802
  175   NCOUNT=NCOUNT+1                                                          KFPARA2.1803
C                                                                                KFPARA2.1804
C*****************************************************************               KFPARA2.1805
C                                                                *               KFPARA2.1806
C           COMPUTE PROPERTIES FOR COMPENSATIONAL SUBSIDENCE     *               KFPARA2.1807
C                                                                *               KFPARA2.1808
C*****************************************************************               KFPARA2.1809
C                                                                                KFPARA2.1810
C...DETERMINE OMEGA VALUE NECESSARY AT TOP AND BOTTOM OF EACH LAYER TO           KFPARA2.1811
C...SATISFY MASS CONTINUITY...                                                   KFPARA2.1812
C                                                                                KFPARA2.1813
  185   CONTINUE                                                                 KFPARA2.1814
        DTT=TIMEC                                                                KFPARA2.1815
        DO 200 NK=1,LTOP                                                         KFPARA2.1816
          DOMGDP(NK)=-(UER(NK)-DER(NK)-UDR(NK)-DDR(NK))*EMSD(NK)                 KFPARA2.1817
          IF(NK.GT.1)THEN                                                        KFPARA2.1818
            OMG(NK)=OMG(NK-1)-DP(NK-1)*DOMGDP(NK-1)                              KFPARA2.1819
            ABSOMG = ABS(OMG(NK))                                                23DEC04.3187
            ABSOMGTC = ABSOMG*TIMEC                                              23DEC04.3188
            FRDP = 0.75*DP(NK-1)                                                 23DEC04.3189
            IF(ABSOMGTC.GT.FRDP)THEN                                             23DEC04.3190
              DTT1 = FRDP/ABSOMG                                                 23DEC04.3191
              DTT=AMIN1(DTT,DTT1)                                                23DEC04.3192
            ENDIF                                                                23DEC04.3193
          ENDIF                                                                  KFPARA2.1822
  200   CONTINUE                                                                 KFPARA2.1823
        DO 488 NK=1,LTOP                                                         KFPARA2.1824
          THPA(NK)=THTA0(NK)                                                     KFPARA2.1825
          QPA(NK)=Q0(NK)                                                         KFPARA2.1826
          NSTEP=NINT(TIMEC/DTT+1)                                                KFPARA2.1827
          DTIME=TIMEC/FLOAT(NSTEP)                                               KFPARA2.1828
          FXM(NK)=OMG(NK)*DXSQ/G                                                 KFPARA2.1829
  488   CONTINUE                                                                 KFPARA2.1830
C                                                                                KFPARA2.1831
C...DO AN UPSTREAM/FORWARD-IN-TIME ADVECTION OF THETA, QV...                     KFPARA2.1832
C                                                                                KFPARA2.1833
        DO 495 NTC=1,NSTEP                                                       KFPARA2.1834
C                                                                                KFPARA2.1835
C...ASSIGN THETA AND Q VALUES AT THE TOP AND BOTTOM OF EACH LAYER BASED          KFPARA2.1836
C...SIGN OF OMEGA...                                                             KFPARA2.1837
C                                                                                KFPARA2.1838
          DO 493 NK=1,LTOP                                                       KFPARA2.1839
            THFXIN(NK)=0.                                                        KFPARA2.1840
            THFXOUT(NK)=0.                                                       KFPARA2.1841
            QFXIN(NK)=0.                                                         KFPARA2.1842
  493     QFXOUT(NK)=0.                                                          KFPARA2.1843
          DO 494 NK=2,LTOP                                                       KFPARA2.1844
            IF(OMG(NK).LE.0.)THEN                                                KFPARA2.1845
              THFXIN(NK)=-FXM(NK)*THPA(NK-1)                                     KFPARA2.1846
              QFXIN(NK)=-FXM(NK)*QPA(NK-1)                                       KFPARA2.1847
              THFXOUT(NK-1)=THFXOUT(NK-1)+THFXIN(NK)                             KFPARA2.1848
              QFXOUT(NK-1)=QFXOUT(NK-1)+QFXIN(NK)                                KFPARA2.1849
            ELSE                                                                 KFPARA2.1850
              THFXOUT(NK)=FXM(NK)*THPA(NK)                                       KFPARA2.1851
              QFXOUT(NK)=FXM(NK)*QPA(NK)                                         KFPARA2.1852
              THFXIN(NK-1)=THFXIN(NK-1)+THFXOUT(NK)                              KFPARA2.1853
              QFXIN(NK-1)=QFXIN(NK-1)+QFXOUT(NK)                                 KFPARA2.1854
            ENDIF                                                                KFPARA2.1855
  494     CONTINUE                                                               KFPARA2.1856
C                                                                                KFPARA2.1857
C...UPDATE THE THETA AND QV VALUES AT EACH LEVEL...                              KFPARA2.1858
C                                                                                KFPARA2.1859
          DO 492 NK=1,LTOP                                                       KFPARA2.1860
            THPA(NK)=THPA(NK)+(THFXIN(NK)+UDR(NK)*THTAU(NK)+DDR(NK)*             KFPARA2.1861
     +               THTAD(NK)-THFXOUT(NK)-(UER(NK)-DER(NK))*THTA0(NK))*         KFPARA2.1862
     +               DTIME*EMSD(NK)                                              KFPARA2.1863
            IF(NK.GE.LC .AND. NK.LE.KPBL)THEN                                    KFPARA2.1864
            QPA(NK)=QPA(NK)+(QFXIN(NK)+UDR(NK)*QDT(NK)+DDR(NK)*QD(NK)-           KFPARA2.1865
     +              QFXOUT(NK)-UER(NK)*QUER(NK)+DER(NK)*Q0(NK))*                 KFPARA2.1866
     +              DTIME*EMSD(NK)                                               KFPARA2.1867
            ELSE                                                                 KFPARA2.1868
            QPA(NK)=QPA(NK)+(QFXIN(NK)+UDR(NK)*QDT(NK)+DDR(NK)*QD(NK)-           KFPARA2.1869
     +              QFXOUT(NK)-(UER(NK)-DER(NK))*Q0(NK))*DTIME*EMSD(NK)          KFPARA2.1870
            ENDIF                                                                KFPARA2.1871
  492     CONTINUE                                                               KFPARA2.1872
  495   CONTINUE                                                                 KFPARA2.1873
        DO 498 NK=1,LTOP                                                         KFPARA2.1874
          THTAG(NK)=THPA(NK)                                                     KFPARA2.1875
          QG(NK)=QPA(NK)                                                         KFPARA2.1876
  498   CONTINUE                                                                 KFPARA2.1877
C                                                                                KFPARA2.1878
C...CHECK TO SEE IF MIXING RATIO DIPS BELOW ZERO ANYWHERE;  IF SO, BORRO         KFPARA2.1879
C...MOISTURE FROM ADJACENT LAYERS TO BRING IT BACK UP ABOVE ZERO...              KFPARA2.1880
C                                                                                KFPARA2.1881
      DO 499 NK=1,LTOP                                                           KFPARA2.1882
        IF(QG(NK).LT.0.)THEN                                                     KFPARA2.1883
          IF(NK.EQ.1)THEN                             ! JSK MODS                 KFPARA2.1884
            PRINT *,'!!!!! PROBLEM WITH KF SCHEME:  ' ! JSK MODS                 KFPARA2.1885
            PRINT *,'QG = 0 AT THE SURFACE!!!!!!!'    ! JSK MODS                 KFPARA2.1886
            STOP 'QG'                                 ! JSK MODS                 KFPARA2.1887
          ENDIF                                       ! JSK MODS                 KFPARA2.1888
          NK1=NK+1                                                               KFPARA2.1889
          IF(NK.EQ.LTOP)NK1=KLCL                                                 KFPARA2.1890
          TMA=QG(NK1)*EMS(NK1)                                                   KFPARA2.1891
          TMB=QG(NK-1)*EMS(NK-1)                                                 KFPARA2.1892
          TMM=(QG(NK)-1.E-9)*EMS(NK  )                                           KFPARA2.1893
          BCOEFF=-TMM/((TMA*TMA)/TMB+TMB)                                        KFPARA2.1894
          ACOEFF=BCOEFF*TMA/TMB                                                  KFPARA2.1895
          TMB=TMB*(1.-BCOEFF)                                                    KFPARA2.1896
          TMA=TMA*(1.-ACOEFF)                                                    KFPARA2.1897
          IF(NK.EQ.LTOP)THEN                                                     KFPARA2.1898
            QVDIFF=(QG(NK1)-TMA*EMSD(NK1))*100./QG(NK1)                          KFPARA2.1899
            IF(ABS(QVDIFF).GT.1.)THEN                                            KFPARA2.1900
           PRINT *,'!!!WARNING!!! CLOUD BASE WATER VAPOR CHANGES BY ',           KFPARA2.1901
     *              QVDIFF,                                                      KFPARA2.1902
     *             '% WHEN MOISTURE IS BORROWED TO PREVENT NEGATIVE ',           KFPARA2.1903
     *             'VALUES IN KAIN-FRITSCH'                                      KFPARA2.1904
            ENDIF                                                                KFPARA2.1905
          ENDIF                                                                  KFPARA2.1906
          QG(NK)=1.E-9                                                           KFPARA2.1907
          QG(NK1)=TMA*EMSD(NK1)                                                  KFPARA2.1908
          QG(NK-1)=TMB*EMSD(NK-1)                                                KFPARA2.1909
        ENDIF                                                                    KFPARA2.1910
499   CONTINUE                                                                   KFPARA2.1911
        TOPOMG=(UDR(LTOP)-UER(LTOP))*DP(LTOP)*EMSD(LTOP)                         KFPARA2.1912
        IF(ABS(TOPOMG-OMG(LTOP)).GT.1.E-3)THEN                                   KFPARA2.1913
        WRITE(99,*)'ERROR:  MASS DOES NOT BALANCE IN KF SCHEME;                  KFPARA2.1914
     * TOPOMG, OMG =',TOPOMG,OMG(LTOP)                                           KFPARA2.1915
        write(6,*)'I, J, KTAU =',i,j,ktau                                        KFPARA2.1916
        WRITE(6,*)'ERROR:  MASS DOES NOT BALANCE IN KF SCHEME;                   KFPARA2.1917
     * TOPOMG, OMG =',TOPOMG,OMG(LTOP)                                           KFPARA2.1918
          ISTOP=1                                                                KFPARA2.1919
          IPRNT=.TRUE.                                                           KFPARA2.1920
          GOTO 265                                                               KFPARA2.1921
        ENDIF                                                                    KFPARA2.1922
C                                                                                KFPARA2.1923
C...CONVERT THETA TO T...                                                        KFPARA2.1924
C                                                                                KFPARA2.1925
        DO 230 NK=1,LTOP                                                         KFPARA2.1926
          EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*QG(NK)))                        KFPARA2.1927
          TG(NK)=THTAG(NK)/EXN(NK)                                               KFPARA2.1928
          TVG(NK)=TG(NK)*(1.+0.608*QG(NK))                                       KFPARA2.1929
  230   CONTINUE                                                                 KFPARA2.1930
c...Shallow mods...                                                              KFPARA2.1931
        IF(ISHALL.EQ.1)THEN                                                      KFPARA2.1932
          GOTO 265                                                               KFPARA2.1933
        ENDIF                                                                    KFPARA2.1934
C                                                                                KFPARA2.1935
C*******************************************************************             KFPARA2.1936
C                                                                  *             KFPARA2.1937
C     COMPUTE NEW CLOUD AND CHANGE IN AVAILABLE BUOYANT ENERGY.    *             KFPARA2.1938
C                                                                  *             KFPARA2.1939
C*******************************************************************             KFPARA2.1940
C                                                                                KFPARA2.1941
C...THE FOLLOWING COMPUTATIONS ARE SIMILAR TO THAT FOR UPDRAFT                   KFPARA2.1942
C                                                                                KFPARA2.1943
c        THMIX=0.                                                                KFPARA2.1944
        TMIX=0.                                                                  KFPARA2.1945
        QMIX=0.                                                                  KFPARA2.1946
C                                                                                KFPARA2.1947
C...FIND THE THERMODYNAMIC CHARACTERISTICS OF THE LAYER BY                       KFPARA2.1948
C...MASS-WEIGHTING THE CHARACTERISTICS OF THE INDIVIDUAL MODEL                   KFPARA2.1949
C...LAYERS...                                                                    KFPARA2.1950
C                                                                                KFPARA2.1951
        DO 717 NK=LC,KPBL                                                        KFPARA2.1952
          TMIX=TMIX+DP(NK)*TG(NK)                                                KFPARA2.1953
          QMIX=QMIX+DP(NK)*QG(NK)                                                KFPARA2.1954
 717    CONTINUE                                                                 KFPARA2.1955
        TMIX=TMIX/DPTHMX                                                         KFPARA2.1956
        QMIX=QMIX/DPTHMX                                                         KFPARA2.1957
        ES=ALIQ*EXP((TMIX*BLIQ-CLIQ)/(TMIX-DLIQ))                                KFPARA2.1958
        QSS=0.622*ES/(PMIX-ES)                                                   KFPARA2.1959
C                                                                                KFPARA2.1960
C...REMOVE SUPERSATURATION FOR DIAGNOSTIC PURPOSES, IF NECESSARY...              KFPARA2.1961
C                                                                                KFPARA2.1962
        IF(QMIX.GT.QSS)THEN                                                      KFPARA2.1963
          RL=XLV0-XLV1*TMIX                                                      KFPARA2.1964
          CPM=CP*(1.+0.887*QMIX)                                                 KFPARA2.1965
          DSSDT=QSS*(CLIQ-BLIQ*DLIQ)/((TMIX-DLIQ)*(TMIX-DLIQ))                   KFPARA2.1966
          DQ=(QMIX-QSS)/(1.+RL*DSSDT/CPM)                                        KFPARA2.1967
          TMIX=TMIX+RL/CP*DQ                                                     KFPARA2.1968
          QMIX=QMIX-DQ                                                           KFPARA2.1969
          TLCL=TMIX                                                              KFPARA2.1970
        ELSE                                                                     KFPARA2.1971
          QMIX=AMAX1(QMIX,0.)                                                    KFPARA2.1972
          EMIX=QMIX*PMIX/(0.622+QMIX)                                            KFPARA2.1973
          astrt=1.e-3                                                            KFPARA2.1974
          binc=0.075                                                             KFPARA2.1975
          a1=emix/aliq                                                           KFPARA2.1976
          tp=(a1-astrt)/binc                                                     KFPARA2.1977
          indlu=int(tp)+1                                                        KFPARA2.1978
          value=(indlu-1)*binc+astrt                                             KFPARA2.1979
          aintrp=(a1-value)/binc                                                 KFPARA2.1980
          tlog=aintrp*alu(indlu+1)+(1-aintrp)*alu(indlu)                         KFPARA2.1981
          TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)                                      KFPARA2.1982
          TLCL=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(TMIX-T00))*(TMIX-         KFPARA2.1983
     +         TDPT)                                                             KFPARA2.1984
          TLCL=AMIN1(TLCL,TMIX)                                                  KFPARA2.1985
        ENDIF                                                                    KFPARA2.1986
        TVLCL=TLCL*(1.+0.608*QMIX)                                               KFPARA2.1987
        ZLCL = ZMIX+(TLCL-TMIX)/GDRY                                             KFPARA2.1988
        DO 729 NK = LC,KL                                                        KFPARA2.1989
          KLCL=NK                                                                KFPARA2.1990
          IF(ZLCL.LE.Z00(NK)) GOTO 735                                           KFPARA2.1991
 729    CONTINUE                                                                 KFPARA2.1992
        print *,'PROBLEM...LCL cannot be found after conv adjustment!!'          KFPARA2.1993
        STOP 'KF BAD ADJUST'                                                     KFPARA2.1994
c       GOTO 325                                                                 KFPARA2.1995
 735    K=KLCL-1                                                                 KFPARA2.1996
        DLP=(ZLCL-Z00(K))/(Z00(KLCL)-Z00(K))                                     KFPARA2.1997
C                                                                                KFPARA2.1998
C...ESTIMATE ENVIRONMENTAL TEMPERATURE AND MIXING RATIO AT THE LCL...            KFPARA2.1999
C                                                                                KFPARA2.2000
        TENV=TG(K)+(TG(KLCL)-TG(K))*DLP                                          KFPARA2.2001
        QENV=QG(K)+(QG(KLCL)-QG(K))*DLP                                          KFPARA2.2002
        TVEN=TENV*(1.+0.608*QENV)                                                KFPARA2.2003
        PLCL=P0(K)+(P0(KLCL)-P0(K))*DLP                                          KFPARA2.2004
        THETEU(K)=TMIX*(1.E5/PMIX)**(0.2854*(1.-0.28*QMIX))*                     KFPARA2.2005
     +            EXP((3374.6525/TLCL-2.5403)*QMIX*(1.+0.81*QMIX))               KFPARA2.2006
        ES=ALIQ*EXP((TENV*BLIQ-CLIQ)/(TENV-DLIQ))                                KFPARA2.2007
        QESE=0.622*ES/(PLCL-ES)                                                  KFPARA2.2008
        THTESG(K)=TENV*(1.E5/PLCL)**(0.2854*(1.-0.28*QESE))*                     KFPARA2.2009
     +            EXP((3374.6525/TENV-2.5403)*QESE*(1.+0.81*QESE))               KFPARA2.2010
C                                                                                KFPARA2.2011
C...COMPUTE ADJUSTED ABE(ABEG).                                                  KFPARA2.2012
C                                                                                KFPARA2.2013
        ABEG=0.                                                                  KFPARA2.2014
        THTUDL=THETEU(K)                                                         KFPARA2.2015
        theu_adj =THTUDL                                                         KFPARA2.2016
        DO 245 NK=K,LTOPM1                                                       KFPARA2.2017
          NK1=NK+1                                                               KFPARA2.2018
          THETEU(NK1) = THETEU(NK)                                               KFPARA2.2019
          call tp_cape(p0(nk1),theteu(nk1),tgu(nk1),qgu(nk1))                    KFPARA2.2020
          TVQU(NK1)=TGU(NK1)*(1.+0.608*QGU(NK1)-QLIQ(NK1)-QICE(NK1))             KFPARA2.2021
          IF(NK.EQ.K)THEN                                                        KFPARA2.2022
            DZZ=Z00(KLCL)-ZLCL                                                   KFPARA2.2023
            DILBE=((TVLCL+TVQU(NK1))/(TVEN+TVG(NK1))-1.)*DZZ                     KFPARA2.2024
          ELSE                                                                   KFPARA2.2025
            DZZ=DZA(NK)                                                          KFPARA2.2026
            DILBE=((TVQU(NK)+TVQU(NK1))/(TVG(NK)+TVG(NK1))-1.)*DZZ               KFPARA2.2027
          ENDIF                                                                  KFPARA2.2028
          IF(DILBE.GT.0.)ABEG=ABEG+DILBE*G                                       KFPARA2.2029
C                                                                                KFPARA2.2030
C...DILUTE BY ENTRAINMENT BY THE RATE AS ORIGINAL UPDRAFT...                     KFPARA2.2031
C                                                                                KFPARA2.2032
          CALL ENVIRTHT(P0(NK1),TG(NK1),QG(NK1),THTEEG(NK1),0.,                  KFPARA2.2033
     *                 RL,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)               KFPARA2.2034
          THETEU(NK1)=                                                           KFPARA2.2035
     *      THETEU(NK1)*DDILFRC(NK1)+THTEEG(NK1)*(1.-DDILFRC(NK1))               KFPARA2.2036
 245   CONTINUE                                                                  KFPARA2.2037
C                                                                                KFPARA2.2038
C...ASSUME AT LEAST 90% OF CAPE (ABE) IS REMOVED BY CONVECTION DURING            KFPARA2.2039
C...THE PERIOD TIMEC...                                                          KFPARA2.2040
C                                                                                KFPARA2.2041
        IF(NOITR.EQ.1)THEN                                                       KFPARA2.2042
c         write(98,*)' '                                                         KFPARA2.2043
c         write(98,*)'TAU, I, J, =',NTSD,I,J                                     KFPARA2.2044
c         WRITE(98,1060)FABE                                                     KFPARA2.2045
          GOTO 265                                                               KFPARA2.2046
        ENDIF                                                                    KFPARA2.2047
        DABE=AMAX1(ABE-ABEG,0.1*ABE)                                             KFPARA2.2048
        FABE=ABEG/(ABE+1.E-8)                                                    KFPARA2.2049
        IF(FABE.GT.1. .and. ISHALL.EQ.0)THEN                                     KFPARA2.2050
c        WRITE(98,*)'UPDRAFT/DOWNDRAFT COUPLET INCREASES CAPE AT THIS            KFPARA2.2051
c     *GRID POINT; NO CONVECTION ALLOWED!'                                       KFPARA2.2052
          GOTO 325                                                               KFPARA2.2053
        ENDIF                                                                    KFPARA2.2054
        IF(NCOUNT.NE.1)THEN                                                      KFPARA2.2055
          IF(ABS(AINC-AINCOLD).LT.0.0001)THEN                                    23DEC04.3194
            NOITR=1                                                              23DEC04.3195
            AINC=AINCOLD                                                         23DEC04.3196
            GOTO 255                                                             23DEC04.3197
          ENDIF                                                                  23DEC04.3198
          DFDA=(FABE-FABEOLD)/(AINC-AINCOLD)                                     KFPARA2.2056
          IF(DFDA.GT.0.)THEN                                                     KFPARA2.2057
            NOITR=1                                                              KFPARA2.2058
            AINC=AINCOLD                                                         KFPARA2.2059
            GOTO 255                                                             KFPARA2.2060
          ENDIF                                                                  KFPARA2.2061
        ENDIF                                                                    KFPARA2.2062
        AINCOLD=AINC                                                             KFPARA2.2063
        FABEOLD=FABE                                                             KFPARA2.2064
        IF(AINC/AINCMX.GT.0.999.AND.FABE.GT.1.05-STAB)THEN                       KFPARA2.2065
c         write(98,*)' '                                                         KFPARA2.2066
c         write(98,*)'TAU, I, J, =',NTSD,I,J                                     KFPARA2.2067
c         WRITE(98,1055)FABE                                                     KFPARA2.2068
          GOTO 265                                                               KFPARA2.2069
        ENDIF                                                                    KFPARA2.2070
        IF(FABE.LE.1.05-STAB.AND.FABE.GE.0.95-STAB)GOTO 265                      KFPARA2.2071
        IF(NCOUNT.GT.10)THEN                                                     KFPARA2.2072
c         write(98,*)' '                                                         KFPARA2.2073
c         write(98,*)'TAU, I, J, =',NTSD,I,J                                     KFPARA2.2074
c         WRITE(98,1060)FABE                                                     KFPARA2.2075
          GOTO 265                                                               KFPARA2.2076
        ENDIF                                                                    KFPARA2.2077
C                                                                                KFPARA2.2078
C...IF MORE THAN 10% OF THE ORIGINAL CAPE REMAINS, INCREASE THE CONVECTI         KFPARA2.2079
C...MASS FLUX BY THE FACTOR AINC:                                                KFPARA2.2080
C                                                                                KFPARA2.2081
        IF(FABE.EQ.0.)THEN                                                       KFPARA2.2082
          AINC=AINC*0.5                                                          KFPARA2.2083
        ELSE                                                                     KFPARA2.2084
          IF(DABE.LT.1.e-4)THEN                                                  23DEC04.3199
            NOITR=1                                                              23DEC04.3200
            AINC=AINCOLD                                                         23DEC04.3201
          ELSE                                                                   23DEC04.3202
            AINC=AINC*STAB*ABE/DABE                                              23DEC04.3203
          ENDIF                                                                  23DEC04.3204
        ENDIF                                                                    KFPARA2.2086
  255   AINC=AMIN1(AINCMX,AINC)                                                  KFPARA2.2087
C...IF AINC BECOMES VERY SMALL, EFFECTS OF CONVECTION ! JSK MODS                 KFPARA2.2088
C...WILL BE MINIMAL SO JUST IGNORE IT...              ! JSK MODS                 KFPARA2.2089
c      IF(AINC.LT.0.05)GOTO 325                        ! JSK MODS                KFPARA2.2090
      IF(AINC.LT.0.05)then                                                       KFPARA2.2091
        GOTO 325                        ! JSK MODS                               KFPARA2.2092
      ENDIF                                                                      KFPARA2.2093
C        AINC=AMAX1(AINC,0.05)                        ! JSK MODS                 KFPARA2.2094
        TDER=TDER2*AINC                                                          KFPARA2.2095
        PPTFLX=PPTFL2*AINC                                                       KFPARA2.2096
c     IF (XTIME.LT.10.)THEN                                                      KFPARA2.2097
c     WRITE(98,1080)LFS,LDB,LDT,TIMEC,TADVEC,NSTEP,NCOUNT,                       KFPARA2.2098
c    *              FABEOLD,AINCOLD                                              KFPARA2.2099
c     ENDIF                                                                      KFPARA2.2100
        DO 260 NK=1,LTOP                                                         KFPARA2.2101
          UMF(NK)=UMF2(NK)*AINC                                                  KFPARA2.2102
          DMF(NK)=DMF2(NK)*AINC                                                  KFPARA2.2103
          DETLQ(NK)=DETLQ2(NK)*AINC                                              KFPARA2.2104
          DETIC(NK)=DETIC2(NK)*AINC                                              KFPARA2.2105
          UDR(NK)=UDR2(NK)*AINC                                                  KFPARA2.2106
          UER(NK)=UER2(NK)*AINC                                                  KFPARA2.2107
          DER(NK)=DER2(NK)*AINC                                                  KFPARA2.2108
          DDR(NK)=DDR2(NK)*AINC                                                  KFPARA2.2109
  260   CONTINUE                                                                 KFPARA2.2110
C                                                                                KFPARA2.2111
C...GO BACK UP FOR ANOTHER ITERATION...                                          KFPARA2.2112
C                                                                                KFPARA2.2113
        GOTO 175                                                                 KFPARA2.2114
  265   CONTINUE                                                                 KFPARA2.2115
C                                                                                KFPARA2.2116
C...COMPUTE HYDROMETEOR TENDENCIES AS IS DONE FOR T, QV...                       KFPARA2.2117
C                                                                                KFPARA2.2118
C...FRC2 IS THE FRACTION OF TOTAL CONDENSATE      !  PPT FB MODS                 KFPARA2.2119
C...GENERATED THAT GOES INTO PRECIPITIATION       !  PPT FB MODS                 KFPARA2.2120
c       IF(I.EQ.95 .AND. J.EQ.82)THEN                                            KFPARA2.2121
c        write(98,*)'PPTFLX, CPR, AINC =',PPTFLX,CPR,AINC                        KFPARA2.2122
c       ENDIF                                                                    KFPARA2.2123
        IF(CPR.GT.0.)THEN                                                        KFPARA2.2124
          FRC2=PPTFLX/(CPR*AINC)                    !  PPT FB MODS               KFPARA2.2125
        ELSE                                                                     KFPARA2.2126
           FRC2=0.                                                               KFPARA2.2127
        ENDIF                                                                    KFPARA2.2128
        DO 270 NK=1,LTOP                                                         KFPARA2.2129
          QLPA(NK)=QL0(NK)                                                       KFPARA2.2130
          QIPA(NK)=QI0(NK)                                                       KFPARA2.2131
          QRPA(NK)=QR0(NK)                                                       KFPARA2.2132
          QSPA(NK)=QS0(NK)                                                       KFPARA2.2133
          RAINFB(NK)=PPTLIQ(NK)*AINC*FBFRC*FRC2   !  PPT FB MODS                 KFPARA2.2134
          SNOWFB(NK)=PPTICE(NK)*AINC*FBFRC*FRC2   !  PPT FB MODS                 KFPARA2.2135
  270   CONTINUE                                                                 KFPARA2.2136
        DO 290 NTC=1,NSTEP                                                       KFPARA2.2137
C                                                                                KFPARA2.2138
C...ASSIGN HYDROMETEORS CONCENTRATIONS AT THE TOP AND BOTTOM OF EACH LAY         KFPARA2.2139
C...BASED ON THE SIGN OF OMEGA...                                                KFPARA2.2140
C                                                                                KFPARA2.2141
          DO 275 NK=1,LTOP                                                       KFPARA2.2142
            QLFXIN(NK)=0.                                                        KFPARA2.2143
            QLFXOUT(NK)=0.                                                       KFPARA2.2144
            QIFXIN(NK)=0.                                                        KFPARA2.2145
            QIFXOUT(NK)=0.                                                       KFPARA2.2146
            QRFXIN(NK)=0.                                                        KFPARA2.2147
            QRFXOUT(NK)=0.                                                       KFPARA2.2148
            QSFXIN(NK)=0.                                                        KFPARA2.2149
            QSFXOUT(NK)=0.                                                       KFPARA2.2150
  275     CONTINUE                                                               KFPARA2.2151
          DO 280 NK=2,LTOP                                                       KFPARA2.2152
            IF(OMG(NK).LE.0.)THEN                                                KFPARA2.2153
              QLFXIN(NK)=-FXM(NK)*QLPA(NK-1)                                     KFPARA2.2154
              QIFXIN(NK)=-FXM(NK)*QIPA(NK-1)                                     KFPARA2.2155
              QRFXIN(NK)=-FXM(NK)*QRPA(NK-1)                                     KFPARA2.2156
              QSFXIN(NK)=-FXM(NK)*QSPA(NK-1)                                     KFPARA2.2157
              QLFXOUT(NK-1)=QLFXOUT(NK-1)+QLFXIN(NK)                             KFPARA2.2158
              QIFXOUT(NK-1)=QIFXOUT(NK-1)+QIFXIN(NK)                             KFPARA2.2159
              QRFXOUT(NK-1)=QRFXOUT(NK-1)+QRFXIN(NK)                             KFPARA2.2160
              QSFXOUT(NK-1)=QSFXOUT(NK-1)+QSFXIN(NK)                             KFPARA2.2161
            ELSE                                                                 KFPARA2.2162
              QLFXOUT(NK)=FXM(NK)*QLPA(NK)                                       KFPARA2.2163
              QIFXOUT(NK)=FXM(NK)*QIPA(NK)                                       KFPARA2.2164
              QRFXOUT(NK)=FXM(NK)*QRPA(NK)                                       KFPARA2.2165
              QSFXOUT(NK)=FXM(NK)*QSPA(NK)                                       KFPARA2.2166
              QLFXIN(NK-1)=QLFXIN(NK-1)+QLFXOUT(NK)                              KFPARA2.2167
              QIFXIN(NK-1)=QIFXIN(NK-1)+QIFXOUT(NK)                              KFPARA2.2168
              QRFXIN(NK-1)=QRFXIN(NK-1)+QRFXOUT(NK)                              KFPARA2.2169
              QSFXIN(NK-1)=QSFXIN(NK-1)+QSFXOUT(NK)                              KFPARA2.2170
            ENDIF                                                                KFPARA2.2171
  280     CONTINUE                                                               KFPARA2.2172
C                                                                                KFPARA2.2173
C...UPDATE THE HYDROMETEOR CONCENTRATION VALUES AT EACH LEVEL...                 KFPARA2.2174
C                                                                                KFPARA2.2175
          DO 285 NK=1,LTOP                                                       KFPARA2.2176
            QLPA(NK)=QLPA(NK)+(QLFXIN(NK)+DETLQ(NK)-QLFXOUT(NK))*DTIME*          KFPARA2.2177
     +               EMSD(NK)                                                    KFPARA2.2178
            QIPA(NK)=QIPA(NK)+(QIFXIN(NK)+DETIC(NK)-QIFXOUT(NK))*DTIME*          KFPARA2.2179
     +               EMSD(NK)                                                    KFPARA2.2180
c...reverse the mod that allows feedback of rain/snow that originates in         KFPARA2.2181
c...detrained air...12/9/98...jsk                                                KFPARA2.2182
c           QRPA(NK)=QRPA(NK)+(QRFXIN(NK)+QLQOUT(NK)*UDR(NK)-QRFXOUT(NK)         KFPARA2.2183
            QRPA(NK)=QRPA(NK)+(QRFXIN(NK)-QRFXOUT(NK)                            KFPARA2.2184
     +               +RAINFB(NK))*DTIME*EMSD(NK)         !  PPT FB MODS          KFPARA2.2185
C     +               )*DTIME*EMSD(NK)                   !  PPT FB MODS          KFPARA2.2186
c...reverse the mod that allows feedback of rain/snow that originates in         KFPARA2.2187
c...detrained air...12/9/98...jsk                                                KFPARA2.2188
c           QSPA(NK)=QSPA(NK)+(QSFXIN(NK)+QICOUT(NK)*UDR(NK)-QSFXOUT(NK)         KFPARA2.2189
            QSPA(NK)=QSPA(NK)+(QSFXIN(NK)-QSFXOUT(NK)                            KFPARA2.2190
     +               +SNOWFB(NK))*DTIME*EMSD(NK)         !  PPT FB MODS          KFPARA2.2191
C     +               )*DTIME*EMSD(NK)                   !  PPT FB MODS          KFPARA2.2192
c                                                                                KFPARA2.2193
  285     CONTINUE                                                               KFPARA2.2194
  290   CONTINUE                                                                 KFPARA2.2195
        DO 295 NK=1,LTOP                                                         KFPARA2.2196
          QLG(NK)=QLPA(NK)                                                       KFPARA2.2197
          QIG(NK)=QIPA(NK)                                                       KFPARA2.2198
          QRG(NK)=QRPA(NK)                                                       KFPARA2.2199
          QSG(NK)=QSPA(NK)                                                       KFPARA2.2200
  295   CONTINUE                                                                 KFPARA2.2201
C                                                                                KFPARA2.2202
C...CLEAN THINGS UP, CALCULATE CONVECTIVE FEEDBACK TENDENCIES FOR THIS           KFPARA2.2203
C...GRID POINT...                                                                KFPARA2.2204
C                                                                                KFPARA2.2205
c     IF (XTIME.LT.10.)THEN                                                      KFPARA2.2206
c     WRITE(98,1080)LFS,LDB,LDT,TIMEC,TADVEC,NSTEP,NCOUNT,FABE,AINC              KFPARA2.2207
c     ENDIF                                                                      KFPARA2.2208
       IF(IPRNT)THEN                                                             KFPARA2.2209
      WRITE(98,1080)LFS,LDB,LDT,TIMEC,TADVEC,NSTEP,NCOUNT,FABE,AINC              KFPARA2.2210
c       call flush(98)                                                           KFPARA2.2211
       endif                                                                     KFPARA2.2212
C                                                                                KFPARA2.2213
C...SEND FINAL PARAMETERIZED VALUES TO OUTPUT FILES...                           KFPARA2.2214
C                                                                                KFPARA2.2215
c297   IF(XTIME.LT.10..OR.ISTOP.EQ.1)THEN                                        KFPARA2.2216
c 297   IF(ISTOP.EQ.1)THEN                                                       KFPARA2.2217
c297   IF((I.GE.78 .AND. I.LE.82 .AND. J.GE.102 .AND. J.LE.106).or.              KFPARA2.2218
 297   IF(IPRNT)then                                                             KFPARA2.2219
c      IF(ISTOP.EQ.1)THEN                                                        KFPARA2.2220
c        write(98,*)                                                             KFPARA2.2221
c        write(98,*)'At t(h), I, J =',float(NTSD)*72./3600.,I,J                  KFPARA2.2222
c        write(98,*)'P(LC), DTP, WKL, WKLCL =',p0(LC)/100.,                      KFPARA2.2223
c    *               TLCL+DTLCL+dtrh-TENV,WKL,WKLCL                              KFPARA2.2224
c        write(98,*)'TLCL, DTLCL, DTRH, TENV =',TLCL,DTLCL,                      KFPARA2.2225
c    *                DTRH,TENV                                                  KFPARA2.2226
c        WRITE(98,1025)KLCL,ZLCL,DTLCL,LTOP,P0(LTOP),IFLAG,                      KFPARA2.2227
c    *   TMIX-T00,PMIX,QMIX,ABE                                                  KFPARA2.2228
c        WRITE(98,1030)P0(LET)/100.,P0(LTOP)/100.,VMFLCL,PLCL/100.,              KFPARA2.2229
c    *   WLCL,CLDHGT(LC)                                                         KFPARA2.2230
c        WRITE(98,1035)PEF,PEFCBH,LC,LET,WKL,VWS                                 KFPARA2.2231
c        write(98,*)'PRECIP EFFICIENCY =',PEFF                                   KFPARA2.2232
c     WRITE(98,1080)LFS,LDB,LDT,TIMEC,TADVEC,NSTEP,NCOUNT,FABE,AINC              KFPARA2.2233
c      ENDIF                                                                     KFPARA2.2234
c!!!! HERE !!!!!!!                                                               KFPARA2.2235
       WRITE(98,1070)'  P  ','   DP ',' DT K/D ',' DR K/D ','   OMG  ',          KFPARA2.2236
     *' DOMGDP ','   UMF  ','   UER  ','   UDR  ','   DMF  ','   DER  '          KFPARA2.2237
     *,'   DDR  ','   EMS  ','    W0  ','  DETLQ ',' DETIC '                     KFPARA2.2238
       write(98,*)'just before DO 300...'                                        KFPARA2.2239
c      call flush(98)                                                            KFPARA2.2240
           DO 300 NK=1,LTOP                                                      KFPARA2.2241
             K=LTOP-NK+1                                                         KFPARA2.2242
             DTT=(TG(K)-T0(K))*86400./TIMEC                                      KFPARA2.2243
             RL=XLV0-XLV1*TG(K)                                                  KFPARA2.2244
             DR=-(QG(K)-Q0(K))*RL*86400./(TIMEC*CP)                              KFPARA2.2245
             UDFRC=UDR(K)*TIMEC*EMSD(K)                                          KFPARA2.2246
             UEFRC=UER(K)*TIMEC*EMSD(K)                                          KFPARA2.2247
             DDFRC=DDR(K)*TIMEC*EMSD(K)                                          KFPARA2.2248
             DEFRC=-DER(K)*TIMEC*EMSD(K)                                         KFPARA2.2249
       WRITE(98,1075)P0(K)/100.,DP(K)/100.,DTT,DR,OMG(K),DOMGDP(K)*1.E4,         KFPARA2.2250
     *  UMF(K)/1.E6,UEFRC,UDFRC,DMF(K)/1.E6,DEFRC,DDFRC,EMS(K)/1.E11,            KFPARA2.2251
     *  W0(K)*1.E2,DETLQ(K)*TIMEC*EMSD(K)*1.E3,DETIC(K)*                         KFPARA2.2252
     *  TIMEC*EMSD(K)*1.E3                                                       KFPARA2.2253
  300     CONTINUE                                                               KFPARA2.2254
       write(98,*)'just after DO 300...'                                         KFPARA2.2255
c      call flush(98)                                                            KFPARA2.2256
      WRITE(98,1085)'K','P','Z','T0','TG','DT','TU','TD','Q0','QG',              KFPARA2.2257
     *            'DQ','QU','QD','QLG','QIG','QRG','QSG','RH0','RHG'             KFPARA2.2258
           DO 305 NK=1,KL                                                        KFPARA2.2259
             K=KX-NK+1-NLEV                                                      KFPARA2.2260
             DTT=TG(K)-T0(K)                                                     KFPARA2.2261
             TUC=TU(K)-T00                                                       KFPARA2.2262
             IF(K.LT.LC.OR.K.GT.LTOP)TUC=0.                                      KFPARA2.2263
             IF(K.GE.LC .AND. K.LE.KPBL)then                                     KFPARA2.2264
               QU(K)=QUER(K)                                                     KFPARA2.2265
             ENDIF                                                               KFPARA2.2266
             TDC=TZ(K)-T00                                                       KFPARA2.2267
             IF((K.LT.LDB.OR.K.GT.LDT).AND.K.NE.LFS)TDC=0.                       KFPARA2.2268
             IF(T0(K).LT.T00)THEN                                                KFPARA2.2269
               ES=ALIQ*EXP((BLIQ*TG(K)-CLIQ)/(TG(K)-DLIQ))                       KFPARA2.2270
             ELSE                                                                KFPARA2.2271
               ES=ALIQ*EXP((BLIQ*TG(K)-CLIQ)/(TG(K)-DLIQ))                       KFPARA2.2272
             ENDIF                                                               KFPARA2.2273
             QGS=ES*0.622/(P0(K)-ES)                                             KFPARA2.2274
             RH0=Q0(K)/QES(K)                                                    KFPARA2.2275
             RHG=QG(K)/QGS                                                       KFPARA2.2276
      WRITE(98,1090)K,P0(K)/100.,Z00(K),T0(K)-T00,TG(K)-T00,DTT,TUC,             KFPARA2.2277
     *TDC,Q0(K)*1000.,QG(K)*1000.,(QG(K)-Q0(K))*1000.,QU(K)*                     KFPARA2.2278
     *1000.,QD(K)*1000.,QLG(K)*1000.,QIG(K)*1000.,QRG(K)*1000.,                  KFPARA2.2279
     *QSG(K)*1000.,RH0,RHG                                                       KFPARA2.2280
  305     CONTINUE                                                               KFPARA2.2281
C                                                                                KFPARA2.2282
C...IF CALCULATIONS ABOVE SHOW AN ERROR IN THE MASS BUDGET, PRINT OUT A          KFPARA2.2283
C...TO BE USED LATER FOR DIAGNOSTIC PURPOSES, THEN ABORT RUN...                  KFPARA2.2284
C                                                                                KFPARA2.2285
c         IF(ISTOP.EQ.1 .or. ISHALL.EQ.1)THEN                                    KFPARA2.2286
c         IF(ISHALL.NE.1)THEN                                                    KFPARA2.2287
c            write(98,4421)i,j,iyr,imo,idy,ihr,imn                               KFPARA2.2288
c           write(98)i,j,iyr,imo,idy,ihr,imn,kl                                  KFPARA2.2289
c 4421       format(7i4)                                                         KFPARA2.2290
c            write(98,4422)kl                                                    KFPARA2.2291
c 4422       format(i6)                                                          KFPARA2.2292
c           DO 310 NK = 1,KL                                                     KFPARA2.2293
c             k = kl - nk + 1                                                    KFPARA2.2294
c             write(98,4455) p0(k)/100.,t0(k)-273.16,q0(k)*1000.,                KFPARA2.2295
c    *                 u0(k),v0(k),w0avg(i,j,nk),dp(k)                           KFPARA2.2296
c             write(98) p0,t0,q0,u0,v0,w0,dp,tke                                 KFPARA2.2297
c           WRITE(98,1115)Z00(K),P0(K)/100.,T0(K)-273.16,Q0(K)*1000.,            KFPARA2.2298
c    *               U0(K),V0(K),DP(K)/100.,W0AVG(I,J,K)                         KFPARA2.2299
c310        CONTINUE                                                             KFPARA2.2300
            IF(ISTOP.EQ.1)THEN                                                   KFPARA2.2301
              STOP 'KAIN-FRITSCH'                                                KFPARA2.2302
            ENDIF                                                                KFPARA2.2303
c         ENDIF                                                                  KFPARA2.2304
c 4455  format(7f11.3)                                                           KFPARA2.2305
       ENDIF                                                                     KFPARA2.2306
        CNDTNF=(1.-EQFRC(LFS))*(QLIQ(LFS)+QICE(LFS))*DMF(LFS)                    KFPARA2.2307
C     WRITE(98,1095)CPR*AINC,TDER+PPTFLX+CNDTNF                                  KFPARA2.2308
C                                                                                KFPARA2.2309
C  EVALUATE MOISTURE BUDGET...                                                   KFPARA2.2310
C                                                                                KFPARA2.2311
        QINIT=0.                                                                 KFPARA2.2312
        QFNL=0.                                                                  KFPARA2.2313
        DPT=0.                                                                   KFPARA2.2314
        DO 315 NK=1,LTOP                                                         KFPARA2.2315
          DPT=DPT+DP(NK)                                                         KFPARA2.2316
          QINIT=QINIT+Q0(NK)*EMS(NK)                                             KFPARA2.2317
          QFNL=QFNL+QG(NK)*EMS(NK)                                               KFPARA2.2318
          QFNL=QFNL+(QLG(NK)+QIG(NK)+QRG(NK)+QSG(NK))*EMS(NK)                    KFPARA2.2319
  315   CONTINUE                                                                 KFPARA2.2320
        QFNL=QFNL+PPTFLX*TIMEC*(1.-FBFRC)       !  PPT FB MODS                   KFPARA2.2321
C        QFNL=QFNL+PPTFLX*TIMEC                 !  PPT FB MODS                   KFPARA2.2322
        ERR2=(QFNL-QINIT)*100./QINIT                                             KFPARA2.2323
c      WRITE(98,1110)QINIT,QFNL,ERR2                                             KFPARA2.2324
      IF(ABS(ERR2).GT.0.05 .AND. ISTOP.EQ.0)THEN                                 KFPARA2.2325
        write(99,*)'!!!!!!!! MOISTURE BUDGET ERROR IN KFPARA !!!'                KFPARA2.2326
        WRITE(99,1110)QINIT,QFNL,ERR2                                            KFPARA2.2327
        IPRNT=.TRUE.                                                             KFPARA2.2328
        ISTOP=1                                                                  KFPARA2.2329
        GOTO 297                                                                 KFPARA2.2330
         STOP 'QVERR'                                                            KFPARA2.2331
      ENDIF                                                                      KFPARA2.2332
        IF(PPTFLX.GT.0.)THEN                                                     23DEC04.3205
          RELERR=ERR2*QINIT/(PPTFLX*TIMEC)                                       23DEC04.3206
        ELSE                                                                     23DEC04.3207
          RELERR=0.                                                              23DEC04.3208
        ENDIF                                                                    23DEC04.3209
c     WRITE(98,1120)RELERR                                                       KFPARA2.2334
c     WRITE(98,*)'TDER, CPR, USR, TRPPT =',                                      KFPARA2.2335
c    *TDER,CPR*AINC,USR*AINC,TRPPT*AINC                                          KFPARA2.2336
C                                                                                KFPARA2.2337
C...FEEDBACK TO RESOLVABLE SCALE TENDENCIES.                                     KFPARA2.2338
C                                                                                KFPARA2.2339
C...IF THE ADVECTIVE TIME PERIOD (TADVEC) IS LESS THAN SPECIFIED MINIMUM         KFPARA2.2340
C...TIMEC, ALLOW FEEDBACK TO OCCUR ONLY DURING TADVEC...                         KFPARA2.2341
C                                                                                KFPARA2.2342
        IF(TADVEC.LT.TIMEC)NIC=NINT(TADVEC/(0.5*DT2))                            KFPARA2.2343
        NCA(I,J)=NIC                                                             KFPARA2.2344
       IF(ISHALL.EQ.1)THEN                                                       KFPARA2.2345
         TIMEC = 2400.                                                           KFPARA2.2346
          NCA(I,J) = NCLDCK                                                      KFPARA2.2347
c        WRITE(98,*)'!!!!! SHALLOW CONVECTION !!!!'                              KFPARA2.2348
c        write(98,*)'At t(h), I, J =',float(NTSD)*72./3600.,I,J                  KFPARA2.2349
c        write(98,*)'TIMEC, NCA =',TIMEC,NCA(I,J)                                KFPARA2.2350
         NSHALL = NSHALL+1                                                       KFPARA2.2351
       ENDIF                                                                     KFPARA2.2352
        nkfp = nkfp+1                                                            KFPARA2.2353
        DO 320 K=1,KX                                                            KFPARA2.2354
          NK=KX-K+1                                                              KFPARA2.2355
          IF(IMOIST(INEST).NE.2)THEN                                             KFPARA2.2356
C                                                                                KFPARA2.2357
C...IF HYDROMETEORS ARE NOT ALLOWED, THEY MUST BE EVAPORATED OR SUBLIMAT         KFPARA2.2358
C...AND FED BACK AS VAPOR, ALONG WITH ASSOCIATED CHANGES IN TEMPERATURE.         KFPARA2.2359
C...NOTE:  THIS WILL INTRODUCE CHANGES IN THE CONVECTIVE TEMPERATURE AND         KFPARA2.2360
C...WATER VAPOR FEEDBACK TENDENCIES AND MAY LEAD TO SUPERSATURATED VALUE         KFPARA2.2361
C...OF QG...                                                                     KFPARA2.2362
C                                                                                KFPARA2.2363
            RLC=XLV0-XLV1*TG(K)                                                  KFPARA2.2364
            RLS=XLS0-XLS1*TG(K)                                                  KFPARA2.2365
            CPM=CP*(1.+0.887*QG(K))                                              KFPARA2.2366
            TG(K)=TG(K)-(RLC*(QLG(K)+QRG(K))+RLS*(QIG(K)+QSG(K)))/CPM            KFPARA2.2367
            QG(K)=QG(K)+(QLG(K)+QRG(K)+QIG(K)+QSG(K))                            KFPARA2.2368
            DQLDT(I,J,NK)=0.                                                     KFPARA2.2369
            DQIDT(I,J,NK)=0.                                                     KFPARA2.2370
            DQRDT(I,J,NK)=0.                                                     KFPARA2.2371
            DQSDT(I,J,NK)=0.                                                     KFPARA2.2372
          ELSE                                                                   KFPARA2.2373
C        IF(IEXICE.NE.1 .AND. IICE.NE.1) THEN                                    KFPARA2.2374
            IF(IMPHYS(INEST).EQ.3)THEN                                           KFPARA2.2375
C                                                                                KFPARA2.2376
C...IF ICE PHASE IS NOT ALLOWED, MELT ALL FROZEN HYDROMETEORS...                 KFPARA2.2377
C                                                                                KFPARA2.2378
              CPM=CP*(1.+0.887*QG(K))                                            KFPARA2.2379
              TG(K)=TG(K)-(QIG(K)+QSG(K))*RLF/CPM                                KFPARA2.2380
              DQLDT(I,J,NK)=(QLG(K)+QIG(K)-QL0(K)-QI0(K))/TIMEC                  KFPARA2.2381
              DQIDT(I,J,NK)=0.                                                   KFPARA2.2382
              DQRDT(I,J,NK)=(QRG(K)+QSG(K)-QR0(K)-QS0(K))/TIMEC                  KFPARA2.2383
              DQSDT(I,J,NK)=0.                                                   KFPARA2.2384
C        ELSEIF(IEXICE.EQ.1 .AND. IICE.EQ.0)THEN                                 KFPARA2.2385
            ELSEIF(IMPHYS(INEST).EQ.4)THEN                                       KFPARA2.2386
C                                                                                KFPARA2.2387
C...IF ICE PHASE IS ALLOWED, BUT MIXED PHASE IS NOT, MELT FROZEN HYDROME         KFPARA2.2388
C...BELOW THE MELTING LEVEL, FREEZE LIQUID WATER ABOVE THE MELTING LEVEL         KFPARA2.2389
C                                                                                KFPARA2.2390
              CPM=CP*(1.+0.887*QG(K))                                            KFPARA2.2391
              IF(K.LE.ML)THEN                                                    KFPARA2.2392
                TG(K)=TG(K)-(QIG(K)+QSG(K))*RLF/CPM                              KFPARA2.2393
              ELSEIF(K.GT.ML)THEN                                                KFPARA2.2394
                TG(K)=TG(K)+(QLG(K)+QRG(K))*RLF/CPM                              KFPARA2.2395
              ENDIF                                                              KFPARA2.2396
              DQLDT(I,J,NK)=(QLG(K)+QIG(K)-QL0(K)-QI0(K))/TIMEC                  KFPARA2.2397
              DQIDT(I,J,NK)=0.                                                   KFPARA2.2398
              DQRDT(I,J,NK)=(QRG(K)+QSG(K)-QR0(K)-QS0(K))/TIMEC                  KFPARA2.2399
              DQSDT(I,J,NK)=0.                                                   KFPARA2.2400
C        ELSEIF(IICE.EQ.1 .AND. IEXICE.EQ.0)THEN                                 KFPARA2.2401
            ELSEIF(IMPHYS(INEST).GE.5)THEN                                       KFPARA2.2402
C                                                                                KFPARA2.2403
C...IF MIXED PHASE HYDROMETEORS ARE ALLOWED, FEED BACK CONVECTIVE TENDEN         KFPARA2.2404
C...OF HYDROMETEORS DIRECTLY...                                                  KFPARA2.2405
C                                                                                KFPARA2.2406
              DQLDT(I,J,NK)=(QLG(K)-QL0(K))/TIMEC                                KFPARA2.2407
              DQIDT(I,J,NK)=(QIG(K)-QI0(K))/TIMEC                                KFPARA2.2408
              DQRDT(I,J,NK)=(QRG(K)-QR0(K))/TIMEC                                KFPARA2.2409
              DQSDT(I,J,NK)=(QSG(K)-QS0(K))/TIMEC                                KFPARA2.2410
            ELSE                                                                 KFPARA2.2411
         PRINT *,'THIS COMBINATION OF IMOIST, IEXICE, IICE NOT ALLOWED!'         KFPARA2.2412
              STOP 'KAIN-FRITSCH'                                                KFPARA2.2413
            ENDIF                                                                KFPARA2.2414
          ENDIF                                                                  KFPARA2.2415
          DTDT(I,J,NK)=(TG(K)-T0(K))/TIMEC                                       KFPARA2.2416
          DQDT(I,J,NK)=(QG(K)-Q0(K))/TIMEC                                       KFPARA2.2417
c         dtdth = abs(dtdt(i,j,nk)*3600.)                                        KFPARA2.2418
c         dqdth = abs(dqdt(i,j,nk)*3600.)                                        KFPARA2.2419
c         if(dtdth .gt. 10. .or. dqdth                                           KFPARA2.2420
c    *   .gt. 0.01)then                                                          KFPARA2.2421
c          write(6,*)'I, J, PRS, dtdt, dqdt =',i,j,P0(K)/100.,DTDTH,             KFPARA2.2422
c    *     DQDTH                                                                 KFPARA2.2423
c          CALL FLUSH(6)                                                         KFPARA2.2424
c         endif                                                                  KFPARA2.2425
  320   CONTINUE                                                                 KFPARA2.2426
        IF(ISHALL.EQ.0)THEN                                                      KFPARA2.2427
          NCAD(I,J) = NCA(I,J)                                                   KFPARA2.2428
          PSRC(I,J) = PMIX0/100.                                                 KFPARA2.2429
          PCLB(I,J) = PLCL0/100.                                                 KFPARA2.2430
          emst = dpthmx*dxsq/g                                                   KFPARA2.2431
          UMFB(I,J) = 100.*vmflcl*timec*ainc/emst                                KFPARA2.2432
          CIN(I,J) = CIN1                                                        KFPARA2.2433
        ENDIF                                                                    KFPARA2.2434
        RAINCV(I,J)=.1*.5*DT2*PPTFLX*(1.-FBFRC)/DXSQ     !  PPT FB MODS          KFPARA2.2435
C        RAINCV(I,J)=.1*.5*DT2*PPTFLX/DXSQ               !  PPT FB MODS          KFPARA2.2436
C         RNC=0.1*TIMEC*PPTFLX/DXSQ                                              KFPARA2.2437
        RNC=RAINCV(I,J)*NIC                                                      KFPARA2.2438
#ifdef MPP1                                                                      KFPARA2.2439
        enddo                                                                    KFPARA2.2440
#endif                                                                           KFPARA2.2441
 909     FORMAT(' CONVECTIVE RAINFALL =',F8.4,' CM')                             KFPARA2.2442
      NCCNT=NCCNT+1                                                              KFPARA2.2443
  325 CONTINUE                                                                   KFPARA2.2444
c      WRITE(98,*)'at NTSD =',NTSD,',No. of KF points activated =',              KFPARA2.2445
c     *            NCCNT                                                         KFPARA2.2446
c      call flush(98)                                                            KFPARA2.2447
1000  FORMAT(' ',10A8)                                                           KFPARA2.2448
1005  FORMAT(' ',F6.0,2X,F6.4,2X,F7.3,1X,F6.4,2X,4(F6.3,2X),2(F7.3,1X))          KFPARA2.2449
1010  FORMAT(' ',' VERTICAL VELOCITY IS NEGATIVE AT ',F4.0,' MB')                KFPARA2.2450
1015   FORMAT(' ','ALL REMAINING MASS DETRAINS BELOW ',F4.0,' MB')               KFPARA2.2451
1025   FORMAT(5X,' KLCL=',I2,' ZLCL=',F7.1,'M',                                  KFPARA2.2452
     *  ' DTLCL=',F5.2,' LTOP=',I2,' P0(LTOP)=',F5.1,'MB FRZ LV=',               KFPARA2.2453
     *  I2,' TMIX=',0PF4.1,1X,'PMIX=',F6.1,' QMIX=',3PF5.1,                      KFPARA2.2454
     *  ' CAPE=',0PF7.1)                                                         KFPARA2.2455
1030   FORMAT(' ',' P0(LET) = ',F6.1,' P0(LTOP) = ',F6.1,' VMFLCL =',            KFPARA2.2456
     *E12.3,' PLCL =',F6.1,' WLCL =',F6.3,' CLDHGT =',                           KFPARA2.2457
     *F8.1)                                                                      KFPARA2.2458
1035  FORMAT(1X,'PEF(WS)=',F4.2,'(CB)=',F4.2,'LC,LET=',2I3,'WKL='                KFPARA2.2459
     *,F6.3,'VWS=',F5.2)                                                         KFPARA2.2460
1040          FORMAT(' ','PRECIP EFF = 100%, ENVIR CANNOT SUPPORT',              KFPARA2.2461
     *' DOWNDRAFTS!')                                                            KFPARA2.2462
1045  FORMAT('NUMBER OF DOWNDRAFT ITERATIONS EXCEEDS 10...PPTFLX ',              KFPARA2.2463
     * ' IS DIFFERENT FROM THAT GIVEN BY PRECIP EFF RELATION!')                  KFPARA2.2464
1050  FORMAT(' ','LCOUNT= ',I3,' PPTFLX/CPR, PEFF= ',F5.3,1X,F5.3,               KFPARA2.2465
     *'DMF(LFS)/UMF(LCL)= ',F5.3)                                                KFPARA2.2466
1055     FORMAT(/'*** DEGREE OF STABILIZATION =',F5.3,                           KFPARA2.2467
     *', NO MORE MASS FLUX IS ALLOWED!')                                         KFPARA2.2468
1060  FORMAT(/' ITERATION DOES NOT CONVERGE TO GIVE THE SPECIFIED',              KFPARA2.2469
     * ' DEGREE OF STABILIZATION!  FABE= ',F6.4)                                 KFPARA2.2470
 1070 FORMAT (16A8)                                                              KFPARA2.2471
 1075 FORMAT (F8.2,3(F8.2),2(F8.3),F8.2,2F8.3,F8.2,6F8.3)                        KFPARA2.2472
1080   FORMAT(2X,'LFS,LDB,LDT =',3I3,' TIMEC, TADVEC, NSTEP=',                   KFPARA2.2473
     *        2(1X,F5.0),I3,'NCOUNT, FABE, AINC=',I2,1X,F5.3,F6.2)               KFPARA2.2474
 1085 FORMAT (A3,16A7,2A8)                                                       KFPARA2.2475
 1090 FORMAT (I3,F7.2,F7.0,10F7.2,4F7.3,2F8.3)                                   KFPARA2.2476
1095   FORMAT(' ','  PPT PRODUCTION RATE= ',F10.0,' TOTAL EVAP+PPT= ',           KFPARA2.2477
     * F10.0)                                                                    KFPARA2.2478
1105   FORMAT(' ','NET LATENT HEAT RELEASE =',E12.5,' ACTUAL HEATING =',         KFPARA2.2479
     * E12.5,' J/KG-S, DIFFERENCE = ',F9.3,'percent')                            KFPARA2.2480
1110   FORMAT(' ','INITIAL WATER =',E12.5,' FINAL WATER =',E12.5,                KFPARA2.2481
     * ' TOTAL WATER CHANGE =',F8.2,'percent')                                   KFPARA2.2482
 1115 FORMAT (2X,F6.0,2X,F7.2,2X,F5.1,2X,F6.3,2(2X,F5.1),2X,F7.2,2X,F7.4         KFPARA2.2483
     +       )                                                                   KFPARA2.2484
1120   FORMAT(' ','MOISTURE ERROR AS FUNCTION OF TOTAL PPT =',F9.3,              KFPARA2.2485
     +        'percent')                                                         KFPARA2.2486
      RETURN                                                                     KFPARA2.2487
      END                                                                        KFPARA2.2488
                                                                                 23DEC04.3210
