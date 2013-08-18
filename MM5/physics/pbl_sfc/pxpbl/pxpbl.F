      SUBROUTINE PXPBL(IYY,JXX,J,IN,U3D,V3D,T3D,QV3D,PP3D,                       PXPBL.1
     1                 QC3D,QI3D,QNC3D,U3DTEN,V3DTEN,T3DTEN,                     PXPBL.2
     2                 QC3DTEN,QV3DTEN,QI3DTEN,QNC3DTEN)                         PXPBL.3
      IMPLICIT NONE                                                              PXPBL.4
C                                                                                PXPBL.5
C***********************************************************************         PXPBL.6
C                                                                                PXPBL.7
C  FUNCTION:                                                                     PXPBL.8
C    THIS MODULE CONTAINS THE PLEIM-XIU LAND-SURFACE AND PBL MODEL (PX-LSM).     PXPBL.9
C    IT IS DESIGNED TO SIMULATE CHARACTERISTICS OF THE LAND SURFACE AND          PXPBL.10
C    VEGETATION AND EXCHANGE WITH THE PLANETARY BOUNDARY LAYER (PBL). THE        PXPBL.11
C    SOIL MOISTURE MODEL IS BASED ON THE ISBA SCHEME DEVELOPED BY NOILHAN        PXPBL.12
C    AND PLANTON (1989) AND JACQUEMIN AND NOILHAN (1990) AND INCLUDES            PXPBL.13
C    PROGNOSTIC EQUATIONS FOR SOIL MOISTURE AND SOIL TEMPERATURE IN TWO          PXPBL.14
C    LAYERS (1 CM AND 1 M) AS WELL AS CANOPY WATER CONTENT.  SURFACE             PXPBL.15
C    MOISTURE FLUXES ARE MODELED BY 3 PATHWAYS: SOIL EVAPORATION, CANOPY         PXPBL.16
C    EVAPORATION , AND VEGETATIVE EVAPOTRANSPIRRATION.                           PXPBL.17
C    EVAPOTRANSPIRATION DIRECTLY FROM THE ROOT ZONE SOIL LAYER IS MODELED        PXPBL.18
C    VIA A CANOPY RESISTANCE ANALOG ALGORITHM WHERE STOMATAL CONDUCTANCE         PXPBL.19
C    IS CONTROLLED BY SOLAR RADIATION, AIR TEMPERATURE, AIR HUMIDITY, AND        PXPBL.20
C    ROOT ZONE SOIL MOISTURE.  REQUIRED VEGETATION CHARACTERISTICS DERIVED       PXPBL.21
C    FROM THE USGS LANDUSE DATA INCLUDE: LEAF AREA INDEX, FRACTIONAL VEGETATION  PXPBL.22
C    COVERAGE, ROUGHNESS LENGTH, AND MINIMUM STOMATAL RESISTANCE.  AN INDIRECT   PXPBL.23
C    NUDGING SCHEME ADJUSTS SOIL MOISTURE ACCORDING TO DIFFERENCES BETWEEN       PXPBL.24
C    MODELED TEMPERATURE AND HUMIDITY AND ANALYSED SURFACE FIELDS.               PXPBL.25
C    THE PBL SCHEME IS A HYBRID OF NON-LOCAL CLOSURE, FOR CONVECTIVE             PXPBL.26
C    CONDITIONS, AND EDDY DIFFUSION. THE NON-LOCAL CLOSURE MODEL, CALL THE       PXPBL.27
C    ASYMMETRICAL CONVECTIVE MODEL (ACM), IS DESIGNED TO SIMULATE RAPID          PXPBL.28
C    UPWARD TRANSPORT FROM THE SURFACE LAYER TO ALL LEVELS WITHIN THE            PXPBL.29
C    CONVECTIVE BOUNDARY LAYER (CBL) BY RAPIDLY RISING BUOYANT PLUMES            PXPBL.30
C    AND MORE GRADUAL DOWNWARD TRANSPORT BY BROAD SLOW COMPENSATORY              PXPBL.31
C    SUBSIDENCE (PLEIM AND CHANG, 1992). THE EDDY DIFFUSION MODEL IS BASED ON    PXPBL.32
C    BLACKADAR (1976) WHICH IS ESSENTIALLY THE SAME AS IN HIRPBL.                PXPBL.33
C    THE SURFACE FLUXES ARE DETERMINED FROM THE MEAN WIND, HUMIDITY AND          PXPBL.34
C    TEMPERATURE DIFFERENCES BETWEEN THE SURFACE AND THE PBL USING NEW           PXPBL.35
C    PARAMETERIZATIONS .                                                         PXPBL.36
C                                                                                PXPBL.37
C  SUBROUTINES CALLED:                                                           PXPBL.38
C    RADSFC             calculate short- and long-wave radiation at sfc          PXPBL.39
C    SURFPX             land-surface model                                       PXPBL.40
C    EDDYMM4            calculate eddy coefficients                              PXPBL.41
C    VDIFF              eddy diffusion model                                     PXPBL.42
C    ACM                Asymmetrical Convective Model                            PXPBL.43
C                                                                                PXPBL.44
C  REFERENCES:                                                                   PXPBL.45
C    Grell et al (1993), MM5 model description                                   PXPBL.46
C    HOLTSLAG A.A.A. AND NIEUWSTALDT F.T.M. (1986), BOUND.-LAYER                 PXPBL.47
C         METEOROL., 36, 201-209.                                                PXPBL.48
C    JACQUEMIN B. AND NOILHAN J. (1990), BOUND.-LAYER METEOROL., 52,             PXPBL.49
C         93-134.                                                                PXPBL.50
C    NOILHAN J. AND PLANTON S. (1989), J. GEOPHYS. RES., 86, 9929-9938.          PXPBL.51
C    PLEIM J. E. AND CHANG J.S. (1992), ATMOS. ENVIRON., 26A, 965-981.           PXPBL.52
C    Pleim J. E. and A. Xiu (1995), JAM, 34, 16-32.                              PXPBL.53
C    Xiu and Pleim (2000), JAM, 40, 192-209.                                     PXPBL.54
                                                                                 PXPBL.56
C                                                                                PXPBL.57
C  REVISION HISTORY:                                                             PXPBL.58
C    JP            4/01 - Clean up extra arrays and mods for ACM2                PXPBL.59
C    JP & JD       2/00 - NCAR compatable - added soilp                          PXPBL.60
C    JP            1/00 - Removed reads of vegeland and soil                     PXPBL.61
C    A. Xiu        9/99 - Implement in MM5V3                                     PXPBL.62
C    D. TIVEL      8/95 - Minor adjustments from MM4 to MM5                      PXPBL.63
C    JP            3/95 - Added switch and read for initializing                 PXPBL.64
C                         soil moisture from previous run                        PXPBL.65
C    AX & JEP     12/94 - Added SM nudging and sigf = 0.5                        PXPBL.66
C    AX           11/94 - Replaced Byun flux-prof sub w/ MM5 scheme              PXPBL.67
C    A. XIU        7/93 - 3-d                                                    PXPBL.68
C    J. PLEIM      4/93 - 1-d                                                    PXPBL.69
C                                                                                PXPBL.70
C***********************************************************************         PXPBL.71
C..........  Parameters                                                          PXPBL.72
#     include <parame.incl>                                                      PXPBL.73
#     include <rpstar.incl>                                                      PXPBL.74
#     include <sum.incl>                                                         PXPBL.75
#     include <varia.incl>                                                       PXPBL.76
#     include <dusolve1.incl>                                                    PXPBL.77
#     include <param2.incl>                                                      PXPBL.78
#     include <param3.incl>                                                      PXPBL.79
#     include <pmoist.incl>                                                      PXPBL.80
#     include <point2d.incl>                                                     PXPBL.81
#     include <various.incl>                                                     PXPBL.82
#     include <nhcnst.incl>                                                      PXPBL.83
#     include <fddagd.incl>                                                      PXPBL.84
#     include <parfdda.incl>                                                     PXPBL.85
#     include <comicl.incl>                                                      PXPBL.86
#     include <parpx.incl>                                                       PXPBL.87
#     include <soilp.incl>                                                       PXPBL.88
C                                                                                PXPBL.89
      INTEGER IYY,JXX,J,IN                                                       PXPBL.90
      REAL      U3D(MIX,MJX,MKX),V3D(MIX,MJX,MKX),T3D(MIX,MJX,MKX),              PXPBL.91
     -    QV3D(MIX,MJX,MKX),PP3D(MIX,MJX,MKX),QC3D(MIXM,MJXM,MKXM),              PXPBL.92
     -    QI3D(MIXIC,MJXIC,MKXIC),QNC3D(MIXIG,MJXIG,MKXIG),                      PXPBL.93
     -    U3DTEN(MIX,MJX,MKX),V3DTEN(MIX,MJX,MKX),T3DTEN(MIX,MJX,MKX),           PXPBL.94
     -    QV3DTEN(MIX,MJX,MKX),QC3DTEN(MIXM,MJXM,MKXM),                          PXPBL.95
     -    QI3DTEN(MIXIC,MJXIC,MKXIC),QNC3DTEN(MIXIG,MJXIG,MKXIG)                 PXPBL.96
C                                                                                PXPBL.97
C..........  Local variables                                                     PXPBL.98
C                                                                                PXPBL.99
      REAL PSURF(MIX)                                                            PXPBL.100
C                                                                                PXPBL.101
      REAL       TTNP(MIX,MKX),  QTNP(MIX,MKX), QCTNP(MIXM,MKXM),                PXPBL.102
     &            UXS(MIX,MKX),                                                  PXPBL.103
     &            VXS(MIX,MKX),  THXS(MIX,MKX),   QXS(MIX,MKX),                  PXPBL.104
     &           QCXS(MIXM,MKXM),THCON(MIX,MKX)                                  PXPBL.105
C                                                                                PXPBL.106
      REAL      QITNP(MIXM,MKXM), QIXS(MIXM,MKXM)                                PXPBL.107
      REAL      QNCNP(MIXM,MKXM), QNCXS(MIXM,MKXM)                               PXPBL.108
      REAL        UTNP(MIX,MKX),    VTNP(MIX,MKX)                                PXPBL.109
C                                                                                PXPBL.110
      INTEGER     KPBL(MIX)                                                      PXPBL.111
C                                                                                PXPBL.112
C...  CFRLO,CFRMD and CFRHI are temperary arrays for holding cloud               PXPBL.113
C       cover in order to be printed out                                         PXPBL.114
      REAL CFRLO(MIX,MJX),CFRMD(MIX,MJX),CFRHI(MIX,MJX)                          PXPBL.115
      REAL RIB(MIX,MKX)    ! temp. array for bulk-Richardson number              PXPBL.116
      INTEGER KPBLH(MIX)   ! temp. array for computing PBL height                PXPBL.117
      REAL FINT(MIX)       ! temp. array for interpolation of PBL height         PXPBL.118
      REAL THOBS(MIX)      ! obs. TEMP for FDDA                                  PXPBL.119
C                                                                                PXPBL.120
C... for computing the CBL mixing rate                                           PXPBL.121
      REAL MBMAX,MBMIN,AMOL                                                      PXPBL.122
CC                                                                               PXPBL.123
C--------------------------------------                                          PXPBL.124
C-- SOIL PARAMETERS ARE SPECIFIED BY SOIL TYPE:                                  PXPBL.125
C   #  SOIL TYPE  WSAT  WFC  WWLT    B   CGSAT   JP   AS   C2R  C1SAT            PXPBL.126
C   _  _________  ____  ___  ____  ____  _____   ___  ___  ___  _____            PXPBL.127
C   1  SAND       .395 .135  .068  4.05  3.222    4  .387  3.9  .082             PXPBL.128
C   2  LOAMY SAND .410 .150  .075  4.38  3.057    4  .404  3.7  .098             PXPBL.129
C   3  SANDY LOAM .435 .195  .114  4.90  3.560    4  .219  1.8  .132             PXPBL.130
C   4  SILT LOAM  .485 .255  .179  5.30  4.418    6  .105  0.8  .153             PXPBL.131
C   5  LOAM       .451 .240  .155  5.39  4.111    6  .148  0.8  .191             PXPBL.132
C   6  SND CLY LM .420 .255  .175  7.12  3.670    6  .135  0.8  .213             PXPBL.133
C   7  SLT CLY LM .477 .322  .218  7.75  3.593    8  .127  0.4  .385             PXPBL.134
C   8  CLAY LOAM  .476 .325  .250  8.52  3.995   10  .084  0.6  .227             PXPBL.135
C   9  SANDY CLAY .426 .310  .219 10.40  3.058    8  .139  0.3  .421             PXPBL.136
C  10  SILTY CLAY .482 .370  .283 10.40  3.729   10  .075  0.3  .375             PXPBL.137
C  11  CLAY       .482 .367  .286 11.40  3.600   12  .083  0.3  .342             PXPBL.138
C                                                                                PXPBL.139
C.......... DATA statement for SOIL PARAMETERS for the 11 soil types             PXPBL.140
C                                                                                PXPBL.141
C-- WSAT is saturated soil moisture (M^3/M^3) (JN90)                             PXPBL.142
      DATA WSAT/.395,.410,.435,.485,.451,.420,.477,.476,.426,.482,.482/          PXPBL.143
C                                                                                PXPBL.144
C-- WFC is field capacity (M^3/M^3) (JN90)                                       PXPBL.145
      DATA WFC/.135,.150,.195,.255,.240,.255,.322,.325,.310,.370,.367/           PXPBL.146
C                                                                                PXPBL.147
C-- WWLT is wilting point (M^3/M^3) (JN90)                                       PXPBL.148
      DATA WWLT/.068,.075,.114,.179,.155,.175,.218,.250,.219,.283,.286/          PXPBL.149
C                                                                                PXPBL.150
C-- B is slop of the retention curve (NP89)                                      PXPBL.151
      DATA B/4.05,4.38,4.90,5.30,5.39,7.12,7.75,8.52,10.40,10.40,11.40/          PXPBL.152
C                                                                                PXPBL.153
C-- CGSAT is soil thermal coef. at saturation (10^-6 K M^2 J^-1) (NP89)          PXPBL.154
      DATA CGSAT/3.222,3.057,3.560,4.418,4.111,3.670,3.593,3.995,                PXPBL.155
     &           3.058,3.729,3.600/                                              PXPBL.156
C                                                                                PXPBL.157
C-- JP is coefficient of WGEQ formulation (NP89)                                 PXPBL.158
      DATA JP/4,4,4,6,6,6,8,10,8,10,12/                                          PXPBL.159
C                                                                                PXPBL.160
C-- AS is coefficient of WGEQ formulation (NP89)                                 PXPBL.161
      DATA AS/.387,.404,.219,.105,.148,.135,.127,.084,.139,.075,.083/            PXPBL.162
C                                                                                PXPBL.163
C-- C2R is the value of C2 for W2=0.5WSAT (NP89)                                 PXPBL.164
      DATA C2R/3.9,3.7,1.8,0.8,0.8,0.8,0.4,0.6,0.3,0.3,0.3/                      PXPBL.165
C                                                                                PXPBL.166
C-- C1SAT is the value of C1 at saturation (NP89)                                PXPBL.167
      DATA C1SAT/.082,.098,.132,.153,.191,.213,.385,.227,.421,                   PXPBL.168
     &           .375,.342/                                                      PXPBL.169
C                                                                                PXPBL.170
C                                                                                PXPBL.171
C..........  DATA statement for CONSTANTS                                        PXPBL.172
C-- for soil                                                                     PXPBL.173
      DATA TAUINV/.000011574074074/  ! 1/1DAY(SEC)                               PXPBL.174
      DATA CV/1.0E-3/      ! K M2/J                                              PXPBL.175
      DATA DS1/0.01/       ! first soil layer thickness (=1CM) in meter          PXPBL.176
      DATA DS2/1.0/        ! second soil layer thickness in meter                PXPBL.177
      DATA DENW/1000.0/    ! water density in KG/M3                              PXPBL.178
      DATA SIGF/0.5/       ! rain interception see LSM (can be 0-1)              PXPBL.179
C                                                                                PXPBL.180
      REAL RIC                                                                   PXPBL.181
      DATA RIC/0.25/       ! critical Richardson number                          PXPBL.182
C                                                                                PXPBL.183
C-- for calculating CBL mixing rate                                              PXPBL.184
      DATA C1ACM/0.2721655/     !  SQRT(2/27)                                    PXPBL.185
      DATA C2ACM/-0.33333/                                                       PXPBL.186
C                                                                                PXPBL.187
C-- minor loop time step                                                         PXPBL.188
      REAL DTPBLX                                                                PXPBL.189
      DATA DTPBLX/40.0/       ! SEC, INTERNAL TIMESTEP (FOR 30 LAYERS)           PXPBL.190
C                                                                                PXPBL.191
C... indexes                                                                     PXPBL.192
      INTEGER I,JJ,K,JJL,IIL,KK,IT,IR,JR                                         PXPBL.193
C                                                                                PXPBL.194
C... local temparory variables                                                   PXPBL.195
      INTEGER ISTI,KCBL,NOCONVI,JBGN,JEND,IBGN,IEND,JDAY,NTSPS                   PXPBL.196
      REAL DZERO,T2I,FSEAS                                                       PXPBL.197
      REAL DTPBL,TIMEC,TIMEMIN,XT24,TIMEGMT,TIMEG                                PXPBL.198
      REAL ES,HU,CVIRT,TV0,CPOT,WSS,TCONV,TH1                                    PXPBL.199
      REAL DTMP,TOG,WSSQ,HFLXSL,HFMIX,VAPPRS,QSBT                                PXPBL.200
      REAL CORB,CORE,CURRTIME,TOBS                                               PXPBL.201
      REAL PL,TVCON,PRES0,CELL                                                   PXPBL.202
      REAL WGOLD,W2OLD,WROLD,T2OLD,XXT                                           PXPBL.203
C     LOGICAL      FIRSTIME                                                      PXPBL.204
C     DATA         FIRSTIME / .TRUE. /                                           PXPBL.205
C     SAVE         FIRSTIME                                                      PXPBL.206
                                                                                 PXPBL.207
      INTEGER KPBLHT                                                             PXPBL.208
C                                                                                PXPBL.209
C                                                                                PXPBL.210
C-------------------------------Exicutable starts here--------------------       PXPBL.211
C     IF ( FIRSTIME ) THEN                                                       PXPBL.212
C        FIRSTIME = .FALSE.                                                      PXPBL.213
C........... calculate PI                                                        PXPBL.214
         ALN10  = ALOG(10.0)                                                     PXPBL.215
         PI     = 4.0 * ATAN(1.0)                                                PXPBL.216
         G1000  = G*1.0E-3            ! G/1000                                   PXPBL.217
         CRANKP = 0.5                 ! CRANK-NIC PARAMETER                      PXPBL.218
C---SIGMA---------------                                                         PXPBL.219
         DO K = 1,KLP1                                                           PXPBL.220
           KK = KLP1 - K                                                         PXPBL.221
           SIGMAF(KK) = SIGMA(K)       ! SIGMAF starts from 0 to KL              PXPBL.222
         ENDDO                                                                   PXPBL.223
C                                                                                PXPBL.224
         DO K = 1,KL                                                             PXPBL.225
           KK = KL - K + 1                                                       PXPBL.226
           SIGMAH(K) = A(KK)                                                     PXPBL.227
           DSIGH(K)=SIGMAF(K)-SIGMAF(K-1)                                        PXPBL.228
           DSIGHI(K)=1./DSIGH(K)                                                 PXPBL.229
         ENDDO                                                                   PXPBL.230
C                                                                                PXPBL.231
         DO K=1,KLM                                                              PXPBL.232
           DSIGFI(K)=1./(SIGMAH(K+1)-SIGMAH(K))                                  PXPBL.233
         ENDDO                                                                   PXPBL.234
C                                                                                PXPBL.235
         DSIGFI(KL)=DSIGFI(KLM)                                                  PXPBL.236
         DSIGFI(0)=DSIGFI(1)                                                     PXPBL.237
C                                                                                PXPBL.238
C-- DEFINE HEIGHT AND PRESSURE OF FULL AND HALF LEVELS                           PXPBL.239
C                                                                                PXPBL.240
        DO 221 I = 1,ILX                                                         PXPBL.241
          ZF(I,0) = 0.0                                                          PXPBL.242
          CELL = PTOP * RPSB(I,J)                                                PXPBL.243
          Z(I,1) = ROVG * T0(I,J,KL) * ALOG((SIGMAF(0)                           PXPBL.244
     >             + CELL)/ (SIGMAH(1) + CELL))                                  PXPBL.245
221     CONTINUE                                                                 PXPBL.246
C                                                                                PXPBL.247
        DO 211 K=1,KL                                                            PXPBL.248
          DO 210 I = 1,ILX                                                       PXPBL.249
            KK = KL - K + 1 ! T0 is in MM5 vertical coordinate                   PXPBL.250
            CELL = PTOP * RPSB(I,J)                                              PXPBL.251
            ZF(I,K)  = ZF(I,K-1) + ROVG * T0(I,J,KK)                             PXPBL.252
     &                 * ALOG((SIGMAF(K-1) + CELL) /                             PXPBL.253
     &                 (SIGMAF(K) + CELL))                                       PXPBL.254
            DZZ(I,K) = ZF(I,K) - ZF(I,K-1)                                       PXPBL.255
210       CONTINUE                                                               PXPBL.256
211     CONTINUE                                                                 PXPBL.257
C                                                                                PXPBL.258
        DO 231 K = 2,KL                                                          PXPBL.259
          DO 230 I = 1,ILX                                                       PXPBL.260
            KK = KL - K + 1                                                      PXPBL.261
            CELL = PTOP * RPSB(I,J)                                              PXPBL.262
            Z(I,K) = Z(I,K-1) + ROVG * 0.5 * (T0(I,J,KK+1) +                     PXPBL.263
     &               T0(I,J,KK)) * ALOG((SIGMAH(K-1)                             PXPBL.264
     >               + CELL) / (SIGMAH(K)  + CELL))                              PXPBL.265
230       CONTINUE                                                               PXPBL.266
231     CONTINUE                                                                 PXPBL.267
C     END IF          !  if firstime                                             PXPBL.268
C                                                                                PXPBL.269
C                                                                                PXPBL.270
C-----DECOUPLE FLUX-FORM VARIABLES TO GIVE U,V,T,THETA,THETA-VIR.,               PXPBL.271
C     T-VIR., QV, AND QC AT CROSS POINTS AND AT KTAU-1.                          PXPBL.272
C                                                                                PXPBL.273
C     *** NOTE ***                                                               PXPBL.274
C         THE BOUNDARY WINDS MAY NOT BE ADEQUATELY AFFECTED BY FRICTION,         PXPBL.275
C         SO USE ONLY INTERIOR VALUES OF UX AND VX TO CALCULATE                  PXPBL.276
C         TENDENCIES.                                                            PXPBL.277
C                                                                                PXPBL.278
        DO 30 K = 1,KL                                                           PXPBL.279
          KK = KL - K +1                                                         PXPBL.280
          DO 25 I = 1,ILX                                                        PXPBL.281
            US(I,KK) = 0.25 * (U3D(I,J,K) + U3D(I+1,J,K) +                       PXPBL.282
     &                U3D(I,J+1,K) + U3D(I+1,J+1,K))                             PXPBL.283
            VS(I,KK) = 0.25 * (V3D(I,J,K) + V3D(I+1,J,K) +                       PXPBL.284
     &                V3D(I,J+1,K) + V3D(I+1,J+1,K))                             PXPBL.285
25        CONTINUE                                                               PXPBL.286
30      CONTINUE                                                                 PXPBL.287
C                                                                                PXPBL.288
C                                                                                PXPBL.289
      DO 40 K = 1,KL                                                             PXPBL.290
        KK = KL - K +1                                                           PXPBL.291
        DO 35 I = 1,ILX                                                          PXPBL.292
          P(I,KK) = A(K) * PSB(I,J) + PTOP + PP3D(I,J,K)*0.001                   PXPBL.293
          TT(I,KK) = T3D(I,J,K)                                                  PXPBL.294
          THCON(I,K) = (100.0 / P(I,KK)) ** ROVCP                                PXPBL.295
          THETA(I,KK) = TT(I,KK) * THCON(I,K)                                    PXPBL.296
          TV(I,KK) = T3D(I,J,K)                                                  PXPBL.297
          THETAV(I,KK) = THETA(I,KK)                                             PXPBL.298
          QVS(I,K)   = 0.0                                                       PXPBL.299
35      CONTINUE                                                                 PXPBL.300
40    CONTINUE                                                                   PXPBL.301
C                                                                                PXPBL.302
      IF (IDRY(IN) .EQ. 0) THEN        ! not dry case                            PXPBL.303
        DO 50 K = 1,KL                                                           PXPBL.304
          KK = KL - K +1                                                         PXPBL.305
          DO 45 I = 1,ILX                                                        PXPBL.306
            QVS(I,K) = QV3D(I,J,KK)                                              PXPBL.307
            TVCON = (1.0 + XMOIST(IN) * EP1 * QVS(I,K))                          PXPBL.308
            THETAV(I,K) = THETA(I,K) * TVCON                                     PXPBL.309
            TV(I,K) = TT(I,K) * TVCON                                            PXPBL.310
45        CONTINUE                                                               PXPBL.311
50      CONTINUE                                                                 PXPBL.312
C                                                                                PXPBL.313
        IF (IMOIST(IN) .NE. 1 .AND. IMOIST(IN) .NE. 3) THEN                      PXPBL.314
          DO 70 K = 1,KL                                                         PXPBL.315
            KK = KL - K +1                                                       PXPBL.316
            DO 65 I = 1,ILX                                                      PXPBL.317
              QCS(I,KK) = QC3D(I,J,K)                                            PXPBL.318
              QIS(I,K) = 0.0                                                     PXPBL.319
              IF (IICE .EQ. 1) QIS(I,KK) = QI3D(I,J,K)                           PXPBL.320
              IF (IICEG .EQ. 1) QNCS(I,KK) = QNC3D(I,J,K)                        PXPBL.321
65          CONTINUE                                                             PXPBL.322
70        CONTINUE                                                               PXPBL.323
        ENDIF                                                                    PXPBL.324
C                                                                                PXPBL.325
      ENDIF  ! IDRY                                                              PXPBL.326
C                                                                                PXPBL.327
C-----SAVE THE INITIAL VALUE BEFORE MINOR LOOP IN THE SAVING ARRAYS.             PXPBL.328
C                                                                                PXPBL.329
      DO 80 K = 1,KL                                                             PXPBL.330
        DO 75 I = 1,ILX                                                          PXPBL.331
          THXS(I,K) = THETA(I,K)                                                 PXPBL.332
          UXS(I,K)  = US(I,K)                                                    PXPBL.333
          VXS(I,K)  = VS(I,K)                                                    PXPBL.334
75      CONTINUE                                                                 PXPBL.335
80    CONTINUE                                                                   PXPBL.336
C                                                                                PXPBL.337
      IF (IDRY(IN) .NE. 1) THEN          ! not dry case                          PXPBL.338
        DO 90 K = 1,KL                                                           PXPBL.339
          DO 85 I = 1,ILX                                                        PXPBL.340
            QXS (I,K) = QVS(I,K)                                                 PXPBL.341
85        CONTINUE                                                               PXPBL.342
90     CONTINUE                                                                  PXPBL.343
C                                                                                PXPBL.344
        IF (IMOIST(IN) .NE. 1 .AND. IMOIST(IN) .NE. 3) THEN                      PXPBL.345
          DO 100 K = 1,KL                                                        PXPBL.346
            DO 95 I = 1,ILX                                                      PXPBL.347
              QCXS(I,K) = QCS(I,K)                                               PXPBL.348
              IF (IICE .EQ. 1) QIXS(I,K) = QIS(I,K)                              PXPBL.349
              IF (IICEG .EQ. 1) QNCXS(I,K) = QNCS(I,K)                           PXPBL.350
95          CONTINUE                                                             PXPBL.351
100       CONTINUE                                                               PXPBL.352
        ENDIF                                                                    PXPBL.353
      ENDIF                                                                      PXPBL.354
C                                                                                PXPBL.355
C..........  Start the land-surface/pbl model                                    PXPBL.356
C                                                                                PXPBL.357
      DO 101 I = 1,ILX                                                           PXPBL.358
        ISNOW(I) = 0                                                             PXPBL.359
        IF (SNOWC(I,J) .GT. 0.0) THEN                                            PXPBL.360
          ISNOW(I) = 1               ! =0, no snow; =1, have snow                PXPBL.361
        ENDIF                                                                    PXPBL.362
101   CONTINUE                                                                   PXPBL.363
C                                                                                PXPBL.364
C..........  INITIALIZE THE ARRAYS NEEDED (FIRST TIME ONLY!!!)                   PXPBL.365
      IF (ABS(XTIME) .GT. 0.001) GO TO 130                                       PXPBL.366
C                                                                                PXPBL.367
        IF(J.EQ.1) print *,' PX Namelist: ISMRD=',ISMRD,                         PXPBL.368
     >          ' NUDGE=',NUDGE,                                                 PXPBL.369
     >          ' IFGROW=',IFGROW                                                PXPBL.370
        DO 121 I = 1,ILX                                                         PXPBL.371
C..... make ist=0 when landuse is water                                          PXPBL.372
          IF (XLAND(I,J) .GT.1.5) ISLTYP(I,J) = 0                                PXPBL.373
          ISTI = ISLTYP(I,J)                                                     PXPBL.374
          IF(ISTI.GT.NSTPS) THEN                                                 PXPBL.375
             PRINT *,' I,J,ISTI=',I,J,ISTI                                       PXPBL.376
             print*,' XLAND =',XLAND(I,J)                                        PXPBL.377
             PRINT *,' BAD SOIL TYPE - RESETTING TO IST=5 (LOAM)'                PXPBL.378
C            STOP ' BAD SOIL TYPE'                                               PXPBL.379
             ISLTYP(I,J) = 5                                                     PXPBL.380
          ENDIF                                                                  PXPBL.381
          MOL(I,J) = 1000.                                                       PXPBL.383
          TST(I,J) = 0.0                                                         19DEC02.1803
          WST(I,J) = 0.0                                                         19DEC02.1804
121     CONTINUE                                                                 PXPBL.386
                                                                                 PXPBL.387
C-- Initialize soil moisture                                                     PXPBL.388
C*                                                                               PXPBL.389
C---Either read soil moisture from previous run or create from MAVAIL            PXPBL.390
C                                                                                PXPBL.391
      IF(ISMRD.EQ.1) THEN                                                        PXPBL.392
C-- Initial soil moisture, soil temperature, and canopy moisture are read        PXPBL.393
C-- in from MMINPUT                                                              PXPBL.394
C                                                                                PXPBL.395
          DO 1201 I = 1,ILX                                                      PXPBL.396
C... for water we use the MM5 initialization since SST is updated weekely, AX    PXPBL.397
            IF(XLAND(I,J) .GT.1.5) THEN                                          PXPBL.398
              TGA(I,J) = TSS(I,J)                                                PXPBL.399
              STCA(I,J,2) = TSS(I,J)                                             PXPBL.400
            ELSE                                                                 PXPBL.401
              TGA(I,J) = STCA(I,J,1)                                             PXPBL.402
            ENDIF                                                                PXPBL.403
1201      CONTINUE                                                               PXPBL.404
      ELSE IF(ISMRD.EQ.0) THEN  ! first run - use internal initialization        PXPBL.405
C                                                                                PXPBL.406
        DO 122 I = 1,ILX                                                         PXPBL.407
          CMCA(I,J)    = 0.0                                                     PXPBL.408
          STCA(I,J,2) = TMN(I,J)                                                 PXPBL.409
          ISTI = ISLTYP(I,J)                                                     PXPBL.410
          IF (ISTI .EQ. 0) THEN                                                  PXPBL.411
C-- FOR WATER AND PERMANENT ICE NO DEFINITION OF WG,W2 AND WR                    PXPBL.412
            SMCA(I,J,1) = 0.0                                                    PXPBL.413
            SMCA(I,J,2) = 0.0                                                    PXPBL.414
          ELSE                                                                   PXPBL.415
C-- Initialize soil moisture by MAVAIL                                           PXPBL.416
            SMCA(I,J,2) = WWLT(ISTI) + (WFC(ISTI) - WWLT(ISTI))                  PXPBL.417
     >                    * MAVAIL(I,J)                                          PXPBL.418
            SMCA(I,J,1) = SMCA(I,J,2)                                            PXPBL.419
C           PRINT *,' I,J,ISTI=',I,J,ISTI,' WWLT,WFC=',WWLT(ISTI),               PXPBL.420
C    >       WFC(ISTI),' MAVAIL=',MAVAIL(I,J),' SMCA=',SMCA(I,J,2)               PXPBL.421
          ENDIF                                                                  PXPBL.422
122     CONTINUE                                                                 PXPBL.423
      ELSE IF(ISMRD.EQ.2) THEN    ! SOIL MOISTURE INTITIALIZED BY ETA            PXPBL.424
         DO 1221 I = 1,ILX                                                       PXPBL.425
           STCA(I,J,2) = TMN(I,J)                                                PXPBL.426
           CMCA(I,J)    = 0.0                                                    PXPBL.427
1221     CONTINUE                                                                PXPBL.428
      ELSE                                                                       PXPBL.429
         PRINT *,' INVALID SELECTION FOR ISMRD =',ISMRD                          PXPBL.430
         STOP                                                                    PXPBL.431
      ENDIF  ! ISMRD                                                             PXPBL.432
C---                                                                             PXPBL.433
      DO 123 I=1,ILX                                                             PXPBL.434
        SMCB(I,J,1) = SMCA(I,J,1)                                                PXPBL.435
        SMCB(I,J,2) = SMCA(I,J,2)                                                PXPBL.436
        CMCB(I,J) = CMCA(I,J)                                                    PXPBL.437
        STCB(I,J,2) = STCA(I,J,2)                                                PXPBL.438
        TGB(I,J) = TGA(I,J)                                                      PXPBL.439
C-- Save veg and soil arrays                                                     PXPBL.440
c       ISLTYP(I,J) = IST(I,J)                                                   PXPBL.441
c       ZNT(I,J)  = XZ0(I,J)                                                     PXPBL.442
c       RSTMIN(I,J) = XRSMIN(I,J)                                                PXPBL.443
123   CONTINUE                                                                   PXPBL.444
C                                                                                PXPBL.447
130   CONTINUE  ! End bypass for XTIME .GT. 0                                    PXPBL.448
C -----------------------END OF INITIAL SETUP------------------------            PXPBL.449
C                                                                                PXPBL.450
CC..........  LOAD LOCAL VARIABLES                                               PXPBL.451
      DO 10 I = 1,ILX                                                            PXPBL.452
        TG(I) = TGB(I,J)                                                         PXPBL.453
        T2(I) = STCB(I,J,2)                                                      PXPBL.454
        WG(I) = SMCB(I,J,1)                                                      PXPBL.455
        W2(I) = SMCB(I,J,2)                                                      PXPBL.456
        WR(I) = CMCB(I,J)                                                        PXPBL.457
        NOCONV(I) = 0
        RUNOFFR(I) = 0.0
10    CONTINUE                                                                   PXPBL.458
      TEVA = 0.0           ! intial value for evaporation from ground
                                                                                 PXPBL.459
C..........  Calculate the precipitation rate in (m/s)                           PXPBL.460
C                                                                                PXPBL.461
      DO 131 I = 1,ILX                                                           PXPBL.462
        PRECIP(I) = 0.01 * RAINP(I,J) / (0.5*DT)                                 PXPBL.463
        IF (PRECIP(I) .LT. -1.0E-15) THEN                                        PXPBL.464
132       PRINT *,'SOMETHING WRONG WITH THE PRECIPITATION RATE AT',              PXPBL.465
     &            ' I J XTIME',I,J,XTIME,' RAINC=',RAINC(I,J),                   PXPBL.466
     &            ' RAINNC=',RAINNC(I,J),' RAINP=',RAINP(I,J),                   PXPBL.467
     &            ' PRECIP=',PRECIP(I)                                           PXPBL.468
C          STOP 132                                                              PXPBL.469
        ENDIF                                                                    PXPBL.470
        PRECIP(I) = AMAX1(PRECIP(I),0.0)                                         PXPBL.471
C                                                                                PXPBL.472
131   CONTINUE                                                                   PXPBL.473
C                                                                                PXPBL.474
      DO 181 K = 1,KL                                                            PXPBL.475
      DO 181 I = 1,ILX                                                           PXPBL.476
          KK = KL - K + 1 ! T0 and PP3D are in MM5 vertical coordinate           PXPBL.477
          PF(I,K) = SIGMAF(K) * PSB(I,J) + PTOP +                                PXPBL.478
     &              PP3D(I,J,KK) * 0.001                                         PXPBL.479
          PRES0= (PSB(I,J)*SIGMAH(K)+PTOP)*1000. !! ref pres at half sigma       PXPBL.480
          DENSX(I,K) = PRES0 / (R * T0(I,J,KK))                                  PXPBL.481
181   CONTINUE                                                                   PXPBL.482
C                                                                                PXPBL.483
      DO 191 I = 1,ILX                                                           PXPBL.484
        PF(I,0) =  PSB(I,J) + PTOP + PP3D(I,J,KL) * 0.001                        PXPBL.485
191   CONTINUE                                                                   PXPBL.486
C                                                                                PXPBL.487
C--Setup for surface radiation                                                   PXPBL.488
C                                                                                PXPBL.489
C--CALCULATE SQADR, THE SQURE OF THE RATIO OF THE AVERAGE DISTANCE OF            PXPBL.490
C  THE EARTH FROM THE SUN TO ITS LOCATION AT ANY TIME OF THE YEAR                PXPBL.491
C  SEE PIELKE, P.211, EQ.(8-58A)                                                 PXPBL.492
C                                                                                PXPBL.493
C--DZERO IS DEFINED IN TERMS OF THE DAY NUMBER STARTING WITH 0                   PXPBL.494
C  ON JANUARY 1 AND ENDING ON 364 ON DECEMBER 31                                 PXPBL.495
      JDAY  = JULDAY + INT((XTIME / 60.0 + GMT) / 24.0 + 0.001)                  PXPBL.496
      DZERO = 2.0 * PI * (JDAY - 1.0) / 365.0                                    PXPBL.497
      SQADR = 1.000110 + 0.034221*COS(DZERO) + 0.001280*SIN(DZERO) +             PXPBL.498
     &        0.000719*COS(2.0*DZERO) + 0.000077*SIN(2.0*DZERO)                  PXPBL.499
C                                                                                PXPBL.500
C-- Define Vegetation Parameters similarly to BATS                               PXPBL.501
C-- Note that when IFGROW = 0 is selected in Vegeland then max and min           PXPBL.502
C-- LAI and Veg are the same                                                     PXPBL.503
      DO 201 I = 1, ILX                                                          PXPBL.504
        T2I = T2(I)                                                              PXPBL.505
C        FSEAS = AMAX1(1.0 - 0.0016 * (298.0 - T2I) ** 2,0.0)  ! BATS            PXPBL.506
        FSEAS = AMAX1(1.0 - 0.015625 * (290.0 - T2I) ** 2,0.0) ! JP97            PXPBL.507
        IF (T2I .GE. 290.0) FSEAS = 1.0                                          PXPBL.508
        LAI(I,J) = XLAIMN(I,J) + FSEAS*(XLAI(I,J) - XLAIMN(I,J))                 PXPBL.509
        VEGFRC(I,J) = XVEGMN(I,J) + FSEAS*(XVEG(I,J) - XVEGMN(I,J))              PXPBL.510
        IF (XLAND(I,J) .GT. 1.5) THEN ! if over water                            PXPBL.511
           VEGFRC(I,J) = 0. !MAKE Sure veg algorithms not used for water         PXPBL.512
           ZNT(I,J) = 1.0E-6 ! m                                                 PXPBL.513
        ENDIF                                                                    PXPBL.514
201   CONTINUE                                                                   PXPBL.515
C                                                                                PXPBL.516
C                                                                                PXPBL.517
C----------TIME LOOP------------                                                 PXPBL.518
      DTPBL = DT                                                                 PXPBL.519
      TIMEC = 0.0                                                                PXPBL.520
      TIMEMIN=0.0                                                                PXPBL.521
      XT24 = AMOD(XTIME,1440.0)                                                  PXPBL.522
      TIMEGMT = GMT + XT24 / 60.0                                                PXPBL.523
      TIMEG = AMOD(TIMEGMT,24.)                                                  PXPBL.524
C     IF (XTIME .NE. 0.0) THEN ! USE SHORT TS FIRST TIME                         PXPBL.525
C       DO 2333 I=1,ILX                                                          PXPBL.526
C        IF(ABS(HFX(I,J)).GT.50.0.OR.                                            PXPBL.527
C    >      UST(I,J).GT.0.4) THEN                                                PXPBL.528
C          DTPBL = DTPBLX                                                        PXPBL.529
C        ENDIF                                                                   PXPBL.530
C2333    CONTINUE                                                                PXPBL.531
C     ELSE                                                                       PXPBL.532
        DTPBL = DTPBLX                                                           PXPBL.533
C     ENDIF                                                                      PXPBL.534
      NTSPS = INT(DT / (DTPBL + 0.000001) + 1.0)                                 PXPBL.535
      DTPBL = DT / NTSPS                                                         PXPBL.536
C                                                                                PXPBL.537
C      WRITE(6,*) ' DT,DTPBL,NTSPS=',DT,DTPBL,NTSPS                              PXPBL.538
      DO 1111 IT=1,NTSPS                                                         PXPBL.539
C                                                                                PXPBL.540
        DO 2111 K=1,KL                                                           PXPBL.541
          DO 2101 I = 1,ILX                                                      PXPBL.542
            DENS(I,K) = P(I,K) * 1000.0 / (R * TV(I,K))                          PXPBL.543
2101      CONTINUE                                                               PXPBL.544
2111    CONTINUE                                                                 PXPBL.545
                                                                                 PXPBL.546
        DO 232 I = 1,ILX                                                         PXPBL.547
C..........  compute the saturated mixing ratio at surface temp.(TG)             PXPBL.548
C SATURATION VAPOR PRESSURE (MB) OVER WATER                                      PXPBL.549
        IF(ISNOW(I).GT.0.AND.TG(I).LE.273.15) THEN                               PXPBL.550
          ES=6.11*EXP(22.514-6.15E3/TG(I))    ! mb                               PXPBL.551
        ELSE                                                                     PXPBL.552
          ES = 10.*SVP1 * EXP(SVP2 * (TG(I) - SVPT0) /  !mb                      PXPBL.553
     &               (TG(I) - SVP3))                                             PXPBL.554
        ENDIF                                                                    PXPBL.555
C PRESSURE IN MB                                                                 PXPBL.556
          PSURF(I) = PF(I,0) * 10.0                                              PXPBL.557
          QSS(I)  = ES * 0.622 / (PSURF(I) - ES)                                 PXPBL.558
C                                                                                PXPBL.559
C-- compute sfc rh from soil moisture                                            PXPBL.560
C*  SATURATED FOR WATER SURFACE AND SNOW (ISNOW=1)                               PXPBL.561
          HU = 1.0                                                               PXPBL.562
          BETAP(I) = 1.0                                                         PXPBL.563
C*                                                                               PXPBL.564
C-- FOR SURFACE OVER LAND                                                        PXPBL.565
          ISTI = ISLTYP(I,J)                                                     PXPBL.566
C..... beta method, Lee & Pielke (JAM,May1992)                                   PXPBL.567
          IF(ISTI.NE.0.AND.ISNOW(I).NE.1.AND.WG(I).LE.WFC(ISTI)) THEN            PXPBL.568
            BETAP(I) = 0.25*(1.0 - COS(WG(I) / WFC(ISTI) * PI))**2               PXPBL.569
            HU = BETAP(I)+ (1.0 - BETAP(I)) * QVS(I,1) / QSS(I)                  PXPBL.570
          ENDIF                                                                  PXPBL.571
C*                                                                               PXPBL.572
          QFC(I) = HU * QSS(I)                                                   PXPBL.573
232     CONTINUE                                                                 PXPBL.574
C                                                                                PXPBL.575
C.......... compute the thetav at ground                                         PXPBL.576
        DO 233 I = 1,ILX                                                         PXPBL.577
          CVIRT=(1.0 +EP1*QFC(I))                                                PXPBL.578
          TV0 = TG(I) *CVIRT                                                     PXPBL.579
          CPOT= (1000.0 /PSURF(I))**ROVCP                                        PXPBL.580
          TH0(I) = TV0*CPOT                                                      PXPBL.581
          THETAG(I) = CPOT * TG(I)                                               PXPBL.582
          CPAIR(I) = 1004.67 * (1.0 + 0.84 * QVS(I,1))   ! J/(K KG)              PXPBL.583
C                                                                                PXPBL.584
233     CONTINUE                                                                 PXPBL.585
C                                                                                PXPBL.586
C..........  Calculate PBL height                                                PXPBL.587
C  COMPUTE PBL WHERE RICHARDSON NUMBER = RIC (0.25) HOLTSLAG ET AL 1990          PXPBL.588
        DO 243 I = 1,ILX                                                         PXPBL.589
          IF(MOL(I,J).LT.0.) THEN                                                PXPBL.590
            WSS= (UST(I,J)**3+0.6*WST(I,J)**3)**0.33333                          19DEC02.1805
            TCONV= -8.5*UST(I,J)*TST(I,J)/WSS                                    19DEC02.1806
            TH1=THETAV(I,1)+TCONV                                                PXPBL.593
           ELSE                                                                  PXPBL.594
            TH1=THETAV(I,1)                                                      PXPBL.595
          ENDIF                                                                  PXPBL.596
C                                                                                PXPBL.597
          TOG = THETAV(I,1)  / G                                                 PXPBL.598
          RIB(I,1) = 0.0                                                         PXPBL.599
          DO 241 K = 2,KL                                                        PXPBL.600
            DTMP   = THETAV(I,K) - TH1                                           PXPBL.601
            WSSQ = (US(I,K)-US(I,1))**2 + (VS(I,K)-VS(I,1))**2 +1.E-6            PXPBL.602
            RIB(I,K) = (Z(I,K) - Z(I,1)) * DTMP /(TOG * WSSQ)                    PXPBL.603
            IF (RIB(I,K) .GE. RIC) GO TO 242                                     PXPBL.604
241       CONTINUE                                                               PXPBL.605
          print *,' RIB never exceeds RIC, RIB(i,kl)=',rib(i,kl)                 PXPBL.606
     >           ,' psurf=',psurf(i),' thetav(i,1)=',thetav(i,1),                PXPBL.607
     >            ' TCONV=',TCONV,' TG(i)=',TG(i),' wst=',WST(I,J),              19DEC02.1807
     >       ' pbl=',pbl(i,j),' ust=',UST(I,J),' znt=', ZNT(I,J),                PXPBL.609
     >       ' tst=',TST(I,J),' u,v=',US(I,1),VS(I,1),' i,j=',I,J,               19DEC02.1808
     >       ' wg,w2=',WG(I),W2(I)                                               PXPBL.611
242       CONTINUE                                                               PXPBL.612
C         IF (K .EQ. 1) THEN                                                     PXPBL.613
C           KLPBL(I) = 1                                                         PXPBL.614
C           PBL(I,J) = ZF(I,1)                                                   PXPBL.615
C           PBLSIG(I) = SIGMAF(1)                                                PXPBL.616
C         ENDIF                                                                  PXPBL.617
          KPBLH(I) = K                                                           PXPBL.618
243     CONTINUE                                                                 PXPBL.619
C                                                                                PXPBL.620
        DO 245 I = 1,ILX                                                         PXPBL.621
C         IF (KPBLH(I) .NE. 1) THEN                                              PXPBL.622
C---------INTERPOLATE BETWEEN LEVELS -- jp 7/93                                  PXPBL.623
            FINT(I) = (RIC - RIB(I,KPBLH(I)-1)) / (RIB(I,KPBLH(I)) -             PXPBL.624
     &                 RIB(I,KPBLH(I)-1))                                        PXPBL.625
            IF (FINT(I) .GT. 0.5) THEN                                           PXPBL.626
              KPBLHT = KPBLH(I)                                                  PXPBL.627
              FINT(I) = FINT(I) - 0.5                                            PXPBL.628
             ELSE                                                                PXPBL.629
              KPBLHT = KPBLH(I)-1                                                PXPBL.630
              FINT(I) = FINT(I) + 0.5                                            PXPBL.631
            ENDIF                                                                PXPBL.632
            PBL(I,J) = FINT(I) * (ZF(I,KPBLHT) - ZF(I,KPBLHT-1)) +               PXPBL.633
     &                 ZF(I,KPBLHT-1)                                            PXPBL.634
            KLPBL(I) = KPBLHT                                                    PXPBL.635
            PBLSIG(I) = FINT(I) * DSIGH(KPBLHT)                                  PXPBL.636
     &                  + SIGMAF(KPBLHT-1)    ! sigma at PBL height              PXPBL.637
C         ENDIF                                                                  PXPBL.638
          if(klpbl(i).lt.1.or.klpbl(i).gt.kl) print *,' klpbl=',                 PXPBL.639
     >       KLPBL(I),' at i,j=',i,j,' pbl=',pbl(i,j)                            PXPBL.640
245     CONTINUE                                                                 PXPBL.641
C                                                                                PXPBL.642
C                                                                                PXPBL.643
        DO 2004 K = 1,KL                                                         PXPBL.644
          DO 2004 I = 1,ILX                                                      PXPBL.645
            VAPPRS = SVP1 * EXP(SVP2 * (TT(I,K) - SVPT0) /                       PXPBL.646
     &               (TT(I,K) - SVP3))                                           PXPBL.647
            QSBT   = EP2 * VAPPRS / (P(I,K) - VAPPRS)                            PXPBL.648
            RHX(I,K) = QVS(I,K) / QSBT                                           PXPBL.649
            RHX(I,K) = AMIN1(1.0,RHX(I,K))                                       PXPBL.650
2004    CONTINUE                                                                 PXPBL.651
C                                                                                PXPBL.652
C-- For surface radiation only                                                   PXPBL.653
      IF (IFRAD.LE.1) THEN                                                       PXPBL.654
        IF (IT. EQ. 1) THEN                                                      PXPBL.655
            CALL RADSFC(IYY,J,IN,TIMEG)                                          PXPBL.656
        ENDIF                                                                    PXPBL.657
      ELSE                                                                       PXPBL.658
        DO I=1,ILX                                                               PXPBL.659
          ALBEDO(I,J) = ALB(I,J)                                                 PXPBL.660
          RG(I) = GSW(I,J)/(1.0 - ALBEDO(I,J))                                   PXPBL.661
        ENDDO                                                                    PXPBL.662
      ENDIF                                                                      PXPBL.663
C                                                                                PXPBL.664
        IF (NUDGE.EQ.1) THEN    ! Switch for soil moisture nudging               PXPBL.665
C... no nudge for j<3 or J>jmax-1 since the obs. are not updated                 PXPBL.666
          IF (J .GT. 2 .AND. J .LT. JLX) THEN                                    PXPBL.667
C.........  For soil moisture assimilation, to get obs. T and RH                 PXPBL.668
            IF (XTIME .EQ. 0.0) THEN                                             PXPBL.669
              CORB = 1.0                                                         PXPBL.670
              CORE = 0.0                                                         PXPBL.671
            ELSE                                                                 PXPBL.672
              CURRTIME = XTIME - DTMIN + TIMEMIN                                 PXPBL.673
C....... Since all the surface data are achieved at same time interval,          PXPBL.674
C     we use the parameters (e.g. time,timb,ntb,nte) for u's FDDA to             PXPBL.675
C     calculate the corb and core. When the velocity assimilation and            PXPBL.676
C     temperature and RH assimilations are different, this has to be             PXPBL.677
C     changed!    AX 8/94                                                        PXPBL.678
C                                                                                PXPBL.679
              CORB = (TIME(1) - CURRTIME) / (TIME(1) - TIMB(1))                  PXPBL.680
              CORE = 1.0 - CORB                                                  PXPBL.681
            ENDIF                                                                PXPBL.682
            DO 2006 I = 1,ILX                                                    PXPBL.683
              RHOBS(I,J) = CORB * SFCOBS(NTB(1),6,I,J) +                         PXPBL.684
     &                   CORE * SFCOBS(NTE(1),6,I,J)                             PXPBL.685
C          QVOBS(I) = CORB * SFCOBS(NTB(1),4,I,J) +                              PXPBL.686
C                                                                                PXPBL.687
              TOBS = CORB * SFCOBS(NTB(1),3,I,J) +                               PXPBL.688
     &               CORE * SFCOBS(NTE(1),3,I,J)                                 PXPBL.689
              THOBS(I) = TOBS       !  try nudge with T                          PXPBL.690
2006        CONTINUE                                                             PXPBL.691
          ENDIF         ! endif for 2 < J < JLX                                  PXPBL.692
        ENDIF           ! endif for nudge=1                                      PXPBL.693
C                                                                                PXPBL.694
C---SUBROUTINES                                                                  PXPBL.695
        CALL SURFPX(PSURF,GSW,GLW,EMISS,PBL,MOL,ZNT,                             PXPBL.696
     &               UST,HFX,QFX,HOL,REGIME,XLAND,THOBS,                         PXPBL.697
     &               CAPG,PTOP,STBOLT,KARMAN,R,                                  PXPBL.698
     &               ILX,JLX,KL,J,DTPBL,NUDGE,TA2)                               19DEC02.1809
C-- EDYCOFN IS ALTERNATE KZ MODEL FROM RADM AND EPA'S CMAQ                       PXPBL.700
C         CALL EDYCOFN(PSB,PBL,MOL,UST,KZOGS,KARMAN,G,ILX,KL,KLM,J,DTPBL)        PXPBL.701
         CALL EDDYMM4(PSB,KZOGS,G,ILX,KL,KLM,J,DTPBL,IMOIST(IN),                 PXPBL.702
     >                IMVDIF,R)                                                  PXPBL.703
C                                                                                PXPBL.704
C...CHECK FOR FREE CONVECTION FOR ACM                                            PXPBL.705
        NOCONVI=0                                                                PXPBL.706
        IF (XTIME .NE. 0.0) THEN !don't call ACM at first time step              PXPBL.707
          DO 2001 I = 1,ILX                                                      PXPBL.708
            NOCONV(I) = 0                                                        PXPBL.709
            IF (THETAV(I,1) .GT. THETAV(I,2)) THEN                               PXPBL.710
              IF (PBL(I,J)/MOL(I,J) .LT. -3.0 .AND.KLPBL(I).GT.3)THEN            PXPBL.711
                NOCONV(I) = 1                                                    PXPBL.712
                NOCONVI=1                                                        PXPBL.713
                REGIME(I,J) = 4.0  ! FREE CONVECTIVE - ACM                       PXPBL.714
              ENDIF                                                              PXPBL.715
            ENDIF                                                                PXPBL.716
2001      CONTINUE                                                               PXPBL.717
                                                                                 PXPBL.718
        ENDIF                                                                    PXPBL.719
C                                                                                PXPBL.720
        IF(NOCONVI.EQ.1)THEN                                                     PXPBL.721
           CALL ACM(PSB,UST,KL,KLM,J,DTPBL,ILX,IMOIST(IN))                       PXPBL.722
         ELSE                                                                    PXPBL.723
           CALL VDIFF(PSB,UST,KL,KLM,J,DTPBL,ILX,IMOIST(IN))                     PXPBL.724
         ENDIF                                                                   PXPBL.725
C                                                                                PXPBL.726
        TIMEC = TIMEC + DTPBL  ! update the total time within time loop          PXPBL.727
        TIMEMIN=TIMEC/60.                                                        PXPBL.728
        TIMEG=TIMEGMT+TIMEMIN/60.                                                PXPBL.729
C                                                                                PXPBL.730
C-- COMPUTE NEW TV AND T                                                         PXPBL.731
        DO 251 K = 1,KL                                                          PXPBL.732
        DO 251 I = 1,ILX                                                         PXPBL.733
          THETAV(I,K) = THETA(I,K) * (1.0 + EP1 * QVS(I,K))                      PXPBL.734
          TT(I,K) = THETA(I,K) * (P(I,K) / 100.0) ** ROVCP                       PXPBL.735
          TV(I,K) = TT(I,K) * (1.0 + EP1 * QVS(I,K))                             PXPBL.736
251     CONTINUE                                                                 PXPBL.737
C                                                                                PXPBL.738
C..........  Save the water evaporated from ground                               PXPBL.739
          DO 252 I = 1,ILX                                                       PXPBL.740
            TEVA = TEVA + (EG(I) + ER(I) + ETR(I)) * DTPBL                       PXPBL.741
252       CONTINUE                                                               PXPBL.742
C                                                                                PXPBL.743
1111  CONTINUE      ! END TIME LOOP (IT)                                         PXPBL.744
C                                                                                PXPBL.745
C..........  Update variables after the minor time loop                          PXPBL.746
C......... KPBL is in the MM5 vertical structure and need to be updated          PXPBL.747
      DO 261 I = 1,ILX                                                           PXPBL.748
        KPBL(I) = KL - KLPBL(I) + 1                                              PXPBL.749
        SMCB(I,J,1) = SMCA(I,J,1)                                                PXPBL.750
        SMCB(I,J,2) = SMCA(I,J,2)                                                PXPBL.751
        CMCB(I,J)   = CMCA(I,J)                                                  PXPBL.752
        STCB(I,J,1) = STCA(I,J,1)                                                PXPBL.753
        STCB(I,J,2) = STCA(I,J,2)                                                PXPBL.754
        SMCA(I,J,1) = WG(I)                                                      PXPBL.755
        SMCA(I,J,2) = W2(I)                                                      PXPBL.756
        CMCA(I,J)   = WR(I)                                                      PXPBL.757
        STCA(I,J,2) = T2(I)                                                      PXPBL.758
261   CONTINUE                                                                   PXPBL.759
C                                                                                PXPBL.760
C...........  save the total water evaporated from the grid cells                PXPBL.761
C  Note: TEVA accumulates for 2 time steps because of the leapfrog               PXPBL.762
C        scheme used so it should be cut to half (*0.5)                          PXPBL.763
C                                                                                PXPBL.764
      PARJSUM(ITQEVA_SUM,J) = PARJSUM(ITQEVA_SUM,J) + TEVA*DX*DX*0.5             PXPBL.765
      TEVA = 0.0                                                                 PXPBL.766
C                                                                                PXPBL.767
C-----COMPUTE THE TENDENCIES FROM THE SAVED INITIAL VALUES:                      PXPBL.768
C                                                                                PXPBL.769
      DO 281 K = 1,KL                                                            PXPBL.770
        KK = KL - K +1                                                           PXPBL.771
        DO 280 I = 1,ILX                                                         PXPBL.772
          UTNP(I,K) = (US(I,KK) - UXS(I,KK)) / DT                                PXPBL.773
          VTNP(I,K) = (VS(I,KK) - VXS(I,KK)) / DT                                PXPBL.774
          TTNP(I,K) = (THETA(I,KK) - THXS(I,KK)) / DT                            PXPBL.775
280     CONTINUE                                                                 PXPBL.776
281   CONTINUE                                                                   PXPBL.777
C                                                                                PXPBL.778
      IF (IDRY(IN) .EQ. 0) THEN          ! not dry case                          PXPBL.779
        DO 291 K = 1,KL                                                          PXPBL.780
        KK = KL - K +1                                                           PXPBL.781
        DO 291 I = 1,ILX                                                         PXPBL.782
          QTNP(I,K) = (QVS(I,KK) - QXS(I,KK)) / DT                               PXPBL.783
291     CONTINUE                                                                 PXPBL.784
C                                                                                PXPBL.785
        IF (IMOIST(IN) .NE. 1 .AND. IMOIST(IN) .NE. 3) THEN                      PXPBL.786
          DO 301 K = 1,KL                                                        PXPBL.787
            KK = KL - K +1                                                       PXPBL.788
            DO 300 I = 1,ILX                                                     PXPBL.789
              QCTNP(I,K) = (QCS(I,KK) - QCXS(I,KK)) / DT                         PXPBL.790
              QITNP(I,K) = (QIS(I,KK) - QIXS(I,KK)) / DT                         PXPBL.791
              IF(IICEG.EQ.1) QNCNP(I,K)=(QNCS(I,KK)-QNCXS(I,KK))/DT              PXPBL.792
300         CONTINUE                                                             PXPBL.793
301       CONTINUE                                                               PXPBL.794
        ENDIF                                                                    PXPBL.795
      ENDIF                                                                      PXPBL.796
C                                                                                PXPBL.797
C                                                                                PXPBL.798
C-----CONVERT ALL THE TENDENCIES TO FLUX FORM:                                   PXPBL.799
C                                                                                PXPBL.800
      DO 311 K = 1,KL                                                            PXPBL.801
        DO 310 I = 1,ILX                                                         PXPBL.802
          UTNP(I,K) = PSB(I,J) * UTNP(I,K)                                       PXPBL.803
          VTNP(I,K) = PSB(I,J) * VTNP(I,K)                                       PXPBL.804
          TTNP(I,K) = PSB(I,J) * TTNP(I,K)                                       PXPBL.805
310     CONTINUE                                                                 PXPBL.806
311   CONTINUE                                                                   PXPBL.807
C                                                                                PXPBL.808
      IF (IDRY(IN) .EQ. 0) THEN                                                  PXPBL.809
        DO 321 K = 1,KL                                                          PXPBL.810
          DO 320 I = 1,ILX                                                       PXPBL.811
            QTNP(I,K) = PSB(I,J) * QTNP(I,K)                                     PXPBL.812
320       CONTINUE                                                               PXPBL.813
321     CONTINUE                                                                 PXPBL.814
C                                                                                PXPBL.815
        IF (IMOIST(IN) .NE. 1 .AND. IMOIST(IN) .NE. 3) THEN                      PXPBL.816
          DO 331 K = 1,KL                                                        PXPBL.817
            DO 330 I = 1,ILX                                                     PXPBL.818
              QCTNP(I,K) = PSB(I,J) * QCTNP(I,K)                                 PXPBL.819
              QITNP(I,K) = PSB(I,J) * QITNP(I,K)                                 PXPBL.820
              IF (IICEG .EQ. 1) QNCNP(I,K)=PSB(I,J)*QNCNP(I,K)                   PXPBL.821
330         CONTINUE                                                             PXPBL.822
331       CONTINUE                                                               PXPBL.823
        ENDIF                                                                    PXPBL.824
      ENDIF                                                                      PXPBL.825
C                                                                                PXPBL.826
C                                                                                PXPBL.827
C-----CALCULATION OF NEW VALUES DUE TO VERTICAL EXCHANGE PROCESSES IS            PXPBL.828
C     COMPLETED. THE FINAL STEP IS TO ADD THE TENDENCIES CALCULATED              PXPBL.829
C     IN PXPBL TO THOSE OF MM5.                                                  PXPBL.830
C                                                                                PXPBL.831
      DO 341 K = 1,KL                                                            PXPBL.832
        DO 340 I = 1,ILX                                                         PXPBL.833
          U3DTEN(I,J,K) = UTNP(I,K)                                              PXPBL.834
          V3DTEN(I,J,K) = VTNP(I,K)                                              PXPBL.835
340     CONTINUE                                                                 PXPBL.836
341   CONTINUE                                                                   PXPBL.837
C                                                                                PXPBL.838
      JBGN=3                                                                     PXPBL.839
      JEND=JLXM-1                                                                PXPBL.840
      IF (IN .EQ. 1) THEN                                                        PXPBL.841
        JBGN = 2                                                                 PXPBL.842
        JEND = JLXM                                                              PXPBL.843
      ENDIF                                                                      PXPBL.844
      IF (J .GE. JBGN .AND. J .LE. JEND) THEN                                    PXPBL.845
      IBGN=3                                                                     PXPBL.846
      IEND=ILXM-1                                                                PXPBL.847
      IF (IN .EQ. 1)THEN                                                         PXPBL.848
        IBGN=2                                                                   PXPBL.849
        IEND=ILXM                                                                PXPBL.850
      ENDIF                                                                      PXPBL.851
C                                                                                PXPBL.852
        DO 351 K = 1,KL                                                          PXPBL.853
          DO 350 I = IBGN,IEND                                                   PXPBL.854
C...TTNP is the tendency for potential temperature                               PXPBL.855
C                                                                                PXPBL.856
C          TTBL(I,J,K)= TTNP(I,K) / THCON(I,K) ! for Grell scheme               PXPBL.857
           T3DTEN(I,J,K) = T3DTEN(I,J,K) + TTNP(I,K) / THCON(I,K)                PXPBL.858
350       CONTINUE                                                               PXPBL.859
351     CONTINUE                                                                 PXPBL.860
C                                                                                PXPBL.861
        IF (IDRY(IN) .EQ. 0) THEN                                                PXPBL.862
          DO 361 K = 1,KL                                                        PXPBL.863
            DO 360 I = IBGN,IEND                                                 PXPBL.864
C             QTBL(I,J,K)= QTNP(I,K)   ! for Grell scheme                       PXPBL.865
              QV3DTEN(I,J,K) = QV3DTEN(I,J,K) + QTNP(I,K)                        PXPBL.866
360         CONTINUE                                                             PXPBL.867
361       CONTINUE                                                               PXPBL.868
C                                                                                PXPBL.869
          IF (IMOIST(IN) .NE. 1 .AND. IMOIST(IN) .NE. 3) THEN                    PXPBL.870
            DO 371 K = 1,KL                                                      PXPBL.871
              DO 370 I = IBGN,IEND                                               PXPBL.872
                QC3DTEN(I,J,K) = QC3DTEN(I,J,K) + QCTNP(I,K)                     PXPBL.873
                IF (IICE .EQ. 1)                                                 PXPBL.874
     &            QI3DTEN(I,J,K) = QI3DTEN(I,J,K) + QITNP(I,K)                   PXPBL.875
                IF(IICEG.EQ.1)                                                   PXPBL.876
     &            QNC3DTEN(I,J,K) = QNC3DTEN(I,J,K) + QNCNP(I,K)                 PXPBL.877
370           CONTINUE                                                           PXPBL.878
371         CONTINUE                                                             PXPBL.879
          ENDIF ! IMOIST                                                         PXPBL.880
        ENDIF   ! IDRY                                                           PXPBL.881
       ENDIF    ! J LIMITS                                                       PXPBL.882
C                                                                                PXPBL.883
C-----APPLY AE TIME STEP:                                                        PXPBL.884
C                                                                                PXPBL.885
C      IF (ITGFLG(IN) .EQ. 1) THEN                                               PXPBL.886
C        DO 391 I = 1,ILX                                                        PXPBL.887
C          TGB(I,J) = OMUHF * TGA(I,J) + GNUHF * (TGDSA(I) + TGDSB(I))           PXPBL.888
C          TGA(I,J) = TGDSA(I)                                                   PXPBL.889
C391     CONTINUE                                                                PXPBL.890
C      ENDIF                                                                     PXPBL.891
      DO 391 I =1,ILX                                                            PXPBL.892
        TGB(I,J) = TGA(I,J)                                                      PXPBL.893
        TGA(I,J) = TG(I)                                                         PXPBL.894
        STCA(I,J,1) = TG(I)     ! FOR OUTPUT                                     PXPBL.895
391   CONTINUE                                                                   PXPBL.896
C                                                                                PXPBL.897
C-----OUTPUT BOUNDARY LAYER VARIABLES, IF PBLCHK=.TRUE..                         PXPBL.898
C                                                                                PXPBL.899
      IF (IN .EQ. 1) THEN                                                        PXPBL.900
        XXT=XTIME+DTMIN                                                          PXPBL.901
       ELSE                                                                      PXPBL.902
        XXT=XTIME+FLOAT(IRAX)*DTMIN                                              PXPBL.903
      ENDIF                                                                      PXPBL.904
C                                                                                PXPBL.905
C     KPBL IS NEEDED FOR THE FDDA, AND SINCE THERE IS NO LONGER JUST             PXPBL.906
C     ONE LARGE J-LOOP IT MUST BE STORED AS (I,J)                                PXPBL.907
C                                                                                PXPBL.908
      DO 950 I=1,ILX                                                             PXPBL.909
       IF(REGIME(I,J).GT.3.5) THEN                                               PXPBL.910
         KPBL2D(I,J)=KPBL(I)                                                     PXPBL.911
       ELSE                                                                      PXPBL.912
         KPBL2D(I,J)= KL                                                         PXPBL.913
       ENDIF                                                                     PXPBL.914
950   CONTINUE                                                                   PXPBL.915
C                                                                                PXPBL.916
      RETURN                                                                     PXPBL.917
      END                                                                        PXPBL.918
