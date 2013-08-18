      SUBROUTINE SFLX (ICE,DT,ZLVL,NSOIL,SLDPTH,                        &    !C  SFLX.1
     &                 SATFLG,LCH, OFFLINE,                             &    !L  SFLX.2
     &                 LLANDUSE, LSOIL,                                 &    !CL SFLX.3
     &                 LWDN,SOLDN,SFCPRS,PRCP,SFCTMP,Q2,SFCSPD,         &    !F  SFLX.4
     &                 COSZ,PRCPRAIN, SOLARDIRECT,                      &    !F  SFLX.5
     &                 TH2,Q2SAT,DQSDT2,                                &    !I  SFLX.6
     &                 VEGTYP,SOILTYP,SLOPETYP,SHDFAC,SHDMIN,SHDMAX,    &    !I  SFLX.7
     &                 ALB, SNOALB,TBOT, Z0BRD, Z0,EMISSI,              &    !S  23DEC04.3653
     &                 CMC,T1,STC,SMC,SH2O,SNOWH,SNEQV,ALBEDO,CH,CM,    &    !H  SFLX.9
! ----------------------------------------------------------------------         SFLX.10
! OUTPUTS, DIAGNOSTICS, PARAMETERS BELOW GENERALLY NOT NECESSARY WHEN            SFLX.11
! COUPLED WITH E.G. A NWP MODEL (SUCH AS THE NOAA/NWS/NCEP MESOSCALE ETA         SFLX.12
! MODEL).  OTHER APPLICATIONS MAY REQUIRE DIFFERENT OUTPUT VARIABLES.            SFLX.13
! ----------------------------------------------------------------------         SFLX.14
     &                 ETA,SHEAT, ETA_KINEMATIC,FDOWN,                  &    !O  SFLX.15
     &                 EC,EDIR,ET,ETT,ESNOW,DRIP,DEW,                   &    !O  SFLX.16
     &                 BETA,ETP,SSOIL,                                  &    !O  SFLX.17
     &                 FLX1,FLX2,FLX3,                                  &    !O  SFLX.18
     &                 SNOMLT,SNCOVR,                                   &    !O  SFLX.19
     &                 RUNOFF1,RUNOFF2,RUNOFF3,                         &    !O  SFLX.20
     &                 RC,PC,RSMIN,XLAI,RCS,RCT,RCQ,RCSOIL,             &    !O  SFLX.21
     &                 SOILW,SOILM,Q1,                                  &    !D  SFLX.22
     &                 SMCWLT,SMCDRY,SMCREF,SMCMAX,NROOT)                    !P  SFLX.23
                                                                                 SFLX.24
!                                                                                23DEC04.3654
! ----------------------------------------------------------------------         SFLX.25
! SUBROUTINE SFLX - VERSION 3.X - October 2002                                   SFLX.26
! ----------------------------------------------------------------------         SFLX.27
! SUB-DRIVER FOR "Noah LSM" FAMILY OF PHYSICS SUBROUTINES FOR A                  SFLX.28
! SOIL/VEG/SNOWPACK LAND-SURFACE MODEL TO UPDATE SOIL MOISTURE, SOIL             SFLX.29
! ICE, SOIL TEMPERATURE, SKIN TEMPERATURE, SNOWPACK WATER CONTENT,               SFLX.30
! SNOWDEPTH, AND ALL TERMS OF THE SURFACE ENERGY BALANCE AND SURFACE             SFLX.31
! WATER BALANCE (EXCLUDING INPUT ATMOSPHERIC FORCINGS OF DOWNWARD                SFLX.32
! RADIATION AND PRECIP)                                                          SFLX.33
! ----------------------------------------------------------------------         SFLX.34
! SFLX ARGUMENT LIST KEY:                                                        SFLX.35
! ----------------------------------------------------------------------         SFLX.36
!  C  CONFIGURATION INFORMATION                                                  SFLX.37
!  L  LOGICAL                                                                    SFLX.38
! CL  4-string character bearing logical meaning                                 SFLX.39
!  F  FORCING DATA                                                               SFLX.40
!  I  OTHER (INPUT) FORCING DATA                                                 SFLX.41
!  S  SURFACE CHARACTERISTICS                                                    SFLX.42
!  H  HISTORY (STATE) VARIABLES                                                  SFLX.43
!  O  OUTPUT VARIABLES                                                           SFLX.44
!  D  DIAGNOSTIC OUTPUT                                                          SFLX.45
!  P  Parameters                                                                 SFLX.46
! ----------------------------------------------------------------------         SFLX.47
! 1. CONFIGURATION INFORMATION (C):                                              SFLX.48
! ----------------------------------------------------------------------         SFLX.49
!   ICE        SEA-ICE FLAG  (=1: SEA-ICE, =0: LAND)                             23DEC04.3655
!   DT         TIMESTEP (SEC) (DT SHOULD NOT EXCEED 3600 SECS, RECOMMEND         23DEC04.3656
!                1800 SECS OR LESS)                                              SFLX.52
!   ZLVL       HEIGHT (M) ABOVE GROUND OF ATMOSPHERIC FORCING VARIABLES          SFLX.53
!   NSOIL      NUMBER OF SOIL LAYERS (AT LEAST 2, AND NOT GREATER THAN           SFLX.54
!                PARAMETER NSOLD SET BELOW)                                      SFLX.55
!   SLDPTH     THE THICKNESS OF EACH SOIL LAYER (M)                              SFLX.56
! ----------------------------------------------------------------------         SFLX.57
! 2. LOGICAL:                                                                    SFLX.58
! ----------------------------------------------------------------------         SFLX.59
!   LCH       Exchange coefficient (Ch) calculation flag (false: using           SFLX.60
!                ch-routine SFCDIF; true: Ch is brought in)                      SFLX.61
!   SATFLG    Indicating saturation at the lowest model level                    SFLX.62
!             (true: saturated, then set ETP =0.0)                               SFLX.63
!   OFFLINE   Offline simulation flag (true: offline version                     SFLX.64
!                 1) all LSM parameters will be defined by three tables          SFLX.65
!   LLANDUSE  (=USGS, using USGS landuse classification)                         SFLX.66
!   LSOIL     (=STAS, using FAO/STATSGO soil texture classification)             SFLX.67
! ----------------------------------------------------------------------         SFLX.68
! 3. FORCING DATA (F):                                                           SFLX.69
! ----------------------------------------------------------------------         SFLX.70
!   LWDN       LW DOWNWARD RADIATION (W M-2; POSITIVE, NOT NET LONGWAVE)         SFLX.71
!   SOLDN      SOLAR DOWNWARD RADIATION (W M-2; POSITIVE, NOT NET SOLAR)         SFLX.72
!   SFCPRS     PRESSURE AT HEIGHT ZLVL ABOVE GROUND (PASCALS)                    SFLX.73
!   PRCP       PRECIP RATE (KG M-2 S-1) (NOTE, THIS IS A RATE)                   SFLX.74
!   SFCTMP     AIR TEMPERATURE (K) AT HEIGHT ZLVL ABOVE GROUND                   SFLX.75
!   TH2        AIR POTENTIAL TEMPERATURE (K) AT HEIGHT ZLVL ABOVE GROUND         SFLX.76
!   Q2         MIXING RATIO AT HEIGHT ZLVL ABOVE GROUND (KG KG-1)                SFLX.77
!   COSZ       Solar zenith angle (not used for now)                             SFLX.78
!   PRCPRAIN   Liquid-precipitation rate (KG M-2 S-1) (not used)                 SFLX.79
! SOLARDIRECT  Direct component of downward solar radiation (W M-2) (not used)   SFLX.80
! ----------------------------------------------------------------------         SFLX.81
! 4. OTHER FORCING (INPUT) DATA (I):                                             SFLX.82
! ----------------------------------------------------------------------         SFLX.83
!   SFCSPD     WIND SPEED (M S-1) AT HEIGHT ZLVL ABOVE GROUND                    SFLX.84
!   Q2SAT      SAT MIXING RATIO AT HEIGHT ZLVL ABOVE GROUND (KG KG-1)            SFLX.85
!   DQSDT2     SLOPE OF SAT SPECIFIC HUMIDITY CURVE AT T=SFCTMP                  SFLX.86
!                (KG KG-1 K-1)                                                   SFLX.87
! ----------------------------------------------------------------------         SFLX.88
! 5. CANOPY/SOIL CHARACTERISTICS (S):                                            SFLX.89
! ----------------------------------------------------------------------         SFLX.90
!   VEGTYP     VEGETATION TYPE (INTEGER INDEX)                                   SFLX.91
!   SOILTYP    SOIL TYPE (INTEGER INDEX)                                         SFLX.92
!   SLOPETYP   CLASS OF SFC SLOPE (INTEGER INDEX)                                SFLX.93
!   SHDFAC     AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION                     SFLX.94
!                (FRACTION= 0.0-1.0)                                             SFLX.95
!   SHDMIN     MINIMUM AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION             SFLX.96
!                (FRACTION= 0.0-1.0) <= SHDFAC                                   SFLX.97
!   PTU        PHOTO THERMAL UNIT (PLANT PHENOLOGY FOR ANNUALS/CROPS)            SFLX.98
!                (NOT YET USED, BUT PASSED TO REDPRM FOR FUTURE USE IN           SFLX.99
!                VEG PARMS)                                                      SFLX.100
!   ALB        BACKROUND SNOW-FREE SURFACE ALBEDO (FRACTION), FOR JULIAN         SFLX.101
!                DAY OF YEAR (USUALLY FROM TEMPORAL INTERPOLATION OF             SFLX.102
!                MONTHLY MEAN VALUES' CALLING PROG MAY OR MAY NOT                SFLX.103
!                INCLUDE DIURNAL SUN ANGLE EFFECT)                               SFLX.104
!   SNOALB     UPPER BOUND ON MAXIMUM ALBEDO OVER DEEP SNOW (E.G. FROM           SFLX.105
!                ROBINSON AND KUKLA, 1985, J. CLIM. & APPL. METEOR.)             SFLX.106
!   TBOT       BOTTOM SOIL TEMPERATURE (LOCAL YEARLY-MEAN SFC AIR                SFLX.107
!                TEMPERATURE)                                                    SFLX.108
!   Z0BRD      Background fixed roughness length (M)                             SFLX.109
!   Z0         Time varying roughness length (M) as function of snow depth       SFLX.110
!   EMISSI     surface emissivity  (dimentionless 0-1.0)                         23DEC04.3657
! ----------------------------------------------------------------------         SFLX.111
! 6. HISTORY (STATE) VARIABLES (H):                                              SFLX.112
! ----------------------------------------------------------------------         SFLX.113
!  CMC         CANOPY MOISTURE CONTENT (M)                                       SFLX.114
!  T1          GROUND/CANOPY/SNOWPACK) EFFECTIVE SKIN TEMPERATURE (K)            SFLX.115
!  STC(NSOIL)  SOIL TEMP (K)                                                     SFLX.116
!  SMC(NSOIL)  TOTAL SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)                 SFLX.117
!  SH2O(NSOIL) UNFROZEN SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)              SFLX.118
!                NOTE: FROZEN SOIL MOISTURE = SMC - SH2O                         SFLX.119
!  SNOWH       ACTUAL SNOW DEPTH (M)                                             SFLX.120
!  SNEQV       LIQUID WATER-EQUIVALENT SNOW DEPTH (M)                            SFLX.121
!                NOTE: SNOW DENSITY = SNEQV/SNOWH                                SFLX.122
!  ALBEDO      SURFACE ALBEDO INCLUDING SNOW EFFECT (UNITLESS FRACTION)          SFLX.123
!                =SNOW-FREE ALBEDO (ALB) WHEN SNEQV=0, OR                        SFLX.124
!                =FCT(MSNOALB,ALB,VEGTYP,SHDFAC,SHDMIN) WHEN SNEQV>0             SFLX.125
!  CH          SURFACE EXCHANGE COEFFICIENT FOR HEAT AND MOISTURE                SFLX.126
!                (M S-1); NOTE: CH IS TECHNICALLY A CONDUCTANCE SINCE            SFLX.127
!                IT HAS BEEN MULTIPLIED BY WIND SPEED.                           SFLX.128
!  CM          SURFACE EXCHANGE COEFFICIENT FOR MOMENTUM (M S-1); NOTE:          SFLX.129
!                CM IS TECHNICALLY A CONDUCTANCE SINCE IT HAS BEEN               SFLX.130
!                MULTIPLIED BY WIND SPEED.                                       SFLX.131
! ----------------------------------------------------------------------         SFLX.132
! 7. OUTPUT (O):                                                                 SFLX.133
! ----------------------------------------------------------------------         SFLX.134
! OUTPUT VARIABLES NECESSARY FOR A COUPLED NUMERICAL WEATHER PREDICTION          SFLX.135
! MODEL, E.G. NOAA/NWS/NCEP MESOSCALE ETA MODEL.  FOR THIS APPLICATION,          SFLX.136
! THE REMAINING OUTPUT/DIAGNOSTIC/PARAMETER BLOCKS BELOW ARE NOT                 SFLX.137
! NECESSARY.  OTHER APPLICATIONS MAY REQUIRE DIFFERENT OUTPUT VARIABLES.         SFLX.138
!   ETA        ACTUAL LATENT HEAT FLUX (W m-2: NEGATIVE, IF UP FROM              SFLX.139
!              SURFACE)                                                          23DEC04.3658
!  ETA_KINEMATIC atctual latent heat flux in Kg m s-1                            SFLX.141
!   SHEAT      SENSIBLE HEAT FLUX (W M-2: NEGATIVE, IF UPWARD FROM               SFLX.142
!              SURFACE)                                                          23DEC04.3659
!   FDOWN      Radiation forcing at the surface (W m-2) = SOLDN*(1-alb)+LWDN     SFLX.144
! ----------------------------------------------------------------------         SFLX.145
!   EC         CANOPY WATER EVAPORATION (M S-1)                                  SFLX.146
!   EDIR       DIRECT SOIL EVAPORATION (M S-1)                                   SFLX.147
!   ET(NSOIL)  PLANT TRANSPIRATION FROM A PARTICULAR ROOT (SOIL) LAYER           SFLX.148
!                 (M S-1)                                                        SFLX.149
!   ETT        TOTAL PLANT TRANSPIRATION (M S-1)                                 SFLX.150
!   ESNOW      SUBLIMATION FROM (OR DEPOSITION TO IF <0) SNOWPACK                SFLX.151
!                (M S-1)                                                         SFLX.152
!   DRIP       THROUGH-FALL OF PRECIP AND/OR DEW IN EXCESS OF CANOPY             SFLX.153
!                WATER-HOLDING CAPACITY (M)                                      SFLX.154
!   DEW        DEWFALL (OR FROSTFALL FOR T<273.15) (M)                           SFLX.155
! ----------------------------------------------------------------------         SFLX.156
!   BETA       RATIO OF ACTUAL/POTENTIAL EVAP (DIMENSIONLESS)                    SFLX.157
!   ETP        POTENTIAL EVAPORATION (KG M-2 S-1)                                SFLX.158
!   SSOIL      SOIL HEAT FLUX (W M-2: NEGATIVE IF DOWNWARD FROM SURFACE)         SFLX.159
! ----------------------------------------------------------------------         SFLX.160
!   FLX1       PRECIP-SNOW SFC (W M-2)                                           SFLX.161
!   FLX2       FREEZING RAIN LATENT HEAT FLUX (W M-2)                            SFLX.162
!   FLX3       PHASE-CHANGE HEAT FLUX FROM SNOWMELT (W M-2)                      SFLX.163
! ----------------------------------------------------------------------         SFLX.164
!   SNOMLT     SNOW MELT (M) (WATER EQUIVALENT)                                  SFLX.165
!   SNCOVR     FRACTIONAL SNOW COVER (UNITLESS FRACTION, 0-1)                    SFLX.166
! ----------------------------------------------------------------------         SFLX.167
!   RUNOFF1    SURFACE RUNOFF (M S-1), NOT INFILTRATING THE SURFACE              SFLX.168
!   RUNOFF2    SUBSURFACE RUNOFF (M S-1), DRAINAGE OUT BOTTOM OF LAST            SFLX.169
!                SOIL LAYER (BASEFLOW)                                           SFLX.170
!   RUNOFF3    NUMERICAL TRUNCTATION IN EXCESS OF POROSITY (SMCMAX)              SFLX.171
!                FOR A GIVEN SOIL LAYER AT THE END OF A TIME STEP (M S-1).       SFLX.172
! Note: the above RUNOFF2 is actually the sum of RUNOFF2 and RUNOFF3             SFLX.173
! ----------------------------------------------------------------------         SFLX.174
!   RC         CANOPY RESISTANCE (S M-1)                                         SFLX.175
!   PC         PLANT COEFFICIENT (UNITLESS FRACTION, 0-1) WHERE PC*ETP           SFLX.176
!                = ACTUAL TRANSP                                                 SFLX.177
!   XLAI       LEAF AREA INDEX (DIMENSIONLESS)                                   SFLX.178
!   RSMIN      MINIMUM CANOPY RESISTANCE (S M-1)                                 SFLX.179
!   RCS        INCOMING SOLAR RC FACTOR (DIMENSIONLESS)                          SFLX.180
!   RCT        AIR TEMPERATURE RC FACTOR (DIMENSIONLESS)                         SFLX.181
!   RCQ        ATMOS VAPOR PRESSURE DEFICIT RC FACTOR (DIMENSIONLESS)            SFLX.182
!   RCSOIL     SOIL MOISTURE RC FACTOR (DIMENSIONLESS)                           SFLX.183
! ----------------------------------------------------------------------         SFLX.184
! 8. DIAGNOSTIC OUTPUT (D):                                                      SFLX.185
! ----------------------------------------------------------------------         SFLX.186
!   SOILW      AVAILABLE SOIL MOISTURE IN ROOT ZONE (UNITLESS FRACTION           SFLX.187
!              BETWEEN SMCWLT AND SMCMAX)                                        23DEC04.3660
!   SOILM      TOTAL SOIL COLUMN MOISTURE CONTENT (FROZEN+UNFROZEN) (M)          SFLX.189
!   Q1         Effective mixing ratio at surface (kg kg-1), used for             SFLX.190
!              diagnosing the mixing ratio at 2 meter for coupled model          SFLX.191
! ----------------------------------------------------------------------         SFLX.192
! 9. PARAMETERS (P):                                                             SFLX.193
! ----------------------------------------------------------------------         SFLX.194
!   SMCWLT     WILTING POINT (VOLUMETRIC)                                        SFLX.195
!   SMCDRY     DRY SOIL MOISTURE THRESHOLD WHERE DIRECT EVAP FRM TOP             SFLX.196
!                LAYER ENDS (VOLUMETRIC)                                         SFLX.197
!   SMCREF     SOIL MOISTURE THRESHOLD WHERE TRANSPIRATION BEGINS TO             SFLX.198
!                STRESS (VOLUMETRIC)                                             SFLX.199
!   SMCMAX     POROSITY, I.E. SATURATED VALUE OF SOIL MOISTURE                   SFLX.200
!                (VOLUMETRIC)                                                    SFLX.201
!   NROOT      NUMBER OF ROOT LAYERS, A FUNCTION OF VEG TYPE, DETERMINED         SFLX.202
!              IN SUBROUTINE REDPRM.                                             SFLX.203
! ----------------------------------------------------------------------         SFLX.204
      IMPLICIT NONE                                                              SFLX.205
#     include <paramgen_LSM.incl>                                                SFLX.206
      INTEGER NSOLD,iout                                                         SFLX.207
      PARAMETER (NSOLD = 20)                                                     SFLX.208
!      CHARACTER*4, INTENT(IN)  LLANDUSE, LSOIL                                  SFLX.209
      CHARACTER*4  LLANDUSE, LSOIL                                               SFLX.210
                                                                                 SFLX.211
! DECLARATIONS - LOGICAL                                                         SFLX.212
! ----------------------------------------------------------------------         SFLX.213
!      LOGICAL, INTENT(IN)  SATFLG, OFFLINE, LCH                                 SFLX.214
      LOGICAL  SATFLG, OFFLINE, LCH                                              SFLX.215
      LOGICAL  FRZGRA, SNOWNG                                                    SFLX.216
                                                                                 SFLX.217
! ----------------------------------------------------------------------         SFLX.218
! DECLARATIONS - INTEGER                                                         SFLX.219
! ----------------------------------------------------------------------         SFLX.220
!      INTEGER, INTENT(IN)  ICE,NSOIL,SLOPETYP,SOILTYP,VEGTYP                    SFLX.221
!      INTEGER, INTENT(OUT)  NROOT                                               SFLX.222
      INTEGER  ICE,NSOIL,SLOPETYP,SOILTYP,VEGTYP                                 SFLX.223
      INTEGER  NROOT                                                             SFLX.224
      INTEGER  KZ ,K                                                             SFLX.225
                                                                                 SFLX.226
! ----------------------------------------------------------------------         SFLX.227
! DECLARATIONS - REAL                                                            SFLX.228
! ----------------------------------------------------------------------         SFLX.229
!      REAL, INTENT(IN)   ALB,SHDMIN,SHDMAX,DT,DQSDT2,LWDN,PRCP,PRCPRAIN,        SFLX.230
!                           Q2,Q2SAT,SFCPRS,SFCSPD,SFCTMP,SHDFAC, SNOALB,        SFLX.231
      REAL               ALB,SHDMIN,SHDMAX,DT,DQSDT2,LWDN,PRCP,PRCPRAIN,       & SFLX.232
     &                     Q2,Q2SAT,SFCPRS,SFCSPD,SFCTMP,SHDFAC, SNOALB,       & SFLX.233
     &                     SOLDN,TBOT,TH2,ZLVL,Z0BRD,EMISSI                      23DEC04.3661
!      REAL, INTENT(INOUT)   COSZ, SOLARDIRECT,ALBEDO,CH,CM,                     SFLX.235
!                              CMC,SNEQV,SNCOVR,SNOWH,T1,XLAI                    SFLX.236
      REAL  COSZ, SOLARDIRECT,ALBEDO,CH,CM,                                      SFLX.237
     &                        CMC,SNEQV,SNCOVR,SNOWH,T1,XLAI                     SFLX.238
!      REAL, DIMENSION(1:NSOIL), INTENT(IN)                          SLDPTH      SFLX.239
!      REAL, DIMENSION(1:NSOIL), INTENT(OUT)                         ET          SFLX.240
!      REAL, DIMENSION(1:NSOIL), INTENT(INOUT)                       SH2O,       SFLX.241
      REAL                          SLDPTH(NSOIL)                                SFLX.242
      REAL                         ET  (NSOIL)                                   SFLX.243
      REAL                      SH2O (NSOIL),                                  & SFLX.244
     &                          SMC (NSOIL),                                   & SFLX.245
     &                          STC (NSOIL)                                      SFLX.246
      REAL                      RTDIS(NSOLD),                                  & SFLX.247
     &                          ZSOIL(NSOLD)                                     SFLX.248
!      REAL, INTENT(OUT)   ETA_KINEMATIC,BETA,DEW,DRIP,EC,EDIR,ESNOW,ETA,     &  SFLX.249
      REAL                ETA_KINEMATIC,BETA,DEW,DRIP,EC,EDIR,ESNOW,ETA,       & SFLX.250
     &                      ETP,FLX1,FLX2,FLX3,SHEAT,PC,RUNOFF1,RUNOFF2,       & SFLX.251
     &                      RUNOFF3,RC,RSMIN,RCQ,RCS,RCSOIL,RCT,SSOIL,         & SFLX.252
     &                      SMCDRY,SMCMAX,SMCREF,SMCWLT,SNOMLT, SOILM,         & SFLX.253
     &                      SOILW,FDOWN,Q1                                       SFLX.254
      REAL  BEXP,CFACTR,CMCMAX,CP,CSOIL,CZIL,DF1,DF1H,DF1A,DKSAT,DWSAT,        & SFLX.255
     &        DSOIL,DTOT,ETT,FRCSNO,FRCSOI,EPSCA,F1,FXEXP,FRZX,HS,             & SFLX.256
     &        KDT,LVH2O,PRCP1,PSISAT,QUARTZ,R,RCH,REFKDT,RR,RGL,               & SFLX.257
     &        RSMAX,                                                           & SFLX.258
     &        RSNOW,SNDENS,SNCOND,SBETA,SN_NEW,SLOPE,SNUP,SALP,SOILWM,         & SFLX.259
     &        SOILWW,T1V,T24,T2V,TH2V,TOPT,TFREEZ,TSNOW,ZBOT,Z0,PRCPF,         & SFLX.260
     &        ETNS                                                               SFLX.261
! ----------------------------------------------------------------------         SFLX.262
! DECLARATIONS - PARAMETERS                                                      SFLX.263
! ----------------------------------------------------------------------         SFLX.264
      REAL     LSUBS                                                             SFLX.265
      PARAMETER (TFREEZ = 273.15)                                                SFLX.266
!ek                                                                              SFLX.267
      PARAMETER (LVH2O = 2.501E+6)                                               SFLX.268
      PARAMETER (LSUBS = 2.83E+6)                                                SFLX.269
      PARAMETER (R = 287.04)                                                     SFLX.270
                                                                                 SFLX.271
! ----------------------------------------------------------------------         SFLX.272
!   INITIALIZATION                                                               SFLX.273
! ----------------------------------------------------------------------         SFLX.274
      PARAMETER (CP = 1004.5)                                                    SFLX.275
         RUNOFF1 = 0.0                                                           SFLX.276
         RUNOFF2 = 0.0                                                           SFLX.277
         RUNOFF3 = 0.0                                                           SFLX.278
                                                                                 SFLX.279
! ----------------------------------------------------------------------         SFLX.280
!  THE VARIABLE "ICE" IS A FLAG DENOTING SEA-ICE CASE                            SFLX.281
! ----------------------------------------------------------------------         SFLX.282
         SNOMLT = 0.0                                                            SFLX.283
                                                                                 SFLX.284
! ----------------------------------------------------------------------         SFLX.285
! SEA-ICE LAYERS ARE EQUAL THICKNESS AND SUM TO 3 METERS                         SFLX.286
! ----------------------------------------------------------------------         SFLX.287
         IF (ICE .eq. 1) THEN                                                    SFLX.288
            DO KZ = 1,NSOIL                                                      SFLX.289
               ZSOIL (KZ) = -3.* FLOAT (KZ)/ FLOAT (NSOIL)                       SFLX.290
            END DO                                                               SFLX.291
                                                                                 SFLX.292
! ----------------------------------------------------------------------         SFLX.293
! CALCULATE DEPTH (NEGATIVE) BELOW GROUND FROM TOP SKIN SFC TO BOTTOM OF         SFLX.294
!   EACH SOIL LAYER.  NOTE:  SIGN OF ZSOIL IS NEGATIVE (DENOTING BELOW           SFLX.295
!   GROUND)                                                                      SFLX.296
! ----------------------------------------------------------------------         SFLX.297
         ELSE                                                                    SFLX.298
            ZSOIL (1) = - SLDPTH (1)                                             SFLX.299
            DO KZ = 2,NSOIL                                                      SFLX.300
               ZSOIL (KZ) = - SLDPTH (KZ) + ZSOIL (KZ -1)                        SFLX.301
            END DO                                                               SFLX.302
! ----------------------------------------------------------------------         SFLX.303
! NEXT IS CRUCIAL CALL TO SET THE LAND-SURFACE PARAMETERS, INCLUDING             SFLX.304
! SOIL-TYPE AND VEG-TYPE DEPENDENT PARAMETERS.                                   SFLX.305
! ----------------------------------------------------------------------         SFLX.306
          CALL REDPRM (VEGTYP,SOILTYP,SLOPETYP,CFACTR,CMCMAX,RSMAX,TOPT,   &     SFLX.307
     &                 REFKDT,KDT,SBETA, SHDFAC,RSMIN,RGL,HS,ZBOT,FRZX,    &     SFLX.308
     &                   PSISAT,SLOPE,SNUP,SALP,BEXP,DKSAT,DWSAT,          &     SFLX.309
     &                   SMCMAX,SMCWLT,SMCREF,SMCDRY,F1,QUARTZ,FXEXP,      &     SFLX.310
     &                   RTDIS,SLDPTH,ZSOIL,NROOT,NSOIL,Z0BRD,CZIL,XLAI,   &     SFLX.311
     &                   CSOIL,ALB,PTU,LLANDUSE,LSOIL,OFFLINE)                   SFLX.312
         END IF                                                                  SFLX.313
                                                                                 23DEC04.3662
!urban change                                                                    23DEC04.3663
                                                                                 23DEC04.3664
        IF(VEGTYP.eq.1)THEN                                                      23DEC04.3665
              SHDFAC=0.05                                                        23DEC04.3666
              RSMIN=400.0                                                        23DEC04.3667
              SMCMAX = 0.45                                                      23DEC04.3668
              SMCREF = 0.42                                                      23DEC04.3669
              SMCWLT = 0.35 
              SMCDRY = 0.35
         ENDIF                                                                   23DEC04.3671
                                                                                 SFLX.314
! ----------------------------------------------------------------------         SFLX.315
!  INITIALIZE PRECIPITATION LOGICALS.                                            SFLX.316
! ----------------------------------------------------------------------         SFLX.317
         SNOWNG = .FALSE.                                                        SFLX.318
                                                                                 SFLX.319
! ----------------------------------------------------------------------         SFLX.320
! IF SEA-ICE CASE, ASSIGN DEFAULT WATER-EQUIV SNOW ON TOP                        SFLX.321
! ----------------------------------------------------------------------         SFLX.322
         FRZGRA = .FALSE.                                                        SFLX.323
         IF (ICE .eq. 1) THEN                                                    SFLX.324
            SNEQV = 0.01                                                         SFLX.325
            SNOWH = 0.05                                                         SFLX.326
! ----------------------------------------------------------------------         SFLX.327
! IF INPUT SNOWPACK IS NONZERO, THEN COMPUTE SNOW DENSITY "SNDENS" AND           SFLX.328
!   SNOW THERMAL CONDUCTIVITY "SNCOND" (NOTE THAT CSNOW IS A FUNCTION            SFLX.329
!   SUBROUTINE)                                                                  SFLX.330
! ----------------------------------------------------------------------         SFLX.331
         END IF                                                                  SFLX.332
         IF (SNEQV .eq. 0.0) THEN                                                SFLX.333
            SNDENS = 0.0                                                         SFLX.334
            SNOWH = 0.0                                                          SFLX.335
            SNCOND = 1.0                                                         SFLX.336
         ELSE                                                                    SFLX.337
            SNDENS = SNEQV / SNOWH                                               SFLX.338
            IF(SNDENS.GT.1.0) THEN                                               SFLX.339
        write(*,*)'Physical snow depth is less than snow water equiv.'           SFLX.340
          STOP 333                                                               SFLX.341
            ENDIF                                                                SFLX.342
!        SNCOND = CSNOW(SNDENS)                                                  SFLX.343
            CALL CSNOW (SNCOND,SNDENS)                                           SFLX.344
! ----------------------------------------------------------------------         SFLX.345
! DETERMINE IF IT'S PRECIPITATING AND WHAT KIND OF PRECIP IT IS.                 SFLX.346
! IF IT'S PRCPING AND THE AIR TEMP IS COLDER THAN 0 C, IT'S SNOWING!             SFLX.347
! IF IT'S PRCPING AND THE AIR TEMP IS WARMER THAN 0 C, BUT THE GRND              SFLX.348
! TEMP IS COLDER THAN 0 C, FREEZING RAIN IS PRESUMED TO BE FALLING.              SFLX.349
! ----------------------------------------------------------------------         SFLX.350
         END IF                                                                  SFLX.351
         IF (PRCP .gt. 0.0) THEN                                                 SFLX.352
            IF (SFCTMP .le. TFREEZ) THEN                                         SFLX.353
               SNOWNG = .TRUE.                                                   SFLX.354
            ELSE                                                                 SFLX.355
               IF (T1 .le. TFREEZ) FRZGRA = .TRUE.                               SFLX.356
            END IF                                                               SFLX.357
! ----------------------------------------------------------------------         SFLX.358
! IF EITHER PRCP FLAG IS SET, DETERMINE NEW SNOWFALL (CONVERTING PRCP            SFLX.359
! RATE FROM KG M-2 S-1 TO A LIQUID EQUIV SNOW DEPTH IN METERS) AND ADD           SFLX.360
! IT TO THE EXISTING SNOWPACK.                                                   SFLX.361
! NOTE THAT SINCE ALL PRECIP IS ADDED TO SNOWPACK, NO PRECIP INFILTRATES         SFLX.362
! INTO THE SOIL SO THAT PRCP1 IS SET TO ZERO.                                    SFLX.363
! ----------------------------------------------------------------------         SFLX.364
         END IF                                                                  SFLX.365
         IF ( (SNOWNG) .OR. (FRZGRA) ) THEN                                      SFLX.366
            SN_NEW = PRCP * DT * 0.001                                           SFLX.367
!        PRCP1 = 0.0                                                             SFLX.368
! change name of PRCP1 to PRCPF                                                  SFLX.369
            SNEQV = SNEQV + SN_NEW                                               SFLX.370
                                                                                 SFLX.371
! ----------------------------------------------------------------------         SFLX.372
! UPDATE SNOW DENSITY BASED ON NEW SNOWFALL, USING OLD AND NEW SNOW.             SFLX.373
! UPDATE SNOW THERMAL CONDUCTIVITY                                               SFLX.374
! ----------------------------------------------------------------------         SFLX.375
            PRCPF = 0.0                                                          SFLX.376
            CALL SNOW_NEW (SFCTMP,SN_NEW,SNOWH,SNDENS)                           SFLX.377
!        SNCOND = CSNOW (SNDENS)                                                 SFLX.378
            CALL CSNOW (SNCOND,SNDENS)                                           SFLX.379
                                                                                 SFLX.380
! ----------------------------------------------------------------------         SFLX.381
! PRECIP IS LIQUID (RAIN), HENCE SAVE IN THE PRECIP VARIABLE THAT                SFLX.382
! LATER CAN WHOLELY OR PARTIALLY INFILTRATE THE SOIL (ALONG WITH                 SFLX.383
! ANY CANOPY "DRIP" ADDED TO THIS LATER)                                         SFLX.384
! ----------------------------------------------------------------------         SFLX.385
!        PRCP1 = PRCP                                                            SFLX.386
! change name of PRCP1 to PRCPF                                                  SFLX.387
         ELSE                                                                    SFLX.388
                                                                                 SFLX.389
            PRCPF = PRCP                                                         SFLX.390
! ----------------------------------------------------------------------         SFLX.391
! DETERMINE SNOWCOVER AND ALBEDO OVER LAND.                                      SFLX.392
! ----------------------------------------------------------------------         SFLX.393
         END IF                                                                  SFLX.394
                                                                                 SFLX.395
! ----------------------------------------------------------------------         SFLX.396
! IF SNOW DEPTH=0, SET SNOW FRACTION=0, ALBEDO=SNOW FREE ALBEDO.                 SFLX.397
! ----------------------------------------------------------------------         SFLX.398
         IF (ICE .eq. 0) THEN                                                    SFLX.399
            IF (SNEQV .eq. 0.0) THEN                                             SFLX.400
               SNCOVR = 0.0                                                      SFLX.401
                                                                                 SFLX.402
               ALBEDO = ALB                                                      SFLX.403
! ----------------------------------------------------------------------         SFLX.404
! DETERMINE SNOW FRACTIONAL COVERAGE.                                            SFLX.405
! DETERMINE SURFACE ALBEDO MODIFICATION DUE TO SNOWDEPTH STATE.                  SFLX.406
! ----------------------------------------------------------------------         SFLX.407
            ELSE                                                                 SFLX.408
              CALL SNFRAC (SNEQV,SNUP,SALP,SNOWH,SNCOVR)                         SFLX.409
              CALL ALCALC (ALB,SNOALB,SHDFAC,SHDMIN,SNCOVR,TSNOW,ALBEDO)         SFLX.410
            END IF                                                               SFLX.411
! ----------------------------------------------------------------------         SFLX.412
! SNOW COVER, ALBEDO OVER SEA-ICE                                                SFLX.413
! ----------------------------------------------------------------------         SFLX.414
         ELSE                                                                    SFLX.415
            SNCOVR = 1.0                                                         SFLX.416
            ALBEdo = 0.60                                                        SFLX.417
! ----------------------------------------------------------------------         SFLX.418
! THERMAL CONDUCTIVITY FOR SEA-ICE CASE                                          SFLX.419
! ----------------------------------------------------------------------         SFLX.420
         END IF                                                                  SFLX.421
         IF (ICE .eq. 1) THEN                                                    SFLX.422
                                                                                 SFLX.423
            DF1 = 2.2                                                            SFLX.424
                                                                                 SFLX.425
! ----------------------------------------------------------------------         SFLX.426
! NEXT CALCULATE THE SUBSURFACE HEAT FLUX, WHICH FIRST REQUIRES                  SFLX.427
! CALCULATION OF THE THERMAL DIFFUSIVITY.  TREATMENT OF THE                      SFLX.428
! LATTER FOLLOWS THAT ON PAGES 148-149 FROM "HEAT TRANSFER IN                    SFLX.429
! COLD CLIMATES", BY V. J. LUNARDINI (PUBLISHED IN 1981                          SFLX.430
! BY VAN NOSTRAND REINHOLD CO.) I.E. TREATMENT OF TWO CONTIGUOUS                 SFLX.431
! "PLANE PARALLEL" MEDIUMS (NAMELY HERE THE FIRST SOIL LAYER                     SFLX.432
! AND THE SNOWPACK LAYER, IF ANY). THIS DIFFUSIVITY TREATMENT                    SFLX.433
! BEHAVES WELL FOR BOTH ZERO AND NONZERO SNOWPACK, INCLUDING THE                 SFLX.434
! LIMIT OF VERY THIN SNOWPACK.  THIS TREATMENT ALSO ELIMINATES                   SFLX.435
! THE NEED TO IMPOSE AN ARBITRARY UPPER BOUND ON SUBSURFACE                      SFLX.436
! HEAT FLUX WHEN THE SNOWPACK BECOMES EXTREMELY THIN.                            SFLX.437
! ----------------------------------------------------------------------         SFLX.438
! FIRST CALCULATE THERMAL DIFFUSIVITY OF TOP SOIL LAYER, USING                   SFLX.439
! BOTH THE FROZEN AND LIQUID SOIL MOISTURE, FOLLOWING THE                        SFLX.440
! SOIL THERMAL DIFFUSIVITY FUNCTION OF PETERS-LIDARD ET AL.                      SFLX.441
! (1998,JAS, VOL 55, 1209-1224), WHICH REQUIRES THE SPECIFYING                   SFLX.442
! THE QUARTZ CONTENT OF THE GIVEN SOIL CLASS (SEE ROUTINE REDPRM)                SFLX.443
! ----------------------------------------------------------------------         SFLX.444
         ELSE                                                                    SFLX.445
! ----------------------------------------------------------------------         SFLX.446
! NEXT ADD SUBSURFACE HEAT FLUX REDUCTION EFFECT FROM THE                        SFLX.447
! OVERLYING GREEN CANOPY, ADAPTED FROM SECTION 2.1.2 OF                          SFLX.448
! PETERS-LIDARD ET AL. (1997, JGR, VOL 102(D4))                                  SFLX.449
! ----------------------------------------------------------------------         SFLX.450
            CALL TDFCND (DF1,SMC (1),QUARTZ,SMCMAX,SH2O (1))                     SFLX.451
                                                                                 23DEC04.3672
! urban change                                                                   23DEC04.3673
            IF(VEGTYP.eq.1) DF1 = 3.24                                           23DEC04.3674
                                                                                 23DEC04.3675
            DF1 = DF1 * EXP (SBETA * SHDFAC)                                     SFLX.452
! ----------------------------------------------------------------------         SFLX.453
! FINALLY "PLANE PARALLEL" SNOWPACK EFFECT FOLLOWING                             SFLX.454
! V.J. LINARDINI REFERENCE CITED ABOVE. NOTE THAT DTOT IS                        SFLX.455
! COMBINED DEPTH OF SNOWDEPTH AND THICKNESS OF FIRST SOIL LAYER                  SFLX.456
! ----------------------------------------------------------------------         SFLX.457
         END IF                                                                  SFLX.458
                                                                                 SFLX.459
         DSOIL = - (0.5 * ZSOIL (1))                                             SFLX.460
         IF (SNEQV .eq. 0.) THEN                                                 SFLX.461
            SSOIL = DF1 * (T1- STC (1) ) / DSOIL                                 SFLX.462
         ELSE                                                                    SFLX.463
            DTOT = SNOWH + DSOIL                                                 SFLX.464
            FRCSNO = SNOWH / DTOT                                                SFLX.465
                                                                                 SFLX.466
! 1. HARMONIC MEAN (SERIES FLOW)                                                 SFLX.467
!        DF1 = (SNCOND*DF1)/(FRCSOI*SNCOND+FRCSNO*DF1)                           SFLX.468
            FRCSOI = DSOIL / DTOT                                                SFLX.469
! 2. ARITHMETIC MEAN (PARALLEL FLOW)                                             SFLX.470
!        DF1 = FRCSNO*SNCOND + FRCSOI*DF1                                        SFLX.471
            DF1H = (SNCOND * DF1)/ (FRCSOI * SNCOND+ FRCSNO * DF1)               SFLX.472
                                                                                 SFLX.473
! 3. GEOMETRIC MEAN (INTERMEDIATE BETWEEN HARMONIC AND ARITHMETIC MEAN)          SFLX.474
!        DF1 = (SNCOND**FRCSNO)*(DF1**FRCSOI)                                    SFLX.475
! TEST - MBEK, 10 Jan 2002                                                       SFLX.476
! weigh DF by snow fraction                                                      SFLX.477
!        DF1 = DF1H*SNCOVR + DF1A*(1.0-SNCOVR)                                   SFLX.478
!        DF1 = DF1H*SNCOVR + DF1*(1.0-SNCOVR)                                    SFLX.479
            DF1A = FRCSNO * SNCOND+ FRCSOI * DF1                                 SFLX.480
                                                                                 SFLX.481
! ----------------------------------------------------------------------         SFLX.482
! CALCULATE SUBSURFACE HEAT FLUX, SSOIL, FROM FINAL THERMAL DIFFUSIVITY          SFLX.483
! OF SURFACE MEDIUMS, DF1 ABOVE, AND SKIN TEMPERATURE AND TOP                    SFLX.484
! MID-LAYER SOIL TEMPERATURE                                                     SFLX.485
! ----------------------------------------------------------------------         SFLX.486
            DF1 = DF1A * SNCOVR + DF1* (1.0- SNCOVR)                             SFLX.487
            SSOIL = DF1 * (T1- STC (1) ) / DTOT                                  SFLX.488
         END IF                                                                  SFLX.489
! ----------------------------------------------------------------------         SFLX.490
! DETERMINE SURFACE ROUGHNESS OVER SNOWPACK USING SNOW CONDITION FROM            SFLX.491
! THE PREVIOUS TIMESTEP.                                                         SFLX.492
! ----------------------------------------------------------------------         SFLX.493
         IF (SNCOVR .gt. 0. ) THEN                                               SFLX.494
            CALL SNOWZ0 (SNCOVR,Z0,Z0BRD)                                        SFLX.495
         ELSE                                                                    23DEC04.3676
            Z0 = Z0BRD                                                           23DEC04.3677
         END IF                                                                  SFLX.496
! ----------------------------------------------------------------------         SFLX.497
! NEXT CALL ROUTINE SFCDIF TO CALCULATE THE SFC EXCHANGE COEF (CH) FOR           SFLX.498
! HEAT AND MOISTURE.                                                             SFLX.499
                                                                                 SFLX.500
! NOTE !!!                                                                       SFLX.501
! DO NOT CALL SFCDIF UNTIL AFTER ABOVE CALL TO REDPRM, IN CASE                   SFLX.502
! ALTERNATIVE VALUES OF ROUGHNESS LENGTH (Z0) AND ZILINTINKEVICH COEF            SFLX.503
! (CZIL) ARE SET THERE VIA NAMELIST I/O.                                         SFLX.504
                                                                                 SFLX.505
! NOTE !!!                                                                       SFLX.506
! ROUTINE SFCDIF RETURNS A CH THAT REPRESENTS THE WIND SPD TIMES THE             SFLX.507
! "ORIGINAL" NONDIMENSIONAL "Ch" TYPICAL IN LITERATURE.  HENCE THE CH            SFLX.508
! RETURNED FROM SFCDIF HAS UNITS OF M/S.  THE IMPORTANT COMPANION                SFLX.509
! COEFFICIENT OF CH, CARRIED HERE AS "RCH", IS THE CH FROM SFCDIF TIMES          SFLX.510
! AIR DENSITY AND PARAMETER "CP".  "RCH" IS COMPUTED IN "CALL PENMAN".           SFLX.511
! RCH RATHER THAN CH IS THE COEFF USUALLY INVOKED LATER IN EQNS.                 SFLX.512
                                                                                 SFLX.513
! NOTE !!!                                                                       SFLX.514
! ----------------------------------------------------------------------         SFLX.515
! SFCDIF ALSO RETURNS THE SURFACE EXCHANGE COEFFICIENT FOR MOMENTUM, CM,         SFLX.516
! ALSO KNOWN AS THE SURFACE DRAGE COEFFICIENT. Needed as a state variable        SFLX.517
! for iterative/implicit solution of CH in SFCDIF                                SFLX.518
! ----------------------------------------------------------------------         SFLX.519
         IF(.NOT.LCH) THEN                                                       23DEC04.3678
           T1V = T1 * (1.0+ 0.61 * Q2)                                           SFLX.521
           TH2V = TH2 * (1.0+ 0.61 * Q2)                                         SFLX.522
           CALL SFCDIF_off (ZLVL,Z0,T1V,TH2V,SFCSPD,CZIL,CM,CH)                  SFLX.523
         ENDIF                                                                   SFLX.524
                                                                                 SFLX.525
! ----------------------------------------------------------------------         SFLX.526
! CALL PENMAN SUBROUTINE TO CALCULATE POTENTIAL EVAPORATION (ETP), AND           SFLX.527
! OTHER PARTIAL PRODUCTS AND SUMS SAVE IN COMMON/RITE FOR LATER                  SFLX.528
! CALCULATIONS.                                                                  SFLX.529
! ----------------------------------------------------------------------         SFLX.530
! ----------------------------------------------------------------------         SFLX.531
! CALCULATE TOTAL DOWNWARD RADIATION (SOLAR PLUS LONGWAVE) NEEDED IN             SFLX.532
! PENMAN EP SUBROUTINE THAT FOLLOWS                                              SFLX.533
! ----------------------------------------------------------------------         SFLX.534
         FDOWN = SOLDN * (1.0- ALBEDO) + LWDN                                    SFLX.535
! ----------------------------------------------------------------------         SFLX.536
! CALC VIRTUAL TEMPS AND VIRTUAL POTENTIAL TEMPS NEEDED BY SUBROUTINES           SFLX.537
! PENMAN.                                                                        SFLX.538
         T2V = SFCTMP * (1.0+ 0.61 * Q2 )                                        SFLX.539
                                                                                 SFLX.540
         iout=0                                                                  SFLX.541
         if(iout.eq.1) then                                                      SFLX.542
         print*,'before penman'                                                  23DEC04.3679
         print*,' SFCTMP',SFCTMP,'SFCPRS',SFCPRS,'CH',CH,'T2V',T2V,              SFLX.544
     & 'TH2',TH2,'PRCP',PRCP,'FDOWN',FDOWN,'T24',T24,'SSOIL',SSOIL,              SFLX.545
     &  'Q2',Q2,'Q2SAT',Q2SAT,'ETP',ETP,'RCH',RCH,                               SFLX.546
     &  'EPSCA',EPSCA,'RR',RR,'SNOWNG',SNOWNG,'FRZGRA',FRZGRA,                   SFLX.547
     &  'DQSDT2',DQSDT2,'FLX2',FLX2,'SNOWH',SNOWH,'SNEQV',SNEQV,                 SFLX.548
     &  ' DSOIL',DSOIL,' FRCSNO',FRCSNO,' SNCOVR',SNCOVR,' DTOT',DTOT,           SFLX.549
     & ' ZSOIL (1)',ZSOIL(1),' DF1',DF1,'T1',T1,' STC1',STC(1),                  SFLX.550
     &  'ALBEDO',ALBEDO,'SMC',SMC,'STC',STC,'SH2O',SH2O                          SFLX.551
         endif                                                                   23DEC04.3680
                                                                                 SFLX.553
         CALL PENMAN (SFCTMP,SFCPRS,CH,T2V,TH2,PRCP,FDOWN,T24,SSOIL,             SFLX.554
     &                 Q2,Q2SAT,ETP,RCH,EPSCA,RR,SNOWNG,FRZGRA,                  SFLX.555
     &                   DQSDT2,FLX2,EMISSI)                                     23DEC04.3681
                                                                                 SFLX.557
! ----------------------------------------------------------------------         SFLX.558
! CALL CANRES TO CALCULATE THE CANOPY RESISTANCE AND CONVERT IT INTO PC          SFLX.559
! IF NONZERO GREENNESS FRACTION                                                  SFLX.560
! ----------------------------------------------------------------------         SFLX.561
                                                                                 SFLX.562
!       print*,'after penman ETP',ETP                                            SFLX.563
        IF(SATFLG) ETP=0.0                                                       SFLX.564
! ----------------------------------------------------------------------         SFLX.565
!  FROZEN GROUND EXTENSION: TOTAL SOIL WATER "SMC" WAS REPLACED                  SFLX.566
!  BY UNFROZEN SOIL WATER "SH2O" IN CALL TO CANRES BELOW                         SFLX.567
! ----------------------------------------------------------------------         SFLX.568
         IF (SHDFAC .gt. 0.) THEN                                                SFLX.569
            CALL CANRES (SOLDN,CH,SFCTMP,Q2,SFCPRS,SH2O,ZSOIL,NSOIL,             SFLX.570
     &                    SMCWLT,SMCREF,RSMIN,RC,PC,NROOT,Q2SAT,DQSDT2,          SFLX.571
     &                    TOPT,RSMAX,RGL,HS,XLAI,                                SFLX.572
     &                    RCS,RCT,RCQ,RCSOIL,EMISSI)                             23DEC04.3682
         END IF                                                                  SFLX.574
! ----------------------------------------------------------------------         SFLX.575
! NOW DECIDE MAJOR PATHWAY BRANCH TO TAKE DEPENDING ON WHETHER SNOWPACK          SFLX.576
! EXISTS OR NOT:                                                                 SFLX.577
! ----------------------------------------------------------------------         SFLX.578
         ESNOW = 0.0                                                             SFLX.579
         IF (SNEQV .eq. 0.0) THEN                                                SFLX.580
!urban                                                                           23DEC04.3683
            CALL NOPAC (ETP,ETA,PRCP,SMC,SMCMAX,SMCWLT,                       &  SFLX.581
     &                         SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,             &  23DEC04.3684
     &                         SHDFAC,                                        &  23DEC04.3685
     &                      SBETA,Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,EMISSI,       &  23DEC04.3686
     &                         SSOIL,                                         &  23DEC04.3687
     &                         STC,EPSCA,BEXP,PC,RCH,RR,CFACTR,               &  23DEC04.3688
     &                         SH2O,SLOPE,KDT,FRZX,PSISAT,ZSOIL,              &  23DEC04.3689
     &                         DKSAT,DWSAT,TBOT,ZBOT,RUNOFF1,RUNOFF2,         &  23DEC04.3690
     &                         RUNOFF3,EDIR,EC,ET,ETT,NROOT,ICE,RTDIS,        &  23DEC04.3691
     &                         QUARTZ,FXEXP,CSOIL,                            &  23DEC04.3692
     &                         BETA,DRIP,DEW,FLX1,FLX2,FLX3,VEGTYP)              23DEC04.3693
            ETA_KINEMATIC = ETA                                                  SFLX.592
            ETA = ETA * LVH2O                                                    SFLX.593
            ETP = ETP*LVH2O                                                      SFLX.594
!            BETA = ETA/ETP                                                      SFLX.595
         ELSE                                                                    SFLX.596
!urban                                                                           23DEC04.3694
            CALL SNOPAC (ETP,ETA,PRCP,PRCPF,SNOWNG,SMC,SMCMAX,SMCWLT,          & SFLX.597
     &                    SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,                   & SFLX.598
     &                    SBETA,DF1,                                           & SFLX.599
     &                    Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,SSOIL,STC,EPSCA,       & SFLX.600
     &                 SFCPRS,BEXP,PC,RCH,RR,CFACTR,SNCOVR,SNEQV,SNDENS,       & SFLX.601
     &                    SNOWH,SH2O,SLOPE,KDT,FRZX,PSISAT,SNUP,               & SFLX.602
     &                    ZSOIL,DWSAT,DKSAT,TBOT,ZBOT,SHDFAC,RUNOFF1,          & SFLX.603
     &                    RUNOFF2,RUNOFF3,EDIR,EC,ET,ETT,NROOT,SNOMLT,         & SFLX.604
     &                    ICE,RTDIS,QUARTZ,FXEXP,CSOIL,                        & SFLX.605
!     &                 BETA,DRIP,DEW,FLX1,FLX2,FLX3,ESNOW,ETNS,EMISSI)          23DEC04.3695
     &                 BETA,DRIP,DEW,FLX1,FLX2,FLX3,ESNOW,ETNS,EMISSI,           23DEC04.3696
     &                 VEGTYP)                                                   23DEC04.3697
            ETA_KINEMATIC =  ESNOW + ETNS                                        SFLX.607
            ESNOW = ESNOW * LSUBS                                                SFLX.608
!           ETA = ESNOW * LSUBS + ETNS * LVH2O                                   SFLX.609
            ETA = ESNOW + ETNS * LVH2O                                           SFLX.610
            ETP = ETP*LSUBS                                                      SFLX.611
!           BETA = ETA/ETP                                                       SFLX.612
         END IF                                                                  SFLX.613
                                                                                 SFLX.614
!     Calculate effective mixing ratio at grnd level (skin)                      SFLX.615
!                                                                                SFLX.616
!     Q1=Q2+ETA*CP/RCH                                                           SFLX.617
      Q1=Q2+ETA_KINEMATIC*CP/RCH                                                 SFLX.618
!                                                                                SFLX.619
! ----------------------------------------------------------------------         SFLX.620
!   PREPARE SENSIBLE HEAT (H) FOR RETURN TO PARENT MODEL                         SFLX.621
! ----------------------------------------------------------------------         SFLX.622
! ----------------------------------------------------------------------         SFLX.623
!  CONVERT UNITS AND/OR SIGN OF TOTAL EVAP (ETA), POTENTIAL EVAP (ETP),          SFLX.624
!  SUBSURFACE HEAT FLUX (S), AND RUNOFFS FOR WHAT PARENT MODEL EXPECTS           SFLX.625
!  CONVERT ETA FROM KG M-2 S-1 TO W M-2                                          SFLX.626
! ----------------------------------------------------------------------         SFLX.627
!ek      ETA = ETA*LVH2O                                                         SFLX.628
         SHEAT = - (CH * CP * SFCPRS)/ (R * T2V) * ( TH2- T1 )                   SFLX.629
                                                                                 SFLX.630
! ----------------------------------------------------------------------         SFLX.631
! CONVERT OTHER EVAP TERMS FROM M S-1 TO KG M-2 S-1, THEN TO W M-2               SFLX.632
! ----------------------------------------------------------------------         SFLX.633
!      EDIR = EDIR*1000.                                                         SFLX.634
!      EC = EC*1000.                                                             SFLX.635
!      DO K = 1,NSOIL                                                            SFLX.636
!       ET(K) = ET(K)*1000.                                                      23DEC04.3698
!      END DO                                                                    SFLX.638
!      ETT = ETT*1000.                                                           SFLX.639
                                                                                 SFLX.640
!        ETP = ETP * LVH2O                                                       SFLX.641
         EC = EC * LVH2O                                                         SFLX.642
         EDIR = EDIR * LVH2O                                                     SFLX.643
         DO K = 1,NSOIL                                                          SFLX.644
            ET (K) = ET (K)* LVH2O                                               SFLX.645
         END DO                                                                  SFLX.646
         ETT = ETT * LVH2O                                                       SFLX.647
                                                                                 SFLX.648
! ----------------------------------------------------------------------         SFLX.649
! CONVERT THE SIGN OF SOIL HEAT FLUX SO THAT:                                    SFLX.650
!   SSOIL>0: WARM THE SURFACE  (NIGHT TIME)                                      SFLX.651
!   SSOIL<0: COOL THE SURFACE  (DAY TIME)                                        SFLX.652
! ----------------------------------------------------------------------         SFLX.653
!        ESNOW = ESNOW * LVH2O                                                   SFLX.654
                                                                                 SFLX.655
! ----------------------------------------------------------------------         SFLX.656
!  CONVERT RUNOFF3 (INTERNAL LAYER RUNOFF FROM SUPERSAT) FROM M TO M S-1         SFLX.657
!  AND ADD TO SUBSURFACE RUNOFF/DRAINAGE/BASEFLOW                                SFLX.658
! ----------------------------------------------------------------------         SFLX.659
         SSOIL = -1.0* SSOIL                                                     SFLX.660
         RUNOFF3 = RUNOFF3/ DT                                                   SFLX.661
                                                                                 SFLX.662
! ----------------------------------------------------------------------         SFLX.663
! TOTAL COLUMN SOIL MOISTURE IN METERS (SOILM) AND ROOT-ZONE                     SFLX.664
! SOIL MOISTURE AVAILABILITY (FRACTION) RELATIVE TO POROSITY/SATURATION          SFLX.665
! ----------------------------------------------------------------------         SFLX.666
         RUNOFF2 = RUNOFF2+ RUNOFF3                                              SFLX.667
         IF (ICE .EQ. 0) THEN                                                    SFLX.668
         SOILM = -1.0* SMC (1)* ZSOIL (1)                                        SFLX.669
         DO K = 2,NSOIL                                                          SFLX.670
            SOILM = SOILM + SMC (K)* (ZSOIL (K -1) - ZSOIL (K))                  SFLX.671
         END DO                                                                  SFLX.672
         SOILWM = -1.0* (SMCMAX - SMCWLT)* ZSOIL (1)                             SFLX.673
         SOILWW = -1.0* (SMC (1) - SMCWLT)* ZSOIL (1)                            SFLX.674
                                                                                 SFLX.675
         IF (NROOT .GE. 2) THEN                                                  SFLX.676
           DO K = 2,NROOT                                                        SFLX.677
        SOILWM = SOILWM + (SMCMAX - SMCWLT)* (ZSOIL (K -1) - ZSOIL (K))          SFLX.678
        SOILWW = SOILWW + (SMC(K) - SMCWLT)* (ZSOIL (K -1) - ZSOIL (K))          SFLX.679
           END DO                                                                SFLX.680
         END IF                                                                  SFLX.681
                                                                                 SFLX.682
         IF (SOILWM .LT. 1.E-6) THEN                                             SFLX.683
           SOILWM = 0.0                                                          SFLX.684
           SOILW  = 0.0                                                          SFLX.685
           SOILM  = 0.0                                                          SFLX.686
         ELSE                                                                    SFLX.687
           SOILW = SOILWW / SOILWM                                               SFLX.688
         END IF                                                                  SFLX.689
                                                                                 SFLX.690
         ELSE                                                                    SFLX.691
           SOILWM = 0.0                                                          SFLX.692
           SOILW  = 0.0                                                          SFLX.693
           SOILM  = 0.0                                                          SFLX.694
         END IF                                                                  SFLX.695
                                                                                 SFLX.696
! ----------------------------------------------------------------------         SFLX.697
! END SUBROUTINE SFLX                                                            SFLX.698
! ----------------------------------------------------------------------         SFLX.699
      END                                                                        SFLX.700
                                                                                 SFLX.701
      SUBROUTINE ALCALC (ALB,SNOALB,SHDFAC,SHDMIN,SNCOVR,TSNOW,ALBEDO)           SFLX.702
                                                                                 SFLX.703
! ----------------------------------------------------------------------         SFLX.704
! CALCULATE ALBEDO INCLUDING SNOW EFFECT (0 -> 1)                                SFLX.705
!   ALB     SNOWFREE ALBEDO                                                      SFLX.706
!   SNOALB  MAXIMUM (DEEP) SNOW ALBEDO                                           SFLX.707
!   SHDFAC    AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION                      SFLX.708
!   SHDMIN    MINIMUM AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION              SFLX.709
!   SNCOVR  FRACTIONAL SNOW COVER                                                SFLX.710
!   ALBEDO  SURFACE ALBEDO INCLUDING SNOW EFFECT                                 SFLX.711
!   TSNOW   SNOW SURFACE TEMPERATURE (K)                                         SFLX.712
! ----------------------------------------------------------------------         SFLX.713
      IMPLICIT NONE                                                              SFLX.714
                                                                                 SFLX.715
! ----------------------------------------------------------------------         SFLX.716
! SNOALB IS ARGUMENT REPRESENTING MAXIMUM ALBEDO OVER DEEP SNOW,                 SFLX.717
! AS PASSED INTO SFLX, AND ADAPTED FROM THE SATELLITE-BASED MAXIMUM              SFLX.718
! SNOW ALBEDO FIELDS PROVIDED BY D. ROBINSON AND G. KUKLA                        SFLX.719
! (1985, JCAM, VOL 24, 402-411)                                                  SFLX.720
! ----------------------------------------------------------------------         SFLX.721
      REAL     ALB, SNOALB, SHDFAC, SHDMIN, SNCOVR, ALBEDO, TSNOW                SFLX.722
      ALBEDO = ALB + (1.0- (SHDFAC - SHDMIN))* SNCOVR * (SNOALB - ALB)           SFLX.723
                                                                                 SFLX.724
!     BASE FORMULATION (DICKINSON ET AL., 1986, COGLEY ET AL., 1990)             SFLX.725
!          IF (TSNOW.LE.263.16) THEN                                             SFLX.726
!            ALBEDO=SNOALB                                                       SFLX.727
!          ELSE                                                                  SFLX.728
!            IF (TSNOW.LT.273.16) THEN                                           SFLX.729
!              TM=0.1*(TSNOW-263.16)                                             SFLX.730
!              ALBEDO=0.5*((0.9-0.2*(TM**3))+(0.8-0.16*(TM**3)))                 SFLX.731
!            ELSE                                                                SFLX.732
!              ALBEDO=0.67                                                       SFLX.733
!            ENDIF                                                               SFLX.734
!          ENDIF                                                                 SFLX.735
                                                                                 SFLX.736
!     ISBA FORMULATION (VERSEGHY, 1991; BAKER ET AL., 1990)                      SFLX.737
!          IF (TSNOW.LT.273.16) THEN                                             SFLX.738
!            ALBEDO=SNOALB-0.008*DT/86400                                        SFLX.739
!          ELSE                                                                  SFLX.740
!            ALBEDO=(SNOALB-0.5)*EXP(-0.24*DT/86400)+0.5                         SFLX.741
!          ENDIF                                                                 SFLX.742
                                                                                 SFLX.743
! ----------------------------------------------------------------------         SFLX.744
! END SUBROUTINE ALCALC                                                          SFLX.745
! ----------------------------------------------------------------------         SFLX.746
      IF (ALBEdo.gt. SNOALB) ALBEDO = SNOALB                                     SFLX.747
      END                                                                        SFLX.748
                                                                                 SFLX.749
      SUBROUTINE CANRES (SOLAR,CH,SFCTMP,Q2,SFCPRS,SMC,ZSOIL,NSOIL,        &     SFLX.750
     &                    SMCWLT,SMCREF,RSMIN,RC,PC,NROOT,Q2SAT,DQSDT2,    &     SFLX.751
     &                    TOPT,RSMAX,RGL,HS,XLAI,                          &     SFLX.752
     &                    RCS,RCT,RCQ,RCSOIL,EMISSI)                             23DEC04.3699
                                                                                 SFLX.754
! ----------------------------------------------------------------------         SFLX.755
! SUBROUTINE CANRES                                                              SFLX.756
! ----------------------------------------------------------------------         SFLX.757
! CALCULATE CANOPY RESISTANCE WHICH DEPENDS ON INCOMING SOLAR RADIATION,         SFLX.758
! AIR TEMPERATURE, ATMOSPHERIC WATER VAPOR PRESSURE DEFICIT AT THE               SFLX.759
! LOWEST MODEL LEVEL, AND SOIL MOISTURE (PREFERABLY UNFROZEN SOIL                SFLX.760
! MOISTURE RATHER THAN TOTAL)                                                    SFLX.761
! ----------------------------------------------------------------------         SFLX.762
! SOURCE:  JARVIS (1976), NOILHAN AND PLANTON (1989, MWR), JACQUEMIN AND         SFLX.763
! NOILHAN (1990, BLM)                                                            SFLX.764
! SEE ALSO:  CHEN ET AL (1996, JGR, VOL 101(D3), 7251-7268), EQNS 12-14          SFLX.765
! AND TABLE 2 OF SEC. 3.1.2                                                      SFLX.766
! ----------------------------------------------------------------------         SFLX.767
! INPUT:                                                                         SFLX.768
!   SOLAR   INCOMING SOLAR RADIATION                                             SFLX.769
!   CH      SURFACE EXCHANGE COEFFICIENT FOR HEAT AND MOISTURE                   SFLX.770
!   SFCTMP  AIR TEMPERATURE AT 1ST LEVEL ABOVE GROUND                            SFLX.771
!   Q2      AIR HUMIDITY AT 1ST LEVEL ABOVE GROUND                               SFLX.772
!   Q2SAT   SATURATION AIR HUMIDITY AT 1ST LEVEL ABOVE GROUND                    SFLX.773
!   DQSDT2  SLOPE OF SATURATION HUMIDITY FUNCTION WRT TEMP                       SFLX.774
!   SFCPRS  SURFACE PRESSURE                                                     SFLX.775
!   SMC     VOLUMETRIC SOIL MOISTURE                                             SFLX.776
!   ZSOIL   SOIL DEPTH (NEGATIVE SIGN, AS IT IS BELOW GROUND)                    SFLX.777
!   NSOIL   NO. OF SOIL LAYERS                                                   SFLX.778
!   NROOT   NO. OF SOIL LAYERS IN ROOT ZONE (1.LE.NROOT.LE.NSOIL)                SFLX.779
!   XLAI    LEAF AREA INDEX                                                      SFLX.780
!   SMCWLT  WILTING POINT                                                        SFLX.781
!   SMCREF  REFERENCE SOIL MOISTURE (WHERE SOIL WATER DEFICIT STRESS             SFLX.782
!             SETS IN)                                                           SFLX.783
! RSMIN, RSMAX, TOPT, RGL, HS ARE CANOPY STRESS PARAMETERS SET IN                SFLX.784
!   SURBOUTINE REDPRM                                                            SFLX.785
! OUTPUT:                                                                        SFLX.786
!   PC  PLANT COEFFICIENT                                                        SFLX.787
!   RC  CANOPY RESISTANCE                                                        SFLX.788
! ----------------------------------------------------------------------         SFLX.789
      IMPLICIT NONE                                                              SFLX.790
      INTEGER  NSOLD                                                             SFLX.791
                                                                                 SFLX.792
      PARAMETER (NSOLD = 20)                                                     SFLX.793
      INTEGER  K                                                                 SFLX.794
      INTEGER  NROOT                                                             SFLX.795
                                                                                 SFLX.796
      INTEGER  NSOIL                                                             SFLX.797
      REAL     CH                                                                SFLX.798
      REAL     CP                                                                SFLX.799
      REAL     DELTA                                                             SFLX.800
      REAL     DQSDT2                                                            SFLX.801
      REAL     FF                                                                SFLX.802
      REAL     GX                                                                SFLX.803
      REAL     HS                                                                SFLX.804
      REAL     P                                                                 SFLX.805
      REAL     PART (NSOLD)                                                      SFLX.806
      REAL     PC                                                                SFLX.807
      REAL     Q2                                                                SFLX.808
      REAL     Q2SAT                                                             SFLX.809
      REAL     RC                                                                SFLX.810
      REAL     RSMIN                                                             SFLX.811
      REAL     RCQ                                                               SFLX.812
      REAL     RCS                                                               SFLX.813
      REAL     RCSOIL                                                            SFLX.814
      REAL     RCT                                                               SFLX.815
      REAL     RD                                                                SFLX.816
      REAL     RGL                                                               SFLX.817
      REAL     RR                                                                SFLX.818
      REAL     RSMAX                                                             SFLX.819
      REAL     SFCPRS                                                            SFLX.820
      REAL     SFCTMP                                                            SFLX.821
      REAL     SIGMA                                                             SFLX.822
      REAL     SLV                                                               SFLX.823
      REAL     SMC (NSOIL)                                                       SFLX.824
      REAL     SMCREF                                                            SFLX.825
      REAL     SMCWLT                                                            SFLX.826
      REAL     SOLAR                                                             SFLX.827
      REAL     TOPT                                                              SFLX.828
      REAL     SLVCP                                                             SFLX.829
      REAL     ST1                                                               SFLX.830
      REAL     TAIR4                                                             SFLX.831
      REAL     XLAI                                                              SFLX.832
      REAL     EMISSI                                                            23DEC04.3700
                                                                                 SFLX.833
      REAL     ZSOIL (NSOIL)                                                     SFLX.834
      PARAMETER (CP = 1004.5)                                                    SFLX.835
      PARAMETER (RD = 287.04)                                                    SFLX.836
      PARAMETER (SIGMA = 5.67E-8)                                                SFLX.837
                                                                                 SFLX.838
! ----------------------------------------------------------------------         SFLX.839
! INITIALIZE CANOPY RESISTANCE MULTIPLIER TERMS.                                 SFLX.840
! ----------------------------------------------------------------------         SFLX.841
      PARAMETER (SLV = 2.501000E6)                                               SFLX.842
      RCS = 0.0                                                                  SFLX.843
      RCT = 0.0                                                                  SFLX.844
      RCQ = 0.0                                                                  SFLX.845
      RCSOIL = 0.0                                                               SFLX.846
                                                                                 SFLX.847
! ----------------------------------------------------------------------         SFLX.848
! CONTRIBUTION DUE TO INCOMING SOLAR RADIATION                                   SFLX.849
! ----------------------------------------------------------------------         SFLX.850
      RC = 0.0                                                                   SFLX.851
      FF = 0.55*2.0* SOLAR / (RGL * XLAI)                                        SFLX.852
      RCS = (FF + RSMIN / RSMAX) / (1.0+ FF)                                     SFLX.853
                                                                                 SFLX.854
! ----------------------------------------------------------------------         SFLX.855
! CONTRIBUTION DUE TO AIR TEMPERATURE AT FIRST MODEL LEVEL ABOVE GROUND          SFLX.856
! RCT EXPRESSION FROM NOILHAN AND PLANTON (1989, MWR).                           SFLX.857
! ----------------------------------------------------------------------         SFLX.858
      RCS = MAX (RCS,0.0001)                                                     SFLX.859
      RCT = 1.0- 0.0016* ( (TOPT - SFCTMP)**2.0)                                 SFLX.860
                                                                                 SFLX.861
! ----------------------------------------------------------------------         SFLX.862
! CONTRIBUTION DUE TO VAPOR PRESSURE DEFICIT AT FIRST MODEL LEVEL.               SFLX.863
! RCQ EXPRESSION FROM SSIB                                                       SFLX.864
! ----------------------------------------------------------------------         SFLX.865
      RCT = MAX (RCT,0.0001)                                                     SFLX.866
      RCQ = 1.0/ (1.0+ HS * (Q2SAT - Q2))                                        SFLX.867
                                                                                 SFLX.868
! ----------------------------------------------------------------------         SFLX.869
! CONTRIBUTION DUE TO SOIL MOISTURE AVAILABILITY.                                SFLX.870
! DETERMINE CONTRIBUTION FROM EACH SOIL LAYER, THEN ADD THEM UP.                 SFLX.871
! ----------------------------------------------------------------------         SFLX.872
      RCQ = MAX (RCQ,0.01)                                                       SFLX.873
      GX = (SMC (1) - SMCWLT) / (SMCREF - SMCWLT)                                SFLX.874
      IF (GX .gt. 1.) GX = 1.                                                    SFLX.875
                                                                                 SFLX.876
! ----------------------------------------------------------------------         SFLX.877
! USE SOIL DEPTH AS WEIGHTING FACTOR                                             SFLX.878
! ----------------------------------------------------------------------         SFLX.879
      IF (GX .lt. 0.) GX = 0.                                                    SFLX.880
! ----------------------------------------------------------------------         SFLX.881
! USE ROOT DISTRIBUTION AS WEIGHTING FACTOR                                      SFLX.882
!      PART(1) = RTDIS(1) * GX                                                   SFLX.883
! ----------------------------------------------------------------------         SFLX.884
      PART (1) = (ZSOIL (1)/ ZSOIL (NROOT)) * GX                                 SFLX.885
      DO K = 2,NROOT                                                             SFLX.886
         GX = (SMC (K) - SMCWLT) / (SMCREF - SMCWLT)                             SFLX.887
         IF (GX .gt. 1.) GX = 1.                                                 SFLX.888
! ----------------------------------------------------------------------         SFLX.889
! USE SOIL DEPTH AS WEIGHTING FACTOR                                             SFLX.890
! ----------------------------------------------------------------------         SFLX.891
         IF (GX .lt. 0.) GX = 0.                                                 SFLX.892
! ----------------------------------------------------------------------         SFLX.893
! USE ROOT DISTRIBUTION AS WEIGHTING FACTOR                                      SFLX.894
!        PART(K) = RTDIS(K) * GX                                                 SFLX.895
! ----------------------------------------------------------------------         SFLX.896
         PART (K) = ( (ZSOIL (K) - ZSOIL (K -1))/ ZSOIL (NROOT)) * GX            SFLX.897
      END DO                                                                     SFLX.898
      DO K = 1,NROOT                                                             SFLX.899
         RCSOIL = RCSOIL + PART (K)                                              SFLX.900
      END DO                                                                     SFLX.901
                                                                                 SFLX.902
! ----------------------------------------------------------------------         SFLX.903
! DETERMINE CANOPY RESISTANCE DUE TO ALL FACTORS.  CONVERT CANOPY                SFLX.904
! RESISTANCE (RC) TO PLANT COEFFICIENT (PC) TO BE USED WITH POTENTIAL            SFLX.905
! EVAP IN DETERMINING ACTUAL EVAP.  PC IS DETERMINED BY:                         SFLX.906
!   PC * LINERIZED PENMAN POTENTIAL EVAP =                                       SFLX.907
!   PENMAN-MONTEITH ACTUAL EVAPORATION (CONTAINING RC TERM).                     SFLX.908
! ----------------------------------------------------------------------         SFLX.909
      RCSOIL = MAX (RCSOIL,0.0001)                                               SFLX.910
                                                                                 SFLX.911
!      TAIR4 = SFCTMP**4.                                                        SFLX.912
!      ST1 = (4.*SIGMA*RD)/CP                                                    SFLX.913
!      SLVCP = SLV/CP                                                            SFLX.914
!      RR = ST1*TAIR4/(SFCPRS*CH) + 1.0                                          SFLX.915
      RC = RSMIN / (XLAI * RCS * RCT * RCQ * RCSOIL)                             SFLX.916
      RR = (4.* EMISSI *SIGMA * RD / CP)* (SFCTMP **4.)/ (SFCPRS * CH)  &        23DEC04.3701
     &      + 1.0                                                                23DEC04.3702
                                                                                 SFLX.918
      DELTA = (SLV / CP)* DQSDT2                                                 SFLX.919
                                                                                 SFLX.920
! ----------------------------------------------------------------------         SFLX.921
! END SUBROUTINE CANRES                                                          SFLX.922
! ----------------------------------------------------------------------         SFLX.923
      PC = (RR + DELTA)/ (RR * (1. + RC * CH) + DELTA)                           SFLX.924
      END                                                                        SFLX.925
                                                                                 SFLX.926
!      FUNCTION CSNOW (DSNOW)                                                    SFLX.927
                                                                                 SFLX.928
      SUBROUTINE CSNOW (SNCOND,DSNOW)                                            SFLX.929
                                                                                 SFLX.930
! ----------------------------------------------------------------------         SFLX.931
! SUBROUTINE CSNOW                                                               SFLX.932
! FUNCTION CSNOW                                                                 SFLX.933
! ----------------------------------------------------------------------         SFLX.934
! CALCULATE SNOW TERMAL CONDUCTIVITY                                             SFLX.935
! ----------------------------------------------------------------------         SFLX.936
      IMPLICIT NONE                                                              SFLX.937
      REAL     C                                                                 SFLX.938
!      REAL CSNOW                                                                SFLX.939
      REAL     DSNOW                                                             SFLX.940
      REAL     SNCOND                                                            SFLX.941
                                                                                 SFLX.942
      REAL     UNIT                                                              SFLX.943
                                                                                 SFLX.944
! ----------------------------------------------------------------------         SFLX.945
! SNCOND IN UNITS OF CAL/(CM*HR*C), RETURNED IN W/(M*C)                          SFLX.946
! CSNOW IN UNITS OF CAL/(CM*HR*C), RETURNED IN W/(M*C)                           SFLX.947
! BASIC VERSION IS DYACHKOVA EQUATION (1960), FOR RANGE 0.1-0.4                  SFLX.948
! ----------------------------------------------------------------------         SFLX.949
      PARAMETER (UNIT = 0.11631)                                                 SFLX.950
      C = 0.328*10** (2.25* DSNOW)                                               SFLX.951
!      CSNOW=UNIT*C                                                              SFLX.952
                                                                                 SFLX.953
! ----------------------------------------------------------------------         SFLX.954
! DE VAUX EQUATION (1933), IN RANGE 0.1-0.6                                      SFLX.955
! ----------------------------------------------------------------------         SFLX.956
!      SNCOND=0.0293*(1.+100.*DSNOW**2)                                          SFLX.957
!      CSNOW=0.0293*(1.+100.*DSNOW**2)                                           SFLX.958
                                                                                 SFLX.959
! ----------------------------------------------------------------------         SFLX.960
! E. ANDERSEN FROM FLERCHINGER                                                   SFLX.961
! ----------------------------------------------------------------------         SFLX.962
!      SNCOND=0.021+2.51*DSNOW**2                                                SFLX.963
!      CSNOW=0.021+2.51*DSNOW**2                                                 SFLX.964
                                                                                 SFLX.965
! ----------------------------------------------------------------------         SFLX.966
! END SUBROUTINE CSNOW                                                           SFLX.967
! END FUNCTION CSNOW                                                             SFLX.968
! ----------------------------------------------------------------------         SFLX.969
      SNCOND = UNIT * C                                                          SFLX.970
      END                                                                        SFLX.971
                                                                                 SFLX.972
      SUBROUTINE DEVAP (EDIR,ETP1,SMC,ZSOIL,SHDFAC,SMCMAX,BEXP,         &        SFLX.973
     &                     DKSAT,DWSAT,SMCDRY,SMCREF,SMCWLT,FXEXP)               SFLX.974
                                                                                 SFLX.975
! ----------------------------------------------------------------------         SFLX.976
! SUBROUTINE DEVAP                                                               SFLX.977
! FUNCTION DEVAP                                                                 SFLX.978
! ----------------------------------------------------------------------         SFLX.979
! CALCULATE DIRECT SOIL EVAPORATION                                              SFLX.980
! ----------------------------------------------------------------------         SFLX.981
      IMPLICIT NONE                                                              SFLX.982
!      REAL DEVAP                                                                SFLX.983
      REAL     BEXP                                                              SFLX.984
      REAL     EDIR                                                              SFLX.985
      REAL     DKSAT                                                             SFLX.986
      REAL     DWSAT                                                             SFLX.987
      REAL     ETP1                                                              SFLX.988
      REAL     FX                                                                SFLX.989
      REAL     FXEXP                                                             SFLX.990
      REAL     SHDFAC                                                            SFLX.991
      REAL     SMC                                                               SFLX.992
      REAL     SMCDRY                                                            SFLX.993
      REAL     SMCMAX                                                            SFLX.994
      REAL     ZSOIL                                                             SFLX.995
      REAL     SMCREF                                                            SFLX.996
      REAL     SRATIO                                                            SFLX.997
                                                                                 SFLX.998
! ----------------------------------------------------------------------         SFLX.999
! DIRECT EVAP A FUNCTION OF RELATIVE SOIL MOISTURE AVAILABILITY, LINEAR          SFLX.1000
! WHEN FXEXP=1.                                                                  SFLX.1001
! ----------------------------------------------------------------------         SFLX.1002
      REAL     SMCWLT                                                            SFLX.1003
! ----------------------------------------------------------------------         SFLX.1004
! FX > 1 REPRESENTS DEMAND CONTROL                                               SFLX.1005
! FX < 1 REPRESENTS FLUX CONTROL                                                 SFLX.1006
! ----------------------------------------------------------------------         SFLX.1007
      SRATIO = (SMC - SMCDRY) / (SMCMAX - SMCDRY)                                SFLX.1008
      IF (SRATIO .GT. 0.) THEN                                                   SFLX.1009
        FX = SRATIO**FXEXP                                                       SFLX.1010
        FX = MAX ( MIN ( FX, 1. ) ,0. )                                          SFLX.1011
      ELSE                                                                       SFLX.1012
        FX = 0.                                                                  SFLX.1013
      ENDIF                                                                      SFLX.1014
                                                                                 SFLX.1015
! ----------------------------------------------------------------------         SFLX.1016
! ALLOW FOR THE DIRECT-EVAP-REDUCING EFFECT OF SHADE                             SFLX.1017
! ----------------------------------------------------------------------         SFLX.1018
!      DEVAP = FX * ( 1.0 - SHDFAC ) * ETP1                                      SFLX.1019
                                                                                 SFLX.1020
! ----------------------------------------------------------------------         SFLX.1021
! END SUBROUTINE DEVAP                                                           SFLX.1022
! END FUNCTION DEVAP                                                             SFLX.1023
! ----------------------------------------------------------------------         SFLX.1024
      EDIR = FX * ( 1.0- SHDFAC ) * ETP1                                         SFLX.1025
      END                                                                        SFLX.1026
                                                                                 SFLX.1027
      SUBROUTINE EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,               &        SFLX.1028
     &                   SH2O,                                          &        SFLX.1029
     &                   SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,             &        SFLX.1030
     &                   SMCREF,SHDFAC,CMCMAX,                          &        SFLX.1031
     &                   SMCDRY,CFACTR,                                 &        SFLX.1032
     &                   EDIR,EC,ET,ETT,SFCTMP,Q2,NROOT,RTDIS,FXEXP)             SFLX.1033
                                                                                 SFLX.1034
! ----------------------------------------------------------------------         SFLX.1035
! SUBROUTINE EVAPO                                                               SFLX.1036
! ----------------------------------------------------------------------         SFLX.1037
! CALCULATE SOIL MOISTURE FLUX.  THE SOIL MOISTURE CONTENT (SMC - A PER          SFLX.1038
! UNIT VOLUME MEASUREMENT) IS A DEPENDENT VARIABLE THAT IS UPDATED WITH          SFLX.1039
! PROGNOSTIC EQNS. THE CANOPY MOISTURE CONTENT (CMC) IS ALSO UPDATED.            SFLX.1040
! FROZEN GROUND VERSION:  NEW STATES ADDED: SH2O, AND FROZEN GROUND              SFLX.1041
! CORRECTION FACTOR, FRZFACT AND PARAMETER SLOPE.                                SFLX.1042
! ----------------------------------------------------------------------         SFLX.1043
      IMPLICIT NONE                                                              SFLX.1044
      INTEGER  NSOLD                                                             SFLX.1045
                                                                                 SFLX.1046
      PARAMETER (NSOLD = 20)                                                     SFLX.1047
      INTEGER  I                                                                 SFLX.1048
      INTEGER  K                                                                 SFLX.1049
      INTEGER  NSOIL                                                             SFLX.1050
                                                                                 SFLX.1051
      INTEGER  NROOT                                                             SFLX.1052
      REAL     BEXP                                                              SFLX.1053
      REAL     CFACTR                                                            SFLX.1054
      REAL     CMC                                                               SFLX.1055
      REAL     CMC2MS                                                            SFLX.1056
!      REAL DEVAP                                                                SFLX.1057
      REAL     CMCMAX                                                            SFLX.1058
      REAL     DKSAT                                                             SFLX.1059
      REAL     DT                                                                SFLX.1060
      REAL     DWSAT                                                             SFLX.1061
      REAL     EC                                                                SFLX.1062
      REAL     EDIR                                                              SFLX.1063
      REAL     ET (NSOIL)                                                        SFLX.1064
      REAL     ETA1                                                              SFLX.1065
      REAL     ETP1                                                              SFLX.1066
      REAL     ETT                                                               SFLX.1067
      REAL     FXEXP                                                             SFLX.1068
      REAL     PC                                                                SFLX.1069
      REAL     Q2                                                                SFLX.1070
      REAL     RTDIS (NSOIL)                                                     SFLX.1071
      REAL     SFCTMP                                                            SFLX.1072
      REAL     SHDFAC                                                            SFLX.1073
      REAL     SMC (NSOIL)                                                       SFLX.1074
      REAL     SH2O (NSOIL)                                                      SFLX.1075
      REAL     SMCDRY                                                            SFLX.1076
      REAL     SMCMAX                                                            SFLX.1077
      REAL     SMCREF                                                            SFLX.1078
      REAL     SMCWLT                                                            SFLX.1079
                                                                                 SFLX.1080
! ----------------------------------------------------------------------         SFLX.1081
! EXECUTABLE CODE BEGINS HERE IF THE POTENTIAL EVAPOTRANSPIRATION IS             SFLX.1082
! GREATER THAN ZERO.                                                             SFLX.1083
! ----------------------------------------------------------------------         SFLX.1084
      REAL     ZSOIL (NSOIL)                                                     SFLX.1085
      EDIR = 0.                                                                  SFLX.1086
      EC = 0.                                                                    SFLX.1087
      ETT = 0.                                                                   SFLX.1088
      DO K = 1,NSOIL                                                             SFLX.1089
         ET (K) = 0.                                                             SFLX.1090
      END DO                                                                     SFLX.1091
                                                                                 SFLX.1092
!      print*,'SHDFAC',SHDFAC,'SMCMAX', SMCMAX,'BEXP',BEXP,                      SFLX.1093
!    & 'DKSAT',DKSAT,'DWSAT',DWSAT,'DRY',SMCDRY,'REF',SMCREF,                    SFLX.1094
!    & 'WLT',SMCWLT,'FXEX',FXEXP,'SMC',SMC                                       SFLX.1095
! ----------------------------------------------------------------------         SFLX.1096
! RETRIEVE DIRECT EVAPORATION FROM SOIL SURFACE.  CALL THIS FUNCTION             SFLX.1097
! ONLY IF VEG COVER NOT COMPLETE.                                                SFLX.1098
! FROZEN GROUND VERSION:  SH2O STATES REPLACE SMC STATES.                        SFLX.1099
! ----------------------------------------------------------------------         SFLX.1100
      IF (ETP1 .gt. 0.0) THEN                                                    SFLX.1101
         IF (SHDFAC .lt. 1.) THEN                                                SFLX.1102
!            CALL DEVAP (EDIR,ETP1,SH2O (1),ZSOIL (1),SHDFAC,SMCMAX,             SFLX.1103
             CALL DEVAP (EDIR,ETP1,SMC (1),ZSOIL (1),SHDFAC,SMCMAX,      &       SFLX.1104
     &                   BEXP,DKSAT,DWSAT,SMCDRY,SMCREF,SMCWLT,FXEXP)            SFLX.1105
! ----------------------------------------------------------------------         SFLX.1106
! INITIALIZE PLANT TOTAL TRANSPIRATION, RETRIEVE PLANT TRANSPIRATION,            SFLX.1107
! AND ACCUMULATE IT FOR ALL SOIL LAYERS.                                         SFLX.1108
! ----------------------------------------------------------------------         SFLX.1109
         END IF                                                                  SFLX.1110
                                                                                 SFLX.1111
         IF (SHDFAC .gt. 0.0) THEN                                               SFLX.1112
            CALL TRANSP (ET,NSOIL,ETP1,SH2O,CMC,ZSOIL,SHDFAC,SMCWLT,     &       SFLX.1113
     &                    CMCMAX,PC,CFACTR,SMCREF,SFCTMP,Q2,NROOT,RTDIS)         SFLX.1114
            DO K = 1,NSOIL                                                       SFLX.1115
               ETT = ETT + ET ( K )                                              SFLX.1116
! ----------------------------------------------------------------------         SFLX.1117
! CALCULATE CANOPY EVAPORATION.                                                  SFLX.1118
! IF STATEMENTS TO AVOID TANGENT LINEAR PROBLEMS NEAR CMC=0.0.                   SFLX.1119
! ----------------------------------------------------------------------         SFLX.1120
            END DO                                                               SFLX.1121
            IF (CMC .gt. 0.0) THEN                                               SFLX.1122
               EC = SHDFAC * ( ( CMC / CMCMAX ) ** CFACTR ) * ETP1               SFLX.1123
            ELSE                                                                 SFLX.1124
               EC = 0.0                                                          SFLX.1125
! ----------------------------------------------------------------------         SFLX.1126
! EC SHOULD BE LIMITED BY THE TOTAL AMOUNT OF AVAILABLE WATER ON THE             SFLX.1127
! CANOPY.  -F.CHEN, 18-OCT-1994                                                  SFLX.1128
! ----------------------------------------------------------------------         SFLX.1129
            END IF                                                               SFLX.1130
            CMC2MS = CMC / DT                                                    SFLX.1131
            EC = MIN ( CMC2MS, EC )                                              SFLX.1132
         END IF                                                                  SFLX.1133
! ----------------------------------------------------------------------         SFLX.1134
! TOTAL UP EVAP AND TRANSP TYPES TO OBTAIN ACTUAL EVAPOTRANSP                    SFLX.1135
! ----------------------------------------------------------------------         SFLX.1136
      END IF                                                                     SFLX.1137
                                                                                 SFLX.1138
! ----------------------------------------------------------------------         SFLX.1139
! END SUBROUTINE EVAPO                                                           SFLX.1140
! ----------------------------------------------------------------------         SFLX.1141
      ETA1 = EDIR + ETT + EC                                                     SFLX.1142
      END                                                                        SFLX.1143
                                                                                 SFLX.1144
!      FUNCTION FRH2O (TKELV,SMC,SH2O,SMCMAX,BEXP,PSIS)                          SFLX.1145
                                                                                 SFLX.1146
      SUBROUTINE FRH2O (FREE,TKELV,SMC,SH2O,SMCMAX,BEXP,PSIS)                    SFLX.1147
                                                                                 SFLX.1148
! ----------------------------------------------------------------------         SFLX.1149
! SUBROUTINE FRH2O                                                               SFLX.1150
! FUNCTION FRH2O                                                                 SFLX.1151
! ----------------------------------------------------------------------         SFLX.1152
! CALCULATE AMOUNT OF SUPERCOOLED LIQUID SOIL WATER CONTENT IF                   SFLX.1153
! TEMPERATURE IS BELOW 273.15K (T0).  REQUIRES NEWTON-TYPE ITERATION TO          SFLX.1154
! SOLVE THE NONLINEAR IMPLICIT EQUATION GIVEN IN EQN 17 OF KOREN ET AL           SFLX.1155
! (1999, JGR, VOL 104(D16), 19569-19585).                                        SFLX.1156
! ----------------------------------------------------------------------         SFLX.1157
! NEW VERSION (JUNE 2001): MUCH FASTER AND MORE ACCURATE NEWTON                  SFLX.1158
! ITERATION ACHIEVED BY FIRST TAKING LOG OF EQN CITED ABOVE -- LESS THAN         SFLX.1159
! 4 (TYPICALLY 1 OR 2) ITERATIONS ACHIEVES CONVERGENCE.  ALSO, EXPLICIT          SFLX.1160
! 1-STEP SOLUTION OPTION FOR SPECIAL CASE OF PARAMETER CK=0, WHICH               SFLX.1161
! REDUCES THE ORIGINAL IMPLICIT EQUATION TO A SIMPLER EXPLICIT FORM,             SFLX.1162
! KNOWN AS THE "FLERCHINGER EQN". IMPROVED HANDLING OF SOLUTION IN THE           SFLX.1163
! LIMIT OF FREEZING POINT TEMPERATURE T0.                                        SFLX.1164
! ----------------------------------------------------------------------         SFLX.1165
! INPUT:                                                                         SFLX.1166
                                                                                 SFLX.1167
!   TKELV.........TEMPERATURE (Kelvin)                                           SFLX.1168
!   SMC...........TOTAL SOIL MOISTURE CONTENT (VOLUMETRIC)                       SFLX.1169
!   SH2O..........LIQUID SOIL MOISTURE CONTENT (VOLUMETRIC)                      SFLX.1170
!   SMCMAX........SATURATION SOIL MOISTURE CONTENT (FROM REDPRM)                 SFLX.1171
!   B.............SOIL TYPE "B" PARAMETER (FROM REDPRM)                          SFLX.1172
!   PSIS..........SATURATED SOIL MATRIC POTENTIAL (FROM REDPRM)                  SFLX.1173
                                                                                 SFLX.1174
! OUTPUT:                                                                        SFLX.1175
!   FRH2O.........SUPERCOOLED LIQUID WATER CONTENT                               SFLX.1176
!   FREE..........SUPERCOOLED LIQUID WATER CONTENT                               SFLX.1177
! ----------------------------------------------------------------------         SFLX.1178
      IMPLICIT NONE                                                              SFLX.1179
      REAL     BEXP                                                              SFLX.1180
      REAL     BLIM                                                              SFLX.1181
      REAL     BX                                                                SFLX.1182
      REAL     CK                                                                SFLX.1183
      REAL     DENOM                                                             SFLX.1184
      REAL     DF                                                                SFLX.1185
      REAL     DH2O                                                              SFLX.1186
      REAL     DICE                                                              SFLX.1187
      REAL     DSWL                                                              SFLX.1188
      REAL     ERROR                                                             SFLX.1189
!      REAL FRH2O                                                                SFLX.1190
      REAL     FK                                                                SFLX.1191
      REAL     FREE                                                              SFLX.1192
      REAL     GS                                                                SFLX.1193
      REAL     HLICE                                                             SFLX.1194
      REAL     PSIS                                                              SFLX.1195
      REAL     SH2O                                                              SFLX.1196
      REAL     SMC                                                               SFLX.1197
      REAL     SMCMAX                                                            SFLX.1198
      REAL     SWL                                                               SFLX.1199
      REAL     SWLK                                                              SFLX.1200
      REAL     TKELV                                                             SFLX.1201
                                                                                 SFLX.1202
      REAL     T0                                                                SFLX.1203
      INTEGER  NLOG                                                              SFLX.1204
                                                                                 SFLX.1205
      INTEGER  KCOUNT                                                            SFLX.1206
!      PARAMETER(CK = 0.0)                                                       SFLX.1207
      PARAMETER (CK = 8.0)                                                       SFLX.1208
      PARAMETER (BLIM = 5.5)                                                     SFLX.1209
                                                                                 SFLX.1210
      PARAMETER (ERROR = 0.005)                                                  SFLX.1211
      PARAMETER (HLICE = 3.335E5)                                                SFLX.1212
      PARAMETER (GS = 9.81)                                                      SFLX.1213
      PARAMETER (DICE = 920.0)                                                   SFLX.1214
      PARAMETER (DH2O = 1000.0)                                                  SFLX.1215
                                                                                 SFLX.1216
! ----------------------------------------------------------------------         SFLX.1217
! LIMITS ON PARAMETER B: B < 5.5  (use parameter BLIM)                           SFLX.1218
! SIMULATIONS SHOWED IF B > 5.5 UNFROZEN WATER CONTENT IS                        SFLX.1219
! NON-REALISTICALLY HIGH AT VERY LOW TEMPERATURES.                               SFLX.1220
! ----------------------------------------------------------------------         SFLX.1221
      PARAMETER (T0 = 273.15)                                                    SFLX.1222
      BX = BEXP                                                                  SFLX.1223
                                                                                 SFLX.1224
! ----------------------------------------------------------------------         SFLX.1225
! INITIALIZING ITERATIONS COUNTER AND ITERATIVE SOLUTION FLAG.                   SFLX.1226
! ----------------------------------------------------------------------         SFLX.1227
      IF (BEXP .gt. BLIM) BX = BLIM                                              SFLX.1228
      NLOG = 0                                                                   SFLX.1229
                                                                                 SFLX.1230
! ----------------------------------------------------------------------         SFLX.1231
!  IF TEMPERATURE NOT SIGNIFICANTLY BELOW FREEZING (T0), SH2O = SMC              SFLX.1232
! ----------------------------------------------------------------------         SFLX.1233
      KCOUNT = 0                                                                 SFLX.1234
!      FRH2O = SMC                                                               23DEC04.3703
      IF (TKELV .gt. (T0- 1.E-3)) THEN                                           SFLX.1236
          FREE = SMC                                                             23DEC04.3704
      ELSE                                                                       SFLX.1238
                                                                                 SFLX.1239
! ----------------------------------------------------------------------         SFLX.1240
! OPTION 1: ITERATED SOLUTION FOR NONZERO CK                                     SFLX.1241
! IN KOREN ET AL, JGR, 1999, EQN 17                                              SFLX.1242
! ----------------------------------------------------------------------         SFLX.1243
! INITIAL GUESS FOR SWL (frozen content)                                         SFLX.1244
! ----------------------------------------------------------------------         SFLX.1245
         IF (CK .ne. 0.0) THEN                                                   SFLX.1246
                                                                                 SFLX.1247
! ----------------------------------------------------------------------         SFLX.1248
! KEEP WITHIN BOUNDS.                                                            SFLX.1249
! ----------------------------------------------------------------------         SFLX.1250
            SWL = SMC - SH2O                                                     SFLX.1251
            IF (SWL .gt. (SMC -0.02)) SWL = SMC -0.02                            SFLX.1252
                                                                                 SFLX.1253
! ----------------------------------------------------------------------         SFLX.1254
!  START OF ITERATIONS                                                           SFLX.1255
! ----------------------------------------------------------------------         SFLX.1256
            IF (SWL .lt. 0.) SWL = 0.                                            SFLX.1257
 1001       Continue                                                             SFLX.1258
              IF ( .NOT. ( (NLOG .lt. 10) .AND. (KCOUNT .eq. 0) ) )     &        SFLX.1259
     &           goto 1002                                                       SFLX.1260
               NLOG = NLOG +1                                                    SFLX.1261
        DF = ALOG ( ( PSIS * GS / HLICE ) * ( ( 1. + CK * SWL )**2. ) * &        SFLX.1262
     &              ( SMCMAX / (SMC - SWL) )** BX) - ALOG ( - (         &        SFLX.1263
     &               TKELV - T0)/ TKELV)                                         SFLX.1264
               DENOM = 2. * CK / ( 1. + CK * SWL ) + BX / ( SMC - SWL )          SFLX.1265
! ----------------------------------------------------------------------         SFLX.1266
! BOUNDS USEFUL FOR MATHEMATICAL SOLUTION.                                       SFLX.1267
! ----------------------------------------------------------------------         SFLX.1268
               SWLK = SWL - DF / DENOM                                           SFLX.1269
               IF (SWLK .gt. (SMC -0.02)) SWLK = SMC - 0.02                      SFLX.1270
                                                                                 SFLX.1271
! ----------------------------------------------------------------------         SFLX.1272
! MATHEMATICAL SOLUTION BOUNDS APPLIED.                                          SFLX.1273
! ----------------------------------------------------------------------         SFLX.1274
               IF (SWLK .lt. 0.) SWLK = 0.                                       SFLX.1275
               DSWL = ABS (SWLK - SWL)                                           SFLX.1276
                                                                                 SFLX.1277
! ----------------------------------------------------------------------         SFLX.1278
! IF MORE THAN 10 ITERATIONS, USE EXPLICIT METHOD (CK=0 APPROX.)                 SFLX.1279
! WHEN DSWL LESS OR EQ. ERROR, NO MORE ITERATIONS REQUIRED.                      SFLX.1280
! ----------------------------------------------------------------------         SFLX.1281
               SWL = SWLK                                                        SFLX.1282
               IF ( DSWL .le. ERROR ) THEN                                       SFLX.1283
                    KCOUNT = KCOUNT +1                                           23DEC04.3705
               END IF                                                            SFLX.1285
! ----------------------------------------------------------------------         SFLX.1286
!  END OF ITERATIONS                                                             SFLX.1287
! ----------------------------------------------------------------------         SFLX.1288
! BOUNDS APPLIED WITHIN DO-BLOCK ARE VALID FOR PHYSICAL SOLUTION.                SFLX.1289
! ----------------------------------------------------------------------         SFLX.1290
!          FRH2O = SMC - SWL                                                     SFLX.1291
        goto 1001                                                                SFLX.1292
 1002   continue                                                                 SFLX.1293
! ----------------------------------------------------------------------         SFLX.1294
! END OPTION 1                                                                   SFLX.1295
! ----------------------------------------------------------------------         SFLX.1296
            FREE = SMC - SWL                                                     SFLX.1297
! ----------------------------------------------------------------------         SFLX.1298
! OPTION 2: EXPLICIT SOLUTION FOR FLERCHINGER EQ. i.e. CK=0                      SFLX.1299
! IN KOREN ET AL., JGR, 1999, EQN 17                                             SFLX.1300
! APPLY PHYSICAL BOUNDS TO FLERCHINGER SOLUTION                                  SFLX.1301
! ----------------------------------------------------------------------         SFLX.1302
         END IF                                                                  SFLX.1303
         IF (KCOUNT .eq. 0) THEN                                                 SFLX.1304
             PRINT *,'Flerchinger USEd in NEW version. Iterations=',NLOG         23DEC04.3706
                  FK = ( ( (HLICE / (GS * ( - PSIS)))*                           23DEC04.3707
     &                 ( (TKELV - T0)/ TKELV))** ( -1/ BX))* SMCMAX              23DEC04.3708
!            FRH2O = MIN (FK, SMC)                                               23DEC04.3709
             IF (FK .lt. 0.02) FK = 0.02                                         23DEC04.3710
! ----------------------------------------------------------------------         SFLX.1310
! END OPTION 2                                                                   SFLX.1311
! ----------------------------------------------------------------------         SFLX.1312
             FREE = MIN (FK, SMC)                                                23DEC04.3711
         END IF                                                                  SFLX.1314
! ----------------------------------------------------------------------         SFLX.1315
! END SUBROUTINE FRH2O                                                           SFLX.1316
! END FUNCTION FRH2O                                                             SFLX.1317
! ----------------------------------------------------------------------         SFLX.1318
      END IF                                                                     SFLX.1319
      END                                                                        SFLX.1320
                                                                                 SFLX.1321
!urban                                                                           23DEC04.3712
      SUBROUTINE HRT (RHSTS,STC,SMC,SMCMAX,NSOIL,ZSOIL,YY,ZZ1,          &        SFLX.1322
     &                 TBOT,ZBOT,PSISAT,SH2O,DT,BEXP,                   &        SFLX.1323
     &                 F1,DF1,QUARTZ,CSOIL,AI,BI,CI,VEGTYP)                      23DEC04.3713
                                                                                 SFLX.1325
! ----------------------------------------------------------------------         SFLX.1326
! SUBROUTINE HRT                                                                 SFLX.1327
! ----------------------------------------------------------------------         SFLX.1328
! CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY TERM OF THE SOIL            SFLX.1329
! THERMAL DIFFUSION EQUATION.  ALSO TO COMPUTE ( PREPARE ) THE MATRIX            SFLX.1330
! COEFFICIENTS FOR THE TRI-DIAGONAL MATRIX OF THE IMPLICIT TIME SCHEME.          SFLX.1331
! ----------------------------------------------------------------------         SFLX.1332
      IMPLICIT NONE                                                              SFLX.1333
      INTEGER  NSOLD                                                             SFLX.1334
                                                                                 SFLX.1335
      PARAMETER (NSOLD = 20)                                                     SFLX.1336
                                                                                 SFLX.1337
      LOGICAL  ITAVG                                                             SFLX.1338
      INTEGER  I                                                                 SFLX.1339
      INTEGER  K                                                                 SFLX.1340
                                                                                 SFLX.1341
! ----------------------------------------------------------------------         SFLX.1342
! DECLARE WORK ARRAYS NEEDED IN TRI-DIAGONAL IMPLICIT SOLVER                     SFLX.1343
! ----------------------------------------------------------------------         SFLX.1344
!urban                                                                           23DEC04.3714
      INTEGER  NSOIL, VEGTYP                                                     23DEC04.3715
      REAL     AI (NSOLD)                                                        SFLX.1346
      REAL     BI (NSOLD)                                                        SFLX.1347
                                                                                 SFLX.1348
! ----------------------------------------------------------------------         SFLX.1349
! DECLARATIONS                                                                   SFLX.1350
! ----------------------------------------------------------------------         SFLX.1351
      REAL     CI (NSOLD)                                                        SFLX.1352
      REAL     BEXP                                                              SFLX.1353
      REAL     CAIR                                                              SFLX.1354
      REAL     CH2O                                                              SFLX.1355
      REAL     CICE                                                              SFLX.1356
      REAL     CSOIL                                                             SFLX.1357
      REAL     CSOIL_LOC                                                         23DEC04.3716
      REAL     DDZ                                                               SFLX.1358
      REAL     DDZ2                                                              SFLX.1359
      REAL     DENOM                                                             SFLX.1360
      REAL     DF1                                                               SFLX.1361
      REAL     DF1N                                                              SFLX.1362
      REAL     DF1K                                                              SFLX.1363
      REAL     DT                                                                SFLX.1364
      REAL     DTSDZ                                                             SFLX.1365
      REAL     DTSDZ2                                                            SFLX.1366
      REAL     F1                                                                SFLX.1367
      REAL     HCPCT                                                             SFLX.1368
      REAL     PSISAT                                                            SFLX.1369
      REAL     QUARTZ                                                            SFLX.1370
      REAL     QTOT                                                              SFLX.1371
      REAL     RHSTS (NSOIL)                                                     SFLX.1372
      REAL     SSOIL                                                             SFLX.1373
      REAL     SICE                                                              SFLX.1374
      REAL     SMC (NSOIL)                                                       SFLX.1375
      REAL     SH2O (NSOIL)                                                      SFLX.1376
!      REAL SNKSRC                                                               SFLX.1377
      REAL     SMCMAX                                                            SFLX.1378
      REAL     STC (NSOIL)                                                       SFLX.1379
      REAL     T0                                                                SFLX.1380
      REAL     TAVG                                                              SFLX.1381
      REAL     TBK                                                               SFLX.1382
      REAL     TBK1                                                              SFLX.1383
      REAL     TBOT                                                              SFLX.1384
      REAL     ZBOT                                                              SFLX.1385
      REAL     TSNSR                                                             SFLX.1386
      REAL     TSURF                                                             SFLX.1387
      REAL     YY                                                                SFLX.1388
      REAL     ZSOIL (NSOIL)                                                     SFLX.1389
                                                                                 SFLX.1390
      REAL     ZZ1                                                               SFLX.1391
                                                                                 SFLX.1392
! ----------------------------------------------------------------------         SFLX.1393
! SET SPECIFIC HEAT CAPACITIES OF AIR, WATER, ICE, SOIL MINERAL                  SFLX.1394
! ----------------------------------------------------------------------         SFLX.1395
      PARAMETER (T0 = 273.15)                                                    SFLX.1396
      PARAMETER (CAIR = 1004.0)                                                  SFLX.1397
      PARAMETER (CICE = 2.106E6)                                                 SFLX.1398
      PARAMETER (CH2O = 4.2E6)                                                   SFLX.1399
! NOTE: CSOIL NOW SET IN ROUTINE REDPRM AND PASSED IN                            SFLX.1400
!      PARAMETER(CSOIL = 1.26E6)                                                 SFLX.1401
                                                                                 SFLX.1402
                                                                                 23DEC04.3717
! urban                                                                          23DEC04.3718
                                                                                 23DEC04.3719
        IF(VEGTYP.eq.1) then                                                     23DEC04.3720
            CSOIL_LOC=3.0E6                                                      23DEC04.3721
        ELSE                                                                     23DEC04.3722
            CSOIL_LOC=CSOIL                                                      23DEC04.3723
        ENDIF                                                                    23DEC04.3724
                                                                                 23DEC04.3725
! ----------------------------------------------------------------------         SFLX.1403
! INITIALIZE LOGICAL FOR SOIL LAYER TEMPERATURE AVERAGING.                       SFLX.1404
! ----------------------------------------------------------------------         SFLX.1405
       ITAVG = .TRUE.                                                            SFLX.1406
! ----------------------------------------------------------------------         SFLX.1407
! BEGIN SECTION FOR TOP SOIL LAYER                                               SFLX.1408
! ----------------------------------------------------------------------         SFLX.1409
! CALC THE HEAT CAPACITY OF THE TOP SOIL LAYER                                   SFLX.1410
! ----------------------------------------------------------------------         SFLX.1411
      HCPCT = SH2O(1)*CH2O+(1.0- SMCMAX)*CSOIL_LOC+(SMCMAX-SMC (1))     & 
     & * CAIR                                                           &        SFLX.1413
     &        + ( SMC (1) - SH2O (1) )* CICE                                     SFLX.1414
                                                                                 SFLX.1415
! ----------------------------------------------------------------------         SFLX.1416
! CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER                  SFLX.1417
! ----------------------------------------------------------------------         SFLX.1418
      DDZ = 1.0 / ( -0.5 * ZSOIL (2) )                                           SFLX.1419
      AI (1) = 0.0                                                               SFLX.1420
      CI (1) = (DF1 * DDZ) / (ZSOIL (1) * HCPCT)                                 SFLX.1421
                                                                                 SFLX.1422
! ----------------------------------------------------------------------         SFLX.1423
! CALCULATE THE VERTICAL SOIL TEMP GRADIENT BTWN THE 1ST AND 2ND SOIL            SFLX.1424
! LAYERS.  THEN CALCULATE THE SUBSURFACE HEAT FLUX. USE THE TEMP                 SFLX.1425
! GRADIENT AND SUBSFC HEAT FLUX TO CALC "RIGHT-HAND SIDE TENDENCY                SFLX.1426
! TERMS", OR "RHSTS", FOR TOP SOIL LAYER.                                        SFLX.1427
! ----------------------------------------------------------------------         SFLX.1428
      BI (1) = - CI (1) + DF1 / (0.5 * ZSOIL (1) * ZSOIL (1)* HCPCT *    &       SFLX.1429
     & ZZ1)                                                                      SFLX.1430
      DTSDZ = (STC (1) - STC (2)) / ( -0.5 * ZSOIL (2))                          SFLX.1431
      SSOIL = DF1 * (STC (1) - YY) / (0.5 * ZSOIL (1) * ZZ1)                     SFLX.1432
!      RHSTS(1) = (DF1 * DTSDZ - SSOIL) / (ZSOIL(1) * HCPCT)                     SFLX.1433
      DENOM = (ZSOIL (1) * HCPCT)                                                SFLX.1434
                                                                                 SFLX.1435
! ----------------------------------------------------------------------         SFLX.1436
! NEXT CAPTURE THE VERTICAL DIFFERENCE OF THE HEAT FLUX AT TOP AND               SFLX.1437
! BOTTOM OF FIRST SOIL LAYER FOR USE IN HEAT FLUX CONSTRAINT APPLIED TO          SFLX.1438
! POTENTIAL SOIL FREEZING/THAWING IN ROUTINE SNKSRC.                             SFLX.1439
! ----------------------------------------------------------------------         SFLX.1440
!      QTOT = SSOIL - DF1*DTSDZ                                                  SFLX.1441
      RHSTS (1) = (DF1 * DTSDZ - SSOIL) / DENOM                                  SFLX.1442
                                                                                 SFLX.1443
! ----------------------------------------------------------------------         SFLX.1444
! CALCULATE FROZEN WATER CONTENT IN 1ST SOIL LAYER.                              SFLX.1445
! ----------------------------------------------------------------------         SFLX.1446
      QTOT = -1.0* RHSTS (1)* DENOM                                              SFLX.1447
                                                                                 SFLX.1448
! ----------------------------------------------------------------------         SFLX.1449
! IF TEMPERATURE AVERAGING INVOKED (ITAVG=TRUE; ELSE SKIP):                      SFLX.1450
! SET TEMP "TSURF" AT TOP OF SOIL COLUMN (FOR USE IN FREEZING SOIL               SFLX.1451
! PHYSICS LATER IN FUNCTION SUBROUTINE SNKSRC).  IF SNOWPACK CONTENT IS          SFLX.1452
! ZERO, THEN TSURF EXPRESSION BELOW GIVES TSURF = SKIN TEMP.  IF                 SFLX.1453
! SNOWPACK IS NONZERO (HENCE ARGUMENT ZZ1=1), THEN TSURF EXPRESSION              SFLX.1454
! BELOW YIELDS SOIL COLUMN TOP TEMPERATURE UNDER SNOWPACK.  THEN                 SFLX.1455
! CALCULATE TEMPERATURE AT BOTTOM INTERFACE OF 1ST SOIL LAYER FOR USE            SFLX.1456
! LATER IN FUNCTION SUBROUTINE SNKSRC                                            SFLX.1457
! ----------------------------------------------------------------------         SFLX.1458
      SICE = SMC (1) - SH2O (1)                                                  SFLX.1459
      IF (ITAVG) THEN                                                            SFLX.1460
         TSURF = (YY + (ZZ1-1) * STC (1)) / ZZ1                                  SFLX.1461
! ----------------------------------------------------------------------         SFLX.1462
! IF FROZEN WATER PRESENT OR ANY OF LAYER-1 MID-POINT OR BOUNDING                SFLX.1463
! INTERFACE TEMPERATURES BELOW FREEZING, THEN CALL SNKSRC TO                     SFLX.1464
! COMPUTE HEAT SOURCE/SINK (AND CHANGE IN FROZEN WATER CONTENT)                  SFLX.1465
! DUE TO POSSIBLE SOIL WATER PHASE CHANGE                                        SFLX.1466
! ----------------------------------------------------------------------         SFLX.1467
         CALL TBND (STC (1),STC (2),ZSOIL,ZBOT,1,NSOIL,TBK)                      SFLX.1468
         IF ( (SICE .gt. 0.) .OR. (STC (1) .lt. T0) .OR.                      &  SFLX.1469
     &      (TSURF .lt. T0) .OR. (TBK .lt. T0) ) THEN                            SFLX.1470
!          TSNSR = SNKSRC (TAVG,SMC(1),SH2O(1),                                  SFLX.1471
            CALL TMPAVG (TAVG,TSURF,STC (1),TBK,ZSOIL,NSOIL,1)                   SFLX.1472
            CALL SNKSRC (TSNSR,TAVG,SMC (1),SH2O (1),                         &  SFLX.1473
     &                    ZSOIL,NSOIL,SMCMAX,PSISAT,BEXP,DT,1,QTOT)              SFLX.1474
!          RHSTS(1) = RHSTS(1) - TSNSR / ( ZSOIL(1) * HCPCT )                    SFLX.1475
            RHSTS (1) = RHSTS (1) - TSNSR / DENOM                                SFLX.1476
         END IF                                                                  SFLX.1477
      ELSE                                                                       SFLX.1478
!          TSNSR = SNKSRC (STC(1),SMC(1),SH2O(1),                                SFLX.1479
         IF ( (SICE .gt. 0.) .OR. (STC (1) .lt. T0) ) THEN                       SFLX.1480
            CALL SNKSRC (TSNSR,STC (1),SMC (1),SH2O (1),                      &  SFLX.1481
     &                    ZSOIL,NSOIL,SMCMAX,PSISAT,BEXP,DT,1,QTOT)              SFLX.1482
!          RHSTS(1) = RHSTS(1) - TSNSR / ( ZSOIL(1) * HCPCT )                    SFLX.1483
            RHSTS (1) = RHSTS (1) - TSNSR / DENOM                                SFLX.1484
         END IF                                                                  SFLX.1485
! ----------------------------------------------------------------------         SFLX.1486
! THIS ENDS SECTION FOR TOP SOIL LAYER.                                          SFLX.1487
! ----------------------------------------------------------------------         SFLX.1488
! INITIALIZE DDZ2                                                                SFLX.1489
! ----------------------------------------------------------------------         SFLX.1490
      END IF                                                                     SFLX.1491
                                                                                 SFLX.1492
! ----------------------------------------------------------------------         SFLX.1493
! LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABOVE PROCESS               SFLX.1494
! (EXCEPT SUBSFC OR "GROUND" HEAT FLUX NOT REPEATED IN LOWER LAYERS)             SFLX.1495
! ----------------------------------------------------------------------         SFLX.1496
      DDZ2 = 0.0                                                                 SFLX.1497
      DF1K = DF1                                                                 SFLX.1498
                                                                                 SFLX.1499
! ----------------------------------------------------------------------         SFLX.1500
! CALCULATE HEAT CAPACITY FOR THIS SOIL LAYER.                                   SFLX.1501
! ----------------------------------------------------------------------         SFLX.1502
      DO K = 2,NSOIL                                                             SFLX.1503
         HCPCT = SH2O(K)*CH2O+(1.0-SMCMAX)*CSOIL_LOC+(SMCMAX-SMC(       & 
     &          K))* CAIR                                               &        SFLX.1505
     &             + ( SMC (K) - SH2O (K) )* CICE                                SFLX.1506
! ----------------------------------------------------------------------         SFLX.1507
! THIS SECTION FOR LAYER 2 OR GREATER, BUT NOT LAST LAYER.                       SFLX.1508
! ----------------------------------------------------------------------         SFLX.1509
! CALCULATE THERMAL DIFFUSIVITY FOR THIS LAYER.                                  SFLX.1510
! ----------------------------------------------------------------------         SFLX.1511
         IF (K .ne. NSOIL) THEN                                                  SFLX.1512
                                                                                 SFLX.1513
! ----------------------------------------------------------------------         SFLX.1514
! CALC THE VERTICAL SOIL TEMP GRADIENT THRU THIS LAYER                           SFLX.1515
! ----------------------------------------------------------------------         SFLX.1516
            CALL TDFCND (DF1N,SMC (K),QUARTZ,SMCMAX,SH2O (K))                    SFLX.1517
                                                                                 23DEC04.3726
! urban                                                                          23DEC04.3727
                                                                                 23DEC04.3728
            IF(VEGTYP.eq.1) DF1N = 3.24                                          23DEC04.3729
                                                                                 23DEC04.3730
            DENOM = 0.5 * ( ZSOIL (K -1) - ZSOIL (K +1) )                        SFLX.1518
                                                                                 SFLX.1519
! ----------------------------------------------------------------------         SFLX.1520
! CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT                    SFLX.1521
! ----------------------------------------------------------------------         SFLX.1522
            DTSDZ2 = ( STC (K) - STC (K +1) ) / DENOM                            SFLX.1523
            DDZ2 = 2. / (ZSOIL (K -1) - ZSOIL (K +1))                            SFLX.1524
                                                                                 SFLX.1525
! ----------------------------------------------------------------------         SFLX.1526
! IF TEMPERATURE AVERAGING INVOKED (ITAVG=TRUE; ELSE SKIP):  CALCULATE           SFLX.1527
! TEMP AT BOTTOM OF LAYER.                                                       SFLX.1528
! ----------------------------------------------------------------------         SFLX.1529
            CI (K) = - DF1N * DDZ2 / ( (ZSOIL (K -1) - ZSOIL (K)) *      &       SFLX.1530
     & HCPCT)                                                                    SFLX.1531
            IF (ITAVG) THEN                                                      SFLX.1532
               CALL TBND (STC (K),STC (K +1),ZSOIL,ZBOT,K,NSOIL,TBK1)            SFLX.1533
            END IF                                                               SFLX.1534
                                                                                 SFLX.1535
! ----------------------------------------------------------------------         SFLX.1536
! SPECIAL CASE OF BOTTOM SOIL LAYER:  CALCULATE THERMAL DIFFUSIVITY FOR          SFLX.1537
! BOTTOM LAYER.                                                                  SFLX.1538
! ----------------------------------------------------------------------         SFLX.1539
         ELSE                                                                    SFLX.1540
                                                                                 SFLX.1541
! ----------------------------------------------------------------------         SFLX.1542
! CALC THE VERTICAL SOIL TEMP GRADIENT THRU BOTTOM LAYER.                        SFLX.1543
! ----------------------------------------------------------------------         SFLX.1544
            CALL TDFCND (DF1N,SMC (K),QUARTZ,SMCMAX,SH2O (K))                    SFLX.1545
                                                                                 23DEC04.3731
! urban                                                                          23DEC04.3732
                                                                                 23DEC04.3733
       IF(VEGTYP.eq.1) DF1N = 3.24                                               23DEC04.3734
                                                                                 23DEC04.3735
            DENOM = .5 * (ZSOIL (K -1) + ZSOIL (K)) - ZBOT                       SFLX.1546
                                                                                 SFLX.1547
! ----------------------------------------------------------------------         SFLX.1548
! SET MATRIX COEF, CI TO ZERO IF BOTTOM LAYER.                                   SFLX.1549
! ----------------------------------------------------------------------         SFLX.1550
            DTSDZ2 = (STC (K) - TBOT) / DENOM                                    SFLX.1551
                                                                                 SFLX.1552
! ----------------------------------------------------------------------         SFLX.1553
! IF TEMPERATURE AVERAGING INVOKED (ITAVG=TRUE; ELSE SKIP):  CALCULATE           SFLX.1554
! TEMP AT BOTTOM OF LAST LAYER.                                                  SFLX.1555
! ----------------------------------------------------------------------         SFLX.1556
            CI (K) = 0.                                                          SFLX.1557
            IF (ITAVG) THEN                                                      SFLX.1558
               CALL TBND (STC (K),TBOT,ZSOIL,ZBOT,K,NSOIL,TBK1)                  SFLX.1559
            END IF                                                               SFLX.1560
! ----------------------------------------------------------------------         SFLX.1561
! THIS ENDS SPECIAL LOOP FOR BOTTOM LAYER.                                       SFLX.1562
! ----------------------------------------------------------------------         SFLX.1563
! CALCULATE RHSTS FOR THIS LAYER AFTER CALC'NG A PARTIAL PRODUCT.                SFLX.1564
! ----------------------------------------------------------------------         SFLX.1565
         END IF                                                                  SFLX.1566
         DENOM = ( ZSOIL (K) - ZSOIL (K -1) ) * HCPCT                            SFLX.1567
         RHSTS (K) = ( DF1N * DTSDZ2- DF1K * DTSDZ ) / DENOM                     SFLX.1568
         QTOT = -1.0* DENOM * RHSTS (K)                                          SFLX.1569
                                                                                 SFLX.1570
         SICE = SMC (K) - SH2O (K)                                               SFLX.1571
         IF (ITAVG) THEN                                                         SFLX.1572
            CALL TMPAVG (TAVG,TBK,STC (K),TBK1,ZSOIL,NSOIL,K)                    SFLX.1573
!            TSNSR = SNKSRC(TAVG,SMC(K),SH2O(K),ZSOIL,NSOIL,                     SFLX.1574
            IF ( (SICE .gt. 0.) .OR. (STC (K) .lt. T0) .OR.                   &  SFLX.1575
     &         (TBK .lt. T0) .OR. (TBK1 .lt. T0) ) THEN                          SFLX.1576
               CALL SNKSRC (TSNSR,TAVG,SMC (K),SH2O (K),ZSOIL,NSOIL,    &        SFLX.1577
     &                       SMCMAX,PSISAT,BEXP,DT,K,QTOT)                       SFLX.1578
               RHSTS (K) = RHSTS (K) - TSNSR / DENOM                             SFLX.1579
            END IF                                                               SFLX.1580
         ELSE                                                                    SFLX.1581
!            TSNSR = SNKSRC(STC(K),SMC(K),SH2O(K),ZSOIL,NSOIL,                   SFLX.1582
            IF ( (SICE .gt. 0.) .OR. (STC (K) .lt. T0) ) THEN                    SFLX.1583
               CALL SNKSRC (TSNSR,STC (K),SMC (K),SH2O (K),ZSOIL,NSOIL, &        SFLX.1584
     &                       SMCMAX,PSISAT,BEXP,DT,K,QTOT)                       SFLX.1585
               RHSTS (K) = RHSTS (K) - TSNSR / DENOM                             SFLX.1586
            END IF                                                               SFLX.1587
! ----------------------------------------------------------------------         SFLX.1588
! CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER.                                  SFLX.1589
! ----------------------------------------------------------------------         SFLX.1590
         END IF                                                                  SFLX.1591
         AI (K) = - DF1 * DDZ / ( (ZSOIL (K -1) - ZSOIL (K)) * HCPCT)            SFLX.1592
                                                                                 SFLX.1593
! ----------------------------------------------------------------------         SFLX.1594
! RESET VALUES OF DF1, DTSDZ, DDZ, AND TBK FOR LOOP TO NEXT SOIL LAYER.          SFLX.1595
! ----------------------------------------------------------------------         SFLX.1596
         BI (K) = - (AI (K) + CI (K))                                            SFLX.1597
         TBK = TBK1                                                              SFLX.1598
         DF1K = DF1N                                                             SFLX.1599
         DTSDZ = DTSDZ2                                                          SFLX.1600
         DDZ = DDZ2                                                              SFLX.1601
! ----------------------------------------------------------------------         SFLX.1602
! END SUBROUTINE HRT                                                             SFLX.1603
! ----------------------------------------------------------------------         SFLX.1604
      END DO                                                                     SFLX.1605
      END                                                                        SFLX.1606
                                                                                 SFLX.1607
                                                                                 SFLX.1608
      SUBROUTINE HRTICE (RHSTS,STC,NSOIL,ZSOIL,YY,ZZ1,DF1,AI,BI,CI)              SFLX.1609
                                                                                 SFLX.1610
! ----------------------------------------------------------------------         SFLX.1611
! SUBROUTINE HRTICE                                                              SFLX.1612
! ----------------------------------------------------------------------         SFLX.1613
! CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY TERM OF THE SOIL            SFLX.1614
! THERMAL DIFFUSION EQUATION IN THE CASE OF SEA-ICE PACK.  ALSO TO               SFLX.1615
! COMPUTE (PREPARE) THE MATRIX COEFFICIENTS FOR THE TRI-DIAGONAL MATRIX          SFLX.1616
! OF THE IMPLICIT TIME SCHEME.                                                   SFLX.1617
! ----------------------------------------------------------------------         SFLX.1618
      IMPLICIT NONE                                                              SFLX.1619
      INTEGER  NSOLD                                                             SFLX.1620
                                                                                 SFLX.1621
      PARAMETER (NSOLD = 20)                                                     SFLX.1622
      INTEGER  K                                                                 SFLX.1623
                                                                                 SFLX.1624
      INTEGER  NSOIL                                                             SFLX.1625
      REAL     AI (NSOLD)                                                        SFLX.1626
      REAL     BI (NSOLD)                                                        SFLX.1627
                                                                                 SFLX.1628
      REAL     CI (NSOLD)                                                        SFLX.1629
      REAL     DDZ                                                               SFLX.1630
      REAL     DDZ2                                                              SFLX.1631
      REAL     DENOM                                                             SFLX.1632
      REAL     DF1                                                               SFLX.1633
      REAL     DTSDZ                                                             SFLX.1634
      REAL     DTSDZ2                                                            SFLX.1635
      REAL     HCPCT                                                             SFLX.1636
      REAL     RHSTS (NSOIL)                                                     SFLX.1637
      REAL     SSOIL                                                             SFLX.1638
      REAL     STC (NSOIL)                                                       SFLX.1639
      REAL     TBOT                                                              SFLX.1640
      REAL     YY                                                                SFLX.1641
      REAL     ZBOT                                                              SFLX.1642
      REAL     ZSOIL (NSOIL)                                                     SFLX.1643
                                                                                 SFLX.1644
      REAL     ZZ1                                                               SFLX.1645
                                                                                 SFLX.1646
! ----------------------------------------------------------------------         SFLX.1647
! SET A NOMINAL UNIVERSAL VALUE OF THE SEA-ICE SPECIFIC HEAT CAPACITY,           SFLX.1648
! HCPCT = 1880.0*917.0.                                                          SFLX.1649
! ----------------------------------------------------------------------         SFLX.1650
      DATA TBOT /271.16/                                                         SFLX.1651
                                                                                 SFLX.1652
! ----------------------------------------------------------------------         SFLX.1653
! THE INPUT ARGUMENT DF1 IS A UNIVERSALLY CONSTANT VALUE OF SEA-ICE              SFLX.1654
! THERMAL DIFFUSIVITY, SET IN ROUTINE SNOPAC AS DF1 = 2.2.                       SFLX.1655
! ----------------------------------------------------------------------         SFLX.1656
! SET ICE PACK DEPTH.  USE TBOT AS ICE PACK LOWER BOUNDARY TEMPERATURE           SFLX.1657
! (THAT OF UNFROZEN SEA WATER AT BOTTOM OF SEA ICE PACK).  ASSUME ICE            SFLX.1658
! PACK IS OF N=NSOIL LAYERS SPANNING A UNIFORM CONSTANT ICE PACK                 SFLX.1659
! THICKNESS AS DEFINED BY ZSOIL(NSOIL) IN ROUTINE SFLX.                          SFLX.1660
! ----------------------------------------------------------------------         SFLX.1661
      PARAMETER (HCPCT = 1.72396E+6)                                             SFLX.1662
! ----------------------------------------------------------------------         SFLX.1663
! CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER                  SFLX.1664
! ----------------------------------------------------------------------         SFLX.1665
      ZBOT = ZSOIL (NSOIL)                                                       SFLX.1666
      DDZ = 1.0 / ( -0.5 * ZSOIL (2) )                                           SFLX.1667
      AI (1) = 0.0                                                               SFLX.1668
      CI (1) = (DF1 * DDZ) / (ZSOIL (1) * HCPCT)                                 SFLX.1669
                                                                                 SFLX.1670
! ----------------------------------------------------------------------         SFLX.1671
! CALC THE VERTICAL SOIL TEMP GRADIENT BTWN THE TOP AND 2ND SOIL LAYERS.         SFLX.1672
! RECALC/ADJUST THE SOIL HEAT FLUX.  USE THE GRADIENT AND FLUX TO CALC           SFLX.1673
! RHSTS FOR THE TOP SOIL LAYER.                                                  SFLX.1674
! ----------------------------------------------------------------------         SFLX.1675
      BI (1) = - CI (1) + DF1/ (0.5 * ZSOIL (1) * ZSOIL (1) * HCPCT *    &       SFLX.1676
     & ZZ1)                                                                      SFLX.1677
      DTSDZ = ( STC (1) - STC (2) ) / ( -0.5 * ZSOIL (2) )                       SFLX.1678
      SSOIL = DF1 * ( STC (1) - YY ) / ( 0.5 * ZSOIL (1) * ZZ1 )                 SFLX.1679
                                                                                 SFLX.1680
! ----------------------------------------------------------------------         SFLX.1681
! INITIALIZE DDZ2                                                                SFLX.1682
! ----------------------------------------------------------------------         SFLX.1683
      RHSTS (1) = ( DF1 * DTSDZ - SSOIL ) / ( ZSOIL (1) * HCPCT )                SFLX.1684
                                                                                 SFLX.1685
! ----------------------------------------------------------------------         SFLX.1686
! LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABOVE PROCESS               SFLX.1687
! ----------------------------------------------------------------------         SFLX.1688
      DDZ2 = 0.0                                                                 SFLX.1689
      DO K = 2,NSOIL                                                             SFLX.1690
                                                                                 SFLX.1691
! ----------------------------------------------------------------------         SFLX.1692
! CALC THE VERTICAL SOIL TEMP GRADIENT THRU THIS LAYER.                          SFLX.1693
! ----------------------------------------------------------------------         SFLX.1694
         IF (K .ne. NSOIL) THEN                                                  SFLX.1695
            DENOM = 0.5 * ( ZSOIL (K -1) - ZSOIL (K +1) )                        SFLX.1696
                                                                                 SFLX.1697
! ----------------------------------------------------------------------         SFLX.1698
! CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT.                   SFLX.1699
! ----------------------------------------------------------------------         SFLX.1700
            DTSDZ2 = ( STC (K) - STC (K +1) ) / DENOM                            SFLX.1701
            DDZ2 = 2. / (ZSOIL (K -1) - ZSOIL (K +1))                            SFLX.1702
            CI (K) = - DF1 * DDZ2 / ( (ZSOIL (K -1) - ZSOIL (K))*HCPCT)          SFLX.1703
                                                                                 SFLX.1704
! ----------------------------------------------------------------------         SFLX.1705
! CALC THE VERTICAL SOIL TEMP GRADIENT THRU THE LOWEST LAYER.                    SFLX.1706
! ----------------------------------------------------------------------         SFLX.1707
         ELSE                                                                    SFLX.1708
                                                                                 SFLX.1709
! ----------------------------------------------------------------------         SFLX.1710
! SET MATRIX COEF, CI TO ZERO.                                                   SFLX.1711
! ----------------------------------------------------------------------         SFLX.1712
            DTSDZ2 = (STC (K) - TBOT)/ (.5 * (ZSOIL (K -1) + ZSOIL (K)) &        SFLX.1713
     &               - ZBOT)                                                     SFLX.1714
            CI (K) = 0.                                                          SFLX.1715
! ----------------------------------------------------------------------         SFLX.1716
! CALC RHSTS FOR THIS LAYER AFTER CALC'NG A PARTIAL PRODUCT.                     SFLX.1717
! ----------------------------------------------------------------------         SFLX.1718
         END IF                                                                  SFLX.1719
         DENOM = ( ZSOIL (K) - ZSOIL (K -1) ) * HCPCT                            SFLX.1720
                                                                                 SFLX.1721
! ----------------------------------------------------------------------         SFLX.1722
! CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER.                                  SFLX.1723
! ----------------------------------------------------------------------         SFLX.1724
         RHSTS (K) = ( DF1 * DTSDZ2- DF1 * DTSDZ ) / DENOM                       SFLX.1725
         AI (K) = - DF1 * DDZ / ( (ZSOIL (K -1) - ZSOIL (K)) * HCPCT)            SFLX.1726
                                                                                 SFLX.1727
! ----------------------------------------------------------------------         SFLX.1728
! RESET VALUES OF DTSDZ AND DDZ FOR LOOP TO NEXT SOIL LYR.                       SFLX.1729
! ----------------------------------------------------------------------         SFLX.1730
         BI (K) = - (AI (K) + CI (K))                                            SFLX.1731
         DTSDZ = DTSDZ2                                                          SFLX.1732
                                                                                 SFLX.1733
         DDZ = DDZ2                                                              SFLX.1734
! ----------------------------------------------------------------------         SFLX.1735
! END SUBROUTINE HRTICE                                                          SFLX.1736
! ----------------------------------------------------------------------         SFLX.1737
      END DO                                                                     SFLX.1738
      END                                                                        SFLX.1739
                                                                                 SFLX.1740
                                                                                 SFLX.1741
      SUBROUTINE HSTEP (STCOUT,STCIN,RHSTS,DT,NSOIL,AI,BI,CI)                    SFLX.1742
                                                                                 SFLX.1743
! ----------------------------------------------------------------------         SFLX.1744
! SUBROUTINE HSTEP                                                               SFLX.1745
! ----------------------------------------------------------------------         SFLX.1746
! CALCULATE/UPDATE THE SOIL TEMPERATURE FIELD.                                   SFLX.1747
! ----------------------------------------------------------------------         SFLX.1748
      IMPLICIT NONE                                                              SFLX.1749
      INTEGER  NSOLD                                                             SFLX.1750
                                                                                 SFLX.1751
      PARAMETER (NSOLD = 20)                                                     SFLX.1752
      INTEGER  K                                                                 SFLX.1753
                                                                                 SFLX.1754
      INTEGER  NSOIL                                                             SFLX.1755
      REAL     AI (NSOLD)                                                        SFLX.1756
      REAL     BI (NSOLD)                                                        SFLX.1757
      REAL     CI (NSOLD)                                                        SFLX.1758
      REAL     CIin (NSOLD)                                                      SFLX.1759
      REAL     DT                                                                SFLX.1760
      REAL     RHSTS (NSOIL)                                                     SFLX.1761
      REAL     RHSTSin (NSOIL)                                                   SFLX.1762
      REAL     STCIN (NSOIL)                                                     SFLX.1763
                                                                                 SFLX.1764
! ----------------------------------------------------------------------         SFLX.1765
! CREATE FINITE DIFFERENCE VALUES FOR USE IN ROSR12 ROUTINE                      SFLX.1766
! ----------------------------------------------------------------------         SFLX.1767
      REAL     STCOUT (NSOIL)                                                    SFLX.1768
      DO K = 1,NSOIL                                                             SFLX.1769
         RHSTS (K) = RHSTS (K) * DT                                              SFLX.1770
         AI (K) = AI (K) * DT                                                    SFLX.1771
         BI (K) = 1. + BI (K) * DT                                               SFLX.1772
         CI (K) = CI (K) * DT                                                    SFLX.1773
! ----------------------------------------------------------------------         SFLX.1774
! COPY VALUES FOR INPUT VARIABLES BEFORE CALL TO ROSR12                          SFLX.1775
! ----------------------------------------------------------------------         SFLX.1776
      END DO                                                                     SFLX.1777
      DO K = 1,NSOIL                                                             SFLX.1778
         RHSTSin (K) = RHSTS (K)                                                 SFLX.1779
      END DO                                                                     SFLX.1780
      DO K = 1,NSOLD                                                             SFLX.1781
         CIin (K) = CI (K)                                                       SFLX.1782
! ----------------------------------------------------------------------         SFLX.1783
! SOLVE THE TRI-DIAGONAL MATRIX EQUATION                                         SFLX.1784
! ----------------------------------------------------------------------         SFLX.1785
      END DO                                                                     SFLX.1786
                                                                                 SFLX.1787
! ----------------------------------------------------------------------         SFLX.1788
! CALC/UPDATE THE SOIL TEMPS USING MATRIX SOLUTION                               SFLX.1789
! ----------------------------------------------------------------------         SFLX.1790
      CALL ROSR12 (CI,AI,BI,CIin,RHSTSin,RHSTS,NSOIL)                            SFLX.1791
      DO K = 1,NSOIL                                                             SFLX.1792
         STCOUT (K) = STCIN (K) + CI (K)                                         SFLX.1793
! ----------------------------------------------------------------------         SFLX.1794
! END SUBROUTINE HSTEP                                                           SFLX.1795
! ----------------------------------------------------------------------         SFLX.1796
      END DO                                                                     SFLX.1797
      END                                                                        SFLX.1798
                                                                                 SFLX.1799
!urban                                                                           23DEC04.3736
      SUBROUTINE NOPAC (ETP,ETA,PRCP,SMC,SMCMAX,SMCWLT,                 &        SFLX.1800
     &                   SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,SHDFAC,      &        SFLX.1801
     &                   SBETA,Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,EMISSI,    &        23DEC04.3737
     &                   SSOIL,                                         &        23DEC04.3738
     &                   STC,EPSCA,BEXP,PC,RCH,RR,CFACTR,               &        SFLX.1803
     &                   SH2O,SLOPE,KDT,FRZFACT,PSISAT,ZSOIL,           &        SFLX.1804
     &                   DKSAT,DWSAT,TBOT,ZBOT,RUNOFF1,RUNOFF2,         &        SFLX.1805
     &                   RUNOFF3,EDIR,EC,ET,ETT,NROOT,ICE,RTDIS,        &        SFLX.1806
     &                   QUARTZ,FXEXP,CSOIL,                            &        SFLX.1807
     &                   BETA,DRIP,DEW,FLX1,FLX2,FLX3,VEGTYP)                    23DEC04.3739
                                                                                 SFLX.1809
! ----------------------------------------------------------------------         SFLX.1810
! SUBROUTINE NOPAC                                                               SFLX.1811
! ----------------------------------------------------------------------         SFLX.1812
! CALCULATE SOIL MOISTURE AND HEAT FLUX VALUES AND UPDATE SOIL MOISTURE          SFLX.1813
! CONTENT AND SOIL HEAT CONTENT VALUES FOR THE CASE WHEN NO SNOW PACK IS         SFLX.1814
! PRESENT.                                                                       SFLX.1815
! ----------------------------------------------------------------------         SFLX.1816
      IMPLICIT NONE                                                              SFLX.1817
      INTEGER  ICE                                                               SFLX.1818
      INTEGER  NROOT                                                             SFLX.1819
      INTEGER  K                                                                 SFLX.1820
                                                                                 SFLX.1821
      INTEGER  NSOIL                                                             SFLX.1822
      INTEGER  VEGTYP                                                            23DEC04.3740
      REAL     BEXP                                                              SFLX.1823
      REAL     BETA                                                              SFLX.1824
      REAL     CFACTR                                                            SFLX.1825
      REAL     CMC                                                               SFLX.1826
      REAL     CMCMAX                                                            SFLX.1827
      REAL     CP                                                                SFLX.1828
      REAL     CSOIL                                                             SFLX.1829
      REAL     DEW                                                               SFLX.1830
      REAL     DF1                                                               SFLX.1831
      REAL     DKSAT                                                             SFLX.1832
      REAL     DRIP                                                              SFLX.1833
      REAL     DT                                                                SFLX.1834
      REAL     DWSAT                                                             SFLX.1835
      REAL     EC                                                                SFLX.1836
      REAL     EDIR                                                              SFLX.1837
      REAL     EPSCA                                                             SFLX.1838
      REAL     ETA                                                               SFLX.1839
      REAL     ETA1                                                              SFLX.1840
      REAL     ETP                                                               SFLX.1841
      REAL     ETP1                                                              SFLX.1842
      REAL     ET (NSOIL)                                                        SFLX.1843
      REAL     ETT                                                               SFLX.1844
      REAL     FDOWN                                                             SFLX.1845
      REAL     F1                                                                SFLX.1846
      REAL     FXEXP                                                             SFLX.1847
      REAL     FLX1                                                              SFLX.1848
      REAL     FLX2                                                              SFLX.1849
      REAL     FLX3                                                              SFLX.1850
      REAL     FRZFACT                                                           SFLX.1851
      REAL     KDT                                                               SFLX.1852
      REAL     PC                                                                SFLX.1853
      REAL     PRCP                                                              SFLX.1854
      REAL     PRCP1                                                             SFLX.1855
      REAL     PSISAT                                                            SFLX.1856
      REAL     Q2                                                                SFLX.1857
      REAL     QUARTZ                                                            SFLX.1858
      REAL     RCH                                                               SFLX.1859
      REAL     RR                                                                SFLX.1860
      REAL     RTDIS (NSOIL)                                                     SFLX.1861
      REAL     RUNOFF1                                                           SFLX.1862
      REAL     RUNOFF2                                                           SFLX.1863
      REAL     RUNOFF3                                                           SFLX.1864
      REAL     SSOIL                                                             SFLX.1865
      REAL     SBETA                                                             SFLX.1866
      REAL     SFCTMP                                                            SFLX.1867
      REAL     SHDFAC                                                            SFLX.1868
      REAL     SH2O (NSOIL)                                                      SFLX.1869
      REAL     SIGMA                                                             SFLX.1870
      REAL     SLOPE                                                             SFLX.1871
      REAL     SMC (NSOIL)                                                       SFLX.1872
      REAL     SMCDRY                                                            SFLX.1873
      REAL     SMCMAX                                                            SFLX.1874
      REAL     SMCREF                                                            SFLX.1875
      REAL     SMCWLT                                                            SFLX.1876
      REAL     STC (NSOIL)                                                       SFLX.1877
      REAL     T1                                                                SFLX.1878
      REAL     T24                                                               SFLX.1879
      REAL     TBOT                                                              SFLX.1880
      REAL     TH2                                                               SFLX.1881
      REAL     YY                                                                SFLX.1882
      REAL     YYNUM                                                             SFLX.1883
      REAL     ZBOT                                                              SFLX.1884
      REAL     ZSOIL (NSOIL)                                                     SFLX.1885
      REAL     ZZ1                                                               SFLX.1886
      REAL     EMISSI                                                            23DEC04.3741
                                                                                 SFLX.1887
      REAL EC1                                                                   SFLX.1888
      REAL EDIR1                                                                 SFLX.1889
      REAL ET1(NSOIL)                                                            SFLX.1890
      REAL ETT1                                                                  SFLX.1891
      PARAMETER (CP = 1004.5)                                                    SFLX.1892
                                                                                 SFLX.1893
! ----------------------------------------------------------------------         SFLX.1894
! EXECUTABLE CODE BEGINS HERE:                                                   SFLX.1895
! CONVERT ETP Fnd PRCP FROM KG M-2 S-1 TO M S-1 AND INITIALIZE DEW.              SFLX.1896
! ----------------------------------------------------------------------         SFLX.1897
      PARAMETER (SIGMA = 5.67E-8)                                                SFLX.1898
      PRCP1 = PRCP * 0.001                                                       SFLX.1899
      ETP1 = ETP * 0.001                                                         SFLX.1900
      DEW = 0.0                                                                  SFLX.1901
C ----------------------------------------------------------------------         SFLX.1902
C INITIALIZE EVAP TERMS.                                                         SFLX.1903
C ----------------------------------------------------------------------         SFLX.1904
      EDIR = 0.                                                                  SFLX.1905
      EDIR1 = 0.                                                                 SFLX.1906
      EC1 = 0.                                                                   SFLX.1907
      EC = 0.                                                                    SFLX.1908
      DO K = 1,NSOIL                                                             SFLX.1909
        ET(K) = 0.                                                               SFLX.1910
        ET1(K) = 0.                                                              SFLX.1911
      END DO                                                                     SFLX.1912
      ETT = 0.                                                                   SFLX.1913
      ETT1 = 0.                                                                  SFLX.1914
                                                                                 SFLX.1915
      IF (ETP .gt. 0.0) THEN                                                     SFLX.1916
         CALL EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,                  &        SFLX.1917
     &                SH2O,                                             &        SFLX.1918
     &                SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,                &        SFLX.1919
     &                SMCREF,SHDFAC,CMCMAX,                             &        SFLX.1920
     &                SMCDRY,CFACTR,                                    &        SFLX.1921
!    &                EDIR,EC,ET,ETT,SFCTMP,Q2,NROOT,RTDIS,FXEXP)                SFLX.1922
     &                 EDIR1,EC1,ET1,ETT1,SFCTMP,Q2,NROOT,RTDIS,FXEXP)           SFLX.1923
         CALL SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,                      &        SFLX.1924
     &                SH2O,SLOPE,KDT,FRZFACT,                           &        SFLX.1925
     &                SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,                   &        SFLX.1926
     &                SHDFAC,CMCMAX,                                    &        SFLX.1927
     &                RUNOFF1,RUNOFF2,RUNOFF3,                          &        SFLX.1928
!    &                EDIR,EC,ET,                                       &        SFLX.1929
     &                EDIR1,EC1,ET1,                                    &        SFLX.1930
     &                DRIP)                                                      SFLX.1931
                                                                                 SFLX.1932
! ----------------------------------------------------------------------         SFLX.1933
! CONVERT MODELED EVAPOTRANSPIRATION FROM  M S-1  TO  KG M-2 S-1.                SFLX.1934
! ----------------------------------------------------------------------         SFLX.1935
                                                                                 SFLX.1936
         ETA = ETA1 * 1000.0                                                     SFLX.1937
                                                                                 SFLX.1938
! ----------------------------------------------------------------------         SFLX.1939
! IF ETP < 0, ASSUME DEW FORMS (TRANSFORM ETP1 INTO DEW AND REINITIALIZE         SFLX.1940
! ETP1 TO ZERO).                                                                 SFLX.1941
! ----------------------------------------------------------------------         SFLX.1942
      ELSE                                                                       SFLX.1943
         DEW = - ETP1                                                            SFLX.1944
                                                                                 SFLX.1945
! ----------------------------------------------------------------------         SFLX.1946
! CONVERT PRCP FROM 'KG M-2 S-1' TO 'M S-1' AND ADD DEW AMOUNT.                  SFLX.1947
! ----------------------------------------------------------------------         SFLX.1948
         ETP1 = 0.0                                                              SFLX.1949
                                                                                 SFLX.1950
         PRCP1 = PRCP1+ DEW                                                      SFLX.1951
         CALL EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,                  &        SFLX.1952
     &                SH2O,                                             &        SFLX.1953
     &                SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,                &        SFLX.1954
     &                SMCREF,SHDFAC,CMCMAX,                             &        SFLX.1955
     &                SMCDRY,CFACTR,                                    &        SFLX.1956
!    &                EDIR,EC,ET,ETT,SFCTMP,Q2,NROOT,RTDIS,FXEXP)                SFLX.1957
     &                EDIR1,EC1,ET1,ETT1,SFCTMP,Q2,NROOT,RTDIS,FXEXP)            SFLX.1958
         CALL SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,                      &        SFLX.1959
     &                SH2O,SLOPE,KDT,FRZFACT,                           &        SFLX.1960
     &                SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,                   &        SFLX.1961
     &                SHDFAC,CMCMAX,                                    &        SFLX.1962
     &                RUNOFF1,RUNOFF2,RUNOFF3,                          &        SFLX.1963
!    &                EDIR,EC,ET,                                       &        SFLX.1964
     &                EDIR1,EC1,ET1,                                    &        SFLX.1965
     &                DRIP)                                                      SFLX.1966
                                                                                 SFLX.1967
! ----------------------------------------------------------------------         SFLX.1968
! CONVERT MODELED EVAPOTRANSPIRATION FROM 'M S-1' TO 'KG M-2 S-1'.               SFLX.1969
! ----------------------------------------------------------------------         SFLX.1970
         ETA = ETA1 * 1000.0                                                     SFLX.1971
      END IF                                                                     SFLX.1972
                                                                                 SFLX.1973
! ----------------------------------------------------------------------         SFLX.1974
! BASED ON ETP AND E VALUES, DETERMINE BETA                                      SFLX.1975
! ----------------------------------------------------------------------         SFLX.1976
                                                                                 SFLX.1977
      IF ( ETP .le. 0.0 ) THEN                                                   SFLX.1978
         BETA = 0.0                                                              SFLX.1979
         IF ( ETP .lt. 0.0 ) THEN                                                SFLX.1980
            BETA = 1.0                                                           SFLX.1981
            ETA = ETP                                                            SFLX.1982
         END IF                                                                  SFLX.1983
      ELSE                                                                       SFLX.1984
         BETA = ETA / ETP                                                        SFLX.1985
      END IF                                                                     SFLX.1986
                                                                                 SFLX.1987
! ----------------------------------------------------------------------         SFLX.1988
! CONVERT MODELED EVAPOTRANSPIRATION COMPONENTS 'M S-1' TO 'KG M-2 S-1'.         SFLX.1989
! ----------------------------------------------------------------------         SFLX.1990
      EDIR = EDIR1*1000.                                                         SFLX.1991
      EC = EC1*1000.                                                             SFLX.1992
      DO K = 1,NSOIL                                                             SFLX.1993
        ET(K) = ET1(K)*1000.                                                     SFLX.1994
      END DO                                                                     SFLX.1995
      ETT = ETT1*1000.                                                           SFLX.1996
                                                                                 SFLX.1997
! ----------------------------------------------------------------------         SFLX.1998
! GET SOIL THERMAL DIFFUXIVITY/CONDUCTIVITY FOR TOP SOIL LYR,                    SFLX.1999
! CALC. ADJUSTED TOP LYR SOIL TEMP AND ADJUSTED SOIL FLUX, THEN                  SFLX.2000
! CALL SHFLX TO COMPUTE/UPDATE SOIL HEAT FLUX AND SOIL TEMPS.                    SFLX.2001
! ----------------------------------------------------------------------         SFLX.2002
                                                                                 SFLX.2003
      CALL TDFCND (DF1,SMC (1),QUARTZ,SMCMAX,SH2O (1))                           SFLX.2004
                                                                                 23DEC04.3742
! urban                                                                          23DEC04.3743
                                                                                 23DEC04.3744
      IF(VEGTYP.eq.1) DF1 = 3.24                                                 23DEC04.3745
                                                                                 SFLX.2005
! ----------------------------------------------------------------------         SFLX.2006
! VEGETATION GREENNESS FRACTION REDUCTION IN SUBSURFACE HEAT FLUX                SFLX.2007
! VIA REDUCTION FACTOR, WHICH IS CONVENIENT TO APPLY HERE TO THERMAL             SFLX.2008
! DIFFUSIVITY THAT IS LATER USED IN HRT TO COMPUTE SUB SFC HEAT FLUX             SFLX.2009
! (SEE ADDITIONAL COMMENTS ON VEG EFFECT SUB-SFC HEAT FLX IN                     SFLX.2010
! ROUTINE SFLX)                                                                  SFLX.2011
! ----------------------------------------------------------------------         SFLX.2012
      DF1 = DF1 * EXP (SBETA * SHDFAC)                                           SFLX.2013
! ----------------------------------------------------------------------         SFLX.2014
! COMPUTE INTERMEDIATE TERMS PASSED TO ROUTINE HRT (VIA ROUTINE                  SFLX.2015
! SHFLX BELOW) FOR USE IN COMPUTING SUBSURFACE HEAT FLUX IN HRT                  SFLX.2016
! ----------------------------------------------------------------------         SFLX.2017
       YYNUM = FDOWN - EMISSI*SIGMA * T24    
      YY = SFCTMP + (YYNUM / RCH + TH2- SFCTMP - BETA * EPSCA) / RR              SFLX.2019
                                                                                 SFLX.2020
      ZZ1 = DF1 / ( -0.5 * ZSOIL (1) * RCH * RR ) + 1.0                          SFLX.2021
!urban                                                                           23DEC04.3748
      CALL SHFLX (SSOIL,STC,SMC,SMCMAX,NSOIL,T1,DT,YY,ZZ1,ZSOIL,        &        SFLX.2022
     &             TBOT,ZBOT,SMCWLT,PSISAT,SH2O,BEXP,F1,DF1,ICE,        &        SFLX.2023
     &             QUARTZ,CSOIL,VEGTYP)                                          23DEC04.3749
                                                                                 SFLX.2025
! ----------------------------------------------------------------------         SFLX.2026
! SET FLX1 AND FLX3 (SNOPACK PHASE CHANGE HEAT FLUXES) TO ZERO SINCE             SFLX.2027
! THEY ARE NOT USED HERE IN SNOPAC.  FLX2 (FREEZING RAIN HEAT FLUX) WAS          SFLX.2028
! SIMILARLY INITIALIZED IN THE PENMAN ROUTINE.                                   SFLX.2029
! ----------------------------------------------------------------------         SFLX.2030
      FLX1 = 0.0                                                                 SFLX.2031
                                                                                 SFLX.2032
! ----------------------------------------------------------------------         SFLX.2033
! END SUBROUTINE NOPAC                                                           SFLX.2034
! ----------------------------------------------------------------------         SFLX.2035
      FLX3 = 0.0                                                                 SFLX.2036
      END                                                                        SFLX.2037
                                                                                 SFLX.2038
      SUBROUTINE PENMAN (SFCTMP,SFCPRS,CH,T2V,TH2,PRCP,FDOWN,T24,SSOIL, &        SFLX.2039
     &                   Q2,Q2SAT,ETP,RCH,EPSCA,RR,SNOWNG,FRZGRA,       &        SFLX.2040
     &                   DQSDT2,FLX2,EMISSI)                                     23DEC04.3750
                                                                                 SFLX.2042
! ----------------------------------------------------------------------         SFLX.2043
! SUBROUTINE PENMAN                                                              SFLX.2044
! ----------------------------------------------------------------------         SFLX.2045
! CALCULATE POTENTIAL EVAPORATION FOR THE CURRENT POINT.  VARIOUS                SFLX.2046
! PARTIAL SUMS/PRODUCTS ARE ALSO CALCULATED AND PASSED BACK TO THE               SFLX.2047
! CALLING ROUTINE FOR LATER USE.                                                 SFLX.2048
! ----------------------------------------------------------------------         SFLX.2049
      IMPLICIT NONE                                                              SFLX.2050
      LOGICAL  SNOWNG                                                            SFLX.2051
                                                                                 SFLX.2052
      LOGICAL  FRZGRA                                                            SFLX.2053
      REAL     A                                                                 SFLX.2054
      REAL     BETA                                                              SFLX.2055
      REAL     CH                                                                SFLX.2056
      REAL     CP                                                                SFLX.2057
      REAL     CPH2O                                                             SFLX.2058
      REAL     CPICE                                                             SFLX.2059
      REAL     DELTA                                                             SFLX.2060
      REAL     DQSDT2                                                            SFLX.2061
      REAL     ELCP                                                              SFLX.2062
      REAL     EPSCA                                                             SFLX.2063
      REAL     ETP                                                               SFLX.2064
      REAL     FDOWN                                                             SFLX.2065
      REAL     FLX2                                                              SFLX.2066
      REAL     FNET                                                              SFLX.2067
      REAL     LSUBC                                                             SFLX.2068
      REAL     LSUBF                                                             SFLX.2069
      REAL     PRCP                                                              SFLX.2070
      REAL     Q2                                                                SFLX.2071
      REAL     Q2SAT                                                             SFLX.2072
      REAL     R                                                                 SFLX.2073
      REAL     RAD                                                               SFLX.2074
      REAL     RCH                                                               SFLX.2075
      REAL     RHO                                                               SFLX.2076
      REAL     RR                                                                SFLX.2077
      REAL     SSOIL                                                             SFLX.2078
      REAL     SFCPRS                                                            SFLX.2079
      REAL     SFCTMP                                                            SFLX.2080
      REAL     SIGMA                                                             SFLX.2081
      REAL     T24                                                               SFLX.2082
      REAL     T2V                                                               SFLX.2083
      REAL     EMISSI                                                            23DEC04.3751
                                                                                 SFLX.2084
      REAL     TH2                                                               SFLX.2085
      PARAMETER (CP = 1004.6)                                                    SFLX.2086
      PARAMETER (CPH2O = 4.218E+3)                                               SFLX.2087
      PARAMETER (CPICE = 2.106E+3)                                               SFLX.2088
      PARAMETER (R = 287.04)                                                     SFLX.2089
      PARAMETER (ELCP = 2.4888E+3)                                               SFLX.2090
      PARAMETER (LSUBF = 3.335E+5)                                               SFLX.2091
      PARAMETER (LSUBC = 2.501000E+6)                                            SFLX.2092
                                                                                 SFLX.2093
! ----------------------------------------------------------------------         SFLX.2094
! EXECUTABLE CODE BEGINS HERE:                                                   SFLX.2095
! ----------------------------------------------------------------------         SFLX.2096
      PARAMETER (SIGMA = 5.67E-8)                                                SFLX.2097
! ----------------------------------------------------------------------         SFLX.2098
! PREPARE PARTIAL QUANTITIES FOR PENMAN EQUATION.                                SFLX.2099
! ----------------------------------------------------------------------         SFLX.2100
      FLX2 = 0.0                                                                 SFLX.2101
      DELTA = ELCP * DQSDT2                                                      SFLX.2102
      T24 = SFCTMP * SFCTMP * SFCTMP * SFCTMP                                    SFLX.2103
!     RR = T24 * 6.48E-8 / (SFCPRS * CH) + 1.0                                   23DEC04.3752
      RR = EMISSI*T24 * 6.48E-8 / (SFCPRS * CH) + 1.0                            23DEC04.3753
!!! Fei: place for EMISSIVITY                                                    23DEC04.3754
      RHO = SFCPRS / (R * T2V)                                                   SFLX.2105
                                                                                 SFLX.2106
! ----------------------------------------------------------------------         SFLX.2107
! ADJUST THE PARTIAL SUMS / PRODUCTS WITH THE LATENT HEAT                        SFLX.2108
! EFFECTS CAUSED BY FALLING PRECIPITATION.                                       SFLX.2109
! ----------------------------------------------------------------------         SFLX.2110
      RCH = RHO * CP * CH                                                        SFLX.2111
      IF (.NOT. SNOWNG) THEN                                                     SFLX.2112
         IF (PRCP .gt. 0.0) RR = RR + CPH2O * PRCP / RCH                         SFLX.2113
      ELSE                                                                       SFLX.2114
         RR = RR + CPICE * PRCP / RCH                                            SFLX.2115
      END IF                                                                     SFLX.2116
                                                                                 SFLX.2117
! ----------------------------------------------------------------------         SFLX.2118
! INCLUDE THE LATENT HEAT EFFECTS OF FRZNG RAIN CONVERTING TO ICE ON             SFLX.2119
! IMPACT IN THE CALCULATION OF FLX2 AND FNET.                                    SFLX.2120
! ----------------------------------------------------------------------         SFLX.2121
!!! Fei: place for EMISSIVITY                                                    23DEC04.3755
!     FNET = FDOWN - SIGMA * T24- SSOIL                                          23DEC04.3756
      FNET = FDOWN -  EMISSI*SIGMA * T24- SSOIL                                  23DEC04.3757
      IF (FRZGRA) THEN                                                           SFLX.2123
         FLX2 = - LSUBF * PRCP                                                   SFLX.2124
         FNET = FNET - FLX2                                                      SFLX.2125
! ----------------------------------------------------------------------         SFLX.2126
! FINISH PENMAN EQUATION CALCULATIONS.                                           SFLX.2127
! ----------------------------------------------------------------------         SFLX.2128
      END IF                                                                     SFLX.2129
      RAD = FNET / RCH + TH2- SFCTMP                                             SFLX.2130
      A = ELCP * (Q2SAT - Q2)                                                    SFLX.2131
      EPSCA = (A * RR + RAD * DELTA) / (DELTA + RR)                              SFLX.2132
                                                                                 SFLX.2133
! ----------------------------------------------------------------------         SFLX.2134
! END SUBROUTINE PENMAN                                                          SFLX.2135
! ----------------------------------------------------------------------         SFLX.2136
      ETP = EPSCA * RCH / LSUBC                                                  SFLX.2137
                                                                                 SFLX.2138
      END                                                                        SFLX.2139
                                                                                 SFLX.2140
      SUBROUTINE REDPRM (VEGTYP,SOILTYP,SLOPETYP,CFACTR,CMCMAX,RSMAX,      &     SFLX.2141
     &                   TOPT,                                             &     SFLX.2142
     &                  REFKDT,KDT,SBETA, SHDFAC,RSMIN,RGL,HS,ZBOT,FRZX,   &     SFLX.2143
     &                   PSISAT,SLOPE,SNUP,SALP,BEXP,DKSAT,DWSAT,          &     SFLX.2144
     &                   SMCMAX,SMCWLT,SMCREF,SMCDRY,F1,QUARTZ,FXEXP,      &     SFLX.2145
     &                   RTDIS,SLDPTH,ZSOIL, NROOT,NSOIL,Z0BRD,CZIL,LAI,   &     SFLX.2146
     &                   CSOIL,ALBBRD,PTU,LLANDUSE,LSOIL,OFFLINE)                SFLX.2147
                                                                                 SFLX.2148
      IMPLICIT NONE                                                              SFLX.2149
#     include <paramsoil_STAS.incl>                                              SFLX.2150
#     include <paramveg_USGS.incl>                                               SFLX.2151
#     include <paramgen_LSM.incl>                                                SFLX.2152
! ----------------------------------------------------------------------         SFLX.2153
! Internally set (default valuess)                                               SFLX.2154
! all soil and vegetation parameters required for the execusion oF               SFLX.2155
! the Noah lsm are defined in VEGPARM.TBL, SOILPARM.TB, and GENPARM.TBL.         SFLX.2156
! ----------------------------------------------------------------------         SFLX.2157
!     Vegetation parameters:                                                     SFLX.2158
!             ALBBRD: SFC background snow-free albedo                            SFLX.2159
!             CMXTBL: MAX CNPY Capacity                                          SFLX.2160
!              Z0BRD: Background roughness length                                SFLX.2161
!             SHDFAC: Green vegetation fraction                                  SFLX.2162
!              NROOT: Rooting depth                                              SFLX.2163
!              RSMIN: Mimimum stomatal resistance                                SFLX.2164
!              RSMAX: Max. stomatal resistance                                   SFLX.2165
!                RGL: Parameters used in radiation stress function               SFLX.2166
!                 HS: Parameter used in vapor pressure deficit functio           SFLX.2167
!               TOPT: Optimum transpiration air temperature.                     SFLX.2168
!             CMCMAX: Maximum canopy water capacity                              SFLX.2169
!             CFACTR: Parameter used in the canopy inteception calculation       SFLX.2170
!               SNUP: Threshold snow depth (in water equivalent m) that          SFLX.2171
!                     implies 100 percent snow cover                             SFLX.2172
!                LAI: Leaf area index                                            SFLX.2173
!                                                                                SFLX.2174
! ----------------------------------------------------------------------         SFLX.2175
!      Soil parameters:                                                          SFLX.2176
!        SMCMAX: MAX soil moisture content (porosity)                            SFLX.2177
!        SMCREF: Reference soil moisture  (field capacity)                       SFLX.2178
!        SMCWLT: Wilting point soil moisture                                     SFLX.2179
!        SMCWLT: Air dry soil moist content limits                               SFLX.2180
!       SSATPSI: SAT (saturation) soil potential                                 SFLX.2181
!         DKSAT: SAT soil conductivity                                           SFLX.2182
!          BEXP: B parameter                                                     SFLX.2183
!        SSATDW: SAT soil diffusivity                                            SFLX.2184
!           F1: Soil thermal diffusivity/conductivity coef.                      SFLX.2185
!        QUARTZ: Soil quartz content                                             SFLX.2186
!  Modified by F. Chen (12/22/97)  to use the STATSGO soil map                   SFLX.2187
!  Modified By F. Chen (01/22/00)  to include PLaya, Lava, and White San         SFLX.2188
!  Modified By F. Chen (08/05/02)  to include additional parameters for the Noah SFLX.2189
! NOTE: SATDW = BB*SATDK*(SATPSI/MAXSMC)                                         SFLX.2190
!         F11 = ALOG10(SATPSI) + BB*ALOG10(MAXSMC) + 2.0                         SFLX.2191
!       REFSMC1=MAXSMC*(5.79E-9/SATDK)**(1/(2*BB+3)) 5.79E-9 m/s= 0.5 mm         SFLX.2192
!       REFSMC=REFSMC1+1./3.(MAXSMC-REFSMC1)                                     SFLX.2193
!       WLTSMC1=MAXSMC*(200./SATPSI)**(-1./BB)    (Wetzel and Chang, 198         SFLX.2194
!       WLTSMC=WLTSMC1-0.5*WLTSMC1                                               SFLX.2195
! Note: the values for playa is set for it to have a thermal conductivit         SFLX.2196
! as sand and to have a hydrulic conductivity as clay                            SFLX.2197
!                                                                                SFLX.2198
! ----------------------------------------------------------------------         SFLX.2199
! Class parameter 'SLOPETYP' was included to estimate linear reservoir           SFLX.2200
! coefficient 'SLOPE' to the baseflow runoff out of the bottom layer.            SFLX.2201
! lowest class (slopetyp=0) means highest slope parameter = 1.                   SFLX.2202
! definition of slopetyp from 'zobler' slope type:                               SFLX.2203
! slope class  percent slope                                                     SFLX.2204
! 1            0-8                                                               SFLX.2205
! 2            8-30                                                              SFLX.2206
! 3            > 30                                                              SFLX.2207
! 4            0-30                                                              SFLX.2208
! 5            0-8 & > 30                                                        SFLX.2209
! 6            8-30 & > 30                                                       SFLX.2210
! 7            0-8, 8-30, > 30                                                   SFLX.2211
! 9            GLACIAL ICE                                                       SFLX.2212
! BLANK        OCEAN/SEA                                                         SFLX.2213
!       SLOPE_DATA: linear reservoir coefficient                                 SFLX.2214
!       SBETA_DATA: parameter used to caluculate vegetation effect on soil heat  SFLX.2215
!       FXEXP_DAT:  soil evaporation exponent used in DEVAP                      SFLX.2216
!       CSOIL_DATA: soil heat capacity [J M-3 K-1]                               SFLX.2217
!       SALP_DATA: shape parameter of  distribution function of snow cover       SFLX.2218
!       REFDK_DATA and REFKDT_DATA: parameters in the surface runoff parameteriz SFLX.2219
!       FRZK_DATA: frozen ground parameter                                       SFLX.2220
!       ZBOT_DATA: depth[M] of lower boundary soil temperature                   SFLX.2221
!       CZIL_DATA: calculate roughness length of heat                            SFLX.2222
!       SMLOW_DATA and MHIGH_DATA: two soil moisture wilt, soil moisture referen SFLX.2223
!                 parameters                                                     SFLX.2224
! Set maximum number of soil-, veg-, and slopetyp in data statement.             SFLX.2225
! ----------------------------------------------------------------------         SFLX.2226
      INTEGER            MAX_SLOPETYP ,    MAX_SOILTYP,                 &        SFLX.2227
     &                     MAX_VEGTYP                                            SFLX.2228
      PARAMETER( MAX_SLOPETYP = 30,MAX_SOILTYP = 30,                    &        SFLX.2229
     &                     MAX_VEGTYP = 30)                                      SFLX.2230
      LOGICAL  OFFLINE                                                           SFLX.2231
      CHARACTER*4  LLANDUSE, LSOIL                                               SFLX.2232
      INTEGER  DEFINED_VEG,DEFINED_SOIL,DEFINED_SLOPE                            SFLX.2233
                                                                                 SFLX.2234
! Veg parameters                                                                 SFLX.2235
      INTEGER  VEGTYP,NROOT                                                      SFLX.2236
      REAL     HS,LAI,RSMIN,RGL,SHDFAC,SNUP,Z0BRD,CMCMAX,RSMAX,          &       SFLX.2237
     &         TOPT,ALBBRD                                                       SFLX.2238
                                                                                 SFLX.2239
! Soil parameters                                                                SFLX.2240
      INTEGER  SOILTYP                                                           SFLX.2241
      REAL     BEXP,DKSAT,DWSAT,F1,QUARTZ,REFSMC1,SMCDRY,SMCMAX,         &       SFLX.2242
     &           SMCREF,SMCWLT,WLTSMC1,PSISAT,SMLOW,SMHIGH                       SFLX.2243
                                                                                 SFLX.2244
! General parameters                                                             SFLX.2245
      LOGICAL  LFIRST                                                            SFLX.2246
      INTEGER  SLOPETYP,NSOIL,I                                                  SFLX.2247
      REAL     RTDIS(NSOIL),SLDPTH(NSOIL),ZSOIL(NSOIL)                           SFLX.2248
      REAL     FRZFACT,SLOPE,CZIL,SBETA,FXEXP,                           &       SFLX.2249
     &           CSOIL,SALP,REFDK,                                       &       SFLX.2250
     &           FRZX,FRZK,KDT,CFACTR,                                   &       SFLX.2251
     &           ZBOT,REFKDT                                                     SFLX.2252
                                                                                 SFLX.2253
      SAVE                                                                       SFLX.2254
! ----------------------------------------------------------------------         SFLX.2255
!                                                                                SFLX.2256
               IF (SOILTYP .gt. SLCATS) THEN                                     SFLX.2257
                        WRITE (*,*) 'Warning: too many input soil types'         SFLX.2258
                        STOP 333                                                 23DEC04.3758
               END IF                                                            SFLX.2260
               IF (VEGTYP .gt. LUCATS) THEN                                      SFLX.2261
!                     print *, 'VEGTYP=',VEGTYP=,'LUCATS=',LUCATS                23DEC04.3759
                     WRITE (*,*) 'Warning: too many input landuse types'         SFLX.2262
                     STOP 333                                                    SFLX.2263
               END IF                                                            SFLX.2264
               IF (SLOPETYP .gt. SLPCATS) THEN                                   SFLX.2265
                     WRITE (*,*) 'Warning: too many input slope types'           SFLX.2266
                     STOP 333                                                    SFLX.2267
               END IF                                                            SFLX.2268
                                                                                 SFLX.2269
! ----------------------------------------------------------------------         SFLX.2270
!  SET-UP SOIL PARAMETERS                                                        SFLX.2271
! ----------------------------------------------------------------------         SFLX.2272
      CSOIL = CSOIL_DATA                                                         SFLX.2273
      BEXP = BB (SOILTYP)                                                        SFLX.2274
      DKSAT = SATDK (SOILTYP)                                                    SFLX.2275
      DWSAT = SATDW (SOILTYP)                                                    SFLX.2276
      F1 = F11 (SOILTYP)                                                         SFLX.2277
      PSISAT = SATPSI (SOILTYP)                                                  SFLX.2278
      QUARTZ = QTZ (SOILTYP)                                                     SFLX.2279
      SMCDRY = DRYSMC (SOILTYP)                                                  SFLX.2280
      SMCMAX = MAXSMC (SOILTYP)                                                  SFLX.2281
      SMCREF = REFSMC (SOILTYP)                                                  SFLX.2282
      SMCWLT = WLTSMC (SOILTYP)                                                  SFLX.2283
! ----------------------------------------------------------------------         SFLX.2284
! Set-up universal parameters (not dependent on SOILTYP, VEGTYP or               SFLX.2285
! SLOPETYP)                                                                      SFLX.2286
! ----------------------------------------------------------------------         SFLX.2287
      ZBOT = ZBOT_DATA                                                           SFLX.2288
      SALP = SALP_DATA                                                           SFLX.2289
      SBETA = SBETA_DATA                                                         SFLX.2290
      REFDK = REFDK_DATA                                                         SFLX.2291
      FRZK = FRZK_DATA                                                           SFLX.2292
      FXEXP = FXEXP_DATA                                                         SFLX.2293
      REFKDT = REFKDT_DATA                                                       SFLX.2294
      KDT = REFKDT * DKSAT / REFDK                                               SFLX.2295
      CZIL = CZIL_DATA                                                           SFLX.2296
      SLOPE = SLOPE_DATA (SLOPETYP)                                              SFLX.2297
                                                                                 SFLX.2298
! ----------------------------------------------------------------------         SFLX.2299
! TO ADJUST FRZK PARAMETER TO ACTUAL SOIL TYPE: FRZK * FRZFACT                   SFLX.2300
! ----------------------------------------------------------------------         SFLX.2301
      FRZFACT = (SMCMAX / SMCREF) * (0.412 / 0.468)                              SFLX.2302
      FRZX = FRZK * FRZFACT                                                      SFLX.2303
                                                                                 SFLX.2304
! ----------------------------------------------------------------------         SFLX.2305
! SET-UP VEGETATION PARAMETERS                                                   SFLX.2306
! ----------------------------------------------------------------------         SFLX.2307
      TOPT = TOPT_DATA                                                           SFLX.2308
      CMCMAX = CMCMAX_DATA                                                       SFLX.2309
      CFACTR = CFACTR_DATA                                                       SFLX.2310
      RSMAX = RSMAX_DATA                                                         SFLX.2311
      NROOT = NROTBL (VEGTYP)                                                    SFLX.2312
      SNUP = SNUPTBL (VEGTYP)                                                    SFLX.2313
      RSMIN = RSTBL (VEGTYP)                                                     SFLX.2314
      RGL = RGLTBL (VEGTYP)                                                      SFLX.2315
      HS = HSTBL (VEGTYP)                                                        SFLX.2316
      LAI = LAITBL (VEGTYP)                                                      SFLX.2317
               IF(OFFLINE) THEN                                                  SFLX.2318
                 ALBBRD = ALBTBL(VEGTYP)                                         SFLX.2319
                 Z0BRD = Z0TBL(VEGTYP)                                           SFLX.2320
                 SHDFAC = SHDTBL(VEGTYP)                                         SFLX.2321
               ENDIF                                                             SFLX.2322
                                                                                 SFLX.2323
               IF (VEGTYP .eq. BARE) SHDFAC = 0.0                                SFLX.2324
               IF (NROOT .gt. NSOIL) THEN                                        SFLX.2325
                  WRITE (*,*) 'Warning: too many root layers'                    SFLX.2326
                  STOP 333                                                       SFLX.2327
! ----------------------------------------------------------------------         SFLX.2328
! CALCULATE ROOT DISTRIBUTION.  PRESENT VERSION ASSUMES UNIFORM                  SFLX.2329
! DISTRIBUTION BASED ON SOIL LAYER DEPTHS.                                       SFLX.2330
! ----------------------------------------------------------------------         SFLX.2331
               END IF                                                            SFLX.2332
               DO I = 1,NROOT                                                    SFLX.2333
                  RTDIS (I) = - SLDPTH (I)/ ZSOIL (NROOT)                        SFLX.2334
! ----------------------------------------------------------------------         SFLX.2335
!  SET-UP SLOPE PARAMETER                                                        SFLX.2336
! ----------------------------------------------------------------------         SFLX.2337
               END DO                                                            SFLX.2338
                                                                                 SFLX.2339
!        print*,'end of PRMRED'                                                  SFLX.2340
!       print*,'VEGTYP',VEGTYP,'SOILTYP',SOILTYP,'SLOPETYP',SLOPETYP,    &       SFLX.2341
!    & 'CFACTR',CFACTR,'CMCMAX',CMCMAX,'RSMAX',RSMAX,'TOPT',TOPT,        &       SFLX.2342
!    & 'REFKDT',REFKDT,'KDT',KDT,'SBETA',SBETA, 'SHDFAC',SHDFAC,         &       SFLX.2343
!    &  'RSMIN',RSMIN,'RGL',RGL,'HS',HS,'ZBOT',ZBOT,'FRZX',FRZX,         &       SFLX.2344
!    &  'PSISAT',PSISAT,'SLOPE',SLOPE,'SNUP',SNUP,'SALP',SALP,'BEXP',    &       SFLX.2345
!    &   BEXP,                                                           &       SFLX.2346
!    &  'DKSAT',DKSAT,'DWSAT',DWSAT,                                     &       SFLX.2347
!    &  'SMCMAX',SMCMAX,'SMCWLT',SMCWLT,'SMCREF',SMCREF,'SMCDRY',SMCDRY, &       SFLX.2348
!    &  'F1',F1,'QUARTZ',QUARTZ,'FXEXP',FXEXP,                           &       SFLX.2349
!    &  'RTDIS',RTDIS,'SLDPTH',SLDPTH,'ZSOIL',ZSOIL, 'NROOT',NROOT,      &       SFLX.2350
!    &  'NSOIL',NSOIL,'Z0',Z0,'CZIL',CZIL,'LAI',LAI,                     &       SFLX.2351
!    &  'CSOIL',CSOIL,'PTU',PTU,                                         &       SFLX.2352
!    &  'OFFLINE', OFFLINE                                                       SFLX.2353
                                                                                 SFLX.2354
      END                                                                        SFLX.2355
                                                                                 SFLX.2356
      SUBROUTINE ROSR12 (P,A,B,C,D,DELTA,NSOIL)                                  SFLX.2357
                                                                                 SFLX.2358
! ----------------------------------------------------------------------         SFLX.2359
! SUBROUTINE ROSR12                                                              SFLX.2360
! ----------------------------------------------------------------------         SFLX.2361
! INVERT (SOLVE) THE TRI-DIAGONAL MATRIX PROBLEM SHOWN BELOW:                    SFLX.2362
! ###                                            ### ###  ###   ###  ###         SFLX.2363
! #B(1), C(1),  0  ,  0  ,  0  ,   . . .  ,    0   # #      #   #      #         SFLX.2364
! #A(2), B(2), C(2),  0  ,  0  ,   . . .  ,    0   # #      #   #      #         SFLX.2365
! # 0  , A(3), B(3), C(3),  0  ,   . . .  ,    0   # #      #   # D(3) #         SFLX.2366
! # 0  ,  0  , A(4), B(4), C(4),   . . .  ,    0   # # P(4) #   # D(4) #         SFLX.2367
! # 0  ,  0  ,  0  , A(5), B(5),   . . .  ,    0   # # P(5) #   # D(5) #         SFLX.2368
! # .                                          .   # #  .   # = #   .  #         SFLX.2369
! # .                                          .   # #  .   #   #   .  #         SFLX.2370
! # .                                          .   # #  .   #   #   .  #         SFLX.2371
! # 0  , . . . , 0 , A(M-2), B(M-2), C(M-2),   0   # #P(M-2)#   #D(M-2)#         SFLX.2372
! # 0  , . . . , 0 ,   0   , A(M-1), B(M-1), C(M-1)# #P(M-1)#   #D(M-1)#         SFLX.2373
! # 0  , . . . , 0 ,   0   ,   0   ,  A(M) ,  B(M) # # P(M) #   # D(M) #         SFLX.2374
! ###                                            ### ###  ###   ###  ###         SFLX.2375
! ----------------------------------------------------------------------         SFLX.2376
      IMPLICIT NONE                                                              SFLX.2377
      INTEGER  K                                                                 SFLX.2378
      INTEGER  KK                                                                SFLX.2379
                                                                                 SFLX.2380
      INTEGER  NSOIL                                                             SFLX.2381
      REAL     A (NSOIL)                                                         SFLX.2382
      REAL     B (NSOIL)                                                         SFLX.2383
      REAL     C (NSOIL)                                                         SFLX.2384
      REAL     D (NSOIL)                                                         SFLX.2385
      REAL     DELTA (NSOIL)                                                     SFLX.2386
                                                                                 SFLX.2387
! ----------------------------------------------------------------------         SFLX.2388
! INITIALIZE EQN COEF C FOR THE LOWEST SOIL LAYER                                SFLX.2389
! ----------------------------------------------------------------------         SFLX.2390
      REAL     P (NSOIL)                                                         SFLX.2391
! ----------------------------------------------------------------------         SFLX.2392
! SOLVE THE COEFS FOR THE 1ST SOIL LAYER                                         SFLX.2393
! ----------------------------------------------------------------------         SFLX.2394
      C (NSOIL) = 0.0                                                            SFLX.2395
      P (1) = - C (1) / B (1)                                                    SFLX.2396
                                                                                 SFLX.2397
! ----------------------------------------------------------------------         SFLX.2398
! SOLVE THE COEFS FOR SOIL LAYERS 2 THRU NSOIL                                   SFLX.2399
! ----------------------------------------------------------------------         SFLX.2400
      DELTA (1) = D (1) / B (1)                                                  SFLX.2401
      DO K = 2,NSOIL                                                             SFLX.2402
         P (K) = - C (K) * ( 1.0 / (B (K) + A (K) * P (K -1)) )                  SFLX.2403
         DELTA (K) = (D (K) - A (K)* DELTA (K -1))* (1.0/ (B (K) + A (K)&        SFLX.2404
     &              * P (K -1)))                                                 SFLX.2405
! ----------------------------------------------------------------------         SFLX.2406
! SET P TO DELTA FOR LOWEST SOIL LAYER                                           SFLX.2407
! ----------------------------------------------------------------------         SFLX.2408
      END DO                                                                     SFLX.2409
                                                                                 SFLX.2410
! ----------------------------------------------------------------------         SFLX.2411
! ADJUST P FOR SOIL LAYERS 2 THRU NSOIL                                          SFLX.2412
! ----------------------------------------------------------------------         SFLX.2413
      P (NSOIL) = DELTA (NSOIL)                                                  SFLX.2414
      DO K = 2,NSOIL                                                             SFLX.2415
         KK = NSOIL - K + 1                                                      SFLX.2416
         P (KK) = P (KK) * P (KK +1) + DELTA (KK)                                SFLX.2417
! ----------------------------------------------------------------------         SFLX.2418
! END SUBROUTINE ROSR12                                                          SFLX.2419
! ----------------------------------------------------------------------         SFLX.2420
      END DO                                                                     SFLX.2421
      END                                                                        SFLX.2422
                                                                                 SFLX.2423
!urban                                                                           23DEC04.3760
      SUBROUTINE SHFLX (SSOIL,STC,SMC,SMCMAX,NSOIL,T1,DT,YY,ZZ1,ZSOIL,  &        SFLX.2424
     &                   TBOT,ZBOT,SMCWLT,PSISAT,SH2O,BEXP,F1,DF1,ICE,  &        SFLX.2425
     &                   QUARTZ,CSOIL,VEGTYP)                                    23DEC04.3761
                                                                                 SFLX.2427
! ----------------------------------------------------------------------         SFLX.2428
! SUBROUTINE SHFLX                                                               SFLX.2429
! ----------------------------------------------------------------------         SFLX.2430
! UPDATE THE TEMPERATURE STATE OF THE SOIL COLUMN BASED ON THE THERMAL           SFLX.2431
! DIFFUSION EQUATION AND UPDATE THE FROZEN SOIL MOISTURE CONTENT BASED           SFLX.2432
! ON THE TEMPERATURE.                                                            SFLX.2433
! ----------------------------------------------------------------------         SFLX.2434
      IMPLICIT NONE                                                              SFLX.2435
      INTEGER  NSOLD                                                             SFLX.2436
                                                                                 SFLX.2437
      PARAMETER (NSOLD = 20)                                                     SFLX.2438
      INTEGER  I                                                                 SFLX.2439
      INTEGER  ICE                                                               SFLX.2440
      INTEGER  IFRZ                                                              SFLX.2441
                                                                                 SFLX.2442
      INTEGER  NSOIL                                                             SFLX.2443
      INTEGER  VEGTYP                                                            23DEC04.3762
      REAL     AI (NSOLD)                                                        SFLX.2444
      REAL     BI (NSOLD)                                                        SFLX.2445
                                                                                 SFLX.2446
      REAL     CI (NSOLD)                                                        SFLX.2447
      REAL     BEXP                                                              SFLX.2448
      REAL     CSOIL                                                             SFLX.2449
      REAL     DF1                                                               SFLX.2450
      REAL     DT                                                                SFLX.2451
      REAL     F1                                                                SFLX.2452
      REAL     PSISAT                                                            SFLX.2453
      REAL     QUARTZ                                                            SFLX.2454
      REAL     RHSTS (NSOLD)                                                     SFLX.2455
      REAL     SSOIL                                                             SFLX.2456
      REAL     SH2O (NSOIL)                                                      SFLX.2457
      REAL     SMC (NSOIL)                                                       SFLX.2458
      REAL     SMCMAX                                                            SFLX.2459
      REAL     SMCWLT                                                            SFLX.2460
      REAL     STC (NSOIL)                                                       SFLX.2461
      REAL     STCF (NSOLD)                                                      SFLX.2462
      REAL     T0                                                                SFLX.2463
      REAL     T1                                                                SFLX.2464
      REAL     TBOT                                                              SFLX.2465
      REAL     YY                                                                SFLX.2466
      REAL     ZBOT                                                              SFLX.2467
      REAL     ZSOIL (NSOIL)                                                     SFLX.2468
                                                                                 SFLX.2469
      REAL     ZZ1                                                               SFLX.2470
                                                                                 SFLX.2471
! ----------------------------------------------------------------------         SFLX.2472
! HRT ROUTINE CALCS THE RIGHT HAND SIDE OF THE SOIL TEMP DIF EQN                 SFLX.2473
! ----------------------------------------------------------------------         SFLX.2474
      PARAMETER (T0 = 273.15)                                                    SFLX.2475
                                                                                 SFLX.2476
! ----------------------------------------------------------------------         SFLX.2477
! SEA-ICE CASE                                                                   SFLX.2478
! ----------------------------------------------------------------------         SFLX.2479
      IF (ICE .eq. 1) THEN                                                       SFLX.2480
                                                                                 SFLX.2481
         CALL HRTICE (RHSTS,STC,NSOIL,ZSOIL,YY,ZZ1,DF1,AI,BI,CI)                 SFLX.2482
                                                                                 SFLX.2483
         CALL HSTEP (STCF,STC,RHSTS,DT,NSOIL,AI,BI,CI)                           SFLX.2484
                                                                                 SFLX.2485
! ----------------------------------------------------------------------         SFLX.2486
! LAND-MASS CASE                                                                 SFLX.2487
! ----------------------------------------------------------------------         SFLX.2488
      ELSE                                                                       SFLX.2489
!urban                                                                           23DEC04.3763
         CALL HRT (RHSTS,STC,SMC,SMCMAX,NSOIL,ZSOIL,YY,ZZ1,TBOT,        &        SFLX.2490
     &              ZBOT,PSISAT,SH2O,DT,                                &        SFLX.2491
     &              BEXP,F1,DF1,QUARTZ,CSOIL,AI,BI,CI,VEGTYP)                    23DEC04.3764
                                                                                 SFLX.2493
         CALL HSTEP (STCF,STC,RHSTS,DT,NSOIL,AI,BI,CI)                           SFLX.2494
      END IF                                                                     SFLX.2495
      DO I = 1,NSOIL                                                             SFLX.2496
         STC (I) = STCF (I)                                                      SFLX.2497
! ----------------------------------------------------------------------         SFLX.2498
! IN THE NO SNOWPACK CASE (VIA ROUTINE NOPAC BRANCH,) UPDATE THE GRND            SFLX.2499
! (SKIN) TEMPERATURE HERE IN RESPONSE TO THE UPDATED SOIL TEMPERATURE            SFLX.2500
! PROFILE ABOVE.  (NOTE: INSPECTION OF ROUTINE SNOPAC SHOWS THAT T1              SFLX.2501
! BELOW IS A DUMMY VARIABLE ONLY, AS SKIN TEMPERATURE IS UPDATED                 SFLX.2502
! DIFFERENTLY IN ROUTINE SNOPAC)                                                 SFLX.2503
! ----------------------------------------------------------------------         SFLX.2504
      END DO                                                                     SFLX.2505
                                                                                 SFLX.2506
! ----------------------------------------------------------------------         SFLX.2507
! CALCULATE SURFACE SOIL HEAT FLUX                                               SFLX.2508
! ----------------------------------------------------------------------         SFLX.2509
      T1 = (YY + (ZZ1- 1.0) * STC (1)) / ZZ1                                     SFLX.2510
                                                                                 SFLX.2511
! ----------------------------------------------------------------------         SFLX.2512
! END SUBROUTINE SHFLX                                                           SFLX.2513
! ----------------------------------------------------------------------         SFLX.2514
      SSOIL = DF1 * (STC (1) - T1) / (0.5 * ZSOIL (1))                           SFLX.2515
      END                                                                        SFLX.2516
                                                                                 SFLX.2517
      SUBROUTINE SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,                   &        SFLX.2518
     &                   SH2O,SLOPE,KDT,FRZFACT,                        &        SFLX.2519
     &                   SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,                &        SFLX.2520
     &                   SHDFAC,CMCMAX,                                 &        SFLX.2521
     &                   RUNOFF1,RUNOFF2,RUNOFF3,                       &        SFLX.2522
     &                   EDIR,EC,ET,                                    &        SFLX.2523
     &                   DRIP)                                                   SFLX.2524
                                                                                 SFLX.2525
! ----------------------------------------------------------------------         SFLX.2526
! SUBROUTINE SMFLX                                                               SFLX.2527
! ----------------------------------------------------------------------         SFLX.2528
! CALCULATE SOIL MOISTURE FLUX.  THE SOIL MOISTURE CONTENT (SMC - A PER          SFLX.2529
! UNIT VOLUME MEASUREMENT) IS A DEPENDENT VARIABLE THAT IS UPDATED WITH          SFLX.2530
! PROGNOSTIC EQNS. THE CANOPY MOISTURE CONTENT (CMC) IS ALSO UPDATED.            SFLX.2531
! FROZEN GROUND VERSION:  NEW STATES ADDED: SH2O, AND FROZEN GROUND              SFLX.2532
! CORRECTION FACTOR, FRZFACT AND PARAMETER SLOPE.                                SFLX.2533
! ----------------------------------------------------------------------         SFLX.2534
      IMPLICIT NONE                                                              SFLX.2535
      INTEGER  NSOLD                                                             SFLX.2536
                                                                                 SFLX.2537
      PARAMETER (NSOLD = 20)                                                     SFLX.2538
      INTEGER  I                                                                 SFLX.2539
      INTEGER  K                                                                 SFLX.2540
                                                                                 SFLX.2541
      INTEGER  NSOIL                                                             SFLX.2542
      REAL     AI (NSOLD)                                                        SFLX.2543
      REAL     BI (NSOLD)                                                        SFLX.2544
                                                                                 SFLX.2545
      REAL     CI (NSOLD)                                                        SFLX.2546
      REAL     BEXP                                                              SFLX.2547
      REAL     CMC                                                               SFLX.2548
      REAL     CMCMAX                                                            SFLX.2549
      REAL     DKSAT                                                             SFLX.2550
      REAL     DRIP                                                              SFLX.2551
      REAL     DT                                                                SFLX.2552
      REAL     DUMMY                                                             SFLX.2553
      REAL     DWSAT                                                             SFLX.2554
      REAL     EC                                                                SFLX.2555
      REAL     EDIR                                                              SFLX.2556
      REAL     ET (NSOIL)                                                        SFLX.2557
      REAL     EXCESS                                                            SFLX.2558
      REAL     FRZFACT                                                           SFLX.2559
      REAL     KDT                                                               SFLX.2560
      REAL     PCPDRP                                                            SFLX.2561
      REAL     PRCP1                                                             SFLX.2562
      REAL     RHSCT                                                             SFLX.2563
      REAL     RHSTT (NSOLD)                                                     SFLX.2564
      REAL     RUNOFF1                                                           SFLX.2565
      REAL     RUNOFF2                                                           SFLX.2566
      REAL     RUNOFF3                                                           SFLX.2567
      REAL     SHDFAC                                                            SFLX.2568
      REAL     SMC (NSOIL)                                                       SFLX.2569
      REAL     SH2O (NSOIL)                                                      SFLX.2570
      REAL     SICE (NSOLD)                                                      SFLX.2571
      REAL     SH2OA (NSOLD)                                                     SFLX.2572
      REAL     SH2OFG (NSOLD)                                                    SFLX.2573
      REAL     SLOPE                                                             SFLX.2574
      REAL     SMCMAX                                                            SFLX.2575
      REAL     SMCWLT                                                            SFLX.2576
      REAL     TRHSCT                                                            SFLX.2577
                                                                                 SFLX.2578
! ----------------------------------------------------------------------         SFLX.2579
! EXECUTABLE CODE BEGINS HERE.                                                   SFLX.2580
! ----------------------------------------------------------------------         SFLX.2581
      REAL     ZSOIL (NSOIL)                                                     SFLX.2582
! ----------------------------------------------------------------------         SFLX.2583
! COMPUTE THE RIGHT HAND SIDE OF THE CANOPY EQN TERM ( RHSCT )                   SFLX.2584
! ----------------------------------------------------------------------         SFLX.2585
      DUMMY = 0.                                                                 SFLX.2586
                                                                                 SFLX.2587
! ----------------------------------------------------------------------         SFLX.2588
! CONVERT RHSCT (A RATE) TO TRHSCT (AN AMOUNT) AND ADD IT TO EXISTING            SFLX.2589
! CMC.  IF RESULTING AMT EXCEEDS MAX CAPACITY, IT BECOMES DRIP AND WILL          SFLX.2590
! FALL TO THE GRND.                                                              SFLX.2591
! ----------------------------------------------------------------------         SFLX.2592
      RHSCT = SHDFAC * PRCP1- EC                                                 SFLX.2593
      DRIP = 0.                                                                  SFLX.2594
      TRHSCT = DT * RHSCT                                                        SFLX.2595
      EXCESS = CMC + TRHSCT                                                      SFLX.2596
                                                                                 SFLX.2597
! ----------------------------------------------------------------------         SFLX.2598
! PCPDRP IS THE COMBINED PRCP1 AND DRIP (FROM CMC) THAT GOES INTO THE            SFLX.2599
! SOIL                                                                           SFLX.2600
! ----------------------------------------------------------------------         SFLX.2601
      IF (EXCESS .gt. CMCMAX) DRIP = EXCESS - CMCMAX                             SFLX.2602
                                                                                 SFLX.2603
! ----------------------------------------------------------------------         SFLX.2604
! STORE ICE CONTENT AT EACH SOIL LAYER BEFORE CALLING SRT & SSTEP                SFLX.2605
! ---&------------------------------------------------------------------         SFLX.2606
      PCPDRP = (1. - SHDFAC) * PRCP1+ DRIP / DT                                  SFLX.2607
      DO I = 1,NSOIL                                                             SFLX.2608
         SICE (I) = SMC (I) - SH2O (I)                                           SFLX.2609
! ----------------------------------------------------------------------         SFLX.2610
! CALL SUBROUTINES SRT AND SSTEP TO SOLVE THE SOIL MOISTURE                      SFLX.2611
! TENDENCY EQUATIONS.                                                            SFLX.2612
                                                                                 SFLX.2613
! IF THE INFILTRATING PRECIP RATE IS NONTRIVIAL,                                 SFLX.2614
!   (WE CONSIDER NONTRIVIAL TO BE A PRECIP TOTAL OVER THE TIME STEP              SFLX.2615
!    EXCEEDING ONE ONE-THOUSANDTH OF THE WATER HOLDING CAPACITY OF               SFLX.2616
!    THE FIRST SOIL LAYER)                                                       SFLX.2617
! THEN CALL THE SRT/SSTEP SUBROUTINE PAIR TWICE IN THE MANNER OF                 SFLX.2618
!   TIME SCHEME "F" (IMPLICIT STATE, AVERAGED COEFFICIENT)                       SFLX.2619
!   OF SECTION 2 OF KALNAY AND KANAMITSU (1988, MWR, VOL 116,                    SFLX.2620
!   PAGES 1945-1958)TO MINIMIZE 2-DELTA-T OSCILLATIONS IN THE                    SFLX.2621
!   SOIL MOISTURE VALUE OF THE TOP SOIL LAYER THAT CAN ARISE BECAUSE             SFLX.2622
!   OF THE EXTREME NONLINEAR DEPENDENCE OF THE SOIL HYDRAULIC                    SFLX.2623
!   DIFFUSIVITY COEFFICIENT AND THE HYDRAULIC CONDUCTIVITY ON THE                SFLX.2624
!   SOIL MOISTURE STATE                                                          SFLX.2625
! OTHERWISE CALL THE SRT/SSTEP SUBROUTINE PAIR ONCE IN THE MANNER OF             SFLX.2626
!   TIME SCHEME "D" (IMPLICIT STATE, EXPLICIT COEFFICIENT)                       SFLX.2627
!   OF SECTION 2 OF KALNAY AND KANAMITSU                                         SFLX.2628
! PCPDRP IS UNITS OF KG/M**2/S OR MM/S, ZSOIL IS NEGATIVE DEPTH IN M             SFLX.2629
! ----------------------------------------------------------------------         SFLX.2630
!      IF ( PCPDRP .GT. 0.0 ) THEN                                               SFLX.2631
      END DO                                                                     SFLX.2632
                                                                                 SFLX.2633
! ----------------------------------------------------------------------         SFLX.2634
! FROZEN GROUND VERSION:                                                         SFLX.2635
! SMC STATES REPLACED BY SH2O STATES IN SRT SUBR.  SH2O & SICE STATES            SFLX.2636
! INC&UDED IN SSTEP SUBR.  FROZEN GROUND CORRECTION FACTOR, FRZFACT              SFLX.2637
! ADDED.  ALL WATER BALANCE CALCULATIONS USING UNFROZEN WATER                    SFLX.2638
! ----------------------------------------------------------------------         SFLX.2639
      IF ( (PCPDRP * DT) .gt. (0.001*1000.0* ( - ZSOIL (1))* SMCMAX) )  &        SFLX.2640
     & THEN                                                                      SFLX.2641
         CALL SRT (RHSTT,EDIR,ET,SH2O,SH2O,NSOIL,PCPDRP,ZSOIL,          &        SFLX.2642
     &              DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1,                    &        SFLX.2643
     &              RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZFACT,SICE,AI,BI,CI)           SFLX.2644
         CALL SSTEP (SH2OFG,SH2O,DUMMY,RHSTT,RHSCT,DT,NSOIL,SMCMAX,     &        SFLX.2645
     &                  CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,AI,BI,CI)                  SFLX.2646
         DO K = 1,NSOIL                                                          SFLX.2647
            SH2OA (K) = (SH2O (K) + SH2OFG (K)) * 0.5                            SFLX.2648
         END DO                                                                  SFLX.2649
         CALL SRT (RHSTT,EDIR,ET,SH2O,SH2OA,NSOIL,PCPDRP,ZSOIL,         &        SFLX.2650
     &              DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1,                    &        SFLX.2651
     &              RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZFACT,SICE,AI,BI,CI)           SFLX.2652
         CALL SSTEP (SH2O,SH2O,CMC,RHSTT,RHSCT,DT,NSOIL,SMCMAX,         &        SFLX.2653
     &                  CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,AI,BI,CI)                  SFLX.2654
                                                                                 SFLX.2655
      ELSE                                                                       SFLX.2656
         CALL SRT (RHSTT,EDIR,ET,SH2O,SH2O,NSOIL,PCPDRP,ZSOIL,          &        SFLX.2657
     &              DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1,                    &        SFLX.2658
     &                RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZFACT,SICE,AI,BI,CI)         SFLX.2659
         CALL SSTEP (SH2O,SH2O,CMC,RHSTT,RHSCT,DT,NSOIL,SMCMAX,         &        SFLX.2660
     &                  CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,AI,BI,CI)                  SFLX.2661
!      RUNOF = RUNOFF                                                            SFLX.2662
                                                                                 SFLX.2663
! ----------------------------------------------------------------------         SFLX.2664
! END SUBROUTINE SMFLX                                                           SFLX.2665
! ----------------------------------------------------------------------         SFLX.2666
      END IF                                                                     SFLX.2667
      END                                                                        SFLX.2668
                                                                                 SFLX.2669
                                                                                 SFLX.2670
      SUBROUTINE SNFRAC (SNEQV,SNUP,SALP,SNOWH,SNCOVR)                           SFLX.2671
                                                                                 SFLX.2672
! ----------------------------------------------------------------------         SFLX.2673
! SUBROUTINE SNFRAC                                                              SFLX.2674
! ----------------------------------------------------------------------         SFLX.2675
! CALCULATE SNOW FRACTION (0 -> 1)                                               SFLX.2676
! SNEQV   SNOW WATER EQUIVALENT (M)                                              SFLX.2677
! SNUP    THRESHOLD SNEQV DEPTH ABOVE WHICH SNCOVR=1                             SFLX.2678
! SALP    TUNING PARAMETER                                                       SFLX.2679
! SNCOVR  FRACTIONAL SNOW COVER                                                  SFLX.2680
! ----------------------------------------------------------------------         SFLX.2681
      IMPLICIT NONE                                                              SFLX.2682
                                                                                 SFLX.2683
! ----------------------------------------------------------------------         SFLX.2684
! SNUP IS VEG-CLASS DEPENDENT SNOWDEPTH THRESHHOLD (SET IN ROUTINE               SFLX.2685
! REDPRM) ABOVE WHICH SNOCVR=1.                                                  SFLX.2686
! ----------------------------------------------------------------------         SFLX.2687
      REAL     SNEQV, SNUP, SALP, SNCOVR, RSNOW, Z0N, SNOWH                      SFLX.2688
      IF (SNEQV .lt. SNUP) THEN                                                  SFLX.2689
         RSNOW = SNEQV / SNUP                                                    SFLX.2690
         SNCOVR = 1. - ( EXP ( - SALP * RSNOW) - RSNOW * EXP ( - SALP))          SFLX.2691
      ELSE                                                                       SFLX.2692
         SNCOVR = 1.0                                                            SFLX.2693
      END IF                                                                     SFLX.2694
!     FORMULATION OF DICKINSON ET AL. 1986                                       SFLX.2695
                                                                                 SFLX.2696
!        SNCOVR=SNOWH/(SNOWH + 5*Z0N)                                            SFLX.2697
                                                                                 SFLX.2698
!     FORMULATION OF MARSHALL ET AL. 1994                                        SFLX.2699
!        SNCOVR=SNEQV/(SNEQV + 2*Z0N)                                            SFLX.2700
                                                                                 SFLX.2701
! ----------------------------------------------------------------------         SFLX.2702
! END SUBROUTINE SNFRAC                                                          SFLX.2703
! ----------------------------------------------------------------------         SFLX.2704
      Z0N = 0.035                                                                SFLX.2705
      END                                                                        SFLX.2706
                                                                                 SFLX.2707
!      FUNCTION SNKSRC (TAVG,SMC,SH2O,ZSOIL,NSOIL,                               SFLX.2708
      SUBROUTINE SNKSRC (TSNSR,TAVG,SMC,SH2O,ZSOIL,NSOIL,               &        SFLX.2709
     &                      SMCMAX,PSISAT,BEXP,DT,K,QTOT)                        SFLX.2710
                                                                                 SFLX.2711
! ----------------------------------------------------------------------         SFLX.2712
! SUBROUTINE SNKSRC                                                              SFLX.2713
! FUNCTION SNKSRC                                                                SFLX.2714
! ----------------------------------------------------------------------         SFLX.2715
! CALCULATE SINK/SOURCE TERM OF THE TERMAL DIFFUSION EQUATION. (SH2O) IS         SFLX.2716
! AVAILABLE LIQUED WATER.                                                        SFLX.2717
! ----------------------------------------------------------------------         SFLX.2718
      IMPLICIT NONE                                                              SFLX.2719
      INTEGER  K                                                                 SFLX.2720
                                                                                 SFLX.2721
      INTEGER  NSOIL                                                             SFLX.2722
      REAL     BEXP                                                              SFLX.2723
      REAL     DF                                                                SFLX.2724
      REAL     DH2O                                                              SFLX.2725
      REAL     DT                                                                SFLX.2726
      REAL     DZ                                                                SFLX.2727
      REAL     DZH                                                               SFLX.2728
!      REAL FRH2O                                                                SFLX.2729
      REAL     FREE                                                              SFLX.2730
      REAL     HLICE                                                             SFLX.2731
      REAL     PSISAT                                                            SFLX.2732
      REAL     QTOT                                                              SFLX.2733
      REAL     SH2O                                                              SFLX.2734
      REAL     SMC                                                               SFLX.2735
!      REAL SNKSRC                                                               SFLX.2736
      REAL     SMCMAX                                                            SFLX.2737
      REAL     TSNSR                                                             SFLX.2738
      REAL     T0                                                                SFLX.2739
      REAL     TAVG                                                              SFLX.2740
      REAL     TDN                                                               SFLX.2741
      REAL     TM                                                                SFLX.2742
      REAL     TUP                                                               SFLX.2743
      REAL     TZ                                                                SFLX.2744
      REAL     X0                                                                SFLX.2745
      REAL     XDN                                                               SFLX.2746
      REAL     XH2O                                                              SFLX.2747
      REAL     XUP                                                               SFLX.2748
                                                                                 SFLX.2749
      REAL     ZSOIL (NSOIL)                                                     SFLX.2750
      PARAMETER (DH2O = 1.0000E3)                                                SFLX.2751
      PARAMETER (HLICE = 3.3350E5)                                               SFLX.2752
                                                                                 SFLX.2753
      PARAMETER (T0 = 2.7315E2)                                                  SFLX.2754
      IF (K .eq. 1) THEN                                                         SFLX.2755
         DZ = - ZSOIL (1)                                                        SFLX.2756
      ELSE                                                                       SFLX.2757
         DZ = ZSOIL (K -1) - ZSOIL (K)                                           SFLX.2758
! ----------------------------------------------------------------------         SFLX.2759
! VIA FUNCTION FRH2O, COMPUTE POTENTIAL OR 'EQUILIBRIUM' UNFROZEN                SFLX.2760
! SUPERCOOLED FREE WATER FOR GIVEN SOIL TYPE AND SOIL LAYER TEMPERATURE.         SFLX.2761
! FUNCTION FRH20 INVOKES EQN (17) FROM V. KOREN ET AL (1999, JGR, VOL.           SFLX.2762
! 104, PG 19573).  (ASIDE:  LATTER EQN IN JOURNAL IN CENTIGRADE UNITS.           SFLX.2763
! ROUTINE FRH2O USE FORM OF EQN IN KELVIN UNITS.)                                SFLX.2764
! ----------------------------------------------------------------------         SFLX.2765
      END IF                                                                     SFLX.2766
!      FREE = FRH2O(TAVG,SMC,SH2O,SMCMAX,BEXP,PSISAT)                            SFLX.2767
                                                                                 SFLX.2768
! ----------------------------------------------------------------------         SFLX.2769
! IN NEXT BLOCK OF CODE, INVOKE EQN 18 OF V. KOREN ET AL (1999, JGR,             SFLX.2770
! VOL. 104, PG 19573.)  THAT IS, FIRST ESTIMATE THE NEW AMOUNTOF LIQUID          SFLX.2771
! WATER, 'XH2O', IMPLIED BY THE SUM OF (1) THE LIQUID WATER AT THE BEGIN         SFLX.2772
! OF CURRENT TIME STEP, AND (2) THE FREEZE OF THAW CHANGE IN LIQUID              SFLX.2773
! WATER IMPLIED BY THE HEAT FLUX 'QTOT' PASSED IN FROM ROUTINE HRT.              SFLX.2774
! SECOND, DETERMINE IF XH2O NEEDS TO BE BOUNDED BY 'FREE' (EQUIL AMT) OR         SFLX.2775
! IF 'FREE' NEEDS TO BE BOUNDED BY XH2O.                                         SFLX.2776
! ----------------------------------------------------------------------         SFLX.2777
      CALL FRH2O (FREE,TAVG,SMC,SH2O,SMCMAX,BEXP,PSISAT)                         SFLX.2778
                                                                                 SFLX.2779
! ----------------------------------------------------------------------         SFLX.2780
! FIRST, IF FREEZING AND REMAINING LIQUID LESS THAN LOWER BOUND, THEN            SFLX.2781
! REDUCE EXTENT OF FREEZING, THEREBY LETTING SOME OR ALL OF HEAT FLUX            SFLX.2782
! QTOT COOL THE SOIL TEMP LATER IN ROUTINE HRT.                                  SFLX.2783
! ----------------------------------------------------------------------         SFLX.2784
      XH2O = SH2O + QTOT * DT / (DH2O * HLICE * DZ)                              SFLX.2785
      IF ( XH2O .lt. SH2O .AND. XH2O .lt. FREE) THEN                             SFLX.2786
         IF ( FREE .gt. SH2O ) THEN                                              SFLX.2787
            XH2O = SH2O                                                          SFLX.2788
         ELSE                                                                    SFLX.2789
            XH2O = FREE                                                          SFLX.2790
         END IF                                                                  SFLX.2791
! ----------------------------------------------------------------------         SFLX.2792
! SECOND, IF THAWING AND THE INCREASE IN LIQUID WATER GREATER THAN UPPER         SFLX.2793
! BOUND, THEN REDUCE EXTENT OF THAW, THEREBY LETTING SOME OR ALL OF HEAT         SFLX.2794
! FLUX QTOT WARM THE SOIL TEMP LATER IN ROUTINE HRT.                             SFLX.2795
! ----------------------------------------------------------------------         SFLX.2796
      END IF                                                                     SFLX.2797
      IF ( XH2O .gt. SH2O .AND. XH2O .gt. FREE ) THEN                            SFLX.2798
         IF ( FREE .lt. SH2O ) THEN                                              SFLX.2799
            XH2O = SH2O                                                          SFLX.2800
         ELSE                                                                    SFLX.2801
            XH2O = FREE                                                          SFLX.2802
         END IF                                                                  SFLX.2803
      END IF                                                                     SFLX.2804
      IF (XH2O .lt. 0.) XH2O = 0.                                                SFLX.2805
                                                                                 SFLX.2806
! ----------------------------------------------------------------------         SFLX.2807
! CALCULATE PHASE-CHANGE HEAT SOURCE/SINK TERM FOR USE IN ROUTINE HRT            SFLX.2808
! AND UPDATE LIQUID WATER TO REFLCET FINAL FREEZE/THAW INCREMENT.                SFLX.2809
! ----------------------------------------------------------------------         SFLX.2810
!      SNKSRC = -DH2O*HLICE*DZ*(XH2O-SH2O)/DT                                    SFLX.2811
      IF (XH2O .gt. SMC) XH2O = SMC                                              SFLX.2812
      TSNSR = - DH2O * HLICE * DZ * (XH2O - SH2O)/ DT                            SFLX.2813
                                                                                 SFLX.2814
! ----------------------------------------------------------------------         SFLX.2815
! END SUBROUTINE SNKSRC                                                          SFLX.2816
! END FUNCTION SNKSRC                                                            SFLX.2817
! ----------------------------------------------------------------------         SFLX.2818
      SH2O = XH2O                                                                SFLX.2819
77    CONTINUE                                                                   SFLX.2820
!      SUBROUTINE SNOPAC (ETP,ETA,PRCP,PRCP1,SNOWNG,SMC,SMCMAX,SMCWLT,           SFLX.2821
      END                                                                        SFLX.2822
                                                                                 SFLX.2823
!urban                                                                           23DEC04.3765
      SUBROUTINE SNOPAC (ETP,ETA,PRCP,PRCPF,SNOWNG,SMC,SMCMAX,SMCWLT,   &        SFLX.2824
     &                    SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,            &        SFLX.2825
     &                    SBETA,DF1,                                    &        SFLX.2826
     &                    Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,SSOIL,STC,EPSCA,&        SFLX.2827
     &                   SFCPRS,BEXP,PC,RCH,RR,CFACTR,SNCOVR,ESD,SNDENS,&        SFLX.2828
     &                    SNOWH,SH2O,SLOPE,KDT,FRZFACT,PSISAT,SNUP,     &        SFLX.2829
     &                    ZSOIL,DWSAT,DKSAT,TBOT,ZBOT,SHDFAC,RUNOFF1,   &        SFLX.2830
     &                    RUNOFF2,RUNOFF3,EDIR,EC,ET,ETT,NROOT,SNOMLT,  &        SFLX.2831
     &                    ICE,RTDIS,QUARTZ,FXEXP,CSOIL,                 &        SFLX.2832
     &                   BETA,DRIP,DEW,FLX1,FLX2,FLX3,ESNOW,ETNS,EMISSI,&        23DEC04.3766
     &                   VEGTYP)                                                 23DEC04.3767
                                                                                 SFLX.2834
! ----------------------------------------------------------------------         SFLX.2835
! SUBROUTINE SNOPAC                                                              SFLX.2836
! ----------------------------------------------------------------------         SFLX.2837
! CALCULATE SOIL MOISTURE AND HEAT FLUX VALUES & UPDATE SOIL MOISTURE            SFLX.2838
! CONTENT AND SOIL HEAT CONTENT VALUES FOR THE CASE WHEN A SNOW PACK IS          SFLX.2839
! PRESENT.                                                                       SFLX.2840
! ----------------------------------------------------------------------         SFLX.2841
      IMPLICIT NONE                                                              SFLX.2842
      INTEGER  ICE                                                               SFLX.2843
      INTEGER  NROOT                                                             SFLX.2844
                                                                                 SFLX.2845
! MBEK Jan2002                                                                   SFLX.2846
! new integers that I've added                                                   SFLX.2847
      INTEGER  NSOIL                                                             SFLX.2848
      INTEGER  VEGTYP                                                            23DEC04.3768
      INTEGER  K                                                                 SFLX.2849
                                                                                 SFLX.2850
      LOGICAL  SNOWNG                                                            SFLX.2851
      REAL     BEXP                                                              SFLX.2852
      REAL     BETA                                                              SFLX.2853
      REAL     CFACTR                                                            SFLX.2854
      REAL     CMC                                                               SFLX.2855
      REAL     CMCMAX                                                            SFLX.2856
      REAL     CP                                                                SFLX.2857
      REAL     CPH2O                                                             SFLX.2858
      REAL     CPICE                                                             SFLX.2859
      REAL     CSOIL                                                             SFLX.2860
      REAL     DENOM                                                             SFLX.2861
      REAL     DEW                                                               SFLX.2862
      REAL     DF1                                                               SFLX.2863
      REAL     DKSAT                                                             SFLX.2864
      REAL     DRIP                                                              SFLX.2865
      REAL     DSOIL                                                             SFLX.2866
      REAL     DTOT                                                              SFLX.2867
      REAL     DT                                                                SFLX.2868
      REAL     DWSAT                                                             SFLX.2869
      REAL     EC                                                                SFLX.2870
      REAL     EDIR                                                              SFLX.2871
      REAL     EPSCA                                                             SFLX.2872
      REAL     ESD                                                               SFLX.2873
      REAL     ESDMIN                                                            SFLX.2874
      REAL     FRCSNO                                                            SFLX.2875
      REAL     FRCSOI                                                            SFLX.2876
      REAL     ETA                                                               SFLX.2877
      REAL     ETA1                                                              SFLX.2878
      REAL     ETP                                                               SFLX.2879
      REAL     ETP1                                                              SFLX.2880
      REAL     ETP2                                                              SFLX.2881
      REAL     ET (NSOIL)                                                        SFLX.2882
      REAL     ETT                                                               SFLX.2883
      REAL     EX                                                                SFLX.2884
      REAL     EXPFAC                                                            SFLX.2885
      REAL     FDOWN                                                             SFLX.2886
      REAL     FXEXP                                                             SFLX.2887
      REAL     FLX1                                                              SFLX.2888
      REAL     FLX2                                                              SFLX.2889
      REAL     FLX3                                                              SFLX.2890
      REAL     F1                                                                SFLX.2891
      REAL     KDT                                                               SFLX.2892
      REAL     LSUBF                                                             SFLX.2893
      REAL     LSUBC                                                             SFLX.2894
      REAL     LSUBS                                                             SFLX.2895
      REAL     PC                                                                SFLX.2896
      REAL     PRCP                                                              SFLX.2897
      REAL     PRCP1                                                             SFLX.2898
      REAL     Q2                                                                SFLX.2899
      REAL     RCH                                                               SFLX.2900
      REAL     RR                                                                SFLX.2901
      REAL     RTDIS (NSOIL)                                                     SFLX.2902
      REAL     SSOIL                                                             SFLX.2903
      REAL     SBETA                                                             SFLX.2904
      REAL     SSOIL1                                                            SFLX.2905
      REAL     SFCTMP                                                            SFLX.2906
      REAL     SHDFAC                                                            SFLX.2907
      REAL     SIGMA                                                             SFLX.2908
      REAL     SMC (NSOIL)                                                       SFLX.2909
      REAL     SH2O (NSOIL)                                                      SFLX.2910
      REAL     SMCDRY                                                            SFLX.2911
      REAL     SMCMAX                                                            SFLX.2912
      REAL     SMCREF                                                            SFLX.2913
      REAL     SMCWLT                                                            SFLX.2914
      REAL     SNOMLT                                                            SFLX.2915
      REAL     SNOWH                                                             SFLX.2916
      REAL     STC (NSOIL)                                                       SFLX.2917
      REAL     T1                                                                SFLX.2918
      REAL     T11                                                               SFLX.2919
      REAL     T12                                                               SFLX.2920
      REAL     T12A                                                              SFLX.2921
      REAL     T12B                                                              SFLX.2922
      REAL     T24                                                               SFLX.2923
      REAL     TBOT                                                              SFLX.2924
      REAL     ZBOT                                                              SFLX.2925
      REAL     TH2                                                               SFLX.2926
      REAL     YY                                                                SFLX.2927
      REAL     ZSOIL (NSOIL)                                                     SFLX.2928
      REAL     ZZ1                                                               SFLX.2929
      REAL     TFREEZ                                                            SFLX.2930
      REAL     SALP                                                              SFLX.2931
      REAL     SFCPRS                                                            SFLX.2932
      REAL     SLOPE                                                             SFLX.2933
      REAL     FRZFACT                                                           SFLX.2934
      REAL     PSISAT                                                            SFLX.2935
      REAL     SNUP                                                              SFLX.2936
      REAL     RUNOFF1                                                           SFLX.2937
      REAL     RUNOFF2                                                           SFLX.2938
      REAL     RUNOFF3                                                           SFLX.2939
      REAL     QUARTZ                                                            SFLX.2940
      REAL     SNDENS                                                            SFLX.2941
      REAL     SNCOND                                                            SFLX.2942
      REAL     RSNOW                                                             SFLX.2943
      REAL     SNCOVR                                                            SFLX.2944
      REAL     QSAT                                                              SFLX.2945
      REAL     ETP3                                                              SFLX.2946
      REAL     SEH                                                               SFLX.2947
!      REAL CSNOW                                                                SFLX.2948
                                                                                 SFLX.2949
! MBEK Jan2002                                                                   SFLX.2950
! new reals that I've added                                                      SFLX.2951
      REAL     T14                                                               SFLX.2952
      REAL     PRCPF                                                             SFLX.2953
      REAL     ETNS                                                              SFLX.2954
      REAL     ESNOW                                                             SFLX.2955
      REAL     EC1                                                               SFLX.2956
      REAL     EDIR1                                                             SFLX.2957
      REAL     ET1 (NSOIL)                                                       SFLX.2958
      REAL     ETT1                                                              SFLX.2959
      REAL     ETNS1                                                             SFLX.2960
      REAL     ESNOW1                                                            SFLX.2961
      REAL     ESNOW2                                                            SFLX.2962
      REAL     ETANRG                                                            SFLX.2963
      REAL     T12AX                                                             SFLX.2964
      REAL     ESDFLX                                                            SFLX.2965
      REAL     FLX3X                                                             SFLX.2966
      REAL     ETAX                                                              SFLX.2967
                                                                                 SFLX.2968
      REAL     SNOEXP                                                            SFLX.2969
!Fei                                                                             23DEC04.3769
      REAL     EMISSI                                                            23DEC04.3770
      REAL     EMISSI_S                                                          23DEC04.3771
      PARAMETER (CP = 1004.5)                                                    SFLX.2970
      PARAMETER (CPH2O = 4.218E+3)                                               SFLX.2971
      PARAMETER (CPICE = 2.106E+3)                                               SFLX.2972
      PARAMETER (ESDMIN = 1.E-6)                                                 SFLX.2973
      PARAMETER (LSUBF = 3.335E+5)                                               SFLX.2974
      PARAMETER (LSUBC = 2.501000E+6)                                            SFLX.2975
      PARAMETER (LSUBS = 2.83E+6)                                                SFLX.2976
      PARAMETER (SIGMA = 5.67E-8)                                                SFLX.2977
      PARAMETER (TFREEZ = 273.15)                                                SFLX.2978
                                                                                 SFLX.2979
! ----------------------------------------------------------------------         SFLX.2980
! EXECUTABLE CODE BEGINS HERE:                                                   SFLX.2981
! INITIALIZE EVAP TERMS.                                                         SFLX.2982
! ----------------------------------------------------------------------         SFLX.2983
! conversions:                                                                   SFLX.2984
! ESNOW [KG M-2 S-1]                                                             SFLX.2985
! ESDFLX [KG M-2 S-1] .le. ESNOW                                                 SFLX.2986
! ESNOW1 [M S-1]                                                                 SFLX.2987
! ESNOW2 [M]                                                                     SFLX.2988
! ETP [KG M-2 S-1]                                                               SFLX.2989
! ETP1 [M S-1]                                                                   SFLX.2990
! ETP2 [M]                                                                       SFLX.2991
! ----------------------------------------------------------------------         SFLX.2992
      DATA SNOEXP /1.0/                                                          SFLX.2993
      EDIR = 0.                                                                  SFLX.2994
      EDIR1 = 0.                                                                 SFLX.2995
      EC1 = 0.                                                                   SFLX.2996
      EC = 0.                                                                    SFLX.2997
      EMISSI_S=0.9     ! For snow                                                23DEC04.3772
      DO K = 1,NSOIL                                                             SFLX.2998
         ET (K) = 0.                                                             SFLX.2999
         ET1 (K) = 0.                                                            SFLX.3000
      END DO                                                                     SFLX.3001
      ETT = 0.                                                                   SFLX.3002
      ETT1 = 0.                                                                  SFLX.3003
      ETNS = 0.                                                                  SFLX.3004
      ETNS1 = 0.                                                                 SFLX.3005
      ESNOW = 0.                                                                 SFLX.3006
      ESNOW1 = 0.                                                                SFLX.3007
      ESNOW2 = 0.                                                                SFLX.3008
                                                                                 SFLX.3009
! CONVERT POTENTIAL EVAP (ETP) FROM KG M-2 S-1 TO M S-1 AND THEN TO AN           SFLX.3010
! AMOUNT (M) GIVEN TIMESTEP (DT) AND CALL IT AN EFFECTIVE SNOWPACK               SFLX.3011
! REDUCTION AMOUNT, ETP2 (M).  THIS IS THE AMOUNT THE SNOWPACK WOULD BE          SFLX.3012
! REDUCED DUE TO EVAPORATION FROM THE SNOW SFC DURING THE TIMESTEP.              SFLX.3013
! EVAPORATION WILL PROCEED AT THE POTENTIAL RATE UNLESS THE SNOW DEPTH           SFLX.3014
! IS LESS THAN THE EXPECTED SNOWPACK REDUCTION.                                  SFLX.3015
! IF SEAICE (ICE=1), BETA REMAINS=1.                                             SFLX.3016
! ----------------------------------------------------------------------         SFLX.3017
!      PRCP1 = PRCP1*0.001                                                       SFLX.3018
! change name of PRCP1 to PRCPF                                                  SFLX.3019
      DEW = 0.                                                                   SFLX.3020
                                                                                 SFLX.3021
      PRCP1 = PRCPF *0.001                                                       SFLX.3022
      ETP1 = ETP * 0.001                                                         SFLX.3023
      ESNOW = ETP * SNCOVR                                                       SFLX.3024
!      write(*,*) 'ESNOW,ESDFLX=',ESNOW,ESDFLX                                   SFLX.3025
!     ESDFLX = ESD *1000./ DT                                                    SFLX.3026
!     IF (ESDFLX .lt. ESNOW) THEN                                                SFLX.3027
!ek        ESD = 0.                                                              SFLX.3028
!        ESNOW = ESDFLX                                                          SFLX.3029
!        ESD = 0.                                                                SFLX.3030
!     ELSE                                                                       SFLX.3031
         ESNOW1 = ESNOW * 0.001                                                  SFLX.3032
!ek        ESD = ESD - ESNOW2                                                    SFLX.3033
         ESNOW2 = ESNOW1 * DT                                                    SFLX.3034
!        ESD = ESD- ESNOW2                                                       SFLX.3035
!     END IF                                                                     SFLX.3036
!     ETP2 = ETP * 0.001 * DT                                                    SFLX.3037
!      IF (ICE .NE. 1) THEN                                                      SFLX.3038
!        IF (ESD .LT. ETP2) THEN                                                 SFLX.3039
!          BETA = ESD / ETP2                                                     SFLX.3040
!        ENDIF                                                                   SFLX.3041
!      ENDIF                                                                     SFLX.3042
                                                                                 SFLX.3043
! ----------------------------------------------------------------------         SFLX.3044
! IF ETP<0 (DOWNWARD) THEN DEWFALL (=FROSTFALL IN THIS CASE).                    SFLX.3045
! ----------------------------------------------------------------------         SFLX.3046
      BETA = 1.0                                                                 SFLX.3047
      IF (ETP .le. 0.0) THEN                                                     SFLX.3048
        IF(ETP.EQ.0.) BETA = 0.0                                                 SFLX.3049
!      ENDIF                                                                     SFLX.3050
         DEW = - ETP * 0.001                                                     SFLX.3051
!        ETP1 = 0.0                                                              SFLX.3052
      ELSE                                                                       SFLX.3053
         IF (ICE .ne. 1) THEN                                                    SFLX.3054
!           IF (ESD .lt. ETP2) THEN                                              SFLX.3055
!              BETA = ESD / ETP2                                                 SFLX.3056
!           END IF                                                               SFLX.3057
!            write(*,*) 'ETP1=',ETP1                                             SFLX.3058
             IF (SNCOVR .lt. 1.) THEN                                            SFLX.3059
               CALL EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,            &        SFLX.3060
     &                      SH2O,                                       &        SFLX.3061
     &                      SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,          &        SFLX.3062
     &                      SMCREF,SHDFAC,CMCMAX,                       &        SFLX.3063
     &                      SMCDRY,CFACTR,                              &        SFLX.3064
     &                      EDIR1,EC1,ET1,ETT1,SFCTMP,Q2,NROOT,RTDIS,   &        SFLX.3065
     &                      FXEXP)                                               SFLX.3066
               EDIR1 = EDIR1* (1. - SNCOVR)                                      SFLX.3067
               EC1 = EC1* (1. - SNCOVR)                                          SFLX.3068
               DO K = 1,NSOIL                                                    SFLX.3069
                  ET1 (K) = ET1 (K)* (1. - SNCOVR)                               SFLX.3070
               END DO                                                            SFLX.3071
               ETT1 = ETT1*(1.-SNCOVR)                                           SFLX.3072
               ETNS1 = EDIR1+ EC1+ ETT1                                          SFLX.3073
               EDIR = EDIR1*1000.                                                SFLX.3074
               EC = EC1*1000.                                                    SFLX.3075
               DO K = 1,NSOIL                                                    SFLX.3076
                  ET (K) = ET1 (K)*1000.                                         SFLX.3077
               END DO                                                            SFLX.3078
               ETT = ETT1*1000.                                                  SFLX.3079
               ETNS = ETNS1*1000.                                                SFLX.3080
!            write(*,*) EDIR*2.5E+9,                                             SFLX.3081
!     &                 EC*2.5E+9,                                               SFLX.3082
!     &                 ET(1)*2.5E+9,                                            SFLX.3083
!     &                 ET(2)*2.5E+9,                                            SFLX.3084
!     &                 ET(3)*2.5E+9,                                            SFLX.3085
!     &                 ET(4)*2.5E+9,                                            SFLX.3086
!     &                 ETT*2.5E+9                                               SFLX.3087
!            write(*,*) 'SNCOVR=',SNCOVR                                         SFLX.3088
               ETNS = ETNS1*1000.                                                SFLX.3089
!            write(*,*) 'ESNOW[W/M2],ETNS[W/M2]=',                               SFLX.3090
!     &                  ESNOW*LSUBS,ETNS*LSUBC                                  SFLX.3091
!            write(*,*) 'ETP[W/M2],ETANRG=',                                     SFLX.3092
!     &                  ETP*LSUBS,ETANRG                                        SFLX.3093
! ----------------------------------------------------------------------         SFLX.3094
                                                                                 SFLX.3095
!   end IF (SNCOVR .lt. 1.)                                                      SFLX.3096
            END IF                                                               SFLX.3097
                                                                                 SFLX.3098
!   end IF (ICE .ne. 1)                                                          SFLX.3099
         END IF                                                                  SFLX.3100
                                                                                 SFLX.3101
!   end IF (ETP .le. 0.0)                                                        SFLX.3102
      END IF                                                                     SFLX.3103
! ----------------------------------------------------------------------         SFLX.3104
! COMPUTE TOTAL EVAPORATION, SNOW AND NON-SNOW                                   SFLX.3105
! also compute energy units (ETANRG) needed later below                          SFLX.3106
! ----------------------------------------------------------------------         SFLX.3107
                                                                                 SFLX.3108
!      write(*,*)'ESNOW*LSUBS,ETNS*LSUBC,ETANRG=',                               SFLX.3109
!     &           ESNOW*LSUBS,ETNS*LSUBC,ETANRG                                  SFLX.3110
      ETANRG = ESNOW * LSUBS + ETNS * LSUBC                                      SFLX.3111
                                                                                 SFLX.3112
! ----------------------------------------------------------------------         SFLX.3113
! IF PRECIP IS FALLING, CALCULATE HEAT FLUX FROM SNOW SFC TO NEWLY               SFLX.3114
! ACCUMULATING PRECIP.  NOTE THAT THIS REFLECTS THE FLUX APPROPRIATE FOR         SFLX.3115
! THE NOT-YET-UPDATED SKIN TEMPERATURE (T1).  ASSUMES TEMPERATURE OF THE         SFLX.3116
! SNOWFALL STRIKING THE GROUND IS =SFCTMP (LOWEST MODEL LEVEL AIR TEMP).         SFLX.3117
! ----------------------------------------------------------------------         SFLX.3118
      FLX1 = 0.0                                                                 SFLX.3119
      IF (SNOWNG) THEN                                                           SFLX.3120
         FLX1 = CPICE * PRCP * (T1- SFCTMP)                                      SFLX.3121
      ELSE                                                                       SFLX.3122
         IF (PRCP .gt. 0.0) FLX1 = CPH2O * PRCP * (T1- SFCTMP)                   SFLX.3123
! ----------------------------------------------------------------------         SFLX.3124
! CALCULATE AN 'EFFECTIVE SNOW-GRND SFC TEMP' (T12) BASED ON HEAT FLUXES         SFLX.3125
! BETWEEN THE SNOW PACK AND THE SOIL AND ON NET RADIATION.                       SFLX.3126
! INCLUDE FLX1 (PRECIP-SNOW SFC) AND FLX2 (FREEZING RAIN LATENT HEAT)            SFLX.3127
! FLUXES.  FLX1 FROM ABOVE, FLX2 BROUGHT IN VIA COMMOM BLOCK RITE.               SFLX.3128
! FLX2 REFLECTS FREEZING RAIN LATENT HEAT FLUX USING T1 CALCULATED IN            SFLX.3129
! PENMAN.                                                                        SFLX.3130
! ----------------------------------------------------------------------         SFLX.3131
      END IF                                                                     SFLX.3132
      DSOIL = - (0.5 * ZSOIL (1))                                                SFLX.3133
      DTOT = SNOWH + DSOIL                                                       SFLX.3134
!ek      T12A = ( (FDOWN-FLX1-FLX2-SIGMA*T24)/RCH                                SFLX.3135
!ek     &       + TH2 - SFCTMP - BETA*EPSCA ) / RR                               SFLX.3136
!ek      T12AX = ( (FDOWN-FLX1-FLX2-SIGMA*T24)/RCH                               SFLX.3137
!ek     &       + TH2 - SFCTMP - ETANRG/RCH ) / RR                               SFLX.3138
      DENOM = 1.0+ DF1 / (DTOT * RR * RCH)                                       SFLX.3139
!      write(*,*) 'T12A,T12AX=',T12A,T12AX                                       SFLX.3140
!      T12A = ( (FDOWN - FLX1- FLX2- SIGMA * T24)/ RCH                  &        23DEC04.3773
!Fei: emmissivity                                                                23DEC04.3774
!      T12A = ( (FDOWN - FLX1- FLX2- EMISSI*SIGMA * T24)/ RCH           &        23DEC04.3775
!Fei: snow emissi=0.90                                                           23DEC04.3776
!     T12A = ( (FDOWN - FLX1- FLX2- 0.90*SIGMA * T24)/ RCH              &        23DEC04.3777
!      T12A = ( (FDOWN - FLX1- FLX2- EMISSI_S*SIGMA * T1**4)/ RCH       &        23DEC04.3778
!      T12A = ( (FDOWN - FLX1- FLX2- EMISSI_S*SIGMA * T24)/ RCH         &        23DEC04.3779
      T12A = ( (FDOWN - FLX1- FLX2- EMISSI_S*SIGMA * T1**4)/ RCH        &        23DEC04.3780
     &       + TH2- SFCTMP - ETANRG / RCH ) / RR                                 SFLX.3142
      T12B = DF1 * STC (1) / (DTOT * RR * RCH)                                   SFLX.3143
                                                                                 SFLX.3144
! ----------------------------------------------------------------------         SFLX.3145
! IF THE 'EFFECTIVE SNOW-GRND SFC TEMP' IS AT OR BELOW FREEZING, NO SNOW         SFLX.3146
! MELT WILL OCCUR.  SET THE SKIN TEMP TO THIS EFFECTIVE TEMP.  REDUCE            SFLX.3147
! (BY SUBLIMINATION ) OR INCREASE (BY FROST) THE DEPTH OF THE SNOWPACK,          SFLX.3148
! DEPENDING ON SIGN OF ETP.                                                      SFLX.3149
! UPDATE SOIL HEAT FLUX (SSOIL) USING NEW SKIN TEMPERATURE (T1)                  SFLX.3150
! SINCE NO SNOWMELT, SET ACCUMULATED SNOWMELT TO ZERO, SET 'EFFECTIVE'           SFLX.3151
! PRECIP FROM SNOWMELT TO ZERO, SET PHASE-CHANGE HEAT FLUX FROM SNOWMELT         SFLX.3152
! TO ZERO.                                                                       SFLX.3153
! ----------------------------------------------------------------------         SFLX.3154
! SUB-FREEZING BLOCK                                                             SFLX.3155
! ----------------------------------------------------------------------         SFLX.3156
      T12 = (SFCTMP + T12A + T12B) / DENOM                                       SFLX.3157
      IF (T12 .le. TFREEZ) THEN                                                  SFLX.3158
         T1 = T12                                                                SFLX.3159
         SSOIL = DF1 * (T1- STC (1)) / DTOT                                      SFLX.3160
!        ESD = MAX (0.0, ESD- ETP2)                                              SFLX.3161
         ESD = MAX(0.0, ESD-ESNOW2)                                              SFLX.3162
         FLX3 = 0.0                                                              SFLX.3163
         EX = 0.0                                                                SFLX.3164
                                                                                 SFLX.3165
         SNOMLT = 0.0                                                            SFLX.3166
! ----------------------------------------------------------------------         SFLX.3167
! IF THE 'EFFECTIVE SNOW-GRND SFC TEMP' IS ABOVE FREEZING, SNOW MELT             SFLX.3168
! WILL OCCUR.  CALL THE SNOW MELT RATE,EX AND AMT, SNOMLT.  REVISE THE           SFLX.3169
! EFFECTIVE SNOW DEPTH.  REVISE THE SKIN TEMP BECAUSE IT WOULD HAVE CHGD         SFLX.3170
! DUE TO THE LATENT HEAT RELEASED BY THE MELTING. CALC THE LATENT HEAT           SFLX.3171
! RELEASED, FLX3. SET THE EFFECTIVE PRECIP, PRCP1 TO THE SNOW MELT RATE,         SFLX.3172
! EX FOR USE IN SMFLX.  ADJUSTMENT TO T1 TO ACCOUNT FOR SNOW PATCHES.            SFLX.3173
! CALCULATE QSAT VALID AT FREEZING POINT.  NOTE THAT ESAT (SATURATION            SFLX.3174
! VAPOR PRESSURE) VALUE OF 6.11E+2 USED HERE IS THAT VALID AT FRZZING            SFLX.3175
! POINT.  NOTE THAT ETP FROM CALL PENMAN IN SFLX IS IGNORED HERE IN              SFLX.3176
! FAVOR OF BULK ETP OVER 'OPEN WATER' AT FREEZING TEMP.                          SFLX.3177
! UPDATE SOIL HEAT FLUX (S) USING NEW SKIN TEMPERATURE (T1)                      SFLX.3178
! ----------------------------------------------------------------------         SFLX.3179
! ABOVE FREEZING BLOCK                                                           SFLX.3180
! ----------------------------------------------------------------------         SFLX.3181
!ek2        T1 = TFREEZ * SNCOVR + T12 * (1.0 - SNCOVR)                          SFLX.3182
      ELSE                                                                       SFLX.3183
!ek        QSAT = (0.622*6.11E2)/(SFCPRS-0.378*6.11E2)                           SFLX.3184
!ek        ETP = RCH*(QSAT-Q2)/CP                                                SFLX.3185
!ek        ETP2 = ETP*0.001*DT                                                   SFLX.3186
         T1 = TFREEZ * SNCOVR ** SNOEXP + T12 * (1.0- SNCOVR ** SNOEXP)          SFLX.3187
         BETA = 1.0                                                              SFLX.3188
                                                                                 SFLX.3189
! ----------------------------------------------------------------------         SFLX.3190
! IF POTENTIAL EVAP (SUBLIMATION) GREATER THAN DEPTH OF SNOWPACK.                SFLX.3191
! BETA<1                                                                         SFLX.3192
! SNOWPACK HAS SUBLIMATED AWAY, SET DEPTH TO ZERO.                               SFLX.3193
! ----------------------------------------------------------------------         SFLX.3194
         SSOIL = DF1 * (T1- STC (1)) / DTOT                                      SFLX.3195
!        IF (ESD .le. ETP2) THEN                                                 SFLX.3196
!           BETA = ESD / ETP2                                                    SFLX.3197
         IF (ESD .LE. ESNOW2) THEN                                               SFLX.3198
            ESD = 0.0                                                            SFLX.3199
            EX = 0.0                                                             SFLX.3200
            SNOMLT = 0.0                                                         SFLX.3201
! ----------------------------------------------------------------------         SFLX.3202
! POTENTIAL EVAP (SUBLIMATION) LESS THAN DEPTH OF SNOWPACK, RETAIN               SFLX.3203
!   BETA=1.                                                                      SFLX.3204
! SNOWPACK (ESD) REDUCED BY POTENTIAL EVAP RATE                                  SFLX.3205
! ETP3 (CONVERT TO FLUX)                                                         SFLX.3206
! ----------------------------------------------------------------------         SFLX.3207
         ELSE                                                                    SFLX.3208
!           ESD = ESD- ETP2                                                      SFLX.3209
            ESD = ESD-ESNOW2                                                     SFLX.3210
            ETP3 = ETP * LSUBC                                                   SFLX.3211
!         WRITE (*,*) 'SNCOVR=',SNCOVR                                           SFLX.3212
!         WRITE (*,*) 'ETP3,ETANRG=',ETP3,ETANRG,'ETP',ETP,'LSUBC',LSUBC         SFLX.3213
            SEH = RCH * (T1- TH2)                                                SFLX.3214
            T14 = T1* T1                                                         SFLX.3215
!ek          FLX3 = FDOWN - FLX1 - FLX2 - SIGMA*T14 - SSOIL - SEH - ETP3         SFLX.3216
!ek          FLX3X = FDOWN - FLX1 - FLX2 - SIGMA*T14 - SSOIL - SEH - ETANRG      SFLX.3217
            T14 = T14* T14                                                       SFLX.3218
!           FLX3 = FDOWN - FLX1- FLX2- SIGMA * T14- SSOIL - SEH - ETANRG         23DEC04.3781
!Fei: emmissivity                                                                23DEC04.3782
!           FLX3 = FDOWN - FLX1- FLX2- EMISSI*SIGMA * T14- SSOIL- SEH-    &      23DEC04.3783
           FLX3 = FDOWN - FLX1- FLX2- EMISSI_S*SIGMA * T14- SSOIL- SEH-   &      23DEC04.3784
     &            ETANRG                                                         23DEC04.3785
!            WRITE (*,*) 'FLX3,FLX3X=',FLX3,FLX3X                                SFLX.3220
            IF (FLX3 .LE.0.0) FLX3 = 0.0                                         SFLX.3221
! ----------------------------------------------------------------------         SFLX.3222
! SNOWMELT REDUCTION DEPENDING ON SNOW COVER                                     SFLX.3223
! IF SNOW COVER LESS THAN 5% NO SNOWMELT REDUCTION                               SFLX.3224
! ***NOTE:  DOES 'IF' BELOW FAIL TO MATCH THE MELT WATER WITH THE MELT           SFLX.3225
!           ENERGY?                                                              SFLX.3226
! ----------------------------------------------------------------------         SFLX.3227
            EX = FLX3*0.001/ LSUBF                                               SFLX.3228
!           IF (SNCOVR .gt. 0.05) EX = EX * SNCOVR                               SFLX.3229
                                                                                 SFLX.3230
! ----------------------------------------------------------------------         SFLX.3231
! ESDMIN REPRESENTS A SNOWPACK DEPTH THRESHOLD VALUE BELOW WHICH WE              SFLX.3232
! CHOOSE NOT TO RETAIN ANY SNOWPACK, AND INSTEAD INCLUDE IT IN SNOWMELT.         SFLX.3233
! ----------------------------------------------------------------------         SFLX.3234
            SNOMLT = EX * DT                                                     SFLX.3235
            IF (ESD- SNOMLT .ge. ESDMIN) THEN                                    SFLX.3236
               ESD = ESD- SNOMLT                                                 SFLX.3237
! ----------------------------------------------------------------------         SFLX.3238
! SNOWMELT EXCEEDS SNOW DEPTH                                                    SFLX.3239
! ----------------------------------------------------------------------         SFLX.3240
            ELSE                                                                 SFLX.3241
               EX = ESD / DT                                                     SFLX.3242
               FLX3 = EX *1000.0* LSUBF                                          SFLX.3243
               SNOMLT = ESD                                                      SFLX.3244
                                                                                 SFLX.3245
               ESD = 0.0                                                         SFLX.3246
! ----------------------------------------------------------------------         SFLX.3247
! END OF 'ESD .LE. ETP2' IF-BLOCK                                                SFLX.3248
! ----------------------------------------------------------------------         SFLX.3249
            END IF                                                               SFLX.3250
         END IF                                                                  SFLX.3251
                                                                                 SFLX.3252
! ----------------------------------------------------------------------         SFLX.3253
! END OF 'T12 .LE. TFREEZ' IF-BLOCK                                              SFLX.3254
! ----------------------------------------------------------------------         SFLX.3255
         PRCP1 = PRCP1+ EX                                                       SFLX.3256
! ----------------------------------------------------------------------         SFLX.3257
! FINAL BETA NOW IN HAND, SO COMPUTE EVAPORATION.  EVAP EQUALS ETP               SFLX.3258
! UNLESS BETA<1.                                                                 SFLX.3259
! ----------------------------------------------------------------------         SFLX.3260
!ek      ETA = BETA*ETP                                                          SFLX.3261
!      write(*,*) 'ETA,ETAX,ETAX/ETA=',ETA,ETAX,ETAX/ETA                         SFLX.3262
!      IF (ETAX/ETA .GE. 1.0) THEN                                               SFLX.3263
!        write(*,*) '*********************'                                      SFLX.3264
!        write(*,*) '*********************'                                      SFLX.3265
!        write(*,*) '*********************'                                      SFLX.3266
!        write(*,*) 'ETAX/ETA .GE. 1.0 !!!'                                      SFLX.3267
!        write(*,*) '*********************'                                      SFLX.3268
!        write(*,*) '*********************'                                      SFLX.3269
!        write(*,*) '*********************'                                      SFLX.3270
!      ENDIF                                                                     SFLX.3271
                                                                                 SFLX.3272
! ----------------------------------------------------------------------         SFLX.3273
! SET THE EFFECTIVE POTNL EVAPOTRANSP (ETP1) TO ZERO SINCE THIS IS SNOW          SFLX.3274
! CASE, SO SURFACE EVAP NOT CALCULATED FROM EDIR, EC, OR ETT IN SMFLX            SFLX.3275
! (BELOW).                                                                       SFLX.3276
! IF SEAICE (ICE=1) SKIP CALL TO SMFLX.                                          SFLX.3277
! SMFLX RETURNS UPDATED SOIL MOISTURE VALUES.  IN THIS, THE SNOW PACK            SFLX.3278
! CASE, ETA1 IS NOT USED IN CALCULATION OF EVAP.                                 SFLX.3279
! ----------------------------------------------------------------------         SFLX.3280
!      ETP1 = 0.0                                                                SFLX.3281
      END IF                                                                     SFLX.3282
!        CALL EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,                           SFLX.3283
!     &              SH2O,                                                       SFLX.3284
!     &              SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,                          SFLX.3285
!     &              SMCREF,SHDFAC,CMCMAX,                                       SFLX.3286
!     &              SMCDRY,CFACTR,                                              SFLX.3287
!     &              EDIR,EC,ET,ETT,SFCTMP,Q2,NROOT,RTDIS,FXEXP)                 SFLX.3288
      IF (ICE .ne. 1) THEN                                                       SFLX.3289
         CALL SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,                      &        SFLX.3290
     &                SH2O,SLOPE,KDT,FRZFACT,                           &        SFLX.3291
     &                SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,                   &        SFLX.3292
     &                SHDFAC,CMCMAX,                                    &        SFLX.3293
     &                RUNOFF1,RUNOFF2,RUNOFF3,                          &        SFLX.3294
!    &                EDIR,EC,ET,                                       &        SFLX.3295
     &              EDIR1,EC1,ET1,                                      &        SFLX.3296
     &                DRIP)                                                      SFLX.3297
! ----------------------------------------------------------------------         SFLX.3298
! BEFORE CALL SHFLX IN THIS SNOWPACK CASE, SET ZZ1 AND YY ARGUMENTS TO           SFLX.3299
! SPECIAL VALUES THAT ENSURE THAT GROUND HEAT FLUX CALCULATED IN SHFLX           SFLX.3300
! MATCHES THAT ALREADY COMPUTER FOR BELOW THE SNOWPACK, THUS THE SFC             SFLX.3301
! HEAT FLUX TO BE COMPUTED IN SHFLX WILL EFFECTIVELY BE THE FLUX AT THE          SFLX.3302
! SNOW TOP SURFACE.  T11 IS A DUMMY ARGUEMENT SO WE WILL NOT USE THE             SFLX.3303
! SKIN TEMP VALUE AS REVISED BY SHFLX.                                           SFLX.3304
! ----------------------------------------------------------------------         SFLX.3305
      END IF                                                                     SFLX.3306
      ZZ1 = 1.0                                                                  SFLX.3307
      YY = STC (1) -0.5* SSOIL * ZSOIL (1)* ZZ1/ DF1                             SFLX.3308
                                                                                 SFLX.3309
! ----------------------------------------------------------------------         SFLX.3310
! SHFLX WILL CALC/UPDATE THE SOIL TEMPS.  NOTE:  THE SUB-SFC HEAT FLUX           SFLX.3311
! (SSOIL1) AND THE SKIN TEMP (T11) OUTPUT FROM THIS SHFLX CALL ARE NOT           SFLX.3312
! USED  IN ANY SUBSEQUENT CALCULATIONS. RATHER, THEY ARE DUMMY VARIABLES         SFLX.3313
! HERE IN THE SNOPAC CASE, SINCE THE SKIN TEMP AND SUB-SFC HEAT FLUX ARE         SFLX.3314
! UPDATED INSTEAD NEAR THE BEGINNING OF THE CALL TO SNOPAC.                      SFLX.3315
! ----------------------------------------------------------------------         SFLX.3316
      T11 = T1                                                                   SFLX.3317
!urban                                                                           23DEC04.3786
      CALL SHFLX (SSOIL1,STC,SMC,SMCMAX,NSOIL,T11,DT,YY,ZZ1,ZSOIL,      &        SFLX.3318
     &             TBOT,ZBOT,SMCWLT,PSISAT,SH2O,BEXP,F1,DF1,ICE,        &        SFLX.3319
     &             QUARTZ,CSOIL,VEGTYP)                                          23DEC04.3787
                                                                                 SFLX.3321
! ----------------------------------------------------------------------         SFLX.3322
! SNOW DEPTH AND DENSITY ADJUSTMENT BASED ON SNOW COMPACTION.  YY IS             SFLX.3323
! ASSUMED TO BE THE SOIL TEMPERTURE AT THE TOP OF THE SOIL COLUMN.               SFLX.3324
! ----------------------------------------------------------------------         SFLX.3325
      IF (ESD .gt. 0.) THEN                                                      SFLX.3326
         CALL SNOWPACK (ESD,DT,SNOWH,SNDENS,T1,YY)                               SFLX.3327
      ELSE                                                                       SFLX.3328
         ESD = 0.                                                                SFLX.3329
         SNOWH = 0.                                                              SFLX.3330
         SNDENS = 0.                                                             SFLX.3331
         SNCOND = 1.                                                             SFLX.3332
      END IF                                                                     SFLX.3333
      END                                                                        SFLX.3334
! ----------------------------------------------------------------------         SFLX.3335
! END SUBROUTINE SNOPAC                                                          SFLX.3336
! ----------------------------------------------------------------------         SFLX.3337
                                                                                 SFLX.3338
                                                                                 SFLX.3339
      SUBROUTINE SNOWPACK (ESD,DTSEC,SNOWH,SNDENS,TSNOW,TSOIL)                   SFLX.3340
                                                                                 SFLX.3341
! ----------------------------------------------------------------------         SFLX.3342
! SUBROUTINE SNOWPACK                                                            SFLX.3343
! ----------------------------------------------------------------------         SFLX.3344
! CALCULATE COMPACTION OF SNOWPACK UNDER CONDITIONS OF INCREASING SNOW           SFLX.3345
! DENSITY, AS OBTAINED FROM AN APPROXIMATE SOLUTION OF E. ANDERSON'S             SFLX.3346
! DIFFERENTIAL EQUATION (3.29), NOAA TECHNICAL REPORT NWS 19, BY VICTOR          SFLX.3347
! KOREN, 03/25/95.                                                               SFLX.3348
! ----------------------------------------------------------------------         SFLX.3349
! ESD     WATER EQUIVALENT OF SNOW (M)                                           SFLX.3350
! DTSEC   TIME STEP (SEC)                                                        SFLX.3351
! SNOWH   SNOW DEPTH (M)                                                         SFLX.3352
! SNDENS  SNOW DENSITY (G/CM3=DIMENSIONLESS FRACTION OF H2O DENSITY)             SFLX.3353
! TSNOW   SNOW SURFACE TEMPERATURE (K)                                           SFLX.3354
! TSOIL   SOIL SURFACE TEMPERATURE (K)                                           SFLX.3355
                                                                                 SFLX.3356
! SUBROUTINE WILL RETURN NEW VALUES OF SNOWH AND SNDENS                          SFLX.3357
! ----------------------------------------------------------------------         SFLX.3358
      IMPLICIT NONE                                                              SFLX.3359
                                                                                 SFLX.3360
      INTEGER  IPOL, J                                                           SFLX.3361
      REAL     BFAC,C1,C2,SNDENS,DSX,DTHR,DTSEC,DW,SNOWHC,SNOWH,PEXP,     &      SFLX.3362
     &         TAVGC,                                                     &      SFLX.3363
     &             TSNOW,TSNOWC,TSOIL,TSOILC,ESD,ESDC,ESDCX,G,KN                 SFLX.3364
                                                                                 SFLX.3365
! ----------------------------------------------------------------------         SFLX.3366
! CONVERSION INTO SIMULATION UNITS                                               SFLX.3367
! ----------------------------------------------------------------------         SFLX.3368
      PARAMETER (C1 = 0.01, C2 = 21.0, G = 9.81, KN = 4000.0)                    SFLX.3369
      SNOWHC = SNOWH *100.                                                       SFLX.3370
      ESDC = ESD *100.                                                           SFLX.3371
      DTHR = DTSEC /3600.                                                        SFLX.3372
      TSNOWC = TSNOW -273.15                                                     SFLX.3373
                                                                                 SFLX.3374
! ----------------------------------------------------------------------         SFLX.3375
! CALCULATING OF AVERAGE TEMPERATURE OF SNOW PACK                                SFLX.3376
! ----------------------------------------------------------------------         SFLX.3377
      TSOILC = TSOIL -273.15                                                     SFLX.3378
                                                                                 SFLX.3379
! ----------------------------------------------------------------------         SFLX.3380
! CALCULATING OF SNOW DEPTH AND DENSITY AS A RESULT OF COMPACTION                SFLX.3381
!  SNDENS=DS0*(EXP(BFAC*ESD)-1.)/(BFAC*ESD)                                      SFLX.3382
!  BFAC=DTHR*C1*EXP(0.08*TAVGC-C2*DS0)                                           SFLX.3383
! NOTE: BFAC*ESD IN SNDENS EQN ABOVE HAS TO BE CAREFULLY TREATED                 SFLX.3384
! NUMERICALLY BELOW:                                                             SFLX.3385
!   C1 IS THE FRACTIONAL INCREASE IN DENSITY (1/(CM*HR))                         SFLX.3386
!   C2 IS A CONSTANT (CM3/G) KOJIMA ESTIMATED AS 21 CMS/G                        SFLX.3387
! ----------------------------------------------------------------------         SFLX.3388
      TAVGC = 0.5* (TSNOWC + TSOILC)                                             SFLX.3389
      IF (ESDC .gt. 1.E-2) THEN                                                  SFLX.3390
         ESDCX = ESDC                                                            SFLX.3391
      ELSE                                                                       SFLX.3392
         ESDCX = 1.E-2                                                           SFLX.3393
      END IF                                                                     SFLX.3394
                                                                                 SFLX.3395
!      DSX = SNDENS*((DEXP(BFAC*ESDC)-1.)/(BFAC*ESDC))                           SFLX.3396
! ----------------------------------------------------------------------         SFLX.3397
! THE FUNCTION OF THE FORM (e**x-1)/x IMBEDDED IN ABOVE EXPRESSION               SFLX.3398
! FOR DSX WAS CAUSING NUMERICAL DIFFICULTIES WHEN THE DENOMINATOR "x"            SFLX.3399
! (I.E. BFAC*ESDC) BECAME ZERO OR APPROACHED ZERO (DESPITE THE FACT THAT         SFLX.3400
! THE ANALYTICAL FUNCTION (e**x-1)/x HAS A WELL DEFINED LIMIT AS                 SFLX.3401
! "x" APPROACHES ZERO), HENCE BELOW WE REPLACE THE (e**x-1)/x                    SFLX.3402
! EXPRESSION WITH AN EQUIVALENT, NUMERICALLY WELL-BEHAVED                        SFLX.3403
! POLYNOMIAL EXPANSION.                                                          SFLX.3404
                                                                                 SFLX.3405
! NUMBER OF TERMS OF POLYNOMIAL EXPANSION, AND HENCE ITS ACCURACY,               SFLX.3406
! IS GOVERNED BY ITERATION LIMIT "IPOL".                                         SFLX.3407
!      IPOL GREATER THAN 9 ONLY MAKES A DIFFERENCE ON DOUBLE                     SFLX.3408
!            PRECISION (RELATIVE ERRORS GIVEN IN PERCENT %).                     SFLX.3409
!       IPOL=9, FOR REL.ERROR <~ 1.6 E-6 % (8 SIGNIFICANT DIGITS)                SFLX.3410
!       IPOL=8, FOR REL.ERROR <~ 1.8 E-5 % (7 SIGNIFICANT DIGITS)                SFLX.3411
!       IPOL=7, FOR REL.ERROR <~ 1.8 E-4 % ...                                   SFLX.3412
! ----------------------------------------------------------------------         SFLX.3413
      BFAC = DTHR * C1* EXP (0.08* TAVGC - C2* SNDENS)                           SFLX.3414
      IPOL = 4                                                                   SFLX.3415
      PEXP = 0.                                                                  SFLX.3416
!        PEXP = (1. + PEXP)*BFAC*ESDC/REAL(J+1)                                  SFLX.3417
      DO J = IPOL,1, -1                                                          SFLX.3418
         PEXP = (1. + PEXP)* BFAC * ESDCX / REAL (J +1)                          SFLX.3419
      END DO                                                                     SFLX.3420
                                                                                 SFLX.3421
      PEXP = PEXP + 1.                                                           SFLX.3422
! ----------------------------------------------------------------------         SFLX.3423
! ABOVE LINE ENDS POLYNOMIAL SUBSTITUTION                                        SFLX.3424
! ----------------------------------------------------------------------         SFLX.3425
!     END OF KOREAN FORMULATION                                                  SFLX.3426
                                                                                 SFLX.3427
!     BASE FORMULATION (COGLEY ET AL., 1990)                                     SFLX.3428
!     CONVERT DENSITY FROM G/CM3 TO KG/M3                                        SFLX.3429
!       DSM=SNDENS*1000.0                                                        SFLX.3430
                                                                                 SFLX.3431
!       DSX=DSM+DTSEC*0.5*DSM*G*ESD/                                             SFLX.3432
!    &      (1E7*EXP(-0.02*DSM+KN/(TAVGC+273.16)-14.643))                        SFLX.3433
                                                                                 SFLX.3434
!  &   CONVERT DENSITY FROM KG/M3 TO G/CM3                                       SFLX.3435
!       DSX=DSX/1000.0                                                           SFLX.3436
                                                                                 SFLX.3437
!     END OF COGLEY ET AL. FORMULATION                                           SFLX.3438
                                                                                 SFLX.3439
! ----------------------------------------------------------------------         SFLX.3440
! SET UPPER/LOWER LIMIT ON SNOW DENSITY                                          SFLX.3441
! ----------------------------------------------------------------------         SFLX.3442
      DSX = SNDENS * (PEXP)                                                      SFLX.3443
      IF (DSX .gt. 0.40) DSX = 0.40                                              SFLX.3444
      IF (DSX .lt. 0.05) DSX = 0.05                                              SFLX.3445
! ----------------------------------------------------------------------         SFLX.3446
! UPDATE OF SNOW DEPTH AND DENSITY DEPENDING ON LIQUID WATER DURING              SFLX.3447
! SNOWMELT.  ASSUMED THAT 13% OF LIQUID WATER CAN BE STORED IN SNOW PER          SFLX.3448
! DAY DURING SNOWMELT TILL SNOW DENSITY 0.40.                                    SFLX.3449
! ----------------------------------------------------------------------         SFLX.3450
      SNDENS = DSX                                                               SFLX.3451
      IF (TSNOWC .ge. 0.) THEN                                                   SFLX.3452
         DW = 0.13* DTHR /24.                                                    SFLX.3453
         SNDENS = SNDENS * (1. - DW) + DW                                        SFLX.3454
         IF (SNDENS .gt. 0.40) SNDENS = 0.40                                     SFLX.3455
! ----------------------------------------------------------------------         SFLX.3456
! CALCULATE SNOW DEPTH (CM) FROM SNOW WATER EQUIVALENT AND SNOW DENSITY.         SFLX.3457
! CHANGE SNOW DEPTH UNITS TO METERS                                              SFLX.3458
! ----------------------------------------------------------------------         SFLX.3459
      END IF                                                                     SFLX.3460
      SNOWHC = ESDC / SNDENS                                                     SFLX.3461
                                                                                 SFLX.3462
! ----------------------------------------------------------------------         SFLX.3463
! END SUBROUTINE SNOWPACK                                                        SFLX.3464
! ----------------------------------------------------------------------         SFLX.3465
      SNOWH = SNOWHC *0.01                                                       SFLX.3466
      END                                                                        SFLX.3467
                                                                                 SFLX.3468
                                                                                 SFLX.3469
      SUBROUTINE SNOWZ0 (SNCOVR,Z0, Z0BRD)                                       SFLX.3470
                                                                                 SFLX.3471
! ----------------------------------------------------------------------         SFLX.3472
! SUBROUTINE SNOWZ0                                                              SFLX.3473
! ----------------------------------------------------------------------         SFLX.3474
! CALCULATE TOTAL ROUGHNESS LENGTH OVER SNOW                                     SFLX.3475
! SNCOVR  FRACTIONAL SNOW COVER                                                  SFLX.3476
! Z0      ROUGHNESS LENGTH (m)                                                   SFLX.3477
! Z0S     SNOW ROUGHNESS LENGTH:=0.001 (m)                                       SFLX.3478
! ----------------------------------------------------------------------         SFLX.3479
      IMPLICIT NONE                                                              SFLX.3480
      REAL   SNCOVR, Z0, Z0S, Z0BRD                                              SFLX.3481
      PARAMETER (Z0S=0.001)                                                      SFLX.3482
                                                                                 SFLX.3483
! CURRENT NOAH LSM CONDITION - MBEK, 09-OCT-2001                                 SFLX.3484
!     Z0S = Z0BRD                                                                SFLX.3485
! ----------------------------------------------------------------------         SFLX.3486
! END SUBROUTINE SNOWZ0                                                          SFLX.3487
! ----------------------------------------------------------------------         SFLX.3488
      Z0 = (1.- SNCOVR)* Z0BRD + SNCOVR * Z0S                                    SFLX.3489
      END                                                                        SFLX.3490
                                                                                 SFLX.3491
                                                                                 SFLX.3492
      SUBROUTINE SNOW_NEW (TEMP,NEWSN,SNOWH,SNDENS)                              SFLX.3493
                                                                                 SFLX.3494
! ----------------------------------------------------------------------         SFLX.3495
! SUBROUTINE SNOW_NEW                                                            SFLX.3496
! ----------------------------------------------------------------------         SFLX.3497
! CALCULATE SNOW DEPTH AND DENSITITY TO ACCOUNT FOR THE NEW SNOWFALL.            SFLX.3498
! NEW VALUES OF SNOW DEPTH & DENSITY RETURNED.                                   SFLX.3499
                                                                                 SFLX.3500
! TEMP    AIR TEMPERATURE (K)                                                    SFLX.3501
! NEWSN   NEW SNOWFALL (M)                                                       SFLX.3502
! SNOWH   SNOW DEPTH (M)                                                         SFLX.3503
! SNDENS  SNOW DENSITY (G/CM3=DIMENSIONLESS FRACTION OF H2O DENSITY)             SFLX.3504
! ----------------------------------------------------------------------         SFLX.3505
      IMPLICIT NONE                                                              SFLX.3506
      REAL     SNDENS                                                            SFLX.3507
      REAL     DSNEW                                                             SFLX.3508
      REAL     SNOWHC                                                            SFLX.3509
      REAL     HNEWC                                                             SFLX.3510
      REAL     SNOWH                                                             SFLX.3511
      REAL     NEWSN                                                             SFLX.3512
      REAL     NEWSNC                                                            SFLX.3513
      REAL     TEMP                                                              SFLX.3514
                                                                                 SFLX.3515
! ----------------------------------------------------------------------         SFLX.3516
! CONVERSION INTO SIMULATION UNITS                                               SFLX.3517
! ----------------------------------------------------------------------         SFLX.3518
      REAL     TEMPC                                                             SFLX.3519
      SNOWHC = SNOWH *100.                                                       SFLX.3520
      NEWSNC = NEWSN *100.                                                       SFLX.3521
                                                                                 SFLX.3522
! ----------------------------------------------------------------------         SFLX.3523
! CALCULATING NEW SNOWFALL DENSITY DEPENDING ON TEMPERATURE                      SFLX.3524
! EQUATION FROM GOTTLIB L. 'A GENERAL RUNOFF MODEL FOR SNOWCOVERED               SFLX.3525
! AND GLACIERIZED BASIN', 6TH NORDIC HYDROLOGICAL CONFERENCE,                    SFLX.3526
! VEMADOLEN, SWEDEN, 1980, 172-177PP.                                            SFLX.3527
!-----------------------------------------------------------------------         SFLX.3528
      TEMPC = TEMP -273.15                                                       SFLX.3529
      IF (TEMPC .le. -15.) THEN                                                  SFLX.3530
         DSNEW = 0.05                                                            SFLX.3531
      ELSE                                                                       SFLX.3532
         DSNEW = 0.05+0.0017* (TEMPC +15.)**1.5                                  SFLX.3533
! ----------------------------------------------------------------------         SFLX.3534
! ADJUSTMENT OF SNOW DENSITY DEPENDING ON NEW SNOWFALL                           SFLX.3535
! ----------------------------------------------------------------------         SFLX.3536
      END IF                                                                     SFLX.3537
      HNEWC = NEWSNC / DSNEW                                                     SFLX.3538
      SNDENS = (SNOWHC * SNDENS + HNEWC * DSNEW)/ (SNOWHC + HNEWC)               SFLX.3539
      SNOWHC = SNOWHC + HNEWC                                                    SFLX.3540
                                                                                 SFLX.3541
! ----------------------------------------------------------------------         SFLX.3542
! END SUBROUTINE SNOW_NEW                                                        SFLX.3543
! ----------------------------------------------------------------------         SFLX.3544
      SNOWH = SNOWHC *0.01                                                       SFLX.3545
      END                                                                        SFLX.3546
                                                                                 SFLX.3547
      SUBROUTINE SRT (RHSTT,EDIR,ET,SH2O,SH2OA,NSOIL,PCPDRP,            &        SFLX.3548
     &                 ZSOIL,DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1,           &        SFLX.3549
     &                 RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZX,SICE,AI,BI,CI)           SFLX.3550
                                                                                 SFLX.3551
! ----------------------------------------------------------------------         SFLX.3552
! SUBROUTINE SRT                                                                 SFLX.3553
! ----------------------------------------------------------------------         SFLX.3554
! CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY TERM OF THE SOIL            SFLX.3555
! WATER DIFFUSION EQUATION.  ALSO TO COMPUTE ( PREPARE ) THE MATRIX              SFLX.3556
! COEFFICIENTS FOR THE TRI-DIAGONAL MATRIX OF THE IMPLICIT TIME SCHEME.          SFLX.3557
! ----------------------------------------------------------------------         SFLX.3558
      IMPLICIT NONE                                                              SFLX.3559
      INTEGER  NSOLD                                                             SFLX.3560
                                                                                 SFLX.3561
      PARAMETER (NSOLD = 20)                                                     SFLX.3562
      INTEGER  CVFRZ                                                             SFLX.3563
      INTEGER  IALP1                                                             SFLX.3564
      INTEGER  IOHINF                                                            SFLX.3565
      INTEGER  J                                                                 SFLX.3566
      INTEGER  JJ                                                                SFLX.3567
      INTEGER  K                                                                 SFLX.3568
      INTEGER  KS                                                                SFLX.3569
                                                                                 SFLX.3570
      INTEGER  NSOIL                                                             SFLX.3571
      REAL     ACRT                                                              SFLX.3572
      REAL     AI (NSOLD)                                                        SFLX.3573
      REAL     BEXP                                                              SFLX.3574
      REAL     BI (NSOLD)                                                        SFLX.3575
      REAL     CI (NSOLD)                                                        SFLX.3576
      REAL     DD                                                                SFLX.3577
      REAL     DDT                                                               SFLX.3578
      REAL     DDZ                                                               SFLX.3579
      REAL     DDZ2                                                              SFLX.3580
      REAL     DENOM                                                             SFLX.3581
      REAL     DENOM2                                                            SFLX.3582
      REAL     DICE                                                              SFLX.3583
      REAL     DKSAT                                                             SFLX.3584
      REAL     DMAX (NSOLD)                                                      SFLX.3585
      REAL     DSMDZ                                                             SFLX.3586
      REAL     DSMDZ2                                                            SFLX.3587
      REAL     DT                                                                SFLX.3588
      REAL     DT1                                                               SFLX.3589
      REAL     DWSAT                                                             SFLX.3590
      REAL     EDIR                                                              SFLX.3591
      REAL     ET (NSOIL)                                                        SFLX.3592
      REAL     FCR                                                               SFLX.3593
      REAL     FRZX                                                              SFLX.3594
      REAL     INFMAX                                                            SFLX.3595
      REAL     KDT                                                               SFLX.3596
      REAL     MXSMC                                                             SFLX.3597
      REAL     MXSMC2                                                            SFLX.3598
      REAL     NUMER                                                             SFLX.3599
      REAL     PCPDRP                                                            SFLX.3600
      REAL     PDDUM                                                             SFLX.3601
      REAL     PX                                                                SFLX.3602
      REAL     RHSTT (NSOIL)                                                     SFLX.3603
      REAL     RUNOFF1                                                           SFLX.3604
      REAL     RUNOFF2                                                           SFLX.3605
      REAL     SH2O (NSOIL)                                                      SFLX.3606
      REAL     SH2OA (NSOIL)                                                     SFLX.3607
      REAL     SICE (NSOIL)                                                      SFLX.3608
      REAL     SICEMAX                                                           SFLX.3609
      REAL     SLOPE                                                             SFLX.3610
      REAL     SLOPX                                                             SFLX.3611
      REAL     SMCAV                                                             SFLX.3612
      REAL     SMCMAX                                                            SFLX.3613
      REAL     SMCWLT                                                            SFLX.3614
      REAL     SSTT                                                              SFLX.3615
      REAL     SUM                                                               SFLX.3616
      REAL     VAL                                                               SFLX.3617
      REAL     WCND                                                              SFLX.3618
      REAL     WCND2                                                             SFLX.3619
      REAL     WDF                                                               SFLX.3620
      REAL     WDF2                                                              SFLX.3621
                                                                                 SFLX.3622
! ----------------------------------------------------------------------         SFLX.3623
! FROZEN GROUND VERSION:                                                         SFLX.3624
! REFERENCE FROZEN GROUND PARAMETER, CVFRZ, IS A SHAPE PARAMETER OF              SFLX.3625
! AREAL DISTRIBUTION FUNCTION OF SOIL ICE CONTENT WHICH EQUALS 1/CV.             SFLX.3626
! CV IS A COEFFICIENT OF SPATIAL VARIATION OF SOIL ICE CONTENT.  BASED           SFLX.3627
! ON FIELD DATA CV DEPENDS ON AREAL MEAN OF FROZEN DEPTH, AND IT CLOSE           SFLX.3628
! TO CONSTANT = 0.6 IF AREAL MEAN FROZEN DEPTH IS ABOVE 20 CM.  THAT IS          SFLX.3629
! WHY PARAMETER CVFRZ = 3 (INT{1/0.6*0.6}).                                      SFLX.3630
! CURRENT LOGIC DOESN'T ALLOW CVFRZ BE BIGGER THAN 3                             SFLX.3631
! ----------------------------------------------------------------------         SFLX.3632
      REAL     ZSOIL (NSOIL)                                                     SFLX.3633
                                                                                 SFLX.3634
! ----------------------------------------------------------------------         SFLX.3635
! DETERMINE RAINFALL INFILTRATION RATE AND RUNOFF.  INCLUDE THE                  SFLX.3636
! INFILTRATION FORMULE FROM SCHAAKE AND KOREN MODEL.                             SFLX.3637
! MODIFIED BY Q DUAN                                                             SFLX.3638
! ----------------------------------------------------------------------         SFLX.3639
      PARAMETER (CVFRZ = 3)                                                      SFLX.3640
! ----------------------------------------------------------------------         SFLX.3641
! LET SICEMAX BE THE GREATEST, IF ANY, FROZEN WATER CONTENT WITHIN SOIL          SFLX.3642
! LAYERS.                                                                        SFLX.3643
! ----------------------------------------------------------------------         SFLX.3644
      IOHINF = 1                                                                 SFLX.3645
      SICEMAX = 0.0                                                              SFLX.3646
      DO KS = 1,NSOIL                                                            SFLX.3647
         IF (SICE (KS) .gt. SICEMAX) SICEMAX = SICE (KS)                         SFLX.3648
! ----------------------------------------------------------------------         SFLX.3649
! DETERMINE RAINFALL INFILTRATION RATE AND RUNOFF                                SFLX.3650
! ----------------------------------------------------------------------         SFLX.3651
      END DO                                                                     SFLX.3652
      PDDUM = PCPDRP                                                             SFLX.3653
      RUNOFF1 = 0.0                                                              SFLX.3654
                                                                                 SFLX.3655
! ----------------------------------------------------------------------         SFLX.3656
! MODIFIED BY Q. DUAN, 5/16/94                                                   SFLX.3657
! ----------------------------------------------------------------------         SFLX.3658
!        IF (IOHINF .EQ. 1) THEN                                                 SFLX.3659
                                                                                 SFLX.3660
      IF (PCPDRP .ne. 0.0) THEN                                                  SFLX.3661
         DT1 = DT /86400.                                                        SFLX.3662
         SMCAV = SMCMAX - SMCWLT                                                 SFLX.3663
                                                                                 SFLX.3664
! ----------------------------------------------------------------------         SFLX.3665
! FROZEN GROUND VERSION:                                                         SFLX.3666
! ----------------------------------------------------------------------         SFLX.3667
         DMAX (1)= - ZSOIL (1)* SMCAV                                            SFLX.3668
                                                                                 SFLX.3669
         DICE = - ZSOIL (1) * SICE (1)                                           SFLX.3670
         DMAX (1)= DMAX (1)* (1.0- (SH2OA (1) + SICE (1) - SMCWLT)/      &       SFLX.3671
     &              SMCAV)                                                       SFLX.3672
                                                                                 SFLX.3673
         DD = DMAX (1)                                                           SFLX.3674
                                                                                 SFLX.3675
! ----------------------------------------------------------------------         SFLX.3676
! FROZEN GROUND VERSION:                                                         SFLX.3677
! ----------------------------------------------------------------------         SFLX.3678
         DO KS = 2,NSOIL                                                         SFLX.3679
                                                                                 SFLX.3680
            DICE = DICE+ ( ZSOIL (KS -1) - ZSOIL (KS) ) * SICE (KS)              SFLX.3681
            DMAX (KS) = (ZSOIL (KS -1) - ZSOIL (KS))* SMCAV                      SFLX.3682
            DMAX (KS) = DMAX (KS)* (1.0- (SH2OA (KS) + SICE (KS)        &        SFLX.3683
     &                  - SMCWLT)/ SMCAV)                                        SFLX.3684
            DD = DD+ DMAX (KS)                                                   SFLX.3685
! ----------------------------------------------------------------------         SFLX.3686
! VAL = (1.-EXP(-KDT*SQRT(DT1)))                                                 SFLX.3687
! IN BELOW, REMOVE THE SQRT IN ABOVE                                             SFLX.3688
! ----------------------------------------------------------------------         SFLX.3689
         END DO                                                                  SFLX.3690
         VAL = (1. - EXP ( - KDT * DT1))                                         SFLX.3691
         DDT = DD * VAL                                                          SFLX.3692
         PX = PCPDRP * DT                                                        SFLX.3693
         IF (PX .lt. 0.0) PX = 0.0                                               SFLX.3694
                                                                                 SFLX.3695
! ----------------------------------------------------------------------         SFLX.3696
! FROZEN GROUND VERSION:                                                         SFLX.3697
! REDUCTION OF INFILTRATION BASED ON FROZEN GROUND PARAMETERS                    SFLX.3698
! ----------------------------------------------------------------------         SFLX.3699
         INFMAX = (PX * (DDT / (PX + DDT)))/ DT                                  SFLX.3700
         FCR = 1.                                                                SFLX.3701
         IF (DICE .gt. 1.E-2) THEN                                               SFLX.3702
            ACRT = CVFRZ * FRZX / DICE                                           SFLX.3703
            SUM = 1.                                                             SFLX.3704
            IALP1 = CVFRZ - 1                                                    SFLX.3705
            DO J = 1,IALP1                                                       SFLX.3706
               K = 1                                                             SFLX.3707
               DO JJ = J +1,IALP1                                                SFLX.3708
                  K = K * JJ                                                     SFLX.3709
               END DO                                                            SFLX.3710
               SUM = SUM + (ACRT ** ( CVFRZ - J)) / FLOAT (K)                    SFLX.3711
            END DO                                                               SFLX.3712
            FCR = 1. - EXP ( - ACRT) * SUM                                       SFLX.3713
         END IF                                                                  SFLX.3714
                                                                                 SFLX.3715
! ----------------------------------------------------------------------         SFLX.3716
! CORRECTION OF INFILTRATION LIMITATION:                                         SFLX.3717
! IF INFMAX .LE. HYDROLIC CONDUCTIVITY ASSIGN INFMAX THE VALUE OF                SFLX.3718
! HYDROLIC CONDUCTIVITY                                                          SFLX.3719
! ----------------------------------------------------------------------         SFLX.3720
!         MXSMC = MAX ( SH2OA(1), SH2OA(2) )                                     SFLX.3721
         INFMAX = INFMAX * FCR                                                   SFLX.3722
                                                                                 SFLX.3723
         MXSMC = SH2OA (1)                                                       SFLX.3724
         CALL WDFCND (WDF,WCND,MXSMC,SMCMAX,BEXP,DKSAT,DWSAT,           &        SFLX.3725
     &                   SICEMAX)                                                SFLX.3726
         INFMAX = MAX (INFMAX,WCND)                                              SFLX.3727
                                                                                 SFLX.3728
         INFMAX = MIN (INFMAX,PX)                                                SFLX.3729
         IF (PCPDRP .gt. INFMAX) THEN                                            SFLX.3730
            RUNOFF1 = PCPDRP - INFMAX                                            SFLX.3731
            PDDUM = INFMAX                                                       SFLX.3732
         END IF                                                                  SFLX.3733
! ----------------------------------------------------------------------         SFLX.3734
! TO AVOID SPURIOUS DRAINAGE BEHAVIOR, 'UPSTREAM DIFFERENCING' IN LINE           SFLX.3735
! BELOW REPLACED WITH NEW APPROACH IN 2ND LINE:                                  SFLX.3736
! 'MXSMC = MAX(SH2OA(1), SH2OA(2))'                                              SFLX.3737
! ----------------------------------------------------------------------         SFLX.3738
      END IF                                                                     SFLX.3739
                                                                                 SFLX.3740
      MXSMC = SH2OA (1)                                                          SFLX.3741
      CALL WDFCND (WDF,WCND,MXSMC,SMCMAX,BEXP,DKSAT,DWSAT,              &        SFLX.3742
     &              SICEMAX)                                                     SFLX.3743
! ----------------------------------------------------------------------         SFLX.3744
! CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER                  SFLX.3745
! ----------------------------------------------------------------------         SFLX.3746
      DDZ = 1. / ( - .5 * ZSOIL (2) )                                            SFLX.3747
      AI (1) = 0.0                                                               SFLX.3748
      BI (1) = WDF * DDZ / ( - ZSOIL (1) )                                       SFLX.3749
                                                                                 SFLX.3750
! ----------------------------------------------------------------------         SFLX.3751
! CALC RHSTT FOR THE TOP LAYER AFTER CALC'NG THE VERTICAL SOIL MOISTURE          SFLX.3752
! GRADIENT BTWN THE TOP AND NEXT TO TOP LAYERS.                                  SFLX.3753
! ----------------------------------------------------------------------         SFLX.3754
      CI (1) = - BI (1)                                                          SFLX.3755
      DSMDZ = ( SH2O (1) - SH2O (2) ) / ( - .5 * ZSOIL (2) )                     SFLX.3756
      RHSTT (1) = (WDF * DSMDZ + WCND- PDDUM + EDIR + ET (1))/ ZSOIL (1)         SFLX.3757
                                                                                 SFLX.3758
! ----------------------------------------------------------------------         SFLX.3759
! INITIALIZE DDZ2                                                                SFLX.3760
! ----------------------------------------------------------------------         SFLX.3761
      SSTT = WDF * DSMDZ + WCND+ EDIR + ET (1)                                   SFLX.3762
                                                                                 SFLX.3763
! ----------------------------------------------------------------------         SFLX.3764
! LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABV PROCESS                 SFLX.3765
! ----------------------------------------------------------------------         SFLX.3766
      DDZ2 = 0.0                                                                 SFLX.3767
      DO K = 2,NSOIL                                                             SFLX.3768
         DENOM2 = (ZSOIL (K -1) - ZSOIL (K))                                     SFLX.3769
         IF (K .ne. NSOIL) THEN                                                  SFLX.3770
                                                                                 SFLX.3771
! ----------------------------------------------------------------------         SFLX.3772
! AGAIN, TO AVOID SPURIOUS DRAINAGE BEHAVIOR, 'UPSTREAM DIFFERENCING' IN         SFLX.3773
! LINE BELOW REPLACED WITH NEW APPROACH IN 2ND LINE:                             SFLX.3774
! 'MXSMC2 = MAX (SH2OA(K), SH2OA(K+1))'                                          SFLX.3775
! ----------------------------------------------------------------------         SFLX.3776
            SLOPX = 1.                                                           SFLX.3777
                                                                                 SFLX.3778
            MXSMC2 = SH2OA (K)                                                   SFLX.3779
            CALL WDFCND (WDF2,WCND2,MXSMC2,SMCMAX,BEXP,DKSAT,DWSAT,     &        SFLX.3780
     &                    SICEMAX)                                               SFLX.3781
! -----------------------------------------------------------------------        SFLX.3782
! CALC SOME PARTIAL PRODUCTS FOR LATER USE IN CALC'NG RHSTT                      SFLX.3783
! ----------------------------------------------------------------------         SFLX.3784
            DENOM = (ZSOIL (K -1) - ZSOIL (K +1))                                SFLX.3785
                                                                                 SFLX.3786
! ----------------------------------------------------------------------         SFLX.3787
! CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT                    SFLX.3788
! ----------------------------------------------------------------------         SFLX.3789
            DSMDZ2 = (SH2O (K) - SH2O (K +1)) / (DENOM * 0.5)                    SFLX.3790
            DDZ2 = 2.0 / DENOM                                                   SFLX.3791
            CI (K) = - WDF2 * DDZ2 / DENOM2                                      SFLX.3792
                                                                                 SFLX.3793
         ELSE                                                                    SFLX.3794
! ----------------------------------------------------------------------         SFLX.3795
! SLOPE OF BOTTOM LAYER IS INTRODUCED                                            SFLX.3796
! ----------------------------------------------------------------------         SFLX.3797
                                                                                 SFLX.3798
! ----------------------------------------------------------------------         SFLX.3799
! RETRIEVE THE SOIL WATER DIFFUSIVITY AND HYDRAULIC CONDUCTIVITY FOR             SFLX.3800
! THIS LAYER                                                                     SFLX.3801
! ----------------------------------------------------------------------         SFLX.3802
            SLOPX = SLOPE                                                        SFLX.3803
          CALL WDFCND (WDF2,WCND2,SH2OA (NSOIL),SMCMAX,BEXP,DKSAT,DWSAT,     &   SFLX.3804
     &                      SICEMAX)                                             SFLX.3805
                                                                                 SFLX.3806
! ----------------------------------------------------------------------         SFLX.3807
! CALC A PARTIAL PRODUCT FOR LATER USE IN CALC'NG RHSTT                          SFLX.3808
! ----------------------------------------------------------------------         SFLX.3809
                                                                                 SFLX.3810
! ----------------------------------------------------------------------         SFLX.3811
! SET MATRIX COEF CI TO ZERO                                                     SFLX.3812
! ----------------------------------------------------------------------         SFLX.3813
            DSMDZ2 = 0.0                                                         SFLX.3814
            CI (K) = 0.0                                                         SFLX.3815
! ----------------------------------------------------------------------         SFLX.3816
! CALC RHSTT FOR THIS LAYER AFTER CALC'NG ITS NUMERATOR                          SFLX.3817
! ----------------------------------------------------------------------         SFLX.3818
         END IF                                                                  SFLX.3819
         NUMER = (WDF2 * DSMDZ2) + SLOPX * WCND2- (WDF * DSMDZ)         &        SFLX.3820
     &           - WCND+ ET (K)                                                  SFLX.3821
                                                                                 SFLX.3822
! ----------------------------------------------------------------------         SFLX.3823
! CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER                                   SFLX.3824
! ----------------------------------------------------------------------         SFLX.3825
         RHSTT (K) = NUMER / ( - DENOM2)                                         SFLX.3826
         AI (K) = - WDF * DDZ / DENOM2                                           SFLX.3827
                                                                                 SFLX.3828
! ----------------------------------------------------------------------         SFLX.3829
! RESET VALUES OF WDF, WCND, DSMDZ, AND DDZ FOR LOOP TO NEXT LYR                 SFLX.3830
! RUNOFF2:  SUB-SURFACE OR BASEFLOW RUNOFF                                       SFLX.3831
! ----------------------------------------------------------------------         SFLX.3832
         BI (K) = - ( AI (K) + CI (K) )                                          SFLX.3833
         IF (K .eq. NSOIL) THEN                                                  SFLX.3834
            RUNOFF2 = SLOPX * WCND2                                              SFLX.3835
         END IF                                                                  SFLX.3836
         IF (K .ne. NSOIL) THEN                                                  SFLX.3837
            WDF = WDF2                                                           SFLX.3838
            WCND = WCND2                                                         SFLX.3839
            DSMDZ = DSMDZ2                                                       SFLX.3840
            DDZ = DDZ2                                                           SFLX.3841
         END IF                                                                  SFLX.3842
! ----------------------------------------------------------------------         SFLX.3843
! END SUBROUTINE SRT                                                             SFLX.3844
! ----------------------------------------------------------------------         SFLX.3845
      END DO                                                                     SFLX.3846
      END                                                                        SFLX.3847
                                                                                 SFLX.3848
      SUBROUTINE SSTEP (SH2OOUT,SH2OIN,CMC,RHSTT,RHSCT,DT,              &        SFLX.3849
     &                   NSOIL,SMCMAX,CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,    &        SFLX.3850
     &                     AI,BI,CI)                                             SFLX.3851
                                                                                 SFLX.3852
! ----------------------------------------------------------------------         SFLX.3853
! SUBROUTINE SSTEP                                                               SFLX.3854
! ----------------------------------------------------------------------         SFLX.3855
! CALCULATE/UPDATE SOIL MOISTURE CONTENT VALUES AND CANOPY MOISTURE              SFLX.3856
! CONTENT VALUES.                                                                SFLX.3857
! ----------------------------------------------------------------------         SFLX.3858
      IMPLICIT NONE                                                              SFLX.3859
      INTEGER  NSOLD                                                             SFLX.3860
                                                                                 SFLX.3861
      PARAMETER (NSOLD = 20)                                                     SFLX.3862
      INTEGER  I                                                                 SFLX.3863
      INTEGER  K                                                                 SFLX.3864
      INTEGER  KK11                                                              SFLX.3865
                                                                                 SFLX.3866
      INTEGER  NSOIL                                                             SFLX.3867
      REAL     AI (NSOLD)                                                        SFLX.3868
      REAL     BI (NSOLD)                                                        SFLX.3869
      REAL     CI (NSOLD)                                                        SFLX.3870
      REAL     CIin (NSOLD)                                                      SFLX.3871
      REAL     CMC                                                               SFLX.3872
      REAL     CMCMAX                                                            SFLX.3873
      REAL     DDZ                                                               SFLX.3874
      REAL     DT                                                                SFLX.3875
      REAL     RHSCT                                                             SFLX.3876
      REAL     RHSTT (NSOIL)                                                     SFLX.3877
      REAL     RHSTTin (NSOIL)                                                   SFLX.3878
      REAL     RUNOFF3                                                           SFLX.3879
      REAL     SH2OIN (NSOIL)                                                    SFLX.3880
      REAL     SH2OOUT (NSOIL)                                                   SFLX.3881
      REAL     SICE (NSOIL)                                                      SFLX.3882
      REAL     SMC (NSOIL)                                                       SFLX.3883
      REAL     SMCMAX                                                            SFLX.3884
      REAL     STOT                                                              SFLX.3885
      REAL     WPLUS                                                             SFLX.3886
                                                                                 SFLX.3887
! ----------------------------------------------------------------------         SFLX.3888
! CREATE 'AMOUNT' VALUES OF VARIABLES TO BE INPUT TO THE                         SFLX.3889
! TRI-DIAGONAL MATRIX ROUTINE.                                                   SFLX.3890
! ----------------------------------------------------------------------         SFLX.3891
      REAL     ZSOIL (NSOIL)                                                     SFLX.3892
      DO K = 1,NSOIL                                                             SFLX.3893
         RHSTT (K) = RHSTT (K) * DT                                              SFLX.3894
         AI (K) = AI (K) * DT                                                    SFLX.3895
         BI (K) = 1. + BI (K) * DT                                               SFLX.3896
         CI (K) = CI (K) * DT                                                    SFLX.3897
! ----------------------------------------------------------------------         SFLX.3898
! COPY VALUES FOR INPUT VARIABLES BEFORE CALL TO ROSR12                          SFLX.3899
! ----------------------------------------------------------------------         SFLX.3900
      END DO                                                                     SFLX.3901
      DO K = 1,NSOIL                                                             SFLX.3902
         RHSTTin (K) = RHSTT (K)                                                 SFLX.3903
      END DO                                                                     SFLX.3904
      DO K = 1,NSOLD                                                             SFLX.3905
         CIin (K) = CI (K)                                                       SFLX.3906
      END DO                                                                     SFLX.3907
! ----------------------------------------------------------------------         SFLX.3908
! CALL ROSR12 TO SOLVE THE TRI-DIAGONAL MATRIX                                   SFLX.3909
! ----------------------------------------------------------------------         SFLX.3910
      CALL ROSR12 (CI,AI,BI,CIin,RHSTTin,RHSTT,NSOIL)                            SFLX.3911
! ----------------------------------------------------------------------         SFLX.3912
! SUM THE PREVIOUS SMC VALUE AND THE MATRIX SOLUTION TO GET A                    SFLX.3913
! NEW VALUE.  MIN ALLOWABLE VALUE OF SMC WILL BE 0.02.                           SFLX.3914
! RUNOFF3: RUNOFF WITHIN SOIL LAYERS                                             SFLX.3915
! ----------------------------------------------------------------------         SFLX.3916
      WPLUS = 0.0                                                                SFLX.3917
      RUNOFF3 = 0.                                                               SFLX.3918
                                                                                 SFLX.3919
      DDZ = - ZSOIL (1)                                                          SFLX.3920
      DO K = 1,NSOIL                                                             SFLX.3921
         IF (K .ne. 1) DDZ = ZSOIL (K - 1) - ZSOIL (K)                           SFLX.3922
                                                                                 SFLX.3923
         SH2OOUT (K) = SH2OIN (K) + CI (K) + WPLUS / DDZ                         SFLX.3924
         STOT = SH2OOUT (K) + SICE (K)                                           SFLX.3925
         IF (STOT .gt. SMCMAX) THEN                                              SFLX.3926
            IF (K .eq. 1) THEN                                                   SFLX.3927
               DDZ = - ZSOIL (1)                                                 SFLX.3928
            ELSE                                                                 SFLX.3929
               KK11 = K - 1                                                      SFLX.3930
               DDZ = - ZSOIL (K) + ZSOIL (KK11)                                  SFLX.3931
            END IF                                                               SFLX.3932
            WPLUS = (STOT - SMCMAX) * DDZ                                        SFLX.3933
         ELSE                                                                    SFLX.3934
            WPLUS = 0.                                                           SFLX.3935
         END IF                                                                  SFLX.3936
         SMC (K) = MAX ( MIN (STOT,SMCMAX),0.02 )                                SFLX.3937
         SH2OOUT (K) = MAX ( (SMC (K) - SICE (K)),0.0)                           SFLX.3938
      END DO                                                                     SFLX.3939
                                                                                 SFLX.3940
! ----------------------------------------------------------------------         SFLX.3941
! UPDATE CANOPY WATER CONTENT/INTERCEPTION (CMC).  CONVERT RHSCT TO              SFLX.3942
! AN 'AMOUNT' VALUE AND ADD TO PREVIOUS CMC VALUE TO GET NEW CMC.                SFLX.3943
! ----------------------------------------------------------------------         SFLX.3944
      RUNOFF3 = WPLUS                                                            SFLX.3945
      CMC = CMC + DT * RHSCT                                                     SFLX.3946
      IF (CMC .lt. 1.E-20) CMC = 0.0                                             SFLX.3947
                                                                                 SFLX.3948
! ----------------------------------------------------------------------         SFLX.3949
! END SUBROUTINE SSTEP                                                           SFLX.3950
! ----------------------------------------------------------------------         SFLX.3951
      CMC = MIN (CMC,CMCMAX)                                                     SFLX.3952
      END                                                                        SFLX.3953
                                                                                 SFLX.3954
                                                                                 SFLX.3955
      SUBROUTINE TBND (TU,TB,ZSOIL,ZBOT,K,NSOIL,TBND1)                           SFLX.3956
                                                                                 SFLX.3957
! ----------------------------------------------------------------------         SFLX.3958
! SUBROUTINE TBND                                                                SFLX.3959
! ----------------------------------------------------------------------         SFLX.3960
! CALCULATE TEMPERATURE ON THE BOUNDARY OF THE LAYER BY INTERPOLATION OF         SFLX.3961
! THE MIDDLE LAYER TEMPERATURES                                                  SFLX.3962
! ----------------------------------------------------------------------         SFLX.3963
      IMPLICIT NONE                                                              SFLX.3964
      INTEGER  NSOIL                                                             SFLX.3965
                                                                                 SFLX.3966
      INTEGER  K                                                                 SFLX.3967
      REAL     TBND1                                                             SFLX.3968
      REAL     T0                                                                SFLX.3969
      REAL     TU                                                                SFLX.3970
      REAL     TB                                                                SFLX.3971
      REAL     ZB                                                                SFLX.3972
      REAL     ZBOT                                                              SFLX.3973
      REAL     ZUP                                                               SFLX.3974
                                                                                 SFLX.3975
      REAL     ZSOIL (NSOIL)                                                     SFLX.3976
                                                                                 SFLX.3977
! ----------------------------------------------------------------------         SFLX.3978
! USE SURFACE TEMPERATURE ON THE TOP OF THE FIRST LAYER                          SFLX.3979
! ----------------------------------------------------------------------         SFLX.3980
      PARAMETER (T0 = 273.15)                                                    SFLX.3981
      IF (K .eq. 1) THEN                                                         SFLX.3982
         ZUP = 0.                                                                SFLX.3983
      ELSE                                                                       SFLX.3984
         ZUP = ZSOIL (K -1)                                                      SFLX.3985
! ----------------------------------------------------------------------         SFLX.3986
! USE DEPTH OF THE CONSTANT BOTTOM TEMPERATURE WHEN INTERPOLATE                  SFLX.3987
! TEMPERATURE INTO THE LAST LAYER BOUNDARY                                       SFLX.3988
! ----------------------------------------------------------------------         SFLX.3989
      END IF                                                                     SFLX.3990
      IF (K .eq. NSOIL) THEN                                                     SFLX.3991
         ZB = 2.* ZBOT - ZSOIL (K)                                               SFLX.3992
      ELSE                                                                       SFLX.3993
         ZB = ZSOIL (K +1)                                                       SFLX.3994
! ----------------------------------------------------------------------         SFLX.3995
! LINEAR INTERPOLATION BETWEEN THE AVERAGE LAYER TEMPERATURES                    SFLX.3996
! ----------------------------------------------------------------------         SFLX.3997
      END IF                                                                     SFLX.3998
                                                                                 SFLX.3999
! ----------------------------------------------------------------------         SFLX.4000
! END SUBROUTINE TBND                                                            SFLX.4001
! ----------------------------------------------------------------------         SFLX.4002
      TBND1 = TU + (TB - TU)* (ZUP - ZSOIL (K))/ (ZUP - ZB)                      SFLX.4003
      END                                                                        SFLX.4004
                                                                                 SFLX.4005
                                                                                 SFLX.4006
      SUBROUTINE TDFCND ( DF, SMC, QZ, SMCMAX, SH2O)                             SFLX.4007
                                                                                 SFLX.4008
! ----------------------------------------------------------------------         SFLX.4009
! SUBROUTINE TDFCND                                                              SFLX.4010
! ----------------------------------------------------------------------         SFLX.4011
! CALCULATE THERMAL DIFFUSIVITY AND CONDUCTIVITY OF THE SOIL FOR A GIVEN         SFLX.4012
! POINT AND TIME.                                                                SFLX.4013
! ----------------------------------------------------------------------         SFLX.4014
! PETERS-LIDARD APPROACH (PETERS-LIDARD et al., 1998)                            SFLX.4015
! June 2001 CHANGES: FROZEN SOIL CONDITION.                                      SFLX.4016
! ----------------------------------------------------------------------         SFLX.4017
      IMPLICIT NONE                                                              SFLX.4018
      REAL     DF                                                                SFLX.4019
      REAL     GAMMD                                                             SFLX.4020
      REAL     THKDRY                                                            SFLX.4021
      REAL     AKE                                                               SFLX.4022
      REAL     THKICE                                                            SFLX.4023
      REAL     THKO                                                              SFLX.4024
      REAL     THKQTZ                                                            SFLX.4025
      REAL     THKSAT                                                            SFLX.4026
      REAL     THKS                                                              SFLX.4027
      REAL     THKW                                                              SFLX.4028
      REAL     QZ                                                                SFLX.4029
      REAL     SATRATIO                                                          SFLX.4030
      REAL     SH2O                                                              SFLX.4031
      REAL     SMC                                                               SFLX.4032
      REAL     SMCMAX                                                            SFLX.4033
      REAL     XU                                                                SFLX.4034
                                                                                 SFLX.4035
! ----------------------------------------------------------------------         SFLX.4036
! WE NOW GET QUARTZ AS AN INPUT ARGUMENT (SET IN ROUTINE REDPRM):                SFLX.4037
!      DATA QUARTZ /0.82, 0.10, 0.25, 0.60, 0.52,                                SFLX.4038
!     &             0.35, 0.60, 0.40, 0.82/                                      SFLX.4039
! ----------------------------------------------------------------------         SFLX.4040
! IF THE SOIL HAS ANY MOISTURE CONTENT COMPUTE A PARTIAL SUM/PRODUCT             SFLX.4041
! OTHERWISE USE A CONSTANT VALUE WHICH WORKS WELL WITH MOST SOILS                SFLX.4042
! ----------------------------------------------------------------------         SFLX.4043
!  THKW ......WATER THERMAL CONDUCTIVITY                                         SFLX.4044
!  THKQTZ ....THERMAL CONDUCTIVITY FOR QUARTZ                                    SFLX.4045
!  THKO ......THERMAL CONDUCTIVITY FOR OTHER SOIL COMPONENTS                     SFLX.4046
!  THKS ......THERMAL CONDUCTIVITY FOR THE SOLIDS COMBINED(QUARTZ+OTHER)         SFLX.4047
!  THKICE ....ICE THERMAL CONDUCTIVITY                                           SFLX.4048
!  SMCMAX ....POROSITY (= SMCMAX)                                                SFLX.4049
!  QZ .........QUARTZ CONTENT (SOIL TYPE DEPENDENT)                              SFLX.4050
! ----------------------------------------------------------------------         SFLX.4051
! USE AS IN PETERS-LIDARD, 1998 (MODIF. FROM JOHANSEN, 1975).                    SFLX.4052
                                                                                 SFLX.4053
!                                  PABLO GRUNMANN, 08/17/98                      SFLX.4054
! REFS.:                                                                         SFLX.4055
!      FAROUKI, O.T.,1986: THERMAL PROPERTIES OF SOILS. SERIES ON ROCK           SFLX.4056
!              AND SOIL MECHANICS, VOL. 11, TRANS TECH, 136 PP.                  SFLX.4057
!      JOHANSEN, O., 1975: THERMAL CONDUCTIVITY OF SOILS. PH.D. THESIS,          SFLX.4058
!              UNIVERSITY OF TRONDHEIM,                                          SFLX.4059
!      PETERS-LIDARD, C. D., ET AL., 1998: THE EFFECT OF SOIL THERMAL            SFLX.4060
!              CONDUCTIVITY PARAMETERIZATION ON SURFACE ENERGY FLUXES            SFLX.4061
!              AND TEMPERATURES. JOURNAL OF THE ATMOSPHERIC SCIENCES,            SFLX.4062
!              VOL. 55, PP. 1209-1224.                                           SFLX.4063
! ----------------------------------------------------------------------         SFLX.4064
! NEEDS PARAMETERS                                                               SFLX.4065
! POROSITY(SOIL TYPE):                                                           SFLX.4066
!      POROS = SMCMAX                                                            SFLX.4067
! SATURATION RATIO:                                                              SFLX.4068
      REAL     XUNFROZ                                                           SFLX.4069
! PARAMETERS  W/(M.K)                                                            SFLX.4070
      SATRATIO = SMC / SMCMAX                                                    SFLX.4071
      THKICE = 2.2                                                               SFLX.4072
      THKW = 0.57                                                                SFLX.4073
!      IF (QZ .LE. 0.2) THKO = 3.0                                               SFLX.4074
      THKO = 2.0                                                                 SFLX.4075
! SOLIDS' CONDUCTIVITY                                                           SFLX.4076
! QUARTZ' CONDUCTIVITY                                                           SFLX.4077
      THKQTZ = 7.7                                                               SFLX.4078
                                                                                 SFLX.4079
! UNFROZEN FRACTION (FROM 1., i.e., 100%LIQUID, TO 0. (100% FROZEN))             SFLX.4080
      THKS = (THKQTZ ** QZ)* (THKO ** (1. - QZ))                                 SFLX.4081
                                                                                 SFLX.4082
! UNFROZEN VOLUME FOR SATURATION (POROSITY*XUNFROZ)                              SFLX.4083
      XUNFROZ = SH2O / SMC                                                       SFLX.4084
! SATURATED THERMAL CONDUCTIVITY                                                 SFLX.4085
      XU = XUNFROZ * SMCMAX                                                      SFLX.4086
                                                                                 SFLX.4087
! DRY DENSITY IN KG/M3                                                           SFLX.4088
      THKSAT = THKS ** (1. - SMCMAX)* THKICE ** (SMCMAX - XU)* THKW **   &       SFLX.4089
     &   (XU)                                                                    SFLX.4090
                                                                                 SFLX.4091
! DRY THERMAL CONDUCTIVITY IN W.M-1.K-1                                          SFLX.4092
      GAMMD = (1. - SMCMAX)*2700.                                                SFLX.4093
                                                                                 SFLX.4094
      THKDRY = (0.135* GAMMD+ 64.7)/ (2700. - 0.947* GAMMD)                      SFLX.4095
! FROZEN                                                                         SFLX.4096
      IF ( (SH2O + 0.0005) .lt. SMC ) THEN                                       SFLX.4097
         AKE = SATRATIO                                                          SFLX.4098
! UNFROZEN                                                                       SFLX.4099
! RANGE OF VALIDITY FOR THE KERSTEN NUMBER (AKE)                                 SFLX.4100
      ELSE                                                                       SFLX.4101
                                                                                 SFLX.4102
! KERSTEN NUMBER (USING "FINE" FORMULA, VALID FOR SOILS CONTAINING AT            SFLX.4103
! LEAST 5% OF PARTICLES WITH DIAMETER LESS THAN 2.E-6 METERS.)                   SFLX.4104
! (FOR "COARSE" FORMULA, SEE PETERS-LIDARD ET AL., 1998).                        SFLX.4105
                                                                                 SFLX.4106
         IF ( SATRATIO .gt. 0.1 ) THEN                                           SFLX.4107
                                                                                 SFLX.4108
            AKE = LOG10 (SATRATIO) + 1.0                                         SFLX.4109
                                                                                 SFLX.4110
! USE K = KDRY                                                                   SFLX.4111
         ELSE                                                                    SFLX.4112
                                                                                 SFLX.4113
            AKE = 0.0                                                            SFLX.4114
         END IF                                                                  SFLX.4115
!  THERMAL CONDUCTIVITY                                                          SFLX.4116
                                                                                 SFLX.4117
      END IF                                                                     SFLX.4118
                                                                                 SFLX.4119
! ----------------------------------------------------------------------         SFLX.4120
! END SUBROUTINE TDFCND                                                          SFLX.4121
! ----------------------------------------------------------------------         SFLX.4122
      DF = AKE * (THKSAT - THKDRY) + THKDRY                                      SFLX.4123
      END                                                                        SFLX.4124
                                                                                 SFLX.4125
                                                                                 SFLX.4126
      SUBROUTINE TMPAVG (TAVG,TUP,TM,TDN,ZSOIL,NSOIL,K)                          SFLX.4127
                                                                                 SFLX.4128
! ----------------------------------------------------------------------         SFLX.4129
! SUBROUTINE TMPAVG                                                              SFLX.4130
! ----------------------------------------------------------------------         SFLX.4131
! CALCULATE SOIL LAYER AVERAGE TEMPERATURE (TAVG) IN FREEZING/THAWING            SFLX.4132
! LAYER USING UP, DOWN, AND MIDDLE LAYER TEMPERATURES (TUP, TDN, TM),            SFLX.4133
! WHERE TUP IS AT TOP BOUNDARY OF LAYER, TDN IS AT BOTTOM BOUNDARY OF            SFLX.4134
! LAYER.  TM IS LAYER PROGNOSTIC STATE TEMPERATURE.                              SFLX.4135
! ----------------------------------------------------------------------         SFLX.4136
      IMPLICIT NONE                                                              SFLX.4137
      INTEGER  K                                                                 SFLX.4138
                                                                                 SFLX.4139
      INTEGER  NSOIL                                                             SFLX.4140
      REAL     DZ                                                                SFLX.4141
      REAL     DZH                                                               SFLX.4142
      REAL     T0                                                                SFLX.4143
      REAL     TAVG                                                              SFLX.4144
      REAL     TDN                                                               SFLX.4145
      REAL     TM                                                                SFLX.4146
      REAL     TUP                                                               SFLX.4147
      REAL     X0                                                                SFLX.4148
      REAL     XDN                                                               SFLX.4149
      REAL     XUP                                                               SFLX.4150
                                                                                 SFLX.4151
      REAL     ZSOIL (NSOIL)                                                     SFLX.4152
                                                                                 SFLX.4153
! ----------------------------------------------------------------------         SFLX.4154
      PARAMETER (T0 = 2.7315E2)                                                  SFLX.4155
      IF (K .eq. 1) THEN                                                         SFLX.4156
         DZ = - ZSOIL (1)                                                        SFLX.4157
      ELSE                                                                       SFLX.4158
         DZ = ZSOIL (K -1) - ZSOIL (K)                                           SFLX.4159
      END IF                                                                     SFLX.4160
                                                                                 SFLX.4161
      DZH = DZ *0.5                                                              SFLX.4162
      IF (TUP .lt. T0) THEN                                                      SFLX.4163
         IF (TM .lt. T0) THEN                                                    SFLX.4164
! ----------------------------------------------------------------------         SFLX.4165
! TUP, TM, TDN < T0                                                              SFLX.4166
! ----------------------------------------------------------------------         SFLX.4167
            IF (TDN .lt. T0) THEN                                                SFLX.4168
               TAVG = (TUP + 2.0* TM + TDN)/ 4.0                                 SFLX.4169
! ----------------------------------------------------------------------         SFLX.4170
! TUP & TM < T0,  TDN .ge. T0                                                    SFLX.4171
! ----------------------------------------------------------------------         SFLX.4172
            ELSE                                                                 SFLX.4173
               X0 = (T0- TM) * DZH / (TDN - TM)                                  SFLX.4174
               TAVG = 0.5 * (TUP * DZH + TM * (DZH + X0) + T0* (        &        SFLX.4175
     &               2.* DZH - X0)) / DZ                                         SFLX.4176
            END IF                                                               SFLX.4177
         ELSE                                                                    SFLX.4178
! ----------------------------------------------------------------------         SFLX.4179
! TUP < T0, TM .ge. T0, TDN < T0                                                 SFLX.4180
! ----------------------------------------------------------------------         SFLX.4181
            IF (TDN .lt. T0) THEN                                                SFLX.4182
               XUP = (T0- TUP) * DZH / (TM - TUP)                                SFLX.4183
               XDN = DZH - (T0- TM) * DZH / (TDN - TM)                           SFLX.4184
               TAVG = 0.5 * (TUP * XUP + T0* (2.* DZ - XUP - XDN)       &        SFLX.4185
     &                + TDN * XDN) / DZ                                          SFLX.4186
! ----------------------------------------------------------------------         SFLX.4187
! TUP < T0, TM .ge. T0, TDN .ge. T0                                              SFLX.4188
! ----------------------------------------------------------------------         SFLX.4189
            ELSE                                                                 SFLX.4190
               XUP = (T0- TUP) * DZH / (TM - TUP)                                SFLX.4191
               TAVG = 0.5 * (TUP * XUP + T0* (2.* DZ - XUP)) / DZ                SFLX.4192
            END IF                                                               SFLX.4193
         END IF                                                                  SFLX.4194
      ELSE                                                                       SFLX.4195
         IF (TM .lt. T0) THEN                                                    SFLX.4196
! ----------------------------------------------------------------------         SFLX.4197
! TUP .ge. T0, TM < T0, TDN < T0                                                 SFLX.4198
! ----------------------------------------------------------------------         SFLX.4199
            IF (TDN .lt. T0) THEN                                                SFLX.4200
               XUP = DZH - (T0- TUP) * DZH / (TM - TUP)                          SFLX.4201
               TAVG = 0.5 * (T0* (DZ - XUP) + TM * (DZH + XUP)          &        SFLX.4202
     &                + TDN * DZH) / DZ                                          SFLX.4203
! ----------------------------------------------------------------------         SFLX.4204
! TUP .ge. T0, TM < T0, TDN .ge. T0                                              SFLX.4205
! ----------------------------------------------------------------------         SFLX.4206
            ELSE                                                                 SFLX.4207
               XUP = DZH - (T0- TUP) * DZH / (TM - TUP)                          SFLX.4208
               XDN = (T0- TM) * DZH / (TDN - TM)                                 SFLX.4209
               TAVG = 0.5 * (T0* (2.* DZ - XUP - XDN) + TM *            &        SFLX.4210
     & (XUP + XDN)) / DZ                                                         SFLX.4211
            END IF                                                               SFLX.4212
         ELSE                                                                    SFLX.4213
! ----------------------------------------------------------------------         SFLX.4214
! TUP .ge. T0, TM .ge. T0, TDN < T0                                              SFLX.4215
! ----------------------------------------------------------------------         SFLX.4216
            IF (TDN .lt. T0) THEN                                                SFLX.4217
               XDN = DZH - (T0- TM) * DZH / (TDN - TM)                           SFLX.4218
               TAVG = (T0* (DZ - XDN) +0.5* (T0+ TDN)* XDN) / DZ                 SFLX.4219
! ----------------------------------------------------------------------         SFLX.4220
! TUP .ge. T0, TM .ge. T0, TDN .ge. T0                                           SFLX.4221
! ----------------------------------------------------------------------         SFLX.4222
            ELSE                                                                 SFLX.4223
               TAVG = (TUP + 2.0* TM + TDN) / 4.0                                SFLX.4224
            END IF                                                               SFLX.4225
         END IF                                                                  SFLX.4226
! ----------------------------------------------------------------------         SFLX.4227
! END SUBROUTINE TMPAVG                                                          SFLX.4228
! ----------------------------------------------------------------------         SFLX.4229
      END IF                                                                     SFLX.4230
      END                                                                        SFLX.4231
                                                                                 SFLX.4232
      SUBROUTINE TRANSP (ET,NSOIL,ETP1,SMC,CMC,ZSOIL,SHDFAC,SMCWLT,     &        SFLX.4233
     &                      CMCMAX,PC,CFACTR,SMCREF,SFCTMP,Q2,NROOT,    &        SFLX.4234
     &                      RTDIS)                                               SFLX.4235
                                                                                 SFLX.4236
! ----------------------------------------------------------------------         SFLX.4237
! SUBROUTINE TRANSP                                                              SFLX.4238
! ----------------------------------------------------------------------         SFLX.4239
! CALCULATE TRANSPIRATION FOR THE VEG CLASS.                                     SFLX.4240
! ----------------------------------------------------------------------         SFLX.4241
      IMPLICIT NONE                                                              SFLX.4242
      INTEGER  I                                                                 SFLX.4243
      INTEGER  K                                                                 SFLX.4244
      INTEGER  NSOIL                                                             SFLX.4245
                                                                                 SFLX.4246
      INTEGER  NROOT                                                             SFLX.4247
      REAL     CFACTR                                                            SFLX.4248
      REAL     CMC                                                               SFLX.4249
      REAL     CMCMAX                                                            SFLX.4250
      REAL     DENOM                                                             SFLX.4251
      REAL     ET (NSOIL)                                                        SFLX.4252
      REAL     ETP1                                                              SFLX.4253
      REAL     ETP1A                                                             SFLX.4254
!.....REAL PART(NSOIL)                                                           SFLX.4255
      REAL     GX (7)                                                            SFLX.4256
      REAL     PC                                                                SFLX.4257
      REAL     Q2                                                                SFLX.4258
      REAL     RTDIS (NSOIL)                                                     SFLX.4259
      REAL     RTX                                                               SFLX.4260
      REAL     SFCTMP                                                            SFLX.4261
      REAL     SGX                                                               SFLX.4262
      REAL     SHDFAC                                                            SFLX.4263
      REAL     SMC (NSOIL)                                                       SFLX.4264
      REAL     SMCREF                                                            SFLX.4265
      REAL     SMCWLT                                                            SFLX.4266
                                                                                 SFLX.4267
! ----------------------------------------------------------------------         SFLX.4268
! INITIALIZE PLANT TRANSP TO ZERO FOR ALL SOIL LAYERS.                           SFLX.4269
! ----------------------------------------------------------------------         SFLX.4270
      REAL     ZSOIL (NSOIL)                                                     SFLX.4271
      DO K = 1,NSOIL                                                             SFLX.4272
         ET (K) = 0.                                                             SFLX.4273
! ----------------------------------------------------------------------         SFLX.4274
! CALCULATE AN 'ADJUSTED' POTENTIAL TRANSPIRATION                                SFLX.4275
! IF STATEMENT BELOW TO AVOID TANGENT LINEAR PROBLEMS NEAR ZERO                  SFLX.4276
! NOTE: GX AND OTHER TERMS BELOW REDISTRIBUTE TRANSPIRATION BY LAYER,            SFLX.4277
! ET(K), AS A FUNCTION OF SOIL MOISTURE AVAILABILITY, WHILE PRESERVING           SFLX.4278
! TOTAL ETP1A.                                                                   SFLX.4279
! ----------------------------------------------------------------------         SFLX.4280
      END DO                                                                     SFLX.4281
      IF (CMC .ne. 0.0) THEN                                                     SFLX.4282
         ETP1A = SHDFAC * PC * ETP1 * (1.0- (CMC / CMCMAX) ** CFACTR)            SFLX.4283
      ELSE                                                                       SFLX.4284
         ETP1A = SHDFAC * PC * ETP1                                              SFLX.4285
      END IF                                                                     SFLX.4286
      SGX = 0.0                                                                  SFLX.4287
      DO I = 1,NROOT                                                             SFLX.4288
         GX (I) = ( SMC (I) - SMCWLT ) / ( SMCREF - SMCWLT )                     SFLX.4289
         GX (I) = MAX ( MIN ( GX (I), 1. ), 0. )                                 SFLX.4290
         SGX = SGX + GX (I)                                                      SFLX.4291
      END DO                                                                     SFLX.4292
                                                                                 SFLX.4293
      SGX = SGX / NROOT                                                          SFLX.4294
      DENOM = 0.                                                                 SFLX.4295
      DO I = 1,NROOT                                                             SFLX.4296
         RTX = RTDIS (I) + GX (I) - SGX                                          SFLX.4297
         GX (I) = GX (I) * MAX ( RTX, 0. )                                       SFLX.4298
         DENOM = DENOM + GX (I)                                                  SFLX.4299
      END DO                                                                     SFLX.4300
                                                                                 SFLX.4301
      IF (DENOM .le. 0.0) DENOM = 1.                                             SFLX.4302
      DO I = 1,NROOT                                                             SFLX.4303
         ET (I) = ETP1A * GX (I) / DENOM                                         SFLX.4304
! ----------------------------------------------------------------------         SFLX.4305
! ABOVE CODE ASSUMES A VERTICALLY UNIFORM ROOT DISTRIBUTION                      SFLX.4306
! CODE BELOW TESTS A VARIABLE ROOT DISTRIBUTION                                  SFLX.4307
! ----------------------------------------------------------------------         SFLX.4308
!      ET(1) = ( ZSOIL(1) / ZSOIL(NROOT) ) * GX * ETP1A                          SFLX.4309
!      ET(1) = ( ZSOIL(1) / ZSOIL(NROOT) ) * ETP1A                               SFLX.4310
! ----------------------------------------------------------------------         SFLX.4311
! USING ROOT DISTRIBUTION AS WEIGHTING FACTOR                                    SFLX.4312
! ----------------------------------------------------------------------         SFLX.4313
!      ET(1) = RTDIS(1) * ETP1A                                                  SFLX.4314
!      ET(1) = ETP1A * PART(1)                                                   SFLX.4315
! ----------------------------------------------------------------------         SFLX.4316
! LOOP DOWN THRU THE SOIL LAYERS REPEATING THE OPERATION ABOVE,                  SFLX.4317
! BUT USING THE THICKNESS OF THE SOIL LAYER (RATHER THAN THE                     SFLX.4318
! ABSOLUTE DEPTH OF EACH LAYER) IN THE FINAL CALCULATION.                        SFLX.4319
! ----------------------------------------------------------------------         SFLX.4320
!      DO K = 2,NROOT                                                            SFLX.4321
!        GX = ( SMC(K) - SMCWLT ) / ( SMCREF - SMCWLT )                          SFLX.4322
!        GX = MAX ( MIN ( GX, 1. ), 0. )                                         SFLX.4323
! TEST CANOPY RESISTANCE                                                         SFLX.4324
!        GX = 1.0                                                                SFLX.4325
!        ET(K) = ((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOT))*GX*ETP1A                   SFLX.4326
!        ET(K) = ((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOT))*ETP1A                      SFLX.4327
! ----------------------------------------------------------------------         SFLX.4328
! USING ROOT DISTRIBUTION AS WEIGHTING FACTOR                                    SFLX.4329
! ----------------------------------------------------------------------         SFLX.4330
!        ET(K) = RTDIS(K) * ETP1A                                                SFLX.4331
!        ET(K) = ETP1A*PART(K)                                                   SFLX.4332
!      END DO                                                                    SFLX.4333
! ----------------------------------------------------------------------         SFLX.4334
! END SUBROUTINE TRANSP                                                          SFLX.4335
! ----------------------------------------------------------------------         SFLX.4336
      END DO                                                                     SFLX.4337
      END                                                                        SFLX.4338
                                                                                 SFLX.4339
      SUBROUTINE WDFCND (WDF,WCND,SMC,SMCMAX,BEXP,DKSAT,DWSAT,          &        SFLX.4340
     &                      SICEMAX)                                             SFLX.4341
                                                                                 SFLX.4342
! ----------------------------------------------------------------------         SFLX.4343
! SUBROUTINE WDFCND                                                              SFLX.4344
! ----------------------------------------------------------------------         SFLX.4345
! CALCULATE SOIL WATER DIFFUSIVITY AND SOIL HYDRAULIC CONDUCTIVITY.              SFLX.4346
! ----------------------------------------------------------------------         SFLX.4347
      IMPLICIT NONE                                                              SFLX.4348
      REAL     BEXP                                                              SFLX.4349
      REAL     DKSAT                                                             SFLX.4350
      REAL     DWSAT                                                             SFLX.4351
      REAL     EXPON                                                             SFLX.4352
      REAL     FACTR1                                                            SFLX.4353
      REAL     FACTR2                                                            SFLX.4354
      REAL     SICEMAX                                                           SFLX.4355
      REAL     SMC                                                               SFLX.4356
      REAL     SMCMAX                                                            SFLX.4357
      REAL     VKwgt                                                             SFLX.4358
      REAL     WCND                                                              SFLX.4359
                                                                                 SFLX.4360
! ----------------------------------------------------------------------         SFLX.4361
!     CALC THE RATIO OF THE ACTUAL TO THE MAX PSBL SOIL H2O CONTENT              SFLX.4362
! ----------------------------------------------------------------------         SFLX.4363
      REAL     WDF                                                               SFLX.4364
      FACTR1 = 0.2 / SMCMAX                                                      SFLX.4365
                                                                                 SFLX.4366
! ----------------------------------------------------------------------         SFLX.4367
! PREP AN EXPNTL COEF AND CALC THE SOIL WATER DIFFUSIVITY                        SFLX.4368
! ----------------------------------------------------------------------         SFLX.4369
      FACTR2 = SMC / SMCMAX                                                      SFLX.4370
      EXPON = BEXP + 2.0                                                         SFLX.4371
                                                                                 SFLX.4372
! ----------------------------------------------------------------------         SFLX.4373
! FROZEN SOIL HYDRAULIC DIFFUSIVITY.  VERY SENSITIVE TO THE VERTICAL             SFLX.4374
! GRADIENT OF UNFROZEN WATER. THE LATTER GRADIENT CAN BECOME VERY                SFLX.4375
! EXTREME IN FREEZING/THAWING SITUATIONS, AND GIVEN THE RELATIVELY               SFLX.4376
! FEW AND THICK SOIL LAYERS, THIS GRADIENT SUFFERES SERIOUS                      SFLX.4377
! TRUNCTION ERRORS YIELDING ERRONEOUSLY HIGH VERTICAL TRANSPORTS OF              SFLX.4378
! UNFROZEN WATER IN BOTH DIRECTIONS FROM HUGE HYDRAULIC DIFFUSIVITY.             SFLX.4379
! THEREFORE, WE FOUND WE HAD TO ARBITRARILY CONSTRAIN WDF                        SFLX.4380
! --                                                                             SFLX.4381
! VERSION D_10CM: ........  FACTR1 = 0.2/SMCMAX                                  SFLX.4382
! WEIGHTED APPROACH...................... PABLO GRUNMANN, 28_SEP_1999.           SFLX.4383
! ----------------------------------------------------------------------         SFLX.4384
      WDF = DWSAT * FACTR2 ** EXPON                                              SFLX.4385
      IF (SICEMAX .gt. 0.0) THEN                                                 SFLX.4386
         VKWGT = 1./ (1. + (500.* SICEMAX)**3.)                                  SFLX.4387
         WDF = VKWGT * WDF + (1. - VKWGT)* DWSAT * FACTR1** EXPON                SFLX.4388
! ----------------------------------------------------------------------         SFLX.4389
! RESET THE EXPNTL COEF AND CALC THE HYDRAULIC CONDUCTIVITY                      SFLX.4390
! ----------------------------------------------------------------------         SFLX.4391
      END IF                                                                     SFLX.4392
      EXPON = (2.0 * BEXP) + 3.0                                                 SFLX.4393
                                                                                 SFLX.4394
! ----------------------------------------------------------------------         SFLX.4395
! END SUBROUTINE WDFCND                                                          SFLX.4396
! ----------------------------------------------------------------------         SFLX.4397
      WCND = DKSAT * FACTR2 ** EXPON                                             SFLX.4398
      END                                                                        SFLX.4399
                                                                                 SFLX.4400
                                                                                 SFLX.4401
      SUBROUTINE SFCDIF_off (ZLM,Z0,THZ0,THLM,SFCSPD,CZIL,AKMS,AKHS)             SFLX.4402
                                                                                 SFLX.4403
! ----------------------------------------------------------------------         SFLX.4404
! SUBROUTINE SFCDIF (renamed SFCDIF_off to avoid clash with Eta PBL)             SFLX.4405
! ----------------------------------------------------------------------         SFLX.4406
! CALCULATE SURFACE LAYER EXCHANGE COEFFICIENTS VIA ITERATIVE PROCESS.           SFLX.4407
! SEE CHEN ET AL (1997, BLM)                                                     SFLX.4408
! ----------------------------------------------------------------------         SFLX.4409
                                                                                 SFLX.4410
      IMPLICIT NONE                                                              SFLX.4411
      REAL     WWST, WWST2, G, VKRM, EXCM, BETA, BTG, ELFC, WOLD, WNEW           SFLX.4412
      REAL     PIHF, EPSU2, EPSUST, EPSIT, EPSA, ZTMIN, ZTMAX, HPBL,     &       SFLX.4413
     & SQVISC                                                                    SFLX.4414
      REAL     RIC, RRIC, FHNEU, RFC, RFAC, ZZ, PSLMU, PSLMS, PSLHU,     &       SFLX.4415
     & PSLHS                                                                     SFLX.4416
      REAL     XX, PSPMU, YY, PSPMS, PSPHU, PSPHS, ZLM, Z0, THZ0, THLM           SFLX.4417
      REAL     SFCSPD, CZIL, AKMS, AKHS, ZILFC, ZU, ZT, RDZ, CXCH                SFLX.4418
      REAL     DTHV, DU2, BTGH, WSTAR2, USTAR, ZSLU, ZSLT, RLOGU, RLOGT          SFLX.4419
      REAL     RLMO, ZETALT, ZETALU, ZETAU, ZETAT, XLU4, XLT4, XU4, XT4          SFLX.4420
!CC   ......REAL ZTFC                                                            SFLX.4421
                                                                                 SFLX.4422
      REAL     XLU, XLT, XU, XT, PSMZ, SIMM, PSHZ, SIMH, USTARK, RLMN,  &        SFLX.4423
     &         RLMA                                                              SFLX.4424
                                                                                 SFLX.4425
      INTEGER  ITRMX, ILECH, ITR                                                 SFLX.4426
      PARAMETER                                                         &        SFLX.4427
     &        (WWST = 1.2,WWST2 = WWST * WWST,G = 9.8,VKRM = 0.40,      &        SFLX.4428
     &         EXCM = 0.001                                             &        SFLX.4429
     &        ,BETA = 1./270.,BTG = BETA * G,ELFC = VKRM * BTG          &        SFLX.4430
     &                  ,WOLD =.15,WNEW = 1. - WOLD,ITRMX = 05,         &        SFLX.4431
     &                   PIHF = 3.14159265/2.)                                   SFLX.4432
      PARAMETER                                                         &        SFLX.4433
     &         (EPSU2 = 1.E-4,EPSUST = 0.07,EPSIT = 1.E-4,EPSA = 1.E-8  &        SFLX.4434
     &         ,ZTMIN = -5.,ZTMAX = 1.,HPBL = 1000.0                    &        SFLX.4435
     &          ,SQVISC = 258.2)                                                 SFLX.4436
      PARAMETER                                                         &        SFLX.4437
     &       (RIC = 0.183,RRIC = 1.0/ RIC,FHNEU = 0.8,RFC = 0.191       &        SFLX.4438
     &        ,RFAC = RIC / (FHNEU * RFC * RFC))                                 SFLX.4439
                                                                                 SFLX.4440
! ----------------------------------------------------------------------         SFLX.4441
! NOTE: THE TWO CODE BLOCKS BELOW DEFINE FUNCTIONS                               SFLX.4442
! ----------------------------------------------------------------------         SFLX.4443
! LECH'S SURFACE FUNCTIONS                                                       SFLX.4444
! ----------------------------------------------------------------------         SFLX.4445
      PSLMU (ZZ)= -0.96* log (1.0-4.5* ZZ)                                       SFLX.4446
      PSLMS (ZZ)= ZZ * RRIC -2.076* (1. -1./ (ZZ +1.))                           SFLX.4447
      PSLHU (ZZ)= -0.96* log (1.0-4.5* ZZ)                                       SFLX.4448
                                                                                 SFLX.4449
! ----------------------------------------------------------------------         SFLX.4450
! PAULSON'S SURFACE FUNCTIONS                                                    SFLX.4451
! ----------------------------------------------------------------------         SFLX.4452
      PSLHS (ZZ)= ZZ * RFAC -2.076* (1. -1./ (ZZ +1.))                           SFLX.4453
      PSPMU (XX)= -2.* log ( (XX +1.)*0.5) - log ( (XX * XX +1.)*0.5)   &        SFLX.4454
     &        +2.* ATAN (XX)                                            &        SFLX.4455
     &- PIHF                                                                     SFLX.4456
      PSPMS (YY)= 5.* YY                                                         SFLX.4457
      PSPHU (XX)= -2.* log ( (XX * XX +1.)*0.5)                                  SFLX.4458
                                                                                 SFLX.4459
! ----------------------------------------------------------------------         SFLX.4460
! THIS ROUTINE SFCDIF CAN HANDLE BOTH OVER OPEN WATER (SEA, OCEAN) AND           SFLX.4461
! OVER SOLID SURFACE (LAND, SEA-ICE).                                            SFLX.4462
! ----------------------------------------------------------------------         SFLX.4463
      PSPHS (YY)= 5.* YY                                                         SFLX.4464
                                                                                 SFLX.4465
! ----------------------------------------------------------------------         SFLX.4466
!     ZTFC: RATIO OF ZOH/ZOM  LESS OR EQUAL THAN 1                               SFLX.4467
!     C......ZTFC=0.1                                                            SFLX.4468
!     CZIL: CONSTANT C IN Zilitinkevich, S. S.1995,:NOTE ABOUT ZT                SFLX.4469
! ----------------------------------------------------------------------         SFLX.4470
      ILECH = 0                                                                  SFLX.4471
                                                                                 SFLX.4472
! ----------------------------------------------------------------------         SFLX.4473
      ZILFC = - CZIL * VKRM * SQVISC                                             SFLX.4474
!     C.......ZT=Z0*ZTFC                                                         SFLX.4475
      ZU = Z0                                                                    SFLX.4476
      RDZ = 1./ ZLM                                                              SFLX.4477
      CXCH = EXCM * RDZ                                                          SFLX.4478
      DTHV = THLM - THZ0                                                         SFLX.4479
                                                                                 SFLX.4480
! ----------------------------------------------------------------------         SFLX.4481
! BELJARS CORRECTION OF USTAR                                                    SFLX.4482
! ----------------------------------------------------------------------         SFLX.4483
      DU2 = MAX (SFCSPD * SFCSPD,EPSU2)                                          SFLX.4484
!cc   If statements to avoid TANGENT LINEAR problems near zero                   SFLX.4485
      BTGH = BTG * HPBL                                                          SFLX.4486
      IF (BTGH * AKHS * DTHV .ne. 0.0) THEN                                      SFLX.4487
         WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)                      SFLX.4488
      ELSE                                                                       SFLX.4489
         WSTAR2 = 0.0                                                            SFLX.4490
      END IF                                                                     SFLX.4491
                                                                                 SFLX.4492
! ----------------------------------------------------------------------         SFLX.4493
! ZILITINKEVITCH APPROACH FOR ZT                                                 SFLX.4494
! ----------------------------------------------------------------------         SFLX.4495
      USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)                      SFLX.4496
                                                                                 SFLX.4497
! ----------------------------------------------------------------------         SFLX.4498
      ZT = EXP (ZILFC * SQRT (USTAR * Z0))* Z0                                   SFLX.4499
      ZSLU = ZLM + ZU                                                            SFLX.4500
!     PRINT*,'ZSLT=',ZSLT                                                        SFLX.4501
!     PRINT*,'ZLM=',ZLM                                                          SFLX.4502
!     PRINT*,'ZT=',ZT                                                            SFLX.4503
                                                                                 SFLX.4504
      ZSLT = ZLM + ZT                                                            SFLX.4505
      RLOGU = log (ZSLU / ZU)                                                    SFLX.4506
                                                                                 SFLX.4507
      RLOGT = log (ZSLT / ZT)                                                    SFLX.4508
!     PRINT*,'RLMO=',RLMO                                                        SFLX.4509
!     PRINT*,'ELFC=',ELFC                                                        SFLX.4510
!     PRINT*,'AKHS=',AKHS                                                        SFLX.4511
!     PRINT*,'DTHV=',DTHV                                                        SFLX.4512
!     PRINT*,'USTAR=',USTAR                                                      SFLX.4513
                                                                                 SFLX.4514
      RLMO = ELFC * AKHS * DTHV / USTAR **3                                      SFLX.4515
! ----------------------------------------------------------------------         SFLX.4516
! 1./MONIN-OBUKKHOV LENGTH-SCALE                                                 SFLX.4517
! ----------------------------------------------------------------------         SFLX.4518
      DO ITR = 1,ITRMX                                                           SFLX.4519
         ZETALT = MAX (ZSLT * RLMO,ZTMIN)                                        SFLX.4520
         RLMO = ZETALT / ZSLT                                                    SFLX.4521
         ZETALU = ZSLU * RLMO                                                    SFLX.4522
         ZETAU = ZU * RLMO                                                       SFLX.4523
                                                                                 SFLX.4524
         ZETAT = ZT * RLMO                                                       SFLX.4525
         IF (ILECH .eq. 0) THEN                                                  SFLX.4526
            IF (RLMO .lt. 0.)THEN                                                SFLX.4527
               XLU4 = 1. -16.* ZETALU                                            SFLX.4528
               XLT4 = 1. -16.* ZETALT                                            SFLX.4529
               XU4 = 1. -16.* ZETAU                                              SFLX.4530
                                                                                 SFLX.4531
               XT4 = 1. -16.* ZETAT                                              SFLX.4532
               XLU = SQRT (SQRT (XLU4))                                          SFLX.4533
               XLT = SQRT (SQRT (XLT4))                                          SFLX.4534
               XU = SQRT (SQRT (XU4))                                            SFLX.4535
                                                                                 SFLX.4536
               XT = SQRT (SQRT (XT4))                                            SFLX.4537
!     PRINT*,'-----------1------------'                                          SFLX.4538
!     PRINT*,'PSMZ=',PSMZ                                                        SFLX.4539
!     PRINT*,'PSPMU(ZETAU)=',PSPMU(ZETAU)                                        SFLX.4540
!     PRINT*,'XU=',XU                                                            SFLX.4541
!     PRINT*,'------------------------'                                          SFLX.4542
               PSMZ = PSPMU (XU)                                                 SFLX.4543
               SIMM = PSPMU (XLU) - PSMZ + RLOGU                                 SFLX.4544
               PSHZ = PSPHU (XT)                                                 SFLX.4545
               SIMH = PSPHU (XLT) - PSHZ + RLOGT                                 SFLX.4546
            ELSE                                                                 SFLX.4547
               ZETALU = MIN (ZETALU,ZTMAX)                                       SFLX.4548
               ZETALT = MIN (ZETALT,ZTMAX)                                       SFLX.4549
!     PRINT*,'-----------2------------'                                          SFLX.4550
!     PRINT*,'PSMZ=',PSMZ                                                        SFLX.4551
!     PRINT*,'PSPMS(ZETAU)=',PSPMS(ZETAU)                                        SFLX.4552
!     PRINT*,'ZETAU=',ZETAU                                                      SFLX.4553
!     PRINT*,'------------------------'                                          SFLX.4554
               PSMZ = PSPMS (ZETAU)                                              SFLX.4555
               SIMM = PSPMS (ZETALU) - PSMZ + RLOGU                              SFLX.4556
               PSHZ = PSPHS (ZETAT)                                              SFLX.4557
               SIMH = PSPHS (ZETALT) - PSHZ + RLOGT                              SFLX.4558
            END IF                                                               SFLX.4559
! ----------------------------------------------------------------------         SFLX.4560
! LECH'S FUNCTIONS                                                               SFLX.4561
! ----------------------------------------------------------------------         SFLX.4562
         ELSE                                                                    SFLX.4563
            IF (RLMO .lt. 0.)THEN                                                SFLX.4564
!     PRINT*,'-----------3------------'                                          SFLX.4565
!     PRINT*,'PSMZ=',PSMZ                                                        SFLX.4566
!     PRINT*,'PSLMU(ZETAU)=',PSLMU(ZETAU)                                        SFLX.4567
!     PRINT*,'ZETAU=',ZETAU                                                      SFLX.4568
!     PRINT*,'------------------------'                                          SFLX.4569
               PSMZ = PSLMU (ZETAU)                                              SFLX.4570
               SIMM = PSLMU (ZETALU) - PSMZ + RLOGU                              SFLX.4571
               PSHZ = PSLHU (ZETAT)                                              SFLX.4572
               SIMH = PSLHU (ZETALT) - PSHZ + RLOGT                              SFLX.4573
            ELSE                                                                 SFLX.4574
               ZETALU = MIN (ZETALU,ZTMAX)                                       SFLX.4575
                                                                                 SFLX.4576
               ZETALT = MIN (ZETALT,ZTMAX)                                       SFLX.4577
!     PRINT*,'-----------4------------'                                          SFLX.4578
!     PRINT*,'PSMZ=',PSMZ                                                        SFLX.4579
!     PRINT*,'PSLMS(ZETAU)=',PSLMS(ZETAU)                                        SFLX.4580
!     PRINT*,'ZETAU=',ZETAU                                                      SFLX.4581
!     PRINT*,'------------------------'                                          SFLX.4582
               PSMZ = PSLMS (ZETAU)                                              SFLX.4583
               SIMM = PSLMS (ZETALU) - PSMZ + RLOGU                              SFLX.4584
               PSHZ = PSLHS (ZETAT)                                              SFLX.4585
               SIMH = PSLHS (ZETALT) - PSHZ + RLOGT                              SFLX.4586
            END IF                                                               SFLX.4587
! ----------------------------------------------------------------------         SFLX.4588
! BELJAARS CORRECTION FOR USTAR                                                  SFLX.4589
! ----------------------------------------------------------------------         SFLX.4590
         END IF                                                                  SFLX.4591
                                                                                 SFLX.4592
! ----------------------------------------------------------------------         SFLX.4593
! ZILITINKEVITCH FIX FOR ZT                                                      SFLX.4594
! ----------------------------------------------------------------------         SFLX.4595
         USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)                   SFLX.4596
                                                                                 SFLX.4597
         ZT = EXP (ZILFC * SQRT (USTAR * Z0))* Z0                                SFLX.4598
         ZSLT = ZLM + ZT                                                         SFLX.4599
!-----------------------------------------------------------------------         SFLX.4600
         RLOGT = log (ZSLT / ZT)                                                 SFLX.4601
         USTARK = USTAR * VKRM                                                   SFLX.4602
         AKMS = MAX (USTARK / SIMM,CXCH)                                         SFLX.4603
!-----------------------------------------------------------------------         SFLX.4604
! IF STATEMENTS TO AVOID TANGENT LINEAR PROBLEMS NEAR ZERO                       SFLX.4605
!-----------------------------------------------------------------------         SFLX.4606
         AKHS = MAX (USTARK / SIMH,CXCH)                                         SFLX.4607
         IF (BTGH * AKHS * DTHV .ne. 0.0) THEN                                   SFLX.4608
            WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)                   SFLX.4609
         ELSE                                                                    SFLX.4610
            WSTAR2 = 0.0                                                         SFLX.4611
         END IF                                                                  SFLX.4612
!-----------------------------------------------------------------------         SFLX.4613
         RLMN = ELFC * AKHS * DTHV / USTAR **3                                   SFLX.4614
!-----------------------------------------------------------------------         SFLX.4615
!     IF(ABS((RLMN-RLMO)/RLMA).LT.EPSIT)    GO TO 110                            SFLX.4616
!-----------------------------------------------------------------------         SFLX.4617
         RLMA = RLMO * WOLD+ RLMN * WNEW                                         SFLX.4618
!-----------------------------------------------------------------------         SFLX.4619
         RLMO = RLMA                                                             SFLX.4620
!     PRINT*,'----------------------------'                                      SFLX.4621
!     PRINT*,'SFCDIF OUTPUT !  ! ! ! ! ! ! ! !  !   !    !'                      SFLX.4622
                                                                                 SFLX.4623
!     PRINT*,'ZLM=',ZLM                                                          SFLX.4624
!     PRINT*,'Z0=',Z0                                                            SFLX.4625
!     PRINT*,'THZ0=',THZ0                                                        SFLX.4626
!     PRINT*,'THLM=',THLM                                                        SFLX.4627
!     PRINT*,'SFCSPD=',SFCSPD                                                    SFLX.4628
!     PRINT*,'CZIL=',CZIL                                                        SFLX.4629
!     PRINT*,'AKMS=',AKMS                                                        SFLX.4630
!     PRINT*,'AKHS=',AKHS                                                        SFLX.4631
!     PRINT*,'----------------------------'                                      SFLX.4632
                                                                                 SFLX.4633
! ----------------------------------------------------------------------         SFLX.4634
! END SUBROUTINE SFCDIF                                                          SFLX.4635
! ----------------------------------------------------------------------         SFLX.4636
      END DO                                                                     SFLX.4637
      END                                                                        SFLX.4638
