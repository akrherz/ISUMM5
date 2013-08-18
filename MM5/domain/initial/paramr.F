             SUBROUTINE PARAMR                                                   PARAMR.1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         PARAMR.2
C                                                                      C         PARAMR.3
C      CHANGES                                                         C         PARAMR.4
C        CONSTANTS CHANGED FOR 2-10 EXMOISG RELEASE                    C         PARAMR.5
C        CNP CHANGED IN BOTH EXMOISG AND EXMOISR                       C         PARAMR.6
C        CONSTANTS CHANGED FOR 2-11 EXMOISG RELEASE                    C         PARAMR.7
C        CHANGE COMMON BLOCK JRG TO A INCLUDE FILE                     C         PARAMR.8
C                                                                      C         PARAMR.9
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         PARAMR.10
                                                                                 PARAMR.11
#     include <jrg.incl>                                                         PARAMR.12
                                                                                 PARAMR.13
      COMMON/BER/ ABER1(31),ABER2(31)                                            PARAMR.14
C LOOKUP TABLE FOR A1 AND A2 IN BERGERGON PROCESS                                PARAMR.15
                                                                                 PARAMR.16
      DATA ABER1/.7939E-07,.7841E-06,.3369E-05,.4336E-05,                        PARAMR.17
     1 .5285E-05,.3728E-05,.1852E-05,.2991E-06,.4248E-06,                        PARAMR.18
     2 .7434E-06,.1812E-05,.4394E-05,.9145E-05,.1725E-06,                        PARAMR.19
     3 .3348E-04,.1725E-04,.9175E-05,.4412E-05,.2252E-05,                        PARAMR.20
     4 .9115E-06,.4876E-06,.3473E-06,.4758E-06,.6306E-06,                        PARAMR.21
     5 .8573E-06,.7868E-06,.7192E-06,.6513E-06,.5956E-06,                        PARAMR.22
     6 .5333E-06,.4834E-06/                                                      PARAMR.23
                                                                                 PARAMR.24
      DATA ABER2/.4006,.4831,.5320,.5307,.5319,.5249,                            PARAMR.25
     1 .4888,.3894,.4047,.4318,.4771,.5183,.5463,.5651,                          PARAMR.26
     1 .5813,.5655,.5478,.5203,.4906,.4447,.4126,.3960,                          PARAMR.27
     1 .4149,.4320,.4506,.4483,.4460,.4433,.4413,.4382,                          23DEC04.210
     1 .4361/                                                                    PARAMR.29
                                                                                 PARAMR.30
      PI=ACOS(-1.)                                                               PARAMR.31
      RHO_NOT = 101325.0/(287.05*298.0)                                          19DEC02.506
C SLOPE INTERCEPT FOR RAIN, SNOW, AND GRAUPEL                                    PARAMR.32
      RON=8.E6                                                                   PARAMR.33
c     RON2=1.E10                                                                 23DEC04.211
      RON2=1.E9                                                                  23DEC04.212
      SON=2.E7                                                                   PARAMR.36
c     GON=5.E7                                                                   23DEC04.213
      GON=4.E6                                                                   23DEC04.214
c     ron_min = 8.e6                                                             23DEC04.215
      ron_min = 2.e7                                                             23DEC04.216
c     ron_qr0 = 0.00010                                                          23DEC04.217
      ron_qr0 = 0.00020                                                          23DEC04.218
c     ron_delqr0 = 0.25*ron_qr0                                                  23DEC04.219
      ron_delqr0 = 0.5*ron_qr0                                                   23DEC04.220
      ron_const1r = (ron2-ron_min)*0.5                                           19DEC02.511
      ron_const2r = (ron2+ron_min)*0.5                                           19DEC02.512
C EXPONENT FOR RAIN, SNOW, AND GRAUPEL, IN FALL SPEED V(D)=A EXP(B)              PARAMR.38
      BR=0.8                                                                     PARAMR.39
      BS=0.41                                                                    PARAMR.40
      BG=0.37                                                                    PARAMR.41
C A IN FALL SPEED                                                                PARAMR.42
      ARAIN=842.                                                                 PARAMR.43
      ASNOW=11.72                                                                PARAMR.44
      AGRAUPEL=19.3                                                              PARAMR.45
C DENSITY OF RAIN, SNOW, AND GRAUPEL                                             PARAMR.46
      DRAIN=1000.                                                                PARAMR.47
      DSNOW=100.                                                                 PARAMR.48
      DGRAUPEL=400.                                                              PARAMR.49
      DICE=500.                                                                  PARAMR.50
C SMALLEST SIZE OF SNOW AND GRAUPEL (RADIUS, METERS)                             PARAMR.51
      XR0S= 75.E-6                                                               23DEC04.221
      XR0G=150.E-6                                                               23DEC04.222
C MINIMUM MASS OF ICE, SNOW, GRAUPEL                                             PARAMR.54
      XM01=1.0E-12                                                               PARAMR.55
      XM0S=4.*PI/3.*DSNOW*XR0S**3                                                PARAMR.56
      XM0G=4.*PI/3.*DGRAUPEL*XR0G**3                                             PARAMR.57
      DIACE_min = 2.0 * (3.0*XM01/(4.0*PI*DICE))**0.3333                         19DEC02.513
C TOP OF SLOPE FOR RAIN, SNOW, AND GRAUPEL                                       PARAMR.58
      TOPR=PI*DRAIN*RON                                                          PARAMR.59
      TOPS=PI*DSNOW*SON                                                          PARAMR.60
      TOPG=PI*DGRAUPEL*GON                                                       PARAMR.61
CONSTANTS FOR VARIABLE SON                                                       PARAMR.62
      CONST1A=2.5E6*(1./1000*1./3600.)**(0.94)                                   PARAMR.63
      GI=4.+BS                                                                   PARAMR.64
      CONSTA=GAMMA(GI)                                                           PARAMR.65
      CONST1B=ASNOW*CONSTA/6.                                                    PARAMR.66
C CONSTANT IN FLETCHER CURVES                                                    PARAMR.67
CC    TNO=1.E-2                                                                  PARAMR.68
CC    ATO=0.6                                                                    PARAMR.69
C CONSTANT IN COOPER CURVES                                                      PARAMR.70
      TNO=5.0                                                                    PARAMR.71
      ATO=0.304                                                                  PARAMR.72
C CONSTANTS FOR BERGERON PROCESS                                                 PARAMR.73
      INT0=273                                                                   PARAMR.74
      BERC1=PI*50.0E-06*50.0E-06                                                 PARAMR.75
C FREEZING OF CLOUD DROPLETS, MURAKAMI (1989)                                    PARAMR.76
      BP=100.                                                                    PARAMR.77
      AP=0.66                                                                    PARAMR.78
      CNP=100.E6                                                                 23DEC04.223
C FREEZING OF RAIN DROPLETS, LIN ET AL (45)                                      PARAMR.80
      FRD1=PI*PI*20.*BP*RON                                                      PARAMR.81
      FRA1=AP                                                                    PARAMR.82
C COLLECTION EFFICIENCIES                                                        PARAMR.83
      EFIS=0.1                                                                   PARAMR.84
      EFIR=1.0                                                                   PARAMR.85
      EFSR=1.0                                                                   PARAMR.86
      EFCS=1.0                                                                   PARAMR.87
      EFGI=0.1                                                                   PARAMR.88
      EFGC=1.0                                                                   PARAMR.89
      EFGR=1.0                                                                   PARAMR.90
      EFGS=0.1                                                                   PARAMR.91
      EFCR=1.0                                                                   PARAMR.92
C COLLECTION OF CLOUD ICE BY SNOW                                                PARAMR.93
      GI=3.+BS                                                                   PARAMR.94
      GS=GAMMA(GI)                                                               PARAMR.95
      ACRIS=0.25*PI*ASNOW*EFIS*SON*GS                                            PARAMR.96
      BACRIS=3+BS                                                                PARAMR.97
COLLECTION OF CLOUD ICE BY RAIN                                                  PARAMR.98
      CIR=0.25*PI*EFIR*RON                                                       PARAMR.99
C RATE AT WHICH RAIN IS FROZEN BY COLLISION WITH CLOUD ICE                       PARAMR.100
      CIRF=1./24.*PI*PI*DRAIN*RON*EFIR                                           PARAMR.101
C PARAMETERS FOR MEAN FALL SPEED                                                 PARAMR.102
      GI=4+BR                                                                    PARAMR.103
      GR=GAMMA(GI)                                                               PARAMR.104
      FRAIN=ARAIN*GR/6.                                                          PARAMR.105
      GI=4+BS                                                                    PARAMR.106
      GS=GAMMA(GI)                                                               PARAMR.107
      FSNOW=ASNOW*GS/6.                                                          PARAMR.108
      GI=4+BG                                                                    PARAMR.109
      GG=GAMMA(GI)                                                               PARAMR.110
      FGRAUPEL=AGRAUPEL*GG/6.                                                    PARAMR.111
COLLECTION OF SNOW BY RAIN                                                       PARAMR.112
      CSR=PI*PI*EFSR*DRAIN*RON*SON                                               PARAMR.113
      ALPHA1=1.2                                                                 PARAMR.114
      BETA1=0.95                                                                 PARAMR.115
      GAMMA1=0.08                                                                PARAMR.116
COLLECTION OF RAIN BY SNOW                                                       PARAMR.117
      CRS=PI*PI*EFSR*DSNOW*RON*SON                                               PARAMR.118
COLLECTION OF CLOUD WATER BY SNOW                                                PARAMR.119
      GI=BS+3.                                                                   PARAMR.120
      GS=GAMMA(GI)                                                               PARAMR.121
      ACRCS=0.25*PI*ASNOW*EFCS*SON*GS                                            PARAMR.122
      BACRCS=3.+BS                                                               PARAMR.123
C LOSS OF SNOW DUE TO COLLISION WITH CLOUD WATER                                 PARAMR.124
      GI=2.*BS+2.0                                                               PARAMR.125
      GS=GAMMA(GI)                                                               PARAMR.126
      ACRLS=3.0*PI*SON*GS*ASNOW*ASNOW                                            PARAMR.127
      BACLS=GI                                                                   PARAMR.128
COLLECTION LOSS OF SNOW DUE TO COLLISION WITH CLOUD WATER                        PARAMR.129
CC    RMC=4.E-12                                                                 PARAMR.130
CC    GI=6.+BS                                                                   PARAMR.131
CC    GS=GAMMA(GI)                                                               PARAMR.132
CC    ACRLS=(1./24.)*PI*PI*EFCS*SON*ASNOW*DSNOW*GS                               PARAMR.133
CC    BACLS=6.+BS                                                                PARAMR.134
COLLECTION OF CLOUD WATER BY GRAUPEL                                             PARAMR.135
      GI=3.+BG                                                                   23DEC04.224
C     GI=4.+BG                                                                   23DEC04.225
      GG=GAMMA(GI)                                                               PARAMR.137
      ACRCG=0.25*PI*AGRAUPEL*EFGC*GON*GG                                         PARAMR.138
      BACRCG=3.+BG                                                               23DEC04.226
C     BACRCG=4.+BG                                                               23DEC04.227
COLLECTION OF CLOUD ICE BY GRAUPEL                                               PARAMR.140
      GI=3.+BG                                                                   19DEC02.519
C     GI=4.+BG                                                                   19DEC02.520
      GG=GAMMA(GI)                                                               PARAMR.142
      ACRIG=0.25*PI*AGRAUPEL*EFGI*GON*GG                                         PARAMR.143
      BACRIG=3.+BG                                                               PARAMR.144
COLLECTION OF RAIN BY GRAUPEL                                                    PARAMR.145
      CRG=PI*PI*EFGR*DRAIN*RON*GON                                               PARAMR.146
COLLECTION OF SNOW BY GRAUPEL                                                    PARAMR.147
      CSG=PI*PI*EFGS*DSNOW*SON*GON                                               PARAMR.148
C DEPOSITIONAL GROWTH OF GRAUPEL                                                 PARAMR.149
      GI=BG/2.+2.5                                                               PARAMR.150
      GG=GAMMA(GI)                                                               PARAMR.151
      DEPG1=2*PI*GON                                                             PARAMR.152
      DEPG2=AGRAUPEL                                                             PARAMR.153
      DEPG3=0.31*GG                                                              PARAMR.154
      DEPG4=BG/2.+2.5                                                            PARAMR.155
C DEPOSITIONAL GROWTH OF SNOW                                                    PARAMR.156
      GI=BS/2.+2.5                                                               PARAMR.157
      GS=GAMMA(GI)                                                               PARAMR.158
      DEPS1=4.*SON                                                               PARAMR.159
      DEPS2=ASNOW                                                                PARAMR.160
      DEPS3=0.44*GS                                                              PARAMR.161
      DEPS4=BS/2.+2.5                                                            PARAMR.162
C AGGREGATION OF CLOUD ICE                                                       PARAMR.163
      C1=700.*0.1*0.25/DICE                                                      PARAMR.164
C AUTOCONVERSION TO SNOW                                                         PARAMR.165
      XSMAX=9.4E-10                                                              PARAMR.166
C COLLECTION OF CLOUD WATER BY RAIN                                              PARAMR.167
      GI=3.+BR                                                                   PARAMR.168
      GR=GAMMA(GI)                                                               PARAMR.169
      ACRCR=0.25*PI*ARAIN*EFCR*RON*GR                                            PARAMR.170
      BACRCR=3.+BR                                                               PARAMR.171
C DEPOSITIONAL GROWTH OF RAIN                                                    PARAMR.172
      GI=BR/2.+2.5                                                               PARAMR.173
      GR=GAMMA(GI)                                                               PARAMR.174
      DEPR1=2*PI*RON                                                             PARAMR.175
      DEPR2=ARAIN                                                                PARAMR.176
      DEPR3=0.31*GR                                                              PARAMR.177
      DEPR4=BR/2.+2.5                                                            PARAMR.178
C FOR MELTING OF SNOW                                                            PARAMR.179
      GI=BS/2.+2.5                                                               PARAMR.180
      GS=GAMMA(GI)                                                               PARAMR.181
      PSM1=2*PI*SON                                                              PARAMR.182
      PSM2=ASNOW                                                                 PARAMR.183
      PSM3=0.44*GS                                                               PARAMR.184
      PSM4=BS/2.+2.5                                                             PARAMR.185
C FOR MELTING OF GRAUPEL                                                         PARAMR.186
      GI=BG/2.+2.5                                                               PARAMR.187
      GG=GAMMA(GI)                                                               PARAMR.188
      PGM1=2*PI*GON                                                              PARAMR.189
      PGM2=AGRAUPEL                                                              PARAMR.190
      PGM3=0.31*GG                                                               PARAMR.191
      PGM4=BG/2.+2.5                                                             PARAMR.192
CONSTANT FOR ENHANCED MELTING OF GRAUPEL BY RAIN AND CLOUD WATER                 PARAMR.193
      CW=4218.                                                                   PARAMR.194
CONSTANT FOR HOMOGENEOUS FREEZING OF CLOUD DROPLETS                              PARAMR.195
      HGFR=233.15                                                                PARAMR.196
      RETURN                                                                     PARAMR.197
      END                                                                        PARAMR.198
                                                                                 PARAMR.199
