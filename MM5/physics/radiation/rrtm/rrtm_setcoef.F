C **************************************************************************     RRTM_SETCOEF.1
      SUBROUTINE SETCOEF                                                         07NOV00.2467
C **************************************************************************     RRTM_SETCOEF.3
C  RRTM Longwave Radiative Transfer Model                                        RRTM_SETCOEF.4
C  Atmospheric and Environmental Research, Inc., Cambridge, MA                   RRTM_SETCOEF.5
C                                                                                RRTM_SETCOEF.6
C  Original version:       E. J. Mlawer, et al.                                  RRTM_SETCOEF.7
C  Revision for NCAR CCM:  Michael J. Iacono; September, 1998                    RRTM_SETCOEF.8
C                                                                                RRTM_SETCOEF.9
C  For a given atmosphere, calculate the indices and fractions related to the    RRTM_SETCOEF.10
C  pressure and temperature interpolations.  Also calculate the values of the    RRTM_SETCOEF.11
C  integrated Planck functions for each band at the level and layer              RRTM_SETCOEF.12
C  temperatures.                                                                 RRTM_SETCOEF.13
C **************************************************************************     RRTM_SETCOEF.14
                                                                                 RRTM_SETCOEF.15
C Parameter                                                                      RRTM_SETCOEF.16
      PARAMETER (MXLAY=203)                                                      RRTM_SETCOEF.17
C  Input                                                                         RRTM_SETCOEF.18
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       RRTM_SETCOEF.19
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  RRTM_SETCOEF.20
      COMMON /SPECIES/  COLDRY(MXLAY),WKL(35,MXLAY),WBROAD(MXLAY),               RRTM_SETCOEF.21
     &                  NMOL                                                     RRTM_SETCOEF.22
C  Output                                                                        RRTM_SETCOEF.23
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,COLH2O(MXLAY),                   RRTM_SETCOEF.24
     &                  COLCO2(MXLAY),COLO3(MXLAY),COLN2O(MXLAY),                RRTM_SETCOEF.25
     &                  COLCH4(MXLAY),COLO2(MXLAY),CO2MULT(MXLAY)                RRTM_SETCOEF.26
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                               RRTM_SETCOEF.27
     &                  FAC10(MXLAY),FAC11(MXLAY)                                RRTM_SETCOEF.28
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                           RRTM_SETCOEF.29
      COMMON /SELF/     SELFFAC, SELFFRAC, INDSELF                               RRTM_SETCOEF.30
      COMMON /FOREIGN/  FORFAC(MXLAY)                                            RRTM_SETCOEF.31
                                                                                 RRTM_SETCOEF.32
C This compiler directive was added to insure private common block storage       RRTM_SETCOEF.33
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_SETCOEF.34
C carry constants.                                                               RRTM_SETCOEF.35
C SGI                                                                            RRTM_SETCOEF.38
C$OMP THREADPRIVATE (/PROFILE/,/SPECIES/,/PROFDATA/,/INTFAC/)                    RRTM_SETCOEF.39
C$OMP THREADPRIVATE (/INTIND/,/SELF/,/FOREIGN/)                                  RRTM_SETCOEF.40
C ***                                                                            RRTM_SETCOEF.41
                                                                                 RRTM_SETCOEF.42
      DIMENSION SELFFAC(MXLAY),SELFFRAC(MXLAY),INDSELF(MXLAY)                    RRTM_SETCOEF.43
      DIMENSION PREF(59),PREFLOG(59),TREF(59)                                    RRTM_SETCOEF.44
                                                                                 RRTM_SETCOEF.45
C     These pressures are chosen such that the ln of the first pressure          RRTM_SETCOEF.46
C     has only a few non-zero digits (i.e. ln(PREF(1)) = 6.96000) and            RRTM_SETCOEF.47
C     each subsequent ln(pressure) differs from the previous one by 0.2.         RRTM_SETCOEF.48
      DATA PREF /                                                                RRTM_SETCOEF.49
     &    1.05363E+03,8.62642E+02,7.06272E+02,5.78246E+02,4.73428E+02,           RRTM_SETCOEF.50
     &    3.87610E+02,3.17348E+02,2.59823E+02,2.12725E+02,1.74164E+02,           RRTM_SETCOEF.51
     &    1.42594E+02,1.16746E+02,9.55835E+01,7.82571E+01,6.40715E+01,           RRTM_SETCOEF.52
     &    5.24573E+01,4.29484E+01,3.51632E+01,2.87892E+01,2.35706E+01,           RRTM_SETCOEF.53
     &    1.92980E+01,1.57998E+01,1.29358E+01,1.05910E+01,8.67114E+00,           RRTM_SETCOEF.54
     &    7.09933E+00,5.81244E+00,4.75882E+00,3.89619E+00,3.18993E+00,           RRTM_SETCOEF.55
     &    2.61170E+00,2.13828E+00,1.75067E+00,1.43333E+00,1.17351E+00,           RRTM_SETCOEF.56
     &    9.60789E-01,7.86628E-01,6.44036E-01,5.27292E-01,4.31710E-01,           RRTM_SETCOEF.57
     &    3.53455E-01,2.89384E-01,2.36928E-01,1.93980E-01,1.58817E-01,           RRTM_SETCOEF.58
     &    1.30029E-01,1.06458E-01,8.71608E-02,7.13612E-02,5.84256E-02,           RRTM_SETCOEF.59
     &    4.78349E-02,3.91639E-02,3.20647E-02,2.62523E-02,2.14936E-02,           RRTM_SETCOEF.60
     &    1.75975E-02,1.44076E-02,1.17959E-02,9.65769E-03/                       RRTM_SETCOEF.61
      DATA PREFLOG /                                                             RRTM_SETCOEF.62
     &     6.9600E+00, 6.7600E+00, 6.5600E+00, 6.3600E+00, 6.1600E+00,           RRTM_SETCOEF.63
     &     5.9600E+00, 5.7600E+00, 5.5600E+00, 5.3600E+00, 5.1600E+00,           RRTM_SETCOEF.64
     &     4.9600E+00, 4.7600E+00, 4.5600E+00, 4.3600E+00, 4.1600E+00,           RRTM_SETCOEF.65
     &     3.9600E+00, 3.7600E+00, 3.5600E+00, 3.3600E+00, 3.1600E+00,           RRTM_SETCOEF.66
     &     2.9600E+00, 2.7600E+00, 2.5600E+00, 2.3600E+00, 2.1600E+00,           RRTM_SETCOEF.67
     &     1.9600E+00, 1.7600E+00, 1.5600E+00, 1.3600E+00, 1.1600E+00,           RRTM_SETCOEF.68
     &     9.6000E-01, 7.6000E-01, 5.6000E-01, 3.6000E-01, 1.6000E-01,           RRTM_SETCOEF.69
     &    -4.0000E-02,-2.4000E-01,-4.4000E-01,-6.4000E-01,-8.4000E-01,           RRTM_SETCOEF.70
     &    -1.0400E+00,-1.2400E+00,-1.4400E+00,-1.6400E+00,-1.8400E+00,           RRTM_SETCOEF.71
     &    -2.0400E+00,-2.2400E+00,-2.4400E+00,-2.6400E+00,-2.8400E+00,           RRTM_SETCOEF.72
     &    -3.0400E+00,-3.2400E+00,-3.4400E+00,-3.6400E+00,-3.8400E+00,           RRTM_SETCOEF.73
     &    -4.0400E+00,-4.2400E+00,-4.4400E+00,-4.6400E+00/                       RRTM_SETCOEF.74
C     These are the temperatures associated with the respective                  RRTM_SETCOEF.75
C     pressures for the MLS standard atmosphere.                                 RRTM_SETCOEF.76
      DATA TREF /                                                                RRTM_SETCOEF.77
     &     2.9420E+02, 2.8799E+02, 2.7894E+02, 2.6925E+02, 2.5983E+02,           RRTM_SETCOEF.78
     &     2.5017E+02, 2.4077E+02, 2.3179E+02, 2.2306E+02, 2.1578E+02,           RRTM_SETCOEF.79
     &     2.1570E+02, 2.1570E+02, 2.1570E+02, 2.1706E+02, 2.1858E+02,           RRTM_SETCOEF.80
     &     2.2018E+02, 2.2174E+02, 2.2328E+02, 2.2479E+02, 2.2655E+02,           RRTM_SETCOEF.81
     &     2.2834E+02, 2.3113E+02, 2.3401E+02, 2.3703E+02, 2.4022E+02,           RRTM_SETCOEF.82
     &     2.4371E+02, 2.4726E+02, 2.5085E+02, 2.5457E+02, 2.5832E+02,           RRTM_SETCOEF.83
     &     2.6216E+02, 2.6606E+02, 2.6999E+02, 2.7340E+02, 2.7536E+02,           RRTM_SETCOEF.84
     &     2.7568E+02, 2.7372E+02, 2.7163E+02, 2.6955E+02, 2.6593E+02,           RRTM_SETCOEF.85
     &     2.6211E+02, 2.5828E+02, 2.5360E+02, 2.4854E+02, 2.4348E+02,           RRTM_SETCOEF.86
     &     2.3809E+02, 2.3206E+02, 2.2603E+02, 2.2000E+02, 2.1435E+02,           RRTM_SETCOEF.87
     &     2.0887E+02, 2.0340E+02, 1.9792E+02, 1.9290E+02, 1.8809E+02,           RRTM_SETCOEF.88
     &     1.8329E+02, 1.7849E+02, 1.7394E+02, 1.7212E+02/                       RRTM_SETCOEF.89
                                                                                 RRTM_SETCOEF.90
C ****************** START OF EXECUTABLE CODE ***************************        RRTM_SETCOEF.91
                                                                                 RRTM_SETCOEF.92
      STPFAC = 296./1013.                                                        RRTM_SETCOEF.93
                                                                                 RRTM_SETCOEF.94
      LAYTROP = 0                                                                RRTM_SETCOEF.95
      LAYSWTCH = 0                                                               RRTM_SETCOEF.96
      LAYLOW = 0                                                                 RRTM_SETCOEF.97
      DO 7000 LAY = 1, NLAYERS                                                   RRTM_SETCOEF.98
C        Find the two reference pressures on either side of the                  RRTM_SETCOEF.99
C        layer pressure.  Store them in JP and JP1.  Store in FP the             RRTM_SETCOEF.100
C        fraction of the difference (in ln(pressure)) between these              RRTM_SETCOEF.101
C        two values that the layer pressure lies.                                RRTM_SETCOEF.102
         PLOG = LOG(PAVEL(LAY))                                                  RRTM_SETCOEF.103
         JP(LAY) = INT(36. - 5*(PLOG+0.04))                                      RRTM_SETCOEF.104
         IF (JP(LAY) .LT. 1) THEN                                                RRTM_SETCOEF.105
            JP(LAY) = 1                                                          RRTM_SETCOEF.106
         ELSEIF (JP(LAY) .GT. 58) THEN                                           RRTM_SETCOEF.107
            JP(LAY) = 58                                                         RRTM_SETCOEF.108
         ENDIF                                                                   RRTM_SETCOEF.109
         JP1 = JP(LAY) + 1                                                       RRTM_SETCOEF.110
         FP = 5. * (PREFLOG(JP(LAY)) - PLOG)                                     RRTM_SETCOEF.111
                                                                                 RRTM_SETCOEF.112
C        Determine, for each reference pressure (JP and JP1), which              RRTM_SETCOEF.113
C        reference temperature (these are different for each                     RRTM_SETCOEF.114
C        reference pressure) is nearest the layer temperature but does           RRTM_SETCOEF.115
C        not exceed it.  Store these indices in JT and JT1, resp.                RRTM_SETCOEF.116
C        Store in FT (resp. FT1) the fraction of the way between JT              RRTM_SETCOEF.117
C        (JT1) and the next highest reference temperature that the               RRTM_SETCOEF.118
C        layer temperature falls.                                                RRTM_SETCOEF.119
         JT(LAY) = INT(3. + (TAVEL(LAY)-TREF(JP(LAY)))/15.)                      RRTM_SETCOEF.120
         IF (JT(LAY) .LT. 1) THEN                                                RRTM_SETCOEF.121
            JT(LAY) = 1                                                          RRTM_SETCOEF.122
         ELSEIF (JT(LAY) .GT. 4) THEN                                            RRTM_SETCOEF.123
            JT(LAY) = 4                                                          RRTM_SETCOEF.124
         ENDIF                                                                   RRTM_SETCOEF.125
         FT = ((TAVEL(LAY)-TREF(JP(LAY)))/15.) - FLOAT(JT(LAY)-3)                RRTM_SETCOEF.126
         JT1(LAY) = INT(3. + (TAVEL(LAY)-TREF(JP1))/15.)                         RRTM_SETCOEF.127
         IF (JT1(LAY) .LT. 1) THEN                                               RRTM_SETCOEF.128
            JT1(LAY) = 1                                                         RRTM_SETCOEF.129
         ELSEIF (JT1(LAY) .GT. 4) THEN                                           RRTM_SETCOEF.130
            JT1(LAY) = 4                                                         RRTM_SETCOEF.131
         ENDIF                                                                   RRTM_SETCOEF.132
         FT1 = ((TAVEL(LAY)-TREF(JP1))/15.) - FLOAT(JT1(LAY)-3)                  RRTM_SETCOEF.133
                                                                                 RRTM_SETCOEF.134
         WATER = WKL(1,LAY)/COLDRY(LAY)                                          RRTM_SETCOEF.135
         SCALEFAC = PAVEL(LAY) * STPFAC / TAVEL(LAY)                             RRTM_SETCOEF.136
                                                                                 RRTM_SETCOEF.137
C        If the pressure is less than ~100mb, perform a different                RRTM_SETCOEF.138
C        set of species interpolations.                                          RRTM_SETCOEF.139
         IF (PLOG .LE. 4.56) GO TO 5300                                          RRTM_SETCOEF.140
         LAYTROP =  LAYTROP + 1                                                  RRTM_SETCOEF.141
C        For one band, the "switch" occurs at ~300 mb.                           RRTM_SETCOEF.142
c JD: changed from (PLOG .GE. 5.76) to avoid out-of-range                        RRTM_SETCOEF.143
         IF (PLOG .Gt. 5.76) LAYSWTCH = LAYSWTCH + 1                             RRTM_SETCOEF.144
         IF (PLOG .GE. 6.62) LAYLOW = LAYLOW + 1                                 RRTM_SETCOEF.145
C                                                                                RRTM_SETCOEF.146
         FORFAC(LAY) = SCALEFAC / (1.+WATER)                                     RRTM_SETCOEF.147
C        Set up factors needed to separately include the water vapor             RRTM_SETCOEF.148
C        self-continuum in the calculation of absorption coefficient.            RRTM_SETCOEF.149
         SELFFAC(LAY) = WATER * FORFAC(LAY)                                      RRTM_SETCOEF.150
         FACTOR = (TAVEL(LAY)-188.0)/7.2                                         RRTM_SETCOEF.151
         INDSELF(LAY) = MIN(9, MAX(1, INT(FACTOR)-7))                            RRTM_SETCOEF.152
         SELFFRAC(LAY) = FACTOR - FLOAT(INDSELF(LAY) + 7)                        RRTM_SETCOEF.153
                                                                                 RRTM_SETCOEF.154
C        Calculate needed column amounts.                                        RRTM_SETCOEF.155
         COLH2O(LAY) = 1.E-20 * WKL(1,LAY)                                       RRTM_SETCOEF.156
         COLCO2(LAY) = 1.E-20 * WKL(2,LAY)                                       RRTM_SETCOEF.157
         COLO3(LAY) = 1.E-20 * WKL(3,LAY)                                        RRTM_SETCOEF.158
         COLN2O(LAY) = 1.E-20 * WKL(4,LAY)                                       RRTM_SETCOEF.159
         COLCH4(LAY) = 1.E-20 * WKL(6,LAY)                                       RRTM_SETCOEF.160
         COLO2(LAY) = 1.E-20 * WKL(7,LAY)                                        RRTM_SETCOEF.161
         IF (COLCO2(LAY) .EQ. 0.) COLCO2(LAY) = 1.E-32 * COLDRY(LAY)             RRTM_SETCOEF.162
         IF (COLN2O(LAY) .EQ. 0.) COLN2O(LAY) = 1.E-32 * COLDRY(LAY)             RRTM_SETCOEF.163
         IF (COLCH4(LAY) .EQ. 0.) COLCH4(LAY) = 1.E-32 * COLDRY(LAY)             RRTM_SETCOEF.164
C        Using E = 1334.2 cm-1.                                                  RRTM_SETCOEF.165
         CO2REG = 3.55E-24 * COLDRY(LAY)                                         RRTM_SETCOEF.166
         CO2MULT(LAY)= (COLCO2(LAY) - CO2REG) *                                  RRTM_SETCOEF.167
     &        272.63*EXP(-1919.4/TAVEL(LAY))/(8.7604E-4*TAVEL(LAY))              RRTM_SETCOEF.168
         GO TO 5400                                                              RRTM_SETCOEF.169
                                                                                 RRTM_SETCOEF.170
C        Above LAYTROP.                                                          RRTM_SETCOEF.171
 5300    CONTINUE                                                                RRTM_SETCOEF.172
                                                                                 RRTM_SETCOEF.173
         FORFAC(LAY) = SCALEFAC / (1.+WATER)                                     RRTM_SETCOEF.174
C        Calculate needed column amounts.                                        RRTM_SETCOEF.175
         COLH2O(LAY) = 1.E-20 * WKL(1,LAY)                                       RRTM_SETCOEF.176
         COLCO2(LAY) = 1.E-20 * WKL(2,LAY)                                       RRTM_SETCOEF.177
         COLO3(LAY) = 1.E-20 * WKL(3,LAY)                                        RRTM_SETCOEF.178
         COLN2O(LAY) = 1.E-20 * WKL(4,LAY)                                       RRTM_SETCOEF.179
         COLCH4(LAY) = 1.E-20 * WKL(6,LAY)                                       RRTM_SETCOEF.180
         COLO2(LAY) = 1.E-20 * WKL(7,LAY)                                        RRTM_SETCOEF.181
         IF (COLCO2(LAY) .EQ. 0.) COLCO2(LAY) = 1.E-32 * COLDRY(LAY)             RRTM_SETCOEF.182
         IF (COLN2O(LAY) .EQ. 0.) COLN2O(LAY) = 1.E-32 * COLDRY(LAY)             RRTM_SETCOEF.183
         IF (COLCH4(LAY) .EQ. 0.) COLCH4(LAY) = 1.E-32 * COLDRY(LAY)             RRTM_SETCOEF.184
         CO2REG = 3.55E-24 * COLDRY(LAY)                                         RRTM_SETCOEF.185
         CO2MULT(LAY)= (COLCO2(LAY) - CO2REG) *                                  RRTM_SETCOEF.186
     &        272.63*EXP(-1919.4/TAVEL(LAY))/(8.7604E-4*TAVEL(LAY))              RRTM_SETCOEF.187
 5400    CONTINUE                                                                RRTM_SETCOEF.188
                                                                                 RRTM_SETCOEF.189
C        We have now isolated the layer ln pressure and temperature,             RRTM_SETCOEF.190
C        between two reference pressures and two reference temperatures          RRTM_SETCOEF.191
C        (for each reference pressure).  We multiply the pressure                RRTM_SETCOEF.192
C        fraction FP with the appropriate temperature fractions to get           RRTM_SETCOEF.193
C        the factors that will be needed for the interpolation that yields       RRTM_SETCOEF.194
C        the optical depths (performed in routines TAUGBn for band n).           RRTM_SETCOEF.195
                                                                                 RRTM_SETCOEF.196
         COMPFP = 1. - FP                                                        RRTM_SETCOEF.197
         FAC10(LAY) = COMPFP * FT                                                RRTM_SETCOEF.198
         FAC00(LAY) = COMPFP * (1. - FT)                                         RRTM_SETCOEF.199
         FAC11(LAY) = FP * FT1                                                   RRTM_SETCOEF.200
         FAC01(LAY) = FP * (1. - FT1)                                            RRTM_SETCOEF.201
                                                                                 RRTM_SETCOEF.202
 7000 CONTINUE                                                                   RRTM_SETCOEF.203
                                                                                 RRTM_SETCOEF.204
C        Set LAYLOW for profiles with surface pressure less than 750mb.          RRTM_SETCOEF.205
         IF (LAYLOW.EQ.0) LAYLOW=1                                               RRTM_SETCOEF.206
                                                                                 RRTM_SETCOEF.207
      RETURN                                                                     RRTM_SETCOEF.208
      END                                                                        RRTM_SETCOEF.209
                                                                                 RRTM_SETCOEF.210
                                                                                 RRTM_SETCOEF.211
      BLOCK DATA AVPLANK                                                         RRTM_SETCOEF.212
                                                                                 RRTM_SETCOEF.213
      PARAMETER (NBANDS = 16)                                                    RRTM_SETCOEF.214
                                                                                 RRTM_SETCOEF.215
      COMMON /AVGPLNK/  TOTPLNK(181,NBANDS), TOTPLK16(181)                       RRTM_SETCOEF.216
                                                                                 RRTM_SETCOEF.217
      DATA (TOTPLNK(I, 1),I=1,50)/                                               RRTM_SETCOEF.218
     &1.13735E-06,1.15150E-06,1.16569E-06,1.17992E-06,1.19419E-06,               RRTM_SETCOEF.219
     &1.20850E-06,1.22285E-06,1.23723E-06,1.25164E-06,1.26610E-06,               RRTM_SETCOEF.220
     &1.28059E-06,1.29511E-06,1.30967E-06,1.32426E-06,1.33889E-06,               RRTM_SETCOEF.221
     &1.35355E-06,1.36824E-06,1.38296E-06,1.39772E-06,1.41250E-06,               RRTM_SETCOEF.222
     &1.42732E-06,1.44217E-06,1.45704E-06,1.47195E-06,1.48689E-06,               RRTM_SETCOEF.223
     &1.50185E-06,1.51684E-06,1.53186E-06,1.54691E-06,1.56198E-06,               RRTM_SETCOEF.224
     &1.57709E-06,1.59222E-06,1.60737E-06,1.62255E-06,1.63776E-06,               RRTM_SETCOEF.225
     &1.65299E-06,1.66825E-06,1.68352E-06,1.69883E-06,1.71416E-06,               RRTM_SETCOEF.226
     &1.72951E-06,1.74488E-06,1.76028E-06,1.77570E-06,1.79114E-06,               RRTM_SETCOEF.227
     &1.80661E-06,1.82210E-06,1.83760E-06,1.85313E-06,1.86868E-06/               RRTM_SETCOEF.228
      DATA (TOTPLNK(I, 1),I=51,100)/                                             RRTM_SETCOEF.229
     &1.88425E-06,1.89985E-06,1.91546E-06,1.93109E-06,1.94674E-06,               RRTM_SETCOEF.230
     &1.96241E-06,1.97811E-06,1.99381E-06,2.00954E-06,2.02529E-06,               RRTM_SETCOEF.231
     &2.04105E-06,2.05684E-06,2.07264E-06,2.08846E-06,2.10429E-06,               RRTM_SETCOEF.232
     &2.12015E-06,2.13602E-06,2.15190E-06,2.16781E-06,2.18373E-06,               RRTM_SETCOEF.233
     &2.19966E-06,2.21562E-06,2.23159E-06,2.24758E-06,2.26358E-06,               RRTM_SETCOEF.234
     &2.27959E-06,2.29562E-06,2.31167E-06,2.32773E-06,2.34381E-06,               RRTM_SETCOEF.235
     &2.35990E-06,2.37601E-06,2.39212E-06,2.40825E-06,2.42440E-06,               RRTM_SETCOEF.236
     &2.44056E-06,2.45673E-06,2.47292E-06,2.48912E-06,2.50533E-06,               RRTM_SETCOEF.237
     &2.52157E-06,2.53781E-06,2.55406E-06,2.57032E-06,2.58660E-06,               RRTM_SETCOEF.238
     &2.60289E-06,2.61919E-06,2.63550E-06,2.65183E-06,2.66817E-06/               RRTM_SETCOEF.239
      DATA (TOTPLNK(I, 1),I=101,150)/                                            RRTM_SETCOEF.240
     &2.68452E-06,2.70088E-06,2.71726E-06,2.73364E-06,2.75003E-06,               RRTM_SETCOEF.241
     &2.76644E-06,2.78286E-06,2.79929E-06,2.81572E-06,2.83218E-06,               RRTM_SETCOEF.242
     &2.84864E-06,2.86510E-06,2.88159E-06,2.89807E-06,2.91458E-06,               RRTM_SETCOEF.243
     &2.93109E-06,2.94762E-06,2.96415E-06,2.98068E-06,2.99724E-06,               RRTM_SETCOEF.244
     &3.01379E-06,3.03036E-06,3.04693E-06,3.06353E-06,3.08013E-06,               RRTM_SETCOEF.245
     &3.09674E-06,3.11335E-06,3.12998E-06,3.14661E-06,3.16324E-06,               RRTM_SETCOEF.246
     &3.17989E-06,3.19656E-06,3.21323E-06,3.22991E-06,3.24658E-06,               RRTM_SETCOEF.247
     &3.26328E-06,3.27998E-06,3.29669E-06,3.31341E-06,3.33013E-06,               RRTM_SETCOEF.248
     &3.34686E-06,3.36360E-06,3.38034E-06,3.39709E-06,3.41387E-06,               RRTM_SETCOEF.249
     &3.43063E-06,3.44742E-06,3.46420E-06,3.48099E-06,3.49779E-06/               RRTM_SETCOEF.250
      DATA (TOTPLNK(I, 1),I=151,181)/                                            RRTM_SETCOEF.251
     &3.51461E-06,3.53141E-06,3.54824E-06,3.56506E-06,3.58191E-06,               RRTM_SETCOEF.252
     &3.59875E-06,3.61559E-06,3.63244E-06,3.64931E-06,3.66617E-06,               RRTM_SETCOEF.253
     &3.68305E-06,3.69992E-06,3.71682E-06,3.73372E-06,3.75061E-06,               RRTM_SETCOEF.254
     &3.76753E-06,3.78443E-06,3.80136E-06,3.81829E-06,3.83522E-06,               RRTM_SETCOEF.255
     &3.85215E-06,3.86910E-06,3.88605E-06,3.90301E-06,3.91997E-06,               RRTM_SETCOEF.256
     &3.93694E-06,3.95390E-06,3.97087E-06,3.98788E-06,4.00485E-06,               RRTM_SETCOEF.257
     &4.02187E-06/                                                               RRTM_SETCOEF.258
      DATA (TOTPLNK(I, 2),I=1,50)/                                               RRTM_SETCOEF.259
     &2.13441E-06,2.18076E-06,2.22758E-06,2.27489E-06,2.32268E-06,               RRTM_SETCOEF.260
     &2.37093E-06,2.41966E-06,2.46886E-06,2.51852E-06,2.56864E-06,               RRTM_SETCOEF.261
     &2.61922E-06,2.67026E-06,2.72175E-06,2.77370E-06,2.82609E-06,               RRTM_SETCOEF.262
     &2.87893E-06,2.93221E-06,2.98593E-06,3.04008E-06,3.09468E-06,               RRTM_SETCOEF.263
     &3.14970E-06,3.20515E-06,3.26103E-06,3.31732E-06,3.37404E-06,               RRTM_SETCOEF.264
     &3.43118E-06,3.48873E-06,3.54669E-06,3.60506E-06,3.66383E-06,               RRTM_SETCOEF.265
     &3.72301E-06,3.78259E-06,3.84256E-06,3.90293E-06,3.96368E-06,               RRTM_SETCOEF.266
     &4.02483E-06,4.08636E-06,4.14828E-06,4.21057E-06,4.27324E-06,               RRTM_SETCOEF.267
     &4.33629E-06,4.39971E-06,4.46350E-06,4.52765E-06,4.59217E-06,               RRTM_SETCOEF.268
     &4.65705E-06,4.72228E-06,4.78787E-06,4.85382E-06,4.92011E-06/               RRTM_SETCOEF.269
      DATA (TOTPLNK(I, 2),I=51,100)/                                             RRTM_SETCOEF.270
     &4.98675E-06,5.05374E-06,5.12106E-06,5.18873E-06,5.25674E-06,               RRTM_SETCOEF.271
     &5.32507E-06,5.39374E-06,5.46274E-06,5.53207E-06,5.60172E-06,               RRTM_SETCOEF.272
     &5.67169E-06,5.74198E-06,5.81259E-06,5.88352E-06,5.95475E-06,               RRTM_SETCOEF.273
     &6.02629E-06,6.09815E-06,6.17030E-06,6.24276E-06,6.31552E-06,               RRTM_SETCOEF.274
     &6.38858E-06,6.46192E-06,6.53557E-06,6.60950E-06,6.68373E-06,               RRTM_SETCOEF.275
     &6.75824E-06,6.83303E-06,6.90810E-06,6.98346E-06,7.05909E-06,               RRTM_SETCOEF.276
     &7.13500E-06,7.21117E-06,7.28763E-06,7.36435E-06,7.44134E-06,               RRTM_SETCOEF.277
     &7.51859E-06,7.59611E-06,7.67388E-06,7.75192E-06,7.83021E-06,               RRTM_SETCOEF.278
     &7.90875E-06,7.98755E-06,8.06660E-06,8.14589E-06,8.22544E-06,               RRTM_SETCOEF.279
     &8.30522E-06,8.38526E-06,8.46553E-06,8.54604E-06,8.62679E-06/               RRTM_SETCOEF.280
      DATA (TOTPLNK(I, 2),I=101,150)/                                            RRTM_SETCOEF.281
     &8.70777E-06,8.78899E-06,8.87043E-06,8.95211E-06,9.03402E-06,               RRTM_SETCOEF.282
     &9.11616E-06,9.19852E-06,9.28109E-06,9.36390E-06,9.44692E-06,               RRTM_SETCOEF.283
     &9.53015E-06,9.61361E-06,9.69729E-06,9.78117E-06,9.86526E-06,               RRTM_SETCOEF.284
     &9.94957E-06,1.00341E-05,1.01188E-05,1.02037E-05,1.02888E-05,               RRTM_SETCOEF.285
     &1.03742E-05,1.04597E-05,1.05454E-05,1.06313E-05,1.07175E-05,               RRTM_SETCOEF.286
     &1.08038E-05,1.08903E-05,1.09770E-05,1.10639E-05,1.11509E-05,               RRTM_SETCOEF.287
     &1.12382E-05,1.13257E-05,1.14133E-05,1.15011E-05,1.15891E-05,               RRTM_SETCOEF.288
     &1.16773E-05,1.17656E-05,1.18542E-05,1.19429E-05,1.20317E-05,               RRTM_SETCOEF.289
     &1.21208E-05,1.22100E-05,1.22994E-05,1.23890E-05,1.24787E-05,               RRTM_SETCOEF.290
     &1.25686E-05,1.26587E-05,1.27489E-05,1.28393E-05,1.29299E-05/               RRTM_SETCOEF.291
      DATA (TOTPLNK(I, 2),I=151,181)/                                            RRTM_SETCOEF.292
     &1.30206E-05,1.31115E-05,1.32025E-05,1.32937E-05,1.33850E-05,               RRTM_SETCOEF.293
     &1.34765E-05,1.35682E-05,1.36600E-05,1.37520E-05,1.38441E-05,               RRTM_SETCOEF.294
     &1.39364E-05,1.40288E-05,1.41213E-05,1.42140E-05,1.43069E-05,               RRTM_SETCOEF.295
     &1.43999E-05,1.44930E-05,1.45863E-05,1.46797E-05,1.47733E-05,               RRTM_SETCOEF.296
     &1.48670E-05,1.49608E-05,1.50548E-05,1.51489E-05,1.52431E-05,               RRTM_SETCOEF.297
     &1.53375E-05,1.54320E-05,1.55267E-05,1.56214E-05,1.57164E-05,               RRTM_SETCOEF.298
     &1.58114E-05/                                                               RRTM_SETCOEF.299
      DATA (TOTPLNK(I, 3),I=1,50)/                                               RRTM_SETCOEF.300
     &1.34822E-06,1.39134E-06,1.43530E-06,1.48010E-06,1.52574E-06,               RRTM_SETCOEF.301
     &1.57222E-06,1.61956E-06,1.66774E-06,1.71678E-06,1.76666E-06,               RRTM_SETCOEF.302
     &1.81741E-06,1.86901E-06,1.92147E-06,1.97479E-06,2.02898E-06,               RRTM_SETCOEF.303
     &2.08402E-06,2.13993E-06,2.19671E-06,2.25435E-06,2.31285E-06,               RRTM_SETCOEF.304
     &2.37222E-06,2.43246E-06,2.49356E-06,2.55553E-06,2.61837E-06,               RRTM_SETCOEF.305
     &2.68207E-06,2.74664E-06,2.81207E-06,2.87837E-06,2.94554E-06,               RRTM_SETCOEF.306
     &3.01356E-06,3.08245E-06,3.15221E-06,3.22282E-06,3.29429E-06,               RRTM_SETCOEF.307
     &3.36662E-06,3.43982E-06,3.51386E-06,3.58876E-06,3.66451E-06,               RRTM_SETCOEF.308
     &3.74112E-06,3.81857E-06,3.89688E-06,3.97602E-06,4.05601E-06,               RRTM_SETCOEF.309
     &4.13685E-06,4.21852E-06,4.30104E-06,4.38438E-06,4.46857E-06/               RRTM_SETCOEF.310
      DATA (TOTPLNK(I, 3),I=51,100)/                                             RRTM_SETCOEF.311
     &4.55358E-06,4.63943E-06,4.72610E-06,4.81359E-06,4.90191E-06,               RRTM_SETCOEF.312
     &4.99105E-06,5.08100E-06,5.17176E-06,5.26335E-06,5.35573E-06,               RRTM_SETCOEF.313
     &5.44892E-06,5.54292E-06,5.63772E-06,5.73331E-06,5.82970E-06,               RRTM_SETCOEF.314
     &5.92688E-06,6.02485E-06,6.12360E-06,6.22314E-06,6.32346E-06,               RRTM_SETCOEF.315
     &6.42455E-06,6.52641E-06,6.62906E-06,6.73247E-06,6.83664E-06,               RRTM_SETCOEF.316
     &6.94156E-06,7.04725E-06,7.15370E-06,7.26089E-06,7.36883E-06,               RRTM_SETCOEF.317
     &7.47752E-06,7.58695E-06,7.69712E-06,7.80801E-06,7.91965E-06,               RRTM_SETCOEF.318
     &8.03201E-06,8.14510E-06,8.25891E-06,8.37343E-06,8.48867E-06,               RRTM_SETCOEF.319
     &8.60463E-06,8.72128E-06,8.83865E-06,8.95672E-06,9.07548E-06,               RRTM_SETCOEF.320
     &9.19495E-06,9.31510E-06,9.43594E-06,9.55745E-06,9.67966E-06/               RRTM_SETCOEF.321
      DATA (TOTPLNK(I, 3),I=101,150)/                                            RRTM_SETCOEF.322
     &9.80254E-06,9.92609E-06,1.00503E-05,1.01752E-05,1.03008E-05,               RRTM_SETCOEF.323
     &1.04270E-05,1.05539E-05,1.06814E-05,1.08096E-05,1.09384E-05,               RRTM_SETCOEF.324
     &1.10679E-05,1.11980E-05,1.13288E-05,1.14601E-05,1.15922E-05,               RRTM_SETCOEF.325
     &1.17248E-05,1.18581E-05,1.19920E-05,1.21265E-05,1.22616E-05,               RRTM_SETCOEF.326
     &1.23973E-05,1.25337E-05,1.26706E-05,1.28081E-05,1.29463E-05,               RRTM_SETCOEF.327
     &1.30850E-05,1.32243E-05,1.33642E-05,1.35047E-05,1.36458E-05,               RRTM_SETCOEF.328
     &1.37875E-05,1.39297E-05,1.40725E-05,1.42159E-05,1.43598E-05,               RRTM_SETCOEF.329
     &1.45044E-05,1.46494E-05,1.47950E-05,1.49412E-05,1.50879E-05,               RRTM_SETCOEF.330
     &1.52352E-05,1.53830E-05,1.55314E-05,1.56803E-05,1.58297E-05,               RRTM_SETCOEF.331
     &1.59797E-05,1.61302E-05,1.62812E-05,1.64327E-05,1.65848E-05/               RRTM_SETCOEF.332
      DATA (TOTPLNK(I, 3),I=151,181)/                                            RRTM_SETCOEF.333
     &1.67374E-05,1.68904E-05,1.70441E-05,1.71982E-05,1.73528E-05,               RRTM_SETCOEF.334
     &1.75079E-05,1.76635E-05,1.78197E-05,1.79763E-05,1.81334E-05,               RRTM_SETCOEF.335
     &1.82910E-05,1.84491E-05,1.86076E-05,1.87667E-05,1.89262E-05,               RRTM_SETCOEF.336
     &1.90862E-05,1.92467E-05,1.94076E-05,1.95690E-05,1.97309E-05,               RRTM_SETCOEF.337
     &1.98932E-05,2.00560E-05,2.02193E-05,2.03830E-05,2.05472E-05,               RRTM_SETCOEF.338
     &2.07118E-05,2.08768E-05,2.10423E-05,2.12083E-05,2.13747E-05,               RRTM_SETCOEF.339
     &2.15414E-05/                                                               RRTM_SETCOEF.340
      DATA (TOTPLNK(I, 4),I=1,50)/                                               RRTM_SETCOEF.341
     &8.90528E-07,9.24222E-07,9.58757E-07,9.94141E-07,1.03038E-06,               RRTM_SETCOEF.342
     &1.06748E-06,1.10545E-06,1.14430E-06,1.18403E-06,1.22465E-06,               RRTM_SETCOEF.343
     &1.26618E-06,1.30860E-06,1.35193E-06,1.39619E-06,1.44136E-06,               RRTM_SETCOEF.344
     &1.48746E-06,1.53449E-06,1.58246E-06,1.63138E-06,1.68124E-06,               RRTM_SETCOEF.345
     &1.73206E-06,1.78383E-06,1.83657E-06,1.89028E-06,1.94495E-06,               RRTM_SETCOEF.346
     &2.00060E-06,2.05724E-06,2.11485E-06,2.17344E-06,2.23303E-06,               RRTM_SETCOEF.347
     &2.29361E-06,2.35519E-06,2.41777E-06,2.48134E-06,2.54592E-06,               RRTM_SETCOEF.348
     &2.61151E-06,2.67810E-06,2.74571E-06,2.81433E-06,2.88396E-06,               RRTM_SETCOEF.349
     &2.95461E-06,3.02628E-06,3.09896E-06,3.17267E-06,3.24741E-06,               RRTM_SETCOEF.350
     &3.32316E-06,3.39994E-06,3.47774E-06,3.55657E-06,3.63642E-06/               RRTM_SETCOEF.351
      DATA (TOTPLNK(I, 4),I=51,100)/                                             RRTM_SETCOEF.352
     &3.71731E-06,3.79922E-06,3.88216E-06,3.96612E-06,4.05112E-06,               RRTM_SETCOEF.353
     &4.13714E-06,4.22419E-06,4.31227E-06,4.40137E-06,4.49151E-06,               RRTM_SETCOEF.354
     &4.58266E-06,4.67485E-06,4.76806E-06,4.86229E-06,4.95754E-06,               RRTM_SETCOEF.355
     &5.05383E-06,5.15113E-06,5.24946E-06,5.34879E-06,5.44916E-06,               RRTM_SETCOEF.356
     &5.55053E-06,5.65292E-06,5.75632E-06,5.86073E-06,5.96616E-06,               RRTM_SETCOEF.357
     &6.07260E-06,6.18003E-06,6.28848E-06,6.39794E-06,6.50838E-06,               RRTM_SETCOEF.358
     &6.61983E-06,6.73229E-06,6.84573E-06,6.96016E-06,7.07559E-06,               RRTM_SETCOEF.359
     &7.19200E-06,7.30940E-06,7.42779E-06,7.54715E-06,7.66749E-06,               RRTM_SETCOEF.360
     &7.78882E-06,7.91110E-06,8.03436E-06,8.15859E-06,8.28379E-06,               RRTM_SETCOEF.361
     &8.40994E-06,8.53706E-06,8.66515E-06,8.79418E-06,8.92416E-06/               RRTM_SETCOEF.362
      DATA (TOTPLNK(I, 4),I=101,150)/                                            RRTM_SETCOEF.363
     &9.05510E-06,9.18697E-06,9.31979E-06,9.45356E-06,9.58826E-06,               RRTM_SETCOEF.364
     &9.72389E-06,9.86046E-06,9.99793E-06,1.01364E-05,1.02757E-05,               RRTM_SETCOEF.365
     &1.04159E-05,1.05571E-05,1.06992E-05,1.08422E-05,1.09861E-05,               RRTM_SETCOEF.366
     &1.11309E-05,1.12766E-05,1.14232E-05,1.15707E-05,1.17190E-05,               RRTM_SETCOEF.367
     &1.18683E-05,1.20184E-05,1.21695E-05,1.23214E-05,1.24741E-05,               RRTM_SETCOEF.368
     &1.26277E-05,1.27822E-05,1.29376E-05,1.30939E-05,1.32509E-05,               RRTM_SETCOEF.369
     &1.34088E-05,1.35676E-05,1.37273E-05,1.38877E-05,1.40490E-05,               RRTM_SETCOEF.370
     &1.42112E-05,1.43742E-05,1.45380E-05,1.47026E-05,1.48680E-05,               RRTM_SETCOEF.371
     &1.50343E-05,1.52014E-05,1.53692E-05,1.55379E-05,1.57074E-05,               RRTM_SETCOEF.372
     &1.58778E-05,1.60488E-05,1.62207E-05,1.63934E-05,1.65669E-05/               RRTM_SETCOEF.373
      DATA (TOTPLNK(I, 4),I=151,181)/                                            RRTM_SETCOEF.374
     &1.67411E-05,1.69162E-05,1.70920E-05,1.72685E-05,1.74459E-05,               RRTM_SETCOEF.375
     &1.76240E-05,1.78029E-05,1.79825E-05,1.81629E-05,1.83440E-05,               RRTM_SETCOEF.376
     &1.85259E-05,1.87086E-05,1.88919E-05,1.90760E-05,1.92609E-05,               RRTM_SETCOEF.377
     &1.94465E-05,1.96327E-05,1.98199E-05,2.00076E-05,2.01961E-05,               RRTM_SETCOEF.378
     &2.03853E-05,2.05752E-05,2.07658E-05,2.09571E-05,2.11491E-05,               RRTM_SETCOEF.379
     &2.13418E-05,2.15352E-05,2.17294E-05,2.19241E-05,2.21196E-05,               RRTM_SETCOEF.380
     &2.23158E-05/                                                               RRTM_SETCOEF.381
      DATA (TOTPLNK(I, 5),I=1,50)/                                               RRTM_SETCOEF.382
     &5.70230E-07,5.94788E-07,6.20085E-07,6.46130E-07,6.72936E-07,               RRTM_SETCOEF.383
     &7.00512E-07,7.28869E-07,7.58019E-07,7.87971E-07,8.18734E-07,               RRTM_SETCOEF.384
     &8.50320E-07,8.82738E-07,9.15999E-07,9.50110E-07,9.85084E-07,               RRTM_SETCOEF.385
     &1.02093E-06,1.05765E-06,1.09527E-06,1.13378E-06,1.17320E-06,               RRTM_SETCOEF.386
     &1.21353E-06,1.25479E-06,1.29698E-06,1.34011E-06,1.38419E-06,               RRTM_SETCOEF.387
     &1.42923E-06,1.47523E-06,1.52221E-06,1.57016E-06,1.61910E-06,               RRTM_SETCOEF.388
     &1.66904E-06,1.71997E-06,1.77192E-06,1.82488E-06,1.87886E-06,               RRTM_SETCOEF.389
     &1.93387E-06,1.98991E-06,2.04699E-06,2.10512E-06,2.16430E-06,               RRTM_SETCOEF.390
     &2.22454E-06,2.28584E-06,2.34821E-06,2.41166E-06,2.47618E-06,               RRTM_SETCOEF.391
     &2.54178E-06,2.60847E-06,2.67626E-06,2.74514E-06,2.81512E-06/               RRTM_SETCOEF.392
      DATA (TOTPLNK(I, 5),I=51,100)/                                             RRTM_SETCOEF.393
     &2.88621E-06,2.95841E-06,3.03172E-06,3.10615E-06,3.18170E-06,               RRTM_SETCOEF.394
     &3.25838E-06,3.33618E-06,3.41511E-06,3.49518E-06,3.57639E-06,               RRTM_SETCOEF.395
     &3.65873E-06,3.74221E-06,3.82684E-06,3.91262E-06,3.99955E-06,               RRTM_SETCOEF.396
     &4.08763E-06,4.17686E-06,4.26725E-06,4.35880E-06,4.45150E-06,               RRTM_SETCOEF.397
     &4.54537E-06,4.64039E-06,4.73659E-06,4.83394E-06,4.93246E-06,               RRTM_SETCOEF.398
     &5.03215E-06,5.13301E-06,5.23504E-06,5.33823E-06,5.44260E-06,               RRTM_SETCOEF.399
     &5.54814E-06,5.65484E-06,5.76272E-06,5.87177E-06,5.98199E-06,               RRTM_SETCOEF.400
     &6.09339E-06,6.20596E-06,6.31969E-06,6.43460E-06,6.55068E-06,               RRTM_SETCOEF.401
     &6.66793E-06,6.78636E-06,6.90595E-06,7.02670E-06,7.14863E-06,               RRTM_SETCOEF.402
     &7.27173E-06,7.39599E-06,7.52142E-06,7.64802E-06,7.77577E-06/               RRTM_SETCOEF.403
      DATA (TOTPLNK(I, 5),I=101,150)/                                            RRTM_SETCOEF.404
     &7.90469E-06,8.03477E-06,8.16601E-06,8.29841E-06,8.43198E-06,               RRTM_SETCOEF.405
     &8.56669E-06,8.70256E-06,8.83957E-06,8.97775E-06,9.11706E-06,               RRTM_SETCOEF.406
     &9.25753E-06,9.39915E-06,9.54190E-06,9.68580E-06,9.83085E-06,               RRTM_SETCOEF.407
     &9.97704E-06,1.01243E-05,1.02728E-05,1.04224E-05,1.05731E-05,               RRTM_SETCOEF.408
     &1.07249E-05,1.08779E-05,1.10320E-05,1.11872E-05,1.13435E-05,               RRTM_SETCOEF.409
     &1.15009E-05,1.16595E-05,1.18191E-05,1.19799E-05,1.21418E-05,               RRTM_SETCOEF.410
     &1.23048E-05,1.24688E-05,1.26340E-05,1.28003E-05,1.29676E-05,               RRTM_SETCOEF.411
     &1.31361E-05,1.33056E-05,1.34762E-05,1.36479E-05,1.38207E-05,               RRTM_SETCOEF.412
     &1.39945E-05,1.41694E-05,1.43454E-05,1.45225E-05,1.47006E-05,               RRTM_SETCOEF.413
     &1.48797E-05,1.50600E-05,1.52413E-05,1.54236E-05,1.56070E-05/               RRTM_SETCOEF.414
      DATA (TOTPLNK(I, 5),I=151,181)/                                            RRTM_SETCOEF.415
     &1.57914E-05,1.59768E-05,1.61633E-05,1.63509E-05,1.65394E-05,               RRTM_SETCOEF.416
     &1.67290E-05,1.69197E-05,1.71113E-05,1.73040E-05,1.74976E-05,               RRTM_SETCOEF.417
     &1.76923E-05,1.78880E-05,1.80847E-05,1.82824E-05,1.84811E-05,               RRTM_SETCOEF.418
     &1.86808E-05,1.88814E-05,1.90831E-05,1.92857E-05,1.94894E-05,               RRTM_SETCOEF.419
     &1.96940E-05,1.98996E-05,2.01061E-05,2.03136E-05,2.05221E-05,               RRTM_SETCOEF.420
     &2.07316E-05,2.09420E-05,2.11533E-05,2.13657E-05,2.15789E-05,               RRTM_SETCOEF.421
     &2.17931E-05/                                                               RRTM_SETCOEF.422
      DATA (TOTPLNK(I, 6),I=1,50)/                                               RRTM_SETCOEF.423
     &2.73493E-07,2.87408E-07,3.01848E-07,3.16825E-07,3.32352E-07,               RRTM_SETCOEF.424
     &3.48439E-07,3.65100E-07,3.82346E-07,4.00189E-07,4.18641E-07,               RRTM_SETCOEF.425
     &4.37715E-07,4.57422E-07,4.77774E-07,4.98784E-07,5.20464E-07,               RRTM_SETCOEF.426
     &5.42824E-07,5.65879E-07,5.89638E-07,6.14115E-07,6.39320E-07,               RRTM_SETCOEF.427
     &6.65266E-07,6.91965E-07,7.19427E-07,7.47666E-07,7.76691E-07,               RRTM_SETCOEF.428
     &8.06516E-07,8.37151E-07,8.68607E-07,9.00896E-07,9.34029E-07,               RRTM_SETCOEF.429
     &9.68018E-07,1.00287E-06,1.03860E-06,1.07522E-06,1.11274E-06,               RRTM_SETCOEF.430
     &1.15117E-06,1.19052E-06,1.23079E-06,1.27201E-06,1.31418E-06,               RRTM_SETCOEF.431
     &1.35731E-06,1.40141E-06,1.44650E-06,1.49257E-06,1.53965E-06,               RRTM_SETCOEF.432
     &1.58773E-06,1.63684E-06,1.68697E-06,1.73815E-06,1.79037E-06/               RRTM_SETCOEF.433
      DATA (TOTPLNK(I, 6),I=51,100)/                                             RRTM_SETCOEF.434
     &1.84365E-06,1.89799E-06,1.95341E-06,2.00991E-06,2.06750E-06,               RRTM_SETCOEF.435
     &2.12619E-06,2.18599E-06,2.24691E-06,2.30895E-06,2.37212E-06,               RRTM_SETCOEF.436
     &2.43643E-06,2.50189E-06,2.56851E-06,2.63628E-06,2.70523E-06,               RRTM_SETCOEF.437
     &2.77536E-06,2.84666E-06,2.91916E-06,2.99286E-06,3.06776E-06,               RRTM_SETCOEF.438
     &3.14387E-06,3.22120E-06,3.29975E-06,3.37953E-06,3.46054E-06,               RRTM_SETCOEF.439
     &3.54280E-06,3.62630E-06,3.71105E-06,3.79707E-06,3.88434E-06,               RRTM_SETCOEF.440
     &3.97288E-06,4.06270E-06,4.15380E-06,4.24617E-06,4.33984E-06,               RRTM_SETCOEF.441
     &4.43479E-06,4.53104E-06,4.62860E-06,4.72746E-06,4.82763E-06,               RRTM_SETCOEF.442
     &4.92911E-06,5.03191E-06,5.13603E-06,5.24147E-06,5.34824E-06,               RRTM_SETCOEF.443
     &5.45634E-06,5.56578E-06,5.67656E-06,5.78867E-06,5.90213E-06/               RRTM_SETCOEF.444
      DATA (TOTPLNK(I, 6),I=101,150)/                                            RRTM_SETCOEF.445
     &6.01694E-06,6.13309E-06,6.25060E-06,6.36947E-06,6.48968E-06,               RRTM_SETCOEF.446
     &6.61126E-06,6.73420E-06,6.85850E-06,6.98417E-06,7.11120E-06,               RRTM_SETCOEF.447
     &7.23961E-06,7.36938E-06,7.50053E-06,7.63305E-06,7.76694E-06,               RRTM_SETCOEF.448
     &7.90221E-06,8.03887E-06,8.17690E-06,8.31632E-06,8.45710E-06,               RRTM_SETCOEF.449
     &8.59928E-06,8.74282E-06,8.88776E-06,9.03409E-06,9.18179E-06,               RRTM_SETCOEF.450
     &9.33088E-06,9.48136E-06,9.63323E-06,9.78648E-06,9.94111E-06,               RRTM_SETCOEF.451
     &1.00971E-05,1.02545E-05,1.04133E-05,1.05735E-05,1.07351E-05,               RRTM_SETCOEF.452
     &1.08980E-05,1.10624E-05,1.12281E-05,1.13952E-05,1.15637E-05,               RRTM_SETCOEF.453
     &1.17335E-05,1.19048E-05,1.20774E-05,1.22514E-05,1.24268E-05,               RRTM_SETCOEF.454
     &1.26036E-05,1.27817E-05,1.29612E-05,1.31421E-05,1.33244E-05/               RRTM_SETCOEF.455
      DATA (TOTPLNK(I, 6),I=151,181)/                                            RRTM_SETCOEF.456
     &1.35080E-05,1.36930E-05,1.38794E-05,1.40672E-05,1.42563E-05,               RRTM_SETCOEF.457
     &1.44468E-05,1.46386E-05,1.48318E-05,1.50264E-05,1.52223E-05,               RRTM_SETCOEF.458
     &1.54196E-05,1.56182E-05,1.58182E-05,1.60196E-05,1.62223E-05,               RRTM_SETCOEF.459
     &1.64263E-05,1.66317E-05,1.68384E-05,1.70465E-05,1.72559E-05,               RRTM_SETCOEF.460
     &1.74666E-05,1.76787E-05,1.78921E-05,1.81069E-05,1.83230E-05,               RRTM_SETCOEF.461
     &1.85404E-05,1.87591E-05,1.89791E-05,1.92005E-05,1.94232E-05,               RRTM_SETCOEF.462
     &1.96471E-05/                                                               RRTM_SETCOEF.463
      DATA (TOTPLNK(I, 7),I=1,50)/                                               RRTM_SETCOEF.464
     &1.25349E-07,1.32735E-07,1.40458E-07,1.48527E-07,1.56954E-07,               RRTM_SETCOEF.465
     &1.65748E-07,1.74920E-07,1.84481E-07,1.94443E-07,2.04814E-07,               RRTM_SETCOEF.466
     &2.15608E-07,2.26835E-07,2.38507E-07,2.50634E-07,2.63229E-07,               RRTM_SETCOEF.467
     &2.76301E-07,2.89864E-07,3.03930E-07,3.18508E-07,3.33612E-07,               RRTM_SETCOEF.468
     &3.49253E-07,3.65443E-07,3.82195E-07,3.99519E-07,4.17428E-07,               RRTM_SETCOEF.469
     &4.35934E-07,4.55050E-07,4.74785E-07,4.95155E-07,5.16170E-07,               RRTM_SETCOEF.470
     &5.37844E-07,5.60186E-07,5.83211E-07,6.06929E-07,6.31355E-07,               RRTM_SETCOEF.471
     &6.56498E-07,6.82373E-07,7.08990E-07,7.36362E-07,7.64501E-07,               RRTM_SETCOEF.472
     &7.93420E-07,8.23130E-07,8.53643E-07,8.84971E-07,9.17128E-07,               RRTM_SETCOEF.473
     &9.50123E-07,9.83969E-07,1.01868E-06,1.05426E-06,1.09073E-06/               RRTM_SETCOEF.474
      DATA (TOTPLNK(I, 7),I=51,100)/                                             RRTM_SETCOEF.475
     &1.12810E-06,1.16638E-06,1.20558E-06,1.24572E-06,1.28680E-06,               RRTM_SETCOEF.476
     &1.32883E-06,1.37183E-06,1.41581E-06,1.46078E-06,1.50675E-06,               RRTM_SETCOEF.477
     &1.55374E-06,1.60174E-06,1.65078E-06,1.70087E-06,1.75200E-06,               RRTM_SETCOEF.478
     &1.80421E-06,1.85749E-06,1.91186E-06,1.96732E-06,2.02389E-06,               RRTM_SETCOEF.479
     &2.08159E-06,2.14040E-06,2.20035E-06,2.26146E-06,2.32372E-06,               RRTM_SETCOEF.480
     &2.38714E-06,2.45174E-06,2.51753E-06,2.58451E-06,2.65270E-06,               RRTM_SETCOEF.481
     &2.72210E-06,2.79272E-06,2.86457E-06,2.93767E-06,3.01201E-06,               RRTM_SETCOEF.482
     &3.08761E-06,3.16448E-06,3.24261E-06,3.32204E-06,3.40275E-06,               RRTM_SETCOEF.483
     &3.48476E-06,3.56808E-06,3.65271E-06,3.73866E-06,3.82595E-06,               RRTM_SETCOEF.484
     &3.91456E-06,4.00453E-06,4.09584E-06,4.18851E-06,4.28254E-06/               RRTM_SETCOEF.485
      DATA (TOTPLNK(I, 7),I=101,150)/                                            RRTM_SETCOEF.486
     &4.37796E-06,4.47475E-06,4.57293E-06,4.67249E-06,4.77346E-06,               RRTM_SETCOEF.487
     &4.87583E-06,4.97961E-06,5.08481E-06,5.19143E-06,5.29948E-06,               RRTM_SETCOEF.488
     &5.40896E-06,5.51989E-06,5.63226E-06,5.74608E-06,5.86136E-06,               RRTM_SETCOEF.489
     &5.97810E-06,6.09631E-06,6.21597E-06,6.33713E-06,6.45976E-06,               RRTM_SETCOEF.490
     &6.58388E-06,6.70950E-06,6.83661E-06,6.96521E-06,7.09531E-06,               RRTM_SETCOEF.491
     &7.22692E-06,7.36005E-06,7.49468E-06,7.63084E-06,7.76851E-06,               RRTM_SETCOEF.492
     &7.90773E-06,8.04846E-06,8.19072E-06,8.33452E-06,8.47985E-06,               RRTM_SETCOEF.493
     &8.62674E-06,8.77517E-06,8.92514E-06,9.07666E-06,9.22975E-06,               RRTM_SETCOEF.494
     &9.38437E-06,9.54057E-06,9.69832E-06,9.85762E-06,1.00185E-05,               RRTM_SETCOEF.495
     &1.01810E-05,1.03450E-05,1.05106E-05,1.06777E-05,1.08465E-05/               RRTM_SETCOEF.496
      DATA (TOTPLNK(I, 7),I=151,181)/                                            RRTM_SETCOEF.497
     &1.10168E-05,1.11887E-05,1.13621E-05,1.15372E-05,1.17138E-05,               RRTM_SETCOEF.498
     &1.18920E-05,1.20718E-05,1.22532E-05,1.24362E-05,1.26207E-05,               RRTM_SETCOEF.499
     &1.28069E-05,1.29946E-05,1.31839E-05,1.33749E-05,1.35674E-05,               RRTM_SETCOEF.500
     &1.37615E-05,1.39572E-05,1.41544E-05,1.43533E-05,1.45538E-05,               RRTM_SETCOEF.501
     &1.47558E-05,1.49595E-05,1.51647E-05,1.53716E-05,1.55800E-05,               RRTM_SETCOEF.502
     &1.57900E-05,1.60017E-05,1.62149E-05,1.64296E-05,1.66460E-05,               RRTM_SETCOEF.503
     &1.68640E-05/                                                               RRTM_SETCOEF.504
      DATA (TOTPLNK(I, 8),I=1,50)/                                               RRTM_SETCOEF.505
     &6.74445E-08,7.18176E-08,7.64153E-08,8.12456E-08,8.63170E-08,               RRTM_SETCOEF.506
     &9.16378E-08,9.72168E-08,1.03063E-07,1.09184E-07,1.15591E-07,               RRTM_SETCOEF.507
     &1.22292E-07,1.29296E-07,1.36613E-07,1.44253E-07,1.52226E-07,               RRTM_SETCOEF.508
     &1.60540E-07,1.69207E-07,1.78236E-07,1.87637E-07,1.97421E-07,               RRTM_SETCOEF.509
     &2.07599E-07,2.18181E-07,2.29177E-07,2.40598E-07,2.52456E-07,               RRTM_SETCOEF.510
     &2.64761E-07,2.77523E-07,2.90755E-07,3.04468E-07,3.18673E-07,               RRTM_SETCOEF.511
     &3.33381E-07,3.48603E-07,3.64352E-07,3.80638E-07,3.97474E-07,               RRTM_SETCOEF.512
     &4.14871E-07,4.32841E-07,4.51395E-07,4.70547E-07,4.90306E-07,               RRTM_SETCOEF.513
     &5.10687E-07,5.31699E-07,5.53357E-07,5.75670E-07,5.98652E-07,               RRTM_SETCOEF.514
     &6.22315E-07,6.46672E-07,6.71731E-07,6.97511E-07,7.24018E-07/               RRTM_SETCOEF.515
      DATA (TOTPLNK(I, 8),I=51,100)/                                             RRTM_SETCOEF.516
     &7.51266E-07,7.79269E-07,8.08038E-07,8.37584E-07,8.67922E-07,               RRTM_SETCOEF.517
     &8.99061E-07,9.31016E-07,9.63797E-07,9.97417E-07,1.03189E-06,               RRTM_SETCOEF.518
     &1.06722E-06,1.10343E-06,1.14053E-06,1.17853E-06,1.21743E-06,               RRTM_SETCOEF.519
     &1.25726E-06,1.29803E-06,1.33974E-06,1.38241E-06,1.42606E-06,               RRTM_SETCOEF.520
     &1.47068E-06,1.51630E-06,1.56293E-06,1.61056E-06,1.65924E-06,               RRTM_SETCOEF.521
     &1.70894E-06,1.75971E-06,1.81153E-06,1.86443E-06,1.91841E-06,               RRTM_SETCOEF.522
     &1.97350E-06,2.02968E-06,2.08699E-06,2.14543E-06,2.20500E-06,               RRTM_SETCOEF.523
     &2.26573E-06,2.32762E-06,2.39068E-06,2.45492E-06,2.52036E-06,               RRTM_SETCOEF.524
     &2.58700E-06,2.65485E-06,2.72393E-06,2.79424E-06,2.86580E-06,               RRTM_SETCOEF.525
     &2.93861E-06,3.01269E-06,3.08803E-06,3.16467E-06,3.24259E-06/               RRTM_SETCOEF.526
      DATA (TOTPLNK(I, 8),I=101,150)/                                            RRTM_SETCOEF.527
     &3.32181E-06,3.40235E-06,3.48420E-06,3.56739E-06,3.65192E-06,               RRTM_SETCOEF.528
     &3.73779E-06,3.82502E-06,3.91362E-06,4.00359E-06,4.09494E-06,               RRTM_SETCOEF.529
     &4.18768E-06,4.28182E-06,4.37737E-06,4.47434E-06,4.57273E-06,               RRTM_SETCOEF.530
     &4.67254E-06,4.77380E-06,4.87651E-06,4.98067E-06,5.08630E-06,               RRTM_SETCOEF.531
     &5.19339E-06,5.30196E-06,5.41201E-06,5.52356E-06,5.63660E-06,               RRTM_SETCOEF.532
     &5.75116E-06,5.86722E-06,5.98479E-06,6.10390E-06,6.22453E-06,               RRTM_SETCOEF.533
     &6.34669E-06,6.47042E-06,6.59569E-06,6.72252E-06,6.85090E-06,               RRTM_SETCOEF.534
     &6.98085E-06,7.11238E-06,7.24549E-06,7.38019E-06,7.51646E-06,               RRTM_SETCOEF.535
     &7.65434E-06,7.79382E-06,7.93490E-06,8.07760E-06,8.22192E-06,               RRTM_SETCOEF.536
     &8.36784E-06,8.51540E-06,8.66459E-06,8.81542E-06,8.96786E-06/               RRTM_SETCOEF.537
      DATA (TOTPLNK(I, 8),I=151,181)/                                            RRTM_SETCOEF.538
     &9.12197E-06,9.27772E-06,9.43513E-06,9.59419E-06,9.75490E-06,               RRTM_SETCOEF.539
     &9.91728E-06,1.00813E-05,1.02471E-05,1.04144E-05,1.05835E-05,               RRTM_SETCOEF.540
     &1.07543E-05,1.09267E-05,1.11008E-05,1.12766E-05,1.14541E-05,               RRTM_SETCOEF.541
     &1.16333E-05,1.18142E-05,1.19969E-05,1.21812E-05,1.23672E-05,               RRTM_SETCOEF.542
     &1.25549E-05,1.27443E-05,1.29355E-05,1.31284E-05,1.33229E-05,               RRTM_SETCOEF.543
     &1.35193E-05,1.37173E-05,1.39170E-05,1.41185E-05,1.43217E-05,               RRTM_SETCOEF.544
     &1.45267E-05/                                                               RRTM_SETCOEF.545
      DATA (TOTPLNK(I, 9),I=1,50)/                                               RRTM_SETCOEF.546
     &2.61522E-08,2.80613E-08,3.00838E-08,3.22250E-08,3.44899E-08,               RRTM_SETCOEF.547
     &3.68841E-08,3.94129E-08,4.20820E-08,4.48973E-08,4.78646E-08,               RRTM_SETCOEF.548
     &5.09901E-08,5.42799E-08,5.77405E-08,6.13784E-08,6.52001E-08,               RRTM_SETCOEF.549
     &6.92126E-08,7.34227E-08,7.78375E-08,8.24643E-08,8.73103E-08,               RRTM_SETCOEF.550
     &9.23832E-08,9.76905E-08,1.03240E-07,1.09039E-07,1.15097E-07,               RRTM_SETCOEF.551
     &1.21421E-07,1.28020E-07,1.34902E-07,1.42075E-07,1.49548E-07,               RRTM_SETCOEF.552
     &1.57331E-07,1.65432E-07,1.73860E-07,1.82624E-07,1.91734E-07,               RRTM_SETCOEF.553
     &2.01198E-07,2.11028E-07,2.21231E-07,2.31818E-07,2.42799E-07,               RRTM_SETCOEF.554
     &2.54184E-07,2.65983E-07,2.78205E-07,2.90862E-07,3.03963E-07,               RRTM_SETCOEF.555
     &3.17519E-07,3.31541E-07,3.46039E-07,3.61024E-07,3.76507E-07/               RRTM_SETCOEF.556
      DATA (TOTPLNK(I, 9),I=51,100)/                                             RRTM_SETCOEF.557
     &3.92498E-07,4.09008E-07,4.26050E-07,4.43633E-07,4.61769E-07,               RRTM_SETCOEF.558
     &4.80469E-07,4.99744E-07,5.19606E-07,5.40067E-07,5.61136E-07,               RRTM_SETCOEF.559
     &5.82828E-07,6.05152E-07,6.28120E-07,6.51745E-07,6.76038E-07,               RRTM_SETCOEF.560
     &7.01010E-07,7.26674E-07,7.53041E-07,7.80124E-07,8.07933E-07,               RRTM_SETCOEF.561
     &8.36482E-07,8.65781E-07,8.95845E-07,9.26683E-07,9.58308E-07,               RRTM_SETCOEF.562
     &9.90732E-07,1.02397E-06,1.05803E-06,1.09292E-06,1.12866E-06,               RRTM_SETCOEF.563
     &1.16526E-06,1.20274E-06,1.24109E-06,1.28034E-06,1.32050E-06,               RRTM_SETCOEF.564
     &1.36158E-06,1.40359E-06,1.44655E-06,1.49046E-06,1.53534E-06,               RRTM_SETCOEF.565
     &1.58120E-06,1.62805E-06,1.67591E-06,1.72478E-06,1.77468E-06,               RRTM_SETCOEF.566
     &1.82561E-06,1.87760E-06,1.93066E-06,1.98479E-06,2.04000E-06/               RRTM_SETCOEF.567
      DATA (TOTPLNK(I, 9),I=101,150)/                                            RRTM_SETCOEF.568
     &2.09631E-06,2.15373E-06,2.21228E-06,2.27196E-06,2.33278E-06,               RRTM_SETCOEF.569
     &2.39475E-06,2.45790E-06,2.52222E-06,2.58773E-06,2.65445E-06,               RRTM_SETCOEF.570
     &2.72238E-06,2.79152E-06,2.86191E-06,2.93354E-06,3.00643E-06,               RRTM_SETCOEF.571
     &3.08058E-06,3.15601E-06,3.23273E-06,3.31075E-06,3.39009E-06,               RRTM_SETCOEF.572
     &3.47074E-06,3.55272E-06,3.63605E-06,3.72072E-06,3.80676E-06,               RRTM_SETCOEF.573
     &3.89417E-06,3.98297E-06,4.07315E-06,4.16474E-06,4.25774E-06,               RRTM_SETCOEF.574
     &4.35217E-06,4.44802E-06,4.54532E-06,4.64406E-06,4.74428E-06,               RRTM_SETCOEF.575
     &4.84595E-06,4.94911E-06,5.05376E-06,5.15990E-06,5.26755E-06,               RRTM_SETCOEF.576
     &5.37671E-06,5.48741E-06,5.59963E-06,5.71340E-06,5.82871E-06,               RRTM_SETCOEF.577
     &5.94559E-06,6.06403E-06,6.18404E-06,6.30565E-06,6.42885E-06/               RRTM_SETCOEF.578
      DATA (TOTPLNK(I, 9),I=151,181)/                                            RRTM_SETCOEF.579
     &6.55364E-06,6.68004E-06,6.80806E-06,6.93771E-06,7.06898E-06,               RRTM_SETCOEF.580
     &7.20190E-06,7.33646E-06,7.47267E-06,7.61056E-06,7.75010E-06,               RRTM_SETCOEF.581
     &7.89133E-06,8.03423E-06,8.17884E-06,8.32514E-06,8.47314E-06,               RRTM_SETCOEF.582
     &8.62284E-06,8.77427E-06,8.92743E-06,9.08231E-06,9.23893E-06,               RRTM_SETCOEF.583
     &9.39729E-06,9.55741E-06,9.71927E-06,9.88291E-06,1.00483E-05,               RRTM_SETCOEF.584
     &1.02155E-05,1.03844E-05,1.05552E-05,1.07277E-05,1.09020E-05,               RRTM_SETCOEF.585
     &1.10781E-05/                                                               RRTM_SETCOEF.586
      DATA (TOTPLNK(I,10),I=1,50)/                                               RRTM_SETCOEF.587
     &8.89300E-09,9.63263E-09,1.04235E-08,1.12685E-08,1.21703E-08,               RRTM_SETCOEF.588
     &1.31321E-08,1.41570E-08,1.52482E-08,1.64090E-08,1.76428E-08,               RRTM_SETCOEF.589
     &1.89533E-08,2.03441E-08,2.18190E-08,2.33820E-08,2.50370E-08,               RRTM_SETCOEF.590
     &2.67884E-08,2.86402E-08,3.05969E-08,3.26632E-08,3.48436E-08,               RRTM_SETCOEF.591
     &3.71429E-08,3.95660E-08,4.21179E-08,4.48040E-08,4.76294E-08,               RRTM_SETCOEF.592
     &5.05996E-08,5.37201E-08,5.69966E-08,6.04349E-08,6.40411E-08,               RRTM_SETCOEF.593
     &6.78211E-08,7.17812E-08,7.59276E-08,8.02670E-08,8.48059E-08,               RRTM_SETCOEF.594
     &8.95508E-08,9.45090E-08,9.96873E-08,1.05093E-07,1.10733E-07,               RRTM_SETCOEF.595
     &1.16614E-07,1.22745E-07,1.29133E-07,1.35786E-07,1.42711E-07,               RRTM_SETCOEF.596
     &1.49916E-07,1.57410E-07,1.65202E-07,1.73298E-07,1.81709E-07/               RRTM_SETCOEF.597
      DATA (TOTPLNK(I,10),I=51,100)/                                             RRTM_SETCOEF.598
     &1.90441E-07,1.99505E-07,2.08908E-07,2.18660E-07,2.28770E-07,               RRTM_SETCOEF.599
     &2.39247E-07,2.50101E-07,2.61340E-07,2.72974E-07,2.85013E-07,               RRTM_SETCOEF.600
     &2.97467E-07,3.10345E-07,3.23657E-07,3.37413E-07,3.51623E-07,               RRTM_SETCOEF.601
     &3.66298E-07,3.81448E-07,3.97082E-07,4.13212E-07,4.29848E-07,               RRTM_SETCOEF.602
     &4.47000E-07,4.64680E-07,4.82898E-07,5.01664E-07,5.20991E-07,               RRTM_SETCOEF.603
     &5.40888E-07,5.61369E-07,5.82440E-07,6.04118E-07,6.26410E-07,               RRTM_SETCOEF.604
     &6.49329E-07,6.72887E-07,6.97095E-07,7.21964E-07,7.47506E-07,               RRTM_SETCOEF.605
     &7.73732E-07,8.00655E-07,8.28287E-07,8.56635E-07,8.85717E-07,               RRTM_SETCOEF.606
     &9.15542E-07,9.46122E-07,9.77469E-07,1.00960E-06,1.04251E-06,               RRTM_SETCOEF.607
     &1.07623E-06,1.11077E-06,1.14613E-06,1.18233E-06,1.21939E-06/               RRTM_SETCOEF.608
      DATA (TOTPLNK(I,10),I=101,150)/                                            RRTM_SETCOEF.609
     &1.25730E-06,1.29610E-06,1.33578E-06,1.37636E-06,1.41785E-06,               RRTM_SETCOEF.610
     &1.46027E-06,1.50362E-06,1.54792E-06,1.59319E-06,1.63942E-06,               RRTM_SETCOEF.611
     &1.68665E-06,1.73487E-06,1.78410E-06,1.83435E-06,1.88564E-06,               RRTM_SETCOEF.612
     &1.93797E-06,1.99136E-06,2.04582E-06,2.10137E-06,2.15801E-06,               RRTM_SETCOEF.613
     &2.21576E-06,2.27463E-06,2.33462E-06,2.39577E-06,2.45806E-06,               RRTM_SETCOEF.614
     &2.52153E-06,2.58617E-06,2.65201E-06,2.71905E-06,2.78730E-06,               RRTM_SETCOEF.615
     &2.85678E-06,2.92749E-06,2.99946E-06,3.07269E-06,3.14720E-06,               RRTM_SETCOEF.616
     &3.22299E-06,3.30007E-06,3.37847E-06,3.45818E-06,3.53923E-06,               RRTM_SETCOEF.617
     &3.62161E-06,3.70535E-06,3.79046E-06,3.87695E-06,3.96481E-06,               RRTM_SETCOEF.618
     &4.05409E-06,4.14477E-06,4.23687E-06,4.33040E-06,4.42538E-06/               RRTM_SETCOEF.619
      DATA (TOTPLNK(I,10),I=151,181)/                                            RRTM_SETCOEF.620
     &4.52180E-06,4.61969E-06,4.71905E-06,4.81991E-06,4.92226E-06,               RRTM_SETCOEF.621
     &5.02611E-06,5.13148E-06,5.23839E-06,5.34681E-06,5.45681E-06,               RRTM_SETCOEF.622
     &5.56835E-06,5.68146E-06,5.79614E-06,5.91242E-06,6.03030E-06,               RRTM_SETCOEF.623
     &6.14978E-06,6.27088E-06,6.39360E-06,6.51798E-06,6.64398E-06,               RRTM_SETCOEF.624
     &6.77165E-06,6.90099E-06,7.03198E-06,7.16468E-06,7.29906E-06,               RRTM_SETCOEF.625
     &7.43514E-06,7.57294E-06,7.71244E-06,7.85369E-06,7.99666E-06,               RRTM_SETCOEF.626
     &8.14138E-06/                                                               RRTM_SETCOEF.627
      DATA (TOTPLNK(I,11),I=1,50)/                                               RRTM_SETCOEF.628
     &2.53767E-09,2.77242E-09,3.02564E-09,3.29851E-09,3.59228E-09,               RRTM_SETCOEF.629
     &3.90825E-09,4.24777E-09,4.61227E-09,5.00322E-09,5.42219E-09,               RRTM_SETCOEF.630
     &5.87080E-09,6.35072E-09,6.86370E-09,7.41159E-09,7.99628E-09,               RRTM_SETCOEF.631
     &8.61974E-09,9.28404E-09,9.99130E-09,1.07437E-08,1.15436E-08,               RRTM_SETCOEF.632
     &1.23933E-08,1.32953E-08,1.42522E-08,1.52665E-08,1.63410E-08,               RRTM_SETCOEF.633
     &1.74786E-08,1.86820E-08,1.99542E-08,2.12985E-08,2.27179E-08,               RRTM_SETCOEF.634
     &2.42158E-08,2.57954E-08,2.74604E-08,2.92141E-08,3.10604E-08,               RRTM_SETCOEF.635
     &3.30029E-08,3.50457E-08,3.71925E-08,3.94476E-08,4.18149E-08,               RRTM_SETCOEF.636
     &4.42991E-08,4.69043E-08,4.96352E-08,5.24961E-08,5.54921E-08,               RRTM_SETCOEF.637
     &5.86277E-08,6.19081E-08,6.53381E-08,6.89231E-08,7.26681E-08/               RRTM_SETCOEF.638
      DATA (TOTPLNK(I,11),I=51,100)/                                             RRTM_SETCOEF.639
     &7.65788E-08,8.06604E-08,8.49187E-08,8.93591E-08,9.39879E-08,               RRTM_SETCOEF.640
     &9.88106E-08,1.03834E-07,1.09063E-07,1.14504E-07,1.20165E-07,               RRTM_SETCOEF.641
     &1.26051E-07,1.32169E-07,1.38525E-07,1.45128E-07,1.51982E-07,               RRTM_SETCOEF.642
     &1.59096E-07,1.66477E-07,1.74132E-07,1.82068E-07,1.90292E-07,               RRTM_SETCOEF.643
     &1.98813E-07,2.07638E-07,2.16775E-07,2.26231E-07,2.36015E-07,               RRTM_SETCOEF.644
     &2.46135E-07,2.56599E-07,2.67415E-07,2.78592E-07,2.90137E-07,               RRTM_SETCOEF.645
     &3.02061E-07,3.14371E-07,3.27077E-07,3.40186E-07,3.53710E-07,               RRTM_SETCOEF.646
     &3.67655E-07,3.82031E-07,3.96848E-07,4.12116E-07,4.27842E-07,               RRTM_SETCOEF.647
     &4.44039E-07,4.60713E-07,4.77876E-07,4.95537E-07,5.13706E-07,               RRTM_SETCOEF.648
     &5.32392E-07,5.51608E-07,5.71360E-07,5.91662E-07,6.12521E-07/               RRTM_SETCOEF.649
      DATA (TOTPLNK(I,11),I=101,150)/                                            RRTM_SETCOEF.650
     &6.33950E-07,6.55958E-07,6.78556E-07,7.01753E-07,7.25562E-07,               RRTM_SETCOEF.651
     &7.49992E-07,7.75055E-07,8.00760E-07,8.27120E-07,8.54145E-07,               RRTM_SETCOEF.652
     &8.81845E-07,9.10233E-07,9.39318E-07,9.69113E-07,9.99627E-07,               RRTM_SETCOEF.653
     &1.03087E-06,1.06286E-06,1.09561E-06,1.12912E-06,1.16340E-06,               RRTM_SETCOEF.654
     &1.19848E-06,1.23435E-06,1.27104E-06,1.30855E-06,1.34690E-06,               RRTM_SETCOEF.655
     &1.38609E-06,1.42614E-06,1.46706E-06,1.50886E-06,1.55155E-06,               RRTM_SETCOEF.656
     &1.59515E-06,1.63967E-06,1.68512E-06,1.73150E-06,1.77884E-06,               RRTM_SETCOEF.657
     &1.82715E-06,1.87643E-06,1.92670E-06,1.97797E-06,2.03026E-06,               RRTM_SETCOEF.658
     &2.08356E-06,2.13791E-06,2.19330E-06,2.24975E-06,2.30728E-06,               RRTM_SETCOEF.659
     &2.36589E-06,2.42560E-06,2.48641E-06,2.54835E-06,2.61142E-06/               RRTM_SETCOEF.660
      DATA (TOTPLNK(I,11),I=151,181)/                                            RRTM_SETCOEF.661
     &2.67563E-06,2.74100E-06,2.80754E-06,2.87526E-06,2.94417E-06,               RRTM_SETCOEF.662
     &3.01429E-06,3.08562E-06,3.15819E-06,3.23199E-06,3.30704E-06,               RRTM_SETCOEF.663
     &3.38336E-06,3.46096E-06,3.53984E-06,3.62002E-06,3.70151E-06,               RRTM_SETCOEF.664
     &3.78433E-06,3.86848E-06,3.95399E-06,4.04084E-06,4.12907E-06,               RRTM_SETCOEF.665
     &4.21868E-06,4.30968E-06,4.40209E-06,4.49592E-06,4.59117E-06,               RRTM_SETCOEF.666
     &4.68786E-06,4.78600E-06,4.88561E-06,4.98669E-06,5.08926E-06,               RRTM_SETCOEF.667
     &5.19332E-06/                                                               RRTM_SETCOEF.668
      DATA (TOTPLNK(I,12),I=1,50)/                                               RRTM_SETCOEF.669
     &2.73921E-10,3.04500E-10,3.38056E-10,3.74835E-10,4.15099E-10,               RRTM_SETCOEF.670
     &4.59126E-10,5.07214E-10,5.59679E-10,6.16857E-10,6.79103E-10,               RRTM_SETCOEF.671
     &7.46796E-10,8.20335E-10,9.00144E-10,9.86671E-10,1.08039E-09,               RRTM_SETCOEF.672
     &1.18180E-09,1.29142E-09,1.40982E-09,1.53757E-09,1.67529E-09,               RRTM_SETCOEF.673
     &1.82363E-09,1.98327E-09,2.15492E-09,2.33932E-09,2.53726E-09,               RRTM_SETCOEF.674
     &2.74957E-09,2.97710E-09,3.22075E-09,3.48145E-09,3.76020E-09,               RRTM_SETCOEF.675
     &4.05801E-09,4.37595E-09,4.71513E-09,5.07672E-09,5.46193E-09,               RRTM_SETCOEF.676
     &5.87201E-09,6.30827E-09,6.77205E-09,7.26480E-09,7.78794E-09,               RRTM_SETCOEF.677
     &8.34304E-09,8.93163E-09,9.55537E-09,1.02159E-08,1.09151E-08,               RRTM_SETCOEF.678
     &1.16547E-08,1.24365E-08,1.32625E-08,1.41348E-08,1.50554E-08/               RRTM_SETCOEF.679
      DATA (TOTPLNK(I,12),I=51,100)/                                             RRTM_SETCOEF.680
     &1.60264E-08,1.70500E-08,1.81285E-08,1.92642E-08,2.04596E-08,               RRTM_SETCOEF.681
     &2.17171E-08,2.30394E-08,2.44289E-08,2.58885E-08,2.74209E-08,               RRTM_SETCOEF.682
     &2.90290E-08,3.07157E-08,3.24841E-08,3.43371E-08,3.62782E-08,               RRTM_SETCOEF.683
     &3.83103E-08,4.04371E-08,4.26617E-08,4.49878E-08,4.74190E-08,               RRTM_SETCOEF.684
     &4.99589E-08,5.26113E-08,5.53801E-08,5.82692E-08,6.12826E-08,               RRTM_SETCOEF.685
     &6.44245E-08,6.76991E-08,7.11105E-08,7.46634E-08,7.83621E-08,               RRTM_SETCOEF.686
     &8.22112E-08,8.62154E-08,9.03795E-08,9.47081E-08,9.92066E-08,               RRTM_SETCOEF.687
     &1.03879E-07,1.08732E-07,1.13770E-07,1.18998E-07,1.24422E-07,               RRTM_SETCOEF.688
     &1.30048E-07,1.35880E-07,1.41924E-07,1.48187E-07,1.54675E-07,               RRTM_SETCOEF.689
     &1.61392E-07,1.68346E-07,1.75543E-07,1.82988E-07,1.90688E-07/               RRTM_SETCOEF.690
      DATA (TOTPLNK(I,12),I=101,150)/                                            RRTM_SETCOEF.691
     &1.98650E-07,2.06880E-07,2.15385E-07,2.24172E-07,2.33247E-07,               RRTM_SETCOEF.692
     &2.42617E-07,2.52289E-07,2.62272E-07,2.72571E-07,2.83193E-07,               RRTM_SETCOEF.693
     &2.94147E-07,3.05440E-07,3.17080E-07,3.29074E-07,3.41430E-07,               RRTM_SETCOEF.694
     &3.54155E-07,3.67259E-07,3.80747E-07,3.94631E-07,4.08916E-07,               RRTM_SETCOEF.695
     &4.23611E-07,4.38725E-07,4.54267E-07,4.70245E-07,4.86666E-07,               RRTM_SETCOEF.696
     &5.03541E-07,5.20879E-07,5.38687E-07,5.56975E-07,5.75751E-07,               RRTM_SETCOEF.697
     &5.95026E-07,6.14808E-07,6.35107E-07,6.55932E-07,6.77293E-07,               RRTM_SETCOEF.698
     &6.99197E-07,7.21656E-07,7.44681E-07,7.68278E-07,7.92460E-07,               RRTM_SETCOEF.699
     &8.17235E-07,8.42614E-07,8.68606E-07,8.95223E-07,9.22473E-07,               RRTM_SETCOEF.700
     &9.50366E-07,9.78915E-07,1.00813E-06,1.03802E-06,1.06859E-06/               RRTM_SETCOEF.701
      DATA (TOTPLNK(I,12),I=151,181)/                                            RRTM_SETCOEF.702
     &1.09986E-06,1.13184E-06,1.16453E-06,1.19796E-06,1.23212E-06,               RRTM_SETCOEF.703
     &1.26703E-06,1.30270E-06,1.33915E-06,1.37637E-06,1.41440E-06,               RRTM_SETCOEF.704
     &1.45322E-06,1.49286E-06,1.53333E-06,1.57464E-06,1.61679E-06,               RRTM_SETCOEF.705
     &1.65981E-06,1.70370E-06,1.74847E-06,1.79414E-06,1.84071E-06,               RRTM_SETCOEF.706
     &1.88821E-06,1.93663E-06,1.98599E-06,2.03631E-06,2.08759E-06,               RRTM_SETCOEF.707
     &2.13985E-06,2.19310E-06,2.24734E-06,2.30260E-06,2.35888E-06,               RRTM_SETCOEF.708
     &2.41619E-06/                                                               RRTM_SETCOEF.709
      DATA (TOTPLNK(I,13),I=1,50)/                                               RRTM_SETCOEF.710
     &4.53634E-11,5.11435E-11,5.75754E-11,6.47222E-11,7.26531E-11,               RRTM_SETCOEF.711
     &8.14420E-11,9.11690E-11,1.01921E-10,1.13790E-10,1.26877E-10,               RRTM_SETCOEF.712
     &1.41288E-10,1.57140E-10,1.74555E-10,1.93665E-10,2.14613E-10,               RRTM_SETCOEF.713
     &2.37548E-10,2.62633E-10,2.90039E-10,3.19948E-10,3.52558E-10,               RRTM_SETCOEF.714
     &3.88073E-10,4.26716E-10,4.68719E-10,5.14331E-10,5.63815E-10,               RRTM_SETCOEF.715
     &6.17448E-10,6.75526E-10,7.38358E-10,8.06277E-10,8.79625E-10,               RRTM_SETCOEF.716
     &9.58770E-10,1.04410E-09,1.13602E-09,1.23495E-09,1.34135E-09,               RRTM_SETCOEF.717
     &1.45568E-09,1.57845E-09,1.71017E-09,1.85139E-09,2.00268E-09,               RRTM_SETCOEF.718
     &2.16464E-09,2.33789E-09,2.52309E-09,2.72093E-09,2.93212E-09,               RRTM_SETCOEF.719
     &3.15740E-09,3.39757E-09,3.65341E-09,3.92579E-09,4.21559E-09/               RRTM_SETCOEF.720
      DATA (TOTPLNK(I,13),I=51,100)/                                             RRTM_SETCOEF.721
     &4.52372E-09,4.85115E-09,5.19886E-09,5.56788E-09,5.95928E-09,               RRTM_SETCOEF.722
     &6.37419E-09,6.81375E-09,7.27917E-09,7.77168E-09,8.29256E-09,               RRTM_SETCOEF.723
     &8.84317E-09,9.42487E-09,1.00391E-08,1.06873E-08,1.13710E-08,               RRTM_SETCOEF.724
     &1.20919E-08,1.28515E-08,1.36514E-08,1.44935E-08,1.53796E-08,               RRTM_SETCOEF.725
     &1.63114E-08,1.72909E-08,1.83201E-08,1.94008E-08,2.05354E-08,               RRTM_SETCOEF.726
     &2.17258E-08,2.29742E-08,2.42830E-08,2.56545E-08,2.70910E-08,               RRTM_SETCOEF.727
     &2.85950E-08,3.01689E-08,3.18155E-08,3.35373E-08,3.53372E-08,               RRTM_SETCOEF.728
     &3.72177E-08,3.91818E-08,4.12325E-08,4.33727E-08,4.56056E-08,               RRTM_SETCOEF.729
     &4.79342E-08,5.03617E-08,5.28915E-08,5.55270E-08,5.82715E-08,               RRTM_SETCOEF.730
     &6.11286E-08,6.41019E-08,6.71951E-08,7.04119E-08,7.37560E-08/               RRTM_SETCOEF.731
      DATA (TOTPLNK(I,13),I=101,150)/                                            RRTM_SETCOEF.732
     &7.72315E-08,8.08424E-08,8.45927E-08,8.84866E-08,9.25281E-08,               RRTM_SETCOEF.733
     &9.67218E-08,1.01072E-07,1.05583E-07,1.10260E-07,1.15107E-07,               RRTM_SETCOEF.734
     &1.20128E-07,1.25330E-07,1.30716E-07,1.36291E-07,1.42061E-07,               RRTM_SETCOEF.735
     &1.48031E-07,1.54206E-07,1.60592E-07,1.67192E-07,1.74015E-07,               RRTM_SETCOEF.736
     &1.81064E-07,1.88345E-07,1.95865E-07,2.03628E-07,2.11643E-07,               RRTM_SETCOEF.737
     &2.19912E-07,2.28443E-07,2.37244E-07,2.46318E-07,2.55673E-07,               RRTM_SETCOEF.738
     &2.65316E-07,2.75252E-07,2.85489E-07,2.96033E-07,3.06891E-07,               RRTM_SETCOEF.739
     &3.18070E-07,3.29576E-07,3.41417E-07,3.53600E-07,3.66133E-07,               RRTM_SETCOEF.740
     &3.79021E-07,3.92274E-07,4.05897E-07,4.19899E-07,4.34288E-07,               RRTM_SETCOEF.741
     &4.49071E-07,4.64255E-07,4.79850E-07,4.95863E-07,5.12300E-07/               RRTM_SETCOEF.742
      DATA (TOTPLNK(I,13),I=151,181)/                                            RRTM_SETCOEF.743
     &5.29172E-07,5.46486E-07,5.64250E-07,5.82473E-07,6.01164E-07,               RRTM_SETCOEF.744
     &6.20329E-07,6.39979E-07,6.60122E-07,6.80767E-07,7.01922E-07,               RRTM_SETCOEF.745
     &7.23596E-07,7.45800E-07,7.68539E-07,7.91826E-07,8.15669E-07,               RRTM_SETCOEF.746
     &8.40076E-07,8.65058E-07,8.90623E-07,9.16783E-07,9.43544E-07,               RRTM_SETCOEF.747
     &9.70917E-07,9.98912E-07,1.02754E-06,1.05681E-06,1.08673E-06,               RRTM_SETCOEF.748
     &1.11731E-06,1.14856E-06,1.18050E-06,1.21312E-06,1.24645E-06,               RRTM_SETCOEF.749
     &1.28049E-06/                                                               RRTM_SETCOEF.750
      DATA (TOTPLNK(I,14),I=1,50)/                                               RRTM_SETCOEF.751
     &1.40113E-11,1.59358E-11,1.80960E-11,2.05171E-11,2.32266E-11,               RRTM_SETCOEF.752
     &2.62546E-11,2.96335E-11,3.33990E-11,3.75896E-11,4.22469E-11,               RRTM_SETCOEF.753
     &4.74164E-11,5.31466E-11,5.94905E-11,6.65054E-11,7.42522E-11,               RRTM_SETCOEF.754
     &8.27975E-11,9.22122E-11,1.02573E-10,1.13961E-10,1.26466E-10,               RRTM_SETCOEF.755
     &1.40181E-10,1.55206E-10,1.71651E-10,1.89630E-10,2.09265E-10,               RRTM_SETCOEF.756
     &2.30689E-10,2.54040E-10,2.79467E-10,3.07128E-10,3.37190E-10,               RRTM_SETCOEF.757
     &3.69833E-10,4.05243E-10,4.43623E-10,4.85183E-10,5.30149E-10,               RRTM_SETCOEF.758
     &5.78755E-10,6.31255E-10,6.87910E-10,7.49002E-10,8.14824E-10,               RRTM_SETCOEF.759
     &8.85687E-10,9.61914E-10,1.04385E-09,1.13186E-09,1.22631E-09,               RRTM_SETCOEF.760
     &1.32761E-09,1.43617E-09,1.55243E-09,1.67686E-09,1.80992E-09/               RRTM_SETCOEF.761
      DATA (TOTPLNK(I,14),I=51,100)/                                             RRTM_SETCOEF.762
     &1.95212E-09,2.10399E-09,2.26607E-09,2.43895E-09,2.62321E-09,               RRTM_SETCOEF.763
     &2.81949E-09,3.02844E-09,3.25073E-09,3.48707E-09,3.73820E-09,               RRTM_SETCOEF.764
     &4.00490E-09,4.28794E-09,4.58819E-09,4.90647E-09,5.24371E-09,               RRTM_SETCOEF.765
     &5.60081E-09,5.97875E-09,6.37854E-09,6.80120E-09,7.24782E-09,               RRTM_SETCOEF.766
     &7.71950E-09,8.21740E-09,8.74271E-09,9.29666E-09,9.88054E-09,               RRTM_SETCOEF.767
     &1.04956E-08,1.11434E-08,1.18251E-08,1.25422E-08,1.32964E-08,               RRTM_SETCOEF.768
     &1.40890E-08,1.49217E-08,1.57961E-08,1.67140E-08,1.76771E-08,               RRTM_SETCOEF.769
     &1.86870E-08,1.97458E-08,2.08553E-08,2.20175E-08,2.32342E-08,               RRTM_SETCOEF.770
     &2.45077E-08,2.58401E-08,2.72334E-08,2.86900E-08,3.02122E-08,               RRTM_SETCOEF.771
     &3.18021E-08,3.34624E-08,3.51954E-08,3.70037E-08,3.88899E-08/               RRTM_SETCOEF.772
      DATA (TOTPLNK(I,14),I=101,150)/                                            RRTM_SETCOEF.773
     &4.08568E-08,4.29068E-08,4.50429E-08,4.72678E-08,4.95847E-08,               RRTM_SETCOEF.774
     &5.19963E-08,5.45058E-08,5.71161E-08,5.98309E-08,6.26529E-08,               RRTM_SETCOEF.775
     &6.55857E-08,6.86327E-08,7.17971E-08,7.50829E-08,7.84933E-08,               RRTM_SETCOEF.776
     &8.20323E-08,8.57035E-08,8.95105E-08,9.34579E-08,9.75488E-08,               RRTM_SETCOEF.777
     &1.01788E-07,1.06179E-07,1.10727E-07,1.15434E-07,1.20307E-07,               RRTM_SETCOEF.778
     &1.25350E-07,1.30566E-07,1.35961E-07,1.41539E-07,1.47304E-07,               RRTM_SETCOEF.779
     &1.53263E-07,1.59419E-07,1.65778E-07,1.72345E-07,1.79124E-07,               RRTM_SETCOEF.780
     &1.86122E-07,1.93343E-07,2.00792E-07,2.08476E-07,2.16400E-07,               RRTM_SETCOEF.781
     &2.24568E-07,2.32988E-07,2.41666E-07,2.50605E-07,2.59813E-07,               RRTM_SETCOEF.782
     &2.69297E-07,2.79060E-07,2.89111E-07,2.99455E-07,3.10099E-07/               RRTM_SETCOEF.783
      DATA (TOTPLNK(I,14),I=151,181)/                                            RRTM_SETCOEF.784
     &3.21049E-07,3.32311E-07,3.43893E-07,3.55801E-07,3.68041E-07,               RRTM_SETCOEF.785
     &3.80621E-07,3.93547E-07,4.06826E-07,4.20465E-07,4.34473E-07,               RRTM_SETCOEF.786
     &4.48856E-07,4.63620E-07,4.78774E-07,4.94325E-07,5.10280E-07,               RRTM_SETCOEF.787
     &5.26648E-07,5.43436E-07,5.60652E-07,5.78302E-07,5.96397E-07,               RRTM_SETCOEF.788
     &6.14943E-07,6.33949E-07,6.53421E-07,6.73370E-07,6.93803E-07,               RRTM_SETCOEF.789
     &7.14731E-07,7.36157E-07,7.58095E-07,7.80549E-07,8.03533E-07,               RRTM_SETCOEF.790
     &8.27050E-07/                                                               RRTM_SETCOEF.791
      DATA (TOTPLNK(I,15),I=1,50)/                                               RRTM_SETCOEF.792
     &3.90483E-12,4.47999E-12,5.13122E-12,5.86739E-12,6.69829E-12,               RRTM_SETCOEF.793
     &7.63467E-12,8.68833E-12,9.87221E-12,1.12005E-11,1.26885E-11,               RRTM_SETCOEF.794
     &1.43534E-11,1.62134E-11,1.82888E-11,2.06012E-11,2.31745E-11,               RRTM_SETCOEF.795
     &2.60343E-11,2.92087E-11,3.27277E-11,3.66242E-11,4.09334E-11,               RRTM_SETCOEF.796
     &4.56935E-11,5.09455E-11,5.67338E-11,6.31057E-11,7.01127E-11,               RRTM_SETCOEF.797
     &7.78096E-11,8.62554E-11,9.55130E-11,1.05651E-10,1.16740E-10,               RRTM_SETCOEF.798
     &1.28858E-10,1.42089E-10,1.56519E-10,1.72243E-10,1.89361E-10,               RRTM_SETCOEF.799
     &2.07978E-10,2.28209E-10,2.50173E-10,2.73999E-10,2.99820E-10,               RRTM_SETCOEF.800
     &3.27782E-10,3.58034E-10,3.90739E-10,4.26067E-10,4.64196E-10,               RRTM_SETCOEF.801
     &5.05317E-10,5.49631E-10,5.97347E-10,6.48689E-10,7.03891E-10/               RRTM_SETCOEF.802
      DATA (TOTPLNK(I,15),I=51,100)/                                             RRTM_SETCOEF.803
     &7.63201E-10,8.26876E-10,8.95192E-10,9.68430E-10,1.04690E-09,               RRTM_SETCOEF.804
     &1.13091E-09,1.22079E-09,1.31689E-09,1.41957E-09,1.52922E-09,               RRTM_SETCOEF.805
     &1.64623E-09,1.77101E-09,1.90401E-09,2.04567E-09,2.19647E-09,               RRTM_SETCOEF.806
     &2.35690E-09,2.52749E-09,2.70875E-09,2.90127E-09,3.10560E-09,               RRTM_SETCOEF.807
     &3.32238E-09,3.55222E-09,3.79578E-09,4.05375E-09,4.32682E-09,               RRTM_SETCOEF.808
     &4.61574E-09,4.92128E-09,5.24420E-09,5.58536E-09,5.94558E-09,               RRTM_SETCOEF.809
     &6.32575E-09,6.72678E-09,7.14964E-09,7.59526E-09,8.06470E-09,               RRTM_SETCOEF.810
     &8.55897E-09,9.07916E-09,9.62638E-09,1.02018E-08,1.08066E-08,               RRTM_SETCOEF.811
     &1.14420E-08,1.21092E-08,1.28097E-08,1.35446E-08,1.43155E-08,               RRTM_SETCOEF.812
     &1.51237E-08,1.59708E-08,1.68581E-08,1.77873E-08,1.87599E-08/               RRTM_SETCOEF.813
      DATA (TOTPLNK(I,15),I=101,150)/                                            RRTM_SETCOEF.814
     &1.97777E-08,2.08423E-08,2.19555E-08,2.31190E-08,2.43348E-08,               RRTM_SETCOEF.815
     &2.56045E-08,2.69302E-08,2.83140E-08,2.97578E-08,3.12636E-08,               RRTM_SETCOEF.816
     &3.28337E-08,3.44702E-08,3.61755E-08,3.79516E-08,3.98012E-08,               RRTM_SETCOEF.817
     &4.17265E-08,4.37300E-08,4.58143E-08,4.79819E-08,5.02355E-08,               RRTM_SETCOEF.818
     &5.25777E-08,5.50114E-08,5.75393E-08,6.01644E-08,6.28896E-08,               RRTM_SETCOEF.819
     &6.57177E-08,6.86521E-08,7.16959E-08,7.48520E-08,7.81239E-08,               RRTM_SETCOEF.820
     &8.15148E-08,8.50282E-08,8.86675E-08,9.24362E-08,9.63380E-08,               RRTM_SETCOEF.821
     &1.00376E-07,1.04555E-07,1.08878E-07,1.13349E-07,1.17972E-07,               RRTM_SETCOEF.822
     &1.22751E-07,1.27690E-07,1.32793E-07,1.38064E-07,1.43508E-07,               RRTM_SETCOEF.823
     &1.49129E-07,1.54931E-07,1.60920E-07,1.67099E-07,1.73473E-07/               RRTM_SETCOEF.824
      DATA (TOTPLNK(I,15),I=151,181)/                                            RRTM_SETCOEF.825
     &1.80046E-07,1.86825E-07,1.93812E-07,2.01014E-07,2.08436E-07,               RRTM_SETCOEF.826
     &2.16082E-07,2.23957E-07,2.32067E-07,2.40418E-07,2.49013E-07,               RRTM_SETCOEF.827
     &2.57860E-07,2.66963E-07,2.76328E-07,2.85961E-07,2.95868E-07,               RRTM_SETCOEF.828
     &3.06053E-07,3.16524E-07,3.27286E-07,3.38345E-07,3.49707E-07,               RRTM_SETCOEF.829
     &3.61379E-07,3.73367E-07,3.85676E-07,3.98315E-07,4.11287E-07,               RRTM_SETCOEF.830
     &4.24602E-07,4.38265E-07,4.52283E-07,4.66662E-07,4.81410E-07,               RRTM_SETCOEF.831
     &4.96535E-07/                                                               RRTM_SETCOEF.832
      DATA (TOTPLNK(I,16),I=1,50)/                                               RRTM_SETCOEF.833
     &4.65378E-13,5.41927E-13,6.29913E-13,7.30869E-13,8.46510E-13,               RRTM_SETCOEF.834
     &9.78750E-13,1.12972E-12,1.30181E-12,1.49764E-12,1.72016E-12,               RRTM_SETCOEF.835
     &1.97260E-12,2.25858E-12,2.58206E-12,2.94744E-12,3.35955E-12,               RRTM_SETCOEF.836
     &3.82372E-12,4.34581E-12,4.93225E-12,5.59010E-12,6.32711E-12,               RRTM_SETCOEF.837
     &7.15171E-12,8.07317E-12,9.10159E-12,1.02480E-11,1.15244E-11,               RRTM_SETCOEF.838
     &1.29438E-11,1.45204E-11,1.62697E-11,1.82084E-11,2.03545E-11,               RRTM_SETCOEF.839
     &2.27278E-11,2.53494E-11,2.82424E-11,3.14313E-11,3.49431E-11,               RRTM_SETCOEF.840
     &3.88064E-11,4.30522E-11,4.77139E-11,5.28273E-11,5.84308E-11,               RRTM_SETCOEF.841
     &6.45658E-11,7.12764E-11,7.86103E-11,8.66176E-11,9.53534E-11,               RRTM_SETCOEF.842
     &1.04875E-10,1.15245E-10,1.26528E-10,1.38796E-10,1.52123E-10/               RRTM_SETCOEF.843
      DATA (TOTPLNK(I,16),I=51,100)/                                             RRTM_SETCOEF.844
     &1.66590E-10,1.82281E-10,1.99287E-10,2.17704E-10,2.37632E-10,               RRTM_SETCOEF.845
     &2.59182E-10,2.82468E-10,3.07610E-10,3.34738E-10,3.63988E-10,               RRTM_SETCOEF.846
     &3.95504E-10,4.29438E-10,4.65951E-10,5.05212E-10,5.47402E-10,               RRTM_SETCOEF.847
     &5.92707E-10,6.41329E-10,6.93477E-10,7.49371E-10,8.09242E-10,               RRTM_SETCOEF.848
     &8.73338E-10,9.41911E-10,1.01524E-09,1.09359E-09,1.17728E-09,               RRTM_SETCOEF.849
     &1.26660E-09,1.36190E-09,1.46350E-09,1.57177E-09,1.68709E-09,               RRTM_SETCOEF.850
     &1.80984E-09,1.94044E-09,2.07932E-09,2.22693E-09,2.38373E-09,               RRTM_SETCOEF.851
     &2.55021E-09,2.72689E-09,2.91429E-09,3.11298E-09,3.32353E-09,               RRTM_SETCOEF.852
     &3.54655E-09,3.78265E-09,4.03251E-09,4.29679E-09,4.57620E-09,               RRTM_SETCOEF.853
     &4.87148E-09,5.18341E-09,5.51276E-09,5.86037E-09,6.22708E-09/               RRTM_SETCOEF.854
      DATA (TOTPLNK(I,16),I=101,150)/                                            RRTM_SETCOEF.855
     &6.61381E-09,7.02145E-09,7.45097E-09,7.90336E-09,8.37967E-09,               RRTM_SETCOEF.856
     &8.88092E-09,9.40827E-09,9.96280E-09,1.05457E-08,1.11583E-08,               RRTM_SETCOEF.857
     &1.18017E-08,1.24773E-08,1.31865E-08,1.39306E-08,1.47111E-08,               RRTM_SETCOEF.858
     &1.55295E-08,1.63872E-08,1.72860E-08,1.82274E-08,1.92132E-08,               RRTM_SETCOEF.859
     &2.02450E-08,2.13247E-08,2.24541E-08,2.36352E-08,2.48699E-08,               RRTM_SETCOEF.860
     &2.61602E-08,2.75082E-08,2.89161E-08,3.03860E-08,3.19203E-08,               RRTM_SETCOEF.861
     &3.35213E-08,3.51913E-08,3.69330E-08,3.87486E-08,4.06411E-08,               RRTM_SETCOEF.862
     &4.26129E-08,4.46668E-08,4.68058E-08,4.90325E-08,5.13502E-08,               RRTM_SETCOEF.863
     &5.37617E-08,5.62703E-08,5.88791E-08,6.15915E-08,6.44107E-08,               RRTM_SETCOEF.864
     &6.73404E-08,7.03841E-08,7.35453E-08,7.68278E-08,8.02355E-08/               RRTM_SETCOEF.865
      DATA (TOTPLNK(I,16),I=151,181)/                                            RRTM_SETCOEF.866
     &8.37721E-08,8.74419E-08,9.12486E-08,9.51968E-08,9.92905E-08,               RRTM_SETCOEF.867
     &1.03534E-07,1.07932E-07,1.12490E-07,1.17211E-07,1.22100E-07,               RRTM_SETCOEF.868
     &1.27163E-07,1.32404E-07,1.37829E-07,1.43443E-07,1.49250E-07,               RRTM_SETCOEF.869
     &1.55257E-07,1.61470E-07,1.67893E-07,1.74532E-07,1.81394E-07,               RRTM_SETCOEF.870
     &1.88485E-07,1.95810E-07,2.03375E-07,2.11189E-07,2.19256E-07,               RRTM_SETCOEF.871
     &2.27583E-07,2.36177E-07,2.45046E-07,2.54196E-07,2.63634E-07,               RRTM_SETCOEF.872
     &2.73367E-07/                                                               RRTM_SETCOEF.873
                                                                                 RRTM_SETCOEF.874
      DATA (TOTPLK16(I),I=1,50)/                                                 RRTM_SETCOEF.875
     &4.46128E-13,5.19008E-13,6.02681E-13,6.98580E-13,8.08302E-13,               RRTM_SETCOEF.876
     &9.33629E-13,1.07654E-12,1.23925E-12,1.42419E-12,1.63407E-12,               RRTM_SETCOEF.877
     &1.87190E-12,2.14099E-12,2.44498E-12,2.78793E-12,3.17424E-12,               RRTM_SETCOEF.878
     &3.60881E-12,4.09698E-12,4.64461E-12,5.25813E-12,5.94456E-12,               RRTM_SETCOEF.879
     &6.71156E-12,7.56752E-12,8.52154E-12,9.58357E-12,1.07644E-11,               RRTM_SETCOEF.880
     &1.20758E-11,1.35304E-11,1.51420E-11,1.69256E-11,1.88973E-11,               RRTM_SETCOEF.881
     &2.10746E-11,2.34762E-11,2.61227E-11,2.90356E-11,3.22388E-11,               RRTM_SETCOEF.882
     &3.57574E-11,3.96187E-11,4.38519E-11,4.84883E-11,5.35616E-11,               RRTM_SETCOEF.883
     &5.91075E-11,6.51647E-11,7.17743E-11,7.89797E-11,8.68284E-11,               RRTM_SETCOEF.884
     &9.53697E-11,1.04658E-10,1.14748E-10,1.25701E-10,1.37582E-10/               RRTM_SETCOEF.885
      DATA (TOTPLK16(I),I=51,100)/                                               RRTM_SETCOEF.886
     &1.50457E-10,1.64400E-10,1.79487E-10,1.95799E-10,2.13422E-10,               RRTM_SETCOEF.887
     &2.32446E-10,2.52970E-10,2.75094E-10,2.98925E-10,3.24578E-10,               RRTM_SETCOEF.888
     &3.52172E-10,3.81833E-10,4.13695E-10,4.47897E-10,4.84588E-10,               RRTM_SETCOEF.889
     &5.23922E-10,5.66063E-10,6.11182E-10,6.59459E-10,7.11081E-10,               RRTM_SETCOEF.890
     &7.66251E-10,8.25172E-10,8.88065E-10,9.55155E-10,1.02668E-09,               RRTM_SETCOEF.891
     &1.10290E-09,1.18406E-09,1.27044E-09,1.36233E-09,1.46002E-09,               RRTM_SETCOEF.892
     &1.56382E-09,1.67406E-09,1.79108E-09,1.91522E-09,2.04686E-09,               RRTM_SETCOEF.893
     &2.18637E-09,2.33416E-09,2.49063E-09,2.65622E-09,2.83136E-09,               RRTM_SETCOEF.894
     &3.01653E-09,3.21221E-09,3.41890E-09,3.63712E-09,3.86740E-09,               RRTM_SETCOEF.895
     &4.11030E-09,4.36641E-09,4.63631E-09,4.92064E-09,5.22003E-09/               RRTM_SETCOEF.896
      DATA (TOTPLK16(I),I=101,150)/                                              RRTM_SETCOEF.897
     &5.53516E-09,5.86670E-09,6.21538E-09,6.58191E-09,6.96708E-09,               RRTM_SETCOEF.898
     &7.37165E-09,7.79645E-09,8.24229E-09,8.71007E-09,9.20066E-09,               RRTM_SETCOEF.899
     &9.71498E-09,1.02540E-08,1.08186E-08,1.14100E-08,1.20290E-08,               RRTM_SETCOEF.900
     &1.26767E-08,1.33544E-08,1.40630E-08,1.48038E-08,1.55780E-08,               RRTM_SETCOEF.901
     &1.63867E-08,1.72313E-08,1.81130E-08,1.90332E-08,1.99932E-08,               RRTM_SETCOEF.902
     &2.09945E-08,2.20385E-08,2.31267E-08,2.42605E-08,2.54416E-08,               RRTM_SETCOEF.903
     &2.66716E-08,2.79520E-08,2.92846E-08,3.06711E-08,3.21133E-08,               RRTM_SETCOEF.904
     &3.36128E-08,3.51717E-08,3.67918E-08,3.84749E-08,4.02232E-08,               RRTM_SETCOEF.905
     &4.20386E-08,4.39231E-08,4.58790E-08,4.79083E-08,5.00132E-08,               RRTM_SETCOEF.906
     &5.21961E-08,5.44592E-08,5.68049E-08,5.92356E-08,6.17537E-08/               RRTM_SETCOEF.907
      DATA (TOTPLK16(I),I=151,181)/                                              RRTM_SETCOEF.908
     &6.43617E-08,6.70622E-08,6.98578E-08,7.27511E-08,7.57449E-08,               RRTM_SETCOEF.909
     &7.88419E-08,8.20449E-08,8.53568E-08,8.87805E-08,9.23190E-08,               RRTM_SETCOEF.910
     &9.59753E-08,9.97526E-08,1.03654E-07,1.07682E-07,1.11841E-07,               RRTM_SETCOEF.911
     &1.16134E-07,1.20564E-07,1.25135E-07,1.29850E-07,1.34712E-07,               RRTM_SETCOEF.912
     &1.39726E-07,1.44894E-07,1.50221E-07,1.55711E-07,1.61367E-07,               RRTM_SETCOEF.913
     &1.67193E-07,1.73193E-07,1.79371E-07,1.85732E-07,1.92279E-07,               RRTM_SETCOEF.914
     &1.99016E-07/                                                               RRTM_SETCOEF.915
                                                                                 RRTM_SETCOEF.916
      END                                                                        RRTM_SETCOEF.917
                                                                                 RRTM_SETCOEF.918
