                                                                                 RRTM_TAUMOL.1
C******************************************************************************* RRTM_TAUMOL.2
C*                                                                             * RRTM_TAUMOL.3
C*                  Optical depths developed for the                           * RRTM_TAUMOL.4
C*                                                                             * RRTM_TAUMOL.5
C*                RAPID RADIATIVE TRANSFER MODEL (RRTM)                        * RRTM_TAUMOL.6
C*                                                                             * RRTM_TAUMOL.7
C*                                                                             * RRTM_TAUMOL.8
C*            ATMOSPHERIC AND ENVIRONMENTAL RESEARCH, INC.                     * RRTM_TAUMOL.9
C*                        840 MEMORIAL DRIVE                                   * RRTM_TAUMOL.10
C*                        CAMBRIDGE, MA 02139                                  * RRTM_TAUMOL.11
C*                                                                             * RRTM_TAUMOL.12
C*                                                                             * RRTM_TAUMOL.13
C*                           ELI J. MLAWER                                     * RRTM_TAUMOL.14
C*                         STEVEN J. TAUBMAN                                   * RRTM_TAUMOL.15
C*                         SHEPARD A. CLOUGH                                   * RRTM_TAUMOL.16
C*                                                                             * RRTM_TAUMOL.17
C*                                                                             * RRTM_TAUMOL.18
C*                                                                             * RRTM_TAUMOL.19
C*                                                                             * RRTM_TAUMOL.20
C*                       email:  mlawer@aer.com                                * RRTM_TAUMOL.21
C*                                                                             * RRTM_TAUMOL.22
C*        The authors wish to acknowledge the contributions of the             * RRTM_TAUMOL.23
C*        following people:  Patrick D. Brown, Michael J. Iacono,              * RRTM_TAUMOL.24
C*        Ronald E. Farren, Luke Chen, Robert Bergstrom.                       * RRTM_TAUMOL.25
C*                                                                             * RRTM_TAUMOL.26
C******************************************************************************* RRTM_TAUMOL.27
C*                                                                             * RRTM_TAUMOL.28
C*  Revision for NCAR CCM:  Michael J. Iacono; September, 1998                 * RRTM_TAUMOL.29
C*                                                                             * RRTM_TAUMOL.30
C*     TAUMOL                                                                  * RRTM_TAUMOL.31
C*                                                                             * RRTM_TAUMOL.32
C*     This file contains the subroutines TAUGBn (where n goes from            * RRTM_TAUMOL.33
C*     1 to 16).  TAUGBn calculates the optical depths and Planck fractions    * RRTM_TAUMOL.34
C*     per g-value and layer for band n.                                       * RRTM_TAUMOL.35
C*                                                                             * RRTM_TAUMOL.36
C*  Output:  optical depths (unitless)                                         * RRTM_TAUMOL.37
C*           fractions needed to compute Planck functions at every layer       * RRTM_TAUMOL.38
C*               and g-value                                                   * RRTM_TAUMOL.39
C*                                                                             * RRTM_TAUMOL.40
C*     COMMON /TAUGCOM/  TAUG(MXLAY,MG)                                        * RRTM_TAUMOL.41
C*     COMMON /PLANKG/   FRACS(MXLAY,MG)                                       * RRTM_TAUMOL.42
C*                                                                             * RRTM_TAUMOL.43
C*  Input                                                                      * RRTM_TAUMOL.44
C*                                                                             * RRTM_TAUMOL.45
C*     COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                  * RRTM_TAUMOL.46
C*     COMMON /PRECISE/  ONEMINUS                                              * RRTM_TAUMOL.47
C*     COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                    * RRTM_TAUMOL.48
C*    &                  PZ(0:MXLAY),TZ(0:MXLAY)                               * RRTM_TAUMOL.49
C*     COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                              * RRTM_TAUMOL.50
C*    &                  COLH2O(MXLAY),COLCO2(MXLAY),                          * RRTM_TAUMOL.51
C*    &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),             * RRTM_TAUMOL.52
C*    &                  COLO2(MXLAY),CO2MULT(MXLAY)                           * RRTM_TAUMOL.53
C*     COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                            * RRTM_TAUMOL.54
C*    &                  FAC10(MXLAY),FAC11(MXLAY)                             * RRTM_TAUMOL.55
C*     COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                        * RRTM_TAUMOL.56
C*     COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)       * RRTM_TAUMOL.57
C*                                                                             * RRTM_TAUMOL.58
C*     Description:                                                            * RRTM_TAUMOL.59
C*     NG(IBAND) - number of g-values in band IBAND                            * RRTM_TAUMOL.60
C*     NSPA(IBAND) - for the lower atmosphere, the number of reference         * RRTM_TAUMOL.61
C*                   atmospheres that are stored for band IBAND per            * RRTM_TAUMOL.62
C*                   pressure level and temperature.  Each of these            * RRTM_TAUMOL.63
C*                   atmospheres has different relative amounts of the         * RRTM_TAUMOL.64
C*                   key species for the band (i.e. different binary           * RRTM_TAUMOL.65
C*                   species parameters).                                      * RRTM_TAUMOL.66
C*     NSPB(IBAND) - same for upper atmosphere                                 * RRTM_TAUMOL.67
C*     ONEMINUS - since problems are caused in some cases by interpolation     * RRTM_TAUMOL.68
C*                parameters equal to or greater than 1, for these cases       * RRTM_TAUMOL.69
C*                these parameters are set to this value, slightly < 1.        * RRTM_TAUMOL.70
C*     PAVEL - layer pressures (mb)                                            * RRTM_TAUMOL.71
C*     TAVEL - layer temperatures (degrees K)                                  * RRTM_TAUMOL.72
C*     PZ - level pressures (mb)                                               * RRTM_TAUMOL.73
C*     TZ - level temperatures (degrees K)                                     * RRTM_TAUMOL.74
C*     LAYTROP - layer at which switch is made from one combination of         * RRTM_TAUMOL.75
C*               key species to another                                        * RRTM_TAUMOL.76
C*     COLH2O, COLCO2, COLO3, COLN2O, COLCH4 - column amounts of water         * RRTM_TAUMOL.77
C*               vapor,carbon dioxide, ozone, nitrous ozide, methane,          * RRTM_TAUMOL.78
C*               respectively (molecules/cm**2)                                * RRTM_TAUMOL.79
C*     CO2MULT - for bands in which carbon dioxide is implemented as a         * RRTM_TAUMOL.80
C*               trace species, this is the factor used to multiply the        * RRTM_TAUMOL.81
C*               band's average CO2 absorption coefficient to get the added    * RRTM_TAUMOL.82
C*               contribution to the optical depth relative to 355 ppm.        * RRTM_TAUMOL.83
C*     FACij(LAY) - for layer LAY, these are factors that are needed to        * RRTM_TAUMOL.84
C*                  compute the interpolation factors that multiply the        * RRTM_TAUMOL.85
C*                  appropriate reference k-values.  A value of 0 (1) for      * RRTM_TAUMOL.86
C*                  i,j indicates that the corresponding factor multiplies     * RRTM_TAUMOL.87
C*                  reference k-value for the lower (higher) of the two        * RRTM_TAUMOL.88
C*                  appropriate temperatures, and altitudes, respectively.     * RRTM_TAUMOL.89
C*     JP - the index of the lower (in altitude) of the two appropriate        * RRTM_TAUMOL.90
C*          reference pressure levels needed for interpolation                 * RRTM_TAUMOL.91
C*     JT, JT1 - the indices of the lower of the two appropriate reference     * RRTM_TAUMOL.92
C*               temperatures needed for interpolation (for pressure           * RRTM_TAUMOL.93
C*               levels JP and JP+1, respectively)                             * RRTM_TAUMOL.94
C*     SELFFAC - scale factor needed to water vapor self-continuum, equals     * RRTM_TAUMOL.95
C*               (water vapor density)/(atmospheric density at 296K and        * RRTM_TAUMOL.96
C*               1013 mb)                                                      * RRTM_TAUMOL.97
C*     SELFFRAC - factor needed for temperature interpolation of reference     * RRTM_TAUMOL.98
C*                water vapor self-continuum data                              * RRTM_TAUMOL.99
C*     INDSELF - index of the lower of the two appropriate reference           * RRTM_TAUMOL.100
C*               temperatures needed for the self-continuum interpolation      * RRTM_TAUMOL.101
C*                                                                             * RRTM_TAUMOL.102
C*  Data input                                                                 * RRTM_TAUMOL.103
C*     COMMON /Kn/ KA(NSPA(n),5,13,MG), KB(NSPB(n),5,13:59,MG), SELFREF(10,MG) * RRTM_TAUMOL.104
C*        (note:  n is the band number)                                        * RRTM_TAUMOL.105
C*                                                                             * RRTM_TAUMOL.106
C*     Description:                                                            * RRTM_TAUMOL.107
C*     KA - k-values for low reference atmospheres (no water vapor             * RRTM_TAUMOL.108
C*          self-continuum) (units: cm**2/molecule)                            * RRTM_TAUMOL.109
C*     KB - k-values for high reference atmospheres (all sources)              * RRTM_TAUMOL.110
C*          (units: cm**2/molecule)                                            * RRTM_TAUMOL.111
C*     SELFREF - k-values for water vapor self-continuum for reference         * RRTM_TAUMOL.112
C*               atmospheres (used below LAYTROP)                              * RRTM_TAUMOL.113
C*               (units: cm**2/molecule)                                       * RRTM_TAUMOL.114
C*                                                                             * RRTM_TAUMOL.115
C*     DIMENSION ABSA(65*NSPA(n),MG), ABSB(235*NSPB(n),MG)                     * RRTM_TAUMOL.116
C*     EQUIVALENCE (KA,ABSA),(KB,ABSB)                                         * RRTM_TAUMOL.117
C*                                                                             * RRTM_TAUMOL.118
C******************************************************************************* RRTM_TAUMOL.119
                                                                                 RRTM_TAUMOL.120
                                                                                 RRTM_TAUMOL.121
      SUBROUTINE TAUGB1                                                          RRTM_TAUMOL.122
                                                                                 RRTM_TAUMOL.123
C     Written by Eli J. Mlawer, Atmospheric & Environmental Research.            RRTM_TAUMOL.124
C     Revised by Michael J. Iacono, Atmospheric & Environmental Research.        RRTM_TAUMOL.125
                                                                                 RRTM_TAUMOL.126
C     BAND 1:  10-250 cm-1 (low - H2O; high - H2O)                               RRTM_TAUMOL.127
                                                                                 RRTM_TAUMOL.128
      PARAMETER (MXLAY=203, NBANDS=16)                                           RRTM_TAUMOL.129
      PARAMETER (NGPT=140, NG1=8)                                                RRTM_TAUMOL.130
C  Output                                                                        RRTM_TAUMOL.131
                                                                                 RRTM_TAUMOL.132
      COMMON /TAUS/     TAUG(NGPT,MXLAY)                                         RRTM_TAUMOL.133
                                                                                 RRTM_TAUMOL.134
C  Input                                                                         RRTM_TAUMOL.135
                                                                                 RRTM_TAUMOL.136
      COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                     RRTM_TAUMOL.137
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       RRTM_TAUMOL.138
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  RRTM_TAUMOL.139
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                                 RRTM_TAUMOL.140
     &                  COLH2O(MXLAY),COLCO2(MXLAY),                             RRTM_TAUMOL.141
     &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),                RRTM_TAUMOL.142
     &                  COLO2(MXLAY),CO2MULT(MXLAY)                              RRTM_TAUMOL.143
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                               RRTM_TAUMOL.144
     &                  FAC10(MXLAY),FAC11(MXLAY)                                RRTM_TAUMOL.145
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                           RRTM_TAUMOL.146
      COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)          RRTM_TAUMOL.147
      COMMON /FOREIGN/  FORFAC(MXLAY)                                            RRTM_TAUMOL.148
      COMMON /K1C/      KA(5,13,NG1), KB(5,13:59,NG1),                           RRTM_TAUMOL.149
     &                  SELFREF(10,NG1), FORREF(NG1)                             RRTM_TAUMOL.150
      COMMON /PF1C/     FRACREFA(NG1), FRACREFB(NG1)                             RRTM_TAUMOL.151
      COMMON /SP/       PFRAC(NGPT,MXLAY)                                        RRTM_TAUMOL.152
                                                                                 RRTM_TAUMOL.153
C This compiler directive was added to insure private common block storage       RRTM_TAUMOL.154
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_TAUMOL.155
C carry constants.                                                               RRTM_TAUMOL.156
C SGI                                                                            RRTM_TAUMOL.159
C$OMP THREADPRIVATE (/TAUS/,/PROFILE/,/PROFDATA/,/INTFAC/,/INTIND/)              RRTM_TAUMOL.160
C$OMP THREADPRIVATE (/SELF/,/FOREIGN/,/SP/)                                      RRTM_TAUMOL.161
C ***                                                                            RRTM_TAUMOL.162
                                                                                 RRTM_TAUMOL.163
      DIMENSION ABSA(65,NG1),ABSB(235,NG1)                                       RRTM_TAUMOL.164
      REAL KA,KB                                                                 RRTM_TAUMOL.165
      EQUIVALENCE (KA,ABSA),(KB,ABSB)                                            RRTM_TAUMOL.166
                                                                                 RRTM_TAUMOL.167
C     Compute the optical depth by interpolating in ln(pressure) and             RRTM_TAUMOL.168
C     temperature.  Below LAYTROP, the water vapor self-continuum                RRTM_TAUMOL.169
C     is interpolated (in temperature) separately.                               RRTM_TAUMOL.170
      DO 2500 LAY = 1, LAYTROP                                                   RRTM_TAUMOL.171
         IND0 = ((JP(LAY)-1)*5+(JT(LAY)-1))*NSPA(1) + 1                          RRTM_TAUMOL.172
         IND1 = (JP(LAY)*5+(JT1(LAY)-1))*NSPA(1) + 1                             RRTM_TAUMOL.173
         INDS = INDSELF(LAY)                                                     RRTM_TAUMOL.174
         DO 2000 IG = 1, NG1                                                     RRTM_TAUMOL.175
            TAUG(IG,LAY) = COLH2O(LAY) *                                         RRTM_TAUMOL.176
     &          (FAC00(LAY) * ABSA(IND0,IG) +                                    RRTM_TAUMOL.177
     &           FAC10(LAY) * ABSA(IND0+1,IG) +                                  RRTM_TAUMOL.178
     &           FAC01(LAY) * ABSA(IND1,IG) +                                    RRTM_TAUMOL.179
     &           FAC11(LAY) * ABSA(IND1+1,IG) +                                  RRTM_TAUMOL.180
     &           SELFFAC(LAY) * (SELFREF(INDS,IG) +                              RRTM_TAUMOL.181
     &           SELFFRAC(LAY) *                                                 RRTM_TAUMOL.182
     &           (SELFREF(INDS+1,IG) - SELFREF(INDS,IG))) +                      RRTM_TAUMOL.183
     &           FORFAC(LAY) * FORREF(IG))                                       RRTM_TAUMOL.184
            PFRAC(IG,LAY) = FRACREFA(IG)                                         RRTM_TAUMOL.185
 2000    CONTINUE                                                                RRTM_TAUMOL.186
 2500 CONTINUE                                                                   RRTM_TAUMOL.187
                                                                                 RRTM_TAUMOL.188
      DO 3500 LAY = LAYTROP+1, NLAYERS                                           RRTM_TAUMOL.189
         IND0 = ((JP(LAY)-13)*5+(JT(LAY)-1))*NSPB(1) + 1                         RRTM_TAUMOL.190
         IND1 = ((JP(LAY)-12)*5+(JT1(LAY)-1))*NSPB(1) + 1                        RRTM_TAUMOL.191
         DO 3000 IG = 1, NG1                                                     RRTM_TAUMOL.192
            TAUG(IG,LAY) = COLH2O(LAY) *                                         RRTM_TAUMOL.193
     &          (FAC00(LAY) * ABSB(IND0,IG) +                                    RRTM_TAUMOL.194
     &           FAC10(LAY) * ABSB(IND0+1,IG) +                                  RRTM_TAUMOL.195
     &           FAC01(LAY) * ABSB(IND1,IG) +                                    RRTM_TAUMOL.196
     &           FAC11(LAY) * ABSB(IND1+1,IG) +                                  RRTM_TAUMOL.197
     &           FORFAC(LAY) * FORREF(IG))                                       RRTM_TAUMOL.198
            PFRAC(IG,LAY) = FRACREFB(IG)                                         RRTM_TAUMOL.199
 3000    CONTINUE                                                                RRTM_TAUMOL.200
 3500 CONTINUE                                                                   RRTM_TAUMOL.201
                                                                                 RRTM_TAUMOL.202
      RETURN                                                                     RRTM_TAUMOL.203
      END                                                                        RRTM_TAUMOL.204
                                                                                 RRTM_TAUMOL.205
C----------------------------------------------------------------------------    RRTM_TAUMOL.206
                                                                                 RRTM_TAUMOL.207
      SUBROUTINE TAUGB2                                                          RRTM_TAUMOL.208
                                                                                 RRTM_TAUMOL.209
C     BAND 2:  250-500 cm-1 (low - H2O; high - H2O)                              RRTM_TAUMOL.210
                                                                                 RRTM_TAUMOL.211
      PARAMETER (MXLAY=203, NBANDS=16)                                           RRTM_TAUMOL.212
      PARAMETER (NGPT=140, NG2=14, NGS1=8)                                       RRTM_TAUMOL.213
                                                                                 RRTM_TAUMOL.214
C  Output                                                                        RRTM_TAUMOL.215
                                                                                 RRTM_TAUMOL.216
      COMMON /TAUS/     TAUG(NGPT,MXLAY)                                         RRTM_TAUMOL.217
                                                                                 RRTM_TAUMOL.218
C  Input                                                                         RRTM_TAUMOL.219
                                                                                 RRTM_TAUMOL.220
      COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                     RRTM_TAUMOL.221
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       RRTM_TAUMOL.222
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  RRTM_TAUMOL.223
      COMMON /SPECIES/  COLDRY(MXLAY),WKL(35,MXLAY),WBRODL(MXLAY),               RRTM_TAUMOL.224
     &                  NMOL                                                     RRTM_TAUMOL.225
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                                 RRTM_TAUMOL.226
     &                  COLH2O(MXLAY),COLCO2(MXLAY),                             RRTM_TAUMOL.227
     &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),                RRTM_TAUMOL.228
     &                  COLO2(MXLAY),CO2MULT(MXLAY)                              RRTM_TAUMOL.229
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                               RRTM_TAUMOL.230
     &                  FAC10(MXLAY),FAC11(MXLAY)                                RRTM_TAUMOL.231
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                           RRTM_TAUMOL.232
      COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)          RRTM_TAUMOL.233
      COMMON /FOREIGN/  FORFAC(MXLAY)                                            RRTM_TAUMOL.234
      COMMON /K2C/      KA(5,13,NG2), KB(5,13:59,NG2),                           RRTM_TAUMOL.235
     &                  SELFREF(10,NG2), FORREF(NG2)                             RRTM_TAUMOL.236
      COMMON /PF2C/     FRACREFA(NG2,13), FRACREFB(NG2)                          RRTM_TAUMOL.237
      COMMON /SP/       PFRAC(NGPT,MXLAY)                                        RRTM_TAUMOL.238
      COMMON /TBG2TBL/  CORR1(0:200),CORR2(0:200)                                RRTM_TAUMOL.239
                                                                                 RRTM_TAUMOL.240
C This compiler directive was added to insure private common block storage       RRTM_TAUMOL.241
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_TAUMOL.242
C carry constants.                                                               RRTM_TAUMOL.243
C SGI                                                                            RRTM_TAUMOL.247
C$OMP THREADPRIVATE (/TAUS/,/PROFILE/,/SPECIES/,/PROFDATA/)                      RRTM_TAUMOL.248
C$OMP THREADPRIVATE (/INTFAC/,/INTIND/,/SELF/)                                   RRTM_TAUMOL.249
C$OMP THREADPRIVATE (/FOREIGN/,/SP/)                                             RRTM_TAUMOL.250
C ***                                                                            RRTM_TAUMOL.251
                                                                                 RRTM_TAUMOL.252
      DIMENSION ABSA(65,NG2),ABSB(235,NG2)                                       RRTM_TAUMOL.253
      DIMENSION FC00(MXLAY),FC01(MXLAY),FC10(MXLAY),FC11(MXLAY)                  RRTM_TAUMOL.254
      DIMENSION REFPARAM(13)                                                     RRTM_TAUMOL.255
      REAL KA,KB                                                                 RRTM_TAUMOL.256
      EQUIVALENCE (KA,ABSA),(KB,ABSB)                                            RRTM_TAUMOL.257
                                                                                 RRTM_TAUMOL.258
C     These are the mixing ratios for H2O for a MLS atmosphere at the            RRTM_TAUMOL.259
C     13 RRTM reference pressure levels:  1.8759999E-02, 1.2223309E-02,          RRTM_TAUMOL.260
C     5.8908667E-03, 2.7675382E-03, 1.4065107E-03, 7.5969833E-04,                RRTM_TAUMOL.261
C     3.8875898E-04, 1.6542293E-04, 3.7189537E-05, 7.4764857E-06,                RRTM_TAUMOL.262
C     4.3081886E-06, 3.3319423E-06, 3.2039343E-06/                               RRTM_TAUMOL.263
                                                                                 RRTM_TAUMOL.264
C     The following are parameters related to the reference water vapor          RRTM_TAUMOL.265
C     mixing ratios by REFPARAM(I) = REFH2O(I) / (.002+REFH2O(I)).               RRTM_TAUMOL.266
C     These parameters are used for the Planck function interpolation.           RRTM_TAUMOL.267
      DATA REFPARAM/                                                             RRTM_TAUMOL.268
     &  0.903661, 0.859386, 0.746542, 0.580496, 0.412889, 0.275283,              RRTM_TAUMOL.269
     &  0.162745, 7.63929E-02, 1.82553E-02, 3.72432E-03,                         RRTM_TAUMOL.270
     &  2.14946E-03, 1.66320E-03, 1.59940E-03/                                   RRTM_TAUMOL.271
                                                                                 RRTM_TAUMOL.272
C     Compute the optical depth by interpolating in ln(pressure) and             RRTM_TAUMOL.273
C     temperature.  Below LAYTROP, the water vapor self-continuum is             RRTM_TAUMOL.274
C     interpolated (in temperature) separately.                                  RRTM_TAUMOL.275
      DO 2500 LAY = 1, LAYTROP                                                   RRTM_TAUMOL.276
         WATER = 1.E20 * COLH2O(LAY) / COLDRY(LAY)                               RRTM_TAUMOL.277
         H2OPARAM = WATER/(WATER +.002)                                          RRTM_TAUMOL.278
         DO 1800 IFRAC = 2, 12                                                   RRTM_TAUMOL.279
            IF (H2OPARAM .GE. REFPARAM(IFRAC)) GO TO 1900                        RRTM_TAUMOL.280
 1800    CONTINUE                                                                RRTM_TAUMOL.281
 1900    CONTINUE                                                                RRTM_TAUMOL.282
         FRACINT = (H2OPARAM-REFPARAM(IFRAC))/                                   RRTM_TAUMOL.283
     &        (REFPARAM(IFRAC-1)-REFPARAM(IFRAC))                                RRTM_TAUMOL.284
                                                                                 RRTM_TAUMOL.285
         FP = FAC11(LAY) + FAC01(LAY)                                            RRTM_TAUMOL.286
         IFP = 2.E2*FP+0.5                                                       RRTM_TAUMOL.287
         IF (IFP.LE.0) IFP = 0                                                   RRTM_TAUMOL.288
         FC00(LAY) = FAC00(LAY) * CORR2(IFP)                                     RRTM_TAUMOL.289
         FC10(LAY) = FAC10(LAY) * CORR2(IFP)                                     RRTM_TAUMOL.290
         FC01(LAY) = FAC01(LAY) * CORR1(IFP)                                     RRTM_TAUMOL.291
         FC11(LAY) = FAC11(LAY) * CORR1(IFP)                                     RRTM_TAUMOL.292
         IND0 = ((JP(LAY)-1)*5+(JT(LAY)-1))*NSPA(2) + 1                          RRTM_TAUMOL.293
         IND1 = (JP(LAY)*5+(JT1(LAY)-1))*NSPA(2) + 1                             RRTM_TAUMOL.294
         INDS = INDSELF(LAY)                                                     RRTM_TAUMOL.295
         DO 2000 IG = 1, NG2                                                     RRTM_TAUMOL.296
            TAUG(NGS1+IG,LAY) = COLH2O(LAY) *                                    RRTM_TAUMOL.297
     &          (FC00(LAY) * ABSA(IND0,IG) +                                     RRTM_TAUMOL.298
     &           FC10(LAY) * ABSA(IND0+1,IG) +                                   RRTM_TAUMOL.299
     &           FC01(LAY) * ABSA(IND1,IG) +                                     RRTM_TAUMOL.300
     &           FC11(LAY) * ABSA(IND1+1,IG) +                                   RRTM_TAUMOL.301
     &           SELFFAC(LAY) * (SELFREF(INDS,IG) +                              RRTM_TAUMOL.302
     &           SELFFRAC(LAY) *                                                 RRTM_TAUMOL.303
     &           (SELFREF(INDS+1,IG) - SELFREF(INDS,IG))) +                      RRTM_TAUMOL.304
     &           FORFAC(LAY) * FORREF(IG))                                       RRTM_TAUMOL.305
            PFRAC(NGS1+IG,LAY) = FRACREFA(IG,IFRAC) + FRACINT *                  RRTM_TAUMOL.306
     &           (FRACREFA(IG,IFRAC-1)-FRACREFA(IG,IFRAC))                       RRTM_TAUMOL.307
 2000    CONTINUE                                                                RRTM_TAUMOL.308
 2500 CONTINUE                                                                   RRTM_TAUMOL.309
                                                                                 RRTM_TAUMOL.310
      DO 3500 LAY = LAYTROP+1, NLAYERS                                           RRTM_TAUMOL.311
         FP = FAC11(LAY) + FAC01(LAY)                                            RRTM_TAUMOL.312
         IFP = 2.E2*FP+0.5                                                       RRTM_TAUMOL.313
         IF (IFP.LE.0) IFP = 0                                                   RRTM_TAUMOL.314
         FC00(LAY) = FAC00(LAY) * CORR2(IFP)                                     RRTM_TAUMOL.315
         FC10(LAY) = FAC10(LAY) * CORR2(IFP)                                     RRTM_TAUMOL.316
         FC01(LAY) = FAC01(LAY) * CORR1(IFP)                                     RRTM_TAUMOL.317
         FC11(LAY) = FAC11(LAY) * CORR1(IFP)                                     RRTM_TAUMOL.318
         IND0 = ((JP(LAY)-13)*5+(JT(LAY)-1))*NSPB(2) + 1                         RRTM_TAUMOL.319
         IND1 = ((JP(LAY)-12)*5+(JT1(LAY)-1))*NSPB(2) + 1                        RRTM_TAUMOL.320
         DO 3000 IG = 1, NG2                                                     RRTM_TAUMOL.321
            TAUG(NGS1+IG,LAY) = COLH2O(LAY) *                                    RRTM_TAUMOL.322
     &          (FC00(LAY) * ABSB(IND0,IG) +                                     RRTM_TAUMOL.323
     &           FC10(LAY) * ABSB(IND0+1,IG) +                                   RRTM_TAUMOL.324
     &           FC01(LAY) * ABSB(IND1,IG) +                                     RRTM_TAUMOL.325
     &           FC11(LAY) * ABSB(IND1+1,IG) +                                   RRTM_TAUMOL.326
     &           FORFAC(LAY) * FORREF(IG))                                       RRTM_TAUMOL.327
            PFRAC(NGS1+IG,LAY) = FRACREFB(IG)                                    RRTM_TAUMOL.328
 3000    CONTINUE                                                                RRTM_TAUMOL.329
 3500 CONTINUE                                                                   RRTM_TAUMOL.330
                                                                                 RRTM_TAUMOL.331
      RETURN                                                                     RRTM_TAUMOL.332
      END                                                                        RRTM_TAUMOL.333
                                                                                 RRTM_TAUMOL.334
C----------------------------------------------------------------------------    RRTM_TAUMOL.335
                                                                                 RRTM_TAUMOL.336
      SUBROUTINE TAUGB3                                                          RRTM_TAUMOL.337
                                                                                 RRTM_TAUMOL.338
C     BAND 3:  500-630 cm-1 (low - H2O,CO2; high - H2O,CO2)                      RRTM_TAUMOL.339
                                                                                 RRTM_TAUMOL.340
      PARAMETER (MXLAY=203, NBANDS=16)                                           RRTM_TAUMOL.341
      PARAMETER (NGPT=140, NG3=16, NGS2=22)                                      RRTM_TAUMOL.342
                                                                                 RRTM_TAUMOL.343
C  Output                                                                        RRTM_TAUMOL.344
                                                                                 RRTM_TAUMOL.345
      COMMON /TAUS/     TAUG(NGPT,MXLAY)                                         RRTM_TAUMOL.346
                                                                                 RRTM_TAUMOL.347
C  Input                                                                         RRTM_TAUMOL.348
                                                                                 RRTM_TAUMOL.349
      COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                     RRTM_TAUMOL.350
      COMMON /PRECISE/  ONEMINUS                                                 RRTM_TAUMOL.351
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       RRTM_TAUMOL.352
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  RRTM_TAUMOL.353
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                                 RRTM_TAUMOL.354
     &                  COLH2O(MXLAY),COLCO2(MXLAY),                             RRTM_TAUMOL.355
     &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),                RRTM_TAUMOL.356
     &                  COLO2(MXLAY),CO2MULT(MXLAY)                              RRTM_TAUMOL.357
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                               RRTM_TAUMOL.358
     &                  FAC10(MXLAY),FAC11(MXLAY)                                RRTM_TAUMOL.359
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                           RRTM_TAUMOL.360
      COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)          RRTM_TAUMOL.361
      COMMON /FOREIGN/  FORFAC(MXLAY)                                            RRTM_TAUMOL.362
      COMMON /K3C/      KA(10,5,13,NG3), KB(5,5,13:59,NG3),                      RRTM_TAUMOL.363
     &                  SELFREF(10,NG3), FORREF(NG3),                            RRTM_TAUMOL.364
     &                  ABSN2OA(NG3), ABSN2OB(NG3)                               RRTM_TAUMOL.365
      COMMON /PF3C/     FRACREFA(NG3,10), FRACREFB(NG3,5)                        RRTM_TAUMOL.366
      COMMON /SP/       PFRAC(NGPT,MXLAY)                                        RRTM_TAUMOL.367
                                                                                 RRTM_TAUMOL.368
C This compiler directive was added to insure private common block storage       RRTM_TAUMOL.369
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_TAUMOL.370
C carry constants.                                                               RRTM_TAUMOL.371
C SGI                                                                            RRTM_TAUMOL.374
C$OMP THREADPRIVATE (/TAUS/,/PROFILE/,/PROFDATA/,/INTFAC/,/INTIND/)              RRTM_TAUMOL.375
C$OMP THREADPRIVATE (/SELF/,/FOREIGN/,/SP/)                                      RRTM_TAUMOL.376
C ***                                                                            RRTM_TAUMOL.377
                                                                                 RRTM_TAUMOL.378
      DIMENSION ABSA(650,NG3),ABSB(1175,NG3)                                     RRTM_TAUMOL.379
      DIMENSION H2OREF(59),CO2REF(59), ETAREF(10)                                RRTM_TAUMOL.380
      REAL KA,KB,N2OMULT,N2OREF(59)                                              RRTM_TAUMOL.381
      EQUIVALENCE (KA,ABSA),(KB,ABSB)                                            RRTM_TAUMOL.382
                                                                                 RRTM_TAUMOL.383
      DATA ETAREF/                                                               RRTM_TAUMOL.384
     &     0.,0.125,0.25,0.375,0.5,0.625,0.75,0.875,0.9875,1.0/                  RRTM_TAUMOL.385
      DATA H2OREF/                                                               RRTM_TAUMOL.386
     &     1.87599E-02,1.22233E-02,5.89086E-03,2.76753E-03,1.40651E-03,          RRTM_TAUMOL.387
     &     7.59698E-04,3.88758E-04,1.65422E-04,3.71895E-05,7.47648E-06,          RRTM_TAUMOL.388
     &     4.30818E-06,3.33194E-06,3.20393E-06,3.16186E-06,3.25235E-06,          RRTM_TAUMOL.389
     &     3.42258E-06,3.62884E-06,3.91482E-06,4.14875E-06,4.30810E-06,          RRTM_TAUMOL.390
     &     4.44204E-06,4.57783E-06,4.70865E-06,4.79432E-06,4.86971E-06,          RRTM_TAUMOL.391
     &     4.92603E-06,4.96688E-06,4.99628E-06,5.05266E-06,5.12658E-06,          RRTM_TAUMOL.392
     &     5.25028E-06,5.35708E-06,5.45085E-06,5.48304E-06,5.50000E-06,          RRTM_TAUMOL.393
     &     5.50000E-06,5.45359E-06,5.40468E-06,5.35576E-06,5.25327E-06,          RRTM_TAUMOL.394
     &     5.14362E-06,5.03396E-06,4.87662E-06,4.69787E-06,4.51911E-06,          RRTM_TAUMOL.395
     &     4.33600E-06,4.14416E-06,3.95232E-06,3.76048E-06,3.57217E-06,          RRTM_TAUMOL.396
     &     3.38549E-06,3.19881E-06,3.01212E-06,2.82621E-06,2.64068E-06,          RRTM_TAUMOL.397
     &     2.45515E-06,2.26962E-06,2.08659E-06,1.93029E-06/                      RRTM_TAUMOL.398
      DATA N2OREF/                                                               RRTM_TAUMOL.399
     &     3.20000E-07,3.20000E-07,3.20000E-07,3.20000E-07,3.20000E-07,          RRTM_TAUMOL.400
     &     3.19652E-07,3.15324E-07,3.03830E-07,2.94221E-07,2.84953E-07,          RRTM_TAUMOL.401
     &     2.76714E-07,2.64709E-07,2.42847E-07,2.09547E-07,1.71945E-07,          RRTM_TAUMOL.402
     &     1.37491E-07,1.13319E-07,1.00354E-07,9.12812E-08,8.54633E-08,          RRTM_TAUMOL.403
     &     8.03631E-08,7.33718E-08,6.59754E-08,5.60386E-08,4.70901E-08,          RRTM_TAUMOL.404
     &     3.99774E-08,3.29786E-08,2.60642E-08,2.10663E-08,1.65918E-08,          RRTM_TAUMOL.405
     &     1.30167E-08,1.00900E-08,7.62490E-09,6.11592E-09,4.66725E-09,          RRTM_TAUMOL.406
     &     3.28574E-09,2.84838E-09,2.46198E-09,2.07557E-09,1.85507E-09,          RRTM_TAUMOL.407
     &     1.65675E-09,1.45843E-09,1.31948E-09,1.20716E-09,1.09485E-09,          RRTM_TAUMOL.408
     &     9.97803E-10,9.31260E-10,8.64721E-10,7.98181E-10,7.51380E-10,          RRTM_TAUMOL.409
     &     7.13670E-10,6.75960E-10,6.38250E-10,6.09811E-10,5.85998E-10,          RRTM_TAUMOL.410
     &     5.62185E-10,5.38371E-10,5.15183E-10,4.98660E-10/                      RRTM_TAUMOL.411
      DATA CO2REF/                                                               RRTM_TAUMOL.412
     &     53*3.55E-04, 3.5470873E-04, 3.5427220E-04, 3.5383567E-04,             RRTM_TAUMOL.413
     &     3.5339911E-04, 3.5282588E-04, 3.5079606E-04/                          RRTM_TAUMOL.414
                                                                                 RRTM_TAUMOL.415
      STRRAT = 1.19268                                                           RRTM_TAUMOL.416
                                                                                 RRTM_TAUMOL.417
C     Compute the optical depth by interpolating in ln(pressure),                RRTM_TAUMOL.418
C     temperature, and appropriate species.  Below LAYTROP, the water            RRTM_TAUMOL.419
C     vapor self-continuum is interpolated (in temperature) separately.          RRTM_TAUMOL.420
CDIR$ NOVECTOR                                                                   RRTM_TAUMOL.421
      DO 2500 LAY = 1, LAYTROP                                                   RRTM_TAUMOL.422
         SPECCOMB = COLH2O(LAY) + STRRAT*COLCO2(LAY)                             RRTM_TAUMOL.423
         SPECPARM = COLH2O(LAY)/SPECCOMB                                         RRTM_TAUMOL.424
         IF (SPECPARM .GE. ONEMINUS) SPECPARM = ONEMINUS                         RRTM_TAUMOL.425
         SPECMULT = 8.*(SPECPARM)                                                RRTM_TAUMOL.426
         JS = 1 + INT(SPECMULT)                                                  RRTM_TAUMOL.427
         FS = AMOD(SPECMULT,1.0)                                                 RRTM_TAUMOL.428
         IF (JS .EQ. 8) THEN                                                     RRTM_TAUMOL.429
            IF (FS .GE. 0.9) THEN                                                RRTM_TAUMOL.430
               JS = 9                                                            RRTM_TAUMOL.431
               FS = 10. * (FS - 0.9)                                             RRTM_TAUMOL.432
            ELSE                                                                 RRTM_TAUMOL.433
               FS = FS/0.9                                                       RRTM_TAUMOL.434
            ENDIF                                                                RRTM_TAUMOL.435
         ENDIF                                                                   RRTM_TAUMOL.436
         NS = JS + INT(FS + 0.5)                                                 RRTM_TAUMOL.437
         FP = FAC01(LAY) + FAC11(LAY)                                            RRTM_TAUMOL.438
         FAC000 = (1. - FS) * FAC00(LAY)                                         RRTM_TAUMOL.439
         FAC010 = (1. - FS) * FAC10(LAY)                                         RRTM_TAUMOL.440
         FAC100 = FS * FAC00(LAY)                                                RRTM_TAUMOL.441
         FAC110 = FS * FAC10(LAY)                                                RRTM_TAUMOL.442
         FAC001 = (1. - FS) * FAC01(LAY)                                         RRTM_TAUMOL.443
         FAC011 = (1. - FS) * FAC11(LAY)                                         RRTM_TAUMOL.444
         FAC101 = FS * FAC01(LAY)                                                RRTM_TAUMOL.445
         FAC111 = FS * FAC11(LAY)                                                RRTM_TAUMOL.446
         IND0 = ((JP(LAY)-1)*5+(JT(LAY)-1))*NSPA(3) + JS                         RRTM_TAUMOL.447
         IND1 = (JP(LAY)*5+(JT1(LAY)-1))*NSPA(3) + JS                            RRTM_TAUMOL.448
         INDS = INDSELF(LAY)                                                     RRTM_TAUMOL.449
         COLREF1 = N2OREF(JP(LAY))                                               RRTM_TAUMOL.450
         COLREF2 = N2OREF(JP(LAY)+1)                                             RRTM_TAUMOL.451
         IF (NS .EQ. 10) THEN                                                    RRTM_TAUMOL.452
            WCOMB1 = H2OREF(JP(LAY))                                             RRTM_TAUMOL.453
            WCOMB2 = H2OREF(JP(LAY)+1)                                           RRTM_TAUMOL.454
         ELSE                                                                    RRTM_TAUMOL.455
            WCOMB1 = STRRAT * CO2REF(JP(LAY))/(1.-ETAREF(NS))                    RRTM_TAUMOL.456
            WCOMB2 = STRRAT * CO2REF(JP(LAY)+1)/(1.-ETAREF(NS))                  RRTM_TAUMOL.457
         ENDIF                                                                   RRTM_TAUMOL.458
         RATIO = (COLREF1/WCOMB1)+FP*((COLREF2/WCOMB2)-(COLREF1/WCOMB1))         RRTM_TAUMOL.459
         CURRN2O = SPECCOMB * RATIO                                              RRTM_TAUMOL.460
         N2OMULT = COLN2O(LAY) - CURRN2O                                         RRTM_TAUMOL.461
CDIR$ VECTOR                                                                     RRTM_TAUMOL.462
         DO 2000 IG = 1, NG3                                                     RRTM_TAUMOL.463
            TAUG(NGS2+IG,LAY) = SPECCOMB *                                       RRTM_TAUMOL.464
     &          (FAC000 * ABSA(IND0,IG) +                                        RRTM_TAUMOL.465
     &           FAC100 * ABSA(IND0+1,IG) +                                      RRTM_TAUMOL.466
     &           FAC010 * ABSA(IND0+10,IG) +                                     RRTM_TAUMOL.467
     &           FAC110 * ABSA(IND0+11,IG) +                                     RRTM_TAUMOL.468
     &           FAC001 * ABSA(IND1,IG) +                                        RRTM_TAUMOL.469
     &           FAC101 * ABSA(IND1+1,IG) +                                      RRTM_TAUMOL.470
     &           FAC011 * ABSA(IND1+10,IG) +                                     RRTM_TAUMOL.471
     &           FAC111 * ABSA(IND1+11,IG)) +                                    RRTM_TAUMOL.472
     &           COLH2O(LAY) *                                                   RRTM_TAUMOL.473
     &           (SELFFAC(LAY) * (SELFREF(INDS,IG) +                             RRTM_TAUMOL.474
     &           SELFFRAC(LAY) *                                                 RRTM_TAUMOL.475
     &           (SELFREF(INDS+1,IG) - SELFREF(INDS,IG))) +                      RRTM_TAUMOL.476
     &           FORFAC(LAY) * FORREF(IG))                                       RRTM_TAUMOL.477
     &           + N2OMULT * ABSN2OA(IG)                                         RRTM_TAUMOL.478
            PFRAC(NGS2+IG,LAY) = FRACREFA(IG,JS) + FS *                          RRTM_TAUMOL.479
     &           (FRACREFA(IG,JS+1) - FRACREFA(IG,JS))                           RRTM_TAUMOL.480
 2000    CONTINUE                                                                RRTM_TAUMOL.481
 2500 CONTINUE                                                                   RRTM_TAUMOL.482
                                                                                 RRTM_TAUMOL.483
CDIR$ NOVECTOR                                                                   RRTM_TAUMOL.484
      DO 3500 LAY = LAYTROP+1, NLAYERS                                           RRTM_TAUMOL.485
         SPECCOMB = COLH2O(LAY) + STRRAT*COLCO2(LAY)                             RRTM_TAUMOL.486
         SPECPARM = COLH2O(LAY)/SPECCOMB                                         RRTM_TAUMOL.487
         IF (SPECPARM .GE. ONEMINUS) SPECPARM = ONEMINUS                         RRTM_TAUMOL.488
         SPECMULT = 4.*(SPECPARM)                                                RRTM_TAUMOL.489
         JS = 1 + INT(SPECMULT)                                                  RRTM_TAUMOL.490
         FS = AMOD(SPECMULT,1.0)                                                 RRTM_TAUMOL.491
         NS = JS + INT(FS + 0.5)                                                 RRTM_TAUMOL.492
         FP = FAC01(LAY) + FAC11(LAY)                                            RRTM_TAUMOL.493
         FAC000 = (1. - FS) * FAC00(LAY)                                         RRTM_TAUMOL.494
         FAC010 = (1. - FS) * FAC10(LAY)                                         RRTM_TAUMOL.495
         FAC100 = FS * FAC00(LAY)                                                RRTM_TAUMOL.496
         FAC110 = FS * FAC10(LAY)                                                RRTM_TAUMOL.497
         FAC001 = (1. - FS) * FAC01(LAY)                                         RRTM_TAUMOL.498
         FAC011 = (1. - FS) * FAC11(LAY)                                         RRTM_TAUMOL.499
         FAC101 = FS * FAC01(LAY)                                                RRTM_TAUMOL.500
         FAC111 = FS * FAC11(LAY)                                                RRTM_TAUMOL.501
         IND0 = ((JP(LAY)-13)*5+(JT(LAY)-1))*NSPB(3) + JS                        RRTM_TAUMOL.502
         IND1 = ((JP(LAY)-12)*5+(JT1(LAY)-1))*NSPB(3) + JS                       RRTM_TAUMOL.503
         COLREF1 = N2OREF(JP(LAY))                                               RRTM_TAUMOL.504
         COLREF2 = N2OREF(JP(LAY)+1)                                             RRTM_TAUMOL.505
         IF (NS .EQ. 5) THEN                                                     RRTM_TAUMOL.506
            WCOMB1 = H2OREF(JP(LAY))                                             RRTM_TAUMOL.507
            WCOMB2 = H2OREF(JP(LAY)+1)                                           RRTM_TAUMOL.508
         ELSE                                                                    RRTM_TAUMOL.509
            WCOMB1 = STRRAT * CO2REF(JP(LAY))/(1.-ETAREF(NS))                    RRTM_TAUMOL.510
            WCOMB2 = STRRAT * CO2REF(JP(LAY)+1)/(1.-ETAREF(NS))                  RRTM_TAUMOL.511
         ENDIF                                                                   RRTM_TAUMOL.512
         RATIO = (COLREF1/WCOMB1)+FP*((COLREF2/WCOMB2)-(COLREF1/WCOMB1))         RRTM_TAUMOL.513
         CURRN2O = SPECCOMB * RATIO                                              RRTM_TAUMOL.514
         N2OMULT = COLN2O(LAY) - CURRN2O                                         RRTM_TAUMOL.515
CDIR$ VECTOR                                                                     RRTM_TAUMOL.516
         DO 3000 IG = 1, NG3                                                     RRTM_TAUMOL.517
            TAUG(NGS2+IG,LAY) = SPECCOMB *                                       RRTM_TAUMOL.518
     &          (FAC000 * ABSB(IND0,IG) +                                        RRTM_TAUMOL.519
     &           FAC100 * ABSB(IND0+1,IG) +                                      RRTM_TAUMOL.520
     &           FAC010 * ABSB(IND0+5,IG) +                                      RRTM_TAUMOL.521
     &           FAC110 * ABSB(IND0+6,IG) +                                      RRTM_TAUMOL.522
     &           FAC001 * ABSB(IND1,IG) +                                        RRTM_TAUMOL.523
     &           FAC101 * ABSB(IND1+1,IG) +                                      RRTM_TAUMOL.524
     &           FAC011 * ABSB(IND1+5,IG) +                                      RRTM_TAUMOL.525
     &           FAC111 * ABSB(IND1+6,IG)) +                                     RRTM_TAUMOL.526
     &           COLH2O(LAY) * FORFAC(LAY) * FORREF(IG)                          RRTM_TAUMOL.527
     &           + N2OMULT * ABSN2OB(IG)                                         RRTM_TAUMOL.528
            PFRAC(NGS2+IG,LAY) = FRACREFB(IG,JS) + FS *                          RRTM_TAUMOL.529
     &           (FRACREFB(IG,JS+1) - FRACREFB(IG,JS))                           RRTM_TAUMOL.530
 3000    CONTINUE                                                                RRTM_TAUMOL.531
 3500 CONTINUE                                                                   RRTM_TAUMOL.532
                                                                                 RRTM_TAUMOL.533
      RETURN                                                                     RRTM_TAUMOL.534
      END                                                                        RRTM_TAUMOL.535
                                                                                 RRTM_TAUMOL.536
C----------------------------------------------------------------------------    RRTM_TAUMOL.537
                                                                                 RRTM_TAUMOL.538
      SUBROUTINE TAUGB4                                                          RRTM_TAUMOL.539
                                                                                 RRTM_TAUMOL.540
C     BAND 4:  630-700 cm-1 (low - H2O,CO2; high - O3,CO2)                       RRTM_TAUMOL.541
                                                                                 RRTM_TAUMOL.542
      PARAMETER (MXLAY=203, NBANDS=16)                                           RRTM_TAUMOL.543
      PARAMETER (NGPT=140, NG4=14, NGS3=38)                                      RRTM_TAUMOL.544
                                                                                 RRTM_TAUMOL.545
C  Output                                                                        RRTM_TAUMOL.546
                                                                                 RRTM_TAUMOL.547
      COMMON /TAUS/     TAUG(NGPT,MXLAY)                                         RRTM_TAUMOL.548
                                                                                 RRTM_TAUMOL.549
C  Input                                                                         RRTM_TAUMOL.550
                                                                                 RRTM_TAUMOL.551
      COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                     RRTM_TAUMOL.552
      COMMON /PRECISE/  ONEMINUS                                                 RRTM_TAUMOL.553
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       RRTM_TAUMOL.554
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  RRTM_TAUMOL.555
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                                 RRTM_TAUMOL.556
     &                  COLH2O(MXLAY),COLCO2(MXLAY),                             RRTM_TAUMOL.557
     &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),                RRTM_TAUMOL.558
     &                  COLO2(MXLAY),CO2MULT(MXLAY)                              RRTM_TAUMOL.559
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                               RRTM_TAUMOL.560
     &                  FAC10(MXLAY),FAC11(MXLAY)                                RRTM_TAUMOL.561
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                           RRTM_TAUMOL.562
      COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)          RRTM_TAUMOL.563
      COMMON /K4C/      KA(9,5,13,NG4), KB(6,5,13:59,NG4),                       RRTM_TAUMOL.564
     &                  SELFREF(10,NG4)                                          RRTM_TAUMOL.565
      COMMON /PF4C/     FRACREFA(NG4,9), FRACREFB(NG4,6)                         RRTM_TAUMOL.566
      COMMON /SP/       PFRAC(NGPT,MXLAY)                                        RRTM_TAUMOL.567
                                                                                 RRTM_TAUMOL.568
C This compiler directive was added to insure private common block storage       RRTM_TAUMOL.569
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_TAUMOL.570
C carry constants.                                                               RRTM_TAUMOL.571
C SGI                                                                            RRTM_TAUMOL.574
C$OMP THREADPRIVATE (/TAUS/,/PROFILE/,/PROFDATA/,/INTFAC/,/INTIND/)              RRTM_TAUMOL.575
C$OMP THREADPRIVATE (/SELF/,/SP/)                                                RRTM_TAUMOL.576
C ***                                                                            RRTM_TAUMOL.577
                                                                                 RRTM_TAUMOL.578
      DIMENSION ABSA(585,NG4),ABSB(1410,NG4)                                     RRTM_TAUMOL.579
      REAL KA,KB                                                                 RRTM_TAUMOL.580
      EQUIVALENCE (KA,ABSA),(KB,ABSB)                                            RRTM_TAUMOL.581
                                                                                 RRTM_TAUMOL.582
      STRRAT1 = 850.577                                                          RRTM_TAUMOL.583
      STRRAT2 = 35.7416                                                          RRTM_TAUMOL.584
                                                                                 RRTM_TAUMOL.585
C     Compute the optical depth by interpolating in ln(pressure),                RRTM_TAUMOL.586
C     temperature, and appropriate species.  Below LAYTROP, the water            RRTM_TAUMOL.587
C     vapor self-continuum is interpolated (in temperature) separately.          RRTM_TAUMOL.588
CDIR$ NOVECTOR                                                                   RRTM_TAUMOL.589
      DO 2500 LAY = 1, LAYTROP                                                   RRTM_TAUMOL.590
         SPECCOMB = COLH2O(LAY) + STRRAT1*COLCO2(LAY)                            RRTM_TAUMOL.591
         SPECPARM = COLH2O(LAY)/SPECCOMB                                         RRTM_TAUMOL.592
         IF (SPECPARM .GE. ONEMINUS) SPECPARM = ONEMINUS                         RRTM_TAUMOL.593
         SPECMULT = 8.*(SPECPARM)                                                RRTM_TAUMOL.594
         JS = 1 + INT(SPECMULT)                                                  RRTM_TAUMOL.595
         FS = AMOD(SPECMULT,1.0)                                                 RRTM_TAUMOL.596
         FAC000 = (1. - FS) * FAC00(LAY)                                         RRTM_TAUMOL.597
         FAC010 = (1. - FS) * FAC10(LAY)                                         RRTM_TAUMOL.598
         FAC100 = FS * FAC00(LAY)                                                RRTM_TAUMOL.599
         FAC110 = FS * FAC10(LAY)                                                RRTM_TAUMOL.600
         FAC001 = (1. - FS) * FAC01(LAY)                                         RRTM_TAUMOL.601
         FAC011 = (1. - FS) * FAC11(LAY)                                         RRTM_TAUMOL.602
         FAC101 = FS * FAC01(LAY)                                                RRTM_TAUMOL.603
         FAC111 = FS * FAC11(LAY)                                                RRTM_TAUMOL.604
         IND0 = ((JP(LAY)-1)*5+(JT(LAY)-1))*NSPA(4) + JS                         RRTM_TAUMOL.605
         IND1 = (JP(LAY)*5+(JT1(LAY)-1))*NSPA(4) + JS                            RRTM_TAUMOL.606
         INDS = INDSELF(LAY)                                                     RRTM_TAUMOL.607
CDIR$ VECTOR                                                                     RRTM_TAUMOL.608
         DO 2000 IG = 1, NG4                                                     RRTM_TAUMOL.609
            TAUG(NGS3+IG,LAY) = SPECCOMB *                                       RRTM_TAUMOL.610
     &          (FAC000 * ABSA(IND0,IG) +                                        RRTM_TAUMOL.611
     &           FAC100 * ABSA(IND0+1,IG) +                                      RRTM_TAUMOL.612
     &           FAC010 * ABSA(IND0+9,IG) +                                      RRTM_TAUMOL.613
     &           FAC110 * ABSA(IND0+10,IG) +                                     RRTM_TAUMOL.614
     &           FAC001 * ABSA(IND1,IG) +                                        RRTM_TAUMOL.615
     &           FAC101 * ABSA(IND1+1,IG) +                                      RRTM_TAUMOL.616
     &           FAC011 * ABSA(IND1+9,IG) +                                      RRTM_TAUMOL.617
     &           FAC111 * ABSA(IND1+10,IG)) +                                    RRTM_TAUMOL.618
     &           COLH2O(LAY) *                                                   RRTM_TAUMOL.619
     &           SELFFAC(LAY) * (SELFREF(INDS,IG) +                              RRTM_TAUMOL.620
     &           SELFFRAC(LAY) *                                                 RRTM_TAUMOL.621
     &           (SELFREF(INDS+1,IG) - SELFREF(INDS,IG)))                        RRTM_TAUMOL.622
            PFRAC(NGS3+IG,LAY) = FRACREFA(IG,JS) + FS *                          RRTM_TAUMOL.623
     &           (FRACREFA(IG,JS+1) - FRACREFA(IG,JS))                           RRTM_TAUMOL.624
 2000    CONTINUE                                                                RRTM_TAUMOL.625
 2500 CONTINUE                                                                   RRTM_TAUMOL.626
                                                                                 RRTM_TAUMOL.627
CDIR$ NOVECTOR                                                                   RRTM_TAUMOL.628
      DO 3500 LAY = LAYTROP+1, NLAYERS                                           RRTM_TAUMOL.629
         SPECCOMB = COLO3(LAY) + STRRAT2*COLCO2(LAY)                             RRTM_TAUMOL.630
         SPECPARM = COLO3(LAY)/SPECCOMB                                          RRTM_TAUMOL.631
         IF (SPECPARM .GE. ONEMINUS) SPECPARM = ONEMINUS                         RRTM_TAUMOL.632
         SPECMULT = 4.*(SPECPARM)                                                RRTM_TAUMOL.633
         JS = 1 + INT(SPECMULT)                                                  RRTM_TAUMOL.634
         FS = AMOD(SPECMULT,1.0)                                                 RRTM_TAUMOL.635
         IF (JS .GT. 1) THEN                                                     RRTM_TAUMOL.636
            JS = JS + 1                                                          RRTM_TAUMOL.637
         ELSEIF (FS .GE. 0.0024) THEN                                            RRTM_TAUMOL.638
            JS = 2                                                               RRTM_TAUMOL.639
            FS = (FS - 0.0024)/0.9976                                            RRTM_TAUMOL.640
         ELSE                                                                    RRTM_TAUMOL.641
            JS = 1                                                               RRTM_TAUMOL.642
            FS = FS/0.0024                                                       RRTM_TAUMOL.643
         ENDIF                                                                   RRTM_TAUMOL.644
         FAC000 = (1. - FS) * FAC00(LAY)                                         RRTM_TAUMOL.645
         FAC010 = (1. - FS) * FAC10(LAY)                                         RRTM_TAUMOL.646
         FAC100 = FS * FAC00(LAY)                                                RRTM_TAUMOL.647
         FAC110 = FS * FAC10(LAY)                                                RRTM_TAUMOL.648
         FAC001 = (1. - FS) * FAC01(LAY)                                         RRTM_TAUMOL.649
         FAC011 = (1. - FS) * FAC11(LAY)                                         RRTM_TAUMOL.650
         FAC101 = FS * FAC01(LAY)                                                RRTM_TAUMOL.651
         FAC111 = FS * FAC11(LAY)                                                RRTM_TAUMOL.652
         IND0 = ((JP(LAY)-13)*5+(JT(LAY)-1))*NSPB(4) + JS                        RRTM_TAUMOL.653
         IND1 = ((JP(LAY)-12)*5+(JT1(LAY)-1))*NSPB(4) + JS                       RRTM_TAUMOL.654
CDIR$ VECTOR                                                                     RRTM_TAUMOL.655
         DO 3000 IG = 1, NG4                                                     RRTM_TAUMOL.656
            TAUG(NGS3+IG,LAY) = SPECCOMB *                                       RRTM_TAUMOL.657
     &          (FAC000 * ABSB(IND0,IG) +                                        RRTM_TAUMOL.658
     &           FAC100 * ABSB(IND0+1,IG) +                                      RRTM_TAUMOL.659
     &           FAC010 * ABSB(IND0+6,IG) +                                      RRTM_TAUMOL.660
     &           FAC110 * ABSB(IND0+7,IG) +                                      RRTM_TAUMOL.661
     &           FAC001 * ABSB(IND1,IG) +                                        RRTM_TAUMOL.662
     &           FAC101 * ABSB(IND1+1,IG) +                                      RRTM_TAUMOL.663
     &           FAC011 * ABSB(IND1+6,IG) +                                      RRTM_TAUMOL.664
     &           FAC111 * ABSB(IND1+7,IG))                                       RRTM_TAUMOL.665
            PFRAC(NGS3+IG,LAY) = FRACREFB(IG,JS) + FS *                          RRTM_TAUMOL.666
     &           (FRACREFB(IG,JS+1) - FRACREFB(IG,JS))                           RRTM_TAUMOL.667
 3000    CONTINUE                                                                RRTM_TAUMOL.668
 3500 CONTINUE                                                                   RRTM_TAUMOL.669
                                                                                 RRTM_TAUMOL.670
      RETURN                                                                     RRTM_TAUMOL.671
      END                                                                        RRTM_TAUMOL.672
                                                                                 RRTM_TAUMOL.673
C----------------------------------------------------------------------------    RRTM_TAUMOL.674
                                                                                 RRTM_TAUMOL.675
      SUBROUTINE TAUGB5                                                          RRTM_TAUMOL.676
                                                                                 RRTM_TAUMOL.677
C     BAND 5:  700-820 cm-1 (low - H2O,CO2; high - O3,CO2)                       RRTM_TAUMOL.678
                                                                                 RRTM_TAUMOL.679
      PARAMETER (MXLAY=203, MAXXSEC=4, NBANDS=16)                                RRTM_TAUMOL.680
      PARAMETER (NGPT=140, NG5=16, NGS4=52)                                      RRTM_TAUMOL.681
                                                                                 RRTM_TAUMOL.682
C  Output                                                                        RRTM_TAUMOL.683
                                                                                 RRTM_TAUMOL.684
      COMMON /TAUS/     TAUG(NGPT,MXLAY)                                         RRTM_TAUMOL.685
                                                                                 RRTM_TAUMOL.686
C  Input                                                                         RRTM_TAUMOL.687
                                                                                 RRTM_TAUMOL.688
      COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                     RRTM_TAUMOL.689
      COMMON /PRECISE/  ONEMINUS                                                 RRTM_TAUMOL.690
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       RRTM_TAUMOL.691
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  RRTM_TAUMOL.692
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                                 RRTM_TAUMOL.693
     &                  COLH2O(MXLAY),COLCO2(MXLAY),                             RRTM_TAUMOL.694
     &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),                RRTM_TAUMOL.695
     &                  COLO2(MXLAY),CO2MULT(MXLAY)                              RRTM_TAUMOL.696
      COMMON /XSEC/     WX(MAXXSEC,MXLAY)                                        RRTM_TAUMOL.697
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                               RRTM_TAUMOL.698
     &                  FAC10(MXLAY),FAC11(MXLAY)                                RRTM_TAUMOL.699
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                           RRTM_TAUMOL.700
      COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)          RRTM_TAUMOL.701
      COMMON /K5C/      KA(9,5,13,NG5), KB(5,5,13:59,NG5),                       RRTM_TAUMOL.702
     &                  SELFREF(10,NG5),                                         RRTM_TAUMOL.703
     &                  CCL4(NG5)                                                RRTM_TAUMOL.704
      COMMON /PF5C/     FRACREFA(NG5,9), FRACREFB(NG5,5)                         RRTM_TAUMOL.705
      COMMON /SP/       PFRAC(NGPT,MXLAY)                                        RRTM_TAUMOL.706
                                                                                 RRTM_TAUMOL.707
C This compiler directive was added to insure private common block storage       RRTM_TAUMOL.708
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_TAUMOL.709
C carry constants.                                                               RRTM_TAUMOL.710
C SGI                                                                            RRTM_TAUMOL.713
C$OMP THREADPRIVATE (/TAUS/,/PROFILE/,/PROFDATA/,/XSEC/,/INTFAC/)                RRTM_TAUMOL.714
C$OMP THREADPRIVATE (/INTIND/,/SELF/,/SP/)                                       RRTM_TAUMOL.715
C ***                                                                            RRTM_TAUMOL.716
                                                                                 RRTM_TAUMOL.717
      DIMENSION ABSA(585,NG5),ABSB(1175,NG5)                                     RRTM_TAUMOL.718
      REAL KA,KB                                                                 RRTM_TAUMOL.719
      EQUIVALENCE (KA,ABSA),(KB,ABSB)                                            RRTM_TAUMOL.720
                                                                                 RRTM_TAUMOL.721
      STRRAT1 = 90.4894                                                          RRTM_TAUMOL.722
      STRRAT2 = 0.900502                                                         RRTM_TAUMOL.723
                                                                                 RRTM_TAUMOL.724
C     Compute the optical depth by interpolating in ln(pressure),                RRTM_TAUMOL.725
C     temperature, and appropriate species.  Below LAYTROP, the water            RRTM_TAUMOL.726
C     vapor self-continuum is interpolated (in temperature) separately.          RRTM_TAUMOL.727
CDIR$ NOVECTOR                                                                   RRTM_TAUMOL.728
      DO 2500 LAY = 1, LAYTROP                                                   RRTM_TAUMOL.729
         SPECCOMB = COLH2O(LAY) + STRRAT1*COLCO2(LAY)                            RRTM_TAUMOL.730
         SPECPARM = COLH2O(LAY)/SPECCOMB                                         RRTM_TAUMOL.731
         IF (SPECPARM .GE. ONEMINUS) SPECPARM = ONEMINUS                         RRTM_TAUMOL.732
         SPECMULT = 8.*(SPECPARM)                                                RRTM_TAUMOL.733
         JS = 1 + INT(SPECMULT)                                                  RRTM_TAUMOL.734
         FS = AMOD(SPECMULT,1.0)                                                 RRTM_TAUMOL.735
         FAC000 = (1. - FS) * FAC00(LAY)                                         RRTM_TAUMOL.736
         FAC010 = (1. - FS) * FAC10(LAY)                                         RRTM_TAUMOL.737
         FAC100 = FS * FAC00(LAY)                                                RRTM_TAUMOL.738
         FAC110 = FS * FAC10(LAY)                                                RRTM_TAUMOL.739
         FAC001 = (1. - FS) * FAC01(LAY)                                         RRTM_TAUMOL.740
         FAC011 = (1. - FS) * FAC11(LAY)                                         RRTM_TAUMOL.741
         FAC101 = FS * FAC01(LAY)                                                RRTM_TAUMOL.742
         FAC111 = FS * FAC11(LAY)                                                RRTM_TAUMOL.743
         IND0 = ((JP(LAY)-1)*5+(JT(LAY)-1))*NSPA(5) + JS                         RRTM_TAUMOL.744
         IND1 = (JP(LAY)*5+(JT1(LAY)-1))*NSPA(5) + JS                            RRTM_TAUMOL.745
         INDS = INDSELF(LAY)                                                     RRTM_TAUMOL.746
CDIR$ VECTOR                                                                     RRTM_TAUMOL.747
         DO 2000 IG = 1, NG5                                                     RRTM_TAUMOL.748
            TAUG(NGS4+IG,LAY) = SPECCOMB *                                       RRTM_TAUMOL.749
     &          (FAC000 * ABSA(IND0,IG) +                                        RRTM_TAUMOL.750
     &           FAC100 * ABSA(IND0+1,IG) +                                      RRTM_TAUMOL.751
     &           FAC010 * ABSA(IND0+9,IG) +                                      RRTM_TAUMOL.752
     &           FAC110 * ABSA(IND0+10,IG) +                                     RRTM_TAUMOL.753
     &           FAC001 * ABSA(IND1,IG) +                                        RRTM_TAUMOL.754
     &           FAC101 * ABSA(IND1+1,IG) +                                      RRTM_TAUMOL.755
     &           FAC011 * ABSA(IND1+9,IG) +                                      RRTM_TAUMOL.756
     &           FAC111 * ABSA(IND1+10,IG)) +                                    RRTM_TAUMOL.757
     &           COLH2O(LAY) *                                                   RRTM_TAUMOL.758
     &           SELFFAC(LAY) * (SELFREF(INDS,IG) +                              RRTM_TAUMOL.759
     &           SELFFRAC(LAY) *                                                 RRTM_TAUMOL.760
     &           (SELFREF(INDS+1,IG) - SELFREF(INDS,IG)))                        RRTM_TAUMOL.761
     &           + WX(1,LAY) * CCL4(IG)                                          RRTM_TAUMOL.762
            PFRAC(NGS4+IG,LAY) = FRACREFA(IG,JS) + FS *                          RRTM_TAUMOL.763
     &           (FRACREFA(IG,JS+1) - FRACREFA(IG,JS))                           RRTM_TAUMOL.764
 2000    CONTINUE                                                                RRTM_TAUMOL.765
 2500 CONTINUE                                                                   RRTM_TAUMOL.766
                                                                                 RRTM_TAUMOL.767
CDIR$ NOVECTOR                                                                   RRTM_TAUMOL.768
      DO 3500 LAY = LAYTROP+1, NLAYERS                                           RRTM_TAUMOL.769
         SPECCOMB = COLO3(LAY) + STRRAT2*COLCO2(LAY)                             RRTM_TAUMOL.770
         SPECPARM = COLO3(LAY)/SPECCOMB                                          RRTM_TAUMOL.771
         IF (SPECPARM .GE. ONEMINUS) SPECPARM = ONEMINUS                         RRTM_TAUMOL.772
         SPECMULT = 4.*(SPECPARM)                                                RRTM_TAUMOL.773
         JS = 1 + INT(SPECMULT)                                                  RRTM_TAUMOL.774
         FS = AMOD(SPECMULT,1.0)                                                 RRTM_TAUMOL.775
         FAC000 = (1. - FS) * FAC00(LAY)                                         RRTM_TAUMOL.776
         FAC010 = (1. - FS) * FAC10(LAY)                                         RRTM_TAUMOL.777
         FAC100 = FS * FAC00(LAY)                                                RRTM_TAUMOL.778
         FAC110 = FS * FAC10(LAY)                                                RRTM_TAUMOL.779
         FAC001 = (1. - FS) * FAC01(LAY)                                         RRTM_TAUMOL.780
         FAC011 = (1. - FS) * FAC11(LAY)                                         RRTM_TAUMOL.781
         FAC101 = FS * FAC01(LAY)                                                RRTM_TAUMOL.782
         FAC111 = FS * FAC11(LAY)                                                RRTM_TAUMOL.783
         IND0 = ((JP(LAY)-13)*5+(JT(LAY)-1))*NSPB(5) + JS                        RRTM_TAUMOL.784
         IND1 = ((JP(LAY)-12)*5+(JT1(LAY)-1))*NSPB(5) + JS                       RRTM_TAUMOL.785
CDIR$ VECTOR                                                                     RRTM_TAUMOL.786
         DO 3000 IG = 1, NG5                                                     RRTM_TAUMOL.787
            TAUG(NGS4+IG,LAY) = SPECCOMB *                                       RRTM_TAUMOL.788
     &          (FAC000 * ABSB(IND0,IG) +                                        RRTM_TAUMOL.789
     &           FAC100 * ABSB(IND0+1,IG) +                                      RRTM_TAUMOL.790
     &           FAC010 * ABSB(IND0+5,IG) +                                      RRTM_TAUMOL.791
     &           FAC110 * ABSB(IND0+6,IG) +                                      RRTM_TAUMOL.792
     &           FAC001 * ABSB(IND1,IG) +                                        RRTM_TAUMOL.793
     &           FAC101 * ABSB(IND1+1,IG) +                                      RRTM_TAUMOL.794
     &           FAC011 * ABSB(IND1+5,IG) +                                      RRTM_TAUMOL.795
     &           FAC111 * ABSB(IND1+6,IG))                                       RRTM_TAUMOL.796
     &           + WX(1,LAY) * CCL4(IG)                                          RRTM_TAUMOL.797
            PFRAC(NGS4+IG,LAY) = FRACREFB(IG,JS) + FS *                          RRTM_TAUMOL.798
     &           (FRACREFB(IG,JS+1) - FRACREFB(IG,JS))                           RRTM_TAUMOL.799
 3000    CONTINUE                                                                RRTM_TAUMOL.800
 3500 CONTINUE                                                                   RRTM_TAUMOL.801
                                                                                 RRTM_TAUMOL.802
      RETURN                                                                     RRTM_TAUMOL.803
      END                                                                        RRTM_TAUMOL.804
                                                                                 RRTM_TAUMOL.805
C----------------------------------------------------------------------------    RRTM_TAUMOL.806
                                                                                 RRTM_TAUMOL.807
      SUBROUTINE TAUGB6                                                          RRTM_TAUMOL.808
                                                                                 RRTM_TAUMOL.809
C     BAND 6:  820-980 cm-1 (low - H2O; high - nothing)                          RRTM_TAUMOL.810
                                                                                 RRTM_TAUMOL.811
      PARAMETER (MXLAY=203, MAXXSEC=4, NBANDS=16)                                RRTM_TAUMOL.812
      PARAMETER (NGPT=140, NG6=8, NGS5=68)                                       RRTM_TAUMOL.813
                                                                                 RRTM_TAUMOL.814
C  Output                                                                        RRTM_TAUMOL.815
                                                                                 RRTM_TAUMOL.816
      COMMON /TAUS/     TAUG(NGPT,MXLAY)                                         RRTM_TAUMOL.817
                                                                                 RRTM_TAUMOL.818
C  Input                                                                         RRTM_TAUMOL.819
                                                                                 RRTM_TAUMOL.820
      COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                     RRTM_TAUMOL.821
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       RRTM_TAUMOL.822
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  RRTM_TAUMOL.823
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                                 RRTM_TAUMOL.824
     &                  COLH2O(MXLAY),COLCO2(MXLAY),                             RRTM_TAUMOL.825
     &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),                RRTM_TAUMOL.826
     &                  COLO2(MXLAY),CO2MULT(MXLAY)                              RRTM_TAUMOL.827
      COMMON /XSEC/     WX(MAXXSEC,MXLAY)                                        RRTM_TAUMOL.828
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                               RRTM_TAUMOL.829
     &                  FAC10(MXLAY),FAC11(MXLAY)                                RRTM_TAUMOL.830
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                           RRTM_TAUMOL.831
      COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)          RRTM_TAUMOL.832
      COMMON /K6C/      KA(5,13,NG6), SELFREF(10,NG6),                           RRTM_TAUMOL.833
     &                  ABSCO2(NG6), CFC11ADJ(NG6), CFC12(NG6)                   RRTM_TAUMOL.834
      COMMON /PF6C/     FRACREFA(NG6)                                            RRTM_TAUMOL.835
      COMMON /SP/       PFRAC(NGPT,MXLAY)                                        RRTM_TAUMOL.836
                                                                                 RRTM_TAUMOL.837
C This compiler directive was added to insure private common block storage       RRTM_TAUMOL.838
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_TAUMOL.839
C carry constants.                                                               RRTM_TAUMOL.840
C SGI                                                                            RRTM_TAUMOL.843
C$OMP THREADPRIVATE (/TAUS/,/PROFILE/,/PROFDATA/,/XSEC/,/INTFAC/)                RRTM_TAUMOL.844
C$OMP THREADPRIVATE (/INTIND/,/SELF/,/SP/)                                       RRTM_TAUMOL.845
C ***                                                                            RRTM_TAUMOL.846
                                                                                 RRTM_TAUMOL.847
      DIMENSION ABSA(65,NG6)                                                     RRTM_TAUMOL.848
      REAL KA                                                                    RRTM_TAUMOL.849
      EQUIVALENCE (KA,ABSA)                                                      RRTM_TAUMOL.850
                                                                                 RRTM_TAUMOL.851
C     Compute the optical depth by interpolating in ln(pressure) and             RRTM_TAUMOL.852
C     temperature. The water vapor self-continuum is interpolated                RRTM_TAUMOL.853
C     (in temperature) separately.                                               RRTM_TAUMOL.854
      DO 2500 LAY = 1, LAYTROP                                                   RRTM_TAUMOL.855
         IND0 = ((JP(LAY)-1)*5+(JT(LAY)-1))*NSPA(6) + 1                          RRTM_TAUMOL.856
         IND1 = (JP(LAY)*5+(JT1(LAY)-1))*NSPA(6) + 1                             RRTM_TAUMOL.857
         INDS = INDSELF(LAY)                                                     RRTM_TAUMOL.858
         DO 2000 IG = 1, NG6                                                     RRTM_TAUMOL.859
            TAUG(NGS5+IG,LAY) = COLH2O(LAY) *                                    RRTM_TAUMOL.860
     &          (FAC00(LAY) * ABSA(IND0,IG) +                                    RRTM_TAUMOL.861
     &           FAC10(LAY) * ABSA(IND0+1,IG) +                                  RRTM_TAUMOL.862
     &           FAC01(LAY) * ABSA(IND1,IG) +                                    RRTM_TAUMOL.863
     &           FAC11(LAY) * ABSA(IND1+1,IG) +                                  RRTM_TAUMOL.864
     &           SELFFAC(LAY) * (SELFREF(INDS,IG) +                              RRTM_TAUMOL.865
     &           SELFFRAC(LAY)*                                                  RRTM_TAUMOL.866
     &           (SELFREF(INDS+1,IG)-SELFREF(INDS,IG))))                         RRTM_TAUMOL.867
     &           + WX(2,LAY) * CFC11ADJ(IG)                                      RRTM_TAUMOL.868
     &           + WX(3,LAY) * CFC12(IG)                                         RRTM_TAUMOL.869
     &           + CO2MULT(LAY) * ABSCO2(IG)                                     RRTM_TAUMOL.870
            PFRAC(NGS5+IG,LAY) = FRACREFA(IG)                                    RRTM_TAUMOL.871
 2000    CONTINUE                                                                RRTM_TAUMOL.872
 2500 CONTINUE                                                                   RRTM_TAUMOL.873
                                                                                 RRTM_TAUMOL.874
C     Nothing important goes on above LAYTROP in this band.                      RRTM_TAUMOL.875
      DO 3500 LAY = LAYTROP+1, NLAYERS                                           RRTM_TAUMOL.876
         DO 3000 IG = 1, NG6                                                     RRTM_TAUMOL.877
            TAUG(NGS5+IG,LAY) = 0.0                                              RRTM_TAUMOL.878
     &           + WX(2,LAY) * CFC11ADJ(IG)                                      RRTM_TAUMOL.879
     &           + WX(3,LAY) * CFC12(IG)                                         RRTM_TAUMOL.880
            PFRAC(NGS5+IG,LAY) = FRACREFA(IG)                                    RRTM_TAUMOL.881
 3000    CONTINUE                                                                RRTM_TAUMOL.882
 3500 CONTINUE                                                                   RRTM_TAUMOL.883
                                                                                 RRTM_TAUMOL.884
      RETURN                                                                     RRTM_TAUMOL.885
      END                                                                        RRTM_TAUMOL.886
                                                                                 RRTM_TAUMOL.887
C----------------------------------------------------------------------------    RRTM_TAUMOL.888
                                                                                 RRTM_TAUMOL.889
      SUBROUTINE TAUGB7                                                          RRTM_TAUMOL.890
                                                                                 RRTM_TAUMOL.891
C     BAND 7:  980-1080 cm-1 (low - H2O,O3; high - O3)                           RRTM_TAUMOL.892
                                                                                 RRTM_TAUMOL.893
      PARAMETER (MXLAY=203, NBANDS=16)                                           RRTM_TAUMOL.894
      PARAMETER (NGPT=140, NG7=12, NGS6=76)                                      RRTM_TAUMOL.895
                                                                                 RRTM_TAUMOL.896
C  Output                                                                        RRTM_TAUMOL.897
                                                                                 RRTM_TAUMOL.898
      COMMON /TAUS/     TAUG(NGPT,MXLAY)                                         RRTM_TAUMOL.899
                                                                                 RRTM_TAUMOL.900
C  Input                                                                         RRTM_TAUMOL.901
                                                                                 RRTM_TAUMOL.902
      COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                     RRTM_TAUMOL.903
      COMMON /PRECISE/  ONEMINUS                                                 RRTM_TAUMOL.904
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       RRTM_TAUMOL.905
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  RRTM_TAUMOL.906
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                                 RRTM_TAUMOL.907
     &                  COLH2O(MXLAY),COLCO2(MXLAY),                             RRTM_TAUMOL.908
     &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),                RRTM_TAUMOL.909
     &                  COLO2(MXLAY),CO2MULT(MXLAY)                              RRTM_TAUMOL.910
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                               RRTM_TAUMOL.911
     &                  FAC10(MXLAY),FAC11(MXLAY)                                RRTM_TAUMOL.912
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                           RRTM_TAUMOL.913
      COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)          RRTM_TAUMOL.914
      COMMON /K7C/      KA(9,5,13,NG7), KB(5,13:59,NG7),                         RRTM_TAUMOL.915
     &                  SELFREF(10,NG7),                                         RRTM_TAUMOL.916
     &                  ABSCO2(NG7)                                              RRTM_TAUMOL.917
      COMMON /PF7C/     FRACREFA(NG7,9), FRACREFB(NG7)                           RRTM_TAUMOL.918
      COMMON /SP/       PFRAC(NGPT,MXLAY)                                        RRTM_TAUMOL.919
                                                                                 RRTM_TAUMOL.920
C This compiler directive was added to insure private common block storage       RRTM_TAUMOL.921
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_TAUMOL.922
C carry constants.                                                               RRTM_TAUMOL.923
C SGI                                                                            RRTM_TAUMOL.926
C$OMP THREADPRIVATE (/TAUS/,/PROFILE/,/PROFDATA/,/INTFAC/)                       RRTM_TAUMOL.927
C$OMP THREADPRIVATE (/INTIND/,/SELF/,/SP/)                                       RRTM_TAUMOL.928
C ***                                                                            RRTM_TAUMOL.929
                                                                                 RRTM_TAUMOL.930
      DIMENSION ABSA(585,NG7),ABSB(235,NG7)                                      RRTM_TAUMOL.931
      REAL KA,KB                                                                 RRTM_TAUMOL.932
      EQUIVALENCE (KA,ABSA),(KB,ABSB)                                            RRTM_TAUMOL.933
                                                                                 RRTM_TAUMOL.934
      STRRAT1 = 8.21104E4                                                        RRTM_TAUMOL.935
                                                                                 RRTM_TAUMOL.936
C     Compute the optical depth by interpolating in ln(pressure),                RRTM_TAUMOL.937
C     temperature, and appropriate species.  Below LAYTROP, the water            RRTM_TAUMOL.938
C     vapor self-continuum is interpolated (in temperature) separately.          RRTM_TAUMOL.939
CDIR$ NOVECTOR                                                                   RRTM_TAUMOL.940
      DO 2500 LAY = 1, LAYTROP                                                   RRTM_TAUMOL.941
         SPECCOMB = COLH2O(LAY) + STRRAT1*COLO3(LAY)                             RRTM_TAUMOL.942
         SPECPARM = COLH2O(LAY)/SPECCOMB                                         RRTM_TAUMOL.943
         IF (SPECPARM .GE. ONEMINUS) SPECPARM = ONEMINUS                         RRTM_TAUMOL.944
         SPECMULT = 8.*SPECPARM                                                  RRTM_TAUMOL.945
         JS = 1 + INT(SPECMULT)                                                  RRTM_TAUMOL.946
         FS = AMOD(SPECMULT,1.0)                                                 RRTM_TAUMOL.947
         FAC000 = (1. - FS) * FAC00(LAY)                                         RRTM_TAUMOL.948
         FAC010 = (1. - FS) * FAC10(LAY)                                         RRTM_TAUMOL.949
         FAC100 = FS * FAC00(LAY)                                                RRTM_TAUMOL.950
         FAC110 = FS * FAC10(LAY)                                                RRTM_TAUMOL.951
         FAC001 = (1. - FS) * FAC01(LAY)                                         RRTM_TAUMOL.952
         FAC011 = (1. - FS) * FAC11(LAY)                                         RRTM_TAUMOL.953
         FAC101 = FS * FAC01(LAY)                                                RRTM_TAUMOL.954
         FAC111 = FS * FAC11(LAY)                                                RRTM_TAUMOL.955
         IND0 = ((JP(LAY)-1)*5+(JT(LAY)-1))*NSPA(7) + JS                         RRTM_TAUMOL.956
         IND1 = (JP(LAY)*5+(JT1(LAY)-1))*NSPA(7) + JS                            RRTM_TAUMOL.957
         INDS = INDSELF(LAY)                                                     RRTM_TAUMOL.958
CDIR$ VECTOR                                                                     RRTM_TAUMOL.959
         DO 2000 IG = 1, NG7                                                     RRTM_TAUMOL.960
            TAUG(NGS6+IG,LAY) = SPECCOMB *                                       RRTM_TAUMOL.961
     &          (FAC000 * ABSA(IND0,IG) +                                        RRTM_TAUMOL.962
     &           FAC100 * ABSA(IND0+1,IG) +                                      RRTM_TAUMOL.963
     &           FAC010 * ABSA(IND0+9,IG) +                                      RRTM_TAUMOL.964
     &           FAC110 * ABSA(IND0+10,IG) +                                     RRTM_TAUMOL.965
     &           FAC001 * ABSA(IND1,IG) +                                        RRTM_TAUMOL.966
     &           FAC101 * ABSA(IND1+1,IG) +                                      RRTM_TAUMOL.967
     &           FAC011 * ABSA(IND1+9,IG) +                                      RRTM_TAUMOL.968
     &           FAC111 * ABSA(IND1+10,IG)) +                                    RRTM_TAUMOL.969
     &           COLH2O(LAY) *                                                   RRTM_TAUMOL.970
     &           SELFFAC(LAY) * (SELFREF(INDS,IG) +                              RRTM_TAUMOL.971
     &           SELFFRAC(LAY) *                                                 RRTM_TAUMOL.972
     &           (SELFREF(INDS+1,IG) - SELFREF(INDS,IG)))                        RRTM_TAUMOL.973
     &           + CO2MULT(LAY) * ABSCO2(IG)                                     RRTM_TAUMOL.974
         PFRAC(NGS6+IG,LAY) = FRACREFA(IG,JS) + FS *                             RRTM_TAUMOL.975
     &           (FRACREFA(IG,JS+1) - FRACREFA(IG,JS))                           RRTM_TAUMOL.976
 2000    CONTINUE                                                                RRTM_TAUMOL.977
 2500 CONTINUE                                                                   RRTM_TAUMOL.978
                                                                                 RRTM_TAUMOL.979
      DO 3500 LAY = LAYTROP+1, NLAYERS                                           RRTM_TAUMOL.980
         IND0 = ((JP(LAY)-13)*5+(JT(LAY)-1))*NSPB(7) + 1                         RRTM_TAUMOL.981
         IND1 = ((JP(LAY)-12)*5+(JT1(LAY)-1))*NSPB(7) + 1                        RRTM_TAUMOL.982
         DO 3000 IG = 1, NG7                                                     RRTM_TAUMOL.983
            TAUG(NGS6+IG,LAY) = COLO3(LAY) *                                     RRTM_TAUMOL.984
     &          (FAC00(LAY) * ABSB(IND0,IG) +                                    RRTM_TAUMOL.985
     &           FAC10(LAY) * ABSB(IND0+1,IG) +                                  RRTM_TAUMOL.986
     &           FAC01(LAY) * ABSB(IND1,IG) +                                    RRTM_TAUMOL.987
     &           FAC11(LAY) * ABSB(IND1+1,IG))                                   RRTM_TAUMOL.988
     &           + CO2MULT(LAY) * ABSCO2(IG)                                     RRTM_TAUMOL.989
            PFRAC(NGS6+IG,LAY) = FRACREFB(IG)                                    RRTM_TAUMOL.990
 3000    CONTINUE                                                                RRTM_TAUMOL.991
 3500 CONTINUE                                                                   RRTM_TAUMOL.992
                                                                                 RRTM_TAUMOL.993
      RETURN                                                                     RRTM_TAUMOL.994
      END                                                                        RRTM_TAUMOL.995
                                                                                 RRTM_TAUMOL.996
C******************************************************************************  07NOV00.2468
                                                                                 RRTM_TAUMOL.998
      SUBROUTINE TAUGB8                                                          RRTM_TAUMOL.999
                                                                                 RRTM_TAUMOL.1000
C     BAND 8:  1080-1180 cm-1 (low (i.e.>~300mb) - H2O; high - O3)               RRTM_TAUMOL.1001
                                                                                 RRTM_TAUMOL.1002
      PARAMETER (MXLAY=203, MAXXSEC=4, NBANDS=16)                                RRTM_TAUMOL.1003
      PARAMETER (NGPT=140, NG8=8, NGS7=88)                                       RRTM_TAUMOL.1004
                                                                                 RRTM_TAUMOL.1005
C  Output                                                                        RRTM_TAUMOL.1006
                                                                                 RRTM_TAUMOL.1007
      COMMON /TAUS/     TAUG(NGPT,MXLAY)                                         RRTM_TAUMOL.1008
                                                                                 RRTM_TAUMOL.1009
C  Input                                                                         RRTM_TAUMOL.1010
                                                                                 RRTM_TAUMOL.1011
      COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                     RRTM_TAUMOL.1012
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       RRTM_TAUMOL.1013
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  RRTM_TAUMOL.1014
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                                 RRTM_TAUMOL.1015
     &                  COLH2O(MXLAY),COLCO2(MXLAY),                             RRTM_TAUMOL.1016
     &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),                RRTM_TAUMOL.1017
     &                  COLO2(MXLAY),CO2MULT(MXLAY)                              RRTM_TAUMOL.1018
      COMMON /XSEC/     WX(MAXXSEC,MXLAY)                                        RRTM_TAUMOL.1019
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                               RRTM_TAUMOL.1020
     &                  FAC10(MXLAY),FAC11(MXLAY)                                RRTM_TAUMOL.1021
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                           RRTM_TAUMOL.1022
      COMMON /SELF/     SELFFAC(MXLAY),SELFFRAC(MXLAY),INDSELF(MXLAY)            RRTM_TAUMOL.1023
      COMMON /K8C/      KA(5,7,NG8), KB(5,7:59,NG8),                             RRTM_TAUMOL.1024
     &                  SELFREF(10,NG8),                                         RRTM_TAUMOL.1025
     &                  ABSCO2A(NG8), ABSCO2B(NG8),                              RRTM_TAUMOL.1026
     &                  ABSN2OA(NG8), ABSN2OB(NG8),                              RRTM_TAUMOL.1027
     &                  CFC12(NG8), CFC22ADJ(NG8)                                RRTM_TAUMOL.1028
      COMMON /PF8C/     FRACREFA(NG8), FRACREFB(NG8)                             RRTM_TAUMOL.1029
      COMMON /SP/       PFRAC(NGPT,MXLAY)                                        RRTM_TAUMOL.1030
                                                                                 RRTM_TAUMOL.1031
C This compiler directive was added to insure private common block storage       RRTM_TAUMOL.1032
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_TAUMOL.1033
C carry constants.                                                               RRTM_TAUMOL.1034
C SGI                                                                            RRTM_TAUMOL.1037
C$OMP THREADPRIVATE (/TAUS/,/PROFILE/,/PROFDATA/,/XSEC/,/INTFAC/)                RRTM_TAUMOL.1038
C$OMP THREADPRIVATE (/INTIND/,/SELF/,/SP/)                                       RRTM_TAUMOL.1039
C ***                                                                            RRTM_TAUMOL.1040
                                                                                 RRTM_TAUMOL.1041
      DIMENSION ABSA(35,NG8),ABSB(265,NG8)                                       RRTM_TAUMOL.1042
      DIMENSION H2OREF(59),O3REF(59)                                             RRTM_TAUMOL.1043
      REAL KA,KB,N2OMULT,N2OREF(59)                                              RRTM_TAUMOL.1044
      EQUIVALENCE (KA,ABSA),(KB,ABSB)                                            RRTM_TAUMOL.1045
                                                                                 RRTM_TAUMOL.1046
      DATA H2OREF/                                                               RRTM_TAUMOL.1047
     &     1.87599E-02,1.22233E-02,5.89086E-03,2.76753E-03,1.40651E-03,          RRTM_TAUMOL.1048
     &     7.59698E-04,3.88758E-04,1.65422E-04,3.71895E-05,7.47648E-06,          RRTM_TAUMOL.1049
     &     4.30818E-06,3.33194E-06,3.20393E-06,3.16186E-06,3.25235E-06,          RRTM_TAUMOL.1050
     &     3.42258E-06,3.62884E-06,3.91482E-06,4.14875E-06,4.30810E-06,          RRTM_TAUMOL.1051
     &     4.44204E-06,4.57783E-06,4.70865E-06,4.79432E-06,4.86971E-06,          RRTM_TAUMOL.1052
     &     4.92603E-06,4.96688E-06,4.99628E-06,5.05266E-06,5.12658E-06,          RRTM_TAUMOL.1053
     &     5.25028E-06,5.35708E-06,5.45085E-06,5.48304E-06,5.50000E-06,          RRTM_TAUMOL.1054
     &     5.50000E-06,5.45359E-06,5.40468E-06,5.35576E-06,5.25327E-06,          RRTM_TAUMOL.1055
     &     5.14362E-06,5.03396E-06,4.87662E-06,4.69787E-06,4.51911E-06,          RRTM_TAUMOL.1056
     &     4.33600E-06,4.14416E-06,3.95232E-06,3.76048E-06,3.57217E-06,          RRTM_TAUMOL.1057
     &     3.38549E-06,3.19881E-06,3.01212E-06,2.82621E-06,2.64068E-06,          RRTM_TAUMOL.1058
     &     2.45515E-06,2.26962E-06,2.08659E-06,1.93029E-06/                      RRTM_TAUMOL.1059
      DATA N2OREF/                                                               RRTM_TAUMOL.1060
     &     3.20000E-07,3.20000E-07,3.20000E-07,3.20000E-07,3.20000E-07,          RRTM_TAUMOL.1061
     &     3.19652E-07,3.15324E-07,3.03830E-07,2.94221E-07,2.84953E-07,          RRTM_TAUMOL.1062
     &     2.76714E-07,2.64709E-07,2.42847E-07,2.09547E-07,1.71945E-07,          RRTM_TAUMOL.1063
     &     1.37491E-07,1.13319E-07,1.00354E-07,9.12812E-08,8.54633E-08,          RRTM_TAUMOL.1064
     &     8.03631E-08,7.33718E-08,6.59754E-08,5.60386E-08,4.70901E-08,          RRTM_TAUMOL.1065
     &     3.99774E-08,3.29786E-08,2.60642E-08,2.10663E-08,1.65918E-08,          RRTM_TAUMOL.1066
     &     1.30167E-08,1.00900E-08,7.62490E-09,6.11592E-09,4.66725E-09,          RRTM_TAUMOL.1067
     &     3.28574E-09,2.84838E-09,2.46198E-09,2.07557E-09,1.85507E-09,          RRTM_TAUMOL.1068
     &     1.65675E-09,1.45843E-09,1.31948E-09,1.20716E-09,1.09485E-09,          RRTM_TAUMOL.1069
     &     9.97803E-10,9.31260E-10,8.64721E-10,7.98181E-10,7.51380E-10,          RRTM_TAUMOL.1070
     &     7.13670E-10,6.75960E-10,6.38250E-10,6.09811E-10,5.85998E-10,          RRTM_TAUMOL.1071
     &     5.62185E-10,5.38371E-10,5.15183E-10,4.98660E-10/                      RRTM_TAUMOL.1072
      DATA O3REF/                                                                RRTM_TAUMOL.1073
     &     3.01700E-08,3.47254E-08,4.24769E-08,5.27592E-08,6.69439E-08,          RRTM_TAUMOL.1074
     &     8.71295E-08,1.13911E-07,1.56771E-07,2.17878E-07,3.24430E-07,          RRTM_TAUMOL.1075
     &     4.65942E-07,5.68057E-07,6.96065E-07,1.11863E-06,1.76175E-06,          RRTM_TAUMOL.1076
     &     2.32689E-06,2.95769E-06,3.65930E-06,4.59503E-06,5.31891E-06,          RRTM_TAUMOL.1077
     &     5.96179E-06,6.51133E-06,7.06350E-06,7.69169E-06,8.25771E-06,          RRTM_TAUMOL.1078
     &     8.70824E-06,8.83245E-06,8.71486E-06,8.09434E-06,7.33071E-06,          RRTM_TAUMOL.1079
     &     6.31014E-06,5.36717E-06,4.48289E-06,3.83913E-06,3.28270E-06,          RRTM_TAUMOL.1080
     &     2.82351E-06,2.49061E-06,2.16453E-06,1.83845E-06,1.66182E-06,          RRTM_TAUMOL.1081
     &     1.50517E-06,1.34852E-06,1.19718E-06,1.04822E-06,8.99264E-07,          RRTM_TAUMOL.1082
     &     7.63432E-07,6.53806E-07,5.44186E-07,4.34564E-07,3.64210E-07,          RRTM_TAUMOL.1083
     &     3.11938E-07,2.59667E-07,2.07395E-07,1.91456E-07,1.93639E-07,          RRTM_TAUMOL.1084
     &     1.95821E-07,1.98004E-07,2.06442E-07,2.81546E-07/                      RRTM_TAUMOL.1085
                                                                                 RRTM_TAUMOL.1086
C     Compute the optical depth by interpolating in ln(pressure) and             RRTM_TAUMOL.1087
C     temperature.                                                               RRTM_TAUMOL.1088
      DO 2500 LAY = 1, LAYSWTCH                                                  RRTM_TAUMOL.1089
         FP = FAC01(LAY) + FAC11(LAY)                                            RRTM_TAUMOL.1090
         IND0 = ((JP(LAY)-1)*5+(JT(LAY)-1))*NSPA(8) + 1                          RRTM_TAUMOL.1091
         IND1 = (JP(LAY)*5+(JT1(LAY)-1))*NSPA(8) + 1                             RRTM_TAUMOL.1092
         INDS = INDSELF(LAY)                                                     RRTM_TAUMOL.1093
         COLREF1 = N2OREF(JP(LAY))                                               RRTM_TAUMOL.1094
         COLREF2 = N2OREF(JP(LAY)+1)                                             RRTM_TAUMOL.1095
         WCOMB1 = H2OREF(JP(LAY))                                                RRTM_TAUMOL.1096
         WCOMB2 = H2OREF(JP(LAY)+1)                                              RRTM_TAUMOL.1097
         RATIO = (COLREF1/WCOMB1)+FP*((COLREF2/WCOMB2)-(COLREF1/WCOMB1))         RRTM_TAUMOL.1098
         CURRN2O = COLH2O(LAY) * RATIO                                           RRTM_TAUMOL.1099
         N2OMULT = COLN2O(LAY) - CURRN2O                                         RRTM_TAUMOL.1100
         DO 2000 IG = 1, NG8                                                     RRTM_TAUMOL.1101
            TAUG(NGS7+IG,LAY) = COLH2O(LAY) *                                    RRTM_TAUMOL.1102
     &          (FAC00(LAY) * ABSA(IND0,IG) +                                    RRTM_TAUMOL.1103
     &           FAC10(LAY) * ABSA(IND0+1,IG) +                                  RRTM_TAUMOL.1104
     &           FAC01(LAY) * ABSA(IND1,IG) +                                    RRTM_TAUMOL.1105
     &           FAC11(LAY) * ABSA(IND1+1,IG) +                                  RRTM_TAUMOL.1106
     &           SELFFAC(LAY) * (SELFREF(INDS,IG) +                              RRTM_TAUMOL.1107
     &           SELFFRAC(LAY) *                                                 RRTM_TAUMOL.1108
     &           (SELFREF(INDS+1,IG) - SELFREF(INDS,IG))))                       RRTM_TAUMOL.1109
     &           + WX(3,LAY) * CFC12(IG)                                         RRTM_TAUMOL.1110
     &           + WX(4,LAY) * CFC22ADJ(IG)                                      RRTM_TAUMOL.1111
     &           + CO2MULT(LAY) * ABSCO2A(IG)                                    RRTM_TAUMOL.1112
     &           + N2OMULT * ABSN2OA(IG)                                         RRTM_TAUMOL.1113
            PFRAC(NGS7+IG,LAY) = FRACREFA(IG)                                    RRTM_TAUMOL.1114
 2000    CONTINUE                                                                RRTM_TAUMOL.1115
 2500 CONTINUE                                                                   RRTM_TAUMOL.1116
                                                                                 RRTM_TAUMOL.1117
      DO 3500 LAY = LAYSWTCH+1, NLAYERS                                          RRTM_TAUMOL.1118
         FP = FAC01(LAY) + FAC11(LAY)                                            RRTM_TAUMOL.1119
         IND0 = ((JP(LAY)-7)*5+(JT(LAY)-1))*NSPB(8) + 1                          RRTM_TAUMOL.1120
         IND1 = ((JP(LAY)-6)*5+(JT1(LAY)-1))*NSPB(8) + 1                         RRTM_TAUMOL.1121
         COLREF1 = N2OREF(JP(LAY))                                               RRTM_TAUMOL.1122
         COLREF2 = N2OREF(JP(LAY)+1)                                             RRTM_TAUMOL.1123
         WCOMB1 = O3REF(JP(LAY))                                                 RRTM_TAUMOL.1124
         WCOMB2 = O3REF(JP(LAY)+1)                                               RRTM_TAUMOL.1125
         RATIO = (COLREF1/WCOMB1)+FP*((COLREF2/WCOMB2)-(COLREF1/WCOMB1))         RRTM_TAUMOL.1126
         CURRN2O = COLO3(LAY) * RATIO                                            RRTM_TAUMOL.1127
         N2OMULT = COLN2O(LAY) - CURRN2O                                         RRTM_TAUMOL.1128
         DO 3000 IG = 1, NG8                                                     RRTM_TAUMOL.1129
            TAUG(NGS7+IG,LAY) = COLO3(LAY) *                                     RRTM_TAUMOL.1130
     &          (FAC00(LAY) * ABSB(IND0,IG) +                                    RRTM_TAUMOL.1131
     &           FAC10(LAY) * ABSB(IND0+1,IG) +                                  RRTM_TAUMOL.1132
     &           FAC01(LAY) * ABSB(IND1,IG) +                                    RRTM_TAUMOL.1133
     &           FAC11(LAY) * ABSB(IND1+1,IG))                                   RRTM_TAUMOL.1134
     &           + WX(3,LAY) * CFC12(IG)                                         RRTM_TAUMOL.1135
     &           + WX(4,LAY) * CFC22ADJ(IG)                                      RRTM_TAUMOL.1136
     &           + CO2MULT(LAY) * ABSCO2B(IG)                                    RRTM_TAUMOL.1137
     &           + N2OMULT * ABSN2OB(IG)                                         RRTM_TAUMOL.1138
            PFRAC(NGS7+IG,LAY) = FRACREFB(IG)                                    RRTM_TAUMOL.1139
 3000    CONTINUE                                                                RRTM_TAUMOL.1140
 3500 CONTINUE                                                                   RRTM_TAUMOL.1141
                                                                                 RRTM_TAUMOL.1142
      RETURN                                                                     RRTM_TAUMOL.1143
      END                                                                        RRTM_TAUMOL.1144
                                                                                 RRTM_TAUMOL.1145
C----------------------------------------------------------------------------    RRTM_TAUMOL.1146
                                                                                 RRTM_TAUMOL.1147
      SUBROUTINE TAUGB9                                                          RRTM_TAUMOL.1148
                                                                                 RRTM_TAUMOL.1149
C     BAND 9:  1180-1390 cm-1 (low - H2O,CH4; high - CH4)                        RRTM_TAUMOL.1150
                                                                                 RRTM_TAUMOL.1151
      PARAMETER (MXLAY=203, NBANDS=16)                                           RRTM_TAUMOL.1152
      PARAMETER (NGPT=140, NG9=12, NGS8=96)                                      RRTM_TAUMOL.1153
                                                                                 RRTM_TAUMOL.1154
C  Output                                                                        RRTM_TAUMOL.1155
                                                                                 RRTM_TAUMOL.1156
      COMMON /TAUS/     TAUG(NGPT,MXLAY)                                         RRTM_TAUMOL.1157
                                                                                 RRTM_TAUMOL.1158
C  Input                                                                         RRTM_TAUMOL.1159
                                                                                 RRTM_TAUMOL.1160
      COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                     RRTM_TAUMOL.1161
      COMMON /PRECISE/  ONEMINUS                                                 RRTM_TAUMOL.1162
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       RRTM_TAUMOL.1163
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  RRTM_TAUMOL.1164
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                                 RRTM_TAUMOL.1165
     &                  COLH2O(MXLAY),COLCO2(MXLAY),                             RRTM_TAUMOL.1166
     &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),                RRTM_TAUMOL.1167
     &                  COLO2(MXLAY),CO2MULT(MXLAY)                              RRTM_TAUMOL.1168
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                               RRTM_TAUMOL.1169
     &                  FAC10(MXLAY),FAC11(MXLAY)                                RRTM_TAUMOL.1170
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                           RRTM_TAUMOL.1171
      COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)          RRTM_TAUMOL.1172
      COMMON /K9C/      KA(11,5,13,NG9), KB(5,13:59,NG9),                        RRTM_TAUMOL.1173
     &                  SELFREF(10,NG9),                                         RRTM_TAUMOL.1174
     &                  ABSN2O(3*NG9)                                            RRTM_TAUMOL.1175
      COMMON /PF9C/     FRACREFA(NG9,9), FRACREFB(NG9)                           RRTM_TAUMOL.1176
      COMMON /SP/       PFRAC(NGPT,MXLAY)                                        RRTM_TAUMOL.1177
                                                                                 RRTM_TAUMOL.1178
C This compiler directive was added to insure private common block storage       RRTM_TAUMOL.1179
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_TAUMOL.1180
C carry constants.                                                               RRTM_TAUMOL.1181
C SGI                                                                            RRTM_TAUMOL.1184
C$OMP THREADPRIVATE (/TAUS/,/PROFILE/,/PROFDATA/,/INTFAC/)                       RRTM_TAUMOL.1185
C$OMP THREADPRIVATE (/INTIND/,/SELF/,/SP/)                                       RRTM_TAUMOL.1186
C ***                                                                            RRTM_TAUMOL.1187
                                                                                 RRTM_TAUMOL.1188
      DIMENSION ABSA(715,NG9),ABSB(235,NG9)                                      RRTM_TAUMOL.1189
      DIMENSION H2OREF(13),CH4REF(13),ETAREF(11)                                 RRTM_TAUMOL.1190
      REAL KA,KB,N2OMULT,N2OREF(13)                                              RRTM_TAUMOL.1191
      EQUIVALENCE (KA,ABSA),(KB,ABSB)                                            RRTM_TAUMOL.1192
                                                                                 RRTM_TAUMOL.1193
      DATA N2OREF/                                                               RRTM_TAUMOL.1194
     &     3.20000E-07,3.20000E-07,3.20000E-07,3.20000E-07,3.20000E-07,          RRTM_TAUMOL.1195
     &     3.19652E-07,3.15324E-07,3.03830E-07,2.94221E-07,2.84953E-07,          RRTM_TAUMOL.1196
     &     2.76714E-07,2.64709E-07,2.42847E-07/                                  RRTM_TAUMOL.1197
      DATA H2OREF/                                                               RRTM_TAUMOL.1198
     &     1.8759999E-02, 1.2223309E-02, 5.8908667E-03, 2.7675382E-03,           RRTM_TAUMOL.1199
     &     1.4065107E-03, 7.5969833E-04, 3.8875898E-04, 1.6542293E-04,           RRTM_TAUMOL.1200
     &     3.7189537E-05, 7.4764857E-06, 4.3081886E-06, 3.3319423E-06,           RRTM_TAUMOL.1201
     &     3.2039343E-06/                                                        RRTM_TAUMOL.1202
      DATA CH4REF/                                                               RRTM_TAUMOL.1203
     &     1.7000001E-06, 1.7000001E-06, 1.6998713E-06, 1.6904165E-06,           RRTM_TAUMOL.1204
     &     1.6671424E-06, 1.6350652E-06, 1.6097551E-06, 1.5590465E-06,           RRTM_TAUMOL.1205
     &     1.5119849E-06, 1.4741138E-06, 1.4384609E-06, 1.4002215E-06,           RRTM_TAUMOL.1206
     &     1.3573376E-06/                                                        RRTM_TAUMOL.1207
      DATA ETAREF/                                                               RRTM_TAUMOL.1208
     &     0.,0.125,0.25,0.375,0.5,0.625,0.75,0.875,0.96,0.99,1.0/               RRTM_TAUMOL.1209
                                                                                 RRTM_TAUMOL.1210
      STRRAT = 21.6282                                                           RRTM_TAUMOL.1211
      IOFF = 0                                                                   RRTM_TAUMOL.1212
                                                                                 RRTM_TAUMOL.1213
C     Compute the optical depth by interpolating in ln(pressure),                RRTM_TAUMOL.1214
C     temperature, and appropriate species.  Below LAYTROP, the water            RRTM_TAUMOL.1215
C     vapor self-continuum is interpolated (in temperature) separately.          RRTM_TAUMOL.1216
      DO 2500 LAY = 1, LAYTROP                                                   RRTM_TAUMOL.1217
         SPECCOMB = COLH2O(LAY) + STRRAT*COLCH4(LAY)                             RRTM_TAUMOL.1218
         SPECPARM = COLH2O(LAY)/SPECCOMB                                         RRTM_TAUMOL.1219
         IF (SPECPARM .GE. ONEMINUS) SPECPARM = ONEMINUS                         RRTM_TAUMOL.1220
         SPECMULT = 8.*(SPECPARM)                                                RRTM_TAUMOL.1221
         JS = 1 + INT(SPECMULT)                                                  RRTM_TAUMOL.1222
         JFRAC = JS                                                              RRTM_TAUMOL.1223
         FS = AMOD(SPECMULT,1.0)                                                 RRTM_TAUMOL.1224
         FFRAC = FS                                                              RRTM_TAUMOL.1225
         IF (JS .EQ. 8) THEN                                                     RRTM_TAUMOL.1226
            IF (FS. LE. 0.68) THEN                                               RRTM_TAUMOL.1227
               FS = FS/0.68                                                      RRTM_TAUMOL.1228
            ELSEIF (FS .LE. 0.92) THEN                                           RRTM_TAUMOL.1229
               JS = JS + 1                                                       RRTM_TAUMOL.1230
               FS = (FS-0.68)/0.24                                               RRTM_TAUMOL.1231
            ELSE                                                                 RRTM_TAUMOL.1232
               JS = JS + 2                                                       RRTM_TAUMOL.1233
               FS = (FS-0.92)/0.08                                               RRTM_TAUMOL.1234
            ENDIF                                                                RRTM_TAUMOL.1235
         ELSEIF (JS .EQ.9) THEN                                                  RRTM_TAUMOL.1236
            JS = 10                                                              RRTM_TAUMOL.1237
            FS = 1.                                                              RRTM_TAUMOL.1238
            JFRAC = 8                                                            RRTM_TAUMOL.1239
            FFRAC = 1.                                                           RRTM_TAUMOL.1240
         ENDIF                                                                   RRTM_TAUMOL.1241
         FP = FAC01(LAY) + FAC11(LAY)                                            RRTM_TAUMOL.1242
         NS = JS + INT(FS + 0.5)                                                 RRTM_TAUMOL.1243
         FAC000 = (1. - FS) * FAC00(LAY)                                         RRTM_TAUMOL.1244
         FAC010 = (1. - FS) * FAC10(LAY)                                         RRTM_TAUMOL.1245
         FAC100 = FS * FAC00(LAY)                                                RRTM_TAUMOL.1246
         FAC110 = FS * FAC10(LAY)                                                RRTM_TAUMOL.1247
         FAC001 = (1. - FS) * FAC01(LAY)                                         RRTM_TAUMOL.1248
         FAC011 = (1. - FS) * FAC11(LAY)                                         RRTM_TAUMOL.1249
         FAC101 = FS * FAC01(LAY)                                                RRTM_TAUMOL.1250
         FAC111 = FS * FAC11(LAY)                                                RRTM_TAUMOL.1251
         IND0 = ((JP(LAY)-1)*5+(JT(LAY)-1))*NSPA(9) + JS                         RRTM_TAUMOL.1252
         IND1 = (JP(LAY)*5+(JT1(LAY)-1))*NSPA(9) + JS                            RRTM_TAUMOL.1253
         INDS = INDSELF(LAY)                                                     RRTM_TAUMOL.1254
         IF (LAY .EQ. LAYLOW) IOFF = NG9                                         RRTM_TAUMOL.1255
         IF (LAY .EQ. LAYSWTCH) IOFF = 2*NG9                                     RRTM_TAUMOL.1256
         COLREF1 = N2OREF(JP(LAY))                                               RRTM_TAUMOL.1257
         COLREF2 = N2OREF(JP(LAY)+1)                                             RRTM_TAUMOL.1258
         IF (NS .EQ. 11) THEN                                                    RRTM_TAUMOL.1259
            WCOMB1 = H2OREF(JP(LAY))                                             RRTM_TAUMOL.1260
            WCOMB2 = H2OREF(JP(LAY)+1)                                           RRTM_TAUMOL.1261
         ELSE                                                                    RRTM_TAUMOL.1262
            WCOMB1 = STRRAT * CH4REF(JP(LAY))/(1.-ETAREF(NS))                    RRTM_TAUMOL.1263
            WCOMB2 = STRRAT * CH4REF(JP(LAY)+1)/(1.-ETAREF(NS))                  RRTM_TAUMOL.1264
         ENDIF                                                                   RRTM_TAUMOL.1265
         RATIO = (COLREF1/WCOMB1)+FP*((COLREF2/WCOMB2)-(COLREF1/WCOMB1))         RRTM_TAUMOL.1266
         CURRN2O = SPECCOMB * RATIO                                              RRTM_TAUMOL.1267
         N2OMULT = COLN2O(LAY) - CURRN2O                                         RRTM_TAUMOL.1268
         DO 2000 IG = 1, NG9                                                     RRTM_TAUMOL.1269
            TAUG(NGS8+IG,LAY) = SPECCOMB *                                       RRTM_TAUMOL.1270
     &          (FAC000 * ABSA(IND0,IG) +                                        RRTM_TAUMOL.1271
     &           FAC100 * ABSA(IND0+1,IG) +                                      RRTM_TAUMOL.1272
     &           FAC010 * ABSA(IND0+11,IG) +                                     RRTM_TAUMOL.1273
     &           FAC110 * ABSA(IND0+12,IG) +                                     RRTM_TAUMOL.1274
     &           FAC001 * ABSA(IND1,IG) +                                        RRTM_TAUMOL.1275
     &           FAC101 * ABSA(IND1+1,IG) +                                      RRTM_TAUMOL.1276
     &           FAC011 * ABSA(IND1+11,IG) +                                     RRTM_TAUMOL.1277
     &           FAC111 * ABSA(IND1+12,IG)) +                                    RRTM_TAUMOL.1278
     &           COLH2O(LAY) *                                                   RRTM_TAUMOL.1279
     &           SELFFAC(LAY) * (SELFREF(INDS,IG) +                              RRTM_TAUMOL.1280
     &           SELFFRAC(LAY) *                                                 RRTM_TAUMOL.1281
     &           (SELFREF(INDS+1,IG) - SELFREF(INDS,IG)))                        RRTM_TAUMOL.1282
     &           + N2OMULT * ABSN2O(IG+IOFF)                                     RRTM_TAUMOL.1283
            PFRAC(NGS8+IG,LAY) = FRACREFA(IG,JFRAC) + FFRAC *                    RRTM_TAUMOL.1284
     &           (FRACREFA(IG,JFRAC+1) - FRACREFA(IG,JFRAC))                     RRTM_TAUMOL.1285
 2000    CONTINUE                                                                RRTM_TAUMOL.1286
 2500 CONTINUE                                                                   RRTM_TAUMOL.1287
                                                                                 RRTM_TAUMOL.1288
      DO 3500 LAY = LAYTROP+1, NLAYERS                                           RRTM_TAUMOL.1289
         IND0 = ((JP(LAY)-13)*5+(JT(LAY)-1))*NSPB(9) + 1                         RRTM_TAUMOL.1290
         IND1 = ((JP(LAY)-12)*5+(JT1(LAY)-1))*NSPB(9) + 1                        RRTM_TAUMOL.1291
         DO 3000 IG = 1, NG9                                                     RRTM_TAUMOL.1292
            TAUG(NGS8+IG,LAY) = COLCH4(LAY) *                                    RRTM_TAUMOL.1293
     &          (FAC00(LAY) * ABSB(IND0,IG) +                                    RRTM_TAUMOL.1294
     &           FAC10(LAY) * ABSB(IND0+1,IG) +                                  RRTM_TAUMOL.1295
     &           FAC01(LAY) * ABSB(IND1,IG) +                                    RRTM_TAUMOL.1296
     &           FAC11(LAY) * ABSB(IND1+1,IG))                                   RRTM_TAUMOL.1297
            PFRAC(NGS8+IG,LAY) = FRACREFB(IG)                                    RRTM_TAUMOL.1298
 3000    CONTINUE                                                                RRTM_TAUMOL.1299
 3500 CONTINUE                                                                   RRTM_TAUMOL.1300
                                                                                 RRTM_TAUMOL.1301
      RETURN                                                                     RRTM_TAUMOL.1302
      END                                                                        RRTM_TAUMOL.1303
                                                                                 RRTM_TAUMOL.1304
C******************************************************************************  07NOV00.2469
                                                                                 RRTM_TAUMOL.1306
      SUBROUTINE TAUGB10                                                         RRTM_TAUMOL.1307
                                                                                 RRTM_TAUMOL.1308
C     BAND 10:  1390-1480 cm-1 (low - H2O; high - H2O)                           RRTM_TAUMOL.1309
                                                                                 RRTM_TAUMOL.1310
      PARAMETER (MXLAY=203, NBANDS=16)                                           RRTM_TAUMOL.1311
      PARAMETER (NGPT=140, NG10=6, NGS9=108)                                     RRTM_TAUMOL.1312
                                                                                 RRTM_TAUMOL.1313
C  Output                                                                        RRTM_TAUMOL.1314
                                                                                 RRTM_TAUMOL.1315
      COMMON /TAUS/     TAUG(NGPT,MXLAY)                                         RRTM_TAUMOL.1316
                                                                                 RRTM_TAUMOL.1317
C  Input                                                                         RRTM_TAUMOL.1318
                                                                                 RRTM_TAUMOL.1319
      COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                     RRTM_TAUMOL.1320
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       RRTM_TAUMOL.1321
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  RRTM_TAUMOL.1322
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                                 RRTM_TAUMOL.1323
     &                  COLH2O(MXLAY),COLCO2(MXLAY),                             RRTM_TAUMOL.1324
     &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),                RRTM_TAUMOL.1325
     &                  COLO2(MXLAY),CO2MULT(MXLAY)                              RRTM_TAUMOL.1326
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                               RRTM_TAUMOL.1327
     &                  FAC10(MXLAY),FAC11(MXLAY)                                RRTM_TAUMOL.1328
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                           RRTM_TAUMOL.1329
      COMMON /K10C/     KA(5,13,NG10), KB(5,13:59,NG10)                          RRTM_TAUMOL.1330
      COMMON /PF10C/    FRACREFA(NG10), FRACREFB(NG10)                           RRTM_TAUMOL.1331
      COMMON /SP/       PFRAC(NGPT,MXLAY)                                        RRTM_TAUMOL.1332
                                                                                 RRTM_TAUMOL.1333
C This compiler directive was added to insure private common block storage       RRTM_TAUMOL.1334
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_TAUMOL.1335
C carry constants.                                                               RRTM_TAUMOL.1336
C SGI                                                                            RRTM_TAUMOL.1339
C$OMP THREADPRIVATE (/TAUS/,/PROFILE/,/PROFDATA/,/INTFAC/)                       RRTM_TAUMOL.1340
C$OMP THREADPRIVATE (/INTIND/,/SP/)                                              RRTM_TAUMOL.1341
C ***                                                                            RRTM_TAUMOL.1342
                                                                                 RRTM_TAUMOL.1343
      DIMENSION ABSA(65,NG10),ABSB(235,NG10)                                     RRTM_TAUMOL.1344
      REAL KA,KB                                                                 RRTM_TAUMOL.1345
      EQUIVALENCE (KA,ABSA),(KB,ABSB)                                            RRTM_TAUMOL.1346
                                                                                 RRTM_TAUMOL.1347
C     Compute the optical depth by interpolating in ln(pressure) and             RRTM_TAUMOL.1348
C     temperature.                                                               RRTM_TAUMOL.1349
      DO 2500 LAY = 1, LAYTROP                                                   RRTM_TAUMOL.1350
         IND0 = ((JP(LAY)-1)*5+(JT(LAY)-1))*NSPA(10) + 1                         RRTM_TAUMOL.1351
         IND1 = (JP(LAY)*5+(JT1(LAY)-1))*NSPA(10) + 1                            RRTM_TAUMOL.1352
         DO 2000 IG = 1, NG10                                                    RRTM_TAUMOL.1353
            TAUG(NGS9+IG,LAY) = COLH2O(LAY) *                                    RRTM_TAUMOL.1354
     &          (FAC00(LAY) * ABSA(IND0,IG) +                                    RRTM_TAUMOL.1355
                                                                                 RRTM_TAUMOL.1356
     &           FAC10(LAY) * ABSA(IND0+1,IG) +                                  RRTM_TAUMOL.1357
     &           FAC01(LAY) * ABSA(IND1,IG) +                                    RRTM_TAUMOL.1358
     &           FAC11(LAY) * ABSA(IND1+1,IG))                                   RRTM_TAUMOL.1359
            PFRAC(NGS9+IG,LAY) = FRACREFA(IG)                                    RRTM_TAUMOL.1360
 2000    CONTINUE                                                                RRTM_TAUMOL.1361
 2500 CONTINUE                                                                   RRTM_TAUMOL.1362
                                                                                 RRTM_TAUMOL.1363
      DO 3500 LAY = LAYTROP+1, NLAYERS                                           RRTM_TAUMOL.1364
         IND0 = ((JP(LAY)-13)*5+(JT(LAY)-1))*NSPB(10) + 1                        RRTM_TAUMOL.1365
         IND1 = ((JP(LAY)-12)*5+(JT1(LAY)-1))*NSPB(10) + 1                       RRTM_TAUMOL.1366
         DO 3000 IG = 1, NG10                                                    RRTM_TAUMOL.1367
            TAUG(NGS9+IG,LAY) = COLH2O(LAY) *                                    RRTM_TAUMOL.1368
     &          (FAC00(LAY) * ABSB(IND0,IG) +                                    RRTM_TAUMOL.1369
     &           FAC10(LAY) * ABSB(IND0+1,IG) +                                  RRTM_TAUMOL.1370
     &           FAC01(LAY) * ABSB(IND1,IG) +                                    RRTM_TAUMOL.1371
     &           FAC11(LAY) * ABSB(IND1+1,IG))                                   RRTM_TAUMOL.1372
            PFRAC(NGS9+IG,LAY) = FRACREFB(IG)                                    RRTM_TAUMOL.1373
 3000    CONTINUE                                                                RRTM_TAUMOL.1374
 3500 CONTINUE                                                                   RRTM_TAUMOL.1375
                                                                                 RRTM_TAUMOL.1376
      RETURN                                                                     RRTM_TAUMOL.1377
      END                                                                        RRTM_TAUMOL.1378
                                                                                 RRTM_TAUMOL.1379
C******************************************************************************  07NOV00.2470
                                                                                 RRTM_TAUMOL.1381
      SUBROUTINE TAUGB11                                                         RRTM_TAUMOL.1382
                                                                                 RRTM_TAUMOL.1383
C     BAND 11:  1480-1800 cm-1 (low - H2O; high - H2O)                           RRTM_TAUMOL.1384
                                                                                 RRTM_TAUMOL.1385
      PARAMETER (MXLAY=203, NBANDS=16)                                           RRTM_TAUMOL.1386
      PARAMETER (NGPT=140, NG11=8, NGS10=114)                                    RRTM_TAUMOL.1387
                                                                                 RRTM_TAUMOL.1388
C  Output                                                                        RRTM_TAUMOL.1389
                                                                                 RRTM_TAUMOL.1390
      COMMON /TAUS/     TAUG(NGPT,MXLAY)                                         RRTM_TAUMOL.1391
                                                                                 RRTM_TAUMOL.1392
C  Input                                                                         RRTM_TAUMOL.1393
                                                                                 RRTM_TAUMOL.1394
      COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                     RRTM_TAUMOL.1395
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       RRTM_TAUMOL.1396
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  RRTM_TAUMOL.1397
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                                 RRTM_TAUMOL.1398
     &                  COLH2O(MXLAY),COLCO2(MXLAY),                             RRTM_TAUMOL.1399
     &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),                RRTM_TAUMOL.1400
     &                  COLO2(MXLAY),CO2MULT(MXLAY)                              RRTM_TAUMOL.1401
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                               RRTM_TAUMOL.1402
     &                  FAC10(MXLAY),FAC11(MXLAY)                                RRTM_TAUMOL.1403
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                           RRTM_TAUMOL.1404
      COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)          RRTM_TAUMOL.1405
      COMMON /K11C/     KA(5,13,NG11), KB(5,13:59,NG11),                         RRTM_TAUMOL.1406
     &                  SELFREF(10,NG11)                                         RRTM_TAUMOL.1407
      COMMON /PF11C/    FRACREFA(NG11), FRACREFB(NG11)                           RRTM_TAUMOL.1408
      COMMON /SP/       PFRAC(NGPT,MXLAY)                                        RRTM_TAUMOL.1409
                                                                                 RRTM_TAUMOL.1410
C This compiler directive was added to insure private common block storage       RRTM_TAUMOL.1411
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_TAUMOL.1412
C carry constants.                                                               RRTM_TAUMOL.1413
C SGI                                                                            RRTM_TAUMOL.1416
C$OMP THREADPRIVATE (/TAUS/,/PROFILE/,/PROFDATA/,/INTFAC/)                       RRTM_TAUMOL.1417
C$OMP THREADPRIVATE (/INTIND/,/SELF/,/SP/)                                       RRTM_TAUMOL.1418
C ***                                                                            RRTM_TAUMOL.1419
                                                                                 RRTM_TAUMOL.1420
      DIMENSION ABSA(65,NG11),ABSB(235,NG11)                                     RRTM_TAUMOL.1421
      REAL KA,KB                                                                 RRTM_TAUMOL.1422
      EQUIVALENCE (KA,ABSA),(KB,ABSB)                                            RRTM_TAUMOL.1423
                                                                                 RRTM_TAUMOL.1424
C     Compute the optical depth by interpolating in ln(pressure) and             RRTM_TAUMOL.1425
C     temperature.  Below LAYTROP, the water vapor self-continuum                RRTM_TAUMOL.1426
C     is interpolated (in temperature) separately.                               RRTM_TAUMOL.1427
      DO 2500 LAY = 1, LAYTROP                                                   RRTM_TAUMOL.1428
         IND0 = ((JP(LAY)-1)*5+(JT(LAY)-1))*NSPA(11) + 1                         RRTM_TAUMOL.1429
         IND1 = (JP(LAY)*5+(JT1(LAY)-1))*NSPA(11) + 1                            RRTM_TAUMOL.1430
         INDS = INDSELF(LAY)                                                     RRTM_TAUMOL.1431
         DO 2000 IG = 1, NG11                                                    RRTM_TAUMOL.1432
            TAUG(NGS10+IG,LAY) = COLH2O(LAY) *                                   RRTM_TAUMOL.1433
     &          (FAC00(LAY) * ABSA(IND0,IG) +                                    RRTM_TAUMOL.1434
     &           FAC10(LAY) * ABSA(IND0+1,IG) +                                  RRTM_TAUMOL.1435
     &           FAC01(LAY) * ABSA(IND1,IG) +                                    RRTM_TAUMOL.1436
     &           FAC11(LAY) * ABSA(IND1+1,IG) +                                  RRTM_TAUMOL.1437
     &           SELFFAC(LAY) * (SELFREF(INDS,IG) +                              RRTM_TAUMOL.1438
     &           SELFFRAC(LAY) *                                                 RRTM_TAUMOL.1439
     &           (SELFREF(INDS+1,IG) - SELFREF(INDS,IG))))                       RRTM_TAUMOL.1440
            PFRAC(NGS10+IG,LAY) = FRACREFA(IG)                                   RRTM_TAUMOL.1441
 2000    CONTINUE                                                                RRTM_TAUMOL.1442
 2500 CONTINUE                                                                   RRTM_TAUMOL.1443
                                                                                 RRTM_TAUMOL.1444
      DO 3500 LAY = LAYTROP+1, NLAYERS                                           RRTM_TAUMOL.1445
         IND0 = ((JP(LAY)-13)*5+(JT(LAY)-1))*NSPB(11) + 1                        RRTM_TAUMOL.1446
         IND1 = ((JP(LAY)-12)*5+(JT1(LAY)-1))*NSPB(11) + 1                       RRTM_TAUMOL.1447
         DO 3000 IG = 1, NG11                                                    RRTM_TAUMOL.1448
            TAUG(NGS10+IG,LAY) = COLH2O(LAY) *                                   RRTM_TAUMOL.1449
     &          (FAC00(LAY) * ABSB(IND0,IG) +                                    RRTM_TAUMOL.1450
     &           FAC10(LAY) * ABSB(IND0+1,IG) +                                  RRTM_TAUMOL.1451
     &           FAC01(LAY) * ABSB(IND1,IG) +                                    RRTM_TAUMOL.1452
     &           FAC11(LAY) * ABSB(IND1+1,IG))                                   RRTM_TAUMOL.1453
            PFRAC(NGS10+IG,LAY) = FRACREFB(IG)                                   RRTM_TAUMOL.1454
 3000    CONTINUE                                                                RRTM_TAUMOL.1455
 3500 CONTINUE                                                                   RRTM_TAUMOL.1456
                                                                                 RRTM_TAUMOL.1457
      RETURN                                                                     RRTM_TAUMOL.1458
      END                                                                        RRTM_TAUMOL.1459
                                                                                 RRTM_TAUMOL.1460
C----------------------------------------------------------------------------    RRTM_TAUMOL.1461
                                                                                 RRTM_TAUMOL.1462
      SUBROUTINE TAUGB12                                                         RRTM_TAUMOL.1463
                                                                                 RRTM_TAUMOL.1464
C     BAND 12:  1800-2080 cm-1 (low - H2O,CO2; high - nothing)                   RRTM_TAUMOL.1465
                                                                                 RRTM_TAUMOL.1466
      PARAMETER (MXLAY=203, NBANDS=16)                                           RRTM_TAUMOL.1467
      PARAMETER (NGPT=140, NG12=8, NGS11=122)                                    RRTM_TAUMOL.1468
                                                                                 RRTM_TAUMOL.1469
C  Output                                                                        RRTM_TAUMOL.1470
                                                                                 RRTM_TAUMOL.1471
      COMMON /TAUS/     TAUG(NGPT,MXLAY)                                         RRTM_TAUMOL.1472
                                                                                 RRTM_TAUMOL.1473
C  Input                                                                         RRTM_TAUMOL.1474
                                                                                 RRTM_TAUMOL.1475
      COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                     RRTM_TAUMOL.1476
      COMMON /PRECISE/  ONEMINUS                                                 RRTM_TAUMOL.1477
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       RRTM_TAUMOL.1478
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  RRTM_TAUMOL.1479
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                                 RRTM_TAUMOL.1480
     &                  COLH2O(MXLAY),COLCO2(MXLAY),                             RRTM_TAUMOL.1481
     &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),                RRTM_TAUMOL.1482
     &                  COLO2(MXLAY),CO2MULT(MXLAY)                              RRTM_TAUMOL.1483
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                               RRTM_TAUMOL.1484
     &                  FAC10(MXLAY),FAC11(MXLAY)                                RRTM_TAUMOL.1485
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                           RRTM_TAUMOL.1486
      COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)          RRTM_TAUMOL.1487
      COMMON /K12C/     KA(9,5,13,NG12),                                         RRTM_TAUMOL.1488
     &                  SELFREF(10,NG12)                                         RRTM_TAUMOL.1489
      COMMON /PF12C/    FRACREFA(NG12,9)                                         RRTM_TAUMOL.1490
      COMMON /SP/       PFRAC(NGPT,MXLAY)                                        RRTM_TAUMOL.1491
                                                                                 RRTM_TAUMOL.1492
C This compiler directive was added to insure private common block storage       RRTM_TAUMOL.1493
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_TAUMOL.1494
C carry constants.                                                               RRTM_TAUMOL.1495
C SGI                                                                            RRTM_TAUMOL.1498
C$OMP THREADPRIVATE (/TAUS/,/PROFILE/,/PROFDATA/,/INTFAC/)                       RRTM_TAUMOL.1499
C$OMP THREADPRIVATE (/INTIND/,/SELF/,/SP/)                                       RRTM_TAUMOL.1500
C ***                                                                            RRTM_TAUMOL.1501
                                                                                 RRTM_TAUMOL.1502
      DIMENSION ABSA(585,NG12)                                                   RRTM_TAUMOL.1503
      REAL KA                                                                    RRTM_TAUMOL.1504
      EQUIVALENCE (KA,ABSA)                                                      RRTM_TAUMOL.1505
                                                                                 RRTM_TAUMOL.1506
      STRRAT1 = 0.009736757                                                      RRTM_TAUMOL.1507
                                                                                 RRTM_TAUMOL.1508
C     Compute the optical depth by interpolating in ln(pressure),                RRTM_TAUMOL.1509
C     temperature, and appropriate species.  Below LAYTROP, the water            RRTM_TAUMOL.1510
C     vapor self-continuum is interpolated (in temperature) separately.          RRTM_TAUMOL.1511
CDIR$ NOVECTOR                                                                   RRTM_TAUMOL.1512
      DO 2500 LAY = 1, LAYTROP                                                   RRTM_TAUMOL.1513
         SPECCOMB = COLH2O(LAY) + STRRAT1*COLCO2(LAY)                            RRTM_TAUMOL.1514
         SPECPARM = COLH2O(LAY)/SPECCOMB                                         RRTM_TAUMOL.1515
         IF (SPECPARM .GE. ONEMINUS) SPECPARM = ONEMINUS                         RRTM_TAUMOL.1516
         SPECMULT = 8.*(SPECPARM)                                                RRTM_TAUMOL.1517
         JS = 1 + INT(SPECMULT)                                                  RRTM_TAUMOL.1518
         FS = AMOD(SPECMULT,1.0)                                                 RRTM_TAUMOL.1519
         FAC000 = (1. - FS) * FAC00(LAY)                                         RRTM_TAUMOL.1520
         FAC010 = (1. - FS) * FAC10(LAY)                                         RRTM_TAUMOL.1521
         FAC100 = FS * FAC00(LAY)                                                RRTM_TAUMOL.1522
         FAC110 = FS * FAC10(LAY)                                                RRTM_TAUMOL.1523
         FAC001 = (1. - FS) * FAC01(LAY)                                         RRTM_TAUMOL.1524
         FAC011 = (1. - FS) * FAC11(LAY)                                         RRTM_TAUMOL.1525
         FAC101 = FS * FAC01(LAY)                                                RRTM_TAUMOL.1526
         FAC111 = FS * FAC11(LAY)                                                RRTM_TAUMOL.1527
         IND0 = ((JP(LAY)-1)*5+(JT(LAY)-1))*NSPA(12) + JS                        RRTM_TAUMOL.1528
         IND1 = (JP(LAY)*5+(JT1(LAY)-1))*NSPA(12) + JS                           RRTM_TAUMOL.1529
         INDS = INDSELF(LAY)                                                     RRTM_TAUMOL.1530
CDIR$ VECTOR                                                                     RRTM_TAUMOL.1531
         DO 2000 IG = 1, NG12                                                    RRTM_TAUMOL.1532
            TAUG(NGS11+IG,LAY) = SPECCOMB *                                      RRTM_TAUMOL.1533
     &          (FAC000 * ABSA(IND0,IG) +                                        RRTM_TAUMOL.1534
     &           FAC100 * ABSA(IND0+1,IG) +                                      RRTM_TAUMOL.1535
     &           FAC010 * ABSA(IND0+9,IG) +                                      RRTM_TAUMOL.1536
     &           FAC110 * ABSA(IND0+10,IG) +                                     RRTM_TAUMOL.1537
     &           FAC001 * ABSA(IND1,IG) +                                        RRTM_TAUMOL.1538
     &           FAC101 * ABSA(IND1+1,IG) +                                      RRTM_TAUMOL.1539
     &           FAC011 * ABSA(IND1+9,IG) +                                      RRTM_TAUMOL.1540
     &           FAC111 * ABSA(IND1+10,IG)) +                                    RRTM_TAUMOL.1541
     &           COLH2O(LAY) *                                                   RRTM_TAUMOL.1542
     &           SELFFAC(LAY) * (SELFREF(INDS,IG) +                              RRTM_TAUMOL.1543
     &           SELFFRAC(LAY) *                                                 RRTM_TAUMOL.1544
     &           (SELFREF(INDS+1,IG) - SELFREF(INDS,IG)))                        RRTM_TAUMOL.1545
            PFRAC(NGS11+IG,LAY) = FRACREFA(IG,JS) + FS *                         RRTM_TAUMOL.1546
     &           (FRACREFA(IG,JS+1) - FRACREFA(IG,JS))                           RRTM_TAUMOL.1547
 2000    CONTINUE                                                                RRTM_TAUMOL.1548
 2500 CONTINUE                                                                   RRTM_TAUMOL.1549
                                                                                 RRTM_TAUMOL.1550
      DO 3500 LAY = LAYTROP+1, NLAYERS                                           RRTM_TAUMOL.1551
         DO 3000 IG = 1, NG12                                                    RRTM_TAUMOL.1552
            TAUG(NGS11+IG,LAY) = 0.0                                             RRTM_TAUMOL.1553
            PFRAC(NGS11+IG,LAY) = 0.0                                            RRTM_TAUMOL.1554
 3000    CONTINUE                                                                RRTM_TAUMOL.1555
 3500 CONTINUE                                                                   RRTM_TAUMOL.1556
                                                                                 RRTM_TAUMOL.1557
      RETURN                                                                     RRTM_TAUMOL.1558
      END                                                                        RRTM_TAUMOL.1559
                                                                                 RRTM_TAUMOL.1560
C----------------------------------------------------------------------------    RRTM_TAUMOL.1561
                                                                                 RRTM_TAUMOL.1562
      SUBROUTINE TAUGB13                                                         RRTM_TAUMOL.1563
                                                                                 RRTM_TAUMOL.1564
C     BAND 13:  2080-2250 cm-1 (low - H2O,N2O; high - nothing)                   RRTM_TAUMOL.1565
                                                                                 RRTM_TAUMOL.1566
      PARAMETER (MXLAY=203, NBANDS=16)                                           RRTM_TAUMOL.1567
      PARAMETER (NGPT=140, NG13=4, NGS12=130)                                    RRTM_TAUMOL.1568
                                                                                 RRTM_TAUMOL.1569
C  Output                                                                        RRTM_TAUMOL.1570
                                                                                 RRTM_TAUMOL.1571
      COMMON /TAUS/     TAUG(NGPT,MXLAY)                                         RRTM_TAUMOL.1572
                                                                                 RRTM_TAUMOL.1573
C  Input                                                                         RRTM_TAUMOL.1574
                                                                                 RRTM_TAUMOL.1575
      COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                     RRTM_TAUMOL.1576
      COMMON /PRECISE/  ONEMINUS                                                 RRTM_TAUMOL.1577
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       RRTM_TAUMOL.1578
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  RRTM_TAUMOL.1579
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                                 RRTM_TAUMOL.1580
     &                  COLH2O(MXLAY),COLCO2(MXLAY),                             RRTM_TAUMOL.1581
     &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),                RRTM_TAUMOL.1582
     &                  COLO2(MXLAY),CO2MULT(MXLAY)                              RRTM_TAUMOL.1583
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                               RRTM_TAUMOL.1584
     &                  FAC10(MXLAY),FAC11(MXLAY)                                RRTM_TAUMOL.1585
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                           RRTM_TAUMOL.1586
      COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)          RRTM_TAUMOL.1587
      COMMON /K13C/     KA(9,5,13,NG13),                                         RRTM_TAUMOL.1588
     &                  SELFREF(10,NG13)                                         RRTM_TAUMOL.1589
      COMMON /PF13C/    FRACREFA(NG13,9)                                         RRTM_TAUMOL.1590
      COMMON /SP/       PFRAC(NGPT,MXLAY)                                        RRTM_TAUMOL.1591
                                                                                 RRTM_TAUMOL.1592
C This compiler directive was added to insure private common block storage       RRTM_TAUMOL.1593
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_TAUMOL.1594
C carry constants.                                                               RRTM_TAUMOL.1595
C SGI                                                                            RRTM_TAUMOL.1598
C$OMP THREADPRIVATE (/TAUS/,/PROFILE/,/PROFDATA/,/INTFAC/)                       RRTM_TAUMOL.1599
C$OMP THREADPRIVATE (/INTIND/,/SELF/,/SP/)                                       RRTM_TAUMOL.1600
C ***                                                                            RRTM_TAUMOL.1601
                                                                                 RRTM_TAUMOL.1602
      DIMENSION ABSA(585,NG13)                                                   RRTM_TAUMOL.1603
      REAL KA                                                                    RRTM_TAUMOL.1604
      EQUIVALENCE (KA,ABSA)                                                      RRTM_TAUMOL.1605
                                                                                 RRTM_TAUMOL.1606
      STRRAT1 = 16658.87                                                         RRTM_TAUMOL.1607
                                                                                 RRTM_TAUMOL.1608
C     Compute the optical depth by interpolating in ln(pressure),                RRTM_TAUMOL.1609
C     temperature, and appropriate species.  Below LAYTROP, the water            RRTM_TAUMOL.1610
C     vapor self-continuum is interpolated (in temperature) separately.          RRTM_TAUMOL.1611
      DO 2500 LAY = 1, LAYTROP                                                   RRTM_TAUMOL.1612
         SPECCOMB = COLH2O(LAY) + STRRAT1*COLN2O(LAY)                            RRTM_TAUMOL.1613
         SPECPARM = COLH2O(LAY)/SPECCOMB                                         RRTM_TAUMOL.1614
         IF (SPECPARM .GE. ONEMINUS) SPECPARM = ONEMINUS                         RRTM_TAUMOL.1615
         SPECMULT = 8.*(SPECPARM)                                                RRTM_TAUMOL.1616
         JS = 1 + INT(SPECMULT)                                                  RRTM_TAUMOL.1617
         FS = AMOD(SPECMULT,1.0)                                                 RRTM_TAUMOL.1618
         FAC000 = (1. - FS) * FAC00(LAY)                                         RRTM_TAUMOL.1619
         FAC010 = (1. - FS) * FAC10(LAY)                                         RRTM_TAUMOL.1620
         FAC100 = FS * FAC00(LAY)                                                RRTM_TAUMOL.1621
         FAC110 = FS * FAC10(LAY)                                                RRTM_TAUMOL.1622
         FAC001 = (1. - FS) * FAC01(LAY)                                         RRTM_TAUMOL.1623
         FAC011 = (1. - FS) * FAC11(LAY)                                         RRTM_TAUMOL.1624
         FAC101 = FS * FAC01(LAY)                                                RRTM_TAUMOL.1625
         FAC111 = FS * FAC11(LAY)                                                RRTM_TAUMOL.1626
         IND0 = ((JP(LAY)-1)*5+(JT(LAY)-1))*NSPA(13) + JS                        RRTM_TAUMOL.1627
         IND1 = (JP(LAY)*5+(JT1(LAY)-1))*NSPA(13) + JS                           RRTM_TAUMOL.1628
         INDS = INDSELF(LAY)                                                     RRTM_TAUMOL.1629
         DO 2000 IG = 1, NG13                                                    RRTM_TAUMOL.1630
            TAUG(NGS12+IG,LAY) = SPECCOMB *                                      RRTM_TAUMOL.1631
     &          (FAC000 * ABSA(IND0,IG) +                                        RRTM_TAUMOL.1632
     &           FAC100 * ABSA(IND0+1,IG) +                                      RRTM_TAUMOL.1633
     &           FAC010 * ABSA(IND0+9,IG) +                                      RRTM_TAUMOL.1634
     &           FAC110 * ABSA(IND0+10,IG) +                                     RRTM_TAUMOL.1635
     &           FAC001 * ABSA(IND1,IG) +                                        RRTM_TAUMOL.1636
     &           FAC101 * ABSA(IND1+1,IG) +                                      RRTM_TAUMOL.1637
     &           FAC011 * ABSA(IND1+9,IG) +                                      RRTM_TAUMOL.1638
     &           FAC111 * ABSA(IND1+10,IG)) +                                    RRTM_TAUMOL.1639
     &           COLH2O(LAY) *                                                   RRTM_TAUMOL.1640
     &           SELFFAC(LAY) * (SELFREF(INDS,IG) +                              RRTM_TAUMOL.1641
     &           SELFFRAC(LAY) *                                                 RRTM_TAUMOL.1642
     &           (SELFREF(INDS+1,IG) - SELFREF(INDS,IG)))                        RRTM_TAUMOL.1643
            PFRAC(NGS12+IG,LAY) = FRACREFA(IG,JS) + FS *                         RRTM_TAUMOL.1644
     &           (FRACREFA(IG,JS+1) - FRACREFA(IG,JS))                           RRTM_TAUMOL.1645
 2000    CONTINUE                                                                RRTM_TAUMOL.1646
 2500 CONTINUE                                                                   RRTM_TAUMOL.1647
                                                                                 RRTM_TAUMOL.1648
      DO 3500 LAY = LAYTROP+1, NLAYERS                                           RRTM_TAUMOL.1649
         DO 3000 IG = 1, NG13                                                    RRTM_TAUMOL.1650
            TAUG(NGS12+IG,LAY) = 0.0                                             RRTM_TAUMOL.1651
            PFRAC(NGS12+IG,LAY) = 0.0                                            RRTM_TAUMOL.1652
 3000    CONTINUE                                                                RRTM_TAUMOL.1653
 3500 CONTINUE                                                                   RRTM_TAUMOL.1654
                                                                                 RRTM_TAUMOL.1655
      RETURN                                                                     RRTM_TAUMOL.1656
      END                                                                        RRTM_TAUMOL.1657
                                                                                 RRTM_TAUMOL.1658
C******************************************************************************  07NOV00.2471
                                                                                 RRTM_TAUMOL.1660
      SUBROUTINE TAUGB14                                                         RRTM_TAUMOL.1661
                                                                                 RRTM_TAUMOL.1662
C     BAND 14:  2250-2380 cm-1 (low - CO2; high - CO2)                           RRTM_TAUMOL.1663
                                                                                 RRTM_TAUMOL.1664
      PARAMETER (MXLAY=203, NBANDS=16)                                           RRTM_TAUMOL.1665
      PARAMETER (NGPT=140, NG14=2, NGS13=134)                                    RRTM_TAUMOL.1666
                                                                                 RRTM_TAUMOL.1667
C  Output                                                                        RRTM_TAUMOL.1668
                                                                                 RRTM_TAUMOL.1669
      COMMON /TAUS/     TAUG(NGPT,MXLAY)                                         RRTM_TAUMOL.1670
                                                                                 RRTM_TAUMOL.1671
C  Input                                                                         RRTM_TAUMOL.1672
                                                                                 RRTM_TAUMOL.1673
      COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                     RRTM_TAUMOL.1674
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       RRTM_TAUMOL.1675
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  RRTM_TAUMOL.1676
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                                 RRTM_TAUMOL.1677
     &                  COLH2O(MXLAY),COLCO2(MXLAY),                             RRTM_TAUMOL.1678
     &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),                RRTM_TAUMOL.1679
     &                  COLO2(MXLAY),CO2MULT(MXLAY)                              RRTM_TAUMOL.1680
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                               RRTM_TAUMOL.1681
     &                  FAC10(MXLAY),FAC11(MXLAY)                                RRTM_TAUMOL.1682
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                           RRTM_TAUMOL.1683
      COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)          RRTM_TAUMOL.1684
      COMMON /K14C/     KA(5,13,NG14), KB(5,13:59,NG14),                         RRTM_TAUMOL.1685
     &                  SELFREF(10,NG14)                                         RRTM_TAUMOL.1686
      COMMON /PF14C/    FRACREFA(NG14), FRACREFB(NG14)                           RRTM_TAUMOL.1687
      COMMON /SP/       PFRAC(NGPT,MXLAY)                                        RRTM_TAUMOL.1688
                                                                                 RRTM_TAUMOL.1689
C This compiler directive was added to insure private common block storage       RRTM_TAUMOL.1690
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_TAUMOL.1691
C carry constants.                                                               RRTM_TAUMOL.1692
C SGI                                                                            RRTM_TAUMOL.1695
C$OMP THREADPRIVATE (/TAUS/,/PROFILE/,/PROFDATA/,/INTFAC/)                       RRTM_TAUMOL.1696
C$OMP THREADPRIVATE (/INTIND/,/SELF/,/SP/)                                       RRTM_TAUMOL.1697
C ***                                                                            RRTM_TAUMOL.1698
                                                                                 RRTM_TAUMOL.1699
      DIMENSION ABSA(65,NG14),ABSB(235,NG14)                                     RRTM_TAUMOL.1700
      REAL KA,KB                                                                 RRTM_TAUMOL.1701
      EQUIVALENCE (KA,ABSA),(KB,ABSB)                                            RRTM_TAUMOL.1702
                                                                                 RRTM_TAUMOL.1703
C     Compute the optical depth by interpolating in ln(pressure) and             RRTM_TAUMOL.1704
C     temperature.  Below LAYTROP, the water vapor self-continuum                RRTM_TAUMOL.1705
C     is interpolated (in temperature) separately.                               RRTM_TAUMOL.1706
      DO 2500 LAY = 1, LAYTROP                                                   RRTM_TAUMOL.1707
         IND0 = ((JP(LAY)-1)*5+(JT(LAY)-1))*NSPA(14) + 1                         RRTM_TAUMOL.1708
         IND1 = (JP(LAY)*5+(JT1(LAY)-1))*NSPA(14) + 1                            RRTM_TAUMOL.1709
         INDS = INDSELF(LAY)                                                     RRTM_TAUMOL.1710
         DO 2000 IG = 1, NG14                                                    RRTM_TAUMOL.1711
            TAUG(NGS13+IG,LAY) = COLCO2(LAY) *                                   RRTM_TAUMOL.1712
     &          (FAC00(LAY) * ABSA(IND0,IG) +                                    RRTM_TAUMOL.1713
     &           FAC10(LAY) * ABSA(IND0+1,IG) +                                  RRTM_TAUMOL.1714
     &           FAC01(LAY) * ABSA(IND1,IG) +                                    RRTM_TAUMOL.1715
     &           FAC11(LAY) * ABSA(IND1+1,IG) +                                  RRTM_TAUMOL.1716
     &           SELFFAC(LAY) * (SELFREF(INDS,IG) +                              RRTM_TAUMOL.1717
     &           SELFFRAC(LAY) *                                                 RRTM_TAUMOL.1718
     &           (SELFREF(INDS+1,IG) - SELFREF(INDS,IG))))                       RRTM_TAUMOL.1719
            PFRAC(NGS13+IG,LAY) = FRACREFA(IG)                                   RRTM_TAUMOL.1720
 2000    CONTINUE                                                                RRTM_TAUMOL.1721
 2500 CONTINUE                                                                   RRTM_TAUMOL.1722
                                                                                 RRTM_TAUMOL.1723
      DO 3500 LAY = LAYTROP+1, NLAYERS                                           RRTM_TAUMOL.1724
         IND0 = ((JP(LAY)-13)*5+(JT(LAY)-1))*NSPB(14) + 1                        RRTM_TAUMOL.1725
         IND1 = ((JP(LAY)-12)*5+(JT1(LAY)-1))*NSPB(14) + 1                       RRTM_TAUMOL.1726
         DO 3000 IG = 1, NG14                                                    RRTM_TAUMOL.1727
            TAUG(NGS13+IG,LAY) = COLCO2(LAY) *                                   RRTM_TAUMOL.1728
     &          (FAC00(LAY) * ABSB(IND0,IG) +                                    RRTM_TAUMOL.1729
     &           FAC10(LAY) * ABSB(IND0+1,IG) +                                  RRTM_TAUMOL.1730
     &           FAC01(LAY) * ABSB(IND1,IG) +                                    RRTM_TAUMOL.1731
     &           FAC11(LAY) * ABSB(IND1+1,IG))                                   RRTM_TAUMOL.1732
            PFRAC(NGS13+IG,LAY) = FRACREFB(IG)                                   RRTM_TAUMOL.1733
 3000    CONTINUE                                                                RRTM_TAUMOL.1734
 3500 CONTINUE                                                                   RRTM_TAUMOL.1735
                                                                                 RRTM_TAUMOL.1736
      RETURN                                                                     RRTM_TAUMOL.1737
      END                                                                        RRTM_TAUMOL.1738
                                                                                 RRTM_TAUMOL.1739
C----------------------------------------------------------------------------    RRTM_TAUMOL.1740
                                                                                 RRTM_TAUMOL.1741
      SUBROUTINE TAUGB15                                                         RRTM_TAUMOL.1742
                                                                                 RRTM_TAUMOL.1743
C     BAND 15:  2380-2600 cm-1 (low - N2O,CO2; high - nothing)                   RRTM_TAUMOL.1744
                                                                                 RRTM_TAUMOL.1745
      PARAMETER (MXLAY=203, NBANDS=16)                                           RRTM_TAUMOL.1746
      PARAMETER (NGPT=140, NG15=2, NGS14=136)                                    RRTM_TAUMOL.1747
                                                                                 RRTM_TAUMOL.1748
C  Output                                                                        RRTM_TAUMOL.1749
                                                                                 RRTM_TAUMOL.1750
      COMMON /TAUS/     TAUG(NGPT,MXLAY)                                         RRTM_TAUMOL.1751
                                                                                 RRTM_TAUMOL.1752
C  Input                                                                         RRTM_TAUMOL.1753
                                                                                 RRTM_TAUMOL.1754
      COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                     RRTM_TAUMOL.1755
      COMMON /PRECISE/  ONEMINUS                                                 RRTM_TAUMOL.1756
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       RRTM_TAUMOL.1757
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  RRTM_TAUMOL.1758
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                                 RRTM_TAUMOL.1759
     &                  COLH2O(MXLAY),COLCO2(MXLAY),                             RRTM_TAUMOL.1760
     &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),                RRTM_TAUMOL.1761
     &                  COLO2(MXLAY),CO2MULT(MXLAY)                              RRTM_TAUMOL.1762
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                               RRTM_TAUMOL.1763
     &                  FAC10(MXLAY),FAC11(MXLAY)                                RRTM_TAUMOL.1764
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                           RRTM_TAUMOL.1765
      COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)          RRTM_TAUMOL.1766
      COMMON /K15C/     KA(9,5,13,NG15),                                         RRTM_TAUMOL.1767
     &                  SELFREF(10,NG15)                                         RRTM_TAUMOL.1768
      COMMON /PF15C/    FRACREFA(NG15,9)                                         RRTM_TAUMOL.1769
      COMMON /SP/       PFRAC(NGPT,MXLAY)                                        RRTM_TAUMOL.1770
                                                                                 RRTM_TAUMOL.1771
C This compiler directive was added to insure private common block storage       RRTM_TAUMOL.1772
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_TAUMOL.1773
C carry constants.                                                               RRTM_TAUMOL.1774
C SGI                                                                            RRTM_TAUMOL.1777
C$OMP THREADPRIVATE (/TAUS/,/PROFILE/,/PROFDATA/,/INTFAC/)                       RRTM_TAUMOL.1778
C$OMP THREADPRIVATE (/INTIND/,/SELF/,/SP/)                                       RRTM_TAUMOL.1779
C ***                                                                            RRTM_TAUMOL.1780
                                                                                 RRTM_TAUMOL.1781
      DIMENSION ABSA(585,NG15)                                                   RRTM_TAUMOL.1782
      REAL KA                                                                    RRTM_TAUMOL.1783
      EQUIVALENCE (KA,ABSA)                                                      RRTM_TAUMOL.1784
                                                                                 RRTM_TAUMOL.1785
      STRRAT1 = 0.2883201                                                        RRTM_TAUMOL.1786
                                                                                 RRTM_TAUMOL.1787
C     Compute the optical depth by interpolating in ln(pressure),                RRTM_TAUMOL.1788
C     temperature, and appropriate species.  Below LAYTROP, the water            RRTM_TAUMOL.1789
C     vapor self-continuum is interpolated (in temperature) separately.          RRTM_TAUMOL.1790
      DO 2500 LAY = 1, LAYTROP                                                   RRTM_TAUMOL.1791
         SPECCOMB = COLN2O(LAY) + STRRAT1*COLCO2(LAY)                            RRTM_TAUMOL.1792
         SPECPARM = COLN2O(LAY)/SPECCOMB                                         RRTM_TAUMOL.1793
         IF (SPECPARM .GE. ONEMINUS) SPECPARM = ONEMINUS                         RRTM_TAUMOL.1794
         SPECMULT = 8.*(SPECPARM)                                                RRTM_TAUMOL.1795
         JS = 1 + INT(SPECMULT)                                                  RRTM_TAUMOL.1796
         FS = AMOD(SPECMULT,1.0)                                                 RRTM_TAUMOL.1797
         FAC000 = (1. - FS) * FAC00(LAY)                                         RRTM_TAUMOL.1798
         FAC010 = (1. - FS) * FAC10(LAY)                                         RRTM_TAUMOL.1799
         FAC100 = FS * FAC00(LAY)                                                RRTM_TAUMOL.1800
         FAC110 = FS * FAC10(LAY)                                                RRTM_TAUMOL.1801
         FAC001 = (1. - FS) * FAC01(LAY)                                         RRTM_TAUMOL.1802
         FAC011 = (1. - FS) * FAC11(LAY)                                         RRTM_TAUMOL.1803
         FAC101 = FS * FAC01(LAY)                                                RRTM_TAUMOL.1804
         FAC111 = FS * FAC11(LAY)                                                RRTM_TAUMOL.1805
         IND0 = ((JP(LAY)-1)*5+(JT(LAY)-1))*NSPA(15) + JS                        RRTM_TAUMOL.1806
         IND1 = (JP(LAY)*5+(JT1(LAY)-1))*NSPA(15) + JS                           RRTM_TAUMOL.1807
         INDS = INDSELF(LAY)                                                     RRTM_TAUMOL.1808
         DO 2000 IG = 1, NG15                                                    RRTM_TAUMOL.1809
            TAUG(NGS14+IG,LAY) = SPECCOMB *                                      RRTM_TAUMOL.1810
     &          (FAC000 * ABSA(IND0,IG) +                                        RRTM_TAUMOL.1811
     &           FAC100 * ABSA(IND0+1,IG) +                                      RRTM_TAUMOL.1812
     &           FAC010 * ABSA(IND0+9,IG) +                                      RRTM_TAUMOL.1813
     &           FAC110 * ABSA(IND0+10,IG) +                                     RRTM_TAUMOL.1814
     &           FAC001 * ABSA(IND1,IG) +                                        RRTM_TAUMOL.1815
     &           FAC101 * ABSA(IND1+1,IG) +                                      RRTM_TAUMOL.1816
     &           FAC011 * ABSA(IND1+9,IG) +                                      RRTM_TAUMOL.1817
     &           FAC111 * ABSA(IND1+10,IG)) +                                    RRTM_TAUMOL.1818
     &           COLH2O(LAY) *                                                   RRTM_TAUMOL.1819
     &           SELFFAC(LAY) * (SELFREF(INDS,IG) +                              RRTM_TAUMOL.1820
     &           SELFFRAC(LAY) *                                                 RRTM_TAUMOL.1821
     &           (SELFREF(INDS+1,IG) - SELFREF(INDS,IG)))                        RRTM_TAUMOL.1822
            PFRAC(NGS14+IG,LAY) = FRACREFA(IG,JS) + FS *                         RRTM_TAUMOL.1823
     &           (FRACREFA(IG,JS+1) - FRACREFA(IG,JS))                           RRTM_TAUMOL.1824
 2000    CONTINUE                                                                RRTM_TAUMOL.1825
 2500 CONTINUE                                                                   RRTM_TAUMOL.1826
                                                                                 RRTM_TAUMOL.1827
      DO 3500 LAY = LAYTROP+1, NLAYERS                                           RRTM_TAUMOL.1828
         DO 3000 IG = 1, NG15                                                    RRTM_TAUMOL.1829
            TAUG(NGS14+IG,LAY) = 0.0                                             RRTM_TAUMOL.1830
            PFRAC(NGS14+IG,LAY) = 0.0                                            RRTM_TAUMOL.1831
 3000    CONTINUE                                                                RRTM_TAUMOL.1832
 3500 CONTINUE                                                                   RRTM_TAUMOL.1833
                                                                                 RRTM_TAUMOL.1834
      RETURN                                                                     RRTM_TAUMOL.1835
      END                                                                        RRTM_TAUMOL.1836
                                                                                 RRTM_TAUMOL.1837
C----------------------------------------------------------------------------    RRTM_TAUMOL.1838
                                                                                 RRTM_TAUMOL.1839
      SUBROUTINE TAUGB16                                                         RRTM_TAUMOL.1840
                                                                                 RRTM_TAUMOL.1841
C     BAND 16:  2600-3000 cm-1 (low - H2O,CH4; high - nothing)                   RRTM_TAUMOL.1842
                                                                                 RRTM_TAUMOL.1843
      PARAMETER (MXLAY=203, NBANDS=16)                                           RRTM_TAUMOL.1844
      PARAMETER (NGPT=140, NG16=2, NGS15=138)                                    RRTM_TAUMOL.1845
                                                                                 RRTM_TAUMOL.1846
C  Output                                                                        RRTM_TAUMOL.1847
                                                                                 RRTM_TAUMOL.1848
      COMMON /TAUS/     TAUG(NGPT,MXLAY)                                         RRTM_TAUMOL.1849
                                                                                 RRTM_TAUMOL.1850
C  Input                                                                         RRTM_TAUMOL.1851
      COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                     RRTM_TAUMOL.1852
      COMMON /PRECISE/  ONEMINUS                                                 RRTM_TAUMOL.1853
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       RRTM_TAUMOL.1854
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  RRTM_TAUMOL.1855
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                                 RRTM_TAUMOL.1856
     &                  COLH2O(MXLAY),COLCO2(MXLAY),                             RRTM_TAUMOL.1857
     &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),                RRTM_TAUMOL.1858
     &                  COLO2(MXLAY),CO2MULT(MXLAY)                              RRTM_TAUMOL.1859
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                               RRTM_TAUMOL.1860
     &                  FAC10(MXLAY),FAC11(MXLAY)                                RRTM_TAUMOL.1861
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                           RRTM_TAUMOL.1862
      COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)          RRTM_TAUMOL.1863
      COMMON /K16C/     KA(9,5,13,NG16),                                         RRTM_TAUMOL.1864
     &                  SELFREF(10,NG16)                                         RRTM_TAUMOL.1865
      COMMON /PF16C/    FRACREFA(NG16,9)                                         RRTM_TAUMOL.1866
      COMMON /SP/       PFRAC(NGPT,MXLAY)                                        RRTM_TAUMOL.1867
                                                                                 RRTM_TAUMOL.1868
C This compiler directive was added to insure private common block storage       RRTM_TAUMOL.1869
C in multi-tasked mode on a CRAY or SGI for all commons except those that        RRTM_TAUMOL.1870
C carry constants.                                                               RRTM_TAUMOL.1871
C SGI                                                                            RRTM_TAUMOL.1874
C$OMP THREADPRIVATE (/TAUS/,/PROFILE/,/PROFDATA/,/INTFAC/)                       RRTM_TAUMOL.1875
C$OMP THREADPRIVATE (/INTIND/,/SELF/,/SP/)                                       RRTM_TAUMOL.1876
C ***                                                                            RRTM_TAUMOL.1877
                                                                                 RRTM_TAUMOL.1878
      DIMENSION ABSA(585,NG16)                                                   RRTM_TAUMOL.1879
      REAL KA                                                                    RRTM_TAUMOL.1880
      EQUIVALENCE (KA,ABSA)                                                      RRTM_TAUMOL.1881
                                                                                 RRTM_TAUMOL.1882
      STRRAT1 = 830.411                                                          RRTM_TAUMOL.1883
                                                                                 RRTM_TAUMOL.1884
C     Compute the optical depth by interpolating in ln(pressure),                RRTM_TAUMOL.1885
C     temperature, and appropriate species.  Below LAYTROP, the water            RRTM_TAUMOL.1886
C     vapor self-continuum is interpolated (in temperature) separately.          RRTM_TAUMOL.1887
      DO 2500 LAY = 1, LAYTROP                                                   RRTM_TAUMOL.1888
         SPECCOMB = COLH2O(LAY) + STRRAT1*COLCH4(LAY)                            RRTM_TAUMOL.1889
         SPECPARM = COLH2O(LAY)/SPECCOMB                                         RRTM_TAUMOL.1890
         IF (SPECPARM .GE. ONEMINUS) SPECPARM = ONEMINUS                         RRTM_TAUMOL.1891
         SPECMULT = 8.*(SPECPARM)                                                RRTM_TAUMOL.1892
         JS = 1 + INT(SPECMULT)                                                  RRTM_TAUMOL.1893
         FS = AMOD(SPECMULT,1.0)                                                 RRTM_TAUMOL.1894
         FAC000 = (1. - FS) * FAC00(LAY)                                         RRTM_TAUMOL.1895
         FAC010 = (1. - FS) * FAC10(LAY)                                         RRTM_TAUMOL.1896
         FAC100 = FS * FAC00(LAY)                                                RRTM_TAUMOL.1897
         FAC110 = FS * FAC10(LAY)                                                RRTM_TAUMOL.1898
         FAC001 = (1. - FS) * FAC01(LAY)                                         RRTM_TAUMOL.1899
         FAC011 = (1. - FS) * FAC11(LAY)                                         RRTM_TAUMOL.1900
         FAC101 = FS * FAC01(LAY)                                                RRTM_TAUMOL.1901
         FAC111 = FS * FAC11(LAY)                                                RRTM_TAUMOL.1902
         IND0 = ((JP(LAY)-1)*5+(JT(LAY)-1))*NSPA(16) + JS                        RRTM_TAUMOL.1903
         IND1 = (JP(LAY)*5+(JT1(LAY)-1))*NSPA(16) + JS                           RRTM_TAUMOL.1904
         INDS = INDSELF(LAY)                                                     RRTM_TAUMOL.1905
         DO 2000 IG = 1, NG16                                                    RRTM_TAUMOL.1906
            TAUG(NGS15+IG,LAY) = SPECCOMB *                                      RRTM_TAUMOL.1907
     &          (FAC000 * ABSA(IND0,IG) +                                        RRTM_TAUMOL.1908
     &           FAC100 * ABSA(IND0+1,IG) +                                      RRTM_TAUMOL.1909
     &           FAC010 * ABSA(IND0+9,IG) +                                      RRTM_TAUMOL.1910
     &           FAC110 * ABSA(IND0+10,IG) +                                     RRTM_TAUMOL.1911
     &           FAC001 * ABSA(IND1,IG) +                                        RRTM_TAUMOL.1912
     &           FAC101 * ABSA(IND1+1,IG) +                                      RRTM_TAUMOL.1913
     &           FAC011 * ABSA(IND1+9,IG) +                                      RRTM_TAUMOL.1914
     &           FAC111 * ABSA(IND1+10,IG)) +                                    RRTM_TAUMOL.1915
     &           COLH2O(LAY) *                                                   RRTM_TAUMOL.1916
     &           SELFFAC(LAY) * (SELFREF(INDS,IG) +                              RRTM_TAUMOL.1917
     &           SELFFRAC(LAY) *                                                 RRTM_TAUMOL.1918
     &           (SELFREF(INDS+1,IG) - SELFREF(INDS,IG)))                        RRTM_TAUMOL.1919
            PFRAC(NGS15+IG,LAY) = FRACREFA(IG,JS) + FS *                         RRTM_TAUMOL.1920
     &           (FRACREFA(IG,JS+1) - FRACREFA(IG,JS))                           RRTM_TAUMOL.1921
 2000    CONTINUE                                                                RRTM_TAUMOL.1922
 2500 CONTINUE                                                                   RRTM_TAUMOL.1923
                                                                                 RRTM_TAUMOL.1924
      DO 3500 LAY = LAYTROP+1, NLAYERS                                           RRTM_TAUMOL.1925
         DO 3000 IG = 1, NG16                                                    RRTM_TAUMOL.1926
            TAUG(NGS15+IG,LAY) = 0.0                                             RRTM_TAUMOL.1927
            PFRAC(NGS15+IG,LAY) = 0.0                                            RRTM_TAUMOL.1928
 3000    CONTINUE                                                                RRTM_TAUMOL.1929
 3500 CONTINUE                                                                   RRTM_TAUMOL.1930
                                                                                 RRTM_TAUMOL.1931
      RETURN                                                                     RRTM_TAUMOL.1932
      END                                                                        RRTM_TAUMOL.1933
                                                                                 RRTM_TAUMOL.1934
