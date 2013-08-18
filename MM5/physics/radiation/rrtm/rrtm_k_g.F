C **************************************************************************     RRTM_K_G.1
C      SUBROUTINE KGBn                                                           RRTM_K_G.2
C **************************************************************************     RRTM_K_G.3
C  RRTM Longwave Radiative Transfer Model                                        RRTM_K_G.4
C  Atmospheric and Environmental Research, Inc., Cambridge, MA                   RRTM_K_G.5
C                                                                                RRTM_K_G.6
C  Original version:       E. J. Mlawer, et al.                                  RRTM_K_G.7
C  Revision for NCAR CCM:  Michael J. Iacono; September, 1998                    RRTM_K_G.8
C                                                                                RRTM_K_G.9
C  This module contains 16 SUBROUTINE statements that include the                RRTM_K_G.10
C  absorption coefficients and other data for each of the 16 longwave            RRTM_K_G.11
C  spectral bands used in RRTM.  Here, the data are defined for 16               RRTM_K_G.12
C  g-points, or sub-intervals, per band.  These data are combined and            RRTM_K_G.13
C  weighted using a mapping procedure in routine RRTMINIT to reduce              RRTM_K_G.14
C  the total number of g-points from 256 to 140 for use in the CCM.              RRTM_K_G.15
C **************************************************************************     RRTM_K_G.16
                                                                                 RRTM_K_G.17
      SUBROUTINE KGB1                                                            RRTM_K_G.18
      IMPLICIT NONE                                                              07NOV00.2217
                                                                                 RRTM_K_G.19
      INTEGER MG                                                                 07NOV00.2218
      PARAMETER (MG = 16)                                                        RRTM_K_G.20
      REAL KA(5,13,MG),KB(5,13:59,MG)                                            RRTM_K_G.21
      REAL      SELFREF(10,MG)                                                   07NOV00.2219
      EXTERNAL DM_IONODE                                                         07NOV00.2220
      LOGICAL DM_IONODE                                                          07NOV00.2221
                                                                                 RRTM_K_G.23
      COMMON /K1/ KA ,KB, SELFREF                                                RRTM_K_G.24
                                                                                 RRTM_K_G.25
C     The array KA contains absorption coefs at the 16 chosen g-values           RRTM_K_G.26
C     for a range of pressure levels > ~100mb and temperatures.  The first       RRTM_K_G.27
C     index in the array, JT, which runs from 1 to 5, corresponds to             RRTM_K_G.28
C     different temperatures.  More specifically, JT = 3 means that the          RRTM_K_G.29
C     data are for the corresponding TREF for this  pressure level,              RRTM_K_G.30
C     JT = 2 refers to the temperatureTREF-15, JT = 1 is for TREF-30,            RRTM_K_G.31
C     JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  The second              RRTM_K_G.32
C     index, JP, runs from 1 to 13 and refers to the corresponding               RRTM_K_G.33
C     pressure level in PREF (e.g. JP = 1 is for a pressure of 1053.63 mb).      RRTM_K_G.34
C     The third index, IG, goes from 1 to 16, and tells us which                 RRTM_K_G.35
C     g-interval the absorption coefficients are for.                            RRTM_K_G.36
                                                                                 RRTM_K_G.453
C     The array KB contains absorption coefs at the 16 chosen g-values           RRTM_K_G.454
C     for a range of pressure levels < ~100mb and temperatures. The first        RRTM_K_G.455
C     index in the array, JT, which runs from 1 to 5, corresponds to             RRTM_K_G.456
C     different temperatures.  More specifically, JT = 3 means that the          RRTM_K_G.457
C     data are for the reference temperature TREF for this pressure              RRTM_K_G.458
C     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for             RRTM_K_G.459
C     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.                 RRTM_K_G.460
C     The second index, JP, runs from 13 to 59 and refers to the JPth            RRTM_K_G.461
C     reference pressure level (see taumol.f for the value of these              RRTM_K_G.462
C     pressure levels in mb).  The third index, IG, goes from 1 to 16,           RRTM_K_G.463
C     and tells us which g-interval the absorption coefficients are for.         RRTM_K_G.464
                                                                                 RRTM_K_G.1969
C     The array SELFREF contains the coefficient of the water vapor              RRTM_K_G.1970
C     self-continuum (including the energy term).  The first index               RRTM_K_G.1971
C     refers to temperature in 7.2 degree increments.  For instance,             RRTM_K_G.1972
C     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,           RRTM_K_G.1973
C     etc.  The second index runs over the g-channel (1 to 16).                  RRTM_K_G.1974
                                                                                 07NOV00.2222
      IF (DM_IONODE()) THEN                                                      07NOV00.2223
         READ (20,ERR=9010) KA, KB, SELFREF                                      07NOV00.2224
      END IF                                                                     07NOV00.2225
      CALL DM_BCAST_REALS ( SELFREF , 10*MG )                                    07NOV00.2226
      CALL DM_BCAST_REALS ( KA      , 5*13*MG )                                  07NOV00.2227
      CALL DM_BCAST_REALS ( KB      , 5*(59-13+1)*MG )                           07NOV00.2228
                                                                                 07NOV00.2229
      RETURN                                                                     07NOV00.2230
9010  PRINT *, 'ERROR IN READING RRTM DATA FILE KGB1'                            07NOV00.2231
      STOP 'IN RRTM ROUTINE KGB1'                                                07NOV00.2232
                                                                                 RRTM_K_G.2023
      END                                                                        RRTM_K_G.2024
                                                                                 RRTM_K_G.2025
      SUBROUTINE KGB2                                                            RRTM_K_G.2026
      IMPLICIT NONE                                                              07NOV00.2233
                                                                                 RRTM_K_G.2027
      INTEGER MG                                                                 07NOV00.2234
      PARAMETER (MG = 16)                                                        RRTM_K_G.2028
      REAL KA(5,13,MG),KB(5,13:59,MG)                                            RRTM_K_G.2029
      REAL      SELFREF(10,MG)                                                   07NOV00.2235
      EXTERNAL DM_IONODE                                                         07NOV00.2236
      LOGICAL DM_IONODE                                                          07NOV00.2237
                                                                                 RRTM_K_G.2031
      COMMON /K2/ KA ,KB, SELFREF                                                RRTM_K_G.2032
                                                                                 RRTM_K_G.2033
C     The array KA contains absorption coefs at the 16 chosen g-values           RRTM_K_G.2034
C     for a range of pressure levels > ~100mb and temperatures.  The first       RRTM_K_G.2035
C     index in the array, JT, which runs from 1 to 5, corresponds to             RRTM_K_G.2036
C     different temperatures.  More specifically, JT = 3 means that the          RRTM_K_G.2037
C     data are for the corresponding TREF for this  pressure level,              RRTM_K_G.2038
C     JT = 2 refers to the temperatureTREF-15, JT = 1 is for TREF-30,            RRTM_K_G.2039
C     JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  The second              RRTM_K_G.2040
C     index, JP, runs from 1 to 13 and refers to the corresponding               RRTM_K_G.2041
C     pressure level in PREF (e.g. JP = 1 is for a pressure of 1053.63 mb).      RRTM_K_G.2042
C     The third index, IG, goes from 1 to 16, and tells us which                 RRTM_K_G.2043
C     g-interval the absorption coefficients are for.                            RRTM_K_G.2044
                                                                                 RRTM_K_G.2461
C     The array KB contains absorption coefs at the 16 chosen g-values           RRTM_K_G.2462
C     for a range of pressure levels < ~100mb and temperatures. The first        RRTM_K_G.2463
C     index in the array, JT, which runs from 1 to 5, corresponds to             RRTM_K_G.2464
C     different temperatures.  More specifically, JT = 3 means that the          RRTM_K_G.2465
C     data are for the reference temperature TREF for this pressure              RRTM_K_G.2466
C     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for             RRTM_K_G.2467
C     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.                 RRTM_K_G.2468
C     The second index, JP, runs from 13 to 59 and refers to the JPth            RRTM_K_G.2469
C     reference pressure level (see taumol.f for the value of these              RRTM_K_G.2470
C     pressure levels in mb).  The third index, IG, goes from 1 to 16,           RRTM_K_G.2471
C     and tells us which g-interval the absorption coefficients are for.         RRTM_K_G.2472
                                                                                 RRTM_K_G.3977
C     The array SELFREF contains the coefficient of the water vapor              RRTM_K_G.3978
C     self-continuum (including the energy term).  The first index               RRTM_K_G.3979
C     refers to temperature in 7.2 degree increments.  For instance,             RRTM_K_G.3980
C     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,           RRTM_K_G.3981
C     etc.  The second index runs over the g-channel (1 to 16).                  RRTM_K_G.3982
                                                                                 07NOV00.2238
      IF (DM_IONODE()) THEN                                                      07NOV00.2239
         READ (20,ERR=9010) KA, KB, SELFREF                                      07NOV00.2240
      END IF                                                                     07NOV00.2241
      CALL DM_BCAST_REALS ( SELFREF , 10*MG )                                    07NOV00.2242
      CALL DM_BCAST_REALS ( KA      , 5*13*MG )                                  07NOV00.2243
      CALL DM_BCAST_REALS ( KB      , 5*(59-13+1)*MG )                           07NOV00.2244
                                                                                 07NOV00.2245
      RETURN                                                                     07NOV00.2246
9010  PRINT *, 'ERROR IN READING RRTM DATA FILE KGB2'                            07NOV00.2247
      STOP 'IN RRTM ROUTINE KGB2'                                                07NOV00.2248
                                                                                 RRTM_K_G.4031
      END                                                                        RRTM_K_G.4032
                                                                                 RRTM_K_G.4033
      SUBROUTINE KGB3                                                            RRTM_K_G.4034
      IMPLICIT NONE                                                              07NOV00.2249
                                                                                 RRTM_K_G.4035
      INTEGER MG                                                                 07NOV00.2250
      PARAMETER (MG = 16)                                                        RRTM_K_G.4036
      REAL KA(10,5,13,MG),KB(5,5,13:59,MG)                                       RRTM_K_G.4037
      REAL      SELFREF(10,MG)                                                   07NOV00.2251
      EXTERNAL DM_IONODE                                                         07NOV00.2252
      LOGICAL DM_IONODE                                                          07NOV00.2253
                                                                                 RRTM_K_G.4039
      COMMON /K3/ KA ,KB, SELFREF                                                RRTM_K_G.4040
                                                                                 RRTM_K_G.4041
C     The array KA contains absorption coefs for each of the 16 g-intervals      RRTM_K_G.4042
C     for a range of pressure levels > ~100mb, temperatures, and ratios          RRTM_K_G.4043
C     of water vapor to CO2.  The first index in the array, JS, runs             RRTM_K_G.4044
C     from 1 to 10, and corresponds to different water vapor to CO2 ratios,      RRTM_K_G.4045
C     as expressed through the binary species parameter eta, defined as          RRTM_K_G.4046
C     eta = h2o/(h20 + (rat) * co2), where rat is the ratio of the integrated    RRTM_K_G.4047
C     line strength in the band of co2 to that of h2o.  For instance,            RRTM_K_G.4048
C     JS=1 refers to dry air (eta = 0), JS = 10 corresponds to eta = 1.0.        RRTM_K_G.4049
C     The 2nd index in the array, JT, which runs from 1 to 5, corresponds        RRTM_K_G.4050
C     to different temperatures.  More specifically, JT = 3 means that the       RRTM_K_G.4051
C     data are for the reference temperature TREF for this  pressure             RRTM_K_G.4052
C     level, JT = 2 refers to the temperature                                    RRTM_K_G.4053
C     TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5          RRTM_K_G.4054
C     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers         RRTM_K_G.4055
C     to the reference pressure level (e.g. JP = 1 is for a                      RRTM_K_G.4056
C     pressure of 1053.63 mb).  The fourth index, IG, goes from 1 to 16,         RRTM_K_G.4057
C     and tells us which g-interval the absorption coefficients are for.         RRTM_K_G.4058
                                                                                 RRTM_K_G.6139
C     The array KB contains absorption coefs for each of the 16 g-intervals      RRTM_K_G.6140
C     for a range of pressure levels  < ~100mb, temperatures, and ratios         RRTM_K_G.6141
C     of H2O to CO2.  The first index in the array, JS, runs from 1 to 5,        RRTM_K_G.6142
C     and corresponds to different H2O to CO2 ratios, as expressed through       RRTM_K_G.6143
C     the binary species parameter eta, defined as eta = H2O/(H2O+RAT*CO2),      RRTM_K_G.6144
C     where RAT is the ratio of the integrated line strength in the band         RRTM_K_G.6145
C     of CO2 to that of H2O.  For instance, JS=1 refers to no H2O,               RRTM_K_G.6146
C     JS = 2 corresponds to eta = 0.25, etc.  The second index, JT, which        RRTM_K_G.6147
C     runs from 1 to 5, corresponds to different temperatures.  More             RRTM_K_G.6148
C     specifically, JT = 3 means that the data are for the corresponding         RRTM_K_G.6149
C     reference temperature TREF for this  pressure level, JT = 2 refers         RRTM_K_G.6150
C     to the TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and          RRTM_K_G.6151
C     JT = 5 is for TREF+30.  The third index, JP, runs from 13 to 59 and        RRTM_K_G.6152
C     refers to the corresponding pressure level in PREF (e.g. JP = 13 is        RRTM_K_G.6153
C     for a pressure of 95.5835 mb).  The fourth index, IG, goes from 1 to       RRTM_K_G.6154
C     16, and tells us which g-interval the absorption coefficients are for.     RRTM_K_G.6155
                                                                                 RRTM_K_G.10668
C     The array SELFREF contains the coefficient of the water vapor              RRTM_K_G.10669
C     self-continuum (including the energy term).  The first index               RRTM_K_G.10670
C     refers to temperature in 7.2 degree increments.  For instance,             RRTM_K_G.10671
C     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,           RRTM_K_G.10672
C     etc.  The second index runs over the g-channel (1 to 16).                  RRTM_K_G.10673
                                                                                 07NOV00.2254
      IF (DM_IONODE()) THEN                                                      07NOV00.2255
         READ (20,ERR=9010) KA, KB, SELFREF                                      07NOV00.2256
      END IF                                                                     07NOV00.2257
      CALL DM_BCAST_REALS ( SELFREF , 10*MG )                                    07NOV00.2258
      CALL DM_BCAST_REALS ( KA      , 10*5*13*MG )                               07NOV00.2259
      CALL DM_BCAST_REALS ( KB      , 5*5*(59-13+1)*MG )                         07NOV00.2260
                                                                                 07NOV00.2261
      RETURN                                                                     07NOV00.2262
9010  PRINT *, 'ERROR IN READING RRTM DATA FILE KGB3'                            07NOV00.2263
      STOP 'IN RRTM ROUTINE KGB3'                                                07NOV00.2264
                                                                                 RRTM_K_G.10722
      END                                                                        RRTM_K_G.10723
                                                                                 RRTM_K_G.10724
      SUBROUTINE KGB4                                                            RRTM_K_G.10725
      IMPLICIT NONE                                                              07NOV00.2265
                                                                                 RRTM_K_G.10726
      INTEGER MG                                                                 07NOV00.2266
      PARAMETER (MG = 16)                                                        RRTM_K_G.10727
      REAL KA(9,5,13,MG),KB(6,5,13:59,MG)                                        RRTM_K_G.10728
      REAL      SELFREF(10,MG)                                                   07NOV00.2267
      EXTERNAL DM_IONODE                                                         07NOV00.2268
      LOGICAL DM_IONODE                                                          07NOV00.2269
                                                                                 RRTM_K_G.10730
      COMMON /K4/ KA, KB, SELFREF                                                RRTM_K_G.10731
                                                                                 RRTM_K_G.10732
C     The array KA contains absorption coefs for each of the 16 g-intervals      RRTM_K_G.10733
C     for a range of pressure levels > ~100mb, temperatures, and ratios          RRTM_K_G.10734
C     of water vapor to CO2.  The first index in the array, JS, runs             RRTM_K_G.10735
C     from 1 to 9 and corresponds to different water vapor to CO2 ratios,        RRTM_K_G.10736
C     as expressed through the binary species parameter eta, defined as          RRTM_K_G.10737
C     eta = h2o/(h20 + (rat) * co2), where rat is the ratio of the integrated    RRTM_K_G.10738
C     line strength in the band of co2 to that of h2o.  For instance,            RRTM_K_G.10739
C     JS=1 refers to dry air (eta = 0), JS = 9 corresponds to eta = 1.0.         RRTM_K_G.10740
C     The 2nd index in the array, JT, which runs from 1 to 5, corresponds        RRTM_K_G.10741
C     to different temperatures.  More specifically, JT = 3 means that the       RRTM_K_G.10742
C     data are for the reference temperature TREF for this pressure              RRTM_K_G.10743
C     level, JT = 2 refers to the temperature TREF-15,                           RRTM_K_G.10744
C     JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5                   RRTM_K_G.10745
C     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers         RRTM_K_G.10746
C     to the reference pressure level (e.g. JP = 1 is for a                      RRTM_K_G.10747
C     pressure of 1053.63 mb).  The fourth index, IG, goes from 1 to 16,         RRTM_K_G.10748
C     and tells us which g-interval the absorption coefficients are for.         RRTM_K_G.10749
                                                                                 RRTM_K_G.12622
C     The array KB contains absorption coefs for each of the 16 g-intervals      RRTM_K_G.12623
C     for a range of pressure levels  < ~100mb, temperatures, and ratios         RRTM_K_G.12624
C     of O3 to CO2.  The first index in the array, JS, runs from 1 to 6,         RRTM_K_G.12625
C     and corresponds to different O3 to CO2 ratios, as expressed through        RRTM_K_G.12626
C     the binary species parameter eta, defined as eta = O3/(O3+RAT*H2O),        RRTM_K_G.12627
C     where RAT is the ratio of the integrated line strength in the band         RRTM_K_G.12628
C     of CO2 to that of O3.  For instance, JS=1 refers to no O3 (eta = 0)        RRTM_K_G.12629
C     and JS = 5 corresponds to eta = 1.0.  The second index, JT, which          RRTM_K_G.12630
C     runs from 1 to 5, corresponds to different temperatures.  More             RRTM_K_G.12631
C     specifically, JT = 3 means that the data are for the corresponding         RRTM_K_G.12632
C     reference temperature TREF for this  pressure level, JT = 2 refers         RRTM_K_G.12633
C     to the TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and          RRTM_K_G.12634
C     JT = 5 is for TREF+30.  The third index, JP, runs from 13 to 59 and        RRTM_K_G.12635
C     refers to the corresponding pressure level in PREF (e.g. JP = 13 is        RRTM_K_G.12636
C     for a pressure of 95.5835 mb).  The fourth index, IG, goes from 1 to       RRTM_K_G.12637
C     16, and tells us which g-interval the absorption coefficients are for.     RRTM_K_G.12638
                                                                                 RRTM_K_G.17151
C     The array SELFREF contains the coefficient of the water vapor              RRTM_K_G.17152
C     self-continuum (including the energy term).  The first index               RRTM_K_G.17153
C     refers to temperature in 7.2 degree increments.  For instance,             RRTM_K_G.17154
C     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,           RRTM_K_G.17155
C     etc.  The second index runs over the g-channel (1 to 16).                  RRTM_K_G.17156
                                                                                 07NOV00.2270
      IF (DM_IONODE()) THEN                                                      07NOV00.2271
         READ (20,ERR=9010) KA, KB, SELFREF                                      07NOV00.2272
      END IF                                                                     07NOV00.2273
      CALL DM_BCAST_REALS ( SELFREF , 10*MG )                                    07NOV00.2274
      CALL DM_BCAST_REALS ( KA      , 9*5*13*MG )                                07NOV00.2275
      CALL DM_BCAST_REALS ( KB      , 6*5*(59-13+1)*MG )                         07NOV00.2276
                                                                                 07NOV00.2277
      RETURN                                                                     07NOV00.2278
9010  PRINT *, 'ERROR IN READING RRTM DATA FILE KGB4'                            07NOV00.2279
      STOP 'IN RRTM ROUTINE KGB4'                                                07NOV00.2280
                                                                                 RRTM_K_G.17205
      END                                                                        RRTM_K_G.17206
                                                                                 RRTM_K_G.17207
      SUBROUTINE KGB5                                                            RRTM_K_G.17208
      IMPLICIT NONE                                                              07NOV00.2281
                                                                                 RRTM_K_G.17209
      INTEGER MG                                                                 07NOV00.2282
      PARAMETER (MG = 16)                                                        RRTM_K_G.17210
      REAL KA(9,5,13,MG),KB(5,5,13:59,MG)                                        RRTM_K_G.17211
      REAL     SELFREF(10,MG)                                                    07NOV00.2283
      EXTERNAL DM_IONODE                                                         07NOV00.2284
      LOGICAL DM_IONODE                                                          07NOV00.2285
                                                                                 RRTM_K_G.17213
      COMMON /K5/ KA, KB, SELFREF                                                RRTM_K_G.17214
                                                                                 RRTM_K_G.17215
C     The array KA contains absorption coefs for each of the 16 g-intervals      RRTM_K_G.17216
C     for a range of pressure levels > ~100mb, temperatures, and ratios          RRTM_K_G.17217
C     of water vapor to CO2.  The first index in the array, JS, runs             RRTM_K_G.17218
C     from 1 to 9 and corresponds to different water vapor to CO2 ratios,        RRTM_K_G.17219
C     as expressed through the binary species parameter eta, defined as          RRTM_K_G.17220
C     eta = h2o/(h20 + (rat) * co2), where rat is the ratio of the integrated    RRTM_K_G.17221
C     line strength in the band of co2 to that of h2o.  For instance,            RRTM_K_G.17222
C     JS=1 refers to dry air (eta = 0), JS = 9 corresponds to eta = 1.0.         RRTM_K_G.17223
C     The 2nd index in the array, JT, which runs from 1 to 5, corresponds        RRTM_K_G.17224
C     to different temperatures.  More specifically, JT = 3 means that the       RRTM_K_G.17225
C     data are for the reference temperature TREF for this  pressure             RRTM_K_G.17226
C     level, JT = 2 refers to the temperature TREF-15,                           RRTM_K_G.17227
C     JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5                   RRTM_K_G.17228
C     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers         RRTM_K_G.17229
C     to the reference pressure level (e.g. JP = 1 is for a                      RRTM_K_G.17230
C     pressure of 1053.63 mb).  The fourth index, IG, goes from 1 to 16,         RRTM_K_G.17231
C     and tells us which g-interval the absorption coefficients are for.         RRTM_K_G.17232
                                                                                 RRTM_K_G.19105
C     The array KB contains absorption coefs for each of the 16 g-intervals      RRTM_K_G.19106
C     for a range of pressure levels  < ~100mb, temperatures, and ratios         RRTM_K_G.19107
C     of O3 to CO2.  The first index in the array, JS, runs from 1 to 5,         RRTM_K_G.19108
C     and corresponds to different O3 to CO2 ratios, as expressed through        RRTM_K_G.19109
C     the binary species parameter eta, defined as eta = O3/(O3+RAT*CO2),        RRTM_K_G.19110
C     where RAT is the ratio of the integrated line strength in the band         RRTM_K_G.19111
C     of co2 to that of O3.  For instance, JS=1 refers to no O3 (eta = 0)        RRTM_K_G.19112
C     and JS = 5 corresponds to eta = 1.0.  The second index, JT, which          RRTM_K_G.19113
C     runs from 1 to 5, corresponds to different temperatures.  More             RRTM_K_G.19114
C     specifically, JT = 3 means that the data are for the corresponding         RRTM_K_G.19115
C     reference temperature TREF for this  pressure level, JT = 2 refers         RRTM_K_G.19116
C     to the TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and          RRTM_K_G.19117
C     JT = 5 is for TREF+30.  The third index, JP, runs from 13 to 59 and        RRTM_K_G.19118
C     refers to the corresponding pressure level in PREF (e.g. JP = 13 is        RRTM_K_G.19119
C     for a pressure of 95.5835 mb).  The fourth index, IG, goes from 1 to       RRTM_K_G.19120
C     16, and tells us which g-interval the absorption coefficients are for.     RRTM_K_G.19121
                                                                                 RRTM_K_G.23634
C     The array SELFREF contains the coefficient of the water vapor              RRTM_K_G.23635
C     self-continuum (including the energy term).  The first index               RRTM_K_G.23636
C     refers to temperature in 7.2 degree increments.  For instance,             RRTM_K_G.23637
C     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,           RRTM_K_G.23638
C     etc.  The second index runs over the g-channel (1 to 16).                  RRTM_K_G.23639
                                                                                 07NOV00.2286
      IF (DM_IONODE()) THEN                                                      07NOV00.2287
         READ (20,ERR=9010) KA, KB, SELFREF                                      07NOV00.2288
      END IF                                                                     07NOV00.2289
      CALL DM_BCAST_REALS ( SELFREF , 10*MG )                                    07NOV00.2290
      CALL DM_BCAST_REALS ( KA      , 9*5*13*MG )                                07NOV00.2291
      CALL DM_BCAST_REALS ( KB      , 5*5*(59-13+1)*MG )                         07NOV00.2292
                                                                                 07NOV00.2293
      RETURN                                                                     07NOV00.2294
9010  PRINT *, 'ERROR IN READING RRTM DATA FILE KGB5'                            07NOV00.2295
      STOP 'IN RRTM ROUTINE KGB5'                                                07NOV00.2296
                                                                                 RRTM_K_G.23688
      END                                                                        RRTM_K_G.23689
                                                                                 RRTM_K_G.23690
      SUBROUTINE KGB6                                                            RRTM_K_G.23691
      IMPLICIT NONE                                                              07NOV00.2297
                                                                                 RRTM_K_G.23692
      INTEGER MG                                                                 07NOV00.2298
      PARAMETER (MG = 16)                                                        RRTM_K_G.23693
      REAL KA(5,13,MG)                                                           RRTM_K_G.23694
      REAL      SELFREF(10,MG)                                                   07NOV00.2299
      EXTERNAL DM_IONODE                                                         07NOV00.2300
      LOGICAL DM_IONODE                                                          07NOV00.2301
                                                                                 RRTM_K_G.23696
      COMMON /K6/ KA ,SELFREF                                                    RRTM_K_G.23697
                                                                                 RRTM_K_G.23698
C     The array KA contains absorption coefs at the 16 chosen g-values           RRTM_K_G.23699
C     for a range of pressure levels > ~100mb and temperatures.  The first       RRTM_K_G.23700
C     index in the array, JT, which runs from 1 to 5, corresponds to             RRTM_K_G.23701
C     different temperatures.  More specifically, JT = 3 means that the          RRTM_K_G.23702
C     data are for the corresponding TREF for this  pressure level,              RRTM_K_G.23703
C     JT = 2 refers to the temperatureTREF-15, JT = 1 is for TREF-30,            RRTM_K_G.23704
C     JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  The second              RRTM_K_G.23705
C     index, JP, runs from 1 to 13 and refers to the corresponding               RRTM_K_G.23706
C     pressure level in PREF (e.g. JP = 1 is for a pressure of 1053.63 mb).      RRTM_K_G.23707
C     The third index, IG, goes from 1 to 16, and tells us which                 RRTM_K_G.23708
C     g-interval the absorption coefficients are for.                            RRTM_K_G.23709
                                                                                 RRTM_K_G.24126
C     The array SELFREF contains the coefficient of the water vapor              RRTM_K_G.24127
C     self-continuum (including the energy term).  The first index               RRTM_K_G.24128
C     refers to temperature in 7.2 degree increments.  For instance,             RRTM_K_G.24129
C     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,           RRTM_K_G.24130
C     etc.  The second index runs over the g-channel (1 to 16).                  RRTM_K_G.24131
                                                                                 07NOV00.2302
      IF (DM_IONODE()) THEN                                                      07NOV00.2303
         READ (20,ERR=9010) KA, SELFREF                                          07NOV00.2304
      END IF                                                                     07NOV00.2305
      CALL DM_BCAST_REALS ( SELFREF , 10*MG )                                    07NOV00.2306
      CALL DM_BCAST_REALS ( KA      , 5*13*MG )                                  07NOV00.2307
                                                                                 07NOV00.2308
      RETURN                                                                     07NOV00.2309
9010  PRINT *, 'ERROR IN READING RRTM DATA FILE KGB6'                            07NOV00.2310
      STOP 'IN RRTM ROUTINE KGB6'                                                07NOV00.2311
                                                                                 RRTM_K_G.24180
      END                                                                        RRTM_K_G.24181
                                                                                 RRTM_K_G.24182
      SUBROUTINE KGB7                                                            RRTM_K_G.24183
      IMPLICIT NONE                                                              07NOV00.2312
                                                                                 RRTM_K_G.24184
      INTEGER MG                                                                 07NOV00.2313
      PARAMETER (MG = 16)                                                        RRTM_K_G.24185
      REAL KA(9,5,13,MG),KB(5,13:59,MG)                                          RRTM_K_G.24186
      REAL      SELFREF(10,MG)                                                   07NOV00.2314
      EXTERNAL DM_IONODE                                                         07NOV00.2315
      LOGICAL DM_IONODE                                                          07NOV00.2316
                                                                                 RRTM_K_G.24188
      COMMON /K7/ KA ,KB, SELFREF                                                RRTM_K_G.24189
                                                                                 RRTM_K_G.24190
C     The array KA contains absorption coefs at the 16 chosen g-values           RRTM_K_G.24191
C     for a range of pressure levels> ~100mb, temperatures, and binary           RRTM_K_G.24192
C     species parameters (see taumol.f for definition).  The first               RRTM_K_G.24193
C     index in the array, JS, runs from 1 to 9, and corresponds to               RRTM_K_G.24194
C     different values of the binary species parameter.  For instance,           RRTM_K_G.24195
C     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8,      RRTM_K_G.24196
C     JS = 3 corresponds to the parameter value 2/8, etc.  The second index      RRTM_K_G.24197
C     in the array, JT, which runs from 1 to 5, corresponds to different         RRTM_K_G.24198
C     temperatures.  More specifically, JT = 3 means that the data are for       RRTM_K_G.24199
C     the reference temperature TREF for this  pressure level, JT = 2 refers     RRTM_K_G.24200
C     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5       RRTM_K_G.24201
C     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers         RRTM_K_G.24202
C     to the JPth reference pressure level (see taumol.f for these levels        RRTM_K_G.24203
C     in mb).  The fourth index, IG, goes from 1 to 16, and indicates            RRTM_K_G.24204
C     which g-interval the absorption coefficients are for.                      RRTM_K_G.24205
                                                                                 RRTM_K_G.26078
C     The array KB contains absorption coefs at the 16 chosen g-values           RRTM_K_G.26079
C     for a range of pressure levels < ~100mb and temperatures. The first        RRTM_K_G.26080
C     index in the array, JT, which runs from 1 to 5, corresponds to             RRTM_K_G.26081
C     different temperatures.  More specifically, JT = 3 means that the          RRTM_K_G.26082
C     data are for the reference temperature TREF for this pressure              RRTM_K_G.26083
C     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for             RRTM_K_G.26084
C     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.                 RRTM_K_G.26085
C     The second index, JP, runs from 13 to 59 and refers to the JPth            RRTM_K_G.26086
C     reference pressure level (see taumol.f for the value of these              RRTM_K_G.26087
C     pressure levels in mb).  The third index, IG, goes from 1 to 16,           RRTM_K_G.26088
C     and tells us which g-interval the absorption coefficients are for.         RRTM_K_G.26089
                                                                                 RRTM_K_G.27594
C     The array SELFREF contains the coefficient of the water vapor              RRTM_K_G.27595
C     self-continuum (including the energy term).  The first index               RRTM_K_G.27596
C     refers to temperature in 7.2 degree increments.  For instance,             RRTM_K_G.27597
C     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,           RRTM_K_G.27598
C     etc.  The second index runs over the g-channel (1 to 16).                  RRTM_K_G.27599
                                                                                 07NOV00.2317
      IF (DM_IONODE()) THEN                                                      07NOV00.2318
         READ (20,ERR=9010) KA, KB, SELFREF                                      07NOV00.2319
      END IF                                                                     07NOV00.2320
      CALL DM_BCAST_REALS ( SELFREF , 10*MG )                                    07NOV00.2321
      CALL DM_BCAST_REALS ( KA      , 9*5*13*MG )                                07NOV00.2322
      CALL DM_BCAST_REALS ( KB      , 5*(59-13+1)*MG )                           07NOV00.2323
                                                                                 07NOV00.2324
      RETURN                                                                     07NOV00.2325
9010  PRINT *, 'ERROR IN READING RRTM DATA FILE KGB7'                            07NOV00.2326
      STOP 'IN RRTM ROUTINE KGB7'                                                07NOV00.2327
                                                                                 RRTM_K_G.27648
      END                                                                        RRTM_K_G.27649
                                                                                 RRTM_K_G.27650
      SUBROUTINE KGB8                                                            RRTM_K_G.27651
      IMPLICIT NONE                                                              07NOV00.2328
                                                                                 RRTM_K_G.27652
      INTEGER MG                                                                 07NOV00.2329
      PARAMETER (MG = 16)                                                        RRTM_K_G.27653
      REAL KA(5,7,MG),KB(5,7:59,MG), SELFREF(10,MG)                              RRTM_K_G.27654
      EXTERNAL DM_IONODE                                                         07NOV00.2330
      LOGICAL DM_IONODE                                                          07NOV00.2331
                                                                                 RRTM_K_G.27655
      COMMON /K8/ KA ,KB, SELFREF                                                RRTM_K_G.27656
                                                                                 RRTM_K_G.27657
C     The array KA contains absorption coefs at the 16 chosen g-values           RRTM_K_G.27658
C     for a range of pressure levels > ~100mb and temperatures.  The first       RRTM_K_G.27659
C     index in the array, JT, which runs from 1 to 5, corresponds to             RRTM_K_G.27660
C     different temperatures.  More specifically, JT = 3 means that the          RRTM_K_G.27661
C     data are for the corresponding TREF for this  pressure level,              RRTM_K_G.27662
C     JT = 2 refers to the temperatureTREF-15, JT = 1 is for TREF-30,            RRTM_K_G.27663
C     JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  The second              RRTM_K_G.27664
C     index, JP, runs from 1 to 13 and refers to the corresponding               RRTM_K_G.27665
C     pressure level in PREF (e.g. JP = 1 is for a pressure of 1053.63 mb).      RRTM_K_G.27666
C     The third index, IG, goes from 1 to 16, and tells us which                 RRTM_K_G.27667
C     g-interval the absorption coefficients are for.                            RRTM_K_G.27668
C     The array KA contains absorption coef5s at the 16 chosen g-values          RRTM_K_G.27669
C     for a range of pressure levels > ~100mb and temperatures.  The first       RRTM_K_G.27670
C     index in the array, JT, which runs from 1 to 5, corresponds to             RRTM_K_G.27671
C     different temperatures.  More specifically, JT = 3 means that the          RRTM_K_G.27672
C     data are for the cooresponding TREF for this  pressure level,              RRTM_K_G.27673
C     JT = 2 refers to the temperature                                           RRTM_K_G.27674
C     TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5          RRTM_K_G.27675
C     is for TREF+30.  The second index, JP, runs from 1 to 13 and refers        RRTM_K_G.27676
C     to the corresponding pressure level in PREF (e.g. JP = 1 is for a          RRTM_K_G.27677
C     pressure of 1053.63 mb).  The third index, IG, goes from 1 to 16,          RRTM_K_G.27678
C     and tells us which "g-channel" the absorption coefficients are for.        RRTM_K_G.27679
                                                                                 RRTM_K_G.27904
C     The array KB contains absorption coefs at the 16 chosen g-values           RRTM_K_G.27905
C     for a range of pressure levels < ~100mb and temperatures. The first        RRTM_K_G.27906
C     index in the array, JT, which runs from 1 to 5, corresponds to             RRTM_K_G.27907
C     different temperatures.  More specifically, JT = 3 means that the          RRTM_K_G.27908
C     data are for the reference temperature TREF for this pressure              RRTM_K_G.27909
C     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for             RRTM_K_G.27910
C     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.                 RRTM_K_G.27911
C     The second index, JP, runs from 13 to 59 and refers to the JPth            RRTM_K_G.27912
C     reference pressure level (see taumol.f for the value of these              RRTM_K_G.27913
C     pressure levels in mb).  The third index, IG, goes from 1 to 16,           RRTM_K_G.27914
C     and tells us which g-interval the absorption coefficients are for.         RRTM_K_G.27915
C                                                                                RRTM_K_G.29612
C     SELFREF is the array for the self-continuum.                               07NOV00.2332
C                                                                                RRTM_K_G.29614
      IF (DM_IONODE()) THEN                                                      07NOV00.2333
         READ (20,ERR=9010) KA, KB, SELFREF                                      07NOV00.2334
      END IF                                                                     07NOV00.2335
      CALL DM_BCAST_REALS ( SELFREF , 10*MG )                                    07NOV00.2336
      CALL DM_BCAST_REALS ( KA      , 5*7*MG )                                   07NOV00.2337
      CALL DM_BCAST_REALS ( KB      , 5*(59-7+1)*MG )                            07NOV00.2338
                                                                                 07NOV00.2339
      RETURN                                                                     07NOV00.2340
9010  PRINT *, 'ERROR IN READING RRTM DATA FILE KGB8'                            07NOV00.2341
      STOP 'IN RRTM ROUTINE KGB8'                                                07NOV00.2342
                                                                                 RRTM_K_G.29663
      END                                                                        RRTM_K_G.29664
                                                                                 RRTM_K_G.29665
      SUBROUTINE KGB9                                                            RRTM_K_G.29666
      IMPLICIT NONE                                                              07NOV00.2343
                                                                                 RRTM_K_G.29667
      INTEGER MG                                                                 07NOV00.2344
      PARAMETER (MG = 16)                                                        RRTM_K_G.29668
      REAL KA(11,5,13,MG), KB(5,13:59,MG)                                        RRTM_K_G.29669
      REAL      SELFREF(10,MG)                                                   07NOV00.2345
      EXTERNAL DM_IONODE                                                         07NOV00.2346
      LOGICAL DM_IONODE                                                          07NOV00.2347
                                                                                 RRTM_K_G.29671
      COMMON /K9/ KA, KB, SELFREF                                                RRTM_K_G.29672
                                                                                 RRTM_K_G.29673
C     The array KA contains absorption coefs at the 16 chosen g-values           RRTM_K_G.29674
C     for a range of pressure levels> ~100mb, temperatures, and binary           RRTM_K_G.29675
C     species parameters (see taumol.f for definition).  The first               RRTM_K_G.29676
C     index in the array, JS, runs from 1 to 11, and corresponds to              RRTM_K_G.29677
C     different values of the binary species parameter.  For instance,           RRTM_K_G.29678
C     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8,      RRTM_K_G.29679
C     JS = 3 corresponds to the parameter value 2/8, etc.  The second index      RRTM_K_G.29680
C     in the array, JT, which runs from 1 to 5, corresponds to different         RRTM_K_G.29681
C     temperatures.  More specifically, JT = 3 means that the data are for       RRTM_K_G.29682
C     the reference temperature TREF for this  pressure level, JT = 2 refers     RRTM_K_G.29683
C     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5       RRTM_K_G.29684
C     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers         RRTM_K_G.29685
C     to the JPth reference pressure level (see taumol.f for these levels        RRTM_K_G.29686
C     in mb).  The fourth index, IG, goes from 1 to 16, and indicates            RRTM_K_G.29687
C     which g-interval the absorption coefficients are for.                      RRTM_K_G.29688
                                                                                 RRTM_K_G.31977
C     The array KB contains absorption coefs at the 16 chosen g-values           RRTM_K_G.31978
C     for a range of pressure levels < ~100mb and temperatures. The first        RRTM_K_G.31979
C     index in the array, JT, which runs from 1 to 5, corresponds to             RRTM_K_G.31980
C     different temperatures.  More specifically, JT = 3 means that the          RRTM_K_G.31981
C     data are for the reference temperature TREF for this pressure              RRTM_K_G.31982
C     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for             RRTM_K_G.31983
C     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.                 RRTM_K_G.31984
C     The second index, JP, runs from 13 to 59 and refers to the JPth            RRTM_K_G.31985
C     reference pressure level (see taumol.f for the value of these              RRTM_K_G.31986
C     pressure levels in mb).  The third index, IG, goes from 1 to 16,           RRTM_K_G.31987
C     and tells us which g-interval the absorption coefficients are for.         RRTM_K_G.31988
                                                                                 RRTM_K_G.33493
C     The array SELFREF contains the coefficient of the water vapor              RRTM_K_G.33494
C     self-continuum (including the energy term).  The first index               RRTM_K_G.33495
C     refers to temperature in 7.2 degree increments.  For instance,             RRTM_K_G.33496
C     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,           RRTM_K_G.33497
C     etc.  The second index runs over the g-channel (1 to 16).                  RRTM_K_G.33498
                                                                                 RRTM_K_G.33547
      IF (DM_IONODE()) THEN                                                      07NOV00.2348
         READ (20,ERR=9010) KA, KB, SELFREF                                      07NOV00.2349
      END IF                                                                     07NOV00.2350
      CALL DM_BCAST_REALS ( SELFREF , 10*MG )                                    07NOV00.2351
      CALL DM_BCAST_REALS ( KA      , 11*5*13*MG )                               07NOV00.2352
      CALL DM_BCAST_REALS ( KB      , 5*(59-13+1)*MG )                           07NOV00.2353
                                                                                 07NOV00.2354
      RETURN                                                                     07NOV00.2355
9010  PRINT *, 'ERROR IN READING RRTM DATA FILE KGB9'                            07NOV00.2356
      STOP 'IN RRTM ROUTINE KGB9'                                                07NOV00.2357
                                                                                 07NOV00.2358
      END                                                                        RRTM_K_G.33548
                                                                                 RRTM_K_G.33549
      SUBROUTINE KGB10                                                           RRTM_K_G.33550
      IMPLICIT NONE                                                              07NOV00.2359
                                                                                 RRTM_K_G.33551
      INTEGER MG                                                                 07NOV00.2360
      PARAMETER (MG = 16)                                                        RRTM_K_G.33552
      REAL KA(5,13,MG),KB(5,13:59,MG)                                            RRTM_K_G.33553
      EXTERNAL DM_IONODE                                                         07NOV00.2361
      LOGICAL DM_IONODE                                                          07NOV00.2362
                                                                                 RRTM_K_G.33554
      COMMON /K10/ KA ,KB                                                        RRTM_K_G.33555
                                                                                 RRTM_K_G.33556
C     The array KA contains absorption coefs at the 16 chosen g-values           RRTM_K_G.33557
C     for a range of pressure levels > ~100mb and temperatures.  The first       RRTM_K_G.33558
C     index in the array, JT, which runs from 1 to 5, corresponds to             RRTM_K_G.33559
C     different temperatures.  More specifically, JT = 3 means that the          RRTM_K_G.33560
C     data are for the corresponding TREF for this  pressure level,              RRTM_K_G.33561
C     JT = 2 refers to the temperatureTREF-15, JT = 1 is for TREF-30,            RRTM_K_G.33562
C     JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  The second              RRTM_K_G.33563
C     index, JP, runs from 1 to 13 and refers to the corresponding               RRTM_K_G.33564
C     pressure level in PREF (e.g. JP = 1 is for a pressure of 1053.63 mb).      RRTM_K_G.33565
C     The third index, IG, goes from 1 to 16, and tells us which                 RRTM_K_G.33566
C     g-interval the absorption coefficients are for.                            RRTM_K_G.33567
                                                                                 RRTM_K_G.33984
C     The array KB contains absorption coefs at the 16 chosen g-values           RRTM_K_G.33985
C     for a range of pressure levels < ~100mb and temperatures. The first        RRTM_K_G.33986
C     index in the array, JT, which runs from 1 to 5, corresponds to             RRTM_K_G.33987
C     different temperatures.  More specifically, JT = 3 means that the          RRTM_K_G.33988
C     data are for the reference temperature TREF for this pressure              RRTM_K_G.33989
C     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for             RRTM_K_G.33990
C     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.                 RRTM_K_G.33991
C     The second index, JP, runs from 13 to 59 and refers to the JPth            RRTM_K_G.33992
C     reference pressure level (see taumol.f for the value of these              RRTM_K_G.33993
C     pressure levels in mb).  The third index, IG, goes from 1 to 16,           RRTM_K_G.33994
C     and tells us which g-interval the absorption coefficients are for.         RRTM_K_G.33995
                                                                                 07NOV00.2363
      IF (DM_IONODE()) THEN                                                      07NOV00.2364
         READ (20,ERR=9010) KA, KB                                               07NOV00.2365
      END IF                                                                     07NOV00.2366
      CALL DM_BCAST_REALS ( KA      , 5*13*MG )                                  07NOV00.2367
      CALL DM_BCAST_REALS ( KB      , 5*(59-13+1)*MG )                           07NOV00.2368
                                                                                 07NOV00.2369
      RETURN                                                                     07NOV00.2370
9010  PRINT *, 'ERROR IN READING RRTM DATA FILE KGB10'                           07NOV00.2371
      STOP 'IN RRTM ROUTINE KGB10'                                               07NOV00.2372
                                                                                 RRTM_K_G.35500
      END                                                                        RRTM_K_G.35501
                                                                                 RRTM_K_G.35502
      SUBROUTINE KGB11                                                           RRTM_K_G.35503
      IMPLICIT NONE                                                              07NOV00.2373
                                                                                 RRTM_K_G.35504
      INTEGER MG                                                                 07NOV00.2374
      PARAMETER (MG = 16)                                                        RRTM_K_G.35505
      REAL KA(5,13,MG),KB(5,13:59,MG)                                            RRTM_K_G.35506
      REAL      SELFREF(10,MG)                                                   07NOV00.2375
      EXTERNAL DM_IONODE                                                         07NOV00.2376
      LOGICAL DM_IONODE                                                          07NOV00.2377
                                                                                 RRTM_K_G.35508
      COMMON /K11/ KA ,KB, SELFREF                                               RRTM_K_G.35509
                                                                                 RRTM_K_G.35510
C     The array KA contains absorption coefs at the 16 chosen g-values           RRTM_K_G.35511
C     for a range of pressure levels > ~100mb and temperatures.  The first       RRTM_K_G.35512
C     index in the array, JT, which runs from 1 to 5, corresponds to             RRTM_K_G.35513
C     different temperatures.  More specifically, JT = 3 means that the          RRTM_K_G.35514
C     data are for the corresponding TREF for this  pressure level,              RRTM_K_G.35515
C     JT = 2 refers to the temperatureTREF-15, JT = 1 is for TREF-30,            RRTM_K_G.35516
C     JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  The second              RRTM_K_G.35517
C     index, JP, runs from 1 to 13 and refers to the corresponding               RRTM_K_G.35518
C     pressure level in PREF (e.g. JP = 1 is for a pressure of 1053.63 mb).      RRTM_K_G.35519
C     The third index, IG, goes from 1 to 16, and tells us which                 RRTM_K_G.35520
C     g-interval the absorption coefficients are for.                            RRTM_K_G.35521
                                                                                 RRTM_K_G.35938
C     The array KB contains absorption coefs at the 16 chosen g-values           RRTM_K_G.35939
C     for a range of pressure levels < ~100mb and temperatures. The first        RRTM_K_G.35940
C     index in the array, JT, which runs from 1 to 5, corresponds to             RRTM_K_G.35941
C     different temperatures.  More specifically, JT = 3 means that the          RRTM_K_G.35942
C     data are for the reference temperature TREF for this pressure              RRTM_K_G.35943
C     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for             RRTM_K_G.35944
C     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.                 RRTM_K_G.35945
C     The second index, JP, runs from 13 to 59 and refers to the JPth            RRTM_K_G.35946
C     reference pressure level (see taumol.f for the value of these              RRTM_K_G.35947
C     pressure levels in mb).  The third index, IG, goes from 1 to 16,           RRTM_K_G.35948
C     and tells us which g-interval the absorption coefficients are for.         RRTM_K_G.35949
                                                                                 RRTM_K_G.37454
C     The array SELFREF contains the coefficient of the water vapor              RRTM_K_G.37455
C     self-continuum (including the energy term).  The first index               RRTM_K_G.37456
C     refers to temperature in 7.2 degree increments.  For instance,             RRTM_K_G.37457
C     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,           RRTM_K_G.37458
C     etc.  The second index runs over the g-channel (1 to 16).                  RRTM_K_G.37459
                                                                                 07NOV00.2378
      IF (DM_IONODE()) THEN                                                      07NOV00.2379
         READ (20,ERR=9010) KA, KB, SELFREF                                      07NOV00.2380
      END IF                                                                     07NOV00.2381
      CALL DM_BCAST_REALS ( SELFREF , 10*MG )                                    07NOV00.2382
      CALL DM_BCAST_REALS ( KA      , 5*13*MG )                                  07NOV00.2383
      CALL DM_BCAST_REALS ( KB      , 5*(59-13+1)*MG )                           07NOV00.2384
                                                                                 07NOV00.2385
      RETURN                                                                     07NOV00.2386
9010  PRINT *, 'ERROR IN READING RRTM DATA FILE KGB11'                           07NOV00.2387
      STOP 'IN RRTM ROUTINE KGB11'                                               07NOV00.2388
                                                                                 07NOV00.2389
      END                                                                        RRTM_K_G.37509
                                                                                 RRTM_K_G.37510
      SUBROUTINE KGB12                                                           RRTM_K_G.37511
      IMPLICIT NONE                                                              07NOV00.2390
                                                                                 RRTM_K_G.37512
      INTEGER MG                                                                 07NOV00.2391
      PARAMETER (MG = 16)                                                        RRTM_K_G.37513
      REAL KA(9,5,13,MG)                                                         RRTM_K_G.37514
      REAL      SELFREF(10,MG)                                                   07NOV00.2392
      EXTERNAL DM_IONODE                                                         07NOV00.2393
      LOGICAL DM_IONODE                                                          07NOV00.2394
                                                                                 RRTM_K_G.37516
      COMMON /K12/ KA, SELFREF                                                   RRTM_K_G.37517
                                                                                 RRTM_K_G.37518
C     The array KA contains absorption coefs at the 16 chosen g-values           RRTM_K_G.37519
C     for a range of pressure levels> ~100mb, temperatures, and binary           RRTM_K_G.37520
C     species parameters (see taumol.f for definition).  The first               RRTM_K_G.37521
C     index in the array, JS, runs from 1 to 9, and corresponds to               RRTM_K_G.37522
C     different values of the binary species parameter.  For instance,           RRTM_K_G.37523
C     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8,      RRTM_K_G.37524
C     JS = 3 corresponds to the parameter value 2/8, etc.  The second index      RRTM_K_G.37525
C     in the array, JT, which runs from 1 to 5, corresponds to different         RRTM_K_G.37526
C     temperatures.  More specifically, JT = 3 means that the data are for       RRTM_K_G.37527
C     the reference temperature TREF for this  pressure level, JT = 2 refers     RRTM_K_G.37528
C     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5       RRTM_K_G.37529
C     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers         RRTM_K_G.37530
C     to the JPth reference pressure level (see taumol.f for these levels        RRTM_K_G.37531
C     in mb).  The fourth index, IG, goes from 1 to 16, and indicates            RRTM_K_G.37532
C     which g-interval the absorption coefficients are for.                      RRTM_K_G.37533
                                                                                 RRTM_K_G.39406
C     The array SELFREF contains the coefficient of the water vapor              RRTM_K_G.39407
C     self-continuum (including the energy term).  The first index               RRTM_K_G.39408
C     refers to temperature in 7.2 degree increments.  For instance,             RRTM_K_G.39409
C     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,           RRTM_K_G.39410
C     etc.  The second index runs over the g-channel (1 to 16).                  RRTM_K_G.39411
                                                                                 RRTM_K_G.39460
      IF (DM_IONODE()) THEN                                                      07NOV00.2395
         READ (20,ERR=9010) KA, SELFREF                                          07NOV00.2396
      END IF                                                                     07NOV00.2397
      CALL DM_BCAST_REALS ( SELFREF , 10*MG )                                    07NOV00.2398
      CALL DM_BCAST_REALS ( KA      , 9*5*13*MG )                                07NOV00.2399
                                                                                 07NOV00.2400
      RETURN                                                                     07NOV00.2401
9010  PRINT *, 'ERROR IN READING RRTM DATA FILE KGB12'                           07NOV00.2402
      STOP 'IN RRTM ROUTINE KGB12'                                               07NOV00.2403
                                                                                 07NOV00.2404
      END                                                                        RRTM_K_G.39461
                                                                                 RRTM_K_G.39462
      SUBROUTINE KGB13                                                           RRTM_K_G.39463
      IMPLICIT NONE                                                              07NOV00.2405
                                                                                 RRTM_K_G.39464
      INTEGER MG                                                                 07NOV00.2406
      PARAMETER (MG = 16)                                                        RRTM_K_G.39465
      REAL KA(9,5,13,MG)                                                         RRTM_K_G.39466
      REAL      SELFREF(10,MG)                                                   07NOV00.2407
      EXTERNAL DM_IONODE                                                         07NOV00.2408
      LOGICAL DM_IONODE                                                          07NOV00.2409
                                                                                 RRTM_K_G.39468
      COMMON /K13/ KA, SELFREF                                                   RRTM_K_G.39469
                                                                                 RRTM_K_G.39470
C     The array KA contains absorption coefs at the 16 chosen g-values           RRTM_K_G.39471
C     for a range of pressure levels> ~100mb, temperatures, and binary           RRTM_K_G.39472
C     species parameters (see taumol.f for definition).  The first               RRTM_K_G.39473
C     index in the array, JS, runs from 1 to 9, and corresponds to               RRTM_K_G.39474
C     different values of the binary species parameter.  For instance,           RRTM_K_G.39475
C     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8,      RRTM_K_G.39476
C     JS = 3 corresponds to the parameter value 2/8, etc.  The second index      RRTM_K_G.39477
C     in the array, JT, which runs from 1 to 5, corresponds to different         RRTM_K_G.39478
C     temperatures.  More specifically, JT = 3 means that the data are for       RRTM_K_G.39479
C     the reference temperature TREF for this  pressure level, JT = 2 refers     RRTM_K_G.39480
C     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5       RRTM_K_G.39481
C     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers         RRTM_K_G.39482
C     to the JPth reference pressure level (see taumol.f for these levels        RRTM_K_G.39483
C     in mb).  The fourth index, IG, goes from 1 to 16, and indicates            RRTM_K_G.39484
C     which g-interval the absorption coefficients are for.                      RRTM_K_G.39485
                                                                                 RRTM_K_G.41358
C     The array SELFREF contains the coefficient of the water vapor              RRTM_K_G.41359
C     self-continuum (including the energy term).  The first index               RRTM_K_G.41360
C     refers to temperature in 7.2 degree increments.  For instance,             RRTM_K_G.41361
C     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,           RRTM_K_G.41362
C     etc.  The second index runs over the g-channel (1 to 16).                  RRTM_K_G.41363
                                                                                 RRTM_K_G.41412
      IF (DM_IONODE()) THEN                                                      07NOV00.2410
         READ (20,ERR=9010) KA, SELFREF                                          07NOV00.2411
      END IF                                                                     07NOV00.2412
      CALL DM_BCAST_REALS ( SELFREF , 10*MG )                                    07NOV00.2413
      CALL DM_BCAST_REALS ( KA      , 9*5*13*MG )                                07NOV00.2414
                                                                                 07NOV00.2415
      RETURN                                                                     07NOV00.2416
9010  PRINT *, 'ERROR IN READING RRTM DATA FILE KGB13'                           07NOV00.2417
      STOP 'IN RRTM ROUTINE KGB13'                                               07NOV00.2418
                                                                                 07NOV00.2419
      END                                                                        RRTM_K_G.41413
                                                                                 RRTM_K_G.41414
      SUBROUTINE KGB14                                                           RRTM_K_G.41415
      IMPLICIT NONE                                                              07NOV00.2420
                                                                                 RRTM_K_G.41416
      INTEGER MG                                                                 07NOV00.2421
      PARAMETER (MG = 16)                                                        RRTM_K_G.41417
      REAL KA(5,13,MG),KB(5,13:59,MG)                                            RRTM_K_G.41418
      REAL      SELFREF(10,MG)                                                   07NOV00.2422
      EXTERNAL DM_IONODE                                                         07NOV00.2423
      LOGICAL DM_IONODE                                                          07NOV00.2424
                                                                                 RRTM_K_G.41420
      COMMON /K14/ KA ,KB, SELFREF                                               RRTM_K_G.41421
                                                                                 RRTM_K_G.41422
C     The array KA contains absorption coefs at the 16 chosen g-values           RRTM_K_G.41423
C     for a range of pressure levels > ~100mb and temperatures.  The first       RRTM_K_G.41424
C     index in the array, JT, which runs from 1 to 5, corresponds to             RRTM_K_G.41425
C     different temperatures.  More specifically, JT = 3 means that the          RRTM_K_G.41426
C     data are for the corresponding TREF for this  pressure level,              RRTM_K_G.41427
C     JT = 2 refers to the temperatureTREF-15, JT = 1 is for TREF-30,            RRTM_K_G.41428
C     JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  The second              RRTM_K_G.41429
C     index, JP, runs from 1 to 13 and refers to the corresponding               RRTM_K_G.41430
C     pressure level in PREF (e.g. JP = 1 is for a pressure of 1053.63 mb).      RRTM_K_G.41431
C     The third index, IG, goes from 1 to 16, and tells us which                 RRTM_K_G.41432
C     g-interval the absorption coefficients are for.                            RRTM_K_G.41433
                                                                                 RRTM_K_G.41850
C     The array KB contains absorption coefs at the 16 chosen g-values           RRTM_K_G.41851
C     for a range of pressure levels < ~100mb and temperatures. The first        RRTM_K_G.41852
C     index in the array, JT, which runs from 1 to 5, corresponds to             RRTM_K_G.41853
C     different temperatures.  More specifically, JT = 3 means that the          RRTM_K_G.41854
C     data are for the reference temperature TREF for this pressure              RRTM_K_G.41855
C     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for             RRTM_K_G.41856
C     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.                 RRTM_K_G.41857
C     The second index, JP, runs from 13 to 59 and refers to the JPth            RRTM_K_G.41858
C     reference pressure level (see taumol.f for the value of these              RRTM_K_G.41859
C     pressure levels in mb).  The third index, IG, goes from 1 to 16,           RRTM_K_G.41860
C     and tells us which g-interval the absorption coefficients are for.         RRTM_K_G.41861
                                                                                 RRTM_K_G.43366
C     The array SELFREF contains the coefficient of the water vapor              RRTM_K_G.43367
C     self-continuum (including the energy term).  The first index               RRTM_K_G.43368
C     refers to temperature in 7.2 degree increments.  For instance,             RRTM_K_G.43369
C     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,           RRTM_K_G.43370
C     etc.  The second index runs over the g-channel (1 to 16).                  RRTM_K_G.43371
                                                                                 07NOV00.2425
      IF (DM_IONODE()) THEN                                                      07NOV00.2426
         READ (20,ERR=9010) KA, KB, SELFREF                                      07NOV00.2427
      END IF                                                                     07NOV00.2428
      CALL DM_BCAST_REALS ( SELFREF , 10*MG )                                    07NOV00.2429
      CALL DM_BCAST_REALS ( KA      , 5*13*MG )                                  07NOV00.2430
      CALL DM_BCAST_REALS ( KB      , 5*(59-13+1)*MG )                           07NOV00.2431
                                                                                 07NOV00.2432
      RETURN                                                                     07NOV00.2433
9010  PRINT *, 'ERROR IN READING RRTM DATA FILE KGB14'                           07NOV00.2434
      STOP 'IN RRTM ROUTINE KGB14'                                               07NOV00.2435
                                                                                 07NOV00.2436
      END                                                                        RRTM_K_G.43421
                                                                                 RRTM_K_G.43422
      SUBROUTINE KGB15                                                           RRTM_K_G.43423
      IMPLICIT NONE                                                              07NOV00.2437
                                                                                 RRTM_K_G.43424
      INTEGER MG                                                                 07NOV00.2438
      PARAMETER (MG = 16)                                                        RRTM_K_G.43425
      REAL KA(9,5,13,MG)                                                         RRTM_K_G.43426
      REAL      SELFREF(10,MG)                                                   07NOV00.2439
      EXTERNAL DM_IONODE                                                         07NOV00.2440
      LOGICAL DM_IONODE                                                          07NOV00.2441
                                                                                 RRTM_K_G.43428
      COMMON /K15/ KA, SELFREF                                                   RRTM_K_G.43429
                                                                                 RRTM_K_G.43430
C     The array KA contains absorption coefs at the 16 chosen g-values           RRTM_K_G.43431
C     for a range of pressure levels> ~100mb, temperatures, and binary           RRTM_K_G.43432
C     species parameters (see taumol.f for definition).  The first               RRTM_K_G.43433
C     index in the array, JS, runs from 1 to 9, and corresponds to               RRTM_K_G.43434
C     different values of the binary species parameter.  For instance,           RRTM_K_G.43435
C     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8,      RRTM_K_G.43436
C     JS = 3 corresponds to the parameter value 2/8, etc.  The second index      RRTM_K_G.43437
C     in the array, JT, which runs from 1 to 5, corresponds to different         RRTM_K_G.43438
C     temperatures.  More specifically, JT = 3 means that the data are for       RRTM_K_G.43439
C     the reference temperature TREF for this  pressure level, JT = 2 refers     RRTM_K_G.43440
C     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5       RRTM_K_G.43441
C     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers         RRTM_K_G.43442
C     to the JPth reference pressure level (see taumol.f for these levels        RRTM_K_G.43443
C     in mb).  The fourth index, IG, goes from 1 to 16, and indicates            RRTM_K_G.43444
C     which g-interval the absorption coefficients are for.                      RRTM_K_G.43445
                                                                                 RRTM_K_G.45318
C     The array SELFREF contains the coefficient of the water vapor              RRTM_K_G.45319
C     self-continuum (including the energy term).  The first index               RRTM_K_G.45320
C     refers to temperature in 7.2 degree increments.  For instance,             RRTM_K_G.45321
C     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,           RRTM_K_G.45322
C     etc.  The second index runs over the g-channel (1 to 16).                  RRTM_K_G.45323
                                                                                 RRTM_K_G.45372
      IF (DM_IONODE()) THEN                                                      07NOV00.2442
         READ (20,ERR=9010) KA, SELFREF                                          07NOV00.2443
      END IF                                                                     07NOV00.2444
      CALL DM_BCAST_REALS ( SELFREF , 10*MG )                                    07NOV00.2445
      CALL DM_BCAST_REALS ( KA      , 9*5*13*MG )                                07NOV00.2446
                                                                                 07NOV00.2447
      RETURN                                                                     07NOV00.2448
9010  PRINT *, 'ERROR IN READING RRTM DATA FILE KGB15'                           07NOV00.2449
      STOP 'IN RRTM ROUTINE KGB15'                                               07NOV00.2450
                                                                                 07NOV00.2451
      END                                                                        RRTM_K_G.45373
                                                                                 RRTM_K_G.45374
      SUBROUTINE KGB16                                                           RRTM_K_G.45375
      IMPLICIT NONE                                                              07NOV00.2452
                                                                                 RRTM_K_G.45376
      INTEGER MG                                                                 07NOV00.2453
      PARAMETER (MG = 16)                                                        RRTM_K_G.45377
      REAL KA(9,5,13,MG)                                                         RRTM_K_G.45378
      REAL      SELFREF(10,MG)                                                   07NOV00.2454
      EXTERNAL DM_IONODE                                                         07NOV00.2455
      LOGICAL DM_IONODE                                                          07NOV00.2456
                                                                                 RRTM_K_G.45380
      COMMON /K16/ KA, SELFREF                                                   RRTM_K_G.45381
                                                                                 RRTM_K_G.45382
C     The array KA contains absorption coefs at the 16 chosen g-values           RRTM_K_G.45383
C     for a range of pressure levels> ~100mb, temperatures, and binary           RRTM_K_G.45384
C     species parameters (see taumol.f for definition).  The first               RRTM_K_G.45385
C     index in the array, JS, runs from 1 to 9, and corresponds to               RRTM_K_G.45386
C     different values of the binary species parameter.  For instance,           RRTM_K_G.45387
C     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8,      RRTM_K_G.45388
C     JS = 3 corresponds to the parameter value 2/8, etc.  The second index      RRTM_K_G.45389
C     in the array, JT, which runs from 1 to 5, corresponds to different         RRTM_K_G.45390
C     temperatures.  More specifically, JT = 3 means that the data are for       RRTM_K_G.45391
C     the reference temperature TREF for this  pressure level, JT = 2 refers     RRTM_K_G.45392
C     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5       RRTM_K_G.45393
C     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers         RRTM_K_G.45394
C     to the JPth reference pressure level (see taumol.f for these levels        RRTM_K_G.45395
C     in mb).  The fourth index, IG, goes from 1 to 16, and indicates            RRTM_K_G.45396
C     which g-interval the absorption coefficients are for.                      RRTM_K_G.45397
                                                                                 RRTM_K_G.47270
C     The array SELFREF contains the coefficient of the water vapor              RRTM_K_G.47271
C     self-continuum (including the energy term).  The first index               RRTM_K_G.47272
C     refers to temperature in 7.2 degree increments.  For instance,             RRTM_K_G.47273
C     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,           RRTM_K_G.47274
C     etc.  The second index runs over the g-channel (1 to 16).                  RRTM_K_G.47275
                                                                                 RRTM_K_G.47324
      IF (DM_IONODE()) THEN                                                      07NOV00.2457
         READ (20,ERR=9010) KA, SELFREF                                          07NOV00.2458
      END IF                                                                     07NOV00.2459
      CALL DM_BCAST_REALS ( SELFREF , 10*MG )                                    07NOV00.2460
      CALL DM_BCAST_REALS ( KA      , 9*5*13*MG )                                07NOV00.2461
                                                                                 07NOV00.2462
      RETURN                                                                     07NOV00.2463
9010  PRINT *, 'ERROR IN READING RRTM DATA FILE KGB16'                           07NOV00.2464
      STOP 'IN RRTM ROUTINE KGB16'                                               07NOV00.2465
                                                                                 07NOV00.2466
      END                                                                        RRTM_K_G.47325
