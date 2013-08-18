      SUBROUTINE SCHULTZ_MIC (P, T, RHO, lapse, dx, DT, RV, RC,                  SCHULTZ_MIC.1
     +      RR, RP, RS, RI, TVR, TVP, TVS, TVI, PFLAG)                           SCHULTZ_MIC.2
                                                                                 SCHULTZ_MIC.3
C p ------ pressure                 Pa                                           SCHULTZ_MIC.4
C t ------ temp                     K                                            SCHULTZ_MIC.5
C rho ---- density                  kg/m**3                                      SCHULTZ_MIC.6
C lapse -- lapse rate               K/km                                         SCHULTZ_MIC.7
C dx ----- grid increment           m?  km?                                      SCHULTZ_MIC.8
C dt ----- time step                sec                                          SCHULTZ_MIC.9
                                                                                 SCHULTZ_MIC.10
C Mixing ratios (kgx/kg) are generally used by dynamic models                    SCHULTZ_MIC.11
C because they conserve, but some of the conversion processes                    SCHULTZ_MIC.12
C aren't influenced by the presence of other gases, so specific                  SCHULTZ_MIC.13
C contents (kgx/m**3) are used.  The difference is scaling by                    SCHULTZ_MIC.14
C air density, rho (kg/m**3).                                                    SCHULTZ_MIC.15
                                                                                 SCHULTZ_MIC.16
C rv, qv ----- water vapor                                                       SCHULTZ_MIC.17
C rc, qc ----- cloud water                                                       SCHULTZ_MIC.18
C rr, qr ----- rain water                                                        SCHULTZ_MIC.19
C rp, qp ----- pristine crystals                                                 SCHULTZ_MIC.20
C rs, qs ----- snow                                                              SCHULTZ_MIC.21
C ri, qi ----- graupel/hail/sleet                                                SCHULTZ_MIC.22
                                                                                 SCHULTZ_MIC.23
C tvp, tvr, tvs, tvi -- terminal velocities (m/s)                                SCHULTZ_MIC.24
                                                                                 SCHULTZ_MIC.25
C Tuning knobs:                                                                  SCHULTZ_MIC.26
C qcmin and qpmin are the minumum amounts of cloud matter                        SCHULTZ_MIC.27
C content before conversion to rain and snow begin.                              SCHULTZ_MIC.28
C v2p, c2p, etc. adjust the conversion rates.                                    SCHULTZ_MIC.29
                                                                                 SCHULTZ_MIC.30
      LOGICAL PFLAG                                                              SCHULTZ_MIC.31
      REAL P, T, RHO, lapse, dx, DT, QCMIN, QPMIN                                SCHULTZ_MIC.32
      REAL V2P, C2P, C2R, C2S, C2I, P2S                                          SCHULTZ_MIC.33
      REAL R2I, I2R, S2R, R2V, P2V, S2V, I2V                                     SCHULTZ_MIC.34
      REAL RV,  RC,  RR,  RP,  RS,  RI                                           SCHULTZ_MIC.35
      REAL QV,  QC,  QR,  QP,  QS,  QI                                           SCHULTZ_MIC.36
      REAL          TVR, TVP, TVS, TVI                                           SCHULTZ_MIC.37
                                                                                 SCHULTZ_MIC.38
      REAL RVSATL, RVSATI, RSAT                                                  SCHULTZ_MIC.39
      REAL RLIQ, RICE, RTOT0, RTOT1, TIL0, TIL1, T1, X                           SCHULTZ_MIC.40
      REAL RP_NUC, CHANGE, MAXMELT, EFF, RATE                                    SCHULTZ_MIC.41
      REAL NEED, NEEDR, NEEDS, NEEDI, RATIO                                      SCHULTZ_MIC.42
      REAL LVL, LVI, LLI                                                         SCHULTZ_MIC.43
      REAL CP, CPINV, AKLV, AKIV                                                 SCHULTZ_MIC.44
      Real rh, rvrc, prev, frac                                                  SCHULTZ_MIC.45
      INTEGER I                                                                  SCHULTZ_MIC.46
                                                                                 SCHULTZ_MIC.47
      PARAMETER(QCMIN = .0005)  ! threshold for rain autocon (kg/m**3)           SCHULTZ_MIC.48
      PARAMETER(QPMIN = .0001)  ! threshold for snow autocon (kg/m**3)           SCHULTZ_MIC.49
      PARAMETER(V2P   = 25.0)   ! diffusional crystal growth                     SCHULTZ_MIC.50
      PARAMETER(C2P   = 16.7E-6)! cloud liquid freezing at t<253                 SCHULTZ_MIC.51
      PARAMETER(C2R   = 33.0)   ! warm rainmaking                                SCHULTZ_MIC.52
      PARAMETER(C2S   = 33.3)   ! riming of snow                                 SCHULTZ_MIC.53
      PARAMETER(C2I   = 16.7)   ! riming of ice                                  SCHULTZ_MIC.54
      PARAMETER(P2S   = 5.0)    ! aggregation                                    SCHULTZ_MIC.55
      PARAMETER(R2I   = 8.33E-6)! freezing rain                                  SCHULTZ_MIC.56
      PARAMETER(S2R   = 8.33E-6)! melting snow                                   SCHULTZ_MIC.57
      PARAMETER(I2R   = 1.67E-6)! melting ice                                    SCHULTZ_MIC.58
      PARAMETER(R2V   = .001)   ! rain evaporation (must be <= 1/dt)             SCHULTZ_MIC.59
      PARAMETER(P2V   = .004)   ! cloud ice evaporation (must be <= 1/dt)        SCHULTZ_MIC.60
      PARAMETER(S2V   = .002)   ! snow evaporation (must be <= 1/dt)             SCHULTZ_MIC.61
      PARAMETER(I2V   = .001)   ! graupel evaporation (must be <= 1/dt)          SCHULTZ_MIC.62
                                                                                 SCHULTZ_MIC.63
      PARAMETER(CP=1004., CPINV=1.0/CP, AKLV=2.5E6/CP, AKIV=2.834E6/CP)          SCHULTZ_MIC.64
      PARAMETER(LLI=0.334E6)                                                     SCHULTZ_MIC.65
                                                                                 SCHULTZ_MIC.66
C Initialize the terminal velocities.                                            SCHULTZ_MIC.67
      TVP = 0.                                                                   SCHULTZ_MIC.68
      TVR = 0.                                                                   SCHULTZ_MIC.69
      TVS = 0.                                                                   SCHULTZ_MIC.70
      TVI = 0.                                                                   SCHULTZ_MIC.71
                                                                                 SCHULTZ_MIC.72
C Nothing in this routine can change the quantities "til", which                 SCHULTZ_MIC.73
C is the temperature with corrections for latent heating by ice                  SCHULTZ_MIC.74
C and liquid, and "rtot".  "til" is similar to theta-il of Tripoli               SCHULTZ_MIC.75
C and Cotton (1981 MWR).  These are checked at the end.                          SCHULTZ_MIC.76
                                                                                 SCHULTZ_MIC.77
      RLIQ = RC + RR                                                             SCHULTZ_MIC.78
      RICE = RP + RS + RI                                                        SCHULTZ_MIC.79
      RTOT0 = RV + RLIQ + RICE                                                   SCHULTZ_MIC.80
      TIL0 = T  - (AKLV*RLIQ+AKIV*RICE)                                          SCHULTZ_MIC.81
                                                                                 SCHULTZ_MIC.82
C The value "t1" returned from SatAdjL is the temperature of the                 SCHULTZ_MIC.83
C updated parcel *if* all vapor over the rsat value condenses.                   SCHULTZ_MIC.84
C The new diagnostic for cloud liquid allows for condensation                    SCHULTZ_MIC.85
C to occur when rv < rsat, so "t1" is no longer used.                            SCHULTZ_MIC.86
                                                                                 SCHULTZ_MIC.87
      RSAT = RVSATL(P,T)                                                         SCHULTZ_MIC.88
      CALL SATADJL (RV, P, T, RSAT, T1)                                          SCHULTZ_MIC.89
                                                                                 SCHULTZ_MIC.90
C We allow for cloud liquid to exist in subsaturated grid boxes;                 SCHULTZ_MIC.91
C we no longer require the grid box to be saturated before cloud                 SCHULTZ_MIC.92
C liquid begins to form.  The purpose is to include the effects                  SCHULTZ_MIC.93
C of small, subgrid-scale cumuli in mixing, latent heating, and                  SCHULTZ_MIC.94
C (perhaps eventually) shading.                                                  SCHULTZ_MIC.95
                                                                                 SCHULTZ_MIC.96
C Diagnose cloud liquid, and then compute temp change from rc                    SCHULTZ_MIC.97
C change.  These changes are assumed instantaneous.  Note the                    SCHULTZ_MIC.98
C slightly odd use of "rh":  it includes both vapor and cloud                    SCHULTZ_MIC.99
C liquid.  The value "frac" coming back from Cu_fraction is the                  SCHULTZ_MIC.100
C fraction of the grid box occupied by shallow cumuli.  It isn't                 SCHULTZ_MIC.101
C used outside that subroutine, but it could be useful for                       SCHULTZ_MIC.102
C radiative transfer.                                                            SCHULTZ_MIC.103
                                                                                 SCHULTZ_MIC.104
      rvrc = rv + rc                                                             SCHULTZ_MIC.105
      rh = rvrc / rsat                                                           SCHULTZ_MIC.106
      prev = rc                                                                  SCHULTZ_MIC.107
      Call Cu_fraction (dx, lapse, rsat, rh, rc, frac)                           SCHULTZ_MIC.108
      rv = rvrc - rc                                                             SCHULTZ_MIC.109
      t = t + (rc-prev)*aklv                                                     SCHULTZ_MIC.110
      rliq = rc + rr                                                             SCHULTZ_MIC.111
                                                                                 SCHULTZ_MIC.112
C The evaporation process eats up liquid before ice, and                         SCHULTZ_MIC.113
C small particles before large, so the order is rain, pristine                   SCHULTZ_MIC.114
C crystals, snow, and finally ice.   The new diagnostic for cloud                SCHULTZ_MIC.115
C liquid allows for nonzero cloud liquid when the gridpoint is                   SCHULTZ_MIC.116
C subsaturated, but it doesn't make sense to evaporate anything                  SCHULTZ_MIC.117
C if there is cloud liquid present, so we deny evaporation of                    SCHULTZ_MIC.118
C in that case.                                                                  SCHULTZ_MIC.119
                                                                                 SCHULTZ_MIC.120
      If (rc .gt. 0.) go to 10                                                   SCHULTZ_MIC.121
                                                                                 SCHULTZ_MIC.122
C Evaporation of rain as in Dudhia and Moncrieff (JAS 89).                       SCHULTZ_MIC.123
C No condensational growth of rain.                                              SCHULTZ_MIC.124
                                                                                 SCHULTZ_MIC.125
      IF (RR.GT.0. .AND. RV.LT.RSAT) THEN                                        SCHULTZ_MIC.126
         prev = rr                                                               SCHULTZ_MIC.127
         RATE = R2V * (RSAT-RV)                                                  SCHULTZ_MIC.128
         CHANGE = RATE * DT                                                      SCHULTZ_MIC.129
CC         IF (CHANGE.GT.(RSAT-RV)) CHANGE=(RSAT-RV)                             SCHULTZ_MIC.130
         IF (CHANGE.GT.RR) THEN                                                  SCHULTZ_MIC.131
            RV = RV + RR                                                         SCHULTZ_MIC.132
            RR = 0.                                                              SCHULTZ_MIC.133
         ELSE                                                                    SCHULTZ_MIC.134
            RV = RV + CHANGE                                                     SCHULTZ_MIC.135
            RR = RR - CHANGE                                                     SCHULTZ_MIC.136
         ENDIF                                                                   SCHULTZ_MIC.137
         t = t + (rr-prev)*aklv                                                  SCHULTZ_MIC.138
         RLIQ = RC + RR                                                          SCHULTZ_MIC.139
      ENDIF                                                                      SCHULTZ_MIC.140
                                                                                 SCHULTZ_MIC.141
10    Continue                                                                   SCHULTZ_MIC.142
                                                                                 SCHULTZ_MIC.143
C Ice growth and evaporation.  Ice nucleation is more a function of              SCHULTZ_MIC.144
C supersaturation than temperature (the same is true of diffusional ice          SCHULTZ_MIC.145
C crystal growth).  In the presence of cloud water, ice supersaturation          SCHULTZ_MIC.146
C is greatest at -12C, and actually decreases at very cold temperatures.         SCHULTZ_MIC.147
                                                                                 SCHULTZ_MIC.148
C Ice nucleation generates ice mass much slower than diffusional growth,         SCHULTZ_MIC.149
C so we don't compute that function it if there's any cloud ice present.         SCHULTZ_MIC.150
C Also, no nucleation in the absence of cloud water.                             SCHULTZ_MIC.151
                                                                                 SCHULTZ_MIC.152
C The diffusion from vapor to pristine crystals is proportional to the           SCHULTZ_MIC.153
C vapor excess and is a function of the crystal mass already there.              SCHULTZ_MIC.154
C In the presence of cloud water, the excess is .17 g/kg at 1000 mb              SCHULTZ_MIC.155
C and .85 g/kg at 200 mb.  This is a supersaturation of 12.4% at -12 C,          SCHULTZ_MIC.156
C the temperature at which the difference is greatest.                           SCHULTZ_MIC.157
                                                                                 SCHULTZ_MIC.158
C Diffusional growth of snow is not allowed at this time.  The water             SCHULTZ_MIC.159
C mass will get there anyway via crystal growth and collection.                  SCHULTZ_MIC.160
                                                                                 SCHULTZ_MIC.161
      RSAT = RVSATI(P,T)                                                         SCHULTZ_MIC.162
      IF (RICE.EQ.0. .AND. RV.LT.RSAT) GO TO 20                                  SCHULTZ_MIC.163
                                                                                 SCHULTZ_MIC.164
      CALL SATADJI (RV, P, T, RSAT, T1)                                          SCHULTZ_MIC.165
      IF (RV.GT.RSAT) THEN      ! ICE GROWTH                                     SCHULTZ_MIC.166
                                                                                 SCHULTZ_MIC.167
         IF (RC.GT.0. .AND. RP.LT.1E-6) THEN                                     SCHULTZ_MIC.168
            CALL NUCLEATE_PRISTINE (RV, RSAT, T, RHO, RP_NUC)                    SCHULTZ_MIC.169
            IF (RP_NUC.GT.RP) THEN                                               SCHULTZ_MIC.170
               RV = RV + RP                                                      SCHULTZ_MIC.171
               RP = RP_NUC                                                       SCHULTZ_MIC.172
               RV = RV - RP_NUC                                                  SCHULTZ_MIC.173
            ENDIF                                                                SCHULTZ_MIC.174
         ENDIF                                                                   SCHULTZ_MIC.175
                                                                                 SCHULTZ_MIC.176
         IF (RP.GT.0. .AND. RV.GT.RSAT) THEN                                     SCHULTZ_MIC.177
            RATE = V2P * (RV-RSAT) * RP*RHO                                      SCHULTZ_MIC.178
            CHANGE = RATE * DT                                                   SCHULTZ_MIC.179
              IF (CHANGE.GT.(RV-RSAT)) CHANGE=(RV-RSAT)                          SCHULTZ_MIC.180
            RP = RP + CHANGE                                                     SCHULTZ_MIC.181
            RV = RV - CHANGE                                                     SCHULTZ_MIC.182
         ENDIF                                                                   SCHULTZ_MIC.183
                                                                                 SCHULTZ_MIC.184
      ELSE                      ! sublimation                                    SCHULTZ_MIC.185
                                                                                 SCHULTZ_MIC.186
         If (rc.gt.0) go to 20  ! see comment above                              SCHULTZ_MIC.187
                                                                                 SCHULTZ_MIC.188
C Pristine crystals.                                                             SCHULTZ_MIC.189
         IF (RP.GT.0. .AND. RV.LT.RSAT) THEN                                     SCHULTZ_MIC.190
            RATE = P2V * (RSAT-RV)                                               SCHULTZ_MIC.191
            CHANGE = RATE * DT                                                   SCHULTZ_MIC.192
CC            IF (CHANGE.GT.(RSAT-RV)) CHANGE=(RSAT-RV)                          SCHULTZ_MIC.193
            IF (CHANGE.GT.RP) THEN                                               SCHULTZ_MIC.194
               RV = RV + RP                                                      SCHULTZ_MIC.195
               RP = 0.                                                           SCHULTZ_MIC.196
            ELSE                                                                 SCHULTZ_MIC.197
               RV = RV + CHANGE                                                  SCHULTZ_MIC.198
               RP = RP - CHANGE                                                  SCHULTZ_MIC.199
            ENDIF                                                                SCHULTZ_MIC.200
         ENDIF                                                                   SCHULTZ_MIC.201
                                                                                 SCHULTZ_MIC.202
C Then snow.                                                                     SCHULTZ_MIC.203
         IF (RS.GT.0. .AND. RV.LT.RSAT) THEN                                     SCHULTZ_MIC.204
            RATE = S2V * (RSAT-RV)                                               SCHULTZ_MIC.205
            CHANGE = RATE * DT                                                   SCHULTZ_MIC.206
CC            IF (CHANGE.GT.(RSAT-RV)) CHANGE=(RSAT-RV)                          SCHULTZ_MIC.207
            IF (CHANGE.GT.RS) THEN                                               SCHULTZ_MIC.208
               RV = RV + RS                                                      SCHULTZ_MIC.209
               RS = 0.                                                           SCHULTZ_MIC.210
            ELSE                                                                 SCHULTZ_MIC.211
               RV = RV + CHANGE                                                  SCHULTZ_MIC.212
               RS = RS - CHANGE                                                  SCHULTZ_MIC.213
            ENDIF                                                                SCHULTZ_MIC.214
         ENDIF                                                                   SCHULTZ_MIC.215
                                                                                 SCHULTZ_MIC.216
C And finally ice.  It might be argued that graupel and hail can be              SCHULTZ_MIC.217
C water-coated and thus evaporate wrt liquid saturation (i.e., faster).          SCHULTZ_MIC.218
                                                                                 SCHULTZ_MIC.219
         IF (RI.GT.0. .AND. RV.LT.RSAT) THEN                                     SCHULTZ_MIC.220
            RATE = I2V * (RSAT-RV)                                               SCHULTZ_MIC.221
            CHANGE = RATE * DT                                                   SCHULTZ_MIC.222
CC            IF (CHANGE.GT.(RSAT-RV)) CHANGE=(RSAT-RV)                          SCHULTZ_MIC.223
            IF (CHANGE.GT.RI) THEN                                               SCHULTZ_MIC.224
               RV = RV + RI                                                      SCHULTZ_MIC.225
               RI = 0.                                                           SCHULTZ_MIC.226
            ELSE                                                                 SCHULTZ_MIC.227
               RV = RV + CHANGE                                                  SCHULTZ_MIC.228
               RI = RI - CHANGE                                                  SCHULTZ_MIC.229
            ENDIF                                                                SCHULTZ_MIC.230
         ENDIF                                                                   SCHULTZ_MIC.231
                                                                                 SCHULTZ_MIC.232
      ENDIF                                                                      SCHULTZ_MIC.233
                                                                                 SCHULTZ_MIC.234
      X = (RP + RS + RI) - RICE                                                  SCHULTZ_MIC.235
      T = T + X*AKIV                                                             SCHULTZ_MIC.236
      RICE = RP + RS + RI                                                        SCHULTZ_MIC.237
                                                                                 SCHULTZ_MIC.238
20    CONTINUE                                                                   SCHULTZ_MIC.239
                                                                                 SCHULTZ_MIC.240
C Melting and freezing.                                                          SCHULTZ_MIC.241
C Melting first.  Cloud ice melts immediately.  Cloud ice                        SCHULTZ_MIC.242
C melts into cloud liquid.  Snow and ice melt into rain.                         SCHULTZ_MIC.243
C The amount of melting is limited to the amount it takes                        SCHULTZ_MIC.244
C to cool the parcel to the freezing point.  It works out                        SCHULTZ_MIC.245
C to about 3 g/kg per centigrade degree.  Start by calculating                   SCHULTZ_MIC.246
C the maximum amount of melting possible in this time step.                      SCHULTZ_MIC.247
                                                                                 SCHULTZ_MIC.248
      IF (T.GT.273.1) THEN                                                       SCHULTZ_MIC.249
                                                                                 SCHULTZ_MIC.250
         MAXMELT = (T-273.1) * CP/LLI                                            SCHULTZ_MIC.251
                                                                                 SCHULTZ_MIC.252
         IF (RP.GT.0.) THEN                                                      SCHULTZ_MIC.253
            IF (RP.GT.MAXMELT) THEN                                              SCHULTZ_MIC.254
               RC = RC + MAXMELT                                                 SCHULTZ_MIC.255
               RP = RP - MAXMELT                                                 SCHULTZ_MIC.256
               GO TO 30                                                          SCHULTZ_MIC.257
            ELSE                                                                 SCHULTZ_MIC.258
               MAXMELT = MAXMELT - RP                                            SCHULTZ_MIC.259
               RC = RC + RP                                                      SCHULTZ_MIC.260
               RP = 0.                                                           SCHULTZ_MIC.261
            ENDIF                                                                SCHULTZ_MIC.262
         ENDIF                                                                   SCHULTZ_MIC.263
                                                                                 SCHULTZ_MIC.264
         IF (RS.GT.0.) THEN                                                      SCHULTZ_MIC.265
            RATE = S2R * (T-273.1)                                               SCHULTZ_MIC.266
            CHANGE = RATE*DT                                                     SCHULTZ_MIC.267
            IF (CHANGE.GT.MAXMELT) CHANGE=MAXMELT                                SCHULTZ_MIC.268
            IF (CHANGE.LT.RS) THEN                                               SCHULTZ_MIC.269
               RR = RR + CHANGE                                                  SCHULTZ_MIC.270
               RS = RS - CHANGE                                                  SCHULTZ_MIC.271
               MAXMELT = MAXMELT - CHANGE                                        SCHULTZ_MIC.272
            ELSE                                                                 SCHULTZ_MIC.273
               RR = RR + RS                                                      SCHULTZ_MIC.274
               MAXMELT = MAXMELT - RS                                            SCHULTZ_MIC.275
               RS = 0.                                                           SCHULTZ_MIC.276
            ENDIF                                                                SCHULTZ_MIC.277
         ENDIF                                                                   SCHULTZ_MIC.278
                                                                                 SCHULTZ_MIC.279
         IF (RI.GT.0.) THEN                                                      SCHULTZ_MIC.280
            RATE = I2R * (T-273.1)                                               SCHULTZ_MIC.281
            CHANGE = RATE*DT                                                     SCHULTZ_MIC.282
            IF (CHANGE.GT.MAXMELT) CHANGE=MAXMELT                                SCHULTZ_MIC.283
            IF (CHANGE.LT.RI) THEN                                               SCHULTZ_MIC.284
               RR = RR + CHANGE                                                  SCHULTZ_MIC.285
               RI = RI - CHANGE                                                  SCHULTZ_MIC.286
            ELSE                                                                 SCHULTZ_MIC.287
               RR = RR + RI                                                      SCHULTZ_MIC.288
               RI = 0.                                                           SCHULTZ_MIC.289
            ENDIF                                                                SCHULTZ_MIC.290
         ENDIF                                                                   SCHULTZ_MIC.291
                                                                                 SCHULTZ_MIC.292
 30      CONTINUE                                                                SCHULTZ_MIC.293
                                                                                 SCHULTZ_MIC.294
      ELSE                                                                       SCHULTZ_MIC.295
                                                                                 SCHULTZ_MIC.296
C Now freezing.  First the cloud liquid, then the rain.                          SCHULTZ_MIC.297
C Cloud water stays supercooled well below freezing, but how much?               SCHULTZ_MIC.298
C This is just freezing because it's cold.  There is very little                 SCHULTZ_MIC.299
C cloud water below -25C, and almost none observed below -40C.                   SCHULTZ_MIC.300
C Ramp parabolically from -20 to -40.                                            SCHULTZ_MIC.301
                                                                                 SCHULTZ_MIC.302
         IF (RC.GT.0. .AND. T.LT.253.) THEN                                      SCHULTZ_MIC.303
            RATE = C2P * ((253.-T)/20.)**2                                       SCHULTZ_MIC.304
            CHANGE = RATE * DT                                                   SCHULTZ_MIC.305
            IF (CHANGE.GT.RC) THEN                                               SCHULTZ_MIC.306
               RP = RP + RC                                                      SCHULTZ_MIC.307
               RC = 0.                                                           SCHULTZ_MIC.308
            ELSE                                                                 SCHULTZ_MIC.309
               RP = RP + CHANGE                                                  SCHULTZ_MIC.310
               RC = RC - CHANGE                                                  SCHULTZ_MIC.311
            ENDIF                                                                SCHULTZ_MIC.312
         ENDIF                                                                   SCHULTZ_MIC.313
                                                                                 SCHULTZ_MIC.314
C Rain freezing into ice; parabolic function similar to c2p.                     SCHULTZ_MIC.315
C Based loosely on Fig.1 from Cotton (MWR 72B).                                  SCHULTZ_MIC.316
                                                                                 SCHULTZ_MIC.317
         IF (RR.GT.0. .AND. T.LT.267.) THEN                                      SCHULTZ_MIC.318
            RATE = R2I * ((267.-T)/14.)**2                                       SCHULTZ_MIC.319
            CHANGE = RATE * DT                                                   SCHULTZ_MIC.320
            IF (CHANGE.GT.RR) THEN                                               SCHULTZ_MIC.321
               RI = RI + RR                                                      SCHULTZ_MIC.322
               RR = 0.                                                           SCHULTZ_MIC.323
            ELSE                                                                 SCHULTZ_MIC.324
               RI = RI + CHANGE                                                  SCHULTZ_MIC.325
               RR = RR - CHANGE                                                  SCHULTZ_MIC.326
            ENDIF                                                                SCHULTZ_MIC.327
         ENDIF                                                                   SCHULTZ_MIC.328
                                                                                 SCHULTZ_MIC.329
      ENDIF                                                                      SCHULTZ_MIC.330
                                                                                 SCHULTZ_MIC.331
C Temperature after freezing or melting.                                         SCHULTZ_MIC.332
      X = (RC + RR) - RLIQ                                                       SCHULTZ_MIC.333
      T = T - X*LLI*CPINV                                                        SCHULTZ_MIC.334
C This should give the same answer.                                              SCHULTZ_MIC.335
C     X = (RP + RS + RI) - RICE                                                  SCHULTZ_MIC.336
C     T = T + X*LLI/CP                                                           SCHULTZ_MIC.337
                                                                                 SCHULTZ_MIC.338
      RLIQ = RC + RR                                                             SCHULTZ_MIC.339
      RICE = RP + RS + RI                                                        SCHULTZ_MIC.340
                                                                                 SCHULTZ_MIC.341
C Collection.  These processes are determined by spacing between                 SCHULTZ_MIC.342
C particles, regardless of how much gas is also in the volume.                   SCHULTZ_MIC.343
C So we'll first convert mixing ratios to specific contents, and                 SCHULTZ_MIC.344
C then later back to mixing ratios.                                              SCHULTZ_MIC.345
                                                                                 SCHULTZ_MIC.346
      QC = RC * RHO                                                              SCHULTZ_MIC.347
      QP = RP * RHO                                                              SCHULTZ_MIC.348
      QR = RR * RHO                                                              SCHULTZ_MIC.349
      QS = RS * RHO                                                              SCHULTZ_MIC.350
      QI = RI * RHO                                                              SCHULTZ_MIC.351
                                                                                 SCHULTZ_MIC.352
C Autoconversion.  As soon as you build up enough cloud matter,                  SCHULTZ_MIC.353
C it starts converting to rain or snow.  This "nucleates" the                    SCHULTZ_MIC.354
C collection process, which is nonlinear.  The nucleated amount                  SCHULTZ_MIC.355
C determines how long before rapid collection occurs.  If some                   SCHULTZ_MIC.356
C precipitate is already present, the autoconv procedure just                    SCHULTZ_MIC.357
C makes sure there's enough.                                                     SCHULTZ_MIC.358
                                                                                 SCHULTZ_MIC.359
      IF (QC.GT.QCMIN) THEN                                                      SCHULTZ_MIC.360
         CHANGE = QC - QCMIN                                                     SCHULTZ_MIC.361
         IF (CHANGE.GT.QR) THEN                                                  SCHULTZ_MIC.362
            CHANGE = CHANGE - QR                                                 SCHULTZ_MIC.363
            QR = QR + CHANGE                                                     SCHULTZ_MIC.364
            QC = QC - CHANGE                                                     SCHULTZ_MIC.365
         ENDIF                                                                   SCHULTZ_MIC.366
      ENDIF                                                                      SCHULTZ_MIC.367
                                                                                 SCHULTZ_MIC.368
      IF (QP.GT.QPMIN) THEN                                                      SCHULTZ_MIC.369
         CHANGE = QP - QPMIN                                                     SCHULTZ_MIC.370
         IF (CHANGE.GT.QS) THEN                                                  SCHULTZ_MIC.371
            CHANGE = CHANGE - QS                                                 SCHULTZ_MIC.372
            QS = QS + CHANGE                                                     SCHULTZ_MIC.373
            QP = QP - CHANGE                                                     SCHULTZ_MIC.374
         ENDIF                                                                   SCHULTZ_MIC.375
      ENDIF                                                                      SCHULTZ_MIC.376
                                                                                 SCHULTZ_MIC.377
C There can be a three-way competition for cloud water among the                 SCHULTZ_MIC.378
C rain, snow, and ice, so we distribute it among the three                       SCHULTZ_MIC.379
C categories.  The "need" variables are the amount of cloud water                SCHULTZ_MIC.380
C that species would in a time step if it didn't have to compete.                SCHULTZ_MIC.381
C The collection formula for rain is very nearly the same as                     SCHULTZ_MIC.382
C Soong and Ogura (1973); the other functions are based on that.                 SCHULTZ_MIC.383
                                                                                 SCHULTZ_MIC.384
      IF (QC.GT.0.) THEN                                                         SCHULTZ_MIC.385
         RATE = C2R * QC * QR                                                    SCHULTZ_MIC.386
         NEEDR = RATE*DT                                                         SCHULTZ_MIC.387
         RATE = C2S * QC * QS                                                    SCHULTZ_MIC.388
         NEEDS = RATE*DT                                                         SCHULTZ_MIC.389
         RATE = C2I * QC * QI                                                    SCHULTZ_MIC.390
         NEEDI = RATE*DT                                                         SCHULTZ_MIC.391
         NEED = NEEDR + NEEDS + NEEDI                                            SCHULTZ_MIC.392
         IF (NEED .GT. QC) THEN                                                  SCHULTZ_MIC.393
            NEEDR = NEEDR * QC / NEED                                            SCHULTZ_MIC.394
            NEEDS = NEEDS * QC / NEED                                            SCHULTZ_MIC.395
            NEEDI = NEEDI * QC / NEED                                            SCHULTZ_MIC.396
         ENDIF                                                                   SCHULTZ_MIC.397
                                                                                 SCHULTZ_MIC.398
C The riming process nucleates a little cloud ice.  Until a better               SCHULTZ_MIC.399
C number comes along we'll say 1% of the collected liquid, both for              SCHULTZ_MIC.400
C snow and graupel.                                                              SCHULTZ_MIC.401
C        QP = QP + .01*NEEDS                                                     SCHULTZ_MIC.402
C        NEEDS = NEEDS - .01*NEEDS                                               SCHULTZ_MIC.403
C        QP = QP + .01*NEEDI                                                     SCHULTZ_MIC.404
C        NEEDI = NEEDI - .01*NEEDI                                               SCHULTZ_MIC.405
                                                                                 SCHULTZ_MIC.406
C We assume that the result of snow riming is to convert the cloud               SCHULTZ_MIC.407
C water, but not the snow, to the graupel category.                              SCHULTZ_MIC.408
                                                                                 SCHULTZ_MIC.409
         QR = QR + NEEDR                                                         SCHULTZ_MIC.410
         QI = QI + NEEDI + NEEDS                                                 SCHULTZ_MIC.411
         QC = QC - NEEDR - NEEDS - NEEDI                                         SCHULTZ_MIC.412
                                                                                 SCHULTZ_MIC.413
      ENDIF                                                                      SCHULTZ_MIC.414
                                                                                 SCHULTZ_MIC.415
C Unlike the collection of cloud water, which can be complete, we                SCHULTZ_MIC.416
C don't want to zero out the pristine crystals.  For one thing, some             SCHULTZ_MIC.417
C are so tiny they won't get collected, but also, we want to be able             SCHULTZ_MIC.418
C to produce more condensate if it still supersaturated wrt ice,                 SCHULTZ_MIC.419
C which won't happen if there's zero cloud ice.  We'll leave behind              SCHULTZ_MIC.420
C the equivalent of 100 per liter.  At 1E-11 kg per crystal and                  SCHULTZ_MIC.421
C 1000 liters per m**3, that would be 1E-6 kg/m**3 (1 mg/m**3).                  SCHULTZ_MIC.422
C The temperature-dependent efficiency follows Lin et al (JCAM 83).              SCHULTZ_MIC.423
                                                                                 SCHULTZ_MIC.424
      IF (QP.GT.0. .AND. QS.GT.0.) THEN                                          SCHULTZ_MIC.425
         EFF = 1. - (273.1-T)/50.                                                SCHULTZ_MIC.426
         RATE = P2S * EFF * QP * QS                                              SCHULTZ_MIC.427
         CHANGE = RATE * DT                                                      SCHULTZ_MIC.428
         IF (CHANGE.GT.QP) THEN                                                  SCHULTZ_MIC.429
            QS = QS + QP - 1E-6                                                  SCHULTZ_MIC.430
            QP = 1E-6                                                            SCHULTZ_MIC.431
         ELSE                                                                    SCHULTZ_MIC.432
            QS = QS + CHANGE                                                     SCHULTZ_MIC.433
            QP = QP - CHANGE                                                     SCHULTZ_MIC.434
         ENDIF                                                                   SCHULTZ_MIC.435
      ENDIF                                                                      SCHULTZ_MIC.436
                                                                                 SCHULTZ_MIC.437
C Convert back to mixing ratios.                                                 SCHULTZ_MIC.438
      RC = QC / RHO                                                              SCHULTZ_MIC.439
      RP = QP / RHO                                                              SCHULTZ_MIC.440
      RR = QR / RHO                                                              SCHULTZ_MIC.441
      RS = QS / RHO                                                              SCHULTZ_MIC.442
      RI = QI / RHO                                                              SCHULTZ_MIC.443
                                                                                 SCHULTZ_MIC.444
C Calculate temperature after phase changes resulting from collection.           SCHULTZ_MIC.445
      X = (RC + RR) - RLIQ                                                       SCHULTZ_MIC.446
      T = T - X*LLI*CPINV                                                        SCHULTZ_MIC.447
      RLIQ = RC + RR                                                             SCHULTZ_MIC.448
      RICE = RP + RS + RI                                                        SCHULTZ_MIC.449
                                                                                 SCHULTZ_MIC.450
C Conservation checks.                                                           SCHULTZ_MIC.451
      RTOT1 = RV + RLIQ + RICE                                                   SCHULTZ_MIC.452
      IF (ABS(RTOT0-RTOT1).GT..000001) THEN                                      SCHULTZ_MIC.453
         WRITE(*,*) 'RTOT CHECK', RTOT0, RTOT1                                   SCHULTZ_MIC.454
         WRITE(*,*) RV, RC, RR, RP, RS, RI                                       SCHULTZ_MIC.455
      ENDIF                                                                      SCHULTZ_MIC.456
                                                                                 SCHULTZ_MIC.457
      TIL1 = T - (AKLV*RLIQ+AKIV*RICE)                                           SCHULTZ_MIC.458
      IF (ABS(TIL1-TIL0).GT..001) THEN                                           SCHULTZ_MIC.459
         WRITE(*,*) 'TIL CHECK', TIL0, TIL1                                      SCHULTZ_MIC.460
      ENDIF                                                                      SCHULTZ_MIC.461
                                                                                 SCHULTZ_MIC.462
C These terminal velocity formulations are similar in form to                    SCHULTZ_MIC.463
C Ogura and Takahashi (1971).  The curves for rain and snow were                 SCHULTZ_MIC.464
C tweaked until they matched the curves on page 241 of Pielke's                  SCHULTZ_MIC.465
C book (they are very, very close).  The curve for ice is based                  SCHULTZ_MIC.466
C on the curve for rain; the only difference is in the exponent.                 SCHULTZ_MIC.467
C The effect is that small values of ice, presumed to be                         SCHULTZ_MIC.468
C heavily-rimed snow, fall slower than rain of the same                          SCHULTZ_MIC.469
C concentration, but higher values, presumed to be big graupel                   SCHULTZ_MIC.470
C or hail, fall faster than rain.  The transition is at 1 g/kg.                  SCHULTZ_MIC.471
C The terminal velocity for pristine crystals does not have a                    SCHULTZ_MIC.472
C dependency on mixing ratio like the others, because the others                 SCHULTZ_MIC.473
C incorporate the assumption that higher mixing ratios imply                     SCHULTZ_MIC.474
C bigger particles, which fall faster, but that's not the case                   SCHULTZ_MIC.475
C for pristine crystals.                                                         SCHULTZ_MIC.476
                                                                                 SCHULTZ_MIC.477
      IF (RP.GT.0.) TVP = -0.5 * SQRT(1./RHO)                                    SCHULTZ_MIC.478
      IF (RR.GT.0.) TVR = -5.5 * (RR/.001)**.125 * SQRT(1./RHO)                  SCHULTZ_MIC.479
      IF (RS.GT.0.) TVS = -2.0 * (RS/.001)**.100 * SQRT(1./RHO)                  SCHULTZ_MIC.480
      IF (RI.GT.0.) TVI = -5.5 * (RI/.001)**.333 * SQRT(1./RHO)                  SCHULTZ_MIC.481
                                                                                 SCHULTZ_MIC.482
      RETURN                                                                     SCHULTZ_MIC.483
      END                                                                        SCHULTZ_MIC.484
                                                                                 SCHULTZ_MIC.485
C+---+-----------------------------------------------------------------+         SCHULTZ_MIC.486
C This is basically the wet-bulb temperature problem, where                      SCHULTZ_MIC.487
C condensing or evaporating water changes the parcel temperature                 SCHULTZ_MIC.488
C and thus the saturation vapor mixing ratio.  Asai (1965)                       SCHULTZ_MIC.489
                                                                                 SCHULTZ_MIC.490
      SUBROUTINE SATADJL (RV, P, T, RSAT, T1)                                    SCHULTZ_MIC.491
      REAL RV, P, T, RSAT, T1                                                    SCHULTZ_MIC.492
      Real cpm, factor                                                           SCHULTZ_MIC.493
      Real Lvl, cp, Rvap                                                         SCHULTZ_MIC.494
      Parameter (Lvl=2.500E6, cp=1004., Rvap=461.5)                              SCHULTZ_MIC.495
                                                                                 SCHULTZ_MIC.496
      cpm = cp*(1.+.81*rsat)                                                     SCHULTZ_MIC.497
      factor = 1./(1.+(Lvl*Lvl*rsat/(cpm*Rvap*t*t)))                             SCHULTZ_MIC.498
      rsat = rsat + factor*(rv-rsat)                                             SCHULTZ_MIC.499
      t1 = t + (rv-rsat)*Lvl/cp                                                  SCHULTZ_MIC.500
                                                                                 SCHULTZ_MIC.501
      RETURN                                                                     SCHULTZ_MIC.502
      END                                                                        SCHULTZ_MIC.503
                                                                                 SCHULTZ_MIC.504
C+---+-----------------------------------------------------------------+         SCHULTZ_MIC.505
      SUBROUTINE SATADJI (RV, P, T, RSAT, T1)                                    SCHULTZ_MIC.506
      REAL RV, P, T, RSAT, T1                                                    SCHULTZ_MIC.507
      Real cpm, factor                                                           SCHULTZ_MIC.508
      Real Lvi, cp, Rvap                                                         SCHULTZ_MIC.509
      Parameter (Lvi=2.834E6, cp=1004., Rvap=461.5)                              SCHULTZ_MIC.510
                                                                                 SCHULTZ_MIC.511
      cpm = cp*(1.+.81*rsat)                                                     SCHULTZ_MIC.512
      factor = 1./(1.+(Lvi*Lvi*rsat/(cpm*Rvap*t*t)))                             SCHULTZ_MIC.513
      rsat = rsat + factor*(rv-rsat)                                             SCHULTZ_MIC.514
      t1 = t + (rv-rsat)*Lvi/cp                                                  SCHULTZ_MIC.515
                                                                                 SCHULTZ_MIC.516
      RETURN                                                                     SCHULTZ_MIC.517
      END                                                                        SCHULTZ_MIC.518
                                                                                 SCHULTZ_MIC.519
C+---+-----------------------------------------------------------------+         SCHULTZ_MIC.520
C Following Meyers et al (JAM 91).  The original expression gave                 SCHULTZ_MIC.521
C the number of crystals per liter.  The factor 1E3 converts to                  SCHULTZ_MIC.522
C cubic meters.  PMAS (this is the variable's name in RAMS) is the               SCHULTZ_MIC.523
C average mass of nucleated pristine crystals.  Dividing by rho                  SCHULTZ_MIC.524
C converts from specific content (kgp/m**3) to mixing ratio (kgp/kg).            SCHULTZ_MIC.525
C The amount of cloud ice nucleated is arbitrarily limited to half               SCHULTZ_MIC.526
C the vapor excess, but this should never happen.                                SCHULTZ_MIC.527
                                                                                 SCHULTZ_MIC.528
      SUBROUTINE NUCLEATE_PRISTINE (RV, RSAT, T, RHO, RP_NUC)                    SCHULTZ_MIC.529
      REAL RV, RSAT, T, RHO, RP_NUC                                              SCHULTZ_MIC.530
      REAL PMAS                                                                  SCHULTZ_MIC.531
      PARAMETER (PMAS=1.E-11)                                                    SCHULTZ_MIC.532
                                                                                 SCHULTZ_MIC.533
      RP_NUC = 0.                                                                SCHULTZ_MIC.534
      IF (T.GT.268.) RETURN                                                      SCHULTZ_MIC.535
                                                                                 SCHULTZ_MIC.536
      RP_NUC = 1.E3 * EXP(-.639+12.96*(RV/RSAT-1.)) * PMAS / RHO                 SCHULTZ_MIC.537
      IF (RP_NUC.GT.(RV-RSAT)/2.) RP_NUC = (RV-RSAT)/2.                          SCHULTZ_MIC.538
                                                                                 SCHULTZ_MIC.539
      RETURN                                                                     SCHULTZ_MIC.540
      END                                                                        SCHULTZ_MIC.541
                                                                                 SCHULTZ_MIC.542
C+---+-----------------------------------------------------------------+         SCHULTZ_MIC.543
C These functions compute the saturation mixing ratios over liquid               SCHULTZ_MIC.544
C and ice.                                                                       SCHULTZ_MIC.545
                                                                                 SCHULTZ_MIC.546
      REAL FUNCTION RVSATL(P,T)                                                  SCHULTZ_MIC.547
                                                                                 SCHULTZ_MIC.548
      REAL ESL,X,T,P,C0,C1,C2,C3,C4,C5,C6,C7,C8                                  SCHULTZ_MIC.549
      PARAMETER (C0= .611583699E03,  C1= .444606896E02)                          SCHULTZ_MIC.550
      PARAMETER (C2= .143177157E01,  C3= .264224321E-1)                          SCHULTZ_MIC.551
      PARAMETER (C4= .299291081E-3,  C5= .203154182E-5)                          SCHULTZ_MIC.552
      PARAMETER (C6= .702620698E-8,  C7= .379534310E-11)                         SCHULTZ_MIC.553
      PARAMETER (C8=-.321582393E-13)                                             SCHULTZ_MIC.554
                                                                                 SCHULTZ_MIC.555
      X=MAX(-80.,T-273.16)                                                       SCHULTZ_MIC.556
      ESL=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))               SCHULTZ_MIC.557
      RVSATL=.622*ESL/(P-ESL)                                                    SCHULTZ_MIC.558
                                                                                 SCHULTZ_MIC.559
      RETURN                                                                     SCHULTZ_MIC.560
      END                                                                        SCHULTZ_MIC.561
                                                                                 SCHULTZ_MIC.562
C+---+-----------------------------------------------------------------+         SCHULTZ_MIC.563
      REAL FUNCTION RVSATI(P,T)                                                  SCHULTZ_MIC.564
                                                                                 SCHULTZ_MIC.565
      REAL ESI,X,T,P,C0,C1,C2,C3,C4,C5,C6,C7,C8                                  SCHULTZ_MIC.566
      PARAMETER (C0= .609868993E03,  C1= .499320233E02)                          SCHULTZ_MIC.567
      PARAMETER (C2= .184672631E01,  C3= .402737184E-1)                          SCHULTZ_MIC.568
      PARAMETER (C4= .565392987E-3,  C5= .521693933E-5)                          SCHULTZ_MIC.569
      PARAMETER (C6= .307839583E-7,  C7= .105785160E-9)                          SCHULTZ_MIC.570
      PARAMETER (C8= .161444444E-12)                                             SCHULTZ_MIC.571
                                                                                 SCHULTZ_MIC.572
      X=MAX(-80.,T-273.16)                                                       SCHULTZ_MIC.573
      ESI=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))               SCHULTZ_MIC.574
      RVSATI=.622*ESI/(P-ESI)                                                    SCHULTZ_MIC.575
                                                                                 SCHULTZ_MIC.576
      RETURN                                                                     SCHULTZ_MIC.577
      END                                                                        SCHULTZ_MIC.578
                                                                                 SCHULTZ_MIC.579
C+---+-----------------------------------------------------------------+         SCHULTZ_MIC.580
      Subroutine Cu_fraction (dx, lapse, rvsat, rh, rc, frac)                    SCHULTZ_MIC.581
      Implicit none                                                              SCHULTZ_MIC.582
                                                                                 SCHULTZ_MIC.583
cThis routine uses dx,lapse,rvsat,rh as inputs only, their values                SCHULTZ_MIC.584
care not changed herein.  The variables rc,frac are outputs only.                SCHULTZ_MIC.585
                                                                                 SCHULTZ_MIC.586
      Real dx, dxmin, dxmax                                                      SCHULTZ_MIC.587
      Real lapse, lapsemin, lapsemax                                             SCHULTZ_MIC.588
      Real rh, rh0, rhmin, rhmax, rhclear, rhcloud                               SCHULTZ_MIC.589
      Real rc, rcmax, rccloud                                                    SCHULTZ_MIC.590
      Real rvsat, frac, x                                                        SCHULTZ_MIC.591
                                                                                 SCHULTZ_MIC.592
cThese are the limits of grid increment and lapse rate for which this            SCHULTZ_MIC.593
calgorithm is intended.                                                          SCHULTZ_MIC.594
                                                                                 SCHULTZ_MIC.595
      Parameter (dxmin=2.)       ! km                                            SCHULTZ_MIC.596
      Parameter (dxmax=20.)      ! km                                            SCHULTZ_MIC.597
      Parameter (lapsemin=-10.)  ! K/km                                          SCHULTZ_MIC.598
      Parameter (lapsemax=-6.)   ! K/km                                          SCHULTZ_MIC.599
                                                                                 SCHULTZ_MIC.600
cGrid-volume average rh at which this routine begins to diagnose cloud           SCHULTZ_MIC.601
cliquid at dx=dxmax and lapse=lapsemin.  Used to compute rhmin.                  SCHULTZ_MIC.602
                                                                                 SCHULTZ_MIC.603
      Parameter (rh0=.6)                                                         SCHULTZ_MIC.604
                                                                                 SCHULTZ_MIC.605
cMaximum cloud liquid mixing ratio in the cloudy part of the grid                SCHULTZ_MIC.606
cvolume.  Used to compute rccloud and rhmax.  rc=rccloud at frac=1,              SCHULTZ_MIC.607
cand rc=rccloud=rcmax at frac=1 and lapse=lapsemin.  No dependence               SCHULTZ_MIC.608
con dx.                                                                          SCHULTZ_MIC.609
                                                                                 SCHULTZ_MIC.610
      Parameter (rcmax=1.)       ! g/kg                                          SCHULTZ_MIC.611
                                                                                 SCHULTZ_MIC.612
cInitialize return variables to clear-air conditions.                            SCHULTZ_MIC.613
                                                                                 SCHULTZ_MIC.614
      frac = 0.                                                                  SCHULTZ_MIC.615
      rhclear = rh                                                               SCHULTZ_MIC.616
      rc = 0.                                                                    SCHULTZ_MIC.617
      rhcloud = 1.                                                               SCHULTZ_MIC.618
                                                                                 SCHULTZ_MIC.619
cHandle the trivial cases first and fastest.                                     SCHULTZ_MIC.620
                                                                                 SCHULTZ_MIC.621
      If (rh.lt.rh0) Return                                                      SCHULTZ_MIC.622
      If (dx.lt.dxmin .OR. lapse.gt.lapsemax) then                               SCHULTZ_MIC.623
        If (rh.ge.1.) then                                                       SCHULTZ_MIC.624
          frac = 1.                                                              SCHULTZ_MIC.625
          rc = rvsat * (rh-1.)                                                   SCHULTZ_MIC.626
          rhclear = 1.                                                           SCHULTZ_MIC.627
          rhcloud = 1. + rc/rvsat                                                SCHULTZ_MIC.628
        End if                                                                   SCHULTZ_MIC.629
        Return                                                                   SCHULTZ_MIC.630
      End if                                                                     SCHULTZ_MIC.631
                                                                                 SCHULTZ_MIC.632
cThe basic procedure is to calculate the cloud fraction as an                    SCHULTZ_MIC.633
cinterpolation of rh between the limits of rhmin, which is a function            SCHULTZ_MIC.634
cof gridsize and static stability, and rhmax, which is the "rh" of               SCHULTZ_MIC.635
cthe cloudy fraction of the gridbox.  (Likewise, rccloud is the mixing           SCHULTZ_MIC.636
cratio of cloud liquid in the cloudy fraction of the gridbox.)                   SCHULTZ_MIC.637
crhmin is constrained to be no higher than .99                                   SCHULTZ_MIC.638
                                                                                 SCHULTZ_MIC.639
      If (dx.lt.dxmax) then                                                      SCHULTZ_MIC.640
        rhmin = rh0 + (.99-rh0)*((dxmax-dx)/(dxmax-dxmin))                       SCHULTZ_MIC.641
      Else                                                                       SCHULTZ_MIC.642
        rhmin = rh0                                                              SCHULTZ_MIC.643
      End if                                                                     SCHULTZ_MIC.644
                                                                                 SCHULTZ_MIC.645
      x = lapse                                                                  SCHULTZ_MIC.646
      if (lapse.lt.lapsemin) x = lapsemin                                        SCHULTZ_MIC.647
      if (lapse.gt.lapsemax) x = lapsemax                                        SCHULTZ_MIC.648
      rhmin = rhmin +                                                            SCHULTZ_MIC.649
     +       (.99-rhmin)*(x-lapsemin)/(lapsemax-lapsemin)                        SCHULTZ_MIC.650
      If (rh.lt.rhmin) Return                                                    SCHULTZ_MIC.651
                                                                                 SCHULTZ_MIC.652
      rccloud = rcmax * (1. - (x-lapsemin)/(lapsemax-lapsemin) )                 SCHULTZ_MIC.653
      rhmax = (rccloud+rvsat)/rvsat                                              SCHULTZ_MIC.654
      If (rh.gt.rhmax) then                                                      SCHULTZ_MIC.655
        frac = 1.                                                                SCHULTZ_MIC.656
        rc = rvsat * (rh-1.)                                                     SCHULTZ_MIC.657
        rhclear = 1.                                                             SCHULTZ_MIC.658
        rhcloud = rh                                                             SCHULTZ_MIC.659
        Return                                                                   SCHULTZ_MIC.660
      End if                                                                     SCHULTZ_MIC.661
                                                                                 SCHULTZ_MIC.662
      frac = (rh-rhmin)/(rhmax-rhmin)                                            SCHULTZ_MIC.663
                                                                                 SCHULTZ_MIC.664
      If (frac.lt..01) then                                                      SCHULTZ_MIC.665
        frac = 0.                                                                SCHULTZ_MIC.666
      Else if (frac.gt..99) then                                                 SCHULTZ_MIC.667
        frac = 1.                                                                SCHULTZ_MIC.668
        rc = rvsat * (rh-1.)                                                     SCHULTZ_MIC.669
        if (rc.lt.0.) rc = 0.                                                    SCHULTZ_MIC.670
        rhcloud = 1. + rc/rvsat                                                  SCHULTZ_MIC.671
        rhclear = 1.                                                             SCHULTZ_MIC.672
      Else                                                                       SCHULTZ_MIC.673
        rc = rccloud * frac                                                      SCHULTZ_MIC.674
        rhcloud = (rvsat+rccloud)/rvsat                                          SCHULTZ_MIC.675
        rhclear = (rh - frac*rhcloud) / (1.-frac)                                SCHULTZ_MIC.676
        If (abs(rhclear-rhmin).gt..00001) print*,                                SCHULTZ_MIC.677
     +         'This shouldn''t happen:',  rhclear, rhmin                        SCHULTZ_MIC.678
      End if                                                                     SCHULTZ_MIC.679
                                                                                 SCHULTZ_MIC.680
      Return                                                                     SCHULTZ_MIC.681
      End                                                                        SCHULTZ_MIC.682
