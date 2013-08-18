C-*-fortran-*-                                                                   NESTLSM.1
      subroutine nestlsm(nescou, help1, help1n, lmask, imask)                    NESTLSM.2
      implicit none                                                              NESTLSM.3
#     include <defines.incl>                                                     NESTLSM.4
#     include <parame.incl>                                                      NESTLSM.5
#     include <pmoist.incl>                                                      NESTLSM.6
#     include <param2.incl>                                                      NESTLSM.7
#     include <parfdda.incl>                                                     NESTLSM.8
#     include <param3.incl>                                                      NESTLSM.9
#     include <addr0.incl>                                                       NESTLSM.10
#     include <varia.incl>                                                       NESTLSM.11
#     include <point3d.incl>                                                     NESTLSM.12
#     include <point2d.incl>                                                     NESTLSM.13
#     include <navypb.incl>                                                      NESTLSM.14
#     include <various.incl>                                                     NESTLSM.15
#     include <fddagd.incl>                                                      NESTLSM.16
#     include <fddaob.incl>                                                      NESTLSM.17
#     include <dusolve1.incl>                                                    NESTLSM.18
#     include <soil.incl>                                                        NESTLSM.19
#     include <nestl.incl>                                                       NESTLSM.20
#     include <point3dn.incl>                                                    NESTLSM.21
#     include <point2dn.incl>                                                    NESTLSM.22
#     include <variousn.incl>                                                    NESTLSM.23
#     include <nonhyd.incl>                                                      NESTLSM.24
#     include <nhcnst.incl>                                                      NESTLSM.25
#     include <nnnhyd.incl>                                                      NESTLSM.26
#     include <nncnst.incl>                                                      NESTLSM.27
#     include <pnavyn.incl>                                                      NESTLSM.28
#     include <addrcun.incl>                                                     NESTLSM.29
#     include <soiln.incl>                                                       NESTLSM.30
#     include <soilp.incl>                                                       NESTLSM.31
#     include <soilpn.incl>                                                      NESTLSM.32
#     include <rpstar.incl>                                                      NESTLSM.33
#     include <rpstarn.incl>                                                     NESTLSM.34
#     include <uprad.incl>                                                       NESTLSM.35
#ifdef MPP1                                                                      NESTLSM.36
#     include <radiat.incl>                                                      NESTLSM.37
#endif                                                                           NESTLSM.38
#     include <pbltke.incl>                                                      NESTLSM.39
#     include <pbltken.incl>                                                     NESTLSM.40
#     include <soilcnst.incl>                                                    NESTLSM.41
#     include <landuse.incl>                                                     NESTLSM.42
                                                                                 NESTLSM.43
      integer nescou                                                             NESTLSM.44
      real help1(MIX,MJX)                                                        NESTLSM.45
      real help1n(MIX,MJX)                                                       NESTLSM.46
      integer i, j, l, ip, jp                                                    NESTLSM.47
      real nearw, stfun, missing_soil                                            NESTLSM.48
      real tmpval                                                                NESTLSM.49
                                                                                 NESTLSM.50
c These two arrays are declared as real in the calling routine.  Some            NESTLSM.51
c compilers may have legimitate complaints about this:                           NESTLSM.52
      integer lmask(MIX,MJX)                                                     NESTLSM.53
      integer imask(MIX,MJX)                                                     NESTLSM.54
                                                                                 NESTLSM.55
#ifdef MPP1                                                                      NESTLSM.56
#     include <mpp_nestlsm_00.incl>                                              NESTLSM.57
#endif                                                                           NESTLSM.58
                                                                                 NESTLSM.59
c Define two integer masks:                                                      NESTLSM.60
c     lmask indicates where the MOTHER land points are.                          NESTLSM.61
c     imask indicates where the MOTHER ice points are.                           NESTLSM.62
      do i = 1, il-1                                                             NESTLSM.63
         do j = 1, jl-1                                                          NESTLSM.64
            lmask(i,j) = min(1,nint(abs(satbrt(i,j)-iswater)))                   NESTLSM.65
            imask(i,j) = nint(xice(i,j))                                         NESTLSM.66
         enddo                                                                   NESTLSM.67
      enddo                                                                      NESTLSM.68
                                                                                 NESTLSM.69
#ifdef MPP1                                                                      NESTLSM.70
#     include <mpp_nestlsm_10.incl>                                              NESTLSM.71
#endif                                                                           NESTLSM.72
                                                                                 NESTLSM.73
      do j = 1, jnl-1                                                            NESTLSM.74
         do i = 1, inl-1                                                         NESTLSM.75
                                                                                 NESTLSM.76
            if(satbrn(i,j) .eq. iswater) then                                    NESTLSM.77
c This NESTED point is a water point (either open water or sea[lake]-ice).       NESTLSM.78
                                                                                 NESTLSM.79
c Water points have soil moisture set to 1.0:                                    NESTLSM.80
                                                                                 NESTLSM.81
               do l = 1, MLX                                                     NESTLSM.82
                  smcan(i,j,l) = 1.0                                             NESTLSM.83
                  smcbn(i,j,l) = 1.0                                             NESTLSM.84
                  swcan(i,j,l) = 1.0                                             19DEC02.1149
                  swcbn(i,j,l) = 1.0                                             19DEC02.1150
               enddo                                                             NESTLSM.85
                                                                                 NESTLSM.86
c Water points have canopy moisture set to 0.0:                                  NESTLSM.87
                                                                                 NESTLSM.88
               cmcan(i,j) = 0.                                                   NESTLSM.89
               cmcbn(i,j) = 0.                                                   NESTLSM.90
                                                                                 19DEC02.1151
c Water points have albedos set to 8.0                                           19DEC02.1152
                                                                                 19DEC02.1153
               albedon(i,j) = 8.                                                 19DEC02.1154
               albmaxn(i,j) = 8.                                                 19DEC02.1155
               do l=1,12                                                         19DEC02.1156
                 albmonn(i,j,l) = 8.                                             19DEC02.1157
               enddo                                                             19DEC02.1158
                                                                                 NESTLSM.91
c Now, determine whether this NESTED point is ice or open water,                 NESTLSM.92
c based on the nearest MOTHER points:                                            NESTLSM.93
                                                                                 NESTLSM.94
#ifndef MPP1                                                                     NESTLSM.95
               xicen(i,j) = nearw(i, j, xice, lmask)                             NESTLSM.96
#else                                                                            NESTLSM.97
               xicen(i,j) = nearw(FLIC_L2G_M(i), FLIC_L2G_N(j),                  NESTLSM.98
     +                      xice_g, lmask_g)                                     NESTLSM.99
#endif                                                                           NESTLSM.100
                                                                                 NESTLSM.101
C  How to treat ice/open-water flag if we cannot find nearby                     NESTLSM.102
C  MOTHER water point?  Check the surrounding MOTHER soil T from                 NESTLSM.103
C  level 2, and use an arbitrary threshold.                                      NESTLSM.104
                                                                                 NESTLSM.105
               if (xicen(i,j) .lt. -1.E24) then                                  NESTLSM.106
#ifndef MPP1                                                                     NESTLSM.107
                  tmpval = stfun(i, j, 2, stca, lmask)                           NESTLSM.108
#else                                                                            NESTLSM.109
                  tmpval = stfun(FLIC_L2G_M(i), FLIC_L2G_N(j),                   NESTLSM.110
     +                           2, stca_g, lmask_g)                             NESTLSM.111
#endif                                                                           NESTLSM.112
                  if (tmpval .lt. 270) then                                      NESTLSM.113
                     xicen(i,j) = 1.                                             19DEC02.1159
                  else                                                           NESTLSM.115
                     xicen(i,j) = 0.                                             19DEC02.1160
                  endif                                                          NESTLSM.117
               endif                                                             NESTLSM.118
                                                                                 NESTLSM.119
c Now that we've decided whether this water NESTED point is ice or open          NESTLSM.120
c water, we can treat the temperature and other fields accordingly:              NESTLSM.121
                                                                                 NESTLSM.122
               if (nint(xicen(i,j)).eq.0) then                                   NESTLSM.123
                                                                                 NESTLSM.124
c This is an open-water point:                                                   NESTLSM.125
                                                                                 NESTLSM.126
c               Open water points have the snow set to 0:                        NESTLSM.127
                                                                                 NESTLSM.128
                  snoan(i,j) = 0.                                                NESTLSM.129
                  snobn(i,j) = 0.                                                NESTLSM.130
                  snowhan(i,j) = 0.                                              19DEC02.1161
                  snowhbn(i,j) = 0.                                              19DEC02.1162
                                                                                 NESTLSM.131
c               Open water points have the soil temperature set to 0 C:          19DEC02.1163
                                                                                 NESTLSM.133
                  do l = 1, MLX                                                  NESTLSM.134
                     stcan(i,j,l) = 273.16                                       19DEC02.1164
                     stcbn(i,j,l) = 273.16                                       19DEC02.1165
                  enddo                                                          NESTLSM.137
               else                                                              NESTLSM.138
                                                                                 NESTLSM.139
c This is an ice point:                                                          NESTLSM.140
                                                                                 NESTLSM.141
c                 Ice points have the TMN field set to -2 degrees Celsius        NESTLSM.142
                                                                                 NESTLSM.143
                  tmnn(i,j) = 271.16                                             NESTLSM.144
                                                                                 NESTLSM.145
c                 NEST ice points have soil temperature fields based on          NESTLSM.146
c                 the temperatures of nearby MOTHER ice points:                  NESTLSM.147
                                                                                 NESTLSM.148
                  do l = 1, MLX                                                  NESTLSM.149
#ifndef MPP1                                                                     NESTLSM.150
                     stcan(i,j,l) = stfun(i, j, l, stca, imask)                  NESTLSM.151
                     stcbn(i,j,l) = stfun(i, j, l, stcb, imask)                  NESTLSM.152
#else                                                                            NESTLSM.153
                     stcan(i,j,l) = stfun(FLIC_L2G_M(i), FLIC_L2G_N(j),          NESTLSM.154
     +                                    l, stca_g, imask_g)                    NESTLSM.155
                     stcbn(i,j,l) = stfun(FLIC_L2G_M(i), FLIC_L2G_N(j),          NESTLSM.156
     +                                    l, stcb_g, imask_g)                    NESTLSM.157
#endif                                                                           NESTLSM.158
                                                                                 NESTLSM.159
C  How to treat SOIL T over ice if we cannot find nearby ice points?             NESTLSM.160
C  Somewhate arbitrary values:                                                   NESTLSM.161
                     if (stcan(i,j,l) .lt. -1.E24) then                          NESTLSM.162
                        if (l.le.2) then                                         NESTLSM.163
                           stcan(i,j,l) = 262.                                   NESTLSM.164
                        else                                                     NESTLSM.165
                           stcan(i,j,l) = 269.                                   NESTLSM.166
                        endif                                                    NESTLSM.167
                     endif                                                       NESTLSM.168
                     if (stcbn(i,j,l) .lt. -1.E24) then                          NESTLSM.169
                        if (l.le.2) then                                         NESTLSM.170
                           stcbn(i,j,l) = 262.                                   NESTLSM.171
                        else                                                     NESTLSM.172
                           stcbn(i,j,l) = 269.                                   NESTLSM.173
                        endif                                                    NESTLSM.174
                     endif                                                       NESTLSM.175
                  enddo                                                          NESTLSM.176
               endif                                                             NESTLSM.177
                                                                                 NESTLSM.178
            else                                                                 NESTLSM.179
c This NEST point is land point:                                                 NESTLSM.180
                                                                                 NESTLSM.181
c              Land points are not ice points:                                   NESTLSM.182
               xicen(i,j) = 0.                                                   NESTLSM.183
                                                                                 NESTLSM.184
c              For NEST land points, the canopy water, soil temperature,         NESTLSM.185
c              soil moisture, and albedo are determined from the values          19DEC02.1166
c              of nearby MOTHER land points:                                     19DEC02.1167
                                                                                 NESTLSM.188
#ifndef MPP1                                                                     NESTLSM.189
               cmcan(i,j) = stfun(i, j, 1, cmca, lmask)                          NESTLSM.190
               cmcbn(i,j) = stfun(i, j, 1, cmcb, lmask)                          NESTLSM.191
               snoan(i,j) = stfun(i, j, 1, snoa, lmask)                          19DEC02.1168
               snobn(i,j) = stfun(i, j, 1, snob, lmask)                          19DEC02.1169
               snowhan(i,j) = stfun(i, j, 1, snowha, lmask)                      19DEC02.1170
               snowhbn(i,j) = stfun(i, j, 1, snowhb, lmask)                      19DEC02.1171
               albedon(i,j) = stfun(i, j, 1, albedo, lmask)                      19DEC02.1172
               albmaxn(i,j) = stfun(i, j, 1, albmax, lmask)                      19DEC02.1173
#else                                                                            NESTLSM.192
               cmcan(i,j) = stfun(FLIC_L2G_M(i), FLIC_L2G_N(j),                  NESTLSM.193
     +                            1, cmca_g, lmask_g)                            NESTLSM.194
               cmcbn(i,j) = stfun(FLIC_L2G_M(i), FLIC_L2G_N(j),                  NESTLSM.195
     +                            1, cmcb_g, lmask_g)                            NESTLSM.196
               snoan(i,j) = stfun(FLIC_L2G_M(i), FLIC_L2G_N(j),                  19DEC02.1174
     +                            1, snoa_g, lmask_g)                            19DEC02.1175
               snobn(i,j) = stfun(FLIC_L2G_M(i), FLIC_L2G_N(j),                  19DEC02.1176
     +                            1, snob_g, lmask_g)                            19DEC02.1177
               snowhan(i,j) = stfun(FLIC_L2G_M(i), FLIC_L2G_N(j),                19DEC02.1178
     +                            1, snowha_g, lmask_g)                          19DEC02.1179
               snowhbn(i,j) = stfun(FLIC_L2G_M(i), FLIC_L2G_N(j),                19DEC02.1180
     +                            1, snowhb_g, lmask_g)                          19DEC02.1181
               albedon(i,j) = stfun(FLIC_L2G_M(i), FLIC_L2G_N(j),                19DEC02.1182
     +                            1, albedo_g, lmask_g)                          19DEC02.1183
               albmaxn(i,j) = stfun(FLIC_L2G_M(i), FLIC_L2G_N(j),                19DEC02.1184
     +                            1, albmax_g, lmask_g)                          19DEC02.1185
#endif                                                                           NESTLSM.197
               if (cmcan(i,j) .lt. -1.E24) cmcan(i,j) = 0                        NESTLSM.198
               if (cmcbn(i,j) .lt. -1.E24) cmcbn(i,j) = 0                        NESTLSM.199
               if (snoan(i,j) .lt. -1.E24) snoan(i,j) = 0                        19DEC02.1186
               if (snobn(i,j) .lt. -1.E24) snobn(i,j) = 0                        19DEC02.1187
               if (snowhan(i,j) .lt. -1.E24) snowhan(i,j) = 0                    19DEC02.1188
               if (snowhbn(i,j) .lt. -1.E24) snowhbn(i,j) = 0                    19DEC02.1189
               if (albedon(i,j) .lt. -1.E24) albedon(i,j) = 16.                  19DEC02.1190
               if (albmaxn(i,j) .lt. -1.E24) albmaxn(i,j) = 65.                  19DEC02.1191
                                                                                 NESTLSM.200
               do l = 1, MLX                                                     NESTLSM.201
C                 Masked interpolation of soil T, with climatological            NESTLSM.202
c                 soil T if we cannot find nearby land points:                   NESTLSM.203
#ifndef MPP1                                                                     NESTLSM.204
                  stcan(i,j,l) = stfun(i, j, l, stca, lmask)                     NESTLSM.205
                  stcbn(i,j,l) = stfun(i, j, l, stcb, lmask)                     NESTLSM.206
#else                                                                            NESTLSM.207
                  stcan(i,j,l) = stfun(FLIC_L2G_M(i), FLIC_L2G_N(j),             NESTLSM.208
     +                                 l, stca_g, lmask_g)                       NESTLSM.209
                  stcbn(i,j,l) = stfun(FLIC_L2G_M(i), FLIC_L2G_N(j),             NESTLSM.210
     +                                 l, stcb_g, lmask_g)                       NESTLSM.211
#endif                                                                           NESTLSM.212
                                                                                 NESTLSM.213
                                                                                 NESTLSM.214
                  if (stcan(i,j,l) .lt. -1.E24)                                  NESTLSM.215
     &                 stcan(i,j,l) = missing_soil(xlatn(i,j))                   NESTLSM.216
                  if (stcbn(i,j,l) .lt. -1.E24)                                  NESTLSM.217
     &                 stcbn(i,j,l) = missing_soil(xlatn(i,j))                   NESTLSM.218
                                                                                 NESTLSM.219
                                                                                 NESTLSM.220
C                 Masked interpolation of soil moisture:                         NESTLSM.221
#ifndef MPP1                                                                     NESTLSM.222
                  smcan(i,j,l) = stfun(i, j, l, smca, lmask)                     NESTLSM.223
                  smcbn(i,j,l) = stfun(i, j, l, smcb, lmask)                     NESTLSM.224
#else                                                                            NESTLSM.225
                  smcan(i,j,l) = stfun(FLIC_L2G_M(i), FLIC_L2G_N(j),             NESTLSM.226
     +                                 l, smca_g, lmask_g)                       NESTLSM.227
                  smcbn(i,j,l) = stfun(FLIC_L2G_M(i), FLIC_L2G_N(j),             NESTLSM.228
     +                                 l, smcb_g, lmask_g)                       NESTLSM.229
#endif                                                                           NESTLSM.230
                  if (smcan(i,j,l) .lt. -1.E24) smcan(i,j,l) = 0.25              NESTLSM.231
                  if (smcbn(i,j,l) .lt. -1.E24) smcbn(i,j,l) = 0.25              NESTLSM.232
                                                                                 19DEC02.1192
C                                                                                19DEC02.1193
C                 Masked interpolation of soil liquid water                      19DEC02.1194
#ifndef MPP1                                                                     19DEC02.1195
                  swcan(i,j,l) = stfun(i, j, l, swca, lmask)                     19DEC02.1196
                  swcbn(i,j,l) = stfun(i, j, l, swcb, lmask)                     19DEC02.1197
#else                                                                            19DEC02.1198
                  swcan(i,j,l) = stfun(FLIC_L2G_M(i), FLIC_L2G_N(j),             19DEC02.1199
     +                                 l, swca_g, lmask_g)                       19DEC02.1200
                  swcbn(i,j,l) = stfun(FLIC_L2G_M(i), FLIC_L2G_N(j),             19DEC02.1201
     +                                 l, swcb_g, lmask_g)                       19DEC02.1202
#endif                                                                           19DEC02.1203
                  if (swcan(i,j,l) .lt. -1.E24) swcan(i,j,l) = 0.25              19DEC02.1204
                  if (swcbn(i,j,l) .lt. -1.E24) swcbn(i,j,l) = 0.25              19DEC02.1205
                                                                                 NESTLSM.233
               enddo                                                             NESTLSM.234
c               Masked interpolation of monthly albedo                           19DEC02.1206
               do l = 1, 12                                                      19DEC02.1207
#ifndef MPP1                                                                     19DEC02.1208
                 albmonn(i,j,l) = stfun(i, j, 1, albmon(1,1,l), lmask)           19DEC02.1209
#else                                                                            19DEC02.1210
                 albmonn(i,j,l) = stfun(FLIC_L2G_M(i), FLIC_L2G_N(j),            19DEC02.1211
     +                                 1, albmon_g(1,1,l), lmask_g)              19DEC02.1212
#endif                                                                           19DEC02.1213
                 if (albmonn(i,j,l) .lt. -1.E24) albmonn(i,j,l) = 16.            19DEC02.1214
               enddo                                                             19DEC02.1215
                                                                                 NESTLSM.235
            endif                                                                NESTLSM.236
         enddo                                                                   NESTLSM.237
      enddo                                                                      NESTLSM.238
                                                                                 NESTLSM.239
C                                                                                NESTLSM.240
#ifndef MPP1                                                                     NESTLSM.241
C                                                                                NESTLSM.242
C Now adjust the interpolated Soil T to the nested terrain:                      NESTLSM.243
C                                                                                NESTLSM.244
c     First, get the interpolated value of T0 on this nest:                      NESTLSM.245
      do j = 1, jl                                                               NESTLSM.246
         do i = 1, il                                                            NESTLSM.247
            help1(i,j) = T0(i,j,kl)                                              NESTLSM.248
         enddo                                                                   NESTLSM.249
      enddo                                                                      NESTLSM.250
      call exaint(help1, il, jl, help1n, inl, jnl, isouth, jwest, 1)             NESTLSM.251
#else                                                                            NESTLSM.252
#  include <mpp_nestlsm_20.incl>                                                 NESTLSM.253
#endif                                                                           NESTLSM.254
                                                                                 NESTLSM.255
c     Now, apply the difference between the interpolated value and               NESTLSM.256
c     the "real" value to the soil temperature fields:                           NESTLSM.257
                                                                                 NESTLSM.258
      do j = 1, jnl-1                                                            NESTLSM.259
         do i = 1, inl-1                                                         NESTLSM.260
            do l = 1, mlx                                                        NESTLSM.261
               stcan(i,j,l) = stcan(i,j,l) + (t0n(i,j,kl) - help1n(i,j))         NESTLSM.262
               stcbn(i,j,l) = stcbn(i,j,l) + (t0n(i,j,kl) - help1n(i,j))         NESTLSM.263
            enddo                                                                NESTLSM.264
         enddo                                                                   NESTLSM.265
      enddo                                                                      NESTLSM.266
                                                                                 NESTLSM.267
      end                                                                        NESTLSM.268
                                                                                 NESTLSM.269
      real function missing_soil(xlat)                                           NESTLSM.270
                                                                                 NESTLSM.271
c  Use latitude to define soil temperature for missing values.                   NESTLSM.272
c  This routine and coefficients were obtained from Yong-Run Guo, Sep 1999.      NESTLSM.273
c  This routine was swiped directly from NESTDOWN, Mar 2000                      NESTLSM.274
                                                                                 NESTLSM.275
      implicit none                                                              NESTLSM.276
                                                                                 NESTLSM.277
      real xlat, angle                                                           NESTLSM.278
                                                                                 NESTLSM.279
      real c0, c1, c2                                                            NESTLSM.280
      parameter (c0=0.24206e+03)                                                 NESTLSM.281
      parameter (c1=0.59736e+02)                                                 NESTLSM.282
      parameter (c2=0.19445e+01)                                                 NESTLSM.283
                                                                                 NESTLSM.284
      angle = 0.5*3.1415926*((89.5-xlat)/89.5)                                   NESTLSM.285
      missing_soil = c0 + c1*sin(angle) + c2*cos(angle)                          NESTLSM.286
                                                                                 NESTLSM.287
      end                                                                        NESTLSM.288
                                                                                 NESTLSM.289
      real function stfun(ival, jval, lval, mthr, mask)                          NESTLSM.290
C Return a value of -1.E25 if a good value cannot be constructed.                NESTLSM.291
      implicit none                                                              NESTLSM.292
      integer ival, jval, lval                                                   NESTLSM.293
#     include <parame.incl>                                                      NESTLSM.294
#     include <point2d.incl>                                                     NESTLSM.295
#     include <landuse.incl>                                                     NESTLSM.296
#     include <nestl.incl>                                                       NESTLSM.297
#     include <soilp.incl>                                                       NESTLSM.298
                                                                                 NESTLSM.299
#ifndef MPP1                                                                     NESTLSM.300
      real mthr(MIX,MJX,MLX)                                                     NESTLSM.301
      integer mask(MIX,MJX)                                                      NESTLSM.302
#else                                                                            NESTLSM.303
      real mthr(mix_g,mjx_g,MLX)                                                 NESTLSM.304
      integer mask(mix_g,mjx_g)                                                  NESTLSM.305
#endif                                                                           NESTLSM.306
                                                                                 NESTLSM.307
      integer ils1(17), jls1(17), ils2(18), jls2(18)                             NESTLSM.308
      data ils1 /0,0,1,-1,1,-1,0,0,1,-1,1,-1,2,-2,2,-2,0/                        NESTLSM.309
      data jls1 /0,1,0,0,1,1,-1,2,-1,-1,2,2,0,0,1,1,-2/                          NESTLSM.310
      data ils2 /0,1,0,1,0,-1,1,-1,2,0,2,1,-1,2,-1,2,0,-2/                       NESTLSM.311
      data jls2 /0,0,1,1,-1,0,-1,1,0,2,1,2,-1,-1,2,2,-2,0/                       NESTLSM.312
                                                                                 NESTLSM.313
      integer ii, jj, k, lcount, icount, iii, jjj                                NESTLSM.314
      integer ioffs, joffs, itest, jtest                                         NESTLSM.315
      real ycoord, xcoord                                                        NESTLSM.316
      real rx, ry, ddx, ddy                                                      NESTLSM.317
                                                                                 NESTLSM.318
C_FLIC_BEGIN_NOFLIC                                                              NESTLSM.319
                                                                                 NESTLSM.320
      ycoord = float(isouth-1) + float(ival+1)/3.                                NESTLSM.321
      xcoord = float(jwest -1) + float(jval+1)/3.                                NESTLSM.322
                                                                                 NESTLSM.323
      ii = ycoord+1.E-5                                                          NESTLSM.324
      jj = xcoord+1.E-5                                                          NESTLSM.325
                                                                                 NESTLSM.326
C LCOUNT here is the number of valid points.                                     NESTLSM.327
      lcount = mask(ii  ,jj  ) + mask(ii+1,jj  ) +                               NESTLSM.328
     &         mask(ii  ,jj+1) + mask(ii+1,jj+1)                                 NESTLSM.329
                                                                                 NESTLSM.330
      if (lcount .eq.4) then                                                     NESTLSM.331
         ddx = (xcoord-jj)                                                       NESTLSM.332
         ddy = (ycoord-ii)                                                       NESTLSM.333
         rx = 1.0 - ddx                                                          NESTLSM.334
         ry = 1.0 - ddy                                                          NESTLSM.335
                                                                                 NESTLSM.336
         stfun = mthr(ii  ,jj  ,lval)*RX*RY +                                    NESTLSM.337
     &           mthr(ii+1,jj  ,lval)*RX*DDY +                                   NESTLSM.338
     &           mthr(ii  ,jj+1,lval)*DDX*RY +                                   NESTLSM.339
     &           mthr(ii+1,jj+1,lval)*DDX*DDY                                    NESTLSM.340
      else if (lcount .eq. 0) then                                               NESTLSM.341
C        Do an average of nearby valid points:                                   NESTLSM.342
                                                                                 NESTLSM.343
         itest = isouth + (ival-1)/3                                             NESTLSM.344
         jtest = jwest + (jval-1)/3                                              NESTLSM.345
         ioffs = mod(ival+2,3) - 1                                               NESTLSM.346
         joffs = mod(jval+2,3) - 1                                               NESTLSM.347
                                                                                 NESTLSM.348
         if (abs(ioffs).eq.1 .and. abs(joffs).eq.1) then                         NESTLSM.349
            stfun = 0.                                                           NESTLSM.350
            icount = 0                                                           NESTLSM.351
            do k = 1, 18                                                         NESTLSM.352
               ii = itest + ils2(k) * ioffs                                      NESTLSM.353
               jj = jtest + jls2(k) * joffs                                      NESTLSM.354
               if (mask(ii,jj) .eq. 1) then                                      NESTLSM.355
                  stfun = stfun + mthr(ii,jj,lval)                               NESTLSM.356
                  icount = icount + 1                                            NESTLSM.357
               endif                                                             NESTLSM.358
            enddo                                                                NESTLSM.359
            if (icount .eq. 0) then                                              NESTLSM.360
               stfun = -1.E25                                                    NESTLSM.361
            else                                                                 NESTLSM.362
               stfun = stfun / float(icount)                                     NESTLSM.363
            endif                                                                NESTLSM.364
         else if (ioffs .eq. 0) then                                             NESTLSM.365
            stfun = 0.                                                           NESTLSM.366
            icount = 0                                                           NESTLSM.367
            do k = 1, 17                                                         NESTLSM.368
               ii = itest+ils1(k)                                                NESTLSM.369
               jj = jtest+jls1(k)*joffs                                          NESTLSM.370
               if (mask(ii,jj) .eq. 1) then                                      NESTLSM.371
                  stfun = stfun + mthr(ii,jj,lval)                               NESTLSM.372
                  icount = icount + 1                                            NESTLSM.373
               endif                                                             NESTLSM.374
            enddo                                                                NESTLSM.375
            if (icount .eq. 0) then                                              NESTLSM.376
               stfun = -1.E25                                                    NESTLSM.377
            else                                                                 NESTLSM.378
               stfun = stfun / float(icount)                                     NESTLSM.379
            endif                                                                NESTLSM.380
         elseif (joffs .eq. 0) then                                              NESTLSM.381
            stfun = 0.                                                           NESTLSM.382
            icount = 0                                                           NESTLSM.383
            do k = 1, 17                                                         NESTLSM.384
               ii = itest+jls1(k)*ioffs                                          NESTLSM.385
               jj = jtest+ils1(k)                                                NESTLSM.386
               if (mask(ii,jj) .eq. 1) then                                      NESTLSM.387
                  stfun = stfun + mthr(ii,jj,lval)                               NESTLSM.388
                  icount = icount + 1                                            NESTLSM.389
               endif                                                             NESTLSM.390
            enddo                                                                NESTLSM.391
            if (icount .eq. 0) then                                              NESTLSM.392
               stfun = -1.E25                                                    NESTLSM.393
            else                                                                 NESTLSM.394
               stfun = stfun / float(icount)                                     NESTLSM.395
            endif                                                                NESTLSM.396
         endif                                                                   NESTLSM.397
                                                                                 NESTLSM.398
      else                                                                       NESTLSM.399
C        (lcount == 1, 2, or 3.)                                                 NESTLSM.400
C        Do an average of valid points of the four nearby points:                NESTLSM.401
         stfun = 0.                                                              NESTLSM.402
                                                                                 NESTLSM.403
         do iii = 0,1                                                            NESTLSM.404
            do jjj = 0,1                                                         NESTLSM.405
                                                                                 NESTLSM.406
               if (mask(ii+iii,jj+jjj) .eq. 1) then                              NESTLSM.407
                  stfun=                                                         NESTLSM.408
     &                 stfun+(mthr(ii+iii,jj+jjj,lval)/float(lcount))            NESTLSM.409
                                                                                 NESTLSM.410
               endif                                                             NESTLSM.411
            enddo                                                                NESTLSM.412
         enddo                                                                   NESTLSM.413
                                                                                 NESTLSM.414
      endif                                                                      NESTLSM.415
                                                                                 NESTLSM.416
C_FLIC_END_NOFLIC                                                                NESTLSM.417
      return                                                                     NESTLSM.418
      end                                                                        NESTLSM.419
                                                                                 NESTLSM.420
      real function nearw(ival, jval, xice, mask)                                NESTLSM.421
c     Assign the ice value based on a four-point interpolation, if the           NESTLSM.422
c     surrounding four points are indeed water or ice.  If not, assign           NESTLSM.423
c     the ice value of the nearest water point to the water point in             NESTLSM.424
c     question.                                                                  NESTLSM.425
      implicit none                                                              NESTLSM.426
#     include <parame.incl>                                                      NESTLSM.427
#     include <point2d.incl>                                                     NESTLSM.428
#     include <landuse.incl>                                                     NESTLSM.429
#     include <nestl.incl>                                                       NESTLSM.430
                                                                                 NESTLSM.431
#ifndef MPP1                                                                     NESTLSM.432
      real xice(MIX,MJX)                                                         NESTLSM.433
      integer mask(MIX,MJX)                                                      NESTLSM.434
#else                                                                            NESTLSM.435
      real xice(mix_g,mjx_g)                                                     NESTLSM.436
      integer mask(mix_g,mjx_g)                                                  NESTLSM.437
#endif                                                                           NESTLSM.438
                                                                                 NESTLSM.439
      integer ival, jval                                                         NESTLSM.440
      integer ii, jj                                                             NESTLSM.441
c                                                                                NESTLSM.442
      real rx, ry, ddx, ddy, xcoord, ycoord                                      NESTLSM.443
      integer ioffs, joffs, k, itest, jtest, lcount                              NESTLSM.444
      integer ils0(21), jls0(21), ils1(17), jls1(17), ils2(18), jls2(18)         NESTLSM.445
      data ils0 /0,1,0,0,-1,1,1,-1,-1,2,0,0,-2,2,2,1,1,-1,-1,-2,-2/              NESTLSM.446
      data jls0 /0,0,-1,1,0,-1,1,-1,1,0,-2,2,0,-1,1,-2,2,-2,2,-1,1/              NESTLSM.447
      data ils1 /0,0,1,-1,1,-1,0,0,1,-1,1,-1,2,-2,2,-2,0/                        NESTLSM.448
      data jls1 /0,1,0,0,1,1,-1,2,-1,-1,2,2,0,0,1,1,-2/                          NESTLSM.449
      data ils2 /0,1,0,1,0,-1,1,-1,2,0,2,1,-1,2,-1,2,0,-2/                       NESTLSM.450
      data jls2 /0,0,1,1,-1,0,-1,1,0,2,1,2,-1,-1,2,2,-2,0/                       NESTLSM.451
                                                                                 NESTLSM.452
C_FLIC_BEGIN_NOFLIC                                                              NESTLSM.453
                                                                                 NESTLSM.454
      ycoord = float(isouth-1) + float(ival+1)/3.                                NESTLSM.455
      xcoord = float(jwest -1) + float(jval+1)/3.                                NESTLSM.456
                                                                                 NESTLSM.457
      ii = ycoord+1.E-5                                                          NESTLSM.458
      jj = xcoord+1.E-5                                                          NESTLSM.459
                                                                                 NESTLSM.460
c     LCOUNT here is the number of valid points.                                 NESTLSM.461
      lcount = mask(ii  ,jj  ) + mask(ii+1,jj  ) +                               NESTLSM.462
     &     mask(ii  ,jj+1) + mask(ii+1,jj+1)                                     NESTLSM.463
                                                                                 NESTLSM.464
      if (lcount .eq.0) then                                                     NESTLSM.465
         ddx = (xcoord-jj)                                                       NESTLSM.466
         ddy = (ycoord-ii)                                                       NESTLSM.467
         rx = 1.0 - ddx                                                          NESTLSM.468
         ry = 1.0 - ddy                                                          NESTLSM.469
                                                                                 NESTLSM.470
         nearw = xice(ii  ,jj  )*RX*RY +                                         NESTLSM.471
     &        xice(ii+1,jj  )*RX*DDY +                                           NESTLSM.472
     &        xice(ii  ,jj+1)*DDX*RY +                                           NESTLSM.473
     &        xice(ii+1,jj+1)*DDX*DDY                                            NESTLSM.474
         nearw = float(nint(nearw))                                              NESTLSM.475
                                                                                 NESTLSM.476
      else                                                                       NESTLSM.477
                                                                                 NESTLSM.478
         ioffs = mod(ival+2,3) - 1                                               NESTLSM.479
         joffs = mod(jval+2,3) - 1                                               NESTLSM.480
         itest = isouth + (ival-1)/3                                             NESTLSM.481
         jtest = jwest + (jval-1)/3                                              NESTLSM.482
                                                                                 NESTLSM.483
         if ((ioffs .eq. 0) .and. (joffs .eq. 0)) then                           NESTLSM.484
            do k = 1, 21                                                         NESTLSM.485
               ii = itest + ils0(k)                                              NESTLSM.486
               jj = jtest + jls0(k)                                              NESTLSM.487
               if (mask(ii,jj) .eq. 0) then                                      NESTLSM.488
                  nearw = xice(ii,jj)                                            NESTLSM.489
                  return                                                         NESTLSM.490
               endif                                                             NESTLSM.491
            enddo                                                                NESTLSM.492
                                                                                 NESTLSM.493
         else if (abs(ioffs).eq.1 .and. abs(joffs).eq.1) then                    NESTLSM.494
            do k = 1, 18                                                         NESTLSM.495
               ii = itest + ils2(k) * ioffs                                      NESTLSM.496
               jj = jtest + jls2(k) * joffs                                      NESTLSM.497
                                                                                 NESTLSM.498
               if (mask(ii,jj) .eq. 0) then                                      NESTLSM.499
                  nearw = xice(ii,jj)                                            NESTLSM.500
                  return                                                         NESTLSM.501
               endif                                                             NESTLSM.502
            enddo                                                                NESTLSM.503
                                                                                 NESTLSM.504
         else if (ioffs .eq. 0) then                                             NESTLSM.505
            do k = 1, 17                                                         NESTLSM.506
               ii = itest+ils1(k)                                                NESTLSM.507
               jj = jtest+jls1(k)*joffs                                          NESTLSM.508
               if (mask(ii,jj) .eq. 0) then                                      NESTLSM.509
                  nearw = xice(ii,jj)                                            NESTLSM.510
                  return                                                         NESTLSM.511
               endif                                                             NESTLSM.512
            enddo                                                                NESTLSM.513
                                                                                 NESTLSM.514
         elseif (joffs .eq. 0) then                                              NESTLSM.515
            do k = 1, 17                                                         NESTLSM.516
               ii = itest+jls1(k)*ioffs                                          NESTLSM.517
               jj = jtest+ils1(k)                                                NESTLSM.518
                                                                                 NESTLSM.519
               if (mask(ii,jj) .eq. 0) then                                      NESTLSM.520
                  nearw = xice(ii,jj)                                            NESTLSM.521
                  return                                                         NESTLSM.522
               endif                                                             NESTLSM.523
            enddo                                                                NESTLSM.524
         endif                                                                   NESTLSM.525
                                                                                 NESTLSM.526
         nearw = -1.E25                                                          NESTLSM.527
      endif                                                                      NESTLSM.528
                                                                                 NESTLSM.529
C_FLIC_END_NOFLIC                                                                NESTLSM.530
      return                                                                     NESTLSM.531
      end                                                                        NESTLSM.532
