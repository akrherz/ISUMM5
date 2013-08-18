      SUBROUTINE ADDALL                                                          ADDALL.1
#include <parame.incl>                                                           ADDALL.2
#include <addr0.incl>                                                            ADDALL.3
#if defined (DEC_ALPHA) || defined (crayx1) 
      INTEGER*8 ISAVE,ISAVEI,ISAVER,ISAVEIF,ISAVERF,ISAVEIOF                     ADDALL.5
#endif                                                                           ADDALL.6
      IX3D=MIX*MJX*MKX                                                           ADDALL.7
      IXM3D=MIXM*MJXM*MKXM                                                       ADDALL.8
      IJX2D=MIX*MJX                                                              ADDALL.9
      IX2D=MIX*MKX                                                               ADDALL.10
      JX2D=MJX*MKX                                                               ADDALL.11
      IX2DND=MIX*MKX*NSPGD                                                       ADDALL.12
      JX2DND=MJX*MKX*NSPGD                                                       ADDALL.13
      IX2DNX=MIX*MKX*NSPGX                                                       ADDALL.14
      JX2DNX=MJX*MKX*NSPGX                                                       ADDALL.15
      IX2DMX=MIXM*MKXM*NSPGX                                                     ADDALL.16
      JX2DMX=MJXM*MKXM*NSPGX                                                     ADDALL.17
      IX1DNX=MIX*NSPGX                                                           ADDALL.18
      JX1DNX=MJX*NSPGX                                                           ADDALL.19
      IX3DFDG=MIXFG*MJXFG*MKXFG                                                  ADDALL.20
      IX2DFDG=MIXFG*MJXFG                                                        ADDALL.21
      IX3DFDO=MIXFO*MJXFO*MKXFO                                                  ADDALL.22
      IX2DFDO=MIXFO*MJXFO                                                        ADDALL.23
      DO 100 K=1,MAXNES                                                          ADDALL.24
        IAXALL(1,K)=LOC(ALLARR(1,K))                                             ADDALL.25
        ISAVEI=LOC(INTALL(1,K))                                                  ADDALL.26
        NCOUNT=1                                                                 ADDALL.27
        NCOU=1                                                                   ADDALL.28
        NICOU=1                                                                  ADDALL.29
        NGCOU=1                                                                  ADDALL.30
        NIGCOU=1                                                                 ADDALL.31
        NOCOU=1                                                                  ADDALL.32
C                                                                                ADDALL.33
C--- 3-D ARRAYS                                                                  ADDALL.34
C                                                                                ADDALL.35
        DO 15 N=2,NVARX+1                                                        ADDALL.36
          NCOUNT=NCOUNT+1                                                        ADDALL.37
          IAXALL(NCOUNT,K)=LOC(ALLARR(1+(N-1)*IX3D,K))                           ADDALL.38
   15   CONTINUE                                                                 ADDALL.39
        NCOU=NCOU+(NVARX)*IX3D                                                   ADDALL.40
C                                                                                ADDALL.41
C--- 3-D CLOUD/RAINWATER ARRAYS                                                  ADDALL.42
C                                                                                ADDALL.43
        DO 20 N=2,NVARMX+1                                                       ADDALL.44
          NCOUNT=NCOUNT+1                                                        ADDALL.45
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*IXM3D,K))                       ADDALL.46
   20   CONTINUE                                                                 ADDALL.47
        NCOU=NCOU+(NVARMX)*IXM3D                                                 ADDALL.48
C                                                                                ADDALL.49
C--- 3-D ICE ARRAYS                                                              ADDALL.50
C                                                                                ADDALL.51
        DO 22 N=2,5                                                              ADDALL.52
          NCOUNT=NCOUNT+1                                                        ADDALL.53
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*MIXIC*MJXIC*MKXIC,K))           ADDALL.54
   22   CONTINUE                                                                 ADDALL.55
        NCOU=NCOU+4*MIXIC*MJXIC*MKXIC                                            ADDALL.56
        DO 23 N=2,5                                                              ADDALL.57
          NCOUNT=NCOUNT+1                                                        ADDALL.58
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*MIXIG*MJXIG*MKXIG,K))           ADDALL.59
   23   CONTINUE                                                                 ADDALL.60
        NCOU=NCOU+2*2*MIXIG*MJXIG*MKXIG                                          ADDALL.61
C                                                                                ADDALL.62
C--- 2-D I,J ARRAYS                                                              ADDALL.63
C                                                                                ADDALL.64
        DO 25 N=2,NVARSX+1                                                       ADDALL.65
          NCOUNT=NCOUNT+1                                                        ADDALL.66
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*IJX2D,K))                       ADDALL.67
   25   CONTINUE                                                                 ADDALL.68
        NCOU=NCOU+(NVARSX)*IJX2D                                                 ADDALL.69
C                                                                                ADDALL.70
C--- BOUNDARY CONDITIONS, FIRST DOT POINTS WEST AND EAST                         ADDALL.71
C                                                                                ADDALL.72
        DO 30 N=2,9                                                              ADDALL.73
          NCOUNT=NCOUNT+1                                                        ADDALL.74
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*IX2DND,K))                      ADDALL.75
   30   CONTINUE                                                                 ADDALL.76
        NCOU=NCOU+8*IX2DND                                                       ADDALL.77
C                                                                                ADDALL.78
C--- CROSS POINTS WEST AND EAST(FOR T,QV)                                        ADDALL.79
C                                                                                ADDALL.80
        DO 35 N=2,9                                                              ADDALL.81
          NCOUNT=NCOUNT+1                                                        ADDALL.82
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*IX2DNX,K))                      ADDALL.83
   35   CONTINUE                                                                 ADDALL.84
        NCOU=NCOU+8*IX2DNX                                                       ADDALL.85
C                                                                                ADDALL.86
C--- CROSS POINTS WEST AND EAST(FOR QC)                                          ADDALL.87
C                                                                                ADDALL.88
        DO 37 N=2,9                                                              ADDALL.89
          NCOUNT=NCOUNT+1                                                        ADDALL.90
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*IX2DMX,K))                      ADDALL.91
   37   CONTINUE                                                                 ADDALL.92
        NCOU=NCOU+8*IX2DMX                                                       ADDALL.93
C                                                                                ADDALL.94
C--- CROSS POINTS WEST AND EAST(FOR QI)                                          ADDALL.95
C                                                                                ADDALL.96
        DO 38 N=2,9                                                              ADDALL.97
          NCOUNT=NCOUNT+1                                                        ADDALL.98
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*MIXIC*MKXIC*NSPGX,K))           ADDALL.99
   38   CONTINUE                                                                 ADDALL.100
        NCOU=NCOU+8*MIXIC*MKXIC*NSPGX                                            ADDALL.101
        DO 39 N=2,9                                                              ADDALL.102
          NCOUNT=NCOUNT+1                                                        ADDALL.103
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*MIXIG*MKXIG*NSPGX,K))           ADDALL.104
   39   CONTINUE                                                                 ADDALL.105
        NCOU=NCOU+4*2*MIXIG*MKXIG*NSPGX                                          ADDALL.106
C                                                                                ADDALL.107
C--- DOT NORTH AND SOUTH                                                         ADDALL.108
C                                                                                ADDALL.109
        DO 40 N=2,9                                                              ADDALL.110
          NCOUNT=NCOUNT+1                                                        ADDALL.111
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*JX2DND,K))                      ADDALL.112
   40   CONTINUE                                                                 ADDALL.113
        NCOU=NCOU+8*JX2DND                                                       ADDALL.114
C                                                                                ADDALL.115
C--- CROSS NORTH AND SOUTH(T,QV)                                                 ADDALL.116
C                                                                                ADDALL.117
        DO 45 N=2,9                                                              ADDALL.118
          NCOUNT=NCOUNT+1                                                        ADDALL.119
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*JX2DNX,K))                      ADDALL.120
   45   CONTINUE                                                                 ADDALL.121
        NCOU=NCOU+8*JX2DNX                                                       ADDALL.122
C                                                                                ADDALL.123
C--- CROSS NORTH AND SOUTH(QC,QR)                                                ADDALL.124
C                                                                                ADDALL.125
        DO 47 N=2,9                                                              ADDALL.126
          NCOUNT=NCOUNT+1                                                        ADDALL.127
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*JX2DMX,K))                      ADDALL.128
   47   CONTINUE                                                                 ADDALL.129
        NCOU=NCOU+8*JX2DMX                                                       ADDALL.130
C                                                                                ADDALL.131
C--- CROSS NORTH AND SOUTH(QI)                                                   ADDALL.132
C                                                                                ADDALL.133
        DO 48 N=2,9                                                              ADDALL.134
          NCOUNT=NCOUNT+1                                                        ADDALL.135
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*MJXIC*MKXIC*NSPGX,K))           ADDALL.136
   48   CONTINUE                                                                 ADDALL.137
        NCOU=NCOU+8*MJXIC*MKXIC*NSPGX                                            ADDALL.138
        DO 49 N=2,9                                                              ADDALL.139
          NCOUNT=NCOUNT+1                                                        ADDALL.140
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*MJXIG*MKXIG*NSPGX,K))           ADDALL.141
   49   CONTINUE                                                                 ADDALL.142
        NCOU=NCOU+4*2*MJXIG*MKXIG*NSPGX                                          ADDALL.143
C                                                                                ADDALL.144
C--- PRESSURE VARIABLES                                                          ADDALL.145
C                                                                                ADDALL.146
        DO 50 N=2,5                                                              ADDALL.147
          NCOUNT=NCOUNT+1                                                        ADDALL.148
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*IX1DNX,K))                      ADDALL.149
   50   CONTINUE                                                                 ADDALL.150
        NCOU=NCOU+4*IX1DNX                                                       ADDALL.151
        DO 55 N=2,5                                                              ADDALL.152
          NCOUNT=NCOUNT+1                                                        ADDALL.153
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*JX1DNX,K))                      ADDALL.154
   55   CONTINUE                                                                 ADDALL.155
        NCOU=NCOU+4*JX1DNX                                                       ADDALL.156
C                                                                                ADDALL.157
C--- DECOUPLED WIND AT BOUNDARIES                                                ADDALL.158
C                                                                                ADDALL.159
        DO 60 N=2,9                                                              ADDALL.160
          NCOUNT=NCOUNT+1                                                        ADDALL.161
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*IX2D,K))                        ADDALL.162
   60   CONTINUE                                                                 ADDALL.163
        NCOU=NCOU+8*IX2D                                                         ADDALL.164
        DO 65 N=2,9                                                              ADDALL.165
          NCOUNT=NCOUNT+1                                                        ADDALL.166
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*JX2D,K))                        ADDALL.167
   65   CONTINUE                                                                 ADDALL.168
        ISAVER=IAXALL(NCOUNT,K)                                                  ADDALL.169
        IAXALL(NCOUNT,K)=ISAVEI                                                  ADDALL.170
        NCOU=NCOU+8*JX2D                                                         ADDALL.171
C                                                                                ADDALL.172
C--- ADDR4 VARIABLES (INTEGERS)                                                  ADDALL.173
C                                                                                ADDALL.174
        DO 70 N=2,NVARI+1                                                        ADDALL.175
          NCOUNT=NCOUNT+1                                                        ADDALL.176
          IAXALL(NCOUNT,K)=LOC(INTALL(NICOU+(N-1),K))                            ADDALL.177
   70   CONTINUE                                                                 ADDALL.178
        ISAVEI=IAXALL(NCOUNT,K)                                                  ADDALL.179
        IAXALL(NCOUNT,K)=ISAVER                                                  ADDALL.180
        NICOU=NICOU+NVARI                                                        ADDALL.181
C                                                                                ADDALL.182
C--- ADDR4 VARIABLES (REALS)                                                     ADDALL.183
C                                                                                ADDALL.184
        DO 75 N=2,NVARP+1                                                        ADDALL.185
          NCOUNT=NCOUNT+1                                                        ADDALL.186
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1),K))                             ADDALL.187
   75   CONTINUE                                                                 ADDALL.188
        ISAVER=IAXALL(NCOUNT,K)                                                  ADDALL.189
        IAXALL(NCOUNT,K)=ISAVEI                                                  ADDALL.190
        NCOU=NCOU+NVARP                                                          ADDALL.191
C                                                                                ADDALL.192
C--- ADDRN SPECIAL NEST VARIABLES (ALL INTEGER)                                  ADDALL.193
C                                                                                ADDALL.194
        DO 80 N=2,NVARNI+1                                                       ADDALL.195
          NCOUNT=NCOUNT+1                                                        ADDALL.196
          IAXALL(NCOUNT,K)=LOC(INTALL(NICOU+N-1,K))                              ADDALL.197
   80   CONTINUE                                                                 ADDALL.198
        NICOU=NICOU+NVARNI                                                       ADDALL.199
C                                                                                ADDALL.200
        DO 85 N=2,7                                                              ADDALL.201
          NCOUNT=NCOUNT+1                                                        ADDALL.202
          IAXALL(NCOUNT,K)=LOC(INTALL(NICOU+(N-1)*MAXSES,K))                     ADDALL.203
   85   CONTINUE                                                                 ADDALL.204
        NICOU=NICOU+6*MAXSES                                                     ADDALL.205
C                                                                                ADDALL.206
        ISAVEI=IAXALL(NCOUNT,K)                                                  ADDALL.207
        IAXALL(NCOUNT,K)=ISAVER                                                  ADDALL.208
C                                                                                ADDALL.209
C--- ADDRCU VARIABLES (9 3-D REAL, 9 2-D REAL, 2 2-D INTEGER)                    05DEC01.667
C                                                                                ADDALL.211
        DO 105 N=1,9                                                             ADDALL.212
          NCOUNT=NCOUNT+1                                                        ADDALL.213
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+N*MIXC*MJXC*MKXC,K))                  ADDALL.214
  105   CONTINUE                                                                 ADDALL.215
        NCOU=NCOU+9*MIXC*MJXC*MKXC                                               ADDALL.216
        DO 106 N=1,9                                                             05DEC01.668
          NCOUNT=NCOUNT+1                                                        05DEC01.669
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+N*MIXC*MJXC,K))                       05DEC01.670
  106   CONTINUE                                                                 05DEC01.671
        ISAVER=IAXALL(NCOUNT,K)                                                  ADDALL.219
        IAXALL(NCOUNT,K)=ISAVEI                                                  ADDALL.220
        NCOU=NCOU+9*MIXC*MJXC                                                    05DEC01.672
        DO 107 N=1,2                                                             05DEC01.673
          NCOUNT=NCOUNT+1                                                        05DEC01.674
          IAXALL(NCOUNT,K)=LOC(INTALL(NICOU+N*MIXC*MJXC,K))                      05DEC01.675
  107   CONTINUE                                                                 05DEC01.676
        ISAVEI=IAXALL(NCOUNT,K)                                                  ADDALL.224
        IAXALL(NCOUNT,K)=ISAVER                                                  ADDALL.225
        NICOU=NICOU+2*MIXC*MJXC                                                  05DEC01.677
        ISAVE=IAXALL(NCOUNT,K)                                                   ADDALL.227
C                                                                                ADDALL.228
C--- ADDRAS VARIABLES (2 3-D REAL, 1 2-D REAL, 1 2-D INTEGER)                    ADDALL.229
C                                                                                ADDALL.230
        DO 110 N=1,2                                                             ADDALL.231
          NCOUNT=NCOUNT+1                                                        ADDALL.232
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+N*MIXA*MJXA*MKXA,K))                  ADDALL.233
  110   CONTINUE                                                                 ADDALL.234
        NCOU=NCOU+2*MIXA*MJXA*MKXA                                               ADDALL.235
        NCOUNT=NCOUNT+1                                                          ADDALL.236
        IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+MIXA*MJXA,K))                           ADDALL.237
        ISAVER=IAXALL(NCOUNT,K)                                                  ADDALL.238
        IAXALL(NCOUNT,K)=ISAVEI                                                  ADDALL.239
        NCOU=NCOU+MIXA*MJXA                                                      ADDALL.240
        NCOUNT=NCOUNT+1                                                          ADDALL.241
        IAXALL(NCOUNT,K)=LOC(INTALL(NICOU+MIXA*MJXA,K))                          ADDALL.242
        ISAVEI=IAXALL(NCOUNT,K)                                                  ADDALL.243
        IAXALL(NCOUNT,K)=ISAVER                                                  ADDALL.244
        NICOU=NICOU+MIXA*MJXA                                                    ADDALL.245
        ISAVE=IAXALL(NCOUNT,K)                                                   ADDALL.246
C                                                                                ADDALL.247
C--- ADDR5 3D FDDA VARIABLES                                                     ADDALL.248
C                                                                                ADDALL.249
        IAXALL(NCOUNT,K)=LOC(ALLFG(1,K))                                         ADDALL.250
        DO 115 N=2,10                                                            ADDALL.251
          NCOUNT=NCOUNT+1                                                        ADDALL.252
          IAXALL(NCOUNT,K)=LOC(ALLFG(NGCOU+(N-1)*IX3DFDG,K))                     ADDALL.253
  115   CONTINUE                                                                 ADDALL.254
        NGCOU=NGCOU+9*IX3DFDG                                                    ADDALL.255
C                                                                                ADDALL.256
C--- ADDR6 2D FDDA VARIABLES (11TH AND 12TH INTEGER OUT OF 14)                   ADDALL.257
C                                                                                ADDALL.258
        DO 120 N=2,11                                                            ADDALL.259
          NCOUNT=NCOUNT+1                                                        ADDALL.260
          IAXALL(NCOUNT,K)=LOC(ALLFG(NGCOU+(N-1)*IX2DFDG,K))                     ADDALL.261
  120   CONTINUE                                                                 ADDALL.262
        ISAVERF=IAXALL(NCOUNT,K)                                                 ADDALL.263
        IAXALL(NCOUNT,K)=LOC(INTFG(1,K))                                         ADDALL.264
        NGCOU=NGCOU+10*IX2DFDG                                                   ADDALL.265
        DO 121 N=2,3                                                             ADDALL.266
          NCOUNT=NCOUNT+1                                                        ADDALL.267
          IAXALL(NCOUNT,K)=LOC(INTFG(NIGCOU+(N-1)*IX2DFDG,K))                    ADDALL.268
  121   CONTINUE                                                                 ADDALL.269
        ISAVEIF=IAXALL(NCOUNT,K)                                                 ADDALL.270
        IAXALL(NCOUNT,K)=ISAVERF                                                 ADDALL.271
        NIGCOU=NIGCOU+2*IX2DFDG                                                  ADDALL.272
        DO 122 N=2,3                                                             ADDALL.273
          NCOUNT=NCOUNT+1                                                        ADDALL.274
          IAXALL(NCOUNT,K)=LOC(ALLFG(NGCOU+(N-1)*IX2DFDG,K))                     ADDALL.275
  122   CONTINUE                                                                 ADDALL.276
        NGCOU=NGCOU+2*IX2DFDG                                                    ADDALL.277
C                                                                                ADDALL.278
C--- ADDR7 FDDA VARIABLES                                                        ADDALL.279
C                                                                                ADDALL.280
        NCOUNT=NCOUNT+1                                                          ADDALL.281
        IAXALL(NCOUNT,K)=LOC(ALLFG(NGCOU+NTIM*NVAR*IX2DFDG,K))                   ADDALL.282
        NGCOU=NGCOU+NTIM*NVAR*IX2DFDG                                            ADDALL.283
C                                                                                ADDALL.284
C                                                                                ADDALL.285
        NCOUNT=NCOUNT+1                                                          ADDALL.286
        IAXALL(NCOUNT,K)=LOC(ALLFG(NGCOU+NVAR*IX2DFDG,K))                        ADDALL.287
        NGCOU=NGCOU+NVAR*IX2DFDG                                                 ADDALL.288
C                                                                                ADDALL.289
C                                                                                ADDALL.290
        DO 125 N=2,6                                                             ADDALL.291
          NCOUNT=NCOUNT+1                                                        ADDALL.292
          IAXALL(NCOUNT,K)=LOC(ALLFG(NGCOU+(N-1)*IX2DFDG,K))                     ADDALL.293
  125   CONTINUE                                                                 ADDALL.294
        NGCOU=NGCOU+5*IX2DFDG                                                    ADDALL.295
C                                                                                ADDALL.296
C                                                                                ADDALL.297
        NCOUNT=NCOUNT+1                                                          ADDALL.298
        IAXALL(NCOUNT,K)=LOC(ALLFG(NGCOU+NTIM,K))                                ADDALL.299
        NGCOU=NGCOU+NTIM                                                         ADDALL.300
C                                                                                ADDALL.301
C                                                                                ADDALL.302
        DO 130 N=2,3                                                             ADDALL.303
          NCOUNT=NCOUNT+1                                                        ADDALL.304
C         IAXALL(NCOUNT,K)=LOC(ALLFG(NGCOU+(N-1)*NVAR,K))                        ADDALL.305
          IF(N.EQ.2)IAXALL(NCOUNT,K)=LOC(ALLFG(NGCOU+(N-1)*NVAR,K))              ADDALL.306
  130   CONTINUE                                                                 ADDALL.307
C          ISAVERF=IAXALL(NCOUNT,K)                                              ADDALL.308
        IAXALL(NCOUNT,K)=ISAVEIF                                                 ADDALL.309
        NGCOU=NGCOU+2*NVAR                                                       ADDALL.310
C                                                                                ADDALL.311
C        SIX INTEGERS IN ADDR7                                                   ADDALL.312
C                                                                                ADDALL.313
        DO 131 N=2,3                                                             ADDALL.314
          NCOUNT=NCOUNT+1                                                        ADDALL.315
          IAXALL(NCOUNT,K)=LOC(INTFG(NIGCOU+(N-1)*NVAR,K))                       ADDALL.316
  131   CONTINUE                                                                 ADDALL.317
        NIGCOU=NIGCOU+2*NVAR                                                     ADDALL.318
C                                                                                ADDALL.319
C                                                                                ADDALL.320
        NCOUNT=NCOUNT+1                                                          ADDALL.321
        IAXALL(NCOUNT,K)=LOC(INTFG(NIGCOU+NTIM*NVAR,K))                          ADDALL.322
        NIGCOU=NIGCOU+NTIM*NVAR                                                  ADDALL.323
C                                                                                ADDALL.324
C                                                                                ADDALL.325
        NCOUNT=NCOUNT+1                                                          ADDALL.326
        IAXALL(NCOUNT,K)=LOC(INTFG(NIGCOU+NCHA,K))                               ADDALL.327
        NIGCOU=NIGCOU+NCHA                                                       ADDALL.328
C                                                                                ADDALL.329
C                                                                                ADDALL.330
        NCOUNT=NCOUNT+1                                                          ADDALL.331
        IAXALL(NCOUNT,K)=LOC(INTFG(NIGCOU+NCHA*NVAR,K))                          ADDALL.332
        NIGCOU=NIGCOU+NCHA*NVAR                                                  ADDALL.333
C                                                                                ADDALL.334
        NCOUNT=NCOUNT+1                                                          ADDALL.335
C        IAXALL(NCOUNT,K)=LOC(INTFG(NIGCOU+NVAR,K))                              ADDALL.336
C          ISAVEIF=IAXALL(NCOUNT,K)                                              ADDALL.337
        IAXALL(NCOUNT,K)=LOC(ALLFO(1,K))                                         ADDALL.338
        NIGCOU=NIGCOU+NVAR                                                       ADDALL.339
C                                                                                ADDALL.340
C--- ADDR8 FDDA VARIABLES                                                        ADDALL.341
C                                                                                ADDALL.342
        NCOUNT=NCOUNT+1                                                          ADDALL.343
        IAXALL(NCOUNT,K)=LOC(ALLFO(NOCOU+4*IX3DFDO,K))                           ADDALL.344
        NOCOU=NOCOU+4*IX3DFDO                                                    ADDALL.345
C                                                                                ADDALL.346
C                                                                                ADDALL.347
        NCOUNT=NCOUNT+1                                                          ADDALL.348
        IAXALL(NCOUNT,K)=LOC(ALLFO(NOCOU+NIOBF*5,K))                             ADDALL.349
        NOCOU=NOCOU+5*NIOBF                                                      ADDALL.350
C                                                                                ADDALL.351
C                                                                                ADDALL.352
        DO 135 N=2,5                                                             ADDALL.353
          NCOUNT=NCOUNT+1                                                        ADDALL.354
          IAXALL(NCOUNT,K)=LOC(ALLFO(NOCOU+(N-1)*NIOBF,K))                       ADDALL.355
  135   CONTINUE                                                                 ADDALL.356
        NOCOU=NOCOU+4*NIOBF                                                      ADDALL.357
C                                                                                ADDALL.358
C                                                                                ADDALL.359
        NCOUNT=NCOUNT+1                                                          ADDALL.360
C        IAXALL(NCOUNT,K)=LOC(ALLFO(NOCOU+NIOBF*6,K))                            ADDALL.361
C         ISAVEIOF=IAXALL(NCOUNT,K)                                              ADDALL.362
        IAXALL(NCOUNT,K)=ISAVE                                                   ADDALL.363
        NOCOU=NOCOU+6*NIOBF                                                      ADDALL.364
C                                                                                ADDALL.365
C--- NON HYDROSTATIC VARIABLES                                                   ADDALL.366
C                                                                                ADDALL.367
        DO 145 N=2,3                                                             ADDALL.368
          NCOUNT=NCOUNT+1                                                        ADDALL.369
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*MIXNH*MJXNH*KXP1NH,K))          ADDALL.370
  145   CONTINUE                                                                 ADDALL.371
        NCOU=NCOU+2*MIXNH*MJXNH*KXP1NH                                           ADDALL.372
C                                                                                ADDALL.373
        DO 155 N=2,3                                                             ADDALL.374
          NCOUNT=NCOUNT+1                                                        ADDALL.375
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*MIXNH*MJXNH*MKXNH,K))           ADDALL.376
  155   CONTINUE                                                                 ADDALL.377
        NCOU=NCOU+2*MIXNH*MJXNH*MKXNH                                            ADDALL.378
C                                                                                ADDALL.379
        DO 165 N=2,5                                                             ADDALL.380
          NCOUNT=NCOUNT+1                                                        ADDALL.381
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*MIXNH*KXP1NH*NSPGX,K))          ADDALL.382
  165   CONTINUE                                                                 ADDALL.383
        NCOU=NCOU+4*MIXNH*KXP1NH*NSPGX                                           ADDALL.384
C                                                                                ADDALL.385
        DO 175 N=2,5                                                             ADDALL.386
          NCOUNT=NCOUNT+1                                                        ADDALL.387
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*MIXNH*MKXNH*NSPGX,K))           ADDALL.388
  175   CONTINUE                                                                 ADDALL.389
        NCOU=NCOU+4*MIXNH*MKXNH*NSPGX                                            ADDALL.390
C                                                                                ADDALL.391
        DO 180 N=2,5                                                             ADDALL.392
          NCOUNT=NCOUNT+1                                                        ADDALL.393
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*MJXNH*KXP1NH*NSPGX,K))          ADDALL.394
  180   CONTINUE                                                                 ADDALL.395
        NCOU=NCOU+4*MJXNH*KXP1NH*NSPGX                                           ADDALL.396
C                                                                                ADDALL.397
        DO 185 N=2,5                                                             ADDALL.398
          NCOUNT=NCOUNT+1                                                        ADDALL.399
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+(N-1)*MJXNH*MKXNH*NSPGX,K))           ADDALL.400
  185   CONTINUE                                                                 ADDALL.401
        NCOU=NCOU+4*MJXNH*MKXNH*NSPGX                                            ADDALL.402
C                                                                                ADDALL.403
C                                                                                ADDALL.404
        NCOUNT=NCOUNT+1                                                          ADDALL.405
        IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+MIXNH*MJXNH*MKXNH,K))                   ADDALL.406
        NCOU=NCOU+MIXNH*MJXNH*MKXNH                                              ADDALL.407
C                                                                                ADDALL.408
C                                                                                ADDALL.409
        NCOUNT=NCOUNT+1                                                          ADDALL.410
        IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+MIXNH*MJXNH,K))                         ADDALL.411
        NCOU=NCOU+MIXNH*MJXNH                                                    ADDALL.412
C                                                                                ADDALL.413
C                                                                                ADDALL.414
        DO 188 N=1,2                                                             ADDALL.415
          NCOUNT=NCOUNT+1                                                        ADDALL.416
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+N*MIXNH*MJXNH*MKXNH,K))               ADDALL.417
  188   CONTINUE                                                                 ADDALL.418
        NCOU=NCOU+2*MIXNH*MJXNH*MKXNH                                            ADDALL.419
C                                                                                ADDALL.420
C                                                                                ADDALL.421
        NCOUNT=NCOUNT+1                                                          ADDALL.422
        IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+MIXV*MJXV*MKXV,K))                      ADDALL.423
        NCOU=NCOU+MIXV*MJXV*MKXV                                                 ADDALL.424
        DO 195 N=1,3                                                             ADDALL.425
          NCOUNT=NCOUNT+1                                                        ADDALL.426
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+N*MIXV2*MJXV2*MKXV2,K))               ADDALL.427
  195   CONTINUE                                                                 ADDALL.428
        NCOU=NCOU+3*MIXV2*MJXV2*MKXV2                                            ADDALL.429
        DO 196 N=1,5                                                             ADDALL.430
          NCOUNT=NCOUNT+1                                                        ADDALL.431
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+N*MIXV3*MJXV3*MKXV3,K))               ADDALL.432
  196   CONTINUE                                                                 ADDALL.433
        NCOU=NCOU+5*MIXV3*MJXV3*MKXV3                                            ADDALL.434
C                                                                                ADDALL.435
C     SOIL MEMORY                                                                ADDALL.436
        DO N=1,2                                                                 ADDALL.437
          NCOUNT=NCOUNT+1                                                        ADDALL.438
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+N*MIXS*MJXS*MLX,K))                   ADDALL.439
        ENDDO                                                                    ADDALL.440
        NCOU=NCOU+2*MIXS*MJXS*MLX                                                ADDALL.441
C                                                                                ADDALL.442
C                                                                                ADDALL.443
C --- FOR RPSTAR, PDOT AND RPDOT.                                                ADDALL.444
C         DPSDXM, DPSDYM, DMDX, DMDY, EX, CRX, SRX                               ADDALL.445
C                                                                                ADDALL.446
        DO 192 N=1,14                                                            ADDALL.447
          NCOUNT=NCOUNT+1                                                        ADDALL.448
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+N*MIX*MJX,K))                         ADDALL.449
  192   CONTINUE                                                                 ADDALL.450
        NCOU=NCOU+14*MIX*MJX                                                     ADDALL.451
C                                                                                ADDALL.452
C                                                                                ADDALL.453
C     LAND-SURFACE MEMORY                                                        ADDALL.454
C       4 REAL 2D ARRAYS                                                         ADDALL.455
        DO N=1,4                                                                 ADDALL.456
          NCOUNT=NCOUNT+1                                                        ADDALL.457
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+N*MIXL*MJXL,K))                       ADDALL.458
        ENDDO                                                                    ADDALL.459
        NCOU=NCOU+4*MIXL*MJXL                                                    ADDALL.460
C       1 INTEGER 2D ARRAY                                                       ADDALL.461
        ISAVER=IAXALL(NCOUNT,K)                                                  ADDALL.462
        IAXALL(NCOUNT,K)=ISAVEI                                                  ADDALL.463
        NCOUNT=NCOUNT+1                                                          ADDALL.464
        IAXALL(NCOUNT,K)=LOC(INTALL(NICOU+MIXL*MJXL,K))                          ADDALL.465
        NICOU=NICOU+MIXL*MJXL                                                    ADDALL.466
        ISAVEI=IAXALL(NCOUNT,K)                                                  ADDALL.467
        IAXALL(NCOUNT,K)=ISAVER                                                  ADDALL.468
C       28 REAL 2D ARRAYS                                                        19DEC02.1345
        DO N=1,28                                                                19DEC02.1346
          NCOUNT=NCOUNT+1                                                        ADDALL.471
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+N*MIXL*MJXL,K))                       ADDALL.472
        ENDDO                                                                    ADDALL.473
        NCOU=NCOU+28*MIXL*MJXL                                                   19DEC02.1347
C       2 INTEGER 2D ARRAYS                                                      ADDALL.475
        ISAVER=IAXALL(NCOUNT,K)                                                  ADDALL.476
        IAXALL(NCOUNT,K)=ISAVEI                                                  ADDALL.477
        DO N=1,2                                                                 ADDALL.478
          NCOUNT=NCOUNT+1                                                        ADDALL.479
          IAXALL(NCOUNT,K)=LOC(INTALL(NICOU+N*MIXL*MJXL,K))                      ADDALL.480
        ENDDO                                                                    ADDALL.481
        NICOU=NICOU+2*MIXL*MJXL                                                  ADDALL.482
        ISAVEI=IAXALL(NCOUNT,K)                                                  ADDALL.483
        IAXALL(NCOUNT,K)=ISAVER                                                  ADDALL.484
C       6 REAL 3D ARRAYS                                                         19DEC02.1348
        DO N=1,6                                                                 19DEC02.1349
          NCOUNT=NCOUNT+1                                                        ADDALL.487
          IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+N*MIXL*MJXL*MLX,K))                   ADDALL.488
        ENDDO                                                                    ADDALL.489
        NCOU=NCOU+6*MIXL*MJXL*MLX                                                19DEC02.1350
        NCOUNT=NCOUNT+1                                                          ADDALL.491
C       2 REAL 2D*12 ARRAYS                                                      19DEC02.1351
        IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+MIXL*MJXL*12,K))                        19DEC02.1352
        NCOU=NCOU+MIXL*MJXL*12                                                   19DEC02.1353
        NCOUNT=NCOUNT+1                                                          19DEC02.1354
        IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+MIXL*MJXL*12,K))                        05DEC01.679
        NCOU=NCOU+MIXL*MJXL*12                                                   05DEC01.680
        NCOUNT=NCOUNT+1                                                          05DEC01.681
                                                                                 05DEC01.682
C       RADIATION ARRAY                                                          05DEC01.683
        IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+MIXR*MJXR*MKXR,K))                      ADDALL.492
        NCOU=NCOU+MIXR*MJXR*MKXR                                                 ADDALL.493
        NCOUNT=NCOUNT+1                                                          ADDALL.494
                                                                                 05DEC01.684
C       UPPER B.C. ARRAY                                                         05DEC01.685
        IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+13*13,K))                               ADDALL.495
        NCOU=NCOU+13*13                                                          ADDALL.496
        NCOUNT=NCOUNT+1                                                          ADDALL.497
C                                                                                ADDALL.498
C GSPBL TKE POINTERS                                                             ADDALL.499
C                                                                                ADDALL.500
        IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+MIXG*MJXG*(MKXG+1),K))                  ADDALL.501
        NCOU=NCOU+MIXG*MJXG*(MKXG+1)                                             ADDALL.502
        NCOUNT=NCOUNT+1                                                          ADDALL.503
C                                                                                ADDALL.504
        IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+MIXG*MJXG*(MKXG+1),K))                  ADDALL.505
        NCOU=NCOU+MIXG*MJXG*(MKXG+1)                                             ADDALL.506
C                                                                                ADDALL.508
        ISAVER = IAXALL(NCOUNT,K)                                                07NOV00.1188
        IAXALL(NCOUNT,K)=ISAVEI                                                  ADDALL.509
        NCOUNT=NCOUNT+1                                                          19DEC02.1355
#ifdef MPP1                                                                      25JAN00.454
C        IAXALL(NCOUNT,K)=LOC(INTALL(NICOU+MIXG*MJXG,K))                         25JAN00.455
        ISAVEI=IAXALL(NCOUNT,K)                                                  25JAN00.456
        IAXALL(NCOUNT,K)=ISAVER                                                  25JAN00.457
        NICOU=NICOU+MIXG*MJXG                                                    25JAN00.458
        NCOUNT=NCOUNT+1                                                          25JAN00.459
#else                                                                            25JAN00.460
C        IAXALL(NCOUNT,K)=LOC(INTALL(NICOU+MJXG,K))                              ADDALL.510
        ISAVEI=IAXALL(NCOUNT,K)                                                  ADDALL.511
        IAXALL(NCOUNT,K)=ISAVER                                                  ADDALL.512
        NICOU=NICOU+MJXG                                                         ADDALL.513
        NCOUNT=NCOUNT+1                                                          ADDALL.514
#endif                                                                           25JAN00.461
C                                                                                ADDALL.515
C GSPBL FOG POINTERS                                                             ADDALL.516
C                                                                                ADDALL.517
        IAXALL(NCOUNT,K)=LOC(ALLARR(NCOU+MIXG*MJXG,K))                           ADDALL.518
        NCOU=NCOU+MIXG*MJXG                                                      ADDALL.519
        NCOUNT=NCOUNT+1                                                          ADDALL.520
C        IAXALL(NCOUNT,K)=                                                       ADDALL.521
C     &              LOC(ALLARR(NCOU+MIXG*MJXG,K))                               ADDALL.522
        NCOU=NCOU+MIXG*MJXG                                                      ADDALL.523
C       NCOUNT=NCOUNT+1                                                          ADDALL.524
C-GSPBL-END                                                                      ADDALL.525
  100 CONTINUE                                                                   ADDALL.526
      IF((NCOU-IRHUGE).NE.1.OR.(NICOU-IIHUGE).NE.1.OR.(NCOUNT-NUMVAR)            ADDALL.527
     +  .NE.1)THEN                                                               ADDALL.528
        PRINT9000,NCOU,IRHUGE,NICOU,IIHUGE,NCOUNT,NUMVAR                         ADDALL.529
        STOP 'MEMORY PROBLEM'                                                    ADDALL.530
      ENDIF                                                                      ADDALL.531
      IF((NGCOU-IFGHUG).NE.1.OR.(NIGCOU-IFGIHUG).NE.1.OR.(NOCOU-IFOHUG)          ADDALL.532
     +  .NE.1)THEN                                                               ADDALL.533
        PRINT9010,NGCOU,IFGHUG,NIGCOU,IFGIHUG,NOCOU,IFOHUG                       ADDALL.534
        STOP 'MEMORY PROBLEM WITH FDDA'                                          ADDALL.535
      ENDIF                                                                      ADDALL.536
9000  FORMAT(1X,'NCOU,IRHUGE,NICOU,IIHUGE,NCOUNT,NUMVAR = ',/5X,6I10)            ADDALL.537
9010  FORMAT(1X,'NGCOU,IFGHUG,NIGCOU,IFGIHUG,NOCOU,IFOHUG = ',/5X,6I10)          ADDALL.538
C     PRINT *,'NCOU,IRHUGE,NICOU,IIHUGE,NCOUNT,NUMV =',                          ADDALL.539
C    -         NCOU,IRHUGE,NICOU,IIHUGE,NCOUNT,NUMVAR                            ADDALL.540
C     PRINT *,'NGCOU,IFGHUG,NIGCOU,IFGIHUG,NOCOU,IFOHUG =',                      ADDALL.541
C    -         NGCOU,IFGHUG,NIGCOU,IFGIHUG,NOCOU,IFOHUG                          ADDALL.542
      RETURN                                                                     ADDALL.543
      END                                                                        ADDALL.544
