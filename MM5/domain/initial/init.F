      SUBROUTINE INIT (IEXEC,IX,JX)                                              INIT.1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          INIT.2
C                                                                     C          INIT.3
C                                                                     C          INIT.4
C     THIS SUBROUTINE READS IN THE INITIAL AND BOUNDARY CONDITIONS.   C          INIT.5
C                                                                     C          INIT.6
C                                                                     C          INIT.7
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          INIT.8
#     include <defines.incl>                                                     INIT.9
#     include <parame.incl>                                                      INIT.10
#     include <param2.incl>                                                      INIT.11
#     include <parfdda.incl>                                                     INIT.12
#     include <param3.incl>                                                      INIT.13
#     include <addr0.incl>                                                       INIT.14
#     include <point3d.incl>                                                     INIT.15
#     include <point2d.incl>                                                     INIT.16
#     include <various.incl>                                                     INIT.17
#     include <nestl.incl>                                                       INIT.18
#     include <varia.incl>                                                       INIT.19
#     include <dusolve1.incl>                                                    INIT.20
#     include <fddagd.incl>                                                      INIT.21
#     include <fddaob.incl>                                                      INIT.22
#     include <nonhyd.incl>                                                      INIT.23
#     include <nonhydb.incl>                                                     INIT.24
#     include <nhcnst.incl>                                                      INIT.25
#     include <addrcu.incl>                                                      INIT.26
#     include <pmoist.incl>                                                      INIT.27
#     include <soil.incl>                                                        INIT.28
#     include <soilp.incl>                                                       INIT.29
#     include <soilcnst.incl>                                                    INIT.30
#     include <rpstar.incl>                                                      INIT.31
#     include <uprad.incl>                                                       INIT.32
#     include <hdrv3.incl>                                                       INIT.33
#     include <parkfcu2.incl>                                                    05DEC01.58
#ifdef MPP1                                                                      INIT.34
#     include <radiat.incl>                                                      INIT.35
#endif                                                                           INIT.36
#     include <navypb.incl>                                                      INIT.37
#     include <pbltke.incl>                                                      INIT.38
#     include <zdiffu.incl>                                                      23DEC04.59
#     include <zdiffu2.incl>                                                     23DEC04.60
C                                                                                INIT.39
      DIMENSION HTNO(MIX,MJX),ISTATE(2)                                          INIT.40
      INTEGER IEXEC(MAXNES)                                                      INIT.41
      EXTERNAL DM_IONODE                                                         INIT.42
      LOGICAL  DM_IONODE                                                         INIT.43
C----------------------------------------------------------------------          INIT.44
#     include <landuse.incl>                                                     INIT.45
#     include <paramveg_USGS.incl>                                               19DEC02.59
C-----SET RAINWATER AND CLOUD WATER EQUAL TO ZERO INITIALLY.                     INIT.46
C                                                                                INIT.47
      IF(IDRY(1).EQ.1)GOTO 20                                                    INIT.48
C                                                                                INIT.49
      DO J=1,JL                                                                  07NOV00.60
        DO I=1,IL                                                                07NOV00.61
          TA2(I,J)=0.                                                            07NOV00.62
          QA2(I,J)=0.                                                            07NOV00.63
          UA10(I,J)=0.                                                           07NOV00.64
          VA10(I,J)=0.                                                           07NOV00.65
          TSS(I,J)=0.                                                            07NOV00.66
          SEAICE(I,J)=0.                                                         23DEC04.61
          SNOWC(I,J)=0.                                                          23DEC04.62
        ENDDO                                                                    07NOV00.67
      ENDDO                                                                      07NOV00.68
      IF(IMOIST(1).EQ.2)THEN                                                     INIT.50
        DO 10 K=1,KL                                                             INIT.51
          DO 10 I=1,ILX                                                          INIT.52
            DO 10 J=1,JLX                                                        INIT.53
              QCA(I,J,K)=0.                                                      INIT.54
              QCB(I,J,K)=0.                                                      INIT.55
              QRA(I,J,K)=0.                                                      INIT.56
              QRB(I,J,K)=0.                                                      INIT.57
   10       CONTINUE                                                             INIT.58
                                                                                 INIT.59
                                                                                 INIT.60
        IF(IICE.EQ.1)THEN                                                        INIT.61
          DO 11 K=1,KL                                                           INIT.62
            DO 11 I=1,ILX                                                        INIT.63
              DO 11 J=1,JLX                                                      INIT.64
                QIA(I,J,K)=0.                                                    INIT.65
                QIB(I,J,K)=0.                                                    INIT.66
                QNIA(I,J,K)=0.                                                   INIT.67
                QNIB(I,J,K)=0.                                                   INIT.68
                IF(IICEG.EQ.1)THEN                                               INIT.69
                  QGA(I,J,K)=0.                                                  INIT.70
                  QGB(I,J,K)=0.                                                  INIT.71
                  QNCA(I,J,K)=0.                                                 INIT.72
                  QNCB(I,J,K)=0.                                                 INIT.73
                ENDIF                                                            INIT.74
   11         CONTINUE                                                           INIT.75
                                                                                 INIT.76
                                                                                 INIT.77
        ENDIF                                                                    INIT.78
      ENDIF                                                                      INIT.79
C                                                                                INIT.80
C                                                                                INIT.81
   20 TDINI=0.                                                                   INIT.82
      TDADV=0.                                                                   INIT.83
      TQINI=0.                                                                   INIT.84
      TQADV=0.                                                                   INIT.85
      TQEVA=0.                                                                   INIT.86
      TQRAI=0.                                                                   INIT.87
C                                                                                INIT.88
C-----SET THE VARIABLES RELATED TO BURK-THOMPSON PBL EQUAL TO 0 INITIALL         INIT.89
C                                                                                INIT.90
      IF(INAV.EQ.1)THEN                                                          INIT.91
        DO K=1,KL                                                                INIT.92
          DO I=1,ILX                                                             INIT.93
            DO J=1,JLX                                                           INIT.94
              TURKE(I,J,K)=0.                                                    INIT.95
              PQWSB(I,J,K)=0.                                                    INIT.96
              PTHLSB(I,J,K)=0.                                                   INIT.97
              PTHLQWB(I,J,K)=0.                                                  INIT.98
            ENDDO                                                                INIT.99
          ENDDO                                                                  INIT.100
        ENDDO                                                                    INIT.101
      ENDIF                                                                      INIT.102
C                                                                                INIT.103
C INITIALIZATION OF TKE FOR GAYNO-SEAMAN SCHEME                                  INIT.104
C                                                                                INIT.105
      IF(IGSPBL.EQ.1)THEN                                                        INIT.106
        DO J=1,JLX                                                               INIT.107
          DO I=1,ILX                                                             INIT.108
            TKEB(I,J,KL+1)=0.0                                                   INIT.109
            TKEA(I,J,KL+1)=0.0                                                   INIT.110
            DO K=1,KL                                                            INIT.111
              TKEB(I,J,K)=0.001                                                  INIT.112
              TKEA(I,J,K)=0.001                                                  INIT.113
            ENDDO                                                                INIT.114
          ENDDO                                                                  INIT.115
        ENDDO                                                                    INIT.116
      ENDIF                                                                      INIT.117
C                                                                                INIT.118
C-----SET THE VARIABLES RELATED TO BLACKADAR PBL EQUAL TO 0 INITIALLY.           INIT.119
C                                                                                INIT.120
      IF(IBLTYP(1).EQ.0)GOTO 40                                                  INIT.121
      DO 30 I=1,ILX                                                              INIT.122
        DO 30 J=1,JLX                                                            INIT.123
          MOL(I,J)=0.                                                            INIT.124
          ZOL(I,J)=0.                                                            INIT.125
          HOL(I,J)=0.                                                            INIT.126
          PBL(I,J)=0.                                                            INIT.127
          HFX(I,J)=0.                                                            INIT.128
          QFX(I,J)=0.                                                            INIT.129
          UST(I,J)=0.                                                            INIT.130
          PRW(I,J)=0.                                                            INIT.131
          GSW(I,J)=0.                                                            INIT.132
          GLW(I,J)=0.                                                            INIT.133
          REGIME(I,J)=0.                                                         INIT.134
   30   CONTINUE                                                                 INIT.135
                                                                                 INIT.136
   40 CONTINUE                                                                   INIT.137
                                                                                 23DEC04.63
C                                                                                23DEC04.64
C   -----SET INITIAL VARIABLES FOR ITPDIF=2                                      23DEC04.65
        oldnest = 0                                                              23DEC04.66
        diffpar = .true.                                                         23DEC04.67
C                                                                                23DEC04.68
      IF (ITPDIF .ge. 2) THEN                                                    23DEC04.69
        DO K=1,KL                                                                23DEC04.70
          DO I=1,ILX                                                             23DEC04.71
            DO J=1,JLX                                                           23DEC04.72
              MRKJP2(i,j,k) = 0.                                                 23DEC04.73
              MRKJM2(i,j,k) = 0.                                                 23DEC04.74
              MRKJP1(i,j,k) = 0.                                                 23DEC04.75
              MRKJM1(i,j,k) = 0.                                                 23DEC04.76
              MRKIP2(i,j,k) = 0.                                                 23DEC04.77
              MRKIM2(i,j,k) = 0.                                                 23DEC04.78
              MRKIP1(i,j,k) = 0.                                                 23DEC04.79
              MRKIM1(i,j,k) = 0.                                                 23DEC04.80
              IRKJP2(i,j,k) = 0                                                  23DEC04.81
              IRKJM2(i,j,k) = 0                                                  23DEC04.82
              IRKJP1(i,j,k) = 0                                                  23DEC04.83
              IRKJM1(i,j,k) = 0                                                  23DEC04.84
              IRKIP2(i,j,k) = 0                                                  23DEC04.85
              IRKIM2(i,j,k) = 0                                                  23DEC04.86
              IRKIP1(i,j,k) = 0                                                  23DEC04.87
              IRKIM1(i,j,k) = 0                                                  23DEC04.88
            ENDDO                                                                23DEC04.89
          ENDDO                                                                  23DEC04.90
        ENDDO                                                                    23DEC04.91
      ENDIF                                                                      23DEC04.92
C                                                                                INIT.138
#ifdef MPP1                                                                      INIT.139
#  include <mpp_init_05.incl>                                                    INIT.140
#endif                                                                           INIT.141
C                                                                                INIT.142
C--- SET COARSE MESH VALUES                                                      INIT.143
C                                                                                INIT.144
C                                                                                INIT.145
C---- READ IN THE COARSE GRID INITIAL CONDITIONS                                 INIT.146
C                                                                                INIT.147
      IHEADER=0                                                                  INIT.148
      CALL RDINIT(11,1,IX,JX)                                                    25JAN00.23
C                 ^  ^ ^  ^                                                      25JAN00.24
C                 |  | |  |  ---- 1=READ HEADER ON DATA SET                      INIT.151
C                 |  | ---------- I,J SIZE OF THIS DOMAIN                        INIT.152
C                 |  ------------ INEST                                          INIT.153
C                 ------------- UNIT OF INITIAL CONDITION FILE                   INIT.154
C                                                                                INIT.155
C                                                                                19DEC02.60
C-----READ IN LOWBDY INFORMATION                                                 19DEC02.61
C MOVED FROM AFTER CALCULATION OF SLAB THERMAL CAPACITY                          19DEC02.62
C FOR CONSISTENCY WITH OPTIONAL SURFACE DATA FILES                               19DEC02.63
C                                                                                19DEC02.64
      CALL LBDYIN(1,TBDYLBE,BDYLTIM,BDYLFRQ,ISOIL,IACTIV,                        19DEC02.65
     -     IFSNOW,IEXSI,IMOIAV,IX,JX)                                            23DEC04.93
C                                                                                19DEC02.67
#ifdef MPP1                                                                      INIT.156
#  include <mpp_init_10.incl>                                                    INIT.157
#endif                                                                           INIT.158
C_FLIC_RUNPAD(2)                                                                 INIT.159
C DEFINE 1/PSTAR                                                                 INIT.160
C                                                                                INIT.161
      DO I=1,IL-1                                                                INIT.162
        DO J=1,JL-1                                                              INIT.163
          RPSA(I,J)=1./PSA(I,J)                                                  INIT.164
          RPSB(I,J)=1./PSB(I,J)                                                  INIT.165
          RPSC(I,J)=RPSA(I,J)                                                    INIT.166
        ENDDO                                                                    INIT.167
      ENDDO                                                                      INIT.168
C                                                                                INIT.169
      CALL DOTS(PSA,PDOTA,IL,JL,MIX,MJX)                                         INIT.170
      DO J=1,JL                                                                  INIT.171
        DO I=1,IL                                                                INIT.172
          RPDOTA(I,J)=1./PDOTA(I,J)                                              INIT.173
          RPDOTB(I,J)=RPDOTA(I,J)                                                INIT.174
          PDOTB(I,J)=PDOTA(I,J)                                                  INIT.175
        ENDDO                                                                    INIT.176
      ENDDO                                                                      INIT.177
C                                                                                INIT.178
C-----INVERT MAPSCALE FACTORS:                                                   INIT.179
C                                                                                INIT.180
      DO 130 I=1,IL                                                              INIT.181
        DO 130 J=1,JL                                                            INIT.182
          MSFD(I,J)=1./MSFD(I,J)                                                 INIT.183
          XMUU(I,J,1)=OMUHF                                                      INIT.184
          XNUU(I,J,1)=GNUHF                                                      INIT.185
          XMUT(I,J,1)=OMUHF                                                      INIT.186
          XNUT(I,J,1)=GNUHF                                                      INIT.187
  130   CONTINUE                                                                 INIT.188
                                                                                 INIT.189
C                                                                                INIT.190
      DO 140 I=1,ILX                                                             INIT.191
        DO 140 J=1,JLX                                                           INIT.192
          MSFX(I,J)=1./MSFX(I,J)                                                 INIT.193
          HT(I,J)=HT(I,J)*G                                                      INIT.194
  140   CONTINUE                                                                 INIT.195
                                                                                 INIT.196
C                                                                                INIT.197
C --- Added Jan 7, 1998                                                          INIT.198
C                                                                                INIT.199
cmic$ do all autoscope                                                           INIT.200
c$doacross                                                                       INIT.201
c$& local(i,j,k)                                                                 INIT.202
c$omp parallel do default(shared)                                                INIT.203
c$omp&private(i,j,k)                                                             INIT.204
      DO J=1,JLX                                                                 INIT.205
        DO I=1,ILX                                                               INIT.206
          IF(I.EQ.1)THEN                                                         INIT.207
            DPSDYM(I,J)=(PSA(I+1,J)-PSA(I,J))/(PSA(I,J)*DX8*MSFX(I,J))           INIT.208
          ELSEIF(I.EQ.ILX)THEN                                                   INIT.209
            DPSDYM(I,J)=(PSA(I,J)-PSA(I-1,J))/(PSA(I,J)*DX8*MSFX(I,J))           INIT.210
          ELSE                                                                   INIT.211
            DPSDYM(I,J)=(PSA(I+1,J)-PSA(I-1,J))/(PSA(I,J)*DX8*MSFX(I,J))         INIT.212
          ENDIF                                                                  INIT.213
        ENDDO                                                                    INIT.214
C                                                                                INIT.215
        IF(J.EQ.1)THEN                                                           INIT.216
          DO I=1,ILX                                                             INIT.217
            DPSDXM(I,J)=(PSA(I,J+1)-PSA(I,J))/(PSA(I,J)*DX8*MSFX(I,J))           INIT.218
          ENDDO                                                                  INIT.219
        ELSEIF(J.EQ.JLX)THEN                                                     INIT.220
          DO I=1,ILX                                                             INIT.221
            DPSDXM(I,J)=(PSA(I,J)-PSA(I,J-1))/(PSA(I,J)*DX8*MSFX(I,J))           INIT.222
          ENDDO                                                                  INIT.223
        ELSE                                                                     INIT.224
          DO I=1,ILX                                                             INIT.225
            DPSDXM(I,J)=(PSA(I,J+1)-PSA(I,J-1))/(PSA(I,J)*DX8*MSFX(I,J))         INIT.226
          ENDDO                                                                  INIT.227
        ENDIF                                                                    INIT.228
C                                                                                INIT.229
        IF(J.NE.1)THEN                                                           INIT.230
          DO I=2,ILX                                                             INIT.231
            DMDY(I,J)=-0.5*(MSFX(I,J)+MSFX(I,J-1)-MSFX(I-1,J)-                   INIT.232
     +                MSFX(I-1,J-1))/(DX*MSFD(I,J)*MSFD(I,J))                    INIT.233
            DMDX(I,J)=-0.5*(MSFX(I,J)+MSFX(I-1,J)-MSFX(I,J-1)-                   INIT.234
     +                MSFX(I-1,J-1))/(DX*MSFD(I,J)*MSFD(I,J))                    INIT.235
          ENDDO                                                                  INIT.236
        ENDIF                                                                    INIT.237
      ENDDO                                                                      INIT.238
#ifdef MPP1                                                                      INIT.239
#  include <mpp_init_10.incl>                                                    INIT.240
#endif                                                                           INIT.241
C                                                                                INIT.242
      DO 142 J=1,JL                                                              INIT.243
        I=1                                                                      INIT.244
        XMUU(I,J,1)=1.                                                           INIT.245
        XNUU(I,J,1)=0.                                                           INIT.246
        XMUT(I,J,1)=1.                                                           INIT.247
        XNUT(I,J,1)=0.                                                           INIT.248
        I=IL                                                                     INIT.249
        XMUU(I,J,1)=1.                                                           INIT.250
        XNUU(I,J,1)=0.                                                           INIT.251
        I=ILX                                                                    INIT.252
        XMUT(I,J,1)=1.                                                           INIT.253
        XNUT(I,J,1)=0.                                                           INIT.254
  142 CONTINUE                                                                   INIT.255
      DO 143 I=2,ILX                                                             INIT.256
        J=1                                                                      INIT.257
        XMUU(I,J,1)=1.                                                           INIT.258
        XNUU(I,J,1)=0.                                                           INIT.259
        XMUT(I,J,1)=1.                                                           INIT.260
        XNUT(I,J,1)=0.                                                           INIT.261
        J=JL                                                                     INIT.262
        XMUU(I,J,1)=1.                                                           INIT.263
        XNUU(I,J,1)=0.                                                           INIT.264
        J=JLX                                                                    INIT.265
        XMUT(I,J,1)=1.                                                           INIT.266
        XNUT(I,J,1)=0.                                                           INIT.267
  143 CONTINUE                                                                   INIT.268
C                                                                                INIT.269
C                                                                                INIT.270
cmic$ do all autoscope                                                           INIT.271
c$doacross                                                                       INIT.272
c$& local(i,j)                                                                   INIT.273
c$omp parallel do default(shared)                                                INIT.274
c$omp&private(i,j)                                                               INIT.275
      DO J=1,JL                                                                  INIT.276
        DO I=1,IL                                                                INIT.277
          WTIJ(I,J,1)=0.                                                         INIT.278
        ENDDO                                                                    INIT.279
      ENDDO                                                                      INIT.280
cmic$ do all autoscope                                                           INIT.281
c$doacross                                                                       INIT.282
c$& local(i,j,wti,wtj)                                                           INIT.283
c$omp parallel do default(shared)                                                INIT.284
c$omp&private(i,j,wti,wtj)                                                       INIT.285
#ifndef MPP1                                                                     INIT.286
      DO J=5,JLX-4                                                               INIT.287
        WTJ=1.0                                                                  INIT.288
        IF(J.LT.8)WTJ=0.25*FLOAT(J-4)                                            INIT.289
        IF(J.GT.JLX-7)WTJ=0.25*FLOAT(JLX-3-J)                                    INIT.290
        DO I=5,ILX-4                                                             INIT.291
          WTI=1.0                                                                INIT.292
          IF(I.LT.8)WTI=0.25*FLOAT(I-4)                                          INIT.293
          IF(I.GT.ILX-7)WTI=0.25*FLOAT(ILX-3-I)                                  INIT.294
          WTIJ(I,J,1)=WTI                                                        INIT.295
          IF(WTI.GT.WTJ)WTIJ(I,J,1)=WTJ                                          INIT.296
        ENDDO                                                                    INIT.297
      ENDDO                                                                      INIT.298
#else                                                                            INIT.299
C_FLIC_RUNPAD(2)                                                                 INIT.300
      DO J=5,JLX-4                                                               INIT.301
        WTJ=1.0                                                                  INIT.302
        IF(J.LT.8)WTJ=0.25*FLOAT(FLIC_L2G_N(J)-4)                                INIT.303
        IF(J.GT.JLX-7)WTJ=0.25*FLOAT(JLX-3-FLIC_L2G_N(J))                        INIT.304
        DO I=5,ILX-4                                                             INIT.305
          WTI=1.0                                                                INIT.306
          IF(I.LT.8)WTI=0.25*FLOAT(FLIC_L2G_M(I)-4)                              INIT.307
          IF(I.GT.ILX-7)WTI=0.25*FLOAT(ILX-3-FLIC_L2G_M(I))                      INIT.308
          WTIJ(I,J,1)=WTI                                                        INIT.309
          IF(WTI.GT.WTJ)WTIJ(I,J,1)=WTJ                                          INIT.310
        ENDDO                                                                    INIT.311
      ENDDO                                                                      INIT.312
C_FLIC_RUNPAD(0)                                                                 INIT.313
#endif                                                                           INIT.314
C                                                                                INIT.315
      ICUT=0                                                                     INIT.316
cmic$ do all autoscope                                                           INIT.317
c$doacross                                                                       INIT.318
c$& local(i,j,nij,in,jn)                                                         INIT.319
c$omp parallel do default(shared)                                                INIT.320
c$omp&private(i,j,nij,in,jn)                                                     INIT.321
      DO NIJ=-6,6                                                                INIT.322
        DO J=1,JLX                                                               INIT.323
#ifndef MPP1                                                                     INIT.324
          JN=J+NIJ                                                               INIT.325
#else                                                                            INIT.326
          JN=FLIC_L2G_N(J+NIJ)                                                   INIT.327
#endif                                                                           INIT.328
          IF(JN.LT.2+ICUT)JN=2+ICUT                                              INIT.329
          IF(JN.GT.JLX-1-ICUT)JN=JLX-1-ICUT                                      INIT.330
          JNSJ(J,NIJ,1)=JN                                                       INIT.331
        ENDDO                                                                    INIT.332
C                                                                                INIT.333
        DO I=1,ILX                                                               INIT.334
#ifndef MPP1                                                                     INIT.335
          IN=I+NIJ                                                               INIT.336
#else                                                                            INIT.337
          IN=FLIC_L2G_M(I+NIJ)                                                   INIT.338
#endif                                                                           INIT.339
          IF(IN.LT.2+ICUT)IN=2+ICUT                                              INIT.340
          IF(IN.GT.ILX-1-ICUT)IN=ILX-1-ICUT                                      INIT.341
          INSI(I,NIJ,1)=IN                                                       INIT.342
        ENDDO                                                                    INIT.343
      ENDDO                                                                      INIT.344
C                                                                                INIT.345
      AMAXROT=-99.                                                               INIT.346
      AMINROT=99.                                                                INIT.347
      DO 145 J=2,JLX                                                             INIT.348
        DO 145 I=2,ILX                                                           INIT.349
          EF(I,J)=0.                                                             INIT.350
          COSROT(I,J)=0.                                                         INIT.351
          SINROT(I,J)=0.                                                         INIT.352
          EX(I,J)=0.                                                             INIT.353
          CRX(I,J)=0.                                                            INIT.354
          SRX(I,J)=0.                                                            INIT.355
          IF(ICOR3D.EQ.1)THEN                                                    INIT.356
C     CALCULATE CONSTANTS FOR 3D CORIOLIS OPTION                                 INIT.357
            XLATD=0.25*(XLAT(I-1,J-1)+XLAT(I-1,J)+XLAT(I,J-1)+XLAT(I,J))         INIT.358
            EF(I,J)=2.*EOMEG*COS(DEGRAD*XLATD)                                   INIT.359
            DLONG=0.5*(XLONG(I,J-1)+XLONG(I,J)-XLONG(I-1,J-1)-                   INIT.360
     +            XLONG(I-1,J))                                                  INIT.361
C     DATELINE                                                                   INIT.362
            IF(DLONG.GT.180.)DLONG=DLONG-360.                                    INIT.363
            IF(DLONG.LT.-180.)DLONG=DLONG+360.                                   INIT.364
            DLAT=0.5*(XLAT(I,J-1)+XLAT(I,J)-XLAT(I-1,J-1)-XLAT(I-1,J))           INIT.365
            IF(ABS(DLAT).LT.1.E-5)DLAT=1.E-5                                     INIT.366
C     ROTATION ANGLE OF NORTH RELATIVE TO Y-AXIS (+VE CLOCKWISE)                 INIT.367
            ROTANG=-ATAN(DLONG/DLAT*COS(DEGRAD*XLATD))                           INIT.368
            IF(DLAT.LT.0.)ROTANG=ROTANG+4.*ATAN(1.)                              INIT.369
            COSROT(I,J)=COS(ROTANG)                                              INIT.370
            SINROT(I,J)=SIN(ROTANG)                                              INIT.371
            AMAXROT=AMAX1(ROTANG,AMAXROT)                                        INIT.372
            AMINROT=AMIN1(ROTANG,AMINROT)                                        INIT.373
          ENDIF                                                                  INIT.374
  145   CONTINUE                                                                 INIT.375
                                                                                 INIT.376
#ifdef MPP1                                                                      INIT.377
#  include <mpp_init_10.incl>                                                    INIT.378
#endif                                                                           INIT.379
      IF(ICOR3D.EQ.1)THEN                                                        INIT.380
cmic$ do all autoscope                                                           INIT.381
c$doacross                                                                       INIT.382
c$& local(i,j,k)                                                                 INIT.383
c$omp parallel do default(shared)                                                INIT.384
c$omp&private(i,j,k)                                                             INIT.385
        DO J=2,JLXM                                                              INIT.386
          DO I=2,ILXM                                                            INIT.387
            EX(I,J)=0.25*(EF(I,J)+EF(I+1,J)+EF(I,J+1)+EF(I+1,J+1))               INIT.388
            CRX(I,J)=0.25*(COSROT(I,J)+COSROT(I+1,J)+COSROT(I,J+1)+              INIT.389
     +               COSROT(I+1,J+1))                                            INIT.390
            SRX(I,J)=0.25*(SINROT(I,J)+SINROT(I+1,J)+SINROT(I,J+1)+              INIT.391
     +               SINROT(I+1,J+1))                                            INIT.392
          ENDDO                                                                  INIT.393
        ENDDO                                                                    INIT.394
        DMAXROT=AMAXROT/DEGRAD                                                   INIT.395
        DMINROT=AMINROT/DEGRAD                                                   INIT.396
C     PRINT *,' FULL 3D CORIOLIS'                                                INIT.397
C     PRINT *,' MAX ROT. ANG. = ',DMAXROT                                        INIT.398
C     PRINT *,' MIN ROT. ANG. = ',DMINROT                                        INIT.399
      ENDIF                                                                      INIT.400
#ifdef MPP1                                                                      INIT.401
#  include <mpp_init_10.incl>                                                    INIT.402
#endif                                                                           INIT.403
C                                                                                INIT.404
C-----COMPUTE THE TOTAL DRY AIR AND WATER SUBSTANCE INITIALLY IN THE             INIT.405
C     DOMAIN:                                                                    INIT.406
C                                                                                INIT.407
      IXM=1                                                                      INIT.408
      JXM=1                                                                      INIT.409
      KXM=1                                                                      INIT.410
      IF(IMOIST(1).EQ.2)THEN                                                     INIT.411
        IXM=IL                                                                   INIT.412
        JXM=JL                                                                   INIT.413
        KXM=KL                                                                   INIT.414
      ENDIF                                                                      INIT.415
#ifndef MPP1                                                                     INIT.416
      CALL TMASS(TDINI,TQINI,TVMASS,TCMASS,TRMASS,IL,JL,KL,IDRY(1),              INIT.417
     +     IMOIST(1),PSA,QVA,QCA,QRA,DSIGMA,DX,G)                                INIT.418
      PRINT150,TDINI,TQINI                                                       INIT.419
  150 FORMAT(' *** INITIAL TOTAL AIR = ',E12.5,' KG, TOTAL WATER = ',            INIT.420
     1       E12.5,' KG IN LARGE DOMAIN.')                                       INIT.421
#endif                                                                           INIT.422
      IF(I4D(1,1).NE.1)GOTO 900                                                  INIT.423
C COMPUTE WXYTOP AND/OR WXYTOPN                                                  INIT.424
      DO 810 I=1,ILX                                                             INIT.425
        DO 810 J=1,JLX                                                           INIT.426
          WXYTOP(I,J)=1.0                                                        INIT.427
C     IF(HT(I,J).LT.0) THEN                                                      INIT.428
C     WXYTOP(I,J)=0.2                                                            INIT.429
C     MAXJ=J+1                                                                   INIT.430
C     MAXJ=MIN0(JLX,MAXJ)                                                        INIT.431
C     MINJ=J-1                                                                   INIT.432
C     MINJ=MAX0(1,MINJ)                                                          INIT.433
C     MAXI=I+1                                                                   INIT.434
C     MAXI=MIN0(ILX,MAXI)                                                        INIT.435
C     MINI=I-1                                                                   INIT.436
C     MINI=MAX0(1,MINI)                                                          INIT.437
C     DO 820 II=MINI,MAXI                                                        INIT.438
C     DO 820 JJ=MINJ,MAXJ                                                        INIT.439
C     IF(HT(II,JJ).GT.0) THEN                                                    INIT.440
C     WXYTOP(I,J)=0.5                                                            INIT.441
C     GO TO 810                                                                  INIT.442
C     ENDIF                                                                      INIT.443
C820  CONTINUE                                                                   INIT.444
C     ENDIF                                                                      INIT.445
  810   CONTINUE                                                                 INIT.446
                                                                                 INIT.447
      DO 830 I=1,IL                                                              INIT.448
        WXYTOP(I,JL)=WXYTOP(I,JLX)                                               INIT.449
  830 CONTINUE                                                                   INIT.450
      DO 840 J=1,JL                                                              INIT.451
        WXYTOP(IL,J)=WXYTOP(ILX,J)                                               INIT.452
  840 CONTINUE                                                                   INIT.453
  900 CONTINUE                                                                   INIT.454
 1000 CONTINUE                                                                   INIT.455
      IF(ISOIL.EQ.2)THEN                                                         INIT.456
       PRINT *,'skip ADJUST NNRP SOIL MOISTURE IN INIT'                          INIT.457
        DO J=1,JLX                                                               INIT.458
          DO I=1,ILX                                                             INIT.459
C COMMENT THE FOLLOWING LINE IF USING THIS ADJUSTMENT                            23DEC04.94
C IF CHANGING THIS ALSO DO THE SAME IN INITNEST                                  23DEC04.95
            GOTO 1001                                                            INIT.460
C                                                                                INIT.461
C            Reduce the high NNRP soil moisture from Month 1-6                   INIT.462
            IF(JULDAY.LE.183)THEN                                                INIT.463
              DO NS=1,MLX                                                        INIT.464
                IF(SMCB(I,J,NS).GT.0.28)THEN                                     INIT.465
                  SMCB(I,J,NS)=MIN(0.7*SMCB(I,J,NS)+0.084,0.35)                  INIT.466
                  SMCA(I,J,NS)=SMCB(I,J,NS)                                      INIT.467
                ENDIF                                                            INIT.468
              ENDDO                                                              INIT.469
            ELSE                                                                 INIT.470
C          Increase the low NNRP soil moisture from Month 7-12                   INIT.471
              DO NS=1,MLX                                                        INIT.472
                IF(SMCB(I,J,NS).LT.0.28)THEN                                     INIT.473
                  IF(SMCB(I,J,NS).GE.0.21)THEN                                   INIT.474
                    SMCB(I,J,NS)=0.429*SMCB(I,J,NS)+0.16                         INIT.475
                    SMCA(I,J,NS)=SMCB(I,J,NS)                                    INIT.476
                  ELSE                                                           INIT.477
                    SMCB(I,J,NS)=1.19*SMCB(I,J,NS)                               INIT.478
                    SMCA(I,J,NS)=SMCB(I,J,NS)                                    INIT.479
                  ENDIF                                                          INIT.480
                ENDIF                                                            INIT.481
              ENDDO                                                              INIT.482
            ENDIF                                                                INIT.483
C         END of NNRP soil moisture adjustment                                   INIT.484
 1001       CONTINUE                                                             INIT.485
          ENDDO                                                                  INIT.486
        ENDDO                                                                    INIT.487
      ENDIF                                                                      INIT.488
C                                                                                19DEC02.68
C SPECIFY SEA ICE BASED ON SST IF IEXSI.EQ.1                                     19DEC02.69
C READ SEA ICE FRACTION FROM LOWBDY FILE IF IEXSI.EQ.2                           19DEC02.70
C                                                                                19DEC02.71
      IF(IEXSI.GE.1)THEN                                                         19DEC02.72
        IF(IEXSI.EQ.1)THEN                                                       19DEC02.73
          print *,'specifying sea ice in subroutine init'                        19DEC02.74
          DO I=1,ILX                                                             19DEC02.75
            DO J=1,JLX                                                           19DEC02.76
              IF((TGA(I,J).LE.271.4).AND.                                        19DEC02.77
     -            NINT(SATBRT(I,J)).EQ.ISWATER)THEN                              19DEC02.78
                SEAICE(I,J)=0.8                                                  19DEC02.79
              else                                                               23DEC04.96
                SEAICE(I,J)=0.0                                                  19DEC02.81
              ENDIF                                                              19DEC02.82
            ENDDO                                                                19DEC02.83
          ENDDO                                                                  19DEC02.84
          PRINT *,'FINISHED SPECIFYING SEAICE BASED ON TG'                       19DEC02.85
        ENDIF                                                                    19DEC02.86
C                                                                                19DEC02.87
C SET DUMMY END VALUES FOR SEAICE AND CHECK FOR ERRORS IN SEAICE INPUT DATA      19DEC02.88
C                                                                                19DEC02.89
        DO I=1,ILX                                                               19DEC02.90
          SEAICE(I,JL)=SEAICE(I,JLX)                                             19DEC02.91
        ENDDO                                                                    19DEC02.92
        DO J=1,JLX                                                               19DEC02.93
          SEAICE(IL,J)=SEAICE(ILX,J)                                             19DEC02.94
        ENDDO                                                                    19DEC02.95
        SEAICE(IL,JL)=SEAICE(ILX,JLX)                                            19DEC02.96
C                                                                                19DEC02.97
C CHECK FOR ERRORS IN SEAICE INPUT DATA                                          19DEC02.98
C ALLOW SEAICE SURFACE TYPE AT WATER GRID POINTS ONLY, WITH SEAICE               19DEC02.99
C FRACTION > 0.02                                                                19DEC02.100
C                                                                                19DEC02.101
        DO I=1,IL                                                                19DEC02.102
          DO J=1,JL                                                              19DEC02.103
            IF((SEAICE(I,J).GT.1.001).OR.                                        19DEC02.104
     +       (SEAICE(I,J).LT.-0.001))THEN                                        19DEC02.105
              WRITE(6,*) 'SEA ICE INITIAL ERROR AT I, J',I,', ',J                19DEC02.106
              STOP 9753                                                          19DEC02.107
            ENDIF                                                                19DEC02.108
            IF(NINT(SATBRT(I,J)).NE.ISWATER.OR.                                  19DEC02.109
     -           (SEAICE(I,J).LE.0.02))THEN                                      19DEC02.110
              SEAICE(I,J)=0.0                                                    19DEC02.111
            ENDIF                                                                19DEC02.112
C                                                                                19DEC02.113
C INITIALIZE SEAICE GROUND TEMPERATURE (TGSI)                                    19DEC02.114
C                                                                                19DEC02.115
            IF((SEAICE(I,J).GT.0.02))THEN                                        19DEC02.116
C ASSUME INITIAL GROUND TEMPERATURE (TGA) REPRESENTS TEMPERATURE OVER SEAICE     19DEC02.117
              TGSI(I,J)=TGA(I,J)                                                 19DEC02.118
C MODIFY TGA (AND TGB) TO REPRESENT AREA AVERAGE OF SEAICE GROUND TEMPERATURE    19DEC02.119
C AND OCEAN WATER TEMPERATURE AT FREEZING POINT (271.4 K)                        19DEC02.120
              TGA(I,J)=SEAICE(I,J)*TGA(I,J)+(1.-SEAICE(I,J))*271.4               19DEC02.121
              TGB(I,J)=TGA(I,J)                                                  19DEC02.122
            ELSE                                                                 19DEC02.123
              TGSI(I,J)=-1.*TGA(I,J)                                             19DEC02.124
            ENDIF                                                                19DEC02.125
          ENDDO                                                                  19DEC02.126
        ENDDO                                                                    19DEC02.127
      ELSE                                                                       19DEC02.128
C                                                                                19DEC02.129
C IEXSI.LT.1                                                                     19DEC02.130
C                                                                                19DEC02.131
        DO I=1,IL                                                                19DEC02.132
          DO J=1,JL                                                              19DEC02.133
            SEAICE(I,J)=0.0                                                      19DEC02.134
            TGSI(I,J)=0.0                                                        19DEC02.135
          ENDDO                                                                  19DEC02.136
        ENDDO                                                                    19DEC02.137
      ENDIF                                                                      19DEC02.138
C                                                                                INIT.489
C-----SPECIFY SURFACE PARAMETERS:                                                INIT.490
C                                                                                INIT.491
C.....IF ISFPAR=0 AND ITGFLG=1, SPECIFY THE ROUGHNESS LENGTH OVER                INIT.492
C     LAND (ZZLND) AND WATER (ZZWTR) IN METERS, THE ALBEDO (ALBLND)              INIT.493
C     AS A DECIMAL FRACTION OF ONE, THE SURFACE THERMAL INERTIAL                 INIT.494
C     (THINLD) IN CAL-1 CM-2 K-1 S-0.5.                                          INIT.495
C     ROUGHNESS LENGTH OVER WATER IS A FUNCTION OF USTAR. THUS, ZZWTR            INIT.496
C     IS A BASE STATE VALUE REPRESENTATIVE OF NO WIND (0.0001 M).                INIT.497
C     XMAVA IS THE MOISTURE AVAILABILITY OVER LAND AS A DECIMAL                  INIT.498
C     FRACTION OF ONE.                                                           INIT.499
C                                                                                INIT.500
      IF(ISFPAR(1).EQ.1)GOTO 170                                                 INIT.510
      DO 160 I=1,ILX                                                             INIT.512
        DO 160 J=1,JLX                                                           INIT.513
          SATBRT(I,J)=0.                                                         INIT.514
          ALB(I,J)=ALBLND                                                        INIT.515
          THC(I,J)=THINLD                                                        INIT.516
          EMISS(I,J)=1.0                                                         INIT.517
          IF(HT(I,J)-1.E-6.GE.0)THEN                                             INIT.518
            ZNT(I,J)=ZZLND                                                       INIT.519
          ELSE                                                                   INIT.520
            ZNT(I,J)=ZZWTR                                                       INIT.521
          ENDIF                                                                  INIT.522
          IF(HT(I,J)-1.E-6.GE.0)THEN                                             INIT.523
            XLAND(I,J)=1.0                                                       INIT.524
          ELSE                                                                   INIT.525
            XLAND(I,J)=2.0                                                       INIT.526
          ENDIF                                                                  INIT.527
          IF(HT(I,J)-1.E-6.GE.0)THEN                                             INIT.528
            MAVAIL(I,J)=XMAVA                                                    INIT.529
          ELSE                                                                   INIT.530
            MAVAIL(I,J)=1.0                                                      INIT.531
          ENDIF                                                                  INIT.532
  160   CONTINUE                                                                 INIT.533
                                                                                 INIT.534
      GOTO 190                                                                   INIT.535
C                                                                                INIT.536
C.....IF ISFPAR=1, SPECIFY THE SURFACE PARAMETERS ACCORDING TO THE               INIT.537
C     SURFACE INDEX READ IN FROM MMINPUT FILE                                    INIT.538
C                                                                                INIT.539
  170 CONTINUE                                                                   INIT.540
      DO 180 I=1,ILX                                                             INIT.541
        DO 180 J=1,JLX                                                           INIT.542
          IS=INT(SATBRT(I,J)+0.001)                                              INIT.543
          IF(IS.LT.0.OR.IS.GT.LUN)THEN                                           INIT.544
            PRINT *,'ERROR: LANDUSE OUTSIDE RANGE =',IS,' AT ',I,J               INIT.545
            STOP 'INIT'                                                          INIT.546
          ENDIF                                                                  INIT.547
C   SET NO-DATA POINTS (IS=0) TO WATER                                           INIT.548
          IF(IS.EQ.0)THEN                                                        INIT.549
            IS=ISWATER                                                           INIT.550
          ENDIF                                                                  INIT.551
          ALB(I,J)=ALBD(IS,ISN)/100.                                             INIT.552
          THC(I,J)=THERIN(IS,ISN)/100.                                           INIT.553
          IF(ISOIL.NE.3) ZNT(I,J)=SFZ0(IS,ISN)/100.                              07NOV00.69
          EMISS(I,J)=SFEM(IS,ISN)                                                INIT.555
          IF(IMOIAV(1).NE.2 .OR. IS.EQ.ISWATER) MAVAIL(I,J)=SLMO(IS,ISN)         07NOV00.70
          IF(IS.NE.ISWATER)THEN                                                  INIT.557
            XLAND(I,J)=1.0                                                       INIT.558
          ELSE                                                                   INIT.559
            XLAND(I,J)=2.0                                                       INIT.560
          ENDIF                                                                  INIT.561
          IF(SEAICE(I,J).GT.0.02)THEN                                            19DEC02.139
C                                                                                19DEC02.140
C CALCULATE AVERAGE SURFACE PROPERTIES FOR SEAICE GRID POINTS AS SIMPLE AVERAGE  19DEC02.141
C OF SEAICE SURFACE PROPERTIES AND WATER SURFACE PROPERTIES                      19DEC02.142
C                                                                                19DEC02.143
            ALB(I,J)=ALBD(ISICE,ISN)/100.*SEAICE(I,J)+                           19DEC02.144
     +        ALBD(ISWATER,ISN)/100.*(1.-SEAICE(I,J))                            19DEC02.145
            THC(I,J)=THERIN(ISICE,ISN)/100.                                      19DEC02.146
c sea-ice roughness=0.1 cm                                                       19DEC02.147
            ZNT(I,J)=0.1/100.                                                    19DEC02.148
            EMISS(I,J)=SFEM(ISICE,ISN)*SEAICE(I,J)+                              19DEC02.149
     +        SFEM(ISWATER,ISN)*(1.-SEAICE(I,J))                                 19DEC02.150
            MAVAIL(I,J)=SLMO(ISICE,ISN)*SEAICE(I,J)+                             19DEC02.151
     +        SLMO(ISWATER,ISN)*(1.-SEAICE(I,J))                                 19DEC02.152
C TREAT SEAICE GRID POINTS AS LAND GRID POINTS                                   19DEC02.153
            XLAND(I,J)=1.0                                                       19DEC02.154
          ENDIF                                                                  19DEC02.155
  180   CONTINUE                                                                 INIT.562
                                                                                 INIT.563
C                                                                                INIT.564
C-----MODIFY THE SURFACE CHARACTERISTICS IF THE SNOW COVER EFFECTS ARE           INIT.565
C     CONSIDERED:                                                                INIT.566
C                                                                                INIT.567
      IF(IFSNOW(1).EQ.0.AND.ISOIL.NE.2)GOTO 190                                  INIT.568
C     FOR LSM SNOWC IS SET ACCORDING TO WEASD IN INPUT (SNOA > 0.1 MM)           24SEP99.27
      DO 185 I=1,ILX                                                             INIT.570
        DO 185 J=1,JLX                                                           INIT.571
          IS=INT(SATBRT(I,J)+0.001)                                              INIT.572
C         comment following line if snowc contains sea-ice (and in initn         INIT.573
          IF(XLAND(I,J).GT.1.5)SNOWC(I,J)=-0.01                                  INIT.574
          IF(SNOWC(I,J).GT.0.)THEN                                               INIT.575
            ALB(I,J)=ALB(I,J)+SCFX(IS)*ALB(I,J)                                  INIT.576
            THC(I,J)=THERIN(ISICE,ISN)/100.                                      INIT.577
            EMISS(I,J)=SFEM(ISICE,ISN)                                           INIT.578
            MAVAIL(I,J)=SLMO(ISICE,ISN)                                          INIT.579
          ENDIF                                                                  INIT.580
C     uncomment these lines if snowc contains sea-ice (and in initnest)          25JAN00.25
C           IF(XLAND(I,J).GT.1.5.AND.SNOWC(I,J).GT.0.5)THEN                      25JAN00.26
C             XLAND(I,J)=1.0                                                     INIT.583
C             PRINT *,'SNOWC OVER WATER ASSUMED TO BE SEA ICE AT ',I,J           INIT.584
C           ENDIF                                                                INIT.585
          IF(ISOIL.EQ.2.AND.XICE(I,J).GT.0.5)THEN                                INIT.586
C         SEA ICE ALBEDO AND ROUGHNESS                                           INIT.587
            ALB(I,J)=ALBD(ISICE,ISN)/100.                                        INIT.588
            ZNT(I,J)=SFZ0(ISICE,ISN)/100.                                        INIT.589
C         SEA ICE WILL HAVE XLAND=1 (LAND VALUE)                                 INIT.590
            XLAND(I,J)=1.0                                                       INIT.591
          ENDIF                                                                  INIT.592
C  NEW NOAH ALBEDO CALCULATION                                                   19DEC02.156
          IF(ISOIL.EQ.2)THEN                                                     19DEC02.157
C                                                                                19DEC02.158
            IF(RDMAXALB) THEN                                                    19DEC02.159
              SNOALB=ALBMAX(I,J)*0.01                                            19DEC02.160
            ELSE                                                                 19DEC02.161
              SNOALB=MAXALB(IS)*0.01                                             19DEC02.162
            ENDIF                                                                19DEC02.163
C                                                                                19DEC02.164
            IF(RDBRDALB) THEN                                                    19DEC02.165
              ALBBRD=ALBEDO(I,J)*0.01                                            19DEC02.166
            ELSE                                                                 19DEC02.167
              ALBBRD=ALBD(IS,ISN)*0.01                                           19DEC02.168
            ENDIF                                                                19DEC02.169
C                                                                                19DEC02.170
            SHDFAC = VEGFRC(I,J)/100.                                            19DEC02.171
            SHDMIN = XVEGMN(I,J)/100.                                            19DEC02.172
            SNCOVR = SNOWC(I,J)                                                  19DEC02.173
C                                                                                19DEC02.174
            ALB(I,J) = ALBBRD + (1.0- (SHDFAC - SHDMIN))* SNCOVR *               19DEC02.175
     -                 (SNOALB - ALBBRD)                                         19DEC02.176
                                                                                 19DEC02.177
          ENDIF                                                                  19DEC02.178
  185   CONTINUE                                                                 INIT.593
                                                                                 INIT.594
  190 CONTINUE                                                                   07NOV00.71
C                                                                                07NOV00.72
C-----CALCULATE SLAB THERMAL CAPACITY CAPG(I,J): IT ARE DEPENDENT ON             07NOV00.73
C     THC(I,J) - SOIL THERMAL INERTIAL, ONLY.                                    07NOV00.74
C                                                                                07NOV00.75
      DO 250 I=1,ILX                                                             07NOV00.76
        DO 250 J=1,JLX                                                           07NOV00.77
           CAPG(I,J)=3.298E6*THC(I,J)                                            07NOV00.78
           IF(ISOIL.EQ.1)THEN                                                    07NOV00.79
C-----CAPG REPRESENTS SOIL HEAT CAPACITY (J/K/M^3) WHEN DIFSL=5.E-7 (M^2/S)      07NOV00.80
C     TO GIVE A CORRECT THERMAL INERTIA (=CAPG*DIFSL^0.5)                        07NOV00.81
          IF(NINT(SATBRT(I,J)).EQ.ISICE)THEN                                     19DEC02.179
C              BASED UPON YEN (1981)                                             19DEC02.180
C              DENSITY * SPECIFIC HEAT / MOL / K / MOLECULAR WEIGHT              19DEC02.181
            CAPG(I,J)=917.*34000./18.                                            19DEC02.182
          ELSEIF(SEAICE(I,J).GT.0.02)THEN                                        19DEC02.183
C              DENSITY * SPECIFIC HEAT / KG / K                                  19DEC02.184
            CAPG(I,J)=900.*3400.                                                 19DEC02.185
          ELSE                                                                   19DEC02.186
             CAPG(I,J)=5.9114E7*THC(I,J)                                         07NOV00.82
          ENDIF                                                                  19DEC02.187
           ENDIF                                                                 07NOV00.83
  250   CONTINUE                                                                 07NOV00.84
cJJC - moved call to LBDYIN after call to RDINIT                                 19DEC02.188
C                                                                                19DEC02.189
C SET TMN TO FREEZING POINT OF SEA WATER (271.4 K) FOR SEAICE GRID POINTS        19DEC02.190
C SET SNOWC TO 1 FOR SEAICE AND PERMANENT ICE GRID POINTS                        19DEC02.191
C                                                                                19DEC02.192
      DO I=1,IL                                                                  19DEC02.193
        DO J=1,JL                                                                19DEC02.194
          IF(SEAICE(I,J).GT.0.02.AND.(IEXSI.GE.1))THEN                           19DEC02.195
            TMN(I,J)=271.4                                                       19DEC02.196
            SNOWC(I,J)=1.0                                                       19DEC02.197
          ENDIF                                                                  19DEC02.198
          IF(NINT(SATBRT(I,J)).EQ.ISICE)THEN                                     19DEC02.199
            SNOWC(I,J)=1.0                                                       19DEC02.200
          ENDIF                                                                  19DEC02.201
        ENDDO                                                                    19DEC02.202
      ENDDO                                                                      19DEC02.203
      IF(MLX.NE.1)THEN                                                           INIT.733
        IF(ISOIL.EQ.1)THEN                                                       INIT.734
C                                                                                INIT.735
C-----INITIALIZE SOIL TEMPERATURE PROFILE (LINEAR WITH DEPTH)                    INIT.736
C                                                                                INIT.737
          DO L=1,MLX                                                             INIT.738
            DO J=1,JLX                                                           INIT.739
              DO I=1,ILX                                                         INIT.740
C CHECK TO SEE IF TSLA ALREADY INITIALIZED FROM INITIAL DATA BEFORE SETTING      25JAN00.54
               IF(TSLA(I,J,L).LE.1.0)THEN                                        25JAN00.55
                TSLA(I,J,L)=(TGA(I,J)*(ZS(MLX)-ZS(L))+TMN(I,J)*(ZS(L)-           INIT.741
     +                      ZS(1)))/(ZS(MLX)-ZS(1))                              INIT.742
                TSLB(I,J,L)=(TGB(I,J)*(ZS(MLX)-ZS(L))+TMN(I,J)*(ZS(L)-           INIT.743
     +                      ZS(1)))/(ZS(MLX)-ZS(1))                              INIT.744
               ENDIF                                                             24SEP99.29
              ENDDO                                                              INIT.745
            ENDDO                                                                INIT.746
          ENDDO                                                                  INIT.747
        ENDIF                                                                    INIT.748
      ENDIF                                                                      INIT.749
C  CORRECT GROUND TEMPERATURE IN SNOW LAYERS (FACTOR 0.01 CONVERTS TO M)         05DEC01.61
      IF(IFSNOW(1).EQ.2.AND.ISOIL.EQ.1)THEN                                      05DEC01.62
      DO J=1,JLX                                                                 05DEC01.63
        DO I=1,ILX                                                               05DEC01.64
          IF(SNOWC(I,J)*0.01.GT.ZS(1).AND.TSLA(I,J,1).GT.273.16)THEN             05DEC01.65
             TGA(I,J)=273.16                                                     05DEC01.68
             TGB(I,J)=273.16                                                     05DEC01.69
          ENDIF                                                                  05DEC01.70
        ENDDO                                                                    05DEC01.87
      ENDDO                                                                      05DEC01.88
      DO L=1,MLX                                                                 23DEC04.97
        DO J=1,JLX                                                               23DEC04.98
          DO I=1,ILX                                                             23DEC04.99
          IF(SNOWC(I,J)*0.01.GT.ZS(L).AND.TSLA(I,J,L).GT.273.16)THEN             23DEC04.100
             TSLA(I,J,L)=273.16                                                  23DEC04.101
             TSLB(I,J,L)=273.16                                                  23DEC04.102
          ENDIF                                                                  23DEC04.103
          ENDDO                                                                  23DEC04.104
        ENDDO                                                                    23DEC04.105
      ENDDO                                                                      23DEC04.106
      ENDIF                                                                      05DEC01.89
C                                                                                INIT.750
C-----SET RAINC AND RAINNC EQUAL TO 0. INITIALLY                                 INIT.751
C                                                                                INIT.752
      DO 280 I=1,ILX                                                             INIT.753
        DO 280 J=1,JLX                                                           INIT.754
          RAINC(I,J)=0.                                                          INIT.755
          RAINNC(I,J)=0.                                                         INIT.756
          RAINP(I,J)=0.                                                          INIT.757
          IF(IKFFC.EQ.1)then                                                     05DEC01.90
            NCA(I,J)=-100                                                        05DEC01.91
            NCAD(I,J)=-100                                                       05DEC01.92
            TNCA(i,j)=0.                                                         05DEC01.93
            PSRC(I,J)=0.                                                         05DEC01.94
            PCLB(I,J)=0.                                                         05DEC01.95
            UMFB(I,J)=0.                                                         05DEC01.96
            SPSRC(I,J)=0.                                                        05DEC01.97
            SPCLB(I,J)=0.                                                        05DEC01.98
            SUMFB(I,J)=0.                                                        05DEC01.99
            CIN(I,J)=0.                                                          05DEC01.100
          ENDIF                                                                  05DEC01.101
  280   CONTINUE                                                                 INIT.759
                                                                                 INIT.760
      IF(IKFFC.EQ.1)THEN                                                         INIT.761
        DO 286 I=1,MIX                                                           INIT.762
          DO 286 J=1,MJX                                                         INIT.763
            RAINCV(I,J)=0.                                                       INIT.764
            DO 286 K=1,KL                                                        INIT.765
              DTDT(I,J,K)=0.                                                     INIT.766
              DQDT(I,J,K)=0.                                                     INIT.767
              DUDT(I,J,K)=0.                                                     INIT.768
              DVDT(I,J,K)=0.                                                     INIT.769
              DQRDT(I,J,K)=0.                                                    INIT.770
              DQLDT(I,J,K)=0.                                                    INIT.771
              DQSDT(I,J,K)=0.                                                    INIT.772
              DQIDT(I,J,K)=0.                                                    INIT.773
              W0AVG(I,J,K)=0.                                                    INIT.774
  286       CONTINUE                                                             INIT.775
                                                                                 INIT.776
                                                                                 INIT.777
      ENDIF                                                                      INIT.778
C                                                                                INIT.779
C-----INITIALIZE THE OLD MITER STEPS TO 1 AND STORE IN SATBRT(1,J):              INIT.780
C                                                                                INIT.781
C     DO 285 J=1,JLX                                                             INIT.782
CCCC    SATBRT(1,J)=SATBRT(1,J)+100.                                             INIT.783
C 285 CONTINUE                                                                   INIT.784
C                                                                                INIT.785
  590 FORMAT(' ***** RESTART FILE FOR LARGE DOMAIN AT TIME = ',F6.0,             INIT.786
     1       ' MINUTES, KTAU = ',I4,'  READ IN FROM UNIT ',I2,'.'/)              INIT.787
C                                                                                INIT.788
  600 FORMAT(' ***** RESTART FILE FOR NESTED DOMAIN AT TIME = ',F6.0,            INIT.789
     1       ' MINUTES, KTAUN = ',I4,'  READ IN FROM UNIT ',I2,'.'/)             INIT.790
  610 CONTINUE                                                                   INIT.791
  620 CONTINUE                                                                   INIT.792
C                                                                                INIT.793
C IF OBS NUDGING ON ANY MESH, IN4DOB MUST BE CALLED HERE...                      INIT.794
C                                                                                INIT.795
#ifdef FDDAOBS1                                                                  INIT.796
      IF(I4DITOT.GE.1)CALL IN4DOB(1,XTIME,KTAU,KTAUR,DTMIN,JULDAY,GMT)           INIT.797
#endif                                                                           INIT.798
#ifdef FDDAGD1                                                                   INIT.799
      IF(I4D(1,1).EQ.1)CALL IN4DGD(1,IL,JL,KL)                                   INIT.800
#endif                                                                           INIT.801
      CALL ADDRX1C(IAXALL(1,1))                                                  INIT.802
#ifdef MPP1                                                                      INIT.803
      IF(IRDDIM.EQ.1)THEN                                                        INIT.804
        DO K=1,MKXR                                                              INIT.805
          DO J=1,MJXR                                                            INIT.806
            DO I=1,MIXR                                                          INIT.807
              RTTEN(I,J,K)=0.                                                    INIT.808
            ENDDO                                                                INIT.809
          ENDDO                                                                  INIT.810
        ENDDO                                                                    INIT.811
      ENDIF                                                                      INIT.812
#endif                                                                           INIT.813
C                                                                                INIT.818
C-----READ IN THE BOUNDARY CONDITIONS FOR LARGE DOMAIN:                          INIT.819
C                                                                                INIT.820
      CALL BDYIN(9,TBDYBE,BDYTIM,BDYFRQ,IL,JL,IBMOIST)                           19DEC02.204
C                                                                                07NOV00.88
C-----OUTPUT THE FIRST TIME PERIOD DATA                                          07NOV00.89
C                                                                                07NOV00.90
      IF(IFTAPE.EQ.1)CALL OUTTAP(41,1,IL,JL,MKX,1)                               07NOV00.91
#ifndef MPP1                                                                     07NOV00.92
      IF(IFPRT.EQ.1)CALL OUTPRT(IEXEC(1),1)                                      07NOV00.93
#endif                                                                           07NOV00.94
C                                                                                INIT.822
C-----COMPUTE THE SOLAR DECLINATION ANGLE:                                       INIT.823
C                                                                                INIT.824
C     NO CONDITION ON ITGFLG                                                     INIT.825
      CALL SOLAR1(XTIME,JULDAY,GMT,1)                                            INIT.826
C                                                                                07NOV00.95
C-----INITIALIZE LOCATIONS FOR TIME SERIES OUTPUT                                07NOV00.96
C                                                                                07NOV00.97
      IF (IFTSOUT) CALL INITTS(1,IL,JL)                                          07NOV00.98
C                                                                                INIT.827
C--- INITIALIZE NESTS                                                            INIT.828
C                                                                                INIT.829
#ifdef NESTED                                                                    INIT.830
      IF(MAXNES.GT.1)THEN                                                        INIT.831
        DO 998 L=2,MAXNES                                                        INIT.832
          CALL CHKNST(IEXEC,L)                                                   INIT.833
  998   CONTINUE                                                                 INIT.834
      ENDIF                                                                      INIT.835
#endif                                                                           INIT.836
      CALL ADDRX1C(IAXALL(1,1))                                                  INIT.837
C                                                                                INIT.838
C-----SET UP OUTPUT TIME:                                                        INIT.839
C                                                                                INIT.840
      SAVTIM=SAVFRQ                                                              INIT.841
      TAPTIM=TAPFRQ                                                              INIT.842
      PRTTIM=PRTFRQ                                                              INIT.843
  640 IF(NINT(SAVTIM/DTMIN).GT.KTAU)GOTO 660                                     07NOV00.99
      SAVTIM=SAVTIM+SAVFRQ                                                       INIT.845
      GOTO 640                                                                   INIT.846
  660 IF(NINT(TAPTIM/DTMIN).GT.KTAU)GOTO 670                                     07NOV00.100
      TAPTIM=TAPTIM+TAPFRQ                                                       INIT.848
      GOTO 660                                                                   INIT.849
  670 IF(NINT(PRTTIM/DTMIN).GT.KTAU)GOTO 710                                     07NOV00.101
      PRTTIM=PRTTIM+PRTFRQ                                                       INIT.851
      GOTO 670                                                                   INIT.852
  710 IF(DECTIM.GT.XTIME)GOTO 720                                                INIT.853
      DECTIM=DECTIM+1440.                                                        INIT.854
      GOTO 710                                                                   INIT.855
C                                                                                INIT.856
  720 CONTINUE                                                                   INIT.857
c...KF2 mods...                                                                  05DEC01.102
      TIMKFDIAG =FRQKFDIAG                                                       05DEC01.103
  725 IF(NINT(TIMKFDIAG/DTMIN).GT.KTAU)GOTO 726                                  05DEC01.104
      TIMKFDIAG=TIMKFDIAG+FRQKFDIAG                                              05DEC01.105
      GOTO 725                                                                   05DEC01.106
  726 CONTINUE                                                                   05DEC01.107
      TIMKFDIAG=TIMKFDIAG-0.000001                                               05DEC01.108
c...end of KF2 mods                                                              05DEC01.109
C                                                                                INIT.862
#ifdef MPP1                                                                      INIT.863
#  include <mpp_init_10.incl>                                                    INIT.864
#endif                                                                           INIT.865
      RETURN                                                                     INIT.866
C                                                                                INIT.867
9007  PRINT *,'ERROR IN READING SUBSTRATE TEMP FILE.'                            INIT.868
      PRINT *,'CHECK IF IT IS THE RIGHT FILE'                                    INIT.869
      STOP '9007 IN INIT'                                                        INIT.870
9008  PRINT *,                                                                   INIT.871
     & 'INPUT SUBSTRATE TEMP FILE EMPTY. CHECK IF THE FILE EXISTS'               INIT.872
      STOP '9008 IN INIT'                                                        INIT.873
9010  PRINT *,'ERROR OPENING SUBSTRATE TEMP FILE'                                25JAN00.56
      STOP '9010 IN INIT'                                                        25JAN00.57
      END                                                                        INIT.874
                                                                                 INIT.875
