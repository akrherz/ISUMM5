      SUBROUTINE INITSAV(IEXEC)                                                  INITSAV.1
#     include <parame.incl>                                                      INITSAV.2
#     include <param2.incl>                                                      INITSAV.3
#     include <param3.incl>                                                      INITSAV.4
#     include <pmoist.incl>                                                      INITSAV.5
#     include <addr0.incl>                                                       INITSAV.6
#     include <various.incl>                                                     INITSAV.7
#     include <variousn.incl>                                                    INITSAV.8
#     include <nestl.incl>                                                       INITSAV.9
#     include <dusolve1.incl>                                                    INITSAV.10
#     include <nhcnst.incl>                                                      INITSAV.11
#     include <defines.incl>                                                     INITSAV.12
#     include <rpstar.incl>                                                      INITSAV.13
#     include <point2d.incl>                                                     INITSAV.14
#     include <uprad.incl>                                                       INITSAV.15
#     include <addrcu.incl>                                                      INITSAV.16
#     include <zdiffu.incl> 
      INTEGER IEXEC(MAXNES)                                                      INITSAV.17
#ifdef MPP1                                                                      INITSAV.18
#  include <mpp_initsav_00.incl>                                                 INITSAV.19
#endif                                                                           INITSAV.20
      REAL DTCOARSE,DTTMP                                                        07NOV00.322
C
C   -----SET INITIAL VARIABLES FOR ITPDIF=2  
        oldnest = 0                         
        diffpar = .true.    
C                                                                                INITSAV.21
C--- READ SAVDATA                                                                INITSAV.22
C                                                                                INITSAV.23
      XTIMR=FLOAT(IXTIMR)                                                        07NOV00.323
   65 CONTINUE                                                                   INITSAV.24
      NSTTOT=0                                                                   INITSAV.25
      DO 66 LLN=1,MAXNES                                                         INITSAV.26
        IF(IACTIV(LLN).EQ.0)THEN                                                 INITSAV.27
          IF(LLN.EQ.1)THEN                                                       INITSAV.28
            PRINT *,'IACTIV(1) RESET TO 1'                                       INITSAV.29
            IACTIV(1)=1                                                          INITSAV.30
          ELSE                                                                   INITSAV.31
            IF(XTIMR.GT.XSTNES(LLN)+1..AND.XTIMR.LT.XENNES(LLN))THEN             07NOV00.324
              PRINT *,'IACTIV(',LLN,') INCONSISTENT WITH XSTNES/XENNES'          INITSAV.33
              PRINT *,'IACTIV(',LLN,') RESET TO 1'                               INITSAV.34
              IACTIV(LLN)=1                                                      INITSAV.35
            ENDIF                                                                INITSAV.36
          ENDIF                                                                  INITSAV.37
        ELSEIF(IACTIV(LLN).EQ.1)THEN                                             INITSAV.38
          IF(XTIMR.LT.XSTNES(LLN).OR.XTIMR.GT.XENNES(LLN)+10.)THEN               07NOV00.325
            PRINT *,'IACTIV(',LLN,') INCONSISTENT WITH XSTNES/XENNES'            INITSAV.40
            PRINT *,'IACTIV(',LLN,') RESET TO 0'                                 INITSAV.41
            IACTIV(LLN)=0                                                        INITSAV.42
          ENDIF                                                                  INITSAV.43
        ENDIF                                                                    INITSAV.44
        IF(IACTIV(LLN).EQ.0)GOTO 66                                              INITSAV.45
        NSTTOT=NSTTOT+1                                                          INITSAV.46
        IF (LLN.EQ.1) DTCOARSE = DT2                                             07NOV00.326
        DTTMP = DTCOARSE/IRAX**LEVIDN(LLN)                                       07NOV00.327
   67   CONTINUE                                                                 INITSAV.47
#ifndef MPP1                                                                     INITSAV.48
        IUTSAV=90+LLN                                                            INITSAV.49
        CALL ADDRX1C(IAXALL(1,LLN))                                              INITSAV.50
        CALL SAVREAD(ALLARR(1,LLN),IRHUGE,INTALL(1,LLN),IIHUGE,                  INITSAV.51
     +       ALLFG(1,LLN),IFGHUG,INTFG(1,LLN),IFGIHUG,ALLFO(1,LLN),              INITSAV.52
     +       IFOHUG,IUTSAV)                                                      07NOV00.328
#else                                                                            INITSAV.54
#  include <mpp_initsav_10.incl>                                                 INITSAV.55
#endif                                                                           INITSAV.56
        IEXEC(LLN)=2                                                             INITSAV.57
        IXTIME=NINT(XTIME)                                                       07NOV00.329
        PRINT 910, LLN,XTIME,KTAU                                                07NOV00.330
910   FORMAT(1X,'READING SAVE FILE FOR DOMAIN ',I2,                              07NOV00.331
     1      ' AT XTIME=',F10.2,' AND KTAU=',I8)                                  07NOV00.332
#ifndef MPP1                                                                     INITSAV.61
        IF(IXTIME.NE.IXTIMR)GOTO 67                                              INITSAV.62
        CLOSE (IUTSAV)                                                           23DEC04.228
#else                                                                            INITSAV.63
#  include <mpp_initsav_20.incl>                                                 INITSAV.64
#endif                                                                           INITSAV.65
C                                                                                07NOV00.333
C ..... IF THE CODE DETECTS THE TIME STEP IS DEFINED DIFFERENTLY AT THE          07NOV00.334
C          RESTART, IT WILL CHANGE DT, DT2, AND DTMIN.                           07NOV00.335
C          THERE ARE OTHER CONSTANTS USED IN THE MODEL THAT DEPEND ON            07NOV00.336
C          TIME STEP. BUT FOR NOW WE ARE KEEPING THEM UNCHANGED WHEN             07NOV00.337
C          TIME STEP CHANGES. THESE ARE: GNUDGE, XKHMAX, XKHZ.                   07NOV00.338
C                                                                                07NOV00.339
        IF (ABS(DT-DTTMP).GE.0.001) THEN                                         07NOV00.340
          KTAU = NINT(DT/DTTMP*FLOAT(KTAU))                                      07NOV00.341
          PRINT 900, DT/2., DTTMP/2., KTAU                                       07NOV00.342
          DT = DTTMP                                                             07NOV00.343
          DT2 = DTTMP                                                            07NOV00.344
          DTMIN = DTTMP/120.                                                     07NOV00.345
        END IF                                                                   07NOV00.346
900   FORMAT(1X,'*** RESET TIME STEP AT RESTART: PREVIOUS TISTEP = ',            07NOV00.347
     +       F4.0,', NEW TISTEP = ',F4.0,', NEW KTAU = ',I8)                     07NOV00.348
C                                                                                INITSAV.66
#ifdef NESTED                                                                    INITSAV.67
        IF(MAXNES.GT.1)THEN                                                      INITSAV.68
          IF(LLN.GT.1)THEN                                                       INITSAV.69
C                                                                                INITSAV.70
C OVERWRITE THE NESTI,NESTJ:                                                     INITSAV.71
            CALL ADDRX1N(IAXALL(1,LLN))                                          INITSAV.72
            NESTI(LLN)=ISOUTH                                                    INITSAV.73
            NESTJ(LLN)=JWEST                                                     INITSAV.74
            IF(IMOVE(LLN).GT.0) PRINT *,'NESTI,NESTJ,IMOVCO:',                   07NOV00.349
     1          NESTI(LLN), NESTJ(LLN), IMOVCO(LLN)                              INITSAV.76
            IF(IMOVE(LLN).GT.0) PRINT*,'INCREMENT (IMOVEI,IMOVEJ)=',             07NOV00.350
     +        IMOVEI(LLN,IMOVCO(LLN)),IMOVEJ(LLN,IMOVCO(LLN))                    INITSAV.78
C                                                                                INITSAV.79
            NUMLV(LEVIDN(LLN),NUMNC(LLN))=NUMLV(LEVIDN(LLN),NUMNC(LLN))+         INITSAV.80
     +                                    1                                      INITSAV.81
          ENDIF                                                                  INITSAV.82
        ENDIF                                                                    INITSAV.83
#endif                                                                           INITSAV.84
        KTAUR=KTAU                                                               INITSAV.85
        NTRAD(LLN)=NINT(RADFRQ/DTMIN)                                            INITSAV.86
#ifdef MPP1                                                                      INITSAV.87
        CALL MP_INITDOMAIN(1)                                                    INITSAV.88
#endif                                                                           INITSAV.89
        CALL ADDRX1C(IAXALL(1,1))                                                INITSAV.90
C                                                                                INITSAV.91
   66 CONTINUE                                                                   INITSAV.92
      PRINT *,'TOTAL NUMBER OF ACTIVE DOMAINS = ',NSTTOT                         INITSAV.93
      DO 130 NU=1,MAXNES                                                         INITSAV.94
        DO 130 I=1,MIX                                                           INITSAV.95
          DO 130 J=1,MJX                                                         INITSAV.96
            XMUU(I,J,NU)=OMUHF                                                   INITSAV.97
            XNUU(I,J,NU)=GNUHF                                                   INITSAV.98
            XMUT(I,J,NU)=OMUHF                                                   INITSAV.99
            XNUT(I,J,NU)=GNUHF                                                   INITSAV.100
  130     CONTINUE                                                               INITSAV.101
                                                                                 INITSAV.102
                                                                                 INITSAV.103
C                                                                                INITSAV.104
      DO 142 J=1,JL                                                              INITSAV.105
        I=1                                                                      INITSAV.106
        XMUU(I,J,1)=1.                                                           INITSAV.107
        XNUU(I,J,1)=0.                                                           INITSAV.108
        XMUT(I,J,1)=1.                                                           INITSAV.109
        XNUT(I,J,1)=0.                                                           INITSAV.110
        I=IL                                                                     INITSAV.111
        XMUU(I,J,1)=1.                                                           INITSAV.112
        XNUU(I,J,1)=0.                                                           INITSAV.113
        I=ILX                                                                    INITSAV.114
        XMUT(I,J,1)=1.                                                           INITSAV.115
        XNUT(I,J,1)=0.                                                           INITSAV.116
  142 CONTINUE                                                                   INITSAV.117
      DO 143 I=2,ILX                                                             INITSAV.118
        J=1                                                                      INITSAV.119
        XMUU(I,J,1)=1.                                                           INITSAV.120
        XNUU(I,J,1)=0.                                                           INITSAV.121
        XMUT(I,J,1)=1.                                                           INITSAV.122
        XNUT(I,J,1)=0.                                                           INITSAV.123
        J=JL                                                                     INITSAV.124
        XMUU(I,J,1)=1.                                                           INITSAV.125
        XNUU(I,J,1)=0.                                                           INITSAV.126
        J=JLX                                                                    INITSAV.127
        XMUT(I,J,1)=1.                                                           INITSAV.128
        XNUT(I,J,1)=0.                                                           INITSAV.129
  143 CONTINUE                                                                   INITSAV.130
C                                                                                INITSAV.131
cmic$ do all autoscope                                                           INITSAV.132
c$doacross                                                                       INITSAV.133
c$& local(i,j)                                                                   INITSAV.134
c$omp parallel do default(shared)                                                INITSAV.135
c$omp&private(i,j)                                                               INITSAV.136
      DO J=1,JL                                                                  INITSAV.137
        DO I=1,IL                                                                INITSAV.138
          WTIJ(I,J,1)=0.                                                         INITSAV.139
        ENDDO                                                                    INITSAV.140
      ENDDO                                                                      INITSAV.141
cmic$ do all autoscope                                                           INITSAV.142
c$doacross                                                                       INITSAV.143
c$& local(i,j,wti,wtj)                                                           INITSAV.144
c$omp parallel do default(shared)                                                INITSAV.145
c$omp&private(i,j,wti,wtj)                                                       INITSAV.146
#ifndef MPP1                                                                     INITSAV.147
      DO J=5,JLX-4                                                               INITSAV.148
        WTJ=1.0                                                                  INITSAV.149
        IF(J.LT.8)WTJ=0.25*FLOAT(J-4)                                            INITSAV.150
        IF(J.GT.JLX-7)WTJ=0.25*FLOAT(JLX-3-J)                                    INITSAV.151
        DO I=5,ILX-4                                                             INITSAV.152
          WTI=1.0                                                                INITSAV.153
          IF(I.LT.8)WTI=0.25*FLOAT(I-4)                                          INITSAV.154
          IF(I.GT.ILX-7)WTI=0.25*FLOAT(ILX-3-I)                                  INITSAV.155
          WTIJ(I,J,1)=WTI                                                        INITSAV.156
          IF(WTI.GT.WTJ)WTIJ(I,J,1)=WTJ                                          INITSAV.157
        ENDDO                                                                    INITSAV.158
      ENDDO                                                                      INITSAV.159
#else                                                                            INITSAV.160
C_FLIC_RUNPAD(2)                                                                 INITSAV.161
      DO J=5,JLX-4                                                               INITSAV.162
        WTJ=1.0                                                                  INITSAV.163
        IF(J.LT.8)WTJ=0.25*FLOAT(FLIC_L2G_N(J)-4)                                INITSAV.164
        IF(J.GT.JLX-7)WTJ=0.25*FLOAT(JLX-3-FLIC_L2G_N(J))                        INITSAV.165
        DO I=5,ILX-4                                                             INITSAV.166
          WTI=1.0                                                                INITSAV.167
          IF(I.LT.8)WTI=0.25*FLOAT(FLIC_L2G_M(I)-4)                              INITSAV.168
          IF(I.GT.ILX-7)WTI=0.25*FLOAT(ILX-3-FLIC_L2G_M(I))                      INITSAV.169
          WTIJ(I,J,1)=WTI                                                        INITSAV.170
          IF(WTI.GT.WTJ)WTIJ(I,J,1)=WTJ                                          INITSAV.171
        ENDDO                                                                    INITSAV.172
      ENDDO                                                                      INITSAV.173
C_FLIC_RUNPAD(0)                                                                 INITSAV.174
#endif                                                                           INITSAV.175
C                                                                                INITSAV.176
      ICUT=0                                                                     INITSAV.177
cmic$ do all autoscope                                                           INITSAV.178
c$doacross                                                                       INITSAV.179
c$& local(i,j,nij,in,jn)                                                         INITSAV.180
c$omp parallel do default(shared)                                                INITSAV.181
c$omp&private(i,j,nij,in,jn)                                                     INITSAV.182
      DO NIJ=-6,6                                                                INITSAV.183
        DO J=1,JLX                                                               INITSAV.184
#ifndef MPP1                                                                     INITSAV.185
          JN=J+NIJ                                                               INITSAV.186
#else                                                                            INITSAV.187
          JN=FLIC_L2G_N(J+NIJ)                                                   INITSAV.188
#endif                                                                           INITSAV.189
          IF(JN.LT.2+ICUT)JN=2+ICUT                                              INITSAV.190
          IF(JN.GT.JLX-1-ICUT)JN=JLX-1-ICUT                                      INITSAV.191
          JNSJ(J,NIJ,1)=JN                                                       INITSAV.192
        ENDDO                                                                    INITSAV.193
C                                                                                INITSAV.194
        DO I=1,ILX                                                               INITSAV.195
#ifndef MPP1                                                                     INITSAV.196
          IN=I+NIJ                                                               INITSAV.197
#else                                                                            INITSAV.198
          IN=FLIC_L2G_M(I+NIJ)                                                   INITSAV.199
#endif                                                                           INITSAV.200
          IF(IN.LT.2+ICUT)IN=2+ICUT                                              INITSAV.201
          IF(IN.GT.ILX-1-ICUT)IN=ILX-1-ICUT                                      INITSAV.202
          INSI(I,NIJ,1)=IN                                                       INITSAV.203
        ENDDO                                                                    INITSAV.204
      ENDDO                                                                      INITSAV.205
C                                                                                INITSAV.206
#ifdef NESTED                                                                    INITSAV.207
      IF(MAXNES.GT.1)THEN                                                        INITSAV.208
        DO 147 NU=2,MAXNES                                                       INITSAV.209
#  ifdef MPP1                                                                    INITSAV.210
          CALL MP_INITDOMAIN(NU)                                                 INITSAV.211
#  endif                                                                         INITSAV.212
          CALL ADDRX1N(IAXALL(1,NU))                                             INITSAV.213
          IYYN=INL                                                               INITSAV.214
          JXXN=JNL                                                               INITSAV.215
          DO 144 J=1,JXXN                                                        INITSAV.216
            XMUU(1,J,NU)=1.                                                      INITSAV.217
            XNUU(1,J,NU)=0.                                                      INITSAV.218
            XMUT(1,J,NU)=1.                                                      INITSAV.219
            XNUT(1,J,NU)=0.                                                      INITSAV.220
            XMUU(2,J,NU)=1.                                                      INITSAV.221
            XNUU(2,J,NU)=0.                                                      INITSAV.222
            XMUT(2,J,NU)=1.                                                      INITSAV.223
            XNUT(2,J,NU)=0.                                                      INITSAV.224
            I=IYYN                                                               INITSAV.225
            XMUU(I,J,NU)=1.                                                      INITSAV.226
            XNUU(I,J,NU)=0.                                                      INITSAV.227
            I=IYYN-1                                                             INITSAV.228
            XMUT(I,J,NU)=1.                                                      INITSAV.229
            XNUT(I,J,NU)=0.                                                      INITSAV.230
            XMUU(I,J,NU)=1.                                                      INITSAV.231
            XNUU(I,J,NU)=0.                                                      INITSAV.232
            I=IYYN-2                                                             INITSAV.233
            XMUT(I,J,NU)=1.                                                      INITSAV.234
            XNUT(I,J,NU)=0.                                                      INITSAV.235
  144     CONTINUE                                                               INITSAV.236
          DO 146 I=3,IYYN-2                                                      INITSAV.237
            J=1                                                                  INITSAV.238
            XMUT(I,J,NU)=1.                                                      INITSAV.239
            XNUT(I,J,NU)=0.                                                      INITSAV.240
            XMUU(I,J,NU)=1.                                                      INITSAV.241
            XNUU(I,J,NU)=0.                                                      INITSAV.242
            J=2                                                                  INITSAV.243
            XMUT(I,J,NU)=1.                                                      INITSAV.244
            XNUT(I,J,NU)=0.                                                      INITSAV.245
            XMUU(I,J,NU)=1.                                                      INITSAV.246
            XNUU(I,J,NU)=0.                                                      INITSAV.247
            J=JXXN                                                               INITSAV.248
            XMUU(I,J,NU)=1.                                                      INITSAV.249
            XNUU(I,J,NU)=0.                                                      INITSAV.250
            J=JXXN-1                                                             INITSAV.251
            XMUU(I,J,NU)=1.                                                      INITSAV.252
            XNUU(I,J,NU)=0.                                                      INITSAV.253
            XMUT(I,J,NU)=1.                                                      INITSAV.254
            XNUT(I,J,NU)=0.                                                      INITSAV.255
            J=JXXN-2                                                             INITSAV.256
            XMUT(I,J,NU)=1.                                                      INITSAV.257
            XNUT(I,J,NU)=0.                                                      INITSAV.258
  146     CONTINUE                                                               INITSAV.259
C                                                                                INITSAV.260
cmic$ do all autoscope                                                           INITSAV.261
c$doacross                                                                       INITSAV.262
c$& local(i,j)                                                                   INITSAV.263
c$omp parallel do default(shared)                                                INITSAV.264
c$omp&private(i,j)                                                               INITSAV.265
          DO J=1,JNL                                                             INITSAV.266
            DO I=1,INL                                                           INITSAV.267
              WTIJ(I,J,NU)=0.                                                    INITSAV.268
            ENDDO                                                                INITSAV.269
          ENDDO                                                                  INITSAV.270
cmic$ do all autoscope                                                           INITSAV.271
c$doacross                                                                       INITSAV.272
c$& local(i,j,wti,wtj)                                                           INITSAV.273
c$omp parallel do default(shared)                                                INITSAV.274
c$omp&private(i,j,wti,wtj)                                                       INITSAV.275
#ifndef MPP1                                                                     INITSAV.276
          DO J=5,JNL-5                                                           INITSAV.277
            WTJ=1.0                                                              INITSAV.278
            IF(J.LT.8)WTJ=0.25*FLOAT(J-4)                                        INITSAV.279
            IF(J.GT.JNL-8)WTJ=0.25*FLOAT(JNL-4-J)                                INITSAV.280
            DO I=5,INL-5                                                         INITSAV.281
              WTI=1.0                                                            INITSAV.282
              IF(I.LT.8)WTI=0.25*FLOAT(I-4)                                      INITSAV.283
              IF(I.GT.INL-8)WTI=0.25*FLOAT(INL-4-I)                              INITSAV.284
              WTIJ(I,J,NU)=WTI                                                   INITSAV.285
              IF(WTI.GT.WTJ)WTIJ(I,J,NU)=WTJ                                     INITSAV.286
            ENDDO                                                                INITSAV.287
          ENDDO                                                                  INITSAV.288
#else                                                                            INITSAV.289
C_FLIC_RUNPAD(2)                                                                 INITSAV.290
          DO J=5,JNL-5                                                           INITSAV.291
            WTJ=1.0                                                              INITSAV.292
            IF(J.LT.8)WTJ=0.25*FLOAT(FLIC_L2G_N(J)-4)                            INITSAV.293
            IF(J.GT.JNL-8)WTJ=0.25*FLOAT(JNL-4-FLIC_L2G_N(J))                    INITSAV.294
            DO I=5,INL-5                                                         INITSAV.295
              WTI=1.0                                                            INITSAV.296
              IF(I.LT.8)WTI=0.25*FLOAT(FLIC_L2G_M(I)-4)                          INITSAV.297
              IF(I.GT.INL-8)WTI=0.25*FLOAT(INL-4-FLIC_L2G_M(I))                  INITSAV.298
              WTIJ(I,J,NU)=WTI                                                   INITSAV.299
              IF(WTI.GT.WTJ)WTIJ(I,J,NU)=WTJ                                     INITSAV.300
            ENDDO                                                                INITSAV.301
          ENDDO                                                                  INITSAV.302
C_FLIC_RUNPAD(0)                                                                 INITSAV.303
#endif                                                                           INITSAV.304
C                                                                                INITSAV.305
          ICUT=1                                                                 INITSAV.306
cmic$ do all autoscope                                                           INITSAV.307
c$doacross                                                                       INITSAV.308
c$& local(i,j,nij,in,jn)                                                         INITSAV.309
c$omp parallel do default(shared)                                                INITSAV.310
c$omp&private(i,j,nij,in,jn)                                                     INITSAV.311
          DO NIJ=-6,6                                                            INITSAV.312
            DO J=1,JNL-1                                                         INITSAV.313
#ifndef MPP1                                                                     INITSAV.314
              JN=J+NIJ                                                           INITSAV.315
#else                                                                            INITSAV.316
              JN=FLIC_L2G_N(J+NIJ)                                               INITSAV.317
#endif                                                                           INITSAV.318
              IF(JN.LT.2+ICUT)JN=2+ICUT                                          INITSAV.319
              IF(JN.GT.JNL-2-ICUT)JN=JNL-2-ICUT                                  INITSAV.320
              JNSJ(J,NIJ,NU)=JN                                                  INITSAV.321
            ENDDO                                                                INITSAV.322
C                                                                                INITSAV.323
            DO I=1,INL-1                                                         INITSAV.324
#ifndef MPP1                                                                     INITSAV.325
              IN=I+NIJ                                                           INITSAV.326
#else                                                                            INITSAV.327
              IN=FLIC_L2G_M(I+NIJ)                                               INITSAV.328
#endif                                                                           INITSAV.329
              IF(IN.LT.2+ICUT)IN=2+ICUT                                          INITSAV.330
              IF(IN.GT.INL-2-ICUT)IN=INL-2-ICUT                                  INITSAV.331
              INSI(I,NIJ,NU)=IN                                                  INITSAV.332
            ENDDO                                                                INITSAV.333
          ENDDO                                                                  INITSAV.334
C                                                                                INITSAV.335
  147   CONTINUE                                                                 INITSAV.336
      ENDIF                                                                      INITSAV.337
#  ifdef MPP1                                                                    INITSAV.338
      CALL MP_INITDOMAIN(1)                                                      INITSAV.339
#  endif                                                                         INITSAV.340
#endif                                                                           INITSAV.341
C-----READ PAST THE SLAB TEMPERATURE IN THE BOUNDARY CONDITIONS FILE             INITSAV.342
C     READ (9,ERR=9001,END=9002) SLBTMPSCR                                       INITSAV.343
C                                                                                INITSAV.344
C-----COMPUTE THE SOLAR DECLINATION ANGLE:                                       INITSAV.345
C                                                                                INITSAV.346
      DECTIM=1440.*IFIX((XTIME+0.01)/1440.)                                      INITSAV.347
C     NO CONDITION ON ITGFLG                                                     INITSAV.348
      CALL SOLAR1(FLOAT(NINT(DECTIM)),JULDAY,GMT,1)                              INITSAV.349
C                                                                                07NOV00.351
C-----INITIALIZE LOCATIONS FOR TIME SERIES OUTPUT AT RESTART                     07NOV00.352
C                                                                                07NOV00.353
      IF (IFTSOUT) THEN                                                          07NOV00.354
         DO LLN=1,MAXNES                                                         07NOV00.355
            CALL ADDRX1C(IAXALL(1,LLN))                                          07NOV00.356
            CALL INITTS(LLN,IL,JL)                                               07NOV00.357
            CALL ADDRX1C(IAXALL(1,1))                                            07NOV00.358
         END DO                                                                  07NOV00.359
      ENDIF                                                                      07NOV00.360
C                                                                                INITSAV.350
C-----SET UP OUTPUT TIME:                                                        INITSAV.351
C                                                                                INITSAV.352
      SAVTIM=SAVFRQ                                                              INITSAV.353
      TAPTIM=TAPFRQ                                                              INITSAV.354
      PRTTIM=PRTFRQ                                                              INITSAV.355
  640 IF(NINT(SAVTIM/DTMIN).GT.KTAU)GOTO 660                                     07NOV00.361
      SAVTIM=SAVTIM+SAVFRQ                                                       INITSAV.357
      GOTO 640                                                                   INITSAV.358
  660 IF(NINT(TAPTIM/DTMIN).GT.KTAU)GOTO 670                                     07NOV00.362
      TAPTIM=TAPTIM+TAPFRQ                                                       INITSAV.360
      GOTO 660                                                                   INITSAV.361
  670 IF(NINT(PRTTIM/DTMIN).GT.KTAU)GOTO 710                                     07NOV00.363
      PRTTIM=PRTTIM+PRTFRQ                                                       INITSAV.363
      GOTO 670                                                                   INITSAV.364
  710 IF(DECTIM.GT.XTIME)GOTO 720                                                INITSAV.365
      DECTIM=DECTIM+1440.                                                        INITSAV.366
      GOTO 710                                                                   INITSAV.367
C                                                                                INITSAV.371
  720 CONTINUE                                                                   INITSAV.372
C                                                                                INITSAV.377
      RETURN                                                                     INITSAV.378
9001  PRINT *,'ERROR IN READING BC FILE. CHECK IF IT IS THE RIGHT FILE'          INITSAV.379
      STOP '9001 IN INIT'                                                        INITSAV.380
9002  PRINT *,'INPUT BC FILE EMPTY. CHECK IF THE FILE EXISTS'                    INITSAV.381
      STOP '9002 IN INIT'                                                        INITSAV.382
      END                                                                        INITSAV.383
                                                                                 INITSAV.384
