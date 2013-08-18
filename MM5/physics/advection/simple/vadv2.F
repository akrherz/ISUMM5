      SUBROUTINE VADV2(KZZ,FTEN,DUM,H,FA,QDOT,PSA,IND,IN,var)                    VADV2.1
      IMPLICIT NONE                                                              VADV2.2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          VADV2.3
C                                                                     C          VADV2.4
C                                                                     C          VADV2.5
C     THIS SUBROUTINE COMPUTES THE VERTICAL FLUX-DIVERGENCE TERMS.    C          VADV2.6
C                                                                     C          VADV2.7
C                                                                     C          VADV2.8
C     IYY AND KZZ : ARE THE DIMENSIONS.                               C          VADV2.9
C                                                                     C          VADV2.10
C     FTEN   : IS THE TENDENCY OF VARIABLE 'F'.                       C          VADV2.11
C                                                                     C          VADV2.12
C     FA     : IS P*F.                                                C          VADV2.13
C                                                                     C          VADV2.14
C     QDOT   : IS THE VERTICAL SIGMA-VELOCITY                         C          VADV2.15
C                                                                     C          VADV2.16
C     F      : IS THE WORKING SPACE USED TO STORE THE INTERLATED      C          VADV2.17
C              VALUES.                                                C          VADV2.18
C                                                                     C          VADV2.19
C     PSA    : IS P* USED TO INTERPOLATE THE TEMPERATURE.             C          VADV2.20
C                                                                     C          VADV2.21
C     J      : J`TH SLICE OF VARIABLE FA.                             C          VADV2.22
C                                                                     C          VADV2.23
C     IND = 1 : FOR T.                                                C          VADV2.24
C           2 : FOR QV.                                               C          VADV2.25
C           3 : FOR QC AND QR.                                        C          VADV2.26
C           4 : FOR U AND V.                                          C          VADV2.27
C                                                                     C          VADV2.28
C                                                                     C          VADV2.29
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          VADV2.30
#       include <parame.incl>                                                    VADV2.31
#       include <param3.incl>                                                    VADV2.32
#       include <various.incl>                                                   VADV2.33
#       include <nhcnst.incl>                                                    VADV2.34
C                                                                                VADV2.35
      INTEGER   KZZ,IND,IN,EPSI                                                  VADV2.36
      REAL      FTEN(MIX,MJX,KZZ),QDOT(MIX,MJX,KXP1)                             VADV2.37
      REAL        FA(MIX,MJX,KZZ), PSA(MIX,MJX)                                  VADV2.38
      REAL         F(MIX,MJX,KXP1),DOTQDOT(MIX,KXP1)                             VADV2.39
C                                                                                VADV2.40
      REAL      PA287(MIX,MJX,MKX),DUM(MIX,MJX,KZZ)                              VADV2.41
      REAL      PS287(MIX,MJX,MKX),H(MIX,MJX,KZZ)                                VADV2.42
      INTEGER   ICUT,I,J,K                                                       VADV2.43
      character*1 var                                                            VADV2.44
C                                                                                VADV2.45
C                                                                                VADV2.46
C_FLIC_RUNPAD(2)                                                                 VADV2.47
      ICUT=0                                                                     VADV2.48
      IF(IN.GT.1)ICUT=1                                                          VADV2.49
C                                                                                VADV2.50
C----------------------------------------------------------------------          VADV2.51
C                                                                                VADV2.52
      GOTO (10,40,40,150,210,65),IND                                             VADV2.53
C                                                                                VADV2.54
C-----VERTICAL ADVECTION TERMS FOR:                                              VADV2.55
C                                                                                VADV2.56
   10 CONTINUE                                                                   VADV2.57
C                                                                                VADV2.58
C.....INTERPOLATE TA TO FULL SIGMA LEVELS:                                       VADV2.59
C                                                                                VADV2.60
cmic$ do all autoscope                                                           VADV2.61
c$doacross                                                                       VADV2.62
c$& local(i,j,k,dotqdot)                                                         VADV2.63
c$omp parallel do default(shared)                                                VADV2.64
c$omp&private(i,j,k,dotqdot)                                                     VADV2.65
      DO J=2+ICUT,JLXM-ICUT                                                      VADV2.66
        DO K=1,KL                                                                VADV2.67
          DO I=2+ICUT,ILXM-ICUT                                                  VADV2.68
            PA287(I,J,K)=1.0/(PSA(I,J)*A(K)+PTOP)**0.287                         VADV2.69
            PS287(I,J,K)=(PSA(I,J)*SIGMA(K)+PTOP)**0.287                         VADV2.70
            DOTQDOT(I,K)=FA(I,J,K)*PA287(I,J,K)                                  VADV2.71
          ENDDO                                                                  VADV2.72
        ENDDO                                                                    VADV2.73
        DO K=2,KL                                                                VADV2.74
          DO I=2+ICUT,ILXM-ICUT                                                  VADV2.75
            F(I,J,K)=PS287(I,J,K)*(TWT(K,1)*DOTQDOT(I,K)+TWT(K,2)*               VADV2.76
     +               DOTQDOT(I,K-1))                                             VADV2.77
          ENDDO                                                                  VADV2.78
        ENDDO                                                                    VADV2.79
      ENDDO                                                                      VADV2.80
C                                                                                VADV2.81
      GOTO 70                                                                    VADV2.82
C                                                                                VADV2.83
C-----VERTICAL ADVECTION TERM FOR QV:                                            VADV2.84
C                                                                                VADV2.85
   40 CONTINUE                                                                   VADV2.86
C                                                                                VADV2.87
C.....INTERPOLATE QV TO FULL SIGMA LEVELS:                                       VADV2.88
C                                                                                VADV2.89
cmic$ do all autoscope                                                           VADV2.90
c$doacross                                                                       VADV2.91
c$& local(i,j,k)                                                                 VADV2.92
c$omp parallel do default(shared)                                                VADV2.93
c$omp&private(i,j,k)                                                             VADV2.94
      DO 60 J=2+ICUT,JLXM-ICUT                                                   VADV2.95
        DO 60 K=2,KL                                                             VADV2.96
          DO 60 I=2+ICUT,ILXM-ICUT                                               VADV2.97
              F(I,J,K)=FA(I,J,K)*(FA(I,J,K-1)/FA(I,J,K))**QCON(K)                VADV2.98
   60     CONTINUE                                                               VADV2.99
                                                                                 VADV2.100
                                                                                 VADV2.101
C                                                                                VADV2.102
C.....K = 1                                                                      VADV2.103
C                                                                                VADV2.104
C                                                                                VADV2.105
C                                                                                VADV2.106
   70 CONTINUE                                                                   VADV2.107
cmic$ do all autoscope                                                           VADV2.108
c$doacross                                                                       VADV2.109
c$& local(i,j,k)                                                                 VADV2.110
c$omp parallel do default(shared)                                                VADV2.111
c$omp&private(i,j,k)                                                             VADV2.112
      DO J=2+ICUT,JLXM-ICUT                                                      VADV2.113
        DO 80 I=2+ICUT,ILXM-ICUT                                                 VADV2.114
          DUM(I,J,1)=-QDOT(I,J,2)*(F(I,J,2)-                                     VADV2.115
     +  FA(I,J,1))/DSIGMA(1)                                                     VADV2.116
   80   CONTINUE                                                                 VADV2.117
C                                                                                VADV2.118
C.....K = 2,KLM                                                                  VADV2.119
C                                                                                VADV2.120
        DO 90 K=2,KLM                                                            VADV2.121
          DO 90 I=2+ICUT,ILXM-ICUT                                               VADV2.122
            DUM(I,J,K)=-(QDOT(I,J,K+1)*(F(I,J,K+1)-                              VADV2.123
     + FA(I,J,K))-QDOT(I,J,K)*(F(I,J,K)-FA(I,J,K)))/DSIGMA(K)                    VADV2.124
   90     CONTINUE                                                               VADV2.125
                                                                                 VADV2.126
C                                                                                VADV2.127
C,....K = KL                                                                     VADV2.128
C                                                                                VADV2.129
        DO 100 I=2+ICUT,ILXM-ICUT                                                VADV2.130
          DUM(I,J,KL)=QDOT(I,J,KL)*(F(I,J,KL)-                                   VADV2.131
     +  FA(I,J,KL))/DSIGMA(KL)                                                   VADV2.132
  100   CONTINUE                                                                 VADV2.133
      ENDDO                                                                      VADV2.134
C                                                                                VADV2.135
      goto 500                                                                   VADV2.136
C                                                                                VADV2.137
C-----VERTICAL ADVECTION TERMS FOR U AND V:                                      VADV2.138
C                                                                                VADV2.139
  150 CONTINUE                                                                   VADV2.140
      stop 'incorrect usage of vadv2'                                            VADV2.141
      RETURN                                                                     VADV2.142
C                                                                                VADV2.143
C --- VERTICAL ADVECTION OF W                                                    VADV2.144
C                                                                                VADV2.145
  210 CONTINUE                                                                   VADV2.146
      stop 'incorrect usage of vadv2'                                            VADV2.147
      RETURN                                                                     VADV2.148
                                                                                 VADV2.149
  65  CONTINUE        ! IND = 6; linear interpolation with                       VADV2.150
                      !          divergence already removed                      VADV2.151
                                                                                 VADV2.152
cmic$ do all autoscope                                                           VADV2.153
c$doacross                                                                       VADV2.154
c$& local(i,j,k)                                                                 VADV2.155
c$omp parallel do default(shared)                                                VADV2.156
c$omp&private(i,j,k)                                                             VADV2.157
      DO J=2+ICUT,JLXM-ICUT                                                      VADV2.158
        DO 380 I=2+ICUT,ILXM-ICUT                                                VADV2.159
          DUM(I,J,1)=QDOT(I,J,2)*TWT(2,1)*(FA(I,J,1)-FA(I,J,2))                  VADV2.160
     f  /DSIGMA(1)                                                               VADV2.161
  380   CONTINUE                                                                 VADV2.162
C                                                                                VADV2.163
C.....K = 2,KLM                                                                  VADV2.164
C                                                                                VADV2.165
        DO 390 K=2,KLM                                                           VADV2.166
          DO 390 I=2+ICUT,ILXM-ICUT                                              VADV2.167
            DUM(I,J,K)=(QDOT(I,J,K+1)*TWT(K+1,1)*                                VADV2.168
     + (FA(I,J,K)-FA(I,J,K+1))+QDOT(I,J,K)*TWT(K,2)*                             VADV2.169
     + (FA(I,J,K-1)-FA(I,J,K)))/DSIGMA(K)                                        VADV2.170
  390     CONTINUE                                                               VADV2.171
                                                                                 VADV2.172
C                                                                                VADV2.173
C,....K = KL                                                                     VADV2.174
C                                                                                VADV2.175
        DO 400 I=2+ICUT,ILXM-ICUT                                                VADV2.176
          DUM(I,J,KL)=QDOT(I,J,KL)*TWT(KL,2)*(FA(I,J,KL-1)-FA(I,J,KL))           VADV2.177
     f  /DSIGMA(KL)                                                              VADV2.178
  400   CONTINUE                                                                 VADV2.179
      ENDDO                                                                      VADV2.180
C                                                                                VADV2.181
 500    continue  ! instability correction (local extrema exceeding a certain    VADV2.182
                  ! threshold must not grow further due to advection)            VADV2.183
                                                                                 VADV2.184
        if (var.eq.'T') then  ! Temperature                                      VADV2.185
                                                                                 VADV2.186
cmic$ do all autoscope                                                           VADV2.187
c$doacross                                                                       VADV2.188
c$& local(i,j,k)                                                                 VADV2.189
c$omp parallel do default(shared)                                                VADV2.190
c$omp&private(i,j,k)                                                             VADV2.191
      DO 220 J=2+ICUT,JLXM-ICUT                                                  VADV2.192
        DO 220 K=2,KLM                                                           VADV2.193
          DO 220 I=2+ICUT,ILXM-ICUT                                              VADV2.194
      if (abs(H(I,J,K+1)+H(I,J,K-1)-2*H(I,J,K)).gt.1) then                       VADV2.195
      if ((H(I,J,K).gt.H(I,J,K+1)).and.(H(I,J,K).gt.H(I,J,K-1))) then            VADV2.196
        DUM(I,J,K) = min(DUM(I,J,K),0.)                                          VADV2.197
      else if ((H(I,J,K).lt.H(I,J,K+1)).and.(H(I,J,K).lt.H(I,J,K-1)))            VADV2.198
     f then                                                                      VADV2.199
      DUM(I,J,K) = max(DUM(I,J,K),0.)                                            VADV2.200
      endif                                                                      VADV2.201
      endif                                                                      VADV2.202
 220  continue                                                                   VADV2.203
                                                                                 VADV2.204
      K = KL                                                                     VADV2.205
cmic$ do all autoscope                                                           VADV2.206
c$doacross                                                                       VADV2.207
c$& local(i,j)                                                                   VADV2.208
c$omp parallel do default(shared)                                                VADV2.209
c$omp&private(i,j)                                                               VADV2.210
        DO 221 J=2+ICUT,JLXM-ICUT                                                VADV2.211
          DO 221 I=2+ICUT,ILXM-ICUT                                              VADV2.212
      if (abs(H(I,J,K-1)-H(I,J,K)).gt.0.5) then                                  VADV2.213
      if (H(I,J,K).gt.H(I,J,K-1)) then                                           VADV2.214
        DUM(I,J,K) = min(DUM(I,J,K),0.)                                          VADV2.215
      else if (H(I,J,K).lt.H(I,J,K-1))                                           VADV2.216
     f then                                                                      VADV2.217
      DUM(I,J,K) = max(DUM(I,J,K),0.)                                            VADV2.218
      endif                                                                      VADV2.219
      endif                                                                      VADV2.220
 221  continue                                                                   VADV2.221
                                                                                 VADV2.222
      else if (var.eq.'Q') then   ! WV mixing ratio                              VADV2.223
                                                                                 VADV2.224
cmic$ do all autoscope                                                           VADV2.225
c$doacross                                                                       VADV2.226
c$& local(i,j,k)                                                                 VADV2.227
c$omp parallel do default(shared)                                                VADV2.228
c$omp&private(i,j,k)                                                             VADV2.229
      DO 230 J=2+ICUT,JLXM-ICUT                                                  VADV2.230
        DO 230 K=2,KLM                                                           VADV2.231
          DO 230 I=2+ICUT,ILXM-ICUT                                              VADV2.232
      if (abs(H(I,J,K+1)+H(I,J,K-1)-2*H(I,J,K))/H(I,J,K).gt.0.2) then            VADV2.233
      if ((H(I,J,K).gt.H(I,J,K+1)).and.(H(I,J,K).gt.H(I,J,K-1))) then            VADV2.234
        DUM(I,J,K) = min(DUM(I,J,K),0.)                                          VADV2.235
      else if ((H(I,J,K).lt.H(I,J,K+1)).and.(H(I,J,K).lt.H(I,J,K-1)))            VADV2.236
     f then                                                                      VADV2.237
      DUM(I,J,K) = max(DUM(I,J,K),0.)                                            VADV2.238
      endif                                                                      VADV2.239
      endif                                                                      VADV2.240
 230  continue                                                                   VADV2.241
                                                                                 VADV2.242
      K = KL                                                                     VADV2.243
cmic$ do all autoscope                                                           VADV2.244
c$doacross                                                                       VADV2.245
c$& local(i,j)                                                                   VADV2.246
c$omp parallel do default(shared)                                                VADV2.247
c$omp&private(i,j)                                                               VADV2.248
       DO 231 J=2+ICUT,JLXM-ICUT                                                 VADV2.249
          DO 231 I=2+ICUT,ILXM-ICUT                                              VADV2.250
      if (abs(H(I,J,K-1)-H(I,J,K))/H(I,J,K).gt.0.1) then                         VADV2.251
      if (H(I,J,K).gt.H(I,J,K-1)) then                                           VADV2.252
        DUM(I,J,K) = min(DUM(I,J,K),0.)                                          VADV2.253
      else if (H(I,J,K).lt.H(I,J,K-1))                                           VADV2.254
     f then                                                                      VADV2.255
      DUM(I,J,K) = max(DUM(I,J,K),0.)                                            VADV2.256
      endif                                                                      VADV2.257
      endif                                                                      VADV2.258
 231  continue                                                                   VADV2.259
                                                                                 VADV2.260
        else if (var.eq.'C') then   ! CLW mixing ratio                           VADV2.261
                                                                                 VADV2.262
cmic$ do all autoscope                                                           VADV2.263
c$doacross                                                                       VADV2.264
c$& local(i,j,k)                                                                 VADV2.265
c$omp parallel do default(shared)                                                VADV2.266
c$omp&private(i,j,k)                                                             VADV2.267
      DO 240 J=2+ICUT,JLXM-ICUT                                                  VADV2.268
        DO 240 K=2,KLM                                                           VADV2.269
          DO 240 I=2+ICUT,ILXM-ICUT                                              VADV2.270
      if (abs(H(I,J,K+1)+H(I,J,K-1)-2*H(I,J,K)).gt.2.e-4) then                   VADV2.271
      if ((H(I,J,K).gt.H(I,J,K+1)).and.(H(I,J,K).gt.H(I,J,K-1))) then            VADV2.272
        DUM(I,J,K) = min(DUM(I,J,K),0.)                                          VADV2.273
      else if ((H(I,J,K).lt.H(I,J,K+1)).and.(H(I,J,K).lt.H(I,J,K-1)))            VADV2.274
     f then                                                                      VADV2.275
      DUM(I,J,K) = max(DUM(I,J,K),0.)                                            VADV2.276
      endif                                                                      VADV2.277
      endif                                                                      VADV2.278
 240    continue                                                                 VADV2.279
                                                                                 VADV2.280
        K = KL                                                                   VADV2.281
cmic$ do all autoscope                                                           VADV2.282
c$doacross                                                                       VADV2.283
c$& local(i,j)                                                                   VADV2.284
c$omp parallel do default(shared)                                                VADV2.285
c$omp&private(i,j)                                                               VADV2.286
        DO 241 J=2+ICUT,JLXM-ICUT                                                VADV2.287
          DO 241 I=2+ICUT,ILXM-ICUT                                              VADV2.288
      if (abs(H(I,J,K-1)-H(I,J,K)).gt.1.e-4) then                                VADV2.289
      if (H(I,J,K).gt.H(I,J,K-1)) then                                           VADV2.290
        DUM(I,J,K) = min(DUM(I,J,K),0.)                                          VADV2.291
      else if (H(I,J,K).lt.H(I,J,K-1))                                           VADV2.292
     f then                                                                      VADV2.293
      DUM(I,J,K) = max(DUM(I,J,K),0.)                                            VADV2.294
      endif                                                                      VADV2.295
      endif                                                                      VADV2.296
 241    continue                                                                 VADV2.297
                                                                                 VADV2.298
       endif                                                                     VADV2.299
                                                                                 VADV2.300
cmic$ do all autoscope                                                           VADV2.301
c$doacross                                                                       VADV2.302
c$& local(i,j,k)                                                                 VADV2.303
c$omp parallel do default(shared)                                                VADV2.304
c$omp&private(i,j,k)                                                             VADV2.305
        DO 600 J=2+ICUT,JLXM-ICUT                                                VADV2.306
        DO 600 K=1,KL                                                            VADV2.307
        DO 600 I=2+ICUT,ILXM-ICUT                                                VADV2.308
          FTEN(I,J,K) = FTEN(I,J,K)+DUM(I,J,K)                                   VADV2.309
 600    continue                                                                 VADV2.310
C_FLIC_RUNPAD(0)                                                                 VADV2.311
      END                                                                        VADV2.312
                                                                                 VADV2.313
