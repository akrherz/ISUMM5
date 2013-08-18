      SUBROUTINE VADV(KZZ,FTEN,FA,QDOT,PSA,IND,IN)                               VADV.1
      IMPLICIT NONE                                                              VADV.2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          VADV.3
C                                                                     C          VADV.4
C                                                                     C          VADV.5
C     THIS SUBROUTINE COMPUTES THE VERTICAL FLUX-DIVERGENCE TERMS.    C          VADV.6
C                                                                     C          VADV.7
C                                                                     C          VADV.8
C     IYY AND KZZ : ARE THE DIMENSIONS.                               C          VADV.9
C                                                                     C          VADV.10
C     FTEN   : IS THE TENDENCY OF VARIABLE 'F'.                       C          VADV.11
C                                                                     C          VADV.12
C     FA     : IS P*F.                                                C          VADV.13
C                                                                     C          VADV.14
C     QDOT   : IS THE VERTICAL SIGMA-VELOCITY                         C          VADV.15
C                                                                     C          VADV.16
C     F      : IS THE WORKING SPACE USED TO STORE THE INTERPOLATED    C          VADV.17
C              VALUES.                                                C          VADV.18
C                                                                     C          VADV.19
C     PSA    : IS P* USED TO INTERPOLATE THE TEMPERATURE.             C          VADV.20
C                                                                     C          VADV.21
C     J      : J`TH SLICE OF VARIABLE FA.                             C          VADV.22
C                                                                     C          VADV.23
C     IND = 1 : FOR T.                                                C          VADV.24
C           2 : FOR QV.                                               C          VADV.25
C           3 : FOR QC AND QR.                                        C          VADV.26
C           4 : FOR U AND V.                                          C          VADV.27
C                                                                     C          VADV.28
C                                                                     C          VADV.29
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          VADV.30
      REAL      EPSI                                                             VADV.31
      PARAMETER (EPSI=1.E-15)                                                    VADV.32
#       include <parame.incl>                                                    VADV.33
#       include <param3.incl>                                                    VADV.34
#       include <various.incl>                                                   VADV.35
#       include <nhcnst.incl>                                                    VADV.36
                                                                                 VADV.37
C.......  Arguments                                                              VADV.38
                                                                                 VADV.39
      INTEGER   KZZ,IND,IN                                                       VADV.40
      REAL      FTEN(MIX,MJX,KZZ),QDOT(MIX,MJX,KXP1)                             VADV.41
      REAL        FA(MIX,MJX,KZZ), PSA(MIX,MJX)                                  VADV.42
                                                                                 VADV.43
C.......  Local Variables:                                                       VADV.44
                                                                                 VADV.45
      REAL      DOTQDOT(MIX,KXP1)                                                VADV.46
      REAL      PA287                                                            VADV.47
      REAL      PS287                                                            VADV.48
      INTEGER   ICUT,I,J,K                                                       VADV.49
                                                                                 VADV.50
      REAL      FF, QQ                                                           VADV.51
      REAL      DDS(KXP1)                                                        VADV.52
                                                                                 VADV.53
C----------------------------------------------------------------------          VADV.54
                                                                                 VADV.55
      ICUT=0                                                                     VADV.56
      IF(IN.GT.1)ICUT=1                                                          VADV.57
                                                                                 VADV.58
      GOTO (10,40,40,150,210,65),IND                                             VADV.59
C                                                                                VADV.60
C-----Vertical advection terms for:                                              VADV.61
C                                                                                VADV.62
   10 CONTINUE                                                                   VADV.63
                                                                                 VADV.64
      DO K=1,KL                                                                  VADV.65
          DDS(K) = 1.0 / DSIGMA(K)                                               VADV.66
      END DO                                                                     VADV.67
                                                                                 VADV.68
C                                                                                VADV.69
C.....Interpolate TA to full sigma levels:                                       VADV.70
C                                                                                VADV.71
cmic$ do all autoscope                                                           VADV.72
c$doacross                                                                       VADV.73
c$&     local(i,j,k,dotqdot,pa287,ps287,FF)                                      VADV.74
c$omp parallel do default(shared)                                                VADV.75
c$omp&private(i,j,k,dotqdot,pa287,ps287,FF)                                      VADV.76
                                                                                 VADV.77
      DO J=2+ICUT,JLXM-ICUT                                                      VADV.78
                                                                                 VADV.79
          K=1                                                                    VADV.80
          DO I=2+ICUT,ILXM-ICUT                                                  VADV.81
              !! pa287 = 1.0/(psa(i,j)*a(k)+ptop)**0.287                         VADV.82
              PA287 = EXP( -0.287*LOG( PSA(I,J)*A(K) + PTOP ) )                  VADV.83
              DOTQDOT(I,K) = FA(I,J,K)*PA287                                     VADV.84
          END DO                                                                 VADV.85
                                                                                 VADV.86
          DO K=2,KL                                                              VADV.87
          DO I=2+ICUT,ILXM-ICUT                                                  VADV.88
              !!  PA287 = 1.0/(PSA(I,J)*A(K)+PTOP)**0.287                        VADV.89
              !!  PS287 = (PSA(I,J)*SIGMA(K)+PTOP)**0.287                        VADV.90
              PA287 = EXP( -0.287*LOG( PSA(I,J)*A(K)     + PTOP ) )              VADV.91
              PS287 = EXP(  0.287*LOG( PSA(I,J)*SIGMA(K) + PTOP ) )              VADV.92
              DOTQDOT(I,K) = FA(I,J,K)*PA287                                     VADV.93
              FF = PS287*QDOT(I,J,K)*( TWT(K,1)*DOTQDOT(I,K)                     VADV.94
     +                               + TWT(K,2)*DOTQDOT(I,K-1))                  VADV.95
              FTEN(I,J,K-1) = FTEN(I,J,K-1) - FF*DDS(K-1)                        VADV.96
              FTEN(I,J,K  ) = FTEN(I,J,K  ) + FF*DDS(K  )                        VADV.97
          END DO                                                                 VADV.98
          END DO                                                                 VADV.99
                                                                                 VADV.100
      END DO                                                                     VADV.101
                                                                                 VADV.102
      RETURN                                                                     VADV.103
C                                                                                VADV.104
C-----Vertical advection term for QV:                                            VADV.105
C                                                                                VADV.106
   40 CONTINUE                                                                   VADV.107
                                                                                 VADV.108
      DO K=1,KL                                                                  VADV.109
          DDS(K) = 1.0 / DSIGMA(K)                                               VADV.110
      END DO                                                                     VADV.111
                                                                                 VADV.112
C                                                                                VADV.113
C.....Interpolate QV to full sigma levels:                                       VADV.114
C                                                                                VADV.115
cmic$ do all autoscope                                                           VADV.116
c$doacross                                                                       VADV.117
c$&     local(i,j,k,FF)                                                          VADV.118
c$omp parallel do default(shared)                                                VADV.119
c$omp&private(i,j,k,FF)                                                          VADV.120
                                                                                 VADV.121
      DO J=2+ICUT,JLXM-ICUT                                                      VADV.122
                                                                                 VADV.123
          DO  K=2,KL                                                             VADV.124
          DO  I=2+ICUT,ILXM-ICUT                                                 VADV.125
              IF(FA(I,J,K).GT.EPSI.AND.FA(I,J,K-1).GT.EPSI)THEN                  VADV.126
                  FF = FA(I,J,K) * (FA(I,J,K-1)/FA(I,J,K))**QCON(K)              VADV.127
                  FF = QDOT(I,J,K)*FF                                            VADV.128
                  FTEN(I,J,K-1) = FTEN(I,J,K-1) - FF*DDS(K-1)                    VADV.129
                  FTEN(I,J,K  ) = FTEN(I,J,K  ) + FF*DDS(K  )                    VADV.130
              END IF                                                             VADV.131
          END DO                                                                 VADV.132
          END DO                                                                 VADV.133
                                                                                 VADV.134
      END DO                                                                     VADV.135
                                                                                 VADV.136
      RETURN                                                                     VADV.137
                                                                                 VADV.138
                                                                                 VADV.139
C----- Vertical advection term for PP                                            VADV.140
                                                                                 VADV.141
   65 CONTINUE                                                                   VADV.142
                                                                                 VADV.143
      DO K=1,KL                                                                  VADV.144
          DDS(K) = 1.0 / DSIGMA(K)                                               VADV.145
      END DO                                                                     VADV.146
                                                                                 VADV.147
cmic$ do all autoscope                                                           VADV.148
c$doacross                                                                       VADV.149
c$&     local(i,j,k,FF)                                                          VADV.150
c$omp parallel do default(shared)                                                VADV.151
c$omp&private(i,j,k,FF)                                                          VADV.152
                                                                                 VADV.153
      DO J=2+ICUT,JLXM-ICUT                                                      VADV.154
                                                                                 VADV.155
          DO  K=2,KL                                                             VADV.156
          DO  I=2+ICUT,ILXM-ICUT                                                 VADV.157
              FF = TWT(K,1)*FA(I,J,K)+TWT(K,2)*FA(I,J,K-1)                       VADV.158
              FF = QDOT(I,J,K)*FF                                                VADV.159
              FTEN(I,J,K-1) = FTEN(I,J,K-1) - FF*DDS(K-1)                        VADV.160
              FTEN(I,J,K  ) = FTEN(I,J,K  ) + FF*DDS(K  )                        VADV.161
          END DO                                                                 VADV.162
          END DO                                                                 VADV.163
                                                                                 VADV.164
      END DO                                                                     VADV.165
                                                                                 VADV.166
      RETURN                                                                     VADV.167
                                                                                 VADV.168
                                                                                 VADV.169
C-----Vertical advection terms for U and V:                                      VADV.170
C                                                                                VADV.171
  150 CONTINUE                                                                   VADV.172
                                                                                 VADV.173
      DO K=1,KL                                                                  VADV.174
          DDS(K) = 1.0 / DSIGMA(K)                                               VADV.175
      END DO                                                                     VADV.176
                                                                                 VADV.177
C                                                                                VADV.178
C.....Interpolate UA or VA to full sigma levels:                                 VADV.179
C                                                                                VADV.180
cmic$ do all autoscope                                                           VADV.181
c$doacross                                                                       VADV.182
c$&     local(i,j,k,FF,QQ)                                                       VADV.183
c$omp parallel do default(shared)                                                VADV.184
c$omp&private(i,j,k,FF,QQ)                                                       VADV.185
                                                                                 VADV.186
      DO J=2+ICUT,JLX-ICUT                                                       VADV.187
                                                                                 VADV.188
          DO  K=2,KL                                                             VADV.189
          DO  I=2+ICUT,ILX-ICUT                                                  VADV.190
              QQ  = 0.25*( QDOT(I,J  ,K) + QDOT(I-1,J  ,K)                       VADV.191
     +                   + QDOT(I,J-1,K) + QDOT(I-1,J-1,K))                      VADV.192
              FF = QQ*( TWT(K,1)*FA(I,J,K) + TWT(K,2)*FA(I,J,K-1) )              VADV.193
              FTEN(I,J,K-1) = FTEN(I,J,K-1) - FF*DDS(K-1)                        VADV.194
              FTEN(I,J,K  ) = FTEN(I,J,K  ) + FF*DDS(K  )                        VADV.195
          END DO                                                                 VADV.196
          END DO                                                                 VADV.197
                                                                                 VADV.198
      END DO                                                                     VADV.199
                                                                                 VADV.200
      RETURN                                                                     VADV.201
C                                                                                VADV.202
C --- Vertical advection of W                                                    VADV.203
C                                                                                VADV.204
  210 CONTINUE                                                                   VADV.205
                                                                                 VADV.206
      DO K=2,KL  
          DDS(K) = 1.0 / (DSIGMA(K) + DSIGMA(K-1))                               VADV.208
      END DO                                                                     VADV.209
      DDS(1) = 0.0
      DDS(KL+1) = 0.0
                                                                                 VADV.210
cmic$ do all autoscope                                                           VADV.211
c$doacross                                                                       VADV.212
c$&     local(i,j,k,FF,QQ)                                                       VADV.213
c$omp parallel do default(shared)                                                VADV.214
c$omp&private(i,j,k,FF,QQ)                                                       VADV.215
                                                                                 VADV.216
      DO J=2+ICUT,JLX-ICUT                                                       VADV.217
                                                                                 VADV.218
          DO K=1,KL                                                              VADV.219
          DO I=2+ICUT,ILXM-ICUT                                                  VADV.220
              QQ = 0.5*(QDOT(I,J,K) + QDOT(I,J,K+1))                             VADV.221
              FF = QQ*(FA(I,J,K) + FA(I,J,K+1))                                  VADV.222
              FTEN(I,J,K+1) = FTEN(I,J,K+1) + FF*DDS(K+1)                        VADV.223
              FTEN(I,J,K  ) = FTEN(I,J,K  ) - FF*DDS(K  )                        VADV.224
          END DO                                                                 VADV.225
          END DO                                                                 VADV.226
                                                                                 VADV.227
      END DO                                                                     VADV.228
                                                                                 VADV.229
      RETURN                                                                     VADV.230
      END                                                                        VADV.231
                                                                                 VADV.232
