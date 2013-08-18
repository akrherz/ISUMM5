      SUBROUTINE CONADV(ID,IM)                                                   CONADV.1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          CONADV.2
C                                                                     C          CONADV.3
C                                                                     C          CONADV.4
C     THIS SUBROUTINE COMPUTES THE AMOUNTS OF DRY AIR AND WATER       C          CONADV.5
C     SUBSTANCE ADVECTED THROUGH THE LATERAL BOUNDARIES.              C          CONADV.6
C                                                                     C          CONADV.7
C     ---THE UNIT FROM ADVECTION IS CONVERTED TO "KG".                C          CONADV.8
C                                                                     C          CONADV.9
C                                                                     C          CONADV.10
C     IYY, JXX, KZZ : DIMENSIONS FOR PSA, UA, VA, QVA, MSFD, AND MSFX.C          CONADV.11
C                                                                     C          CONADV.12
C     IYYM, JXXM, KZZM : DIMENSIONS FOR QCA AND QRA.                  C          CONADV.13
C                                                                     C          CONADV.14
C     ID : = IDRY DEFINED IN SUBROUTINE "PARAM".                      C          CONADV.15
C                                                                     C          CONADV.16
C     IM : = IMOIST DEFINED IN SUBROUTINE "PARAM".                    C          CONADV.17
C                                                                     C          CONADV.18
C                                                                     C          CONADV.19
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          CONADV.20
#     include <parame.incl>                                                      CONADV.21
#     include <param3.incl>                                                      CONADV.22
#     include <point3d.incl>                                                     CONADV.23
#     include <point2d.incl>                                                     CONADV.24
#     include <various.incl>                                                     CONADV.25
#     include <rpstar.incl>                                                      CONADV.26
#     include <sum.incl>                                                         CONADV.27
C                                                                                CONADV.28
C----------------------------------------------------------------------          CONADV.29
c$omp parallel do default(shared)                                                CONADV.30
c$omp&private(i,j,k)                                                             CONADV.31
      DO J=1,JLX                                                                 CONADV.32
        PARJSUM(ITDADV_SUM,J)=0.0                                                CONADV.33
        PARJSUM(ITQADV_SUM,J)=0.0                                                CONADV.34
C-----ADVECTION OF DRY AIR THROUGH THE LATERAL BOUNDARIES:                       CONADV.35
C                                                                                CONADV.36
C.....ADVECTION THROUGH EAST-WEST BOUNDARIES:                                    CONADV.37
C                                                                                CONADV.38
        IF(J.EQ.1)THEN                                                           CONADV.39
          DO K=1,KL                                                              CONADV.40
            DO I=1,ILX                                                           CONADV.41
              PARJSUM(ITDADV_SUM,J)=PARJSUM(ITDADV_SUM,J)-DTMIN*3.E4*            CONADV.42
     +                              DSIGMA(K)*DX/G*(-(UA(I+1,1,K)+               CONADV.43
     +                              UA(I,1,K))/(MSFX(I,1)*MSFX(I,1)))            CONADV.44
            ENDDO                                                                CONADV.45
          ENDDO                                                                  CONADV.46
        ELSEIF(J.EQ.JLX)THEN                                                     CONADV.47
          DO K=1,KL                                                              CONADV.48
            DO I=1,ILX                                                           CONADV.49
              PARJSUM(ITDADV_SUM,J)=PARJSUM(ITDADV_SUM,J)-DTMIN*3.E4*            CONADV.50
     +                              DSIGMA(K)*DX/G*((UA(I+1,JL,K)+               CONADV.51
     +                              UA(I,JL,K))/(MSFX(I,JLX)*MSFX(I,JLX)         CONADV.52
     +                              ))                                           CONADV.53
            ENDDO                                                                CONADV.54
          ENDDO                                                                  CONADV.55
        ENDIF                                                                    CONADV.56
C                                                                                CONADV.57
C.....ADVECTION THROUGH NORTH-SOUTH BOUNDARIES:                                  CONADV.58
C                                                                                CONADV.59
        DO 20 K=1,KL                                                             CONADV.60
          PARJSUM(ITDADV_SUM,J)=PARJSUM(ITDADV_SUM,J)-DTMIN*3.E4*                CONADV.61
     +                          DSIGMA(K)*DX*((VA(IL,J+1,K)+VA(IL,J,K))/         CONADV.62
     +                          (MSFX(ILX,J)*MSFX(ILX,J))-(VA(1,J+1,K)+          CONADV.63
     +                          VA(1,J,K))/(MSFX(1,J)*MSFX(1,J)))/G              CONADV.64
   20   CONTINUE                                                                 CONADV.65
C                                                                                CONADV.66
C----------------------------------------------------------------------          CONADV.67
C-----ADVECTION OF WATER VAPOR THROUGH THE LATERAL BOUNDARIES:                   CONADV.68
C                                                                                CONADV.69
        IF(ID.NE.1)THEN                                                          CONADV.70
C                                                                                CONADV.71
C.....ADVECTION THROUGH EAST-WEST BOUNDARIES:                                    CONADV.72
C                                                                                CONADV.73
          IF(J.EQ.1)THEN                                                         CONADV.74
            DO K=1,KL                                                            CONADV.75
              DO I=1,ILX                                                         CONADV.76
                PARJSUM(ITQADV_SUM,J)=PARJSUM(ITQADV_SUM,J)-DTMIN*3.E4*          CONADV.77
     +                                DSIGMA(K)*DX/G*(-(UA(I+1,1,K)+             CONADV.78
     +                                UA(I,1,K))*(QVA(I,1,K)*RPSA(I,1))/         CONADV.79
     +                                (MSFX(I,1)*MSFX(I,1)))                     CONADV.80
              ENDDO                                                              CONADV.81
            ENDDO                                                                CONADV.82
          ELSEIF(J.EQ.JLX)THEN                                                   CONADV.83
            DO K=1,KL                                                            CONADV.84
              DO I=1,ILX                                                         CONADV.85
                PARJSUM(ITQADV_SUM,J)=PARJSUM(ITQADV_SUM,J)-DTMIN*3.E4*          CONADV.86
     +                                DSIGMA(K)*DX/G*((UA(I+1,JL,K)+             CONADV.87
     +                                UA(I,JL,K))*(QVA(I,JLX,K)*                 CONADV.88
     +                                RPSA(I,JLX))/(MSFX(I,JLX)*                 CONADV.89
     +                                MSFX(I,JLX)))                              CONADV.90
              ENDDO                                                              CONADV.91
            ENDDO                                                                CONADV.92
          ENDIF                                                                  CONADV.93
C                                                                                CONADV.94
C....ADVECTION THROUGH NORTH-SOUTH BOUNDARIES:                                   CONADV.95
C                                                                                CONADV.96
          DO 40 K=1,KL                                                           CONADV.97
            PARJSUM(ITQADV_SUM,J)=PARJSUM(ITQADV_SUM,J)-DTMIN*3.E4*              CONADV.98
     +                            DSIGMA(K)*DX*((VA(IL,J+1,K)+VA(IL,J,K)         CONADV.99
     +                            )*(QVA(ILX,J,K)*RPSA(ILX,J))/(                 CONADV.100
     +                            MSFX(ILX,J)*MSFX(ILX,J))-(VA(1,J+1,K)+         CONADV.101
     +                            VA(1,J,K))*(QVA(1,J,K)*RPSA(1,J))/(            CONADV.102
     +                            MSFX(1,J)*MSFX(1,J)))/G                        CONADV.103
   40     CONTINUE                                                               CONADV.104
C                                                                                CONADV.105
C-----ADVECTION OF CLOUD WATER AND RAINWATER THROUGH LATERAL BOUNDARIES:         CONADV.106
C                                                                                CONADV.107
          IF(IM.NE.1.AND.IM.NE.3)THEN                                            CONADV.108
C                                                                                CONADV.109
C.....ADVECTION THROUGH EAST-WEST BOUNDARIES:                                    CONADV.110
C                                                                                CONADV.111
            IF(J.EQ.1)THEN                                                       CONADV.112
              DO K=1,KL                                                          CONADV.113
                DO I=1,ILX                                                       CONADV.114
                  PARJSUM(ITQADV_SUM,J)=PARJSUM(ITQADV_SUM,J)-DTMIN*3.E4         CONADV.115
     +                                  *DSIGMA(K)*DX/G*(-(UA(I+1,1,K)+          CONADV.116
     +                                  UA(I,1,K))*(QCA(I,1,K)*RPSA(I,1)         CONADV.117
     +                                  )/(MSFX(I,1)*MSFX(I,1)))                 CONADV.118
                ENDDO                                                            CONADV.119
              ENDDO                                                              CONADV.120
            ELSEIF(J.EQ.JLX)THEN                                                 CONADV.121
              DO K=1,KL                                                          CONADV.122
                DO I=1,ILX                                                       CONADV.123
                  PARJSUM(ITQADV_SUM,J)=PARJSUM(ITQADV_SUM,J)-DTMIN*3.E4         CONADV.124
     +                                  *DSIGMA(K)*DX/G*((UA(I+1,JL,K)+          CONADV.125
     +                                  UA(I,JL,K))*(QCA(I,JLX,K)*               CONADV.126
     +                                  RPSA(I,JLX))/(MSFX(I,JLX)*               CONADV.127
     +                                  MSFX(I,JLX)))                            CONADV.128
                ENDDO                                                            CONADV.129
              ENDDO                                                              CONADV.130
            ENDIF                                                                CONADV.131
C                                                                                CONADV.132
            IF(IICE.EQ.1)THEN                                                    CONADV.133
              IF(J.EQ.1)THEN                                                     CONADV.134
                DO K=1,KL                                                        CONADV.135
                  DO I=1,ILX                                                     CONADV.136
                    PARJSUM(ITQADV_SUM,J)=PARJSUM(ITQADV_SUM,J)-DTMIN*           CONADV.137
     +                                    3.E4*DSIGMA(K)*DX/G*(-(                CONADV.138
     +                                    UA(I+1,1,K)+UA(I,1,K))*(               CONADV.139
     +                                    QIA(I,1,K)*RPSA(I,1))/(                CONADV.140
     +                                    MSFX(I,1)*MSFX(I,1)))                  CONADV.141
                  ENDDO                                                          CONADV.142
                ENDDO                                                            CONADV.143
              ELSEIF(J.EQ.JLX)THEN                                               CONADV.144
                DO K=1,KL                                                        CONADV.145
                  DO I=1,ILX                                                     CONADV.146
                    PARJSUM(ITQADV_SUM,J)=PARJSUM(ITQADV_SUM,J)-DTMIN*           CONADV.147
     +                                    3.E4*DSIGMA(K)*DX/G*((                 CONADV.148
     +                                    UA(I+1,JL,K)+UA(I,JL,K))*(             CONADV.149
     +                                    QIA(I,JLX,K)*RPSA(I,JLX))/(            CONADV.150
     +                                    MSFX(I,JLX)*MSFX(I,JLX)))              CONADV.151
                  ENDDO                                                          CONADV.152
                ENDDO                                                            CONADV.153
              ENDIF                                                              CONADV.154
            ELSE                                                                 CONADV.155
              IF(J.EQ.1)THEN                                                     CONADV.156
                DO K=1,KL                                                        CONADV.157
                  DO I=1,ILX                                                     CONADV.158
                    PARJSUM(ITQADV_SUM,J)=PARJSUM(ITQADV_SUM,J)-DTMIN*           CONADV.159
     +                                    3.E4*DSIGMA(K)*DX/G*(-(                CONADV.160
     +                                    UA(I+1,1,K)+UA(I,1,K))*(               CONADV.161
     +                                    QRA(I,1,K)*RPSA(I,1))/(                CONADV.162
     +                                    MSFX(I,1)*MSFX(I,1)))                  CONADV.163
                  ENDDO                                                          CONADV.164
                ENDDO                                                            CONADV.165
              ELSEIF(J.EQ.JLX)THEN                                               CONADV.166
                DO K=1,KL                                                        CONADV.167
                  DO I=1,ILX                                                     CONADV.168
                    PARJSUM(ITQADV_SUM,J)=PARJSUM(ITQADV_SUM,J)-DTMIN*           CONADV.169
     +                                    3.E4*DSIGMA(K)*DX/G*((                 CONADV.170
     +                                    UA(I+1,JL,K)+UA(I,JL,K))*(             CONADV.171
     +                                    QRA(I,JLX,K)*RPSA(I,JLX))/(            CONADV.172
     +                                    MSFX(I,JLX)*MSFX(I,JLX)))              CONADV.173
                  ENDDO                                                          CONADV.174
                ENDDO                                                            CONADV.175
              ENDIF                                                              CONADV.176
            ENDIF                                                                CONADV.177
C                                                                                CONADV.178
C....ADVECTION THROUGH NORTH-SOUTH BOUNDARIES:                                   CONADV.179
C                                                                                CONADV.180
            DO 70 K=1,KL                                                         CONADV.181
              PARJSUM(ITQADV_SUM,J)=PARJSUM(ITQADV_SUM,J)-DTMIN*3.E4*            CONADV.182
     +                              DSIGMA(K)*DX*((VA(IL,J+1,K)+                 CONADV.183
     +                              VA(IL,J,K))*(QCA(ILX,J,K)*                   CONADV.184
     +                              RPSA(ILX,J))/(MSFX(ILX,J)*                   CONADV.185
     +                              MSFX(ILX,J))-(VA(1,J+1,K)+VA(1,J,K))         CONADV.186
     +                              *(QCA(1,J,K)*RPSA(1,J))/(MSFX(1,J)*          CONADV.187
     +                              MSFX(1,J)))/G                                CONADV.188
   70       CONTINUE                                                             CONADV.189
C                                                                                CONADV.190
            IF(IICE.EQ.1)THEN                                                    CONADV.191
              DO K=1,KL                                                          CONADV.192
                PARJSUM(ITQADV_SUM,J)=PARJSUM(ITQADV_SUM,J)-DTMIN*3.E4*          CONADV.193
     +                                DSIGMA(K)*DX*((VA(IL,J+1,K)+               CONADV.194
     +                                VA(IL,J,K))*(QIA(ILX,J,K)*                 CONADV.195
     +                                RPSA(ILX,J))/(MSFX(ILX,J)*                 CONADV.196
     +                                MSFX(ILX,J))-(VA(1,J+1,K)+                 CONADV.197
     +                                VA(1,J,K))*(QIA(1,J,K)*RPSA(1,J))/         CONADV.198
     +                                (MSFX(1,J)*MSFX(1,J)))/G                   CONADV.199
              ENDDO                                                              CONADV.200
            ELSE                                                                 CONADV.201
              DO K=1,KL                                                          CONADV.202
                PARJSUM(ITQADV_SUM,J)=PARJSUM(ITQADV_SUM,J)-DTMIN*3.E4*          CONADV.203
     +                                DSIGMA(K)*DX*((VA(IL,J+1,K)+               CONADV.204
     +                                VA(IL,J,K))*(QRA(ILX,J,K)*                 CONADV.205
     +                                RPSA(ILX,J))/(MSFX(ILX,J)*                 CONADV.206
     +                                MSFX(ILX,J))-(VA(1,J+1,K)+                 CONADV.207
     +                                VA(1,J,K))*(QRA(1,J,K)*RPSA(1,J))/         CONADV.208
     +                                (MSFX(1,J)*MSFX(1,J)))/G                   CONADV.209
              ENDDO                                                              CONADV.210
            ENDIF                                                                CONADV.211
          ENDIF                                                                  CONADV.212
        ENDIF                                                                    CONADV.213
      ENDDO                                                                      CONADV.214
      DO J=1,JLX                                                                 CONADV.215
        TDADV=TDADV+PARJSUM(ITDADV_SUM,J)                                        CONADV.216
        TQADV=TQADV+PARJSUM(ITQADV_SUM,J)                                        CONADV.217
      ENDDO                                                                      CONADV.218
      RETURN                                                                     CONADV.219
      END                                                                        CONADV.220
                                                                                 CONADV.221
