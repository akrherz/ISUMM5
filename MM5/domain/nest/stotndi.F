          SUBROUTINE STOTNDI (INEST,IYYN,JXXN,IYY,JXX)                           STOTNDI.1
C                                                                                STOTNDI.2
C----------------------------------------------------------------------          STOTNDI.3
C                                                                                STOTNDI.4
#     include <parame.incl>                                                      STOTNDI.5
#     include <addr0.incl>                                                       STOTNDI.6
#     include <param2.incl>                                                      STOTNDI.7
#     include <param3.incl>                                                      STOTNDI.8
#     include <varia.incl>                                                       STOTNDI.9
#     include <point3d.incl>                                                     STOTNDI.10
#     include <point2d.incl>                                                     STOTNDI.11
#     include <nonhyd.incl>                                                      STOTNDI.12
#     include <various.incl>                                                     STOTNDI.13
#     include <nestl.incl>                                                       STOTNDI.14
#     include <point3dn.incl>                                                    STOTNDI.15
#     include <point2dn.incl>                                                    STOTNDI.16
#     include <pointbcn.incl>                                                    STOTNDI.17
#     include <nnnhyd.incl>                                                      STOTNDI.18
#     include <nnnhydb.incl>                                                     STOTNDI.19
#     include <variousn.incl>                                                    STOTNDI.20
#     include <pmoist.incl>                                                      STOTNDI.21
#     include <pbltke.incl>                                                      STOTNDI.22
#     include <pbltken.incl>                                                     STOTNDI.23
#     include <soilp.incl>                                                       07NOV00.920
#     include <soilpn.incl>                                                      07NOV00.921
      REAL RPSA(MIX,MJX)                                                         STOTNDI.24
                                                                                 STOTNDI.25
      COMMON /DEPAR2/ XIG(9),XJG(9),IG0(MJX,MIX),JG0(MJX,MIX)                    STOTNDI.26
      DIMENSION PSCA(MJX,MIX,9),PSCA2(MJX,MIX,9)                                 STOTNDI.27
      DIMENSION PSCA3(MJX,MIX,9),PSCA4(MJX,MIX,9)                                STOTNDI.28
      DIMENSION SLP(MIX,MJX),SLPN(MIX,MJX),                                      STOTNDI.29
     1          HSC3(MIX,MJX),HSC4(MIX,MJX)                                      STOTNDI.30
C                                                                                STOTNDI.31
      JENDC=INT(FLOAT(JXXN)/3.+.001)+JWEST                                       STOTNDI.32
      IENDC=INT(FLOAT(IYYN)/3.+.001)+ISOUTH                                      STOTNDI.33
      IORD=2                                                                     STOTNDI.34
      ISS1=ISOUTH-IORD                                                           STOTNDI.35
      ISE1=ISOUTH+IORD                                                           STOTNDI.36
      INS1=IENDC-1-IORD                                                          STOTNDI.37
      INE1=IENDC-1+IORD                                                          STOTNDI.38
      JWS1=JWEST-IORD                                                            STOTNDI.39
      JWE1=JWEST+IORD                                                            STOTNDI.40
      JES1=JENDC-1-IORD                                                          STOTNDI.41
      JEE1=JENDC-1+IORD                                                          STOTNDI.42
      JW2=1                                                                      STOTNDI.43
      IS2=1                                                                      STOTNDI.44
      JC2=JXX-1                                                                  STOTNDI.45
      IC2=IYY-1                                                                  STOTNDI.46
      IFCT=1                                                                     STOTNDI.47
cmic$ do all autoscope                                                           STOTNDI.48
c$doacross                                                                       STOTNDI.49
c$& local(i,j)                                                                   STOTNDI.50
c$omp parallel do default(shared)                                                STOTNDI.51
c$omp&private(i,j)                                                               STOTNDI.52
      DO J=1,JL-1                                                                STOTNDI.53
        DO I=1,IL-1                                                              STOTNDI.54
          RPSA(I,J)=1./PSA(I,J)                                                  STOTNDI.55
        ENDDO                                                                    STOTNDI.56
      ENDDO                                                                      STOTNDI.57
cmic$ do all autoscope                                                           STOTNDI.58
c$doacross                                                                       STOTNDI.59
c$& local(i,nf,j)                                                                STOTNDI.60
c$omp parallel do default(shared)                                                STOTNDI.61
c$omp&private(i,nf,j)                                                            STOTNDI.62
      DO 30 I=1,MIX                                                              STOTNDI.63
        DO 30 NF=1,9                                                             STOTNDI.64
C     DO 30 I=1,MIX                                                              STOTNDI.65
          DO 30 J=1,MJX                                                          STOTNDI.66
            PSCA(J,I,NF)=0.                                                      STOTNDI.67
            PSCA2(J,I,NF)=0.                                                     STOTNDI.68
   30     CONTINUE                                                               STOTNDI.69
                                                                                 STOTNDI.70
                                                                                 STOTNDI.71
      DO 61 J=1,2                                                                STOTNDI.72
        DO 61 I=1,IYYN-1                                                         STOTNDI.73
          PWBN(I,J)=PSAN(I,J)                                                    STOTNDI.74
          PEBN(I,J)=PSAN(I,JNL-J)                                                STOTNDI.75
   61   CONTINUE                                                                 STOTNDI.76
                                                                                 STOTNDI.77
      DO 62 I=1,2                                                                STOTNDI.78
        DO 62 J=1,JXXN-1                                                         STOTNDI.79
          PSSN(J,I)=PSAN(I,J)                                                    STOTNDI.80
          PNBN(J,I)=PSAN(INL-I,J)                                                STOTNDI.81
   62   CONTINUE                                                                 STOTNDI.82
                                                                                 STOTNDI.83
      CALL DOTS(PSA,HSCR1,IL,JL,MIX,MJX)                                         STOTNDI.84
      CALL DOTS(PSAN,HSCR2,IYYN,JXXN,MIX,MJX)                                    STOTNDI.85
C                                                                                STOTNDI.86
C                                                                                STOTNDI.87
C                                                                                STOTNDI.88
cmic$ do all autoscope                                                           STOTNDI.89
cmic$1 shared (ifct,jwest,isouth,iendc,iord,jendc)                               STOTNDI.90
cmic$2 private (psca,psca2,psca3,psca4)                                          STOTNDI.91
c$doacross                                                                       STOTNDI.92
c$& local(iin,jje,ii,jj,nn,k,j,nf,i,psca,psca2,psca3,psca4)                      STOTNDI.93
c$omp parallel do default(shared)                                                STOTNDI.94
c$omp&private(iin,jje,ii,jj,nn,k,j,nf,i,psca,psca2,psca3,psca4)                  STOTNDI.95
c$omp&shared(jendc,iendc)                                                        STOTNDI.96
      DO 270 K=1,KL                                                              STOTNDI.97
C                                                                                STOTNDI.98
C--- TEMPERATURE FIELDS                                                          STOTNDI.99
C                                                                                STOTNDI.100
        DO 70 NF=1,9                                                             STOTNDI.101
          DO 70 I=IS2,IC2                                                        STOTNDI.102
            DO 70 J=JW2,JC2                                                      STOTNDI.103
              PSCA(J,I,NF)=TA(I,J,K)*RPSA(I,J)                                   STOTNDI.104
   70       CONTINUE                                                             STOTNDI.105
                                                                                 STOTNDI.106
                                                                                 STOTNDI.107
        CALL SINTY(PSCA,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                        STOTNDI.108
        CALL SINTY(PSCA,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                      STOTNDI.109
        DO 80 NN=1,2                                                             STOTNDI.110
          DO 80 I=1,INLX                                                         STOTNDI.111
            II=(I-1)/3+ISOUTH                                                    STOTNDI.112
            JJ=MOD(I-1,3)*3+NN                                                   STOTNDI.113
            JJE=MOD(I-1,3)*3+(4-NN)                                              STOTNDI.114
            TAN(I,NN,K)=PSCA(JWEST,II,JJ)*PSAN(I,NN)                             STOTNDI.115
            TAN(I,JNL-NN,K)=PSCA(JENDC-1,II,JJE)*PSAN(I,JNL-NN)                  STOTNDI.116
            TWBN(I,K,NN)=TAN(I,NN,K)                                             STOTNDI.117
            TEBN(I,K,NN)=TAN(I,JNL-NN,K)                                         STOTNDI.118
   80     CONTINUE                                                               STOTNDI.119
                                                                                 STOTNDI.120
        DO 90 NF=1,9                                                             STOTNDI.121
          DO 90 I=IS2,IC2                                                        STOTNDI.122
            DO 90 J=JW2,JC2                                                      STOTNDI.123
              PSCA(J,I,NF)=TA(I,J,K)*RPSA(I,J)                                   STOTNDI.124
   90       CONTINUE                                                             STOTNDI.125
                                                                                 STOTNDI.126
                                                                                 STOTNDI.127
        CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                        STOTNDI.128
        CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                       STOTNDI.129
        DO 100 NN=1,2                                                            STOTNDI.130
          DO 100 J=1,JNLX                                                        STOTNDI.131
            JJ=(J-1)/3+JWEST                                                     STOTNDI.132
            II=MOD(J-1,3)+3*(NN-1)+1                                             STOTNDI.133
            IIN=MOD(J-1,3)+3*(2-NN)+4                                            STOTNDI.134
            TAN(NN,J,K)=PSCA(JJ,ISOUTH,II)*PSAN(NN,J)                            STOTNDI.135
            TAN(INL-NN,J,K)=PSCA(JJ,IENDC-1,IIN)*PSAN(INL-NN,J)                  STOTNDI.136
            TSBN(J,K,NN)=TAN(NN,J,K)                                             STOTNDI.137
            TNBN(J,K,NN)=TAN(INL-NN,J,K)                                         STOTNDI.138
  100     CONTINUE                                                               STOTNDI.139
                                                                                 STOTNDI.140
C                                                                                STOTNDI.141
C--- QV FIELDS                                                                   STOTNDI.142
C                                                                                STOTNDI.143
        IF(IDRY(INEST).EQ.0)THEN                                                 STOTNDI.144
          DO 110 NF=1,9                                                          STOTNDI.145
            DO 110 I=IS2,IC2                                                     STOTNDI.146
              DO 110 J=JW2,JC2                                                   STOTNDI.147
                PSCA2(J,I,NF)=QVA(I,J,K)*RPSA(I,J)                               STOTNDI.148
  110         CONTINUE                                                           STOTNDI.149
                                                                                 STOTNDI.150
                                                                                 STOTNDI.151
          CALL SINTY(PSCA2,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                     STOTNDI.152
          CALL SINTY(PSCA2,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                   STOTNDI.153
          DO 120 NN=1,2                                                          STOTNDI.154
            DO 120 I=1,INLX                                                      STOTNDI.155
              II=(I-1)/3+ISOUTH                                                  STOTNDI.156
              JJ=MOD(I-1,3)*3+NN                                                 STOTNDI.157
              JJE=MOD(I-1,3)*3+(4-NN)                                            STOTNDI.158
              QVAN(I,NN,K)=PSCA2(JWEST,II,JJ)*PSAN(I,NN)                         STOTNDI.159
              QVAN(I,JNL-NN,K)=PSCA2(JENDC-1,II,JJE)*PSAN(I,JNL-NN)              STOTNDI.160
              QWBN(I,K,NN)=QVAN(I,NN,K)                                          STOTNDI.161
              QEBN(I,K,NN)=QVAN(I,JNL-NN,K)                                      STOTNDI.162
  120       CONTINUE                                                             STOTNDI.163
                                                                                 STOTNDI.164
          DO 130 NF=1,9                                                          STOTNDI.165
            DO 130 I=IS2,IC2                                                     STOTNDI.166
              DO 130 J=JW2,JC2                                                   STOTNDI.167
                PSCA2(J,I,NF)=QVA(I,J,K)*RPSA(I,J)                               STOTNDI.168
  130         CONTINUE                                                           STOTNDI.169
                                                                                 STOTNDI.170
                                                                                 STOTNDI.171
          CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                     STOTNDI.172
          CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                    STOTNDI.173
          DO 140 NN=1,2                                                          STOTNDI.174
            DO 140 J=1,JNLX                                                      STOTNDI.175
              JJ=(J-1)/3+JWEST                                                   STOTNDI.176
              II=MOD(J-1,3)+3*(NN-1)+1                                           STOTNDI.177
              IIN=MOD(J-1,3)+3*(2-NN)+4                                          STOTNDI.178
              QVAN(NN,J,K)=PSCA2(JJ,ISOUTH,II)*PSAN(NN,J)                        STOTNDI.179
              QVAN(INL-NN,J,K)=PSCA2(JJ,IENDC-1,IIN)*PSAN(INL-NN,J)              STOTNDI.180
              QSBN(J,K,NN)=QVAN(NN,J,K)                                          STOTNDI.181
              QNBN(J,K,NN)=QVAN(INL-NN,J,K)                                      STOTNDI.182
  140       CONTINUE                                                             STOTNDI.183
                                                                                 STOTNDI.184
        ENDIF                                                                    STOTNDI.185
C                                                                                STOTNDI.186
C                                                                                STOTNDI.187
        IF(IMOIST(INEST).EQ.2)THEN                                               STOTNDI.188
C                                                                                STOTNDI.189
C--- CLOUD AND RAINWATER FIELDS                                                  STOTNDI.190
C                                                                                STOTNDI.191
          DO 150 NF=1,9                                                          STOTNDI.192
            DO 150 I=IS2,IC2                                                     STOTNDI.193
              DO 150 J=JW2,JC2                                                   STOTNDI.194
                PSCA(J,I,NF)=QCA(I,J,K)*RPSA(I,J)                                STOTNDI.195
                PSCA2(J,I,NF)=QRA(I,J,K)*RPSA(I,J)                               STOTNDI.196
  150         CONTINUE                                                           STOTNDI.197
                                                                                 STOTNDI.198
                                                                                 STOTNDI.199
C                                                                                STOTNDI.200
C--- AT JWEST                                                                    STOTNDI.201
C                                                                                STOTNDI.202
          CALL SINTY(PSCA,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                      STOTNDI.203
          CALL SINTY(PSCA2,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                     STOTNDI.204
C                                                                                STOTNDI.205
C--- AT JENDC-1                                                                  STOTNDI.206
C                                                                                STOTNDI.207
          CALL SINTY(PSCA,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                    STOTNDI.208
          CALL SINTY(PSCA2,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                   STOTNDI.209
          DO 160 NN=1,2                                                          STOTNDI.210
            DO 160 I=1,INLX                                                      STOTNDI.211
              II=(I-1)/3+ISOUTH                                                  STOTNDI.212
              JJ=MOD(I-1,3)*3+NN                                                 STOTNDI.213
              JJE=MOD(I-1,3)*3+(4-NN)                                            STOTNDI.214
              QCAN(I,NN,K)=PSCA(JWEST,II,JJ)*PSAN(I,NN)                          STOTNDI.215
              QCAN(I,JNL-NN,K)=PSCA(JENDC-1,II,JJE)*PSAN(I,JNL-NN)               STOTNDI.216
              QRAN(I,NN,K)=PSCA2(JWEST,II,JJ)*PSAN(I,NN)                         STOTNDI.217
              QRAN(I,JNL-NN,K)=PSCA2(JENDC-1,II,JJE)*PSAN(I,JNL-NN)              STOTNDI.218
              QCWBN(I,K,NN)=QCAN(I,NN,K)                                         STOTNDI.219
              QCEBN(I,K,NN)=QCAN(I,JNL-NN,K)                                     STOTNDI.220
              QRWBN(I,K,NN)=QRAN(I,NN,K)                                         STOTNDI.221
              QREBN(I,K,NN)=QRAN(I,JNL-NN,K)                                     STOTNDI.222
  160       CONTINUE                                                             STOTNDI.223
                                                                                 STOTNDI.224
          DO 170 NF=1,9                                                          STOTNDI.225
            DO 170 I=IS2,IC2                                                     STOTNDI.226
              DO 170 J=JW2,JC2                                                   STOTNDI.227
                PSCA(J,I,NF)=QCA(I,J,K)*RPSA(I,J)                                STOTNDI.228
                PSCA2(J,I,NF)=QRA(I,J,K)*RPSA(I,J)                               STOTNDI.229
  170         CONTINUE                                                           STOTNDI.230
                                                                                 STOTNDI.231
                                                                                 STOTNDI.232
          CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                      STOTNDI.233
          CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                     STOTNDI.234
          CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                     STOTNDI.235
          CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                    STOTNDI.236
          DO 180 NN=1,2                                                          STOTNDI.237
            DO 180 J=1,JNLX                                                      STOTNDI.238
              JJ=(J-1)/3+JWEST                                                   STOTNDI.239
              II=MOD(J-1,3)+3*(NN-1)+1                                           STOTNDI.240
              IIN=MOD(J-1,3)+3*(2-NN)+4                                          STOTNDI.241
              QCAN(NN,J,K)=PSCA(JJ,ISOUTH,II)*PSAN(NN,J)                         STOTNDI.242
              QCAN(INL-NN,J,K)=PSCA(JJ,IENDC-1,IIN)*PSAN(INL-NN,J)               STOTNDI.243
              QRAN(NN,J,K)=PSCA2(JJ,ISOUTH,II)*PSAN(NN,J)                        STOTNDI.244
              QRAN(INL-NN,J,K)=PSCA2(JJ,IENDC-1,IIN)*PSAN(INL-NN,J)              STOTNDI.245
              QCSBN(J,K,NN)=QCAN(NN,J,K)                                         STOTNDI.246
              QCNBN(J,K,NN)=QCAN(INL-NN,J,K)                                     STOTNDI.247
              QRSBN(J,K,NN)=QRAN(NN,J,K)                                         STOTNDI.248
              QRNBN(J,K,NN)=QRAN(INL-NN,J,K)                                     STOTNDI.249
  180       CONTINUE                                                             STOTNDI.250
                                                                                 STOTNDI.251
C                                                                                STOTNDI.252
C--- ICE FIELDS                                                                  STOTNDI.253
          IF(IICE.EQ.1)THEN                                                      STOTNDI.254
C                                                                                STOTNDI.255
            DO 155 NF=1,9                                                        STOTNDI.256
              DO 155 I=IS2,IC2                                                   STOTNDI.257
                DO 155 J=JW2,JC2                                                 STOTNDI.258
                  PSCA(J,I,NF)=QIA(I,J,K)*RPSA(I,J)                              STOTNDI.259
                  PSCA2(J,I,NF)=QNIA(I,J,K)*RPSA(I,J)                            STOTNDI.260
                  IF(IICEG.EQ.1)THEN                                             STOTNDI.261
                    PSCA3(J,I,NF)=QGA(I,J,K)*RPSA(I,J)                           STOTNDI.262
                    PSCA4(J,I,NF)=QNCA(I,J,K)*RPSA(I,J)                          STOTNDI.263
                  ENDIF                                                          STOTNDI.264
  155           CONTINUE                                                         STOTNDI.265
                                                                                 STOTNDI.266
                                                                                 STOTNDI.267
C                                                                                STOTNDI.268
C--- AT JWEST                                                                    STOTNDI.269
C                                                                                STOTNDI.270
            CALL SINTY(PSCA,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                    STOTNDI.271
            CALL SINTY(PSCA2,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                   STOTNDI.272
            IF(IICEG.EQ.1)THEN                                                   STOTNDI.273
              CALL SINTY(PSCA3,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                 STOTNDI.274
              CALL SINTY(PSCA4,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                 STOTNDI.275
            ENDIF                                                                STOTNDI.276
C                                                                                STOTNDI.277
C--- AT JENDC-1                                                                  STOTNDI.278
C                                                                                STOTNDI.279
            CALL SINTY(PSCA,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                  STOTNDI.280
            CALL SINTY(PSCA2,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                 STOTNDI.281
            IF(IICEG.EQ.1)THEN                                                   STOTNDI.282
              CALL SINTY(PSCA3,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)               STOTNDI.283
              CALL SINTY(PSCA4,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)               STOTNDI.284
            ENDIF                                                                STOTNDI.285
            DO 165 NN=1,2                                                        STOTNDI.286
              DO 165 I=1,INLX                                                    STOTNDI.287
                II=(I-1)/3+ISOUTH                                                STOTNDI.288
                JJ=MOD(I-1,3)*3+NN                                               STOTNDI.289
                JJE=MOD(I-1,3)*3+(4-NN)                                          STOTNDI.290
                QIAN(I,NN,K)=PSCA(JWEST,II,JJ)*PSAN(I,NN)                        STOTNDI.291
                QIAN(I,JNL-NN,K)=PSCA(JENDC-1,II,JJE)*PSAN(I,JNL-NN)             STOTNDI.292
                QNIAN(I,NN,K)=PSCA2(JWEST,II,JJ)*PSAN(I,NN)                      STOTNDI.293
                QNIAN(I,JNL-NN,K)=PSCA2(JENDC-1,II,JJE)*PSAN(I,JNL-NN)           STOTNDI.294
                QIWBN(I,K,NN)=QIAN(I,NN,K)                                       STOTNDI.295
                QIEBN(I,K,NN)=QIAN(I,JNL-NN,K)                                   STOTNDI.296
                QNIWBN(I,K,NN)=QNIAN(I,NN,K)                                     STOTNDI.297
                QNIEBN(I,K,NN)=QNIAN(I,JNL-NN,K)                                 STOTNDI.298
                IF(IICEG.EQ.1)THEN                                               STOTNDI.299
                  QGAN(I,NN,K)=PSCA3(JWEST,II,JJ)*PSAN(I,NN)                     STOTNDI.300
                  QGAN(I,JNL-NN,K)=PSCA3(JENDC-1,II,JJE)*PSAN(I,JNL-NN)          STOTNDI.301
                  QGWBN(I,K,NN)=QGAN(I,NN,K)                                     STOTNDI.302
                  QGEBN(I,K,NN)=QGAN(I,JNL-NN,K)                                 STOTNDI.303
                  QNCAN(I,NN,K)=PSCA4(JWEST,II,JJ)*PSAN(I,NN)                    STOTNDI.304
                  QNCAN(I,JNL-NN,K)=PSCA4(JENDC-1,II,JJE)*PSAN(I,JNL-NN)         STOTNDI.305
                  QNCWBN(I,K,NN)=QNCAN(I,NN,K)                                   STOTNDI.306
                  QNCEBN(I,K,NN)=QNCAN(I,JNL-NN,K)                               STOTNDI.307
                ENDIF                                                            STOTNDI.308
  165         CONTINUE                                                           STOTNDI.309
                                                                                 STOTNDI.310
            DO 175 NF=1,9                                                        STOTNDI.311
              DO 175 I=IS2,IC2                                                   STOTNDI.312
                DO 175 J=JW2,JC2                                                 STOTNDI.313
                  PSCA(J,I,NF)=QIA(I,J,K)*RPSA(I,J)                              STOTNDI.314
                  PSCA2(J,I,NF)=QNIA(I,J,K)*RPSA(I,J)                            STOTNDI.315
                  IF(IICEG.EQ.1)THEN                                             STOTNDI.316
                    PSCA3(J,I,NF)=QGA(I,J,K)*RPSA(I,J)                           STOTNDI.317
                    PSCA4(J,I,NF)=QNCA(I,J,K)*RPSA(I,J)                          STOTNDI.318
                  ENDIF                                                          STOTNDI.319
  175           CONTINUE                                                         STOTNDI.320
                                                                                 STOTNDI.321
                                                                                 STOTNDI.322
            CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                    STOTNDI.323
            CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                   STOTNDI.324
            CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                   STOTNDI.325
            CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                  STOTNDI.326
            IF(IICEG.EQ.1)THEN                                                   STOTNDI.327
              CALL SINTX(PSCA3,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                 STOTNDI.328
              CALL SINTX(PSCA3,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                STOTNDI.329
              CALL SINTX(PSCA4,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                 STOTNDI.330
              CALL SINTX(PSCA4,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                STOTNDI.331
            ENDIF                                                                STOTNDI.332
            DO 185 NN=1,2                                                        STOTNDI.333
              DO 185 J=1,JNLX                                                    STOTNDI.334
                JJ=(J-1)/3+JWEST                                                 STOTNDI.335
                II=MOD(J-1,3)+3*(NN-1)+1                                         STOTNDI.336
                IIN=MOD(J-1,3)+3*(2-NN)+4                                        STOTNDI.337
                QIAN(NN,J,K)=PSCA(JJ,ISOUTH,II)*PSAN(NN,J)                       STOTNDI.338
                QIAN(INL-NN,J,K)=PSCA(JJ,IENDC-1,IIN)*PSAN(INL-NN,J)             STOTNDI.339
                QNIAN(NN,J,K)=PSCA2(JJ,ISOUTH,II)*PSAN(NN,J)                     STOTNDI.340
                QNIAN(INL-NN,J,K)=PSCA2(JJ,IENDC-1,IIN)*PSAN(INL-NN,J)           STOTNDI.341
                QISBN(J,K,NN)=QIAN(NN,J,K)                                       STOTNDI.342
                QINBN(J,K,NN)=QIAN(INL-NN,J,K)                                   STOTNDI.343
                QNISBN(J,K,NN)=QNIAN(NN,J,K)                                     STOTNDI.344
                QNINBN(J,K,NN)=QNIAN(INL-NN,J,K)                                 STOTNDI.345
                IF(IICEG.EQ.1)THEN                                               STOTNDI.346
                  QGAN(NN,J,K)=PSCA3(JJ,ISOUTH,II)*PSAN(NN,J)                    STOTNDI.347
                  QGAN(INL-NN,J,K)=PSCA3(JJ,IENDC-1,IIN)*PSAN(INL-NN,J)          STOTNDI.348
                  QNCAN(NN,J,K)=PSCA4(JJ,ISOUTH,II)*PSAN(NN,J)                   STOTNDI.349
                  QNCAN(INL-NN,J,K)=PSCA4(JJ,IENDC-1,IIN)*PSAN(INL-NN,J)         STOTNDI.350
                  QGSBN(J,K,NN)=QGAN(NN,J,K)                                     STOTNDI.351
                  QGNBN(J,K,NN)=QGAN(INL-NN,J,K)                                 STOTNDI.352
                  QNCSBN(J,K,NN)=QNCAN(NN,J,K)                                   STOTNDI.353
                  QNCNBN(J,K,NN)=QNCAN(INL-NN,J,K)                               STOTNDI.354
                ENDIF                                                            STOTNDI.355
  185         CONTINUE                                                           STOTNDI.356
                                                                                 STOTNDI.357
          ENDIF                                                                  STOTNDI.358
        ENDIF                                                                    STOTNDI.359
C                                                                                STOTNDI.360
C--- WIND FIELDS                                                                 STOTNDI.361
C                                                                                STOTNDI.362
        DO 190 NF=1,9                                                            STOTNDI.363
          DO 190 I=1,IL                                                          STOTNDI.364
            DO 190 J=1,JL                                                        STOTNDI.365
              PSCA(J,I,NF)=UA(I,J,K)/HSCR1(I,J)                                  STOTNDI.366
              PSCA2(J,I,NF)=VA(I,J,K)/HSCR1(I,J)                                 STOTNDI.367
  190       CONTINUE                                                             STOTNDI.368
                                                                                 STOTNDI.369
                                                                                 STOTNDI.370
C                                                                                STOTNDI.371
C--- AT JWEST                                                                    STOTNDI.372
C                                                                                STOTNDI.373
        CALL SINTY(PSCA,MJX,MIX,JWEST,ISOUTH,IENDC,1,0)                          STOTNDI.374
        CALL SINTY(PSCA2,MJX,MIX,JWEST,ISOUTH,IENDC,1,0)                         STOTNDI.375
C                                                                                STOTNDI.376
C--- AT JENDC-1                                                                  STOTNDI.377
C                                                                                STOTNDI.378
        CALL SINTY(PSCA,MJX,MIX,JENDC,ISOUTH,IENDC,2,0)                          STOTNDI.379
        CALL SINTY(PSCA2,MJX,MIX,JENDC,ISOUTH,IENDC,2,0)                         STOTNDI.380
        DO 200 NN=1,2                                                            STOTNDI.381
          DO 200 I=1,INL                                                         STOTNDI.382
            II=I/3+ISOUTH                                                        STOTNDI.383
            JJ=3*MOD(I,3)+(NN+1)                                                 STOTNDI.384
            JJE=3*MOD(I,3)+(3-NN)                                                STOTNDI.385
            UAN(I,NN,K)=PSCA(JWEST,II,JJ)*HSCR2(I,NN)                            STOTNDI.386
            UAN(I,JNL-NN+1,K)=PSCA(JENDC,II,JJE)*HSCR2(I,JNL-NN+1)               STOTNDI.387
            VAN(I,NN,K)=PSCA2(JWEST,II,JJ)*HSCR2(I,NN)                           STOTNDI.388
            VAN(I,JNL-NN+1,K)=PSCA2(JENDC,II,JJE)*HSCR2(I,JNL-NN+1)              STOTNDI.389
            UWBN(I,K,NN)=UAN(I,NN,K)                                             STOTNDI.390
            UEBN(I,K,NN)=UAN(I,JNL-NN+1,K)                                       STOTNDI.391
            VWBN(I,K,NN)=VAN(I,NN,K)                                             STOTNDI.392
            VEBN(I,K,NN)=VAN(I,JNL-NN+1,K)                                       STOTNDI.393
  200     CONTINUE                                                               STOTNDI.394
                                                                                 STOTNDI.395
        DO 210 NF=1,9                                                            STOTNDI.396
          DO 210 I=1,IL                                                          STOTNDI.397
            DO 210 J=1,JL                                                        STOTNDI.398
              PSCA(J,I,NF)=UA(I,J,K)/HSCR1(I,J)                                  STOTNDI.399
              PSCA2(J,I,NF)=VA(I,J,K)/HSCR1(I,J)                                 STOTNDI.400
  210       CONTINUE                                                             STOTNDI.401
                                                                                 STOTNDI.402
                                                                                 STOTNDI.403
        CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC,ISOUTH,1,0)                          STOTNDI.404
        CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC,IENDC,2,0)                           STOTNDI.405
        CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC,ISOUTH,1,0)                         STOTNDI.406
        CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC,IENDC,2,0)                          STOTNDI.407
        DO 220 NN=1,2                                                            STOTNDI.408
          DO 220 J=1,JNL                                                         STOTNDI.409
            JJ=J/3+JWEST                                                         STOTNDI.410
            II=MOD(J,3)+3*(NN+1)-2                                               STOTNDI.411
            IIN=MOD(J,3)+3*(3-NN)-2                                              STOTNDI.412
            UAN(NN,J,K)=PSCA(JJ,ISOUTH,II)*HSCR2(NN,J)                           STOTNDI.413
            UAN(INL-NN+1,J,K)=PSCA(JJ,IENDC,IIN)*HSCR2(INL-NN+1,J)               STOTNDI.414
            VAN(NN,J,K)=PSCA2(JJ,ISOUTH,II)*HSCR2(NN,J)                          STOTNDI.415
            VAN(INL-NN+1,J,K)=PSCA2(JJ,IENDC,IIN)*HSCR2(INL-NN+1,J)              STOTNDI.416
            USBN(J,K,NN)=UAN(NN,J,K)                                             STOTNDI.417
            UNBN(J,K,NN)=UAN(INL-NN+1,J,K)                                       STOTNDI.418
            VSBN(J,K,NN)=VAN(NN,J,K)                                             STOTNDI.419
            VNBN(J,K,NN)=VAN(INL-NN+1,J,K)                                       STOTNDI.420
  220     CONTINUE                                                               STOTNDI.421
                                                                                 STOTNDI.422
C                                                                                STOTNDI.423
C                                                                                STOTNDI.424
C--- NON-HYDROSTATIC FIELDS IN NEXT SECTION                                      STOTNDI.425
C                                                                                STOTNDI.426
        DO 230 NF=1,9                                                            STOTNDI.427
          DO 230 I=IS2,IC2                                                       STOTNDI.428
            DO 230 J=JW2,JC2                                                     STOTNDI.429
              PSCA(J,I,NF)=PPA(I,J,K)*RPSA(I,J)                                  STOTNDI.430
  230       CONTINUE                                                             STOTNDI.431
                                                                                 STOTNDI.432
                                                                                 STOTNDI.433
        CALL SINTY(PSCA,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                        STOTNDI.434
        CALL SINTY(PSCA,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                      STOTNDI.435
        DO 240 NN=1,2                                                            STOTNDI.436
          DO 240 I=1,INLX                                                        STOTNDI.437
            II=(I-1)/3+ISOUTH                                                    STOTNDI.438
            JJ=MOD(I-1,3)*3+NN                                                   STOTNDI.439
            JJE=MOD(I-1,3)*3+(4-NN)                                              STOTNDI.440
            PPAN(I,NN,K)=PSCA(JWEST,II,JJ)*PSAN(I,NN)                            STOTNDI.441
            PPAN(I,JNL-NN,K)=PSCA(JENDC-1,II,JJE)*PSAN(I,JNL-NN)                 STOTNDI.442
            PPWBN(I,K,NN)=PPAN(I,NN,K)                                           STOTNDI.443
            PPEBN(I,K,NN)=PPAN(I,JNL-NN,K)                                       STOTNDI.444
  240     CONTINUE                                                               STOTNDI.445
                                                                                 STOTNDI.446
        DO 250 NF=1,9                                                            STOTNDI.447
          DO 250 I=IS2,IC2                                                       STOTNDI.448
            DO 250 J=JW2,JC2                                                     STOTNDI.449
              PSCA(J,I,NF)=PPA(I,J,K)*RPSA(I,J)                                  STOTNDI.450
  250       CONTINUE                                                             STOTNDI.451
                                                                                 STOTNDI.452
                                                                                 STOTNDI.453
        CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                        STOTNDI.454
        CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                       STOTNDI.455
        DO 260 NN=1,2                                                            STOTNDI.456
          DO 260 J=1,JNLX                                                        STOTNDI.457
            JJ=(J-1)/3+JWEST                                                     STOTNDI.458
            II=MOD(J-1,3)+3*(NN-1)+1                                             STOTNDI.459
            IIN=MOD(J-1,3)+3*(2-NN)+4                                            STOTNDI.460
            PPAN(NN,J,K)=PSCA(JJ,ISOUTH,II)*PSAN(NN,J)                           STOTNDI.461
            PPAN(INL-NN,J,K)=PSCA(JJ,IENDC-1,IIN)*PSAN(INL-NN,J)                 STOTNDI.462
            PPSBN(J,K,NN)=PPAN(NN,J,K)                                           STOTNDI.463
            PPNBN(J,K,NN)=PPAN(INL-NN,J,K)                                       STOTNDI.464
  260     CONTINUE                                                               STOTNDI.465
                                                                                 STOTNDI.466
C                                                                                STOTNDI.467
  270 CONTINUE                                                                   STOTNDI.468
C                                                                                STOTNDI.469
C                                                                                STOTNDI.470
C--- RAINFALL FIELDS                                                             STOTNDI.471
C                                                                                STOTNDI.472
      DO 272 NF=1,9                                                              STOTNDI.473
        DO 272 I=IS2,IC2                                                         STOTNDI.474
          DO 272 J=JW2,JC2                                                       STOTNDI.475
            PSCA(J,I,NF)=RAINC(I,J)                                              STOTNDI.476
            PSCA2(J,I,NF)=RAINNC(I,J)                                            STOTNDI.477
  272     CONTINUE                                                               STOTNDI.478
                                                                                 STOTNDI.479
                                                                                 STOTNDI.480
C                                                                                STOTNDI.481
C--- AT JWEST                                                                    STOTNDI.482
C                                                                                STOTNDI.483
      CALL SINTY(PSCA,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                          STOTNDI.484
      CALL SINTY(PSCA2,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                         STOTNDI.485
C                                                                                STOTNDI.486
C--- AT JENDC-1                                                                  STOTNDI.487
C                                                                                STOTNDI.488
      CALL SINTY(PSCA,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                        STOTNDI.489
      CALL SINTY(PSCA2,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                       STOTNDI.490
      DO 274 NN=1,2                                                              STOTNDI.491
        DO 274 I=1,INLX                                                          STOTNDI.492
          II=(I-1)/3+ISOUTH                                                      STOTNDI.493
          JJ=MOD(I-1,3)*3+NN                                                     STOTNDI.494
          JJE=MOD(I-1,3)*3+(4-NN)                                                STOTNDI.495
          RAINCN(I,NN)=PSCA(JWEST,II,JJ)                                         STOTNDI.496
          RAINCN(I,JNL-NN)=PSCA(JENDC-1,II,JJE)                                  STOTNDI.497
          RAINNN(I,NN)=PSCA2(JWEST,II,JJ)                                        STOTNDI.498
          RAINNN(I,JNL-NN)=PSCA2(JENDC-1,II,JJE)                                 STOTNDI.499
  274   CONTINUE                                                                 STOTNDI.500
                                                                                 STOTNDI.501
      DO 276 NF=1,9                                                              STOTNDI.502
        DO 276 I=IS2,IC2                                                         STOTNDI.503
          DO 276 J=JW2,JC2                                                       STOTNDI.504
            PSCA(J,I,NF)=RAINC(I,J)                                              STOTNDI.505
            PSCA2(J,I,NF)=RAINNC(I,J)                                            STOTNDI.506
  276     CONTINUE                                                               STOTNDI.507
                                                                                 STOTNDI.508
                                                                                 STOTNDI.509
      CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                          STOTNDI.510
      CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                         STOTNDI.511
      CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                         STOTNDI.512
      CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                        STOTNDI.513
      DO 278 NN=1,2                                                              STOTNDI.514
        DO 278 J=1,JNLX                                                          STOTNDI.515
          JJ=(J-1)/3+JWEST                                                       STOTNDI.516
          II=MOD(J-1,3)+3*(NN-1)+1                                               STOTNDI.517
          IIN=MOD(J-1,3)+3*(2-NN)+4                                              STOTNDI.518
          RAINCN(NN,J)=PSCA(JJ,ISOUTH,II)                                        STOTNDI.519
          RAINCN(INL-NN,J)=PSCA(JJ,IENDC-1,IIN)                                  STOTNDI.520
          RAINNN(NN,J)=PSCA2(JJ,ISOUTH,II)                                       STOTNDI.521
          RAINNN(INL-NN,J)=PSCA2(JJ,IENDC-1,IIN)                                 STOTNDI.522
  278   CONTINUE                                                                 STOTNDI.523
                                                                                 STOTNDI.524
C                                                                                STOTNDI.525
cmic$ do all autoscope                                                           STOTNDI.526
cmic$1 shared (ifct,jwest,isouth,iendc,iord,jendc)                               STOTNDI.527
cmic$2 private (psca,psca2,psca3,psca4)                                          STOTNDI.528
c$doacross                                                                       STOTNDI.529
c$& local(iin,jje,ii,jj,nn,k,j,nf,i,psca,psca2)                                  STOTNDI.530
c$omp parallel do default(shared)                                                STOTNDI.531
c$omp&private(iin,jje,ii,jj,nn,k,j,nf,i,psca,psca2)                              STOTNDI.532
      DO 320 K=1,KL+1                                                            STOTNDI.533
C                                                                                STOTNDI.534
        DO 280 NF=1,9                                                            STOTNDI.535
          DO 280 I=IS2,IC2                                                       STOTNDI.536
            DO 280 J=JW2,JC2                                                     STOTNDI.537
              PSCA(J,I,NF)=WA(I,J,K)*RPSA(I,J)                                   STOTNDI.538
  280       CONTINUE                                                             STOTNDI.539
                                                                                 STOTNDI.540
                                                                                 STOTNDI.541
        CALL SINTY(PSCA,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                        STOTNDI.542
        CALL SINTY(PSCA,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                      STOTNDI.543
        DO 290 NN=1,2                                                            STOTNDI.544
          DO 290 I=1,INLX                                                        STOTNDI.545
            II=(I-1)/3+ISOUTH                                                    STOTNDI.546
            JJ=MOD(I-1,3)*3+NN                                                   STOTNDI.547
            JJE=MOD(I-1,3)*3+(4-NN)                                              STOTNDI.548
            WAN(I,NN,K)=PSCA(JWEST,II,JJ)*PSAN(I,NN)                             STOTNDI.549
            WAN(I,JNL-NN,K)=PSCA(JENDC-1,II,JJE)*PSAN(I,JNL-NN)                  STOTNDI.550
            WWBN(I,K,NN)=WAN(I,NN,K)                                             STOTNDI.551
            WEBN(I,K,NN)=WAN(I,JNL-NN,K)                                         STOTNDI.552
  290     CONTINUE                                                               STOTNDI.553
                                                                                 STOTNDI.554
        DO 300 NF=1,9                                                            STOTNDI.555
          DO 300 I=IS2,IC2                                                       STOTNDI.556
            DO 300 J=JW2,JC2                                                     STOTNDI.557
              PSCA(J,I,NF)=WA(I,J,K)*RPSA(I,J)                                   STOTNDI.558
  300       CONTINUE                                                             STOTNDI.559
                                                                                 STOTNDI.560
                                                                                 STOTNDI.561
        CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                        STOTNDI.562
        CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                       STOTNDI.563
        DO 310 NN=1,2                                                            STOTNDI.564
          DO 310 J=1,JNLX                                                        STOTNDI.565
            JJ=(J-1)/3+JWEST                                                     STOTNDI.566
            II=MOD(J-1,3)+3*(NN-1)+1                                             STOTNDI.567
            IIN=MOD(J-1,3)+3*(2-NN)+4                                            STOTNDI.568
            WAN(NN,J,K)=PSCA(JJ,ISOUTH,II)*PSAN(NN,J)                            STOTNDI.569
            WAN(INL-NN,J,K)=PSCA(JJ,IENDC-1,IIN)*PSAN(INL-NN,J)                  STOTNDI.570
            WSBN(J,K,NN)=WAN(NN,J,K)                                             STOTNDI.571
            WNBN(J,K,NN)=WAN(INL-NN,J,K)                                         STOTNDI.572
  310     CONTINUE                                                               STOTNDI.573
                                                                                 STOTNDI.574
  320 CONTINUE                                                                   STOTNDI.575
C                                                                                STOTNDI.576
      IF(IGSPBL.EQ.1)THEN                                                        STOTNDI.577
cmic$ do all autoscope                                                           STOTNDI.578
cmic$1 shared (jwest,isouth,iendc,jendc)                                         STOTNDI.579
cmic$2 private (psca)                                                            STOTNDI.580
c$doacross                                                                       STOTNDI.581
c$& local(iin,jje,ii,jj,nn,k,j,nf,i,psca)                                        STOTNDI.582
c$omp parallel do default(shared)                                                STOTNDI.583
c$omp&private(iin,jje,ii,jj,nn,k,j,nf,i,psca)                                    STOTNDI.584
        DO 370 K=1,KL+1                                                          STOTNDI.585
C                                                                                STOTNDI.586
          DO 330 NF=1,9                                                          STOTNDI.587
            DO 330 I=IS2,IC2                                                     STOTNDI.588
              DO 330 J=JW2,JC2                                                   STOTNDI.589
                PSCA(J,I,NF)=TKEA(I,J,K)                                         STOTNDI.590
  330         CONTINUE                                                           STOTNDI.591
                                                                                 STOTNDI.592
                                                                                 STOTNDI.593
          CALL SINTY(PSCA,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                      STOTNDI.594
          CALL SINTY(PSCA,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                    STOTNDI.595
          DO 340 NN=1,2                                                          STOTNDI.596
            DO 340 I=1,INLX                                                      STOTNDI.597
              II=(I-1)/3+ISOUTH                                                  STOTNDI.598
              JJ=MOD(I-1,3)*3+NN                                                 STOTNDI.599
              JJE=MOD(I-1,3)*3+(4-NN)                                            STOTNDI.600
              TKEAN(I,NN,K)=PSCA(JWEST,II,JJ)                                    STOTNDI.601
              TKEAN(I,JNL-NN,K)=PSCA(JENDC-1,II,JJE)                             STOTNDI.602
  340       CONTINUE                                                             STOTNDI.603
                                                                                 STOTNDI.604
          DO 350 NF=1,9                                                          STOTNDI.605
            DO 350 I=IS2,IC2                                                     STOTNDI.606
              DO 350 J=JW2,JC2                                                   STOTNDI.607
                PSCA(J,I,NF)=TKEA(I,J,K)                                         STOTNDI.608
  350         CONTINUE                                                           STOTNDI.609
                                                                                 STOTNDI.610
                                                                                 STOTNDI.611
          CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                      STOTNDI.612
          CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                     STOTNDI.613
          DO 360 NN=1,2                                                          STOTNDI.614
            DO 360 J=1,JNLX                                                      STOTNDI.615
              JJ=(J-1)/3+JWEST                                                   STOTNDI.616
              II=MOD(J-1,3)+3*(NN-1)+1                                           STOTNDI.617
              IIN=MOD(J-1,3)+3*(2-NN)+4                                          STOTNDI.618
              TKEAN(NN,J,K)=PSCA(JJ,ISOUTH,II)                                   STOTNDI.619
              TKEAN(INL-NN,J,K)=PSCA(JJ,IENDC-1,IIN)                             STOTNDI.620
  360       CONTINUE                                                             STOTNDI.621
                                                                                 STOTNDI.622
  370   CONTINUE                                                                 STOTNDI.623
      ENDIF                                                                      STOTNDI.624
                                                                                 07NOV00.922
      IF(ISOIL.EQ.2.OR.ISOIL.EQ.3)THEN                                           07NOV00.923
      DO L=1,MLX                                                                 07NOV00.924
C                                                                                07NOV00.925
C--- LSM SOIL FIELDS                                                             07NOV00.926
C                                                                                07NOV00.927
      DO 372 NF=1,9                                                              07NOV00.928
        DO 372 I=IS2,IC2                                                         07NOV00.929
          DO 372 J=JW2,JC2                                                       07NOV00.930
            PSCA(J,I,NF)=STCA(I,J,L)                                             07NOV00.931
            PSCA2(J,I,NF)=SMCA(I,J,L)                                            07NOV00.932
            PSCA3(J,I,NF)=SWCA(I,J,L)                                            19DEC02.1216
  372     CONTINUE                                                               07NOV00.933
C                                                                                07NOV00.934
C--- AT JWEST                                                                    07NOV00.935
C                                                                                07NOV00.936
      CALL SINTY(PSCA,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                          07NOV00.937
      CALL SINTY(PSCA2,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                         07NOV00.938
      CALL SINTY(PSCA3,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                         19DEC02.1217
C                                                                                07NOV00.939
C--- AT JENDC-1                                                                  07NOV00.940
C                                                                                07NOV00.941
      CALL SINTY(PSCA,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                        07NOV00.942
      CALL SINTY(PSCA2,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                       07NOV00.943
      CALL SINTY(PSCA3,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                       19DEC02.1218
      DO 374 NN=1,2                                                              07NOV00.944
        DO 374 I=1,INLX                                                          07NOV00.945
          II=(I-1)/3+ISOUTH                                                      07NOV00.946
          JJ=MOD(I-1,3)*3+NN                                                     07NOV00.947
          JJE=MOD(I-1,3)*3+(4-NN)                                                07NOV00.948
          STCAN(I,NN,L)=PSCA(JWEST,II,JJ)                                        07NOV00.949
          STCAN(I,JNL-NN,L)=PSCA(JENDC-1,II,JJE)                                 07NOV00.950
          STCBN(I,NN,L)=STCAN(I,NN,L)                                            07NOV00.951
          STCBN(I,JNL-NN,L)=STCAN(I,JNL-NN,L)                                    07NOV00.952
          SMCAN(I,NN,L)=PSCA2(JWEST,II,JJ)                                       07NOV00.953
          SMCAN(I,JNL-NN,L)=PSCA2(JENDC-1,II,JJE)                                07NOV00.954
          SMCBN(I,NN,L)=SMCAN(I,NN,L)                                            07NOV00.955
          SMCBN(I,JNL-NN,L)=SMCAN(I,JNL-NN,L)                                    07NOV00.956
          SWCAN(I,NN,L)=PSCA3(JWEST,II,JJ)                                       19DEC02.1219
          SWCAN(I,JNL-NN,L)=PSCA3(JENDC-1,II,JJE)                                19DEC02.1220
          SWCBN(I,NN,L)=SWCAN(I,NN,L)                                            19DEC02.1221
          SWCBN(I,JNL-NN,L)=SWCAN(I,JNL-NN,L)                                    19DEC02.1222
  374   CONTINUE                                                                 07NOV00.957
                                                                                 07NOV00.958
      DO 376 NF=1,9                                                              07NOV00.959
        DO 376 I=IS2,IC2                                                         07NOV00.960
          DO 376 J=JW2,JC2                                                       07NOV00.961
            PSCA(J,I,NF)=STCA(I,J,L)                                             07NOV00.962
            PSCA2(J,I,NF)=SMCA(I,J,L)                                            07NOV00.963
            PSCA3(J,I,NF)=SWCA(I,J,L)                                            19DEC02.1223
  376     CONTINUE                                                               07NOV00.964
                                                                                 07NOV00.965
      CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                          07NOV00.966
      CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                         07NOV00.967
      CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                         07NOV00.968
      CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                        07NOV00.969
      CALL SINTX(PSCA3,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                         19DEC02.1224
      CALL SINTX(PSCA3,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                        19DEC02.1225
      DO 378 NN=1,2                                                              07NOV00.970
        DO 378 J=1,JNLX                                                          07NOV00.971
          JJ=(J-1)/3+JWEST                                                       07NOV00.972
          II=MOD(J-1,3)+3*(NN-1)+1                                               07NOV00.973
          IIN=MOD(J-1,3)+3*(2-NN)+4                                              07NOV00.974
          STCAN(NN,J,L)=PSCA(JJ,ISOUTH,II)                                       07NOV00.975
          STCAN(INL-NN,J,L)=PSCA(JJ,IENDC-1,IIN)                                 07NOV00.976
          STCBN(NN,J,L)=STCAN(NN,J,L)                                            07NOV00.977
          STCBN(INL-NN,J,L)=STCAN(INL-NN,J,L)                                    07NOV00.978
          SMCAN(NN,J,L)=PSCA2(JJ,ISOUTH,II)                                      07NOV00.979
          SMCAN(INL-NN,J,L)=PSCA2(JJ,IENDC-1,IIN)                                07NOV00.980
          SMCBN(NN,J,L)=SMCAN(NN,J,L)                                            07NOV00.981
          SMCBN(INL-NN,J,L)=SMCAN(INL-NN,J,L)                                    07NOV00.982
          SWCAN(NN,J,L)=PSCA3(JJ,ISOUTH,II)                                      19DEC02.1226
          SWCAN(INL-NN,J,L)=PSCA3(JJ,IENDC-1,IIN)                                19DEC02.1227
          SWCBN(NN,J,L)=SWCAN(NN,J,L)                                            19DEC02.1228
          SWCBN(INL-NN,J,L)=SWCAN(INL-NN,J,L)                                    19DEC02.1229
  378   CONTINUE                                                                 07NOV00.983
      ENDDO                                                                      07NOV00.984
      ENDIF                                                                      07NOV00.985
      RETURN                                                                     STOTNDI.625
      END                                                                        STOTNDI.626
