          SUBROUTINE STOTNDT (INEST,IYYN,JXXN,IYY,JXX)                           STOTNDT.1
C                                                                                STOTNDT.2
C----------------------------------------------------------------------          STOTNDT.3
C                                                                                STOTNDT.4
#     include <parame.incl>                                                      STOTNDT.5
#     include <addr0.incl>                                                       STOTNDT.6
#     include <point3d.incl>                                                     STOTNDT.7
#     include <point2d.incl>                                                     STOTNDT.8
#     include <nonhyd.incl>                                                      STOTNDT.9
#     include <various.incl>                                                     STOTNDT.10
#     include <nestl.incl>                                                       STOTNDT.11
#     include <point3dn.incl>                                                    STOTNDT.12
#     include <point2dn.incl>                                                    STOTNDT.13
#     include <pointbcn.incl>                                                    STOTNDT.14
#     include <nnnhyd.incl>                                                      STOTNDT.15
#     include <nnnhydb.incl>                                                     STOTNDT.16
#     include <variousn.incl>                                                    STOTNDT.17
#     include <param2.incl>                                                      STOTNDT.18
#     include <param3.incl>                                                      STOTNDT.19
#     include <varia.incl>                                                       STOTNDT.20
#     include <pmoist.incl>                                                      STOTNDT.21
      COMMON /DEPAR2/ XIG(9),XJG(9),IG0(MJX,MIX),JG0(MJX,MIX)                    STOTNDT.22
      COMMON /PSANET/PSANE,PSANW,PSANN,PSANS                                     STOTNDT.23
      DIMENSION PSANE(2,MIX),PSANW(2,MIX),PSANN(2,MJX),PSANS(2,MJX)              STOTNDT.24
      DIMENSION PSCA(MJX,MIX,9),PSCA2(MJX,MIX,9)                                 STOTNDT.25
      DIMENSION PSCA3(MJX,MIX,9),PSCA4(MJX,MIX,9)                                STOTNDT.26
      DIMENSION SLP(MIX,MJX),SLPN(MIX,MJX),                                      STOTNDT.27
     1          HSC3(MIX,MJX),HSC4(MIX,MJX)                                      STOTNDT.28
      DIMENSION RPSA(MIX,MJX),RHSCR1(MIX,MJX)                                    STOTNDT.29
                                                                                 STOTNDT.30
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         STOTNDT.31
C                                                                                STOTNDT.32
C                 THIS SUBROUTINE FOR TENDENCIES FOR FAST TEND                   STOTNDT.33
      IORD=2                                                                     STOTNDT.34
      JENDC=INT(FLOAT(JNL)/3.+.001)+JWEST                                        STOTNDT.35
      IENDC=INT(FLOAT(INL)/3.+.001)+ISOUTH                                       STOTNDT.36
      ISS1=ISOUTH-IORD                                                           STOTNDT.37
      ISE1=ISOUTH+IORD                                                           STOTNDT.38
      INS1=IENDC-1-IORD                                                          STOTNDT.39
      INE1=IENDC-1+IORD                                                          STOTNDT.40
      JWS1=JWEST-IORD                                                            STOTNDT.41
      JWE1=JWEST+IORD                                                            STOTNDT.42
      JES1=JENDC-1-IORD                                                          STOTNDT.43
      JEE1=JENDC-1+IORD                                                          STOTNDT.44
      JW2=1                                                                      STOTNDT.45
      IS2=1                                                                      STOTNDT.46
      JC2=JLX                                                                    STOTNDT.47
      IC2=ILX                                                                    STOTNDT.48
      IFCT=1                                                                     STOTNDT.49
cmic$ do all autoscope                                                           STOTNDT.50
c$doacross                                                                       STOTNDT.51
c$& local(i,nf,j)                                                                STOTNDT.52
c$omp parallel do default(shared)                                                STOTNDT.53
c$omp&private(i,nf,j)                                                            STOTNDT.54
      DO 30 I=1,MIX                                                              STOTNDT.55
        DO 30 NF=1,9                                                             STOTNDT.56
C     DO 30 I=1,MIX                                                              STOTNDT.57
          DO 30 J=1,MJX                                                          STOTNDT.58
            PSCA(J,I,NF)=0.                                                      STOTNDT.59
            PSCA2(J,I,NF)=0.                                                     STOTNDT.60
   30     CONTINUE                                                               STOTNDT.61
                                                                                 STOTNDT.62
                                                                                 STOTNDT.63
      DT=.5*DT                                                                   STOTNDT.64
      RDT = 1./DT                                                                25JAN00.219
C                                                                                STOTNDT.65
      DO 62 I=1,IYYN-1                                                           STOTNDT.66
        DO 62 J=1,JXXN-1                                                         STOTNDT.67
          HSCR3(I,J)=PSAN(I,J)                                                   STOTNDT.68
   62   CONTINUE                                                                 STOTNDT.69
                                                                                 STOTNDT.70
      DO 63 I=1,2                                                                STOTNDT.71
        DO 63 J=1,JXXN-1                                                         STOTNDT.72
          PSBTN(J,I)=0.                                                          STOTNDT.73
          PNBTN(J,I)=0.                                                          STOTNDT.74
          PSANS(I,J)=PSAN(I,J)                                                   STOTNDT.75
          PSANN(I,J)=PSAN(INL-I,J)                                               STOTNDT.76
   63   CONTINUE                                                                 STOTNDT.77
                                                                                 STOTNDT.78
      DO 64 J=1,2                                                                STOTNDT.79
        DO 64 I=1,IYYN-1                                                         STOTNDT.80
          PWBTN(I,J)=0.                                                          STOTNDT.81
          PEBTN(I,J)=0.                                                          STOTNDT.82
          PSANW(J,I)=PSAN(I,J)                                                   STOTNDT.83
          PSANE(J,I)=PSAN(I,JNL-J)                                               STOTNDT.84
   64   CONTINUE                                                                 STOTNDT.85
                                                                                 STOTNDT.86
      CALL DOTS(PSA,HSCR1,IL,JL,MIX,MJX)                                         STOTNDT.87
      CALL DOTS(HSCR3,HSCR4,INL,JNL,MIX,MJX)                                     STOTNDT.88
      DO J=1,JL-1                                                                STOTNDT.89
        DO I=1,IL-1                                                              STOTNDT.90
          RPSA(I,J)=1./PSA(I,J)                                                  STOTNDT.91
        ENDDO                                                                    STOTNDT.92
      ENDDO                                                                      STOTNDT.93
      DO J=1,JL                                                                  STOTNDT.94
        DO I=1,IL                                                                STOTNDT.95
          RHSCR1(I,J)=1./HSCR1(I,J)                                              STOTNDT.96
        ENDDO                                                                    STOTNDT.97
      ENDDO                                                                      STOTNDT.98
C                                                                                STOTNDT.99
C                                                                                STOTNDT.100
cmic$ do all autoscope                                                           STOTNDT.101
cmic$1 shared (ifct,jwest,isouth,iendc,iord,jendc)                               STOTNDT.102
cmic$2 private (psca,psca2,psca3,psca4)                                          STOTNDT.103
c$doacross                                                                       STOTNDT.104
c$& local(iin,jje,ii,jj,nn,k,j,nf,i,psca,psca2,psca3,psca4)                      STOTNDT.105
c$omp parallel do default(shared)                                                STOTNDT.106
c$omp&private(iin,jje,ii,jj,nn,k,j,nf,i,psca,psca2,psca3,psca4)                  STOTNDT.107
c$omp&shared(jendc,iendc)                                                        STOTNDT.108
      DO 280 K=1,KL                                                              STOTNDT.109
C                                                                                STOTNDT.110
C--- TEMPERATURE TENDENCIES                                                      STOTNDT.111
C                                                                                STOTNDT.112
        DO 80 NF=1,9                                                             STOTNDT.113
          DO 80 I=IS2,IC2                                                        STOTNDT.114
            DO 80 J=JW2,JC2                                                      STOTNDT.115
              PSCA(J,I,NF)=TA(I,J,K)*RPSA(I,J)                                   STOTNDT.116
   80       CONTINUE                                                             STOTNDT.117
                                                                                 STOTNDT.118
                                                                                 STOTNDT.119
        CALL SINTY(PSCA,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                        STOTNDT.120
        CALL SINTY(PSCA,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                      STOTNDT.121
        DO 90 NN=1,2                                                             STOTNDT.122
          DO 90 I=1,INLX                                                         STOTNDT.123
            II=(I-1)/3+ISOUTH                                                    STOTNDT.124
            JJ=MOD(I-1,3)*3+NN                                                   STOTNDT.125
            JJE=MOD(I-1,3)*3+(4-NN)                                              STOTNDT.126
            PSCA(JWEST,II,JJ)=PSCA(JWEST,II,JJ)*PSANW(NN,I)                      STOTNDT.127
            PSCA(JENDC-1,II,JJE)=PSCA(JENDC-1,II,JJE)*PSANE(NN,I)                STOTNDT.128
            TWBTN(I,K,NN)=(PSCA(JWEST,II,JJ)-TWBN(I,K,NN))*RDT                   25JAN00.220
            TEBTN(I,K,NN)=(PSCA(JENDC-1,II,JJE)-TEBN(I,K,NN))*RDT                25JAN00.221
   90     CONTINUE                                                               STOTNDT.131
                                                                                 STOTNDT.132
        DO 100 NF=1,9                                                            STOTNDT.133
          DO 100 I=IS2,IC2                                                       STOTNDT.134
            DO 100 J=JW2,JC2                                                     STOTNDT.135
              PSCA(J,I,NF)=TA(I,J,K)*RPSA(I,J)                                   STOTNDT.136
  100       CONTINUE                                                             STOTNDT.137
                                                                                 STOTNDT.138
                                                                                 STOTNDT.139
        CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                        STOTNDT.140
        CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                       STOTNDT.141
        DO 110 NN=1,2                                                            STOTNDT.142
          DO 110 J=1,JNLX                                                        STOTNDT.143
            JJ=(J-1)/3+JWEST                                                     STOTNDT.144
            II=MOD(J-1,3)+3*(NN-1)+1                                             STOTNDT.145
            IIN=MOD(J-1,3)+3*(2-NN)+4                                            STOTNDT.146
            PSCA(JJ,ISOUTH,II)=PSCA(JJ,ISOUTH,II)*PSANS(NN,J)                    STOTNDT.147
            PSCA(JJ,IENDC-1,IIN)=PSCA(JJ,IENDC-1,IIN)*PSANN(NN,J)                STOTNDT.148
            TSBTN(J,K,NN)=(PSCA(JJ,ISOUTH,II)-TSBN(J,K,NN))* RDT                 25JAN00.222
            TNBTN(J,K,NN)=(PSCA(JJ,IENDC-1,IIN)-TNBN(J,K,NN))* RDT               25JAN00.223
  110     CONTINUE                                                               STOTNDT.151
                                                                                 STOTNDT.152
C                                                                                STOTNDT.153
C---- QV TENDENCIES                                                              STOTNDT.154
C                                                                                STOTNDT.155
        IF(IDRY(INEST).EQ.0)THEN                                                 STOTNDT.156
          DO 120 NF=1,9                                                          STOTNDT.157
            DO 120 I=IS2,IC2                                                     STOTNDT.158
              DO 120 J=JW2,JC2                                                   STOTNDT.159
                PSCA2(J,I,NF)=QVA(I,J,K)*RPSA(I,J)                               STOTNDT.160
  120         CONTINUE                                                           STOTNDT.161
                                                                                 STOTNDT.162
                                                                                 STOTNDT.163
          CALL SINTY(PSCA2,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                     STOTNDT.164
          CALL SINTY(PSCA2,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                   STOTNDT.165
          DO 130 NN=1,2                                                          STOTNDT.166
            DO 130 I=1,INLX                                                      STOTNDT.167
              II=(I-1)/3+ISOUTH                                                  STOTNDT.168
              JJ=MOD(I-1,3)*3+NN                                                 STOTNDT.169
              JJE=MOD(I-1,3)*3+(4-NN)                                            STOTNDT.170
              PSCA2(JWEST,II,JJ)=PSCA2(JWEST,II,JJ)*PSANW(NN,I)                  STOTNDT.171
              PSCA2(JENDC-1,II,JJE)=PSCA2(JENDC-1,II,JJE)*PSANE(NN,I)            STOTNDT.172
              QWBTN(I,K,NN)=(PSCA2(JWEST,II,JJ)-QWBN(I,K,NN))* RDT               25JAN00.224
              QEBTN(I,K,NN)=(PSCA2(JENDC-1,II,JJE)-QEBN(I,K,NN))* RDT            25JAN00.225
  130       CONTINUE                                                             STOTNDT.175
                                                                                 STOTNDT.176
C                                                                                STOTNDT.177
C                                                                                STOTNDT.178
          DO 140 NF=1,9                                                          STOTNDT.179
            DO 140 I=IS2,IC2                                                     STOTNDT.180
              DO 140 J=JW2,JC2                                                   STOTNDT.181
                PSCA2(J,I,NF)=QVA(I,J,K)*RPSA(I,J)                               STOTNDT.182
  140         CONTINUE                                                           STOTNDT.183
                                                                                 STOTNDT.184
                                                                                 STOTNDT.185
          CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                     STOTNDT.186
          CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                    STOTNDT.187
          DO 150 NN=1,2                                                          STOTNDT.188
            DO 150 J=1,JNLX                                                      STOTNDT.189
              JJ=(J-1)/3+JWEST                                                   STOTNDT.190
              II=MOD(J-1,3)+3*(NN-1)+1                                           STOTNDT.191
              IIN=MOD(J-1,3)+3*(2-NN)+4                                          STOTNDT.192
              PSCA2(JJ,ISOUTH,II)=PSCA2(JJ,ISOUTH,II)*PSANS(NN,J)                STOTNDT.193
              PSCA2(JJ,IENDC-1,IIN)=PSCA2(JJ,IENDC-1,IIN)*PSANN(NN,J)            STOTNDT.194
              QSBTN(J,K,NN)=(PSCA2(JJ,ISOUTH,II)-QSBN(J,K,NN))* RDT              25JAN00.226
              QNBTN(J,K,NN)=(PSCA2(JJ,IENDC-1,IIN)-QNBN(J,K,NN))* RDT            25JAN00.227
  150       CONTINUE                                                             STOTNDT.197
                                                                                 STOTNDT.198
        ENDIF                                                                    STOTNDT.199
C                                                                                STOTNDT.200
C--- CLOUD AND RAINWATER TENDENCIES                                              STOTNDT.201
C                                                                                STOTNDT.202
        IF(IMOIST(INEST).EQ.2)THEN                                               STOTNDT.203
          DO 160 NF=1,9                                                          STOTNDT.204
            DO 160 I=IS2,IC2                                                     STOTNDT.205
              DO 160 J=JW2,JC2                                                   STOTNDT.206
                PSCA(J,I,NF)=QCA(I,J,K)*RPSA(I,J)                                STOTNDT.207
                PSCA2(J,I,NF)=QRA(I,J,K)*RPSA(I,J)                               STOTNDT.208
  160         CONTINUE                                                           STOTNDT.209
                                                                                 STOTNDT.210
                                                                                 STOTNDT.211
C                                                                                STOTNDT.212
C--- AT JWEST                                                                    STOTNDT.213
C                                                                                STOTNDT.214
          CALL SINTY(PSCA,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                      STOTNDT.215
          CALL SINTY(PSCA2,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                     STOTNDT.216
C                                                                                STOTNDT.217
C--- AT JENDC-1                                                                  STOTNDT.218
C                                                                                STOTNDT.219
          CALL SINTY(PSCA,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                    STOTNDT.220
          CALL SINTY(PSCA2,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                   STOTNDT.221
          DO 170 NN=1,2                                                          STOTNDT.222
            DO 170 I=1,INLX                                                      STOTNDT.223
              II=(I-1)/3+ISOUTH                                                  STOTNDT.224
              JJ=MOD(I-1,3)*3+NN                                                 STOTNDT.225
              JJE=MOD(I-1,3)*3+(4-NN)                                            STOTNDT.226
              PSCA(JWEST,II,JJ)=PSCA(JWEST,II,JJ)*PSANW(NN,I)                    STOTNDT.227
              PSCA2(JWEST,II,JJ)=PSCA2(JWEST,II,JJ)*PSANW(NN,I)                  STOTNDT.228
              PSCA(JENDC-1,II,JJE)=PSCA(JENDC-1,II,JJE)*PSANE(NN,I)              STOTNDT.229
              PSCA2(JENDC-1,II,JJE)=PSCA2(JENDC-1,II,JJE)*PSANE(NN,I)            STOTNDT.230
              QCWBTN(I,K,NN)=(PSCA(JWEST,II,JJ)-QCWBN(I,K,NN))* RDT              25JAN00.228
              QCEBTN(I,K,NN)=(PSCA(JENDC-1,II,JJE)-QCEBN(I,K,NN))* RDT           25JAN00.229
              QRWBTN(I,K,NN)=(PSCA2(JWEST,II,JJ)-QRWBN(I,K,NN))* RDT             25JAN00.230
              QREBTN(I,K,NN)=(PSCA2(JENDC-1,II,JJE)-QREBN(I,K,NN))* RDT          25JAN00.231
  170       CONTINUE                                                             STOTNDT.235
                                                                                 STOTNDT.236
          DO 180 NF=1,9                                                          STOTNDT.237
            DO 180 I=IS2,IC2                                                     STOTNDT.238
              DO 180 J=JW2,JC2                                                   STOTNDT.239
                PSCA(J,I,NF)=QCA(I,J,K)*RPSA(I,J)                                STOTNDT.240
                PSCA2(J,I,NF)=QRA(I,J,K)*RPSA(I,J)                               STOTNDT.241
  180         CONTINUE                                                           STOTNDT.242
                                                                                 STOTNDT.243
                                                                                 STOTNDT.244
          CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                      STOTNDT.245
          CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                     STOTNDT.246
          CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                     STOTNDT.247
          CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                    STOTNDT.248
          DO 190 NN=1,2                                                          STOTNDT.249
            DO 190 J=1,JNLX                                                      STOTNDT.250
              JJ=(J-1)/3+JWEST                                                   STOTNDT.251
              II=MOD(J-1,3)+3*(NN-1)+1                                           STOTNDT.252
              IIN=MOD(J-1,3)+3*(2-NN)+4                                          STOTNDT.253
              PSCA(JJ,ISOUTH,II)=PSCA(JJ,ISOUTH,II)*PSANS(NN,J)                  STOTNDT.254
              PSCA2(JJ,ISOUTH,II)=PSCA2(JJ,ISOUTH,II)*PSANS(NN,J)                STOTNDT.255
              PSCA(JJ,IENDC-1,IIN)=PSCA(JJ,IENDC-1,IIN)*PSANN(NN,J)              STOTNDT.256
              PSCA2(JJ,IENDC-1,IIN)=PSCA2(JJ,IENDC-1,IIN)*PSANN(NN,J)            STOTNDT.257
              QCSBTN(J,K,NN)=(PSCA(JJ,ISOUTH,II)-QCSBN(J,K,NN))* RDT             25JAN00.232
              QCNBTN(J,K,NN)=(PSCA(JJ,IENDC-1,IIN)-QCNBN(J,K,NN))* RDT           25JAN00.233
              QRSBTN(J,K,NN)=(PSCA2(JJ,ISOUTH,II)-QRSBN(J,K,NN))* RDT            25JAN00.234
              QRNBTN(J,K,NN)=(PSCA2(JJ,IENDC-1,IIN)-QRNBN(J,K,NN))* RDT          25JAN00.235
  190       CONTINUE                                                             STOTNDT.262
                                                                                 STOTNDT.263
C                                                                                STOTNDT.264
C--- ICE AND NICE TENDENCIES                                                     STOTNDT.265
C                                                                                STOTNDT.266
          IF(IICE.EQ.1)THEN                                                      STOTNDT.267
            DO 165 NF=1,9                                                        STOTNDT.268
              DO 165 I=IS2,IC2                                                   STOTNDT.269
                DO 165 J=JW2,JC2                                                 STOTNDT.270
                  PSCA(J,I,NF)=QIA(I,J,K)*RPSA(I,J)                              STOTNDT.271
                  PSCA2(J,I,NF)=QNIA(I,J,K)*RPSA(I,J)                            STOTNDT.272
                  IF(IICEG.EQ.1)THEN                                             STOTNDT.273
                    PSCA3(J,I,NF)=QGA(I,J,K)*RPSA(I,J)                           STOTNDT.274
                    PSCA4(J,I,NF)=QNCA(I,J,K)*RPSA(I,J)                          STOTNDT.275
                  ENDIF                                                          STOTNDT.276
  165           CONTINUE                                                         STOTNDT.277
                                                                                 STOTNDT.278
                                                                                 STOTNDT.279
C                                                                                STOTNDT.280
C--- AT JWEST                                                                    STOTNDT.281
C                                                                                STOTNDT.282
            CALL SINTY(PSCA,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                    STOTNDT.283
            CALL SINTY(PSCA2,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                   STOTNDT.284
            IF(IICEG.EQ.1)THEN                                                   STOTNDT.285
              CALL SINTY(PSCA3,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                 STOTNDT.286
              CALL SINTY(PSCA4,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                 STOTNDT.287
            ENDIF                                                                STOTNDT.288
C                                                                                STOTNDT.289
C--- AT JENDC-1                                                                  STOTNDT.290
C                                                                                STOTNDT.291
            CALL SINTY(PSCA,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                  STOTNDT.292
            CALL SINTY(PSCA2,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                 STOTNDT.293
            IF(IICEG.EQ.1)THEN                                                   STOTNDT.294
              CALL SINTY(PSCA3,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)               STOTNDT.295
              CALL SINTY(PSCA4,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)               STOTNDT.296
            ENDIF                                                                STOTNDT.297
            DO 175 NN=1,2                                                        STOTNDT.298
              DO 175 I=1,INLX                                                    STOTNDT.299
                II=(I-1)/3+ISOUTH                                                STOTNDT.300
                JJ=MOD(I-1,3)*3+NN                                               STOTNDT.301
                JJE=MOD(I-1,3)*3+(4-NN)                                          STOTNDT.302
                PSCA(JWEST,II,JJ)=PSCA(JWEST,II,JJ)*PSANW(NN,I)                  STOTNDT.303
                PSCA2(JWEST,II,JJ)=PSCA2(JWEST,II,JJ)*PSANW(NN,I)                STOTNDT.304
                PSCA(JENDC-1,II,JJE)=PSCA(JENDC-1,II,JJE)*PSANE(NN,I)            STOTNDT.305
                PSCA2(JENDC-1,II,JJE)=PSCA2(JENDC-1,II,JJE)*PSANE(NN,I)          STOTNDT.306
                QIWBTN(I,K,NN)=(PSCA(JWEST,II,JJ)-QIWBN(I,K,NN))* RDT            25JAN00.236
                QIEBTN(I,K,NN)=(PSCA(JENDC-1,II,JJE)-QIEBN(I,K,NN))* RDT         25JAN00.237
                QNIWBTN(I,K,NN)=(PSCA2(JWEST,II,JJ)-QNIWBN(I,K,NN))* RDT         25JAN00.238
                QNIEBTN(I,K,NN)=(PSCA2(JENDC-1,II,JJE)-QNIEBN(I,K,NN))*          25JAN00.239
     +                         RDT                                               25JAN00.240
                IF(IICEG.EQ.1)THEN                                               STOTNDT.312
                  PSCA3(JWEST,II,JJ)=PSCA3(JWEST,II,JJ)*PSANW(NN,I)              STOTNDT.313
                  PSCA4(JWEST,II,JJ)=PSCA4(JWEST,II,JJ)*PSANW(NN,I)              STOTNDT.314
                  PSCA3(JENDC-1,II,JJE)=PSCA3(JENDC-1,II,JJE)*                   STOTNDT.315
     +                                  PSANE(NN,I)                              STOTNDT.316
                  PSCA4(JENDC-1,II,JJE)=PSCA4(JENDC-1,II,JJE)*                   STOTNDT.317
     +                                  PSANE(NN,I)                              STOTNDT.318
                  QGWBTN(I,K,NN)=(PSCA3(JWEST,II,JJ)-QGWBN(I,K,NN))* RDT         25JAN00.241
                  QGEBTN(I,K,NN)=(PSCA3(JENDC-1,II,JJE)-QGEBN(I,K,NN))*          25JAN00.242
     +                          RDT                                              25JAN00.243
                  QNCWBTN(I,K,NN)=(PSCA4(JWEST,II,JJ)-QNCWBN(I,K,NN))            07NOV00.986
     +                            * RDT                                          07NOV00.987
                  QNCEBTN(I,K,NN)=(PSCA4(JENDC-1,II,JJE)-QNCEBN(I,K,NN))         STOTNDT.323
     +                            * RDT                                          25JAN00.245
                ENDIF                                                            STOTNDT.325
  175         CONTINUE                                                           STOTNDT.326
                                                                                 STOTNDT.327
            DO 185 NF=1,9                                                        STOTNDT.328
              DO 185 I=IS2,IC2                                                   STOTNDT.329
                DO 185 J=JW2,JC2                                                 STOTNDT.330
                  PSCA(J,I,NF)=QIA(I,J,K)*RPSA(I,J)                              STOTNDT.331
                  PSCA2(J,I,NF)=QNIA(I,J,K)*RPSA(I,J)                            STOTNDT.332
                  IF(IICEG.EQ.1)THEN                                             STOTNDT.333
                    PSCA3(J,I,NF)=QGA(I,J,K)*RPSA(I,J)                           STOTNDT.334
                    PSCA4(J,I,NF)=QNCA(I,J,K)*RPSA(I,J)                          STOTNDT.335
                  ENDIF                                                          STOTNDT.336
  185           CONTINUE                                                         STOTNDT.337
                                                                                 STOTNDT.338
                                                                                 STOTNDT.339
            CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                    STOTNDT.340
            CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                   STOTNDT.341
            CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                   STOTNDT.342
            CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                  STOTNDT.343
            IF(IICEG.EQ.1)THEN                                                   STOTNDT.344
              CALL SINTX(PSCA3,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                 STOTNDT.345
              CALL SINTX(PSCA3,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                STOTNDT.346
              CALL SINTX(PSCA4,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                 STOTNDT.347
              CALL SINTX(PSCA4,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                STOTNDT.348
            ENDIF                                                                STOTNDT.349
            DO 195 NN=1,2                                                        STOTNDT.350
              DO 195 J=1,JNLX                                                    STOTNDT.351
                JJ=(J-1)/3+JWEST                                                 STOTNDT.352
                II=MOD(J-1,3)+3*(NN-1)+1                                         STOTNDT.353
                IIN=MOD(J-1,3)+3*(2-NN)+4                                        STOTNDT.354
                PSCA(JJ,ISOUTH,II)=PSCA(JJ,ISOUTH,II)*PSANS(NN,J)                STOTNDT.355
                PSCA2(JJ,ISOUTH,II)=PSCA2(JJ,ISOUTH,II)*PSANS(NN,J)              STOTNDT.356
                PSCA(JJ,IENDC-1,IIN)=PSCA(JJ,IENDC-1,IIN)*PSANN(NN,J)            STOTNDT.357
                PSCA2(JJ,IENDC-1,IIN)=PSCA2(JJ,IENDC-1,IIN)*PSANN(NN,J)          STOTNDT.358
                QISBTN(J,K,NN)=(PSCA(JJ,ISOUTH,II)-QISBN(J,K,NN))* RDT           25JAN00.246
                QINBTN(J,K,NN)=(PSCA(JJ,IENDC-1,IIN)-QINBN(J,K,NN))*RDT          25JAN00.247
                QNISBTN(J,K,NN)=(PSCA2(JJ,ISOUTH,II)-QNISBN(J,K,NN))*RDT         25JAN00.248
                QNINBTN(J,K,NN)=(PSCA2(JJ,IENDC-1,IIN)-QNINBN(J,K,NN))*          25JAN00.249
     +                         RDT                                               25JAN00.250
                IF(IICEG.EQ.1)THEN                                               STOTNDT.364
                  PSCA3(JJ,ISOUTH,II)=PSCA3(JJ,ISOUTH,II)*PSANS(NN,J)            STOTNDT.365
                  PSCA4(JJ,ISOUTH,II)=PSCA4(JJ,ISOUTH,II)*PSANS(NN,J)            STOTNDT.366
                  PSCA3(JJ,IENDC-1,IIN)=PSCA3(JJ,IENDC-1,IIN)*                   STOTNDT.367
     +                                  PSANN(NN,J)                              STOTNDT.368
                  PSCA4(JJ,IENDC-1,IIN)=PSCA4(JJ,IENDC-1,IIN)*                   STOTNDT.369
     +                                  PSANN(NN,J)                              STOTNDT.370
                  QGSBTN(J,K,NN)=(PSCA3(JJ,ISOUTH,II)-QGSBN(J,K,NN))*RDT         07NOV00.988
                  QGNBTN(J,K,NN)=(PSCA3(JJ,IENDC-1,IIN)-QGNBN(J,K,NN))*          25JAN00.252
     +                          RDT                                              25JAN00.253
                  QNCSBTN(J,K,NN)=(PSCA4(JJ,ISOUTH,II)-QNCSBN(J,K,NN))*          25JAN00.254
     +                           RDT                                             25JAN00.255
                  QNCNBTN(J,K,NN)=(PSCA4(JJ,IENDC-1,IIN)-QNCNBN(J,K,NN))         STOTNDT.376
     +                            * RDT                                          25JAN00.256
                ENDIF                                                            STOTNDT.378
  195         CONTINUE                                                           STOTNDT.379
                                                                                 STOTNDT.380
          ENDIF                                                                  STOTNDT.381
        ENDIF                                                                    STOTNDT.382
C                                                                                STOTNDT.383
C                                                                                STOTNDT.384
C                                                                                STOTNDT.385
        DO 200 NF=1,9                                                            STOTNDT.386
          DO 200 I=1,IL                                                          STOTNDT.387
            DO 200 J=1,JL                                                        STOTNDT.388
              PSCA(J,I,NF)=UA(I,J,K)*RHSCR1(I,J)                                 STOTNDT.389
              PSCA2(J,I,NF)=VA(I,J,K)*RHSCR1(I,J)                                STOTNDT.390
  200       CONTINUE                                                             STOTNDT.391
                                                                                 STOTNDT.392
                                                                                 STOTNDT.393
C                                                                                STOTNDT.394
C--- AT JWEST                                                                    STOTNDT.395
C                                                                                STOTNDT.396
        CALL SINTY(PSCA,MJX,MIX,JWEST,ISOUTH,IENDC,1,0)                          STOTNDT.397
        CALL SINTY(PSCA2,MJX,MIX,JWEST,ISOUTH,IENDC,1,0)                         STOTNDT.398
C                                                                                STOTNDT.399
C--- AT JENDC-1                                                                  STOTNDT.400
C                                                                                STOTNDT.401
        CALL SINTY(PSCA,MJX,MIX,JENDC,ISOUTH,IENDC,2,0)                          STOTNDT.402
        CALL SINTY(PSCA2,MJX,MIX,JENDC,ISOUTH,IENDC,2,0)                         STOTNDT.403
        DO 210 NN=1,2                                                            STOTNDT.404
          DO 210 I=1,INL                                                         STOTNDT.405
            II=I/3+ISOUTH                                                        STOTNDT.406
            JJ=3*MOD(I,3)+(NN+1)                                                 STOTNDT.407
            JJE=3*MOD(I,3)+(3-NN)                                                STOTNDT.408
            UWBTN(I,K,NN)=(PSCA(JWEST,II,JJ)*HSCR4(I,NN)-UWBN(I,K,NN))*          25JAN00.257
     +                   RDT                                                     25JAN00.258
            UEBTN(I,K,NN)=(PSCA(JENDC,II,JJE)*HSCR4(I,JNL-NN+1)-                 STOTNDT.411
     +                    UEBN(I,K,NN))* RDT                                     25JAN00.259
            VWBTN(I,K,NN)=(PSCA2(JWEST,II,JJ)*HSCR4(I,NN)-VWBN(I,K,NN))*         25JAN00.260
     +                   RDT                                                     25JAN00.261
            VEBTN(I,K,NN)=(PSCA2(JENDC,II,JJE)*HSCR4(I,JNL-NN+1)-                STOTNDT.415
     +                    VEBN(I,K,NN))* RDT                                     25JAN00.262
  210     CONTINUE                                                               STOTNDT.417
                                                                                 STOTNDT.418
        DO 220 NF=1,9                                                            STOTNDT.419
          DO 220 I=1,IL                                                          STOTNDT.420
            DO 220 J=1,JL                                                        STOTNDT.421
              PSCA(J,I,NF)=UA(I,J,K)*RHSCR1(I,J)                                 STOTNDT.422
              PSCA2(J,I,NF)=VA(I,J,K)*RHSCR1(I,J)                                STOTNDT.423
  220       CONTINUE                                                             STOTNDT.424
                                                                                 STOTNDT.425
                                                                                 STOTNDT.426
        CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC,ISOUTH,1,0)                          STOTNDT.427
        CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC,IENDC,2,0)                           STOTNDT.428
        CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC,ISOUTH,1,0)                         STOTNDT.429
        CALL SINTX(PSCA2,MJX,MIX,JWEST,JENDC,IENDC,2,0)                          STOTNDT.430
        DO 230 NN=1,2                                                            STOTNDT.431
          DO 230 J=1,JNL                                                         STOTNDT.432
            JJ=J/3+JWEST                                                         STOTNDT.433
            II=MOD(J,3)+3*(NN+1)-2                                               STOTNDT.434
            IIN=MOD(J,3)+3*(3-NN)-2                                              STOTNDT.435
            USBTN(J,K,NN)=(PSCA(JJ,ISOUTH,II)*HSCR4(NN,J)-USBN(J,K,NN))*         25JAN00.263
     +                   RDT                                                     25JAN00.264
            UNBTN(J,K,NN)=(PSCA(JJ,IENDC,IIN)*HSCR4(INL-NN+1,J)-                 STOTNDT.438
     +                    UNBN(J,K,NN))* RDT                                     25JAN00.265
            VSBTN(J,K,NN)=(PSCA2(JJ,ISOUTH,II)*HSCR4(NN,J)-VSBN(J,K,NN))         STOTNDT.440
     +                    * RDT                                                  25JAN00.266
            VNBTN(J,K,NN)=(PSCA2(JJ,IENDC,IIN)*HSCR4(INL-NN+1,J)-                STOTNDT.442
     +                    VNBN(J,K,NN))* RDT                                     25JAN00.267
  230     CONTINUE                                                               STOTNDT.444
                                                                                 STOTNDT.445
C                                                                                STOTNDT.446
C--- NON-HYDROSTATIC TENDENCIES                                                  STOTNDT.447
C                                                                                STOTNDT.448
        DO 240 NF=1,9                                                            STOTNDT.449
          DO 240 I=IS2,IC2                                                       STOTNDT.450
            DO 240 J=JW2,JC2                                                     STOTNDT.451
              PSCA(J,I,NF)=PPA(I,J,K)*RPSA(I,J)                                  STOTNDT.452
  240       CONTINUE                                                             STOTNDT.453
                                                                                 STOTNDT.454
                                                                                 STOTNDT.455
        CALL SINTY(PSCA,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                        STOTNDT.456
        CALL SINTY(PSCA,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                      STOTNDT.457
        DO 250 NN=1,2                                                            STOTNDT.458
          DO 250 I=1,INLX                                                        STOTNDT.459
            II=(I-1)/3+ISOUTH                                                    STOTNDT.460
            JJ=MOD(I-1,3)*3+NN                                                   STOTNDT.461
            JJE=MOD(I-1,3)*3+(4-NN)                                              STOTNDT.462
            PSCA(JWEST,II,JJ)=PSCA(JWEST,II,JJ)*PSANW(NN,I)                      STOTNDT.463
            PSCA(JENDC-1,II,JJE)=PSCA(JENDC-1,II,JJE)*PSANE(NN,I)                STOTNDT.464
            PPWBTN(I,K,NN)=(PSCA(JWEST,II,JJ)-PPWBN(I,K,NN))* RDT                25JAN00.268
            PPEBTN(I,K,NN)=(PSCA(JENDC-1,II,JJE)-PPEBN(I,K,NN))* RDT             25JAN00.269
  250     CONTINUE                                                               STOTNDT.467
                                                                                 STOTNDT.468
        DO 260 NF=1,9                                                            STOTNDT.469
          DO 260 I=IS2,IC2                                                       STOTNDT.470
            DO 260 J=JW2,JC2                                                     STOTNDT.471
              PSCA(J,I,NF)=PPA(I,J,K)*RPSA(I,J)                                  STOTNDT.472
  260       CONTINUE                                                             STOTNDT.473
                                                                                 STOTNDT.474
                                                                                 STOTNDT.475
        CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                        STOTNDT.476
        CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                       STOTNDT.477
        DO 270 NN=1,2                                                            STOTNDT.478
          DO 270 J=1,JNLX                                                        STOTNDT.479
            JJ=(J-1)/3+JWEST                                                     STOTNDT.480
            II=MOD(J-1,3)+3*(NN-1)+1                                             STOTNDT.481
            IIN=MOD(J-1,3)+3*(2-NN)+4                                            STOTNDT.482
            PSCA(JJ,ISOUTH,II)=PSCA(JJ,ISOUTH,II)*PSANS(NN,J)                    STOTNDT.483
            PSCA(JJ,IENDC-1,IIN)=PSCA(JJ,IENDC-1,IIN)*PSANN(NN,J)                STOTNDT.484
            PPSBTN(J,K,NN)=(PSCA(JJ,ISOUTH,II)-PPSBN(J,K,NN))* RDT               25JAN00.270
            PPNBTN(J,K,NN)=(PSCA(JJ,IENDC-1,IIN)-PPNBN(J,K,NN))* RDT             25JAN00.271
  270     CONTINUE                                                               STOTNDT.487
                                                                                 STOTNDT.488
  280 CONTINUE                                                                   STOTNDT.489
C                                                                                STOTNDT.490
C                                                                                STOTNDT.491
cmic$ do all autoscope                                                           STOTNDT.492
cmic$1 shared (ifct,jwest,isouth,iendc,iord,jendc)                               STOTNDT.493
cmic$2 private (psca,psca2,psca3,psca4)                                          STOTNDT.494
c$doacross                                                                       STOTNDT.495
c$& local(iin,jje,ii,jj,nn,k,j,nf,i,psca,psca2)                                  STOTNDT.496
c$omp parallel do default(shared)                                                STOTNDT.497
c$omp&private(iin,jje,ii,jj,nn,k,j,nf,i,psca,psca2)                              STOTNDT.498
      DO 330 K=1,KL+1                                                            STOTNDT.499
C                                                                                STOTNDT.500
C--- CONTINUE NON-HYDROSTATIC TENDENCIES (W)                                     STOTNDT.501
C                                                                                STOTNDT.502
        DO 290 NF=1,9                                                            STOTNDT.503
          DO 290 I=IS2,IC2                                                       STOTNDT.504
            DO 290 J=JW2,JC2                                                     STOTNDT.505
              PSCA(J,I,NF)=WA(I,J,K)*RPSA(I,J)                                   STOTNDT.506
  290       CONTINUE                                                             STOTNDT.507
                                                                                 STOTNDT.508
                                                                                 STOTNDT.509
        CALL SINTY(PSCA,MJX,MIX,JWEST,ISOUTH,IENDC-1,1,1)                        STOTNDT.510
        CALL SINTY(PSCA,MJX,MIX,JENDC-1,ISOUTH,IENDC-1,2,1)                      STOTNDT.511
        DO 300 NN=1,2                                                            STOTNDT.512
          DO 300 I=1,INLX                                                        STOTNDT.513
            II=(I-1)/3+ISOUTH                                                    STOTNDT.514
            JJ=MOD(I-1,3)*3+NN                                                   STOTNDT.515
            JJE=MOD(I-1,3)*3+(4-NN)                                              STOTNDT.516
            PSCA(JWEST,II,JJ)=PSCA(JWEST,II,JJ)*PSANW(NN,I)                      STOTNDT.517
            PSCA(JENDC-1,II,JJE)=PSCA(JENDC-1,II,JJE)*PSANE(NN,I)                STOTNDT.518
            WWBTN(I,K,NN)=(PSCA(JWEST,II,JJ)-WWBN(I,K,NN))* RDT                  25JAN00.272
            WEBTN(I,K,NN)=(PSCA(JENDC-1,II,JJE)-WEBN(I,K,NN))* RDT               25JAN00.273
  300     CONTINUE                                                               STOTNDT.521
                                                                                 STOTNDT.522
        DO 310 NF=1,9                                                            STOTNDT.523
          DO 310 I=IS2,IC2                                                       STOTNDT.524
            DO 310 J=JW2,JC2                                                     STOTNDT.525
              PSCA(J,I,NF)=WA(I,J,K)*RPSA(I,J)                                   STOTNDT.526
  310       CONTINUE                                                             STOTNDT.527
                                                                                 STOTNDT.528
                                                                                 STOTNDT.529
        CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,ISOUTH,1,1)                        STOTNDT.530
        CALL SINTX(PSCA,MJX,MIX,JWEST,JENDC-1,IENDC-1,2,1)                       STOTNDT.531
        DO 320 NN=1,2                                                            STOTNDT.532
          DO 320 J=1,JNLX                                                        STOTNDT.533
            JJ=(J-1)/3+JWEST                                                     STOTNDT.534
            II=MOD(J-1,3)+3*(NN-1)+1                                             STOTNDT.535
            IIN=MOD(J-1,3)+3*(2-NN)+4                                            STOTNDT.536
            PSCA(JJ,ISOUTH,II)=PSCA(JJ,ISOUTH,II)*PSANS(NN,J)                    STOTNDT.537
            PSCA(JJ,IENDC-1,IIN)=PSCA(JJ,IENDC-1,IIN)*PSANN(NN,J)                STOTNDT.538
            WSBTN(J,K,NN)=(PSCA(JJ,ISOUTH,II)-WSBN(J,K,NN))* RDT                 25JAN00.274
            WNBTN(J,K,NN)=(PSCA(JJ,IENDC-1,IIN)-WNBN(J,K,NN))* RDT               25JAN00.275
  320     CONTINUE                                                               STOTNDT.541
                                                                                 STOTNDT.542
  330 CONTINUE                                                                   STOTNDT.543
      DT=2.*DT                                                                   STOTNDT.544
12333 FORMAT (1X,4E12.3)                                                         STOTNDT.545
      RETURN                                                                     STOTNDT.546
      END                                                                        STOTNDT.547
                                                                                 STOTNDT.548
