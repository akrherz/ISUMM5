         SUBROUTINE BDYOVL1(NESCOU)                                              BDYOVL1.1
C                                                                                BDYOVL1.2
#     include <parame.incl>                                                      BDYOVL1.3
#     include <addr0.incl>                                                       BDYOVL1.4
#     include <pointbc.incl>                                                     BDYOVL1.5
#     include <pointbcn.incl>                                                    BDYOVL1.6
#     include <point3d.incl>                                                     BDYOVL1.7
#     include <point2d.incl>                                                     BDYOVL1.8
#     include <point3dn.incl>                                                    BDYOVL1.9
#     include <point2dn.incl>                                                    BDYOVL1.10
#     include <nonhydb.incl>                                                     BDYOVL1.11
#     include <nnnhydb.incl>                                                     BDYOVL1.12
#     include <nonhyd.incl>                                                      BDYOVL1.13
#     include <nnnhyd.incl>                                                      BDYOVL1.14
#     include <param2.incl>                                                      BDYOVL1.15
#     include <param3.incl>                                                      BDYOVL1.16
#     include <nestl.incl>                                                       BDYOVL1.17
#     include <various.incl>                                                     BDYOVL1.18
#     include <variousn.incl>                                                    BDYOVL1.19
#     include <varia.incl>                                                       BDYOVL1.20
                                                                                 BDYOVL1.21
      DIMENSION HSCR5(MIX,MJX),HSCR6(MIX,MJX)                                    BDYOVL1.22
      IN=NESCOU                                                                  BDYOVL1.23
C                                                                                BDYOVL1.24
C--- THE "COARSE" DOMAIN VARAIBLES HERE ARE USED FOR THE NEST TO                 BDYOVL1.25
C--- BE SWAPPED FROM                                                             BDYOVL1.26
C                                                                                BDYOVL1.27
      LZ=NESCOU                                                                  BDYOVL1.28
      IF(ISTAR(LZ).EQ.-999)RETURN                                                BDYOVL1.29
      ICUTE=2                                                                    BDYOVL1.30
      JCUTN=2                                                                    BDYOVL1.31
      ICUTN=2                                                                    BDYOVL1.32
      ICUTW=3                                                                    BDYOVL1.33
      ICUTS=3                                                                    BDYOVL1.34
      JCUTS=3                                                                    BDYOVL1.35
      IWIN0=1                                                                    BDYOVL1.36
      ISEC1=0                                                                    BDYOVL1.37
      ISEC2=0                                                                    BDYOVL1.38
      ISEC3=0                                                                    BDYOVL1.39
      IOVES=4                                                                    BDYOVL1.40
      IOVEN=-4                                                                   BDYOVL1.41
      JOVEW=4                                                                    BDYOVL1.42
      JOVEE=-4                                                                   BDYOVL1.43
C                                                                                BDYOVL1.44
C--- FIRST THE NEW VALUE AT T+DT IS OVERWRITTEN. THEN THE BOUNDARY               BDYOVL1.45
C--- TENDENCY IS RECALCULATED, BEFORE OVERWRITING THE VALUE AT T.                BDYOVL1.46
C--- OVERWRITING THE TENDENCY IS NOT REALLY NEEDED.                              BDYOVL1.47
C                                                                                BDYOVL1.48
C---IF JSTAR(LZ).EQ.1, GRID IS TO THE EAST, THE WEST BOUNDARY NEEDS              BDYOVL1.49
C--- TO BE PARTIALLY OVERWRITTEN WITH OTHER NEST                                 BDYOVL1.50
C                                                                                BDYOVL1.51
C------- DO 3-D VARIABLES !!                                                     BDYOVL1.52
C                                                                                BDYOVL1.53
cmic$ do all autoscope                                                           BDYOVL1.54
cmic$1 shared (jsto,isto,istar,ien,mkx,il,jl,kl,jnlx,dtn,kxp1,jnl,               BDYOVL1.55
cmic$1 jstar,jovee,icute,ioves,icuts,inlx,inl,ioven,icutn,jen,                   BDYOVL1.56
cmic$1 ta,tan,tb,tbn,twbn,twbtn,tebn,tebtn,tnbn,tnbtn,tsbn,tsbtn,                BDYOVL1.57
cmic$1 wa,wan,wb,wbn,wwbn,wwbtn,webn,webtn,wnbn,wnbtn,wsbn,wsbtn,                BDYOVL1.58
cmic$1 ppa,ppan,ppb,ppbn,ppwbn,ppwbtn,ppebn,                                     BDYOVL1.59
cmic$1 ppebtn,ppnbn,ppnbtn,ppsbn,ppsbtn,                                         BDYOVL1.60
cmic$1 qca,qcan,qcb,qcbn,qcwbn,qcwbtn,qcebn,                                     BDYOVL1.61
cmic$1 qcebtn,qcnbn,qcnbtn,qcsbn,qcsbtn,                                         BDYOVL1.62
cmic$1 qnia,qnian,qnib,qnibn,qniwbn,qniwbtn,qniebn,                              BDYOVL1.63
cmic$1 qniebtn,qninbn,qninbtn,qnisbn,qnisbtn,                                    BDYOVL1.64
cmic$1 qia,qian,qib,qibn,qiwbn,qiwbtn,qiebn,                                     BDYOVL1.65
cmic$1 qga,qgan,qgb,qgbn,qgwbn,qgwbtn,qgebn,                                     BDYOVL1.66
cmic$1 qgebtn,qgnbn,qgnbtn,qgsbn,qgsbtn,                                         BDYOVL1.67
cmic$1 qnca,qncan,qncb,qncbn,qncwbn,qncwbtn,qncebn,                              BDYOVL1.68
cmic$1 qncebtn,qncnbn,qncnbtn,qncsbn,qncsbtn,                                    BDYOVL1.69
cmic$1 qiebtn,qinbn,qinbtn,qisbn,qisbtn,                                         BDYOVL1.70
cmic$1 qra,qran,qrb,qrbn,qrwbn,qrwbtn,qrebn,                                     BDYOVL1.71
cmic$1 qrebtn,qrnbn,qrnbtn,qrsbn,qrsbtn,                                         BDYOVL1.72
cmic$1 qva,qvan,qvb,qvbn,qwbn,qwbtn,qebn,                                        BDYOVL1.73
cmic$1 qebtn,qnbn,qnbtn,qsbn,qsbtn,                                              BDYOVL1.74
cmic$1 nspgd,nspgx,dtn,icutw,jovew,ua,va,uan,van,ub,vb,ubn,vbn,                  BDYOVL1.75
cmic$1 uwbn,vwbn,uwbtn,uebtn,vwbtn,vebtn,uebn,vebn,unbn,vnbn,                    BDYOVL1.76
cmic$1 unj1,unjnl,unj2,unjnlx,vnj1,vnjnl,vnj2,vnjnlx,uni1,uninl,                 BDYOVL1.77
cmic$1 uni2,uninlx,vni1,vninl,vni2,vninlx,                                       BDYOVL1.78
cmic$1 unbtn,vnbtn,usbn,vsbn,usbtn,vsbtn)                                        BDYOVL1.79
cmic$1 private (k)                                                               BDYOVL1.80
c$omp parallel do default(shared)                                                BDYOVL1.81
c$omp&private(k)                                                                 BDYOVL1.82
      DO 100 K=1,KL                                                              BDYOVL1.83
        IF(JSTAR(LZ).EQ.1)THEN                                                   BDYOVL1.84
C                                                                                BDYOVL1.85
C--- TEMPERATURE                                                                 BDYOVL1.86
C                                                                                BDYOVL1.87
          CALL EXCHANJ(JSTO(LZ),1,JOVEW,ISTO(LZ)-1,ISTAR(LZ),IEN(LZ)-1,K         BDYOVL1.88
     +         ,TA,TAN,TB,TBN,TWBN,TWBTN,MKX,NSPGX,DTN,ICUTW)                    BDYOVL1.89
C                                                                                BDYOVL1.90
C--- U AND V                                                                     BDYOVL1.91
C                                                                                BDYOVL1.92
          CALL EXCHANJ(JSTO(LZ),1,JOVEW,ISTO(LZ)-1,ISTAR(LZ),IEN(LZ),K,          BDYOVL1.93
     +         UA,UAN,UB,UBN,UWBN,UWBTN,MKX,NSPGD,DTN,ICUTW)                     BDYOVL1.94
          CALL EXCHANJ(JSTO(LZ),1,JOVEW,ISTO(LZ)-1,ISTAR(LZ),IEN(LZ),K,          BDYOVL1.95
     +         VA,VAN,VB,VBN,VWBN,VWBTN,MKX,NSPGD,DTN,ICUTW)                     BDYOVL1.96
C                                                                                BDYOVL1.97
C--- NON-HYDROSTATIC VARIABLES                                                   BDYOVL1.98
C                                                                                BDYOVL1.99
          CALL EXCHANJ(JSTO(LZ),1,JOVEW,ISTO(LZ)-1,ISTAR(LZ),IEN(LZ)-1,K         BDYOVL1.100
     +         ,WA,WAN,WB,WBN,WWBN,WWBTN,KXP1,NSPGX,DTN,ICUTW)                   BDYOVL1.101
          CALL EXCHANJ(JSTO(LZ),1,JOVEW,ISTO(LZ)-1,ISTAR(LZ),IEN(LZ)-1,K         BDYOVL1.102
     +         ,PPA,PPAN,PPB,PPBN,PPWBN,PPWBTN,MKX,NSPGX,DTN,ICUTW)              BDYOVL1.103
C                                                                                BDYOVL1.104
C--- DECOUPLED BOUNDARY SLICES                                                   BDYOVL1.105
C                                                                                BDYOVL1.106
          DO 206 I=ISTAR(LZ),IEN(LZ)-ICUTW+IWIN0+ISEC3                           BDYOVL1.107
            UNJ1(I,K)=UA(I+ISTO(LZ)-1,JSTO(LZ),K)/                               BDYOVL1.108
     +                HSCR1(I+ISTO(LZ)-1,JSTO(LZ))                               BDYOVL1.109
            VNJ1(I,K)=VA(I+ISTO(LZ)-1,JSTO(LZ),K)/                               BDYOVL1.110
     +                HSCR1(I+ISTO(LZ)-1,JSTO(LZ))                               BDYOVL1.111
            UNJ2(I,K)=UA(I+ISTO(LZ)-1,JSTO(LZ)+1,K)/                             BDYOVL1.112
     +                HSCR1(I+ISTO(LZ)-1,JSTO(LZ)+1)                             BDYOVL1.113
            VNJ2(I,K)=VA(I+ISTO(LZ)-1,JSTO(LZ)+1,K)/                             BDYOVL1.114
     +                HSCR1(I+ISTO(LZ)-1,JSTO(LZ)+1)                             BDYOVL1.115
  206     CONTINUE                                                               BDYOVL1.116
C                                                                                BDYOVL1.117
C--- MOISTURE                                                                    BDYOVL1.118
C                                                                                BDYOVL1.119
          IF(IDRY(IN).EQ.0)THEN                                                  BDYOVL1.120
            CALL EXCHANJ(JSTO(LZ),1,JOVEW,ISTO(LZ)-1,ISTAR(LZ),IEN(LZ)-1         BDYOVL1.121
     +           ,K,QVA,QVAN,QVB,QVBN,QWBN,QWBTN,MKX,NSPGX,DTN,ICUTW)            BDYOVL1.122
C                                                                                BDYOVL1.123
C--- CLOUD AND RAINWATER                                                         BDYOVL1.124
C                                                                                BDYOVL1.125
            IF(IMOIST(NESCOU).EQ.2)THEN                                          BDYOVL1.126
              IF(IICE.EQ.0)THEN                                                  BDYOVL1.127
                CALL EXCHANJ(JSTO(LZ),1,JOVEW,ISTO(LZ)-1,ISTAR(LZ),              BDYOVL1.128
     +               IEN(LZ)-1,K,QCA,QCAN,QCB,QCBN,QCWBN,QCWBTN,MKX,             BDYOVL1.129
     +               NSPGX,DTN,ICUTW)                                            BDYOVL1.130
                CALL EXCHANJ(JSTO(LZ),1,JOVEW,ISTO(LZ)-1,ISTAR(LZ),              BDYOVL1.131
     +               IEN(LZ)-1,K,QRA,QRAN,QRB,QRBN,QRWBN,QRWBTN,MKX,             BDYOVL1.132
     +               NSPGX,DTN,ICUTW)                                            BDYOVL1.133
              ELSEIF(IICE.EQ.1)THEN                                              BDYOVL1.134
                CALL EXCHANJ(JSTO(LZ),1,JOVEW,ISTO(LZ)-1,ISTAR(LZ),              BDYOVL1.135
     +               IEN(LZ)-1,K,QIA,QIAN,QIB,QIBN,QIWBN,QIWBTN,MKX,             BDYOVL1.136
     +               NSPGX,DTN,ICUTW)                                            BDYOVL1.137
                CALL EXCHANJ(JSTO(LZ),1,JOVEW,ISTO(LZ)-1,ISTAR(LZ),              BDYOVL1.138
     +               IEN(LZ)-1,K,QNIA,QNIAN,QNIB,QNIBN,QNIWBN,QNIWBTN,           BDYOVL1.139
     +               MKX,NSPGX,DTN,ICUTW)                                        BDYOVL1.140
                CALL EXCHANJ(JSTO(LZ),1,JOVEW,ISTO(LZ)-1,ISTAR(LZ),              BDYOVL1.141
     +               IEN(LZ)-1,K,QCA,QCAN,QCB,QCBN,QCWBN,QCWBTN,MKX,             BDYOVL1.142
     +               NSPGX,DTN,ICUTW)                                            BDYOVL1.143
                IF(IICEG.EQ.1)THEN                                               BDYOVL1.144
                  CALL EXCHANJ(JSTO(LZ),1,JOVEW,ISTO(LZ)-1,ISTAR(LZ),            BDYOVL1.145
     +                 IEN(LZ)-1,K,QGA,QGAN,QGB,QGBN,QGWBN,QGWBTN,MKX,           BDYOVL1.146
     +                 NSPGX,DTN,ICUTW)                                          BDYOVL1.147
                  CALL EXCHANJ(JSTO(LZ),1,JOVEW,ISTO(LZ)-1,ISTAR(LZ),            BDYOVL1.148
     +                 IEN(LZ)-1,K,QNCA,QNCAN,QNCB,QNCBN,QNCWBN,QNCWBTN,         BDYOVL1.149
     +                 MKX,NSPGX,DTN,ICUTW)                                      BDYOVL1.150
                ENDIF                                                            BDYOVL1.151
              ENDIF                                                              BDYOVL1.152
            ENDIF                                                                BDYOVL1.153
          ENDIF                                                                  BDYOVL1.154
  200     CONTINUE                                                               BDYOVL1.155
C                                                                                BDYOVL1.156
C--- IF GRID IS FURTHER WEST THAN OTHE GRID AT SAME LEVEL, ADJUST                BDYOVL1.157
C--- AT EAST BOUNDARY                                                            BDYOVL1.158
C                                                                                BDYOVL1.159
        ELSEIF(JSTAR(LZ).NE.1)THEN                                               BDYOVL1.160
C                                                                                BDYOVL1.161
C--- TEMPERATURE                                                                 BDYOVL1.162
C                                                                                BDYOVL1.163
          CALL EXCHANJ(JL-JSTAR(LZ),JNLX,JOVEE,1-ISTAR(LZ),ISTAR(LZ),            BDYOVL1.164
     +         IEN(LZ)-1,K,TA,TAN,TB,TBN,TEBN,TEBTN,MKX,NSPGX,DTN,ICUTE)         BDYOVL1.165
C                                                                                BDYOVL1.166
C--- WINDS                                                                       BDYOVL1.167
C                                                                                BDYOVL1.168
          CALL EXCHANJ(JL-JSTAR(LZ)+1,JNL,JOVEE,1-ISTAR(LZ),ISTAR(LZ),           BDYOVL1.169
     +         IEN(LZ),K,UA,UAN,UB,UBN,UEBN,UEBTN,MKX,NSPGD,DTN,ICUTE)           BDYOVL1.170
          CALL EXCHANJ(JL-JSTAR(LZ)+1,JNL,JOVEE,1-ISTAR(LZ),ISTAR(LZ),           BDYOVL1.171
     +         IEN(LZ),K,VA,VAN,VB,VBN,VEBN,VEBTN,MKX,NSPGD,DTN,ICUTE)           BDYOVL1.172
C                                                                                BDYOVL1.173
C--- NON-HYDROSDTATIC VARIABLES                                                  BDYOVL1.174
C                                                                                BDYOVL1.175
          CALL EXCHANJ(JL-JSTAR(LZ)+1,JNL,JOVEE,1-ISTAR(LZ),ISTAR(LZ),           BDYOVL1.176
     +         IEN(LZ)-1,K,WA,WAN,WB,WBN,WEBN,WEBTN,KXP1,NSPGX,DTN,ICUTE         BDYOVL1.177
     +         )                                                                 BDYOVL1.178
          CALL EXCHANJ(JL-JSTAR(LZ)+1,JNL,JOVEE,1-ISTAR(LZ),ISTAR(LZ),           BDYOVL1.179
     +         IEN(LZ)-1,K,PPA,PPAN,PPB,PPBN,PPEBN,PPEBTN,MKX,NSPGX,DTN,         BDYOVL1.180
     +         ICUTE)                                                            BDYOVL1.181
          DO 245 I=ISTAR(LZ)+ICUTE-ISEC2,IEN(LZ)                                 BDYOVL1.182
            UNJNL(I,K)=UA(I-ISTAR(LZ)+1,JL-JSTAR(LZ)+1,K)/                       BDYOVL1.183
     +                 HSCR1(I-ISTAR(LZ)+1,JL-JSTAR(LZ)+1)                       BDYOVL1.184
            VNJNL(I,K)=VA(I-ISTAR(LZ)+1,JL-JSTAR(LZ)+1,K)/                       BDYOVL1.185
     +                 HSCR1(I-ISTAR(LZ)+1,JL-JSTAR(LZ)+1)                       BDYOVL1.186
            UNJNLX(I,K)=UA(I-ISTAR(LZ)+1,JL-JSTAR(LZ),K)/                        BDYOVL1.187
     +                  HSCR1(I-ISTAR(LZ)+1,JL-JSTAR(LZ))                        BDYOVL1.188
            VNJNLX(I,K)=VA(I-ISTAR(LZ)+1,JL-JSTAR(LZ),K)/                        BDYOVL1.189
     +                  HSCR1(I-ISTAR(LZ)+1,JL-JSTAR(LZ))                        BDYOVL1.190
  245     CONTINUE                                                               BDYOVL1.191
C                                                                                BDYOVL1.192
C--- MOISTURE                                                                    BDYOVL1.193
C                                                                                BDYOVL1.194
          IF(IDRY(IN).EQ.0)THEN                                                  BDYOVL1.195
            CALL EXCHANJ(JL-JSTAR(LZ),JNLX,JOVEE,1-ISTAR(LZ),ISTAR(LZ),          BDYOVL1.196
     +           IEN(LZ)-1,K,QVA,QVAN,QVB,QVBN,QEBN,QEBTN,MKX,NSPGX,DTN,         BDYOVL1.197
     +           ICUTE)                                                          BDYOVL1.198
            IF(IMOIST(IN).EQ.2)THEN                                              BDYOVL1.199
              IF(IICE.EQ.0)THEN                                                  BDYOVL1.200
                CALL EXCHANJ(JL-JSTAR(LZ),JNLX,JOVEE,1-ISTAR(LZ),                BDYOVL1.201
     +               ISTAR(LZ),IEN(LZ)-1,K,QCA,QCAN,QCB,QCBN,QCEBN,              BDYOVL1.202
     +               QCEBTN,MKX,NSPGX,DTN,ICUTE)                                 BDYOVL1.203
                CALL EXCHANJ(JL-JSTAR(LZ),JNLX,JOVEE,1-ISTAR(LZ),                BDYOVL1.204
     +               ISTAR(LZ),IEN(LZ)-1,K,QRA,QRAN,QRB,QRBN,QREBN,              BDYOVL1.205
     +               QREBTN,MKX,NSPGX,DTN,ICUTE)                                 BDYOVL1.206
              ELSEIF(IICE.EQ.1)THEN                                              BDYOVL1.207
                CALL EXCHANJ(JL-JSTAR(LZ),JNLX,JOVEE,1-ISTAR(LZ),                BDYOVL1.208
     +               ISTAR(LZ),IEN(LZ)-1,K,QCA,QCAN,QCB,QCBN,QCEBN,              BDYOVL1.209
     +               QCEBTN,MKX,NSPGX,DTN,ICUTE)                                 BDYOVL1.210
                CALL EXCHANJ(JL-JSTAR(LZ),JNLX,JOVEE,1-ISTAR(LZ),                BDYOVL1.211
     +               ISTAR(LZ),IEN(LZ)-1,K,QIA,QIAN,QIB,QIBN,QIEBN,              BDYOVL1.212
     +               QIEBTN,MKX,NSPGX,DTN,ICUTE)                                 BDYOVL1.213
                CALL EXCHANJ(JL-JSTAR(LZ),JNLX,JOVEE,1-ISTAR(LZ),                BDYOVL1.214
     +               ISTAR(LZ),IEN(LZ)-1,K,QNIA,QNIAN,QNIB,QNIBN,QNIEBN,         BDYOVL1.215
     +               QNIEBTN,MKX,NSPGX,DTN,ICUTE)                                BDYOVL1.216
                IF(IICEG.EQ.1)THEN                                               BDYOVL1.217
                  CALL EXCHANJ(JL-JSTAR(LZ),JNLX,JOVEE,1-ISTAR(LZ),              BDYOVL1.218
     +                 ISTAR(LZ),IEN(LZ)-1,K,QGA,QGAN,QGB,QGBN,QGEBN,            BDYOVL1.219
     +                 QGEBTN,MKX,NSPGX,DTN,ICUTE)                               BDYOVL1.220
                  CALL EXCHANJ(JL-JSTAR(LZ),JNLX,JOVEE,1-ISTAR(LZ),              BDYOVL1.221
     +                 ISTAR(LZ),IEN(LZ)-1,K,QNCA,QNCAN,QNCB,QNCBN,              BDYOVL1.222
     +                 QNCEBN,QNCEBTN,MKX,NSPGX,DTN,ICUTE)                       BDYOVL1.223
                ENDIF                                                            BDYOVL1.224
              ENDIF                                                              BDYOVL1.225
            ENDIF                                                                BDYOVL1.226
          ENDIF                                                                  BDYOVL1.227
  240     CONTINUE                                                               BDYOVL1.228
        ENDIF                                                                    BDYOVL1.229
C                                                                                BDYOVL1.230
C--- NOW DO NORTH AND SOUTH BOUNDARIES,ISTAR(LZ)=1, GRID IS FURTHER NORT         BDYOVL1.231
C--- ADJUST SOUTH BOUNDARY                                                       BDYOVL1.232
C                                                                                BDYOVL1.233
        IF(ISTAR(LZ).EQ.1)THEN                                                   BDYOVL1.234
C                                                                                BDYOVL1.235
C--- TEMPERATURE                                                                 BDYOVL1.236
C                                                                                BDYOVL1.237
          CALL EXCHANI(ISTO(LZ),1,IOVES,JSTO(LZ)-1,JSTAR(LZ),JEN(LZ)-1,K         BDYOVL1.238
     +         ,TA,TAN,TB,TBN,TSBN,TSBTN,MKX,NSPGX,DTN,ICUTS)                    BDYOVL1.239
C                                                                                BDYOVL1.240
C--- U AND V                                                                     BDYOVL1.241
C                                                                                BDYOVL1.242
          CALL EXCHANI(ISTO(LZ),1,IOVES,JSTO(LZ)-1,JSTAR(LZ),JEN(LZ),K,          BDYOVL1.243
     +         UA,UAN,UB,UBN,USBN,USBTN,MKX,NSPGD,DTN,ICUTS)                     BDYOVL1.244
          CALL EXCHANI(ISTO(LZ),1,IOVES,JSTO(LZ)-1,JSTAR(LZ),JEN(LZ),K,          BDYOVL1.245
     +         VA,VAN,VB,VBN,VSBN,VSBTN,MKX,NSPGD,DTN,ICUTS)                     BDYOVL1.246
C                                                                                BDYOVL1.247
C--- NON-HYDROSTATIC VARIABLES                                                   BDYOVL1.248
C                                                                                BDYOVL1.249
          CALL EXCHANI(ISTO(LZ),1,IOVES,JSTO(LZ)-1,JSTAR(LZ),JEN(LZ)-1,K         BDYOVL1.250
     +         ,WA,WAN,WB,WBN,WSBN,WSBTN,KXP1,NSPGX,DTN,ICUTS)                   BDYOVL1.251
          CALL EXCHANI(ISTO(LZ),1,IOVES,JSTO(LZ)-1,JSTAR(LZ),JEN(LZ)-1,K         BDYOVL1.252
     +         ,PPA,PPAN,PPB,PPBN,PPSBN,PPSBTN,MKX,NSPGX,DTN,ICUTS)              BDYOVL1.253
C                                                                                BDYOVL1.254
C--- WINDS                                                                       BDYOVL1.255
C                                                                                BDYOVL1.256
          DO 306 J=JSTAR(LZ),JEN(LZ)-ICUTS+IWIN0+ISEC3                           BDYOVL1.257
            UNI1(J,K)=UA(ISTO(LZ),JSTO(LZ)+J-1,K)/                               BDYOVL1.258
     +                HSCR1(ISTO(LZ),JSTO(LZ)+J-1)                               BDYOVL1.259
            VNI1(J,K)=VA(ISTO(LZ),JSTO(LZ)+J-1,K)/                               BDYOVL1.260
     +                HSCR1(ISTO(LZ),JSTO(LZ)+J-1)                               BDYOVL1.261
            UNI2(J,K)=UA(ISTO(LZ)+1,JSTO(LZ)+J-1,K)/                             BDYOVL1.262
     +                HSCR1(ISTO(LZ)+1,JSTO(LZ)+J-1)                             BDYOVL1.263
            VNI2(J,K)=VA(ISTO(LZ)+1,JSTO(LZ)+J-1,K)/                             BDYOVL1.264
     +                HSCR1(ISTO(LZ)+1,JSTO(LZ)+J-1)                             BDYOVL1.265
  306     CONTINUE                                                               BDYOVL1.266
          IF(IDRY(IN).EQ.0)THEN                                                  BDYOVL1.267
            CALL EXCHANI(ISTO(LZ),1,IOVES,JSTO(LZ)-1,JSTAR(LZ),JEN(LZ)-1         BDYOVL1.268
     +           ,K,QVA,QVAN,QVB,QVBN,QSBN,QSBTN,MKX,NSPGX,DTN,ICUTS)            BDYOVL1.269
            IF(IMOIST(IN).EQ.2)THEN                                              BDYOVL1.270
              IF(IICE.EQ.0)THEN                                                  BDYOVL1.271
                CALL EXCHANI(ISTO(LZ),1,IOVES,JSTO(LZ)-1,JSTAR(LZ),              BDYOVL1.272
     +               JEN(LZ)-1,K,QCA,QCAN,QCB,QCBN,QCSBN,QCSBTN,MKX,             BDYOVL1.273
     +               NSPGX,DTN,ICUTS)                                            BDYOVL1.274
                CALL EXCHANI(ISTO(LZ),1,IOVES,JSTO(LZ)-1,JSTAR(LZ),              BDYOVL1.275
     +               JEN(LZ)-1,K,QRA,QRAN,QRB,QRBN,QRSBN,QRSBTN,MKX,             BDYOVL1.276
     +               NSPGX,DTN,ICUTS)                                            BDYOVL1.277
              ELSEIF(IICE.EQ.1)THEN                                              BDYOVL1.278
                CALL EXCHANI(ISTO(LZ),1,IOVES,JSTO(LZ)-1,JSTAR(LZ),              BDYOVL1.279
     +               JEN(LZ)-1,K,QCA,QCAN,QCB,QCBN,QCSBN,QCSBTN,MKX,             BDYOVL1.280
     +               NSPGX,DTN,ICUTS)                                            BDYOVL1.281
                CALL EXCHANI(ISTO(LZ),1,IOVES,JSTO(LZ)-1,JSTAR(LZ),              BDYOVL1.282
     +               JEN(LZ)-1,K,QIA,QIAN,QIB,QIBN,QISBN,QISBTN,MKX,             BDYOVL1.283
     +               NSPGX,DTN,ICUTS)                                            BDYOVL1.284
                CALL EXCHANI(ISTO(LZ),1,IOVES,JSTO(LZ)-1,JSTAR(LZ),              BDYOVL1.285
     +               JEN(LZ)-1,K,QNIA,QNIAN,QNIB,QNIBN,QNISBN,QNISBTN,           BDYOVL1.286
     +               MKX,NSPGX,DTN,ICUTS)                                        BDYOVL1.287
                IF(IICEG.EQ.1)THEN                                               BDYOVL1.288
                  CALL EXCHANI(ISTO(LZ),1,IOVES,JSTO(LZ)-1,JSTAR(LZ),            BDYOVL1.289
     +                 JEN(LZ)-1,K,QGA,QGAN,QGB,QGBN,QGSBN,QGSBTN,MKX,           BDYOVL1.290
     +                 NSPGX,DTN,ICUTS)                                          BDYOVL1.291
                  CALL EXCHANI(ISTO(LZ),1,IOVES,JSTO(LZ)-1,JSTAR(LZ),            BDYOVL1.292
     +                 JEN(LZ)-1,K,QNCA,QNCAN,QNCB,QNCBN,QNCSBN,QNCSBTN,         BDYOVL1.293
     +                 MKX,NSPGX,DTN,ICUTS)                                      BDYOVL1.294
                ENDIF                                                            BDYOVL1.295
              ENDIF                                                              BDYOVL1.296
            ENDIF                                                                BDYOVL1.297
          ENDIF                                                                  BDYOVL1.298
  300     CONTINUE                                                               BDYOVL1.299
C                                                                                BDYOVL1.300
C--- IF GRID IS FURTHER SOUTH,ADJUST NORTH BOUNDARY                              BDYOVL1.301
C                                                                                BDYOVL1.302
        ELSEIF(ISTAR(LZ).NE.1)THEN                                               BDYOVL1.303
C                                                                                BDYOVL1.304
C--- TEMPERATURE                                                                 BDYOVL1.305
C                                                                                BDYOVL1.306
          CALL EXCHANI(IL-ISTAR(LZ),INLX,IOVEN,1-JSTAR(LZ),JSTAR(LZ),            BDYOVL1.307
     +         JEN(LZ)-1,K,TA,TAN,TB,TBN,TNBN,TNBTN,MKX,NSPGX,DTN,ICUTN)         BDYOVL1.308
          CALL EXCHANI(IL-ISTAR(LZ)+1,INL,IOVEN,1-JSTAR(LZ),JSTAR(LZ),           BDYOVL1.309
     +         JEN(LZ),K,UA,UAN,UB,UBN,UNBN,UNBTN,MKX,NSPGD,DTN,ICUTN)           BDYOVL1.310
          CALL EXCHANI(IL-ISTAR(LZ)+1,INL,IOVEN,1-JSTAR(LZ),JSTAR(LZ),           BDYOVL1.311
     +         JEN(LZ),K,VA,VAN,VB,VBN,VNBN,VNBTN,MKX,NSPGD,DTN,ICUTN)           BDYOVL1.312
C                                                                                BDYOVL1.313
C--- NON-HYDROSTATIC VARIABLES                                                   BDYOVL1.314
C                                                                                BDYOVL1.315
          CALL EXCHANI(IL-ISTAR(LZ)+1,INL,IOVEN,1-JSTAR(LZ),JSTAR(LZ),           BDYOVL1.316
     +         JEN(LZ)-1,K,WA,WAN,WB,WBN,WNBN,WNBTN,KXP1,NSPGX,DTN,ICUTN         BDYOVL1.317
     +         )                                                                 BDYOVL1.318
          CALL EXCHANI(IL-ISTAR(LZ)+1,INL,IOVEN,1-JSTAR(LZ),JSTAR(LZ),           BDYOVL1.319
     +         JEN(LZ)-1,K,PPA,PPAN,PPB,PPBN,PPNBN,PPNBTN,MKX,NSPGX,DTN,         BDYOVL1.320
     +         ICUTN)                                                            BDYOVL1.321
C                                                                                BDYOVL1.322
C--- WINDS                                                                       BDYOVL1.323
C                                                                                BDYOVL1.324
          DO 345 J=JSTAR(LZ)+JCUTN,JEN(LZ)                                       BDYOVL1.325
            UNINL(J,K)=UA(IL-ISTAR(LZ)+1,J-JSTAR(LZ)+1,K)/                       BDYOVL1.326
     +                 HSCR1(IL-ISTAR(LZ)+1,J-JSTAR(LZ)+1)                       BDYOVL1.327
            VNINL(J,K)=VA(IL-ISTAR(LZ)+1,J-JSTAR(LZ)+1,K)/                       BDYOVL1.328
     +                 HSCR1(IL-ISTAR(LZ)+1,J-JSTAR(LZ)+1)                       BDYOVL1.329
            UNINLX(J,K)=UA(IL-ISTAR(LZ),J-JSTAR(LZ)+1,K)/                        BDYOVL1.330
     +                  HSCR1(IL-ISTAR(LZ),J-JSTAR(LZ)+1)                        BDYOVL1.331
            VNINLX(J,K)=VA(IL-ISTAR(LZ),J-JSTAR(LZ)+1,K)/                        BDYOVL1.332
     +                  HSCR1(IL-ISTAR(LZ),J-JSTAR(LZ)+1)                        BDYOVL1.333
  345     CONTINUE                                                               BDYOVL1.334
C                                                                                BDYOVL1.335
C--- MOISTURE                                                                    BDYOVL1.336
C                                                                                BDYOVL1.337
          IF(IDRY(IN).EQ.0)THEN                                                  BDYOVL1.338
            CALL EXCHANI(IL-ISTAR(LZ),INLX,IOVEN,1-JSTAR(LZ),JSTAR(LZ),          BDYOVL1.339
     +           JEN(LZ)-1,K,QVA,QVAN,QVB,QVBN,QNBN,QNBTN,MKX,NSPGX,DTN,         BDYOVL1.340
     +           ICUTN)                                                          BDYOVL1.341
            IF(IMOIST(IN).EQ.2)THEN                                              BDYOVL1.342
              IF(IICE.EQ.0)THEN                                                  BDYOVL1.343
                CALL EXCHANI(IL-ISTAR(LZ),INLX,IOVEN,1-JSTAR(LZ),                BDYOVL1.344
     +               JSTAR(LZ),JEN(LZ)-1,K,QCA,QCAN,QCB,QCBN,QCNBN,              BDYOVL1.345
     +               QCNBTN,MKX,NSPGX,DTN,ICUTN)                                 BDYOVL1.346
                CALL EXCHANI(IL-ISTAR(LZ),INLX,IOVEN,1-JSTAR(LZ),                BDYOVL1.347
     +               JSTAR(LZ),JEN(LZ)-1,K,QRA,QRAN,QRB,QRBN,QRNBN,              BDYOVL1.348
     +               QRNBTN,MKX,NSPGX,DTN,ICUTN)                                 BDYOVL1.349
              ELSEIF(IICE.EQ.1)THEN                                              BDYOVL1.350
                CALL EXCHANI(IL-ISTAR(LZ),INLX,IOVEN,1-JSTAR(LZ),                BDYOVL1.351
     +               JSTAR(LZ),JEN(LZ)-1,K,QCA,QCAN,QCB,QCBN,QCNBN,              BDYOVL1.352
     +               QCNBTN,MKX,NSPGX,DTN,ICUTN)                                 BDYOVL1.353
                CALL EXCHANI(IL-ISTAR(LZ),INLX,IOVEN,1-JSTAR(LZ),                BDYOVL1.354
     +               JSTAR(LZ),JEN(LZ)-1,K,QIA,QIAN,QIB,QIBN,QINBN,              BDYOVL1.355
     +               QINBTN,MKX,NSPGX,DTN,ICUTN)                                 BDYOVL1.356
                CALL EXCHANI(IL-ISTAR(LZ),INLX,IOVEN,1-JSTAR(LZ),                BDYOVL1.357
     +               JSTAR(LZ),JEN(LZ)-1,K,QNIA,QNIAN,QNIB,QNIBN,QNINBN,         BDYOVL1.358
     +               QNINBTN,MKX,NSPGX,DTN,ICUTN)                                BDYOVL1.359
                IF(IICEG.EQ.1)THEN                                               BDYOVL1.360
                  CALL EXCHANI(IL-ISTAR(LZ),INLX,IOVEN,1-JSTAR(LZ),              BDYOVL1.361
     +                 JSTAR(LZ),JEN(LZ)-1,K,QGA,QGAN,QGB,QGBN,QGNBN,            BDYOVL1.362
     +                 QGNBTN,MKX,NSPGX,DTN,ICUTN)                               BDYOVL1.363
                  CALL EXCHANI(IL-ISTAR(LZ),INLX,IOVEN,1-JSTAR(LZ),              BDYOVL1.364
     +                 JSTAR(LZ),JEN(LZ)-1,K,QNCA,QNCAN,QNCB,QNCBN,              BDYOVL1.365
     +                 QNCNBN,QNCNBTN,MKX,NSPGX,DTN,ICUTN)                       BDYOVL1.366
                ENDIF                                                            BDYOVL1.367
              ENDIF                                                              BDYOVL1.368
            ENDIF                                                                BDYOVL1.369
          ENDIF                                                                  BDYOVL1.370
  340     CONTINUE                                                               BDYOVL1.371
        ENDIF                                                                    BDYOVL1.372
C                                                                                BDYOVL1.373
  100 CONTINUE                                                                   BDYOVL1.374
      RETURN                                                                     BDYOVL1.375
      END                                                                        BDYOVL1.376
                                                                                 BDYOVL1.377
