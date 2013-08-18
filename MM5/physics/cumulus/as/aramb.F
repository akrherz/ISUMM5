       SUBROUTINE ARAMB(ITEST,DRW,IW1,DDSOL,DPSOL,DB,DC,DA,IW,IWW,MBOTH,         ARAMB.1
     *     NIMSL,ITEST2,IA,MINP,XK,                                              ARAMB.2
     *     F,XMB,KB,IER,DTIME,FTEST,KNUM)                                        ARAMB.3
      DIMENSION D(46,92)                                                         ARAMB.4
      DIMENSION DA(IA,MINP),DDSOL(MBOTH),DPSOL(NIMSL),DB(MBOTH)                  ARAMB.5
     *                ,DC(NIMSL),DRW(IWW)                                        ARAMB.6
      DIMENSION IW(IW1),XK(KNUM,KNUM)                                            ARAMB.7
      DIMENSION F(KNUM),XMB(KNUM),ITEST(KNUM),ITEST2(KNUM)                       ARAMB.8
      IER=0                                                                      ARAMB.9
C                                                                                ARAMB.10
C------- ERASE                                                                   ARAMB.11
C                                                                                ARAMB.12
    9 CONTINUE                                                                   ARAMB.13
      DO 10 K=1,IA                                                               ARAMB.14
        DO 10 KK=1,MINP                                                          ARAMB.15
          DA(K,KK)=0.0                                                           ARAMB.16
   10   CONTINUE                                                                 ARAMB.17
                                                                                 ARAMB.18
      DO 15 K=1,46                                                               ARAMB.19
        DO 15 KK=1,92                                                            ARAMB.20
          D(K,KK)=0.                                                             ARAMB.21
   15   CONTINUE                                                                 ARAMB.22
                                                                                 ARAMB.23
      DO 20 K=1,KNUM                                                             ARAMB.24
        DC(K)=0.0                                                                ARAMB.25
        ITEST(K)=0                                                               ARAMB.26
        ITEST2(K)=0                                                              ARAMB.27
        DPSOL(K)=0.0                                                             ARAMB.28
        XMB(K)=0.                                                                ARAMB.29
   20 CONTINUE                                                                   ARAMB.30
      DO 30 K=KNUM,NIMSL                                                         ARAMB.31
        DPSOL(K)=0.0                                                             ARAMB.32
        DC(K)=0.0                                                                ARAMB.33
   30 CONTINUE                                                                   ARAMB.34
      DO 35 K=1,MBOTH                                                            ARAMB.35
        DB(K)=0.0                                                                ARAMB.36
        DDSOL(K)=0.0                                                             ARAMB.37
   35 CONTINUE                                                                   ARAMB.38
      DO 37 K=1,IWW                                                              ARAMB.39
        DRW(K)=0.0                                                               ARAMB.40
   37 CONTINUE                                                                   ARAMB.41
      DO 39 K=1,IW1                                                              ARAMB.42
        IW(K)=0                                                                  ARAMB.43
   39 CONTINUE                                                                   ARAMB.44
C                                                                                ARAMB.45
C------- CONSTANTS                                                               ARAMB.46
C                                                                                ARAMB.47
      KB2=KB+1                                                                   ARAMB.48
      N=NIMSL                                                                    ARAMB.49
      M1=0                                                                       ARAMB.50
      M2=KNUM                                                                    ARAMB.51
      MBB=M1+M2                                                                  ARAMB.52
      IAA=IA                                                                     ARAMB.53
      KTEST=0                                                                    ARAMB.54
      JJTES=0                                                                    ARAMB.55
      MMTES=0                                                                    ARAMB.56
      IBCOUT=0                                                                   ARAMB.57
      IERT=1                                                                     ARAMB.58
C                                                                                ARAMB.59
C---                                                                             ARAMB.60
C                                                                                ARAMB.61
      DO 400 K=1,KNUM                                                            ARAMB.62
        SUM=0.                                                                   ARAMB.63
        DO 401 KJ=1,KNUM                                                         ARAMB.64
          SUM=SUM+ABS(XK(KJ,K))                                                  ARAMB.65
  401   CONTINUE                                                                 ARAMB.66
        SUM2=SUM/1.E-03                                                          ARAMB.67
        DSUM2=SUM2-IFIX(SUM2)                                                    ARAMB.68
        IF(ABS(DSUM2).LE.1.E-10)THEN                                             ARAMB.69
          F(K)=0.                                                                ARAMB.70
          ITEST2(K)=1                                                            ARAMB.71
          ITEST(K)=1                                                             ARAMB.72
          DO 4011 KJ=1,KNUM                                                      ARAMB.73
            XK(KJ,K)=0.                                                          ARAMB.74
 4011     CONTINUE                                                               ARAMB.75
        ELSE                                                                     ARAMB.76
          IBCOUT=1                                                               ARAMB.77
        ENDIF                                                                    ARAMB.78
  400 CONTINUE                                                                   ARAMB.79
C                                                                                ARAMB.80
C---                                                                             ARAMB.81
C                                                                                ARAMB.82
      DO 410 K=1,KNUM                                                            ARAMB.83
        IF(F(K).LE.FTEST)THEN                                                    ARAMB.84
          ITEST(K)=1                                                             ARAMB.85
          ITEST2(K)=1                                                            ARAMB.86
        ENDIF                                                                    ARAMB.87
        SUM=0.                                                                   ARAMB.88
        DO 411 KJ=1,KNUM                                                         ARAMB.89
          IF(ITEST(KJ).NE.1)SUM=SUM+ABS(XK(K,KJ))                                ARAMB.90
  411   CONTINUE                                                                 ARAMB.91
        SUM2=SUM/1.E-03                                                          ARAMB.92
        DSUM2=SUM2-IFIX(SUM2)                                                    ARAMB.93
        IF(ABS(DSUM2).LT.1.E-10)THEN                                             ARAMB.94
          ITEST(K)=1                                                             ARAMB.95
          ITEST2(K)=1                                                            ARAMB.96
          F(K)=0.                                                                ARAMB.97
          DO 4111 KJ=1,KNUM                                                      ARAMB.98
            XK(K,KJ)=0.                                                          ARAMB.99
 4111     CONTINUE                                                               ARAMB.100
        ELSE                                                                     ARAMB.101
          IBCOUT=1                                                               ARAMB.102
        ENDIF                                                                    ARAMB.103
        IF(F(K).LE.FTEST)THEN                                                    ARAMB.104
          ITEST(K)=1                                                             ARAMB.105
          ITEST2(K)=1                                                            ARAMB.106
        ENDIF                                                                    ARAMB.107
  410 CONTINUE                                                                   ARAMB.108
C                                                                                ARAMB.109
C                                                                                ARAMB.110
C                                                                                ARAMB.111
      IF(IBCOUT.EQ.0)RETURN                                                      ARAMB.112
      IBCOUT=0                                                                   ARAMB.113
      KJ=0                                                                       ARAMB.114
      DO 110 K=1,KNUM                                                            ARAMB.115
        IF((ITEST(K).EQ.1).OR.(ITEST2(K).EQ.1))THEN                              ARAMB.116
          JJTES=JJTES+1                                                          ARAMB.117
          GOTO 110                                                               ARAMB.118
        ENDIF                                                                    ARAMB.119
        KJ=K-JJTES                                                               ARAMB.120
        DO 111 KK=1,KNUM                                                         ARAMB.121
          D(KJ,KK)=-XK(K,KK)                                                     ARAMB.122
  111   CONTINUE                                                                 ARAMB.123
        DB(KJ)=F(K)*DTIME                                                        ARAMB.124
        IF(DB(KJ).GT.0.)IBCOUT=1                                                 ARAMB.125
  110 CONTINUE                                                                   ARAMB.126
      IF(IBCOUT.EQ.0)THEN                                                        ARAMB.127
        IER=999                                                                  ARAMB.128
        RETURN                                                                   ARAMB.129
      ENDIF                                                                      ARAMB.130
C                                                                                ARAMB.131
C                                                                                ARAMB.132
C                                                                                ARAMB.133
      M2=KJ                                                                      ARAMB.134
      IF(M2.LE.1)RETURN                                                          ARAMB.135
      KJ=0                                                                       ARAMB.136
      DO 120 K=1,KNUM                                                            ARAMB.137
        IF((ITEST2(K).EQ.1).OR.(ITEST(K).EQ.1))THEN                              ARAMB.138
          MMTES=MMTES+1                                                          ARAMB.139
          GOTO 120                                                               ARAMB.140
        ENDIF                                                                    ARAMB.141
        KJ=K-MMTES                                                               ARAMB.142
        DO 121 KK=1,M2                                                           ARAMB.143
          DA(KK,KJ)=D(KK,K)                                                      ARAMB.144
  121   CONTINUE                                                                 ARAMB.145
  120 CONTINUE                                                                   ARAMB.146
      N=2*M2                                                                     ARAMB.147
  600 CONTINUE                                                                   ARAMB.148
      DO 475 K=1,IA                                                              ARAMB.149
        DO 475 KK=M2+1,MINP                                                      ARAMB.150
          DA(K,KK)=0.0                                                           ARAMB.151
  475   CONTINUE                                                                 ARAMB.152
                                                                                 ARAMB.153
      DO 450 K=M2+1,IA                                                           ARAMB.154
        DO 450 KK=1,MINP                                                         ARAMB.155
          DA(K,KK)=0.0                                                           ARAMB.156
  450   CONTINUE                                                                 ARAMB.157
                                                                                 ARAMB.158
      DO 490 K=M2+1,MBOTH                                                        ARAMB.159
        DB(K)=0.0                                                                ARAMB.160
  490 CONTINUE                                                                   ARAMB.161
      DO 190 K=1,M2                                                              ARAMB.162
        DA(K,K+M2)=-1.0                                                          ARAMB.163
C      DC(K+M2)=+1.0/DB(K+M2)                                                    ARAMB.164
        DC(K+M2)=-1.0                                                            ARAMB.165
  190 CONTINUE                                                                   ARAMB.166
      DO 200 K=N+1,NIMSL                                                         ARAMB.167
        DC(K)=0.0                                                                ARAMB.168
  200 CONTINUE                                                                   ARAMB.169
cmic$ guard                                                                      ARAMB.170
      CALL ZX4LP(DA,IA,DB,DC,N,M1,M2,DS,DPSOL,DDSOL,DRW,IW,IER)                  ARAMB.171
cmic$ end guard                                                                  ARAMB.172
      IF(IER.GT.100)RETURN                                                       ARAMB.173
      KK=0                                                                       ARAMB.174
      DO 300 K=1,KNUM                                                            ARAMB.175
        IF(ITEST2(K).EQ.0)THEN                                                   ARAMB.176
          KK=KK+1                                                                ARAMB.177
          XMB(K)=DPSOL(KK)/DTIME                                                 ARAMB.178
        ELSE                                                                     ARAMB.179
          XMB(K)=0.                                                              ARAMB.180
        ENDIF                                                                    ARAMB.181
  300 CONTINUE                                                                   ARAMB.182
      RETURN                                                                     ARAMB.183
      END                                                                        ARAMB.184
                                                                                 ARAMB.185
