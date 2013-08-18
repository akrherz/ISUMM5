        SUBROUTINE SOUNDD(HE,HCD,Z,KDIM,KMIN,PW,LP,HES,QES,                      SOUNDD.1
     *        QE,T,ITEST,QRCD,PWD,                                               SOUNDD.2
     *        ZD,EDT,KDET,KNUM)                                                  SOUNDD.3
      DIMENSION PWD(KDIM,KNUM),QCD(80)                                           SOUNDD.4
      DIMENSION QES(KDIM),QE(KDIM),T(KDIM)                                       SOUNDD.5
      INTEGER KMIN(KDIM)                                                         SOUNDD.6
      DIMENSION HE(KDIM),HCD(KDIM),PW(KDIM,KNUM),HES(KDIM)                       SOUNDD.7
      DIMENSION Z(KDIM),QRCD(KDIM,KNUM),EDT(KNUM)                                SOUNDD.8
      DIMENSION ZD(KDIM,KNUM)                                                    SOUNDD.9
      IF(KDIM.GT.80)THEN                                                         SOUNDD.10
        PRINT *,'INCREASE DIMENSION OF QCD '                                     SOUNDD.11
        STOP 1212                                                                SOUNDD.12
      ENDIF                                                                      SOUNDD.13
      XL=2.5E06                                                                  SOUNDD.14
      CP=1004.                                                                   SOUNDD.15
      RD=287.04                                                                  SOUNDD.16
      RV=461.5                                                                   SOUNDD.17
      KMINF=KMIN(LP)-2                                                           SOUNDD.18
C                                                                                SOUNDD.19
C                                                                                SOUNDD.20
C------- ERASE                                                                   SOUNDD.21
C                                                                                SOUNDD.22
C                                                                                SOUNDD.23
      IDE=0                                                                      SOUNDD.24
      KMINS=KMIN(LP)                                                             SOUNDD.25
   99 JMIN=KMIN(LP)                                                              SOUNDD.26
      IF(JMIN.LE.3)THEN                                                          SOUNDD.27
        DO 599 K=1,KDIM                                                          SOUNDD.28
          ZD(K,LP)=0.                                                            SOUNDD.29
          PWD(K,LP)=0.                                                           SOUNDD.30
          QCD(K)=0.                                                              SOUNDD.31
          QRCD(K,LP)=0.                                                          SOUNDD.32
  599   CONTINUE                                                                 SOUNDD.33
        EDT(LP)=0.                                                               SOUNDD.34
        ITEST=-1                                                                 SOUNDD.35
        KMIN(LP)=-1                                                              SOUNDD.36
        RETURN                                                                   SOUNDD.37
      ENDIF                                                                      SOUNDD.38
      PTEST=0.                                                                   SOUNDD.39
      QRCD(KDIM,LP)=QES(KDIM)                                                    SOUNDD.40
      HCD(KDIM)=HES(KDIM)                                                        SOUNDD.41
      DO 50 K=1,KDIM                                                             SOUNDD.42
        PWD(K,LP)=0.                                                             SOUNDD.43
        IF(K.NE.KDIM)HCD(K)=.5*(HES(K)+HES(K+1))                                 SOUNDD.44
        IF(K.NE.KDIM)QRCD(K,LP)=.5*(QES(K)+QES(K+1))                             SOUNDD.45
        QCD(K)=QRCD(K,LP)                                                        SOUNDD.46
        ZD(K,LP)=0.                                                              SOUNDD.47
   50 CONTINUE                                                                   SOUNDD.48
C                                                                                SOUNDD.49
C                                                                                SOUNDD.50
C------- BOUNDARY CONDITIONS9                                                    SOUNDD.51
C                                                                                SOUNDD.52
C                                                                                SOUNDD.53
      ZD(JMIN,LP)=1.                                                             SOUNDD.54
      DR=0.                                                                      SOUNDD.55
      HCD(JMIN)=.5*(HE(JMIN)+HE(JMIN+1))                                         SOUNDD.56
      QCD(JMIN)=.5*(QE(JMIN)+QE(JMIN+1))                                         SOUNDD.57
C                                                                                SOUNDD.58
C                                                                                SOUNDD.59
C------- FIRST CALCULATE AVAILABLE WATER.                                        SOUNDD.60
C                                                                                SOUNDD.61
C                                                                                SOUNDD.62
      PWEV=0.                                                                    SOUNDD.63
      BU=0.                                                                      SOUNDD.64
C                                                                                SOUNDD.65
C                                                                                SOUNDD.66
C------- CALCULATE DOWNDRAFT SOUNDING. MAKE SURE                                 SOUNDD.67
C------- ENOUGH WATER IS AVAILABLE TO SUSTAIN DOWNDRAFT.                         SOUNDD.68
C                                                                                SOUNDD.69
C                                                                                SOUNDD.70
      PWAV=0.                                                                    SOUNDD.71
      DO 110 LL=1,KDIM                                                           SOUNDD.72
        PWAV=PWAV+PW(LL,LP)                                                      SOUNDD.73
  110 CONTINUE                                                                   SOUNDD.74
      DO 100 K=1,JMIN-1                                                          SOUNDD.75
        KK=JMIN-K                                                                SOUNDD.76
        DZ=-(Z(KK)-Z(KK+2))*.5                                                   SOUNDD.77
C                                                                                SOUNDD.78
C------- NORMALIZED MASS FLUX PROFILE                                            SOUNDD.79
C                                                                                SOUNDD.80
        IF(KK.LE.KDET)THEN                                                       SOUNDD.81
          DKK=.25*(ZD(KK+1,LP)*(1.+DR*DZ))                                       SOUNDD.82
        ELSE                                                                     SOUNDD.83
          DKK=0.                                                                 SOUNDD.84
        ENDIF                                                                    SOUNDD.85
        ZD(KK,LP)=ZD(KK+1,LP)*(1.+DR*DZ)-DKK                                     SOUNDD.86
C                                                                                SOUNDD.87
C                                                                                SOUNDD.88
C------- MOIST STATIC ENERGY. REALLY ONLY VALID IF SATURATED!                    SOUNDD.89
C                                                                                SOUNDD.90
C                                                                                SOUNDD.91
        HCD(KK)=((HCD(KK+1)*(ZD(KK+1,LP)-.5*DKK))+DR*DZ*ZD(KK+1,LP)*             SOUNDD.92
     +          HE(KK+1))/(ZD(KK,LP)+.5*DKK)                                     SOUNDD.93
        BU=BU+DZ*(HCD(KK)-.5*(HES(KK)+HES(KK+1)))                                SOUNDD.94
        IF(BU.GE.0.)THEN                                                         SOUNDD.95
          KMIN(LP)=KMIN(LP)-1                                                    SOUNDD.96
          IF(KMIN(LP).LT.KMINF)THEN                                              SOUNDD.97
            ITEST=-1                                                             SOUNDD.98
            KMIN(LP)=-1                                                          SOUNDD.99
            DO 300 KKK=1,KDIM                                                    SOUNDD.100
              PWD(KKK,LP)=0.                                                     SOUNDD.101
              HCD(KKK)=0.                                                        SOUNDD.102
              QCD(KKK)=0.                                                        SOUNDD.103
              ZD(KKK,LP)=0.                                                      SOUNDD.104
              QRCD(KKK,LP)=0.                                                    SOUNDD.105
  300       CONTINUE                                                             SOUNDD.106
            RETURN                                                               SOUNDD.107
          ENDIF                                                                  SOUNDD.108
          IF((EDT(LP).LT.2).AND.(KMINS.NE.0))THEN                                SOUNDD.109
            EDT(LP)=EDT(LP)-EDT(LP)/FLOAT(KMINS)                                 SOUNDD.110
          ENDIF                                                                  SOUNDD.111
          GOTO 99                                                                SOUNDD.112
        ENDIF                                                                    SOUNDD.113
C                                                                                SOUNDD.114
C                                                                                SOUNDD.115
C------- HOW MUCH WATER WITHOUT EVAPORATION?                                     SOUNDD.116
C                                                                                SOUNDD.117
C                                                                                SOUNDD.118
        QCD(KK)=((QCD(KK+1)*(ZD(KK+1,LP)-.5*DKK))+DR*DZ*ZD(KK+1,LP)*             SOUNDD.119
     +          QE(KK+1))/(ZD(KK,LP)+.5*DKK)                                     SOUNDD.120
C                                                                                SOUNDD.121
C                                                                                SOUNDD.122
C------- HOW MUCH WATER HAS TO BE EVAPORATED TO KEEP`M GOING?                    SOUNDD.123
C                                                                                SOUNDD.124
C                                                                                SOUNDD.125
        DQ=(QES(KK)+QES(KK+1))*.5                                                SOUNDD.126
        DT=(T(KK)+T(KK+1))*.5                                                    SOUNDD.127
        GAMMA=(XL/CP)*(XL/(RV*(DT**2)))*DQ                                       SOUNDD.128
        DH=HCD(KK)-.5*(HES(KK)+HES(KK+1))                                        SOUNDD.129
        QRCD(KK,LP)=(DQ+(1./XL)*(GAMMA/(1.+GAMMA))*DH)                           SOUNDD.130
C-CHECK                                                                          SOUNDD.131
        PWD(KK,LP)=-(QRCD(KK,LP)-QCD(KK))*ZD(KK,LP)                              SOUNDD.132
        QCD(KK)=QRCD(KK,LP)                                                      SOUNDD.133
C                                                                                SOUNDD.134
        PWEV=PWEV+PWD(KK,LP)                                                     SOUNDD.135
C                                                                                SOUNDD.136
  100 CONTINUE                                                                   SOUNDD.137
      IF(PWEV.NE.0.)EDTEST=-EDT(LP)*PWAV/PWEV                                    SOUNDD.138
      EDT(LP)=EDTEST                                                             SOUNDD.139
C        IF(EDT(LP).LT.0.25)EDT(LP)=0.25                                         SOUNDD.140
      IF(EDT(LP).GT.1.0)EDT(LP)=1.0                                              SOUNDD.141
C                                                                                SOUNDD.142
C------ MAKE SURE BUOYANCY IS NEGATIVE!                                          SOUNDD.143
C                                                                                SOUNDD.144
      BU=0.                                                                      SOUNDD.145
C                                                                                SOUNDD.146
      DO 200 K=1,JMIN-2                                                          SOUNDD.147
        KK=JMIN-K                                                                SOUNDD.148
        DZ=-(Z(KK-1)-Z(KK+1))*.5                                                 SOUNDD.149
        HS=(HES(KK)+HES(KK+1))*.5                                                SOUNDD.150
        BU=BU+DZ*(HCD(KK)-HS)                                                    SOUNDD.151
        IF(BU.GT.0.)THEN                                                         SOUNDD.152
          ITEST=-1                                                               SOUNDD.153
          DO 400 KKK=1,KDIM                                                      SOUNDD.154
            PWD(KKK,LP)=0.                                                       SOUNDD.155
            HCD(KKK)=0.                                                          SOUNDD.156
            QCD(KKK)=0.                                                          SOUNDD.157
            ZD(KKK,LP)=0.                                                        SOUNDD.158
            QRCD(KKK,LP)=0.                                                      SOUNDD.159
  400     CONTINUE                                                               SOUNDD.160
          RETURN                                                                 SOUNDD.161
        ENDIF                                                                    SOUNDD.162
  200 CONTINUE                                                                   SOUNDD.163
C                                                                                SOUNDD.164
C--- EVAPORATED RAIN FOR THIS CLOUD!                                             SOUNDD.165
C                                                                                SOUNDD.166
  500 CONTINUE                                                                   SOUNDD.167
      DO 600 K=JMIN+1,KDIM                                                       SOUNDD.168
        ZD(K,LP)=0.                                                              SOUNDD.169
        PWD(K,LP)=0.                                                             SOUNDD.170
  600 CONTINUE                                                                   SOUNDD.171
  601 CONTINUE                                                                   SOUNDD.172
      ITEST=0                                                                    SOUNDD.173
      RETURN                                                                     SOUNDD.174
      END                                                                        SOUNDD.175
                                                                                 SOUNDD.176
