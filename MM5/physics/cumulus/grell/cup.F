#include <defines.incl>                                                          07NOV00.1230
        SUBROUTINE CUP(T,Q,Z1,TN,QO,PO,PRE,P,OUTTEM,OUTQ,                        07NOV00.1231
     &             DTIME,PSUR,US,VS,IST,IEN,KDET,DX,IFRAD,CLDFRA,                07NOV00.1232
     &             R,CP,G,TO,XLV,RV,A,PTOP)                                      07NOV00.1233
      IMPLICIT NONE                                                              07NOV00.1234
#     include <parame.incl>                                                      CUP.4
#     include <parccm2.incl>                                                     CUP.6
#     include <pargrcu.incl>                                                     CUP.8
      REAL R,CP,G,TO,XLV,RV,A(MKX),PTOP                                          07NOV00.1235
                                                                                 07NOV00.1236
C                                                                                CUP.15
C  CLFRCV = CLOUD FRACTIONAL COVER FOR CONV. PRECIP.                             CUP.16
C  CLLWCV = CLOUD LIQUID WATER CONTENT FOR CONV. PRECIP.                         CUP.17
C                                                                                CUP.18
C                                                                                CUP.19
C                                                                                CUP.20
      REAL      AE(2),BE(2),HT(2)                                                07NOV00.1237
C                                                                                CUP.22
C--- INPUT VARIABLES                                                             CUP.23
C                                                                                CUP.24
      REAL      P(MIX,MKX),T(MIX,MKX),Q(MIX,MKX),                                07NOV00.1238
     1          PO(MIX,MKX),TN(MIX,MKX),QO(MIX,MKX),Z1(MIX),                     CUP.26
     2          US(MIX,MKX),VS(MIX,MKX),CLDFRA(IPLOND,IPLEVP)                    CUP.27
C                                                                                CUP.28
C--- WORK VARIABLES                                                              CUP.29
C                                                                                CUP.30
      REAL      HE(MIX,MKX),HES(MIX,MKX),QES(MIX,MKX),                           07NOV00.1239
     1          Z(MIX,MKX),TV(MIX,MKX),DBY(MIX,MKX),                             CUP.32
     2          QC(MIX,MKX),QRCD(MIX,MKX),PWD(MIX,MKX),                          CUP.33
     3          PW(MIX,MKX),EDT(MIX),EDTO(MIX),EDTX(MIX),                        CUP.34
     4          AA1(MIX),AA0(MIX),XAA0(MIX),HKB(MIX),HKBO(MIX),                  CUP.35
     5          XHKB(MIX),QKB(MIX),QKBO(MIX),XMB(MIX)                            CUP.36
      REAL      HEO(MIX,MKX),HESO(MIX,MKX),QESO(MIX,MKX),                        07NOV00.1240
     1          ZO(MIX,MKX),TVO(MIX,MKX),DBYO(MIX,MKX),                          CUP.38
     2          QCO(MIX,MKX),QRCDO(MIX,MKX),PWDO(MIX,MKX),                       CUP.39
     3          PWO(MIX,MKX),PRE(MIX),PSUR(MIX),                                 CUP.40
     4          XPWAV(MIX),XPWEV(MIX),PWAV(MIX),PWEV(MIX),                       CUP.41
     5          PWAVO(MIX),PWEVO(MIX),HCD(MIX),QCK(MIX),QCKO(MIX),               CUP.42
     6          HCDO(MIX),QCD(MIX),QCDO(MIX),BU(MIX),BUO(MIX),                   CUP.43
     7          XQKB(MIX),XHCD(MIX),XQCD(MIX),XQCK(MIX)                          CUP.44
      REAL      XHE(MIX,MKX),XHES(MIX,MKX),XQES(MIX,MKX),                        07NOV00.1241
     1          XZ(MIX,MKX),XTV(MIX,MKX),XT(MIX,MKX),XQ(MIX,MKX),                CUP.46
     2          XDBY(MIX,MKX),XQC(MIX,MKX),XQRCD(MIX,MKX),                       CUP.47
     3          XPWD(MIX,MKX),XPW(MIX,MKX),VSHEAR(MIX)                           CUP.48
     4          ,VWS(MIX), SDP(MIX)                                              CUP.49
      REAL      DELLAH(MIX,MKX),DELLAQ(MIX,MKX),DELLAT(MIX,MKX)                  07NOV00.1242
      REAL      DKK(MIX,MKX)                                                     07NOV00.1243
      INTEGER KDET(MIX)                                                          CUP.52
      INTEGER K22(MIX),KBCON(MIX),KB(MIX),JMIN(MIX),KTOP(MIX),                   CUP.53
     & KBHE(MIX)                                                                 CUP.54
      REAL F,XK,MBDT                                                             CUP.55
#ifdef IBMopt                                                                    23DEC04.3041
      real zzz,zz1                                                               23DEC04.3042
#endif                                                                           23DEC04.3043
C                                                                                CUP.56
C--- OUTPUT VARIABLES                                                            CUP.57
C                                                                                CUP.58
      REAL      OUTTEM(MIX,MKX),OUTQ(MIX,MKX)                                    07NOV00.1244
C                                                                                07NOV00.1245
C DECLARATIONS FOR IMPLICIT NONE                                                 07NOV00.1246
C                                                                                07NOV00.1247
      REAL      XL,RD,DTIME,C0,E,EO,TVBAR,TVBARO,DH,PBCDIF,DZ,DZ1                07NOV00.1248
      REAL      GAMMA,GAMMA0,QRCH,DZO,DZ2,GAMMO,GAMMO0,QRCHO,DQ                  07NOV00.1249
      REAL      DT,DP,DV1,DV2,DV3,DV1Q,DV2Q,DV3Q,DETDO,DETDOQ,AUP                07NOV00.1250
      REAL      ADW,GAMMA1,GAMMA2,DHH,DG,XDDD,OUTTES,DXTEMC                      07NOV00.1251
      REAL      DX,DSMALC,DLARGC,CLFRCV,AFRACL,AFRACS,AKCLTH                     07NOV00.1252
      INTEGER   I,J,K,KK,IERR,IPH,IPHO,IST,IEN,KBDIF                             07NOV00.1253
      INTEGER   IFRAD,KCLTH                                                      07NOV00.1254
#ifdef IBMopt                                                                    23DEC04.3044
      real plog(mix,mkx),polog(mix,mkx),psurlog(mix)                             23DEC04.3045
      real temp1(mix),temp2(mix),e1                                              23DEC04.3046
      integer ind1(mix),icount1,is,nn                                            23DEC04.3047
#ifdef MPP1                                                                      23DEC04.3048
#define nf(A) noflic(A)
#else                                                                            23DEC04.3050
#define nf(A) A       
#endif                                                                           23DEC04.3052
#endif                                                                           23DEC04.3053
C                                                                                07NOV00.1255
C                                                                                05DEC01.694
C  AFRACL = FRACTIONAL COVER FOR CONV. PRECIP. WHEN DX=DXLARG=200 KM             05DEC01.695
C  AFRACS =     "        "    "    "      "     "   DX=DXSMAL=10 KM              05DEC01.696
C                                                                                05DEC01.697
      DATA AFRACL,AFRACS,DLARGC,DSMALC /0.3,1.0,200.0E3,10.0E3/                  05DEC01.698
C                                                                                05DEC01.699
C                                                                                CUP.60
C     REPLACE     KBMAX=((MKX*3)/4-3) WITH SIGMA DEPENDENCE IN PARAM             CUP.61
      IERR=0                                                                     CUP.62
C     TCRIT MAY NOW BE RESET IN PPARAM NAMELIST (273.15 DEFAULT)                 CUP.63
C     IN WARM-RAIN CASE STILL MUST SET TCRIT SMALL LIKE TO                       CUP.64
      IF(TO.LT.100.)TCRIT=TO                                                     CUP.65
C      XL=2.5E06                                                                 CUP.66
C      RV=461.9                                                                  CUP.67
C      RD=287.04                                                                 CUP.68
      XL=XLV                                                                     CUP.69
      RD=R                                                                       CUP.70
      HT(1)=XL/CP                                                                CUP.71
      HT(2)=2.834E6/CP                                                           CUP.72
      BE(1)=.622*HT(1)/.286                                                      CUP.73
      AE(1)=BE(1)/273.+ALOG(610.71)                                              CUP.74
      BE(2)=.622*HT(2)/.286                                                      CUP.75
      AE(2)=BE(2)/273.+ALOG(610.71)                                              CUP.76
      MBDT=DTIME*5.E-03                                                          CUP.77
      C0=.002                                                                    CUP.78
      F=-1.                                                                      CUP.79
      XK=-1.                                                                     CUP.80
#ifdef IBMopt                                                                    23DEC04.3054
#ifdef MPP1                                                                      23DEC04.3055
      nn = ie_x0(IEN) - is_x0(IST) + 1                                           23DEC04.3056
      is = is_x0(IST)                                                            23DEC04.3057
#else                                                                            23DEC04.3058
      nn = IEN - IST + 1                                                         23DEC04.3059
      is = IST                                                                   23DEC04.3060
#endif                                                                           23DEC04.3061
#endif                                                                           23DEC04.3062
C                                                                                CUP.81
C--- ENVIRONMENTAL CONDITIONS, FIRST HEIGHTS                                     CUP.82
C                                                                                CUP.83
      DO 5 K=1,MKX                                                               CUP.84
#ifndef IBMopt                                                                   23DEC04.3063
        DO 5 I=IST,IEN                                                           07NOV00.1256
#else                                                                            23DEC04.3064
        DO  I=IST,IEN                                                            23DEC04.3065
#endif                                                                           23DEC04.3066
          DKK(I,K)=1.                                                            CUP.86
          IPH=1                                                                  CUP.87
          IPHO=1                                                                 CUP.88
          IF(T(I,K).LE.TCRIT)IPH=2                                               CUP.89
          IF(TN(I,K).LE.TCRIT)IPHO=2                                             CUP.90
#ifndef IBMopt                                                                   23DEC04.3067
          E=EXP(AE(IPH)-BE(IPH)/T(I,K))                                          CUP.91
          EO=EXP(AE(IPHO)-BE(IPHO)/TN(I,K))                                      CUP.92
#else                                                                            23DEC04.3068
          temp1(i)=AE(IPH)-BE(IPH)/T(I,K)                                        23DEC04.3069
          temp2(i)=AE(IPHO)-BE(IPHO)/TN(I,K)                                     23DEC04.3070
        ENDDO                                                                    23DEC04.3071
                                                                                 23DEC04.3072
        call vsexp(temp1(nf(is)),temp1(nf(is)),nn)                               23DEC04.3073
        call vsexp(temp2(nf(is)),temp2(nf(is)),nn)                               23DEC04.3074
                                                                                 23DEC04.3075
         DO I=IST,IEN                                                            23DEC04.3076
            E=temp1(i)                                                           23DEC04.3077
            EO=temp2(i)                                                          23DEC04.3078
#endif                                                                           23DEC04.3079
          QES(I,K)=.622*E/(100.*P(I,K)-E)                                        25JAN00.471
          QESO(I,K)=.622*EO/(100.*PO(I,K)-EO)                                    25JAN00.472
          IF(QES(I,K).LE.1.E-08)QES(I,K)=1.E-08                                  CUP.95
          IF(Q(I,K).GT.QES(I,K))Q(I,K)=QES(I,K)                                  CUP.96
          IF(QESO(I,K).LE.1.E-08)QESO(I,K)=1.E-08                                CUP.97
          IF(QO(I,K).GT.QESO(I,K))QO(I,K)=QESO(I,K)                              CUP.98
          TV(I,K)=T(I,K)+.608*Q(I,K)*T(I,K)                                      CUP.99
          TVO(I,K)=TN(I,K)+.608*QO(I,K)*TN(I,K)                                  CUP.100
#ifdef IBMopt                                                                    23DEC04.3080
        ENDDO                                                                    23DEC04.3081
#endif                                                                           23DEC04.3082
    5   CONTINUE                                                                 CUP.101
#ifdef IBMopt                                                                    23DEC04.3083
         k=1                                                                     23DEC04.3084
                                                                                 23DEC04.3085
         call vslog(plog(nf(is),k),p(nf(is),k),nn)                               23DEC04.3086
         call vslog(polog(nf(is),k),po(nf(is),k),nn)                             23DEC04.3087
         call vslog(psurlog(nf(is)),psur(nf(is)),nn)                             23DEC04.3088
#endif                                                                           23DEC04.3089
                                                                                 CUP.102
      DO 7 I=IST,IEN                                                             07NOV00.1257
C     HKB(I)=0.                                                                  CUP.104
C     QKB(I)=0.                                                                  CUP.105
C     HKBO(I)=0.                                                                 CUP.106
C     QKBO(I)=0.                                                                 CUP.107
C     XHKB(I)=0.                                                                 CUP.108
C     XQKB(I)=0.                                                                 CUP.109
        AA1(I)=0.                                                                CUP.110
        AA0(I)=0.                                                                CUP.111
        XAA0(I)=0.                                                               CUP.112
        XPWAV(I)=0.                                                              CUP.113
        XPWEV(I)=0.                                                              CUP.114
        PWAV(I)=0.                                                               CUP.115
        PWEV(I)=0.                                                               CUP.116
        PWAVO(I)=0.                                                              CUP.117
        PWEVO(I)=0.                                                              CUP.118
        K22(I)=1                                                                 CUP.119
        KBHE(I)=1                                                                CUP.120
        KTOP(I)=1                                                                CUP.121
        KBCON(I)=1                                                               CUP.122
        KB(I)=1                                                                  CUP.123
        JMIN(I)=1                                                                CUP.124
C     EDT(I)=0.                                                                  CUP.125
C     EDTO(I)=0.                                                                 CUP.126
C     EDTX(I)=0.                                                                 CUP.127
        XMB(I)=0.                                                                CUP.128
        VSHEAR(I)=0.                                                             CUP.129
        VWS(I)=0.                                                                CUP.130
        SDP(I)=0.                                                                CUP.131
#ifndef IBMopt                                                                   23DEC04.3090
        Z(I,1)=Z1(I)-(ALOG(P(I,1))-ALOG(PSUR(I)))*RD*TV(I,1)/9.81                CUP.132
        ZO(I,1)=Z1(I)-(ALOG(PO(I,1))-ALOG(PSUR(I)))*RD*TVO(I,1)/9.81             CUP.133
#else                                                                            23DEC04.3091
        Z(I,1)=Z1(I)-(plog(I,1)-psurlog(I))*RD*TV(I,1)/9.81                      23DEC04.3092
        ZO(I,1)=Z1(I)-(polog(I,1)-psurlog(I))*RD*TVO(I,1)/9.81                   23DEC04.3093
#endif                                                                           23DEC04.3094
    7 CONTINUE                                                                   CUP.134
      DO 10 K=2,MKX                                                              CUP.135
#ifdef IBMopt                                                                    23DEC04.3095
                                                                                 23DEC04.3096
         call vslog(plog(nf(is),k),p(nf(is),k),nn)                               23DEC04.3097
         call vslog(polog(nf(is),k),po(nf(is),k),nn)                             23DEC04.3098
                                                                                 23DEC04.3099
#endif                                                                           23DEC04.3100
        DO 10 I=IST,IEN                                                          07NOV00.1258
          TVBAR=.5*TV(I,K)+.5*TV(I,K-1)                                          CUP.137
#ifndef IBMopt                                                                   23DEC04.3101
          Z(I,K)=Z(I,K-1)-(ALOG(P(I,K))-ALOG(P(I,K-1)))*RD*TVBAR/9.81            CUP.138
#else                                                                            23DEC04.3102
          Z(I,K)=Z(I,K-1)-(plog(I,K)-plog(I,K-1))*RD*TVBAR/9.81                  23DEC04.3103
#endif                                                                           23DEC04.3104
          TVBARO=.5*TVO(I,K)+.5*TVO(I,K-1)                                       CUP.139
#ifndef IBMopt                                                                   23DEC04.3105
          ZO(I,K)=ZO(I,K-1)-(ALOG(PO(I,K))-ALOG(PO(I,K-1)))*RD*TVBARO/           CUP.140
     +            9.81                                                           CUP.141
#else                                                                            23DEC04.3106
          ZO(I,K)=ZO(I,K-1)-(polog(I,K)-polog(I,K-1))*RD*TVBARO/                 23DEC04.3107
     +            9.81                                                           23DEC04.3108
#endif                                                                           23DEC04.3109
   10   CONTINUE                                                                 CUP.142
                                                                                 CUP.143
C                                                                                CUP.144
C--- MOIST STATIC ENERGY                                                         CUP.145
C                                                                                CUP.146
      DO 21 K=1,MKX                                                              CUP.147
        DO 20 I=IST,IEN                                                          07NOV00.1259
          PW(I,K)=0.                                                             CUP.149
          XPW(I,K)=0.                                                            CUP.150
          PWO(I,K)=0.                                                            CUP.151
          QC(I,K)=0.                                                             CUP.152
          XQC(I,K)=0.                                                            CUP.153
          QCO(I,K)=0.                                                            CUP.154
          PWD(I,K)=0.                                                            CUP.155
          PWDO(I,K)=0.                                                           CUP.156
          XPWD(I,K)=0.                                                           CUP.157
          DELLAH(I,K)=0.                                                         CUP.158
          DELLAQ(I,K)=0.                                                         CUP.159
          DELLAT(I,K)=0.                                                         CUP.160
          HE(I,K)=9.81*Z(I,K)+1004.*T(I,K)+2.5E06*Q(I,K)                         CUP.161
          HES(I,K)=9.81*Z(I,K)+1004.*T(I,K)+2.5E06*QES(I,K)                      CUP.162
          IF(HE(I,K).GE.HES(I,K))HE(I,K)=HES(I,K)                                CUP.163
          HEO(I,K)=9.81*ZO(I,K)+1004.*TN(I,K)+2.5E06*QO(I,K)                     CUP.164
          HESO(I,K)=9.81*ZO(I,K)+1004.*TN(I,K)+2.5E06*QESO(I,K)                  CUP.165
          IF(HEO(I,K).GE.HESO(I,K))HEO(I,K)=HESO(I,K)                            CUP.166
          XT(I,K)=T(I,K)                                                         CUP.167
          XQ(I,K)=Q(I,K)                                                         CUP.168
          XHE(I,K)=HE(I,K)                                                       CUP.169
          IF(K.EQ.MKX)GOTO 20                                                    CUP.170
          IF(K.NE.MKX)QRCD(I,K)=.5*(QES(I,K)+QES(I,K+1))                         CUP.171
          IF(K.NE.MKX)QRCDO(I,K)=.5*(QESO(I,K)+QESO(I,K+1))                      CUP.172
   20   CONTINUE                                                                 CUP.173
   21 CONTINUE                                                                   CUP.174
C                                                                                CUP.175
C------- DETERMINE LEVEL WITH HIGHEST MOIST STATIC ENERGY CONTENT.               CUP.176
C                                                                                CUP.177
      CALL MAXIMI(HE,MIX,MJX,MKX,1,KBMAX,K22,IST,IEN)                            07NOV00.1260
      DO 36 I=IST,IEN                                                            07NOV00.1261
        IF(AA0(I).LT.0)GOTO 36                                                   CUP.180
        IF(K22(I).GE.KBMAX)THEN                                                  CUP.181
          AA0(I)=-1.                                                             CUP.182
          GOTO 36                                                                CUP.183
        ENDIF                                                                    CUP.184
C     HKB(I)=HE(I,K22(I))                                                        CUP.185
C     QKB(I)=Q(I,K22(I))                                                         CUP.186
C     HKBO(I)=HEO(I,K22(I))                                                      CUP.187
C     QKBO(I)=QO(I,K22(I))                                                       CUP.188
        HKB(I)=.5*(HE(I,K22(I))+HE(I,K22(I)+1))                                  CUP.189
        QKB(I)=.5*(Q(I,K22(I))+Q(I,K22(I)+1))                                    CUP.190
        HKBO(I)=.5*(HEO(I,K22(I))+HEO(I,K22(I)+1))                               CUP.191
        QKBO(I)=.5*(QO(I,K22(I))+QO(I,K22(I)+1))                                 CUP.192
        QCK(I)=QKB(I)                                                            CUP.193
        QCKO(I)=QKBO(I)                                                          CUP.194
   36 CONTINUE                                                                   CUP.195
C                                                                                CUP.196
C--- DECIDE FOR CONVECTIVE CLOUD BASE                                            CUP.197
C                                                                                CUP.198
      DO 27 I=IST,IEN                                                            07NOV00.1262
        IF(AA0(I).LT.0)GOTO 27                                                   CUP.200
        DO 28 K=1,KDET(I)                                                        CUP.201
          KK=KDET(I)-K+1                                                         CUP.202
          DKK(I,K)=1.-FLOAT(KK)/FLOAT(KDET(I))                                   CUP.203
   28   CONTINUE                                                                 CUP.204
        KB(I)=K22(I)                                                             CUP.205
        KBCON(I)=KB(I)                                                           CUP.206
        GOTO 32                                                                  CUP.207
   31   CONTINUE                                                                 CUP.208
        KBCON(I)=KBCON(I)+1                                                      CUP.209
        IF(KBCON(I).GT.KBMAX)THEN                                                CUP.210
          AA0(I)=-1.                                                             CUP.211
          GOTO 27                                                                CUP.212
        ENDIF                                                                    CUP.213
   32   CONTINUE                                                                 CUP.214
        DH=.5*HES(I,KBCON(I))+.5*HES(I,KBCON(I)+1)                               CUP.215
        IF(HKB(I).LT.DH)GOTO 31                                                  CUP.216
        PBCDIF=-P(I,KBCON(I))+P(I,KB(I))                                         CUP.217
        KBDIF=KBCON(I)-KB(I)                                                     CUP.218
        IF(KBDIF.GT.1)THEN                                                       CUP.219
          IF(PBCDIF.GT.PBCMAX)THEN                                               CUP.220
            K22(I)=K22(I)+1                                                      CUP.221
C       HKB(I)=HE(I,K22(I))                                                      CUP.222
C       QKB(I)=Q(I,K22(I))                                                       CUP.223
C       HKBO(I)=HEO(I,K22(I))                                                    CUP.224
C       QKBO(I)=QO(I,K22(I))                                                     CUP.225
            HKB(I)=.5*(HE(I,K22(I))+HE(I,K22(I)+1))                              CUP.226
            QKB(I)=.5*(Q(I,K22(I))+Q(I,K22(I)+1))                                CUP.227
            HKBO(I)=.5*(HEO(I,K22(I))+HEO(I,K22(I)+1))                           CUP.228
            QKBO(I)=.5*(QO(I,K22(I))+QO(I,K22(I)+1))                             CUP.229
            QCK(I)=QKB(I)                                                        CUP.230
            QCKO(I)=QKBO(I)                                                      CUP.231
            KB(I)=K22(I)                                                         CUP.232
            KBCON(I)=KB(I)                                                       CUP.233
            GOTO 32                                                              CUP.234
          ENDIF                                                                  CUP.235
        ENDIF                                                                    CUP.236
   27 CONTINUE                                                                   CUP.237
C                                                                                CUP.238
C**************************** STATIC CONTROL                                     CUP.239
C                                                                                CUP.240
C                                                                                CUP.241
C--- DETERMINE CLOUD TOP                                                         CUP.242
C                                                                                CUP.243
      DO 37 I=IST,IEN                                                            07NOV00.1263
        IF(AA0(I).LT.0)GOTO 37                                                   CUP.245
        DBY(I,MKX)=HKB(I)-HES(I,MKX)                                             CUP.246
        DBYO(I,MKX)=HKBO(I)-HESO(I,MKX)                                          CUP.247
   37 CONTINUE                                                                   CUP.248
      DO 35 K=1,MKX-1                                                            CUP.249
        DO 35 I=IST,IEN                                                          07NOV00.1264
          IF(AA0(I).EQ.-1.)GOTO 35                                               CUP.251
          DBY(I,K)=HKB(I)-.5*(HES(I,K)+HES(I,K+1))                               CUP.252
          DBYO(I,K)=HKBO(I)-.5*(HESO(I,K)+HESO(I,K+1))                           CUP.253
   35   CONTINUE                                                                 CUP.254
                                                                                 CUP.255
      DO 42 I=IST,IEN                                                            07NOV00.1265
        IF(AA0(I).EQ.-1.)GOTO 42                                                 CUP.257
C       SEARCH FOR CLOUD TOP IS BOTTOM UP FROM LFC                               CUP.258
        DO 40 K=KBCON(I),MKX                                                     CUP.259
          IF(DBY(I,K).LE.0.)THEN                                                 CUP.260
            KTOP(I)=K                                                            CUP.261
            GOTO 41                                                              CUP.262
          ENDIF                                                                  CUP.263
   40   CONTINUE                                                                 CUP.264
        AA0(I)=-1.                                                               CUP.265
        GOTO 42                                                                  CUP.266
   41   CONTINUE                                                                 CUP.267
        IF(KTOP(I).GT.MKX)KTOP(I)=MKX                                            CUP.268
        IF(IFCDZ)THEN                                                            CUP.269
          IF(-Z(I,KBCON(I))+Z(I,KTOP(I)).LT.CDMINZ)AA0(I)=-1.                    CUP.270
        ELSE                                                                     CUP.271
          IF(P(I,KBCON(I))-P(I,KTOP(I)).LT.CDMINP)AA0(I)=-1.                     CUP.272
        ENDIF                                                                    CUP.273
   42 CONTINUE                                                                   CUP.274
C                                                                                CUP.275
C--- DOWNDRAFT ORIGINATING LEVEL                                                 CUP.276
C                                                                                CUP.277
      CALL MINIMI(HE,MIX,MJX,MKX,KB,KTOP,JMIN,IST,IEN)                           07NOV00.1266
C                                                                                CUP.279
C------- MOISTURE AND CLOUD WORK FUNCTIONS                                       CUP.280
C                                                                                CUP.281
      DO 55 K=2,MKX-1                                                            07NOV00.1267
        DO 55 I=IST,IEN                                                          07NOV00.1268
         IF(AA0(I).NE.-1.)THEN                                                   07NOV00.1269
          IF(JMIN(I).LE.3)THEN                                                   07NOV00.1270
            AA0(I)=-1.                                                           07NOV00.1271
          ELSE IF (K.GT.KBCON(I).AND.K.LT.KTOP(I))THEN                           07NOV00.1272
            DZ=-.5*Z(I,K-1)+.5*Z(I,K+1)                                          07NOV00.1273
            DZ1=Z(I,K)-Z(I,K-1)                                                  07NOV00.1274
            GAMMA=(XL/CP)*(XL/(RV*(T(I,K)**2)))*QES(I,K)                         07NOV00.1275
            GAMMA0=(XL/CP)*(XL/(RV*(T(I,K-1)**2)))*QES(I,K-1)                    07NOV00.1276
            QRCH=QES(I,K)+(1./XL)*(GAMMA/(1.+GAMMA))*DBY(I,K)                    07NOV00.1277
C         QC(I,K)=(QCK(I)-QRCH)/(1.+C0*DZ)+QRCH                                  07NOV00.1278
C         PW(I,K)=C0*DZ*(QC(I,K)-QRCH)                                           07NOV00.1279
C         QCK(I)=QC(I,K)                                                         07NOV00.1280
            QC(I,K)=QCK(I)                                                       07NOV00.1281
            PW(I,K)=QCK(I)-QRCH                                                  07NOV00.1282
            QCK(I)=QRCH                                                          07NOV00.1283
            PWAV(I)=PWAV(I)+PW(I,K)                                              07NOV00.1284
            DZ1=Z(I,K)-Z(I,K-1)                                                  07NOV00.1285
            AA0(I)=AA0(I)+DZ1*(9.81/(1004.*(.5*(T(I,K)+T(I,K-1)))))*             07NOV00.1286
     +           DBY(I,K-1)/(1.+.5*GAMMA+.5*GAMMA0)                              07NOV00.1287
            DZO=-.5*ZO(I,K-1)+.5*ZO(I,K+1)                                       07NOV00.1288
            DZ2=ZO(I,K)-ZO(I,K-1)                                                07NOV00.1289
            GAMMO=(XL/CP)*(XL/(RV*(TN(I,K)**2)))*QESO(I,K)                       07NOV00.1290
            GAMMO0=(XL/CP)*(XL/(RV*(TN(I,K-1)**2)))*QESO(I,K-1)                  07NOV00.1291
            QRCHO=QESO(I,K)+(1./XL)*(GAMMO/(1.+GAMMO))*DBYO(I,K)                 07NOV00.1292
C         QCO(I,K)=(QCKO(I)-QRCHO)/(1.+C0*DZO)+QRCHO                             07NOV00.1293
C         PWO(I,K)=C0*DZO*(QCO(I,K)-QRCHO)                                       07NOV00.1294
C         QCKO(I)=QCO(I,K)                                                       07NOV00.1295
            QCO(I,K)=QCKO(I)                                                     07NOV00.1296
            PWO(I,K)=QCKO(I)-QRCHO                                               07NOV00.1297
            QCKO(I)=QRCHO                                                        07NOV00.1298
            PWAVO(I)=PWAVO(I)+PWO(I,K)                                           07NOV00.1299
            AA1(I)=AA1(I)+DZ2*(9.81/(1004.*(.5*(TN(I,K)+TN(I,K-1)))))*           07NOV00.1300
     +           DBYO(I,K-1)/(1.+.5*GAMMO+.5*GAMMO0)                             07NOV00.1301
          ENDIF                                                                  07NOV00.1302
         ENDIF                                                                   07NOV00.1303
   55   CONTINUE                                                                 07NOV00.1304
                                                                                 CUP.321
C                                                                                CUP.322
C                                                                                CUP.323
      DO 50 I=IST,IEN                                                            07NOV00.1305
        IF(AA0(I).EQ.-1.)GOTO 50                                                 CUP.325
        K=KTOP(I)                                                                CUP.326
C       DZ=-.5*Z(I,K-1)+.5*Z(I,K)                                                CUP.327
C       GAMMA=(XL/CP)*(XL/(RV*(T(I,K)**2)))*QES(I,K)                             CUP.328
C       QRCH=QES(I,K)+(1./XL)*(GAMMA/(1.+GAMMA))*DBY(I,K)                        CUP.329
C       QC(I,K)=QES(I,K)                                                         CUP.330
C       PW(I,K)=(QRCH-QES(I,K))                                                  CUP.331
        PW(I,K)=(QCK(I)-QES(I,K))                                                CUP.332
        PWAV(I)=PWAV(I)+PW(I,K)                                                  CUP.333
C                                                                                CUP.334
C       DZ=-.5*ZO(I,K-1)+.5*ZO(I,K)                                              CUP.335
C       GAMMA=(XL/CP)*(XL/(RV*(TN(I,K)**2)))*QESO(I,K)                           CUP.336
C       QRCHO=QESO(I,K)+(1./XL)*(GAMMA/(1.+GAMMA))*DBYO(I,K)                     CUP.337
C       QCO(I,K)=QESO(I,K)                                                       CUP.338
C       PWO(I,K)=(QRCHO-QESO(I,K))                                               CUP.339
        PWO(I,K)=(QCKO(I)-QESO(I,K))                                             CUP.340
        PWAVO(I)=PWAVO(I)+PWO(I,K)                                               CUP.341
   50 CONTINUE                                                                   CUP.342
C                                                                                CUP.343
C------- DOWNDRAFT CALCULATIONS                                                  CUP.344
C                                                                                CUP.345
C                                                                                CUP.346
C--- DETERMINE DOWNDRAFT STRENGTH IN TERMS OF WINDSHEAR                          CUP.347
C                                                                                CUP.348
      DO KK=1,MKX-1                                                              CUP.349
        DO 61 I=IST,IEN                                                          07NOV00.1306
          IF(AA0(I).EQ.-1.)GOTO 61                                               CUP.351
          IF(KK.LE.MIN0(KTOP(I),MKX-1).AND.KK.GE.KBCON(I))THEN                   CUP.352
            VWS(I)=VWS(I)+(                                                      CUP.353
     +             ABS((US(I,KK+1)-US(I,KK))/(Z(I,KK+1)-Z(I,KK)))+               CUP.354
     +             ABS((VS(I,KK+1)-VS(I,KK))/(Z(I,KK+1)-Z(I,KK))))*(             CUP.355
     +             PO(I,KK)-PO(I,KK+1))                                          CUP.356
            SDP(I)=SDP(I)+PO(I,KK)-PO(I,KK+1)                                    CUP.357
          ENDIF                                                                  CUP.358
          IF(KK.EQ.MKX-1)VSHEAR(I)=1.E3*VWS(I)/SDP(I)                            CUP.359
   61   CONTINUE                                                                 CUP.360
      ENDDO                                                                      CUP.361
      DO 72 I=IST,IEN                                                            07NOV00.1307
        IF(AA0(I).EQ.-1.)GOTO 72                                                 CUP.363
        EDT(I)=1.-(1.591-.639*VSHEAR(I)+.0953*(VSHEAR(I)**2)-.00496*(            CUP.364
     +         VSHEAR(I)**3))                                                    CUP.365
        IF(EDT(I).GT.EDTSMX)EDT(I)=EDTSMX                                        CUP.366
        IF(EDT(I).LT.EDTSMN)EDT(I)=EDTSMN                                        CUP.367
        EDTO(I)=EDT(I)                                                           CUP.368
        EDTX(I)=EDT(I)                                                           CUP.369
        QRCD(I,MKX)=QES(I,MKX)                                                   CUP.370
        HCD(I)=.5*(HE(I,JMIN(I))+HE(I,JMIN(I)+1))                                CUP.371
        QCD(I)=.5*(Q(I,JMIN(I))+Q(I,JMIN(I)+1))                                  CUP.372
        QRCDO(I,MKX)=QESO(I,MKX)                                                 CUP.373
        HCDO(I)=HESO(I,MKX)                                                      CUP.374
        HCDO(I)=.5*(HEO(I,JMIN(I))+HEO(I,JMIN(I)+1))                             CUP.375
        QCDO(I)=.5*(QO(I,JMIN(I))+QO(I,JMIN(I)+1))                               CUP.376
        BU(I)=0.                                                                 CUP.377
        BUO(I)=0.                                                                CUP.378
   72 CONTINUE                                                                   CUP.379
      DO 95 K=1,MKX-1                                                            CUP.380
        DO 95 I=IST,IEN                                                          07NOV00.1308
          IF(AA0(I).EQ.-1.)GOTO 95                                               CUP.382
          IF(K.GE.JMIN(I))GOTO 95                                                CUP.383
          KK=JMIN(I)-K                                                           CUP.384
          DZ=-(Z(I,KK)-Z(I,KK+2))*.5                                             CUP.385
          BU(I)=BU(I)+DZ*(HCD(I)-.5*(HES(I,KK)+HES(I,KK+1)))                     CUP.386
          DQ=(QES(I,KK)+QES(I,KK+1))*.5                                          CUP.387
          DT=(T(I,KK)+T(I,KK+1))*.5                                              CUP.388
          GAMMA=(XL/CP)*(XL/(RV*(DT**2)))*DQ                                     CUP.389
          DH=HCD(I)-.5*(HES(I,KK)+HES(I,KK+1))                                   CUP.390
          QRCD(I,KK)=(DQ+(1./XL)*(GAMMA/(1.+GAMMA))*DH)                          CUP.391
          PWD(I,KK)=DKK(I,KK)*(QCD(I)-QRCD(I,KK))                                CUP.392
          QCD(I)=QRCD(I,KK)                                                      CUP.393
          PWEV(I)=PWEV(I)+PWD(I,KK)                                              CUP.394
C                                                                                CUP.395
C                                                                                CUP.396
C                                                                                CUP.397
          DZ=-(ZO(I,KK)-ZO(I,KK+2))*.5                                           CUP.398
          BUO(I)=BUO(I)+DZ*(HCDO(I)-.5*(HESO(I,KK)+HESO(I,KK+1)))                CUP.399
          DQ=(QESO(I,KK)+QESO(I,KK+1))*.5                                        CUP.400
          DT=(TN(I,KK)+TN(I,KK+1))*.5                                            CUP.401
          GAMMA=(XL/CP)*(XL/(RV*(DT**2)))*DQ                                     CUP.402
          DH=HCDO(I)-.5*(HESO(I,KK)+HESO(I,KK+1))                                CUP.403
          QRCDO(I,KK)=(DQ+(1./XL)*(GAMMA/(1.+GAMMA))*DH)                         CUP.404
          PWDO(I,KK)=DKK(I,KK)*(QCDO(I)-QRCDO(I,KK))                             CUP.405
          QCDO(I)=QRCDO(I,KK)                                                    CUP.406
          PWEVO(I)=PWEVO(I)+PWDO(I,KK)                                           CUP.407
C                                                                                CUP.408
   95   CONTINUE                                                                 CUP.409
                                                                                 CUP.410
C                                                                                CUP.411
C                                                                                CUP.412
      DO 96 I=IST,IEN                                                            07NOV00.1309
        IF(AA0(I).EQ.-1.)GOTO 96                                                 CUP.414
        IF(BU(I).GE.0.OR.BUO(I).GE.0.OR.PWEV(I).GE.0.OR.PWEVO(I).GE.0.)          CUP.415
     +    AA0(I)=-1.                                                             CUP.416
        EDT(I)=-EDT(I)*PWAV(I)/PWEV(I)                                           CUP.417
C       IF(EDT(I).GT.1.0)EDT(I)=1.                                               CUP.418
        IF(EDT(I).GT.EDTMAX)EDT(I)=EDTMAX                                        CUP.419
        IF(EDT(I).LT.EDTMIN)EDT(I)=EDTMIN                                        CUP.420
        EDTO(I)=-EDTO(I)*PWAVO(I)/PWEVO(I)                                       CUP.421
C       IF(EDTO(I).GT.1.0)EDTO(I)=1.                                             CUP.422
        IF(EDTO(I).GT.EDTMAX)EDTO(I)=EDTMAX                                      CUP.423
        IF(EDTO(I).LT.EDTMIN)EDTO(I)=EDTMIN                                      CUP.424
   96 CONTINUE                                                                   CUP.425
C--- WHAT WOULD THE CHANGE BE?                                                   CUP.426
C                                                                                CUP.427
      DO 98 I=IST,IEN                                                            07NOV00.1310
        IF(AA0(I).EQ.-1.)GOTO 98                                                 CUP.429
        K=1                                                                      CUP.430
        DZ=.5*(Z(I,2)-Z(I,1))                                                    CUP.431
        DP=50.*(PSUR(I)-P(I,2))                                                  CUP.432
        DELLAH(I,1)=EDT(I)*(DKK(I,2)*HCD(I)-DKK(I,2)*.5*(HE(I,1)+HE(I,2)         CUP.433
     +              ))*G/DP                                                      CUP.434
        DELLAQ(I,1)=EDT(I)*(DKK(I,2)*QRCD(I,1)-DKK(I,2)*.5*(Q(I,1)+              CUP.435
     +              Q(I,2)))*G/DP                                                CUP.436
        XHE(I,K)=DELLAH(I,K)*MBDT+HE(I,K)                                        CUP.437
        XQ(I,K)=DELLAQ(I,K)*MBDT+Q(I,K)                                          CUP.438
        DELLAT(I,K)=(1./1004.)*(DELLAH(I,K)-2.5E06*DELLAQ(I,K))                  CUP.439
        XT(I,K)=(MBDT/1004.)*(DELLAH(I,K)-2.5E06*DELLAQ(I,K))+T(I,K)             CUP.440
        IF(XQ(I,K).LE.0.)THEN                                                    CUP.441
          XQ(I,K)=1.E-08                                                         CUP.442
        ENDIF                                                                    CUP.443
   98 CONTINUE                                                                   CUP.444
C                                                                                CUP.445
C                                                                                CUP.446
C                                                                                CUP.447
      DO 100 K=1,MKX-1                                                           CUP.448
        DO 100 I=IST,IEN                                                         07NOV00.1311
          IF(AA0(I).EQ.-1.)GOTO 100                                              CUP.450
          IF(K.EQ.1.OR.K.GE.KTOP(I))GOTO 100                                     CUP.451
          DV1=.5*(HE(I,K)+HE(I,K+1))                                             CUP.452
          DV2=HE(I,K)                                                            CUP.453
          DV3=.5*(HE(I,K)+HE(I,K-1))                                             CUP.454
          DV1Q=.5*(Q(I,K)+Q(I,K+1))                                              CUP.455
          DV2Q=Q(I,K)                                                            CUP.456
          DV3Q=.5*(Q(I,K)+Q(I,K-1))                                              CUP.457
C                                                                                CUP.458
C--- SPECIFY DETRAINMENT OF DOWNDRAFT, HAS TO BE CONSISTENT                      CUP.459
C--- WITH ZD CALCULATIONS IN SOUNDD.                                             CUP.460
C                                                                                CUP.461
          DETDO=(DKK(I,K+1)-DKK(I,K))*(HCD(I)-DV2)                               CUP.462
          DETDOQ=(DKK(I,K+1)-DKK(I,K))*(QRCD(I,K)-DV2Q)                          CUP.463
          DZ=.5*(Z(I,K+1)-Z(I,K-1))                                              CUP.464
C                                                                                CUP.465
C--- CHANGED DUE TO SUBSIDENCE AND ENTRAINMENT                                   CUP.466
C                                                                                CUP.467
          AUP=1.                                                                 CUP.468
          IF(K.LE.K22(I))AUP=0.                                                  CUP.469
          ADW=1                                                                  CUP.470
          IF(K.GT.JMIN(I))ADW=0.                                                 CUP.471
          DP=1./(50.*(P(I,K-1)-P(I,K+1)))                                        CUP.472
          DELLAH(I,K)=((AUP-ADW*DKK(I,K+1)*EDT(I))*(DV1-DV2)+(AUP-ADW*           CUP.473
     +                DKK(I,K)*EDT(I))*(DV2-DV3))*G*DP+ADW*EDT(I)*DETDO*         CUP.474
     +                G*DP                                                       CUP.475
          DELLAQ(I,K)=((AUP-ADW*DKK(I,K+1)*EDT(I))*(DV1Q-DV2Q)+(AUP-ADW*         CUP.476
     +                DKK(I,K)*EDT(I))*(DV2Q-DV3Q))*G*DP+ADW*EDT(I)*             CUP.477
     +                DETDOQ*G*DP                                                CUP.478
C        DP=+50.*(P(I,K-1)-P(I,K+1))                                             CUP.479
C        DELLAH(I,K)=((AUP-ADW*EDT(I))*(DV1-DV2)                                 CUP.480
C    *            +(AUP-ADW*EDT(I))*(DV2-DV3))*G/DP                              CUP.481
C    2    +ADW*EDT(I)*DETDO*G/DP                                                 CUP.482
C        DELLAQ(I,K)=((AUP-ADW*EDT(I))*(DV1Q-DV2Q)                               CUP.483
C    *             +(AUP-ADW*EDT(I))*(DV2Q-DV3Q))*G/DP                           CUP.484
C    2    +ADW*EDT(I)*DETDOQ*G/DP                                                CUP.485
          XHE(I,K)=DELLAH(I,K)*MBDT+HE(I,K)                                      CUP.486
          XQ(I,K)=DELLAQ(I,K)*MBDT+Q(I,K)                                        CUP.487
          DELLAT(I,K)=(1./1004.)*(DELLAH(I,K)-2.5E06*DELLAQ(I,K))                CUP.488
          XT(I,K)=(MBDT/1004.)*(DELLAH(I,K)-2.5E06*DELLAQ(I,K))+T(I,K)           CUP.489
          IF(XQ(I,K).LE.0.)THEN                                                  CUP.490
            XQ(I,K)=1.E-08                                                       CUP.491
          ENDIF                                                                  CUP.492
  100   CONTINUE                                                                 CUP.493
                                                                                 CUP.494
C                                                                                CUP.495
C------- CLOUD TOP                                                               CUP.496
C                                                                                CUP.497
      DO 101 I=IST,IEN                                                           07NOV00.1312
        IF(AA0(I).EQ.-1.)GOTO 101                                                CUP.499
        K=KTOP(I)                                                                CUP.500
        DP=100.*(P(I,K-1)-P(I,K))                                                CUP.501
        DV1=.5*(HE(I,K)+HE(I,K-1))                                               CUP.502
        DELLAH(I,K)=(HKB(I)-DV1)*G/DP                                            CUP.503
        DV1=.5*(Q(I,K)+Q(I,K-1))                                                 CUP.504
        DELLAQ(I,K)=(QES(I,K)-DV1)*G/DP                                          CUP.505
  101 CONTINUE                                                                   CUP.506
      DO 102 I=IST,IEN                                                           07NOV00.1313
        IF(AA0(I).EQ.-1.)GOTO 102                                                CUP.508
        K=KTOP(I)                                                                CUP.509
        XHE(I,K)=DELLAH(I,K)*MBDT+HE(I,K)                                        CUP.510
        XQ(I,K)=DELLAQ(I,K)*MBDT+Q(I,K)                                          CUP.511
        DELLAT(I,K)=(1./1004.)*(DELLAH(I,K)-2.5E06*DELLAQ(I,K))                  CUP.512
        XT(I,K)=(MBDT/1004.)*(DELLAH(I,K)-2.5E06*DELLAQ(I,K))+T(I,K)             CUP.513
        IF(XQ(I,K).LE.0.)THEN                                                    CUP.514
          XQ(I,K)=1.E-08                                                         CUP.515
        ENDIF                                                                    CUP.516
        XHKB(I)=DELLAH(I,KBCON(I))*MBDT+HKB(I)                                   CUP.517
        XQKB(I)=DELLAQ(I,KBCON(I))*MBDT+QKB(I)                                   CUP.518
        IF(XQKB(I).LE.0.)XQKB(I)=1.E-08                                          CUP.519
  102 CONTINUE                                                                   CUP.520
C                                                                                CUP.521
C------- FINAL CHANGED VARIABLE PER UNIT MASS FLUX                               CUP.522
C                                                                                CUP.523
C     DO 110 K=1,MKX                                                             CUP.524
C     DO 110 I=IST,IEN                                                           07NOV00.1314
C        IF(AA0(I).EQ.-1.)GO TO 110                                              CUP.526
C     IF(K.GT.KTOP(I))GO TO 110                                                  CUP.527
C     XHE(I,K)=DELLAH(I,K)*MBDT+HE(I,K)                                          CUP.528
C     XQ(I,K)=DELLAQ(I,K)*MBDT+Q(I,K)                                            CUP.529
C     DELLAT(I,K)=(1./1004.)*(DELLAH(I,K)-2.5E06*DELLAQ(I,K))                    CUP.530
C     XT(I,K)=(MBDT/1004.)*(DELLAH(I,K)-2.5E06*DELLAQ(I,K))+T(I,K)               CUP.531
C     IF(XQ(I,K).LE.0.)THEN                                                      CUP.532
C       XQ(I,K)=1.E-08                                                           CUP.533
C     ENDIF                                                                      CUP.534
C110  CONTINUE                                                                   CUP.535
C     DO 112 I=IST,IEN                                                           07NOV00.1315
C        IF(AA0(I).EQ.-1.)GO TO 112                                              CUP.537
C     XHKB(I)=DELLAH(I,KBCON(I))*MBDT+HKB(I)                                     CUP.538
C     XQKB(I)=DELLAQ(I,KBCON(I))*MBDT+QKB(I)                                     CUP.539
C     IF(XQKB(I).LE.0.)XQKB(I)=1.E-08                                            CUP.540
C112  CONTINUE                                                                   CUP.541
C                                                                                CUP.542
C--- ENVIRONMENTAL CONDITIONS, FIRST HEIGHTS                                     CUP.543
C                                                                                CUP.544
      DO 115 K=1,MKX                                                             CUP.545
        DO 115 I=IST,IEN                                                         07NOV00.1316
          IF(AA0(I).EQ.-1.)GOTO 115                                              CUP.547
          IPH=1                                                                  CUP.548
          IF(XT(I,K).LE.TCRIT)IPH=2                                              CUP.549
          E=EXP(AE(IPH)-BE(IPH)/XT(I,K))                                         CUP.550
          XQES(I,K)=.622*E/(100.*P(I,K)-E)                                       25JAN00.473
          IF(XQES(I,K).LE.1.E-08)XQES(I,K)=1.E-08                                CUP.552
          IF(XQ(I,K).GT.XQES(I,K))XQ(I,K)=XQES(I,K)                              CUP.553
          XTV(I,K)=XT(I,K)+.608*XQ(I,K)*XT(I,K)                                  CUP.554
  115   CONTINUE                                                                 CUP.555
                                                                                 CUP.556
      DO 116 I=IST,IEN                                                           07NOV00.1317
        IF(AA0(I).EQ.-1.)GOTO 116                                                CUP.558
#ifndef IBMopt                                                                   23DEC04.3110
        XZ(I,1)=Z1(I)-(ALOG(P(I,1))-ALOG(PSUR(I)))*RD*XTV(I,1)/9.81              CUP.559
#else                                                                            23DEC04.3111
        XZ(I,1)=Z1(I)-(plog(I,1)-psurlog(I))*RD*XTV(I,1)/9.81                    23DEC04.3112
#endif                                                                           23DEC04.3113
  116 CONTINUE                                                                   CUP.560
      DO 120 K=2,MKX                                                             CUP.561
        DO 120 I=IST,IEN                                                         07NOV00.1318
          IF(AA0(I).EQ.-1.)GOTO 120                                              CUP.563
          TVBAR=.5*XTV(I,K)+.5*XTV(I,K-1)                                        CUP.564
#ifndef IBMopt                                                                   23DEC04.3114
          XZ(I,K)=XZ(I,K-1)-(ALOG(P(I,K))-ALOG(P(I,K-1)))*RD*TVBAR/9.81          CUP.565
#else                                                                            23DEC04.3115
          XZ(I,K)=XZ(I,K-1)-(plog(I,K)-plog(I,K-1))*RD*TVBAR/9.81                23DEC04.3116
#endif                                                                           23DEC04.3117
  120   CONTINUE                                                                 CUP.566
                                                                                 CUP.567
C                                                                                CUP.568
C--- MOIST STATIC ENERGY                                                         CUP.569
C                                                                                CUP.570
      DO 130 K=1,MKX                                                             CUP.571
        DO 130 I=IST,IEN                                                         07NOV00.1319
          IF(AA0(I).EQ.-1.)GOTO 130                                              CUP.573
          XHES(I,K)=9.81*XZ(I,K)+1004.*XT(I,K)+2.5E06*XQES(I,K)                  CUP.574
          IF(XHE(I,K).GE.XHES(I,K))XHE(I,K)=XHES(I,K)                            CUP.575
  130   CONTINUE                                                                 CUP.576
                                                                                 CUP.577
C                                                                                CUP.578
C                                                                                CUP.579
C**************************** STATIC CONTROL                                     CUP.580
C                                                                                CUP.581
      DO 132 I=IST,IEN                                                           07NOV00.1320
        IF(AA0(I).EQ.-1.)GOTO 132                                                CUP.583
        XQCK(I)=XQKB(I)                                                          CUP.584
        XDBY(I,MKX)=XHKB(I)-XHES(I,MKX)                                          CUP.585
  132 CONTINUE                                                                   CUP.586
C        DO 135 K=1,MKX-1                                                        CUP.587
C        DO 135 I=IST,IEN                                                        07NOV00.1321
C        IF(AA0(I).EQ.-1.)GO TO 135                                              CUP.589
C        XDBY(I,K)=XHKB(I)-.5*(XHES(I,K)+XHES(I,K+1))                            CUP.590
C135     CONTINUE                                                                CUP.591
C                                                                                CUP.592
C------- MOISTURE AND CLOUD WORK FUNCTIONS                                       CUP.593
C                                                                                CUP.594
      DO 140 K=1,MKX-1                                                           CUP.595
        DO 140 I=IST,IEN                                                         07NOV00.1322
          IF(AA0(I).LT.0.)GOTO 140                                               CUP.597
          XDBY(I,K)=XHKB(I)-.5*(XHES(I,K)+XHES(I,K+1))                           CUP.598
          IF(K.LE.KBCON(I).OR.K.GE.KTOP(I))GOTO 140                              CUP.599
          DZ=-.5*XZ(I,K-1)+.5*XZ(I,K+1)                                          CUP.600
          DZ1=XZ(I,K)-XZ(I,K-1)                                                  CUP.601
          GAMMA=(XL/CP)*(XL/(RV*(XT(I,K)**2)))*XQES(I,K)                         CUP.602
          GAMMA0=(XL/CP)*(XL/(RV*(XT(I,K-1)**2)))*XQES(I,K-1)                    CUP.603
          QRCH=XQES(I,K)+(1./XL)*(GAMMA/(1.+GAMMA))*XDBY(I,K)                    CUP.604
C         XQC(I,K)=(XQCK(I)-QRCH)/(1.+C0*DZ)+QRCH                                CUP.605
C         XPW(I,K)=C0*DZ*(XQC(I,K)-QRCH)                                         CUP.606
C         XQCK(I)=XQC(I,K)                                                       CUP.607
          XQC(I,K)=XQCK(I)                                                       CUP.608
          XPW(I,K)=XQCK(I)-QRCH                                                  CUP.609
          XQCK(I)=QRCH                                                           CUP.610
          XPWAV(I)=XPWAV(I)+XPW(I,K)                                             CUP.611
          XAA0(I)=XAA0(I)+DZ1*(9.81/(1004.*(.5*(XT(I,K)+XT(I,K-1)))))*           CUP.612
     +            XDBY(I,K-1)/(1.+.5*GAMMA+.5*GAMMA0)                            CUP.613
  140   CONTINUE                                                                 CUP.614
                                                                                 CUP.615
      DO 142 I=IST,IEN                                                           07NOV00.1323
        IF(AA0(I).LT.0.)GOTO 142                                                 CUP.617
        K=KTOP(I)                                                                CUP.618
C       DZ=-.5*XZ(I,K-1)+.5*XZ(I,K)                                              CUP.619
C       GAMMA=(XL/CP)*(XL/(RV*(XT(I,K)**2)))*XQES(I,K)                           CUP.620
C       QRCH=XQES(I,K)+(1./XL)*(GAMMA/(1.+GAMMA))*XDBY(I,K)                      CUP.621
C       XQC(I,K)=XQES(I,K)                                                       CUP.622
C       XPW(I,K)=(QRCH-XQES(I,K))                                                CUP.623
        XPW(I,K)=(XQCK(I)-XQES(I,K))                                             CUP.624
        XPWAV(I)=XPWAV(I)+XPW(I,K)                                               CUP.625
C       XQRCD(I,MKX)=XQES(I,MKX)                                                 CUP.626
        XHCD(I)=.5*(XHE(I,JMIN(I))+XHE(I,JMIN(I)+1))                             CUP.627
        XQCD(I)=.5*(XQ(I,JMIN(I))+XQ(I,JMIN(I)+1))                               CUP.628
        XPWEV(I)=0.                                                              CUP.629
        BU(I)=0.                                                                 CUP.630
  142 CONTINUE                                                                   CUP.631
C                                                                                CUP.632
C------- DOWNDRAFT CALCULATIONS                                                  CUP.633
C                                                                                CUP.634
C                                                                                CUP.635
C--- DOWNDRAFT MOISTURE PROPERTIES                                               CUP.636
C                                                                                CUP.637
      DO 160 K=1,MKX-1                                                           CUP.638
        DO 160 I=IST,IEN                                                         07NOV00.1324
          IF(AA0(I).LT.0.)GOTO 160                                               CUP.640
          IF(K.GE.JMIN(I))GOTO 160                                               CUP.641
          KK=JMIN(I)-K                                                           CUP.642
          DZ=-(XZ(I,KK)-XZ(I,KK+2))*.5                                           CUP.643
          BU(I)=BU(I)+DZ*(XHCD(I)-.5*(XHES(I,KK)+XHES(I,KK+1)))                  CUP.644
          DQ=(XQES(I,KK)+XQES(I,KK+1))*.5                                        CUP.645
          DT=(XT(I,KK)+XT(I,KK+1))*.5                                            CUP.646
          GAMMA=(XL/CP)*(XL/(RV*(DT**2)))*DQ                                     CUP.647
          DH=XHCD(I)-.5*(XHES(I,KK)+XHES(I,KK+1))                                CUP.648
          XQRCD(I,KK)=(DQ+(1./XL)*(GAMMA/(1.+GAMMA))*DH)                         CUP.649
          XPWD(I,KK)=DKK(I,KK)*(XQCD(I)-XQRCD(I,KK))                             CUP.650
          XQCD(I)=XQRCD(I,KK)                                                    CUP.651
          XPWEV(I)=XPWEV(I)+XPWD(I,KK)                                           CUP.652
  160   CONTINUE                                                                 CUP.653
                                                                                 CUP.654
      DO 162 I=IST,IEN                                                           07NOV00.1325
        IF(AA0(I).LT.0.)GOTO 162                                                 CUP.656
        IF(BU(I).GE.0.)THEN                                                      CUP.657
          AA0(I)=-1.                                                             CUP.658
          GOTO 162                                                               CUP.659
        ENDIF                                                                    CUP.660
        IF(XPWEV(I).NE.0.)EDTX(I)=-EDTX(I)*XPWAV(I)/XPWEV(I)                     CUP.661
C       IF(EDTX(I).GT.1.0)EDTX(I)=1.                                             CUP.662
        IF(EDTX(I).GT.EDTMAX)EDTX(I)=EDTMAX                                      CUP.663
        IF(EDTX(I).LT.EDTMIN)EDTX(I)=EDTMIN                                      CUP.664
  162 CONTINUE                                                                   CUP.665
C                                                                                CUP.666
C                                                                                CUP.667
C--- DOWNDRAFT CLOUDWORK FUNCTIONS                                               CUP.668
C                                                                                CUP.669
C                                                                                CUP.670
      DO 171 K=1,MKX-1                                                           CUP.671
        DO 171 I=IST,IEN                                                         07NOV00.1326
          IF(AA0(I).LT.0.)GOTO 171                                               CUP.673
          IF(K.GE.JMIN(I))GOTO 171                                               CUP.674
          KK=JMIN(I)-K                                                           CUP.675
C                                                                                CUP.676
C--- ORIGINAL                                                                    CUP.677
C                                                                                CUP.678
          GAMMA1=(XL/CP)*(XL/(RV*(T(I,KK)**2)))*QES(I,KK)                        CUP.679
          GAMMA2=(XL/CP)*(XL/(RV*(T(I,KK+1)**2)))*QES(I,KK+1)                    CUP.680
          DHH=HCD(I)                                                             CUP.681
          DT=.5*(T(I,KK)+T(I,KK+1))                                              CUP.682
          DG=.5*(GAMMA1+GAMMA2)                                                  CUP.683
          DH=.5*(HES(I,KK)+HES(I,KK+1))                                          CUP.684
          DZ=(Z(I,KK)-Z(I,KK+1))*DKK(I,KK)                                       CUP.685
          XDDD=DHH-DH                                                            CUP.686
          AA0(I)=AA0(I)+EDT(I)*DZ*(G/(CP*DT))*((DHH-DH)/(1.+DG))                 CUP.687
C                                                                                CUP.688
C--- MODIFIED BY LARGER SCALE                                                    CUP.689
C                                                                                CUP.690
          GAMMA1=(XL/CP)*(XL/(RV*(TN(I,KK)**2)))*QESO(I,KK)                      CUP.691
          GAMMA2=(XL/CP)*(XL/(RV*(TN(I,KK+1)**2)))*QESO(I,KK+1)                  CUP.692
          DHH=HCDO(I)                                                            CUP.693
          DT=.5*(TN(I,KK)+TN(I,KK+1))                                            CUP.694
          DG=.5*(GAMMA1+GAMMA2)                                                  CUP.695
          DH=.5*(HESO(I,KK)+HESO(I,KK+1))                                        CUP.696
          DZ=(ZO(I,KK)-ZO(I,KK+1))*DKK(I,KK)                                     CUP.697
          AA1(I)=AA1(I)+EDTO(I)*DZ*(G/(CP*DT))*((DHH-DH)/(1.+DG))                CUP.698
C                                                                                CUP.699
C--- MODIFIED BY CLOUD                                                           CUP.700
C                                                                                CUP.701
          GAMMA1=(XL/CP)*(XL/(RV*(XT(I,KK)**2)))*XQES(I,KK)                      CUP.702
          GAMMA2=(XL/CP)*(XL/(RV*(XT(I,KK+1)**2)))*XQES(I,KK+1)                  CUP.703
          DHH=XHCD(I)                                                            CUP.704
          DT=.5*(XT(I,KK)+XT(I,KK+1))                                            CUP.705
          DG=.5*(GAMMA1+GAMMA2)                                                  CUP.706
          DH=.5*(XHES(I,KK)+XHES(I,KK+1))                                        CUP.707
          DZ=(XZ(I,KK)-XZ(I,KK+1))*DKK(I,KK)                                     CUP.708
          XAA0(I)=XAA0(I)+EDTX(I)*DZ*(G/(CP*DT))*((DHH-DH)/(1.+DG))              CUP.709
  171   CONTINUE                                                                 CUP.710
                                                                                 CUP.711
C                                                                                CUP.712
C--- LARGE SCALE FORCING                                                         CUP.713
C                                                                                CUP.714
      DO 175 I=IST,IEN                                                           07NOV00.1327
        IF(AA0(I).LT.0.)GOTO 175                                                 CUP.716
#ifndef IBMopt                                                                   23DEC04.3118
        F=(AA1(I)-AA0(I))/DTIME                                                  CUP.717
        XK=(XAA0(I)-AA0(I))/MBDT                                                 CUP.718
#else                                                                            23DEC04.3119
        F=(AA1(I)-AA0(I))                                                        23DEC04.3120
        XK=(XAA0(I)-AA0(I))                                                      23DEC04.3121
#endif                                                                           23DEC04.3122
        IF(F.LE.0.OR.XK.GE.0.)THEN                                               CUP.719
          XMB(I)=0.                                                              CUP.720
        ELSE                                                                     CUP.721
#ifndef IBMopt                                                                   23DEC04.3123
          XMB(I)=-F/XK                                                           CUP.722
#else                                                                            23DEC04.3124
          XMB(I)=-F/XK * 5.e-03                                                  23DEC04.3125
#endif                                                                           23DEC04.3126
        ENDIF                                                                    CUP.723
  175 CONTINUE                                                                   CUP.724
C                                                                                CUP.725
C--- FEEDBACK                                                                    CUP.726
C                                                                                CUP.727
      DO 180 K=1,MKX                                                             CUP.728
        DO 180 I=IST,IEN                                                         07NOV00.1328
          IF(AA0(I).LT.0.)GOTO 180                                               CUP.730
          IF(K.GT.KTOP(I))GOTO 180                                               CUP.731
          OUTTES=DELLAT(I,K)*XMB(I)*86400.                                       CUP.732
          IF((OUTTES.GT.CUQMAX))THEN                                             CUP.733
            XMB(I)=CUQMAX/OUTTES*XMB(I)                                          CUP.734
          ELSEIF(OUTTES.LT.CUQMIN)THEN                                           CUP.735
            XMB(I)=CUQMIN/OUTTES*XMB(I)                                          CUP.736
          ENDIF                                                                  CUP.737
  180   CONTINUE                                                                 CUP.738
                                                                                 CUP.739
      DO 1800 K=1,MKX                                                            CUP.740
        DO 1800 I=IST,IEN                                                        07NOV00.1329
          IF(AA0(I).LT.0.)GOTO 1800                                              CUP.742
          IF(K.GT.KTOP(I))GOTO 1800                                              CUP.743
          OUTTEM(I,K)=OUTTEM(I,K)+DELLAT(I,K)*XMB(I)                             CUP.744
          OUTQ(I,K)=OUTQ(I,K)+DELLAQ(I,K)*XMB(I)                                 CUP.745
          PRE(I)=PRE(I)+(PW(I,K)+EDT(I)*PWD(I,K))*XMB(I)                         CUP.746
 1800   CONTINUE                                                                 CUP.747
                                                                                 CUP.748
      IF(IFRAD.EQ.3.OR.IFRAD.EQ.4)THEN                                           25JAN00.474
        DXTEMC=AMAX1(DX,DSMALC+0.1)                                              CUP.750
        DXTEMC=AMIN1(DXTEMC,DLARGC)                                              CUP.751
        CLFRCV=AFRACL+(AFRACS-AFRACL)*((DLARGC-DXTEMC)/(DLARGC-DSMALC))          CUP.752
     +         **2                                                               CUP.753
C     CLLWCV=0.15E-3                                                             CUP.754
C                                                                                CUP.755
#ifdef IBMopt                                                                    23DEC04.3127
        zz1 = log(1.-CLFRCV)                                                     23DEC04.3128
#endif                                                                           23DEC04.3129
        DO 205 I=IST,IEN                                                         07NOV00.1330
          IF(AA0(I).GE.0.)THEN                                                   CUP.757
            IF(KTOP(I).GT.1.AND.KBCON(I).GT.1)THEN                               CUP.758
              KCLTH=KTOP(I)-KBCON(I)+1                                           CUP.759
              AKCLTH=1./FLOAT(KCLTH)                                             CUP.760
#ifdef IBMopt                                                                    23DEC04.3130
              zzz = exp(AKCLTH*zz1)                                              23DEC04.3131
#endif                                                                           23DEC04.3132
              DO 203 K=KBCON(I),KTOP(I)                                          CUP.761
                KK=MKX-K+1                                                       CUP.762
#ifndef IBMopt                                                                   23DEC04.3133
                CLDFRA(I,KK)=1.-(1.-CLFRCV)**AKCLTH                              CUP.763
#else                                                                            23DEC04.3134
                CLDFRA(I,KK)=1.- zzz                                             23DEC04.3135
#endif                                                                           23DEC04.3136
  203         CONTINUE                                                           CUP.764
            ENDIF                                                                CUP.765
            DO 204 K=1,MKX                                                       CUP.766
              KK=MKX-K+1                                                         CUP.767
              IF(OUTQ(I,K).EQ.0..AND.OUTTEM(I,K).EQ.0)THEN                       CUP.768
                CLDFRA(I,KK)=0.                                                  CUP.769
              ENDIF                                                              CUP.770
  204       CONTINUE                                                             CUP.771
          ENDIF                                                                  CUP.772
  205   CONTINUE                                                                 CUP.773
      ENDIF                                                                      CUP.774
C                                                                                CUP.775
      RETURN                                                                     CUP.776
      END                                                                        CUP.777
                                                                                 CUP.778
