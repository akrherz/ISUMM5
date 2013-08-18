      SUBROUTINE NAVYPB (U3D,V3D,T3D,QV3D,PP3D,PSBD,PSURF,PINP,NN,               NAVYPB.1
     1    INEST,JJ,MKXX,KTAU,DT2,MIXX,MJXX,                                      NAVYPB.2
     1    T3DTEN,QV3DTEN,U3DTEN,V3DTEN,QC3DTEN,QR3DTEN,                          NAVYPB.3
     1    ISFFLX,XTIME,SIGG,PTOP,IYY,JXX)                                        NAVYPB.4
C                                                                                NAVYPB.5
C THE DRIVER ROUTINE FOR THE BURK-THOMPSON PBL SCHEME (BURK AND THOMPSON         NAVYPB.6
C     MWR, NOV. 1989).                                                           NAVYPB.7
C                                                                                NAVYPB.8
C CHANGES:                                                                       NAVYPB.9
C     MODIFY SURFACE VIRTUAL THETA CALCULATION TO NOT DEPEND ON MAVAIL           NAVYPB.10
#     include <parame.incl>                                                      NAVYPB.11
#     include <point2d.incl>                                                     NAVYPB.12
#     include <nhcnst.incl>                                                      NAVYPB.13
      PARAMETER (II=MKX)                                                         NAVYPB.14
      PARAMETER (KDIM=MKX)                                                       NAVYPB.15
C                                                                                NAVYPB.16
C                                                                                NAVYPB.17
      DIMENSION PHI(MIX,MJX,MKX),PSBD(MIX,MJX)                                   NAVYPB.18
#     include <btblk1.incl>                                                      NAVYPB.19
#     include <btblk2.incl>                                                      NAVYPB.20
       DIMENSION PINP(MIX,MJX,MKX)                                               NAVYPB.21
      DIMENSION U3D(MIX,MJX,MKX),V3D(MIX,MJX,MKX),T3D(MIX,MJX,MKX),              NAVYPB.22
     -         QV3D(MIX,MJX,MKX)                                                 NAVYPB.23
      DIMENSION U3DTEN(MIX,MJX,MKX),V3DTEN(MIX,MJX,MKX),                         NAVYPB.24
     -          T3DTEN(MIX,MJX,MKX),QV3DTEN(MIX,MJX,MKX),                        NAVYPB.25
     -          QC3DTEN(MIXM,MJXM,MKXM),QR3DTEN(MIXM,MJXM,MKXM)                  NAVYPB.26
                                                                                 NAVYPB.27
C                                                                                NAVYPB.28
      DIMENSION T99(II),TD99(II),UV99(II),UV9D(II),IH(II)                        NAVYPB.29
     1 ,   IPT(6),JPT(6), IFPT(6),JFPT(6), ITAUFP(4)                             NAVYPB.30
      DIMENSION RFRAC(II), XMDEL(II), ZFT(II)                                    NAVYPB.31
      DIMENSION RHOAU(II),RHOVU(II)                                              NAVYPB.32
      DIMENSION SIGG(MKX)                                                        NAVYPB.33
C----------------------------------------------------------------------          NAVYPB.34
C                                                                                NAVYPB.35
      DIMENSION PSURF(MIX,MJX)                                                   NAVYPB.36
C                                                                                NAVYPB.37
C                                                                                NAVYPB.38
C SET MODEL CONSTANTS                                                            NAVYPB.39
C                                                                                NAVYPB.40
C  ABT=ABSOLUTE TEMP.(KELVIN)                                                    NAVYPB.41
C  ABTH=POTENTIAL TEMP.                                                          NAVYPB.42
C  DT=TIME STEP(SECONDS)                                                         NAVYPB.43
C  II=TOTAL NO. OF GRID PTS.                                                     NAVYPB.44
C  IT=ITERATION NO.(COUNTER)                                                     NAVYPB.45
C  IA,IB,IC,ID,IE,ITOT=ITERATIONS AT WHICH OUTPUTS ARE DESIRED.                  NAVYPB.46
C  P(I)=PRESSURE(MB)                                                             NAVYPB.47
C  QL=LIQUID WATER CONTENT(NONDIMENSIONAL,N.D.)                                  NAVYPB.48
C  QW=TOTAL WATER CONTENT(N.D.)                                                  NAVYPB.49
C  QWZ=SURFACE(SFC) QW VALUE                                                     NAVYPB.50
C  SPECHUM=SPECIFIC HUMIDITY                                                     NAVYPB.51
C  TH=VIRTUAL POTENTIAL TEMP.                                                    NAVYPB.52
C  THL=LIQUID WATER POTENTIAL TEMP.(KELVIN)                                      NAVYPB.53
C  THLZ=SFC VALUE OF THL                                                         NAVYPB.54
C  U=U-COMPONENT OF WIND(X-DIRECTION)                                            NAVYPB.55
C  UG=X-COMPONENT OF GEOSTROPHIC WIND                                            NAVYPB.56
C  V=Y-COMPONENT OF WIND                                                         NAVYPB.57
C  VG=Y-COMP. OF GEOSTROPHIC WIND                                                NAVYPB.58
C  XMZ=SFC XM VALUE                                                              NAVYPB.59
C  Z(I)=GRID PT. HEIGHTS                                                         NAVYPB.60
C  Z1=SFC ROUGHNESS HT.                                                          NAVYPB.61
C  IDELAY=# OF ITERATIONS IN SPIN-UP PERIOD. (THE MODEL                          NAVYPB.62
C         TURBULENCE VARIABLES ARE PERMITTED TO ADJUST TO THE                    NAVYPB.63
C         INITIAL MEAN FIELDS BEFORE THE FORECAST ITSELF BEGINS.)                NAVYPB.64
C  RADJUMP=# OF SECONDS BETWEEN CALLS TO THE RADIATION S/R                       NAVYPB.65
C         IN CLEAR AIR. WHEN THERE IS LIQUID WATER(CLOUDS) THE                   NAVYPB.66
C         RADIATION WILL BE CALLED EVERY TIME STEP.)                             NAVYPB.67
C  SWITCH=0.0, RUN WITHOUT RADIATION                                             NAVYPB.68
C  SWITCH=1.0, RUN WITH RADIATION                                                NAVYPB.69
C  TAUF=LENGTH OF FORECAST, IN HRS                                               NAVYPB.70
C                                                                                NAVYPB.71
C ******* START I LOOP ************************************************          NAVYPB.72
C                                                                                NAVYPB.73
      RAIR=287.04                                                                NAVYPB.74
      CALL SFCRAD(IYY,JXX,JJ,INEST,T3D,QV3D,PP3D)                                NAVYPB.75
      DO IJ=1,IYY-1                                                              NAVYPB.76
        KL=II                                                                    NAVYPB.77
        PS=PSURF(IJ,JJ)                                                          NAVYPB.78
        KLP1=KL+1                                                                NAVYPB.79
        DT=1.*DT2                                                                NAVYPB.80
        TAUF=0.                                                                  NAVYPB.81
        CALL ERASE                                                               NAVYPB.82
        HASSID=0                                                                 NAVYPB.83
        IRADFR=8                                                                 NAVYPB.84
        TDELMAX=50                                                               NAVYPB.85
        SWITCH=0.                                                                NAVYPB.86
        DO 1233 I=1,II                                                           NAVYPB.87
          UTEND(I)=0.                                                            NAVYPB.88
          VTEND(I)=0.                                                            NAVYPB.89
          THTEND(I)=0.                                                           NAVYPB.90
          QTEND(I)=0.                                                            NAVYPB.91
          UF(I)=0.                                                               NAVYPB.92
          VF(I)=0.                                                               NAVYPB.93
 1233   CONTINUE                                                                 NAVYPB.94
        TAUF=0.                                                                  NAVYPB.95
        IDELAY=0                                                                 NAVYPB.96
C                                                                                NAVYPB.97
C  RADJUMP=# OF SECONDS BETWEEN CALLS TO THE RADIATION S/R IN CLEAR AIR          NAVYPB.98
C     RADJUMP=RADFRQ*DT                                                          NAVYPB.99
C                                                                                NAVYPB.100
C  SCHMIDT NUMBER TO ACCOUNT FOR SLIGHT DIFFERENCES IN                           NAVYPB.101
C  TURBULENT DIFFUSIVITY OF HEAT AND MOISTURE.                                   NAVYPB.102
C                                                                                NAVYPB.103
        SCHMIDT=1.0075                                                           NAVYPB.104
C                                                                                NAVYPB.105
        TL1=3600./DT                                                             NAVYPB.106
        IA=IDELAY+1                                                              NAVYPB.107
        IB=TL1*0.5*TAUF+IDELAY                                                   NAVYPB.108
        ITOT=TL1*TAUF+IDELAY                                                     NAVYPB.109
        ITIMOUT=0                                                                NAVYPB.110
        IOUTFRQ=1                                                                NAVYPB.111
        NOUT=ITOT/IOUTFRQ+1                                                      NAVYPB.112
C                                                                                NAVYPB.113
        IM1=II-1                                                                 NAVYPB.114
        IM2=II-2                                                                 NAVYPB.115
        IM3=II-3                                                                 NAVYPB.116
C  Y1= BOUYANCY FACTOR                                                           NAVYPB.117
        Y1=BETA*G                                                                NAVYPB.118
C                                                                                NAVYPB.119
C   CONSTANTS TO BE USED IN PRECIPITATION PARAMETERIZATION.                      NAVYPB.120
C   SEE BARKER(BLM,11,267-294,1977).                                             NAVYPB.121
        CCN=1.0E+7                                                               NAVYPB.122
        RHOW=1.0E+3                                                              NAVYPB.123
        RD=287.                                                                  NAVYPB.124
        CNST=-1.202E+8                                                           NAVYPB.125
        CONV=1.E+2                                                               NAVYPB.126
C                                                                                NAVYPB.127
        CALL INITPB(PSBD,PINP,PHI,GWWG,PS,SST,P,WK,ABT,ZTIME,TRSTORE,            NAVYPB.128
     +       SPECHUM,U,V,TV,PRATIO,THL,ABTH,TH,QW,QL,RC,THLSB,QRL,QRS,           NAVYPB.129
     +       QRN,WQWB,THLQWB,QWSB,TKE,TKESR,RHOAU,RHOVU,X1U,X2U,X2UL,            NAVYPB.130
     +       ENUL,RLAT,RLON,GMT,Z1,IJ,JJ,PTOP,LL,QWZ,KTAU,THLZ,SIGG,U3D,         NAVYPB.131
     +       V3D,T3D,QV3D)                                                       NAVYPB.132
        GW=GWWG                                                                  NAVYPB.133
C                                                                                NAVYPB.134
C-----COMPUTE THE HEIGHT OF FULL- AND HALF-SIGMA LEVELS ABOVE GROUND             NAVYPB.135
C     LEVEL, AND THE LAYER THICKNESSES.                                          NAVYPB.136
C                                                                                NAVYPB.137
        DO K=1,MKX                                                               NAVYPB.138
          KK=MKX-K+1                                                             NAVYPB.139
          Z(KK)=-(RAIR*TLP/(2.*G)*(ALOG(PR0(IJ,JJ,K)/P0))**2+RAIR*TS0/G*         NAVYPB.140
     +          ALOG(PR0(IJ,JJ,K)/P0))-HT(IJ,JJ)/G                               NAVYPB.141
        ENDDO                                                                    NAVYPB.142
C                                                                                NAVYPB.143
        DO 20 I=2,IM1                                                            NAVYPB.144
          ZN(I)=Z(I+1)-Z(I)                                                      NAVYPB.145
          ZS(I)=Z(I)-Z(I-1)                                                      NAVYPB.146
   20   CONTINUE                                                                 NAVYPB.147
        DO 30 I=2,IM1                                                            NAVYPB.148
          C1(I)=ZS(I)/(ZN(I)*(ZN(I)+ZS(I)))                                      NAVYPB.149
          C2(I)=(ZN(I)-ZS(I))/(ZN(I)*ZS(I))                                      NAVYPB.150
          C3(I)=ZN(I)/(ZS(I)*(ZN(I)+ZS(I)))                                      NAVYPB.151
   30   CONTINUE                                                                 NAVYPB.152
C                                                                                NAVYPB.153
C  TL1 = PI/180.                                                                 NAVYPB.154
        TL1=0.0174533                                                            NAVYPB.155
C                                                                                NAVYPB.156
        RADSW=0.0                                                                NAVYPB.157
        TIMER=XTIME*60.                                                          NAVYPB.158
        PSTEND=0.0                                                               NAVYPB.159
        TSTEND=0.0                                                               NAVYPB.160
        TKE(II)=0.0                                                              NAVYPB.161
        TKESR(II)=0.0                                                            NAVYPB.162
        THLSB(II)=0.0                                                            NAVYPB.163
        EKM(II)=0.0                                                              NAVYPB.164
        EKH(II)=0.0                                                              NAVYPB.165
C                                                                                NAVYPB.166
C SET ITERATION COUNTER                                                          NAVYPB.167
        IT=KTAU                                                                  NAVYPB.168
C     IF(INEST.GT.1)IT=KTAU/3                                                    NAVYPB.169
C CALCULATE THE INITIAL M-Y LENGTH SCALES                                        NAVYPB.170
        SUM1=0.5*TKESR(1)*Z(1)                                                   NAVYPB.171
        SUM2=SUM1*Z(1)                                                           NAVYPB.172
        DO 50 I=2,II                                                             NAVYPB.173
          SUM1=SUM1+0.5*(TKESR(I)+TKESR(I-1))*(Z(I)-Z(I-1))                      NAVYPB.174
          SUM2=SUM2+0.5*(TKESR(I)*Z(I)+TKESR(I-1)*Z(I-1))*(Z(I)-Z(I-1))          NAVYPB.175
   50   CONTINUE                                                                 NAVYPB.176
        XLZ=ALF*SUM2/SUM1                                                        NAVYPB.177
        DO 60 I=1,II                                                             NAVYPB.178
          XL(I)=VK*Z(I)/(1.+VK*Z(I)/XLZ)                                         NAVYPB.179
          CLAM2(I)=B2*XL(I)                                                      NAVYPB.180
   60   CONTINUE                                                                 NAVYPB.181
  120   FORMAT (6(/1X,10E11.4))                                                  NAVYPB.182
        CALL BOUND(IJ,JJ,XLAND(IJ,JJ),ISFFLX,HFX,QFX,UST)                        NAVYPB.183
  130   CONTINUE                                                                 NAVYPB.184
C                                                                                NAVYPB.185
C                                                                                NAVYPB.186
C***************************************************                             NAVYPB.187
C CALCULATE IMPORTANT GRADIENTS                                                  NAVYPB.188
        DO 145 I=2,IM1                                                           NAVYPB.189
          DUDZ(I)=C1(I)*U(I+1)+C2(I)*U(I)-C3(I)*U(I-1)                           NAVYPB.190
          DVDZ(I)=C1(I)*V(I+1)+C2(I)*V(I)-C3(I)*V(I-1)                           NAVYPB.191
          DTHLDZ(I)=C1(I)*THL(I+1)+C2(I)*THL(I)-C3(I)*THL(I-1)                   NAVYPB.192
          DTHVDZ(I)=C1(I)*TH(I+1)+C2(I)*TH(I)-C3(I)*TH(I-1)                      NAVYPB.193
          DQWDZ(I)=C1(I)*QW(I+1)+C2(I)*QW(I)-C3(I)*QW(I-1)                       NAVYPB.194
  145   CONTINUE                                                                 NAVYPB.195
C CALCULATE M-Y LENGTH SCALES                                                    NAVYPB.196
        SUM1=0.5*TKESR(1)*Z(1)                                                   NAVYPB.197
        SUM2=0.5*TKESR(1)*Z(1)*Z(1)                                              NAVYPB.198
        DO 150 I=2,II                                                            NAVYPB.199
          SUM1=SUM1+0.5*(TKESR(I)+TKESR(I-1))*(Z(I)-Z(I-1))                      NAVYPB.200
          SUM2=SUM2+0.5*(TKESR(I)*Z(I)+TKESR(I-1)*Z(I-1))*(Z(I)-Z(I-1))          NAVYPB.201
  150   CONTINUE                                                                 NAVYPB.202
        XLZ=ALF*SUM2/SUM1                                                        NAVYPB.203
        DO 160 I=1,II                                                            NAVYPB.204
          XL(I)=VK*Z(I)*XLZ/(XLZ+VK*Z(I))                                        NAVYPB.205
  160   CONTINUE                                                                 NAVYPB.206
        DO 980 I=2,IM1                                                           NAVYPB.207
          IF(DTHVDZ(I).GT.0.0)GOTO 976                                           NAVYPB.208
          GOTO 975                                                               NAVYPB.209
  976     XLD=0.5303*TKESR(I)/SQRT(Y1*DTHVDZ(I))                                 NAVYPB.210
          IF(XLD.LT.XL(I))XL(I)=XLD                                              NAVYPB.211
  975     CONTINUE                                                               NAVYPB.212
          IF(XL(I).LT.5.0)XL(I)=5.0                                              NAVYPB.213
  980   CONTINUE                                                                 NAVYPB.214
        IF(DTHVDZ(IM1).GT.0.0)XL(II)=XL(IM1)                                     NAVYPB.215
        DO 170 I=1,II                                                            NAVYPB.216
          CLAM2(I)=B2*XL(I)                                                      NAVYPB.217
  170   CONTINUE                                                                 NAVYPB.218
C *************** REALIZIBILITY CONSTRAINTS *******************                  NAVYPB.219
C UPDATE TURBULENCE VARIABLES.MAY HAVE TO SET VALUES TO ZERO                     NAVYPB.220
C IF THEY GO NEGATIVE.                                                           NAVYPB.221
        DO 310 I=2,IM1                                                           NAVYPB.222
          IF(TKE(I).LT.0.0)TKE(I)=1.E-4                                          NAVYPB.223
          IF(THLSB(I).LT.0.0)THLSB(I)=0.0                                        NAVYPB.224
          IF(QWSB(I).LT.0.0)QWSB(I)=0.0                                          NAVYPB.225
          TKESR(I)=SQRT(TKE(I))                                                  NAVYPB.226
  310   CONTINUE                                                                 NAVYPB.227
C CALL S/R HOSKEEP TO COMPUTE EKM,EKH AND LIQUID                                 NAVYPB.228
C WATER RELATED VARIABLES                                                        NAVYPB.229
        CALL HOSKEEP(IJ,JJ,INEST)                                                07NOV00.1675
C CALCULATE MOMENTUM,HEAT,AND MOISTURE FLUXES                                    NAVYPB.231
C   WK(I,1)=S(I)=YAMADA STABILITY FACTOR                                         NAVYPB.232
        DO 200 I=2,IM1                                                           NAVYPB.233
          THVWB(I)=EKH(I)*WK(I,1)/BETA                                           NAVYPB.234
          THLWB(I)=EKH(I)*DTHLDZ(I)                                              NAVYPB.235
          WQWB(I)=EKH(I)*DQWDZ(I)                                                NAVYPB.236
  200   CONTINUE                                                                 NAVYPB.237
C THE IMPLICIT FINITE DIFFERENCE SCHEME D.E.S. FOR                               NAVYPB.238
C U,V,THL,QW,TKE,THLSB,QWSB,AND THLQWB ARE PUT                                   NAVYPB.239
C INTO TRIDIAGONAL FORM AND SOLVED BY THE THOMAS ALGORITHM.                      NAVYPB.240
C THE BOUNDARY CONDITIONS RELATE VALUES AT GRID POINTS 1 AND                     NAVYPB.241
C IM1 TO THE VALUES AT THE SURFACE AND GRID POINTS 2,AND VALUES AT               NAVYPB.242
C IM2 AND II, RESPECTIVELY.                                                      NAVYPB.243
C THE CALCULATION OF NEW VALUES OF THE MEAN QUANTITIES                           NAVYPB.244
C MAY BE DELAYED TO ALLOW THE TURBULENT FLUXES TO ADJUST TO                      NAVYPB.245
C THE INITIAL MEAN PROFILES.                                                     NAVYPB.246
C     IF(IT.GT.IDELAY)GO TO 210                                                  NAVYPB.247
C     GO TO 220                                                                  NAVYPB.248
C210  CONTINUE                                                                   NAVYPB.249
        SIGN=1.0                                                                 NAVYPB.250
        CALL UVCOMP(U,UN,EKM,ZS,ZN,WK,DT,USTAR,COSALP,Z,II)                      NAVYPB.251
        SIGN=-1.0                                                                NAVYPB.252
        CALL UVCOMP(V,VN,EKM,ZS,ZN,WK,DT,USTAR,SINALP,Z,II)                      NAVYPB.253
C                                                                                NAVYPB.254
C                                                                                NAVYPB.255
        DO 250 I=1,II                                                            NAVYPB.256
          QRN(I)=0.0                                                             NAVYPB.257
  250   CONTINUE                                                                 NAVYPB.258
C                                                                                NAVYPB.259
C                                                                                NAVYPB.260
  240   CONTINUE                                                                 NAVYPB.261
C                                                                                NAVYPB.262
C                                                                                NAVYPB.263
C   IN THIS SECTION OF CODE THE WORK ARRAY IS DEFINED                            NAVYPB.264
C THE WORK ARRAY IS USED REPEATEDLY FOR DIFFERENT TASKS---                       NAVYPB.265
C SAVING HAVING TO DEFINE A LARGE NUMBER OF ARRAYS.                              NAVYPB.266
C                                                                                NAVYPB.267
C   WHENEVER NEW CODE IS INTRODUCED INTO THIS SECTION,CARE                       NAVYPB.268
C   MUST BE TAKEN TO NOT DISTURB THE CURRENT MEANING OF THE                      NAVYPB.269
C   WORK ARRAY.                                                                  NAVYPB.270
C                                                                                NAVYPB.271
C******************** THL EQUATION**********************                         NAVYPB.272
        DO 260 I=2,IM1                                                           NAVYPB.273
          WK(I,1)=(EKH(I)+EKH(I-1))/(ZS(I)*(ZN(I)+ZS(I)))                        NAVYPB.274
          WK(I,3)=(EKH(I)+EKH(I+1))/(ZN(I)*(ZN(I)+ZS(I)))                        NAVYPB.275
          WK(I,2)=WK(I,1)+WK(I,3)+1.0/DT                                         NAVYPB.276
          WK(I,4)=THL(I)/DT                                                      NAVYPB.277
  260   CONTINUE                                                                 NAVYPB.278
        TL1=EKH(1)+EKH(2)                                                        NAVYPB.279
        TL2=1.0/(Z(2)*(Z(2)-Z(1)))                                               NAVYPB.280
        XNUM=TL1*TL2                                                             NAVYPB.281
        XDENOM=XNUM+1.0/DT                                                       NAVYPB.282
        WK(1,5)=XNUM/XDENOM                                                      NAVYPB.283
        TL3=THL(1)/DT                                                            NAVYPB.284
        TL4=(2.*THLWB(1))/Z(2)                                                   NAVYPB.285
        XNUM2=TL3-TL4                                                            NAVYPB.286
        WK(1,6)=XNUM2/XDENOM                                                     NAVYPB.287
C   INSERT UPPER B.C.                                                            NAVYPB.288
        WK(IM1,4)=WK(IM1,4)+WK(IM1,3)*THL(II)                                    NAVYPB.289
        WK(IM1,3)=0.0                                                            NAVYPB.290
        DO 700 I=2,IM1                                                           NAVYPB.291
          DENOM=WK(I,2)-WK(I,1)*WK(I-1,5)                                        NAVYPB.292
          WK(I,5)=WK(I,3)/DENOM                                                  NAVYPB.293
          WK(I,6)=(WK(I,4)+WK(I,1)*WK(I-1,6))/DENOM                              NAVYPB.294
  700   CONTINUE                                                                 NAVYPB.295
        THL(IM1)=WK(IM1,6)                                                       NAVYPB.296
        DO 701 I=IM2,1,-1                                                        NAVYPB.297
          THL(I)=WK(I,5)*THL(I+1)+WK(I,6)                                        NAVYPB.298
  701   CONTINUE                                                                 NAVYPB.299
C******************* QW EQUATION **************************                      NAVYPB.300
        DO 702 I=2,IM1                                                           NAVYPB.301
          WK(I,1)=SCHMIDT*WK(I,1)                                                NAVYPB.302
          WK(I,3)=SCHMIDT*WK(I,3)                                                NAVYPB.303
          WK(I,2)=WK(I,1)+WK(I,3)+1.0/DT                                         NAVYPB.304
          WK(I,4)=QW(I)/DT                                                       NAVYPB.305
  702   CONTINUE                                                                 NAVYPB.306
        TL1=EKH(1)+EKH(2)                                                        NAVYPB.307
        TL2=1.0/(Z(2)*(Z(2)-Z(1)))                                               NAVYPB.308
        XNUM=TL1*TL2                                                             NAVYPB.309
        XDENOM=XNUM+1.0/DT                                                       NAVYPB.310
        WK(1,5)=XNUM/XDENOM                                                      NAVYPB.311
        TL3=QW(1)/DT                                                             NAVYPB.312
        TL4=(2.*WQWB(1))/Z(2)                                                    NAVYPB.313
        XNUM2=TL3-TL4                                                            NAVYPB.314
        WK(1,6)=XNUM2/XDENOM                                                     NAVYPB.315
C   INSERT UPPER B.C.                                                            NAVYPB.316
        WK(IM1,4)=WK(IM1,4)+WK(IM1,3)*QW(II)                                     NAVYPB.317
        WK(IM1,3)=0.0                                                            NAVYPB.318
        DO 703 I=2,IM1                                                           NAVYPB.319
          DENOM=WK(I,2)-WK(I,1)*WK(I-1,5)                                        NAVYPB.320
          WK(I,5)=WK(I,3)/DENOM                                                  NAVYPB.321
          WK(I,6)=(WK(I,4)+WK(I,1)*WK(I-1,6))/DENOM                              NAVYPB.322
  703   CONTINUE                                                                 NAVYPB.323
        QW(IM1)=WK(IM1,6)                                                        NAVYPB.324
        DO 704 I=IM2,1,-1                                                        NAVYPB.325
          QW(I)=WK(I,5)*QW(I+1)+WK(I,6)                                          NAVYPB.326
  704   CONTINUE                                                                 NAVYPB.327
  220   CONTINUE                                                                 NAVYPB.328
C                                                                                NAVYPB.329
C*************** TKE EQUATION ************************                           NAVYPB.330
C                                                                                NAVYPB.331
        DO 705 I=1,II                                                            NAVYPB.332
C       WK(I,8)=SQ*TKESR(I)*XL(I)                                                NAVYPB.333
C     WKTEST=WK(I,8)                                                             NAVYPB.334
C                                                                                NAVYPB.335
C  TESTING NEW FORM OF TRIPLE MOMENT DIFFUSION                                   NAVYPB.336
C                                                                                NAVYPB.337
          WK(I,8)=EKH(I)                                                         NAVYPB.338
C                                                                                NAVYPB.339
  705   CONTINUE                                                                 NAVYPB.340
        DO 706 I=2,IM1                                                           NAVYPB.341
          WK(I,1)=(WK(I,8)+WK(I-1,8))/(ZS(I)*(ZN(I)+ZS(I)))                      NAVYPB.342
          WK(I,3)=(WK(I,8)+WK(I+1,8))/(ZN(I)*(ZN(I)+ZS(I)))                      NAVYPB.343
          WK(I,2)=WK(I,1)+WK(I,3)+1./DT+(2.*TKESR(I))/(B1*XL(I))                 NAVYPB.344
          WK(I,7)=2.*EKM(I)*(DUDZ(I)*DUDZ(I)+DVDZ(I)*DVDZ(I))-2.*Y1*             NAVYPB.345
     +            EKH(I)*DTHVDZ(I)                                               NAVYPB.346
          WK(I,4)=TKE(I)/DT+WK(I,7)                                              NAVYPB.347
  706   CONTINUE                                                                 NAVYPB.348
        WK(1,5)=0.0                                                              NAVYPB.349
        WK(1,6)=TKE(1)                                                           NAVYPB.350
C   INSERT UPPER B.C.                                                            NAVYPB.351
        WK(IM1,4)=WK(IM1,4)+WK(IM1,3)*TKE(II)                                    NAVYPB.352
        WK(IM1,3)=0.0                                                            NAVYPB.353
        DO 707 I=2,IM1                                                           NAVYPB.354
          DENOM=WK(I,2)-WK(I,1)*WK(I-1,5)                                        NAVYPB.355
          WK(I,5)=WK(I,3)/DENOM                                                  NAVYPB.356
          WK(I,6)=(WK(I,4)+WK(I,1)*WK(I-1,6))/DENOM                              NAVYPB.357
  707   CONTINUE                                                                 NAVYPB.358
        TKE(IM1)=WK(IM1,6)                                                       NAVYPB.359
        DO 708 I=IM2,1,-1                                                        NAVYPB.360
          TKE(I)=WK(I,5)*TKE(I+1)+WK(I,6)                                        NAVYPB.361
  708   CONTINUE                                                                 NAVYPB.362
C****************** THLSB EQUATION *************************                     NAVYPB.363
        DO 709 I=2,IM1                                                           NAVYPB.364
          WK(I,2)=WK(I,1)+WK(I,3)+1.0/DT+(2.*TKESR(I))/(B2*XL(I))                NAVYPB.365
          WK(I,7)=2.*THLWB(I)*DTHLDZ(I)                                          NAVYPB.366
          WK(I,4)=THLSB(I)/DT+WK(I,7)                                            NAVYPB.367
  709   CONTINUE                                                                 NAVYPB.368
        WK(1,5)=0.0                                                              NAVYPB.369
        WK(1,6)=THLSB(1)                                                         NAVYPB.370
C   INSERT UPPER B.C.                                                            NAVYPB.371
        WK(IM1,4)=WK(IM1,4)+WK(IM1,3)*THLSB(II)                                  NAVYPB.372
        WK(IM1,3)=0.0                                                            NAVYPB.373
        DO 710 I=2,IM1                                                           NAVYPB.374
          DENOM=WK(I,2)-WK(I,1)*WK(I-1,5)                                        NAVYPB.375
          WK(I,5)=WK(I,3)/DENOM                                                  NAVYPB.376
          WK(I,6)=(WK(I,4)+WK(I,1)*WK(I-1,6))/DENOM                              NAVYPB.377
  710   CONTINUE                                                                 NAVYPB.378
        THLSB(IM1)=WK(IM1,6)                                                     NAVYPB.379
        DO 711 I=IM2,1,-1                                                        NAVYPB.380
          THLSB(I)=WK(I,5)*THLSB(I+1)+WK(I,6)                                    NAVYPB.381
  711   CONTINUE                                                                 NAVYPB.382
C**************** QWSB EQUATION ****************************                     NAVYPB.383
        DO 712 I=2,IM1                                                           NAVYPB.384
          WK(I,7)=2.*WQWB(I)*DQWDZ(I)                                            NAVYPB.385
          WK(I,4)=QWSB(I)/DT+WK(I,7)                                             NAVYPB.386
  712   CONTINUE                                                                 NAVYPB.387
        WK(1,5)=0.0                                                              NAVYPB.388
        WK(1,6)=QWSB(1)                                                          NAVYPB.389
C   INSERT UPPER B.C.                                                            NAVYPB.390
        WK(IM1,4)=WK(IM1,4)+WK(IM1,3)*QWSB(II)                                   NAVYPB.391
        WK(IM1,3)=0.0                                                            NAVYPB.392
        DO 713 I=2,IM1                                                           NAVYPB.393
          DENOM=WK(I,2)-WK(I,1)*WK(I-1,5)                                        NAVYPB.394
          WK(I,5)=WK(I,3)/DENOM                                                  NAVYPB.395
          WK(I,6)=(WK(I,4)+WK(I,1)*WK(I-1,6))/DENOM                              NAVYPB.396
  713   CONTINUE                                                                 NAVYPB.397
        QWSB(IM1)=WK(IM1,6)                                                      NAVYPB.398
        DO 714 I=IM2,1,-1                                                        NAVYPB.399
          QWSB(I)=WK(I,5)*QWSB(I+1)+WK(I,6)                                      NAVYPB.400
  714   CONTINUE                                                                 NAVYPB.401
C**************** THLQWB EQUATION ************************                       NAVYPB.402
        DO 715 I=2,IM1                                                           NAVYPB.403
          WK(I,7)=THLWB(I)*DQWDZ(I)+WQWB(I)*DTHLDZ(I)                            NAVYPB.404
          WK(I,4)=THLQWB(I)/DT+WK(I,7)                                           NAVYPB.405
  715   CONTINUE                                                                 NAVYPB.406
        WK(1,5)=0.0                                                              NAVYPB.407
        WK(1,6)=THLQWB(1)                                                        NAVYPB.408
C   INSERT UPPER B.C.                                                            NAVYPB.409
        WK(IM1,4)=WK(IM1,4)+WK(IM1,3)*THLQWB(II)                                 NAVYPB.410
        WK(IM1,3)=0.0                                                            NAVYPB.411
        DO 716 I=2,IM1                                                           NAVYPB.412
          DENOM=WK(I,2)-WK(I,1)*WK(I-1,5)                                        NAVYPB.413
          WK(I,5)=WK(I,3)/DENOM                                                  NAVYPB.414
          WK(I,6)=(WK(I,4)+WK(I,1)*WK(I-1,6))/DENOM                              NAVYPB.415
  716   CONTINUE                                                                 NAVYPB.416
        THLQWB(IM1)=WK(IM1,6)                                                    NAVYPB.417
        DO 717 I=IM2,1,-1                                                        NAVYPB.418
          THLQWB(I)=WK(I,5)*THLQWB(I+1)+WK(I,6)                                  NAVYPB.419
  717   CONTINUE                                                                 NAVYPB.420
C                                                                                NAVYPB.421
C COMPUTE NEW MEAN VALUES AFTER THE DELAY PERIOD.                                NAVYPB.422
C   COMPUTE PRECIPITATION VELOCITY USES BARKER FOG PAPER                         NAVYPB.423
C   RELATIONSHIPS.   MKS UNITS    CONV IS A CONVERSION                           NAVYPB.424
C   FACTOR FROM MB TO MKS.                                                       NAVYPB.425
C      DO 335 I=1,IM1                                                            NAVYPB.426
C        RHOA=(CONV*P(I))/(RD*TV(I))                                             NAVYPB.427
C        XNUM=0.1*RHOA*QL(I)                                                     NAVYPB.428
C        XDENOM=3.1416*CCN*RHOW                                                  NAVYPB.429
C        RCSQ=(XNUM/XDENOM)**0.6667                                              NAVYPB.430
C        VT(I)=-10.5*CNST*RCSQ                                                   NAVYPB.431
C 335  CONTINUE                                                                  NAVYPB.432
C                                                                                NAVYPB.433
        IF(IT.GT.IDELAY)GOTO 320                                                 NAVYPB.434
        GOTO 325                                                                 NAVYPB.435
  320   CONTINUE                                                                 NAVYPB.436
        THLZ=SST*(1000./PS)**0.286                                               NAVYPB.437
C                                                                                NAVYPB.438
        TAUNDG=15.*60.                                                           NAVYPB.439
        DO 330 I=1,IM1                                                           NAVYPB.440
          THL(I)=THL(I)+DT*QRN(I)                                                NAVYPB.441
  330   CONTINUE                                                                 NAVYPB.442
  325   CONTINUE                                                                 NAVYPB.443
C  DON`T PERMIT DEWPOINT DEPRESSION TO BE LARGER THAN TDELMAX                    NAVYPB.444
C  READJUST SPECHUM AND QW IF THIS LIMIT IS EXCEEDED.                            NAVYPB.445
C     DO 324 I=1,II                                                              NAVYPB.446
C       IF(SPECHUM(I).LT.1.E-10)SPECHUM(I)=1.E-10                                NAVYPB.447
C       E=(P(I)*SPECHUM(I))/(0.622+0.378*SPECHUM(I))                             NAVYPB.448
C       TDEW(I)=5351./(21.4-ALOG(E))                                             NAVYPB.449
C       TDEL=ABT(I)-TDEW(I)                                                      NAVYPB.450
C       IF(TDEL.LE.TDELMAX)GO TO 324                                             NAVYPB.451
C       TDEW(I)=ABT(I)-TDELMAX                                                   NAVYPB.452
C       E=EXP(21.4-5351./TDEW(I))                                                NAVYPB.453
C       SPECHUM(I)=(0.622*E)/(P(I)-0.378*E)                                      NAVYPB.454
C       QW(I)=SPECHUM(I)                                                         NAVYPB.455
  324   CONTINUE                                                                 NAVYPB.456
C     IF(JJ.EQ.1.AND.IJ.EQ.1)TIMER=TIMER+DT                                      NAVYPB.457
C                                                                                NAVYPB.458
C****************************************************                            NAVYPB.459
C   COMPUTE CHARNOCKS RELATION USING CONST. FROM                                 NAVYPB.460
C   GARRATT(MON.WEA.REV.,105,P915,1977.                                          NAVYPB.461
C                                                                                NAVYPB.462
        IF(XLAND(IJ,JJ).GT.1.5)THEN                                              NAVYPB.463
          Z1=0.0144*USTAR*USTAR/G                                                NAVYPB.464
          ZM=VISK/(VK*USTAR)                                                     NAVYPB.465
          IF(ZM.GT.Z1)Z1=ZM                                                      NAVYPB.466
          IF(Z1.GT.1.E-1)Z1=1.E-1                                                NAVYPB.467
        ENDIF                                                                    NAVYPB.468
  999   CONTINUE                                                                 NAVYPB.469
        CALL OUTPB(PRATIO,GWWG,ABT,SPECHUM,UN,VN,QW,QL,THLSB,THLQWB,QWSB         NAVYPB.470
     +       ,TKE,Z1,IJ,JJ,DT,T3DTEN,QV3DTEN,U3DTEN,V3DTEN,THL,SST,              NAVYPB.471
     +       QC3DTEN,QR3DTEN,INEST,NN,SIGG,KTAU,IDELAY,PSBD,T3D,QV3D,U3D         NAVYPB.472
     +       ,V3D,IYY,JXX)                                                       NAVYPB.473
C                                                                                NAVYPB.474
C *****************************************************************              NAVYPB.475
C                                                                                NAVYPB.476
      ENDDO                                                                      NAVYPB.477
      RETURN                                                                     NAVYPB.478
      END                                                                        NAVYPB.479
                                                                                 NAVYPB.480
