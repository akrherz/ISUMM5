C                                                                                HOSKEEP.1
C ===================================================================            HOSKEEP.2
C                                                                                HOSKEEP.3
      SUBROUTINE HOSKEEP(IRR,JRR,INEST)                                          07NOV00.1669
#     include <parame.incl>                                                      HOSKEEP.5
#     include <param2.incl>                                                      HOSKEEP.6
#     include <point2d.incl>                                                     HOSKEEP.7
C                                                                                HOSKEEP.8
C  SUBROUTINE FOR COMPUTING GRID VOLUME LIQUID WATER CONTENT, QL,                HOSKEEP.9
C  AND FRACTIONAL CLOUDINESS, RC. ALSO, EDDY TRANSFER COEFFS. ARE                HOSKEEP.10
C  COMPUTED IN THIS S/R.                                                         HOSKEEP.11
C    SOMMERIA AND DEARDORFF (1977),  AND HASSID AND GALPERIN (1983)              HOSKEEP.12
C    ARE PRIMARY SOURCES.                                                        HOSKEEP.13
C                                                                                HOSKEEP.14
      PARAMETER (II=MKX)                                                         HOSKEEP.15
C                                                                                HOSKEEP.16
#     include <btblk1.incl>                                                      HOSKEEP.17
#     include <btblk2.incl>                                                      HOSKEEP.18
      DIMENSION EKMTMP(II),EKHTMP(II)                                            HOSKEEP.19
C                                                                                HOSKEEP.20
C                                                                                HOSKEEP.21
      XLCP=HEATL/CPD                                                             HOSKEEP.22
      DO 50 I=1,II                                                               HOSKEEP.23
        TL(I)=THL(I)*PRATIO(I)                                                   HOSKEEP.24
        CALL ESATPB(ES,TL(I))                                                    HOSKEEP.25
        QSL(I)=(0.622*ES)/(P(I)-0.378*ES)                                        HOSKEEP.26
        RH=QW(I)/QSL(I)                                                          HOSKEEP.27
        IF(RH.GT.0.5)GOTO 31                                                     HOSKEEP.28
        RP=0.0                                                                   HOSKEEP.29
        DQSLDT=0.0                                                               HOSKEEP.30
        BTP=BETA*(1.0+0.609*QW(I))                                               HOSKEEP.31
        BWP=0.609*BETA*THL(I)                                                    HOSKEEP.32
        GOTO 55                                                                  HOSKEEP.33
   31   CONTINUE                                                                 HOSKEEP.34
        DQSLDT=0.622*HEATL*QSL(I)/(RD*TL(I)*TL(I))                               HOSKEEP.35
        AL1=DQSLDT*PRATIO(I)                                                     HOSKEEP.36
        SCR2=SQRT(QWSB(I)*THLSB(I))                                              HOSKEEP.37
        IF(SCR2.EQ.0.0)THEN                                                      HOSKEEP.38
          FACR=0.0                                                               HOSKEEP.39
        ELSE                                                                     HOSKEEP.40
          FACR=THLQWB(I)/SCR2                                                    HOSKEEP.41
        ENDIF                                                                    HOSKEEP.42
        DUMV(I)=(QWSB(I)+AL1*AL1*THLSB(I)-2.*FACR*AL1*SCR2)                      HOSKEEP.43
        IF(DUMV(I).LE.0.0)THEN                                                   HOSKEEP.44
          SIG1=0.0                                                               HOSKEEP.45
        ELSE                                                                     HOSKEEP.46
          SIG1=SQRT(DUMV(I))                                                     HOSKEEP.47
          Q1X=(QW(I)-QSL(I))/SIG1                                                HOSKEEP.48
          GOTO 110                                                               HOSKEEP.49
        ENDIF                                                                    HOSKEEP.50
  100   IF(QW(I).GT.QSL(I))GOTO 120                                              HOSKEEP.51
        Q1X=-1.6                                                                 HOSKEEP.52
        GOTO 110                                                                 HOSKEEP.53
  120   CONTINUE                                                                 HOSKEEP.54
C       QL(I)=QW(I)-QSL(I)                                                       HOSKEEP.55
C       IF(QL(I).GT.1.E-3)QL(I)=1.E-3                                            HOSKEEP.56
        IF(QL(I).LT.0.0)QL(I)=0.0                                                HOSKEEP.57
C       SPECHUM(I)=QSL(I)                                                        HOSKEEP.58
C       QW(I)=QSL(I)+QL(I)                                                       HOSKEEP.59
C       RC(I)=1.0                                                                HOSKEEP.60
        Q1X=1.6                                                                  HOSKEEP.61
        XLAM1=1./(1.+DQSLDT*XLCP)                                                HOSKEEP.62
        GOTO 130                                                                 HOSKEEP.63
  110   CONTINUE                                                                 HOSKEEP.64
        XLAM1=1./(1.+DQSLDT*XLCP)                                                HOSKEEP.65
        IF(Q1X.LE.-1.6)GOTO 140                                                  HOSKEEP.66
        IF(Q1X.LE.1.6)GOTO 150                                                   HOSKEEP.67
C       QL(I)=XLAM1*SIG1*Q1X                                                     HOSKEEP.68
C       IF(QL(I).GT.1.E-3)QL(I)=1.E-3                                            HOSKEEP.69
        IF(QL(I).LT.0.0)QL(I)=0.0                                                HOSKEEP.70
C       SPECHUM(I)=QSL(I)                                                        HOSKEEP.71
C       QW(I)=QSL(I)+QL(I)                                                       HOSKEEP.72
C       RC(I)=1.0                                                                HOSKEEP.73
        GOTO 130                                                                 HOSKEEP.74
  140   CONTINUE                                                                 HOSKEEP.75
C       QL(I)=0.0                                                                HOSKEEP.76
C       RC(I)=0.0                                                                HOSKEEP.77
        GOTO 130                                                                 HOSKEEP.78
  150   CONTINUE                                                                 HOSKEEP.79
C       QL(I)=(XLAM1*SIG1*(Q1X+1.6)*(Q1X+1.6))/6.4                               HOSKEEP.80
C       IF(QL(I).GT.1.E-3)QL(I)=1.E-3                                            HOSKEEP.81
        IF(QL(I).LT.0.0)QL(I)=0.0                                                HOSKEEP.82
C       SPECHUM(I)=QW(I)-QL(I)                                                   HOSKEEP.83
C       IF(SPECHUM(I).GT.QSL(I))SPECHUM(I)=QSL(I)                                HOSKEEP.84
        IF(RC(I).LT.1.)RC(I)=0.5*(1.0+Q1X/1.6)                                   HOSKEEP.85
  130   CONTINUE                                                                 HOSKEEP.86
        B=XLAM1*AL1                                                              HOSKEEP.87
        SIGS2=0.25*(XLAM1*XLAM1*QWSB(I)+B*B*THLSB(I)-2.*XLAM1*B*                 HOSKEEP.88
     +        THLQWB(I))                                                         HOSKEEP.89
        IF(SIGS2.LE.0.0)GOTO 165                                                 HOSKEEP.90
        XDEN=SQRT(SIGS2)                                                         HOSKEEP.91
        TL6=-0.5*Q1X*Q1X                                                         HOSKEEP.92
        IF(TL6.LT.-675.0)TL6=-675.0                                              HOSKEEP.93
        RP=RC(I)-0.19947*QL(I)*EXP(TL6)/XDEN                                     HOSKEEP.94
        GOTO 167                                                                 HOSKEEP.95
  165   CONTINUE                                                                 HOSKEEP.96
        RP=0.0                                                                   HOSKEEP.97
  167   CONTINUE                                                                 HOSKEEP.98
        BETAT=BETA*(1.+0.609*QW(I)-1.61*QL(I))                                   HOSKEEP.99
        BETAL=BETA*((1.+0.609*QW(I)-3.22*QL(I))*XLCP/PRATIO(I)-1.61*             HOSKEEP.100
     +        THL(I))                                                            HOSKEEP.101
        BETAW=0.609*BETA*(THL(I)+XLCP*QL(I)/PRATIO(I))                           HOSKEEP.102
        BTP=BETAT-B*BETAL*RP                                                     HOSKEEP.103
        BWP=BETAW+XLAM1*BETAL*RP                                                 HOSKEEP.104
   55   CONTINUE                                                                 HOSKEEP.105
        IF(I.EQ.II.OR.I.EQ.1)GOTO 51                                             HOSKEEP.106
C WK(I,1)=YSF(I)=YAMADA STABILITY FACTOR                                         HOSKEEP.107
        WK(I,1)=BTP*DTHLDZ(I)+BWP*DQWDZ(I)                                       HOSKEEP.108
   51   CONTINUE                                                                 HOSKEEP.109
        QS(I)=QSL(I)+DQSLDT*XLCP*QL(I)                                           HOSKEEP.110
   50 CONTINUE                                                                   HOSKEEP.111
C                                                                                HOSKEEP.112
C   WE WANT TO PRESERVE THE INITIAL ABSOLUTE TEMPERATURE PROFILE                 HOSKEEP.113
C   EVEN IF THERE IS LIQUID WATER--- READJUSTING OTHER PARAMETERS.               HOSKEEP.114
C   AFTER IT=IDELAY, PREDICTED VALUES OF THL DRIVES CHANGES.                     HOSKEEP.115
C                                                                                HOSKEEP.116
C     GO TO 1000                                                                 HOSKEEP.117
C     IF(IT.GT.IDELAY)GO TO 1000                                                 HOSKEEP.118
C     DO 21 I=1,II                                                               HOSKEEP.119
C       SPECHUM(I)=QW(I)-QL(I)                                                   HOSKEEP.120
C       TL1=1.0+0.609*SPECHUM(I)-QL(I)                                           HOSKEEP.121
C       TV(I)=ABT(I)*TL1                                                         HOSKEEP.122
C       TH(I)=TV(I)/PRATIO(I)                                                    HOSKEEP.123
C       ABTH(I)=TH(I)/TL1                                                        HOSKEEP.124
C       THL(I)=ABTH(I)-XLCP*QL(I)/PRATIO(I)                                      HOSKEEP.125
C21   CONTINUE                                                                   HOSKEEP.126
C     GO TO 1010                                                                 HOSKEEP.127
 1000 CONTINUE                                                                   HOSKEEP.128
C     DO 20 I=1,II                                                               HOSKEEP.129
C       SPECHUM(I)=QW(I)-QL(I)                                                   HOSKEEP.130
C       ABTH(I)=THL(I)+XLCP*QL(I)/PRATIO(I)                                      HOSKEEP.131
C       TH(I)=ABTH(I)*(1.0+0.609*SPECHUM(I)-QL(I))                               HOSKEEP.132
C       TV(I)=TH(I)*PRATIO(I)                                                    HOSKEEP.133
C       ABT(I)=TV(I)/(1.0+0.609*SPECHUM(I)-QL(I))                                HOSKEEP.134
C20   CONTINUE                                                                   HOSKEEP.135
 1010 CONTINUE                                                                   HOSKEEP.136
C                                                                                HOSKEEP.137
C  IF HASSID=1.0 USE HASSID TRANSFER COEFS.                                      HOSKEEP.138
C  IF HASSID=0.0 USE YAMADA   "  "   ""                                          HOSKEEP.139
C                                                                                HOSKEEP.140
      IF(HASSID.EQ.1.0)GOTO 2550                                                 HOSKEEP.141
      DO 2000 I=2,IM1                                                            HOSKEEP.142
        SHEAR=DUDZ(I)*DUDZ(I)+DVDZ(I)*DVDZ(I)                                    HOSKEEP.143
        IF(TKESR(I).LE.1.E-20)GOTO 2030                                          HOSKEEP.144
        XL1=A1*XL(I)                                                             HOSKEEP.145
        XL2=A2*XL(I)                                                             HOSKEEP.146
        XNUM=XL1*TKE(I)*TKESR(I)*((1.-3.*C)*TKE(I)+3.*XL2*((CLAM2(I)-3.*         HOSKEEP.147
     +       XL2)-3.*C*(4.*XL1+CLAM2(I)))*G*WK(I,1))                             HOSKEEP.148
        XDENOM=TKE(I)*TKE(I)+6.*XL1*XL1*TKE(I)*SHEAR+3.*XL1*XL2*G*               HOSKEEP.149
     +         WK(I,1)*(6.*XL1*(CLAM2(I)-3.*XL2)*SHEAR+(7.+B2/A1)*TKE(I)         HOSKEEP.150
     +         +9.*XL2*(4.*XL1+CLAM2(I))*G*WK(I,1))                              HOSKEEP.151
C       IF(XNUM.LT.0.0.AND.XDENOM.LT.0.0)GOTO 2010                               HOSKEEP.152
        SCR=0.2*TKE(I)*TKE(I)                                                    HOSKEEP.153
        IF(XDENOM.LT.SCR)XDENOM=SCR                                              HOSKEEP.154
C2010   CONTINUE                                                                 HOSKEEP.155
        EKMTMP(I)=AMAX1(XNUM/XDENOM,0.0)                                         HOSKEEP.156
        XNUM=XL2*(TKE(I)*TKESR(I)-6.*XL1*EKMTMP(I)*SHEAR)                        HOSKEEP.157
        XDENOM=(TKE(I)+3.*XL2*(4.*XL1+CLAM2(I))*G*WK(I,1))                       HOSKEEP.158
C       IF(XNUM.LT.0.0.AND.XDENOM.LT.0.0)GOTO 2020                               HOSKEEP.159
        SCR=0.4*TKE(I)                                                           HOSKEEP.160
        IF(XDENOM.LT.SCR)XDENOM=SCR                                              HOSKEEP.161
C2020   CONTINUE                                                                 HOSKEEP.162
        EKHTMP(I)=AMAX1(XNUM/XDENOM,0.0)                                         HOSKEEP.163
        GOTO 2040                                                                HOSKEEP.164
 2030   CONTINUE                                                                 HOSKEEP.165
        EKMTMP(I)=0.0                                                            HOSKEEP.166
        EKHTMP(I)=0.0                                                            HOSKEEP.167
 2040   CONTINUE                                                                 HOSKEEP.168
 2000 CONTINUE                                                                   HOSKEEP.169
C     GO TO 2100                                                                 HOSKEEP.170
C     IF(IT.EQ.0)GO TO 2100                                                      HOSKEEP.171
C     DO 2110 I=2,IM1                                                            HOSKEEP.172
C       EKM(I)=0.5*(EKMTMP(I)+EKM(I))                                            HOSKEEP.173
C       EKH(I)=0.5*(EKHTMP(I)+EKH(I))                                            HOSKEEP.174
C                                                                                HOSKEEP.175
C       IF(EKM(I).GT.200.)EKM(I)=200.                                            HOSKEEP.176
C       IF(EKH(I).GT.200.)EKH(I)=200.                                            HOSKEEP.177
C                                                                                HOSKEEP.178
 2110 CONTINUE                                                                   HOSKEEP.179
C     GO TO 2120                                                                 HOSKEEP.180
 2100 CONTINUE                                                                   HOSKEEP.181
      DO 2130 I=2,IM1                                                            HOSKEEP.182
C      SET MINIMUM K ACCORDING TO LAYER THICKNESS                                HOSKEEP.183
        DZ=0.5*(Z(I+1)-Z(I-1))                                                   HOSKEEP.184
        EKM(I)=AMAX1(EKMTMP(I),.001*DZ)                                          HOSKEEP.185
        EKH(I)=AMAX1(EKHTMP(I),.001*DZ)                                          HOSKEEP.186
        IF(EKM(I).GT.100.)EKM(I)=100.                                            HOSKEEP.187
        IF(EKH(I).GT.100.)EKH(I)=100.                                            HOSKEEP.188
 2130 CONTINUE                                                                   HOSKEEP.189
 2120 CONTINUE                                                                   HOSKEEP.190
      GOTO 2600                                                                  HOSKEEP.191
 2550 CONTINUE                                                                   HOSKEEP.192
C   COMPUTE TRANSFER COEFFS. BASED ON HASSID-GALPERIN.                           HOSKEEP.193
      DO 3000 I=2,IM1                                                            HOSKEEP.194
        HGL1=A1*XL(I)                                                            HOSKEEP.195
        HGL2=A2*XL(I)                                                            HOSKEEP.196
        HGPHI=3.*XL(I)*HGL2*G*WK(I,1)                                            HOSKEEP.197
        HGPHI=HGPHI/TKE(I)                                                       HOSKEEP.198
        SHEAR=(DUDZ(I)*DUDZ(I)+DVDZ(I)*DVDZ(I))                                  HOSKEEP.199
        HGXI=(6.*HGL1*HGL1*SHEAR)                                                HOSKEEP.200
        HGXI=HGXI/TKE(I)                                                         HOSKEEP.201
        IF(HGPHI.GT.0.)GOTO 3100                                                 HOSKEEP.202
        TL1=6.*A1+B2                                                             HOSKEEP.203
        SCR=1.+TL1*HGPHI                                                         HOSKEEP.204
        TL2=1./TL1                                                               HOSKEEP.205
        IF(SCR.LT.0.0)HGPHI=-TL2                                                 HOSKEEP.206
 3100   CONTINUE                                                                 HOSKEEP.207
        DENOM=1.+(B2-3.*A2)*HGPHI                                                HOSKEEP.208
        XNUM1=(7.0*A1+B2)                                                        HOSKEEP.209
        XNUM1=XNUM1*HGPHI                                                        HOSKEEP.210
        XNUM2=(4.0*A1+B2)*3.0*A1                                                 HOSKEEP.211
        XNUM2=XNUM2*HGPHI*HGPHI                                                  HOSKEEP.212
        XNUM=1.0+XNUM1+XNUM2                                                     HOSKEEP.213
        XIMAX=XNUM/DENOM                                                         HOSKEEP.214
        IF(HGXI.GT.XIMAX)HGXI=XIMAX                                              HOSKEEP.215
        DEN1=(7.0*A1+B2)                                                         HOSKEEP.216
        DEN1=DEN1*HGPHI                                                          HOSKEEP.217
        DEN2=3.*A1*(4.0*A1+B2)                                                   HOSKEEP.218
        DEN2=DEN2*HGPHI*HGPHI                                                    HOSKEEP.219
        DEN3=HGXI*(1.0+(B2-3.0*A2)*HGPHI)                                        HOSKEEP.220
        DENOM=1.0+DEN1+DEN2+DEN3                                                 HOSKEEP.221
        XNUMH=HGL2*TKESR(I)*(1.0+3.0*A1*HGPHI+3.0*C*HGXI)                        HOSKEEP.222
        EKHS=XNUMH/DENOM                                                         HOSKEEP.223
        XNUM1=B2*(1.0-3.0*C)-12.0*A1*C-3.0*A2                                    HOSKEEP.224
        XNUM1=XNUM1*HGPHI                                                        HOSKEEP.225
        XNUM2=1.0-3.0*C                                                          HOSKEEP.226
        XNUMM=HGL1*TKESR(I)*(XNUM1+XNUM2)                                        HOSKEEP.227
        EKMS=XNUMM/DENOM                                                         HOSKEEP.228
C       GOTO 5500                                                                HOSKEEP.229
C       IF(IT.EQ.0)GOTO 5500                                                     HOSKEEP.230
C       EKM(I)=0.5*(EKMS+EKM(I))                                                 HOSKEEP.231
C       EKH(I)=0.5*(EKHS+EKH(I))                                                 HOSKEEP.232
C       GOTO 5502                                                                HOSKEEP.233
C5500   CONTINUE                                                                 HOSKEEP.234
        EKM(I)=EKMS                                                              HOSKEEP.235
        EKH(I)=EKHS                                                              HOSKEEP.236
 5502   CONTINUE                                                                 HOSKEEP.237
 3000 CONTINUE                                                                   HOSKEEP.238
C                                                                                HOSKEEP.239
 2600 CONTINUE                                                                   HOSKEEP.240
C                                                                                HOSKEEP.241
C     P(1)=PS                                                                    HOSKEEP.242
C     PRATIO(1)=(P(1)/1000.)**0.286                                              HOSKEEP.243
C     DO 25 I=2,II                                                               HOSKEEP.244
C       P(I)=P(I-1)*EXP(-GRD*(Z(I)-Z(I-1))/(0.5*(TV(I)+TV(I-1))))                HOSKEEP.245
C       PRATIO(I)=(P(I)/1000.)**.286                                             HOSKEEP.246
C25   CONTINUE                                                                   HOSKEEP.247
C     IF(IT.GT.1)GO TO 1                                                         HOSKEEP.248
      PI=3.141592654                                                             HOSKEEP.249
      TL1=SQRT(PI)                                                               HOSKEEP.250
      C1G=2.*TL1                                                                 HOSKEEP.251
      C2G=2.*PI                                                                  HOSKEEP.252
      TAU1=8.64E4                                                                HOSKEEP.253
      D1P=0.10                                                                   HOSKEEP.254
      C2CAP=0.9                                                                  HOSKEEP.255
      RHOW=1.E3                                                                  HOSKEEP.256
      GAM2=0.25                                                                  HOSKEEP.257
      WK2=0.3                                                                    HOSKEEP.258
      WMAX=0.40                                                                  HOSKEEP.259
      W2BULK=GW*WMAX                                                             HOSKEEP.260
      IF(W2BULK.EQ.0.0)W2BULK=0.0001                                             HOSKEEP.261
      WG=W2BULK                                                                  HOSKEEP.262
      PRATIO1=(PS/1000.)**0.286                                                  HOSKEEP.263
      TH2=TRSTORE/PRATIO1                                                        HOSKEEP.264
      BOLTZ=5.67E-8                                                              HOSKEEP.265
    1 CONTINUE                                                                   HOSKEEP.266
      IF(IT.LE.IDELAY)GOTO 10                                                    HOSKEEP.267
      IF(GW.EQ.1.0)GOTO 10                                                       HOSKEEP.268
      SST=THLZ*(PS/1000.)**0.286                                                 HOSKEEP.269
C     THLZ = SST*(1000./PS)**0.286                                               HOSKEEP.270
      RHOAT=(PS*100.)/(RD*ABT(1))                                                HOSKEEP.271
      RHOC2=(0.27+W2BULK)*4.186E6                                                HOSKEEP.272
      TL2=W2BULK**0.33333                                                        HOSKEEP.273
      XLAM2=(0.001+0.004*TL2)*4.186E2                                            HOSKEEP.274
      D12=SQRT(TAU1*XLAM2/RHOC2)                                                 HOSKEEP.275
C  COMPUTE THE BARE GROUND ALBEDO                                                HOSKEEP.276
      SCR=WG/WK2                                                                 HOSKEEP.277
      IF(SCR.GT.1.0)GOTO 2                                                       HOSKEEP.278
      GAM2=0.31-0.17*SCR                                                         HOSKEEP.279
      GOTO 3                                                                     HOSKEEP.280
    2 CONTINUE                                                                   HOSKEEP.281
      GAM2=0.14                                                                  HOSKEEP.282
    3 CONTINUE                                                                   HOSKEEP.283
      SCR=WG/WMAX                                                                HOSKEEP.284
      IF(SCR.GE.0.75)GOTO 4                                                      HOSKEEP.285
      IF(SCR.GT.0.15)GOTO 5                                                      HOSKEEP.286
      C1CAP=14.0                                                                 HOSKEEP.287
      GOTO 6                                                                     HOSKEEP.288
    4 CONTINUE                                                                   HOSKEEP.289
      C1CAP=0.5                                                                  HOSKEEP.290
      GOTO 6                                                                     HOSKEEP.291
    5 CONTINUE                                                                   HOSKEEP.292
      C1CAP=14.-22.5*(SCR-0.15)                                                  HOSKEEP.293
    6 CONTINUE                                                                   HOSKEEP.294
C                                                                                HOSKEEP.295
C  COMPUTE SOIL PROPERTIES BASED ON EQS 37 AND 38 OF DEARDORFF.                  HOSKEEP.296
C                                                                                HOSKEEP.297
      RHOCG=(0.27+WG)*4.186E6                                                    HOSKEEP.298
      TL2=WG**0.33333                                                            HOSKEEP.299
      XLAMG=(0.001+0.004*TL2)*4.186E2                                            HOSKEEP.300
      D1G=SQRT(TAU1*XLAMG/RHOCG)                                                 HOSKEEP.301
C  COMPUTE EQS 39A AND 39B.                                                      HOSKEEP.302
      RP2=0.3+0.05*WG/W2BULK                                                     HOSKEEP.303
      IF(RP2.GT.1.0)RP2=1.0                                                      HOSKEEP.304
      RHOCSD1=RP2*RHOCG*D1G+(1.-RP2)*RHOC2*D12                                   HOSKEEP.305
C                                                                                HOSKEEP.306
C  COMPUTE THE COMPONENTS NECESSARY FOR THE GROUND                               HOSKEEP.307
C  SFC ENERGY BUDGET EQ                                                          HOSKEEP.308
C                                                                                HOSKEEP.309
      RLGUP=BOLTZ*SST*SST*SST*SST                                                HOSKEEP.310
      HSG=-RHOAT*CPD*THLWB(1)                                                    HOSKEEP.311
      EG=-RHOAT*WQWB(1)                                                          HOSKEEP.312
      IF(EG.LT.0.0)EG=0.0                                                        HOSKEEP.313
      GTOT=-(HSG+HEATL*EG-GSW(IRR,JRR)+EMISS(IRR,JRR)*(RLGUP-                    07NOV00.1670
     +     GLW(IRR,JRR)))                                                        07NOV00.1671
C                                                                                HOSKEEP.320
C  NOW COMPUTE NEW VALUES OF GROUND POTENTIAL TEMP.                              HOSKEEP.321
C  BY USING THE FORCE-RESTORE METHOD, USING EQ.8 REWRITTEN                       HOSKEEP.322
C  IN TERMS OF POTENTIAL TEMPERATURE. THE EXPRESSION                             HOSKEEP.323
C  IS FINITE-DIFFERENCED IMPLICITLY.                                             HOSKEEP.324
C                                                                                HOSKEEP.325
      PRATIO1=(PS/1000.)**0.286                                                  HOSKEEP.326
      TL1=(C1G*GTOT)/(PRATIO1*RHOCSD1)                                           HOSKEEP.327
      TL2=C2G*TH2/TAU1                                                           HOSKEEP.328
      TL3=1.0+C2G*DT/TAU1                                                        HOSKEEP.329
      THLZ=(THLZ+(TL1+TL2)*DT)/TL3                                               HOSKEEP.330
      SST=THLZ*(PS/1000.)**0.286                                                 HOSKEEP.331
      IF(IFSNOW(INEST).EQ.1.AND.SNOWC(IRR,JRR).GT.0.0)THEN                       07NOV00.1672
        SST=AMIN1(SST,273.16)                                                    07NOV00.1673
      ENDIF                                                                      07NOV00.1674
C     THLZ = SST*(1000./PS)**0.286                                               HOSKEEP.332
C     SST=THLZ*PRATIO(1)                                                         HOSKEEP.333
C     TL4=(C1CAP*EG)/(RHOW*D1P)                                                  HOSKEEP.334
C     TL5=C2CAP*W2BULK/TAU1                                                      HOSKEEP.335
C     TL6=-TL4+TL5                                                               HOSKEEP.336
C     WG=(WG+TL6*DT)/(1.+C2CAP*DT/TAU1)                                          HOSKEEP.337
C     IF(WG.GT.WMAX)WG=WMAX                                                      HOSKEEP.338
C     IF(WG.LE.0.0)WG=1.E-5                                                      HOSKEEP.339
C     GWWG=WG/WMAX                                                               HOSKEEP.340
   10 CONTINUE                                                                   HOSKEEP.341
      RETURN                                                                     HOSKEEP.342
      END                                                                        HOSKEEP.343
                                                                                 HOSKEEP.344
