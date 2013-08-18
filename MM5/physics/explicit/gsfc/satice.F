      SUBROUTINE SATICE (D2T,DPT,DQV,QCL,QRN,QCI,QCS,QCG,                        SATICE.1
     1    RHOP,TA1,QA1,P0P,PIP,VFR,VFS,VFG,PRECIP,ISTA,ILES,KLES)                SATICE.2
#     include <parame.incl>                                                      SATICE.3
      DIMENSION DPT(MIX,MKX),DQV(MIX,MKX),QCL(MIX,MKX),QRN(MIX,MKX),             SATICE.4
     1   QCI(MIX,MKX),QCS(MIX,MKX),QCG(MIX,MKX),PRECIP(MIX),                     SATICE.5
     2         RHOP(MIX,MKX),P0P(MIX,MKX),PIP(MIX,MKX),                          SATICE.6
     3         VFR(MIX,MKX),VFS(MIX,MKX),VFG(MIX,MKX)                            SATICE.7
      DIMENSION RHO(MIX),P0(MIX),PI(MIX),TA1(MKX),QA1(MKX)                       SATICE.8
      REAL NCI, MASSI                                                            SATICE.9
C                                                                                SATICE.10
C***********************************************************************         SATICE.11
C     LIN ET AL (83) ICE PHASE MICROPHYSICAL PROCESSES                 *         SATICE.12
C     MODIFIED AND CODED BY GODDARD CUMULUS ENSEMBLE MODELING GROUP    *         SATICE.13
C     TAO, SIMPSON AND MCCUMBER`S SATURATION TECHNIQUE (MWR, 1989)     *         SATICE.14
C     TAO AND SIMPSON (JAS, 1989; TAO, 1993),                          *         SATICE.15
C     BRAUN AND TAO (MWR, 2000), BRAUN ET AL. (JAM, 2001, submitted)   *         SATICE.16
C                                                                      *         SATICE.17
C     D2T - LEAPFROG TIME STEP (S)                                     *         SATICE.18
C     DPT - DEVIATION POTENTIAL TEMPERATURE FIELD (K)                  *         SATICE.19
C     DQV - DEVIATION WATER VAPOR FIELD (G/G)                          *         SATICE.20
C     QCL - CLOUD WATER FIELD (G/G)                                    *         SATICE.21
C     QRN - RAIN FIELD (G/G)                                           *         SATICE.22
C     QCI - CLOUD ICE FIELD (G/G)                                      *         SATICE.23
C     QCS - SNOW FIELD (G/G)                                           *         SATICE.24
C     QCG - HAIL FIELD (G/G)                                           *         SATICE.25
C     RHO - AIR DENSITY (G/CM3)                                        *         SATICE.26
C     TA1 - BASE AIR POTENTIAL TEMPERATURE AT THE LEVEL (K)            *         SATICE.27
C     QA1 - BASE WATER VAPOR AT THE LEVEL (G/G)                        *         SATICE.28
C     P0 - AIR PRESSURE (UB=1.E-3MB)                                   *         SATICE.29
C     PI - EXNER FUNCTION (P/1000.E3)**R/CP                            *         SATICE.30
C                                                                      *         SATICE.31
C     MIX: DIMENSION IN X-DIRECTION                                    *         SATICE.32
C     MKX: DIMENSION IN Z-DIRECTION                                    *         SATICE.33
C     NOTE: PHYSICAL DOMAIN EXTENDS FROM K=1 TO K=MKX-1                *         SATICE.34
C           AND I=2 TO I=MIX-1                                         *         SATICE.35
C     C.G.S UNITS                                                      *         SATICE.36
C                                                                      *         SATICE.37
C     THIS ICE SCHEME HAS BEEN IMPLEMENTED INTO                        *         SATICE.38
C     ARPS (CAPS - V. WONG, M. XUE, K. DROEGEMEIER) - 1993             *         SATICE.39
C     GODDARD`S SCHLENSINGER MODEL (SIMPSON ET AL., 1991)              *         SATICE.40
C     CHEN`S MODEL (C. CHEN) - 1992                                    *         SATICE.41
C     GMASS (BIAK ET AL., 1992)                                        *         SATICE.42
C     MM5V1 (LIU ET AL., 1994)                                         *         SATICE.43
C                                                                      *         SATICE.44
C     RECENT CHANGES (6.25.01) BY SCOTT BRAUN INCLUDE                  *         SATICE.45
C       * OPTION TO CHOOSE HAIL OR GRAUPEL AS THIRD ICE CATEGORY       *         SATICE.46
C       * OPTION OF SATURATION ADJUSTMENT SCHEMES                      *         SATICE.47
C       * NEW FORMULATION OF PSFI                                      *         SATICE.48
C       * REDUCED COLLECTION EFFICIENCIES (EGS) FOR COLLECTION OF SNOW *         SATICE.49
C         BY GRAUPEL                                                   *         SATICE.50
C       * MODIFICATIONS TO PIDEP, PINT INCLUDING NEW COEFFICIENTS FOR  *         SATICE.51
C         THE FLETCHER EQ., A LIMIT OF 1 CM**-3 FOR THE NUMBER         *         SATICE.52
C         CONCENTRATION OF ICE NUCLEI, INCLUSION OF A PISUB TERM       *         SATICE.53
C       * ELIMINATED THE PGAUT TERM                                    *         SATICE.54
C***********************************************************************         SATICE.55
C                                                                                SATICE.56
      COMMON/GSIZE/ TNW,TNSS,TNG,ROQR,ROQS,ROQG                                  SATICE.57
      COMMON/CONT/ C76,C358,C172,C409,C218,C580,C610,C149,C879,C141              SATICE.58
      COMMON/BTERV/ ZRC,ZGC,ZSC,VRC,VGC,VSC                                      SATICE.59
      COMMON/B3CS/ AGG,BGG,ASS,BSS,AWW,BWW,BGH,BGQ,BSH,BSQ,BWH,BWQ               SATICE.60
      COMMON/BSNW/ ALV,ALF,ALS,T0K,T00K,AVC,AFC,ASC,RN1,BND1,RN2,BND2,           SATICE.61
     1   RN3,RN4,RN5,RN6,RN7,RN8,RN9,RN10,RN101,RN10A,RN11,RN11A,                SATICE.62
     2   RN12,RN12A(31),RN12B(31),RN14,RN15,RN15A,RN16,RN17,                     SATICE.63
     3   RN17A,RN17B,RN17C,RN18,RN18A,RN19,RN19A,RN19B,RN20,RN20A,RN20B,         SATICE.64
     4   BND3,RN21,RN22,RN23,RN23A,RN23B,RN25,RN25A(31),RN30A,RN30B,             SATICE.65
     5   RN30C,RN31,BETA,RN32,AMI40,AMI50,AMI100,IHAIL,IWATER                    SATICE.66
C                                                                                SATICE.67
C     ABOVE CONSTANTS DEFINED IN SUBROUTINE CONSAT                               SATICE.68
C                                                                                SATICE.69
C     THE FOLLOWINGS ARE LOCALLY USED VARIABLES                                  SATICE.70
C                                                                                SATICE.71
      DIMENSION COL(MIX),DEP(MIX),RGMP(MIX),DD(MIX),DD1(MIX),                    SATICE.72
     & QVS(MIX),DM(MIX),RSUB1(MIX),WGACR(MIX),CND(MIX),RQ(MIX),                  SATICE.73
     & ERN(MIX),SCV(MIX),TCA(MIX),DWV(MIX),ZR(MIX),VR(MIX),                      SATICE.74
     & ZS(MIX),VS(MIX),ZG(MIX),VG(MIX),EGS(MIX),ESI(MIX),                        SATICE.75
     & QSI(MIX),SSI(MIX),QSW(MIX),SSW(MIX),PIHOM(MIX),PIDW(MIX),                 SATICE.76
     & PIMLT(MIX),PSAUT(MIX),PSACI(MIX),PSACW(MIX),QSACW(MIX),                   SATICE.77
     & PRACI(MIX),PIACR(MIX),PRAUT(MIX),PRACW(MIX),PSFW(MIX),                    SATICE.78
     & PSFI(MIX),DGACS(MIX),DGACW(MIX),DGACI(MIX),DGACR(MIX),                    SATICE.79
     & PGACS(MIX),WGACS(MIX),QGACW(MIX),WGACI(MIX),QGACR(MIX),                   SATICE.80
     & PGWET(MIX),PGAUT(MIX),PRACS(MIX),PSACR(MIX),QSACR(MIX),                   SATICE.81
     & PGFR(MIX),PSMLT(MIX),PGMLT(MIX),PSDEP(MIX),PSSUB(MIX),                    SATICE.82
     & PGSUB(MIX),PINT(MIX),PIDEP(MIX),PISUB(MIX)                                SATICE.83
      DIMENSION PT(MIX),QV(MIX),QC(MIX),QR(MIX),QI(MIX),QS(MIX),                 SATICE.84
     & QG(MIX),TAIR(MIX),TAIRC(MIX),PR(MIX),PS(MIX),PG(MIX),PRN(MIX),            SATICE.85
     & PSN(MIX),DLT1(MIX),DLT2(MIX),DLT3(MIX),RTAIR(MIX),DDA(MIX),               SATICE.86
     & DDB(MIX),Y1(MIX),Y2(MIX),Y3(MIX),Y4(MIX),Y5(MIX),FV(MIX),IT(MIX)          SATICE.87
                                                                                 SATICE.88
C     ******   TWO CLASS WATER AND THREE CLASS OF ICE-PHASE    *********         SATICE.89
      CP=1.003E7                                                                 SATICE.90
      DO 10 I=1,MIX                                                              SATICE.91
   10 IT(I)=1                                                                    SATICE.92
      cmin=1.e-19                                                                SATICE.93
      cmin1=1.e-12                                                               SATICE.94
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         SATICE.95
      RT0=1./(T0K-T00K)                                                          SATICE.96
      BW3=BWW+3.                                                                 SATICE.97
      BS3=BSS+3.                                                                 SATICE.98
      BG3=BGG+3.                                                                 SATICE.99
      BWH5=2.5+BWH                                                               SATICE.100
      BSH5=2.5+BSH                                                               SATICE.101
      BGH5=2.5+BGH                                                               SATICE.102
      BW6=BWW+6.                                                                 SATICE.103
      BS6=BSS+6.                                                                 SATICE.104
      BETAH=.5*BETA                                                              SATICE.105
      R10T=RN10*D2T                                                              SATICE.106
      R11AT=RN11A*D2T                                                            SATICE.107
      R19BT=RN19B*D2T                                                            SATICE.108
      R20T=-RN20*D2T                                                             SATICE.109
      R23T=-RN23*D2T                                                             SATICE.110
CC    ******************************************************************         SATICE.111
      DO 1000 K=1,KLES                                                           SATICE.112
        TB0=TA1(K)                                                               SATICE.113
        QB0=QA1(K)                                                               SATICE.114
        DO 100 I=ISTA,ILES                                                       SATICE.115
          fv(i)=rhop(i,1)                                                        SATICE.116
          RHO(I)=RHOP(I,K)                                                       SATICE.117
          PI(I)=PIP(I,K)                                                         SATICE.118
          P0(I)=P0P(I,K)                                                         SATICE.119
          PT(I)=DPT(I,K)                                                         SATICE.120
          QV(I)=DQV(I,K)                                                         SATICE.121
          QC(I)=QCL(I,K)                                                         SATICE.122
          QR(I)=QRN(I,K)                                                         SATICE.123
          QI(I)=QCI(I,K)                                                         SATICE.124
          QS(I)=QCS(I,K)                                                         SATICE.125
          QG(I)=QCG(I,K)                                                         SATICE.126
          IF(QV(I).LT.cmin1) QV(I)=cmin1                                         SATICE.127
          IF(QC(I).LT.cmin)  QC(I)=0.0                                           SATICE.128
          IF(QR(I).LT.cmin)  QR(I)=0.0                                           SATICE.129
          IF(QI(I).LT.cmin)  QI(I)=0.0                                           SATICE.130
          IF(QS(I).LT.cmin)  QS(I)=0.0                                           SATICE.131
          IF(QG(I).LT.cmin)  QG(I)=0.0                                           SATICE.132
          TAIR(I)=(PT(I)+TB0)*PI(I)                                              SATICE.133
          TAIRC(I)=TAIR(I)-T0K                                                   SATICE.134
C     ***   COMPUTE ZR,ZS,ZG,VR,VS,VG      *****************************         SATICE.135
          ZR(I)=1.E5*ZRC/RHO(I)**0.25                                            SATICE.136
          ZS(I)=1.E5*ZSC/RHO(I)**0.25                                            SATICE.137
          ZG(I)=1.E5*ZGC/RHO(I)**0.25                                            SATICE.138
          VR(I)=0.0                                                              SATICE.139
          VS(I)=0.0                                                              SATICE.140
          VG(I)=0.0                                                              SATICE.141
          IF(QR(I).GT.cmin)THEN                                                  SATICE.142
            DD(I)=RHO(I)*QR(I)                                                   SATICE.143
            Y1(I)=DD(I)**.25                                                     SATICE.144
            ZR(I)=ZRC/Y1(I)                                                      SATICE.145
            VR(I)=VRC*SQRT(FV(I)/RHO(I))*DD(I)**BWQ                              SATICE.146
          ENDIF                                                                  SATICE.147
          IF(QS(I).GT.cmin)THEN                                                  SATICE.148
            DD(I)=RHO(I)*QS(I)                                                   SATICE.149
            Y1(I)=DD(I)**.25                                                     SATICE.150
            ZS(I)=ZSC/Y1(I)                                                      SATICE.151
            VS(I)=VSC*SQRT(FV(I)/RHO(I))*DD(I)**BSQ                              SATICE.152
          ENDIF                                                                  SATICE.153
            vgcr=vgc/sqrt(rho(i))                                                SATICE.154
            vgcf=vgc*sqrt(fv(i)/rho(i))                                          SATICE.155
          if (qg(i) .gt. cmin) then                                              SATICE.156
             dd(i)=rho(i)*qg(i)                                                  SATICE.157
              y1(i)=dd(i)**.25                                                   SATICE.158
            zg(i)=zgc/y1(i)                                                      SATICE.159
            if (ihail .eq. 1) then                                               SATICE.160
             vg(i)=vgcr*dd(i)**bgq                                               SATICE.161
            else                                                                 SATICE.162
             vg(i)=vgcf*dd(i)**bgq                                               SATICE.163
            endif                                                                SATICE.164
          endif                                                                  SATICE.165
          VFR(I,K)=VR(I)                                                         SATICE.166
          VFS(I,K)=VS(I)                                                         SATICE.167
          VFG(I,K)=VG(I)                                                         SATICE.168
C     ******************************************************************         SATICE.169
C     ***   Y1 : DYNAMIC VISCOSITY OF AIR (U)                                    SATICE.170
C     ***   DWV : DIFFUSIVITY OF WATER VAPOR IN AIR (PI)                         SATICE.171
C     ***   TCA : THERMAL CONDUCTIVITY OF AIR (KA)                               SATICE.172
C     ***   Y2 : KINETIC VISCOSITY (V)                                           SATICE.173
          Y1(I)=C149*TAIR(I)**1.5/(TAIR(I)+120.)                                 SATICE.174
          DWV(I)=C879/P0(I)*TAIR(I)**1.81                                        SATICE.175
          TCA(I)=C141*Y1(I)                                                      SATICE.176
          SCV(I)=1./((Y1(I)/RHO(I))**.1666667*DWV(I)**.3333333)                  SATICE.177
  100   CONTINUE                                                                 SATICE.178
C*  1 * PSAUT : AUTOCONVERSION OF QI TO QS                        ***1**         SATICE.179
C*  3 * PSACI : ACCRETION OF QI TO QS                             ***3**         SATICE.180
C*  4 * PSACW : ACCRETION OF QC BY QS (RIMING) (QSACW FOR PSMLT)  ***4**         SATICE.181
C*  5 * PRACI : ACCRETION OF QI BY QR                             ***5**         SATICE.182
C*  6 * PIACR : ACCRETION OF QR OR QG BY QI                       ***6**         SATICE.183
        DO 125 I=ISTA,ILES                                                       SATICE.184
          PSAUT(I)=0.0                                                           SATICE.185
          PSACI(I)=0.0                                                           SATICE.186
          PRACI(I)=0.0                                                           SATICE.187
          PIACR(I)=0.0                                                           SATICE.188
          PSACW(I)=0.0                                                           SATICE.189
          QSACW(I)=0.0                                                           SATICE.190
          PRAUT(I)=0.0                                                           SATICE.191
          PRACW(I)=0.0                                                           SATICE.192
          r3456=sqrt(fv(i)/rho(i))                                               SATICE.193
          DD(I)=1./ZS(I)**BS3                                                    SATICE.194
          IF(QI(I).GT.CMIN) THEN                                                 SATICE.195
           IF(TAIR(I).LT.T0K)THEN                                                SATICE.196
             ESI(I)=EXP(.025*TAIRC(I))                                           SATICE.197
             PSAUT(I)=AMAX1(RN1*ESI(I)*(QI(I)-BND1),0.0)                         SATICE.198
             PSACI(I)=RN3*R3456*.1*QI(I)*DD(I)                                   SATICE.199
             PRACI(I)=RN5*R3456*QI(I)*ZR(I)**(-BW3)                              SATICE.200
             PIACR(I)=RN6*R3456*QI(I)*ZR(I)**(-BW6)                              SATICE.201
           ENDIF                                                                 SATICE.202
          ENDIF                                                                  SATICE.203
          IF(QC(I).GT.CMIN) THEN                                                 SATICE.204
            IF(TAIR(I).LT.T0K)THEN                                               SATICE.205
              PSACW(I)=RN4*R3456*QC(I)*DD(I)                                     SATICE.206
            ELSE                                                                 SATICE.207
              QSACW(I)=RN4*R3456*QC(I)*DD(I)                                     SATICE.208
            ENDIF                                                                SATICE.209
C* 21 * PRAUT : AUTOCONVERSION OF QC TO QR                        **21**         SATICE.210
C* 22 * PRACW : ACCRETION OF QC BY QR                             **22**         SATICE.211
           PRACW(I)=RN22*SQRT(FV(I)/RHO(I))*QC(I)/ZR(I)**BW3                     SATICE.212
           Y1(I)=QC(I)-BND3                                                      SATICE.213
           IF(Y1(I).GT.cmin)THEN                                                 SATICE.214
             PRAUT(I)=RHO(I)*Y1(I)*Y1(I)/(1.2E-4+RN21/Y1(I))                     SATICE.215
           ENDIF                                                                 SATICE.216
          ENDIF                                                                  SATICE.217
C* 12 * PSFW : BERGERON PROCESSES FOR QS (KOENING, 1971)          **12**         SATICE.218
C* 13 * PSFI : BERGERON PROCESSES FOR QI                          **13**         SATICE.219
C* 32 * PIDEP : DEPOSITION OF QI                                  **32**         SATICE.220
          PSFW(I)=0.0                                                            SATICE.221
          PSFI(I)=0.0                                                            SATICE.222
          PIDEP(I)=0.0                                                           SATICE.223
          PISUB(I)=0.0                                                           SATICE.224
          IF(TAIR(I).LT.T0K.AND.QI(I).GT.cmin)THEN                               SATICE.225
            Y1(I)=AMAX1(AMIN1(TAIRC(I),-1.),-31.)                                SATICE.226
            IT(I)=INT(ABS(Y1(I)))                                                SATICE.227
            Y1(I)=RN12A(IT(I))                                                   SATICE.228
            Y2(I)=RN12B(IT(I))                                                   SATICE.229
            PSFW(I)=AMAX1(D2T*Y1(I)*(RN12*RHO(I)*QC(I))*QI(I),0.0)               SATICE.230
            RTAIR(I)=1./(TAIR(I)-C76)                                            SATICE.231
            Y2(I)=EXP(C218-C580*RTAIR(I))                                        SATICE.232
            QSI(I)=3799.1/P0(I)*Y2(I)                                            SATICE.233
            ESI(I)=C610*Y2(I)                                                    SATICE.234
            SSI(I)=(QV(I)+QB0)/QSI(I)-1.                                         SATICE.235
            NCI=AMIN1(RN25*EXP(BETA*TAIRC(I)),1.)                                SATICE.236
            DM(I)=QV(I)+QB0-QSI(I)                                               SATICE.237
            RSUB1(I)=C580*ASC*QSI(I)*RTAIR(I)*RTAIR(I)                           SATICE.238
            Y3(I)=1./TAIR(I)                                                     SATICE.239
            DD(I)=Y3(I)*(RN30A*Y3(I)-RN30B)+RN30C*TAIR(I)/ESI(I)                 SATICE.240
            Y1(I)=206.18*SSI(I)/DD(I)                                            SATICE.241
            DEP(I)=DM(I)/(1.+RSUB1(I))/D2T                                       SATICE.242
            PIDEP(I)=Y1(I)*SQRT(NCI*QI(I)/RHO(I))                                SATICE.243
            IF(DM(I).GT.cmin) THEN                                               SATICE.244
               a2=1.                                                             SATICE.245
               if(pidep(i).gt.dep(i) .and. pidep(i) .gt. cmin) then              SATICE.246
                  a2=dep(i)/pidep(i)                                             SATICE.247
                  pidep(i)=dep(i)                                                SATICE.248
               endif                                                             SATICE.249
               PSFI(I)=a2*.5*QI(I)*Y1(I)/(SQRT(AMI100)-SQRT(AMI40))              SATICE.250
            ELSEIF(DM(I).LT.-cmin) THEN                                          SATICE.251
               PISUB(I)=AMAX1(-QI(I)/D2T,PIDEP(I))                               SATICE.252
               PISUB(I)=AMIN1(-PISUB(I),-DM(I)/D2T)                              SATICE.253
               PIDEP(I)=0.                                                       SATICE.254
               PSFI(I)=0.                                                        SATICE.255
            ELSE                                                                 SATICE.256
               PISUB(I)=0.                                                       SATICE.257
               PIDEP(I)=0.                                                       SATICE.258
               PSFI(I)=0.                                                        SATICE.259
            ENDIF                                                                SATICE.260
          ENDIF                                                                  SATICE.261
CTTT***** QG=QG+MIN(PGDRY,PGWET)                                                 SATICE.262
C*  9 * PGACS : ACCRETION OF QS BY QG (DGACS,WGACS: DRY AND WET)  ***9**         SATICE.263
C* 14 * DGACW : ACCRETION OF QC BY QG (QGACW FOR PGMLT)           **14**         SATICE.264
C* 16 * DGACR : ACCRETION OF QR TO QG (QGACR FOR PGMLT)           **16**         SATICE.265
          DGACS(I)=0.0                                                           SATICE.266
          WGACS(I)=0.0                                                           SATICE.267
          DGACW(I)=0.0                                                           SATICE.268
          DGACR(I)=0.0                                                           SATICE.269
          PGACS(I)=0.0                                                           SATICE.270
          QGACW(I)=0.0                                                           SATICE.271
          QGACR(I)=0.0                                                           SATICE.272
          DGACI(I)=0.0                                                           SATICE.273
          WGACI(I)=0.0                                                           SATICE.274
          PGWET(I)=0.0                                                           SATICE.275
        IF(QG(I).GT.CMIN)THEN                                                    SATICE.276
                                                                                 SATICE.277
         IF(QC(I)+QR(I).LT.1.e-4) THEN                                           SATICE.278
             EE1=.01                                                             SATICE.279
          ELSE                                                                   SATICE.280
             EE1=1.                                                              SATICE.281
          ENDIF                                                                  SATICE.282
          EE2=0.09                                                               SATICE.283
          EGS(I)=EE1*EXP(EE2*TAIRC(I))                                           SATICE.284
          IF(TAIR(I).GE.T0K)EGS(I)=1.0                                           SATICE.285
          Y1(I)=ABS(VG(I)-VS(I))                                                 SATICE.286
          Y2(I)=ZS(I)*ZG(I)                                                      SATICE.287
          Y3(I)=5./Y2(I)                                                         SATICE.288
          Y4(I)=.08*Y3(I)*Y3(I)                                                  SATICE.289
          Y5(I)=.05*Y3(I)*Y4(I)                                                  SATICE.290
          DD(I)=Y1(I)*(Y3(I)/ZS(I)**5+Y4(I)/ZS(I)**3+Y5(I)/ZS(I))                SATICE.291
          PGACS(I)=RN9/RHO(I)*EGS(I)*DD(I)                                       SATICE.292
          DGACS(I)=PGACS(I)                                                      SATICE.293
          WGACS(I)=RN9/RHO(I)*DD(I)                                              SATICE.294
          Y1(I)=1./ZG(I)**BG3                                                    SATICE.295
          IF(IHAIL.EQ.1) THEN                                                    SATICE.296
             DGACW(I)=AMAX1(RN14*QC(I)*Y1(I)/SQRT(RHO(I)),0.0)                   SATICE.297
          ELSE                                                                   SATICE.298
             DGACW(I)=AMAX1(RN14*QC(I)*Y1(I)*R3456,0.0)                          SATICE.299
          ENDIF                                                                  SATICE.300
          QGACW(I)=DGACW(I)                                                      SATICE.301
          Y1(I)=ABS(VG(I)-VR(I))                                                 SATICE.302
          Y2(I)=ZR(I)*ZG(I)                                                      SATICE.303
          Y3(I)=5./Y2(I)                                                         SATICE.304
          Y4(I)=.08*Y3(I)*Y3(I)                                                  SATICE.305
          Y5(I)=.05*Y3(I)*Y4(I)                                                  SATICE.306
          DD(I)=Y1(I)*(Y3(I)/ZR(I)**5+Y4(I)/ZR(I)**3+Y5(I)/ZR(I))*RN16/          SATICE.307
     +          RHO(I)                                                           SATICE.308
          DGACR(I)=AMAX1(DD(I),0.0)                                              SATICE.309
          QGACR(I)=DGACR(I)                                                      SATICE.310
          IF(TAIR(I).GE.T0K)THEN                                                 SATICE.311
            DGACS(I)=0.0                                                         SATICE.312
            WGACS(I)=0.0                                                         SATICE.313
            DGACW(I)=0.0                                                         SATICE.314
            DGACR(I)=0.0                                                         SATICE.315
          ELSE                                                                   SATICE.316
            PGACS(I)=0.0                                                         SATICE.317
            QGACW(I)=0.0                                                         SATICE.318
            QGACR(I)=0.0                                                         SATICE.319
          ENDIF                                                                  SATICE.320
C*******PGDRY : DGACW+DGACI+DGACR+DGACS                           ******         SATICE.321
C* 15 * DGACI : ACCRETION OF QI BY QG (WGACI FOR WET GROWTH)      **15**         SATICE.322
C* 17 * PGWET : WET GROWTH OF QG                                  **17**         SATICE.323
          IF(TAIR(I).LT.T0K.AND.TAIR(I).GT.T0K-40.)THEN                          SATICE.324
            Y1(I)=QI(I)/ZG(I)**BG3                                               SATICE.325
            IF(IHAIL.EQ.1) THEN                                                  SATICE.326
               DGACI(I)=Y1(I)*RN15/SQRT(RHO(I))                                  SATICE.327
               WGACI(I)=Y1(I)*RN15A/SQRT(RHO(I))                                 SATICE.328
            ELSE                                                                 SATICE.329
               DGACI(I)=Y1(I)*RN15*R3456                                         SATICE.330
               WGACI(I)=Y1(I)*RN15A*R3456                                        SATICE.331
            ENDIF                                                                SATICE.332
            Y1(I)=1./(ALF+RN17C*TAIRC(I))                                        SATICE.333
            IF(IHAIL.EQ.1) THEN                                                  SATICE.334
               Y3(I)=.78/ZG(I)**2+SCV(I)*RN17A/RHO(I)**0.25/ZG(I)**BGH5          SATICE.335
            ELSE                                                                 SATICE.336
               Y3(I)=.78/ZG(I)**2+SCV(I)*RN17A*SQRT(R3456) /ZG(I)**BGH5          SATICE.337
            ENDIF                                                                SATICE.338
            Y4(I)=RHO(I)*ALV*DWV(I)*(3799.1/P0(I)-(QV(I)+QB0))-TCA(I)*           SATICE.339
     +            TAIRC(I)                                                       SATICE.340
            DD(I)=Y1(I)*(Y4(I)*Y3(I)*RN17/RHO(I)+(WGACI(I)+WGACS(I))*(           SATICE.341
     +            ALF+RN17B*TAIRC(I)))                                           SATICE.342
            PGWET(I)=AMAX1(DD(I),0.0)                                            SATICE.343
          ENDIF                                                                  SATICE.344
        ENDIF                                                                    SATICE.345
  125   CONTINUE                                                                 SATICE.346
C********   HANDLING THE NEGATIVE CLOUD WATER (QC)    ******************         SATICE.347
C********   HANDLING THE NEGATIVE CLOUD ICE (QI)      ******************         SATICE.348
C********   DIFFERENT FROM LIN ET AL                  ******************         SATICE.349
        DO 150 I=ISTA,ILES                                                       SATICE.350
c                                                                                SATICE.351
           y1(i)=qc(i)/d2t                                                       SATICE.352
          psacw(i)=AMIN1(y1(i), psacw(i))                                        SATICE.353
          praut(i)=AMIN1(y1(i), praut(i))                                        SATICE.354
          pracw(i)=AMIN1(y1(i), pracw(i))                                        SATICE.355
          psfw(i)= AMIN1(y1(i), psfw(i))                                         SATICE.356
          dgacw(i)=AMIN1(y1(i), dgacw(i))                                        SATICE.357
          qsacw(i)=AMIN1(y1(i), qsacw(i))                                        SATICE.358
          qgacw(i)=AMIN1(y1(i), qgacw(i))                                        SATICE.359
                                                                                 SATICE.360
          Y1(I)=(PSACW(I)+PRAUT(I)+PRACW(I)+PSFW(I)+DGACW(I)+QSACW(I)+           SATICE.361
     +          QGACW(I))*D2T                                                    SATICE.362
          QC(I)=QC(I)-Y1(I)                                                      SATICE.363
                                                                                 SATICE.364
          IF(QC(I).LT.0.0) THEN                                                  SATICE.365
             a1=1.                                                               SATICE.366
              if (y1(i) .ne. 0.0) A1=QC(I)/Y1(I)+1.                              SATICE.367
            PSACW(I)=PSACW(I)*A1                                                 SATICE.368
            PRAUT(I)=PRAUT(I)*A1                                                 SATICE.369
            PRACW(I)=PRACW(I)*A1                                                 SATICE.370
            PSFW(I)=PSFW(I)*A1                                                   SATICE.371
            DGACW(I)=DGACW(I)*A1                                                 SATICE.372
            QSACW(I)=QSACW(I)*A1                                                 SATICE.373
            QGACW(I)=QGACW(I)*A1                                                 SATICE.374
            QC(I)=0.0                                                            SATICE.375
          ENDIF                                                                  SATICE.376
c                                                                                SATICE.377
C******** SHED PROCESS (WGACR=PGWET-DGACW-WGACI-WGACS)                           SATICE.378
c     CALCULATIONS OF THIS TERM HAS BEEN MOVED TO THIS LOCATION TO               SATICE.379
C     ACCOUNT FOR RESCALING OF THE DGACW TERM ABOVE. ALTHOUGH RESCALING          SATICE.380
C     OF THE PGWET, WGACI AND WGACS TERMS OCCURS BELOW, THESE CHANGES            SATICE.381
C     ARE EXPECTED TO BE LESS THAN THAT ASSOCIATED WITH DGACW. THIS CALC         SATICE.382
C     IS NOT DONE AFTER RESCALING OF ALL TERMS SINCE WGACR IS NEEDED BEL         SATICE.383
C     FOR RAIN AND WGACI AND DGACI ARE NEEDED FOR ICE.                           SATICE.384
          WGACR(I)=PGWET(I)-DGACW(I)-WGACI(I)-WGACS(I)                           SATICE.385
          Y2(I)=DGACW(I)+DGACI(I)+DGACR(I)+DGACS(I)                              SATICE.386
          IF(PGWET(I).GE.Y2(I))THEN                                              SATICE.387
            WGACR(I)=0.0                                                         SATICE.388
            WGACI(I)=0.0                                                         SATICE.389
            WGACS(I)=0.0                                                         SATICE.390
          ELSE                                                                   SATICE.391
            DGACR(I)=0.0                                                         SATICE.392
            DGACI(I)=0.0                                                         SATICE.393
            DGACS(I)=0.0                                                         SATICE.394
          ENDIF                                                                  SATICE.395
                                                                                 SATICE.396
            y1(i)=qi(i)/d2t                                                      SATICE.397
           psaut(i)=AMIN1(y1(i), psaut(i))                                       SATICE.398
           psaci(i)=AMIN1(y1(i), psaci(i))                                       SATICE.399
           praci(i)=AMIN1(y1(i), praci(i))                                       SATICE.400
           psfi(i)= AMIN1(y1(i), psfi(i))                                        SATICE.401
           dgaci(i)=AMIN1(y1(i), dgaci(i))                                       SATICE.402
           wgaci(i)=AMIN1(y1(i), wgaci(i))                                       SATICE.403
c                                                                                SATICE.404
          Y1(I)=(PSAUT(I)+PSACI(I)+PRACI(I)+PSFI(I)+DGACI(I)+WGACI(I)            SATICE.405
     *          +PISUB(I))*D2T                                                   SATICE.406
                                                                                 SATICE.407
           qi(i)=qi(i)-y1(i)+PIDEP(I)*D2T                                        SATICE.408
           if(qi(i).lt.0.0) then                                                 SATICE.409
               a2=1.                                                             SATICE.410
                if (y1(i) .ne. 0.0) a2=qi(i)/y1(i)+1.                            SATICE.411
            psaut(i)=psaut(i)*a2                                                 SATICE.412
            psaci(i)=psaci(i)*a2                                                 SATICE.413
            praci(i)=praci(i)*a2                                                 SATICE.414
            psfi(i)=psfi(i)*a2                                                   SATICE.415
            dgaci(i)=dgaci(i)*a2                                                 SATICE.416
            wgaci(i)=wgaci(i)*a2                                                 SATICE.417
            pisub(i)=pisub(i)*a2                                                 SATICE.418
            qi(i)=0.0                                                            SATICE.419
           endif                                                                 SATICE.420
c                                                                                SATICE.421
          DLT3(I)=0.0                                                            SATICE.422
          DLT2(I)=0.0                                                            SATICE.423
          IF(TAIR(I).LT.T0K)THEN                                                 SATICE.424
            IF(QR(I).LT.1.E-4)THEN                                               SATICE.425
              DLT3(I)=1.0                                                        SATICE.426
              DLT2(I)=1.0                                                        SATICE.427
            ENDIF                                                                SATICE.428
            IF(QS(I).GE.1.E-4)DLT2(I)=0.0                                        SATICE.429
          ENDIF                                                                  SATICE.430
          PR(I)=(QSACW(I)+PRAUT(I)+PRACW(I)+QGACW(I))*D2T                        SATICE.431
          PS(I)=(PSAUT(I)+PSACI(I)+PSACW(I)+PSFW(I)+PSFI(I)+DLT3(I)*             SATICE.432
     +          PRACI(I))*D2T                                                    SATICE.433
          PG(I)=((1.-DLT3(I))*PRACI(I)+DGACI(I)+WGACI(I)+DGACW(I))*D2T           SATICE.434
  150   CONTINUE                                                                 SATICE.435
C*  7 * PRACS : ACCRETION OF QS BY QR                             ***7**         SATICE.436
C*  8 * PSACR : ACCRETION OF QR BY QS (QSACR FOR PSMLT)           ***8**         SATICE.437
        DO 175 I=ISTA,ILES                                                       SATICE.438
            PRACS(I)=0.0                                                         SATICE.439
            PSACR(I)=0.0                                                         SATICE.440
            QSACR(I)=0.0                                                         SATICE.441
            PGFR(I)=0.0                                                          SATICE.442
            PGAUT(I)=0.0                                                         SATICE.443
        IF(QR(I).GT.CMIN)THEN                                                    SATICE.444
          Y1(I)=ABS(VR(I)-VS(I))                                                 SATICE.445
          Y2(I)=ZR(I)*ZS(I)                                                      SATICE.446
          Y3(I)=5./Y2(I)                                                         SATICE.447
          Y4(I)=.08*Y3(I)*Y3(I)                                                  SATICE.448
          Y5(I)=.05*Y3(I)*Y4(I)                                                  SATICE.449
          R7R=RN7/RHO(I)                                                         SATICE.450
          PRACS(I)=R7R*Y1(I)*(Y3(I)/ZS(I)**5+Y4(I)/ZS(I)**3+Y5(I)/ZS(I))         SATICE.451
          R8R=RN8/RHO(I)                                                         SATICE.452
          PSACR(I)=R8R*Y1(I)*(Y3(I)/ZR(I)**5+Y4(I)/ZR(I)**3+Y5(I)/ZR(I))         SATICE.453
          QSACR(I)=PSACR(I)                                                      SATICE.454
          IF(TAIR(I).GE.T0K)THEN                                                 SATICE.455
            PRACS(I)=0.0                                                         SATICE.456
            PSACR(I)=0.0                                                         SATICE.457
          ELSE                                                                   SATICE.458
            QSACR(I)=0.0                                                         SATICE.459
          ENDIF                                                                  SATICE.460
C*  2 * PGAUT : AUTOCONVERSION OF QS TO QG                        ***2**         SATICE.461
C* 18 * PGFR  : FREEZING OF QR TO QG                              **18**         SATICE.462
          IF(TAIR(I).LT.T0K)THEN                                                 SATICE.463
C           Y1(I)=EXP(.09*TAIRC(I))                                              SATICE.464
C           if (ihail .eq. 1) PGAUT(I)=AMAX1(RN2*Y1(I)*(QS(I)-BND2),0.0)         SATICE.465
            Y2(I)=EXP(RN18A*(T0K-TAIR(I)))                                       SATICE.466
            PGFR(I)=AMAX1((Y2(I)-1.)*RN18/RHO(I)*ZR(I)**(-7),0.0)                SATICE.467
          ENDIF                                                                  SATICE.468
        ENDIF                                                                    SATICE.469
  175   CONTINUE                                                                 SATICE.470
C********   HANDLING THE NEGATIVE RAIN WATER (QR)    *******************         SATICE.471
C********   HANDLING THE NEGATIVE SNOW (QS)          *******************         SATICE.472
C********   DIFFERENT FROM LIN ET AL                 *******************         SATICE.473
        DO 200 I=ISTA,ILES                                                       SATICE.474
          y1(i)=qr(i)/d2t                                                        SATICE.475
          Y2(I)=-QG(I)/D2T                                                       SATICE.476
         piacr(i)=AMIN1(y1(i), piacr(i))                                         SATICE.477
         dgacr(i)=AMIN1(y1(i), dgacr(i))                                         SATICE.478
         wgacr(i)=AMIN1(y1(i), wgacr(i))                                         SATICE.479
         wgacr(i)=AMAX1(y2(i), wgacr(i))                                         SATICE.480
         psacr(i)=AMIN1(y1(i), psacr(i))                                         SATICE.481
         pgfr(i)= AMIN1(y1(i), pgfr(i))                                          SATICE.482
c                                                                                SATICE.483
         del=0.                                                                  SATICE.484
         IF(WGACR(I).LT.0.) del=1.                                               SATICE.485
          Y1(I)=(PIACR(I)+DGACR(I)+(1.-del)*WGACR(I)+PSACR(I)+                   SATICE.486
     *            PGFR(I))*D2T                                                   SATICE.487
          qr(i)=qr(i)+pr(i)-y1(i)-del*WGACR(I)*D2T                               SATICE.488
                                                                                 SATICE.489
          if(qr(i).lt.0.0) then                                                  SATICE.490
            a1=1.0                                                               SATICE.491
             if(y1(i) .ne. 0.) a1=qr(i)/y1(i)+1.                                 SATICE.492
            piacr(i)=piacr(i)*a1                                                 SATICE.493
            dgacr(i)=dgacr(i)*a1                                                 SATICE.494
            if(wgacr(i).gt.0) wgacr(i)=wgacr(i)*a1                               SATICE.495
            pgfr(i)=pgfr(i)*a1                                                   SATICE.496
            psacr(i)=psacr(i)*a1                                                 SATICE.497
            qr(i)=0.0                                                            SATICE.498
          endif                                                                  SATICE.499
c                                                                                SATICE.500
          PRN(I)=D2T*((1.-DLT3(I))*PIACR(I)+DGACR(I)+WGACR(I)+(1.-               SATICE.501
     +           DLT2(I))*PSACR(I)+PGFR(I))                                      SATICE.502
          PS(I)=PS(I)+D2T*(DLT3(I)*PIACR(I)+DLT2(I)*PSACR(I))                    SATICE.503
c                                                                                SATICE.504
           y1(i)=qs(i)/d2t                                                       SATICE.505
          pgacs(i)=AMIN1(y1(i), pgacs(i))                                        SATICE.506
          dgacs(i)=AMIN1(y1(i), dgacs(i))                                        SATICE.507
          wgacs(i)=AMIN1(y1(i), wgacs(i))                                        SATICE.508
          pgaut(i)=AMIN1(y1(i), pgaut(i))                                        SATICE.509
          pracs(i)=AMIN1(y1(i), pracs(i))                                        SATICE.510
c                                                                                SATICE.511
          PRACS(I)=(1.-DLT2(I))*PRACS(I)                                         SATICE.512
          PSN(I)=D2T*(PGACS(I)+DGACS(I)+WGACS(I)+PGAUT(I)+PRACS(I))              SATICE.513
          QS(I)=QS(I)+PS(I)-PSN(I)                                               SATICE.514
          if(qs(i) .lt. 0.0) then                                                SATICE.515
            a2=1.                                                                SATICE.516
              if(psn(i) .ne. 0.) a2=qs(i)/psn(i)+1.                              SATICE.517
            pgacs(i)=pgacs(i)*a2                                                 SATICE.518
            dgacs(i)=dgacs(i)*a2                                                 SATICE.519
            wgacs(i)=wgacs(i)*a2                                                 SATICE.520
            pgaut(i)=pgaut(i)*a2                                                 SATICE.521
            pracs(i)=pracs(i)*a2                                                 SATICE.522
            psn(i)=psn(i)*a2                                                     SATICE.523
            qs(i)=0.0                                                            SATICE.524
          endif                                                                  SATICE.525
          Y2(I)=D2T*(PSACW(I)+PSFW(I)+DGACW(I)+PIACR(I)+DGACR(I)+                SATICE.526
     +          WGACR(I)+PSACR(I)+PGFR(I))                                       SATICE.527
          PT(I)=PT(I)+AFC/PI(I)*Y2(I)                                            SATICE.528
          QG(I)=QG(I)+PG(I)+PRN(I)+PSN(I)                                        SATICE.529
  200   CONTINUE                                                                 SATICE.530
C* 11 * PSMLT : MELTING OF QS                                     **11**         SATICE.531
C* 19 * PGMLT : MELTING OF QG TO QR                               **19**         SATICE.532
        DO 225 I=ISTA,ILES                                                       SATICE.533
          PSMLT(I)=0.0                                                           SATICE.534
          PGMLT(I)=0.0                                                           SATICE.535
          TAIR(I)=(PT(I)+TB0)*PI(I)                                              SATICE.536
          r3456=sqrt(fv(i)/rho(i))                                               SATICE.537
          IF(TAIR(I).GE.T0K .AND. (QS(I)+QG(I)).GT.CMIN) THEN                    SATICE.538
            TAIRC(I)=TAIR(I)-T0K                                                 SATICE.539
            Y1(I)=TCA(I)*TAIRC(I)-RHO(I)*ALV*DWV(I)*(3799.1/P0(I)-(QV(I)         SATICE.540
     +            +QB0))                                                         SATICE.541
            Y2(I)=.78/ZS(I)**2+RN101*(FV(I)/RHO(I))**0.25*SCV(I)/ZS(I)**         SATICE.542
     +            BSH5                                                           SATICE.543
            R11RT=RN11*D2T/RHO(I)                                                SATICE.544
            DD(I)=R11RT*Y1(I)*Y2(I)+R11AT*TAIRC(I)*(QSACW(I)+QSACR(I))           SATICE.545
            PSMLT(I)=AMAX1(0.0,AMIN1(DD(I),QS(I)))                               SATICE.546
            IF(IHAIL.EQ.1) THEN                                                  SATICE.547
               Y3(I)=.78/ZG(I)**2+SCV(I)*RN19A/RHO(I)**0.25/ZG(I)**BGH5          SATICE.548
            ELSE                                                                 SATICE.549
               Y3(I)=.78/ZG(I)**2+SCV(I)*RN19A*SQRT(R3456) /ZG(I)**BGH5          SATICE.550
            ENDIF                                                                SATICE.551
            DD1(I)=Y1(I)*Y3(I)*RN19*D2T/RHO(I)+R19BT*TAIRC(I)*(QGACW(I)+         SATICE.552
     +             QGACR(I))                                                     SATICE.553
            PGMLT(I)=AMAX1(0.0,AMIN1(DD1(I),QG(I)))                              SATICE.554
            PT(I)=PT(I)-AFC/PI(I)*(PSMLT(I)+PGMLT(I))                            SATICE.555
            QR(I)=QR(I)+PSMLT(I)+PGMLT(I)                                        SATICE.556
            QS(I)=QS(I)-PSMLT(I)                                                 SATICE.557
            QG(I)=QG(I)-PGMLT(I)                                                 SATICE.558
          ENDIF                                                                  SATICE.559
C* 24 * PIHOM : HOMOGENEOUS FREEZING OF QC TO QI (T < T00K)    **24**            SATICE.560
C* 25 * PIDW : DEPOSITION GROWTH OF QC TO QI ( T0K < T <= T00K)**25**            SATICE.561
C* 26 * PIMLT : MELTING OF QI TO QC (T >= T0K)                 **26**            SATICE.562
          IF(QC(I).LE.cmin)QC(I)=0.0                                             SATICE.563
          IF(QI(I).LE.cmin)QI(I)=0.0                                             SATICE.564
          PIHOM(I)=0.                                                            SATICE.565
          PIMLT(I)=0.                                                            SATICE.566
          TAIR(I)=(PT(I)+TB0)*PI(I)                                              SATICE.567
          IF((TAIR(I)-T00K).LE.0.) PIHOM(I)=QC(I)                                SATICE.568
          IF((TAIR(I)-T0K) .GE.0.) PIMLT(I)=QI(I)                                SATICE.569
          PIDW(I)=0.0                                                            SATICE.570
          IF(TAIR(I).LT.T0K.AND.TAIR(I).GT.T00K.AND.QC(I).GT.CMIN)THEN           SATICE.571
            TAIRC(I)=TAIR(I)-T0K                                                 SATICE.572
            Y1(I)=AMAX1(AMIN1(TAIRC(I),-1.),-31.)                                SATICE.573
            Y2(I)=RN25A(INT(ABS(Y1(I))))                                         SATICE.574
            Y3(I)=AMIN1(RN25*EXP(BETA*TAIRC(I)),1.)                              SATICE.575
            PIDW(I)=AMIN1(Y2(I)*Y3(I)*D2T/RHO(I),QC(I))                          SATICE.576
          ENDIF                                                                  SATICE.577
          Y1(I)=PIHOM(I)-PIMLT(I)+PIDW(I)                                        SATICE.578
          PT(I)=PT(I)+AFC/PI(I)*Y1(I)+ASC/PI(I)*(PIDEP(I)-                       SATICE.579
     +                PISUB(I))*D2T                                              SATICE.580
          QV(I)=QV(I)-(PIDEP(I)-PISUB(I))*D2T                                    SATICE.581
          QC(I)=QC(I)-Y1(I)                                                      SATICE.582
          QI(I)=QI(I)+Y1(I)                                                      SATICE.583
C* 31 * PINT  : INITIATION OF QI                                  **31**         SATICE.584
C****** DIFFERENT FROM LIN ET AL.                                 ******         SATICE.585
          PINT(I)=0.0                                                            SATICE.586
          TAIR(I)=(PT(I)+TB0)*PI(I)                                              SATICE.587
           if (qi(i) .le. cmin) qi(i)=0.0                                        SATICE.588
          IF(TAIR(I).LT.T0K)THEN                                                 SATICE.589
            TAIRC(I)=TAIR(I)-T0K                                                 SATICE.590
            RTAIR(I)=1./(TAIR(I)-C76)                                            SATICE.591
            Y2(I)=EXP(C218-C580*RTAIR(I))                                        SATICE.592
            QSI(I)=3799.1/P0(I)*Y2(I)                                            SATICE.593
            ESI(I)=C610*Y2(I)                                                    SATICE.594
            SSI(I)=(QV(I)+QB0)/QSI(I)-1.                                         SATICE.595
            NCI=AMIN1(RN25*EXP(BETA*TAIRC(I)),1.)                                SATICE.596
C     Do not initiate the maximum number of nuclei. Some ice may already         SATICE.597
c     present, so only initiate that needed to get up to the maximum num         SATICE.598
c     Use ami50 g as mass of 50 micron size particle (assumed to be the          SATICE.599
c     size of ice particles), and use 1.e-9 g as initial mass of ice             SATICE.600
c     particles                                                                  SATICE.601
            dd(i)=AMAX1(1.e-9*nci/rho(i)-qi(i)*1.e-9/ami50 , 0.)                 SATICE.602
            DM(I)=AMAX1((QV(I)+QB0-QSI(I)),0.)                                   SATICE.603
            RSUB1(I)=C580*ASC*QSI(I)*RTAIR(I)*RTAIR(I)                           SATICE.604
            PINT(I)=AMIN1(DD(I),DM(I))                                           SATICE.605
            DEP(I)=DM(I)/(1.+RSUB1(I))                                           SATICE.606
            PINT(I)=AMIN1(PINT(I),DEP(I))                                        SATICE.607
            if (pint(i) .le. cmin) pint(i)=0.0                                   SATICE.608
            PT(I)=PT(I)+ASC/PI(I)*PINT(I)                                        SATICE.609
            QV(I)=QV(I)-PINT(I)                                                  SATICE.610
            QI(I)=QI(I)+PINT(I)                                                  SATICE.611
          ENDIF                                                                  SATICE.612
  225   CONTINUE                                                                 SATICE.613
C*****   TAO ET AL (1989) SATURATION TECHNIQUE  ***********************          SATICE.614
C*****   NO ITERATION - DIFFERENT FROM LIN ET AL***********************          SATICE.615
C*****                                                                           SATICE.616
C*****   IWATER=0 USES A SLIGHT VARIANT OF THE ORIGINAL TAO ET AL METHOD         SATICE.617
C*****   THE ONLY DIFFERENCE IS THAT THE WEIGHTING BETWEEN THE LIQUID            SATICE.618
C*****   AND ICE SATURATION VALUES IS DONE BY TEMPERATURE RATHER THAN MA         SATICE.619
C*****   IWATER=1 USES A SEQUENTIAL METHOD IN WHICH THE ADJUSTMENT IS FI         SATICE.620
C*****   DONE FOR LIQUID WATER FOR TEMPERATURES WARMER THAN 253K, THEN F         SATICE.621
C*****   ICE ONLY WHERE THE TEMPERATURE IS COLDER THAN 258K. THE MAIN EF         SATICE.622
C*****   THIS CHANGE IS TO REDUCE THE AMOUNT OF SUPERCOOLED WATER AT VER         SATICE.623
C*****   TEMPERATURES                                                            SATICE.624
        IF (IWATER .EQ. 0) THEN                                                  SATICE.625
         DO 250 I=ISTA,ILES                                                      SATICE.626
          TAIR(I)=(PT(I)+TB0)*PI(I)                                              SATICE.627
          CND(I)=RT0*(TAIR(I)-T00K)                                              SATICE.628
          DEP(I)=RT0*(T0K-TAIR(I))                                               SATICE.629
          Y1(I)=1./(TAIR(I)-C358)                                                SATICE.630
          Y2(I)=1./(TAIR(I)-C76)                                                 SATICE.631
          QSW(I)=3799.1/P0(I)*EXP(C172-C409*Y1(I))                               SATICE.632
          QSI(I)=3799.1/P0(I)*EXP(C218-C580*Y2(I))                               SATICE.633
          DD(I)=C409*PI(I)*Y1(I)*Y1(I)                                           SATICE.634
          DD1(I)=C580*PI(I)*Y2(I)*Y2(I)                                          SATICE.635
          IF(QC(I).LE.cmin) QC(I)=cmin                                           SATICE.636
          IF(QI(I).LE.cmin) QI(I)=cmin                                           SATICE.637
          IF(TAIR(I).GE.T0K)THEN                                                 SATICE.638
            DEP(I)=0.0                                                           SATICE.639
            CND(I)=1.0                                                           SATICE.640
            QI(I)=0.0                                                            SATICE.641
          ENDIF                                                                  SATICE.642
          IF(TAIR(I).LE.T00K)THEN                                                SATICE.643
            CND(I)=0.0                                                           SATICE.644
            DEP(I)=1.0                                                           SATICE.645
            QC(I)=0.0                                                            SATICE.646
          ENDIF                                                                  SATICE.647
          Y5(I)=(AVC*CND(I)+ASC*DEP(I))/PI(I)                                    SATICE.648
c          Y1(I)=QC(I)*QSW(I)/(QC(I)+QI(I))                                      SATICE.649
c          Y2(I)=QI(I)*QSI(I)/(QC(I)+QI(I))                                      SATICE.650
          Y1(I)=CND(I)*QSW(I)                                                    SATICE.651
          Y2(I)=DEP(I)*QSI(I)                                                    SATICE.652
          Y4(I)=DD(I)*Y1(I)+DD1(I)*Y2(I)                                         SATICE.653
          QVS(I)=Y1(I)+Y2(I)                                                     SATICE.654
          DM(I)=QV(I)+QB0-QVS(I)                                                 SATICE.655
          RSUB1(I)=DM(I)/(1.+Y4(I)*Y5(I))                                        SATICE.656
          CND(I)=CND(I)*RSUB1(I)                                                 SATICE.657
          DEP(I)=DEP(I)*RSUB1(I)                                                 SATICE.658
          IF(QC(I).LE.cmin)QC(I)=0.                                              SATICE.659
          IF(QI(I).LE.cmin)QI(I)=0.                                              SATICE.660
C    ******   CONDENSATION OR EVAPORATION OF QC  ******                          SATICE.661
          CND(I)=AMAX1(-QC(I),CND(I))                                            SATICE.662
C    ******   DEPOSITION OR SUBLIMATION OF QI    ******                          SATICE.663
          DEP(I)=AMAX1(-QI(I),DEP(I))                                            SATICE.664
          PT(I)=PT(I)+(AVC*CND(I)+ASC*DEP(I))/PI(I)                              SATICE.665
          QV(I)=QV(I)-CND(I)-DEP(I)                                              SATICE.666
          QC(I)=QC(I)+CND(I)                                                     SATICE.667
          QI(I)=QI(I)+DEP(I)                                                     SATICE.668
  250    CONTINUE                                                                SATICE.669
        ELSEIF (IWATER .EQ. 1) THEN                                              SATICE.670
         DO I=ISTA,ILES                                                          SATICE.671
          TAIR(I)=(PT(I)+TB0)*PI(I)                                              SATICE.672
          CND(I)=0.                                                              SATICE.673
          DEP(I)=0.                                                              SATICE.674
          IF (TAIR(I) .GE. 253.16) THEN                                          SATICE.675
             Y1(I)=1./(TAIR(I)-C358)                                             SATICE.676
             QSW(I)=3799.1/P0(I)*EXP(C172-C409*Y1(I))                            SATICE.677
             DD(I)=C409*PI(I)*Y1(I)*Y1(I)                                        SATICE.678
             DM(I)=QV(I)+QB0-QSW(I)                                              SATICE.679
             Y5(I)=AVC/PI(I)                                                     SATICE.680
             Y4(I)=DD(I)*QSW(I)                                                  SATICE.681
             CND(I)=DM(I)/(1.+Y4(I)*Y5(I))                                       SATICE.682
             CND(I)=AMAX1(-QC(I),CND(I))                                         SATICE.683
             PT(I)=PT(I)+AVC/PI(I)*CND(I)                                        SATICE.684
             QV(I)=QV(I)-CND(I)                                                  SATICE.685
             QC(I)=QC(I)+CND(I)                                                  SATICE.686
          ENDIF                                                                  SATICE.687
          IF (TAIR(I) .LE. 258.16) THEN                                          SATICE.688
             Y2(I)=1./(TAIR(I)-C76)                                              SATICE.689
             QSI(I)=3799.1/P0(I)*EXP(C218-C580*Y2(I))                            SATICE.690
             DD1(I)=C580*PI(I)*Y2(I)*Y2(I)                                       SATICE.691
             Y5(I)=ASC/PI(I)                                                     SATICE.692
             Y4(I)=DD1(I)*QSI(I)                                                 SATICE.693
             DM(I)=QV(I)+QB0-QSI(I)                                              SATICE.694
             DEP(I)=DM(I)/(1.+Y4(I)*Y5(I))                                       SATICE.695
             DEP(I)=AMAX1(-QI(I),DEP(I))                                         SATICE.696
             PT(I)=PT(I)+ASC*DEP(I)/PI(I)                                        SATICE.697
             QV(I)=QV(I)-DEP(I)                                                  SATICE.698
             QI(I)=QI(I)+DEP(I)                                                  SATICE.699
          ENDIF                                                                  SATICE.700
         ENDDO                                                                   SATICE.701
        ENDIF                                                                    SATICE.702
C* 10 * PSDEP : DEPOSITION OR SUBLIMATION OF QS                   **10**         SATICE.703
C* 20 * PGSUB : SUBLIMATION OF QG                                 **20**         SATICE.704
        DO 275 I=ISTA,ILES                                                       SATICE.705
          PSDEP(I)=0.0                                                           SATICE.706
          PSSUB(I)=0.0                                                           SATICE.707
          PGSUB(I)=0.0                                                           SATICE.708
          r3456=sqrt(fv(i)/rho(i))                                               SATICE.709
          TAIR(I)=(PT(I)+TB0)*PI(I)                                              SATICE.710
          IF(TAIR(I).LT.T0K.AND.(QS(I)+QG(I)).GT.CMIN)THEN                       SATICE.711
            IF(QS(I).LT.cmin) QS(I)=0.0                                          SATICE.712
            IF(QG(I).LT.cmin) QG(I)=0.0                                          SATICE.713
            RTAIR(I)=1./(TAIR(I)-C76)                                            SATICE.714
            QSI(I)=3799.1/P0(I)*EXP(C218-C580*RTAIR(I))                          SATICE.715
            SSI(I)=(QV(I)+QB0)/QSI(I)-1.                                         SATICE.716
            Y1(I)=RN10A*RHO(I)/(TCA(I)*TAIR(I)**2)+1./(DWV(I)*QSI(I))            SATICE.717
            Y2(I)=.78/ZS(I)**2+RN101*(FV(I)/RHO(I))**0.25*SCV(I)/ZS(I)**         SATICE.718
     +            BSH5                                                           SATICE.719
            PSDEP(I)=R10T*SSI(I)*Y2(I)/Y1(I)                                     SATICE.720
            PSSUB(I)=PSDEP(I)                                                    SATICE.721
            PSDEP(I)=AMAX1(PSDEP(I),0.)                                          SATICE.722
            PSSUB(I)=AMAX1(-QS(I),AMIN1(PSSUB(I),0.))                            SATICE.723
            IF(IHAIL.EQ.1) THEN                                                  SATICE.724
               Y2(I)=.78/ZG(I)**2+SCV(I)*RN20B/RHO(I)**0.25/ZG(I)**BGH5          SATICE.725
            ELSE                                                                 SATICE.726
               Y2(I)=.78/ZG(I)**2+SCV(I)*RN20B*SQRT(R3456) /ZG(I)**BGH5          SATICE.727
            ENDIF                                                                SATICE.728
            PGSUB(I)=R20T*SSI(I)*Y2(I)/Y1(I)                                     SATICE.729
            DM(I)=QV(I)+QB0-QSI(I)                                               SATICE.730
            RSUB1(I)=C580*ASC*QSI(I)*RTAIR(I)*RTAIR(I)                           SATICE.731
C     ********   DEPOSITION OR SUBLIMATION OF QS  **********************         SATICE.732
            Y1(I)=DM(I)/(1.+RSUB1(I))                                            SATICE.733
            PSDEP(I)=AMIN1(PSDEP(I),AMAX1(Y1(I),0.))                             SATICE.734
            Y2(I)=AMIN1(Y1(I),0.)                                                SATICE.735
            PSSUB(I)=AMAX1(PSSUB(I),Y2(I))                                       SATICE.736
C     ********   SUBLIMATION OF QG   ***********************************         SATICE.737
            DD(I)=AMAX1((-Y2(I)-QS(I)),0.)                                       SATICE.738
            PGSUB(I)=AMIN1(DD(I),QG(I),AMAX1(PGSUB(I),0.))                       SATICE.739
C      DLT1(I)=CVMGP(1.,0.,QC(I)+QI(I)-1.E-5)                                    SATICE.740
            IF((QC(I)+QI(I)-1.E-5).GE.0.)THEN                                    SATICE.741
              DLT1(I)=1.                                                         SATICE.742
            ELSE                                                                 SATICE.743
              DLT1(I)=0.                                                         SATICE.744
            ENDIF                                                                SATICE.745
            PSDEP(I)=DLT1(I)*PSDEP(I)                                            SATICE.746
            PSSUB(I)=(1.-DLT1(I))*PSSUB(I)                                       SATICE.747
            PGSUB(I)=(1.-DLT1(I))*PGSUB(I)                                       SATICE.748
            PT(I)=PT(I)+ASC/PI(I)*(PSDEP(I)+PSSUB(I)-PGSUB(I))                   SATICE.749
            QV(I)=QV(I)+PGSUB(I)-PSSUB(I)-PSDEP(I)                               SATICE.750
            QS(I)=QS(I)+PSDEP(I)+PSSUB(I)                                        SATICE.751
            QG(I)=QG(I)-PGSUB(I)                                                 SATICE.752
          ENDIF                                                                  SATICE.753
C* 23 * ERN : EVAPORATION OF QR (SUBSATURATION)                   **23**         SATICE.754
          ERN(I)=0.0                                                             SATICE.755
          IF(QR(I).GT.CMIN)THEN                                                  SATICE.756
            TAIR(I)=(PT(I)+TB0)*PI(I)                                            SATICE.757
            RTAIR(I)=1./(TAIR(I)-C358)                                           SATICE.758
            QSW(I)=3799.1/P0(I)*EXP(C172-C409*RTAIR(I))                          SATICE.759
            SSW(I)=(QV(I)+QB0)/QSW(I)-1.0                                        SATICE.760
            DM(I)=QV(I)+QB0-QSW(I)                                               SATICE.761
            RSUB1(I)=C409*AVC*QSW(I)*RTAIR(I)*RTAIR(I)                           SATICE.762
            DD1(I)=AMAX1(-DM(I)/(1.+RSUB1(I)),0.0)                               SATICE.763
            Y1(I)=.78/ZR(I)**2+RN23A*(FV(I)/RHO(I))**0.25*SCV(I)/ZR(I)**         SATICE.764
     +            BWH5                                                           SATICE.765
            Y2(I)=RN23B*RHO(I)/(TCA(I)*TAIR(I)**2)+1./(DWV(I)*QSW(I))            SATICE.766
            ERN(I)=R23T*SSW(I)*Y1(I)/Y2(I)                                       SATICE.767
            ERN(I)=AMIN1(DD1(I),QR(I),AMAX1(ERN(I),0.))                          SATICE.768
            PT(I)=PT(I)-AVC/PI(I)*ERN(I)                                         SATICE.769
            QV(I)=QV(I)+ERN(I)                                                   SATICE.770
            QR(I)=QR(I)-ERN(I)                                                   SATICE.771
          ENDIF                                                                  SATICE.772
          IF(QC(I).LE.cmin)QC(I)=0.                                              SATICE.773
          IF(QR(I).LE.cmin)QR(I)=0.                                              SATICE.774
          IF(QI(I).LE.cmin)QI(I)=0.                                              SATICE.775
          IF(QS(I).LE.cmin)QS(I)=0.                                              SATICE.776
          IF(QG(I).LE.cmin)QG(I)=0.                                              SATICE.777
          DPT(I,K)=PT(I)                                                         SATICE.778
          DQV(I,K)=QV(I)                                                         SATICE.779
          QCL(I,K)=QC(I)                                                         SATICE.780
          QRN(I,K)=QR(I)                                                         SATICE.781
          QCI(I,K)=QI(I)                                                         SATICE.782
          QCS(I,K)=QS(I)                                                         SATICE.783
          QCG(I,K)=QG(I)                                                         SATICE.784
  275   CONTINUE                                                                 SATICE.785
C PRECIP IS SURFACE RAIN RATE. UNIT: CM/S                                        SATICE.786
        IF(K.EQ.2)THEN                                                           SATICE.787
          DO 300 I=ISTA,ILES                                                     SATICE.788
            PRECIP(I)=(VR(I)*QR(I)+VS(I)*QS(I)+VG(I)*QG(I))*RHO(I)               SATICE.789
  300     CONTINUE                                                               SATICE.790
        ENDIF                                                                    SATICE.791
CC**********************************************************************         SATICE.792
 1000 CONTINUE                                                                   SATICE.793
C                                                                                SATICE.794
      RETURN                                                                     SATICE.795
      END                                                                        SATICE.796
