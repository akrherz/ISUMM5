      SUBROUTINE FCPARA(J,INEST,SCR1,IBEG,IEND,TBASE,                            FCPARA.1
     &                  U3D,V3D,PP3D,T3D,QV3D,PSB)                               FCPARA.2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         FCPARA.3
C                                                                      C         FCPARA.4
C         THIS SUBROUTINE COMPUTES CONVECTIVE EFFECTS USING MODIFIED   C         FCPARA.5
C   FRITSCH/CHAPPELL CUMULUS SCHEME.              MKS UNITS.           C         FCPARA.6
C     INPUT:   TEMPERTURE (T0, K) ;    SPECIFIC HUMIDITY (Q0, KG/KG) ; C         FCPARA.7
C              HORIZONTAL WIND SPEED (U0 AND V0, M/S) ;                C         FCPARA.8
C              PRESSURE (P0, PASCAL) ;  HEIGHT (Z0, M);                C         FCPARA.9
C              VERTICAL MOTION (W0, M/S).                              C         FCPARA.10
C     OUTPUT:  CONVECTIVE EFFECTS OF HEATING (DTDT), MOISTENING (DQDT) C         FCPARA.11
C              MOMENTUM EXCHANGE(DUDT,DVDT),AND RAINFALL AMOUNT(RAINCV)C         FCPARA.12
C                                                                      C         FCPARA.13
C                                   DOCUMENTED BY D.-L. ZHANG          C         FCPARA.14
C                                    SEPT 25, 1986                     C         FCPARA.15
C                                                                      C         FCPARA.16
C     REFERENCES:                                                      C         FCPARA.17
C                                                                      C         FCPARA.18
C         FRITSCH AND CHAPPELL (1980), J. ATMOS. SCI., 1722-1733.      C         FCPARA.19
C                                                                      C         FCPARA.20
C         ZHANG AND FRITSCH (1986), J. ATMOS. SCI., 1913-1943.         C         FCPARA.21
C                                                                      C         FCPARA.22
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         FCPARA.23
                                                                                 FCPARA.24
#     include <parame.incl>                                                      FCPARA.25
#     include <various.incl>                                                     FCPARA.26
#     include <param3.incl>                                                      FCPARA.27
C     include <point3d.incl>                                                     FCPARA.28
C     include <point2d.incl>                                                     FCPARA.29
#     include <varia.incl>                                                       FCPARA.30
#     include <addrcu.incl>                                                      FCPARA.31
C     include <nonhyd.incl>                                                      FCPARA.32
#     include <rpstar.incl>                                                      FCPARA.33
C                                                                                FCPARA.34
C...INPUT VARIABLES                                                              FCPARA.35
C                                                                                FCPARA.36
      DIMENSION U3D(MIX,MJX,MKX),  V3D(MIX,MJX,MKX),                             FCPARA.37
     &          T3D(MIX,MJX,MKX), QV3D(MIX,MJX,MKX),                             FCPARA.38
     &         PP3D(MIX,MJX,MKX),  PSB(MIX,MJX)                                  FCPARA.39
C                                                                                FCPARA.40
C...DEFINE LOCAL VARIABLES...                                                    FCPARA.41
C                                                                                FCPARA.42
      DIMENSION P0(KX),Z0(KX),T0(KX),Q0(KX),U0(KX),V0(KX),W0(KX),                FCPARA.43
     1   TU(KX),QU(KX),UU(KX),VU(KX),WU(KX),RU(KX),AU(KX),UMF(KX),               FCPARA.44
     2   TZ(KX),QD(KX),UD(KX),VD(KX),WD(KX),RD(KX),AD(KX),DMF(KX),               FCPARA.45
     3   TE(KX),QE(KX),UE(KX),VE(KX),WE(KX),RO(KX),AE(KX),PI(KX),                FCPARA.46
     4   TG(KX),QG(KX),UG(KX),VG(KX),ZA(KX),ZB(KX),BM(KX),EY(KX),                FCPARA.47
     5   FT(KX),FQ(KX),FU(KX),FV(KX),RL(KX),ALF(KX),BTA(KX),                     FCPARA.48
     6   DZQ(KX),DZA(KX),WPD(KX),WGT(KX),DQU(KX),DQD(KX),DET(KX),                FCPARA.49
     7   TVG(KX),TVU(KX),TVD(KX),THE(KX),THM(KX),TDP(KX),TST(KX),                FCPARA.50
     8   DM2(KX),AU2(KX),AD2(KX),WGO(KX),FBEA(KX),ACIN(KX)                       FCPARA.51
      DIMENSION SCR1(MIX,MKX),TBASE(MIXNH,MJXNH,MKXNH)                           FCPARA.52
C                                                                                FCPARA.53
      DATA C1,C2,C3,C4 / 2.5E6,2.37E3,2.83E6,259.5 /                             FCPARA.54
      DATA CPV,CV,RV / 1952.,717.,461.5 /                                        FCPARA.55
      DATA POO,WO,RHIC,RHBC / 1.E5,2*1.,0.95 /                                   FCPARA.56
      DATA TO,B61,TICE / 273.16,.608,248. /                                      FCPARA.57
C... BU IS ENTRAINMENT RATE; AGO IS THE INVERSE OF A GRID BOX;                   FCPARA.58
C    AUO IS AN INITIAL GUESS OF UPDRAFT AREA AT THE CLOUD BASE;                  FCPARA.59
C    CRAE IS THE LOWEST LIMIT OF SUBSIDENCE AREA;                                FCPARA.60
C    GDRY IS THE DRY LAPSE RATE;                                                 FCPARA.61
C                                                                                FCPARA.62
      BU=6.E-5                                                                   FCPARA.63
      AGO=1./DXSQ                                                                FCPARA.64
      AUO=.01*DXSQ                                                               FCPARA.65
      CRAE=.1*DXSQ                                                               FCPARA.66
      CRAD=.9*DXSQ                                                               FCPARA.67
      GDRY=-G/CP                                                                 FCPARA.68
      BVRT=1./(1.+.5)                                                            FCPARA.69
      CPOR=CP/R                                                                  FCPARA.70
      D273=1./273.16                                                             FCPARA.71
C                                                                                FCPARA.72
C...INPUT DATA ARE IN A FORM OF NORTH-SOUTH SLICE.                               FCPARA.73
C                                                                                FCPARA.74
      DO 650 I=IBEG,IEND                                                         FCPARA.75
C                                                                                FCPARA.76
C     CHECK THE NECESSITY OF COMPUTING THE CONVECTIVE EFFECTS.                   FCPARA.77
C  IF CONVECTION IS STILL ACTIVE, GO TO 1000. THE SCHEME CHECKS FOR              FCPARA.78
C  THE INITIATING POSSIBILITY OF CONVECTION AT NEIGHBOURING POINTS OF            FCPARA.79
C  CONVECTIVE SYSTEMS EVERY TIME STEP, BUT OTHER POINTS EVERY 10-15              FCPARA.80
C  MINUTES.                                                                      FCPARA.81
C                                                                                FCPARA.82
        JFLAG=0                                                                  FCPARA.83
        IF(NCA(I,J).GT.0)GOTO 650                                                FCPARA.84
C        IF (ICLDCK.NE.0) GO TO 650                                              FCPARA.85
C        IF (ICLDCK.EQ.0) GO TO 10                                               FCPARA.86
C        JCON = MAX0(NCA(I,J+1),NCA(I,J-1),NCA(I+1,J),                           FCPARA.87
C    >          NCA(I-1,J))                                                      FCPARA.88
C        IF (JCON.LT.0) GO TO 650                                                FCPARA.89
   10   CONTINUE                                                                 FCPARA.90
        P300=1000.*(PSB(I,J)*A(KL)+PTOP-30.)+PP3D(I,J,KL)                        FCPARA.91
C                                                                                FCPARA.92
C  INPUT A VERTICAL SOUNDING                                                     FCPARA.93
C                                                                                FCPARA.94
        DO 20 K=1,KX                                                             FCPARA.95
          NK=KX-K+1                                                              FCPARA.96
          P0(K)=1.E3*(A(NK)*PSB(I,J)+PTOP)+PP3D(I,J,NK)                          FCPARA.97
          T0(K)=T3D(I,J,NK)                                                      FCPARA.98
          Q0(K)=QV3D(I,J,NK)                                                     FCPARA.99
          U0(K)=.25*(U3D(I,J,NK)+U3D(I+1,J,NK)+U3D(I,J+1,NK)+                    FCPARA.100
     +          U3D(I+1,J+1,NK))                                                 FCPARA.101
          V0(K)=.25*(V3D(I,J,NK)+V3D(I+1,J,NK)+V3D(I,J+1,NK)+                    FCPARA.102
     +          V3D(I+1,J+1,NK))                                                 FCPARA.103
          TVG(K)=T0(K)*(1.+B61*Q0(K))                                            FCPARA.104
          RO(K)=P0(K)/(R*TVG(K))                                                 FCPARA.105
          W0(K)=-101.9368*SCR1(I,NK)/RO(K)                                       FCPARA.106
          CELL=PTOP*RPSB(I,J)                                                    FCPARA.107
          DZQ(K)=ROVG*TBASE(I,J,NK)*                                             FCPARA.108
     +           ALOG((SIGMA(NK+1)+CELL)/(SIGMA(NK)+CELL))                       FCPARA.109
          PI(K)=(POO/P0(K))**ROVCP                                               FCPARA.110
          IF(P0(K).GE.500E2)L5=K                                                 FCPARA.111
          IF(P0(K).GE.400E2)L4=K                                                 FCPARA.112
          IF(P0(K).GE.P300)LLFC=K                                                FCPARA.113
   20   CONTINUE                                                                 FCPARA.114
        Z0(1)=.5*DZQ(1)                                                          FCPARA.115
        DO 30 K=2,KL                                                             FCPARA.116
          Z0(K)=Z0(K-1)+.5*(DZQ(K)+DZQ(K-1))                                     FCPARA.117
          DZA(K-1)=Z0(K)-Z0(K-1)                                                 FCPARA.118
   30   CONTINUE                                                                 FCPARA.119
        DZA(KL)=DZQ(KL)                                                          FCPARA.120
        LC=1                                                                     FCPARA.121
C      WRITE(98,4557)I,J,XTIME                                                   FCPARA.122
4557    FORMAT('I, J =',2I3,'XTIME =',F10.4)                                     FCPARA.123
   40   LOW=LC+1                                                                 FCPARA.124
        IF(LOW.GT.LLFC)GOTO 650                                                  FCPARA.125
C                                                                                FCPARA.126
C   BEGINING WITH THE LOWEST LAYER(NOT INCLUDING THE SURFACE LAYER               FCPARA.127
C   IF THE LAYER IS LESS THAN 20-30M), EVERY TWO LAYERS(50 MB) ARE               FCPARA.128
C   MIXED, LIFTED TO ITS LCL AND CHECKED FOR BOUYANCY UNTIL REACHING             FCPARA.129
C   THE TOP OF THE LOWEST 300-MB LAYER(P300).                                    FCPARA.130
C                                                                                FCPARA.131
        DO 70 LB=LOW,LLFC                                                        FCPARA.132
          LC=LB                                                                  FCPARA.133
          LC1=LB+1                                                               FCPARA.134
          WT=1./(DZQ(LC)+DZQ(LC1))                                               FCPARA.135
          TMIX=(T0(LC)*DZQ(LC)+T0(LC1)*DZQ(LC1))*WT                              FCPARA.136
          QMIX=(Q0(LC)*DZQ(LC)+Q0(LC1)*DZQ(LC1))*WT                              FCPARA.137
          PMIX=(P0(LC)*DZQ(LC)+P0(LC1)*DZQ(LC1))*WT                              FCPARA.138
          ZMIX=(Z0(LC)*DZQ(LC)+Z0(LC1)*DZQ(LC1))*WT                              FCPARA.139
          RLL=C1-C2*(TMIX-TO)                                                    FCPARA.140
          TDPT=1./(D273-RV/RLL*ALOG(PMIX*QMIX/(611.*(.622+QMIX))))               FCPARA.141
          TDPT=AMIN1(TDPT,TMIX)                                                  FCPARA.142
          TLCL=TDPT-(.212+1.571E-3*(TDPT-TO)-4.36E-4*(TMIX-TO))*(TMIX-           FCPARA.143
     +         TDPT)                                                             FCPARA.144
C                                                                                FCPARA.145
C   COMPUTE THE LCL. KLCL IS THE MODEL LEVEL IMEDIATELY ABOVE THE LCL,           FCPARA.146
C   AND K IS THE MODEL LEVEL BELOW THE LCL.                                      FCPARA.147
C                                                                                FCPARA.148
          ZLCL=ZMIX+(TLCL-TMIX)/GDRY                                             FCPARA.149
          DO 50 K=LC,L5                                                          FCPARA.150
            KLCL=K                                                               FCPARA.151
            IF(ZLCL.LE.Z0(K))GOTO 60                                             FCPARA.152
   50     CONTINUE                                                               FCPARA.153
          GOTO 70                                                                FCPARA.154
C                                                                                FCPARA.155
   60     K=KLCL-1                                                               FCPARA.156
C                                                                                FCPARA.157
C     CHECK TO SEE IF CLOUD IS BUOYANT.                                          FCPARA.158
C                                                                                FCPARA.159
          DZLDZA=(ZLCL-Z0(K))/DZA(K)                                             FCPARA.160
          TENV=T0(K)+(T0(KLCL)-T0(K))*DZLDZA                                     FCPARA.161
C            IF(XTIME.LT.420.)THEN                                               FCPARA.162
C            WKLCL=.0132+4.E-5*XTIME                                             FCPARA.163
C            ELSE                                                                FCPARA.164
C            IF(XTIME.LT.600.)WKLCL=.03                                          FCPARA.165
C            IF(XTIME.GE.600.)WKLCL=.03-5.E-5*(XTIME-600.)                       FCPARA.166
C            END IF                                                              FCPARA.167
C           WKLCL=AMAX1(WKLCL,0.015)                                             FCPARA.168
          WKLCL=0.015                                                            FCPARA.169
          WKL=W0(K)+(W0(KLCL)-W0(K))*DZLDZA-WKLCL                                FCPARA.170
          WABS=ABS(WKL)+1.E-10                                                   FCPARA.171
          WSIGNE=WKL/WABS                                                        FCPARA.172
          DTLCL=4.64*WSIGNE*WABS**0.33                                           FCPARA.173
C      WRITE(98,4558)DTLCL,TLCL+DTLCL,TENV,LC                                    FCPARA.174
4558   FORMAT('DTLCL, TLCL+DTLCL, TENV =',3F10.3,' FOR LC =',I3)                 FCPARA.175
          IF(TLCL+DTLCL.GT.TENV)GOTO 80                                          FCPARA.176
   70   CONTINUE                                                                 FCPARA.177
        GOTO 650                                                                 FCPARA.178
C                                                                                FCPARA.179
C   THE PARCEL IS BUOYANT, COMPUTE POTENTIAL TEMPERATURE(THETA),                 FCPARA.180
C   VIRTUAL TEMPERATURE(TVEN) AND UPDRAFT VELOCITY(WLCL) AT THE LCL.             FCPARA.181
C                                                                                FCPARA.182
   80   THATA=TMIX*(POO/PMIX)**ROVCP                                             FCPARA.183
        TVEN=TENV*(1.+B61*(Q0(K)+(Q0(KLCL)-Q0(K))*DZLDZA))                       FCPARA.184
        GDT=G*DTLCL*(ZLCL-Z0(LC))/(TVG(LC)+TVEN)                                 FCPARA.185
        WLCL=WO+.5*WSIGNE*SQRT(ABS(GDT)+1.E-10)                                  FCPARA.186
        WTW=WLCL*WLCL                                                            FCPARA.187
C                                                                                FCPARA.188
        IF(LOW.GT.2)GOTO 100                                                     FCPARA.189
C                                                                                FCPARA.190
C   COMPUTE EQUIVALENT POTENTIAL TEMPERATURE(THETAE) OF ENVIRONMENT.             FCPARA.191
C   BU IS THE ENTRAINMENT RATE. RL IS LATENT HEAT OF VAPORIZATION.               FCPARA.192
C                                                                                FCPARA.193
        DO 90 NK=1,KX                                                            FCPARA.194
          DET(NK)=0.                                                             FCPARA.195
          WGT(NK)=EXP(BU*DZA(NK))-1.                                             FCPARA.196
          WGO(NK)=1./(1.+WGT(NK))                                                FCPARA.197
          Q0(NK)=AMAX1(1.E-9,Q0(NK))                                             FCPARA.198
C            RL(NK) = CVMGT(C1-C2*(T0(NK)-TO),C3-C4*(T0(NK)-TO),T0(NK)           FCPARA.199
C     *         .GT.TICE)                                                        FCPARA.200
          IF(T0(NK).GT.TICE)THEN                                                 FCPARA.201
            RL(NK)=C1-C2*(T0(NK)-TO)                                             FCPARA.202
          ELSE                                                                   FCPARA.203
            RL(NK)=C3-C4*(T0(NK)-TO)                                             FCPARA.204
          ENDIF                                                                  FCPARA.205
          TDP(NK)=1./(D273-RV/RL(NK)*                                            FCPARA.206
     +            ALOG(P0(NK)*Q0(NK)/(611.*(Q0(NK)+.622))))                      FCPARA.207
C            TST(NK) = CVMGT(TDP(NK)-(.212+1.571E-3*(TDP(NK)-TO)-4.36E-4         FCPARA.208
C     *         (T0(NK)-TO))*(T0(NK)-TDP(NK)),TDP(NK)-(.182+1.13E-3*(TDP         FCPARA.209
C     *         NK)-TO)-3.58E-4*(T0(NK)-TO))*(T0(NK)-TDP(NK)),TDP(NK).GT         FCPARA.210
C     *         TICE)                                                            FCPARA.211
          IF(TDP(NK).GT.TICE)THEN                                                FCPARA.212
            TST(NK)=TDP(NK)-(.212+1.571E-3*(TDP(NK)-TO)-4.36E-4*(T0(NK)-         FCPARA.213
     +              TO))*(T0(NK)-TDP(NK))                                        FCPARA.214
          ELSE                                                                   FCPARA.215
            TST(NK)=TDP(NK)-(.182+1.13E-3*(TDP(NK)-TO)-3.58E-4*(T0(NK)-          FCPARA.216
     +              TO))*(T0(NK)-TDP(NK))                                        FCPARA.217
          ENDIF                                                                  FCPARA.218
          THE(NK)=T0(NK)*PI(NK)*EXP(RL(NK)*Q0(NK)/(CP*TST(NK)))                  FCPARA.219
   90   CONTINUE                                                                 FCPARA.220
  100   CONTINUE                                                                 FCPARA.221
        LCL=KLCL                                                                 FCPARA.222
        LET=LCL                                                                  FCPARA.223
C                                                                                FCPARA.224
C*******************************************************************             FCPARA.225
C                                                                  *             FCPARA.226
C                 COMPUTE UPDRAFT PROPERTIES                       *             FCPARA.227
C                                                                  *             FCPARA.228
C*******************************************************************             FCPARA.229
C                                                                                FCPARA.230
        SDQU=0.                                                                  FCPARA.231
        ABE=0.                                                                   FCPARA.232
        IFLAG=0                                                                  FCPARA.233
        FREZ=0.                                                                  FCPARA.234
        WGLCL=EXP(BU*(Z0(KLCL)-ZLCL))-1.                                         FCPARA.235
        TU(K)=TLCL                                                               FCPARA.236
        TVU(K)=TLCL*(1.+B61*QMIX)                                                FCPARA.237
        QU(K)=QMIX                                                               FCPARA.238
        THM(K)=THATA*EXP((C1-C2*(TLCL-TO))*QMIX/(CP*TLCL))                       FCPARA.239
C                                                                                FCPARA.240
C...UPDRAFT TEMPERTURE, MIXING RATIO, BUOYANCY AND VELOCITY. THE CLOUD           FCPARA.241
C...TOP IS DEFINED AT THE LEVEL WHERE UPDRAFT MOTION VANISHES.                   FCPARA.242
C                                                                                FCPARA.243
        DO 120 NK=K,KLM                                                          FCPARA.244
          LTOP=NK                                                                FCPARA.245
          IF(TU(NK).GT.TICE)GOTO 110                                             FCPARA.246
          IF(IFLAG.NE.0)GOTO 110                                                 FCPARA.247
          FREZ=DTFRZ(TU(NK),P0(NK),QU(NK),.5*SDQU,D273)                          FCPARA.248
          TU(NK)=TU(NK)+FREZ                                                     FCPARA.249
          ES=611.*EXP(RL(NK)/RV*(D273-1./TU(NK)))                                FCPARA.250
          QU(NK)=.622*ES/(P0(NK)-ES)                                             FCPARA.251
          TVU(NK)=TU(NK)*(1.+B61*QU(NK))                                         FCPARA.252
          THM(NK)=TU(NK)*PI(NK)*EXP(RL(NK)*QU(NK)/(CP*TU(NK)))                   FCPARA.253
          IFLAG=NK                                                               FCPARA.254
  110     NK1=NK+1                                                               FCPARA.255
          IF(NK.NE.K)THEN                                                        FCPARA.256
            THM(NK1)=(THM(NK)+WGT(NK)*.5*(THE(NK)+THE(NK1)))*WGO(NK)             FCPARA.257
            TU(NK1) = TP(P0(NK1),THM(NK1),T0(NK1),D273,RL(NK1),QU(NK1)           FCPARA.258
     *            ,PI(NK1),XTIME,NK1,'NK11',JFLAG)                               FCPARA.259
            IF(JFLAG.EQ.1)GOTO 649                                               FCPARA.260
            TVU(NK1)=TU(NK1)*(1.+B61*QU(NK1))                                    FCPARA.261
            DQU(NK)=(QU(NK)+WGT(NK)*.5*(Q0(NK)+Q0(NK1)))*WGO(NK)-QU(NK1)         FCPARA.262
            BE=DZA(NK)*BVRT*((TVU(NK)+TVU(NK1))/(TVG(NK)+TVG(NK1))-1.)           FCPARA.263
            WBE=BE-BU*WTW*DZA(NK)/G                                              FCPARA.264
          ELSE                                                                   FCPARA.265
            THM(NK1)=(THM(K)+WGLCL*.5*(THE(K)*(1.-DZLDZA)+THE(LCL)*(1.+          FCPARA.266
     +               DZLDZA)))/(1.+WGLCL)                                        FCPARA.267
            TU(NK1) = TP(P0(NK1),THM(NK1),T0(NK1),D273,RL(NK1),QU(NK1)           FCPARA.268
     *            ,PI(NK1),XTIME,NK1,'NK12',JFLAG)                               FCPARA.269
            IF(JFLAG.EQ.1)GOTO 649                                               FCPARA.270
            TVU(NK1)=TU(NK1)*(1.+B61*QU(NK1))                                    FCPARA.271
            DQU(NK)=(QU(K)+WGLCL*.5*(Q0(K)*(1.-DZLDZA)+Q0(LCL)*(1.+              FCPARA.272
     +              DZLDZA)))/(1.+WGLCL)-QU(NK1)                                 FCPARA.273
            BE=(Z0(LCL)-ZLCL)*BVRT*((TVU(LCL)+TVU(K))/(TVG(LCL)+TVEN)-1.         FCPARA.274
     +         )                                                                 FCPARA.275
            WBE=BE-BU*WTW*(Z0(LCL)-ZLCL)/G                                       FCPARA.276
          ENDIF                                                                  FCPARA.277
          SDQU=SDQU+DQU(NK)                                                      FCPARA.278
          WTW=WTW+19.62*WBE                                                      FCPARA.279
          WU(NK1)=WTW/SQRT(ABS(WTW)+1.E-10)                                      FCPARA.280
C            ABE = CVMGT(ABE+BE,ABE,BE.GT.0.)                                    FCPARA.281
          IF(BE.GT.0.)ABE=ABE+BE                                                 FCPARA.282
          IF(TU(NK).GE.T0(NK))LET=NK                                             FCPARA.283
          IF(WU(NK1).LT.1.E-6)GOTO 130                                           FCPARA.284
  120   CONTINUE                                                                 FCPARA.285
  130   CLDHT=Z0(LTOP)-ZLCL                                                      FCPARA.286
C                                                                                FCPARA.287
C... IF THE CLOUD DEPTH IS LESS THAN 4 KM, NO CONVECTION IS ALLOWED              FCPARA.288
C... FOR THE LIFTED LAYER.                                                       FCPARA.289
C                                                                                FCPARA.290
        IF(CLDHT.LT.4.E3.OR.LET.LE.LCL)GOTO 40                                   FCPARA.291
        ACP=0.                                                                   FCPARA.292
        CLIQ=0.                                                                  FCPARA.293
        P165=P0(LCL)-1.65E4                                                      FCPARA.294
        TRCP=0.                                                                  FCPARA.295
        LTOP1=LTOP+1                                                             FCPARA.296
        LTOPM1=LTOP-1                                                            FCPARA.297
        DWU=WU(LTOP1)-WU(LTOP)                                                   FCPARA.298
        ZTOP=Z0(LTOP)+WU(LTOP)*DZA(LTOP)/(ABS(DWU)+1.E-3)                        FCPARA.299
        DDT=T0(LET+1)-TU(LET+1)+TU(LET)-T0(LET)                                  FCPARA.300
        ZETL=Z0(LET)+DZA(LET)*(TU(LET)-T0(LET))/(ABS(DDT)+1.E-5)                 FCPARA.301
C                                                                                FCPARA.302
C...UPDRAFT MASS FLUX(VMF), OCCUPIED AREA(AU), HORIZONTAL MOMENTUM               FCPARA.303
C...(UU,VU), AND TOTAL CONDENSATE PRODUCTION RATE(TRCP) .                        FCPARA.304
C...LET IS THE MODEL LEVEL JUST BELOW THE EQUILIBRIUM TEMPERATURE                FCPARA.305
C...LEVEL. ACP IS THE ANVIL CONDENSATION PRODUCTION RATE ABOVE THE LET.          FCPARA.306
C...CLIQ IS THE RATE OF LIQUID PRODUCTION UP TO ABOUT 150 MB ABOVE THE           FCPARA.307
C...  LCL AND USR IS THE UPDRAFT SUPPLY RATE.                                    FCPARA.308
C                                                                                FCPARA.309
        UMF(K)=0.01*DXSQ*WO*POO*(TLCL/THATA)**CPOR/(R*TLCL)                      FCPARA.310
        UU(K)=(U0(LC)*DZQ(LC)+U0(LC1)*DZQ(LC1))*WT                               FCPARA.311
        VU(K)=(V0(LC)*DZQ(LC)+V0(LC1)*DZQ(LC1))*WT                               FCPARA.312
        DO 140 NK=K,LTOP                                                         FCPARA.313
          NK1=NK+1                                                               FCPARA.314
C            WGHT = CVMGT(WGLCL,WGT(NK),NK.EQ.K)                                 FCPARA.315
          IF(NK.EQ.K)THEN                                                        FCPARA.316
            WGHT=WGLCL                                                           FCPARA.317
          ELSE                                                                   FCPARA.318
            WGHT=WGT(NK)                                                         FCPARA.319
          ENDIF                                                                  FCPARA.320
          IF(NK.NE.K)THEN                                                        FCPARA.321
            UU(NK1)=(UU(NK)+WGHT*.5*(U0(NK)+U0(NK1)))*WGO(NK)                    FCPARA.322
            VU(NK1)=(VU(NK)+WGHT*.5*(V0(NK)+V0(NK1)))*WGO(NK)                    FCPARA.323
          ELSE                                                                   FCPARA.324
            UU(NK1)=(UU(K)+WGHT*.5*(U0(K)*(1.-DZLDZA)+U0(LCL)*(1.+DZLDZA         FCPARA.325
     +              )))/(1.+WGLCL)                                               FCPARA.326
            VU(NK1)=(VU(K)+WGHT*.5*(V0(K)*(1.-DZLDZA)+V0(LCL)*(1.+DZLDZA         FCPARA.327
     +              )))/(1.+WGLCL)                                               FCPARA.328
          ENDIF                                                                  FCPARA.329
          RU(NK1)=P0(NK1)/(R*TVU(NK1))                                           FCPARA.330
          IF(NK1.LE.LET)THEN                                                     FCPARA.331
            UMF(NK1)=UMF(NK)*(1.+WGHT)                                           FCPARA.332
          ELSE                                                                   FCPARA.333
            UMF(NK1)=UMF(LET)*(1.-(Z0(NK1)-ZETL)/(ZTOP-ZETL))                    FCPARA.334
            DET(NK1)=UMF(NK)-UMF(NK1)                                            FCPARA.335
            ACP=ACP+.5*(UMF(NK)+UMF(NK1))*DQU(NK)                                FCPARA.336
          ENDIF                                                                  FCPARA.337
          AU(NK1)=UMF(NK1)/(RU(NK1)*WU(NK1))                                     FCPARA.338
          TRCP=TRCP+.5*(UMF(NK)+UMF(NK1))*DQU(NK)                                FCPARA.339
          IF(P0(NK).LT.P165)GOTO 140                                             FCPARA.340
          CLIQ=TRCP                                                              FCPARA.341
          LVF=NK                                                                 FCPARA.342
  140   CONTINUE                                                                 FCPARA.343
        USR=UMF(LVF+1)*QU(LVF+1)+CLIQ                                            FCPARA.344
        USR=AMIN1(USR,.95*TRCP)                                                  FCPARA.345
C                                                                                FCPARA.346
C...INITIALIZE SOME ARRAYS FOR LATER USE.                                        FCPARA.347
C                                                                                FCPARA.348
        DO 150 N=1,K                                                             FCPARA.349
          AU(N)=0.                                                               FCPARA.350
          TU(N)=0.                                                               FCPARA.351
          QU(N)=0.                                                               FCPARA.352
          UU(N)=0.                                                               FCPARA.353
          VU(N)=0.                                                               FCPARA.354
          UMF(N)=0.                                                              FCPARA.355
          RU(N)=0.                                                               FCPARA.356
          WU(N)=0.                                                               FCPARA.357
  150   CONTINUE                                                                 FCPARA.358
        DO 160 NK=LTOP1,KX                                                       FCPARA.359
          AU(NK)=0.                                                              FCPARA.360
          UMF(NK)=0.                                                             FCPARA.361
          UG(NK)=U0(NK)                                                          FCPARA.362
          VG(NK)=V0(NK)                                                          FCPARA.363
          TG(NK)=T0(NK)                                                          FCPARA.364
          QG(NK)=Q0(NK)                                                          FCPARA.365
  160   CONTINUE                                                                 FCPARA.366
        EY(1)=0.                                                                 FCPARA.367
        FU(1)=U0(1)                                                              FCPARA.368
        FV(1)=V0(1)                                                              FCPARA.369
        FT(1)=T0(1)                                                              FCPARA.370
        FQ(1)=Q0(1)                                                              FCPARA.371
        DO 170 NK=1,LTOP1                                                        FCPARA.372
          UE(NK)=U0(NK)                                                          FCPARA.373
          VE(NK)=V0(NK)                                                          FCPARA.374
          WPD(NK)=U0(NK)*U0(NK)+V0(NK)*V0(NK)                                    FCPARA.375
          TZ(NK)=0.                                                              FCPARA.376
          QD(NK)=0.                                                              FCPARA.377
          AD(NK)=0.                                                              FCPARA.378
          WD(NK)=0.                                                              FCPARA.379
          UD(NK)=0.                                                              FCPARA.380
          VD(NK)=0.                                                              FCPARA.381
          RD(NK)=0.                                                              FCPARA.382
          DQD(NK)=0.                                                             FCPARA.383
          DMF(NK)=0.                                                             FCPARA.384
  170   CONTINUE                                                                 FCPARA.385
        DO 180 NK=2,LTOP                                                         FCPARA.386
          ZA(NK)=DZA(NK-1)/(DZA(NK)*(Z0(NK+1)-Z0(NK-1)))                         FCPARA.387
          ZB(NK)=DZA(NK)/(DZA(NK-1)*(Z0(NK+1)-Z0(NK-1)))                         FCPARA.388
  180   CONTINUE                                                                 FCPARA.389
C        PRINT 1000,J,I,KLCL,ZLCL,DTLCL,LTOP,P0(LTOP),IFLAG,TMIX-TO,TDPT         FCPARA.390
C    *      -TO,PMIX,QMIX,ABE                                                    FCPARA.391
 1000    FORMAT (/,2X,'J=',I2,' I=',I2,' KLCL=',I2,' ZLCL=',F7.1,'M',            FCPARA.392
     *      ' DTLCL=',F5.2,' LTOP=',I2,' P(TOP)=',-2PF5.1,'MB FRZ LV=',          FCPARA.393
     *      I2,' TMIX,TD=',2(0PF4.1,1X),'PMIX=',-2PF5.1,' QMIX=',3PF5.1,         FCPARA.394
     *      'ABE=',F10.1)                                                        FCPARA.395
C                                                                                FCPARA.396
C...COMPUTE CONVECTIVE TIME SCALE(TIMEC). THE MEAN WIND AT THE LCL               FCPARA.397
C...AND MIDTROPOSPHERE IS USED.                                                  FCPARA.398
C                                                                                FCPARA.399
        VCONV=.5*(SQRT(WPD(KLCL))+SQRT(WPD(L5)))                                 FCPARA.400
        TIMEC=DX/VCONV                                                           FCPARA.401
        TIMEC=AMAX1(1800.,TIMEC)                                                 FCPARA.402
        TIMEC=AMIN1(3600.,TIMEC)                                                 FCPARA.403
        NIC=NINT(TIMEC/(.5*DT2))                                                 FCPARA.404
        TIMEC=NIC*.5*DT2                                                         FCPARA.405
        DTIME=.1*TIMEC                                                           FCPARA.406
C                                                                                FCPARA.407
C...COMPUTE WIND SHEAR AND PRECIPITATION EFFICIENCY.                             FCPARA.408
C                                                                                FCPARA.409
C        SHSIGN = CVMGT(1.,-1.,WPD(LTOP).GT.WPD(LCL))                            FCPARA.410
        IF(WPD(LTOP).GT.WPD(KLCL))THEN                                           FCPARA.411
          SHSIGN=1.                                                              FCPARA.412
        ELSE                                                                     FCPARA.413
          SHSIGN=-1.                                                             FCPARA.414
        ENDIF                                                                    FCPARA.415
        VWS=(U0(LTOP)-U0(LCL))*(U0(LTOP)-U0(LCL))+(V0(LTOP)-V0(LCL))*(           FCPARA.416
     +      V0(LTOP)-V0(LCL))                                                    FCPARA.417
        VWS=1.E3*SHSIGN*SQRT(VWS)/(Z0(LTOP)-Z0(LCL))                             FCPARA.418
        PEF=1.591+VWS*(-.639+VWS*(9.53E-2-VWS*4.96E-3))                          FCPARA.419
        PEF=AMAX1(PEF,.2)                                                        FCPARA.420
        PEF=AMIN1(PEF,.9)                                                        FCPARA.421
C                                                                                FCPARA.422
C...PRECIPITATION EFFICIENCY IS A FUNCTION OF THE HEIGHT OF CLOUD BASE.          FCPARA.423
C                                                                                FCPARA.424
        CBH=(ZLCL-Z0(1))*3.281E-3                                                FCPARA.425
        IF(CBH.LT.3.)THEN                                                        FCPARA.426
          RCBH=.02                                                               FCPARA.427
        ELSE                                                                     FCPARA.428
          RCBH=.96729352+CBH*(-.70034167+CBH*(.162179896+CBH*(-                  FCPARA.429
     +         1.2569798E-2+CBH*(4.2772E-4-CBH*5.44E-6))))                       FCPARA.430
        ENDIF                                                                    FCPARA.431
        IF(CBH.GT.25)RCBH=2.4                                                    FCPARA.432
        PEFCBH=1./(1.+RCBH)                                                      FCPARA.433
        PEFCBH=AMIN1(PEFCBH,.9)                                                  FCPARA.434
C                                                                                FCPARA.435
C... MEAN PEF. IS USED TO COMPUTE RAINFALL.                                      FCPARA.436
C                                                                                FCPARA.437
        PEFF=.5*(PEF+PEFCBH)                                                     FCPARA.438
C                                                                                FCPARA.439
C...COMPUTE PARTITION OF CONDENSATE AMONG CONVECTIVE PRODUCTION RATE             FCPARA.440
C...(CPR), ANVIL EVARATION(ACP) AND CONDENSATE EVAPORATION IN THE                FCPARA.441
C...DOWNDRAFTS(RCED).                                                            FCPARA.442
C                                                                                FCPARA.443
        CPR=PEFF*USR                                                             FCPARA.444
        RCE=TRCP-CPR                                                             FCPARA.445
        ACP=AMAX1(ACP,.05*TRCP)                                                  FCPARA.446
        ACP=AMIN1(ACP,.15*TRCP)                                                  FCPARA.447
        RCED=RCE-ACP                                                             FCPARA.448
C        PRINT 1010,PEF,PEFCBH,LC,LET,WKL,PTEN(I),ISHR,VWS,TRCP,USR/             FCPARA.449
C    *      TRCP,CPR/TRCP,RCED/TRCP,ACP/TRCP                                     FCPARA.450
 1010    FORMAT (1X,'PEF(WS)=',F4.2,'(CB)=',F4.2,'LC,LET=',2I3,'WKL=',           FCPARA.451
     *      F6.3,'PTEN,IWSR,VWS=',E9.2,I3,F5.2,'TRCP=',E10.3,                    FCPARA.452
     *      'USR,CPR,RCED,AER=',4F5.2,'PERCENT')                                 FCPARA.453
C                                                                                FCPARA.454
C*****************************************************************               FCPARA.455
C                                                                *               FCPARA.456
C                  COMPUTE DOWNDRAFT PROPERTIES                  *               FCPARA.457
C                                                                *               FCPARA.458
C*****************************************************************               FCPARA.459
C                                                                                FCPARA.460
        DTMELT=3.34E5*.5*SDQU/CP                                                 FCPARA.461
        NLFS=MIN0(L4,LET)                                                        FCPARA.462
  190   NLFS=MAX0(NLFS-1,KLCL+1)                                                 FCPARA.463
        NLL=NLFS-KLCL                                                            FCPARA.464
C         IFLG = CVMGT(0,99,NLL.EQ.1)                                            FCPARA.465
        IF(NLL.EQ.1)THEN                                                         FCPARA.466
          IFLG=0                                                                 FCPARA.467
        ELSE                                                                     FCPARA.468
          IFLG=99                                                                FCPARA.469
        ENDIF                                                                    FCPARA.470
        TDER=0.                                                                  FCPARA.471
        IFLAG=0                                                                  FCPARA.472
C                                                                                FCPARA.473
C...COMPUTE THE LEVEL OF FREE SINK(LFS). 50 TO 50 PERCENTDXSQE OF UPDRAF         FCPARA.474
C...AND ENVIRONMENTAL AIR IS MIXED.                                              FCPARA.475
C                                                                                FCPARA.476
        DO 200 NK=1,NLL                                                          FCPARA.477
          LFS=NLFS-NK                                                            FCPARA.478
          THBAR=.5*(THM(LFS)+THE(LFS))                                           FCPARA.479
            TZ(LFS) = TP(P0(LFS),THBAR,T0(LFS),D273,RL(LFS),QS,PI(LFS),          FCPARA.480
     *                XTIME,LFS,'LSF ',JFLAG)                                    FCPARA.481
          IF(JFLAG.EQ.1)GOTO 649                                                 FCPARA.482
          IF(TZ(LFS).LT.T0(LFS))GOTO 210                                         FCPARA.483
  200   CONTINUE                                                                 FCPARA.484
  210   THM(LFS)=THBAR                                                           FCPARA.485
        LFSML=LFS-1                                                              FCPARA.486
C                                                                                FCPARA.487
C...DOWNDRAFT PROPERTIES AT THE LFS. IF THE LFS IS BELOW 0 DEG.,                 FCPARA.488
C...MELTING OF CONDENSATE IS ALLOWED.                                            FCPARA.489
C                                                                                FCPARA.490
        UD(LFS)=.5*(U0(LFS)+UU(LFS))                                             FCPARA.491
        VD(LFS)=.5*(V0(LFS)+VU(LFS))                                             FCPARA.492
        IF(TZ(LFS).GE.TO)THEN                                                    FCPARA.493
          TZ(LFS)=TZ(LFS)-DTMELT                                                 FCPARA.494
          ES=611.*EXP(RL(LFS)/RV*(D273-1./TZ(LFS)))                              FCPARA.495
          QS=.622*ES/(P0(LFS)-ES)                                                FCPARA.496
          THM(LFS)=TZ(LFS)*PI(LFS)*EXP(RL(LFS)*QS/(CP*TZ(LFS)))                  FCPARA.497
          IFLAG=99                                                               FCPARA.498
          ML=LFS                                                                 FCPARA.499
        ENDIF                                                                    FCPARA.500
        QD(LFS)=RHIC*QS                                                          FCPARA.501
        TVD(LFS)=TZ(LFS)*(1.+B61*QD(LFS))                                        FCPARA.502
        RD(LFS)=P0(LFS)/(R*TVD(LFS))                                             FCPARA.503
C                                                                                FCPARA.504
C...ASSUME INITIAL DOWNDRAFT VELOCITY(WD) OF -1 M/S, AND AREA(AD)                FCPARA.505
C...PROPORTIONAL TO UPDRAFT AIR.                                                 FCPARA.506
C                                                                                FCPARA.507
        WD(LFS)=-WO                                                              FCPARA.508
        WTW=-1.                                                                  FCPARA.509
        AD(LFS)=RCED*0.01*DXSQ/USR                                               FCPARA.510
        DMF(LFS)=RD(LFS)*AD(LFS)*WD(LFS)                                         FCPARA.511
C                                                                                FCPARA.512
C...LDB IS SUPPOSIVELY THE LOWEST LEVEL THE DOWNDRAFT CAN PENETRATE .            FCPARA.513
C                                                                                FCPARA.514
        LDB=LFS                                                                  FCPARA.515
C                                                                                FCPARA.516
C...COMPUTE DOWNDRAFT PROPERTIES BELOW THE LFS. TDER IS TOTAL                    FCPARA.517
C...EVAPORATION RATE IN THE DOWNDRAFTS.                                          FCPARA.518
C                                                                                FCPARA.519
        DO 240 NK=1,LFSML                                                        FCPARA.520
          ND=LFSML+2-NK                                                          FCPARA.521
          ND1=ND-1                                                               FCPARA.522
C            RHCLD = CVMGT(RHBC,RHIC,ND1.LT.KLCL)                                FCPARA.523
          IF(ND1.LT.KLCL)THEN                                                    FCPARA.524
            RHCLD=RHBC                                                           FCPARA.525
          ELSE                                                                   FCPARA.526
            RHCLD=RHIC                                                           FCPARA.527
          ENDIF                                                                  FCPARA.528
          UD(ND1)=(UD(ND)+.5*WGT(ND1)*(U0(ND)+U0(ND1)))*WGO(ND1)                 FCPARA.529
          VD(ND1)=(VD(ND)+.5*WGT(ND1)*(V0(ND)+V0(ND1)))*WGO(ND1)                 FCPARA.530
          THM(ND1)=(THM(ND)+.5*WGT(ND1)*(THE(ND)+THE(ND1)))*WGO(ND1)             FCPARA.531
            TZ(ND1) = TP(P0(ND1),THM(ND1),T0(ND1),D273,RL(ND1),QSD,              FCPARA.532
     *         PI(ND1),XTIME,ND1,'ND11',JFLAG)                                   FCPARA.533
          IF(JFLAG.EQ.1)GOTO 649                                                 FCPARA.534
          QPRME=(QD(ND)+.5*WGT(ND1)*(Q0(ND)+Q0(ND1)))*WGO(ND1)                   FCPARA.535
          QD(ND1)=RHCLD*QSD                                                      FCPARA.536
          IF(IFLAG.EQ.99)GOTO 220                                                FCPARA.537
C                                                                                FCPARA.538
C...CHECK FOR MELTING LEVEL IF THE LFS IS ABOVE THE LEVEL OF TO.                 FCPARA.539
C                                                                                FCPARA.540
          IF(TZ(ND1).LE.TO)GOTO 220                                              FCPARA.541
          TZ(ND1)=TZ(ND1)-DTMELT                                                 FCPARA.542
          ES=611.*EXP(RL(ND1)/RV*(D273-1./TZ(ND1)))                              FCPARA.543
          QS=.622*ES/(P0(ND1)-ES)                                                FCPARA.544
          QD(ND1)=RHCLD*QS                                                       FCPARA.545
          THM(ND1)=TZ(ND1)*PI(ND1)*EXP(RL(ND1)*QS/(CP*TZ(ND1)))                  FCPARA.546
          IFLAG=99                                                               FCPARA.547
          ML=ND1                                                                 FCPARA.548
  220     DQD(ND)=QD(ND1)-QPRME                                                  FCPARA.549
          IF(RHCLD.LT.1.)THEN                                                    FCPARA.550
            DM=(QSD-QD(ND1))/(1.+RL(ND1)*RL(ND1)*QSD/(462844.*TZ(ND1)*           FCPARA.551
     +         TZ(ND1)))                                                         FCPARA.552
            TZ(ND1)=TZ(ND1)+RL(ND1)*DM/CP                                        FCPARA.553
          ENDIF                                                                  FCPARA.554
          TVD(ND1)=TZ(ND1)*(1.+B61*QD(ND1))                                      FCPARA.555
          RD(ND1)=P0(ND1)/(R*TVD(ND1))                                           FCPARA.556
          WTW=WTW+19.62*DZA(ND1)*(BVRT*(TVD(ND)+TVD(ND1))/(TVG(ND)+              FCPARA.557
     +        TVG(ND1))-BVRT-BU*WTW/G)                                           FCPARA.558
          WD(ND1)=WTW/SQRT(ABS(WTW)+1.E-10)                                      FCPARA.559
C                                                                                FCPARA.560
C...CHECK IF DOWNDRAFT HAS ENCOUNTERED COLDER AIR, LOST ITS NEGETIVE             FCPARA.561
C...BUOYANCY AND CAUSED WD TO BECOME POSITIVE.                                   FCPARA.562
C                                                                                FCPARA.563
          IF(WD(ND1).GE.0.)GOTO 230                                              FCPARA.564
          WGTDT=WGT(ND1)                                                         FCPARA.565
          IF(TVD(ND1).GT.TVG(ND1))WGTDT=-2.*WGT(ND1)                             FCPARA.566
          DMF(ND1)=DMF(ND)*(1.+WGTDT)                                            FCPARA.567
          AD(ND1)=DMF(ND1)/(RD(ND1)*WD(ND1))                                     FCPARA.568
          LDB=ND1                                                                FCPARA.569
          GOTO 240                                                               FCPARA.570
C                                                                                FCPARA.571
  230     IF(IFLG.EQ.99)GOTO 190                                                 FCPARA.572
  240   TDER=TDER+.5*(DMF(ND)+DMF(ND1))*DQD(ND)                                  FCPARA.573
C                                                                                FCPARA.574
C...THE DOWNDRAFT AREA AND MASS FLUX ARE ADJUSTED ACCORDING TO                   FCPARA.575
C...SPECIFIED EVAPORATION RATE AND ACTUAL EVAPORATION RATE.                      FCPARA.576
C...ADU IS THE NUMBER OF UNITS OF DOWNDRAFT AIR.                                 FCPARA.577
C                                                                                FCPARA.578
        ADU=ABS(RCED/TDER)                                                       FCPARA.579
        DO 250 NK=1,LTOP                                                         FCPARA.580
          AD(NK)=AD(NK)*ADU                                                      FCPARA.581
          DMF(NK)=DMF(NK)*ADU                                                    FCPARA.582
          IF(AU(NK).GT.0.9*DXSQ)AU(NK)=0.9*DXSQ                                  FCPARA.583
          AU2(NK)=AU(NK)                                                         FCPARA.584
          AD2(NK)=AD(NK)                                                         FCPARA.585
          DM2(NK)=DMF(NK)                                                        FCPARA.586
  250   CONTINUE                                                                 FCPARA.587
        AUMX=AU2(1)                                                              FCPARA.588
        NUMAX=1                                                                  FCPARA.589
        DO 451 NK=2,LTOP                                                         FCPARA.590
          IF(AU2(NK).GT.AUMX)THEN                                                FCPARA.591
            AUMX=AU2(NK)                                                         FCPARA.592
            NUMAX=NK                                                             FCPARA.593
          ENDIF                                                                  FCPARA.594
  451   CONTINUE                                                                 FCPARA.595
        KLEVAP=LTOP-KLCL                                                         FCPARA.596
        NCOUNT=0                                                                 FCPARA.597
        AINC=2.                                                                  FCPARA.598
        AER=ACP                                                                  FCPARA.599
        CPRN=CPR                                                                 FCPARA.600
        DFBEM=-0.5                                                               FCPARA.601
        DFBEP=0.5                                                                FCPARA.602
        AINCP=3.                                                                 FCPARA.603
        AINCM=1.                                                                 FCPARA.604
C                                                                                FCPARA.605
C...BEGIN WITH THE LOWEST LAYER, THE DOWNDRAFT AREA IS SUCCESSIVELY              FCPARA.606
C...FILLED USING THE DOWNDRAFT MASS FLOW AT THE LDB DURING THE                   FCPARA.607
C...CONVECTIVE TIME PERIOD(TIMEC).                                               FCPARA.608
  270   NCOUNT=NCOUNT+1                                                          FCPARA.609
        DXSQD=0.9*DXSQ                                                           FCPARA.610
        DM=-DMF(LDB)*TIMEC                                                       FCPARA.611
        DO 280 NK=LDB,LFS                                                        FCPARA.612
          IF(NK.GE.LCL)DXSQD=.6*DXSQ                                             FCPARA.613
          AD(NK)=AD(NK)+DM/(RD(NK)*DZQ(NK))                                      FCPARA.614
          IF(AD(NK).LE.DXSQD)GOTO 290                                            FCPARA.615
          AD(NK)=DXSQD                                                           FCPARA.616
          DM=DM-RD(NK)*DZQ(NK)*(DXSQD-AD2(NK))                                   FCPARA.617
  280   CONTINUE                                                                 FCPARA.618
  290   CONTINUE                                                                 FCPARA.619
        IF(NCOUNT.EQ.1)GOTO 440                                                  FCPARA.620
C                                                                                FCPARA.621
C*****************************************************************               FCPARA.622
C                                                                *               FCPARA.623
C           COMPUTE PROPERTIES FOR COMPENSATIONAL SUBSIDENCE     *               FCPARA.624
C                                                                *               FCPARA.625
C*****************************************************************               FCPARA.626
C                                                                                FCPARA.627
C...OBTAIN ENVIRONMENTAL VERTICAL MOTION(WE), AREA(AE),AND REASONABLE            FCPARA.628
C...TIME-STEP FOR COMPUTING TENDENCY EQUATION.                                   FCPARA.629
C                                                                                FCPARA.630
        DO 300 NK=1,LTOP1                                                        FCPARA.631
          AE(NK)=DXSQ-AU(NK)-AD(NK)                                              FCPARA.632
          IF(AE(NK).LT.0.1*DXSQ)THEN                                             FCPARA.633
            AE(NK)=0.1*DXSQ                                                      FCPARA.634
            AD(NK)=DXSQ-AE(NK)-AU(NK)                                            FCPARA.635
          ENDIF                                                                  FCPARA.636
          WE(NK)=-(UMF(NK)+DET(NK)+DMF(NK))/(RO(NK)*AE(NK))                      FCPARA.637
          WE(NK)=AMIN1(.5,WE(NK))                                                FCPARA.638
          WE(NK)=AMAX1(WE(NK),-2.)                                               FCPARA.639
          DTT=.6*DZA(NK)/(ABS(WE(NK))+1.E-4)                                     FCPARA.640
          DTIME=AMIN1(DTIME,DTT)                                                 FCPARA.641
          QE(NK)=Q0(NK)                                                          FCPARA.642
          TE(NK)=T0(NK)                                                          FCPARA.643
C      IF(XTIME.GE.330. .AND. I.EQ.33 .AND. J.EQ.48)PRINT *,                     FCPARA.644
C     *  'NK, AE, AU, AD, WE =',NK,AE(NK)/DXSQ,AU(NK)/DXSQ,                      FCPARA.645
C     *                         AD(NK)/DXSQ,WE(NK)                               FCPARA.646
  300   CONTINUE                                                                 FCPARA.647
        NSTEP=NINT(TIMEC/DTIME)                                                  FCPARA.648
C                                                                                FCPARA.649
C...COMPUTE TEMPERATURE(TE) MIXING RATIO(RE) AND ANVIL EVAPORATION               FCPARA.650
C...IN THE SUBSIDENCE.                                                           FCPARA.651
C                                                                                FCPARA.652
        DTMEA=ACP*TIMEC/FLOAT(NSTEP)                                             FCPARA.653
        DO 360 NTC=1,NSTEP                                                       FCPARA.654
          DO 310 NK=2,LTOP                                                       FCPARA.655
            ALF(NK)=-WE(NK)*DTIME*ZA(NK)                                         FCPARA.656
            BTA(NK)=WE(NK)*DTIME*ZB(NK)                                          FCPARA.657
            BM(NK)=1.+ALF(NK)+BTA(NK)*(1.-EY(NK-1))                              FCPARA.658
            EY(NK)=ALF(NK)/BM(NK)                                                FCPARA.659
            FT(NK)=(TE(NK)+WE(NK)*DTIME*GDRY+BTA(NK)*FT(NK-1))/BM(NK)            FCPARA.660
            FQ(NK)=(QE(NK)+BTA(NK)*FQ(NK-1))/BM(NK)                              FCPARA.661
  310     CONTINUE                                                               FCPARA.662
          DO 320 NK=2,LTOPM1                                                     FCPARA.663
            NJ=LTOP1-NK                                                          FCPARA.664
            TE(NJ)=EY(NJ)*TE(NJ+1)+FT(NJ)                                        FCPARA.665
            QE(NJ)=EY(NJ)*QE(NJ+1)+FQ(NJ)                                        FCPARA.666
  320     CONTINUE                                                               FCPARA.667
C                                                                                FCPARA.668
C...INCORPORATE DETRAINMENT EFFECT INTO COMPENSATING SUBSIDENCE.                 FCPARA.669
C                                                                                FCPARA.670
          DO 330 NK=LET,LTOP                                                     FCPARA.671
            DTRNM=DET(NK)*DTIME/(RO(NK)*AE(NK)*DZQ(NK))                          FCPARA.672
            TE(NK)=TE(NK)-RL(NK)*DTRNM*DQU(NK)/CP                                FCPARA.673
            QE(NK)=QE(NK)+DTRNM*DQU(NK)                                          FCPARA.674
  330     CONTINUE                                                               FCPARA.675
          DMEA=DTMEA                                                             FCPARA.676
          DO 340 NKP=1,KLEVAP                                                    FCPARA.677
            NK=LTOP1-NKP                                                         FCPARA.678
            ES=611.*EXP(RL(NK)/RV*(D273-1./TE(NK)))                              FCPARA.679
            QS=.622*ES/(P0(NK)-ES)                                               FCPARA.680
            IF(QE(NK).GE.QS)GOTO 340                                             FCPARA.681
            DM=(QE(NK)-QS)/(1.+RL(NK)*RL(NK)*QS/(462844.*TE(NK)*TE(NK)))         FCPARA.682
            CEV=RO(NK)*AE(NK)*DZQ(NK)                                            FCPARA.683
            EVAP=AMIN1(-DM,DMEA/CEV)                                             FCPARA.684
            QE(NK)=QE(NK)+EVAP                                                   FCPARA.685
            TE(NK)=TE(NK)-RL(NK)*EVAP/CP                                         FCPARA.686
            DMEA=DMEA-CEV*EVAP                                                   FCPARA.687
            IF(DMEA.LE.1.E-6)GOTO 360                                            FCPARA.688
  340     CONTINUE                                                               FCPARA.689
  360   CONTINUE                                                                 FCPARA.690
C                                                                                FCPARA.691
C...OBTAIN AREA AVERAGED VALUES OF TEMPERATURE AND MIXING RATIO.                 FCPARA.692
C                                                                                FCPARA.693
        DO 370 NK=1,LTOP                                                         FCPARA.694
          TG(NK)=(AE(NK)*TE(NK)+AU(NK)*TU(NK)+AD(NK)*TZ(NK))/DXSQ                FCPARA.695
          IF(TG(NK).GT.333.0.OR.TG(NK).LT.150.0)THEN                             FCPARA.696
            TG(NK)=T0(NK)                                                        FCPARA.697
       PRINT *, 'TG ERROR!! NK=',NK                                              FCPARA.698
       PRINT *, 'TG=',TG(NK),'TE=',TE(NK),'TZ',TZ(NK),'TU=',TU(NK)               FCPARA.699
       PRINT *, 'AE=',AE(NK),'AU=',AU(NK),'AD',AD(NK)                            FCPARA.700
       PRINT *, 'SET TG=T0'                                                      FCPARA.701
          ENDIF                                                                  FCPARA.702
          QG(NK)=(AE(NK)*QE(NK)+AU(NK)*QU(NK)+AD(NK)*QD(NK))/DXSQ                FCPARA.703
          QG(NK)=AMAX1(QG(NK),1.E-10)                                            FCPARA.704
          TVG(NK)=TG(NK)*(1.+B61*QG(NK))                                         FCPARA.705
  370   CONTINUE                                                                 FCPARA.706
        IF(NCOUNT.GE.10)GOTO 460                                                 FCPARA.707
        IF(NCOUNT.LE.2.AND.ZLCL.GT.3.E3)GOTO 460                                 FCPARA.708
C                                                                                FCPARA.709
C*******************************************************************             FCPARA.710
C                                                                  *             FCPARA.711
C     COMPUTE NEW CLOUD AND CHANGE IN AVAILABLE BUOYANT ENERGY.    *             FCPARA.712
C                                                                  *             FCPARA.713
C*******************************************************************             FCPARA.714
C                                                                                FCPARA.715
C...THE FOLLOWING COMPUTATIONS ARE SIMILAR TO THAT FOR UPDRAFT                   FCPARA.716
C                                                                                FCPARA.717
        TMIX=(TG(LC)*DZQ(LC)+TG(LC1)*DZQ(LC1))*WT                                FCPARA.718
        QMIX=(QG(LC)*DZQ(LC)+QG(LC1)*DZQ(LC1))*WT                                FCPARA.719
        ES=611.*EXP(RL(LC1)/RV*(D273-1./TMIX))                                   FCPARA.720
        QS=.622*ES/(PMIX-ES)                                                     FCPARA.721
        QMIX=AMIN1(QS,QMIX)                                                      FCPARA.722
        TDPT=1./(D273-RV/RL(LC1)*ALOG(PMIX*QMIX/(611.*(.622+QMIX))))             FCPARA.723
C         TDPT = CVMGT(TMIX,TDPT,TDPT.GT.TMIX)                                   FCPARA.724
        IF(TDPT.GT.TMIX)THEN                                                     FCPARA.725
          TDPT=TMIX                                                              FCPARA.726
        ELSE                                                                     FCPARA.727
          TDPT=TDPT                                                              FCPARA.728
        ENDIF                                                                    FCPARA.729
        TLCL=TDPT-(.212+1.571E-3*(TDPT-TO)-4.36E-4*(TMIX-TO))*(TMIX-TDPT         FCPARA.730
     +       )                                                                   FCPARA.731
        ZLCL=ZMIX+(TLCL-TMIX)/GDRY                                               FCPARA.732
        DO 380 NK=LC,L4                                                          FCPARA.733
          KLCL=NK                                                                FCPARA.734
          IF(ZLCL.LE.Z0(NK))GOTO 390                                             FCPARA.735
  380   CONTINUE                                                                 FCPARA.736
  390   K=KLCL-1                                                                 FCPARA.737
        DZLDZA=(ZLCL-Z0(K))/DZA(K)                                               FCPARA.738
        TENV=TG(K)+(TG(KLCL)-TG(K))*DZLDZA                                       FCPARA.739
        TVEN=TENV*(1.+B61*(QG(K)+(QG(KLCL)-QG(K))*DZLDZA))                       FCPARA.740
        THATA=TMIX*(POO/PMIX)**ROVCP                                             FCPARA.741
C                                                                                FCPARA.742
C...RECOMPUTE THETAE FOR THE ADJUSTED STRATIFICATION.                            FCPARA.743
C                                                                                FCPARA.744
        DO 400 NK=K,LTOP1                                                        FCPARA.745
          TDP(NK)=1./(D273-RV/RL(NK)*                                            FCPARA.746
     +            ALOG(P0(NK)*QG(NK)/(611.*(QG(NK)+.622))))                      FCPARA.747
          TDP(NK)=AMIN1(TG(NK),TDP(NK))                                          FCPARA.748
C            TST(NK) = CVMGT(TDP(NK)-(.212+1.571E-3*(TDP(NK)-TO)-4.36E-4         FCPARA.749
C     *         (TG(NK)-TO))*(TG(NK)-TDP(NK)),TDP(NK)-(.182+1.13E-3*(TDP         FCPARA.750
C     *         NK)-TO)-3.58E-4*(TG(NK)-TO))*(TG(NK)-TDP(NK)),TDP(NK).GT         FCPARA.751
C     *         TICE)                                                            FCPARA.752
          IF(TDP(NK).GT.TICE)THEN                                                FCPARA.753
            TST(NK)=TDP(NK)-(.212+1.571E-3*(TDP(NK)-TO)-4.36E-4*(TG(NK)-         FCPARA.754
     +              TO))*(TG(NK)-TDP(NK))                                        FCPARA.755
          ELSE                                                                   FCPARA.756
            TST(NK)=TDP(NK)-(.182+1.13E-3*(TDP(NK)-TO)-3.58E-4*(TG(NK)-          FCPARA.757
     +              TO))*(TG(NK)-TDP(NK))                                        FCPARA.758
          ENDIF                                                                  FCPARA.759
          THE(NK)=TG(NK)*PI(NK)*EXP(RL(NK)*QG(NK)/(CP*TST(NK)))                  FCPARA.760
  400   CONTINUE                                                                 FCPARA.761
C                                                                                FCPARA.762
        IFLAG=0                                                                  FCPARA.763
        ABEG=0.                                                                  FCPARA.764
        SDQU=0.                                                                  FCPARA.765
        WGLCL=EXP(BU*(Z0(KLCL)-ZLCL))-1.                                         FCPARA.766
        TSU=TLCL                                                                 FCPARA.767
        QSU=QMIX                                                                 FCPARA.768
        THM(K)=THATA*EXP((C1-C2*(TLCL-TO))*QMIX/(CP*TLCL))                       FCPARA.769
        TGU=TSU*(1.+B61*QSU)                                                     FCPARA.770
C                                                                                FCPARA.771
C...COMPUTE ADJUSTED ABE(ABEG).                                                  FCPARA.772
C                                                                                FCPARA.773
        DO 420 N=K,LTOP                                                          FCPARA.774
          IF(IFLAG.EQ.10)GOTO 410                                                FCPARA.775
          IF(TSU.GT.TICE)GOTO 410                                                FCPARA.776
          DFRZ=DTFRZ(TSU,P0(N),QSU,.5*SDQU,D273)                                 FCPARA.777
          TSU=TSU+DFRZ                                                           FCPARA.778
          ES=611.*EXP(RL(N)/RV*(D273-1./TSU))                                    FCPARA.779
          QSU=.622*ES/(P0(N)-ES)                                                 FCPARA.780
          TGU=TSU*(1.+B61*QSU)                                                   FCPARA.781
          THM(N)=TSU*PI(N)*EXP(RL(N)*QSU/(CP*TSU))                               FCPARA.782
          IFLAG=10                                                               FCPARA.783
  410     N1=N+1                                                                 FCPARA.784
          IF(N.NE.K)THEN                                                         FCPARA.785
            THM(N1)=(THM(N)+WGT(N)*.5*(THE(N)+THE(N1)))*WGO(N)                   FCPARA.786
               TSV = TP(P0(N1),THM(N1),TG(N1),D273,RL(N1),QSV,PI(N1),            FCPARA.787
     *               XTIME,N1,'N11 ',JFLAG)                                      FCPARA.788
            IF(JFLAG.EQ.1)GOTO 649                                               FCPARA.789
            TGV=TSV*(1.+B61*QSV)                                                 FCPARA.790
            SDQU=SDQU+(QSU+WGT(N)*.5*(QG(N)+QG(N1)))*WGO(N)-QSV                  FCPARA.791
            BE=DZA(N)*BVRT*((TGV+TGU)/(TVG(N)+TVG(N1))-1.)                       FCPARA.792
          ELSE                                                                   FCPARA.793
            THM(N1)=(THM(N)+WGLCL*.5*(THE(K)*(1.-DZLDZA)+THE(KLCL)*(1.+          FCPARA.794
     +              DZLDZA)))/(1.+WGLCL)                                         FCPARA.795
               TSV = TP(P0(N1),THM(N1),TG(N1),D273,RL(N1),QSV,PI(N1),            FCPARA.796
     *                XTIME,N1,'N12 ',JFLAG)                                     FCPARA.797
            IF(JFLAG.EQ.1)GOTO 649                                               FCPARA.798
            TGV=TSV*(1.+B61*QSV)                                                 FCPARA.799
            SDQU=SDQU+(QSU+WGLCL*.5*(QG(K)*(1.-DZLDZA)+QG(KLCL)*(1.+             FCPARA.800
     +           DZLDZA)))/(1.+WGLCL)-QSV                                        FCPARA.801
            BE=(Z0(KLCL)-ZLCL)*BVRT*((TGV+TGU)/(TVG(KLCL)+TVEN)-1.)              FCPARA.802
          ENDIF                                                                  FCPARA.803
          TSU=TSV                                                                FCPARA.804
          QSU=QSV                                                                FCPARA.805
          TGU=TGV                                                                FCPARA.806
          IF(BE.GT.0.)ABEG=ABEG+BE                                               FCPARA.807
  420   CONTINUE                                                                 FCPARA.808
C                                                                                FCPARA.809
C...ASSUME MORE THAN 50% OF ABE IS REMOVED BY CONVECTION DURING THE PERI         FCPARA.810
C...TIMEC.                                                                       FCPARA.811
C                                                                                FCPARA.812
        DABE=AMAX1(ABE-ABEG,.2*ABE)                                              FCPARA.813
        FABE=ABEG/(ABE+1.E-8)                                                    FCPARA.814
        IF(FABE.LT..10.AND.FABE.GT.0.01)GOTO 460                                 FCPARA.815
        FBEA(NCOUNT)=FABE-.1                                                     FCPARA.816
        ACIN(NCOUNT)=AINC                                                        FCPARA.817
        IF(FABE.GT.0.1)DFBEP=AMIN1(DFBEP,FBEA(NCOUNT))                           FCPARA.818
        IF(FABE.LT.0.1)DFBEM=AMAX1(DFBEM,FBEA(NCOUNT))                           FCPARA.819
        AINC=AINC*ABEG/(DABE+1.E-8)*9.                                           FCPARA.820
        IF(NCOUNT.LE.4)GOTO 445                                                  FCPARA.821
        DO 435 NN=2,NCOUNT                                                       FCPARA.822
          IF(FBEA(NN).EQ.DFBEP)AINCP=ACIN(NN)                                    FCPARA.823
          IF(FBEA(NN).EQ.DFBEM)AINCM=ACIN(NN)                                    FCPARA.824
  435   CONTINUE                                                                 FCPARA.825
        AINC=AINCM-(AINCM-AINCP)*DFBEM/(DFBEM-DFBEP)                             FCPARA.826
  445   AUMAX=AMIN1(AUMX*AINC,0.9*DXSQ)                                          FCPARA.827
        AINC=AUMAX/(0.001+AUMX)                                                  FCPARA.828
  440   AINC=AMIN1(AINC,25.0)                                                    FCPARA.829
        AINC=AMAX1(AINC,1.0)                                                     FCPARA.830
        ACP=AER*AINC                                                             FCPARA.831
        CPR=CPRN*AINC                                                            FCPARA.832
        DO 450 NK=1,LTOP                                                         FCPARA.833
          AU(NK)=AU2(NK)*AINC                                                    FCPARA.834
          AD(NK)=AD2(NK)*AINC                                                    FCPARA.835
          DMF(NK)=DM2(NK)*AINC                                                   FCPARA.836
          UMF(NK)=RU(NK)*WU(NK)*AU(NK)                                           FCPARA.837
C            DET(NK)=CVMGT(0.,UMF(NK-1)-UMF(NK),NK.LE.LET)                       FCPARA.838
          IF(NK.LE.LET)THEN                                                      FCPARA.839
            DET(NK)=0.                                                           FCPARA.840
          ELSE                                                                   FCPARA.841
            DET(NK)=UMF(NK-1)-UMF(NK)                                            FCPARA.842
          ENDIF                                                                  FCPARA.843
  450   CONTINUE                                                                 FCPARA.844
        GOTO 270                                                                 FCPARA.845
C                                                                                FCPARA.846
C...COMPUTE CONVECTIVE OVEQSHOOTING, ASSUMING THAT THE TOTAL MASS                FCPARA.847
C...COOLED DOWN IS EQUAL TO THE MASS WARMED UP ABOVE THE LET.                    FCPARA.848
C                                                                                FCPARA.849
  460   DDT=T0(LET+1)-TU(LET+1)+TU(LET)-T0(LET)                                  FCPARA.850
        DZETL=DZA(LET)*(TU(LET)-T0(LET))/(ABS(DDT)+1.E-6)                        FCPARA.851
        TGETL=TG(LET)+(TG(LET+1)-TG(LET))*DZETL/DZA(LET)                         FCPARA.852
        T2ETL=T0(LET)+(T0(LET+1)-T0(LET))*DZETL/DZA(LET)                         FCPARA.853
        BEG=.5*(DZA(LET)-DZETL)*(TG(LET+1)-T0(LET+1)+TGETL-T2ETL)                FCPARA.854
        IF(LET+1.GE.LTOPM1)GOTO 480                                              FCPARA.855
        NK=LET+1                                                                 FCPARA.856
  470   BEG=BEG+.5*DZA(NK)*(TG(NK)-T0(NK)+TG(NK+1)-T0(NK+1))                     FCPARA.857
        IF(NK+1.GE.LTOPM1)GOTO 480                                               FCPARA.858
        NK=NK+1                                                                  FCPARA.859
        GOTO 470                                                                 FCPARA.860
C                                                                                FCPARA.861
  480   BEG=BEG+.125*DZA(LTOPM1)*(TG(LTOPM1)-T0(LTOPM1))                         FCPARA.862
        TG(LTOP)=T0(LTOP)-2.*BEG/(DZA(LTOP)+.5*DZA(LTOPM1))                      FCPARA.863
C                                                                                FCPARA.864
C...COMPUTE HORIZONTAL MOMENTUM IN THE COMPENSATING SUBSIDENCE                   FCPARA.865
C                                                                                FCPARA.866
        DO 510 N=1,NSTEP                                                         FCPARA.867
          DO 490 NK=2,LTOP                                                       FCPARA.868
            FU(NK)=(U0(NK)+BTA(NK)*FU(NK-1))/BM(NK)                              FCPARA.869
            FV(NK)=(V0(NK)+BTA(NK)*FV(NK-1))/BM(NK)                              FCPARA.870
  490     CONTINUE                                                               FCPARA.871
          DO 500 NK=2,LTOPM1                                                     FCPARA.872
            NJ=LTOP1-NK                                                          FCPARA.873
            UE(NJ)=EY(NJ)*UE(NJ+1)+FU(NJ)                                        FCPARA.874
            VE(NJ)=EY(NJ)*VE(NJ+1)+FV(NJ)                                        FCPARA.875
  500     CONTINUE                                                               FCPARA.876
  510   CONTINUE                                                                 FCPARA.877
C                                                                                FCPARA.878
C...OBTAIN AREA AVERAGED HORIZONTAL MOMENTUM.                                    FCPARA.879
C                                                                                FCPARA.880
        DO 520 NK=1,LTOP                                                         FCPARA.881
          UG(NK)=(AE(NK)*UE(NK)+AU(NK)*UU(NK)+AD(NK)*UD(NK))/DXSQ                FCPARA.882
          VG(NK)=(AE(NK)*VE(NK)+AU(NK)*VU(NK)+AD(NK)*VD(NK))/DXSQ                FCPARA.883
  520   CONTINUE                                                                 FCPARA.884
C                                                                                FCPARA.885
        IF(XTIME.LT.2.)THEN                                                      FCPARA.886
C     WRITE(98,3557)' '                                                          FCPARA.887
 3557     FORMAT (A80)                                                           FCPARA.888
C     WRITE(98,3558)I,J,XTIME                                                    FCPARA.889
3558  FORMAT('I =',I3,'J =',I3,' AT XTIME =',F10.3)                              FCPARA.890
C     WRITE(98,1020)LFS,NLFS,LDB,ML,P0(LFS),ADU,TDER,NCOUNT,FABE,TIMEC,          FCPARA.891
C    *      FREZ,DTMELT,AINC                                                     FCPARA.892
 1020    FORMAT (2X,'LFS,NLFS,LDB,ML=',4I3,'P(LFS)=',F7.0,'ADU,TDER=',           FCPARA.893
     *      F5.2,E9.3,'NCNT,FABE,TIMEC=',I2,F6.3,F6.0,'FRZ,MELT=',2F5.2,         FCPARA.894
     *      'AINC=',F5.1)                                                        FCPARA.895
C        IF(LTOP.GE.KX)THEN                                                      FCPARA.896
C         WRITE(98,1040)                                                         FCPARA.897
 1040 FORMAT (1X,' K   P0      Z0    TE    TU    TD    TG    T0    DT'           FCPARA.898
     *,'   UG   U0   VG   V0   WU   WD   WE   W0   QE   QU   QD   QG   '         FCPARA.899
     *,'Q0    AE  AU  AD')                                                       FCPARA.900
          DO 580 NK=1,LTOP1                                                      FCPARA.901
            K=LTOP1-NK+1                                                         FCPARA.902
            DTT=TG(K)-T0(K)                                                      FCPARA.903
C      WRITE(98,1050)K,P0(K),Z0(K),TE(K)-TO,TU(K)-TO,TZ(K)-TO,TG(K)-TO           FCPARA.904
C    *         ,T0(K)-TO,DTT,UG(K),U0(K),VG(K),V0(K),WU(K),WD(K),WE(K),          FCPARA.905
C    *         W0(K),QE(K),QU(K),QD(K),QG(K),Q0(K),AE(K)/DXSQ,AU(K)/DXSQ         FCPARA.906
C    *        ,AD(K)/DXSQ                                                        FCPARA.907
  580     CONTINUE                                                               FCPARA.908
 1050     FORMAT (1X,I2,2F7.0,6(1X,F5.1),6F5.1,2F5.2,5(1X,3PF4.1),1X,0PF         FCPARA.909
     +           4.2,1X,1PF4.2,1PF4.1)                                           FCPARA.910
        ENDIF                                                                    FCPARA.911
C             END IF                                                             FCPARA.912
C                                                                                FCPARA.913
C...FEEDBACK TO RESOLVABLE SCALE TENDENCIES.                                     FCPARA.914
C                                                                                FCPARA.915
        NCA(I,J)=NIC                                                             FCPARA.916
C         WRITE(98,1310)XTIME,I,J                                                FCPARA.917
C1310     FORMAT('***** AT XTIME = ',F6.1,', I, J = ',2I3)                       FCPARA.918
        DO 610 K=1,KX                                                            FCPARA.919
          NK=KX-K+1                                                              FCPARA.920
          DTT1=TG(K)-T0(K)                                                       FCPARA.921
C            IF(K.LE.4) DTT1=CVMGP(0.,DTT1,DTT1)                                 FCPARA.922
          IF(K.LE.4)THEN                                                         FCPARA.923
            IF(DTT1.GE.0.)DTT1=0.                                                FCPARA.924
          ENDIF                                                                  FCPARA.925
          DTT1=AMIN1(DTT1,8.0)                                                   FCPARA.926
          DTDT(I,J,NK)=DTT1/TIMEC                                                FCPARA.927
          DQDT(I,J,NK)=(QG(K)-Q0(K))/TIMEC                                       FCPARA.928
          DUDT(I,J,NK)=(UG(K)-U0(K))/TIMEC                                       FCPARA.929
          DVDT(I,J,NK)=(VG(K)-V0(K))/TIMEC                                       FCPARA.930
 1311       FORMAT('*** AT PRS = ',F6.1,' DTDT, DQDT, DUDT, DVDT =',             FCPARA.931
     *  4F8.2)                                                                   FCPARA.932
  610   CONTINUE                                                                 FCPARA.933
        RAINCV(I,J)=.1*.5*DT2*CPR/DXSQ                                           FCPARA.934
C     IF(XTIME.LT.2)                                                             FCPARA.935
C    *   WRITE(98,3559)I,J,RAINCV(I,J)*FLOAT(NIC)                                FCPARA.936
3559  FORMAT('AT I, J =',2I3,' CONVECTIVE RAINFALL =',F10.4,' CM')               FCPARA.937
  649   IF(JFLAG.EQ.1)PRINT9001,I,J,INEST                                        FCPARA.938
9001  FORMAT(1X,'CONVECTIVE SCHEME HAS BEEN SKIPPED AT I,J,INEST = ',            FCPARA.939
     *       I4,',',I4,',',I3)                                                   FCPARA.940
  650 CONTINUE                                                                   FCPARA.941
      RETURN                                                                     FCPARA.942
      END                                                                        FCPARA.943
                                                                                 FCPARA.944
