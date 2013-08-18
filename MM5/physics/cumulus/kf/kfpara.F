      SUBROUTINE KFPARA (J,INEST,IBEG,IEND,TBASE,                                KFPARA.1
     &                   U3D,V3D,PP3D,T3D,QV3D,PSB)                              KFPARA.2
      IMPLICIT NONE                                                              07NOV00.1403
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         KFPARA.3
C                                                                                KFPARA.4
C        THIS SUBROUTINE COMPUTES THE EFFECTS OF DEEP CONVECTION USING           KFPARA.5
C   THE KAIN-FRITSCH CONVECTIVE PARAMETERIZATION SCHEME.                         KFPARA.6
C                                                 MKS UNITS.                     KFPARA.7
C                                                                                KFPARA.8
C     INPUT:   TEMPERTURE (T0, K) ;    SPECIFIC HUMIDITY (Q0, KG/KG) ;           KFPARA.9
C              HORIZONTAL WIND SPEED (U0 AND V0, M/S) ;                          KFPARA.10
C              PRESSURE (P0, PASCAL) ;  VERTICAL MOTION (W0AVG, M/S).            KFPARA.11
C     OUTPUT:  CONVECTIVE TEMPERATURE (DTDT), WATER VAPOR (DQDT),                KFPARA.12
C              CLOUD WATER (DQLDT), CLOUD ICE (DQIDT),                           KFPARA.13
C              RAIN WATER (DQRDT),  SNOW (DQSDT), AND                            KFPARA.14
C              RAINFALL (RAINCV) TENDENCIES.                                     KFPARA.15
C                                                                                KFPARA.16
C                                   DOCUMENTED BY JACK KAIN                      KFPARA.17
C                                   JANUARY 1995                                 KFPARA.18
C                                   UPDATED SEPTEMBER 1995                       KFPARA.19
C                                            NOVEMBER 1996                       KFPARA.20
C                                                                                KFPARA.21
C     REFERENCES:                                                                KFPARA.22
C                                                                                KFPARA.23
C         KAIN AND FRITSCH (1993): ` CONVECTIVE PARAMETERIZATION IN              KFPARA.24
C         MESOSCALE MODELS:  THE KAIN-FRITSCH SCHEME` IN THE REPRESEN-           KFPARA.25
C         TATION OF CUMULUS CONVECTION IN NUMERICAL MODELS, A.M.S.               KFPARA.26
C         MONOGRAPH, K.A. EMANUEL AND D.J. RAYMOND, EDS., 165-170.               KFPARA.27
C                                                                                KFPARA.28
C         FRITSCH AND KAIN (1993):  `CONVECTIVE PARAMETERIZATION IN              KFPARA.29
C         MESOSCALE MODELS:  THE FRITSCH-CHAPPELL SCHEME` IN THE REP-            KFPARA.30
C         RESENTATION OF CUMULUS CONVECTION IN NUMERICAL MODELS, A.M.S.          KFPARA.31
C         MONOGRAPH, K.A. EMANUEL AND D.J. RAYMOND, EDS., 165-170.               KFPARA.32
C                                                                                KFPARA.33
C         FRITSCH AND CHAPPELL (1980), J. ATMOS. SCI., 1722-1733.                KFPARA.34
C                                                                                KFPARA.35
C NOTE:  THE KAIN-FRITSCH SCHEME IS UNDER CONTINUED DEVELOPMENT.  IF             KFPARA.36
C        YOU WOULD LIKE TO BE NOTIFIED WHEN THERE ARE UPDATES OR                 KFPARA.37
C        CHANGES TO THE SCHEME, OR IF YOU HAVE PROBLEMS, QUESTIONS, OR           KFPARA.38
C        SUGGESTIONS PLEASE NOTIFY ME AT                                         KFPARA.39
C                                                                                KFPARA.40
C                                 jkain@nssl.uoknor.edu                          KFPARA.41
C                                                                                KFPARA.42
C        JACK KAIN                                                               KFPARA.43
C        JANUARY 1995                                                            KFPARA.44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         KFPARA.45
                                                                                 KFPARA.46
#     include <parame.incl>                                                      KFPARA.47
#     include <various.incl>                                                     KFPARA.48
#     include <param2.incl>                                                      KFPARA.49
#     include <param3.incl>                                                      KFPARA.50
#     include <pmoist.incl>                                                      KFPARA.51
#     include <varia.incl>                                                       KFPARA.52
#     include <addrcu.incl>                                                      KFPARA.53
#     include <rpstar.incl>                                                      KFPARA.54
#     include <parkfcu.incl>                                                     KFPARA.55
                                                                                 KFPARA.56
      COMMON/VAPPRS/ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE,XLS0,XLS1            KFPARA.57
C                                                                                KFPARA.58
C...INPUT VARIABLES                                                              KFPARA.59
C                                                                                KFPARA.60
      REAL      U3D(MIX,MJX,MKX),  V3D(MIX,MJX,MKX),                             07NOV00.1404
     &          T3D(MIX,MJX,MKX), QV3D(MIX,MJX,MKX),                             KFPARA.62
     &         PP3D(MIX,MJX,MKX),  PSB(MIX,MJX)                                  KFPARA.63
C                                                                                KFPARA.64
C...DEFINE LOCAL VARIABLES...                                                    KFPARA.65
C                                                                                KFPARA.66
      REAL      P0(KX),Z0(KX),T0(KX),TV0(KX),Q0(KX),U0(KX),    V0(KX),           07NOV00.1405
     2  TU(KX),TVU(KX),QU(KX),TZ(KX),TVD(KX),QD(KX),QES(KX),THTES(KX),           KFPARA.68
     3  TG(KX),TVG(KX),QG(KX),WU(KX), WD(KX),W0(KX),EMS(KX), EMSD(KX),           KFPARA.69
     3  UMF(KX) ,UER(KX) ,UDR(KX) ,DMF(KX) ,DER(KX) ,DDR(KX) ,DZQ(KX),           KFPARA.70
     5  UMF2(KX),UER2(KX),UDR2(KX),DMF2(KX),DER2(KX),DDR2(KX),DZA(KX),           KFPARA.71
     6  THTA0(KX),THETEE(KX),THTAU(KX),THETEU(KX),THTAD(KX),THETED(KX),          KFPARA.72
     7  QLIQ(KX),QICE(KX),QLQOUT(KX),QICOUT(KX),PPTLIQ(KX),PPTICE(KX),           KFPARA.73
     8  DETLQ(KX),DETIC(KX),DETLQ2(KX),DETIC2(KX),RATIO(KX),RATIO2(KX)           KFPARA.74
                                                                                 KFPARA.75
      REAL      DOMGDP(KX),EXN(KX),RHOE(KX),TVQU(KX),DP(KX),RH(KX),              07NOV00.1406
     2  EQFRC(KX),WSPD(KX),QDT(KX),FXM(KX),THTAG(KX),THTESG(KX),                 KFPARA.77
     3  THPA(KX),THFXIN(KX),THFXOUT(KX),QPA(KX),QFXIN(KX),QFXOUT(KX),            KFPARA.78
     4  QLPA(KX),QLFXIN(KX),QLFXOUT(KX),QIPA(KX),QIFXIN(KX),QIFXOUT(KX),         KFPARA.79
     5  QRPA(KX),QRFXIN(KX),QRFXOUT(KX),QSPA(KX),QSFXIN(KX),QSFXOUT(KX),         KFPARA.80
     6  QL0(KX),QLG(KX),QI0(KX),QIG(KX),QR0(KX),QRG(KX),QS0(KX),QSG(KX)          KFPARA.81
                                                                                 KFPARA.82
      REAL      OMG(KXP1)                                                        07NOV00.1407
      REAL      RAINFB(KX),SNOWFB(KX)                                            07NOV00.1408
C     REAL      SCR1(MIX,MKX),PPBS(MIX,MKX+1)                                    07NOV00.1409
      REAL      PPBS(MIX,MKX+1),TBASE(MIXNH,MJXNH,MKXNH)                         07NOV00.1410
C Declarations for IMPLICIT NONE                                                 07NOV00.1411
      REAL     GDRY,FBFRC,P300,CELL,ES,ALIQ,BLIQ,DLIQ,B61,T00,DPTHMX             07NOV00.1412
      REAL     QMIX,ZMIX,PMIX,ROCPQ,P00,TMIX,EMIX,TLOG,CLIQ,THMIX                07NOV00.1413
      REAL     TDPT,TLCL,TVLCL,CPORQ,PLCL,DLP,TENV,QENV,TVEN                     07NOV00.1414
      REAL     TVBAR,ZLCL,WKLCL,WKL,WABS,WSIGNE,DTLCL,GDT,WLCL,TVAVG             07NOV00.1415
      REAL     QESE,WTW,RHOLCL,AU0,PIE,VMFLCL,UPOLD,UPNEW,ABE,TRPPT              07NOV00.1416
      REAL     THTUDL,TUDL,TTEMP,TTFRZ,FRC1,QNEWLQ,QNEWIC,RL,XLS0                07NOV00.1417
      REAL     XLS1,AICE,BICE,CICE,DICE,TBFRZ,R1,QNWFRZ,EFFQ,BE                  07NOV00.1418
      REAL     BOTERM,ENTERM,DZZ,WSQ,RATE,UDLBE,REI,EE2,UD2,TTMP,F1              07NOV00.1419
      REAL     F2,THTTMP,QTMP,TMPLIQ,TMPICE,TU95,TU10,EE1,UD1                    07NOV00.1420
      REAL     CLDHGT,DPTT,DUMFDP,EE,TSAT,THTA,P150,USR,VCONV                    07NOV00.1421
      REAL     TIMEC,TADVEC,SHSIGN,VWS,PEF,CBH,RCBH,PEFCBH,PEFF                  07NOV00.1422
      REAL     PEFF2,TDER,THTMIN,DTMLTD,RLF,QS,DPDD,FRC,DPT,RDD                  07NOV00.1423
      REAL     A1,TPDD,DSSDT,DTMP,RHBC,T1RH,QSRH,PPTFLX,CPR,CNDTNF               07NOV00.1424
      REAL     UPDINC,AINCM2,RCED,DPPTDF,DMFLFS,DMFLFS2                          07NOV00.1425
      REAL     RCED2,DDINC,AINCMX,AINCM1,AINC,TDER2,PPTFL2,FABE                  07NOV00.1426
      REAL     STAB,DEVDMF,PPR,DTT,DTT1,DTIME,TMA,TMB,TMM                        07NOV00.1427
      REAL     BCOEFF,ACOEFF,QVDIFF,TOPOMG,CPM,DQ,ABEG,DABE,DFDA                 07NOV00.1428
      REAL     FABEOLD,AINCOLD,FRC2,DR,UDFRC,UEFRC,DDFRC,DEFRC,TUC               07NOV00.1429
      REAL     TDC,QGS,RH0,RHG,QINIT,QFNL,ERR2,RELERR,RLC,RLS,RNC                07NOV00.1430
      REAL     CV,RHIC,C5                                                        07NOV00.1431
      INTEGER  IBEG,IEND,I,J,K,ML,NK,L5,L4,LLFC,KMIX,LOW,LC,MXLAYR               07NOV00.1432
      INTEGER  NLAYRS,KPBL,KLCL,LCL,LET,IFLAG,KFRZ,NK1,LTOP,NJ                   07NOV00.1433
      INTEGER  LTOP1,LTOPM1,LVF,NIC,KSTART,KMIN,LFS,ND,LDB,LDT                   07NOV00.1434
      INTEGER  ND1,NDK,NM,LMAX,NCOUNT,NOITR,ISTOP,NSTEP,NTC,INEST                07NOV00.1435
C                                                                                KFPARA.87
      DATA P00,T00/1.E5,273.16/                                                  KFPARA.88
      DATA CV,B61,RLF/717.,0.608,3.339E5/                                        KFPARA.89
      DATA RHIC,RHBC/1.,0.90/                                                    KFPARA.90
      DATA PIE,TTFRZ,TBFRZ,C5/3.141592654,268.16,248.16,1.0723E-3/               KFPARA.91
      DATA RATE/0.01/                                                            KFPARA.92
      GDRY=-G/CP                                                                 KFPARA.93
C                                                                                KFPARA.94
C***********************************************************************         KFPARA.95
C...ADDITIONAL PARAMETERS THAT WILL NEED TO BE DEFINED SOMEWHERE...              KFPARA.96
C...IN MM4, THEY ARE DEFINED IN subroutine param AND STORED IN                   KFPARA.97
C...VARIOUS COMMON BLOCKS                                                        KFPARA.98
C                                                                                KFPARA.99
C      R=287.04                                                                  KFPARA.100
C      G=9.81                                                                    KFPARA.101
C      CP=1005.7                                                                 KFPARA.102
C      DX=25000.             ! HORIZONTAL GRID LENGTH (m)                        KFPARA.103
C      DXSQ=DX*DX            ! GRID AREA (M**2)                                  KFPARA.104
C      DT2=80.               ! 2*DT (s)                                          KFPARA.105
C                                                                                KFPARA.106
C...DEFINE CONSTANTS FOR CALCULATION OF LATENT HEATING...                        KFPARA.107
C                                                                                KFPARA.108
C       XLV0=3.147E6         ! LV = XLV0 + TMP*XLV1                              KFPARA.109
C       XLV1=2369.                                                               KFPARA.110
C       XLS0=2.905E6         ! LS = XLS0 + TMP*XLS1                              KFPARA.111
C       XLS1=259.532                                                             KFPARA.112
C                                                                                KFPARA.113
C...DEFINE CONSTANTS FOR CALCULATION OF SATURATION VAPOR PRESSURE                KFPARA.114
C...ACCORDING TO BUCK (J. APPL. METEO., DECEMBER, 1981)...                       KFPARA.115
C                                                                                KFPARA.116
C       ALIQ = 613.3   !ES(LIQ)=ALIQ*EXP((BLIQ*TMP-CLIQ)/(TMP-DLIQ))             KFPARA.117
C       BLIQ = 17.502                                                            KFPARA.118
C       CLIQ = 4780.8                                                            KFPARA.119
C       DLIQ = 32.19                                                             KFPARA.120
C       AICE = 613.2   !ES(ICE)=AICE*EXP((BICE*TMP-CICE)/(TMP-DICE))             KFPARA.121
C       BICE = 22.452                                                            KFPARA.122
C       CICE = 6133.0                                                            KFPARA.123
C       DICE = 0.61                                                              KFPARA.124
C                                                                                KFPARA.125
C***********************************************************************         KFPARA.126
C                                                                                KFPARA.127
C...OPTION TO FEED CONVECTIVELY GENERATED RAINWATER                              KFPARA.128
C...INTO GRID-RESOLVED RAINWATER (OR SNOW/GRAUPEL)                               KFPARA.129
C...FIELD.  'FBFRC' IS THE FRACTION OF AVAILABLE                                 KFPARA.130
C...PRECIPITATION TO BE FED BACK (0.0 - 1.0)...                                  KFPARA.131
C                                                                                KFPARA.132
      FBFRC=0.0                                                                  KFPARA.133
C                                                                                KFPARA.134
C...SCHEME IS CALLED ONCE  ON EACH NORTH-SOUTH SLICE, THE LOOP BELOW             KFPARA.135
C...CHECKS FOR THE POSSIBILITY OF INITIATING PARAMETERIZED                       KFPARA.136
C...CONVECTION AT EACH POINT WITHIN THE SLICE                                    KFPARA.137
C                                                                                KFPARA.138
C...INPUT DATA ARE IN A FORM OF NORTH-SOUTH SLICE.                               KFPARA.139
C                                                                                KFPARA.140
      DO 325 I=IBEG,IEND                                                         KFPARA.141
C                                                                                KFPARA.142
C...SEE IF IT IS NECESSARY TO CHECK FOR CONVECTIVE TRIGGERING AT THIS            KFPARA.143
C...GRID POINT. IF NCA>0, CONVECTION IS ALREADY ACTIVE AT THIS POINT,            KFPARA.144
C...JUST FEED BACK THE TENDENCIES SAVED FROM THE TIME WHEN CONVECTION            KFPARA.145
C...WAS INITIATED.  IF NCA<0, CONVECTION IS NOT ACTIVE                           KFPARA.146
C...AND YOU MAY WANT TO CHECK TO SEE IF IT CAN BE ACTIVATED FOR THE              KFPARA.147
C...CURRENT CONDITIONS.  IN PREVIOUS APLICATIONS OF THIS SCHEME,                 KFPARA.148
C...THE VARIABLE ICLDCK WAS USED BELOW TO SAVE TIME BY ONLY CHECKING             KFPARA.149
C...FOR THE POSSIBILITY OF CONVECTIVE INITIATION EVERY 5 OR 10                   KFPARA.150
C...MINUTES...                                                                   KFPARA.151
C                                                                                KFPARA.152
        IF(NCA(I,J).GT.0)GOTO 325                                                KFPARA.153
C        IF (ICLDCK.NE.0) GO TO 325                                              KFPARA.154
   10   CONTINUE                                                                 KFPARA.155
        P300=1000.*(PSB(I,J)*A(KL)+PTOP-30.)+PP3D(I,J,KL)                        KFPARA.156
C                                                                                KFPARA.157
C...PRESSURE PERTURBATION TERM IS ONLY DEFINED AT MID-POINT OF                   KFPARA.158
C...VERTICAL LAYERS...SINCE TOTAL PRESSURE IS NEEDED AT THE TOP AND              KFPARA.159
C...BOTTOM OF LAYERS BELOW, DO AN INTERPOLATION...                               KFPARA.160
C...IN V3, PPBS IS UNCOUPLED PP                                                  KFPARA.161
C                                                                                KFPARA.162
        DO 5 K=2,KX                                                              KFPARA.163
          PPBS(I,K)=(PP3D(I,J,K-1)*DSIGMA(K)+PP3D(I,J,K)*DSIGMA(K-1))/(          KFPARA.164
     +              DSIGMA(K-1)+DSIGMA(K))                                       KFPARA.165
    5   CONTINUE                                                                 KFPARA.166
        PPBS(I,1)=PP3D(I,J,1)+((PP3D(I,J,1)-PP3D(I,J,2))/(A(1)-A(2))*(           KFPARA.167
     +            SIGMA(1)-A(1)))                                                KFPARA.168
        PPBS(I,KX+1)=PP3D(I,J,KX)+((PP3D(I,J,KX-1)-PP3D(I,J,KX))/(               KFPARA.169
     +               A(KX-1)-A(KX))*(SIGMA(KX+1)-A(KX)))                         KFPARA.170
C                                                                                KFPARA.171
C...INPUT A VERTICAL SOUNDING ... NOTE THAT MODEL LAYERS ARE NUMBERED            KFPARA.172
C...FROM BOTTOM-UP IN THE KF SCHEME...                                           KFPARA.173
C                                                                                KFPARA.174
        ML=0                                                                     KFPARA.175
        CELL=PTOP*RPSB(I,J)                                                      KFPARA.176
        DO 15 K=1,KX                                                             KFPARA.177
          NK=KX-K+1                                                              KFPARA.178
          P0(K)=1.E3*(A(NK)*PSB(I,J)+PTOP)+PP3D(I,J,NK)                          KFPARA.179
          T0(K)=T3D(I,J,NK)                                                      KFPARA.180
          Q0(K)=QV3D(I,J,NK)                                                     KFPARA.181
C                                                                                KFPARA.182
C...IF Q0 IS ABOVE SATURATION VALUE, REDUCE IT TO SATURATION LEVEL...            KFPARA.183
C                                                                                KFPARA.184
          ES=ALIQ*EXP((BLIQ*T0(K)-CLIQ)/(T0(K)-DLIQ))                            KFPARA.185
          QES(K)=0.622*ES/(P0(K)-ES)                                             KFPARA.186
          Q0(K)=AMIN1(QES(K),Q0(K))                                              KFPARA.187
          QL0(K)=0.                                                              KFPARA.188
          QI0(K)=0.                                                              KFPARA.189
          QR0(K)=0.                                                              KFPARA.190
          QS0(K)=0.                                                              KFPARA.191
C                                                                                KFPARA.192
          U0(K)=.25*(U3D(I,J,NK)+U3D(I+1,J,NK)+U3D(I,J+1,NK)+                    KFPARA.193
     +          U3D(I+1,J+1,NK))                                                 KFPARA.194
          V0(K)=.25*(V3D(I,J,NK)+V3D(I+1,J,NK)+V3D(I,J+1,NK)+                    KFPARA.195
     +          V3D(I+1,J+1,NK))                                                 KFPARA.196
          TV0(K)=T0(K)*(1.+B61*Q0(K))                                            KFPARA.197
          RHOE(K)=P0(K)/(R*TV0(K))                                               KFPARA.198
C           W0(K) = -101.9368*SCR1(I,NK)/RHOE(K)                                 KFPARA.199
          DP(K)=(1.E3*(SIGMA(NK+1)*PSB(I,J)+PTOP)+PPBS(I,NK+1))-(1.E3*(          KFPARA.200
     +          SIGMA(NK)*PSB(I,J)+PTOP)+PPBS(I,NK))                             KFPARA.201
          DZQ(K)=ROVG*TBASE(I,J,NK)*                                             KFPARA.202
     +           ALOG((SIGMA(NK+1)+CELL)/(SIGMA(NK)+CELL))                       KFPARA.203
C                                                                                KFPARA.204
C...DZQ IS DZ BETWEEN SIGMA SURFACES, DZA IS DZ BETWEEN MODEL HALF LEVEL         KFPARA.205
C   DP IS THE PRESSURE INTERVAL BETWEEN FULL SIGMA LEVELS...                     KFPARA.206
C                                                                                KFPARA.207
          IF(P0(K).GE.500E2)L5=K                                                 KFPARA.208
          IF(P0(K).GE.400E2)L4=K                                                 KFPARA.209
          IF(P0(K).GE.P300)LLFC=K                                                KFPARA.210
          IF(T0(K).GT.T00)ML=K                                                   KFPARA.211
   15   CONTINUE                                                                 KFPARA.212
        Z0(1)=.5*DZQ(1)                                                          KFPARA.213
        DO 20 K=2,KL                                                             KFPARA.214
          Z0(K)=Z0(K-1)+.5*(DZQ(K)+DZQ(K-1))                                     KFPARA.215
          DZA(K-1)=Z0(K)-Z0(K-1)                                                 KFPARA.216
   20   CONTINUE                                                                 KFPARA.217
        DZA(KL)=0.                                                               KFPARA.218
        KMIX=1                                                                   KFPARA.219
   25   LOW=KMIX                                                                 KFPARA.220
        IF(LOW.GT.LLFC)GOTO 325                                                  KFPARA.221
        LC=LOW                                                                   KFPARA.222
        MXLAYR=0                                                                 KFPARA.223
C                                                                                KFPARA.224
C...ASSUME THAT IN ORDER TO SUPPORT A DEEP UPDRAFT YOU NEED A LAYER OF           KFPARA.225
C...UNSTABLE AIR 50 TO 100 mb DEEP...TO APPROXIMATE THIS, ISOLATE A              KFPARA.226
C...GROUP OF ADJACENT INDIVIDUAL MODEL LAYERS, WITH THE BASE AT LEVEL            KFPARA.227
C...LC, SUCH THAT THE COMBINED DEPTH OF THESE LAYERS IS AT LEAST 60 mb..         KFPARA.228
C                                                                                KFPARA.229
        NLAYRS=0                                                                 KFPARA.230
        DPTHMX=0.                                                                KFPARA.231
        DO 63 NK=LC,KX                                                           KFPARA.232
          DPTHMX=DPTHMX+DP(NK)                                                   KFPARA.233
          NLAYRS=NLAYRS+1                                                        KFPARA.234
   63   IF(DPTHMX.GT.6.E3)GOTO 64                                                KFPARA.235
        GOTO 325                                                                 KFPARA.236
   64   KPBL=LC+NLAYRS-1                                                         KFPARA.237
        KMIX=LC+1                                                                KFPARA.238
   18   THMIX=0.                                                                 KFPARA.239
        QMIX=0.                                                                  KFPARA.240
        ZMIX=0.                                                                  KFPARA.241
        PMIX=0.                                                                  KFPARA.242
        DPTHMX=0.                                                                KFPARA.243
C                                                                                KFPARA.244
C...FIND THE THERMODYNAMIC CHARACTERISTICS OF THE LAYER BY                       KFPARA.245
C...MASS-WEIGHTING THE CHARACTERISTICS OF THE INDIVIDUAL MODEL                   KFPARA.246
C...LAYERS...                                                                    KFPARA.247
C                                                                                KFPARA.248
        DO 17 NK=LC,KPBL                                                         KFPARA.249
          DPTHMX=DPTHMX+DP(NK)                                                   KFPARA.250
          ROCPQ=0.2854*(1.-0.28*Q0(NK))                                          KFPARA.251
          THMIX=THMIX+DP(NK)*T0(NK)*(P00/P0(NK))**ROCPQ                          KFPARA.252
          QMIX=QMIX+DP(NK)*Q0(NK)                                                KFPARA.253
          ZMIX=ZMIX+DP(NK)*Z0(NK)                                                KFPARA.254
   17   PMIX=PMIX+DP(NK)*P0(NK)                                                  KFPARA.255
        THMIX=THMIX/DPTHMX                                                       KFPARA.256
        QMIX=QMIX/DPTHMX                                                         KFPARA.257
        ZMIX=ZMIX/DPTHMX                                                         KFPARA.258
        PMIX=PMIX/DPTHMX                                                         KFPARA.259
        ROCPQ=0.2854*(1.-0.28*QMIX)                                              KFPARA.260
        TMIX=THMIX*(PMIX/P00)**ROCPQ                                             KFPARA.261
        EMIX=QMIX*PMIX/(0.622+QMIX)                                              KFPARA.262
C                                                                                KFPARA.263
C...FIND THE TEMPERATURE OF THE MIXTURE AT ITS LCL, PRESSURE                     KFPARA.264
C...LEVEL OF LCL...                                                              KFPARA.265
C                                                                                KFPARA.266
        TLOG=ALOG(EMIX/ALIQ)                                                     KFPARA.267
        TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)                                        KFPARA.268
        TLCL=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(TMIX-T00))*(TMIX-           KFPARA.269
     +       TDPT)                                                               KFPARA.270
        TLCL=AMIN1(TLCL,TMIX)                                                    KFPARA.271
        TVLCL=TLCL*(1.+0.608*QMIX)                                               KFPARA.272
        CPORQ=1./ROCPQ                                                           KFPARA.273
        PLCL=P00*(TLCL/THMIX)**CPORQ                                             KFPARA.274
        DO 29 NK=LC,KL                                                           KFPARA.275
          KLCL=NK                                                                KFPARA.276
          IF(PLCL.GE.P0(NK))GOTO 35                                              KFPARA.277
   29   CONTINUE                                                                 KFPARA.278
        GOTO 325                                                                 KFPARA.279
   35   K=KLCL-1                                                                 KFPARA.280
        DLP=ALOG(PLCL/P0(K))/ALOG(P0(KLCL)/P0(K))                                KFPARA.281
C                                                                                KFPARA.282
C...ESTIMATE ENVIRONMENTAL TEMPERATURE AND MIXING RATIO AT THE LCL...            KFPARA.283
C                                                                                KFPARA.284
        TENV=T0(K)+(T0(KLCL)-T0(K))*DLP                                          KFPARA.285
        QENV=Q0(K)+(Q0(KLCL)-Q0(K))*DLP                                          KFPARA.286
        TVEN=TENV*(1.+0.608*QENV)                                                KFPARA.287
        TVBAR=0.5*(TV0(K)+TVEN)                                                  KFPARA.288
C        ZLCL=Z0(K)+R*TVBAR*ALOG(P0(K)/PLCL)/G                                   KFPARA.289
        ZLCL=Z0(K)+(Z0(KLCL)-Z0(K))*DLP                                          KFPARA.290
C                                                                                KFPARA.291
C...CHECK TO SEE IF CLOUD IS BUOYANT USING FRITSCH-CHAPPELL TRIGGER              KFPARA.292
C...FUNCTION DESCRIBED IN KAIN AND FRITSCH (1992)...W0AVG IS AN                  KFPARA.293
C...APROXIMATE VALUE FOR THE RUNNING-MEAN GRID-SCALE VERTICAL                    KFPARA.294
C...VELOCITY, WHICH GIVES SMOOTHER FIELDS OF CONVECTIVE INITIATION               KFPARA.295
C...THAN THE INSTANTANEOUS VALUE...FORMULA RELATING TEMPERATURE                  KFPARA.296
C...PERTURBATION TO VERTICAL VELOCITY HAS BEEN USED WITH THE MOST                KFPARA.297
C...SUCCESS AT GRID LENGTHS NEAR 25 km.  FOR DIFFERENT GRID-LENGTHS,             KFPARA.298
C...ADJUST VERTICAL VELOCITY TO EQUIVALENT VALUE FOR 25 KM GRID                  KFPARA.299
C...LENGTH, ASSUMING LINEAR DEPENDENCE OF W ON GRID LENGTH...                    KFPARA.300
C                                                                                KFPARA.301
        WKLCL=0.02*ZLCL/2.5E3                                                    KFPARA.302
        WKL=(W0AVG(I,J,K)+(W0AVG(I,J,KLCL)-W0AVG(I,J,K))*DLP)*DX/25.E3-          KFPARA.303
     +      WKLCL                                                                KFPARA.304
        WABS=ABS(WKL)+1.E-10                                                     KFPARA.305
        WSIGNE=WKL/WABS                                                          KFPARA.306
        DTLCL=4.64*WSIGNE*WABS**0.33                                             KFPARA.307
        GDT=G*DTLCL*(ZLCL-Z0(LC))/(TV0(LC)+TVEN)                                 KFPARA.308
        WLCL=1.+.5*WSIGNE*SQRT(ABS(GDT)+1.E-10)                                  KFPARA.309
        IF(TLCL+DTLCL.GT.TENV)GOTO 45                                            KFPARA.310
        IF(KPBL.GE.LLFC)GOTO 325                                                 KFPARA.311
        GOTO 25                                                                  KFPARA.312
C                                                                                KFPARA.313
C...CONVECTIVE TRIGGERING CRITERIA HAS BEEN SATISFIED...COMPUTE                  KFPARA.314
C...EQUIVALENT POTENTIAL TEMPERATURE                                             KFPARA.315
C...(THETEU) AND VERTICAL VELOCITY OF THE RISING PARCEL AT THE LCL...            KFPARA.316
C                                                                                KFPARA.317
   45   THETEU(K)=TMIX*(1.E5/PMIX)**(0.2854*(1.-0.28*QMIX))*                     KFPARA.318
     +            EXP((3374.6525/TLCL-2.5403)*QMIX*(1.+0.81*QMIX))               KFPARA.319
        ES=ALIQ*EXP((TENV*BLIQ-CLIQ)/(TENV-DLIQ))                                KFPARA.320
        TVAVG=0.5*(TV0(KLCL)+TENV*(1.+0.608*QENV))                               KFPARA.321
        PLCL=P0(KLCL)*EXP(G/(R*TVAVG)*(Z0(KLCL)-ZLCL))                           KFPARA.322
        QESE=0.622*ES/(PLCL-ES)                                                  KFPARA.323
        GDT=G*DTLCL*(ZLCL-Z0(LC))/(TV0(LC)+TVEN)                                 KFPARA.324
        WLCL=1.+.5*WSIGNE*SQRT(ABS(GDT)+1.E-10)                                  KFPARA.325
        THTES(K)=TENV*(1.E5/PLCL)**(0.2854*(1.-0.28*QESE))*                      KFPARA.326
     +           EXP((3374.6525/TENV-2.5403)*QESE*(1.+0.81*QESE))                KFPARA.327
        WTW=WLCL*WLCL                                                            KFPARA.328
        IF(WLCL.LT.0.)GOTO 25                                                    KFPARA.329
        TVLCL=TLCL*(1.+0.608*QMIX)                                               KFPARA.330
        RHOLCL=PLCL/(R*TVLCL)                                                    KFPARA.331
C                                                                                KFPARA.332
        LCL=KLCL                                                                 KFPARA.333
        LET=LCL                                                                  KFPARA.334
C                                                                                KFPARA.335
C*******************************************************************             KFPARA.336
C                                                                  *             KFPARA.337
C                 COMPUTE UPDRAFT PROPERTIES                       *             KFPARA.338
C                                                                  *             KFPARA.339
C*******************************************************************             KFPARA.340
C                                                                                KFPARA.341
C                                                                                KFPARA.342
C...ESTIMATE INITIAL UPDRAFT MASS FLUX (UMF(K))...                               KFPARA.343
C                                                                                KFPARA.344
        WU(K)=WLCL                                                               KFPARA.345
        AU0=PIE*RAD*RAD                                                          KFPARA.346
        UMF(K)=RHOLCL*AU0                                                        KFPARA.347
        VMFLCL=UMF(K)                                                            KFPARA.348
        UPOLD=VMFLCL                                                             KFPARA.349
        UPNEW=UPOLD                                                              KFPARA.350
C                                                                                KFPARA.351
C...RATIO2 IS THE DEGREE OF GLACIATION IN THE CLOUD (0 TO 1),                    KFPARA.352
C...UER IS THE ENVIR ENTRAINMENT RATE, ABE IS AVAILABLE BUOYANT ENERGY,          KFPARA.353
C   TRPPT IS THE TOTAL RATE OF PRECIPITATION PRODUCTION...                       KFPARA.354
C                                                                                KFPARA.355
        RATIO2(K)=0.                                                             KFPARA.356
        UER(K)=0.                                                                KFPARA.357
        ABE=0.                                                                   KFPARA.358
        TRPPT=0.                                                                 KFPARA.359
        TU(K)=TLCL                                                               KFPARA.360
        TVU(K)=TVLCL                                                             KFPARA.361
        QU(K)=QMIX                                                               KFPARA.362
        EQFRC(K)=1.                                                              KFPARA.363
        QLIQ(K)=0.                                                               KFPARA.364
        QICE(K)=0.                                                               KFPARA.365
        QLQOUT(K)=0.                                                             KFPARA.366
        QICOUT(K)=0.                                                             KFPARA.367
        DETLQ(K)=0.                                                              KFPARA.368
        DETIC(K)=0.                                                              KFPARA.369
        PPTLIQ(K)=0.                                                             KFPARA.370
        PPTICE(K)=0.                                                             KFPARA.371
        IFLAG=0                                                                  KFPARA.372
        KFRZ=LC                                                                  KFPARA.373
C                                                                                KFPARA.374
C...THE AMOUNT OF CONV AVAIL POT ENERGY (CAPE) IS CALCULATED WITH                KFPARA.375
C   RESPECT TO UNDILUTE PARCEL ASCENT; EQ POT TEMP OF UNDILUTE                   KFPARA.376
C   PARCEL IS THTUDL, UNDILUTE TEMPERATURE IS GIVEN BY TUDL...                   KFPARA.377
C                                                                                KFPARA.378
        THTUDL=THETEU(K)                                                         KFPARA.379
        TUDL=TLCL                                                                KFPARA.380
C                                                                                KFPARA.381
C...TTEMP IS USED DURING CALCULATION OF THE LINEAR GLACIATION                    KFPARA.382
C   PROCESS; IT IS INITIALLY SET TO THE TEMPERATURE AT WHICH                     KFPARA.383
C   FREEZING IS SPECIFIED TO BEGIN.  WITHIN THE GLACIATION                       KFPARA.384
C   INTERVAL, IT IS SET EQUAL TO THE UPDRAFT TEMP AT THE                         KFPARA.385
C   PREVIOUS MODEL LEVEL...                                                      KFPARA.386
C                                                                                KFPARA.387
        TTEMP=TTFRZ                                                              KFPARA.388
C                                                                                KFPARA.389
C...ENTER THE LOOP FOR UPDRAFT CALCULATIONS...CALCULATE UPDRAFT TEMP,            KFPARA.390
C   MIXING RATIO, VERTICAL MASS FLUX, LATERAL DETRAINMENT OF MASS AND            KFPARA.391
C   MOISTURE, PRECIPITATION RATES AT EACH MODEL LEVEL...                         KFPARA.392
C                                                                                KFPARA.393
        DO 60 NK=K,KLM                                                           KFPARA.394
          NK1=NK+1                                                               KFPARA.395
          RATIO2(NK1)=RATIO2(NK)                                                 KFPARA.396
C                                                                                KFPARA.397
C...UPDATE UPDRAFT PROPERTIES AT THE NEXT MODEL LVL TO REFLECT                   KFPARA.398
C   ENTRAINMENT OF ENVIRONMENTAL AIR...                                          KFPARA.399
C                                                                                KFPARA.400
          FRC1=0.                                                                KFPARA.401
          TU(NK1)=T0(NK1)                                                        KFPARA.402
          THETEU(NK1)=THETEU(NK)                                                 KFPARA.403
          QU(NK1)=QU(NK)                                                         KFPARA.404
          QLIQ(NK1)=QLIQ(NK)                                                     KFPARA.405
          QICE(NK1)=QICE(NK)                                                     KFPARA.406
          CALL TPMIX(P0(NK1),THETEU(NK1),TU(NK1),QU(NK1),QLIQ(NK1),              KFPARA.407
     +         QICE(NK1),QNEWLQ,QNEWIC,RATIO2(NK1),RL,XLV0,XLV1,XLS0,            KFPARA.408
     +         XLS1,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)                     KFPARA.409
          TVU(NK1)=TU(NK1)*(1.+0.608*QU(NK1))                                    KFPARA.410
C                                                                                KFPARA.411
C...CHECK TO SEE IF UPDRAFT TEMP IS WITHIN THE FREEZING INTERVAL,                KFPARA.412
C   IF IT IS, CALCULATE THE FRACTIONAL CONVERSION TO GLACIATION                  KFPARA.413
C   AND ADJUST QNEWLQ TO REFLECT THE GRADUAL CHANGE IN THETAU                    KFPARA.414
C   SINCE THE LAST MODEL LEVEL...THE GLACIATION EFFECTS WILL BE                  KFPARA.415
C   DETERMINED AFTER THE AMOUNT OF CONDENSATE AVAILABLE AFTER                    KFPARA.416
C   PRECIP FALLOUT IS DETERMINED...TTFRZ IS THE TEMP AT WHICH                    KFPARA.417
C   GLACIATION BEGINS, TBFRZ THE TEMP AT WHICH IT ENDS...                        KFPARA.418
C                                                                                KFPARA.419
          IF(TU(NK1).LE.TTFRZ.AND.IFLAG.LT.1)THEN                                KFPARA.420
            IF(TU(NK1).GT.TBFRZ)THEN                                             KFPARA.421
              IF(TTEMP.GT.TTFRZ)TTEMP=TTFRZ                                      KFPARA.422
              FRC1=(TTEMP-TU(NK1))/(TTFRZ-TBFRZ)                                 KFPARA.423
              R1=(TTEMP-TU(NK1))/(TTEMP-TBFRZ)                                   KFPARA.424
            ELSE                                                                 KFPARA.425
              FRC1=(TTEMP-TBFRZ)/(TTFRZ-TBFRZ)                                   KFPARA.426
              R1=1.                                                              KFPARA.427
              IFLAG=1                                                            KFPARA.428
            ENDIF                                                                KFPARA.429
            QNWFRZ=QNEWLQ                                                        KFPARA.430
            QNEWIC=QNEWIC+QNEWLQ*R1*0.5                                          KFPARA.431
            QNEWLQ=QNEWLQ-QNEWLQ*R1*0.5                                          KFPARA.432
            EFFQ=(TTFRZ-TBFRZ)/(TTEMP-TBFRZ)                                     KFPARA.433
            TTEMP=TU(NK1)                                                        KFPARA.434
          ENDIF                                                                  KFPARA.435
C                                                                                KFPARA.436
C  CALCULATE UPDRAFT VERTICAL VELOCITY AND PRECIPITATION FALLOUT...              KFPARA.437
C                                                                                KFPARA.438
          IF(NK.EQ.K)THEN                                                        KFPARA.439
            BE=(TVLCL+TVU(NK1))/(TVEN+TV0(NK1))-1.                               KFPARA.440
            BOTERM=2.*(Z0(NK1)-ZLCL)*G*BE/1.5                                    KFPARA.441
            ENTERM=0.                                                            KFPARA.442
            DZZ=Z0(NK1)-ZLCL                                                     KFPARA.443
          ELSE                                                                   KFPARA.444
            BE=(TVU(NK)+TVU(NK1))/(TV0(NK)+TV0(NK1))-1.                          KFPARA.445
            BOTERM=2.*DZA(NK)*G*BE/1.5                                           KFPARA.446
            ENTERM=2.*UER(NK)*WTW/UPOLD                                          KFPARA.447
            DZZ=DZA(NK)                                                          KFPARA.448
          ENDIF                                                                  KFPARA.449
          WSQ=WTW                                                                KFPARA.450
          CALL CONDLOAD(QLIQ(NK1),QICE(NK1),WTW,DZZ,BOTERM,ENTERM,RATE,          KFPARA.451
     +         QNEWLQ,QNEWIC,QLQOUT(NK1),QICOUT(NK1))                            KFPARA.452
C                                                                                KFPARA.455
C...IF VERT VELOCITY IS LESS THAN ZERO, EXIT THE UPDRAFT LOOP AND,               KFPARA.456
C   IF CLOUD IS TALL ENOUGH, FINALIZE UPDRAFT CALCULATIONS...                    KFPARA.457
C                                                                                KFPARA.458
          IF(WTW.LE.0.)GOTO 65                                                   24SEP99.415
          WABS=SQRT(ABS(WTW))                                                    24SEP99.416
          WU(NK1)=WTW/WABS                                                       24SEP99.417
C                                                                                KFPARA.460
C  UPDATE THE ABE FOR UNDILUTE ASCENT...                                         KFPARA.461
C                                                                                KFPARA.462
          THTES(NK1)=T0(NK1)*(1.E5/P0(NK1))**(0.2854*(1.-0.28*QES(NK1)))         KFPARA.463
     +               *                                                           KFPARA.464
     +               EXP((3374.6525/T0(NK1)-2.5403)*QES(NK1)*(1.+0.81*           KFPARA.465
     +               QES(NK1)))                                                  KFPARA.466
          UDLBE=((2.*THTUDL)/(THTES(NK)+THTES(NK1))-1.)*DZZ                      KFPARA.467
          IF(UDLBE.GT.0.)ABE=ABE+UDLBE*G                                         KFPARA.468
C                                                                                KFPARA.469
C  DETERMINE THE EFFECTS OF CLOUD GLACIATION IF WITHIN THE SPECIFIED             KFPARA.470
C  TEMP INTERVAL...                                                              KFPARA.471
C                                                                                KFPARA.472
          IF(FRC1.GT.1.E-6)THEN                                                  KFPARA.473
            CALL DTFRZNEW(TU(NK1),P0(NK1),THETEU(NK1),QU(NK1),QLIQ(NK1),         KFPARA.474
     +           QICE(NK1),RATIO2(NK1),TTFRZ,TBFRZ,QNWFRZ,RL,FRC1,EFFQ,          KFPARA.475
     +           IFLAG,XLV0,XLV1,XLS0,XLS1,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE         KFPARA.476
     +           ,CICE,DICE)                                                     KFPARA.477
          ENDIF                                                                  KFPARA.478
C                                                                                KFPARA.479
C  CALL SUBROUTINE TO CALCULATE ENVIRONMENTAL EQUIVALENT POTENTIAL TEMP.         KFPARA.480
C  WITHIN GLACIATION INTERVAL, THETAE MUST BE CALCULATED WITH RESPECT TO         KFPARA.481
C  SAME DEGREE OF GLACIATION FOR ALL ENTRAINING AIR...                           KFPARA.482
C                                                                                KFPARA.483
          CALL ENVIRTHT(P0(NK1),T0(NK1),Q0(NK1),THETEE(NK1),RATIO2(NK1),         KFPARA.484
     +         RL,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)                       KFPARA.485
C                                                                                KFPARA.486
C...REI IS THE RATE OF ENVIRONMENTAL INFLOW...                                   KFPARA.487
C                                                                                KFPARA.488
          REI=VMFLCL*DP(NK1)*0.03/RAD                                            KFPARA.489
          TVQU(NK1)=TU(NK1)*(1.+0.608*QU(NK1)-QLIQ(NK1)-QICE(NK1))               KFPARA.490
C                                                                                KFPARA.491
C...IF CLOUD PARCELS ARE VIRTUALLY COLDER THAN THE ENVIRONMENT, NO               KFPARA.492
C   ENTRAINMENT IS ALLOWED AT THIS LEVEL...                                      KFPARA.493
C                                                                                KFPARA.494
          IF(TVQU(NK1).LE.TV0(NK1))THEN                                          KFPARA.495
            UER(NK1)=0.0                                                         KFPARA.496
            UDR(NK1)=REI                                                         KFPARA.497
            EE2=0.                                                               KFPARA.498
            UD2=1.                                                               KFPARA.499
            EQFRC(NK1)=0.                                                        KFPARA.500
            GOTO 55                                                              KFPARA.501
          ENDIF                                                                  KFPARA.502
          LET=NK1                                                                KFPARA.503
          TTMP=TVQU(NK1)                                                         KFPARA.504
C                                                                                KFPARA.505
C...DETERMINE THE CRITICAL MIXED FRACTION OF UPDRAFT AND ENVIRONMENTAL           KFPARA.506
C   AIR FOR ESTIMATION OF ENTRAINMENT AND DETRAINMENT RATES...                   KFPARA.507
C                                                                                KFPARA.508
          F1=0.95                                                                KFPARA.509
          F2=1.-F1                                                               KFPARA.510
          THTTMP=F1*THETEE(NK1)+F2*THETEU(NK1)                                   KFPARA.511
          QTMP=F1*Q0(NK1)+F2*QU(NK1)                                             KFPARA.512
          TMPLIQ=F2*QLIQ(NK1)                                                    KFPARA.513
          TMPICE=F2*QICE(NK1)                                                    KFPARA.514
          CALL TPMIX(P0(NK1),THTTMP,TTMP,QTMP,TMPLIQ,TMPICE,QNEWLQ,              KFPARA.515
     +         QNEWIC,RATIO2(NK1),RL,XLV0,XLV1,XLS0,XLS1,ALIQ,BLIQ,CLIQ,         KFPARA.516
     +         DLIQ,AICE,BICE,CICE,DICE)                                         KFPARA.517
          TU95=TTMP*(1.+0.608*QTMP-TMPLIQ-TMPICE)                                KFPARA.518
          IF(TU95.GT.TV0(NK1))THEN                                               KFPARA.519
            EE2=1.                                                               KFPARA.520
            UD2=0.                                                               KFPARA.521
            EQFRC(NK1)=1.0                                                       KFPARA.522
            GOTO 50                                                              KFPARA.523
          ENDIF                                                                  KFPARA.524
          F1=0.10                                                                KFPARA.525
          F2=1.-F1                                                               KFPARA.526
          THTTMP=F1*THETEE(NK1)+F2*THETEU(NK1)                                   KFPARA.527
          QTMP=F1*Q0(NK1)+F2*QU(NK1)                                             KFPARA.528
          TMPLIQ=F2*QLIQ(NK1)                                                    KFPARA.529
          TMPICE=F2*QICE(NK1)                                                    KFPARA.530
          CALL TPMIX(P0(NK1),THTTMP,TTMP,QTMP,TMPLIQ,TMPICE,QNEWLQ,              KFPARA.531
     +         QNEWIC,RATIO2(NK1),RL,XLV0,XLV1,XLS0,XLS1,ALIQ,BLIQ,CLIQ,         KFPARA.532
     +         DLIQ,AICE,BICE,CICE,DICE)                                         KFPARA.533
          TU10=TTMP*(1.+0.608*QTMP-TMPLIQ-TMPICE)                                KFPARA.534
          IF(TU10.EQ.TVQU(NK1))THEN                                              KFPARA.535
            EE2=1.                                                               KFPARA.536
            UD2=0.                                                               KFPARA.537
            EQFRC(NK1)=1.0                                                       KFPARA.538
            GOTO 50                                                              KFPARA.539
          ENDIF                                                                  KFPARA.540
          EQFRC(NK1)=(TV0(NK1)-TVQU(NK1))*F1/(TU10-TVQU(NK1))                    KFPARA.541
          EQFRC(NK1)=AMAX1(0.0,EQFRC(NK1))                                       KFPARA.542
          EQFRC(NK1)=AMIN1(1.0,EQFRC(NK1))                                       KFPARA.543
          IF(EQFRC(NK1).EQ.1)THEN                                                KFPARA.544
            EE2=1.                                                               KFPARA.545
            UD2=0.                                                               KFPARA.546
            GOTO 50                                                              KFPARA.547
          ELSEIF(EQFRC(NK1).EQ.0.)THEN                                           KFPARA.548
            EE2=0.                                                               KFPARA.549
            UD2=1.                                                               KFPARA.550
            GOTO 50                                                              KFPARA.551
          ELSE                                                                   KFPARA.552
C                                                                                KFPARA.553
C...SUBROUTINE PROF5 INTEGRATES OVER THE GAUSSIAN DIST TO DETERMINE THE          KFPARA.554
C   FRACTIONAL ENTRAINMENT AND DETRAINMENT RATES...                              KFPARA.555
C                                                                                KFPARA.556
            CALL PROF5(EQFRC(NK1),EE2,UD2)                                       KFPARA.557
          ENDIF                                                                  KFPARA.558
C                                                                                KFPARA.559
   50     IF(NK.EQ.K)THEN                                                        KFPARA.560
            EE1=1.                                                               KFPARA.561
            UD1=0.                                                               KFPARA.562
          ENDIF                                                                  KFPARA.563
C                                                                                KFPARA.564
C...NET ENTRAINMENT AND DETRAINMENT RATES ARE GIVEN BY THE AVERAGE               KFPARA.565
C   FRACTIONAL VALUES IN THE LAYER...                                            KFPARA.566
C                                                                                KFPARA.567
          UER(NK1)=0.5*REI*(EE1+EE2)                                             KFPARA.568
          UDR(NK1)=0.5*REI*(UD1+UD2)                                             KFPARA.569
C                                                                                KFPARA.570
C...IF THE CALCULATED UPDRAFT DETRAINMENT RATE IS GREATER THAN THE TOTAL         KFPARA.571
C   UPDRAFT MASS FLUX, ALL CLOUD MASS DETRAINS, EXIT UPDRAFT CALCULATION         KFPARA.572
C                                                                                KFPARA.573
   55     IF(UMF(NK)-UDR(NK1).LT.10.)THEN                                        KFPARA.574
C                                                                                KFPARA.575
C...IF THE CALCULATED DETRAINED MASS FLUX IS GREATER THAN THE TOTAL              KFPARA.576
C   UPDRAFT FLUX, IMPOSE TOTAL DETRAINMENT OF UPDRAFT MASS AT THE                KFPARA.577
C   PREVIOUS MODEL                                                               KFPARA.578
C                                                                                KFPARA.579
            IF(UDLBE.GT.0.)ABE=ABE-UDLBE*G                                       KFPARA.580
            LET=NK                                                               KFPARA.581
C         WRITE(98,1015)P0(NK1)/100.                                             KFPARA.582
            GOTO 65                                                              KFPARA.583
          ENDIF                                                                  KFPARA.584
          EE1=EE2                                                                KFPARA.585
          UD1=UD2                                                                KFPARA.586
          UPOLD=UMF(NK)-UDR(NK1)                                                 KFPARA.587
          UPNEW=UPOLD+UER(NK1)                                                   KFPARA.588
          UMF(NK1)=UPNEW                                                         KFPARA.589
C                                                                                KFPARA.590
C...DETLQ AND DETIC ARE THE RATES OF DETRAINMENT OF LIQUID AND ICE IN            KFPARA.591
C   THE DETRAINING UPDRAFT MASS...                                               KFPARA.592
C                                                                                KFPARA.593
          DETLQ(NK1)=QLIQ(NK1)*UDR(NK1)                                          KFPARA.594
          DETIC(NK1)=QICE(NK1)*UDR(NK1)                                          KFPARA.595
          QDT(NK1)=QU(NK1)                                                       KFPARA.596
          QU(NK1)=(UPOLD*QU(NK1)+UER(NK1)*Q0(NK1))/UPNEW                         KFPARA.597
          THETEU(NK1)=(THETEU(NK1)*UPOLD+THETEE(NK1)*UER(NK1))/UPNEW             KFPARA.598
          QLIQ(NK1)=QLIQ(NK1)*UPOLD/UPNEW                                        KFPARA.599
          QICE(NK1)=QICE(NK1)*UPOLD/UPNEW                                        KFPARA.600
C                                                                                KFPARA.601
C...KFRZ IS THE HIGHEST MODEL LEVEL AT WHICH LIQUID CONDENSATE IS                KFPARA.602
C   GENERATING PPTLIQ IS THE RATE OF GENERATION (FALLOUT) OF LIQUID              KFPARA.603
C   PRECIP AT A GIVING MODEL LVL, PPTICE THE SAME FOR ICE, TRPPT IS              KFPARA.604
C   THE TOTAL RATE OF PRODUCTION OF PRECIP UP TO THE CURRENT MODEL LEVEL         KFPARA.605
C                                                                                KFPARA.606
          IF(ABS(RATIO2(NK1)-1.).GT.1.E-6)KFRZ=NK1                               KFPARA.607
          PPTLIQ(NK1)=QLQOUT(NK1)*(UMF(NK)-UDR(NK1))                             KFPARA.608
          PPTICE(NK1)=QICOUT(NK1)*(UMF(NK)-UDR(NK1))                             KFPARA.609
          TRPPT=TRPPT+PPTLIQ(NK1)+PPTICE(NK1)                                    KFPARA.610
          IF(NK1.LE.KPBL)UER(NK1)=UER(NK1)+VMFLCL*DP(NK1)/DPTHMX                 KFPARA.611
   60   CONTINUE                                                                 KFPARA.612
C                                                                                KFPARA.613
C...CHECK CLOUD DEPTH...IF CLOUD IS TALL ENOUGH, ESTIMATE THE EQUILIBRIU         KFPARA.614
C   TEMPERATURE LEVEL (LET) AND ADJUST MASS FLUX PROFILE AT CLOUD TOP SO         KFPARA.615
C   THAT MASS FLUX DECREASES TO ZERO AS A LINEAR FUNCTION OF PRESSURE            KFPARA.616
C   BETWEEN THE LET AND CLOUD TOP...                                             KFPARA.617
C                                                                                KFPARA.618
C...LTOP IS THE MODEL LEVEL JUST BELOW THE LEVEL AT WHICH VERTICAL               KFPARA.619
C   VELOCITY FIRST BECOMES NEGATIVE...                                           KFPARA.620
C                                                                                KFPARA.621
   65   LTOP=NK                                                                  KFPARA.622
        CLDHGT=Z0(LTOP)-ZLCL                                                     KFPARA.623
C                                                                                KFPARA.624
C...IF CLOUD TOP HGT IS LESS THAN SPECIFIED MINIMUM HEIGHT, GO BACK AND          KFPARA.625
C   THE NEXT HIGHEST 60MB LAYER TO SEE IF A BIGGER CLOUD CAN BE OBTAINED         KFPARA.626
C   THAT SOURCE AIR...                                                           KFPARA.627
C                                                                                KFPARA.628
C       IF(CLDHGT.LT.4.E3.OR.ABE.LT.1.)THEN                                      KFPARA.629
        IF(CLDHGT.LT.3.E3.OR.ABE.LT.1.)THEN                                      KFPARA.630
          DO 70 NK=K,LTOP                                                        KFPARA.631
            UMF(NK)=0.                                                           KFPARA.632
            UDR(NK)=0.                                                           KFPARA.633
            UER(NK)=0.                                                           KFPARA.634
            DETLQ(NK)=0.                                                         KFPARA.635
            DETIC(NK)=0.                                                         KFPARA.636
            PPTLIQ(NK)=0.                                                        KFPARA.637
   70     PPTICE(NK)=0.                                                          KFPARA.638
          GOTO 25                                                                KFPARA.639
        ENDIF                                                                    KFPARA.640
C                                                                                KFPARA.641
C...IF THE LET AND LTOP ARE THE SAME, DETRAIN ALL OF THE UPDRAFT MASS            KFPARA.642
C   FLUX THIS LEVEL...                                                           KFPARA.643
C                                                                                KFPARA.644
        IF(LET.EQ.LTOP)THEN                                                      KFPARA.645
          UDR(LTOP)=UMF(LTOP)+UDR(LTOP)-UER(LTOP)                                KFPARA.646
          DETLQ(LTOP)=QLIQ(LTOP)*UDR(LTOP)*UPNEW/UPOLD                           KFPARA.647
          DETIC(LTOP)=QICE(LTOP)*UDR(LTOP)*UPNEW/UPOLD                           KFPARA.648
          TRPPT=TRPPT-(PPTLIQ(LTOP)+PPTICE(LTOP))                                KFPARA.649
          UER(LTOP)=0.                                                           KFPARA.650
          UMF(LTOP)=0.                                                           KFPARA.651
          GOTO 85                                                                KFPARA.652
        ENDIF                                                                    KFPARA.653
C                                                                                KFPARA.654
C   BEGIN TOTAL DETRAINMENT AT THE LEVEL ABOVE THE LET...                        KFPARA.655
C                                                                                KFPARA.656
        DPTT=0.                                                                  KFPARA.657
        DO 71 NJ=LET+1,LTOP                                                      KFPARA.658
   71   DPTT=DPTT+DP(NJ)                                                         KFPARA.659
        DUMFDP=UMF(LET)/DPTT                                                     KFPARA.660
C                                                                                KFPARA.661
C...ADJUST MASS FLUX PROFILES, DETRAINMENT RATES, AND PRECIPITATION FALL         KFPARA.662
C   RATES TO REFLECT THE LINEAR DECREASE IN MASS FLX BETWEEN THE LET AND         KFPARA.663
C   PTOP                                                                         KFPARA.664
C                                                                                KFPARA.665
        DO 75 NK=LET+1,LTOP                                                      KFPARA.666
          UDR(NK)=DP(NK)*DUMFDP                                                  KFPARA.667
          UMF(NK)=UMF(NK-1)-UDR(NK)                                              KFPARA.668
          DETLQ(NK)=QLIQ(NK)*UDR(NK)                                             KFPARA.669
          DETIC(NK)=QICE(NK)*UDR(NK)                                             KFPARA.670
          TRPPT=TRPPT-PPTLIQ(NK)-PPTICE(NK)                                      KFPARA.671
          PPTLIQ(NK)=(UMF(NK-1)-UDR(NK))*QLQOUT(NK)                              KFPARA.672
          PPTICE(NK)=(UMF(NK-1)-UDR(NK))*QICOUT(NK)                              KFPARA.673
          TRPPT=TRPPT+PPTLIQ(NK)+PPTICE(NK)                                      KFPARA.674
   75   CONTINUE                                                                 KFPARA.675
C                                                                                KFPARA.676
C...SEND UPDRAFT CHARACTERISTICS TO OUTPUT FILES...                              KFPARA.677
C                                                                                KFPARA.678
   85   CONTINUE                                                                 KFPARA.679
C     WRITE(98,3001)                                                             KFPARA.680
3001  FORMAT(' ')                                                                KFPARA.681
C     WRITE(98,3002)XTIME,INEST,I,J                                              KFPARA.682
3002  FORMAT('XTIME =',F9.2,', INEST =',I2,', GRID POINT: I =',I2,               KFPARA.683
     * ' J =',I2)                                                                KFPARA.684
C      IF(MXLAYR.EQ.1)THEN                                                       KFPARA.685
C       PRINT *,'HFX, QFX, PBL, THGV =',HFX(I,J),QFX(I,J),PBL(I,J),THGV          KFPARA.686
C     PRINT *,'RHOE(1), WPTHP, WPQP, WPTHVP, WSTR =',RHOE(1),WPTHP,WPQP,         KFPARA.687
C     *WPTHVP,WSTR                                                               KFPARA.688
C      ENDIF                                                                     KFPARA.689
C     PRINT *,'PRS AT BASE OF UPDRAFT SOURCE LAYER =',P0(LC)/100.                KFPARA.690
C      PRINT *,'DEPTH OF UPDRAFT SOURCE MIXED LAYER =',DPTHMX/100.               KFPARA.691
C      PRINT 1000,'  P  ','  VMFU  ','  TU  ',' EQFRC',' CLDWAT ',               KFPARA.692
C     *' CLDICE ',' PPTLIQ ',' PPTICE ',' DETLQ ',' DETIC '                      KFPARA.693
C      DO 1020 NK=KLCL-1,LTOP                                                    KFPARA.694
C         IF(NK.EQ.KLCL-1)THEN                                                   KFPARA.695
C           PRS=PLCL/100.                                                        KFPARA.696
C         ELSE                                                                   KFPARA.697
C           PRS=P0(NK)/100.                                                      KFPARA.698
C         ENDIF                                                                  KFPARA.699
CC         PRS=CVMGT(PLCL/100.,P0(NK)/100.,NK.EQ.KLCL-1)                         KFPARA.700
C         PRINT 1005,PRS,UMF(NK)/VMFLCL,TU(NK)-273.16,EQFRC(NK),                 KFPARA.701
C     *   QLIQ(NK)*1.E3,QICE(NK)*1.E3,QLQOUT(NK)*1.E3,QICOUT(NK)*1.E3,           KFPARA.702
C     *   DETLQ(NK)/1.E3,DETIC(NK)/1.E3                                          KFPARA.703
C1020  CONTINUE                                                                  KFPARA.704
C      PRINT 1010,P0(NK1)/100.                                                   KFPARA.705
C                                                                                KFPARA.706
C...EXTEND THE UPDRAFT MASS FLUX PROFILE DOWN TO THE SOURCE LAYER FOR            KFPARA.707
C   THE UPDRAFT AIR...ALSO, DEFINE THETAE FOR LEVELS BELOW THE LCL...            KFPARA.708
C                                                                                KFPARA.709
        DO 90 NK=1,K                                                             KFPARA.710
          IF(NK.GE.LC)THEN                                                       KFPARA.711
            IF(NK.EQ.LC)THEN                                                     KFPARA.712
              UMF(NK)=VMFLCL*DP(NK)/DPTHMX                                       KFPARA.713
              UER(NK)=VMFLCL*DP(NK)/DPTHMX                                       KFPARA.714
            ELSEIF(NK.LE.KPBL)THEN                                               KFPARA.715
              UER(NK)=VMFLCL*DP(NK)/DPTHMX                                       KFPARA.716
              UMF(NK)=UMF(NK-1)+UER(NK)                                          KFPARA.717
            ELSE                                                                 KFPARA.718
              UMF(NK)=VMFLCL                                                     KFPARA.719
              UER(NK)=0.                                                         KFPARA.720
            ENDIF                                                                KFPARA.721
            TU(NK)=TMIX+(Z0(NK)-ZMIX)*GDRY                                       KFPARA.722
            QU(NK)=QMIX                                                          KFPARA.723
            WU(NK)=WLCL                                                          KFPARA.724
          ELSE                                                                   KFPARA.725
            TU(NK)=0.                                                            KFPARA.726
            QU(NK)=0.                                                            KFPARA.727
            UMF(NK)=0.                                                           KFPARA.728
            WU(NK)=0.                                                            KFPARA.729
            UER(NK)=0.                                                           KFPARA.730
          ENDIF                                                                  KFPARA.731
          UDR(NK)=0.                                                             KFPARA.732
          QDT(NK)=0.                                                             KFPARA.733
          QLIQ(NK)=0.                                                            KFPARA.734
          QICE(NK)=0.                                                            KFPARA.735
          QLQOUT(NK)=0.                                                          KFPARA.736
          QICOUT(NK)=0.                                                          KFPARA.737
          PPTLIQ(NK)=0.                                                          KFPARA.738
          PPTICE(NK)=0.                                                          KFPARA.739
          DETLQ(NK)=0.                                                           KFPARA.740
          DETIC(NK)=0.                                                           KFPARA.741
          RATIO2(NK)=0.                                                          KFPARA.742
          EE=Q0(NK)*P0(NK)/(0.622+Q0(NK))                                        KFPARA.743
          TLOG=ALOG(EE/ALIQ)                                                     KFPARA.744
          TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)                                      KFPARA.745
          TSAT=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(T0(NK)-T00))*(            KFPARA.746
     +         T0(NK)-TDPT)                                                      KFPARA.747
          THTA=T0(NK)*(1.E5/P0(NK))**(0.2854*(1.-0.28*Q0(NK)))                   KFPARA.748
          THETEE(NK)=THTA*                                                       KFPARA.749
     +               EXP((3374.6525/TSAT-2.5403)*Q0(NK)*(1.+0.81*Q0(NK))         KFPARA.750
     +               )                                                           KFPARA.751
          THTES(NK)=THTA*                                                        KFPARA.752
     +              EXP((3374.6525/T0(NK)-2.5403)*QES(NK)*(1.+0.81*              KFPARA.753
     +              QES(NK)))                                                    KFPARA.754
          EQFRC(NK)=1.0                                                          KFPARA.755
   90   CONTINUE                                                                 KFPARA.756
C                                                                                KFPARA.757
        LTOP1=LTOP+1                                                             KFPARA.758
        LTOPM1=LTOP-1                                                            KFPARA.759
C                                                                                KFPARA.760
C...DEFINE VARIABLES ABOVE CLOUD TOP...                                          KFPARA.761
C                                                                                KFPARA.762
        DO 95 NK=LTOP1,KX                                                        KFPARA.763
          UMF(NK)=0.                                                             KFPARA.764
          UDR(NK)=0.                                                             KFPARA.765
          UER(NK)=0.                                                             KFPARA.766
          QDT(NK)=0.                                                             KFPARA.767
          QLIQ(NK)=0.                                                            KFPARA.768
          QICE(NK)=0.                                                            KFPARA.769
          QLQOUT(NK)=0.                                                          KFPARA.770
          QICOUT(NK)=0.                                                          KFPARA.771
          DETLQ(NK)=0.                                                           KFPARA.772
          DETIC(NK)=0.                                                           KFPARA.773
          PPTLIQ(NK)=0.                                                          KFPARA.774
          PPTICE(NK)=0.                                                          KFPARA.775
          IF(NK.GT.LTOP1)THEN                                                    KFPARA.776
            TU(NK)=0.                                                            KFPARA.777
            QU(NK)=0.                                                            KFPARA.778
            WU(NK)=0.                                                            KFPARA.779
          ENDIF                                                                  KFPARA.780
          THTA0(NK)=0.                                                           KFPARA.781
          THTAU(NK)=0.                                                           KFPARA.782
          EMS(NK)=DP(NK)*DXSQ/G                                                  KFPARA.783
          EMSD(NK)=1./EMS(NK)                                                    KFPARA.784
          TG(NK)=T0(NK)                                                          KFPARA.785
          QG(NK)=Q0(NK)                                                          KFPARA.786
          QLG(NK)=0.                                                             KFPARA.787
          QIG(NK)=0.                                                             KFPARA.788
          QRG(NK)=0.                                                             KFPARA.789
          QSG(NK)=0.                                                             KFPARA.790
   95   OMG(NK)=0.                                                               KFPARA.791
        OMG(KXP1)=0.                                                             KFPARA.792
        P150=P0(KLCL)-1.50E4                                                     KFPARA.793
        DO 100 NK=1,LTOP                                                         KFPARA.794
          THTAD(NK)=0.                                                           KFPARA.795
          EMS(NK)=DP(NK)*DXSQ/G                                                  KFPARA.796
          EMSD(NK)=1./EMS(NK)                                                    KFPARA.797
C                                                                                KFPARA.798
C...INITIALIZE SOME VARIABLES TO BE USED LATER IN THE VERT ADVECTION             KFPARA.799
C   SCHEME                                                                       KFPARA.800
C                                                                                KFPARA.801
          EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*QDT(NK)))                       KFPARA.802
          THTAU(NK)=TU(NK)*EXN(NK)                                               KFPARA.803
          EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*Q0(NK)))                        KFPARA.804
          THTA0(NK)=T0(NK)*EXN(NK)                                               KFPARA.805
C                                                                                KFPARA.806
C...LVF IS THE LEVEL AT WHICH MOISTURE FLUX IS ESTIMATED AS THE BASIS            KFPARA.807
C...FOR PRECIPITATION EFFICIENCY CALCULATIONS...                                 KFPARA.808
C                                                                                KFPARA.809
          IF(P0(NK).GT.P150)LVF=NK                                               KFPARA.810
  100   OMG(NK)=0.                                                               KFPARA.811
        LVF=MIN0(LVF,LET-2)                                                      19DEC02.1361
        USR=UMF(LVF+1)*(QU(LVF+1)+QLIQ(LVF+1)+QICE(LVF+1))                       KFPARA.813
        USR=AMIN1(USR,TRPPT)                                                     KFPARA.814
C                                                                                KFPARA.815
C     WRITE(98,1025)KLCL,ZLCL,DTLCL,LTOP,P0(LTOP),IFLAG,                         KFPARA.816
C    * TMIX-T00,PMIX,QMIX,ABE                                                    KFPARA.817
C     WRITE(98,1030)P0(LET)/100.,P0(LTOP)/100.,VMFLCL,PLCL/100.,                 KFPARA.818
C    * WLCL,CLDHGT                                                               KFPARA.819
C                                                                                KFPARA.820
C...COMPUTE CONVECTIVE TIME SCALE(TIMEC). THE MEAN WIND AT THE LCL               KFPARA.821
C...AND MIDTROPOSPHERE IS USED.                                                  KFPARA.822
C                                                                                KFPARA.823
        WSPD(KLCL)=SQRT(U0(KLCL)*U0(KLCL)+V0(KLCL)*V0(KLCL))                     KFPARA.824
        WSPD(L5)=SQRT(U0(L5)*U0(L5)+V0(L5)*V0(L5))                               KFPARA.825
        WSPD(LTOP)=SQRT(U0(LTOP)*U0(LTOP)+V0(LTOP)*V0(LTOP))                     KFPARA.826
        VCONV=.5*(WSPD(KLCL)+WSPD(L5))                                           KFPARA.827
        TIMEC=DX/VCONV                                                           KFPARA.828
        TADVEC=TIMEC                                                             KFPARA.829
        TIMEC=AMAX1(1800.,TIMEC)                                                 KFPARA.830
        TIMEC=AMIN1(3600.,TIMEC)                                                 KFPARA.831
        NIC=NINT(TIMEC/(.5*DT2))                                                 KFPARA.832
        TIMEC=FLOAT(NIC)*.5*DT2                                                  KFPARA.833
C                                                                                KFPARA.834
C...COMPUTE WIND SHEAR AND PRECIPITATION EFFICIENCY.                             KFPARA.835
C                                                                                KFPARA.836
C        SHSIGN = CVMGT(1.,-1.,WSPD(LTOP).GT.WSPD(KLCL))                         KFPARA.837
        IF(WSPD(LTOP).GT.WSPD(KLCL))THEN                                         KFPARA.838
          SHSIGN=1.                                                              KFPARA.839
        ELSE                                                                     KFPARA.840
          SHSIGN=-1.                                                             KFPARA.841
        ENDIF                                                                    KFPARA.842
        VWS=(U0(LTOP)-U0(KLCL))*(U0(LTOP)-U0(KLCL))+(V0(LTOP)-V0(KLCL))*         KFPARA.843
     +      (V0(LTOP)-V0(KLCL))                                                  KFPARA.844
        VWS=1.E3*SHSIGN*SQRT(VWS)/(Z0(LTOP)-Z0(LCL))                             KFPARA.845
        PEF=1.591+VWS*(-.639+VWS*(9.53E-2-VWS*4.96E-3))                          KFPARA.846
        PEF=AMAX1(PEF,.2)                                                        KFPARA.847
        PEF=AMIN1(PEF,.9)                                                        KFPARA.848
C                                                                                KFPARA.849
C...PRECIPITATION EFFICIENCY IS A FUNCTION OF THE HEIGHT OF CLOUD BASE.          KFPARA.850
C                                                                                KFPARA.851
        CBH=(ZLCL-Z0(1))*3.281E-3                                                KFPARA.852
        IF(CBH.LT.3.)THEN                                                        KFPARA.853
          RCBH=.02                                                               KFPARA.854
        ELSE                                                                     KFPARA.855
          RCBH=.96729352+CBH*(-.70034167+CBH*(.162179896+CBH*(-                  KFPARA.856
     +         1.2569798E-2+CBH*(4.2772E-4-CBH*5.44E-6))))                       KFPARA.857
        ENDIF                                                                    KFPARA.858
        IF(CBH.GT.25)RCBH=2.4                                                    KFPARA.859
        PEFCBH=1./(1.+RCBH)                                                      KFPARA.860
        PEFCBH=AMIN1(PEFCBH,.9)                                                  KFPARA.861
C                                                                                KFPARA.862
C... MEAN PEF. IS USED TO COMPUTE RAINFALL.                                      KFPARA.863
C                                                                                KFPARA.864
        PEFF=.5*(PEF+PEFCBH)                                                     KFPARA.865
        PEFF2=PEFF                                                               KFPARA.866
C        WRITE(98,1035)PEF,PEFCBH,LC,LET,WKL,VWS                                 KFPARA.867
C                                                                                KFPARA.868
C*****************************************************************               KFPARA.869
C                                                                *               KFPARA.870
C                  COMPUTE DOWNDRAFT PROPERTIES                  *               KFPARA.871
C                                                                *               KFPARA.872
C*****************************************************************               KFPARA.873
C                                                                                KFPARA.874
C...LET DOWNDRAFT ORIGINATE AT THE LEVEL OF MINIMUM SATURATION EQUIVALEN         KFPARA.875
C...POTENTIAL TEMPERATURE (SEQT) IN THE CLOUD LAYER, EXTEND DOWNWARD TO          KFPARA.876
C...SURFACE, OR TO THE LAYER BELOW CLOUD BASE AT WHICH ENVIR SEQT IS LES         KFPARA.877
C...THAN MIN SEQT IN THE CLOUD LAYER...LET DOWNDRAFT DETRAIN OVER A LAYE         KFPARA.878
C...OF SPECIFIED PRESSURE-DEPTH (DPDD)...                                        KFPARA.879
C                                                                                KFPARA.880
        TDER=0.                                                                  KFPARA.881
        KSTART=MAX0(KPBL,KLCL)                                                   KFPARA.882
        THTMIN=THTES(KSTART+1)                                                   KFPARA.883
        KMIN=KSTART+1                                                            KFPARA.884
        DO 104 NK=KSTART+2,LTOP-1                                                KFPARA.885
          THTMIN=AMIN1(THTMIN,THTES(NK))                                         KFPARA.886
          IF(THTMIN.EQ.THTES(NK))KMIN=NK                                         KFPARA.887
  104   CONTINUE                                                                 KFPARA.888
        LFS=KMIN                                                                 KFPARA.889
        IF(RATIO2(LFS).GT.0.)CALL ENVIRTHT(P0(LFS),T0(LFS),Q0(LFS),              KFPARA.890
     +    THETEE(LFS),0.,RL,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)             KFPARA.891
        EQFRC(LFS)=(THTES(LFS)-THETEU(LFS))/(THETEE(LFS)-THETEU(LFS))            KFPARA.892
        EQFRC(LFS)=AMAX1(EQFRC(LFS),0.)                                          KFPARA.893
        EQFRC(LFS)=AMIN1(EQFRC(LFS),1.)                                          KFPARA.894
        THETED(LFS)=THTES(LFS)                                                   KFPARA.895
C                                                                                KFPARA.896
C...ESTIMATE THE EFFECT OF MELTING PRECIPITATION IN THE DOWNDRAFT...             KFPARA.897
C                                                                                KFPARA.898
        IF(ML.GT.0)THEN                                                          KFPARA.899
          DTMLTD=0.5*(QU(KLCL)-QU(LTOP))*RLF/CP                                  KFPARA.900
        ELSE                                                                     KFPARA.901
          DTMLTD=0.                                                              KFPARA.902
        ENDIF                                                                    KFPARA.903
        TZ(LFS)=T0(LFS)-DTMLTD                                                   KFPARA.904
        ES=ALIQ*EXP((TZ(LFS)*BLIQ-CLIQ)/(TZ(LFS)-DLIQ))                          KFPARA.905
        QS=0.622*ES/(P0(LFS)-ES)                                                 KFPARA.906
        QD(LFS)=EQFRC(LFS)*Q0(LFS)+(1.-EQFRC(LFS))*QU(LFS)                       KFPARA.907
        THTAD(LFS)=TZ(LFS)*(P00/P0(LFS))**(0.2854*(1.-0.28*QD(LFS)))             KFPARA.908
        IF(QD(LFS).GE.QS)THEN                                                    KFPARA.909
          THETED(LFS)=THTAD(LFS)*                                                KFPARA.910
     +                EXP((3374.6525/TZ(LFS)-2.5403)*QS*(1.+0.81*QS))            KFPARA.911
        ELSE                                                                     KFPARA.912
          CALL ENVIRTHT(P0(LFS),TZ(LFS),QD(LFS),THETED(LFS),0.,RL,ALIQ,          KFPARA.913
     +         BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)                               KFPARA.914
        ENDIF                                                                    KFPARA.915
        DO 107 NK=1,LFS                                                          KFPARA.916
          ND=LFS-NK                                                              KFPARA.917
          IF(THETED(LFS).GT.THTES(ND).OR.ND.EQ.1)THEN                            KFPARA.918
            LDB=ND                                                               KFPARA.919
C                                                                                KFPARA.920
C...IF DOWNDRAFT NEVER BECOMES NEGATIVELY BUOYANT OR IF IT                       KFPARA.921
C...IS SHALLOWER 50 mb, DON'T ALLOW IT TO OCCUR AT ALL...                        KFPARA.922
C                                                                                KFPARA.923
            IF(NK.EQ.1.OR.(P0(LDB)-P0(LFS)).LT.50.E2)GOTO 141                    KFPARA.924
            GOTO 110                                                             KFPARA.925
          ENDIF                                                                  KFPARA.926
  107   CONTINUE                                                                 KFPARA.927
C                                                                                KFPARA.928
C...ALLOW DOWNDRAFT TO DETRAIN IN A SINGLE LAYER, BUT WITH DOWNDRAFT AIR         KFPARA.929
C...TYPICALLY FLUSHED UP INTO HIGHER LAYERS AS ALLOWED IN THE TOTAL              KFPARA.930
C...VERTICAL ADVECTION CALCULATIONS FARTHER DOWN IN THE CODE...                  KFPARA.931
C                                                                                KFPARA.932
  110   DPDD=DP(LDB)                                                             KFPARA.933
        LDT=LDB                                                                  KFPARA.934
        FRC=1.                                                                   KFPARA.935
        DPT=0.                                                                   KFPARA.936
C      DO 115 NK=LDB,LFS                                                         KFPARA.937
C        DPT=DPT+DP(NK)                                                          KFPARA.938
C        IF(DPT.GT.DPDD)THEN                                                     KFPARA.939
C          LDT=NK                                                                KFPARA.940
C          FRC=(DPDD+DP(NK)-DPT)/DP(NK)                                          KFPARA.941
C          GOTO 120                                                              KFPARA.942
C        ENDIF                                                                   KFPARA.943
C        IF(NK.EQ.LFS-1)THEN                                                     KFPARA.944
C         LDT=NK                                                                 KFPARA.945
C        FRC=1.                                                                  KFPARA.946
C        DPDD=DPT                                                                KFPARA.947
C        GOTO 120                                                                KFPARA.948
C        ENDIF                                                                   KFPARA.949
C115   CONTINUE                                                                  KFPARA.950
  120   CONTINUE                                                                 KFPARA.951
C                                                                                KFPARA.952
C...TAKE A FIRST GUESS AT THE INITIAL DOWNDRAFT MASS FLUX...                     KFPARA.953
C                                                                                KFPARA.954
        TVD(LFS)=T0(LFS)*(1.+0.608*QES(LFS))                                     KFPARA.955
        RDD=P0(LFS)/(R*TVD(LFS))                                                 KFPARA.956
        A1=(1.-PEFF)*AU0                                                         KFPARA.957
        DMF(LFS)=-A1*RDD                                                         KFPARA.958
        DER(LFS)=EQFRC(LFS)*DMF(LFS)                                             KFPARA.959
        DDR(LFS)=0.                                                              KFPARA.960
        DO 140 ND=LFS-1,LDB,-1                                                   KFPARA.961
          ND1=ND+1                                                               KFPARA.962
          IF(ND.LE.LDT)THEN                                                      KFPARA.963
            DER(ND)=0.                                                           KFPARA.964
            DDR(ND)=-DMF(LDT+1)*DP(ND)*FRC/DPDD                                  KFPARA.965
            DMF(ND)=DMF(ND1)+DDR(ND)                                             KFPARA.966
            FRC=1.                                                               KFPARA.967
            THETED(ND)=THETED(ND1)                                               KFPARA.968
            QD(ND)=QD(ND1)                                                       KFPARA.969
          ELSE                                                                   KFPARA.970
            DER(ND)=DMF(LFS)*0.03*DP(ND)/RAD                                     KFPARA.971
            DDR(ND)=0.                                                           KFPARA.972
            DMF(ND)=DMF(ND1)+DER(ND)                                             KFPARA.973
            IF(RATIO2(ND).GT.0.)CALL ENVIRTHT(P0(ND),T0(ND),Q0(ND),              KFPARA.974
     +        THETEE(ND),0.,RL,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)          KFPARA.975
            THETED(ND)=(THETED(ND1)*DMF(ND1)+THETEE(ND)*DER(ND))/DMF(ND)         KFPARA.976
            QD(ND)=(QD(ND1)*DMF(ND1)+Q0(ND)*DER(ND))/DMF(ND)                     KFPARA.977
          ENDIF                                                                  KFPARA.978
  140   CONTINUE                                                                 KFPARA.979
        TDER=0.                                                                  KFPARA.980
C                                                                                KFPARA.981
C...CALCULATION AN EVAPORATION RATE FOR GIVEN MASS FLUX...                       KFPARA.982
C                                                                                KFPARA.983
        DO 135 ND=LDB,LDT                                                        KFPARA.984
          TZ(ND)=                                                                KFPARA.985
     +           TPDD(P0(ND),THETED(LDT),T0(ND),QS,QD(ND),1.0,XLV0,XLV1,         KFPARA.986
     +           ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)                        KFPARA.987
          ES=ALIQ*EXP((TZ(ND)*BLIQ-CLIQ)/(TZ(ND)-DLIQ))                          KFPARA.988
          QS=0.622*ES/(P0(ND)-ES)                                                KFPARA.989
          DSSDT=(CLIQ-BLIQ*DLIQ)/((TZ(ND)-DLIQ)*(TZ(ND)-DLIQ))                   KFPARA.990
          RL=XLV0-XLV1*TZ(ND)                                                    KFPARA.991
          DTMP=RL*QS*(1.-RHBC)/(CP+RL*RHBC*QS*DSSDT)                             KFPARA.992
          T1RH=TZ(ND)+DTMP                                                       KFPARA.993
          ES=RHBC*ALIQ*EXP((BLIQ*T1RH-CLIQ)/(T1RH-DLIQ))                         KFPARA.994
          QSRH=0.622*ES/(P0(ND)-ES)                                              KFPARA.995
C                                                                                KFPARA.996
C...CHECK TO SEE IF MIXING RATIO AT SPECIFIED RH IS LESS THAN ACTUAL             KFPARA.997
C...MIXING RATIO...IF SO, ADJUST TO GIVE ZERO EVAPORATION...                     KFPARA.998
C                                                                                KFPARA.999
          IF(QSRH.LT.QD(ND))THEN                                                 KFPARA.1000
            QSRH=QD(ND)                                                          KFPARA.1001
C          T1RH=T1+(QS-QSRH)*RL/CP                                               KFPARA.1002
            T1RH=TZ(ND)                                                          KFPARA.1003
          ENDIF                                                                  KFPARA.1004
          TZ(ND)=T1RH                                                            KFPARA.1005
          QS=QSRH                                                                KFPARA.1006
          TDER=TDER+(QS-QD(ND))*DDR(ND)                                          KFPARA.1007
          QD(ND)=QS                                                              KFPARA.1008
  135   THTAD(ND)=TZ(ND)*(P00/P0(ND))**(0.2854*(1.-0.28*QD(ND)))                 KFPARA.1009
C                                                                                KFPARA.1010
C...IF DOWNDRAFT DOES NOT EVAPORATE ANY WATER FOR SPECIFIED RELATIVE             KFPARA.1011
C...HUMIDITY, NO DOWNDRAFT IS ALLOWED...                                         KFPARA.1012
C                                                                                KFPARA.1013
  141   IF(TDER.LT.1.)THEN                                                       KFPARA.1014
C          WRITE(98,3004)I,J                                                     KFPARA.1015
 3004       FORMAT(' ','I=',I3,2X,'J=',I3)                                       KFPARA.1016
          PPTFLX=TRPPT                                                           KFPARA.1017
          CPR=TRPPT                                                              KFPARA.1018
          TDER=0.                                                                KFPARA.1019
          CNDTNF=0.                                                              KFPARA.1020
          UPDINC=1.                                                              KFPARA.1021
          LDB=LFS                                                                KFPARA.1022
          DO 117 NDK=1,LTOP                                                      KFPARA.1023
            DMF(NDK)=0.                                                          KFPARA.1024
            DER(NDK)=0.                                                          KFPARA.1025
            DDR(NDK)=0.                                                          KFPARA.1026
            THTAD(NDK)=0.                                                        KFPARA.1027
            WD(NDK)=0.                                                           KFPARA.1028
            TZ(NDK)=0.                                                           KFPARA.1029
  117     QD(NDK)=0.                                                             KFPARA.1030
          AINCM2=100.                                                            KFPARA.1031
          GOTO 165                                                               KFPARA.1032
        ENDIF                                                                    KFPARA.1033
C                                                                                KFPARA.1034
C...ADJUST DOWNDRAFT MASS FLUX SO THAT EVAPORATION RATE IN DOWNDRAFT IS          KFPARA.1035
C...CONSISTENT WITH PRECIPITATION EFFICIENCY RELATIONSHIP...                     KFPARA.1036
C                                                                                KFPARA.1037
        DEVDMF=TDER/DMF(LFS)                                                     KFPARA.1038
        PPR=0.                                                                   KFPARA.1039
        PPTFLX=PEFF*USR                                                          KFPARA.1040
        RCED=TRPPT-PPTFLX                                                        KFPARA.1041
C                                                                                KFPARA.1042
C...PPR IS THE TOTAL AMOUNT OF PRECIPITATION THAT FALLS  OUT OF THE              KFPARA.1043
C...UPDRAFT FROM CLOUD BASE TO THE LFS...UPDRAFT MASS FLUX WILL BE               KFPARA.1044
C...INCREASED UP TO THE LFS TO ACCOUNT FOR UPDRAFT AIR MIXING WITH               KFPARA.1045
C...ENVIRONMENTAL AIR TO THE UPDRAFT, SO PPR WILL INCREASE                       KFPARA.1046
C...PROPORTIONATELY...                                                           KFPARA.1047
C                                                                                KFPARA.1048
        DO 132 NM=KLCL,LFS                                                       KFPARA.1049
  132   PPR=PPR+PPTLIQ(NM)+PPTICE(NM)                                            KFPARA.1050
        IF(LFS.GE.KLCL)THEN                                                      KFPARA.1051
          DPPTDF=(1.-PEFF)*PPR*(1.-EQFRC(LFS))/UMF(LFS)                          KFPARA.1052
        ELSE                                                                     KFPARA.1053
          DPPTDF=0.                                                              KFPARA.1054
        ENDIF                                                                    KFPARA.1055
C                                                                                KFPARA.1056
C...CNDTNF IS THE AMOUNT OF CONDENSATE TRANSFERRED ALONG WITH UPDRAFT            KFPARA.1057
C...MASS THE DOWNDRAFT AT THE LFS...                                             KFPARA.1058
C                                                                                KFPARA.1059
        CNDTNF=(QLIQ(LFS)+QICE(LFS))*(1.-EQFRC(LFS))                             KFPARA.1060
        DMFLFS=RCED/(DEVDMF+DPPTDF+CNDTNF)                                       KFPARA.1061
        IF(DMFLFS.GT.0.)THEN                                                     KFPARA.1062
          TDER=0.                                                                KFPARA.1063
          GOTO 141                                                               KFPARA.1064
        ENDIF                                                                    KFPARA.1065
C                                                                                KFPARA.1066
C...DDINC IS THE FACTOR BY WHICH TO INCREASE THE FIRST-GUESS DOWNDRAFT           KFPARA.1067
C...MASS FLUX TO SATISFY THE PRECIP EFFICIENCY RELATIONSHIP, UPDINC IS T         KFPARA.1068
C...WHICH TO INCREASE THE UPDRAFT MASS FLUX BELOW THE LFS TO ACCOUNT FOR         KFPARA.1069
C...TRANSFER OF MASS FROM UPDRAFT TO DOWNDRAFT...                                KFPARA.1070
C                                                                                KFPARA.1071
C       DDINC=DMFLFS/DMF(LFS)                                                    KFPARA.1072
        IF(LFS.GE.KLCL)THEN                                                      KFPARA.1073
          UPDINC=(UMF(LFS)-(1.-EQFRC(LFS))*DMFLFS)/UMF(LFS)                      KFPARA.1074
C                                                                                KFPARA.1075
C...LIMIT UPDINC TO LESS THAN OR EQUAL TO 1.5...                                 KFPARA.1076
C                                                                                KFPARA.1077
          IF(UPDINC.GT.1.5)THEN                                                  KFPARA.1078
            UPDINC=1.5                                                           KFPARA.1079
            DMFLFS2=UMF(LFS)*(UPDINC-1.)/(EQFRC(LFS)-1.)                         KFPARA.1080
            RCED2=DMFLFS2*(DEVDMF+DPPTDF+CNDTNF)                                 KFPARA.1081
            PPTFLX=PPTFLX+(RCED-RCED2)                                           KFPARA.1082
            PEFF2=PPTFLX/USR                                                     KFPARA.1083
            RCED=RCED2                                                           KFPARA.1084
            DMFLFS=DMFLFS2                                                       KFPARA.1085
          ENDIF                                                                  KFPARA.1086
        ELSE                                                                     KFPARA.1087
          UPDINC=1.                                                              KFPARA.1088
        ENDIF                                                                    KFPARA.1089
        DDINC=DMFLFS/DMF(LFS)                                                    KFPARA.1090
        DO 149 NK=LDB,LFS                                                        KFPARA.1091
          DMF(NK)=DMF(NK)*DDINC                                                  KFPARA.1092
          DER(NK)=DER(NK)*DDINC                                                  KFPARA.1093
          DDR(NK)=DDR(NK)*DDINC                                                  KFPARA.1094
  149   CONTINUE                                                                 KFPARA.1095
        CPR=TRPPT+PPR*(UPDINC-1.)                                                KFPARA.1096
        PPTFLX=PPTFLX+PEFF*PPR*(UPDINC-1.)                                       KFPARA.1097
        PEFF=PEFF2                                                               KFPARA.1098
        TDER=TDER*DDINC                                                          KFPARA.1099
C                                                                                KFPARA.1100
C...ADJUST UPDRAFT MASS FLUX, MASS DETRAINMENT RATE, AND LIQUID WATER AN         KFPARA.1101
C   DETRAINMENT RATES TO BE CONSISTENT WITH THE TRANSFER OF THE ESTIMATE         KFPARA.1102
C   FROM THE UPDRAFT TO THE DOWNDRAFT AT THE LFS...                              KFPARA.1103
C                                                                                KFPARA.1104
        DO 155 NK=LC,LFS                                                         KFPARA.1105
          UMF(NK)=UMF(NK)*UPDINC                                                 KFPARA.1106
          UDR(NK)=UDR(NK)*UPDINC                                                 KFPARA.1107
          UER(NK)=UER(NK)*UPDINC                                                 KFPARA.1108
          PPTLIQ(NK)=PPTLIQ(NK)*UPDINC                                           KFPARA.1109
          PPTICE(NK)=PPTICE(NK)*UPDINC                                           KFPARA.1110
          DETLQ(NK)=DETLQ(NK)*UPDINC                                             KFPARA.1111
  155   DETIC(NK)=DETIC(NK)*UPDINC                                               KFPARA.1112
C                                                                                KFPARA.1113
C...ZERO OUT THE ARRAYS FOR DOWNDRAFT DATA AT LEVELS ABOVE AND BELOW THE         KFPARA.1114
C...DOWNDRAFT...                                                                 KFPARA.1115
C                                                                                KFPARA.1116
        IF(LDB.GT.1)THEN                                                         KFPARA.1117
          DO 156 NK=1,LDB-1                                                      KFPARA.1118
            DMF(NK)=0.                                                           KFPARA.1119
            DER(NK)=0.                                                           KFPARA.1120
            DDR(NK)=0.                                                           KFPARA.1121
            WD(NK)=0.                                                            KFPARA.1122
            TZ(NK)=0.                                                            KFPARA.1123
            QD(NK)=0.                                                            KFPARA.1124
            THTAD(NK)=0.                                                         KFPARA.1125
  156     CONTINUE                                                               KFPARA.1126
        ENDIF                                                                    KFPARA.1127
        DO 157 NK=LFS+1,KX                                                       KFPARA.1128
          DMF(NK)=0.                                                             KFPARA.1129
          DER(NK)=0.                                                             KFPARA.1130
          DDR(NK)=0.                                                             KFPARA.1131
          WD(NK)=0.                                                              KFPARA.1132
          TZ(NK)=0.                                                              KFPARA.1133
          QD(NK)=0.                                                              KFPARA.1134
          THTAD(NK)=0.                                                           KFPARA.1135
  157   CONTINUE                                                                 KFPARA.1136
        DO 158 NK=LDT+1,LFS-1                                                    KFPARA.1137
          TZ(NK)=0.                                                              KFPARA.1138
          QD(NK)=0.                                                              KFPARA.1139
  158   CONTINUE                                                                 KFPARA.1140
C                                                                                KFPARA.1141
C...SET LIMITS ON THE UPDRAFT AND DOWNDRAFT MASS FLUXES SO THAT THE              KFPARA.1142
C   INFLOW INTO CONVECTIVE DRAFTS FROM A GIVEN LAYER IS NO MORE THAN             KFPARA.1143
C   IS AVAILABLE IN THAT LAYER INITIALLY...                                      KFPARA.1144
C                                                                                KFPARA.1145
  165   AINCMX=1000.                                                             KFPARA.1146
        LMAX=MAX0(KLCL,LFS)                                                      KFPARA.1147
        DO 166 NK=LC,LMAX                                                        KFPARA.1148
          IF((UER(NK)-DER(NK)).GT.0.)AINCM1=EMS(NK)/((UER(NK)-DER(NK))*          KFPARA.1149
     +      TIMEC)                                                               KFPARA.1150
          AINCMX=AMIN1(AINCMX,AINCM1)                                            KFPARA.1151
  166   CONTINUE                                                                 KFPARA.1152
        AINC=1.                                                                  KFPARA.1153
        IF(AINCMX.LT.AINC)AINC=AINCMX                                            KFPARA.1154
C                                                                                KFPARA.1155
C...SAVE THE RELEVENT VARIABLES FOR A UNIT UPDRFT AND DOWNDRFT...THEY            KFPARA.1156
C...WILL ITERATIVELY ADJUSTED BY THE FACTOR AINC TO SATISFY THE                  KFPARA.1157
C...STABILIZATION CLOSURE...                                                     KFPARA.1158
C                                                                                KFPARA.1159
        NCOUNT=0                                                                 KFPARA.1160
        TDER2=TDER                                                               KFPARA.1161
        PPTFL2=PPTFLX                                                            KFPARA.1162
        DO 170 NK=1,LTOP                                                         KFPARA.1163
          DETLQ2(NK)=DETLQ(NK)                                                   KFPARA.1164
          DETIC2(NK)=DETIC(NK)                                                   KFPARA.1165
          UDR2(NK)=UDR(NK)                                                       KFPARA.1166
          UER2(NK)=UER(NK)                                                       KFPARA.1167
          DDR2(NK)=DDR(NK)                                                       KFPARA.1168
          DER2(NK)=DER(NK)                                                       KFPARA.1169
          UMF2(NK)=UMF(NK)                                                       KFPARA.1170
          DMF2(NK)=DMF(NK)                                                       KFPARA.1171
  170   CONTINUE                                                                 KFPARA.1172
        FABE=1.                                                                  KFPARA.1173
        STAB=0.95                                                                KFPARA.1174
C      XNIN=AMIN0(I,J,ILX-I,JLX-J)                                               KFPARA.1175
C      IF(XNIN.LT.5)STAB=STAB*(XNIN-1.)*0.25                                     KFPARA.1176
        NOITR=0                                                                  KFPARA.1177
        IF(AINC/AINCMX.GT.0.999)THEN                                             KFPARA.1178
          NCOUNT=0                                                               KFPARA.1179
          GOTO 255                                                               KFPARA.1180
        ENDIF                                                                    KFPARA.1181
        ISTOP=0                                                                  KFPARA.1182
  175   NCOUNT=NCOUNT+1                                                          KFPARA.1183
C                                                                                KFPARA.1184
C*****************************************************************               KFPARA.1185
C                                                                *               KFPARA.1186
C           COMPUTE PROPERTIES FOR COMPENSATIONAL SUBSIDENCE     *               KFPARA.1187
C                                                                *               KFPARA.1188
C*****************************************************************               KFPARA.1189
C                                                                                KFPARA.1190
C...DETERMINE OMEGA VALUE NECESSARY AT TOP AND BOTTOM OF EACH LAYER TO           KFPARA.1191
C...SATISFY MASS CONTINUITY...                                                   KFPARA.1192
C                                                                                KFPARA.1193
  185   CONTINUE                                                                 KFPARA.1194
        DTT=TIMEC                                                                KFPARA.1195
        DO 200 NK=1,LTOP                                                         KFPARA.1196
          DOMGDP(NK)=-(UER(NK)-DER(NK)-UDR(NK)-DDR(NK))*EMSD(NK)                 KFPARA.1197
          IF(NK.GT.1)THEN                                                        KFPARA.1198
            OMG(NK)=OMG(NK-1)-DP(NK-1)*DOMGDP(NK-1)                              KFPARA.1199
            DTT1=0.75*DP(NK-1)/(ABS(OMG(NK))+1.E-10)                             KFPARA.1200
            DTT=AMIN1(DTT,DTT1)                                                  KFPARA.1201
          ENDIF                                                                  KFPARA.1202
  200   CONTINUE                                                                 KFPARA.1203
        DO 488 NK=1,LTOP                                                         KFPARA.1204
          THPA(NK)=THTA0(NK)                                                     KFPARA.1205
          QPA(NK)=Q0(NK)                                                         KFPARA.1206
          NSTEP=NINT(TIMEC/DTT+1)                                                KFPARA.1207
          DTIME=TIMEC/FLOAT(NSTEP)                                               KFPARA.1208
          FXM(NK)=OMG(NK)*DXSQ/G                                                 KFPARA.1209
  488   CONTINUE                                                                 KFPARA.1210
C                                                                                KFPARA.1211
C...DO AN UPSTREAM/FORWARD-IN-TIME ADVECTION OF THETA, QV...                     KFPARA.1212
C                                                                                KFPARA.1213
        DO 495 NTC=1,NSTEP                                                       KFPARA.1214
C                                                                                KFPARA.1215
C...ASSIGN THETA AND Q VALUES AT THE TOP AND BOTTOM OF EACH LAYER BASED          KFPARA.1216
C...SIGN OF OMEGA...                                                             KFPARA.1217
C                                                                                KFPARA.1218
          DO 493 NK=1,LTOP                                                       KFPARA.1219
            THFXIN(NK)=0.                                                        KFPARA.1220
            THFXOUT(NK)=0.                                                       KFPARA.1221
            QFXIN(NK)=0.                                                         KFPARA.1222
  493     QFXOUT(NK)=0.                                                          KFPARA.1223
          DO 494 NK=2,LTOP                                                       KFPARA.1224
            IF(OMG(NK).LE.0.)THEN                                                KFPARA.1225
              THFXIN(NK)=-FXM(NK)*THPA(NK-1)                                     KFPARA.1226
              QFXIN(NK)=-FXM(NK)*QPA(NK-1)                                       KFPARA.1227
              THFXOUT(NK-1)=THFXOUT(NK-1)+THFXIN(NK)                             KFPARA.1228
              QFXOUT(NK-1)=QFXOUT(NK-1)+QFXIN(NK)                                KFPARA.1229
            ELSE                                                                 KFPARA.1230
              THFXOUT(NK)=FXM(NK)*THPA(NK)                                       KFPARA.1231
              QFXOUT(NK)=FXM(NK)*QPA(NK)                                         KFPARA.1232
              THFXIN(NK-1)=THFXIN(NK-1)+THFXOUT(NK)                              KFPARA.1233
              QFXIN(NK-1)=QFXIN(NK-1)+QFXOUT(NK)                                 KFPARA.1234
            ENDIF                                                                KFPARA.1235
  494     CONTINUE                                                               KFPARA.1236
C                                                                                KFPARA.1237
C...UPDATE THE THETA AND QV VALUES AT EACH LEVEL...                              KFPARA.1238
C                                                                                KFPARA.1239
          DO 492 NK=1,LTOP                                                       KFPARA.1240
            THPA(NK)=THPA(NK)+(THFXIN(NK)+UDR(NK)*THTAU(NK)+DDR(NK)*             KFPARA.1241
     +               THTAD(NK)-THFXOUT(NK)-(UER(NK)-DER(NK))*THTA0(NK))*         KFPARA.1242
     +               DTIME*EMSD(NK)                                              KFPARA.1243
            QPA(NK)=QPA(NK)+(QFXIN(NK)+UDR(NK)*QDT(NK)+DDR(NK)*QD(NK)-           KFPARA.1244
     +              QFXOUT(NK)-(UER(NK)-DER(NK))*Q0(NK))*DTIME*EMSD(NK)          KFPARA.1245
  492     CONTINUE                                                               KFPARA.1246
  495   CONTINUE                                                                 KFPARA.1247
        DO 498 NK=1,LTOP                                                         KFPARA.1248
          THTAG(NK)=THPA(NK)                                                     KFPARA.1249
          QG(NK)=QPA(NK)                                                         KFPARA.1250
  498   CONTINUE                                                                 KFPARA.1251
C                                                                                KFPARA.1252
C...CHECK TO SEE IF MIXING RATIO DIPS BELOW ZERO ANYWHERE;  IF SO,               KFPARA.1253
C...BORROW MOISTURE FROM ADJACENT LAYERS TO BRING IT BACK UP ABOVE ZERO.         KFPARA.1254
C                                                                                KFPARA.1255
        DO 499 NK=1,LTOP                                                         KFPARA.1256
          IF(QG(NK).LT.0.)THEN                                                   KFPARA.1257
            IF(NK.EQ.1)THEN                                                      KFPARA.1258
              PRINT *,'!!!!! PROBLEM WITH KF SCHEME:  '                          KFPARA.1259
              PRINT *,'QG = 0 AT THE SURFACE!!!!!!!'                             KFPARA.1260
              STOP 'QG'                                                          KFPARA.1261
            ENDIF                                                                KFPARA.1262
            NK1=NK+1                                                             KFPARA.1263
            IF(NK.EQ.LTOP)NK1=KLCL                                               KFPARA.1264
            TMA=QG(NK1)*EMS(NK1)                                                 KFPARA.1265
            TMB=QG(NK-1)*EMS(NK-1)                                               KFPARA.1266
            TMM=(QG(NK)-1.E-9)*EMS(NK)                                           KFPARA.1267
            BCOEFF=-TMM/((TMA*TMA)/TMB+TMB)                                      KFPARA.1268
            ACOEFF=BCOEFF*TMA/TMB                                                KFPARA.1269
            TMB=TMB*(1.-BCOEFF)                                                  KFPARA.1270
            TMA=TMA*(1.-ACOEFF)                                                  KFPARA.1271
            IF(NK.EQ.LTOP)THEN                                                   KFPARA.1272
              QVDIFF=(QG(NK1)-TMA*EMSD(NK1))*100./QG(NK1)                        KFPARA.1273
              IF(ABS(QVDIFF).GT.1.)THEN                                          KFPARA.1274
            PRINT *,'!!WARNING!! CLOUD BASE WATER VAPOR CHANGES BY ',            KFPARA.1275
     *      QVDIFF,                                                              KFPARA.1276
     *       ' PERCENT WHEN MOISTURE IS BORROWED TO PREVENT NEG VALUES',         KFPARA.1277
     *       ' IN KAIN-FRITSCH'                                                  KFPARA.1278
              ENDIF                                                              KFPARA.1279
            ENDIF                                                                KFPARA.1280
            QG(NK)=1.E-9                                                         KFPARA.1281
            QG(NK1)=TMA*EMSD(NK1)                                                KFPARA.1282
            QG(NK-1)=TMB*EMSD(NK-1)                                              KFPARA.1283
          ENDIF                                                                  KFPARA.1284
  499   CONTINUE                                                                 KFPARA.1285
        TOPOMG=(UDR(LTOP)-UER(LTOP))*DP(LTOP)*EMSD(LTOP)                         KFPARA.1286
        IF(ABS(TOPOMG-OMG(LTOP)).GT.1.E-3)THEN                                   KFPARA.1287
C       WRITE(98,*)'ERROR:  MASS DOES NOT BALANCE IN KF SCHEME;'                 KFPARA.1288
C    * ,'TOPOMG, OMG =',TOPOMG,OMG(LTOP)                                         KFPARA.1289
        WRITE(6,*)'ERROR:  MASS DOES NOT BALANCE IN KF SCHEME;'                  KFPARA.1290
     * ,'TOPOMG, OMG =',TOPOMG,OMG(LTOP)                                         KFPARA.1291
          ISTOP=1                                                                KFPARA.1292
          GOTO 265                                                               KFPARA.1293
        ENDIF                                                                    KFPARA.1294
C                                                                                KFPARA.1295
C...CONVERT THETA TO T...                                                        KFPARA.1296
C                                                                                KFPARA.1297
        DO 230 NK=1,LTOP                                                         KFPARA.1298
          EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*QG(NK)))                        KFPARA.1299
          TG(NK)=THTAG(NK)/EXN(NK)                                               KFPARA.1300
          TVG(NK)=TG(NK)*(1.+0.608*QG(NK))                                       KFPARA.1301
  230   CONTINUE                                                                 KFPARA.1302
C                                                                                KFPARA.1303
C*******************************************************************             KFPARA.1304
C                                                                  *             KFPARA.1305
C     COMPUTE NEW CLOUD AND CHANGE IN AVAILABLE BUOYANT ENERGY.    *             KFPARA.1306
C                                                                  *             KFPARA.1307
C*******************************************************************             KFPARA.1308
C                                                                                KFPARA.1309
C...THE FOLLOWING COMPUTATIONS ARE SIMILAR TO THAT FOR UPDRAFT                   KFPARA.1310
C                                                                                KFPARA.1311
        THMIX=0.                                                                 KFPARA.1312
        QMIX=0.                                                                  KFPARA.1313
        PMIX=0.                                                                  KFPARA.1314
        DO 217 NK=LC,KPBL                                                        KFPARA.1315
          ROCPQ=0.2854*(1.-0.28*QG(NK))                                          KFPARA.1316
          THMIX=THMIX+DP(NK)*TG(NK)*(P00/P0(NK))**ROCPQ                          KFPARA.1317
          QMIX=QMIX+DP(NK)*QG(NK)                                                KFPARA.1318
  217   PMIX=PMIX+DP(NK)*P0(NK)                                                  KFPARA.1319
        THMIX=THMIX/DPTHMX                                                       KFPARA.1320
        QMIX=QMIX/DPTHMX                                                         KFPARA.1321
        PMIX=PMIX/DPTHMX                                                         KFPARA.1322
        ROCPQ=0.2854*(1.-0.28*QMIX)                                              KFPARA.1323
        TMIX=THMIX*(PMIX/P00)**ROCPQ                                             KFPARA.1324
        ES=ALIQ*EXP((TMIX*BLIQ-CLIQ)/(TMIX-DLIQ))                                KFPARA.1325
        QS=0.622*ES/(PMIX-ES)                                                    KFPARA.1326
C                                                                                KFPARA.1327
C...REMOVE SUPERSATURATION FOR DIAGNOSTIC PURPOSES, IF NECESSARY...              KFPARA.1328
C                                                                                KFPARA.1329
        IF(QMIX.GT.QS)THEN                                                       KFPARA.1330
          RL=XLV0-XLV1*TMIX                                                      KFPARA.1331
          CPM=CP*(1.+0.887*QMIX)                                                 KFPARA.1332
          DSSDT=QS*(CLIQ-BLIQ*DLIQ)/((TMIX-DLIQ)*(TMIX-DLIQ))                    KFPARA.1333
          DQ=(QMIX-QS)/(1.+RL*DSSDT/CPM)                                         KFPARA.1334
          TMIX=TMIX+RL/CP*DQ                                                     KFPARA.1335
          QMIX=QMIX-DQ                                                           KFPARA.1336
          ROCPQ=0.2854*(1.-0.28*QMIX)                                            KFPARA.1337
          THMIX=TMIX*(P00/PMIX)**ROCPQ                                           KFPARA.1338
          TLCL=TMIX                                                              KFPARA.1339
          PLCL=PMIX                                                              KFPARA.1340
        ELSE                                                                     KFPARA.1341
          QMIX=AMAX1(QMIX,0.)                                                    KFPARA.1342
          EMIX=QMIX*PMIX/(0.622+QMIX)                                            KFPARA.1343
          TLOG=ALOG(EMIX/ALIQ)                                                   KFPARA.1344
          TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)                                      KFPARA.1345
          TLCL=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(TMIX-T00))*(TMIX-         KFPARA.1346
     +         TDPT)                                                             KFPARA.1347
          TLCL=AMIN1(TLCL,TMIX)                                                  KFPARA.1348
          CPORQ=1./ROCPQ                                                         KFPARA.1349
          PLCL=P00*(TLCL/THMIX)**CPORQ                                           KFPARA.1350
        ENDIF                                                                    KFPARA.1351
        TVLCL=TLCL*(1.+0.608*QMIX)                                               KFPARA.1352
        DO 235 NK=LC,KL                                                          KFPARA.1353
          KLCL=NK                                                                KFPARA.1354
  235   IF(PLCL.GE.P0(NK))GOTO 240                                               KFPARA.1355
  240   K=KLCL-1                                                                 KFPARA.1356
        DLP=ALOG(PLCL/P0(K))/ALOG(P0(KLCL)/P0(K))                                KFPARA.1357
C                                                                                KFPARA.1358
C...ESTIMATE ENVIRONMENTAL TEMPERATURE AND MIXING RATIO AT THE LCL...            KFPARA.1359
C                                                                                KFPARA.1360
        TENV=TG(K)+(TG(KLCL)-TG(K))*DLP                                          KFPARA.1361
        QENV=QG(K)+(QG(KLCL)-QG(K))*DLP                                          KFPARA.1362
        TVEN=TENV*(1.+0.608*QENV)                                                KFPARA.1363
        TVBAR=0.5*(TVG(K)+TVEN)                                                  KFPARA.1364
C        ZLCL=Z0(K)+R*TVBAR*ALOG(P0(K)/PLCL)/G                                   KFPARA.1365
        ZLCL=Z0(K)+(Z0(KLCL)-Z0(K))*DLP                                          KFPARA.1366
        TVAVG=0.5*(TVEN+TG(KLCL)*(1.+0.608*QG(KLCL)))                            KFPARA.1367
        PLCL=P0(KLCL)*EXP(G/(R*TVAVG)*(Z0(KLCL)-ZLCL))                           KFPARA.1368
        THETEU(K)=TMIX*(1.E5/PMIX)**(0.2854*(1.-0.28*QMIX))*                     KFPARA.1369
     +            EXP((3374.6525/TLCL-2.5403)*QMIX*(1.+0.81*QMIX))               KFPARA.1370
        ES=ALIQ*EXP((TENV*BLIQ-CLIQ)/(TENV-DLIQ))                                KFPARA.1371
        QESE=0.622*ES/(PLCL-ES)                                                  KFPARA.1372
        THTESG(K)=TENV*(1.E5/PLCL)**(0.2854*(1.-0.28*QESE))*                     KFPARA.1373
     +            EXP((3374.6525/TENV-2.5403)*QESE*(1.+0.81*QESE))               KFPARA.1374
C                                                                                KFPARA.1375
C...COMPUTE ADJUSTED ABE(ABEG).                                                  KFPARA.1376
C                                                                                KFPARA.1377
        ABEG=0.                                                                  KFPARA.1378
        THTUDL=THETEU(K)                                                         KFPARA.1379
        DO 245 NK=K,LTOPM1                                                       KFPARA.1380
          NK1=NK+1                                                               KFPARA.1381
          ES=ALIQ*EXP((TG(NK1)*BLIQ-CLIQ)/(TG(NK1)-DLIQ))                        KFPARA.1382
          QESE=0.622*ES/(P0(NK1)-ES)                                             KFPARA.1383
          THTESG(NK1)=TG(NK1)*(1.E5/P0(NK1))**(0.2854*(1.-0.28*QESE))*           KFPARA.1384
     +                EXP((3374.6525/TG(NK1)-2.5403)*QESE*(1.+0.81*QESE)         KFPARA.1385
     +                )                                                          KFPARA.1386
C         DZZ=CVMGT(Z0(KLCL)-ZLCL,DZA(NK),NK.EQ.K)                               KFPARA.1387
          IF(NK.EQ.K)THEN                                                        KFPARA.1388
            DZZ=Z0(KLCL)-ZLCL                                                    KFPARA.1389
          ELSE                                                                   KFPARA.1390
            DZZ=DZA(NK)                                                          KFPARA.1391
          ENDIF                                                                  KFPARA.1392
          BE=((2.*THTUDL)/(THTESG(NK1)+THTESG(NK))-1.)*DZZ                       KFPARA.1393
  245   IF(BE.GT.0.)ABEG=ABEG+BE*G                                               KFPARA.1394
C                                                                                KFPARA.1395
C...ASSUME AT LEAST 90% OF CAPE (ABE) IS REMOVED BY CONVECTION DURING            KFPARA.1396
C...THE PERIOD TIMEC...                                                          KFPARA.1397
C                                                                                KFPARA.1398
        IF(NOITR.EQ.1)THEN                                                       KFPARA.1399
C        WRITE(98,1060)FABE                                                      KFPARA.1400
          GOTO 265                                                               KFPARA.1401
        ENDIF                                                                    KFPARA.1402
        DABE=AMAX1(ABE-ABEG,0.1*ABE)                                             KFPARA.1403
        FABE=ABEG/(ABE+1.E-8)                                                    KFPARA.1404
        IF(FABE.GT.1.)THEN                                                       KFPARA.1405
C       WRITE(98,*)'UPDRAFT/DOWNDRAFT COUPLET INCREASES CAPE AT THIS '           KFPARA.1406
C    *,'GRID POINT; NO CONVECTION ALLOWED!'                                      KFPARA.1407
          GOTO 325                                                               KFPARA.1408
        ENDIF                                                                    KFPARA.1409
        IF(NCOUNT.NE.1)THEN                                                      KFPARA.1410
          DFDA=(FABE-FABEOLD)/(AINC-AINCOLD)                                     KFPARA.1411
          IF(DFDA.GT.0.)THEN                                                     KFPARA.1412
            NOITR=1                                                              KFPARA.1413
            AINC=AINCOLD                                                         KFPARA.1414
            GOTO 255                                                             KFPARA.1415
          ENDIF                                                                  KFPARA.1416
        ENDIF                                                                    KFPARA.1417
        AINCOLD=AINC                                                             KFPARA.1418
        FABEOLD=FABE                                                             KFPARA.1419
        IF(AINC/AINCMX.GT.0.999.AND.FABE.GT.1.05-STAB)THEN                       KFPARA.1420
C      WRITE(98,1055)FABE                                                        KFPARA.1421
          GOTO 265                                                               KFPARA.1422
        ENDIF                                                                    KFPARA.1423
        IF(FABE.LE.1.05-STAB.AND.FABE.GE.0.95-STAB)GOTO 265                      KFPARA.1424
        IF(NCOUNT.GT.10)THEN                                                     KFPARA.1425
C       WRITE(98,1060)FABE                                                       KFPARA.1426
          GOTO 265                                                               KFPARA.1427
        ENDIF                                                                    KFPARA.1428
C                                                                                KFPARA.1429
C...IF MORE THAN 10% OF THE ORIGINAL CAPE REMAINS, INCREASE THE                  KFPARA.1430
C...CONVECTIVE MASS FLUX BY THE FACTOR AINC:                                     KFPARA.1431
C                                                                                KFPARA.1432
        IF(FABE.EQ.0.)THEN                                                       KFPARA.1433
          AINC=AINC*0.5                                                          KFPARA.1434
        ELSE                                                                     KFPARA.1435
          AINC=AINC*STAB*ABE/(DABE+1.E-8)                                        KFPARA.1436
        ENDIF                                                                    KFPARA.1437
  255   AINC=AMIN1(AINCMX,AINC)                                                  KFPARA.1438
C...IF AINC BECOMES VERY SMALL, EFFECTS OF CONVECTION                            KFPARA.1439
C...WILL BE MINIMAL SO JUST IGNORE IT...                                         KFPARA.1440
        IF(AINC.LT.0.05)GOTO 325                                                 KFPARA.1441
C       AINC=AMAX1(AINC,0.05)                                                    KFPARA.1442
        TDER=TDER2*AINC                                                          KFPARA.1443
        PPTFLX=PPTFL2*AINC                                                       KFPARA.1444
C     WRITE(98,1080)LFS,LDB,LDT,TIMEC,NSTEP,NCOUNT,FABEOLD,AINCOLD               KFPARA.1445
        DO 260 NK=1,LTOP                                                         KFPARA.1446
          UMF(NK)=UMF2(NK)*AINC                                                  KFPARA.1447
          DMF(NK)=DMF2(NK)*AINC                                                  KFPARA.1448
          DETLQ(NK)=DETLQ2(NK)*AINC                                              KFPARA.1449
          DETIC(NK)=DETIC2(NK)*AINC                                              KFPARA.1450
          UDR(NK)=UDR2(NK)*AINC                                                  KFPARA.1451
          UER(NK)=UER2(NK)*AINC                                                  KFPARA.1452
          DER(NK)=DER2(NK)*AINC                                                  KFPARA.1453
          DDR(NK)=DDR2(NK)*AINC                                                  KFPARA.1454
  260   CONTINUE                                                                 KFPARA.1455
C                                                                                KFPARA.1456
C...GO BACK UP FOR ANOTHER ITERATION...                                          KFPARA.1457
C                                                                                KFPARA.1458
        GOTO 175                                                                 KFPARA.1459
  265   CONTINUE                                                                 KFPARA.1460
C                                                                                KFPARA.1461
C...CLEAN THINGS UP, CALCULATE CONVECTIVE FEEDBACK TENDENCIES FOR THIS           KFPARA.1462
C...GRID POINT...                                                                KFPARA.1463
C                                                                                KFPARA.1464
C...COMPUTE HYDROMETEOR TENDENCIES AS IS DONE FOR T, QV...                       KFPARA.1465
C                                                                                KFPARA.1466
C...FRC2 IS THE FRACTION OF TOTAL CONDENSATE                                     KFPARA.1467
C...GENERATED THAT GOES INTO PRECIPITIATION                                      KFPARA.1468
        FRC2=PPTFLX/(CPR*AINC)                                                   KFPARA.1469
        DO 270 NK=1,LTOP                                                         KFPARA.1470
          QLPA(NK)=QL0(NK)                                                       KFPARA.1471
          QIPA(NK)=QI0(NK)                                                       KFPARA.1472
          QRPA(NK)=QR0(NK)                                                       KFPARA.1473
          QSPA(NK)=QS0(NK)                                                       KFPARA.1474
          RAINFB(NK)=PPTLIQ(NK)*AINC*FBFRC*FRC2                                  KFPARA.1475
          SNOWFB(NK)=PPTICE(NK)*AINC*FBFRC*FRC2                                  KFPARA.1476
  270   CONTINUE                                                                 KFPARA.1477
        DO 290 NTC=1,NSTEP                                                       KFPARA.1478
C                                                                                KFPARA.1479
C...ASSIGN HYDROMETEORS CONCENTRATIONS AT THE TOP AND BOTTOM OF EACH             KFPARA.1480
C...LAYER BASED ON THE SIGN OF OMEGA...                                          KFPARA.1481
C                                                                                KFPARA.1482
          DO 275 NK=1,LTOP                                                       KFPARA.1483
            QLFXIN(NK)=0.                                                        KFPARA.1484
            QLFXOUT(NK)=0.                                                       KFPARA.1485
            QIFXIN(NK)=0.                                                        KFPARA.1486
            QIFXOUT(NK)=0.                                                       KFPARA.1487
            QRFXIN(NK)=0.                                                        KFPARA.1488
            QRFXOUT(NK)=0.                                                       KFPARA.1489
            QSFXIN(NK)=0.                                                        KFPARA.1490
            QSFXOUT(NK)=0.                                                       KFPARA.1491
  275     CONTINUE                                                               KFPARA.1492
          DO 280 NK=2,LTOP                                                       KFPARA.1493
            IF(OMG(NK).LE.0.)THEN                                                KFPARA.1494
              QLFXIN(NK)=-FXM(NK)*QLPA(NK-1)                                     KFPARA.1495
              QIFXIN(NK)=-FXM(NK)*QIPA(NK-1)                                     KFPARA.1496
              QRFXIN(NK)=-FXM(NK)*QRPA(NK-1)                                     KFPARA.1497
              QSFXIN(NK)=-FXM(NK)*QSPA(NK-1)                                     KFPARA.1498
              QLFXOUT(NK-1)=QLFXOUT(NK-1)+QLFXIN(NK)                             KFPARA.1499
              QIFXOUT(NK-1)=QIFXOUT(NK-1)+QIFXIN(NK)                             KFPARA.1500
              QRFXOUT(NK-1)=QRFXOUT(NK-1)+QRFXIN(NK)                             KFPARA.1501
              QSFXOUT(NK-1)=QSFXOUT(NK-1)+QSFXIN(NK)                             KFPARA.1502
            ELSE                                                                 KFPARA.1503
              QLFXOUT(NK)=FXM(NK)*QLPA(NK)                                       KFPARA.1504
              QIFXOUT(NK)=FXM(NK)*QIPA(NK)                                       KFPARA.1505
              QRFXOUT(NK)=FXM(NK)*QRPA(NK)                                       KFPARA.1506
              QSFXOUT(NK)=FXM(NK)*QSPA(NK)                                       KFPARA.1507
              QLFXIN(NK-1)=QLFXIN(NK-1)+QLFXOUT(NK)                              KFPARA.1508
              QIFXIN(NK-1)=QIFXIN(NK-1)+QIFXOUT(NK)                              KFPARA.1509
              QRFXIN(NK-1)=QRFXIN(NK-1)+QRFXOUT(NK)                              KFPARA.1510
              QSFXIN(NK-1)=QSFXIN(NK-1)+QSFXOUT(NK)                              KFPARA.1511
            ENDIF                                                                KFPARA.1512
  280     CONTINUE                                                               KFPARA.1513
C                                                                                KFPARA.1514
C...UPDATE THE HYDROMETEOR CONCENTRATION VALUES AT EACH LEVEL...                 KFPARA.1515
C                                                                                KFPARA.1516
          DO 285 NK=1,LTOP                                                       KFPARA.1517
            QLPA(NK)=QLPA(NK)+(QLFXIN(NK)+DETLQ(NK)-QLFXOUT(NK))*DTIME*          KFPARA.1518
     +               EMSD(NK)                                                    KFPARA.1519
            QIPA(NK)=QIPA(NK)+(QIFXIN(NK)+DETIC(NK)-QIFXOUT(NK))*DTIME*          KFPARA.1520
     +               EMSD(NK)                                                    KFPARA.1521
            QRPA(NK)=QRPA(NK)+(QRFXIN(NK)+QLQOUT(NK)*UDR(NK)-QRFXOUT(NK)         KFPARA.1522
     +               +RAINFB(NK))*DTIME*EMSD(NK)                                 KFPARA.1523
            QSPA(NK)=QSPA(NK)+(QSFXIN(NK)+QICOUT(NK)*UDR(NK)-QSFXOUT(NK)         KFPARA.1524
     +               +SNOWFB(NK))*DTIME*EMSD(NK)                                 KFPARA.1525
  285     CONTINUE                                                               KFPARA.1526
  290   CONTINUE                                                                 KFPARA.1527
        DO 295 NK=1,LTOP                                                         KFPARA.1528
          QLG(NK)=QLPA(NK)                                                       KFPARA.1529
          QIG(NK)=QIPA(NK)                                                       KFPARA.1530
          QRG(NK)=QRPA(NK)                                                       KFPARA.1531
          QSG(NK)=QSPA(NK)                                                       KFPARA.1532
  295   CONTINUE                                                                 KFPARA.1533
C     WRITE(98,1080)LFS,LDB,LDT,TIMEC,NSTEP,NCOUNT,FABE,AINC                     KFPARA.1534
C                                                                                KFPARA.1535
C...SEND FINAL PARAMETERIZED VALUES TO OUTPUT FILES...                           KFPARA.1536
C                                                                                KFPARA.1537
        IF(ISTOP.EQ.1)THEN                                                       KFPARA.1538
        WRITE(6,1070)'  P  ','   DP ',' DT K/D ',' DR K/D ','   OMG  ',          KFPARA.1539
     *' DOMGDP ','   UMF  ','   UER  ','   UDR  ','   DMF  ','   DER  '          KFPARA.1540
     *,'   DDR  ','   EMS  ','    W0  ','  DETLQ ',' DETIC '                     KFPARA.1541
          DO 300 NK=1,LTOP                                                       KFPARA.1542
            K=LTOP-NK+1                                                          KFPARA.1543
            DTT=(TG(K)-T0(K))*86400./TIMEC                                       KFPARA.1544
            RL=XLV0-XLV1*TG(K)                                                   KFPARA.1545
            DR=-(QG(K)-Q0(K))*RL*86400./(TIMEC*CP)                               KFPARA.1546
            UDFRC=UDR(K)*TIMEC*EMSD(K)                                           KFPARA.1547
            UEFRC=UER(K)*TIMEC*EMSD(K)                                           KFPARA.1548
            DDFRC=DDR(K)*TIMEC*EMSD(K)                                           KFPARA.1549
            DEFRC=-DER(K)*TIMEC*EMSD(K)                                          KFPARA.1550
            WRITE (6,1075)P0(K)/100.,DP(K)/100.,DTT,DR,OMG(K),DOMGDP(K)*         KFPARA.1551
     +                    1.E4,UMF(K)/1.E6,UEFRC,UDFRC,DMF(K)/1.E6,DEFRC         KFPARA.1552
     +                    ,DDFRC,EMS(K)/1.E11,W0AVG(I,J,K)*1.E2,DETLQ(K)         KFPARA.1553
     +                    *TIMEC*EMSD(K)*1.E3,DETIC(K)*TIMEC*EMSD(K)*            KFPARA.1554
     +                    1.E3                                                   KFPARA.1555
  300     CONTINUE                                                               KFPARA.1556
        WRITE(6,1085)'K','P','Z','T0','TG','DT','TU','TD','Q0','QG',             KFPARA.1557
     *            'DQ','QU','QD','QLG','QIG','QRG','QSG','RH0','RHG'             KFPARA.1558
          DO 305 NK=1,KX                                                         KFPARA.1559
            K=KX-NK+1                                                            KFPARA.1560
            DTT=TG(K)-T0(K)                                                      KFPARA.1561
            TUC=TU(K)-T00                                                        KFPARA.1562
            IF(K.LT.LC.OR.K.GT.LTOP)TUC=0.                                       KFPARA.1563
            TDC=TZ(K)-T00                                                        KFPARA.1564
            IF((K.LT.LDB.OR.K.GT.LDT).AND.K.NE.LFS)TDC=0.                        KFPARA.1565
            ES=ALIQ*EXP((BLIQ*TG(K)-CLIQ)/(TG(K)-DLIQ))                          KFPARA.1566
            QGS=ES*0.622/(P0(K)-ES)                                              KFPARA.1567
            RH0=Q0(K)/QES(K)                                                     KFPARA.1568
            RHG=QG(K)/QGS                                                        KFPARA.1569
            WRITE (6,1090)K,P0(K)/100.,Z0(K),T0(K)-T00,TG(K)-T00,DTT,TUC         KFPARA.1570
     +                    ,TDC,Q0(K)*1000.,QG(K)*1000.,(QG(K)-Q0(K))*            KFPARA.1571
     +                    1000.,QU(K)*1000.,QD(K)*1000.,QLG(K)*1000.,            KFPARA.1572
     +                    QIG(K)*1000.,QRG(K)*1000.,QSG(K)*1000.,RH0,RHG         KFPARA.1573
  305     CONTINUE                                                               KFPARA.1574
C                                                                                KFPARA.1575
C...IF CALCULATIONS ABOVE SHOW AN ERROR IN THE MASS BUDGET, PRINT OUT A          KFPARA.1576
C...TO BE USED LATER FOR DIAGNOSTIC PURPOSES, THEN ABORT RUN...                  KFPARA.1577
C                                                                                KFPARA.1578
          IF(ISTOP.EQ.1)THEN                                                     KFPARA.1579
            DO 310 K=1,KX                                                        KFPARA.1580
              WRITE (6,1115)Z0(K),P0(K)/100.,T0(K)-273.16,Q0(K)*1000.,           KFPARA.1581
     +                      U0(K),V0(K),DP(K)/100.,W0AVG(I,J,K)                  KFPARA.1582
  310       CONTINUE                                                             KFPARA.1583
            STOP 'KAIN-FRITSCH'                                                  KFPARA.1584
          ENDIF                                                                  KFPARA.1585
        ENDIF                                                                    KFPARA.1586
        CNDTNF=(1.-EQFRC(LFS))*(QLIQ(LFS)+QICE(LFS))*DMF(LFS)                    KFPARA.1587
C     WRITE(98,1095)CPR*AINC,TDER+PPTFLX+CNDTNF                                  KFPARA.1588
C                                                                                KFPARA.1589
C  EVALUATE MOISTURE BUDGET...                                                   KFPARA.1590
C                                                                                KFPARA.1591
        QINIT=0.                                                                 KFPARA.1592
        QFNL=0.                                                                  KFPARA.1593
        DPT=0.                                                                   KFPARA.1594
        DO 315 NK=1,LTOP                                                         KFPARA.1595
          DPT=DPT+DP(NK)                                                         KFPARA.1596
          QINIT=QINIT+Q0(NK)*EMS(NK)                                             KFPARA.1597
          QFNL=QFNL+QG(NK)*EMS(NK)                                               KFPARA.1598
          QFNL=QFNL+(QLG(NK)+QIG(NK)+QRG(NK)+QSG(NK))*EMS(NK)                    KFPARA.1599
  315   CONTINUE                                                                 KFPARA.1600
        QFNL=QFNL+PPTFLX*TIMEC*(1.-FBFRC)                                        KFPARA.1601
        ERR2=(QFNL-QINIT)*100./QINIT                                             KFPARA.1602
C     WRITE(98,1110)QINIT,QFNL,ERR2                                              KFPARA.1603
        IF(ABS(ERR2).GT.0.05)STOP 'QVERR'                                        KFPARA.1604
        RELERR=ERR2*QINIT/(PPTFLX*TIMEC+1.E-10)                                  KFPARA.1605
C     WRITE(98,1120)RELERR                                                       KFPARA.1606
C     WRITE(98,*)'TDER, CPR, USR, TRPPT =',                                      KFPARA.1607
C    *TDER,CPR*AINC,USR*AINC,TRPPT*AINC                                          KFPARA.1608
C                                                                                KFPARA.1609
C...FEEDBACK TO RESOLVABLE SCALE TENDENCIES.                                     KFPARA.1610
C                                                                                KFPARA.1611
C...IF THE ADVECTIVE TIME PERIOD (TADVEC) IS LESS THAN SPECIFIED MINIMUM         KFPARA.1612
C...TIMEC, ALLOW FEEDBACK TO OCCUR ONLY DURING TADVEC...                         KFPARA.1613
C                                                                                KFPARA.1614
        IF(TADVEC.LT.TIMEC)NIC=NINT(TADVEC/(0.5*DT2))                            KFPARA.1615
        NCA(I,J)=NIC                                                             KFPARA.1616
        DO 320 K=1,KX                                                            KFPARA.1617
          NK=KX-K+1                                                              KFPARA.1618
          IF(IMOIST(INEST).NE.2)THEN                                             KFPARA.1619
C                                                                                KFPARA.1620
C...IF HYDROMETEORS ARE NOT ALLOWED, THEY MUST BE EVAPORATED OR SUBLIMAT         KFPARA.1621
C...AND FED BACK AS VAPOR, ALONG WITH ASSOCIATED CHANGES IN TEMPERATURE.         KFPARA.1622
C...NOTE:  THIS WILL INTRODUCE CHANGES IN THE CONVECTIVE TEMPERATURE AND         KFPARA.1623
C...WATER VAPOR FEEDBACK TENDENCIES AND MAY LEAD TO SUPERSATURATED VALUE         KFPARA.1624
C...OF QG...                                                                     KFPARA.1625
C                                                                                KFPARA.1626
            RLC=XLV0-XLV1*TG(K)                                                  KFPARA.1627
            RLS=XLS0-XLS1*TG(K)                                                  KFPARA.1628
            CPM=CP*(1.+0.887*QG(K))                                              KFPARA.1629
            TG(K)=TG(K)-(RLC*(QLG(K)+QRG(K))+RLS*(QIG(K)+QSG(K)))/CPM            KFPARA.1630
            QG(K)=QG(K)+(QLG(K)+QRG(K)+QIG(K)+QSG(K))                            KFPARA.1631
            DQLDT(I,J,NK)=0.                                                     KFPARA.1632
            DQIDT(I,J,NK)=0.                                                     KFPARA.1633
            DQRDT(I,J,NK)=0.                                                     KFPARA.1634
            DQSDT(I,J,NK)=0.                                                     KFPARA.1635
          ELSE                                                                   KFPARA.1636
C        IF(IEXICE.NE.1 .AND. IICE.NE.1) THEN                                    KFPARA.1637
            IF(IMPHYS(INEST).EQ.3)THEN                                           KFPARA.1638
C                                                                                KFPARA.1639
C...IF ICE PHASE IS NOT ALLOWED, MELT ALL FROZEN HYDROMETEORS...                 KFPARA.1640
C                                                                                KFPARA.1641
              CPM=CP*(1.+0.887*QG(K))                                            KFPARA.1642
              TG(K)=TG(K)-(QIG(K)+QSG(K))*RLF/CPM                                KFPARA.1643
              DQLDT(I,J,NK)=(QLG(K)+QIG(K)-QL0(K)-QI0(K))/TIMEC                  KFPARA.1644
              DQIDT(I,J,NK)=0.                                                   KFPARA.1645
              DQRDT(I,J,NK)=(QRG(K)+QSG(K)-QR0(K)-QS0(K))/TIMEC                  KFPARA.1646
              DQSDT(I,J,NK)=0.                                                   KFPARA.1647
C        ELSEIF(IEXICE.EQ.1 .AND. IICE.EQ.0)THEN                                 KFPARA.1648
            ELSEIF(IMPHYS(INEST).EQ.4)THEN                                       KFPARA.1649
C                                                                                KFPARA.1650
C...IF ICE PHASE IS ALLOWED, BUT MIXED PHASE IS NOT, MELT FROZEN HYDROME         KFPARA.1651
C...BELOW THE MELTING LEVEL, FREEZE LIQUID WATER ABOVE THE MELTING LEVEL         KFPARA.1652
C                                                                                KFPARA.1653
              CPM=CP*(1.+0.887*QG(K))                                            KFPARA.1654
              IF(K.LE.ML)THEN                                                    KFPARA.1655
                TG(K)=TG(K)-(QIG(K)+QSG(K))*RLF/CPM                              KFPARA.1656
              ELSEIF(K.GT.ML)THEN                                                KFPARA.1657
                TG(K)=TG(K)+(QLG(K)+QRG(K))*RLF/CPM                              KFPARA.1658
              ENDIF                                                              KFPARA.1659
              DQLDT(I,J,NK)=(QLG(K)+QIG(K)-QL0(K)-QI0(K))/TIMEC                  KFPARA.1660
              DQIDT(I,J,NK)=0.                                                   KFPARA.1661
              DQRDT(I,J,NK)=(QRG(K)+QSG(K)-QR0(K)-QS0(K))/TIMEC                  KFPARA.1662
              DQSDT(I,J,NK)=0.                                                   KFPARA.1663
C        ELSEIF(IICE.EQ.1 .AND. IEXICE.EQ.0)THEN                                 KFPARA.1664
            ELSEIF(IMPHYS(INEST).GE.5)THEN                                       KFPARA.1665
C                                                                                KFPARA.1666
C...IF MIXED PHASE HYDROMETEORS ARE ALLOWED, FEED BACK CONVECTIVE                KFPARA.1667
C...TENDENCY OF HYDROMETEORS DIRECTLY...                                         KFPARA.1668
C                                                                                KFPARA.1669
              DQLDT(I,J,NK)=(QLG(K)-QL0(K))/TIMEC                                KFPARA.1670
              DQIDT(I,J,NK)=(QIG(K)-QI0(K))/TIMEC                                KFPARA.1671
              DQRDT(I,J,NK)=(QRG(K)-QR0(K))/TIMEC                                KFPARA.1672
              DQSDT(I,J,NK)=(QSG(K)-QS0(K))/TIMEC                                KFPARA.1673
            ELSE                                                                 KFPARA.1674
         PRINT *,'THIS COMBINATION OF IMOIST, IEXICE, IICE NOT ALLOWED!'         KFPARA.1675
              STOP 'KAIN-FRITSCH'                                                KFPARA.1676
            ENDIF                                                                KFPARA.1677
          ENDIF                                                                  KFPARA.1678
          DTDT(I,J,NK)=(TG(K)-T0(K))/TIMEC                                       KFPARA.1679
          DQDT(I,J,NK)=(QG(K)-Q0(K))/TIMEC                                       KFPARA.1680
  320   CONTINUE                                                                 KFPARA.1681
        RAINCV(I,J)=.1*.5*DT2*PPTFLX*(1.-FBFRC)/DXSQ                             KFPARA.1682
C         RNC=0.1*TIMEC*PPTFLX/DXSQ                                              KFPARA.1683
        RNC=RAINCV(I,J)*NIC                                                      KFPARA.1684
C        WRITE(98,909)RNC                                                        KFPARA.1685
 909     FORMAT(' CONVECTIVE RAINFALL =',F8.4,' CM')                             KFPARA.1686
  325 CONTINUE                                                                   KFPARA.1687
1000  FORMAT(' ',10A8)                                                           KFPARA.1688
1005  FORMAT(' ',F6.0,2X,F6.4,2X,F7.3,1X,F6.4,2X,4(F6.3,2X),2(F7.3,1X))          KFPARA.1689
1010  FORMAT(' ',' VERTICAL VELOCITY IS NEGATIVE AT ',F4.0,' MB')                KFPARA.1690
1015   FORMAT(' ','ALL REMAINING MASS DETRAINS BELOW ',F4.0,' MB')               KFPARA.1691
1025   FORMAT(5X,' KLCL=',I2,' ZLCL=',F7.1,'M',                                  KFPARA.1692
     *  ' DTLCL=',F5.2,' LTOP=',I2,' P0(LTOP)=',-2PF5.1,'MB FRZ LV=',            KFPARA.1693
     *  I2,' TMIX=',0PF4.1,1X,'PMIX=',-2PF6.1,' QMIX=',3PF5.1,                   KFPARA.1694
     *  ' CAPE=',0PF7.1)                                                         KFPARA.1695
1030   FORMAT(' ',' P0(LET) = ',F6.1,' P0(LTOP) = ',F6.1,' VMFLCL =',            KFPARA.1696
     *E12.3,' PLCL =',F6.1,' WLCL =',F6.3,' CLDHGT =',                           KFPARA.1697
     *F8.1)                                                                      KFPARA.1698
1035  FORMAT(1X,'PEF(WS)=',F4.2,'(CB)=',F4.2,'LC,LET=',2I3,'WKL='                KFPARA.1699
     *,F6.3,'VWS=',F5.2)                                                         KFPARA.1700
1040          FORMAT(' ','PRECIP EFF = 100%, ENVIR CANNOT SUPPORT DOWND'         KFPARA.1701
     *,'RAFTS!')                                                                 KFPARA.1702
C1045  FORMAT('NUMBER OF DOWNDRAFT ITERATIONS EXCEEDS 10...PPTFLX'               KFPARA.1703
C     *' IS DIFFERENT FROM THAT GIVEN BY PRECIP EFF RELATION!')                  KFPARA.1704
C FLIC HAS TROUBLE WITH THIS ONE.                                                KFPARA.1705
1045  FORMAT('NUMBER OF DOWNDRAFT ITERATIONS EXCEEDS 10')                        KFPARA.1706
1050  FORMAT(' ','LCOUNT= ',I3,' PPTFLX/CPR, PEFF= ',F5.3,1X,F5.3,               KFPARA.1707
     *'DMF(LFS)/UMF(LCL)= ',F5.3)                                                KFPARA.1708
1055     FORMAT(/'*** DEGREE OF STABILIZATION =',F5.3,', NO MORE MASS F'         KFPARA.1709
     *,'LUX IS ALLOWED!')                                                        KFPARA.1710
C1060     FORMAT(/' ITERATION DOES NOT CONVERGE TO GIVE THE SPECIFIED '          KFPARA.1711
C     *'DEGREE OF STABILIZATION!  FABE= ',F6.4)                                  KFPARA.1712
1060  FORMAT(/' ITERATION DOES NOT CONVERGE.  FABE= ',F6.4)                      KFPARA.1713
 1070 FORMAT (16A8)                                                              KFPARA.1714
 1075 FORMAT (F8.2,3(F8.2),2(F8.3),F8.2,2F8.3,F8.2,6F8.3)                        KFPARA.1715
1080   FORMAT(2X,'LFS,LDB,LDT =',3I3,' TIMEC, NSTEP=',F5.0,I3,                   KFPARA.1716
     *'NCOUNT, FABE, AINC=',I2,1X,F5.3,F6.2)                                     KFPARA.1717
 1085 FORMAT (A3,16A7,2A8)                                                       KFPARA.1718
 1090 FORMAT (I3,F7.2,F7.0,10F7.2,4F7.3,2F8.3)                                   KFPARA.1719
1095   FORMAT(' ','  PPT PRODUCTION RATE= ',F10.0,' TOTAL EVAP+PPT= ',           KFPARA.1720
     * F10.0)                                                                    KFPARA.1721
1105   FORMAT(' ','NET LATENT HEAT RELEASE =',E12.5,' ACTUAL HEATING =',         KFPARA.1722
     * E12.5,' J/KG-S, DIFFERENCE = ',F9.3,'PERCENT')                            KFPARA.1723
1110   FORMAT(' ','INITIAL WATER =',E12.5,' FINAL WATER =',E12.5,                KFPARA.1724
     * ' TOTAL WATER CHANGE =',F8.2,'PERCENT')                                   KFPARA.1725
 1115 FORMAT (2X,F6.0,2X,F7.2,2X,F5.1,2X,F6.3,2(2X,F5.1),2X,F7.2,2X,F7.4         KFPARA.1726
     +       )                                                                   KFPARA.1727
1120   FORMAT(' ','MOISTURE ERROR AS FUNCTION OF TOTAL PPT =',F9.3,              KFPARA.1728
     +      'PERCENT')                                                           KFPARA.1729
      RETURN                                                                     KFPARA.1730
      END                                                                        KFPARA.1731
                                                                                 KFPARA.1732
