      SUBROUTINE BMPARA(T3D,QV3D,PSB,PP3D,TMOD,QMOD,RAINC,ICOND,ICONSS,          BMPARA.1
     &                  J,PTOP,A,DSIGMA,SIGMA,INEST,IBEG,IEND,TBASE,             BMPARA.2
     &                  CLDEFI,XLAND)                                            BMPARA.3
C                                                                                BMPARA.4
C    *** MODIFIED BY JACK KAIN OF PENN STATE TO REPLACE THE LOOK-UP TABL         BMPARA.5
C        BY CALCULATIONS.                                                        BMPARA.6
C                                                                                BMPARA.7
C     ******************************************************************         BMPARA.8
C     *                                                                *         BMPARA.9
C     *  CONVECTIVE ADJUSTMENT FOR DEEP OR SHALLOW CONVECTION          *         BMPARA.10
C     *                                                                *         BMPARA.11
C     *  REFERENCES:                                                   *         BMPARA.12
C     *                                                                *         BMPARA.13
C     *  BETTS, A.K., 1986:  A NEW CONVECTIVE ADJUSTMENT SCHEME.       *         BMPARA.14
C     *    PART I: OBSERVATIONAL AND THEORETICAL BASIS.  QUART. J. R.  *         BMPARA.15
C     *    MET. SOC., 112, 677-691.                                    *         BMPARA.16
C     *                                                                *         BMPARA.17
C     *  BETTS, A.K., AND M.J. MILLER, 1986:  A NEW CONVECTIVE         *         BMPARA.18
C     *    ADJUSTMENT SCHEME.  PART II: SINGLE COLUMN TESTS USING      *         BMPARA.19
C     *    GATE WAVE, BOMEX, ATEX AND ARCTIC AIR MASS DATA SETS.       *         BMPARA.20
C     *    QUART. J. R. MET. SOC., 112, 693-709.                       *         BMPARA.21
C     *                                                                *         BMPARA.22
C     *                                                                *         BMPARA.23
C     *  N.B.  PART OF THE CODE IS SCALAR.  IN GLOBAL MODELS           *         BMPARA.24
C     *  CONVECTION OCCURS IN LESS THAN 30/100 POINTS.  WITH           *         BMPARA.25
C     *  SIMULATANEOUS VECTOR PROCESSING FOR BOTH DEEP AND SHALLOW     *         BMPARA.26
C     *  CONVECTION, THERE WOULD BE A LOT OF REDUNDANT VECTOR          *         BMPARA.27
C     *  COMPUTATIONS.  IF VECTOR PROCESSING IS 10 TIMES FASTER        *         BMPARA.28
C     *  THAN SCALAR, ONE MIGHT HOPE THAT THE CPU TIME WILL BE ABOUT   *         BMPARA.29
C     *  THE SAME FOR BOTH SCALAR AND VECTOR CODE.                     *         BMPARA.30
C     *                                                                *         BMPARA.31
C     ******************************************************************         BMPARA.32
C *** WARNING: THIS SUBROUTINE WILL NOT WORK IF KL.LT.12;                        BMPARA.33
C                                                                                BMPARA.34
#     include <parame.incl>                                                      BMPARA.35
#     include <rpstar.incl>                                                      BMPARA.36
#     include <parbmcu.incl>                                                     BMPARA.37
                                                                                 BMPARA.38
      COMMON/VAPPRS/ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE,XLS0,XLS1            BMPARA.39
      LOGICAL UNIS,UNIL,OCT90                                                    BMPARA.40
      REAL LBLEV                                                                 BMPARA.41
      PARAMETER                                                                  BMPARA.42
     & (H1=1.E0,H1D5=1.5E0,H2D5=2.5E0,H3000=3000.E0,H10E5=100000.E0              BMPARA.43
     &, D00=0.E0,D125=.125E0,D50=.5E0,D608=.608E0                                BMPARA.44
     &, G=9.81E0,CP=1004.E0,R=287.04,DM2859=-R/CP                                BMPARA.45
     &, ELWV=2.50E6,ELIVW=2.72E6,ROW=1.E3,EPSQ=2.E-12                            BMPARA.46
     &, A2=17.2693882E0,A3=273.16E0,A4=35.86E0                                   BMPARA.47
     &, T0=273.16E0,T1=274.16E0,PQ0=379.90516E0,STRESH=1.10E0                    BMPARA.48
     &, STABS=1.0E0,STABD=.90E0,STABFC=1.00E0,DTTOP=1.0E0,LIST=3                 BMPARA.49
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.50
     &, RHF=0.20,PMN=6500.,EPSUP=1.00,EPSDN=1.05,EPSTH=6.0  !RH DROP 10%         BMPARA.51
C     &, RHF=0.20,PMN=6500.,EPSUP=1.00,EPSDN=1.05,EPSTH=6.0                      BMPARA.52
     &, PBM=30000.,PQM=20000.,PNO=9000.,PONE=2500.,PSH=29000.                    BMPARA.53
     &, PFRZ=15000.,PSHU=45000.,ZNO=750.,ZSH=3999.,LBLEV=.98                     BMPARA.54
C    &, RHF=0.20,EPSUP=1.00,EPSDN=1.05,EPSTH=0.3                                 BMPARA.55
C    &, RHF=0.20,EPSUP=0.93,EPSDN=1.00,EPSTH=0.3                                 BMPARA.56
C    &, RHF=0.20,EPSUP=1.00,EPSDN=1.00,EPSTH=0.3                                 BMPARA.57
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.58
C                                                                                BMPARA.59
C    AUGUST `91: SCHEME HAVING THE OPTION OF USING DIFFERENT FAST AND            BMPARA.60
C    SLOW PROFILES FOR SEA AND FOR LAND POINTS; AND ALSO THE "SEA" AND           BMPARA.61
C    THE "LAND" SCHEME EVERYWHERE.  OVER LAND PROFILES DEPART FROM THE           BMPARA.62
C    FAST (DRY) PROFILES ONLY FOR PRECIPITATION/TIME STEP .GT.                   BMPARA.63
C    A PRESCRIBED VALUE (CURRENTLY, IN THE VERSION #3 DONE WEDNESDAY             BMPARA.64
C    18 SEPTEMBER, 1/4 INCH/24 H).  USE OF VARIOUS SWITCHES AS FOLLOWS.          BMPARA.65
C                                                                                BMPARA.66
C       THE "OLD" ("HARD", =ZAVISA OCT.1990) LAND SCHEME WITH FIXED              BMPARA.67
C       LAND PROFILES IS RUN BY                                                  BMPARA.68
C       * SETTING OCT90=.TRUE. IN THE FIRST EXECUTABLE LINE FOLLOWING            BMPARA.69
C            THESE COMMENTS (THIS REACTIVATES EFI=H1 OVER LAND IF                BMPARA.70
C            .NOT.UNIS, AND CLDEFI(K)=EFIMN AS THE LEFTOVER CLDEFI               BMPARA.71
C            VALUE AT SWAP POINTS);                                              BMPARA.72
C       * DEFINING FAST LAND PROFILES SAME AS FAST SEA PROFILES (OR BY           BMPARA.73
C            CHOOSING ANOTHER SET OF LAND PROFILES ZAVISA USED AT                BMPARA.74
C            EARLIER TIME);                                                      BMPARA.75
C       * SETTING FSL=1.; AND                                                    BMPARA.76
C       * DEFINING STEFI (STARTING EFI) EQUAL TO AVGEFI.                         BMPARA.77
C            (THE LAST THREE POINTS ARE HANDLED BY SWITCHING AROUND THE          BMPARA.78
C            "CFM" COMMENTS AT TWO PLACES)                                       BMPARA.79
C                                                                                BMPARA.80
C       THE "OLD,OLD" (APPR. ORIGINAL BETTS) SCHEME IS RUN BY                    BMPARA.81
C       * SPECIFYING UNIL=.TRUE.;                                                BMPARA.82
C       * SETTING FSL=1.;                                                        BMPARA.83
C       * SETTING OCT90=.TRUE.                                                   BMPARA.84
C            (WITH THESE SETTINGS FAST LAND PROFILES ONLY ARE USED).             BMPARA.85
C                                                                     FM         BMPARA.86
     &, UNIS=.FALSE.,UNIL=.TRUE.,OCT90=.TRUE.                                    BMPARA.87
     &,   FSS=.60E0,EFIMN=.20E0,EFMNT=.70E0,FCC=.50,FCP=H1-FCC                   BMPARA.88
C                                                                                BMPARA.89
C         IN THIS VERSION 3.5, OVER LAND AND FOR THE FAST PROFILES, DSPB         BMPARA.90
C         IS PRESCRIBED TO BE 25 PERCENT DRIER THAN THE FAST SEA VALUE           BMPARA.91
C         (IN ROUGH AGREEMENT WITH BINDER, QJ, IN PRESS) WHILE DSP0 AND          BMPARA.92
C         DSPT ARE EACH 20 PERCENT DRIER THAN THE CORRESPONDING FAST             BMPARA.93
C         SEA VALUES.                WITH FSL=.875 THIS MAKES THE                BMPARA.94
C         AVERAGE OF THE FAST AND THE SLOW LAND PROFILES SOMEWHAT DRIER          BMPARA.95
C         THAN THE OCT90 FIXED LAND PROFILES.                         FM         BMPARA.96
C                                                                                BMPARA.97
C    &, DSPBFL=-4843.75E0,DSP0FL=-7050.00E0,DSPTFL=-2250.0E0,FSL=.900E0          BMPARA.98
C***   ACTIVATE THE FOLLOWING LINE IF OCT90=.TRUE. (AND COMMENT OUT THE          BMPARA.99
C***   PRECEDING LINE):                                                          BMPARA.100
     &, DSPBFL=-3875.E0,DSP0FL=-5875.E0,DSPTFL=-1875.E0,FSL=1.0E0                BMPARA.101
     &, DSPBFS=-3875.E0,DSP0FS=-5875.E0,DSPTFS=-1875.E0                          BMPARA.102
     &, DSPBSL=DSPBFL*FSL,DSP0SL=DSP0FL*FSL,DSPTSL=DSPTFL*FSL                    BMPARA.103
     &, DSPBSS=DSPBFS*FSS,DSP0SS=DSP0FS*FSS,DSPTSS=DSPTFS*FSS                    BMPARA.104
C*** NEW CONVECTION SCHEME WITH CROSSING DSP PROFILES ******************         BMPARA.105
C+-  &, UNIS=.FALSE.,EFIMN=.71E0,EFMNT=.71,FCC=0.5,FCP=H1-FCC                    BMPARA.106
C+-  &, DSPBL=-3875.E0,DSP0L=-5875.E0,DSPTL=-1875.E0                             BMPARA.107
C+-  &, DSPBS=-2875.E0,DSP0S=-5125.E0,DSPTS=-4875.E0                             BMPARA.108
C+-  &, DSPBF=-4375.E0,DSP0F=-4375.E0,DSPTF=-1000.E0                             BMPARA.109
C*** BETTS CONVECTION SCHEME *******************************************         BMPARA.110
C    &, UNIS=.FALSE.,EFIMN=.9999E0,EFMNT=.9999E0,FCC=.50,FCP=H1-FCC              BMPARA.111
C    &, DSPBL=-3875.E0,DSP0L=-5875.E0,DSPTL=-1875.E0                             BMPARA.112
C    &, DSPBF=-3875.E0,DSP0F=-5875.E0,DSPTF=-1875.E0                             BMPARA.113
C    &, DSPBS=-3875.E0,DSP0S=-5875.E0,DSPTS=-1875.E0                             BMPARA.114
C***********************************************************************         BMPARA.115
     &, EPSNTP=.0010E0,EFIFC=5.0E0                                               BMPARA.116
     &, AVGEFI=(EFIMN+1.E0)*.5E0,DSPC=-3000.E0,EPSP=1.E-7                        BMPARA.117
C    &, STEFI=1.E0                                                               BMPARA.118
C*** ACTIVATE THE FOLLOWING LINE AND COMMENT OUT THE PRECEDING LINE IF           BMPARA.119
C*** OCT90=.TRUE.                                                                BMPARA.120
     &, STEFI=AVGEFI                                                             BMPARA.121
     &, SLOPBL=(DSPBFL-DSPBSL)/(H1-EFIMN)                                        BMPARA.122
     &, SLOP0L=(DSP0FL-DSP0SL)/(H1-EFIMN)                                        BMPARA.123
     &, SLOPTL=(DSPTFL-DSPTSL)/(H1-EFIMN)                                        BMPARA.124
     &, SLOPBS=(DSPBFS-DSPBSS)/(H1-EFIMN)                                        BMPARA.125
     &, SLOP0S=(DSP0FS-DSP0SS)/(H1-EFIMN)                                        BMPARA.126
     &, SLOPTS=(DSPTFS-DSPTSS)/(H1-EFIMN)                                        BMPARA.127
     &, SLOPE=(H1   -EFMNT)/(H1-EFIMN))                                          BMPARA.128
                             P A R A M E T E R                                   BMPARA.129
     & (A23M4L=A2*(A3-A4)*ELWV                                                   BMPARA.130
     *, D273=1./T0,CPORNG=1./DM2859                                              BMPARA.131
     &, ELOCP=ELWV/CP,CPRLG=CP/(ROW*G*ELWV),RCP=H1/CP)                           BMPARA.132
                             P A R A M E T E R                                   BMPARA.133
CVVVVVVVVVV INSTABILITY FOR TOO LARGE LSH VVVVVVVVVVVVVVVVVVVVVVVVVVVVV          BMPARA.134
     & (LA=13,KSMUD=0,NROW= 0,ITB=100,JTB=150)                                   BMPARA.135
C    & (LA=13,KSMUD=0,NROW= 5,ITB=076,JTB=134)                                   BMPARA.136
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA          BMPARA.137
                             P A R A M E T E R                                   BMPARA.138
C     & ( KNE=MIX,KNW=MIX-1,KSW=-MIX,KSE=-MIX+1                                  BMPARA.139
C     &, KHL00=1,KHL02=2*IM                                                      BMPARA.140
C     &, KHH00=IM*JM-JM/2                                                        BMPARA.141
C     &, KHH01=IM*(JM-1)-(JM-1)/2,KHH02=IM*(JM-2)-(JM-2)/2                       BMPARA.142
C     &, KHN=IM-1,KHS=-IM                                                        BMPARA.143
CVVVVVVVVVV INSTABILITY FOR TOO LARGE LSH VVVVVVVVVVVVVVVVVVVVVVVVVVVVV          BMPARA.144
C    &, LP1=LM+1,LM1=LM-1,LNO=1,LSH=LM/3-1,LSHU=LM/2-1,LQM=LM/5,KBM=6            BMPARA.145
C    &, LP1=LM+1,LM1=LM-1,LNO=1,LSH=LM/3  ,LSHU=LM/2-1,LQM=LM/5,KBM=3            BMPARA.146
     &( LP1=MKX+1,LM1=MKX-1                                                      BMPARA.147
C    &, LP1=LM+1,LM1=LM-1,LNO=3,LSH=LM/3  ,LSHU=LM/2-1,LQM=LM/5,KBM=3            BMPARA.148
C    &, LP1=LM+1,LM1=LM-1,LNO=2,LSH=LM/3-1,LSHU=LM/2-1,LQM=LM/5,KBM=3            BMPARA.149
C    &, LP1=LM+1,LM1=LM-1,LNO=2,LSH=LM/3-2,LSHU=LM/2-1,LQM=LM/5,KBM=3            BMPARA.150
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA          BMPARA.151
     &, LSCRCH=4*MKX+1+LA +1                                                     BMPARA.152
     &, L1=LA+1,L2=LA+MKX+1,L3=LA+2*MKX+1,L4=LA+3*MKX+1)                         BMPARA.153
C----------------------------------------------------------------------          BMPARA.154
      COMMON /TBLS/                                                              BMPARA.155
     & PT,PL,THL,RDQ,RDTH,RDP,RDTHE,QS0(JTB),SQS(JTB),                           BMPARA.156
     & THE0(JTB),STHE(JTB),PTBL(ITB,JTB),TTBL(JTB,ITB)                           BMPARA.157
C----------------------------------------------------------------------          BMPARA.158
#     include <param2.incl>                                                      BMPARA.159
#     include <pmoist.incl>                                                      BMPARA.160
#     include <various.incl>                                                     BMPARA.161
                                                                                 BMPARA.162
      DIMENSION T3D(MIX,MJX,MKX), QV3D(MIX,MJX,MKX),                             BMPARA.163
     &          PSB(MIX,MJX),     PP3D(MIX,MJX,MKX)                              BMPARA.164
      DIMENSION RAINC(MIX,MJX), CLDEFI(MIX,MJX), XLAND(MIX,MJX)                  BMPARA.165
      DIMENSION TBASE(MIXNH,MJXNH,MKXNH),  KDP(MKX)                              BMPARA.166
      DIMENSION SM(MIX)                                                          BMPARA.167
C                                                                                BMPARA.168
      DIMENSION NBMS(MIX,MJX), NBMD(MIX,MJX)                                     BMPARA.169
C                                                                                BMPARA.170
      DIMENSION HTOP(MIX), HBOT(MIX), T(MIX,MKX), Q(MIX,MKX)                     BMPARA.171
     &         ,A(KX), DSIGMA(KX), SIGMA(KX+1), Z0(MIX,MKX)                      BMPARA.172
     &         ,DZQ(MKX), CLDHGT(MIX), PQ(MKX), PDP(MKX)                         BMPARA.173
C-----------------------------------------------------------------------         BMPARA.174
      DIMENSION                                                                  BMPARA.175
     & TREFK (KX),QREFK (KX),PK    (KX),APEK  (KX),TK    (KX)                    BMPARA.176
     &,THSK  (KX),PSK   (KX),APESK (KX),QK    (KX),THERK (KX)                    BMPARA.177
     &,THVREF(KX),THEVRF(KX),THVMOD(KX),DIFT  (KX),DIFQ  (KX)                    BMPARA.178
     &,QSATK (KX),FPK   (KX)                                                     BMPARA.179
     &,NTOPD (KX),NBOTD (KX),NTOPS (KX),NBOTS (KX)                               BMPARA.180
     &,NDPTHD(KX),NDPTHS(KX)                                                     BMPARA.181
C                                                                                BMPARA.182
      DIMENSION                                                                  BMPARA.183
     & LTOP  (MIX),LBOT  (MIX),PRTOP (MIX),PBOT  (MIX)                           BMPARA.184
     &,ITTB  (MIX),IQTB  (MIX),IPTB  (MIX),ITHTB (MIX)                           BMPARA.185
     &,PDSL  (MIX),TBT   (MIX),QBT   (MIX),Q2BT  (MIX)                           BMPARA.186
     &,APEBT (MIX),TTHBT (MIX),TTH   (MIX),PP    (MIX)                           BMPARA.187
     &,BQS00 (MIX),SQS00 (MIX),BQS10 (MIX),SQS10 (MIX)                           BMPARA.188
     &,BQ    (MIX),SQ    (MIX),TQ    (MIX),QQ    (MIX)                           BMPARA.189
     &,P00   (MIX),P10   (MIX),P01   (MIX),P11   (MIX)                           BMPARA.190
     &,TPSP  (MIX),APESP (MIX),TTHES (MIX)                                       BMPARA.191
     &,PSP   (MIX),THBT  (MIX),THESP (MIX)                                       BMPARA.192
     &,P     (MIX),TP    (MIX),BTH   (MIX),STH   (MIX)                           BMPARA.193
     &,BTHE00(MIX),STHE00(MIX),BTHE10(MIX),STHE10(MIX)                           BMPARA.194
     &,T00   (MIX),T10   (MIX),T01   (MIX),T11   (MIX)                           BMPARA.195
     &,DSPB  (MIX),DSP0  (MIX),DSPT  (MIX)                                       BMPARA.196
     &,WF1   (MIX),WF2   (MIX),WF3   (MIX),WF4   (MIX)                           BMPARA.197
     &,TNE   (MIX),QNE   (MIX),TSE   (MIX),QSE   (MIX)                           BMPARA.198
#ifndef MPP1                                                                     BMPARA.199
     &,TL    (MIX),QL    (MIX),KSHAL (MIX),KDEEP (MIX)                           BMPARA.200
#else                                                                            BMPARA.201
C KSHAL and KDEEP are vectors of indices of points in a slab undergoing          BMPARA.202
C shallow and deep convection; prevent FLIC from doing the wrong thing.          BMPARA.203
     &,TL    (MIX),QL    (MIX),KSHAL (MIX_G),KDEEP (MIX_G)                       BMPARA.204
#endif                                                                           BMPARA.205
                                                                                 BMPARA.206
C                                                                                BMPARA.207
     &,APE   (MIX,KX),IP300(MIX),IFBUOY(MIX),KF(MIX),ML(MIX)                     BMPARA.208
C                                                                                BMPARA.209
     &,TREF  (MIX,KX),TMOD  (MIX,KX),QMOD  (MIX,KX),THS(MIX)                     BMPARA.210
C-----------------------------------------------------------------------         BMPARA.211
C     WRITE(3,12345)                                                             BMPARA.212
12345 FORMAT(' ---> ENTER BMPARA')                                               BMPARA.213
      TAUK=DT2/TREL                                                              BMPARA.214
      CTHRS=(0.006350/86400.)*DT2/CPRLG                                          BMPARA.215
      PT=PTOP*1000.                                                              BMPARA.216
C-----------------------------------------------------------------------         BMPARA.217
C                                                                                BMPARA.218
C... SM IS SURFACE MASK: =1 WATER; =0 LAND                                       BMPARA.219
      DO 90 I=IBEG,IEND                                                          BMPARA.220
        IF((XLAND(I,J)-1.5).GE.0.)THEN                                           BMPARA.221
          SM(I)=1.                                                               BMPARA.222
        ELSE                                                                     BMPARA.223
          SM(I)=0.                                                               BMPARA.224
        ENDIF                                                                    BMPARA.225
   90 CONTINUE                                                                   BMPARA.226
      IF(KTAU.EQ.0)THEN                                                          BMPARA.227
        DO 100 I=IBEG,IEND                                                       BMPARA.228
          CLDEFI(I,J)=AVGEFI*SM(I)+STEFI*(H1-SM(I))                              BMPARA.229
  100   CONTINUE                                                                 BMPARA.230
C       PRINT 103,ZNO,ZSH                                                        BMPARA.231
C       PRINT 104,ZSH                                                            BMPARA.232
103     FORMAT(//,'SHALLOW CONVECTION OCCURS WHEN CLOUD DEPTH IS',               BMPARA.233
     1      ' .GE. ',F5.0,' M. AND .LT. ',F5.0,' M.')                            BMPARA.234
104     FORMAT(//,'DEEP CONVECTION OCCURS WHEN CLOUD DEPTH IS .GE. ',            BMPARA.235
     1      F5.0,' KM.')                                                         BMPARA.236
      ENDIF                                                                      BMPARA.237
C     IF((KTAU.EQ.0.OR.KTAU.EQ.KTAUR).AND.J.EQ.JBEG)THEN                         BMPARA.238
C     DIFINIT=1.0                                                                BMPARA.239
C     DO 201 K=1,KL                                                              BMPARA.240
C     DIFK=ABS(A(K)-LBLEV)                                                       BMPARA.241
C     IF(DIFK.LE.DIFINIT)THEN                                                    BMPARA.242
C     LB=K                                                                       BMPARA.243
C     DIFINIT=DIFK                                                               BMPARA.244
C     ENDIF                                                                      BMPARA.245
  201 CONTINUE                                                                   BMPARA.246
C     PRINT *,'AFTER DO 201...'                                                  BMPARA.247
C     LB=MIN(KL-1,LB)                                                            BMPARA.248
C     ENDIF                                                                      BMPARA.249
C...LB IS CURRENTLY SET TO KL-1                                                  BMPARA.250
      LB=KL-1                                                                    BMPARA.251
CVVVVVVVVVVVVV DIAGNOSTICS VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.252
      DO 110 K=1,KL                                                              BMPARA.253
        NTOPD(K)=0                                                               BMPARA.254
        NBOTD(K)=0                                                               BMPARA.255
        NTOPS(K)=0                                                               BMPARA.256
        NBOTS(K)=0                                                               BMPARA.257
        NDPTHS(K)=0                                                              BMPARA.258
        NDPTHD(K)=0                                                              BMPARA.259
  110 CONTINUE                                                                   BMPARA.260
CAAAAAAAAAAAAA DIAGNOSTICS AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.261
C--------------PREPARATIONS---------------------------------------------         BMPARA.262
C...FIND MELTING LEVEL...                                                        BMPARA.263
      DO 115 I=IBEG,IEND                                                         BMPARA.264
        NBMS(I,J)=0                                                              BMPARA.265
        NBMD(I,J)=0                                                              BMPARA.266
  115 ML(I)=KXP1                                                                 BMPARA.267
      DO 120 I=IBEG,IEND                                                         BMPARA.268
C        IF(KF(I).EQ.1)GOTO 120                                                  BMPARA.269
        DO 121 K=1,KL                                                            BMPARA.270
          T(I,K)=T3D(I,J,K)                                                      BMPARA.271
          IF(T(I,K).GT.T0.AND.ML(I).EQ.KXP1)ML(I)=K                              BMPARA.272
          Q(I,K)=QV3D(I,J,K)                                                     BMPARA.273
          APE(I,K)=((A(K)*PSB(I,J)*1000.+PT+PP3D(I,J,K))/H10E5)**DM2859          BMPARA.274
  121   CONTINUE                                                                 BMPARA.275
        LBOT(I)=KL                                                               BMPARA.276
        THESP(I)=D00                                                             BMPARA.277
        THBT(I)=D00                                                              BMPARA.278
        PSP(I)=9.5E4                                                             BMPARA.279
C      PDSL (K)=RES(K)*PD(K)                                                     BMPARA.280
        TREF(I,1)=T(I,1)                                                         BMPARA.281
C...IFBUOY = 0 MEANS NO POSITIVE BUOYANCY; IFBUOY(I) MEANS YES...                BMPARA.282
C...IP300 IS THE HIGHEST MODEL LEVEL IN THE LOWEST 300 MB...                     BMPARA.283
        IFBUOY(I)=0                                                              BMPARA.284
        IP300(I)=0                                                               BMPARA.285
        KF(I)=0                                                                  BMPARA.286
C...ACTIVATE THIS STATEMENT TO RUN ONLY SHALLOW BETTS-MILLER WITH KF AND         BMPARA.287
C...AK DEEP CONVECTION...                                                        BMPARA.288
C      KF(I)=CVMGT(1,0,(ICPS(INEST).EQ.2.AND.NCA(I,J).GT.0) .OR.                 BMPARA.289
C     *                (ICPS(INEST).EQ.1.AND.NAK(I,J).GT.0))                     BMPARA.290
        CELL=PTOP*RPSB(I,J)                                                      BMPARA.291
        DO 378 K=1,KX                                                            BMPARA.292
          TV0=T(I,K)*(1.+D608*Q(I,K))                                            BMPARA.293
          DZQ(K)=R/G*TBASE(I,J,K)*                                               BMPARA.294
     +           ALOG((SIGMA(K+1)+CELL)/(SIGMA(K)+CELL))                         BMPARA.295
C        DZQ(K)=(R/G)*TV0*ALOG((SIGMA(K+1)+CELL)/(SIGMA(K)+CELL))                BMPARA.296
  378   CONTINUE                                                                 BMPARA.297
        Z0(I,KX)=0.5*DZQ(KX)                                                     BMPARA.298
        DO 379 K=KX-1,1,-1                                                       BMPARA.299
          Z0(I,K)=Z0(I,K+1)+0.5*(DZQ(K)+DZQ(K+1))                                BMPARA.300
  379   CONTINUE                                                                 BMPARA.301
  120 CONTINUE                                                                   BMPARA.302
C--------------PADDING SPECIFIC HUMIDITY IF TOO SMALL-------------------         BMPARA.303
      DO 130 K=1,KL                                                              BMPARA.304
        DO 130 I=IBEG,IEND                                                       BMPARA.305
C        IF(KF(I).EQ.1)GOTO 130                                                  BMPARA.306
          IF(Q(I,K).LT.EPSQ)Q(I,K)=EPSQ                                          BMPARA.307
          PDIFF=(1.-A(K))*PSB(I,J)+(PP3D(I,J,KL)-PP3D(I,J,K))*0.001              BMPARA.308
          IF(PDIFF.LT.30..AND.IP300(I).EQ.0)IP300(I)=K                           BMPARA.309
  130   CONTINUE                                                                 BMPARA.310
                                                                                 BMPARA.311
C--------------SEARCH FOR MAXIMUM BUOYANCY LEVEL------------------------         BMPARA.312
      DO 140 KB=1,KL                                                             BMPARA.313
C       PRINT *,'I, J, KB, Z0, PBL =',                                           BMPARA.314
C     *I,J,KB,Z0(I,KB),PBL(I,J)                                                  BMPARA.315
C--------------TRIAL MAXIMUM BUOYANCY LEVEL VARIABLES-------------------         BMPARA.316
        DO 150 I=IBEG,IEND                                                       BMPARA.317
C           IF(Z0(I,KB).GT.PBL(I,J)+10.)GOTO 150                                 BMPARA.318
C           IF(KF(I).EQ.1)GOTO 150                                               BMPARA.319
          PKL=A(KB)*PSB(I,J)*1000.+PT+PP3D(I,J,KB)                               BMPARA.320
          PSFCK=A(KL)*PSB(I,J)*1000.+PT+PP3D(I,J,KL)                             BMPARA.321
          IF(PKL.LT.PSFCK-PBM)GOTO 150                                           BMPARA.322
          TTHBT(I)=T(I,KB)*APE(I,KB)                                             BMPARA.323
          EE=PKL*Q(I,KB)/(0.622+Q(I,KB))                                         BMPARA.324
          TDPT=1./(D273-RV/ELWV*ALOG(EE/611.))                                   BMPARA.325
          TDPT=AMIN1(TDPT,T(I,KB))                                               BMPARA.326
          TLCL=TDPT-(.212+1.571E-3*(TDPT-T0)-4.36E-4*(T(I,KB)-T0))*(             BMPARA.327
     +         T(I,KB)-TDPT)                                                     BMPARA.328
          TTHES(I)=TTHBT(I)*EXP(ELOCP*Q(I,KB)/TLCL)                              BMPARA.329
C--------------CHECK FOR MAXIMUM BUOYANCY-------------------------------         BMPARA.330
          IF(TTHES(I).GT.THESP(I))THEN                                           BMPARA.331
            PSP(I)=H10E5*(TTHBT(I)/TLCL)**CPORNG                                 BMPARA.332
            THBT(I)=TTHBT(I)                                                     BMPARA.333
            THESP(I)=TTHES(I)                                                    BMPARA.334
          ENDIF                                                                  BMPARA.335
C-----------------------------------------------------------------------         BMPARA.336
  150   CONTINUE                                                                 BMPARA.337
  140 CONTINUE                                                                   BMPARA.338
C---------CHOOSE CLOUD BASE AS MODEL LEVEL JUST BELOW PSP--------------          BMPARA.339
      DO 240 K=1,LM1                                                             BMPARA.340
        AK=A(K)                                                                  BMPARA.341
        DO 240 I=IBEG,IEND                                                       BMPARA.342
C          IF(KF(I).EQ.1)GOTO 240                                                BMPARA.343
          P(I)=AK*PSB(I,J)*1000.+PT+PP3D(I,J,K)                                  BMPARA.344
C CLOUD BOTTOM CANNOT BE ABOVE 200 MB                                            BMPARA.345
          IF(P(I).LT.PSP(I).AND.P(I).GE.PQM)LBOT(I)=K+1                          BMPARA.346
  240   CONTINUE                                                                 BMPARA.347
                                                                                 BMPARA.348
C*** WARNING: LBOT MUST NOT BE GT KL-1 IN SHALLOW CONVECTION                     BMPARA.349
C*** MAKE SURE THE CLOUD BASE IS AT LEAST 25 MB ABOVE THE SURFACE                BMPARA.350
      DO 250 I=IBEG,IEND                                                         BMPARA.351
C       IF(KF(I).EQ.1)GOTO 250                                                   BMPARA.352
        PBOT(I)=A(LBOT(I))*PSB(I,J)*1000.+PT+PP3D(I,J,LBOT(I))                   BMPARA.353
        PSFCK=A(KL)*PSB(I,J)*1000.+PT+PP3D(I,J,KL)                               BMPARA.354
        IF(PBOT(I).GE.PSFCK-PONE.OR.LBOT(I).GE.KL)THEN                           BMPARA.355
C*** CLOUD BOTTOM IS AT THE SURFACE SO RECALCULATE CLOUD BOTTOM                  BMPARA.356
          DO 245 K=1,LM1                                                         BMPARA.357
            P(I)=A(K)*PSB(I,J)*1000.+PT+PP3D(I,J,K)                              19DEC02.1360
            IF(P(I).LT.PSFCK-PONE)LBOT(I)=K                                      BMPARA.359
  245     CONTINUE                                                               BMPARA.360
          PBOT(I)=A(LBOT(I))*PSB(I,J)*1000.+PT+PP3D(I,J,LBOT(I))                 BMPARA.361
        ENDIF                                                                    BMPARA.362
  250 CONTINUE                                                                   BMPARA.363
C--------------CLOUD TOP COMPUTATION------------------------------------         BMPARA.364
      DO 260 I=IBEG,IEND                                                         BMPARA.365
        PRTOP(I)=PBOT(I)                                                         BMPARA.366
  260 LTOP(I)=LBOT(I)                                                            BMPARA.367
      DO 270 IVI=1,KL                                                            BMPARA.368
        L=LP1-IVI                                                                BMPARA.369
C--------------FIND ENVIRONMENTAL SATURATION EQUIV POT TEMP...                   BMPARA.370
        DO 280 I=IBEG,IEND                                                       BMPARA.371
          P(I)=A(L)*PSB(I,J)*1000.+PT+PP3D(I,J,L)                                BMPARA.372
          ES=ALIQ*EXP((BLIQ*T(I,L)-CLIQ)/(T(I,L)-DLIQ))                          BMPARA.373
          QS=0.622*ES/(P(I)-ES)                                                  BMPARA.374
          THS(I)=T(I,L)*APE(I,L)*EXP(ELOCP*QS/T(I,L))                            BMPARA.375
  280   CONTINUE                                                                 BMPARA.376
C--------------BUOYANCY CHECK-------------------------------------------         BMPARA.377
        DO 350 I=IBEG,IEND                                                       BMPARA.378
          IF(L.LE.LBOT(I))THEN                                                   BMPARA.379
            IF(THESP(I).GT.THS(I))IFBUOY(I)=1                                    BMPARA.380
            IF(THESP(I).GT.THS(I)-1.5.AND.IFBUOY(I).EQ.1)LTOP(I)=L+1             BMPARA.381
          ENDIF                                                                  BMPARA.382
  350   CONTINUE                                                                 BMPARA.383
C------------------------------------------------                                BMPARA.384
  270 CONTINUE                                                                   BMPARA.385
C--------------CLOUD TOP PRESSURE---------------------------------------         BMPARA.386
      DO 275 I=IBEG,IEND                                                         BMPARA.387
C        IF(KF(I).EQ.1) GOTO 275                                                 BMPARA.388
        PRTOP(I)=A(LTOP(I))*PSB(I,J)*1000.+PT+PP3D(I,J,LTOP(I))                  BMPARA.389
  275 CONTINUE                                                                   BMPARA.390
C-----------------------------------------------------------------------         BMPARA.391
C--------------DEFINE AND SMOOTH DSPS AND CLDEFI------------------------         BMPARA.392
CVVVVV UNIFIED OR SEPARATE LAND/SEA CONV VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.393
      IF(UNIS)THEN                                                               BMPARA.394
        DO 370 I=IBEG,IEND                                                       BMPARA.395
C           IF(KF(I).EQ.1)GOTO 370                                               BMPARA.396
          EFI=CLDEFI(I,J)                                                        BMPARA.397
          DSPB(I)=(EFI-EFIMN)*SLOPBS+DSPBSS                                      BMPARA.398
          DSP0(I)=(EFI-EFIMN)*SLOP0S+DSP0SS                                      BMPARA.399
          DSPT(I)=(EFI-EFIMN)*SLOPTS+DSPTSS                                      BMPARA.400
  370   CONTINUE                                                                 BMPARA.401
      ELSEIF(.NOT.UNIL)THEN                                                      BMPARA.402
        DO 371 I=IBEG,IEND                                                       BMPARA.403
C           IF(KF(I).EQ.1)GOTO 371                                               BMPARA.404
          EFI=CLDEFI(I,J)                                                        BMPARA.405
          DSPB(I)=((EFI-EFIMN)*SLOPBS+DSPBSS)*SM(I)+((EFI-EFIMN)*SLOPBL+         BMPARA.406
     +            DSPBSL)*(H1-SM(I))                                             BMPARA.407
          DSP0(I)=((EFI-EFIMN)*SLOP0S+DSP0SS)*SM(I)+((EFI-EFIMN)*SLOP0L+         BMPARA.408
     +            DSP0SL)*(H1-SM(I))                                             BMPARA.409
          DSPT(I)=((EFI-EFIMN)*SLOPTS+DSPTSS)*SM(I)+((EFI-EFIMN)*SLOPTL+         BMPARA.410
     +            DSPTSL)*(H1-SM(I))                                             BMPARA.411
  371   CONTINUE                                                                 BMPARA.412
      ELSE                                                                       BMPARA.413
        DO 375 I=IBEG,IEND                                                       BMPARA.414
          EFI=CLDEFI(I,J)                                                        BMPARA.415
          DSPB(I)=((EFI-EFIMN)*SLOPBL+DSPBSL)                                    BMPARA.416
          DSP0(I)=((EFI-EFIMN)*SLOP0L+DSP0SL)                                    BMPARA.417
  375   DSPT(I)=((EFI-EFIMN)*SLOPTL+DSPTSL)                                      BMPARA.418
      ENDIF                                                                      BMPARA.419
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.420
C--------------EXTENDING SEA STRUCTURES INLAND ALONG COASTLINE----------         BMPARA.421
C      IF(NROW.GT.0.AND..NOT.UNIS.AND..NOT.UNIL)    THEN                         BMPARA.422
C-----------------------------------------------------------------------         BMPARA.423
C          DO 373 I=IBEG,IEND                                                    BMPARA.424
C      WF1(I)=0.                                                                 BMPARA.425
C      WF2(I)=0.                                                                 BMPARA.426
C      WF3(I)=0.                                                                 BMPARA.427
C 373  WF4(I)=0.                                                                 BMPARA.428
C-----------------------------------------------------------------------         BMPARA.429
C      KROW=NROW                                                                 BMPARA.430
C                             DO 372 KOUNT=1,KROW                                BMPARA.431
C          DO 374 I=IBEG,IEND                                                    BMPARA.432
C      WF1(I)=(DSPB  (I+KSW)+DSPB  (I+KSE)                                       BMPARA.433
C     2       +DSPB  (I+KNW)+DSPB  (I+KNE)+DSPB  (I)*4.0)*HBM2(I)*0.125          BMPARA.434
C      WF2(I)=(DSP0  (I+KSW)+DSP0  (I+KSE)                                       BMPARA.435
C     2       +DSP0  (I+KNW)+DSP0  (I+KNE)+DSP0  (I)*4.0)*HBM2(I)*0.125          BMPARA.436
C      WF3(I)=(DSPT  (I+KSW)+DSPT  (I+KSE)                                       BMPARA.437
C     2       +DSPT  (I+KNW)+DSPT  (I+KNE)+DSPT  (I)*4.0)*HBM2(I)*0.125          BMPARA.438
C 374  WF4(I)=(CLDEFI(I+KSW)+CLDEFI(I+KSE)                                       BMPARA.439
C     2       +CLDEFI(I+KNW)+CLDEFI(I+KNE)+CLDEFI(I)*4.0)*HBM2(I)*0.125          BMPARA.440
C          DO 372 I=IBEG,IEND                                                    BMPARA.441
C      SMK =SM(I,J)                                                              BMPARA.442
C      RSMK=1.0-SMK                                                              BMPARA.443
C      DSPB  (I)=DSPB  (I)*SMK+WF1(I)*RSMK                                       BMPARA.444
C      DSP0  (I)=DSP0  (I)*SMK+WF2(I)*RSMK                                       BMPARA.445
C      DSPT  (I)=DSPT  (I)*SMK+WF3(I)*RSMK                                       BMPARA.446
C 372  CLDEFI(I)=CLDEFI(I)*SMK+WF4(I)*RSMK                                       BMPARA.447
C-----------------------------------------------------------------------         BMPARA.448
C      ENDIF                                                                     BMPARA.449
C--------------INITIALIZE CHANGES OF T AND Q DUE TO CONVECTION----------         BMPARA.450
      DO 360 K=1,KL                                                              BMPARA.451
        DO 360 I=IBEG,IEND                                                       BMPARA.452
          TMOD(I,K)=D00                                                          BMPARA.453
  360   QMOD(I,K)=D00                                                            BMPARA.454
                                                                                 BMPARA.455
C--------------CLEAN UP AND GATHER DEEP CONVECTION POINTS---------------         BMPARA.456
      KHDEEP=0                                                                   BMPARA.457
      NSWAP=0                                                                    BMPARA.458
CDIR$ NOVECTOR                                                                   BMPARA.459
      DO 380 I=IBEG,IEND                                                         BMPARA.460
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.461
C     LLMH=LMH(K)                                                                BMPARA.462
C     UBAR=(U(K+KHS,LLMH)+U(K-1  ,LLMH)+U(K    ,LLMH)+U(K+KHN,LLMH))*.25         BMPARA.463
C     VBAR=(V(K+KHS,LLMH)+V(K-1  ,LLMH)+V(K    ,LLMH)+V(K+KHN,LLMH))*.25         BMPARA.464
C     VCNV=SQRT(UBAR*UBAR+VBAR*VBAR)                                             BMPARA.465
C     IF(VCNV.LT.8.            )    LTOP(K)=LBOT(K)-LSH                          BMPARA.466
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.467
C...ACTIVATE THIS STATEMENT TO RUN ONLY SHALLOW BETTS-MILLER WITH KF AND         BMPARA.468
C...AK DEEP CONVECTION...                                                        BMPARA.469
C      IF(LTOP(I).GT.LBOT(I) .OR. KF(I).EQ.1)  THEN                              BMPARA.470
        IF(LTOP(I).GT.LBOT(I))THEN                                               BMPARA.471
          LTOP(I)=LBOT(I)                                                        BMPARA.472
          PRTOP(I)=PBOT(I)                                                       BMPARA.473
        ENDIF                                                                    BMPARA.474
C     IF(HBM2(I).LT.0.90       )    LTOP(I)=LBOT(I)                              BMPARA.475
        CLDHGT(I)=Z0(I,LTOP(I))-Z0(I,LBOT(I))                                    BMPARA.476
C  CLOUD IS LESS THAN 90 MB DEEP OR LESS THAN 3 SIGMA LAYERS DEEP                BMPARA.477
C     IF(PRTOP(I).GT.PBOT(I)-PNO.OR.LTOP(I).GT.LBOT(I)-2)                        BMPARA.478
C     IF(CLDHGT(I).LT.ZNO.OR.LTOP(I).GT.LBOT(I)-2)                               BMPARA.479
        IF(CLDHGT(I).LT.ZNO)CLDEFI(I,J)=AVGEFI*SM(I)+STEFI*(H1-SM(I))            BMPARA.480
C  CLOUD HAS TO BE AT LEAST 290 MB DEEP                                          BMPARA.481
        IF(CLDHGT(I).GE.ZSH)THEN                                                 BMPARA.482
C     IF(PRTOP(I).LT.PBOT(I)-PSH)THEN                                            BMPARA.483
          KHDEEP=KHDEEP+1                                                        BMPARA.484
#ifndef MPP1                                                                     BMPARA.485
          KDEEP(KHDEEP)=I                                                        BMPARA.486
#else                                                                            BMPARA.487
          KDEEP(KHDEEP)=FLIC_L2G_M(I)                                            BMPARA.488
#endif                                                                           BMPARA.489
        ENDIF                                                                    BMPARA.490
  380 CONTINUE                                                                   BMPARA.491
CDIR$ VECTOR                                                                     BMPARA.492
C************* HORIZONTAL LOOP FOR DEEP CONVECTION *********************         BMPARA.493
C***********************************************************************         BMPARA.494
C     PRINT*,'NUMBER OF DEEP CONVECTIVE POINTS=',KHDEEP                          BMPARA.495
      DO 400 N=1,KHDEEP                                                          BMPARA.496
        I=KDEEP(N)                                                               BMPARA.497
        DENTPY=D00                                                               BMPARA.498
        AVRGT=D00                                                                BMPARA.499
        PRECK=D00                                                                BMPARA.500
C      IF(ICPS(INEST).EQ.2 .OR. ICPS(INEST).EQ.1)GOTO 532                        BMPARA.501
        LTPK=LTOP(I)                                                             BMPARA.502
        LBTK=LBOT(I)                                                             BMPARA.503
CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD         BMPARA.504
CDCDCDCDCDCDC  DEEP CONVECTION   DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD         BMPARA.505
CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD         BMPARA.506
C     LB   =KL-1                                                                 BMPARA.507
        EFI=CLDEFI(I,J)                                                          BMPARA.508
        DSPBK=DSPB(I)                                                            BMPARA.509
        DSP0K=DSP0(I)                                                            BMPARA.510
        DSPTK=DSPT(I)                                                            BMPARA.511
C--------------INITIALIZE VARIABLES IN THE CONVECTIVE COLUMN------------         BMPARA.512
        DO 410 K=1,KL                                                            BMPARA.513
          DIFT(K)=D00                                                            BMPARA.514
          DIFQ(K)=D00                                                            BMPARA.515
          TKL=T(I,K)                                                             BMPARA.516
          TK(K)=TKL                                                              BMPARA.517
          TREFK(K)=TKL                                                           BMPARA.518
          QKL=Q(I,K)                                                             BMPARA.519
          QK(K)=QKL                                                              BMPARA.520
          QREFK(K)=QKL                                                           BMPARA.521
          PKL=A(K)*PSB(I,J)*1000.+PT+PP3D(I,J,K)                                 BMPARA.522
***************                                                                  BMPARA.523
          TREF(I,K)=TPFC(PKL,THESP(I),T(I,K),D273,ELWV,QU,APE(I,K))              BMPARA.524
****************                                                                 BMPARA.525
          PK(K)=PKL                                                              BMPARA.526
          PSK(K)=PKL                                                             BMPARA.527
          APEKL=APE(I,K)                                                         BMPARA.528
          APEK(K)=APEKL                                                          BMPARA.529
          THERK(K)=TREF(I,K)*APEKL                                               BMPARA.530
  410   THVMOD(K)=(QKL*D608+H1)*TKL*APEKL                                        BMPARA.531
C     PRINT *, 'LB,PK(K) = ', LB,PK                                              BMPARA.532
C--------------DEEP CONVECTION REFERENCE TEMPERATURE PROFILE------------         BMPARA.533
        LTP1=LTPK+1                                                              BMPARA.534
        LBM1=LB-1                                                                BMPARA.535
        PKB=PK(LB)                                                               BMPARA.536
        PKT=PK(LTPK)                                                             BMPARA.537
C--------------TEMPERATURE REFERENCE PROFILE BELOW FREEZING LEVEL-------         BMPARA.538
        L0=LB                                                                    BMPARA.539
        PK0=PK(LB)                                                               BMPARA.540
        DO 420 L=LTPK,LBM1                                                       BMPARA.541
          IVI=LTPK+LBM1-L                                                        BMPARA.542
          IF(TREFK(IVI+1).LE.T1)GOTO 430                                         BMPARA.543
          STABDL=STABD                                                           BMPARA.544
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.545
C         IF(IVI.EQ.LBM1.AND.SM(I).GT.D00) STABDL=STABD*STABFC                   BMPARA.546
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.547
          TREFK(IVI)=((THERK(IVI)-THERK(IVI+1))*STABDL+TREFK(IVI+1)*             BMPARA.548
     +               APEK(IVI+1))/APEK(IVI)                                      BMPARA.549
          L0=IVI                                                                 BMPARA.550
          PK0=PK(L0)                                                             BMPARA.551
  420   CONTINUE                                                                 BMPARA.552
C--------------FREEZING LEVEL AT OR ABOVE THE CLOUD TOP-----------------         BMPARA.553
        L0M1=L0-1                                                                BMPARA.554
        GOTO 440                                                                 BMPARA.555
C--------------TEMPERATURE REFERENCE PROFILE ABOVE FREEZING LEVEL-------         BMPARA.556
  430   L0M1=L0-1                                                                BMPARA.557
        RDP0T=H1/(PK0-PKT)                                                       BMPARA.558
        DTHEM=THERK(L0)-TREFK(L0)*APEK(L0)                                       BMPARA.559
        DO 450 L=LTPK,L0M1                                                       BMPARA.560
  450   TREFK(L)=(THERK(L)-(PK(L)-PKT)*DTHEM*RDP0T)/APEK(L)                      BMPARA.561
C--------------DEEP CONVECTION REFERENCE HUMIDITY PROFILE---------------         BMPARA.562
  440   DO 460 L=LTPK,LB                                                         BMPARA.563
C--------------SATURATION PRESSURE DIFFERENCE---------------------------         BMPARA.564
          IF(PKB-PK0.GE.PFRZ)THEN                                                BMPARA.565
! P(KL-1) -P(FRZ) .GE. 150 MB)                                                   BMPARA.566
            IF(L.LT.L0)THEN                                                      BMPARA.567
              DSP=((PK0-PK(L))*DSPTK+(PK(L)-PKT)*DSP0K)/(PK0-PKT)                BMPARA.568
            ELSE                                                                 BMPARA.569
              DSP=((PKB-PK(L))*DSP0K+(PK(L)-PK0)*DSPBK)/(PKB-PK0)                BMPARA.570
            ENDIF                                                                BMPARA.571
          ELSE                                                                   BMPARA.572
            DSP=DSPC                                                             BMPARA.573
          ENDIF                                                                  BMPARA.574
C--------------HUMIDITY PROFILE-----------------------------------------         BMPARA.575
          IF(PK(L).GT.PQM)THEN                                                   BMPARA.576
! PRESSURE MUST BE BELOW 200 MB                                                  BMPARA.577
            PSK(L)=PK(L)+DSP                                                     BMPARA.578
            APESK(L)=(PSK(L)/H10E5)**DM2859                                      BMPARA.579
            THSK(L)=TREFK(L)*APEK(L)                                             BMPARA.580
            QREFK(L)=PQ0/PSK(L)*                                                 BMPARA.581
     +               EXP(A2*(THSK(L)-A3*APESK(L))/(THSK(L)-A4*APESK(L)))         BMPARA.582
          ELSE                                                                   BMPARA.583
            QREFK(L)=Q(I,L)                                                      BMPARA.584
          ENDIF                                                                  BMPARA.585
  460   CONTINUE                                                                 BMPARA.586
CVVVVVVVVVVVVV DIAGNOSTICS VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.587
C      PRINT 4701, KTAU,J,N,LTPK,LB                                              BMPARA.588
C4701  FORMAT(1X,'KTAU,J,N,LTPK,LB = ',5I8)                                      BMPARA.589
        DO 470 L=LTPK,LB                                                         BMPARA.590
  470   THEVRF(L)=TREFK(L)*APEK(L)*(QREFK(L)*D608+H1)                            BMPARA.591
C--------------ENTHALPY CONSERVATION INTEGRAL--------------------------          BMPARA.592
        DO 480 ITER=1,2                                                          BMPARA.593
C-----------------------------------------------------------------------         BMPARA.594
          SUMDE=D00                                                              BMPARA.595
          SUMDP=D00                                                              BMPARA.596
          DO 490 L=LTPK,LB                                                       BMPARA.597
            SUMDE=((TK(L)-TREFK(L))*CP+(QK(L)-QREFK(L))*ELWV)*DSIGMA(L)+         BMPARA.598
     +            SUMDE                                                          BMPARA.599
  490     SUMDP=SUMDP+DSIGMA(L)                                                  BMPARA.600
          HCORR=SUMDE/(SUMDP-DSIGMA(LTPK))                                       BMPARA.601
          LCOR=LTPK+1                                                            BMPARA.602
C--------------FIND LQM-------------------------------------------------         BMPARA.603
          DO 495 L=1,LB                                                          BMPARA.604
            IF(PK(L).LE.PQM)LQM=L                                                BMPARA.605
  495     CONTINUE                                                               BMPARA.606
C      PRINT 4950, LCOR,LQM                                                      BMPARA.607
C4950  FORMAT(1X,' === LCOR,LQM = ',2I6)                                         BMPARA.608
C--------------ABOVE LQM CORRECT TEMPERATURE ONLY-----------------------         BMPARA.609
          IF(LCOR.LE.LQM)THEN                                                    BMPARA.610
            DO 500 L=LCOR,LQM                                                    BMPARA.611
  500       TREFK(L)=TREFK(L)+HCORR*RCP                                          BMPARA.612
            LCOR=LQM+1                                                           BMPARA.613
          ENDIF                                                                  BMPARA.614
C--------------BELOW LQM CORRECT BOTH TEMPERATURE AND MOISTURE----------         BMPARA.615
          DO 510 L=LCOR,LB                                                       BMPARA.616
            TSKL=TREFK(L)*APEK(L)/APESK(L)                                       BMPARA.617
            DHDT=QREFK(L)*A23M4L/(TSKL-A4)**2+CP                                 BMPARA.618
            TREFK(L)=HCORR/DHDT+TREFK(L)                                         BMPARA.619
            THSKL=TREFK(L)*APEK(L)                                               BMPARA.620
            QREFK(L)=PQ0/PSK(L)*                                                 BMPARA.621
     +               EXP(A2*(THSKL-A3*APESK(L))/(THSKL-A4*APESK(L)))             BMPARA.622
  510     CONTINUE                                                               BMPARA.623
C-----------------------------------------------------------------------         BMPARA.624
  480   CONTINUE                                                                 BMPARA.625
CVVVVVVVVVVVVV DIAGNOSTICS VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.626
        DO 520 L=1,KL                                                            BMPARA.627
  520   THVREF(L)=TREFK(L)*APEK(L)*(QREFK(L)*D608+H1)                            BMPARA.628
C--------------HEATING, MOISTENING, PRECIPITATION-----------------------         BMPARA.629
        DO 530 L=LTPK,LB                                                         BMPARA.630
          TKL=TK(L)                                                              BMPARA.631
          DIFTL=(TREFK(L)-TKL)*TAUK                                              BMPARA.632
          DIFQL=(QREFK(L)-QK(L))*TAUK                                            BMPARA.633
          AVRGTL=(TKL+TKL+DIFTL)                                                 BMPARA.634
          DENTPY=(DIFTL*CP+DIFQL*ELWV)*DSIGMA(L)/AVRGTL+DENTPY                   BMPARA.635
          AVRGT=AVRGTL*DSIGMA(L)+AVRGT                                           BMPARA.636
          PRECK=DSIGMA(L)*DIFTL+PRECK                                            BMPARA.637
          DIFT(L)=DIFTL                                                          BMPARA.638
  530   DIFQ(L)=DIFQL                                                            BMPARA.639
        DENTPY=DENTPY+DENTPY                                                     BMPARA.640
        AVRGT=AVRGT/(SUMDP+SUMDP)                                                BMPARA.641
C-------------SWAP IF ENTROPY AND/OR PRECIP .LE. 0 ...------------------         BMPARA.642
C 532   IF(DENTPY.LT.EPSNTP .OR. PRECK.LE.D00 .OR.                               BMPARA.643
C     *   ICPS(INEST).EQ.2)    THEN                                              BMPARA.644
  532   IF(DENTPY.LT.EPSNTP.OR.PRECK.LE.D00)THEN                                 BMPARA.645
          IF(OCT90)THEN                                                          BMPARA.646
            CLDEFI(I,J)=EFIMN                                                    BMPARA.647
          ELSE                                                                   BMPARA.648
            CLDEFI(I,J)=EFIMN*SM(I)+STEFI*(H1-SM(I))                             BMPARA.649
          ENDIF                                                                  BMPARA.650
          ZTOP=Z0(I,LBOT(I))+ZSH-0.000001                                        BMPARA.651
C         PRTOP(I)=PBOT(I)-PSH    ! PTOP IS NOW PBOT - 290.                      BMPARA.652
          DO 535 L=1,LB                                                          BMPARA.653
            IF(Z0(I,L).GE.ZTOP)LTOP(I)=L+1                                       BMPARA.654
C           IF(PK(L).LE.PRTOP(I)) LTOP(I)=L+1                                    BMPARA.655
  535     CONTINUE                                                               BMPARA.656
          PRTOP(I)=PK(LTOP(I))                                                   BMPARA.657
C------------CLOUD MUST BE AT LEAST 2 LAYERS THICK---------------------          BMPARA.658
          IF(LBOT(I)-LTOP(I).LT.2)LTOP(I)=LBOT(I)-2                              BMPARA.659
          PRTOP(I)=PK(LTOP(I))                                                   BMPARA.660
          CLDHGT(I)=Z0(I,LTOP(I))-Z0(I,LBOT(I))                                  BMPARA.661
          NSWAP=NSWAP+1                                                          BMPARA.662
          GOTO 400                                                               BMPARA.663
        ENDIF                                                                    BMPARA.664
C--------------... DEEP CONVECTION OTHERWISE----------------------------         BMPARA.665
        ICOND=ICOND+1                                                            BMPARA.666
C***  KEEP THE LAND VALUE OF EFI EQUAL TO 1 UNTIL PRECIP SURPASSES               BMPARA.667
C***  A THRESHOLD VALUE, CURRENTLY SET TO 0.25 INCHES PER 24 HRS                 BMPARA.668
        PTHRS=CTHRS*RPSB(I,J)                                                    BMPARA.669
        DRHEAT=(PRECK*SM(I)+AMAX1(EPSP,PRECK-PTHRS)*(H1-SM(I)))*CP/AVRGT         BMPARA.670
        EFI=EFIFC*DENTPY/DRHEAT                                                  BMPARA.671
CVVVVV UNIFIED OR SEPARATE LAND/SEA CONV. VVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.672
        IF(OCT90)THEN                                                            BMPARA.673
          IF(UNIS)THEN                                                           BMPARA.674
            EFI=CLDEFI(I,J)*FCP+EFI*FCC                                          BMPARA.675
          ELSEIF(.NOT.UNIL)THEN                                                  BMPARA.676
            EFI=(CLDEFI(I,J)*FCP+EFI*FCC)*SM(I)+H1-SM(I)                         BMPARA.677
          ELSE                                                                   BMPARA.678
            EFI=H1                                                               BMPARA.679
          ENDIF                                                                  BMPARA.680
        ELSE                                                                     BMPARA.681
          EFI=CLDEFI(I,J)*FCP+EFI*FCC                                            BMPARA.682
        ENDIF                                                                    BMPARA.683
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.684
        IF(EFI.GT.H1)EFI=H1                                                      BMPARA.685
        IF(EFI.LT.EFIMN)EFI=EFIMN                                                BMPARA.686
        CLDEFI(I,J)=EFI                                                          BMPARA.687
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.688
        FEFI=EFMNT+SLOPE*(EFI-EFIMN)                                             BMPARA.689
C     FEFI=AMAX1(EFI,EFMNT)                                                      BMPARA.690
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.691
        PRECK=PRECK*FEFI                                                         BMPARA.692
C--------------UPDATE PRECIPITATION, TEMPERATURE & MOISTURE-------------         BMPARA.693
C         PREC  (I,J)=PSB(I,J)*PRECK*CPRLG+PREC  (I,J)                           BMPARA.694
C CORRECT RAINC SO THAT THERE IS NO DOUBLE COUNTING OF PRECIPITATION (GK         BMPARA.695
C         RAINC(I,J)=((PSB(I,J)*1000.*PRECK*CPRLG)*100.)+RAINC(I,J)              BMPARA.696
        RAINC(I,J)=0.5*((PSB(I,J)*1000.*PRECK*CPRLG)*100.)+RAINC(I,J)            BMPARA.697
        NBMD(I,J)=1                                                              BMPARA.698
        DO 550 L=LTPK,LB                                                         BMPARA.699
          TMOD(I,L)=DIFT(L)*FEFI/DT2                                             BMPARA.700
  550   QMOD(I,L)=DIFQ(L)*FEFI/DT2                                               BMPARA.701
C      PRINT *, 'AFTER DO 550; ICOND = ', ICOND                                  BMPARA.702
CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD         BMPARA.703
CDCDCDCDCDCDC  END OF DEEP CONVECTION  DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD         BMPARA.704
CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD         BMPARA.705
C-----------------------------------------------------------------------         BMPARA.706
  400 CONTINUE                                                                   BMPARA.707
C     PRINT *, 'AFTER LOOP 400'                                                  BMPARA.708
CVVVVVVVVVVVVV DIAGNOSTICS VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.709
      NDEEP=0                                                                    BMPARA.710
      DO 570 I=IBEG,IEND                                                         BMPARA.711
        LTPK=LTOP(I)                                                             BMPARA.712
        LBTK=LBOT(I)                                                             BMPARA.713
        PTPK=PRTOP(I)                                                            BMPARA.714
        PBTK=PBOT(I)                                                             BMPARA.715
C     LB  =KL-1                                                                  BMPARA.716
        IF(CLDHGT(I).GE.ZSH)THEN                                                 BMPARA.717
C     IF(PTPK.LT.PBTK-PSH)THEN    ! PSH=290 MB NDEEP=NDEEP+1                     BMPARA.718
          NDEEP=NDEEP+1                                                          BMPARA.719
          NDEPTH=LB-LTPK                                                         BMPARA.720
          NTOPD(LTPK)=NTOPD(LTPK)+1                                              BMPARA.721
          NBOTD(LB)=NBOTD(LB)+1                                                  BMPARA.722
          IF(NDEPTH.GT.0)NDPTHD(NDEPTH)=NDPTHD(NDEPTH)+1                         BMPARA.723
        ENDIF                                                                    BMPARA.724
  570 CONTINUE                                                                   BMPARA.725
      NNEG=KHDEEP-NDEEP                                                          BMPARA.726
C      PRINT *,'NO. OF SWAPPED POINTS, FINAL DEEP: ',NSWAP,NDEEP                 BMPARA.727
CAAAAAAAAAAAAA DIAGNOSTICS AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.728
C--------------GATHER SHALLOW CONVECTION POINTS-------------------------         BMPARA.729
      KHSHAL=0                                                                   BMPARA.730
      NDSTN=0                                                                    BMPARA.731
      NDSTP=0                                                                    BMPARA.732
CDIR$ NOVECTOR                                                                   BMPARA.733
      DO 560 I=IBEG,IEND                                                         BMPARA.734
C  IF CLOUD DEPTH < 90 OR < 2 SIGMA LAYERS NO SHALLOW CONVECTION                 BMPARA.735
        IF(CLDHGT(I).LT.ZNO.OR.LTOP(I).GT.LBOT(I)-2)GOTO 560                     BMPARA.736
C     IF(CLDHGT(I).LT.ZNO)GO TO 560                                              BMPARA.737
C     IF(PRTOP(I).GT.PBOT(I)-PNO.OR.LTOP(I).GT.LBOT(I)-2)GO TO 560               BMPARA.738
        IF(CLDHGT(I).LT.ZSH)THEN                                                 BMPARA.739
C     IF(PRTOP(I).GE.PBOT(I)-PSH)THEN  ! 90 MB < CLD DEPTH < 290 MB              BMPARA.740
          KHSHAL=KHSHAL+1                                                        BMPARA.741
#ifndef MPP1                                                                     BMPARA.742
          KSHAL(KHSHAL)=I                                                        BMPARA.743
#else                                                                            BMPARA.744
          KSHAL(KHSHAL)=FLIC_L2G_M(I)                                            BMPARA.745
#endif                                                                           BMPARA.746
        ENDIF                                                                    BMPARA.747
  560 CONTINUE                                                                   BMPARA.748
CDIR$ VECTOR                                                                     BMPARA.749
C************* HORIZONTAL LOOP FOR SHALLOW CONVECTION ******************         BMPARA.750
C***********************************************************************         BMPARA.751
C      PRINT*,'AT J =',J,' NUMBER OF SHALLOW CONVECTIVE POINTS=',                BMPARA.752
C     *KHSHAL                                                                    BMPARA.753
CSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCS         BMPARA.754
CSCSCSCSCSCSC  SHALLOW CONVECTION  CSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSC         BMPARA.755
CSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCS         BMPARA.756
C-----------------------------------------------------------------------         BMPARA.757
      DO 600 N=1,KHSHAL                                                          BMPARA.758
        I=KSHAL(N)                                                               BMPARA.759
C         PZ0= PSB(I,J)*1000.+PT       ! NOT USED!                               BMPARA.760
        DO 610 K=1,KL                                                            BMPARA.761
          TKL=T(I,K)                                                             BMPARA.762
          TK(K)=TKL                                                              BMPARA.763
          TREFK(K)=TKL                                                           BMPARA.764
          QKL=Q(I,K)                                                             BMPARA.765
          QK(K)=QKL                                                              BMPARA.766
          QREFK(K)=QKL                                                           BMPARA.767
          QSATK(K)=QKL                                                           BMPARA.768
          PKL=A(K)*PSB(I,J)*1000.+PT+PP3D(I,J,K)                                 BMPARA.769
          PK(K)=PKL                                                              BMPARA.770
          APEKL=APE(I,K)                                                         BMPARA.771
CVVVVVVVVVVVV CHOOSE THE PRESSURE FUNCTION VVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.772
C         FPK  (L) =ALOG(PKL)                                                    BMPARA.773
C         FPK  (L) =PKL                                                          BMPARA.774
C         FPK  (L) =-1./PKL                                                      BMPARA.775
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.776
          APEK(K)=APEKL                                                          BMPARA.777
          THVMKL=TKL*APEKL*(QKL*D608+H1)                                         BMPARA.778
          THVMOD(K)=THVMKL                                                       BMPARA.779
          THVREF(K)=THVMKL                                                       BMPARA.780
C... FOR KDP CALCULATION                                                         BMPARA.781
          PDP(K)=PK(K)-PMN                                                       BMPARA.782
  610   CONTINUE                                                                 BMPARA.783
C                                                                                BMPARA.784
C...FIND KDP...KDP(K) IS THE MODEL LEVEL CLOSEST TO 65 MB (PMN) ABOVE K;         BMPARA.785
C...THIS IS THE DEPTH OVER WHICH RELATIVE HUMIDITY DROP IS MEASURED TO           BMPARA.786
C...ESTIMATE SHALLOW CLOUD TOP... SEE DO 545...                                  BMPARA.787
C                                                                                BMPARA.788
        DO 106 KK=KL,1,-1                                                        BMPARA.789
          PFLAG=ABS(PK(KL)-PDP(KK))                                              BMPARA.790
          DO 105 K=KL-1,1,-1                                                     BMPARA.791
            PDIFFK=ABS(PK(K)-PDP(KK))                                            BMPARA.792
            IF(PDIFFK.LT.PFLAG)THEN                                              BMPARA.793
              PFLAG=PDIFFK                                                       BMPARA.794
              IF(KK.EQ.K)THEN                                                    BMPARA.795
                KDP(KK)=K-1                                                      BMPARA.796
              ELSE                                                               BMPARA.797
                KDP(KK)=K                                                        BMPARA.798
              ENDIF                                                              BMPARA.799
            ENDIF                                                                BMPARA.800
  105     CONTINUE                                                               BMPARA.801
          KDP(KK)=MAX(1,KDP(KK))                                                 BMPARA.802
C         PRINT *,'KK, KDP =',KK,KDP(KK)                                         BMPARA.803
  106   CONTINUE                                                                 BMPARA.804
C       IF(KK.LT.1)STOP 'KK'                                                     BMPARA.805
C--------------SEARCH FOR SHALLOW CLOUD TOP-----------------------------         BMPARA.806
        LBTK=LBOT(I)                                                             BMPARA.807
        LTSH=LBTK                                                                BMPARA.808
        LBM1=LBTK-1                                                              BMPARA.809
        PBTK=PK(LBTK)                                                            BMPARA.810
        ZTOP=Z0(I,LBOT(I))+ZSH-0.000001                                          BMPARA.811
C         PTPK=PBTK-PSH                                                          BMPARA.812
C--------------CLOUD TOP IS LEVEL JUST ABOVE PBTK-PSH ------------------         BMPARA.813
        DO 612 L=1,KL                                                            BMPARA.814
C              IF (PK(L).LE.PTPK) LTPK=L-1                                       BMPARA.815
C              IF (PK(L).LE.PTPK) LTPK=L                                         BMPARA.816
          IF(Z0(I,L).GE.ZTOP)LTPK=L                                              BMPARA.817
  612   CONTINUE                                                                 BMPARA.818
        PTPK=PK(LTPK)                                                            BMPARA.819
C--------------HIGHEST LEVEL ALLOWED IS LEVEL JUST BELOW PSHU-----------         BMPARA.820
        IF(PTPK.LE.PSHU)THEN                                                     BMPARA.821
          DO 615 L=1,KL                                                          BMPARA.822
            IF(PK(L).LE.PSHU)LSHU=L+1                                            BMPARA.823
  615     CONTINUE                                                               BMPARA.824
          LTPK=LSHU                                                              BMPARA.825
          PTPK=PK(LTPK)                                                          BMPARA.826
        ENDIF                                                                    BMPARA.827
        LTP1=LTPK+1                                                              BMPARA.828
        LTP2=LTPK+2                                                              BMPARA.829
C--------------NO SHALLOW CONVECTION IN UNSTABLE ENVIRONMENT------------         BMPARA.830
C             DO 541 L=LTP2,LBTK                                                 BMPARA.831
C         IF(THVMOD(L).GT.THVMOD(L-1))    THEN                                   BMPARA.832
C             LTOP(K)=LBOT(K)                                                    BMPARA.833
C             GO TO 600                                                          BMPARA.834
C         ENDIF                                                                  BMPARA.835
C541      CONTINUE                                                               BMPARA.836
C-----------------------------------------------------------------------         BMPARA.837
        DO 540 L=LTPK,LBTK                                                       BMPARA.838
C          ES=CVMGT(ALIQ*EXP((BLIQ*TK(L)-CLIQ)/(TK(L)-DLIQ)),                    BMPARA.839
C     *   AICE*EXP((BICE*TK(L)-CICE)/(TK(L)-DICE)),L.GE.ML(I))                   BMPARA.840
          IF(L.GE.ML(I))THEN                                                     BMPARA.841
            ES=ALIQ*EXP((BLIQ*TK(L)-CLIQ)/(TK(L)-DLIQ))                          BMPARA.842
          ELSE                                                                   BMPARA.843
            ES=AICE*EXP((BICE*TK(L)-CICE)/(TK(L)-DICE))                          BMPARA.844
          ENDIF                                                                  BMPARA.845
          QSATK(L)=0.622*ES/(PK(L)-ES)                                           BMPARA.846
C          QSATK(L)=PQ0/PK(L)*EXP(A2*(TK(L)-A3)/(TK(L)-A4))                      BMPARA.847
CVVVVVVVVVVVV CHOOSE THE PRESSURE FUNCTION VVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.848
C         FPK  (L)=QSATK(L)                                                      BMPARA.849
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.850
  540   CONTINUE                                                                 BMPARA.851
C-----------------------------------------------------------------------         BMPARA.852
C         RHH=QK(LTPK)/QSATK(LTPK)                                               BMPARA.853
C       PRINT *,'BEFORE DO 545; I, J, LBOT, LTOP =',I,J,                         BMPARA.854
C     *LBOT(I),LTOP(I)                                                           BMPARA.855
C       PRINT *,'CLDHGT =',CLDHGT(I)                                             BMPARA.856
        RHDMX=RHF                                                                BMPARA.857
        DO 545 L=LTP1,LBM1                                                       BMPARA.858
          RHL=QK(L)/QSATK(L)                                                     BMPARA.859
C           IF(KDP(L).LT.LTPK.OR.KDP(INEST,L).GT.LBTK)THEN                       BMPARA.860
C             QSATK(L)=PQ0/PK(L)*EXP(A2*(TK(L)-A3)/(TK(L)-A4))                   BMPARA.861
C           ENDIF                                                                BMPARA.862
          RHH=QK(KDP(L))/QSATK(KDP(L))                                           BMPARA.863
          IF(RHH+RHF.LT.RHL)LTSH=L                                               BMPARA.864
C           IF(RHH+RHF.LT.RHL)THEN                                               BMPARA.865
C             RHDMX=AMAX1(RHL-RHH,RHDMX)                                         BMPARA.866
C             IF(RHDMX.EQ.RHL-RHH)LTSH=KDP(L)                                    BMPARA.867
C           ENDIF                                                                BMPARA.868
C       PRINT *,'IN DO 545; L, RHL, KDP, RHH =',                                 BMPARA.869
C     *          L,RHL,KDP(L),RHH                                                BMPARA.870
  545   CONTINUE                                                                 BMPARA.871
C                                                                                BMPARA.872
        LTOP(I)=LTSH                                                             BMPARA.873
        PRTOP(I)=PK(LTSH)                                                        BMPARA.874
        LTP1=LTSH                                                                BMPARA.875
        LTPK=LTSH-1                                                              BMPARA.876
        CLDHGT(I)=Z0(I,LTOP(I))-Z0(I,LBOT(I))                                    BMPARA.877
C       PRINT *,'AFTER DO 545; LTOP, CLDHGT =',LTOP(I),CLDHGT(I)                 BMPARA.878
C       IF(J.GT.2)STOP 'CLDHGT'                                                  BMPARA.879
C-----------------------------------------------------------------------         BMPARA.880
C  IF CLOUD IS NOT AT LEAST 90 MB OR 3 SIGMA LAYERS DEEP, THEN NO CLOUD          BMPARA.881
        IF(CLDHGT(I).LT.ZNO.OR.LTOP(I).GT.LBOT(I)-2)THEN                         BMPARA.882
C         IF(CLDHGT(I).LT.ZNO)THEN                                               BMPARA.883
C         IF(PRTOP(I).GT.PBOT(I)-PNO.OR.LTOP(I).GT.LBOT(I)-2)THEN                BMPARA.884
          LTOP(I)=LBOT(I)                                                        BMPARA.885
          PRTOP(I)=PBOT(I)                                                       BMPARA.886
C          PRINT *,'CLDHGT(I).LT.ZN0 .OR. LTOP.GT.LBOT-2...'                     BMPARA.887
          GOTO 600                                                               BMPARA.888
        ENDIF                                                                    BMPARA.889
C--------------SCALING POTENTIAL TEMPERATURE & TABLE INDEX AT TOP-------         BMPARA.890
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.891
        THTPK=T(I,LTP1)*APE(I,LTP1)                                              BMPARA.892
C        THTPK=T(I,LTPK)*APE(I,LTPK)                                             BMPARA.893
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.894
        PKL=A(LTP1)*PSB(I,J)*1000.+PT+PP3D(I,J,LTP1)                             BMPARA.895
        EE=PKL*Q(I,LTP1)/(0.622+Q(I,LTP1))                                       BMPARA.896
        TDPT=1./(D273-RV/ELWV*ALOG(EE/611.))                                     BMPARA.897
        TDPT=AMIN1(TDPT,T(I,LTP1))                                               BMPARA.898
        TLCL=TDPT-(.212+1.571E-3*(TDPT-T0)-4.36E-4*(T(I,LTP1)-T0))*(             BMPARA.899
     +       T(I,LTP1)-TDPT)                                                     BMPARA.900
        PTPK=H10E5*(THTPK/TLCL)**CPORNG                                          BMPARA.901
C         PRINT *,'T, TDPT, TLCL, P, PLCL =',T(I,LTP1),TDPT,TLCL,PKL,            BMPARA.902
C     *PTPK                                                                      BMPARA.903
        DPMIX=PTPK-PSP(I)                                                        BMPARA.904
        IF(ABS(DPMIX).LT.H3000)DPMIX=-H3000                                      BMPARA.905
C--------------TEMPERATURE PROPFILE SLOPE-------------------------------         BMPARA.906
        SMIX=(THTPK-THBT(I))/DPMIX*STABS                                         BMPARA.907
C        PRINT *,'THTPK, THBT, DPMIX, SMIX  =',                                  BMPARA.908
C     *          THTPK,THBT(I),DPMIX,SMIX                                        BMPARA.909
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.910
        DO 620 L=LTP1,LBTK                                                       BMPARA.911
          IVI=LTP1+LBTK-L                                                        BMPARA.912
C         LBM1=LBTK-1                                                            BMPARA.913
C         TREFK(LBTK)=THVMOD(LBTK+1)/((QK(LBTK)*D608+1.)*APEK(LBTK))             BMPARA.914
C             DO 620 L=LTP1,LBM1                                                 BMPARA.915
C             IVI=LTP1+LBM1-L                                                    BMPARA.916
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.917
          TREFK(IVI)=((PK(IVI)-PK(IVI+1))*SMIX+TREFK(IVI+1)*APEK(IVI+1))         BMPARA.918
     +               /APEK(IVI)                                                  BMPARA.919
          DPKTS=(PK(IVI)-PK(IVI+1))*SMIX                                         BMPARA.920
          THIVI=TREFK(IVI+1)*APEK(IVI+1)                                         BMPARA.921
          SUMMM=DPKTS+THIVI                                                      BMPARA.922
          SUMMD=SUMMM/APEK(IVI)                                                  BMPARA.923
C       PRINT *,'DPKTS, THIVIP1, SUM, SUMD =',DPKTS,THIVI,SUMMM,SUMMD            BMPARA.924
C       PRINT *,'IVI, PK, PK(IVI+1), TREFK(IVI+1), TREF(IVI) =',                 BMPARA.925
C     * IVI,PK(IVI),PK(IVI+1),TREFK(IVI+1),TREF(IVI)                             BMPARA.926
C      PRINT *,'APEK(IVI+1), APEK(IVI) =',                                       BMPARA.927
C     *        APEK(IVI+1),APEK(IVI)                                             BMPARA.928
  620   CONTINUE                                                                 BMPARA.929
C--------------TEMPERATURE REFERENCE PROFILE CORRECTION-----------------         BMPARA.930
        SUMDT=D00                                                                BMPARA.931
        SUMDP=D00                                                                BMPARA.932
        DO 630 L=LTP1,LBTK                                                       BMPARA.933
          SUMDT=(TK(L)-TREFK(L))*DSIGMA(L)+SUMDT                                 BMPARA.934
          SUMDP=SUMDP+DSIGMA(L)                                                  BMPARA.935
  630   CONTINUE                                                                 BMPARA.936
C                                                                                BMPARA.937
        RDPSUM=1./SUMDP                                                          BMPARA.938
        FPK(LBTK)=TREFK(LBTK)                                                    BMPARA.939
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.940
        TCORR=SUMDT*RDPSUM                                                       BMPARA.941
        DO 640 L=LTP1,LBTK                                                       BMPARA.942
C         TCORR=SUMDT/(SUMDP-DSIGMA(LBTK))                                       BMPARA.943
C             DO 640 L=LTP1,LBM1                                                 BMPARA.944
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.945
          TRFKL=TREFK(L)+TCORR                                                   BMPARA.946
          TREFK(L)=TRFKL                                                         BMPARA.947
          FPK(L)=TRFKL                                                           BMPARA.948
  640   CONTINUE                                                                 BMPARA.949
C--------------HUMIDITY PROFILE EQUATIONS-------------------------------         BMPARA.950
        PSUM=0.0                                                                 BMPARA.951
        QSUM=0.0                                                                 BMPARA.952
        POTSUM=0.0                                                               BMPARA.953
        QOTSUM=0.0                                                               BMPARA.954
        OTSUM=0.0                                                                BMPARA.955
        DST=0.0                                                                  BMPARA.956
        FPTK=FPK(LTP1)                                                           BMPARA.957
        DO 642 L=LTP1,LBTK                                                       BMPARA.958
          DPKL=FPK(L)-FPTK                                                       BMPARA.959
          PSUM=DPKL*DSIGMA(L)+PSUM                                               BMPARA.960
          QSUM=QK(L)*DSIGMA(L)+QSUM                                              BMPARA.961
          RTBAR=2./(TREFK(L)+TK(L))                                              BMPARA.962
          OTSUM=DSIGMA(L)*RTBAR+OTSUM                                            BMPARA.963
          POTSUM=DPKL*RTBAR*DSIGMA(L)+POTSUM                                     BMPARA.964
          QOTSUM=QK(L)*RTBAR*DSIGMA(L)+QOTSUM                                    BMPARA.965
          DST=(TREFK(L)-TK(L))*RTBAR*DSIGMA(L)+DST                               BMPARA.966
  642   CONTINUE                                                                 BMPARA.967
C                                                                                BMPARA.968
        PSUM=PSUM*RDPSUM                                                         BMPARA.969
        QSUM=QSUM*RDPSUM                                                         BMPARA.970
        ROTSUM=1./OTSUM                                                          BMPARA.971
        POTSUM=POTSUM*ROTSUM                                                     BMPARA.972
        QOTSUM=QOTSUM*ROTSUM                                                     BMPARA.973
        DST=DST*ROTSUM*CP/ELWV                                                   BMPARA.974
C--------------ENSURE POSITIVE ENTROPY CHANGE---------------------------         BMPARA.975
        IF(DST.GT.0.)THEN                                                        BMPARA.976
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.977
C             DSTQ=DST*EPSUP                                                     BMPARA.978
          PRTOP(I)=PBOT(I)                                                       BMPARA.979
          LTOP(I)=LBOT(I)                                                        BMPARA.980
          NDSTP=NDSTP+1                                                          BMPARA.981
C           PRINT *,'DST.GT.0...'                                                BMPARA.982
          GOTO 600                                                               BMPARA.983
        ELSE                                                                     BMPARA.984
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.985
          DSTQ=DST*EPSDN                                                         BMPARA.986
        ENDIF                                                                    BMPARA.987
C--------------CHECK FOR ISOTHERMAL ATMOSPHERE--------------------------         BMPARA.988
        DEN=POTSUM-PSUM                                                          BMPARA.989
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.990
        IF(-DEN/PSUM.LT.0.00005)THEN                                             BMPARA.991
          LTOP(I)=LBOT(I)                                                        BMPARA.992
          PRTOP(I)=PBOT(I)                                                       BMPARA.993
C          PRINT *,'-DEN/PSUM .LT. 0.00005'                                      BMPARA.994
          GOTO 600                                                               BMPARA.995
        ELSE                                                                     BMPARA.996
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.997
C--------------SLOPE OF THE REFERENCE HUMIDITY PROFILE------------------         BMPARA.998
          DQREF=(QOTSUM-DSTQ-QSUM)/DEN                                           BMPARA.999
        ENDIF                                                                    BMPARA.1000
C--------------HUMIDITY DOESN`T INCREASE WITH HEIGHT--------------------         BMPARA.1001
        IF(DQREF.LT.0.0)THEN                                                     BMPARA.1002
          LTOP(I)=LBOT(I)                                                        BMPARA.1003
          PRTOP(I)=PBOT(I)                                                       BMPARA.1004
C      PRINT *,'DQREF.LT.0....'                                                  BMPARA.1005
          GOTO 600                                                               BMPARA.1006
        ENDIF                                                                    BMPARA.1007
C--------------HUMIDITY AT THE CLOUD TOP--------------------------------         BMPARA.1008
        QRFTP=QSUM-DQREF*PSUM                                                    BMPARA.1009
C--------------HUMIDITY PROFILE-----------------------------------------         BMPARA.1010
        DO 644 L=LTP1,LBTK                                                       BMPARA.1011
          QRFKL=(FPK(L)-FPTK)*DQREF+QRFTP                                        BMPARA.1012
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.1013
C--------------SUPERSATURATION NOT ALLOWED------------------------------         BMPARA.1014
          QNEW=(QRFKL-QK(L))*TAUK+QK(L)                                          BMPARA.1015
          IF(QNEW.GT.QSATK(L)*STRESH)THEN                                        BMPARA.1016
            LTOP(I)=LBOT(I)                                                      BMPARA.1017
            PRTOP(I)=PBOT(I)                                                     BMPARA.1018
C      PRINT *,'QNEW.GT.QSATK(L)*STRESH...'                                      BMPARA.1019
            GOTO 600                                                             BMPARA.1020
          ENDIF                                                                  BMPARA.1021
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.1022
C-----------------------------------------------------------------------         BMPARA.1023
          THVREF(L)=TREFK(L)*APEK(L)*(QRFKL*D608+H1)                             BMPARA.1024
          QREFK(L)=QRFKL                                                         BMPARA.1025
  644   CONTINUE                                                                 BMPARA.1026
C--------------ELIMINATE IMPOSSIBLE SLOPES (BETTS, DTHETA/DQ)-----------         BMPARA.1027
        DO 650 L=LTP1,LBTK                                                       BMPARA.1028
          DTDETA=(THVREF(L-1)-THVREF(L))/(A(L)-A(L-1))                           BMPARA.1029
          IF(DTDETA.LT.EPSTH)THEN                                                BMPARA.1030
            LTOP(I)=LBOT(I)                                                      BMPARA.1031
            PRTOP(I)=PBOT(I)                                                     BMPARA.1032
C       PRINT *,'DTDEAT.LT.EPSTH...'                                             BMPARA.1033
C       PRINT *,'L, THVREF, THVREF(L-1), A, A(L-1) =',                           BMPARA.1034
C     * L,THVREF(L),THVREF(L-1),A(L),A(L-1)                                      BMPARA.1035
C       IF(J.EQ.12.AND. I.EQ.59)STOP 'TREF'                                      BMPARA.1036
            GOTO 600                                                             BMPARA.1037
          ENDIF                                                                  BMPARA.1038
  650   CONTINUE                                                                 BMPARA.1039
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.1040
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.1041
CVVVVVVVVVVVVV DIAGNOSTICS VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.1042
        IF(DST.GT.0.)THEN                                                        BMPARA.1043
          NDSTP=NDSTP+1                                                          BMPARA.1044
        ELSE                                                                     BMPARA.1045
          NDSTN=NDSTN+1                                                          BMPARA.1046
        ENDIF                                                                    BMPARA.1047
        DENTPY=D00                                                               BMPARA.1048
        DO 660 L=LTP1,LBTK                                                       BMPARA.1049
  660   DENTPY=((TREFK(L)-TK(L))*CP+(QREFK(L)-QK(L))*ELWV)/(TK(L)+               BMPARA.1050
     +         TREFK(L))*DSIGMA(L)+DENTPY                                        BMPARA.1051
CAAAAAAAAAAAAA DIAGNOSTICS AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.1052
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.1053
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.1054
C--------------RELAXATION TOWARDS REFERENCE PROFILES--------------------         BMPARA.1055
        ICONSS=ICONSS+1                                                          BMPARA.1056
        NBMS(I,J)=1                                                              BMPARA.1057
        DO 670 L=LTP1,LBTK                                                       BMPARA.1058
          TMOD(I,L)=(TREFK(L)-TK(L))/TREL                                        BMPARA.1059
          QMOD(I,L)=(QREFK(L)-QK(L))/TREL                                        BMPARA.1060
  670   CONTINUE                                                                 BMPARA.1061
C       PRINT *,'AFTER DO 670; ICONSS =',ICONSS                                  BMPARA.1062
CSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCS         BMPARA.1063
CSCSCSCSCSCSC  END OF SHALLOW CONVECTION   SCSCSCSCSCSCSCSCSCSCSCSCSCSCS         BMPARA.1064
CSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCS         BMPARA.1065
C-----------------------------------------------------------------------         BMPARA.1066
  600 CONTINUE                                                                   BMPARA.1067
C     PRINT *, 'AFTER LOOP 600'                                                  BMPARA.1068
CVVVVVVVVVVVVV DIAGNOSTICS VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV         BMPARA.1069
      NSHAL=0                                                                    BMPARA.1070
      DO 680 I=IBEG,IEND                                                         BMPARA.1071
        LTPK=LTOP(I)                                                             BMPARA.1072
        LBTK=LBOT(I)                                                             BMPARA.1073
        PTPK=PRTOP(I)                                                            BMPARA.1074
        PBTK=PBOT(I)                                                             BMPARA.1075
C NO SHALLOW CONVECTION IF CLOUD IS NOT AT LEAST 90 MB OR 3 SIGMA                BMPARA.1076
C LAYERS DEEP                                                                    BMPARA.1077
C     IF(CLDHGT(I).LT.ZNO.OR.LTOP(I).GT.LBOT(I)-2)GO TO 680                      BMPARA.1078
        IF(CLDHGT(I).LT.ZNO)GOTO 680                                             BMPARA.1079
C     IF(PRTOP(I).GT.PBOT(I)-PNO.OR.LTOP(I).GT.LBOT(I)-2)GOTO 680                BMPARA.1080
C SHALLOW CONVECTION IF CLOUD IS .LE. 290 MB                                     BMPARA.1081
        IF(CLDHGT(I).LT.ZSH)THEN                                                 BMPARA.1082
C     IF(PTPK.GE.PBTK-PSH)THEN                                                   BMPARA.1083
          NSHAL=NSHAL+1                                                          BMPARA.1084
          NTOPS(LTPK)=NTOPS(LTPK)+1                                              BMPARA.1085
          NBOTS(LBTK)=NBOTS(LBTK)+1                                              BMPARA.1086
          NDEPTH=LBTK-LTPK                                                       BMPARA.1087
          IF(NDEPTH.GT.0)NDPTHS(NDEPTH)=NDPTHS(NDEPTH)+1                         BMPARA.1088
        ENDIF                                                                    BMPARA.1089
  680 CONTINUE                                                                   BMPARA.1090
C     PRINT *,'AT J =',J,', FINAL NO. SHALLOW CONV POINTS =',NSHAL               BMPARA.1091
      NEGDS=KHSHAL-NSHAL                                                         BMPARA.1092
CAAAAAAAAAAAAA DIAGNOSTICS AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA         BMPARA.1093
C--------------SMOOTHING TEMPERATURE & HUMIDITY CORRECTIONS-------------         BMPARA.1094
C                            DO 750 K=1,KL                                       BMPARA.1095
C-----------------------------------------------------------------------         BMPARA.1096
C          DO 710 I=IBEG,IEND                                                    BMPARA.1097
C      TL(I)=TMOD(I,K)                                                           BMPARA.1098
C 710  QL(I)=QMOD(I,K)                                                           BMPARA.1099
C-----------------------------------------------------------------------         BMPARA.1100
C      IF(KSMUD.GT.0)    THEN                                                    BMPARA.1101
C-----------------------------------------------------------------------         BMPARA.1102
C      NSMUD=KSMUD                                                               BMPARA.1103
C                             DO 740 KS=1,NSMUD                                  BMPARA.1104
C          DO 720 K=KHL00,KHH00                                                  BMPARA.1105
C      TNE(K)=(TL(K+KNE)-TL(K))                                                  BMPARA.1106
C 720  QNE(K)=(QL(K+KNE)-QL(K))                                                  BMPARA.1107
C          DO 730 K=IM   ,KHH00                                                  BMPARA.1108
C      TSE(K)=(TL(K+KSE)-TL(K))                                                  BMPARA.1109
C 730  QSE(K)=(QL(K+KSE)-QL(K))                                                  BMPARA.1110
C          DO 740 K=KHL02,KHH02                                                  BMPARA.1111
C      TL(K)=(TNE(K)-TNE(K+KSW)+TSE(K)-TSE(K+KNW))*HBM2(K)*D125+TL(K)            BMPARA.1112
C 740  QL(K)=(QNE(K)-QNE(K+KSW)+QSE(K)-QSE(K+KNW))*HBM2(K)*D125+QL(K)            BMPARA.1113
C-----------------------------------------------------------------------         BMPARA.1114
C      ENDIF                                                                     BMPARA.1115
C-----------------------------------------------------------------------         BMPARA.1116
C          DO 750 I=IBEG,IEND                                                    BMPARA.1117
C      T(I,L)=T(I,K)+TL(I)                                                       BMPARA.1118
C      Q(I,L)=Q(I,K)+QL(I)                                                       BMPARA.1119
C 750  CONTINUE                                                                  BMPARA.1120
C--------------SAVE CLOUD TOP AND BOTTOM FOR RADIATION------------------         BMPARA.1121
C          DO 800 I=IBEG,IEND                                                    BMPARA.1122
C      HTOP(I)=FLOAT(LTOP(I))                                                    BMPARA.1123
C 800  HBOT(I)=FLOAT(LBOT(I))                                                    BMPARA.1124
C-----------------------------------------------------------------------         BMPARA.1125
      RETURN                                                                     BMPARA.1126
      END                                                                        BMPARA.1127
                                                                                 BMPARA.1128
