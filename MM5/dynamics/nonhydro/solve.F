      SUBROUTINE SOLVE(IEXEC,INEST,NN)                                           SOLVE.1
      IMPLICIT NONE                                                              SOLVE.2
#include <defines.incl>                                                          SOLVE.3
#include <parame.incl>                                                           SOLVE.4
#include <addr0.incl>                                                            SOLVE.5
#include <parccm2.incl>                                                          SOLVE.6
#include <varia.incl>                                                            SOLVE.7
#include <param2.incl>                                                           SOLVE.8
#include <parfdda.incl>                                                          SOLVE.9
#include <param3.incl>                                                           SOLVE.10
#include <pmoist.incl>                                                           SOLVE.11
#include <point3d.incl>                                                          SOLVE.12
#include <point2d.incl>                                                          SOLVE.13
#include <pointbc.incl>                                                          SOLVE.14
#include <various.incl>                                                          SOLVE.15
#include <fddagd.incl>                                                           SOLVE.16
#include <fddaob.incl>                                                           SOLVE.17
#include <radiat.incl>                                                           SOLVE.18
#include <navypb.incl>                                                           SOLVE.19
#include <nonhyd.incl>                                                           SOLVE.20
#include <nonhydb.incl>                                                          SOLVE.21
#include <nhcnst.incl>                                                           SOLVE.22
#include <rpstar.incl>                                                           SOLVE.23
#include <addrcu.incl>                                                           SOLVE.24
#include <sum.incl>                                                              SOLVE.25
#include <fogstuf.incl>                                                          SOLVE.26
#include <addrfog.incl>                                                          SOLVE.27
#include <pbltke.incl>                                                           SOLVE.28
#include <landuse.incl>                                                          05DEC01.464
#include <soil.incl>                                                             07NOV00.992
#include <zdiffu.incl>                                                           23DEC04.297
#include <zdiffu2.incl>                                                          23DEC04.298
C                                                                                SOLVE.29
      INTEGER   IEXEC,INEST,NN,IS                                                05DEC01.465
C                                                                                23DEC04.299
      real cbt1 , cbt2 , cbt3 , cbt4                                             23DEC04.300
      real cbt5 , cbt6 , cbt7 , cbt8                                             23DEC04.301
C                                                                                SOLVE.31
      REAL      PR1(MIX,MJX,MKX),RHO1(MIX,MJX,MKX)                               SOLVE.32
C                                                                                SOLVE.33
      REAL      TCLD(MIX),RVAPOR(MIX),RCLD(MIX),PRESS(MIX)                       SOLVE.34
      REAL      PRECIP2D(MIX,MJX)                                                SOLVE.35
      REAL      PSURF(MIXV,MJXV),P(MIXV,MJXV,MKXV)                               SOLVE.36
      REAL      WMAX(KXP1)                                                       SOLVE.37
      REAL      TBP(MIXNH,MJXNH,MKXNH)                                           SOLVE.38
      REAL      CLDFRA3D(IPLOND,IPLATD,IPLEVP)                                   SOLVE.39
      REAL      CLDFRA(IPLOND,IPLEVP),FRLA(IPLOND),FRSA(IPLOND)                  SOLVE.40
      REAL      FIRTP(IPLOND),SABTP(IPLOND)                                      05DEC01.466
      REAL      QSATF(MIXFG,MKXFG),REGJ(MIX),ZNTJ(MIX)                           SOLVE.41
      REAL      TH(MIX,MJX,MKX),THTEN(MIX,MJX,MKX),THA(MIX,MJX,MKX)              SOLVE.42
      REAL      DIVX(MIX,MJX,MKX),CRSXK(MIX,MJX,MKX),DOTXK(MIX,MJX,MKX)          SOLVE.43
      REAL      PR(MIX,MKX),    WADOT(MIX,KXP1)                                  SOLVE.44
      REAL      DUMMY(MIX)                                                       SOLVE.45
      REAL      SCR1(MIX,MKX),SCR2(MIX,MKX),                                     SOLVE.46
     1          SCR3(MIX,MKX),SCR4(MIX,MKX)                                      SOLVE.47
      INTEGER   IMAX(KXP1),JMAX(KXP1)                                            SOLVE.48
      INTEGER   KPBLJ(MIX)                                                       SOLVE.49
      INTEGER   INDEX(MJX)                                                       SOLVE.50
C                                                                                SOLVE.51
C DIMENSION OF ARRAYS RELATED TO GSPBL ROUTINE                                   SOLVE.52
C                                                                                SOLVE.53
      REAL      TKETENDADV(MIXG,MJXG,MKXG+1)                                     SOLVE.54
      REAL      TKEC(MIXG,MJXG,MKXG+1)                                           SOLVE.55
      REAL      TKEAPSA(MIXG,MJXG,MKXG+1)                                        SOLVE.56
      REAL      TKETEND(MIXG,MJXG,MKXG)                                          SOLVE.57
      REAL      DTTKE                                                            SOLVE.58
C                                                                                SOLVE.59
C WAS DUSOLVE3.INCL                                                              SOLVE.60
C                                                                                SOLVE.61
      REAL PP3D,QC3D,QG3D,QI3D,QNC3D,QNI3D,QR3D,QV3D,T3D,U3D,V3D,W3D             SOLVE.62
      DIMENSION U3D(MIX,  MJX,  MKX),    V3D(MIX,  MJX,  MKX),                   SOLVE.63
     1          T3D(MIX,  MJX,  MKX),    W3D(MIX,  MJX,  MKX+1),                 SOLVE.64
     2         PP3D(MIX,  MJX,  MKX),   QV3D(MIX,  MJX,  MKX),                   SOLVE.65
     3         QC3D(MIXM, MJXM, MKXM),  QR3D(MIXM, MJXM, MKXM),                  SOLVE.66
     4         QI3D(MIXIC,MJXIC,MKXIC),QNI3D(MIXIC,MJXIC,MKXIC),                 SOLVE.67
     5         QG3D(MIXIG,MJXIG,MKXIG),QNC3D(MIXIG,MJXIG,MKXIG)                  SOLVE.68
C                                                                                SOLVE.69
      REAL QDOT                                                                  SOLVE.70
      DIMENSION QDOT(MIX,MJX,KXP1)                                               SOLVE.71
C                                                                                SOLVE.72
      REAL PP3DTEN,QC3DTEN,QG3DTEN,QI3DTEN,QNC3DTEN,QNI3DTEN,QR3DTEN,            SOLVE.73
     1     QV3DTEN,T3DTEN,U3DTEN,V3DTEN,W3DTEN                                   SOLVE.74
      DIMENSION U3DTEN(MIX,  MJX,  MKX),    V3DTEN(MIX,  MJX,  MKX),             SOLVE.75
     1          T3DTEN(MIX,  MJX,  MKX),    W3DTEN(MIX,  MJX,  MKX+1),           SOLVE.76
     2         PP3DTEN(MIX,  MJX,  MKX),   QV3DTEN(MIX,  MJX,  MKX),             SOLVE.77
     3         QC3DTEN(MIXM, MJXM, MKXM),  QR3DTEN(MIXM, MJXM, MKXM),            SOLVE.78
     4         QI3DTEN(MIXIC,MJXIC,MKXIC),QNI3DTEN(MIXIC,MJXIC,MKXIC),           SOLVE.79
     5         QG3DTEN(MIXIG,MJXIG,MKXIG),QNC3DTEN(MIXIG,MJXIG,MKXIG)            SOLVE.80
C+---+-----------------------------------------------------------------+         19DEC02.1230
C+---+-----------------------------------------------------------------+         19DEC02.1231
#undef CALL_EVERY_OTHER_TSTEP 
C+---+-----------------------------------------------------------------+         19DEC02.1233
#if defined (CALL_EVERY_OTHER_TSTEP)                                            
C                                                                                19DEC02.1235
C..These variables are used for calling EXMOISG every other timestep.            19DEC02.1236
C                                                                                19DEC02.1237
      common/ex_ten/tex_ten,qvex_ten,qcex_ten,qrex_ten,qiex_ten,                 19DEC02.1238
     -     qniex_ten,qgex_ten,qncex_ten                                          19DEC02.1239
      real tex_ten(MIX, MJX, MKX, MAXNES)                                        19DEC02.1240
     +    ,qvex_ten(MIX, MJX, MKX, MAXNES)                                       19DEC02.1241
     +    ,qcex_ten(MIXM, MJXM, MKXM, MAXNES)                                    19DEC02.1242
     +    ,qrex_ten(MIXM, MJXM, MKXM, MAXNES)                                    19DEC02.1243
     +    ,qiex_ten(MIXIC, MJXIC, MKXIC, MAXNES)                                 19DEC02.1244
     +    ,qniex_ten(MIXIC, MJXIC, MKXIC, MAXNES)                                19DEC02.1245
     +    ,qgex_ten(MIXIG, MJXIG, MKXIG, MAXNES)                                 19DEC02.1246
     +    ,qncex_ten(MIXIG, MJXIG, MKXIG, MAXNES)                                19DEC02.1247
#endif                                                                           19DEC02.1248
C+---+-----------------------------------------------------------------+         19DEC02.1249
C                                                                                SOLVE.81
      REAL UCC,UCD,VCC,VCD                                                       SOLVE.82
      DIMENSION UCC(MIX,  MJX,  MKX),    VCC(MIX,  MJX,  MKX),                   SOLVE.83
     1          UCD(MIX,  MJX,  MKX),    VCD(MIX,  MJX,  MKX)                    SOLVE.84
                                                                                 SOLVE.85
#ifdef MPP1                                                                      SOLVE.86
#  include <mpp_dusolve3.incl>                                                   SOLVE.87
#endif                                                                           SOLVE.88
                                                                                 SOLVE.89
C                                                                                SOLVE.90
C                                                                                SOLVE.91
C                                                                                SOLVE.92
      REAL      AEARTH, REARTH                                                   SOLVE.93
      DATA      AEARTH /6370.E3/                                                 SOLVE.94
C      DATA      AEARTH /6370.997E3/                                             07NOV00.993
C                                                                                SOLVE.95
      INTEGER   I,J,K,KK                                                         SOLVE.96
      INTEGER   IYY,JXX,KZZ,IYYM,JXXM,KZZM,IB2                                   SOLVE.97
      INTEGER   IBNES,IENES,JBNES,JENES,INDH,INDQ,IADVTKE,IFDDA                  SOLVE.98
      INTEGER   IST,IEN,JST,JEN                                                  07NOV00.994
      INTEGER   IBQ,IMAXP,JMAXP,KMAXP                                            SOLVE.99
      INTEGER   ID,NSTA                                                          SOLVE.100
      INTEGER   IUTRAD,NRADISP                                                   SOLVE.101
      REAL      PT,RHO0S,DIV,EXNER,CPM,TV,ROFAC,UAQ,VAQ,DIVD                     SOLVE.102
      REAL      AMFAC,DDJ,DTB,DTBC,EPSI                                          SOLVE.103
      REAL      QCC,QRC,QIC,QNIC,QGC,QNCC                                        SOLVE.104
      REAL      WTEST,WMAXP                                                      SOLVE.105
#ifdef MPP1                                                                      SOLVE.106
      REAL      DUMJ(MJX)                                                        SOLVE.107
#  include <mpp_solve_00.incl>                                                   SOLVE.108
#endif                                                                           SOLVE.109
      REAL      FDTIM                                                            SOLVE.110
      REAL      TGSTOR(MIX,MJX)                                                  19DEC02.1250
      real      tposdz,snowfrac                                                  05DEC01.467
      INTEGER   KVEG                                                             05DEC01.468
      REAL      VEGFRQ                                                           05DEC01.469
      INTEGER   ICUT                                                             07NOV00.995
      REAL      INTERIOR_MASK(MIX,MJX)                                           07NOV00.996
                                                                                 07NOV00.997
#ifdef VECTOR                                                                    07NOV00.998
#  include <vector_solve_temps.incl>                                             07NOV00.999
#endif                                                                           07NOV00.1000
      real deltaz(mix,mjx,mkx-2:mkx),rmz,rmzf,qdm                                23DEC04.303
      real absv(MIX,MJX),fin(MIX,MJX),fjn(MIX,MJX),lam(MIX,MJX),                 23DEC04.304
     &  thgb(MIX,MJX)                                                            23DEC04.305
      integer kmvl,in,tadv,qadv,cwadv                                            23DEC04.306
      real dz3d(mix,mjx,mkx)                                                     23DEC04.307
C                                                                                23DEC04.308
      common /zdiffpar/ deltaz,dz3d                                              23DEC04.309
C                                                                                SOLVE.111
      REARTH=1.0/AEARTH                                                          SOLVE.112
      IYY=IL                                                                     SOLVE.113
      JXX=JL                                                                     SOLVE.114
      KZZ=KL                                                                     SOLVE.115
      IYYM=1                                                                     SOLVE.116
      JXXM=1                                                                     SOLVE.117
      KZZM=1                                                                     SOLVE.118
C                                                                                SOLVE.119
#ifdef MPP1                                                                      SOLVE.120
#  include <mpp_solve_10.incl>                                                   SOLVE.121
#endif                                                                           SOLVE.122
      rmz = 0.25                                                                 23DEC04.310
      rmzf = 2.5e-3                                                              23DEC04.311
                                                                                 23DEC04.312
      if (inest.ne.oldnest .and. ITPDIF .eq. 2) then                             23DEC04.313
C_FLIC_RUNPAD(2)                                                                 23DEC04.314
C! As these fields are constant, they are computed only if domain has changed    23DEC04.315
C  only calculated for z=diffusion                                               23DEC04.316
                                                                                 23DEC04.317
c  Approximate height differences between sigma levels for z-diffusion           23DEC04.318
c$omp parallel do default(shared)                                                23DEC04.319
c$omp&private(i,j,k)                                                             23DEC04.320
      DO J=1,JLX                                                                 23DEC04.321
      do k=1,kl-1                                                                23DEC04.322
      DO I=1,ILX                                                                 23DEC04.323
      dz3d(i,j,k)=alog(PR0(i,j,k+1)/PR0(i,j,k))*rovg*                            23DEC04.324
     f  0.5*(T0(i,j,k)+T0(i,j,k+1))                                              23DEC04.325
      enddo                                                                      23DEC04.326
      enddo                                                                      23DEC04.327
      enddo                                                                      23DEC04.328
c                                                                                23DEC04.329
                                                                                 23DEC04.330
c$omp parallel do default(shared)                                                23DEC04.331
c$omp&private(i,j)                                                               23DEC04.332
      DO J=1,JLX                                                                 23DEC04.333
        DO I=1,ILX                                                               23DEC04.334
          dz3d(i,j,kl)=dz3d(i,j,kl-1)                                            23DEC04.335
          deltaz(i,j,kl-2) = dz3d(i,j,kl-3)+dz3d(i,j,kl-2)                       23DEC04.336
          deltaz(i,j,kl-1) = dz3d(i,j,kl-2)+dz3d(i,j,kl-1)                       23DEC04.337
          if (tdkorr.eq.1) then                                                  23DEC04.338
            deltaz(i,j,kl) = dz3d(i,j,kl-1)+(1-a(kl))*                           23DEC04.339
     &        rovg*T0(i,j,kl)                                                    23DEC04.340
          else if (tdkorr.eq.2) then                                             23DEC04.341
            deltaz(i,j,kl) = dz3d(i,j,kl-1)                                      23DEC04.342
          endif                                                                  23DEC04.343
        enddo                                                                    23DEC04.344
      enddo                                                                      23DEC04.345
                                                                                 23DEC04.346
C_FLIC_RUNPAD(0)                                                                 23DEC04.347
       endif                                                                     23DEC04.348
C                                                                                SOLVE.123
C --- DEFINE STARTING AND ENDING INDICES FOR PHYSICS CALLS                       SOLVE.124
C                                                                                SOLVE.125
      IB2=1                                                                      SOLVE.126
      IF(IMOIST(INEST).EQ.2)THEN                                                 SOLVE.127
        IYYM=IL                                                                  SOLVE.128
        IB2=2                                                                    SOLVE.129
        JXXM=JL                                                                  SOLVE.130
        KZZM=KL                                                                  SOLVE.131
      ENDIF                                                                      SOLVE.132
      IBNES=2                                                                    SOLVE.133
      IENES=ILX                                                                  SOLVE.134
      JBNES=2                                                                    SOLVE.135
      JENES=JLX                                                                  SOLVE.136
      IF(INEST.GT.1)THEN                                                         SOLVE.137
        IBNES=3                                                                  SOLVE.138
        IENES=ILXM                                                               SOLVE.139
        JBNES=3                                                                  SOLVE.140
        JENES=JLXM                                                               SOLVE.141
      ENDIF                                                                      SOLVE.142
C                                                                                SOLVE.143
C --- SET VERTICAL TEMPERATURE AND MOISTURE ADVECTION SWITCHES                   SOLVE.144
C                                                                                SOLVE.145
      INDH=1                                                                     SOLVE.146
      IF(IVTADV.EQ.1)INDH=6                                                      SOLVE.147
      INDQ=2                                                                     SOLVE.148
      IF(IVQADV.EQ.1)INDQ=6                                                      SOLVE.149
      IADVTKE=0                                                                  SOLVE.150
c  instability correction for advection                                          23DEC04.349
      if (ITADVM(INEST).EQ.0) then                                               23DEC04.350
         tadv = 1                                                                23DEC04.351
      else      ! correction is used for temperature                             23DEC04.352
         tadv = 5                                                                23DEC04.353
      endif                                                                      23DEC04.354
      if (IQADVM(INEST).EQ.0) then                                               23DEC04.355
         qadv = 1                                                                23DEC04.356
         cwadv = 1                                                               23DEC04.357
      else      ! correction is used for QV and CLW                              23DEC04.358
         qadv = 5                                                                23DEC04.359
         cwadv = 5                                                               23DEC04.360
      endif                                                                      23DEC04.361
      IFDDA=0                                                                    SOLVE.151
      IF(IFDDAG+IFDDAO.GE.1)IFDDA=1                                              SOLVE.152
C                                                                                SOLVE.153
C --- ZERO OUT W3DTEN AND                                                        SOLVE.154
C          QDOT ( VERTICAL SIGMA VELOCITY) AT TOP AND BOTTOM.                    SOLVE.155
C                                                                                SOLVE.156
C --- SAVE COUPLED U AND V IN UCD AND VCD.                                       SOLVE.157
C     UA AND VA THEREAFTER ARE COUPLED U AND V WITH MAP SCALE FACTOR.            SOLVE.158
C                                                                                SOLVE.159
C_FLIC_RUNPAD(2)                                                                 SOLVE.160
cmic$ do all autoscope                                                           SOLVE.161
c$doacross                                                                       SOLVE.162
c$& local(i,j,k)                                                                 SOLVE.163
c$omp parallel do default(shared)                                                SOLVE.164
c$omp&private(i,j,k)                                                             SOLVE.165
      DO J=1,JL                                                                  SOLVE.166
        DO I=1,ILX                                                               SOLVE.167
          QDOT(I,J,1)=0.                                                         SOLVE.168
          QDOT(I,J,KLP1)=0.                                                      SOLVE.169
          W3DTEN(I,J,KLP1)=0.                                                    SOLVE.170
        ENDDO                                                                    SOLVE.171
C                                                                                SOLVE.172
        ICON(J)=0                                                                SOLVE.173
C                                                                                SOLVE.174
CB This section of the code was moved into the new routine DCPL3DWND             23DEC04.362
CB This change was done purely for optimization                                  23DEC04.363
CB        DO K=1,KL                                                              23DEC04.364
CB          DO I=1,IL                                                            23DEC04.365
CB            UCD(I,J,K)=UA(I,J,K)                                               23DEC04.366
CB            VCD(I,J,K)=VA(I,J,K)                                               23DEC04.367
CB            UA(I,J,K)=UA(I,J,K)*MSFD(I,J)                                      23DEC04.368
CB            VA(I,J,K)=VA(I,J,K)*MSFD(I,J)                                      23DEC04.369
CB            W3DTEN(I,J,K)=0.                                                   23DEC04.370
CB          ENDDO                                                                23DEC04.371
CB        ENDDO                                                                  23DEC04.372
CB END                                                                           23DEC04.373
      ENDDO                                                                      SOLVE.184
C                                                                                24SEP99.339
C --- INITIALIZE CLOUD FRACTION ARRAY FOR CCM2 RADIATION OPTION                  24SEP99.340
C                                                                                24SEP99.341
      IF(IFRAD.EQ.3.OR.IFRAD.EQ.4)THEN                                           25JAN00.276
        IF(MOD(KTAU,NTRAD(INEST)).EQ.0)THEN                                      24SEP99.343
cmic$ do all autoscope                                                           24SEP99.344
c$doacross                                                                       24SEP99.345
c$& local(i,j,k)                                                                 24SEP99.346
c$omp parallel do default(shared)                                                24SEP99.347
c$omp&private(i,j,k)                                                             24SEP99.348
          DO J=1,JLX                                                             24SEP99.349
            DO K=1,KL                                                            24SEP99.350
              DO I=1,ILX                                                         24SEP99.351
                CLDFRA3D(I,J,K)=0.0                                              24SEP99.352
              ENDDO                                                              24SEP99.353
            ENDDO                                                                24SEP99.354
          ENDDO                                                                  24SEP99.355
        ENDIF                                                                    24SEP99.356
      ENDIF                                                                      24SEP99.357
C                                                                                SOLVE.185
CB New optimized optimized routine DCLP3DWND is called                           23DEC04.374
CB      CALL DCPL3DWND(IBOUDY(INEST),U3D,V3D,UCD,VCD,RPDOTA,IL,JL,KL,ILX,        23DEC04.375
CB     +     JLX)                                                                23DEC04.376
C --- Save coupled U and V in ucd AND VCD.                                       23DEC04.377
C     UA and VA thereafter are coupled U and V with map scale factor.            23DEC04.378
      CALL DCPL3DWND(IBOUDY(INEST),U3D,V3D,UCD,VCD,UA,VA,RPDOTA,MSFD,            23DEC04.379
     &              IL,JL,KL,ILX,JLX)                                            23DEC04.380
CB END                                                                           23DEC04.381
      CALL BDYTEN(U3DTEN,KZZ,IL,JL,INEST,1,USBT,UNBT,UEBT,UWBT)                  SOLVE.188
      CALL BDYTEN(V3DTEN,KZZ,IL,JL,INEST,1,VSBT,VNBT,VEBT,VWBT)                  SOLVE.189
C                                                                                SOLVE.190
      IF(INEST.EQ.1)THEN                                                         SOLVE.191
        IF(IMPHYS(1).GE.3)THEN                                                   SOLVE.192
          CALL BDYVAL(U3D,V3D,QCA,QRA,PSA,RPSA)                                  SOLVE.193
          IF(IMPHYS(1).GE.5)THEN                                                 SOLVE.194
            CALL BDYVAL(U3D,V3D,QIA,QNIA,PSA,RPSA)                               SOLVE.195
            IF(IMPHYS(1).GE.6)THEN                                               SOLVE.196
              CALL BDYVAL(U3D,V3D,QGA,QNCA,PSA,RPSA)                             SOLVE.197
            ENDIF                                                                SOLVE.198
          ENDIF                                                                  SOLVE.199
        ENDIF                                                                    SOLVE.200
      ENDIF                                                                      SOLVE.201
C                                                                                SOLVE.202
      CALL DCPL3D(T3D,TA,RPSA,ILX,JLX,KL,MKX)                                    SOLVE.203
      CALL DCPL3D(PP3D,PPA,RPSA,ILX,JLX,KL,MKX)                                  SOLVE.204
      CALL DCPL3D(W3D,WA,RPSA,ILX,JLX,KLP1,KXP1NH)                               SOLVE.205
C                                                                                SOLVE.206
      CALL BDYTEN(T3DTEN,KZZ,ILX,JLX,INEST,1,TSBT,TNBT,TEBT,TWBT)                SOLVE.207
      CALL BDYTEN(PP3DTEN,KZZ,ILX,JLX,INEST,1,PPSBT,PPNBT,PPEBT,PPWBT)           SOLVE.208
      CALL BDYTEN(W3DTEN,KZZ+1,ILX,JLX,INEST,1,WSBT,WNBT,WEBT,WWBT)              SOLVE.209
      IF(INEST.GT.1)THEN                                                         SOLVE.210
        CALL BDYTEN(W3D,KZZ+1,ILX,JLX,INEST,4,WSB,WNB,WEB,WWB)                   SOLVE.211
      ENDIF                                                                      SOLVE.212
C                                                                                SOLVE.213
      CALL DCPL3D(QV3D,QVA,RPSA,ILX,JLX,KL,MKX)                                  SOLVE.214
      CALL BDYTEN(QV3DTEN,KZZ,ILX,JLX,INEST,1,QSBT,QNBT,QEBT,QWBT)               SOLVE.215
      IF(IEXMS.EQ.1)THEN                                                         SOLVE.216
        CALL DCPL3D(QC3D,QCA,RPSA,ILX,JLX,KL,MKX)                                SOLVE.217
        CALL DCPL3D(QR3D,QRA,RPSA,ILX,JLX,KL,MKX)                                SOLVE.218
        CALL BDYTEN(QC3DTEN,KZZ,ILX,JLX,INEST,IBMOIST,QCSBT,QCNBT,QCEBT,         SOLVE.219
     +       QCWBT)                                                              SOLVE.220
        CALL BDYTEN(QR3DTEN,KZZ,ILX,JLX,INEST,IBMOIST,QRSBT,QRNBT,QREBT,         SOLVE.221
     +       QRWBT)                                                              SOLVE.222
        IF(IICE.EQ.1)THEN                                                        SOLVE.223
          CALL DCPL3D(QI3D,QIA,RPSA,ILX,JLX,KL,MKX)                              SOLVE.224
          CALL DCPL3D(QNI3D,QNIA,RPSA,ILX,JLX,KL,MKX)                            SOLVE.225
          CALL BDYTEN(QI3DTEN,KZZ,ILX,JLX,INEST,IBMOIST,QISBT,QINBT,             SOLVE.226
     +         QIEBT,QIWBT)                                                      SOLVE.227
          CALL BDYTEN(QNI3DTEN,KZZ,ILX,JLX,INEST,IBMOIST,QNISBT,QNINBT,          SOLVE.228
     +         QNIEBT,QNIWBT)                                                    SOLVE.229
          IF(IICEG.EQ.1)THEN                                                     SOLVE.230
            CALL DCPL3D(QG3D,QGA,RPSA,ILX,JLX,KL,MKX)                            SOLVE.231
            CALL DCPL3D(QNC3D,QNCA,RPSA,ILX,JLX,KL,MKX)                          SOLVE.232
            CALL BDYTEN(QG3DTEN,KZZ,ILX,JLX,INEST,IBMOIST,QGSBT,QGNBT,           SOLVE.233
     +           QGEBT,QGWBT)                                                    SOLVE.234
            CALL BDYTEN(QNC3DTEN,KZZ,ILX,JLX,INEST,IBMOIST,QNCSBT,QNCNBT         SOLVE.235
     +           ,QNCEBT,QNCWBT)                                                 SOLVE.236
          ENDIF                                                                  SOLVE.237
        ENDIF                                                                    SOLVE.238
      ENDIF                                                                      SOLVE.239
C                                                                                SOLVE.240
      PT=PTOP*1000.                                                              SOLVE.241
C                                                                                SOLVE.242
cmic$ do all autoscope                                                           SOLVE.243
c$doacross                                                                       SOLVE.244
c$& local(i,j,k)                                                                 SOLVE.245
c$omp parallel do default(shared)                                                SOLVE.246
c$omp&private(i,j,k)                                                             SOLVE.247
      DO J=1,JLX                                                                 SOLVE.248
        DO K=1,KL                                                                SOLVE.249
          DO I=1,ILX                                                             SOLVE.250
            PR1(I,J,K)=PR0(I,J,K)+PP3D(I,J,K)                                    SOLVE.253
            RHO1(I,J,K)=PR1(I,J,K)/(R*T3D(I,J,K)*(1.+EP1*QV3D(I,J,K)*            SOLVE.254
     +                  XMOIST(INEST)))                                          SOLVE.255
          ENDDO                                                                  SOLVE.256
        ENDDO                                                                    SOLVE.257
      ENDDO                                                                      SOLVE.258
C                                                                                SOLVE.259
C                                                                                SOLVE.260
cmic$ do all autoscope                                                           SOLVE.261
c$doacross                                                                       SOLVE.262
c$& local(i,j,k,rho0s,dummy,div)                                                 SOLVE.263
c$omp parallel do default(shared)                                                SOLVE.264
c$omp&private(i,j,k,rho0s,dummy,div)                                             SOLVE.265
Ccsd$ parallel do private(i,j,k,rho0s,dummy,div)                                 23DEC04.382
Ccsd$& schedule(static,1)                                                        23DEC04.383
      DO J=1,JLX                                                                 SOLVE.266
C                                                                                SOLVE.267
C --- CALCULATE WIND COMPONENTS AT CROSS POINT                                   SOLVE.268
C                                                                                SOLVE.269
        DO K=1,KL                                                                SOLVE.270
          DO I=1,ILX                                                             SOLVE.271
            UCC(I,J,K)=U3D(I,J,K)+U3D(I+1,J,K)+U3D(I,J+1,K)+                     SOLVE.272
     +                 U3D(I+1,J+1,K)                                            SOLVE.273
            VCC(I,J,K)=V3D(I,J,K)+V3D(I+1,J,K)+V3D(I,J+1,K)+                     SOLVE.274
     +                 V3D(I+1,J+1,K)                                            SOLVE.275
          ENDDO                                                                  SOLVE.276
        ENDDO                                                                    SOLVE.277
C                                                                                SOLVE.278
C --- CALCULATE QDOT                                                             SOLVE.279
C --- SIGMADOT=-RHO.G.W/PS-SIG/PS.DPSDX.U-SIG/PS.DPSDY.V                         SOLVE.280
C                                                                                SOLVE.281
        DO K=2,KL                                                                SOLVE.282
          DO I=1,ILX                                                             SOLVE.283
            RHO0S=TWT(K,1)*RHO0(I,J,K)+TWT(K,2)*RHO0(I,J,K-1)                    SOLVE.284
            QDOT(I,J,K)=-RHO0S*G*W3D(I,J,K)*RPSA(I,J)*0.001                      SOLVE.285
     +                 -SIGMA(K)*(DPSDXM(I,J)*(TWT(K,1)*UCC(I,J,K)               SOLVE.286
     +                                        +TWT(K,2)*UCC(I,J,K-1))            SOLVE.287
     +                           +DPSDYM(I,J)*(TWT(K,1)*VCC(I,J,K)               SOLVE.288
     +                                        +TWT(K,2)*VCC(I,J,K-1)))           SOLVE.289
          ENDDO                                                                  SOLVE.290
        ENDDO                                                                    SOLVE.291
C                                                                                SOLVE.292
C --- CALCULATE TOTAL DIVERGENCE AND STORE IN DIVX                               SOLVE.293
C                                                                                SOLVE.294
        DO I=1,ILX                                                               SOLVE.295
          DUMMY(I)=1.0/(DX2*MSFX(I,J)*MSFX(I,J))                                 SOLVE.296
        ENDDO                                                                    SOLVE.297
C                                                                                SOLVE.298
        DO K=1,KL                                                                SOLVE.299
          DO I=1,ILX                                                             SOLVE.300
            DIV=UA(I+1,J+1,K)+UA(I,J+1,K)-UA(I+1,J,K)-UA(I,J,K)+                 SOLVE.301
     +          VA(I+1,J+1,K)+VA(I+1,J,K)-VA(I,J+1,K)-VA(I,J,K)                  SOLVE.302
            DIVX(I,J,K)=DIV*DUMMY(I)+(QDOT(I,J,K+1)-QDOT(I,J,K))*                SOLVE.303
     +                  PSA(I,J)/DSIGMA(K)                                       SOLVE.304
          ENDDO                                                                  SOLVE.305
        ENDDO                                                                    SOLVE.306
      ENDDO                                                                      SOLVE.307
Ccsd$ end parallel do                                                            23DEC04.384
C                                                                                SOLVE.308
C *** COMPUTE HORIZONTAL ADVECTION TERMS FOR U, V, PP, W:                        SOLVE.309
C                                                                                23DEC04.385
CB Optimized code for calculation of U and V terms                               23DEC04.386
CB replaces some calles to HSDV and VADV                                         23DEC04.387
C       Flux-divergence terms, divergence terms, curvature terms,                23DEC04.388
C       and Coriolis  terms for U, V:                                            23DEC04.389
C                                                                                23DEC04.390
      CALL ADVDUV( U3DTEN, V3DTEN, UA, VA, QDOT, DX16, REARTH,                   23DEC04.391
     &             WA, DIVX, U3D, V3D, UCD, VCD, INEST )                         23DEC04.392
C                                                                                23DEC04.393
CB      CALL HADV(KZZ,U3DTEN,UA,VA,U3D,MSFD,DX16,3,INEST,'U')                    23DEC04.394
CB      CALL HADV(KZZ,V3DTEN,UA,VA,V3D,MSFD,DX16,3,INEST,'V')                    23DEC04.395
      CALL HADV(KZZ,PP3DTEN,UA,VA,PP3D,MSFX,DX4,1,INEST,'X')                     23DEC04.396
      CALL HADV(KZZ+1,W3DTEN,UA,VA,W3D,MSFX,DX4,4,INEST,'W')                     23DEC04.397
C                                                                                SOLVE.315
C *** COMPUTE VERTICAL ADVECTION TERMS FOR U, V, PP, W:                          SOLVE.316
C                                                                                SOLVE.317
CB      CALL VADV(KZZ,U3DTEN,UCD,QDOT,PSA,4,INEST)                               23DEC04.398
CB      CALL VADV(KZZ,V3DTEN,VCD,QDOT,PSA,4,INEST)                               23DEC04.399
CB END                                                                           23DEC04.400
      CALL VADV(KZZ,PP3DTEN,PPA,QDOT,PSA,6,INEST)                                SOLVE.320
      CALL VADV(KZZ+1,W3DTEN,WA,QDOT,PSA,5,INEST)                                SOLVE.321
C                                                                                SOLVE.322
C *** COMPUTE HORIZONTAL AND VERTICAL ADVECTION TERMS FOR T OR THETA:            SOLVE.323
C                                                                                SOLVE.324
C_FLIC_RUNPAD(0)                                                                 23DEC04.401
CB Moved up here - no effect - only because CRSXK is now needed sooner           23DEC04.402
C                                                                                23DEC04.403
C     INITIALIZE DIFFUSION COEFFS                                                23DEC04.404
C                                                                                23DEC04.405
cmic$ do all autoscope                                                           23DEC04.406
cmic$2 private(i,j,k)                                                            23DEC04.407
c$doacross                                                                       23DEC04.408
c$& local(i,j,k)                                                                 23DEC04.409
c$omp parallel do default(shared)                                                23DEC04.410
c$omp&private(i,j,k)                                                             23DEC04.411
      DO J=1,MJX                                                                 23DEC04.412
        DO K=1,MKX                                                               23DEC04.413
          DO I=1,MIX                                                             23DEC04.414
            CRSXK(I,J,K)=0.                                                      23DEC04.415
            DOTXK(I,J,K)=0.                                                      23DEC04.416
          ENDDO                                                                  23DEC04.417
        ENDDO                                                                    23DEC04.418
      ENDDO                                                                      23DEC04.419
C                                                                                23DEC04.420
CB END                                                                           23DEC04.421
C_FLIC_RUNPAD(1)                                                                 SOLVE.325
      IF(ITHADV.EQ.1)THEN                                                        SOLVE.326
cmic$ do all autoscope                                                           SOLVE.327
c$doacross                                                                       SOLVE.328
c$& local(i,j,k,exner)                                                           SOLVE.329
c$omp parallel do default(shared)                                                SOLVE.330
c$omp&private(i,j,k,exner)                                                       SOLVE.331
        DO J=1,JLX                                                               SOLVE.332
          DO K=1,KL                                                              SOLVE.333
            DO I=1,ILX                                                           SOLVE.334
              EXNER=(1.E5/PR1(I,J,K))**ROVCP                                     SOLVE.335
              THA(I,J,K)=TA(I,J,K)*EXNER                                         SOLVE.336
              TH(I,J,K)=THA(I,J,K)*RPSA(I,J)                                     SOLVE.337
              THTEN(I,J,K)=0.0                                                   SOLVE.338
            ENDDO                                                                SOLVE.339
          ENDDO                                                                  SOLVE.340
        ENDDO                                                                    SOLVE.341
C                                                                                SOLVE.342
        CALL HADV(KZZ,THTEN,UA,VA,TH,MSFX,DX4,tadv,INEST,'T')                    23DEC04.422
        if (ITADVM(INEST).GE.1) then                                             23DEC04.423
          CALL VADV2(KZZ,THTEN,CRSXK,TH,THA,QDOT,PSA,6,INEST,'T')                23DEC04.424
        else                                                                     23DEC04.425
          CALL VADV(KZZ,THTEN,THA,QDOT,PSA,6,INEST)                              23DEC04.426
       endif                                                                     23DEC04.427
      ELSE                                                                       SOLVE.345
        CALL HADV(KZZ,T3DTEN,UA,VA,T3D,MSFX,DX4,tadv,INEST,'T')                  23DEC04.428
        if (ITADVM(INEST).GE.1) then                                             23DEC04.429
          CALL VADV2(KZZ,T3DTEN,CRSXK,T3D,TA,QDOT,PSA,6,INEST,'T')               23DEC04.430
        else                                                                     23DEC04.431
          CALL VADV(KZZ,T3DTEN,TA,QDOT,PSA,INDH,INEST)                           23DEC04.432
        endif                                                                    23DEC04.433
      ENDIF                                                                      SOLVE.348
C_FLIC_RUNPAD(0)                                                                 SOLVE.349
C                                                                                SOLVE.350
C *** COMPUTE HORIZONTAL AND VERTICAL MOISTURE ADVECTION TERMS:                  SOLVE.351
C                                                                                SOLVE.352
      IF(IMPHYS(INEST).GE.2)THEN                                                 SOLVE.353
        CALL HADV(KZZ,QV3DTEN,UA,VA,QV3D,MSFX,DX4,qadv,INEST,'Q')                23DEC04.434
C                                                                                SOLVE.355
C ------ DO NOT CALL VERTICAL ADVECTION FOR ANTHES-KUO SCHEME.                   SOLVE.356
C                                                                                SOLVE.357
        IF(ICUPA(INEST).NE.2)THEN                                                SOLVE.358
          if (IQADVM(INEST).GE.1) then                                           23DEC04.435
            CALL VADV2(KZZ,QV3DTEN,CRSXK,QV3D,QVA,QDOT,PSA,INDQ,                 23DEC04.436
     &                 INEST,'Q')                                                23DEC04.437
          else                                                                   23DEC04.438
            CALL VADV(KZZ,QV3DTEN,QVA,QDOT,PSA,INDQ,INEST)                       23DEC04.439
          endif                                                                  23DEC04.440
        ENDIF                                                                    SOLVE.360
C                                                                                SOLVE.361
C ------ FOR EXPLICIT MOISTURE SCHEMES                                           SOLVE.362
C                                                                                SOLVE.363
        IF(IMPHYS(INEST).GE.3)THEN                                               SOLVE.364
          CALL HADV(KZZ,QC3DTEN,UA,VA,QC3D,MSFX,DX4,cwadv,INEST,'C')             23DEC04.441
          CALL HADV(KZZ,QR3DTEN,UA,VA,QR3D,MSFX,DX4,cwadv,INEST,'P')             23DEC04.442
          if (IQADVM(INEST).GE.1) then                                           23DEC04.443
            CALL VADV2(KZZ,QC3DTEN,CRSXK,QC3D,QCA,QDOT,PSA,6,INEST,'C')          23DEC04.444
            CALL VADV2(KZZ,QR3DTEN,CRSXK,QR3D,QRA,QDOT,PSA,6,INEST,'P')          23DEC04.445
          else                                                                   23DEC04.446
            CALL VADV(KZZ,QC3DTEN,QCA,QDOT,PSA,INDQ,INEST)                       23DEC04.447
            CALL VADV(KZZ,QR3DTEN,QRA,QDOT,PSA,INDQ,INEST)                       23DEC04.448
          endif                                                                  23DEC04.449
          IF(IMPHYS(INEST).GE.5)THEN                                             SOLVE.369
            CALL HADV(KZZ,QI3DTEN,UA,VA,QI3D,MSFX,DX4,cwadv,INEST,'I')           23DEC04.450
            CALL HADV(KZZ,QNI3DTEN,UA,VA,QNI3D,MSFX,DX4,cwadv,INEST,'P')         23DEC04.451
            if (IQADVM(INEST).GE.1) then                                         23DEC04.452
              CALL VADV2(KZZ,QI3DTEN,CRSXK,QI3D,QIA,QDOT,PSA,6,INEST,            23DEC04.453
     &                   'I')                                                    23DEC04.454
              CALL VADV2(KZZ,QNI3DTEN,CRSXK,QNI3D,QNIA,QDOT,PSA,6,INEST,         23DEC04.455
     &                   'P')                                                    23DEC04.456
            else                                                                 23DEC04.457
              CALL VADV(KZZ,QI3DTEN,QIA,QDOT,PSA,INDQ,INEST)                     23DEC04.458
              CALL VADV(KZZ,QNI3DTEN,QNIA,QDOT,PSA,INDQ,INEST)                   23DEC04.459
            endif                                                                23DEC04.460
            IF(IMPHYS(INEST).GE.6)THEN                                           SOLVE.374
              CALL HADV(KZZ,QG3DTEN,UA,VA,QG3D,MSFX,DX4,cwadv,INEST,'P')         23DEC04.461
              CALL HADV(KZZ,QNC3DTEN,UA,VA,QNC3D,MSFX,DX4,cwadv,INEST,           23DEC04.462
     &                  'N')                                                     23DEC04.463
              if (IQADVM(INEST).GE.1) then                                       23DEC04.464
                CALL VADV2(KZZ,QG3DTEN,CRSXK,QG3D,QGA,QDOT,PSA,6,INEST,          23DEC04.465
     &                     'P')                                                  23DEC04.466
                CALL VADV2(KZZ,QNC3DTEN,CRSXK,QNC3D,QNCA,QDOT,PSA,6,             23DEC04.467
     &                     INEST,'N')                                            23DEC04.468
              else                                                               23DEC04.469
                CALL VADV(KZZ,QG3DTEN,QGA,QDOT,PSA,INDQ,INEST)                   23DEC04.470
                CALL VADV(KZZ,QNC3DTEN,QNCA,QDOT,PSA,INDQ,INEST)                 23DEC04.471
              endif                                                              23DEC04.472
            ENDIF                                                                SOLVE.379
          ENDIF                                                                  SOLVE.380
        ENDIF                                                                    SOLVE.381
      ENDIF                                                                      SOLVE.382
C                                                                                SOLVE.383
C                                                                                SOLVE.384
C *** COMPUTE THE ADIABATIC TERM:                                                SOLVE.385
C                                                                                SOLVE.386
cmic$ do all autoscope                                                           SOLVE.387
cmic$1 shared(ibnes,ienes,jbnes,jenes,kl,inest,imoist,idry,                      SOLVE.388
cmic$1        g,ep1,xmoist,cp,rovcp,twt,dsigma,                                  SOLVE.389
cmic$1        pr0,pr1,rho0,rho1,t0,cosrot,psa,ucc,vcc,                           SOLVE.390
cmic$1        ua,va,wa,w3d,t3d,pp3d,qv3d,qca,qra,qia,qnia,qga,qnca,              SOLVE.391
cmic$1        thten,th,w3dten,t3dten,pp3dten,divx)                               SOLVE.392
cmic$2 private(i,j,k,                                                            SOLVE.393
cmic$2         scr1,tv,cpm,pr,uaq,vaq,rofac)                                     SOLVE.394
c$doacross                                                                       SOLVE.395
c$& local(i,j,k,scr1,tv,cpm,pr,uaq,vaq,rofac)                                    SOLVE.396
c$omp parallel do default(shared)                                                SOLVE.397
c$omp&private(i,j,k,scr1,tv,cpm,pr,uaq,vaq,rofac)                                SOLVE.398
C                                                                                SOLVE.399
Ccsd$ parallel do private(i,j,k,scr1,tv,cpm,pr,uaq,vaq,rofac)                    23DEC04.473
Ccsd$& schedule(static,1)                                                        23DEC04.474
      DO J=JBNES,JENES-1                                                         SOLVE.400
        IF(ITHADV.NE.1)THEN                                                      SOLVE.401
C --- OMEGA*PS CALCULATION                                                       SOLVE.402
          DO K=1,KL                                                              SOLVE.403
            DO I=IBNES,IENES-1                                                   SOLVE.404
              SCR1(I,K)=0.5*G*RHO0(I,J,K)*(WA(I,J,K)+WA(I,J,K+1))                SOLVE.405
C                                                                                SOLVE.406
              CPM=CP*(1.+0.8*QV3D(I,J,K)*XMOIST(INEST))                          SOLVE.407
C                                                                                SOLVE.408
C --- REMOVE TOTAL DIVERGENCE TERM IN T                                          SOLVE.409
C                                                                                SOLVE.410
C --- T3DTEN(I,J,K)=T3DTEN(I,J,K)+(SCR1(I,K)*ROVCPM*TV)/(PTOP*RPSA(I,J)+         SOLVE.411
C --- ADD OMEGA AND ADVECTIVE PP ADIABATIC WARMING TERMS                         SOLVE.412
C --- LOCAL PP TENDENCY ADDED LATER PP3DTEN OMEGA TERM IS IMPLICIT               SOLVE.413
C                                                                                SOLVE.414
              T3DTEN(I,J,K)=T3DTEN(I,J,K)+T3D(I,J,K)*DIVX(I,J,K)-(               SOLVE.415
     +                      SCR1(I,K)+PP3DTEN(I,J,K)+PP3D(I,J,K)*                SOLVE.416
     +                      DIVX(I,J,K))/(RHO1(I,J,K)*CPM)                       SOLVE.417
            ENDDO                                                                SOLVE.418
          ENDDO                                                                  SOLVE.419
        ELSE                                                                     SOLVE.420
                                                                                 23DEC04.475
          if (ITADVM(INEST).EQ.0) THEN                                           23DEC04.476
          DO K=1,KL                                                              SOLVE.421
            DO I=IBNES,IENES-1                                                   SOLVE.422
              THTEN(I,J,K)=THTEN(I,J,K)+TH(I,J,K)*DIVX(I,J,K)                    SOLVE.423
              T3DTEN(I,J,K)=T3DTEN(I,J,K)+TA(I,J,K)*THTEN(I,J,K)/                SOLVE.424
     +                      THA(I,J,K)                                           SOLVE.425
            ENDDO                                                                SOLVE.426
          ENDDO                                                                  SOLVE.427
          else                                                                   23DEC04.477
          DO K=1,KL                                                              23DEC04.478
            DO I=IBNES,IENES-1                                                   23DEC04.479
              T3DTEN(I,J,K)=T3DTEN(I,J,K)+TA(I,J,K)*THTEN(I,J,K)/                23DEC04.480
     +                      THA(I,J,K)                                           23DEC04.481
            ENDDO                                                                23DEC04.482
          ENDDO                                                                  23DEC04.483
          endif                                                                  23DEC04.484
                                                                                 23DEC04.485
        ENDIF                                                                    SOLVE.428
C                                                                                SOLVE.429
        DO K=1,KL                                                                SOLVE.430
          DO I=IBNES,IENES-1                                                     SOLVE.431
            TV=T3D(I,J,K)*(1.+EP1*QV3D(I,J,K)*XMOIST(INEST))                     SOLVE.432
C                                                                                SOLVE.433
            PR(I,K)=(TV-T0(I,J,K)-PP3D(I,J,K)/(CP*RHO0(I,J,K)))/                 SOLVE.434
     +              T3D(I,J,K)                                                   SOLVE.435
C                                                                                SOLVE.436
            UCC(I,J,K)=UCD(I,J,K)+UCD(I+1,J,K)+UCD(I,J+1,K)+                     SOLVE.437
     +                 UCD(I+1,J+1,K)                                            SOLVE.438
            VCC(I,J,K)=VCD(I,J,K)+VCD(I+1,J,K)+VCD(I,J+1,K)+                     SOLVE.439
     +                 VCD(I+1,J+1,K)                                            SOLVE.440
          ENDDO                                                                  SOLVE.441
        ENDDO                                                                    SOLVE.442
C                                                                                SOLVE.443
C --- BUOYANCY TERMS ADDED TO WTEN (INCLUDE VIRTUAL TEMP AND LIQ. DRAG)          SOLVE.444
C                                                                                SOLVE.445
        DO K=2,KL                                                                SOLVE.446
          DO I=IBNES,IENES-1                                                     SOLVE.447
            ROFAC=(DSIGMA(K-1)*RHO0(I,J,K)+DSIGMA(K)*RHO0(I,J,K-1))/(            SOLVE.448
     +            DSIGMA(K-1)*RHO1(I,J,K)+DSIGMA(K)*RHO1(I,J,K-1))               SOLVE.449
            UAQ=0.25*(TWT(K,1)*UCC(I,J,K)+TWT(K,2)*UCC(I,J,K-1))                 SOLVE.450
            VAQ=0.25*(TWT(K,1)*VCC(I,J,K)+TWT(K,2)*VCC(I,J,K-1))                 SOLVE.451
C                                                                                SOLVE.452
C --- USE DRY R/CP FOR CONSISTENCY WITH CONSTANT GAMMA (=1./(1.-R/CP))           SOLVE.453
C                                                                                SOLVE.454
            W3DTEN(I,J,K)=W3DTEN(I,J,K)                                          SOLVE.455
     1         +(TWT(K,2)*PR(I,K-1)+TWT(K,1)*PR(I,K))*ROFAC*G*PSA(I,J)           SOLVE.456
C                                                                                SOLVE.457
C --- 3D CORIOLIS TERM: VERTICAL COMPONENT                                       SOLVE.458
C                                                                                SOLVE.459
     2            +EX(I,J)*(UAQ*CRX(I,J) - VAQ*SRX(I,J))                         SOLVE.460
C                                                                                SOLVE.461
C --- NON-CARTESIAN CURVATURE TERM                                               SOLVE.462
C                                                                                SOLVE.463
     3            +(UAQ*UAQ+VAQ*VAQ)*REARTH*RPSA(I,J)                            SOLVE.464
C                                                                                SOLVE.465
C --- REMOVE TOTAL DIVERGENCE TERM                                               SOLVE.466
C                                                                                SOLVE.467
     4            +W3D(I,J,K)*(TWT(K,1)*DIVX(I,J,K)                              SOLVE.468
     5                        +TWT(K,2)*DIVX(I,J,K-1))                           SOLVE.469
          ENDDO                                                                  SOLVE.470
        ENDDO                                                                    SOLVE.471
C                                                                                SOLVE.472
C --- ADD LIQUID DRAG USING PRESSURE-WEIGHTED AVERAGING                          SOLVE.473
C                                                                                SOLVE.474
        IF(IMPHYS(INEST).GE.3)THEN                                               SOLVE.475
          DO K=2,KL                                                              SOLVE.476
            DO I=IBNES,IENES-1                                                   SOLVE.477
              W3DTEN(I,J,K)=W3DTEN(I,J,K)-G*(TWT(K,2)*(QCA(I,J,K-1)+             SOLVE.478
     +                      QRA(I,J,K-1))+TWT(K,1)*(QCA(I,J,K)+                  SOLVE.479
     +                      QRA(I,J,K)))                                         SOLVE.480
            ENDDO                                                                SOLVE.481
          ENDDO                                                                  SOLVE.482
          IF(IMPHYS(INEST).GE.5)THEN                                             SOLVE.483
            DO K=2,KL                                                            SOLVE.484
              DO I=IBNES,IENES-1                                                 SOLVE.485
                W3DTEN(I,J,K)=W3DTEN(I,J,K)-G*(TWT(K,2)*(QIA(I,J,K-1)+           SOLVE.486
     +                        QNIA(I,J,K-1))+TWT(K,1)*(QIA(I,J,K)+               SOLVE.487
     +                        QNIA(I,J,K)))                                      SOLVE.488
              ENDDO                                                              SOLVE.489
            ENDDO                                                                SOLVE.490
            IF(IMPHYS(INEST).GE.6)THEN                                           SOLVE.491
              DO K=2,KL                                                          SOLVE.492
                DO I=IBNES,IENES-1                                               SOLVE.493
                  W3DTEN(I,J,K)=W3DTEN(I,J,K)-G*(TWT(K,2)*QGA(I,J,K-1)+          SOLVE.494
     +                          TWT(K,1)*QGA(I,J,K))                             SOLVE.495
                ENDDO                                                            SOLVE.496
              ENDDO                                                              SOLVE.497
            ENDIF                                                                SOLVE.498
          ENDIF                                                                  SOLVE.499
        ENDIF                                                                    SOLVE.500
C                                                                                SOLVE.501
C --- REMOVE TOTAL DIVERGENCE TERM IN QV,PP                                      SOLVE.502
C                                                                                SOLVE.503
        DO K=1,KL                                                                SOLVE.504
          DO I=IBNES,IENES-1                                                     SOLVE.505
            PP3DTEN(I,J,K)=PP3DTEN(I,J,K)+PP3D(I,J,K)*DIVX(I,J,K)                SOLVE.506
          ENDDO                                                                  SOLVE.507
        ENDDO                                                                    SOLVE.508
C                                                                                SOLVE.509
        if (IQADVM(INEST).EQ.0) then                                             23DEC04.486
        IF(IMPHYS(INEST).GE.2)THEN                                               SOLVE.510
          DO K=1,KL                                                              SOLVE.511
            DO I=IBNES,IENES-1                                                   SOLVE.512
              QV3DTEN(I,J,K)=QV3DTEN(I,J,K)+QV3D(I,J,K)*DIVX(I,J,K)              SOLVE.513
            ENDDO                                                                SOLVE.514
          ENDDO                                                                  SOLVE.515
          IF(IMPHYS(INEST).GE.3)THEN                                             SOLVE.516
            DO K=1,KL                                                            SOLVE.517
              DO I=IBNES,IENES-1                                                 SOLVE.518
                QC3DTEN(I,J,K)=QC3DTEN(I,J,K)+QC3D(I,J,K)*DIVX(I,J,K)            SOLVE.519
                QR3DTEN(I,J,K)=QR3DTEN(I,J,K)+QR3D(I,J,K)*DIVX(I,J,K)            SOLVE.520
              ENDDO                                                              SOLVE.521
            ENDDO                                                                SOLVE.522
            IF(IMPHYS(INEST).GE.5)THEN                                           SOLVE.523
              DO K=1,KL                                                          SOLVE.524
                DO I=IBNES,IENES-1                                               SOLVE.525
                  QI3DTEN(I,J,K)=QI3DTEN(I,J,K)+QI3D(I,J,K)*DIVX(I,J,K)          SOLVE.526
                  QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)+QNI3D(I,J,K)*                  SOLVE.527
     +                            DIVX(I,J,K)                                    SOLVE.528
                ENDDO                                                            SOLVE.529
              ENDDO                                                              SOLVE.530
              IF(IMPHYS(INEST).GE.6)THEN                                         SOLVE.531
                DO K=1,KL                                                        SOLVE.532
                  DO I=IBNES,IENES-1                                             SOLVE.533
                    QG3DTEN(I,J,K)=QG3DTEN(I,J,K)+QG3D(I,J,K)*                   SOLVE.534
     +                             DIVX(I,J,K)                                   SOLVE.535
                    QNC3DTEN(I,J,K)=QNC3DTEN(I,J,K)+QNC3D(I,J,K)*                SOLVE.536
     +                              DIVX(I,J,K)                                  SOLVE.537
                  ENDDO                                                          SOLVE.538
                ENDDO                                                            SOLVE.539
              ENDIF                                                              SOLVE.540
            ENDIF                                                                SOLVE.541
          ENDIF                                                                  SOLVE.542
        ENDIF                                                                    SOLVE.543
       endif                                                                     23DEC04.487
      ENDDO                                                                      SOLVE.544
Ccsd$ end parallel do                                                            23DEC04.488
CB Computation of coriolis and curvature terms moved into routine ADVDVU         23DEC04.489
CB This change was done purely for optimization                                  23DEC04.490
CBC                                                                              23DEC04.491
CBC *** COMPUTE CORIOLIS AND CURVATURE TERMS:                                    23DEC04.492
CBC                                                                              23DEC04.493
CBcmic$ do all autoscope                                                         23DEC04.494
CBcmic$1 shared (IBNES,IENES,JBNES,JENES,KL,f,u3d,v3d,                           23DEC04.495
CBcmic$1         PSA,WA,QDOT,EF,COSROT,SINROT)                                   23DEC04.496
CBcmic$2 private (I,J,K,WADOT,DIVD,AMFAC)                                        23DEC04.497
CBc$doacross                                                                     23DEC04.498
CBc$& local(i,j,k,divd,wadot,amfac)                                              23DEC04.499
CBc$omp parallel do default(shared)                                              23DEC04.500
CBc$omp&private(i,j,k,divd,wadot,amfac)                                          23DEC04.501
CB      DO J=JBNES,JENES                                                         23DEC04.502
CB        DO K=1,KLP1                                                            23DEC04.503
CB          DO I=IBNES,IENES                                                     23DEC04.504
CB            WADOT(I,K)=WA(I-1,J-1,K)+WA(I,J-1,K)+WA(I-1,J,K)+WA(I,J,K)         23DEC04.505
CB          ENDDO                                                                23DEC04.506
CB        ENDDO                                                                  23DEC04.507
CBC                                                                              23DEC04.508
CBC *** 3D CORIOLIS TERM: HORIZONTAL COMPONENT                                   23DEC04.509
CBC                                                                              23DEC04.510
CB        DO K=1,KL                                                              23DEC04.511
CB          DO I=IBNES,IENES                                                     23DEC04.512
CBC     DOT-POINT AVERAGED DIVERGENCE                                            23DEC04.513
CB            DIVD=0.25*(DIVX(I,J,K)+DIVX(I-1,J,K)+DIVX(I,J-1,K)+                23DEC04.514
CB     +           DIVX(I-1,J-1,K))                                              23DEC04.515
CB            U3DTEN(I,J,K)=U3DTEN(I,J,K)+U3D(I,J,K)*DIVD+F(I,J)*                23DEC04.516
CB     +                    VCD(I,J,K)-EF(I,J)*COSROT(I,J)*0.125*(               23DEC04.517
CB     +                    WADOT(I,K)+WADOT(I,K+1))                             23DEC04.518
CB            V3DTEN(I,J,K)=V3DTEN(I,J,K)+V3D(I,J,K)*DIVD-F(I,J)*                23DEC04.519
CB     +                    UCD(I,J,K)+EF(I,J)*SINROT(I,J)*0.125*(               23DEC04.520
CB     +                    WADOT(I,K)+WADOT(I,K+1))                             23DEC04.521
CBC                                                                              23DEC04.522
CBC *** CURVATURE TERMS - THESE ALLOW FOR X AND Y AXES NOT BEING STRAIGHT        23DEC04.523
CBC     LINES (GREAT CIRCLES) DUE TO MAP-SCALE FACTOR GRADIENT                   23DEC04.524
CBC                                                                              23DEC04.525
CB            U3DTEN(I,J,K)=U3DTEN(I,J,K)+V3D(I,J,K)*(UCD(I,J,K)*DMDY(I,J)       23DEC04.526
CB     +                    -VCD(I,J,K)*DMDX(I,J))                               23DEC04.527
CB            V3DTEN(I,J,K)=V3DTEN(I,J,K)-U3D(I,J,K)*(UCD(I,J,K)*DMDY(I,J)       23DEC04.528
CB     +                    -VCD(I,J,K)*DMDX(I,J))                               23DEC04.529
CBC                                                                              23DEC04.530
CBC *** ANGULAR MOMENTUM CONSERVATION (OR VERTICAL CURVATURE) TERM               23DEC04.531
CBC                                                                              23DEC04.532
CB            AMFAC=0.125*(WADOT(I,K)+WADOT(I,K+1))*RPDOTA(I,J)*REARTH           23DEC04.533
CB            U3DTEN(I,J,K)=U3DTEN(I,J,K)-UCD(I,J,K)*AMFAC                       23DEC04.534
CB            V3DTEN(I,J,K)=V3DTEN(I,J,K)-VCD(I,J,K)*AMFAC                       23DEC04.535
CB          ENDDO                                                                23DEC04.536
CB        ENDDO                                                                  23DEC04.537
CB      ENDDO                                                                    23DEC04.538
CB END                                                                           23DEC04.539
C                                                                                SOLVE.593
C --- DECOUPLE 3D-FIELDS FOR USE IN PHYSICS CALLS                                SOLVE.594
C                                                                                SOLVE.595
      CALL DCPL3D(U3D,UB,RPDOTB,IL,JL,KL,MKX)                                    SOLVE.596
      CALL DCPL3D(V3D,VB,RPDOTB,IL,JL,KL,MKX)                                    SOLVE.597
      CALL DCPL3D(T3D,TB,RPSB,ILX,JLX,KL,MKX)                                    SOLVE.598
      CALL DCPL3D(W3D,WB,RPSB,ILX,JLX,KLP1,KXP1NH)                               SOLVE.599
      CALL DCPL3D(PP3D,PPB,RPSB,ILX,JLX,KL,MKX)                                  SOLVE.600
      IF(IMPHYS(INEST).GE.2) THEN                                                SOLVE.601
        CALL DCPL3D(QV3D,QVB,RPSB,ILX,JLX,KL,MKX)                                SOLVE.602
        IF(IMPHYS(INEST).GE.3)THEN                                               SOLVE.603
          CALL DCPL3D(QC3D,QCB,RPSB,ILX,JLX,KL,MKX)                              SOLVE.604
          CALL DCPL3D(QR3D,QRB,RPSB,ILX,JLX,KL,MKX)                              SOLVE.605
          IF(IMPHYS(INEST).GE.5)THEN                                             SOLVE.606
            CALL DCPL3D( QI3D, QIB,RPSB,ILX,JLX,KL,MKX)                          SOLVE.607
            CALL DCPL3D(QNI3D,QNIB,RPSB,ILX,JLX,KL,MKX)                          SOLVE.608
            IF(IMPHYS(INEST).GE.6)THEN                                           SOLVE.609
              CALL DCPL3D( QG3D, QGB,RPSB,ILX,JLX,KL,MKX)                        SOLVE.610
              CALL DCPL3D(QNC3D,QNCB,RPSB,ILX,JLX,KL,MKX)                        SOLVE.611
            ENDIF                                                                SOLVE.612
          ENDIF                                                                  SOLVE.613
        ENDIF                                                                    SOLVE.614
      ENDIF                                                                      SOLVE.615
      CALL COEF_DIFFU(CRSXK,DOTXK,U3D,V3D,XKHZ,XKHMAX,C200,ILX,JLX,KL,           23DEC04.540
     &                INEST)                                                     23DEC04.541
c                                                                                23DEC04.542
c  compute T' / theta for temperature diffusion                                  23DEC04.543
c                                                                                23DEC04.544
C_FLIC_RUNPAD(2)                                                                 23DEC04.545
       if (ITPDIF.eq.1) then                                                     23DEC04.546
cmic$ do all autoscope                                                           23DEC04.547
c$doacross                                                                       23DEC04.548
c$& local(i,j,k)                                                                 23DEC04.549
c$omp parallel do default(shared)                                                23DEC04.550
c$omp&private(i,j,k)                                                             23DEC04.551
      DO J=1,JLX                                                                 23DEC04.552
        DO K=1,KL                                                                23DEC04.553
          DO I=1,ILX                                                             23DEC04.554
            TBP(I,J,K)=T3D(I,J,K)-T0(I,J,K)                                      23DEC04.555
          ENDDO                                                                  23DEC04.556
        ENDDO                                                                    23DEC04.557
      ENDDO                                                                      23DEC04.558
      else if (ITPDIF.ge.2) then                                                 23DEC04.559
cmic$ do all autoscope                                                           23DEC04.560
c$doacross                                                                       23DEC04.561
c$& local(i,j,k)                                                                 23DEC04.562
c$omp parallel do default(shared)                                                23DEC04.563
c$omp&private(i,j,k)                                                             23DEC04.564
      DO J=1,JLX                                                                 23DEC04.565
        DO K=1,KL                                                                23DEC04.566
          DO I=1,ILX                                                             23DEC04.567
            TBP(I,J,K)=T3D(I,J,K)*(1.E5/PR1(I,J,K))**ROVCP                       23DEC04.568
          ENDDO                                                                  23DEC04.569
        ENDDO                                                                    23DEC04.570
      ENDDO                                                                      23DEC04.571
      if (tdkorr.eq.1) then                                                      23DEC04.572
c$omp parallel do default(shared)                                                23DEC04.573
c$omp&private(i,j)                                                               23DEC04.574
      DO J=1,JLX                                                                 23DEC04.575
      DO I=1,ILX                                                                 23DEC04.576
      THGB(I,J) = TGB(I,J)*(1.E5/(PS0(i,j)+PT+PP3D(I,J,KL)))**ROVCP              23DEC04.577
      enddo                                                                      23DEC04.578
      enddo                                                                      23DEC04.579
      else                                                                       23DEC04.580
c$omp parallel do default(shared)                                                23DEC04.581
c$omp&private(i,j)                                                               23DEC04.582
      DO J=1,JLX                                                                 23DEC04.583
      DO I=1,ILX                                                                 23DEC04.584
      THGB(I,J) = TBP(I,J,KL)                                                    23DEC04.585
      enddo                                                                      23DEC04.586
      enddo                                                                      23DEC04.587
      endif                                                                      23DEC04.588
      endif                                                                      23DEC04.589
C                                                                                23DEC04.590
C_FLIC_RUNPAD(0)                                                                 23DEC04.591
                                                                                 23DEC04.592
c  Compute interpolation coefficients for z-diffusion                            23DEC04.593
                                                                                 23DEC04.594
       if (ITPDIF.ge.2) then                                                     23DEC04.595
C_FLIC_RUNPAD(2)                                                                 23DEC04.596
                                                                                 23DEC04.597
      if (diffpar(inest)) then                                                   23DEC04.598
        print*,'Interpolation coefficients for z-diffusion are computed'         23DEC04.599
c                                                                                23DEC04.600
       call diffintp(A,PSB,ptop,IBNES,IENES-1,JBNES,JENES-1,KL,inest,            23DEC04.601
     f T0,dx,rkip1,rkip2,rkim1,rkim2,rkjp1,rkjp2,rkjm1,rkjm2,finv,fjnv,          23DEC04.602
     f kmip,kmjp,kmim,kmjm,kmv,rmn,t0ij,t0ij1,t0i1j,t0i1j1,t0ijmn,               23DEC04.603
     f zdiffst)                                                                  23DEC04.604
        diffpar(inest) = .false.                                                 23DEC04.605
                                                                                 23DEC04.606
        print*,'INEST= ',inest,' kmv= ',kmv(inest),'                             23DEC04.607
     f  zdiffst= ',zdiffst(inest),a(zdiffst(inest))                              23DEC04.608
      endif                                                                      23DEC04.609
                                                                                 23DEC04.610
        kmvl = kmv(inest)                                                        23DEC04.611
                                                                                 23DEC04.612
c  Wind speed at lowermost model level. Weighting depends on height difference   23DEC04.613
                                                                                 23DEC04.614
c$omp parallel do default(shared)                                                23DEC04.615
c$omp&private(i,j)                                                               23DEC04.616
        do j=3,jlxm-1                                                            23DEC04.617
        do i=3,ilxm-1                                                            23DEC04.618
                                                                                 23DEC04.619
      cbt1 =     u3d(i,j,kl)*t0ij(i,j,inest)                                     23DEC04.620
      cbt2 =     u3d(i+1,j,kl)*t0i1j(i,j,inest)                                  23DEC04.621
      cbt3 =     u3d(i,j+1,kl)*t0ij1(i,j,inest)                                  23DEC04.622
      cbt4 =     u3d(i+1,j+1,kl)*t0i1j1(i,j,inest)                               23DEC04.623
      cbt5 =     v3d(i,j,kl)*t0ij(i,j,inest)                                     23DEC04.624
      cbt6 =     v3d(i+1,j,kl)*t0i1j(i,j,inest)                                  23DEC04.625
      cbt7 =     v3d(i,j+1,kl)*t0ij1(i,j,inest)                                  23DEC04.626
      cbt8 =     v3d(i+1,j+1,kl)*t0i1j1(i,j,inest)                               23DEC04.627
       absv(i,j) =                                                               23DEC04.628
     &     sqrt((t0ijmn(i,j,inest)*                                              23DEC04.629
     &     (cbt1+cbt2+cbt3+cbt4))**2+                                            23DEC04.630
     &     (t0ijmn(i,j,inest)*                                                   23DEC04.631
     &     (cbt5+cbt6+cbt7+cbt8))**2)                                            23DEC04.632
                                                                                 23DEC04.633
c  Reduction of diffusion coefficients near the ground depends on wind speed; n  23DEC04.634
c                                                                                23DEC04.635
c  Under weak-wind conditions, the diffusion coefficients are strongly reduced   23DEC04.636
c  for computing diffusion are st significantly different heights, which essent  23DEC04.637
c  is restricted to the direction along a valley. In the presence of strong sur  23DEC04.638
c  that quasi-horizontal mixing along the side slopes of a valley takes place.   23DEC04.639
c  to take place between surface wind speeds of 4 and 12 m/s.                    23DEC04.640
c                                                                                23DEC04.641
      lam(i,j) = min(1.,max(0.,0.125*(absv(i,j)-4.0)))                           23DEC04.642
                                                                                 23DEC04.643
c Reduction factor for orography-adjusted sigma-diffusion                        23DEC04.644
                                                                                 23DEC04.645
      fin(i,j) = exp(log(finv(i,j,inest))*(1-lam(i,j)))                          23DEC04.646
      fjn(i,j) = exp(log(fjnv(i,j,inest))*(1-lam(i,j)))                          23DEC04.647
                                                                                 23DEC04.648
      enddo                                                                      23DEC04.649
      enddo                                                                      23DEC04.650
                                                                                 23DEC04.651
c  Further coefficients                                                          23DEC04.652
                                                                                 23DEC04.653
        if (inest.ne.oldnest) then                                               23DEC04.654
                                                                                 23DEC04.655
        oldnest = inest                                                          23DEC04.656
        in = inest                                                               23DEC04.657
                                                                                 23DEC04.658
cmic$ do all autoscope                                                           23DEC04.659
c$doacross                                                                       23DEC04.660
c$& local(i,j)                                                                   23DEC04.661
c$omp parallel do default(shared)                                                23DEC04.662
c$omp&private(i,j)                                                               23DEC04.663
          do  j=1,mjx                                                            23DEC04.664
            do  i=1,mix                                                          23DEC04.665
              kmi(i,j) = min(kmim(i,j,in),kmip(i,j,in))                          23DEC04.666
              kmj(i,j) = min(kmjm(i,j,in),kmjp(i,j,in))                          23DEC04.667
            enddo                                                                23DEC04.668
          enddo                                                                  23DEC04.669
                                                                                 23DEC04.670
cmic$ do all autoscope                                                           23DEC04.671
c$doacross                                                                       23DEC04.672
c$& local(i,j)                                                                   23DEC04.673
c$omp parallel do default(shared)                                                23DEC04.674
c$omp&private(i,j)                                                               23DEC04.675
        do j=3,jlxm-1                                                            23DEC04.676
        do i=3,ilxm-1                                                            23DEC04.677
                                                                                 23DEC04.678
c  Derivatives for temperature gradient correction                               23DEC04.679
                                                                                 23DEC04.680
      fjt04(i,j) = t0(I,J+2,kl)+t0(I,J-2,kl)-4*(t0(I,J+1,kl)+                    23DEC04.681
     f  t0(I,J-1,kl))+6*t0(I,J,kl)                                               23DEC04.682
      fit04(i,j) = t0(I+2,J,kl)+t0(I-2,J,kl)-4*(t0(I+1,J,kl)+                    23DEC04.683
     f  t0(I-1,J,kl))+6*t0(I,J,kl)                                               23DEC04.684
                                                                                 23DEC04.685
c  Coefficients for subsidiary second-order diffusion                            23DEC04.686
                                                                                 23DEC04.687
      fit0o(i,j) = t0(I,J,kl) - t0(I+1,J,kl)                                     23DEC04.688
      fit0u(i,j) = t0(I,J,kl) - t0(I-1,J,kl)                                     23DEC04.689
      fjt0o(i,j) = t0(I,J,kl) - t0(I,J+1,kl)                                     23DEC04.690
      fjt0u(i,j) = t0(I,J,kl) - t0(I,J-1,kl)                                     23DEC04.691
                                                                                 23DEC04.692
      fion(i,j) = rmz/(rmz+fit0o(i,j)**6)                                        23DEC04.693
      fiun(i,j) = rmz/(rmz+fit0u(i,j)**6)                                        23DEC04.694
      fjon(i,j) = rmz/(rmz+fjt0o(i,j)**6)                                        23DEC04.695
      fjun(i,j) = rmz/(rmz+fjt0u(i,j)**6)                                        23DEC04.696
                                                                                 23DEC04.697
      fionf(i,j) = rmzf/(rmzf+fit0o(i,j)**6)                                     23DEC04.698
      fiunf(i,j) = rmzf/(rmzf+fit0u(i,j)**6)                                     23DEC04.699
      fjonf(i,j) = rmzf/(rmzf+fjt0o(i,j)**6)                                     23DEC04.700
      fjunf(i,j) = rmzf/(rmzf+fjt0u(i,j)**6)                                     23DEC04.701
                                                                                 23DEC04.702
      enddo                                                                      23DEC04.703
      enddo                                                                      23DEC04.704
                                                                                 23DEC04.705
cmic$ do all autoscope                                                           23DEC04.706
c$doacross                                                                       23DEC04.707
c$& local(i,j,k)                                                                 23DEC04.708
c$omp parallel do default(shared)                                                23DEC04.709
c$omp&private(i,j,k)                                                             23DEC04.710
        do k=kmvl+1,kl                                                           23DEC04.711
        do j=3,jlxm-1                                                            23DEC04.712
        do i=3,ilxm-1                                                            23DEC04.713
                                                                                 23DEC04.714
        gezipwh(i,j,k) = (1-min(0.5,(k-kmvl)/(2.*(kmi(i,j)-kmvl+1))))*           23DEC04.715
     f  min(1,max(0,kmip(i,j,in)+1-k))                                           23DEC04.716
        gezimwh(i,j,k) = (1-min(0.5,(k-kmvl)/(2.*(kmi(i,j)-kmvl+1))))*           23DEC04.717
     f  min(1,max(0,kmim(i,j,in)+1-k))                                           23DEC04.718
        gezjpwh(i,j,k) = (1-min(0.5,(k-kmvl)/(2.*(kmj(i,j)-kmvl+1))))*           23DEC04.719
     f  min(1,max(0,kmjp(i,j,in)+1-k))                                           23DEC04.720
        gezjmwh(i,j,k) = (1-min(0.5,(k-kmvl)/(2.*(kmj(i,j)-kmvl+1))))*           23DEC04.721
     f  min(1,max(0,kmjm(i,j,in)+1-k))                                           23DEC04.722
                                                                                 23DEC04.723
        enddo                                                                    23DEC04.724
        enddo                                                                    23DEC04.725
        enddo                                                                    23DEC04.726
                                                                                 23DEC04.727
cmic$ do all autoscope                                                           23DEC04.728
c$doacross                                                                       23DEC04.729
c$& local(i,j,k)                                                                 23DEC04.730
c$omp parallel do default(shared)                                                23DEC04.731
c$omp&private(i,j,k)                                                             23DEC04.732
        do k=zdiffst(inest),kl                                                   23DEC04.733
        do j=3,jlxm-1                                                            23DEC04.734
        do i=3,ilxm-1                                                            23DEC04.735
                                                                                 23DEC04.736
        mrkjp2(i,j,k) = mod(rkjp2(i,j,k,in),1.)                                  23DEC04.737
        irkjp2(i,j,k) = int(rkjp2(i,j,k,in))                                     23DEC04.738
        mrkjm2(i,j,k) = mod(rkjm2(i,j,k,in),1.)                                  23DEC04.739
        irkjm2(i,j,k) = int(rkjm2(i,j,k,in))                                     23DEC04.740
        mrkjp1(i,j,k) = mod(rkjp1(i,j,k,in),1.)                                  23DEC04.741
        irkjp1(i,j,k) = int(rkjp1(i,j,k,in))                                     23DEC04.742
        mrkjm1(i,j,k) = mod(rkjm1(i,j,k,in),1.)                                  23DEC04.743
        irkjm1(i,j,k) = int(rkjm1(i,j,k,in))                                     23DEC04.744
        mrkip2(i,j,k) = mod(rkip2(i,j,k,in),1.)                                  23DEC04.745
        irkip2(i,j,k) = int(rkip2(i,j,k,in))                                     23DEC04.746
        mrkim2(i,j,k) = mod(rkim2(i,j,k,in),1.)                                  23DEC04.747
        irkim2(i,j,k) = int(rkim2(i,j,k,in))                                     23DEC04.748
        mrkip1(i,j,k) = mod(rkip1(i,j,k,in),1.)                                  23DEC04.749
        irkip1(i,j,k) = int(rkip1(i,j,k,in))                                     23DEC04.750
        mrkim1(i,j,k) = mod(rkim1(i,j,k,in),1.)                                  23DEC04.751
        irkim1(i,j,k) = int(rkim1(i,j,k,in))                                     23DEC04.752
                                                                                 23DEC04.753
        enddo                                                                    23DEC04.754
        enddo                                                                    23DEC04.755
        enddo                                                                    23DEC04.756
                                                                                 23DEC04.757
        endif  ! Calculation of coefficients                                     23DEC04.758
                                                                                 23DEC04.759
C_FLIC_RUNPAD(0)                                                                 23DEC04.760
      endif  ! z-diffusion                                                       23DEC04.761
                                                                                 23DEC04.762
                                                                                 23DEC04.763
C                                                                                SOLVE.616
C --- STORE RAIN ACCUMULATION BEFORE ADDING TO IT                                SOLVE.617
C                                                                                SOLVE.618
      IF(ISOIL.GE.2 .OR. IMOIAV(INEST).GE.1 .OR. IFSNOW(INEST).EQ.2)THEN         05DEC01.471
cmic$ do all autoscope                                                           SOLVE.620
cmic$2 private(i,j)                                                              SOLVE.621
c$doacross                                                                       SOLVE.622
c$& local(i,j)                                                                   SOLVE.623
c$omp parallel do default(shared)                                                SOLVE.624
c$omp&private(i,j)                                                               SOLVE.625
        DO J=1,JLX                                                               SOLVE.626
          DO I=1,ILX                                                             SOLVE.627
            PRECIP2D(I,J)=RAINNC(I,J)+RAINC(I,J)                                 SOLVE.628
            SNOWNCT(I,J)=0.                                                      19DEC02.1251
          ENDDO                                                                  SOLVE.629
        ENDDO                                                                    SOLVE.630
      ENDIF                                                                      SOLVE.631
C=======================================================================         23DEC04.764
C CUMULUS SCHEMES MOVED AWAY FROM HERE.                                          23DEC04.765
C They are now located BELOW the PBL schemes                                     23DEC04.766
C=======================================================================         23DEC04.767
C                                                                                SOLVE.808
C     UCD AND VCD WILL BE USED TO STORE CROSS-POINT MOMENTUM TENDENCIES          SOLVE.809
C     UCC AND VCC WILL BE USED TO STORE PBL T AND QV TENDENCIES FOR              SOLVE.810
C         SHALLOW CONVECTION                                                     SOLVE.811
C                                                                                SOLVE.812
C_FLIC_RUNPAD(2)                                                                 SOLVE.813
cmic$ do all autoscope                                                           SOLVE.814
c$doacross                                                                       SOLVE.815
c$& local(i,j,k)                                                                 SOLVE.816
c$omp parallel do default(shared)                                                SOLVE.817
c$omp&private(i,j,k)                                                             SOLVE.818
      DO J=1,JLX                                                                 SOLVE.819
        DO K=1,KL                                                                SOLVE.820
          DO I=1,ILX                                                             SOLVE.821
            UCD(I,J,K)=0.0                                                       SOLVE.822
            VCD(I,J,K)=0.0                                                       SOLVE.823
            UCC(I,J,K)=T3DTEN(I,J,K)                                             SOLVE.824
            VCC(I,J,K)=QV3DTEN(I,J,K)                                            SOLVE.825
          ENDDO                                                                  SOLVE.826
        ENDDO                                                                    SOLVE.827
      ENDDO                                                                      SOLVE.828
C_FLIC_RUNPAD(0)                                                                 SOLVE.829
C                                                                                SOLVE.830
C *** COMPUTE RADIATION TERM:                                                    SOLVE.831
C                                                                                SOLVE.832
C                                                                                19DEC02.1252
C FOR SEA ICE ONLY (IEXSI.GE.1)                                                  19DEC02.1253
C STORE TG**4 AREA WEIGHTED AVG IN TGB FOR RADIATION CALCULATIONS                19DEC02.1254
C STORE ORIGINAL TG IN TGSTOR                                                    19DEC02.1255
C                                                                                19DEC02.1256
      IF(IEXSI.GE.1)THEN                                                         19DEC02.1257
        DO J=1,JLX                                                               19DEC02.1258
          DO I=1,ILX                                                             19DEC02.1259
            TGSTOR(I,J)=TGB(I,J)                                                 19DEC02.1260
            IF(SEAICE(I,J).GT.0.02)THEN                                          19DEC02.1261
              TGB(I,J)=(SEAICE(I,J)*TGSI(I,J)**4+                                19DEC02.1262
     +                  (1-SEAICE(I,J))*271.4**4)**0.25                          19DEC02.1263
            ENDIF                                                                19DEC02.1264
          ENDDO                                                                  19DEC02.1265
        ENDDO                                                                    19DEC02.1266
      ENDIF                                                                      19DEC02.1267
#ifdef FRAD1                                                                     SOLVE.833
C                                                                                SOLVE.834
C --- IFRAD=1: SIMPLE RADIATIVE COOLING TERM                                     SOLVE.835
C                                                                                SOLVE.836
      IF(IFRAD.EQ.1)THEN                                                         SOLVE.837
        CALL RADCOOL(T3DTEN,T3D,PSA,IBNES,IENES-1,JBNES,JENES-1,KL)              SOLVE.838
      ENDIF                                                                      SOLVE.839
#endif                                                                           SOLVE.840
#ifdef FRAD2                                                                     SOLVE.841
C                                                                                SOLVE.842
C --- IFRAD=2: CLOUD/RADIATIVE SCHEME (DUDHIA 1989)                              SOLVE.843
C                                                                                SOLVE.844
      IF(IFRAD.EQ.2)THEN                                                         SOLVE.845
        IF(MOD(KTAU,NTRAD(INEST)).EQ.0)THEN                                      SOLVE.846
cmic$ do all autoscope                                                           SOLVE.847
cmic$1 shared(inest,psa,rtten,glw,gsw,xlat,xlong,tga,emiss,alb,                  SOLVE.848
cmic$1        t3d,qv3d,qc3d,qr3d,qi3d,qni3d,qg3d,pp3d)                           SOLVE.849
cmic$2 private(j)                                                                SOLVE.850
c$doacross                                                                       SOLVE.851
c$& local(j)                                                                     SOLVE.852
c$omp parallel do default(shared)                                                SOLVE.853
c$omp&private(j)                                                                 SOLVE.854
Ccsd$ parallel do private(j) schedule(static,1)                                  23DEC04.768
          DO J=1,JLX                                                             SOLVE.855
#ifdef MPP1                                                                      SOLVE.856
            DUMJ(J)=J                                                            SOLVE.857
#endif                                                                           SOLVE.858
            CALL LWRAD(J,RTTEN,PSA,GLW,OLW,XLAT,XLONG,TGA,EMISS,T3D,             07NOV00.1011
     +           QV3D,QC3D,QR3D,QI3D,QNI3D,PP3D,INEST)                           07NOV00.1012
            CALL SWRAD(J,RTTEN,PSA,GSW,OSW,XLAT,XLONG,ALB,T3D,                   07NOV00.1013
     +           QV3D,QC3D,QR3D,QI3D,QNI3D,QG3D,PP3D,INEST,HT)                   23DEC04.769
          ENDDO                                                                  SOLVE.863
Ccsd$ end parallel do                                                            23DEC04.770
        ENDIF                                                                    SOLVE.864
cmic$ do all autoscope                                                           SOLVE.865
c$doacross                                                                       SOLVE.866
c$& local(i,j,k)                                                                 SOLVE.867
c$omp parallel do default(shared)                                                SOLVE.868
c$omp&private(i,j,k)                                                             SOLVE.869
        DO J=JBNES,JENES-1                                                       SOLVE.870
          DO K=1,KL                                                              SOLVE.871
            DO I=IBNES,IENES-1                                                   SOLVE.872
              T3DTEN(I,J,K)=T3DTEN(I,J,K)+RTTEN(I,J,K)*PSA(I,J)                  SOLVE.873
            ENDDO                                                                SOLVE.874
          ENDDO                                                                  SOLVE.875
        ENDDO                                                                    SOLVE.876
      ENDIF                                                                      SOLVE.877
#endif                                                                           SOLVE.878
#ifdef FRAD3                                                                     SOLVE.879
C                                                                                SOLVE.880
C --- IFRAD=3: CCM2 RADIATIVE SCHEME                                             SOLVE.881
C                                                                                SOLVE.882
      IF(IFRAD.EQ.3)THEN                                                         SOLVE.883
        IF(MOD(KTAU,NTRAD(INEST)).EQ.0)THEN                                      SOLVE.884
cmic$ do all autoscope                                                           SOLVE.885
cmic$1 shared(ibnes,ienes,jbnes,jenes,kl,inest,osw,olw,tgb,emiss,                05DEC01.504
cmic$1     t3dten,rtten,psa,glw,gsw,ktau,iutrad,nradisp,t3d,qv3d,pp3d)           SOLVE.887
cmic$2 private(i,j,k,cldfra,frla,frsa,firtp,sabtp)                               05DEC01.505
c$doacross                                                                       SOLVE.889
c$& local(i,j,k,cldfra,frla,frsa,firtp,sabtp)                                    05DEC01.506
c$omp parallel do default(shared)                                                SOLVE.891
c$omp&private(i,j,k,cldfra,frla,frsa,firtp,sabtp)                                05DEC01.507
          DO J=1,JLX                                                             SOLVE.893
            DO K=1,KL                                                            SOLVE.894
              DO I=1,ILX                                                         SOLVE.895
                CLDFRA(I,K)=CLDFRA3D(I,J,K)                                      SOLVE.896
              ENDDO                                                              SOLVE.897
            ENDDO                                                                SOLVE.898
            CALL COLMOD(J,KTAU,IUTRAD,NRADISP,CLDFRA,FRLA,FRSA,                  05DEC01.508
     +                  FIRTP,SABTP,T3D,QV3D,PP3D,QC3D,QR3D,QI3D,                05DEC01.509
     -                  QNI3D,IMPHYS(INEST),ICLOUD(INEST))                       05DEC01.510
            DO I=1,ILX                                                           SOLVE.901
              GLW(I,J)=-FRLA(I)/EMISS(I,J)+STBOLT*TGB(I,J)**4                    25JAN00.286
              GSW(I,J)=FRSA(I)                                                   SOLVE.903
              OSW(I,J)=SABTP(I)                                                  05DEC01.511
              OLW(I,J)=FIRTP(I)                                                  05DEC01.512
            ENDDO                                                                SOLVE.904
          ENDDO                                                                  SOLVE.905
        ENDIF                                                                    SOLVE.906
C                                                                                SOLVE.907
cmic$ do all autoscope                                                           SOLVE.908
c$doacross                                                                       SOLVE.909
c$& local(i,j,k)                                                                 SOLVE.910
c$omp parallel do default(shared)                                                SOLVE.911
c$omp&private(i,j,k)                                                             SOLVE.912
        DO J=JBNES,JENES-1                                                       SOLVE.913
          DO K=1,KL                                                              SOLVE.914
            DO I=IBNES,IENES-1                                                   SOLVE.915
              T3DTEN(I,J,K)=T3DTEN(I,J,K)+RTTEN(I,J,K)*PSA(I,J)                  SOLVE.916
            ENDDO                                                                SOLVE.917
          ENDDO                                                                  SOLVE.918
        ENDDO                                                                    SOLVE.919
      ENDIF                                                                      SOLVE.920
#endif                                                                           SOLVE.921
#ifdef FRAD4                                                                     25JAN00.287
C                                                                                25JAN00.288
C --- IFRAD=4: RRTM LONGWAVE + CLOUD/RADIATIVE SHORTWAVE (SWRAD)                 25JAN00.289
C                                                                                25JAN00.290
      IF(IFRAD.EQ.4)THEN                                                         25JAN00.291
        IF(MOD(KTAU,NTRAD(INEST)).EQ.0)THEN                                      25JAN00.292
cmic$ do all autoscope                                                           25JAN00.293
cmic$1 shared(inest,psa,rtten,glw,gsw,xlat,xlong,tga,emiss,alb,                  25JAN00.294
cmic$1        t3d,qv3d,qc3d,qr3d,qi3d,qni3d,qg3d,pp3d)                           25JAN00.295
cmic$2 private(i,j,k,cldfra)                                                     23DEC04.771
c$doacross                                                                       25JAN00.297
c$& local(i,j,k,cldfra)                                                          23DEC04.772
c$omp parallel do default(shared)                                                25JAN00.299
c$omp&private(i,j,k,cldfra)                                                      23DEC04.773
          DO J=1,JLX                                                             25JAN00.301
#ifdef MPP1                                                                      25JAN00.302
            DUMJ(J)=J                                                            25JAN00.303
#endif                                                                           25JAN00.304
            DO K=1,KL                                                            25JAN00.305
              DO I=1,ILX                                                         25JAN00.306
                CLDFRA(I,K)=CLDFRA3D(I,J,K)                                      25JAN00.307
              ENDDO                                                              25JAN00.308
            ENDDO                                                                25JAN00.309
            CALL RRTM(J,ILX,CLDFRA,INEST,GLW,OLW,RTTEN,T3D,QV3D,QC3D             07NOV00.1020
     +           ,QR3D,QI3D,QNI3D,QG3D,PP3D,PSA,TGA,EMISS)                       07NOV00.1021
            CALL SWRAD(J,RTTEN,PSA,GSW,OSW,XLAT,XLONG,ALB,T3D,QV3D,QC3D          07NOV00.1022
     +           ,QR3D,QI3D,QNI3D,QG3D,PP3D,INEST,HT)                            23DEC04.774
          ENDDO                                                                  25JAN00.314
        ENDIF                                                                    25JAN00.315
cmic$ do all autoscope                                                           25JAN00.316
c$doacross                                                                       25JAN00.317
c$& local(i,j,k)                                                                 25JAN00.318
c$omp parallel do default(shared)                                                25JAN00.319
c$omp&private(i,j,k)                                                             25JAN00.320
        DO J=JBNES,JENES-1                                                       25JAN00.321
          DO K=1,KL                                                              25JAN00.322
            DO I=IBNES,IENES-1                                                   25JAN00.323
              T3DTEN(I,J,K)=T3DTEN(I,J,K)+RTTEN(I,J,K)*PSA(I,J)                  25JAN00.324
            ENDDO                                                                25JAN00.325
          ENDDO                                                                  25JAN00.326
        ENDDO                                                                    25JAN00.327
      ENDIF                                                                      25JAN00.328
#endif                                                                           25JAN00.329
C                                                                                19DEC02.1268
C FOR SEA ICE ONLY (IEXSI.GE.1)                                                  19DEC02.1269
C COPY TGSTOR TO TG (RETURN ORIGINAL VALUE TO TG)                                19DEC02.1270
C                                                                                19DEC02.1271
      IF(IEXSI.GE.1)THEN                                                         19DEC02.1272
        DO J=1,JLX                                                               19DEC02.1273
          DO I=1,ILX                                                             19DEC02.1274
            TGB(I,J)=TGSTOR(I,J)                                                 19DEC02.1275
          ENDDO                                                                  19DEC02.1276
        ENDDO                                                                    19DEC02.1277
      ENDIF                                                                      19DEC02.1278
C                                                                                SOLVE.922
C *** COMPUTE PBL TENDENCIES:                                                    SOLVE.923
C                                                                                SOLVE.924
#ifdef IBLT1                                                                     SOLVE.925
C                                                                                SOLVE.926
C --- IBLTYP=1: BULK PBL                                                         SOLVE.927
C                                                                                SOLVE.928
      IF(IBLTYP(INEST).EQ.1)THEN                                                 SOLVE.929
cmic$ do all autoscope                                                           SOLVE.930
cmic$1 shared(iyy,jxx,jbnes,jenes,inest,u3d,v3d,t3d,qv3d,pp3d,qc3d,              SOLVE.931
cmic$1        ucd,vcd,qc3dten,t3dten,qv3dten)                                    SOLVE.932
cmic$2 private(j)                                                                SOLVE.933
c$doacross                                                                       SOLVE.934
c$& local(j)                                                                     SOLVE.935
c$omp parallel do default(shared)                                                SOLVE.936
c$omp&private(j)                                                                 SOLVE.937
        DO J=JBNES,JENES                                                         SOLVE.938
          PARJSUM(ITQEVA_SUM,J)=0.0                                              SOLVE.939
          CALL BLKPBL(IYY,JXX,J,INEST,U3D,V3D,T3D,QV3D,PP3D,QC3D,UCD,VCD         SOLVE.940
     +         ,QC3DTEN,T3DTEN,QV3DTEN)                                          SOLVE.941
        ENDDO                                                                    SOLVE.942
      ENDIF                                                                      SOLVE.943
#endif                                                                           SOLVE.944
#ifdef IBLT2                                                                     SOLVE.945
C                                                                                SOLVE.946
C --- IBLTYP=2: BLACKADAR PBL                                                    SOLVE.947
C                                                                                SOLVE.948
      IF(IBLTYP(INEST).EQ.2)THEN                                                 SOLVE.949
cmic$ do all autoscope                                                           SOLVE.950
cmic$1 shared(iyy,jxx,inest,u3d,v3d,t3d,qv3d,pp3d,qc3d,qi3d,qnc3d,               SOLVE.951
cmic$1        ucd,vcd,qc3dten,t3dten,qv3dten,qi3dten,qnc3dten)                   SOLVE.952
cmic$2 private(j)                                                                SOLVE.953
c$doacross                                                                       SOLVE.954
c$& local(j)                                                                     SOLVE.955
c$omp parallel do default(shared)                                                SOLVE.956
c$omp&private(j)                                                                 SOLVE.957
        DO J=1,JLX                                                               SOLVE.958
          PARJSUM(ITQEVA_SUM,J)=0.0                                              SOLVE.959
          CALL HIRPBL(IYY,JXX,J,INEST,U3D,V3D,T3D,QV3D,PP3D,QC3D,QI3D,           SOLVE.960
     +         QNC3D,UCD,VCD,QC3DTEN,T3DTEN,QV3DTEN,QI3DTEN,QNC3DTEN)            SOLVE.961
        ENDDO                                                                    SOLVE.962
      ENDIF                                                                      SOLVE.963
#endif                                                                           SOLVE.964
#ifdef IBLT3                                                                     SOLVE.965
C                                                                                SOLVE.966
C --- IBLTYP=3: NAVY BURT-THOMPSON M-Y PBL                                       SOLVE.967
C                                                                                SOLVE.968
      IF(IBLTYP(INEST).EQ.3)THEN                                                 SOLVE.969
cmic$ do all autoscope                                                           SOLVE.970
cmic$1 shared(iyy,jxx,inest,nn,ibnes,jbnes,ienes,jenes,                          SOLVE.971
cmic$1        u3d,v3d,t3d,qv3d,pp3d,ptop,a,ucd,vcd,t3dten,qv3dten,               SOLVE.972
cmic$1        pdotb,psurf,p,mkx,ktau,dt,mix,mjx,isfflx,xtime)                    SOLVE.973
cmic$2 private(i,j,k)                                                            SOLVE.974
c$doacross                                                                       SOLVE.975
c$& local(i,j,k)                                                                 SOLVE.976
c$omp parallel do default(shared)                                                SOLVE.977
c$omp&private(i,j,k,kk)                                                          05DEC01.513
        DO J=1,JLX                                                               SOLVE.979
          DO I=1,ILX                                                             SOLVE.980
            PSURF(I,J)=10.*PSB(I,J)+10.*PTOP+PP3D(I,J,MKX)/100.                  SOLVE.981
            DO K=1,MKX                                                           SOLVE.982
              KK=MKX-K+1                                                         SOLVE.983
              P(I,J,KK)=.01*PSB(I,J)*A(K)+.01*PTOP+PP3D(I,J,K)/100000.           SOLVE.984
            ENDDO                                                                SOLVE.985
          ENDDO                                                                  SOLVE.986
          PARJSUM(ITQEVA_SUM,J)=0.0                                              SOLVE.987
C                                                                                SOLVE.988
          CALL NAVYPB(U3D,V3D,T3D,QV3D,PP3D,PDOTB,PSURF,P,NN,INEST,J,MKX         SOLVE.989
     +         ,KTAU,DT,MIX,MJX,T3DTEN,QV3DTEN,UCD,VCD,QC3DTEN,QR3DTEN,          SOLVE.990
     +         ISFFLX(INEST),XTIME,A,PTOP,IYY,JXX)                               SOLVE.991
        ENDDO                                                                    SOLVE.992
      ENDIF                                                                      SOLVE.993
#endif                                                                           SOLVE.994
#ifdef IBLT4                                                                     SOLVE.995
C                                                                                SOLVE.996
C --- IBLTYP=4: ETA M-Y PBL                                                      SOLVE.997
C                                                                                SOLVE.998
      IF(IBLTYP(INEST).EQ.4)THEN                                                 SOLVE.999
cmic$ do all autoscope                                                           SOLVE.1000
cmic$1 shared(iyy,jxx,kzz,u3d,v3d,t3d,qv3d,pp3d,                                 SOLVE.1001
cmic$1        ucd,vcd,t3dten,qv3dten,dt,ktau,inest,itgflg,isoil)                 SOLVE.1002
cmic$2 private(j)                                                                SOLVE.1003
c$doacross                                                                       SOLVE.1004
c$& local(j)                                                                     SOLVE.1005
c$omp parallel do default(shared)                                                SOLVE.1006
c$omp&private(j)                                                                 SOLVE.1007
        DO J=1,JLX                                                               SOLVE.1008
#ifdef MPP1                                                                      05DEC01.514
          DUMJ(J)=J                                                              05DEC01.515
#endif                                                                           05DEC01.516
          CALL MYEPBL(J,IYY,JXX,KZZ,DT,KTAU,INEST,ITGFLG(INEST),ISOIL,           SOLVE.1009
     +         U3D,V3D,T3D,QV3D,PP3D,UCD,VCD,T3DTEN,QV3DTEN)                     SOLVE.1010
        ENDDO                                                                    SOLVE.1011
      ENDIF                                                                      SOLVE.1012
#endif                                                                           SOLVE.1013
#ifdef IBLT5                                                                     07NOV00.1024
C                                                                                SOLVE.1015
C --- IBLTYP=5: MRF PBL                                                          SOLVE.1016
C                                                                                SOLVE.1017
      IF(IBLTYP(INEST).EQ.5)THEN                                                 SOLVE.1018
        ICUT = 0                                                                 07NOV00.1025
        IF ( INEST .GT. 1 ) ICUT = 1                                             07NOV00.1026
        DO J = 1,JLX                                                             07NOV00.1027
          DO I = 1,ILX                                                           07NOV00.1028
            IF ( I .GE. (2+ICUT) .AND. I .LE. (ILX-1-ICUT) .AND.                 07NOV00.1029
     +           J .GE. (2+ICUT) .AND. J .LE. (JLX-1-ICUT)) THEN                 07NOV00.1030
              INTERIOR_MASK(I,J) = 1.                                            07NOV00.1031
            ELSE                                                                 07NOV00.1032
              INTERIOR_MASK(I,J) = 0.                                            07NOV00.1033
            ENDIF                                                                07NOV00.1034
          ENDDO                                                                  07NOV00.1035
        ENDDO                                                                    07NOV00.1036
#ifndef VECTOR                                                                   07NOV00.1037
cmic$ do all autoscope                                                           SOLVE.1019
cmic$1 shared(iyy,jxx,inest,u3d,v3d,t3d,qv3d,pp3d,qc3d,qi3d,qnc3d,               SOLVE.1020
cmic$1        ucd,vcd,qc3dten,t3dten,qv3dten,qi3dten,qnc3dten)                   SOLVE.1021
cmic$2 private(j)                                                                SOLVE.1022
c$doacross                                                                       SOLVE.1023
c$& local(j)                                                                     SOLVE.1024
c$omp parallel do default(shared)                                                SOLVE.1025
c$omp&private(j)                                                                 SOLVE.1026
Ccsd$ parallel do private(j) schedule(static,1)                                  23DEC04.775
        DO J=1,JLX                                                               07NOV00.1038
          PARJSUM(ITQEVA_SUM,J)=0.0                                              SOLVE.1028
          CALL MRFPBL(IYY,JXX,J,INEST,U3D,V3D,T3D,QV3D,PP3D,QC3D,QI3D,           SOLVE.1029
     +         QNC3D,UCD,VCD,QC3DTEN,T3DTEN,QV3DTEN,QI3DTEN,QNC3DTEN,            07NOV00.1039
     +         TGA,TGB,PSB,RPSB,XLV,                                             07NOV00.1040
     +         TA2,QA2,UA10,VA10,                                                07NOV00.1041
     +         SVP1,SVP2,SVP3,SVPT0,EP1,EP2,ZNT,XLAND,UST,                       07NOV00.1042
     +         MAVAIL,                                                           07NOV00.1043
     +         REGIME,HOL,PBL,ZOL,MOL,QFX,HFX,RV,TSLA,TSLB,T0,PPB,               07NOV00.1044
     +         HFXSI,QFXSI,TGSI,SEAICE,                                          23DEC04.776
     +         EMISS,GLW,GSW,TMN,CAPG,SNOWC,XLAT,XLONG,                          07NOV00.1045
     +         RAINP,RAINC,RAINNC,                                               07NOV00.1046
     +         PRW,ALB,SHC,THC,SATBRT,                                           07NOV00.1047
     +         XMOIST,ISICE,                                                     23DEC04.777
     +         INTERIOR_MASK,                                                    07NOV00.1049
     +         1,ILX)                                                            07NOV00.1050
        ENDDO                                                                    SOLVE.1031
Ccsd$ end parallel do                                                            23DEC04.778
#else                                                                            07NOV00.1051
#  include <call_vector_mrfpbl.incl>                                             07NOV00.1052
#endif                                                                           07NOV00.1053
      ENDIF                                                                      SOLVE.1032
#endif                                                                           SOLVE.1033
#ifdef IBLT6                                                                     SOLVE.1034
C                                                                                SOLVE.1035
C --- IBLTYP=6: GAYNO-SEAMAN M-Y PBL                                             SOLVE.1036
C                                                                                SOLVE.1037
      IF(IBLTYP(INEST).EQ.6)THEN                                                 SOLVE.1038
C                                                                                SOLVE.1039
C OPTION ASSOCIATED TO GSPBL ROUTINE                                             SOLVE.1040
C                                                                                SOLVE.1041
C   IADVTKE=1 FOR CONSIDERING ADVECTION AND DIFFUSION OF TKE                     SOLVE.1042
C   IADVTKE=0 FOR NOT CONSIDERING THEM                                           SOLVE.1043
C                                                                                SOLVE.1044
        IADVTKE=1                                                                SOLVE.1045
C                                                                                SOLVE.1046
C COUPLING OF TKEA WITH PSA                                                      SOLVE.1047
C                                                                                SOLVE.1048
cmic$ do all autoscope                                                           SOLVE.1049
c$doacross                                                                       SOLVE.1050
c$& local(i,j,k)                                                                 SOLVE.1051
c$omp parallel do default(shared)                                                SOLVE.1052
c$omp&private(i,j,k)                                                             SOLVE.1053
        DO J=1,JLX                                                               SOLVE.1054
          DO I=1,ILX                                                             SOLVE.1055
            DO K=1,KL+1                                                          SOLVE.1056
              TKEAPSA(I,J,K)=TKEA(I,J,K)*PSA(I,J)                                SOLVE.1057
            ENDDO                                                                SOLVE.1058
          ENDDO                                                                  SOLVE.1059
        ENDDO                                                                    SOLVE.1060
C                                                                                SOLVE.1061
C CALL TO GSPBL ROUTINE                                                          SOLVE.1062
C                                                                                SOLVE.1063
cmic$ do all autoscope                                                           SOLVE.1064
cmic$1 shared(iyy,jxx,kzz,inest,nn,u3d,v3d,t3d,qv3d,qc3d,                        SOLVE.1065
cmic$1        pp3d,ucd,vcd,qc3dten,t3dten,qv3dten,tketend,iadvtke)               SOLVE.1066
cmic$2 private(j)                                                                SOLVE.1067
c$doacross                                                                       SOLVE.1068
c$& local(j)                                                                     SOLVE.1069
c$omp parallel do default(shared)                                                SOLVE.1070
c$omp&private(j)                                                                 SOLVE.1071
        DO J=1,JLX                                                               SOLVE.1072
#ifdef MPP1                                                                      25JAN00.330
          DUMJ(J)=J                                                              25JAN00.331
#endif                                                                           25JAN00.332
          PARJSUM(ITQEVA_SUM,J)=0.0                                              25JAN00.333
          CALL GSPBL(IYY,JXX,KZZ,J,INEST,NN,U3D,V3D,T3D,QV3D,QC3D,PP3D,          SOLVE.1073
     +         UCD,VCD,QC3DTEN,T3DTEN,QV3DTEN,TKETEND,IADVTKE)                   SOLVE.1074
        ENDDO                                                                    SOLVE.1075
C                                                                                SOLVE.1076
C CALCULATION OF NEXT TKE OR TKE TENDENCY                                        SOLVE.1077
C                                                                                SOLVE.1078
        IF(IADVTKE.EQ.0)THEN                                                     SOLVE.1079
        DTTKE=DT/2.0                                                             07NOV00.1054
cmic$ do all autoscope                                                           SOLVE.1080
c$doacross                                                                       SOLVE.1081
c$& local(i,j,k)                                                                 SOLVE.1082
c$omp parallel do default(shared)                                                SOLVE.1083
c$omp&private(i,j,k)                                                             SOLVE.1084
          DO J=1,JLX                                                             SOLVE.1085
            DO K=1,KL                                                            SOLVE.1087
              DO I=1,ILX                                                         SOLVE.1088
                TKEB(I,J,K)=AMAX1(0.,TKEB(I,J,K)+DTTKE*TKETEND(I,J,K))           SOLVE.1089
                TKEA(I,J,K) = TKEB(I,J,K)                                        23DEC04.779
              ENDDO                                                              SOLVE.1090
            ENDDO                                                                SOLVE.1091
          ENDDO                                                                  SOLVE.1092
        ELSE                                                                     SOLVE.1093
#ifdef MPP1                                                                      25JAN00.334
#  include <mpp_solve_30.incl>                                                   25JAN00.335
#endif                                                                           25JAN00.336
cmic$ do all autoscope                                                           SOLVE.1094
c$doacross                                                                       SOLVE.1095
c$& local(i,j,k)                                                                 SOLVE.1096
c$omp parallel do default(shared)                                                SOLVE.1097
c$omp&private(i,j,k)                                                             SOLVE.1098
          DO J=1,MJX                                                             25JAN00.337
            DO K=1,KLP1                                                          SOLVE.1100
              DO I=1,MIX                                                         25JAN00.338
                TKETENDADV(I,J,K)=0.0                                            SOLVE.1102
              ENDDO                                                              SOLVE.1103
            ENDDO                                                                SOLVE.1104
          ENDDO                                                                  SOLVE.1105
          CALL HADV(KZZ+1,TKETENDADV,UA,VA,TKEA,MSFX,DX4,4,INEST,'E')            23DEC04.780
          CALL VADV(KZZ+1,TKETENDADV,TKEAPSA,QDOT,PSA,5,INEST)                   SOLVE.1107
          CALL DIFFU(KZZ+1,TKETENDADV,TKEB,PSB,CRSXK,C203,IBNES,IENES-1,         SOLVE.1108
     +         JBNES,JENES-1,KL,INEST)                                           SOLVE.1109
cmic$ do all autoscope                                                           SOLVE.1110
c$doacross                                                                       SOLVE.1111
c$& local(i,j,k)                                                                 SOLVE.1112
c$omp parallel do default(shared)                                                SOLVE.1113
c$omp&private(i,j,k)                                                             SOLVE.1114
          DO J=JBNES,JENES-1                                                     SOLVE.1115
            DO K=2,KL                                                            SOLVE.1116
              DO I=IBNES,IENES-1                                                 SOLVE.1117
                TKETENDADV(I,J,K)=TKETENDADV(I,J,K)+(TKEA(I,J,K)*(               SOLVE.1118
     +                            TWT(K,1)*DIVX(I,J,K)+TWT(K,2)*                 SOLVE.1119
     +                            DIVX(I,J,K-1)))                                SOLVE.1120
              ENDDO                                                              SOLVE.1121
            ENDDO                                                                SOLVE.1122
            DO K=1,KL                                                            SOLVE.1123
              DO I=IBNES,IENES-1                                                 SOLVE.1124
                TKETEND(I,J,K)=TKETEND(I,J,K)+TKETENDADV(I,J,K)*                 SOLVE.1125
     +                         RPSA(I,J)                                         SOLVE.1126
              ENDDO                                                              SOLVE.1127
            ENDDO                                                                SOLVE.1128
          ENDDO                                                                  SOLVE.1129
          DTTKE=DT                                                               SOLVE.1130
cmic$ do all autoscope                                                           SOLVE.1131
c$doacross                                                                       SOLVE.1132
c$& local(i,j,k)                                                                 SOLVE.1133
c$omp parallel do default(shared)                                                SOLVE.1134
c$omp&private(i,j,k)                                                             SOLVE.1135
          DO J=JBNES,JENES-1                                                     SOLVE.1136
            DO K=1,KL                                                            SOLVE.1137
              DO I=IBNES,IENES-1                                                 SOLVE.1138
                TKEC(I,J,K)=                                                     SOLVE.1139
     +                      AMAX1(0.001,TKEB(I,J,K)+DTTKE*TKETEND(I,J,K)         SOLVE.1140
     +                      )                                                    SOLVE.1141
                TKEB(I,J,K)=OMUHF*TKEA(I,J,K)+GNUHF*(TKEB(I,J,K)+                SOLVE.1142
     +                      TKEC(I,J,K))                                         SOLVE.1143
                TKEA(I,J,K)=TKEC(I,J,K)                                          SOLVE.1144
              ENDDO                                                              SOLVE.1145
            ENDDO                                                                SOLVE.1146
          ENDDO                                                                  SOLVE.1147
        ENDIF                                                                    SOLVE.1148
      ENDIF                                                                      SOLVE.1149
#endif                                                                           SOLVE.1150
C                                                                                07NOV00.1055
C... PX mods start                                                               07NOV00.1056
C                                                                                07NOV00.1057
#ifdef IBLT7                                                                     07NOV00.1058
C                                                                                07NOV00.1059
C --- IBLTYP=7: PX PBL                                                           07NOV00.1060
C                                                                                07NOV00.1061
      IF(IBLTYP(INEST).EQ.7)THEN                                                 07NOV00.1062
cmic$ do all autoscope                                                           05DEC01.517
cmic$1 shared(iyy,jxx,inest,u3d,v3d,t3d,qv3d,pp3d,qc3d,qi3d,qnc3d,               05DEC01.518
cmic$1        ucd,vcd,qc3dten,t3dten,qv3dten,qi3dten,qnc3dten)                   05DEC01.519
cmic$2 private(j)                                                                05DEC01.520
c$doacross                                                                       05DEC01.521
c$& local(j)                                                                     05DEC01.522
c$omp parallel do default(shared)                                                05DEC01.523
c$omp&private(j)                                                                 05DEC01.524
C                                                                                07NOV00.1064
        DO J=1,JLX                                                               07NOV00.1065
          PARJSUM(ITQEVA_SUM,J)=0.0                                              07NOV00.1066
          CALL PXPBL(IYY,JXX,J,INEST,U3D,V3D,T3D,QV3D,PP3D,QC3D,QI3D,            07NOV00.1067
     +         QNC3D,UCD,VCD,T3DTEN,QC3DTEN,QV3DTEN,QI3DTEN,QNC3DTEN)            07NOV00.1068
        ENDDO                                                                    07NOV00.1069
      ENDIF                                                                      07NOV00.1070
#endif                                                                           07NOV00.1071
C                                                                                SOLVE.1151
      IF(IBLTYP(INEST).GE.1)THEN                                                 07NOV00.1074
#ifdef MPP1                                                                      SOLVE.1153
#  include <mpp_solve_20.incl>                                                   SOLVE.1154
#endif                                                                           SOLVE.1155
        DDJ=.25                                                                  SOLVE.1156
cmic$ do all autoscope                                                           SOLVE.1157
c$doacross                                                                       SOLVE.1158
c$& local(i,j,k)                                                                 SOLVE.1159
c$omp parallel do default(shared)                                                SOLVE.1160
c$omp&private(i,j,k)                                                             SOLVE.1161
        DO J=JBNES,JENES                                                         SOLVE.1162
          DO K=1,KL                                                              SOLVE.1163
            DO I=IBNES,IENES                                                     SOLVE.1164
              U3DTEN(I,J,K)=DDJ*(UCD(I,J-1,K)+UCD(I,J,K)+UCD(I-1,J-1,K)+         SOLVE.1165
     +                      UCD(I-1,J,K))+U3DTEN(I,J,K)                          SOLVE.1166
              V3DTEN(I,J,K)=DDJ*(VCD(I,J-1,K)+VCD(I,J,K)+VCD(I-1,J-1,K)+         SOLVE.1167
     +                      VCD(I-1,J,K))+V3DTEN(I,J,K)                          SOLVE.1168
            ENDDO                                                                SOLVE.1169
          ENDDO                                                                  SOLVE.1170
        ENDDO                                                                    SOLVE.1171
      ENDIF                                                                      SOLVE.1172
#ifndef MPP1                                                                     SOLVE.1173
      DO J = 1, JLX                                                              SOLVE.1174
         TQEVA = TQEVA + PARJSUM(ITQEVA_SUM,J)                                   SOLVE.1175
      ENDDO                                                                      SOLVE.1176
#endif                                                                           SOLVE.1177
C=======================================================================         23DEC04.781
C cumulus schemes moved HERE                                                     23DEC04.782
C                                                                                23DEC04.783
C *** COMPUTE CUMULUS CONVECTIVE TENDENCIES:                                     23DEC04.784
C                                                                                23DEC04.785
#ifdef ICUPA2                                                                    23DEC04.786
C                                                                                23DEC04.787
C --- ICUPA=2: ANTHES KUO                                                        23DEC04.788
C                                                                                23DEC04.789
      IF(ICUPA(INEST).EQ.2)THEN                                                  23DEC04.790
cmic$ do all autoscope                                                           23DEC04.791
cmic$1 shared(IBNES,IENES,JBNES,JENES,INEST,                                     23DEC04.792
cmic$1        T3DTEN,QV3DTEN,T3D,QV3D,PP3D,QDOT,RAINC,PSB)                       23DEC04.793
cmic$2 private(J)                                                                23DEC04.794
c$doacross                                                                       23DEC04.795
c$& local(j)                                                                     23DEC04.796
c$omp parallel do default(shared)                                                23DEC04.797
c$omp&private(j)                                                                 23DEC04.798
        DO J=JBNES,JENES-1                                                       23DEC04.799
#ifdef MPP1                                                                      23DEC04.800
          DUMJ(J)=J                                                              23DEC04.801
#endif                                                                           23DEC04.802
          CALL CUPARA2(J,INEST,IBNES,IENES-1,QV3DTEN,T3DTEN,T3D,QV3D,            23DEC04.803
     +         PP3D,QDOT,RAINC,PSB)                                              23DEC04.804
        ENDDO                                                                    23DEC04.805
      ENDIF                                                                      23DEC04.806
#endif                                                                           23DEC04.807
#ifdef ICUPA3                                                                    23DEC04.808
C                                                                                23DEC04.809
C --- ICUPA=3: GRELL                                                             23DEC04.810
C                                                                                23DEC04.811
      IF(ICUPA(INEST).EQ.3)THEN                                                  23DEC04.812
#ifndef VECTOR                                                                   23DEC04.813
cmic$ do all autoscope                                                           23DEC04.814
cmic$1 shared(ibnes,ienes,jbnes,jenes,inest,u3d,v3d,t3d,qv3d,pp3d,               23DEC04.815
cmic$1        t3dten,qv3dten,psb,rainc,ht)                                       23DEC04.816
cmic$2 private(j,i,k,cldfra)                                                     23DEC04.817
c$doacross                                                                       23DEC04.818
c$& local(j,i,k,cldfra)                                                          23DEC04.819
c$omp parallel do default(shared)                                                23DEC04.820
c$omp&private(j,i,k,cldfra)                                                      23DEC04.821
Ccsd$ parallel do private(j,i,k,cldfra) schedule(static,1)                       23DEC04.822
        DO J=JBNES,JENES-1                                                       23DEC04.823
#ifdef MPP1                                                                      23DEC04.824
          DUMJ(J)=J                                                              23DEC04.825
#endif                                                                           23DEC04.826
          DO K=1,KL                                                              23DEC04.827
            DO I=IBNES,IENES-1                                                   23DEC04.828
               CLDFRA(I,K) = 0.0                                                 23DEC04.829
            END DO                                                               23DEC04.830
          END DO                                                                 23DEC04.831
          CALL CUPARA3(T3D,QV3D,PSB,T3DTEN,QV3DTEN,RAINC,CLDFRA,HT,              23DEC04.832
     +         U3D,V3D,PP3D,                                                     23DEC04.833
     +         RPSB,DT,DX,R,CP,G,TO,XLV,RV,A,PTOP,                               23DEC04.834
     +         INEST,J,IBNES,IENES-1)                                            23DEC04.835
          DO K=1,KL                                                              23DEC04.836
            DO I=IBNES,IENES-1                                                   23DEC04.837
              CLDFRA3D(I,J,K)=CLDFRA(I,K)                                        23DEC04.838
            ENDDO                                                                23DEC04.839
          ENDDO                                                                  23DEC04.840
        ENDDO                                                                    23DEC04.841
Ccsd$ end parallel do                                                            23DEC04.842
#else                                                                            23DEC04.843
#  include <call_vector_cupara3.incl>                                            23DEC04.844
#endif                                                                           23DEC04.845
      ENDIF                                                                      23DEC04.846
#endif                                                                           23DEC04.847
#ifdef ICUPA4                                                                    23DEC04.848
C                                                                                23DEC04.849
C --- ICUPA=4: ARAKAWA-SCHUBERT                                                  23DEC04.850
C                                                                                23DEC04.851
      IF(ICUPA(INEST).EQ.4)THEN                                                  23DEC04.852
cmic$ do all autoscope                                                           23DEC04.853
cmic$1 shared(ibnes,ienes,jbnes,jenes,inest,                                     23DEC04.854
cmic$1        t3dten,qv3dten,t3d,qv3d,psb,rainc,ht,u3d,v3d,pp3d)                 23DEC04.855
cmic$2 private(j)                                                                23DEC04.856
c$doacross                                                                       23DEC04.857
c$& local(j)                                                                     23DEC04.858
c$omp parallel do default(shared)                                                23DEC04.859
c$omp&private(j)                                                                 23DEC04.860
        DO J=JBNES,JENES-1                                                       23DEC04.861
#ifdef MPP1                                                                      23DEC04.862
          DUMJ(J)=J                                                              23DEC04.863
#endif                                                                           23DEC04.864
          CALL CUPARA4(T3D,QV3D,PSB,T3DTEN,QV3DTEN,RAINC,HT,U3D,V3D,             23DEC04.865
     +         PP3D,INEST,J,IBNES,IENES-1)                                       23DEC04.866
        ENDDO                                                                    23DEC04.867
      ENDIF                                                                      23DEC04.868
#endif                                                                           23DEC04.869
#ifdef ICUPA5                                                                    23DEC04.870
C                                                                                23DEC04.871
C --- ICUPA=5: FRITSCH-CHAPPELL                                                  23DEC04.872
C                                                                                23DEC04.873
      IF(ICUPA(INEST).EQ.5)THEN                                                  23DEC04.874
cmic$ do all autoscopE                                                           23DEC04.875
cmic$1 shared(ilx,ibnes,ienes,jbnes,jenes,inest,                                 23DEC04.876
cmic$1        t3dten,qv3dten,u3d,v3d,t3d,qv3d,pp3d,psb,rainc)                    23DEC04.877
cmic$2 private(j,scr1)                                                           23DEC04.878
c$doacross                                                                       23DEC04.879
c$& local(j,scr1)                                                                23DEC04.880
c$omp parallel do default(shared)                                                23DEC04.881
c$omp&private(j,scr1)                                                            23DEC04.882
        DO J=JBNES,JENES-1                                                       23DEC04.883
          DO K=1,KL                                                              23DEC04.884
            DO I=1,ILX                                                           23DEC04.885
              SCR1(I,K)=-5.0E-4*G*RHO0(I,J,K)*(W3D(I,J,K)+W3D(I,J,K+1))          23DEC04.886
            ENDDO                                                                23DEC04.887
          ENDDO                                                                  23DEC04.888
C                                                                                23DEC04.889
          IF(IMPHYS(INEST).GE.6)THEN                                             23DEC04.890
            CALL CUPARA5(PSB,T3DTEN,QV3DTEN,QR3DTEN,QC3DTEN,QG3DTEN,             23DEC04.891
     +           QI3DTEN,RAINC,SCR1,U3D,V3D,T3D,QV3D,PP3D,J,INEST,               23DEC04.892
     +           IMPHYS(INEST),IBNES,IENES-1)                                    23DEC04.893
          ELSE                                                                   23DEC04.894
            CALL CUPARA5(PSB,T3DTEN,QV3DTEN,QR3DTEN,QC3DTEN,QNI3DTEN,            23DEC04.895
     +           QI3DTEN,RAINC,SCR1,U3D,V3D,T3D,QV3D,PP3D,J,INEST,               23DEC04.896
     +           IMPHYS(INEST),IBNES,IENES-1)                                    23DEC04.897
          ENDIF                                                                  23DEC04.898
        ENDDO                                                                    23DEC04.899
      ENDIF                                                                      23DEC04.900
#endif                                                                           23DEC04.901
#ifdef ICUPA6                                                                    23DEC04.902
C                                                                                23DEC04.903
C --- ICUPA=6: KAIN-FRITSCH                                                      23DEC04.904
C                                                                                23DEC04.905
      IF(ICUPA(INEST).EQ.6)THEN                                                  23DEC04.906
cmic$ do all autoscope                                                           23DEC04.907
cmic$1 shared(ilx,ibnes,ienes,jbnes,jenes,inest,rho0,w3d,                        23DEC04.908
cmic$1        t3dten,qv3dten,u3d,v3d,t3d,qv3d,pp3d,psb,rainc)                    23DEC04.909
cmic$2 private(j,scr1)                                                           23DEC04.910
c$doacross                                                                       23DEC04.911
c$& local(j,scr1)                                                                23DEC04.912
c$omp parallel do default(shared)                                                23DEC04.913
c$omp&private(j,scr1)                                                            23DEC04.914
        DO J=JBNES,JENES-1                                                       23DEC04.915
          DO K=1,KL                                                              23DEC04.916
            DO I=1,ILX                                                           23DEC04.917
              SCR1(I,K)=-5.0E-4*G*RHO0(I,J,K)*(W3D(I,J,K)+W3D(I,J,K+1))          23DEC04.918
            ENDDO                                                                23DEC04.919
          ENDDO                                                                  23DEC04.920
C                                                                                23DEC04.921
          IF(IMPHYS(INEST).GE.6)THEN                                             23DEC04.922
            CALL CUPARA6(PSB,T3DTEN,QV3DTEN,QR3DTEN,QC3DTEN,QG3DTEN,             23DEC04.923
     +           QI3DTEN,RAINC,SCR1,U3D,V3D,T3D,QV3D,PP3D,J,INEST,               23DEC04.924
     +           IMPHYS(INEST),IBNES,IENES-1)                                    23DEC04.925
          ELSE                                                                   23DEC04.926
            CALL CUPARA6(PSB,T3DTEN,QV3DTEN,QR3DTEN,QC3DTEN,QNI3DTEN,            23DEC04.927
     +           QI3DTEN,RAINC,SCR1,U3D,V3D,T3D,QV3D,PP3D,J,INEST,               23DEC04.928
     +           IMPHYS(INEST),IBNES,IENES-1)                                    23DEC04.929
          ENDIF                                                                  23DEC04.930
        ENDDO                                                                    23DEC04.931
      ENDIF                                                                      23DEC04.932
#endif                                                                           23DEC04.933
#ifdef ICUPA7                                                                    23DEC04.934
C                                                                                23DEC04.935
C --- ICUPA=7: BETTS-MILLER                                                      23DEC04.936
C                                                                                23DEC04.937
      IF(ICUPA(INEST).EQ.7)THEN                                                  23DEC04.938
cmic$ do all autoscope                                                           23DEC04.939
cmic$1 shared(ibnes,ienes,jbnes,jenes,inest,                                     23DEC04.940
cmic$&       t3dten,qv3dten,t3d,qv3d,pp3d,psb,rainc,xland,shc,t0)                23DEC04.941
cmic$2 private(j)                                                                23DEC04.942
c$doacross                                                                       23DEC04.943
c$& local(j)                                                                     23DEC04.944
c$omp parallel do default(shared)                                                23DEC04.945
c$omp&private(j)                                                                 23DEC04.946
        DO J=JBNES,JENES-1                                                       23DEC04.947
#ifdef MPP1                                                                      23DEC04.948
          DUMJ(J)=J                                                              23DEC04.949
#endif                                                                           23DEC04.950
          CALL CUPARA7(T3D,QV3D,PSB,PP3D,T3DTEN,QV3DTEN,RAINC,SHC,XLAND,         23DEC04.951
     +         J,INEST,IBNES,IENES-1,T0)                                         23DEC04.952
        ENDDO                                                                    23DEC04.953
      ENDIF                                                                      23DEC04.954
#endif                                                                           23DEC04.955
#ifdef ICUPA8                                                                    23DEC04.956
C                                                                                23DEC04.957
C --- ICUPA=8: KAIN-FRITSCH 2 WITH SHALLOW CONVECTION                            23DEC04.958
C                                                                                23DEC04.959
      IF(ICUPA(INEST).EQ.8)THEN                                                  23DEC04.960
cmic$ do all autoscope                                                           23DEC04.961
cmic$1 shared(ilx,ibnes,ienes,jbnes,jenes,inest,rho0,w3d,                        23DEC04.962
cmic$1        t3dten,qv3dten,u3d,v3d,t3d,qv3d,pp3d,psb,rainc)                    23DEC04.963
cmic$2 private(j,scr1)                                                           23DEC04.964
c$doacross                                                                       23DEC04.965
c$& local(j,scr1)                                                                23DEC04.966
c$omp parallel do default(shared)                                                23DEC04.967
c$omp&private(j,scr1)                                                            23DEC04.968
Ccsd$ parallel do private(j,i,k,scr1) schedule(static,1)                         23DEC04.969
        DO J=JBNES,JENES-1                                                       23DEC04.970
          DO K=1,KL                                                              23DEC04.971
            DO I=1,ILX                                                           23DEC04.972
              SCR1(I,K)=-5.0E-4*G*RHO0(I,J,K)*(W3D(I,J,K)+W3D(I,J,K+1))          23DEC04.973
            ENDDO                                                                23DEC04.974
          ENDDO                                                                  23DEC04.975
C                                                                                23DEC04.976
          IF(IMPHYS(INEST).GE.6)THEN                                             23DEC04.977
            CALL CUPARA8(PSB,T3DTEN,QV3DTEN,QR3DTEN,QC3DTEN,QG3DTEN,             23DEC04.978
     +           QI3DTEN,RAINC,SCR1,U3D,V3D,T3D,QV3D,PP3D,J,INEST,               23DEC04.979
     +           IMPHYS(INEST),IBNES,IENES-1)                                    23DEC04.980
          ELSE                                                                   23DEC04.981
            CALL CUPARA8(PSB,T3DTEN,QV3DTEN,QR3DTEN,QC3DTEN,QNI3DTEN,            23DEC04.982
     +           QI3DTEN,RAINC,SCR1,U3D,V3D,T3D,QV3D,PP3D,J,INEST,               23DEC04.983
     +           IMPHYS(INEST),IBNES,IENES-1)                                    23DEC04.984
          ENDIF                                                                  23DEC04.985
        ENDDO                                                                    23DEC04.986
Ccsd$ end parallel do                                                            23DEC04.987
      ENDIF                                                                      23DEC04.988
#endif                                                                           23DEC04.989
C=======================================================================         23DEC04.990
C                                                                                SOLVE.1178
#ifdef ISHALLO1                                                                  SOLVE.1179
C                                                                                SOLVE.1180
C *** COMPUTE TENDENCIES DUE TO SHALLOW CONVECTION                               SOLVE.1181
C                                                                                SOLVE.1182
      IF(ISHALLO(INEST).EQ.1)THEN                                                SOLVE.1183
cmic$ do all autoscope                                                           SOLVE.1184
c$doacross                                                                       SOLVE.1185
c$& local(i,j,k)                                                                 SOLVE.1186
c$omp parallel do default(shared)                                                SOLVE.1187
c$omp&private(i,j,k)                                                             SOLVE.1188
        DO J=JBNES,JENES-1                                                       SOLVE.1189
          DO K=1,KL                                                              SOLVE.1190
            DO I=IBNES,IENES-1                                                   SOLVE.1191
              UCC(I,J,K)=T3DTEN(I,J,K)-UCC(I,J,K)                                SOLVE.1192
              VCC(I,J,K)=QV3DTEN(I,J,K)-VCC(I,J,K)                               SOLVE.1193
            ENDDO                                                                SOLVE.1194
          ENDDO                                                                  SOLVE.1195
        ENDDO                                                                    SOLVE.1196
cmic$ do all autoscope                                                           SOLVE.1197
cmic$1 shared (jbnes,jenes,ibnes,ienes,                                          SOLVE.1198
cmic$1         t3d,qv3d,t3dten,qv3dten,pp3d,psb,ht,ucc,vcc)                      SOLVE.1199
cmic$2 private (j)                                                               SOLVE.1200
c$doacross                                                                       SOLVE.1201
c$& local(j)                                                                     SOLVE.1202
c$omp parallel do default(shared)                                                SOLVE.1203
c$omp&private(j)                                                                 SOLVE.1204
        DO J=JBNES,JENES-1                                                       SOLVE.1205
#ifdef MPP1                                                                      SOLVE.1206
          DUMJ(J)=J                                                              SOLVE.1207
#endif                                                                           SOLVE.1208
          CALL SHALLCU(T3D,QV3D,T3DTEN,QV3DTEN,PP3D,PSB,HT,UCC,VCC,J,            SOLVE.1209
     +         IBNES,IENES-1)                                                    SOLVE.1210
        ENDDO                                                                    SOLVE.1211
      ENDIF                                                                      SOLVE.1212
#endif                                                                           SOLVE.1213
C                                                                                SOLVE.1214
C *** COMPUTE DIFFUSION TERMS:                                                   SOLVE.1215
C                                                                                SOLVE.1216
      CALL DIFFU(KZZ,U3DTEN,U3D,PDOTB,DOTXK,C203,IBNES,IENES,JBNES,JENES         SOLVE.1229
     +     ,KL,INEST)                                                            SOLVE.1230
      CALL DIFFU(KZZ,V3DTEN,V3D,PDOTB,DOTXK,C203,IBNES,IENES,JBNES,JENES         SOLVE.1231
     +     ,KL,INEST)                                                            SOLVE.1232
C                                                                                SOLVE.1233
      if (ITPDIF.le.1) then                                                      23DEC04.991
C --- DIFFUSION FOR TEMPERATUTRE                                                 SOLVE.1234
C --- INCLUDE PRESSURE TERM IN ADIABATIC DIFFUSION                               SOLVE.1235
c  this term is not needed for z-diffusion because                               23DEC04.992
c  z-diffusion is computed with theta                                            23DEC04.993
C                                                                                SOLVE.1236
cmic$ do all autoscope                                                           SOLVE.1237
c$doacross                                                                       SOLVE.1238
c$& local(i,j,k)                                                                 SOLVE.1239
c$omp parallel do default(shared)                                                SOLVE.1240
c$omp&private(i,j,k)                                                             SOLVE.1241
      DO J=1,JLX                                                                 SOLVE.1242
        DO K=1,KL                                                                SOLVE.1243
          DO I=1,ILX                                                             SOLVE.1244
            UCD(I,J,K)=0.0                                                       SOLVE.1245
          ENDDO                                                                  SOLVE.1246
        ENDDO                                                                    SOLVE.1247
      ENDDO                                                                      SOLVE.1248
C                                                                                SOLVE.1249
      CALL DIFFU(KZZ,UCD,PP3D,PSB,CRSXK,C203,IBNES,IENES-1,JBNES,JENES-1         SOLVE.1250
     +     ,KL,INEST)                                                            SOLVE.1251
C                                                                                SOLVE.1252
cmic$ do all autoscope                                                           SOLVE.1253
c$doacross                                                                       SOLVE.1254
c$& local(i,j,k)                                                                 SOLVE.1255
c$omp parallel do default(shared)                                                SOLVE.1256
c$omp&private(i,j,k)                                                             SOLVE.1257
      DO J=JBNES,JENES-1                                                         SOLVE.1258
        DO K=1,KL                                                                SOLVE.1259
          DO I=IBNES,IENES-1                                                     SOLVE.1260
            T3DTEN(I,J,K)=T3DTEN(I,J,K)-UCD(I,J,K)/(CP*(1.+0.8*                  SOLVE.1261
     +                    QV3D(I,J,K))*RHO1(I,J,K))                              SOLVE.1262
          ENDDO                                                                  SOLVE.1263
        ENDDO                                                                    SOLVE.1264
      ENDDO                                                                      SOLVE.1265
      endif                                                                      23DEC04.994
C                                                                                SOLVE.1266
      IF(ITPDIF.EQ.1)THEN                                                        SOLVE.1267
        CALL DIFFU(KZZ,T3DTEN,TBP,PSB,CRSXK,C203,IBNES,IENES-1,JBNES,            SOLVE.1268
     +       JENES-1,KL,INEST)                                                   SOLVE.1269
      ELSE IF (ITPDIF.EQ.0) THEN                                                 23DEC04.995
        CALL DIFFU(KZZ,T3DTEN,T3D,PSB,CRSXK,C203,IBNES,IENES-1,JBNES,            SOLVE.1271
     +       JENES-1,KL,INEST)                                                   SOLVE.1272
      ELSE IF (ITPDIF.GE.2) THEN                                                 23DEC04.996
      if (IMPHYS(INEST).GE.3)THEN                                                23DEC04.997
      CALL DIFFTH(KZZ,T3DTEN,TBP,T3D,T0,PSB,CRSXK,C203,IBNES,IENES-1,            23DEC04.998
     + JBNES,JENES-1,KL,INEST,QC3D,THGB,fin,fjn,deltaz,lam,kmvl,rmn,             23DEC04.999
     & kmi,kmj,kmip,kmim,kmjp,kmjm,                                              23DEC04.1000
     & mrkip1,mrkip2,mrkim1,mrkim2,mrkjp1,mrkjp2,mrkjm1,mrkjm2,                  23DEC04.1001
     & irkip1,irkip2,irkim1,irkim2,irkjp1,irkjp2,irkjm1,irkjm2,                  23DEC04.1002
     & fjt04,fit04,fit0o,fit0u,fjt0o,fjt0u,fion,fiun,fjon,fjun,dz3d,             23DEC04.1003
     + zdiffst)                                                                  23DEC04.1004
        else                                                                     23DEC04.1005
       CALL DIFFTHD(KZZ,T3DTEN,TBP,T3D,T0,PSB,CRSXK,C203,IBNES,IENES-1,          23DEC04.1006
     + JBNES,JENES-1,KL,INEST,THGB,fin,fjn,deltaz,lam,kmvl,rmn,                  23DEC04.1007
     & kmi,kmj,kmip,kmim,kmjp,kmjm,                                              23DEC04.1008
     & mrkip1,mrkip2,mrkim1,mrkim2,mrkjp1,mrkjp2,mrkjm1,mrkjm2,                  23DEC04.1009
     & irkip1,irkip2,irkim1,irkim2,irkjp1,irkjp2,irkjm1,irkjm2,                  23DEC04.1010
     & fjt04,fit04,fit0o,fit0u,fjt0o,fjt0u,fion,fiun,fjon,fjun,dz3d,             23DEC04.1011
     + zdiffst)                                                                  23DEC04.1012
      endif                                                                      23DEC04.1013
      ENDIF                                                                      SOLVE.1273
C                                                                                SOLVE.1274
      CALL DIFFU(KZZ+1,W3DTEN,W3D,PSB,CRSXK,C203,IBNES,IENES-1,JBNES,            SOLVE.1275
     +     JENES-1,KL,INEST)                                                     SOLVE.1276
C                                                                                SOLVE.1277
      IF(IMPHYS(INEST).GE.2) THEN                                                SOLVE.1278
      if (ITPDIF.ge.2) then                                                      23DEC04.1014
        CALL DIFFMOI(KZZ,QV3DTEN,QV3D,PSB,CRSXK,C203,                            23DEC04.1015
     &           IBNES,IENES-1,JBNES,JENES-1,KL,INEST,                           23DEC04.1016
     &  fin,fjn,2,kmvl,kmi,kmj,gezipwh,gezimwh,gezjpwh,gezjmwh,                  23DEC04.1017
     & mrkip1,mrkip2,mrkim1,mrkim2,mrkjp1,mrkjp2,mrkjm1,mrkjm2,                  23DEC04.1018
     & irkip1,irkip2,irkim1,irkim2,irkjp1,irkjp2,irkjm1,irkjm2,                  23DEC04.1019
     & fionf,fiunf,fjonf,fjunf,dz3d,dx,zdiffst)                                  23DEC04.1020
      else                                                                       23DEC04.1021
        CALL DIFFU(KZZ,QV3DTEN,QV3D,PSB,CRSXK,C203,                              SOLVE.1279
     &           IBNES,IENES-1,JBNES,JENES-1,KL,INEST)                           SOLVE.1280
      endif                                                                      23DEC04.1022
        IF(IMPHYS(INEST).GE.3)THEN                                               SOLVE.1281
      if (ITPDIF.ge.2) then                                                      23DEC04.1023
          CALL DIFFMOI(KZZ,QC3DTEN,QC3D,PSB,CRSXK,C203,                          23DEC04.1024
     &               IBNES,IENES-1,JBNES,JENES-1,KL,INEST,                       23DEC04.1025
     &  fin,fjn,1,kmvl,kmi,kmj,gezipwh,gezimwh,gezjpwh,gezjmwh,                  23DEC04.1026
     & mrkip1,mrkip2,mrkim1,mrkim2,mrkjp1,mrkjp2,mrkjm1,mrkjm2,                  23DEC04.1027
     & irkip1,irkip2,irkim1,irkim2,irkjp1,irkjp2,irkjm1,irkjm2,                  23DEC04.1028
     & fionf,fiunf,fjonf,fjunf,dz3d,dx,zdiffst)                                  23DEC04.1029
      else                                                                       23DEC04.1030
          CALL DIFFU(KZZ,QC3DTEN,QC3D,PSB,CRSXK,C203,                            SOLVE.1282
     &               IBNES,IENES-1,JBNES,JENES-1,KL,INEST)                       SOLVE.1283
      endif                                                                      23DEC04.1031
          CALL DIFFU(KZZ,QR3DTEN,QR3D,PSB,CRSXK,C203,                            SOLVE.1284
     &               IBNES,IENES-1,JBNES,JENES-1,KL,INEST)                       SOLVE.1285
          IF(IMPHYS(INEST).GE.5)THEN                                             SOLVE.1286
            CALL DIFFU(KZZ, QI3DTEN, QI3D,PSB,CRSXK,C203,                        SOLVE.1287
     &                 IBNES,IENES-1,JBNES,JENES-1,KL,INEST)                     SOLVE.1288
            CALL DIFFU(KZZ,QNI3DTEN,QNI3D,PSB,CRSXK,C203,                        SOLVE.1289
     &                 IBNES,IENES-1,JBNES,JENES-1,KL,INEST)                     SOLVE.1290
            IF(IMPHYS(INEST).GE.6)THEN                                           SOLVE.1291
              CALL DIFFU(KZZ, QG3DTEN, QG3D,PSB,CRSXK,C203,                      SOLVE.1292
     &                   IBNES,IENES-1,JBNES,JENES-1,KL,INEST)                   SOLVE.1293
              CALL DIFFU(KZZ,QNC3DTEN,QNC3D,PSB,CRSXK,C203,                      SOLVE.1294
     &                   IBNES,IENES-1,JBNES,JENES-1,KL,INEST)                   SOLVE.1295
            ENDIF                                                                SOLVE.1296
          ENDIF                                                                  SOLVE.1297
        ENDIF                                                                    SOLVE.1298
      ELSE                                                                       SOLVE.1299
cmic$ do all autoscope                                                           SOLVE.1300
c$doacross                                                                       SOLVE.1301
c$& local(i,j,k)                                                                 SOLVE.1302
c$omp parallel do default(shared)                                                SOLVE.1303
c$omp&private(i,j,k)                                                             SOLVE.1304
        DO J=1,JLX                                                               SOLVE.1305
          DO K=1,KL                                                              SOLVE.1306
            DO I=1,ILX                                                           SOLVE.1307
              QV3D(I,J,K)=0.0                                                    SOLVE.1308
            ENDDO                                                                SOLVE.1309
          ENDDO                                                                  SOLVE.1310
        ENDDO                                                                    SOLVE.1311
      ENDIF                                                                      SOLVE.1312
C                                                                                SOLVE.1313
C *** APPLY BOUNDARY CONDITIONS FOR COARSEST DOMAIN:                             SOLVE.1314
C                                                                                SOLVE.1315
      IF(IBOUDY(INEST).EQ.3)THEN                                                 SOLVE.1316
        DTB=(XTIME-DTMIN-TBDYBE)*60.                                             SOLVE.1317
C                                                                                SOLVE.1318
C..UV.APPLY THE NUDGING BOUNDARY CONDITIONS:                                     SOLVE.1319
C                                                                                SOLVE.1320
        CALL NUDGE(ISPGD,FNUDGE,GNUDGE,DTB,U3DTEN,UEB,UWB,USB,UNB,UEBT,          SOLVE.1321
     +       UWBT,USBT,UNBT,UB,C203,KZZ,IL,JL)                                   SOLVE.1322
        CALL NUDGE(ISPGD,FNUDGE,GNUDGE,DTB,V3DTEN,VEB,VWB,VSB,VNB,VEBT,          SOLVE.1323
     +       VWBT,VSBT,VNBT,VB,C203,KZZ,IL,JL)                                   SOLVE.1324
C                                                                                SOLVE.1325
C..TQ.APPLY THE NUDGING BOUNDARY CONDITIONS:                                     SOLVE.1326
C                                                                                SOLVE.1327
        CALL NUDGE(ISPGX,FNUDGE,GNUDGE,DTB,T3DTEN,TEB,TWB,TSB,TNB,TEBT,          SOLVE.1328
     +       TWBT,TSBT,TNBT,TB,C203,KZZ,ILX,JLX)                                 SOLVE.1329
C                                                                                SOLVE.1330
        CALL NUDGE(ISPGX,FNUDGE,GNUDGE,DTB,PP3DTEN,PPEB,PPWB,PPSB,PPNB,          SOLVE.1331
     +       PPEBT,PPWBT,PPSBT,PPNBT,PPB,C203,KZZ,ILX,JLX)                       SOLVE.1332
        IF(IMPHYS(INEST).GE.2)THEN                                               SOLVE.1333
          CALL NUDGE(ISPGX,FNUDGE,GNUDGE,DTB,QV3DTEN,QEB,QWB,QSB,QNB,            SOLVE.1334
     +         QEBT,QWBT,QSBT,QNBT,QVB,C203,KZZ,ILX,JLX)                         SOLVE.1335
          IF(IMPHYS(INEST).GE.3)THEN                                             SOLVE.1336
            IF(IBMOIST.EQ.1)THEN                                                 SOLVE.1337
              CALL NUDGE(ISPGX,FNUDGE,GNUDGE,DTB,QC3DTEN,QCEB,QCWB,QCSB,         SOLVE.1338
     +             QCNB,QCEBT,QCWBT,QCSBT,QCNBT,QCB,C203,KZZ,ILX,JLX)            SOLVE.1339
              CALL NUDGE(ISPGX,FNUDGE,GNUDGE,DTB,QR3DTEN,QREB,QRWB,QRSB,         SOLVE.1340
     +             QRNB,QREBT,QRWBT,QRSBT,QRNBT,QRB,C203,KZZ,ILX,JLX)            SOLVE.1341
              IF(IMPHYS(INEST).GE.5)THEN                                         SOLVE.1342
                CALL NUDGE(ISPGX,FNUDGE,GNUDGE,DTB,QI3DTEN,QIEB,QIWB,            SOLVE.1343
     +               QISB,QINB,QIEBT,QIWBT,QISBT,QINBT,QIB,C203,KZZ,ILX,         SOLVE.1344
     +               JLX)                                                        SOLVE.1345
                CALL NUDGE(ISPGX,FNUDGE,GNUDGE,DTB,QNI3DTEN,QNIEB,QNIWB,         SOLVE.1346
     +               QNISB,QNINB,QNIEBT,QNIWBT,QNISBT,QNINBT,QNIB,C203,          SOLVE.1347
     +               KZZ,ILX,JLX)                                                SOLVE.1348
                IF(IMPHYS(INEST).GE.6)THEN                                       SOLVE.1349
                  CALL NUDGE(ISPGX,FNUDGE,GNUDGE,DTB,QG3DTEN,QGEB,QGWB,          SOLVE.1350
     +                 QGSB,QGNB,QGEBT,QGWBT,QGSBT,QGNBT,QGB,C203,KZZ,           SOLVE.1351
     +                 ILX,JLX)                                                  SOLVE.1352
                  CALL NUDGE(ISPGX,FNUDGE,GNUDGE,DTB,QNC3DTEN,QNCEB,             SOLVE.1353
     +                 QNCWB,QNCSB,QNCNB,QNCEBT,QNCWBT,QNCSBT,QNCNBT,            SOLVE.1354
     +                 QNCB,C203,KZZ,ILX,JLX)                                    SOLVE.1355
                ENDIF                                                            SOLVE.1356
              ENDIF                                                              SOLVE.1357
            ENDIF                                                                SOLVE.1358
          ENDIF                                                                  SOLVE.1359
        ENDIF                                                                    SOLVE.1360
      ENDIF                                                                      SOLVE.1361
C                                                                                SOLVE.1362
#if defined(FDDAGD1) || defined(FDDAOBS1)
C                                                                                SOLVE.1364
C *** COMPUTE TENDENCIES DUE TO 4DDA                                             SOLVE.1365
C                                                                                SOLVE.1366
      IF(IFDDA.EQ.1)THEN                                                         SOLVE.1367
C                                                                                SOLVE.1368
C--- FDDA INITIAL SETUP CALLS                                                    SOLVE.1369
C                                                                                SOLVE.1370
        CALL SETFD(INEST,FDTIM)                                                  SOLVE.1371
cmic$ do all autoscope                                                           SOLVE.1372
cmic$1 shared(ilx,jbnes,jenes,inest,ptop,idq,fdtim,                              SOLVE.1373
cmic$1        znt,regime,kpbl2d,                                                 SOLVE.1374
cmic$1        u3d,v3d,t3d,qv3d,pp3d)                                             SOLVE.1375
cmic$2 private(i,j,zntj,regj,kpblj,qsatf,tboc,qboc,ubod,vbod)                    SOLVE.1376
c$doacross                                                                       SOLVE.1377
c$& local(i,j,k,id,zntj,regj,kpblj,qsatf,tboc,qboc,ubod,vbod)                    SOLVE.1378
c$omp parallel do default(shared)                                                SOLVE.1379
c$omp&private(i,j,k,id,zntj,regj,kpblj,qsatf,tboc,qboc,ubod,vbod)                SOLVE.1380
        DO J=JBNES,JENES                                                         SOLVE.1381
C                                                                                SOLVE.1382
C IF NUDGING SURFACE WINDS IN THE BOUNDARY LAYER, IF IWINDS(INEST+2)=1           SOLVE.1383
C USE A SIMILARITY CORRECTION BASED ON ROUGHNESS TO APPLY 10M                    SOLVE.1384
C WIND TO THE SURFACE LAYER (K=KL) AT 40M.  TO DO THIS WE MUST                   SOLVE.1385
C STORE ROUGHNESS AND REGIME FOR EACH J SLICE AFTER THE CALL TO                  SOLVE.1386
C HIRPBL FOR LATER USE IN BLNUDGD.                                               SOLVE.1387
C                                                                                SOLVE.1388
          DO I=2,ILX                                                             SOLVE.1389
            ZNTJ(I)=ZNT(I,J)                                                     SOLVE.1390
            REGJ(I)=REGIME(I,J)                                                  SOLVE.1391
            KPBLJ(I)=KPBL2D(I,J)                                                 SOLVE.1392
C                                                                                SOLVE.1393
C MAKE SURE ZNTJ (FOR SIMILARITY ADJUSTMENT PURPOSES IN BLNUDGD)                 SOLVE.1394
C IS POSITIVE AND NO GREATER THAN 1.0 M.                                         SOLVE.1395
C                                                                                SOLVE.1396
            ZNTJ(I)=AMAX1(0.0001,ZNTJ(I))                                        SOLVE.1397
            ZNTJ(I)=AMIN1(1.0000,ZNTJ(I))                                        SOLVE.1398
          ENDDO                                                                  SOLVE.1399
C                                                                                SOLVE.1400
#ifdef FDDAOBS1                                                                  SOLVE.1401
C                                                                                SOLVE.1402
C--- OBS NUDGING FOR TEMP AND MOISTURE                                           SOLVE.1403
C                                                                                SOLVE.1404
          NSTA=NSTAT(INEST)                                                      SOLVE.1405
          IF(J.LT.JENES)THEN                                                     SOLVE.1406
            IF(I4DI(INEST).EQ.1.AND.ISTEMP(INEST).EQ.1.AND.NSTA.GT.0)            SOLVE.1407
     +        CALL NUDOB(J,3,T3DTEN,INEST,A,NESTI(INEST),NESTJ(INEST),UB         SOLVE.1408
     +        ,VB,TB,QVB,PSB,PDOTB,KPBLJ,PTOP)                                   SOLVE.1409
C                                                                                SOLVE.1410
            IF(I4DI(INEST).EQ.1.AND.ISMOIS(INEST).EQ.1.AND.NSTA.GT.0)            SOLVE.1411
     +        CALL NUDOB(J,4,QV3DTEN,INEST,A,NESTI(INEST),NESTJ(INEST),          SOLVE.1412
     +        UB,VB,TB,QVB,PSB,PDOTB,KPBLJ,PTOP)                                 SOLVE.1413
          ENDIF                                                                  SOLVE.1414
          IF(I4DI(INEST).EQ.1.AND.ISWIND(INEST).EQ.1.AND.NSTA.GT.0)THEN          SOLVE.1415
            CALL NUDOB(J,1,U3DTEN,INEST,A,NESTI(INEST),NESTJ(INEST),UB,          SOLVE.1416
     +           VB,TB,QVB,PSB,PDOTB,KPBLJ,PTOP)                                 SOLVE.1417
C                                                                                SOLVE.1418
            CALL NUDOB(J,2,V3DTEN,INEST,A,NESTI(INEST),NESTJ(INEST),UB,          SOLVE.1419
     +           VB,TB,QVB,PSB,PDOTB,KPBLJ,PTOP)                                 SOLVE.1420
          ENDIF                                                                  SOLVE.1421
#endif                                                                           SOLVE.1422
#ifdef FDDAGD1                                                                   SOLVE.1423
C                                                                                SOLVE.1424
C--- ANALYSIS NUDGING                                                            SOLVE.1425
C                                                                                SOLVE.1426
          IF(I4D(INEST,1).EQ.1)THEN                                              SOLVE.1427
            IF(J.LT.JENES)THEN                                                   SOLVE.1428
C COMPUTE QSAT FROM THE MODEL TEMP AND MOISTURE AT TAU-1 FOR THIS N-S SL         SOLVE.1429
              CALL QSATGD(J,QSATF,T3D,PSB,PP3D,A,PTOP)                           SOLVE.1430
C OBS P*T                                                                        SOLVE.1431
              IF(ITEMP(INEST,1).EQ.1)CALL BUFSLGD(J,TBO,TBOTEN,TBOC,PSOC         SOLVE.1432
     +          ,FDTIM,3,INEST)                                                  SOLVE.1433
C OBS P*Q                                                                        SOLVE.1434
              IF(IMOIS(INEST,1).EQ.1)CALL BUFSLGD(J,QBO,QBOTEN,QBOC,PSOC         SOLVE.1435
     +          ,FDTIM,4,INEST)                                                  SOLVE.1436
C                                                                                SOLVE.1437
C COMPUTE THE NUDGING TERM FOR TEMPERATURE AND MOISTURE (VAPOR)                  SOLVE.1438
C AND ADD TO THE TENDENCY BEFORE THE SPONGE IS CALLED...                         SOLVE.1439
C COMPUTE THE IND. OBS. NUDGING TERMS FOR TEMP AND MOISTURE                      SOLVE.1440
C FOR OBJECTIVE ANALYSIS NUDGING                                                 SOLVE.1441
C                                                                                SOLVE.1442
              IF(ITEMP(INEST,1).EQ.1)CALL NUDGD(NTIM,NVAR,ITEMP,J,TB,            SOLVE.1443
     +          TBOC,T3DTEN,PSB,PSOC,GT,0,WCS,DUM2D,TFAC,ZFAC,GP,GR,MPSD         SOLVE.1444
     +          ,3,VORDIF,INEST,KPBLJ,BLWCS,BLWNV,SFCOBS,SFCTIM,QSATF,           SOLVE.1445
     +          IQCHK,TIMB,TIME,NTB,NTE,SCR2D,BLDUM2D,BLPOC,BLPOD,REGJ,          SOLVE.1446
     +          ZNTJ,.FALSE.)                                                    SOLVE.1447
C                                                                                SOLVE.1448
              IF(IMOIS(INEST,1).EQ.1)CALL NUDGD(NTIM,NVAR,IMOIS,J,QVB,           SOLVE.1449
     +          QBOC,QV3DTEN,PSB,PSOC,GQ,IDQ,WCS,WQ,TFAC,ZFAC,GP,GR,MPSD         SOLVE.1450
     +          ,4,VORDIF,INEST,KPBLJ,BLWCS,BLWNV,SFCOBS,SFCTIM,QSATF,           SOLVE.1451
     +          IQCHK,TIMB,TIME,NTB,NTE,SCR2D,BLDUM2D,BLPOC,BLPOD,REGJ,          SOLVE.1452
     +          ZNTJ,.FALSE.)                                                    SOLVE.1453
            ENDIF                                                                SOLVE.1454
C                                                                                SOLVE.1455
C                                                                                SOLVE.1456
C FOR OBJECTIVE ANALYSIS NUDGING                                                 SOLVE.1457
C CALCULATE OBS P*U AND P*V AT TAU-1 IF INT4=1, ELSE BUFFER 3-D                  SOLVE.1458
C OBS FIELD INTO VERTICAL SLICE ARRAY...                                         SOLVE.1459
C IDC=0 FOR DOT POINT FIELD                                                      SOLVE.1460
C OBS P*U                                                                        SOLVE.1461
            IF(IWIND(INEST,1).EQ.1)THEN                                          SOLVE.1462
              IF(IROT(INEST).NE.1)THEN                                           SOLVE.1463
                CALL BUFSLGD(J,UBO,UBOTEN,UBOD,PSOD,FDTIM,1,INEST)               SOLVE.1464
                CALL BUFSLGD(J,VBO,VBOTEN,VBOD,PSOD,FDTIM,2,INEST)               SOLVE.1465
                ID=0                                                             SOLVE.1466
              ELSEIF(IROT(INEST).EQ.1)THEN                                       SOLVE.1467
                ID=2                                                             SOLVE.1468
              ENDIF                                                              SOLVE.1469
C                                                                                SOLVE.1470
              CALL NUDGD(NTIM,NVAR,IWIND,J,UB,UBOD,U3DTEN,PDOTB,PSOD,GV,         SOLVE.1471
     +             ID,WDT,WCS,TFAC,ZFAC,GP,GR,MPSD,1,VORDIF,INEST,KPBLJ,         SOLVE.1472
     +             BLWDT,BLWNV,SFCOBS,SFCTIM,QSATF,IQCHK,TIMB,TIME,NTB,          SOLVE.1473
     +             NTE,SCR2D,BLDUM2D,BLPOC,BLPOD,REGJ,ZNTJ,.FALSE.)              SOLVE.1474
C                                                                                SOLVE.1475
              CALL NUDGD(NTIM,NVAR,IWIND,J,VB,VBOD,V3DTEN,PDOTB,PSOD,GV,         SOLVE.1476
     +             ID,WDT,WCS,TFAC,ZFAC,GP,GR,MPSD,2,VORDIF,INEST,KPBLJ,         SOLVE.1477
     +             BLWDT,BLWNV,SFCOBS,SFCTIM,QSATF,IQCHK,TIMB,TIME,NTB,          SOLVE.1478
     +             NTE,SCR2D,BLDUM2D,BLPOC,BLPOD,REGJ,ZNTJ,.FALSE.)              SOLVE.1479
            ENDIF                                                                SOLVE.1480
          ENDIF                                                                  SOLVE.1481
C                                                                                SOLVE.1482
#endif                                                                           SOLVE.1483
        ENDDO                                                                    SOLVE.1484
#ifdef MPP1                                                                      SOLVE.1485
#  include <mpp_solve_40.incl>                                                   SOLVE.1486
#endif                                                                           SOLVE.1487
      ENDIF                                                                      SOLVE.1488
C                                                                                SOLVE.1489
C --- END OF 4DDA                                                                SOLVE.1490
C                                                                                SOLVE.1491
#endif                                                                           SOLVE.1492
C                                                                                SOLVE.1493
C *** COMPUTE EXPLICIT MOISTURE TENDENCIES:                                      SOLVE.1494
C                                                                                SOLVE.1495
#ifdef IMPHYS2                                                                   SOLVE.1496
C                                                                                SOLVE.1497
C --- IMPHYS=2: STABLE PRECIPITATION                                             SOLVE.1498
C                                                                                SOLVE.1499
      IF(IMPHYS(INEST).EQ.2)THEN                                                 SOLVE.1500
        CALL NCONVP(T3D,QV3D,PP3D,QV3DTEN,T3DTEN,RAINNC,PSA,IBNES,IENES-         SOLVE.1501
     +       1,JBNES,JENES-1,DT)                                                 SOLVE.1502
      ENDIF                                                                      SOLVE.1503
#endif                                                                           SOLVE.1504
      IF(IMOIST(INEST).EQ.2)THEN                                                 SOLVE.1505
#if defined(IMPHYS3) || defined(IMPHYS4)
C                                                                                SOLVE.1507
C --- IMPHYS=3 or 4: SIMPLE EXPLICIT SCHEME                                      SOLVE.1508
C                                                                                SOLVE.1509
        IF(IMPHYS(INEST).EQ.3.OR.IMPHYS(INEST).EQ.4)THEN                         SOLVE.1510
cmic$ do all autoscope                                                           SOLVE.1511
cmic$1 shared(ibnes,ienes,jbnes,jenes,inest,kl,                                  SOLVE.1512
cmic$1        t3d,qv3d,qc3d,qr3d,pp3d,rho0,w3d,                                  SOLVE.1513
cmic$1        tb,qcb,qrb,psa,rainnc,                                             07NOV00.1075
cmic$1        t3dten,qv3dten,qc3dten,qr3dten)                                    SOLVE.1515
cmic$2 private(i,j,k,scr1)                                                       SOLVE.1516
c$doacross                                                                       SOLVE.1517
c$& local(i,j,k,scr1)                                                            SOLVE.1518
c$omp parallel do default(shared)                                                SOLVE.1519
c$omp&private(i,j,k,scr1)                                                        SOLVE.1520
Ccsd$ parallel do private(i,j,k,scr1) schedule(static,1)                         23DEC04.1032
          DO J=JBNES,JENES-1                                                     SOLVE.1521
            DO K=1,KL                                                            SOLVE.1522
              DO I=1,ILX                                                         SOLVE.1523
                SCR1(I,K)=-5.0E-4*G*RHO0(I,J,K)*(W3D(I,J,K)+W3D(I,J,K+1)         SOLVE.1524
     +                    )                                                      SOLVE.1525
              ENDDO                                                              SOLVE.1526
            ENDDO                                                                SOLVE.1527
#ifdef MPHYSTBL1                                                                 SOLVE.1528
            CALL LEXMOISS(J,INEST,T3D,QV3D,QC3D,QR3D,PP3D,TB,QCB,QRB,PSA         SOLVE.1529
     +           ,RAINNC,QC3DTEN,QR3DTEN,QV3DTEN,T3DTEN,SCR1,IBNES,IENES         SOLVE.1530
     +           -1)                                                             SOLVE.1531
#else                                                                            SOLVE.1532
            CALL EXMOISS(J,INEST,T3D,QV3D,QC3D,QR3D,PP3D,TB,QCB,QRB,PSA,         SOLVE.1533
     +           RAINNC,QC3DTEN,QR3DTEN,QV3DTEN,T3DTEN,SCR1,IBNES,IENES-         SOLVE.1534
     +           1)                                                              SOLVE.1535
#endif                                                                           SOLVE.1536
          ENDDO                                                                  SOLVE.1537
Ccsd$ end parallel do                                                            23DEC04.1033
        ENDIF                                                                    SOLVE.1538
#endif                                                                           SOLVE.1539
C                                                                                SOLVE.1540
#ifdef IMPHYS5                                                                   SOLVE.1541
C                                                                                SOLVE.1542
C --- IMPHYS=5: REISNER MIXED PHASE SCHEME                                       SOLVE.1543
C                                                                                SOLVE.1544
        IF(IMPHYS(INEST).EQ.5)THEN                                               SOLVE.1545
# ifndef VECTOR                                                                  07NOV00.1076
cmic$ do all autoscope                                                           SOLVE.1546
cmic$1 shared(ibnes,ienes,jbnes,jenes,inest,                                     SOLVE.1547
cmic$1        t3dten,qv3dten,t3d,qv3d,pp3d,qc3d,qr3d,qi3d,qni3d,                 SOLVE.1548
cmic$1        qc3dten,qr3dten,qi3dten,qni3dten,psb,rainnc)                       SOLVE.1549
cmic$2 private(j)                                                                SOLVE.1550
c$doacross                                                                       SOLVE.1551
c$& local(j)                                                                     SOLVE.1552
c$omp parallel do default(shared)                                                SOLVE.1553
c$omp&private(j)                                                                 SOLVE.1554
Ccsd$ parallel do private(j) schedule(static,1)                                  23DEC04.1034
          DO J=JBNES,JENES-1                                                     SOLVE.1555
#  ifdef MPP1                                                                    07NOV00.1077
            DUMJ(J)=J                                                            SOLVE.1557
#  endif                                                                         07NOV00.1078
#  ifdef MPHYSTBL2                                                               23DEC04.1035
CB New optimized exmoisr routine is called if MPHYSTBL set to 2                  23DEC04.1036
           CALL ZEXMOISR(QC3DTEN,QR3DTEN,QI3DTEN,QNI3DTEN,QV3DTEN,T3DTEN         23DEC04.1037
     +          ,T3D,QV3D,QC3D,QR3D,QI3D,QNI3D,PP3D,PSB,RPSB,RAINNC              23DEC04.1038
     +          ,SNOWNCT,INEST,J                                                 23DEC04.1039
     +          ,IBNES,IENES-1)                                                  23DEC04.1040
#  endif                                                                         23DEC04.1041
#  ifdef MPHYSTBL1                                                               07NOV00.1079
            CALL LEXMOISR(QC3DTEN,QR3DTEN,QI3DTEN,QNI3DTEN,QV3DTEN,              24SEP99.382
     +           T3DTEN,T3D,QV3D,QC3D,QR3D,QI3D,QNI3D,PP3D,PSB,RAINNC,           24SEP99.383
     +           SNOWNCT,INEST,J,IBNES,IENES-1)                                  05DEC01.525
#  endif                                                                         23DEC04.1042
#  ifdef MPHYSTBL0                                                               23DEC04.1043
C NOTE EXTRA RPSB ARGUMENT                                                       07NOV00.1081
            CALL EXMOISR(QC3DTEN,QR3DTEN,QI3DTEN,QNI3DTEN,QV3DTEN,T3DTEN         SOLVE.1564
     +           ,T3D,QV3D,QC3D,QR3D,QI3D,QNI3D,PP3D,PSB,RPSB,RAINNC             07NOV00.1082
     +           ,SNOWNCT,INEST,J                                                05DEC01.526
     +           ,IBNES,IENES-1)                                                 07NOV00.1084
#  endif                                                                         07NOV00.1085
          ENDDO                                                                  SOLVE.1568
Ccsd$ end parallel do                                                            23DEC04.1044
# else                                                                           07NOV00.1086
#    include <call_vector_exmoisr.incl>                                          07NOV00.1087
# endif                                                                          07NOV00.1088
        ENDIF                                                                    SOLVE.1569
#endif                                                                           SOLVE.1570
#ifdef IMPHYS6                                                                   SOLVE.1571
C                                                                                SOLVE.1572
C --- IMPHYS=6: GODDARD GRAUPEL SCHEME                                           SOLVE.1573
C                                                                                SOLVE.1574
        IF(IMPHYS(INEST).EQ.6)THEN                                               SOLVE.1575
cmic$ do all autoscope                                                           SOLVE.1576
cmic$1 shared(ibnes,ienes,jbnes,jenes,inest,                                     SOLVE.1577
cmic$1        t3dten,qv3dten,t3d,qv3d,pp3d,qc3d,qr3d,qi3d,qni3d,                 SOLVE.1578
cmic$1        qg3d,qnc3dten,rho0,w3d,a,dsigma,ptop,                              SOLVE.1579
cmic$1        qc3dten,qr3dten,qi3dten,qni3dten,qg3dten,qnc3dten)                 SOLVE.1580
cmic$2 private(j)                                                                SOLVE.1581
c$doacross                                                                       SOLVE.1582
c$& local(j)                                                                     SOLVE.1583
c$omp parallel do default(shared)                                                SOLVE.1584
c$omp&private(j)                                                                 SOLVE.1585
          DO J=JBNES,JENES-1                                                     SOLVE.1586
#ifdef MPP1                                                                      SOLVE.1587
            DUMJ(J)=J                                                            SOLVE.1588
#endif                                                                           SOLVE.1589
            CALL GODMIC(T3DTEN,QV3DTEN,QC3DTEN,QR3DTEN,QI3DTEN,QNI3DTEN,         SOLVE.1590
     +           QG3DTEN,T3D,QV3D,QC3D,QR3D,QI3D,QNI3D,QG3D,PP3D,PSB,            SOLVE.1591
     +           RAINNC,A,DSIGMA,PTOP,INEST,J,IBNES,IENES-1)                     SOLVE.1592
          ENDDO                                                                  SOLVE.1593
        ENDIF                                                                    SOLVE.1594
#endif                                                                           SOLVE.1595
#ifdef IMPHYS7                                                                   SOLVE.1596
C                                                                                SOLVE.1597
C --- IMPHYS=7: REISNER GRAUPEL SCHEME                                           SOLVE.1598
C                                                                                SOLVE.1599
        IF(IMPHYS(INEST).EQ.7)THEN                                               SOLVE.1600
cmic$ do all autoscope                                                           SOLVE.1601
cmic$1 shared(ibnes,ienes,jbnes,jenes,inest,                                     SOLVE.1602
cmic$1        t3dten,qv3dten,t3d,qv3d,pp3d,qc3d,qr3d,qi3d,qni3d,                 SOLVE.1603
cmic$1        qg3d,qnc3dten,rho0,w3d,psb,rainnc,                                 SOLVE.1604
cmic$1        qc3dten,qr3dten,qi3dten,qni3dten,qg3dten),                         SOLVE.1605
cmic$2 private(j)                                                                SOLVE.1606
c$doacross                                                                       SOLVE.1607
c$& local(j)                                                                     SOLVE.1608
c$omp parallel do default(shared)                                                SOLVE.1609
c$omp&private(j)                                                                 SOLVE.1610
          DO J=JBNES,JENES-1                                                     SOLVE.1611
#ifdef MPP1                                                                      SOLVE.1612
            DUMJ(J)=J                                                            SOLVE.1613
#endif                                                                           SOLVE.1614
            CALL EXMOISG(T3DTEN,QV3DTEN,QC3DTEN,QR3DTEN,QI3DTEN,QNI3DTEN         SOLVE.1615
     +           ,QG3DTEN,QNC3DTEN,T3D,QV3D,QC3D,QR3D,QI3D,QNI3D,QG3D,           SOLVE.1616
     +           QNC3D,PP3D,PSB,RAINNC,SNOWNCT,INEST,J,IBNES,IENES-1             19DEC02.1279
C+---+-----------------------------------------------------------------+         19DEC02.1280
#if defined (CALL_EVERY_OTHER_TSTEP)                               
     +           ,tex_ten(1,1,1,INEST), qvex_ten(1,1,1,INEST)                    19DEC02.1282
     +           ,qcex_ten(1,1,1,INEST), qrex_ten(1,1,1,INEST)                   19DEC02.1283
     +           ,qiex_ten(1,1,1,INEST), qniex_ten(1,1,1,INEST)                  19DEC02.1284
     +           ,qgex_ten(1,1,1,INEST), qncex_ten(1,1,1,INEST)                  19DEC02.1285
#endif                                                                           19DEC02.1286
     +)                                                                          19DEC02.1287
C+---+-----------------------------------------------------------------+         19DEC02.1288
          ENDDO                                                                  SOLVE.1618
        ENDIF                                                                    SOLVE.1619
#endif                                                                           SOLVE.1620
#ifdef IMPHYS8                                                                   SOLVE.1621
C                                                                                SOLVE.1622
C --- IMPHYS=8: SCHULTZ MICROPHYSICAL SCHEME (INC. GRAUPEL)                      SOLVE.1623
C                                                                                SOLVE.1624
        IF(IMPHYS(INEST).EQ.8)THEN                                               SOLVE.1625
cmic$ do all autoscope                                                           SOLVE.1626
cmic$1 shared(ibnes,ienes,jbnes,jenes,inest,                                     SOLVE.1627
cmic$1        t3dten,qv3dten,t3d,qv3d,pp3d,qc3d,qr3d,qi3d,qni3d,                 SOLVE.1628
cmic$1        qg3d,qnc3dten,rho0,w3d,psb,rainnc,                                 SOLVE.1629
cmic$1        qc3dten,qr3dten,qi3dten,qni3dten,qg3dten),                         SOLVE.1630
cmic$2 private(j)                                                                SOLVE.1631
c$doacross                                                                       SOLVE.1632
c$& local(j)                                                                     SOLVE.1633
c$omp parallel do default(shared)                                                SOLVE.1634
c$omp&private(j)                                                                 SOLVE.1635
          DO J=JBNES,JENES-1                                                     SOLVE.1636
#ifdef MPP1                                                                      SOLVE.1637
            DUMJ(J)=J                                                            SOLVE.1638
#endif                                                                           SOLVE.1639
            CALL SCHULTZ(T3DTEN,QV3DTEN,QC3DTEN,QR3DTEN,QI3DTEN,QNI3DTEN         SOLVE.1640
     +           ,QG3DTEN,QNC3DTEN,T3D,QV3D,QC3D,QR3D,QI3D,QNI3D,QG3D,           SOLVE.1641
     +           QNC3D,PP3D,PSB,RAINNC,J,INEST,IBNES,IENES-1)                    SOLVE.1642
          ENDDO                                                                  SOLVE.1643
        ENDIF                                                                    SOLVE.1644
#endif                                                                           SOLVE.1645
      ENDIF                                                                      SOLVE.1646
C                                                                                SOLVE.1647
C --- RAINP STORES RAIN TENDENCY FOR NEXT LSM STEP                               SOLVE.1648
C                                                                                SOLVE.1649
      IF(ISOIL.GE.2 .OR. IMOIAV(INEST).GE.1 .OR. IFSNOW(INEST).EQ.2)THEN         05DEC01.528
cmic$ do all autoscope                                                           23DEC04.1045
cmic$2 private(j,i,K,TPOSDZ,SNOWFRAC,IS)                                         23DEC04.1046
c$doacross                                                                       23DEC04.1047
c$& local(j,i,K,TPOSDZ,SNOWFRAC,IS)                                              23DEC04.1048
c$omp parallel do default(shared)                                                23DEC04.1049
c$omp&private(j,i,K,TPOSDZ,SNOWFRAC,IS)                                          23DEC04.1050
        DO J=1,JLX                                                               SOLVE.1657
          DO I=1,ILX                                                             SOLVE.1658
            RAINP(I,J)=RAINNC(I,J)+RAINC(I,J)-PRECIP2D(I,J)                      SOLVE.1659
            if(ifsnow(inest).eq.2.and.xland(i,j).lt.1.5)then                     05DEC01.529
c  IFSNOW=2: SNOW COVER UPDATED WITH EXPLICIT PRECIP RATE                        05DEC01.530
              if(imphys(inest).eq.4)then                                         05DEC01.531
c               no explicit snow so estimate snow melt in atmosphere             05DEC01.532
                if(qr3d(i,j,kl).gt.1.e-6)then                                    05DEC01.533
                  tposdz=0.                                                      05DEC01.534
                  do k=1,kl                                                      05DEC01.535
                    if(t3d(i,j,k).gt.273.16)tposdz=tposdz+                       05DEC01.536
     -                (t3d(i,j,k)-273.16)*dsigma(k)*psa(i,j)*1000.               05DEC01.537
     -                /(rho0(i,j,k)*g)                                           05DEC01.538
                  enddo                                                          05DEC01.539
c      1.e-6*dT*dz = snow melt                                                   05DEC01.540
                  snowfrac=amax1(0.,qr3d(i,j,kl)-1.e-6*tposdz)                   05DEC01.541
     -                     /qr3d(i,j,kl)                                         05DEC01.542
c     snow accumulation (rainp in cm, snowc in mm)                               05DEC01.543
                  snowc(i,j)=snowc(i,j)+10.*rainp(i,j)*snowfrac                  05DEC01.544
                endif                                                            05DEC01.545
              else if(imphys(inest).eq.5.or.imphys(inest).eq.7)then              05DEC01.546
c               explicit snow rate should be in snownct                          05DEC01.547
                if(snownct(i,j).gt.1.e-3)then                                    05DEC01.548
c     snow accumulation (snownct in cm, snowc in mm)                             05DEC01.549
                  snowc(i,j)=snowc(i,j)+10.*snownct(i,j)                         05DEC01.550
                endif                                                            05DEC01.551
              else                                                               05DEC01.552
                print *,'ifsnow=2 not yet implemented for imphys ='              05DEC01.553
     -                   ,imphys(inest)                                          05DEC01.554
              endif                                                              05DEC01.555
                                                                                 05DEC01.556
c     reset land-use properties for snow cover                                   05DEC01.557
              IS=INT(SATBRT(I,J)+0.001)                                          05DEC01.558
              if(snowc(i,j).gt.0.0)then                                          05DEC01.559
                if(isoil.ne.2)then                                               19DEC02.1289
                  ALB(I,J)=ALBD(IS,ISN)/100.*(1.+SCFX(IS))                       19DEC02.1290
                  THC(I,J)=THERIN(ISICE,ISN)/100.                                19DEC02.1291
                  MAVAIL(I,J)=SLMO(ISICE,ISN)                                    19DEC02.1292
                endif                                                            19DEC02.1293
                EMISS(I,J)=SFEM(ISICE,ISN)                                       05DEC01.562
              else                                                               05DEC01.564
                if(isoil.ne.2)then                                               19DEC02.1294
                  ALB(I,J)=ALBD(IS,ISN)/100.                                     19DEC02.1295
                  THC(I,J)=THERIN(IS,ISN)/100.                                   19DEC02.1296
                  IF(IMOIAV(INEST).EQ.0)MAVAIL(I,J)=SLMO(IS,ISN)                 23DEC04.1051
                endif                                                            19DEC02.1298
                EMISS(I,J)=SFEM(IS,ISN)                                          05DEC01.567
              endif                                                              05DEC01.569
            endif                                                                05DEC01.570
          ENDDO                                                                  SOLVE.1660
        ENDDO                                                                    SOLVE.1661
      ENDIF                                                                      SOLVE.1662
C                                                                                SOLVE.1663
C *** UPDATE BOUNDARY VALUES FOR OUTMOST ROW AND COLUMN FOR DOMAIN 1,            SOLVE.1664
C     AND TWO ROWS AND COLUMNS FOR DOMAIN 2                                      SOLVE.1665
C                                                                                SOLVE.1666
      IF(INEST.GT.1)THEN                                                         SOLVE.1667
        DTBC=DTMIN*60.*NN                                                        SOLVE.1668
      ELSE                                                                       SOLVE.1669
        DTBC=(XTIME+DTMIN-TBDYBE)*60.                                            SOLVE.1670
      ENDIF                                                                      SOLVE.1671
      EPSI=0.0                                                                   SOLVE.1672
      CALL BDYRST(TA,TB,T3DTEN,KZZ,ILX,JLX,DTBC,TSB,TNB,TEB,TWB,INEST,2,         SOLVE.1673
     +     EPSI)                                                                 SOLVE.1674
C                                                                                SOLVE.1675
      IF(IMPHYS(INEST).GE.2)THEN                                                 SOLVE.1676
        EPSI=1.0E-12                                                             SOLVE.1677
        CALL BDYRST(QVA,QVB,QV3DTEN,KZZ,ILX,JLX,DTBC,QSB,QNB,QEB,QWB,            SOLVE.1678
     +       INEST,1,EPSI)                                                       SOLVE.1679
        IF((IBMOIST.EQ.1).OR.(INEST.GT.1))THEN                                   SOLVE.1680
          IBQ=1                                                                  SOLVE.1681
          EPSI=0.0                                                               SOLVE.1682
          IF(IMPHYS(INEST).GE.3)THEN                                             SOLVE.1683
            CALL BDYRST(QCA,QCB,QC3DTEN,KZZ,ILX,JLX,DTBC,QCSB,QCNB,QCEB,         SOLVE.1684
     +           QCWB,INEST,IBQ,EPSI)                                            SOLVE.1685
            CALL BDYRST(QRA,QRB,QR3DTEN,KZZ,ILX,JLX,DTBC,QRSB,QRNB,QREB,         SOLVE.1686
     +           QRWB,INEST,IBQ,EPSI)                                            SOLVE.1687
            IF(IMPHYS(INEST).GE.5)THEN                                           SOLVE.1688
              CALL BDYRST(QIA,QIB,QI3DTEN,KZZ,ILX,JLX,DTBC,QISB,QINB,            SOLVE.1689
     +             QIEB,QIWB,INEST,IBQ,EPSI)                                     SOLVE.1690
              CALL BDYRST(QNIA,QNIB,QNI3DTEN,KZZ,ILX,JLX,DTBC,QNISB,             SOLVE.1691
     +             QNINB,QNIEB,QNIWB,INEST,IBQ,EPSI)                             SOLVE.1692
              IF(IMPHYS(INEST).GE.6)THEN                                         SOLVE.1693
                CALL BDYRST(QGA,QGB,QG3DTEN,KZZ,ILX,JLX,DTBC,QGSB,QGNB,          SOLVE.1694
     +               QGEB,QGWB,INEST,IBQ,EPSI)                                   SOLVE.1695
                CALL BDYRST(QNCA,QNCB,QNC3DTEN,KZZ,ILX,JLX,DTBC,QNCSB,           SOLVE.1696
     +               QNCNB,QNCEB,QNCWB,INEST,IBQ,EPSI)                           SOLVE.1697
              ENDIF                                                              SOLVE.1698
            ENDIF                                                                SOLVE.1699
          ENDIF                                                                  SOLVE.1700
        ENDIF                                                                    SOLVE.1701
      ENDIF                                                                      SOLVE.1702
C                                                                                SOLVE.1703
C --- UCD is used to save future T (at KTAU+1)                                   SOLVE.1704
C     VCD is used to save future QV(at KTAU+1)                                   SOLVE.1705
C                                                                                SOLVE.1706
cmic$ do all autoscope                                                           SOLVE.1707
c$doacross                                                                       SOLVE.1708
c$& local(i,j,k)                                                                 SOLVE.1709
c$omp parallel do default(shared)                                                SOLVE.1710
c$omp&private(i,j,k)                                                             SOLVE.1711
      DO J=JBNES,JENES-1                                                         SOLVE.1712
        DO K=1,KL                                                                SOLVE.1713
          DO I=IBNES,IENES-1                                                     SOLVE.1714
            UCD(I,J,K)=TB(I,J,K)+DT*T3DTEN(I,J,K)                                SOLVE.1715
          ENDDO                                                                  SOLVE.1716
        ENDDO                                                                    SOLVE.1717
C                                                                                SOLVE.1718
        IF(IDRY(INEST).EQ.0)THEN                                                 SOLVE.1719
          DO K=1,KL                                                              SOLVE.1720
            DO I=IBNES,IENES-1                                                   SOLVE.1721
              VCD(I,J,K)=QVB(I,J,K)+DT*QV3DTEN(I,J,K)                            SOLVE.1722
            ENDDO                                                                SOLVE.1723
          ENDDO                                                                  SOLVE.1724
        ENDIF                                                                    SOLVE.1725
      ENDDO                                                                      SOLVE.1726
C                                                                                SOLVE.1727
C                                                                                SOLVE.1728
#if defined(ICUPA5) || defined(ICUPA6) || defined(ICUPA8)
      IF(ICUPA(INEST).EQ.5.OR.ICUPA(INEST).EQ.6.OR.                              05DEC01.572
     -   ICUPA(INEST).EQ.8)THEN                                                  05DEC01.573
cmic$ do all autoscope                                                           SOLVE.1731
cmic$1 shared(ibnes,ienes,jbnes,jenes,k700,inest,ucd,vcd,psa)                    SOLVE.1732
cmic$2 private(j)                                                                SOLVE.1733
c$doacross                                                                       SOLVE.1734
c$& local(j)                                                                     SOLVE.1735
c$omp parallel do default(shared)                                                SOLVE.1736
c$omp&private(j)                                                                 SOLVE.1737
Ccsd$ parallel do private(j) schedule(static,1)                                  23DEC04.1052
        DO J=JBNES,JENES-1                                                       SOLVE.1738
#ifdef MPP1                                                                      SOLVE.1739
          DUMJ(J)=J                                                              SOLVE.1740
#endif                                                                           SOLVE.1741
          CALL CONVAD(IBNES,IENES-1,K700-2,INEST,J,UCD,VCD,PSA)                  SOLVE.1742
        ENDDO                                                                    SOLVE.1743
Ccsd$ end parallel do                                                            23DEC04.1053
      ENDIF                                                                      SOLVE.1744
#endif                                                                           SOLVE.1745
C                                                                                SOLVE.1746
#if defined(IBLT0) || defined(IBLT1)
      IF(IBLTYP(INEST).LT.2)THEN                                                 SOLVE.1748
cmic$ do all autoscope                                                           SOLVE.1749
cmic$1 shared(ibnes,ienes,jbnes,jenes,kl,inest,ucd,vcd,psa)                      SOLVE.1750
cmic$2 private(j)                                                                SOLVE.1751
c$doacross                                                                       SOLVE.1752
c$& local(j)                                                                     SOLVE.1753
c$omp parallel do default(shared)                                                SOLVE.1754
c$omp&private(j)                                                                 SOLVE.1755
Ccsd$ parallel do private(j) schedule(static,1)                                  23DEC04.1054
        DO J=JBNES,JENES-1                                                       SOLVE.1756
#ifdef MPP1                                                                      SOLVE.1757
          DUMJ(J)=J                                                              SOLVE.1758
#endif                                                                           SOLVE.1759
          CALL CONVAD(IBNES,IENES-1,KL,INEST,J,UCD,VCD,PSA)                      SOLVE.1760
        ENDDO                                                                    SOLVE.1761
Ccsd$ end parallel do                                                            23DEC04.1055
      ENDIF                                                                      SOLVE.1762
#endif                                                                           SOLVE.1763
C                                                                                SOLVE.1764
C *** COMPUTE FUTURE VALUES OF T AND MOISTURE VARIABLES AT TAU+1:                SOLVE.1765
C                                                                                SOLVE.1766
C --- Note: UCD stores future T (at KTAU+1)                                      SOLVE.1767
C           VCD stores future QV(at KTAU+1)                                      SOLVE.1768
C                                                                                SOLVE.1769
cmic$ do all autoscope                                                           SOLVE.1770
c$doacross                                                                       SOLVE.1771
c$& local(i,j,k,qcc,qrc,qic,qnic,qgc,qncc)                                       SOLVE.1772
c$omp parallel do default(shared)                                                SOLVE.1773
c$omp&private(i,j,k,qcc,qrc,qic,qnic,qgc,qncc)                                   SOLVE.1774
      DO 790 J=JBNES,JENES-1                                                     SOLVE.1775
        DO 750 K=1,KL                                                            SOLVE.1776
          DO 750 I=IBNES,IENES-1                                                 SOLVE.1777
            TB(I,J,K)=OMUHF*TA(I,J,K)+GNUHF*(TB(I,J,K)+UCD(I,J,K))               SOLVE.1778
            TA(I,J,K)=UCD(I,J,K)                                                 SOLVE.1779
  750     CONTINUE                                                               SOLVE.1780
                                                                                 SOLVE.1781
C                                                                                SOLVE.1782
        IF(IMPHYS(INEST).GE.2)THEN                                               SOLVE.1783
          DO 760 K=1,KL                                                          SOLVE.1784
            DO 760 I=IBNES,IENES-1                                               SOLVE.1785
              IF(VCD(I,J,K).LT.1.E-12)VCD(I,J,K)=1.E-12                          SOLVE.1786
              QVB(I,J,K)=OMUHF*QVA(I,J,K)+GNUHF*(QVB(I,J,K)+VCD(I,J,K))          SOLVE.1787
              QVA(I,J,K)=VCD(I,J,K)                                              SOLVE.1788
  760       CONTINUE                                                             SOLVE.1789
                                                                                 SOLVE.1790
          IF(IMPHYS(INEST).GE.3)THEN                                             SOLVE.1791
            DO 770 K=1,KL                                                        SOLVE.1792
              DO 770 I=IBNES,IENES-1                                             SOLVE.1793
                QCC=QCB(I,J,K)+DT*QC3DTEN(I,J,K)                                 SOLVE.1794
                QRC=QRB(I,J,K)+DT*QR3DTEN(I,J,K)                                 SOLVE.1795
C                                                                                SOLVE.1796
                IF(QCC.LT.1.E-20)THEN                                            SOLVE.1797
                  QCC=0.0                                                        SOLVE.1798
                ENDIF                                                            SOLVE.1799
                QCB(I,J,K)=OMUHF*QCA(I,J,K)+GNUHF*(QCB(I,J,K)+QCC)               SOLVE.1800
                QCA(I,J,K)=QCC                                                   SOLVE.1801
C                                                                                SOLVE.1802
                IF(QRC.LT.1.E-20)THEN                                            SOLVE.1803
                  QRC=0.0                                                        SOLVE.1804
                ENDIF                                                            SOLVE.1805
                QRB(I,J,K)=OMUHF*QRA(I,J,K)+GNUHF*(QRB(I,J,K)+QRC)               SOLVE.1806
                QRA(I,J,K)=QRC                                                   SOLVE.1807
  770         CONTINUE                                                           SOLVE.1808
                                                                                 SOLVE.1809
            IF(IMPHYS(INEST).GE.5)THEN                                           SOLVE.1810
              DO 780 K=1,KL                                                      SOLVE.1811
                DO 780 I=IBNES,IENES-1                                           SOLVE.1812
                  QIC=QIB(I,J,K)+DT*QI3DTEN(I,J,K)                               SOLVE.1813
                  QNIC=QNIB(I,J,K)+DT*QNI3DTEN(I,J,K)                            SOLVE.1814
C                                                                                SOLVE.1815
                  IF(QIC.LT.1.E-20)THEN                                          SOLVE.1816
                    QIC=0.0                                                      SOLVE.1817
                  ENDIF                                                          SOLVE.1818
                  QIB(I,J,K)=OMUHF*QIA(I,J,K)+GNUHF*(QIB(I,J,K)+QIC)             SOLVE.1819
                  QIA(I,J,K)=QIC                                                 SOLVE.1820
C                                                                                SOLVE.1821
                  IF(QNIC.LT.1.E-20)THEN                                         SOLVE.1822
                    QNIC=0.0                                                     SOLVE.1823
                  ENDIF                                                          SOLVE.1824
                  QNIB(I,J,K)=OMUHF*QNIA(I,J,K)+GNUHF*(QNIB(I,J,K)+QNIC)         SOLVE.1825
                  QNIA(I,J,K)=QNIC                                               SOLVE.1826
  780           CONTINUE                                                         SOLVE.1827
                                                                                 SOLVE.1828
              IF(IMPHYS(INEST).GE.6)THEN                                         SOLVE.1829
                DO 785 K=1,KL                                                    SOLVE.1830
                  DO 785 I=IBNES,IENES-1                                         SOLVE.1831
                    QGC=QGB(I,J,K)+DT*QG3DTEN(I,J,K)                             SOLVE.1832
                    QNCC=QNCB(I,J,K)+DT*QNC3DTEN(I,J,K)                          SOLVE.1833
C                                                                                SOLVE.1834
                    IF(QGC.LT.1.E-20)THEN                                        SOLVE.1835
                      QGC=0.0                                                    SOLVE.1836
                    ENDIF                                                        SOLVE.1837
                    QGB(I,J,K)=OMUHF*QGA(I,J,K)+GNUHF*(QGB(I,J,K)+QGC)           SOLVE.1838
                    QGA(I,J,K)=QGC                                               SOLVE.1839
C                                                                                SOLVE.1840
                    IF(QNCC.LT.1.E-20)THEN                                       SOLVE.1841
                      QNCC=0.0                                                   SOLVE.1842
                    ENDIF                                                        SOLVE.1843
                    QNCB(I,J,K)=OMUHF*QNCA(I,J,K)+GNUHF*(QNCB(I,J,K)+            SOLVE.1844
     +                          QNCC)                                            SOLVE.1845
                    QNCA(I,J,K)=QNCC                                             SOLVE.1846
  785             CONTINUE                                                       SOLVE.1847
                                                                                 SOLVE.1848
              ENDIF                                                              SOLVE.1849
            ENDIF                                                                SOLVE.1850
          ENDIF                                                                  SOLVE.1851
        ENDIF                                                                    SOLVE.1852
  790 CONTINUE                                                                   SOLVE.1853
C                                                                                SOLVE.1854
C --- PUT TENDENCIES INTO SOUND-WAVE TENDENCY ARRAYS                             SOLVE.1855
C                                                                                SOLVE.1856
cmic$ do all autoscope                                                           SOLVE.1857
c$doacross                                                                       SOLVE.1858
c$& local(i,j,k)                                                                 SOLVE.1859
c$omp parallel do default(shared)                                                SOLVE.1860
c$omp&private(i,j,k)                                                             SOLVE.1861
      DO J=1,JL                                                                  SOLVE.1862
        DO K=1,KL                                                                SOLVE.1863
          DO I=1,IL                                                              SOLVE.1864
            U3DTEN(I,J,K)=U3DTEN(I,J,K)*RPDOTA(I,J)                              SOLVE.1865
            V3DTEN(I,J,K)=V3DTEN(I,J,K)*RPDOTA(I,J)                              SOLVE.1866
          ENDDO                                                                  SOLVE.1867
        ENDDO                                                                    SOLVE.1868
        IF(J.NE.JL)THEN                                                          SOLVE.1869
          DO K=1,KL                                                              SOLVE.1870
            DO I=1,ILX                                                           SOLVE.1871
              W3DTEN(I,J,K)=W3DTEN(I,J,K)*RPSA(I,J)                              SOLVE.1872
              PP3DTEN(I,J,K)=PP3DTEN(I,J,K)*RPSA(I,J)                            SOLVE.1873
            ENDDO                                                                SOLVE.1874
          ENDDO                                                                  SOLVE.1875
          DO I=1,ILX                                                             SOLVE.1876
            W3DTEN(I,J,KLP1)=W3DTEN(I,J,KLP1)*RPSA(I,J)                          SOLVE.1877
          ENDDO                                                                  SOLVE.1878
        ENDIF                                                                    SOLVE.1879
      ENDDO                                                                      SOLVE.1880
C                                                                                SOLVE.1881
C *** COMPUTE U,V,W,PP AT TAU+1:                                                 SOLVE.1882
C                                                                                SOLVE.1883
      CALL SOUND(IYY,JXX,UB,VB,TB,PR1,RHO1,UA,VA,TA,QVB,PSA,HT,MSFD,MSFX         SOLVE.1884
     +     ,U3DTEN,V3DTEN,PP3DTEN,W3DTEN,DX,DT,INEST,KTAU)                       SOLVE.1885
C                                                                                SOLVE.1886
C --- INCREMENT ELAPSED FORECAST TIME:                                           SOLVE.1887
C                                                                                SOLVE.1888
      KTAU=KTAU+1                                                                SOLVE.1889
      XTIME=XSTNES(INEST)+FLOAT(KTAU)*DTMIN                                      19DEC02.1299
      IF(KTAU.NE.0)DT=DT2                                                        SOLVE.1891
#ifndef MPP1                                                                     07NOV00.1090
         PRINT *, NINT(XTIME*60.), INEST                                         07NOV00.1091
#else                                                                            07NOV00.1092
#include <mpp_solve_50.incl>                                                     07NOV00.1093
#endif                                                                           07NOV00.1094
C                                                                                SOLVE.1892
C --- UPDATE THE CONVECTIVE TIME SCALE                                           SOLVE.1893
C                                                                                SOLVE.1894
      IF(ICUPA(INEST).EQ.5.OR.ICUPA(INEST).EQ.6.OR.                              05DEC01.575
     -   ICUPA(INEST).EQ.8)THEN                                                  05DEC01.576
cmic$ do all autoscope                                                           SOLVE.1896
c$doacross                                                                       SOLVE.1897
c$& local(i,j)                                                                   SOLVE.1898
c$omp parallel do default(shared)                                                SOLVE.1899
c$omp&private(i,j)                                                               SOLVE.1900
        DO 1075 J=1,JLX                                                          SOLVE.1901
          DO 1075 I=1,ILX                                                        SOLVE.1902
            NCA(I,J)=NCA(I,J)-1                                                  SOLVE.1903
 1075     CONTINUE                                                               SOLVE.1904
                                                                                 SOLVE.1905
      ENDIF                                                                      SOLVE.1906
C                                                                                SOLVE.1907
C --- PRINT MAX W IN DOMAIN                                                      SOLVE.1908
C                                                                                SOLVE.1909
      IF(IFPRT.EQ.1)THEN                                                         SOLVE.1910
cmic$ do all autoscope                                                           SOLVE.1911
c$doacross                                                                       SOLVE.1912
c$& local(i,j,k,wtest)                                                           SOLVE.1913
c$omp parallel do default(shared)                                                SOLVE.1914
c$omp&private(i,j,k,wtest)                                                       SOLVE.1915
        DO 900 K=1,KLP1                                                          SOLVE.1916
          WMAX(K)=0.                                                             SOLVE.1917
          DO I=1,ILX                                                             SOLVE.1918
            DO J=1,JLX                                                           SOLVE.1919
              WTEST=WA(I,J,K)*RPSA(I,J)                                          SOLVE.1920
              IF(ABS(WTEST).GT.ABS(WMAX(K)))THEN                                 SOLVE.1921
                WMAX(K)=WTEST                                                    SOLVE.1922
                IMAX(K)=I                                                        SOLVE.1923
                JMAX(K)=J                                                        SOLVE.1924
              ENDIF                                                              SOLVE.1925
            ENDDO                                                                SOLVE.1926
          ENDDO                                                                  SOLVE.1927
  900   CONTINUE                                                                 SOLVE.1928
        WMAXP=0.                                                                 SOLVE.1929
        DO 901 K=1,KLP1                                                          SOLVE.1930
          IF(ABS(WMAX(K)).GT.ABS(WMAXP))THEN                                     SOLVE.1931
            WMAXP=WMAX(K)                                                        SOLVE.1932
            IMAXP=IMAX(K)                                                        SOLVE.1933
            JMAXP=JMAX(K)                                                        SOLVE.1934
            KMAXP=K                                                              SOLVE.1935
          ENDIF                                                                  SOLVE.1936
  901   CONTINUE                                                                 SOLVE.1937
      PRINT *,' MAXW I J K INEST =',WMAXP,IMAXP,JMAXP,KMAXP,INEST                SOLVE.1938
      ENDIF                                                                      SOLVE.1939
C                                                                                07NOV00.1095
C --- OUTPUT TIME SERIES                                                         07NOV00.1096
C                                                                                07NOV00.1097
      IF (IFTSOUT) CALL OUTTS(INEST,IL,JL,XTIME)                                 07NOV00.1098
C                                                                                SOLVE.1940
C *** RECALCULATE SOLAR DECLINATION ANGLE IF FORECAST TIME LARGER THAN           SOLVE.1941
C     24 HOURS:                                                                  SOLVE.1942
C                                                                                SOLVE.1943
      IF(INEST.EQ.1)THEN                                                         05DEC01.577
        IF(XTIME.GE.DECTIM)CALL SOLAR1(XTIME,JULDAY,GMT,2)                       05DEC01.578
      ENDIF                                                                      05DEC01.579
C *** RESET VEGETATION FRACTION EVERY 24 HOURS (VEGFRQ=1440.)                    05DEC01.580
      IF((ILDDIM.EQ.1).AND.(ISOIL.EQ.2))THEN                                     23DEC04.1056
        VEGFRQ=1440.                                                             05DEC01.582
        KVEG=NINT(VEGFRQ/DTMIN)                                                  05DEC01.583
        IF(KTAU.NE.0 .AND. MOD(KTAU,KVEG).EQ.0)THEN                              05DEC01.584
          CALL SETVEGFR(ILX,JLX,XTIME,JULDAY,GMT)                                05DEC01.585
        ENDIF                                                                    05DEC01.586
      ENDIF                                                                      05DEC01.587
C                                                                                SOLVE.1948
      IEXEC=2                                                                    SOLVE.1949
C                                                                                SOLVE.1950
      RETURN                                                                     SOLVE.1951
      END                                                                        SOLVE.1952
                                                                                 SOLVE.1953
