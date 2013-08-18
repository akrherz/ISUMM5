#include <defines.incl>                                                          07NOV00.1771
      SUBROUTINE MRFPBL(IYY,JXX,J,IN,U3D,V3D,T3D,QV3D,PP3D,QC3D,QI3D,            MRFPBL.1
     +   QNC3D,U3DTEN,V3DTEN,QC3DTEN,T3DTEN,QV3DTEN,QI3DTEN,QNC3DTEN,            07NOV00.1772
     +   TGA,TGB,PSB,RPSB,XLV,                                                   07NOV00.1773
     +   TA2,QA2,UA10,VA10,                                                      07NOV00.1774
     +   SVP1,SVP2,SVP3,SVPT0,EP1,EP2,ZNT,XLAND,UST,                             07NOV00.1775
     +   MAVAIL,                                                                 07NOV00.1776
     +   REGIME,HOL,PBL,ZOL,MOL,QFX,HFX,RV,TSLA,TSLB,T0,PPB,                     07NOV00.1777
     +   HFXSI,QFXSI,TGSI,SEAICE,                                                23DEC04.3446
     +   EMISS,GLW,GSW,TMN,CAPG,SNOWC,XLAT,XLONG,                                07NOV00.1778
     +   RAINP,RAINC,RAINNC,                                                     07NOV00.1779
     +   PRW,ALB,SHC,THC,SATBRT,                                                 07NOV00.1780
     +   XMOIST,ISICE,                                                           23DEC04.3447
     +   INTERIOR_MASK ,                                                         07NOV00.1782
     +   IST,IEN)                                                                07NOV00.1783
      IMPLICIT NONE                                                              07NOV00.1784
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          MRFPBL.3
C                                                                     C          MRFPBL.4
C     BASED ON THE "COUNTERGRADIENT" TRANSPORT TERM OF TROEN          C          MRFPBL.5
C     AND MAHRT (1986) FOR THE UNSTABLE PBL.                          C          MRFPBL.6
C     THIS ROUTINE USES AN IMPLICIT APPROACH FOR VERTICAL FLUX        C          MRFPBL.7
C     DIVERGENCE AND DOES NOT REQUIRE "MITER" TIMESTEPS.              C          MRFPBL.8
C     IT INCLUDES VERTICAL DIFFUSION IN THE STABLE ATMOSPHERE         C          MRFPBL.9
C     AND MOIST VERTICAL DIFFUSION IN CLOUDS.                         C          MRFPBL.10
C     SURFACE FLUXES CALCULATED AS IN HIRPBL.                         C          MRFPBL.11
C     5-LAYER SOIL MODEL OPTION REQUIRED IN SLAB DUE TO LONG TIMESTEP C          MRFPBL.12
C                                                                     C          MRFPBL.13
C     CODED BY SONG-YOU HONG (NCEP), IMPLEMENTED BY JIMY DUDHIA (NCAR)C          MRFPBL.14
C     FALL 1996                                                       C          MRFPBL.15
C                                                                     C          MRFPBL.16
C     REFERENCES:                                                     C          MRFPBL.17
C                                                                     C          MRFPBL.18
C        HONG AND PAN (1996), MON. WEA. REV.                          C          MRFPBL.19
C        TROEN AND MAHRT (1986), BOUNDARY LAYER MET.                  C          MRFPBL.20
C                                                                     C          MRFPBL.21
C     CHANGES:                                                        C          MRFPBL.22
C        INCREASE RLAM FROM 30 TO 150, AND CHANGE FREE ATMOSPHERE     C          MRFPBL.23
C        STABILITY FUNCTION TO INCREASE VERTICAL DIFFUSION            C          MRFPBL.24
C        (HONG, JUNE 1997)                                            C          MRFPBL.25
C                                                                     C          MRFPBL.26
C        PUT LOWER LIMIT ON PSI FOR STABLE CONDITIONS. THIS WILL      C          MRFPBL.27
C        PREVENT FLUXES FROM BECOMING TOO SMALL (DUDHIA, OCTOBER 1997)C          MRFPBL.28
C                                                                     C          MRFPBL.29
C        CORRECTION TO REGIME CALCULATION. THIS WILL ALLOW POINTS IN  C          MRFPBL.30
C        REGIME 4 MUCH MORE FREQUENTLY GIVING LARGER SURFACE FLUXES   C          MRFPBL.31
C        REGIME 3 NO LONGER USES HOL < 1.5 OR THVX LAPSE-RATE CHECK   C          MRFPBL.32
C        IN MRF SCHEME. THIS WILL MAKE REGIME 3 MUCH LESS FREQUENT.   C          MRFPBL.33
C                                                                     C          MRFPBL.34
C        ADD SURFACE PRESSURE, PS(I), ARRAY FOR EFFICIENCY            C          MRFPBL.35
C                                                                     C          MRFPBL.36
C        FIX FOR PROBLEM WITH THIN LAYERS AND HIGH ROUGHNESS          C          MRFPBL.37
C                                                                     C          MRFPBL.38
C        CHARNOCK CONSTANT NOW COMES FROM NAMELIST (DEFAULT SAME)     C          MRFPBL.39
C                                                                     C          MRFPBL.40
C          USE TGB AS A TEMPERATURE NOT THETA IN PBL                  C          07NOV00.1785
C                                                                     C          07NOV00.1786
C        ADD IZ0TOPT SWITCH                                           C          05DEC01.981
C                                                                     C          05DEC01.982
C        MODIFIED MOIST VERTICAL DIFFUSION TO PREVENT LOW-CLOUD BIAS  C          05DEC01.983
C                                                                     C          05DEC01.984
C        ISFMTHD = 1 ADDED NIELSEN-GAMMON CORRECTION TO STABLE        C          23DEC04.3448
C          REGIME 2M AND 10M INTERPOLATIONS                           C          23DEC04.3449
C                                                                     C          23DEC04.3450
C       ADD POLAR MODS FOR MRF SCHEME (SEA-ICE AND ICE SURFACE EFFECTSC          23DEC04.3451
C                                                                     C          23DEC04.3452
C       FOR 3.7:                                                      C          23DEC04.3453
C         USE BELJAARS FORMULA FOR CONVECTIVE VELOCITY                C          23DEC04.3454
C         REMOVE CONVECTIVE VELOCITY CONTRIBUTION TO SURFACE STRESS   C          23DEC04.3455
C         OUTPUT PBL HEIGHT BASED ON STABILITY ALONE (NOT RI)         C          23DEC04.3456
C                                                                     C          23DEC04.3457
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          MRFPBL.41
#     include <parame.incl>                                                      MRFPBL.42
#ifndef VECTOR                                                                   07NOV00.1787
#     include <sum.incl>                                                         MRFPBL.43
#endif                                                                           07NOV00.1788
C                                                                                07NOV00.1789
C Defines for IMPLICIT NONE                                                      07NOV00.1790
      INTEGER I,J,K,KK,L                                                         07NOV00.1791
      INTEGER IYY,JXX                                                            07NOV00.1792
      INTEGER KLPBL,IPP,ICUT,IN                                                  07NOV00.1793
      INTEGER NZOL,NZOL10,NZOL2                                                  07NOV00.1794
      INTEGER JBGN,JEND                                                          07NOV00.1795
CB    REAL RLAM,PRMIN,PRMAX,XKZMIN,XKZMAX,RIMIN  
      REAL RLAM,RLAMX,PRMIN,PRMAX,XKZMIN,XKZMAX,RIMIN 
      REAL BRCR,CFAC,PFAC,SFCFRAC,CKZ,ZFMIN,APHI5                                07NOV00.1797
      REAL APHI16,GAMCRT,GAMCRQ,XKA,PRT,CONT,CONQ,CONW                           07NOV00.1798
      REAL PL,THCON,TVCON,E1,CELL,DTSTEP,ZL,TGBV,DTHVDZ                          07NOV00.1799
      REAL DTHVM,VCONV,ZOL10,ZOL2,RZOL,RZOL10,RZOL2                              07NOV00.1800
      REAL DTG,PSIX,PSIX10,PSIT,PSIQ,PSIT2,PSIQ2                                 05DEC01.985
      REAL DTTHX,DELTSM,DT4,RDT,SPDK2,BRINT,FM,FH,HOL1                           07NOV00.1802
      REAL GAMFAC,VPERT,PRNUM,ZFAC,XKZO,SS,RI,QMEAN,TMEAN                        07NOV00.1803
      REAL ALPH,CHI,ZK,RL2,DK,SRI,DTODSD,DTODSU,DSIG                             07NOV00.1804
      REAL RDZ,DSDZT,DSDZQ,DSDZ2                                                 07NOV00.1805
      REAL TTEND,QTEND,UTEND,VTEND,QCTEND,QITEND                                 07NOV00.1806
      REAL TGC,TSLC,XXT,VISC,RESTAR                                              05DEC01.986
      REAL FLUXC,VSGD                                                            23DEC04.3458
C                                                                                MRFPBL.45
C-----CONSTANTS FOR MRFPBL                                                       MRFPBL.46
C                                                                                MRFPBL.47
CB    PARAMETER (KLPBL=1,RLAM=150.,PRMIN=0.5,PRMAX=4.)  
      PARAMETER (KLPBL=1,RLAM=100.,PRMIN=0.5,PRMAX=4.)                           
      PARAMETER (XKZMIN=0.01,XKZMAX=1000.,RIMIN=-100.)                           MRFPBL.49
      PARAMETER (BRCR=0.5,CFAC=7.8,PFAC=2.0,SFCFRAC=0.1)                         MRFPBL.50
      PARAMETER (CKZ=0.001,ZFMIN=1.E-8,APHI5=5.,APHI16=16.)                      MRFPBL.51
      PARAMETER (GAMCRT=3.,GAMCRQ=2.E-3)                                         MRFPBL.52
      PARAMETER (XKA=2.4E-5)                                                     MRFPBL.53
      PARAMETER (PRT=1.)                                                         MRFPBL.54
C                                                                                MRFPBL.56
      REAL      U3D(MIX,MJX,MKX),V3D(MIX,MJX,MKX),T3D(MIX,MJX,MKX),              07NOV00.1808
     -    QV3D(MIX,MJX,MKX),PP3D(MIX,MJX,MKX),QC3D(MIXM,MJXM,MKXM),              MRFPBL.58
     -    QI3D(MIXIC,MJXIC,MKXIC),QNC3D(MIXIG,MJXIG,MKXIG),                      MRFPBL.59
     -    U3DTEN(MIX,MJX,MKX),V3DTEN(MIX,MJX,MKX),T3DTEN(MIX,MJX,MKX),           MRFPBL.60
     -    QV3DTEN(MIX,MJX,MKX),QC3DTEN(MIXM,MJXM,MKXM),                          MRFPBL.61
     -    QI3DTEN(MIXIC,MJXIC,MKXIC),QNC3DTEN(MIXIG,MJXIG,MKXIG)                 MRFPBL.62
      REAL      INTERIOR_MASK(MIX,MJX)                                           07NOV00.1809
C                                                                                MRFPBL.63
      REAL         UX(MIX,MKX),    VX(MIX,MKX),    QX(MIX,MKX),                  07NOV00.1810
     1            QCX(MIXM,MKXM), THX(MIX,MKX),  THVX(MIX,MKX),                  MRFPBL.65
     2            DZQ(MIX,MKX),   DZA(MIX,MKX),                                  MRFPBL.66
     3           TTNP(MIX,MKX),  QTNP(MIX,MKX), QCTNP(MIXM,MKXM),                MRFPBL.67
     4             ZA(MIX,MKX),   UXS(MIX,MKX),                                  MRFPBL.68
     5            VXS(MIX,MKX),  THXS(MIX,MKX),   QXS(MIX,MKX),                  MRFPBL.69
     6           QCXS(MIXM,MKXM)                                                 MRFPBL.70
      REAL      QIX(MIXM,MKXM),QITNP(MIXM,MKXM),                                 07NOV00.1811
     1          QIXS(MIXM,MKXM),QIXSV(MIX)                                       MRFPBL.72
      REAL      QNCX(MIXM,MKXM),QNCNP(MIXM,MKXM),FQNC(MIXM,MKXM),                07NOV00.1812
     1          QNCXS(MIXM,MKXM),FLQNC(MIX),QNCXSV(MIX)                          MRFPBL.74
      REAL         ZQ(MIX,KXP1), UTNP(MIX,MKX), VTNP(MIX,MKX)                    07NOV00.1813
C                                                                                MRFPBL.76
      REAL        RHOX(MIX), GOVRTH(MIX), GZ1OZ0(MIX),                           07NOV00.1814
     1            WSPD(MIX),     BR(MIX),   PSIM(MIX),   PSIH(MIX),              MRFPBL.78
     2            FLHC(MIX),   FLQC(MIX),  THXSV(MIX),   UXSV(MIX),              MRFPBL.79
     4            VXSV(MIX),   QXSV(MIX),  QCXSV(MIX),   THGB(MIX),              07NOV00.1815
     5             QGH(MIX),  TGDSA(MIX),  TGDSB(MIX),   PS(MIX)                 MRFPBL.81
      REAL      XKZMKL(MIX)                                                      07NOV00.1816
      INTEGER     KPBL(MIX),KPBL0(MIX)                                           23DEC04.3459
C                                                                                MRFPBL.83
#     include <varia.incl>                                                       MRFPBL.84
#     include <param2.incl>                                                      MRFPBL.86
#     include <param3.incl>                                                      MRFPBL.87
#     include <various.incl>                                                     MRFPBL.90
#     include <soilcnst.incl>                                                    MRFPBL.93
#     include <pbltb.incl>                                                       MRFPBL.95
      REAL XMOIST(MAXSES)                                                        23DEC04.3460
      REAL XLV,SVP1,SVP2,SVP3,SVPT0,RV,EP1,EP2                                   07NOV00.1819
      REAL TGA(MIX,MJX),TGB(MIX,MJX),HOL(MIX,MJX),ZOL(MIX,MJX)                   07NOV00.1820
      REAL TA2(MIX,MJX),QA2(MIX,MJX),UA10(MIX,MJX),VA10(MIX,MJX)                 07NOV00.1821
      REAL ZNT(MIX,MJX),XLAND(MIX,MJX),UST(MIX,MJX),MAVAIL(MIX,MJX)              07NOV00.1822
      REAL PBL(MIX,MJX),QFX(MIX,MJX),HFX(MIX,MJX),MOL(MIX,MJX)                   07NOV00.1823
      REAL HFXSI(MIX,MJX),QFXSI(MIX,MJX),TGSI(MIX,MJX),SEAICE(MIX,MJX)           23DEC04.3461
      REAL PSB(MIX,MJX),RPSB(MIX,MJX),REGIME(MIX,MJX)                            07NOV00.1824
      REAL TSLA(MIX,MJX,MLX),TSLB(MIX,MJX,MLX)                                   07NOV00.1825
                                                                                 07NOV00.1826
      REAL EMISS(MIX,MJX)                                                        07NOV00.1827
      REAL GLW(MIX,MJX),GSW(MIX,MJX),TMN(MIX,MJX)                                07NOV00.1828
      REAL CAPG(MIX,MJX),SNOWC(MIX,MJX)                                          07NOV00.1829
      REAL XLAT(MIX,MJX),XLONG(MIX,MJX)                                          07NOV00.1830
      REAL RAINP(MIX,MJX), RAINC(MIX,MJX), RAINNC(MIX,MJX)                       07NOV00.1831
      REAL PRW(MIX,MJX),ALB(MIX,MJX)                                             07NOV00.1832
      REAL SHC(MIX,MJX),THC(MIX,MJX)                                             07NOV00.1833
      REAL SATBRT(MIX,MJX)                                                       07NOV00.1834
      REAL CKLQ(MIX,MJX)                                                         05DEC01.987
                                                                                 07NOV00.1835
      REAL T0(MIX,MJX,MKX),PPB(MIX,MJX,MKX)                                      07NOV00.1836
      INTEGER IST,IEN,ISICE                                                      23DEC04.3462
C Local                                                                          07NOV00.1837
      REAL SCR3(MIX,MKX),SCR4(MIX,MKX),DUM1(MIX),                                05DEC01.988
     -     DENOMQ(MIX),DENOMQ2(MIX)                                              05DEC01.989
C                                                                                MRFPBL.96
C---- VARIABLES FOR MRFPBL                                                       MRFPBL.97
C                                                                                MRFPBL.98
      REAL      ZL1(MIX),XKZM(MIX,MKX),XKZH(MIX,MKX),                            07NOV00.1840
     1  AL(MIX,MKX-1),AD(MIX,MKX),AU(MIX,MKX-1),A1(MIX,MKX),A2(MIX,MKX),         MRFPBL.100
     2  THERMAL(MIX),WSCALE(MIX),HGAMT(MIX),HGAMQ(MIX),                          MRFPBL.101
     3  BRDN(MIX),BRUP(MIX),PHIM(MIX),PHIH(MIX),                                 MRFPBL.102
     4  DUSFC(MIX),DVSFC(MIX),DTSFC(MIX),DQSFC(MIX),PISFC(MIX),                  23DEC04.3463
     5  WSPD1(MIX),PBL0(MIX)                                                     23DEC04.3464
C---- FOR POLAR MODS                                                             23DEC04.3465
      REAL PSURF,ESG,HSFLXSI,HLFLXSI,HSFLXWT,HLFLXWT                             23DEC04.3466
      REAL QSSI(MIX),QSWT(MIX),THSI(MIX),THWT(MIX)                               23DEC04.3467
                                                                                 23DEC04.3468
C---- FOR NOAH LSM                                                               19DEC02.1716
      REAL      CHS(MIX),CHFF(MIX),PREC(MIX),chs2(mix)                           07NOV00.1842
      REAL      gz10oz0(mix),gz2oz0(mix),psih10(mix),psih2(mix),                 07NOV00.1843
     -     psim10(mix),psim2(mix),u10(mix),v10(mix),th2(mix)                     MRFPBL.107
     -     ,gz1oz0q(mix),gz2oz0q(mix),q2(mix),tx(mix,mkx)                        05DEC01.990
      LOGICAL PBLFLG(MIX),SFCFLG(MIX),STABLE(MIX)                                MRFPBL.109
C                                                                                MRFPBL.110
C                                                                                MRFPBL.111
C----------------------------------------------------------------------          MRFPBL.112
      REAL      CPM(MIX)                                                         07NOV00.1844
      REAL      TSLDSB(MIXS,MLX)                                                 07NOV00.1845
C                                                                                MRFPBL.115
      CONT=1000.*CP/G                                                            MRFPBL.116
      CONQ=1000.*XLV/G                                                           MRFPBL.117
      CONW=1000./G                                                               MRFPBL.118
C                                                                                MRFPBL.119
      IPP=2                                                                      MRFPBL.120
      ICUT=0                                                                     MRFPBL.121
      IF(IN.GT.1)ICUT=1                                                          MRFPBL.122
C                                                                                MRFPBL.123
C----------------------------------------------------------------------          MRFPBL.124
C                                                                                MRFPBL.125
C                                                                                MRFPBL.126
      DO I=IST,IEN                                                               07NOV00.1846
        DUSFC(I)=0.                                                              MRFPBL.128
        DVSFC(I)=0.                                                              MRFPBL.129
        DTSFC(I)=0.                                                              MRFPBL.130
        DQSFC(I)=0.                                                              MRFPBL.131
      ENDDO                                                                      MRFPBL.132
C                                                                                MRFPBL.133
C----CONVERT GROUND TEMPERATURE TO POTENTIAL TEMPERATURE:                        MRFPBL.134
C                                                                                MRFPBL.135
      DO 5 I=IST,IEN                                                             07NOV00.1847
        TGDSA(I)=TGB(I,J)                                                        MRFPBL.137
        PS(I)=PSB(I,J)+PTOP+PP3D(I,J,KL)*0.001                                   MRFPBL.138
        PISFC(I)=(PS(I)/100.)**ROVCP                                             07NOV00.1848
        THGB(I)=TGB(I,J)/PISFC(I)                                                07NOV00.1849
    5 CONTINUE                                                                   MRFPBL.141
C                                                                                MRFPBL.142
C-----DECOUPLE FLUX-FORM VARIABLES TO GIVE U,V,T,THETA,THETA-VIR.,               MRFPBL.143
C     T-VIR., QV, AND QC AT CROSS POINTS AND AT KTAU-1.                          MRFPBL.144
C                                                                                MRFPBL.145
C     *** NOTE ***                                                               MRFPBL.146
C         THE BOUNDARY WINDS MAY NOT BE ADEQUATELY AFFECTED BY FRICTION,         MRFPBL.147
C         SO USE ONLY INTERIOR VALUES OF UX AND VX TO CALCULATE                  MRFPBL.148
C         TENDENCIES.                                                            MRFPBL.149
C                                                                                MRFPBL.150
   10 CONTINUE                                                                   MRFPBL.151
C                                                                                MRFPBL.152
   22 CONTINUE                                                                   MRFPBL.153
      DO 24 K=1,KL                                                               MRFPBL.154
        DO 24 I=IST,IEN                                                          07NOV00.1850
#ifndef VECTOR                                                                   07NOV00.1851
          UX(I,K)=0.25*(U3D(I,J,K)+U3D(I+1,J,K)+U3D(I,J+1,K)+                    MRFPBL.156
     +            U3D(I+1,J+1,K))                                                MRFPBL.157
          VX(I,K)=0.25*(V3D(I,J,K)+V3D(I+1,J,K)+V3D(I,J+1,K)+                    MRFPBL.158
     +            V3D(I+1,J+1,K))                                                MRFPBL.159
#else                                                                            07NOV00.1852
C UX and VX are averaged in caller for vector version                            07NOV00.1853
          UX(I,K)=U3D(I,J,K)                                                     07NOV00.1854
          VX(I,K)=V3D(I,J,K)                                                     07NOV00.1855
#endif                                                                           07NOV00.1856
   24   CONTINUE                                                                 MRFPBL.160
                                                                                 MRFPBL.161
   26 CONTINUE                                                                   MRFPBL.162
C                                                                                MRFPBL.163
C.....SCR3(I,K) STORE TEMPERATURE,                                               MRFPBL.164
C     SCR4(I,K) STORE VIRTUAL TEMPERATURE.                                       MRFPBL.165
C                                                                                MRFPBL.166
      DO 30 K=1,KL                                                               MRFPBL.167
        DO 30 I=IST,IEN                                                          07NOV00.1857
          PL=A(K)*PSB(I,J)+PTOP+PP3D(I,J,K)*0.001                                MRFPBL.169
          SCR3(I,K)=T3D(I,J,K)                                                   MRFPBL.170
          THCON=(100./PL)**ROVCP                                                 MRFPBL.171
          THX(I,K)=SCR3(I,K)*THCON                                               MRFPBL.172
          TX(I,K)=SCR3(I,K)                                                      MRFPBL.173
          SCR4(I,K)=SCR3(I,K)                                                    MRFPBL.174
          THVX(I,K)=THX(I,K)                                                     MRFPBL.175
          QX(I,K)=0.                                                             MRFPBL.176
   30   CONTINUE                                                                 MRFPBL.177
                                                                                 MRFPBL.178
C                                                                                MRFPBL.179
      DO 40 I=IST,IEN                                                            07NOV00.1858
        QGH(I)=0.                                                                MRFPBL.181
        FLHC(I)=0.                                                               MRFPBL.182
        FLQC(I)=0.                                                               MRFPBL.183
        CPM(I)=CP                                                                MRFPBL.184
   40 CONTINUE                                                                   MRFPBL.185
C                                                                                MRFPBL.186
      IF(IDRY(IN).EQ.1)GOTO 80                                                   MRFPBL.187
      DO 50 K=1,KL                                                               MRFPBL.188
        DO 50 I=IST,IEN                                                          07NOV00.1859
          QX(I,K)=QV3D(I,J,K)                                                    MRFPBL.190
          TVCON=(1.+XMOIST(IN)*EP1*QX(I,K))                                      MRFPBL.191
          THVX(I,K)=THX(I,K)*TVCON                                               MRFPBL.192
          SCR4(I,K)=SCR3(I,K)*TVCON                                              MRFPBL.193
   50   CONTINUE                                                                 MRFPBL.194
                                                                                 MRFPBL.195
C                                                                                MRFPBL.196
      DO 60 I=IST,IEN                                                            07NOV00.1860
        E1=SVP1*EXP(SVP2*(TGDSA(I)-SVPT0)/(TGDSA(I)-SVP3))                       MRFPBL.198
        QGH(I)=EP2*E1/(PS(I)-E1)                                                 MRFPBL.199
        CPM(I)=CP*(1.+0.8*QX(I,KL)*XMOIST(IN))                                   MRFPBL.200
C                                                                                23DEC04.3469
C Adjust value of QS to account for ice surface (permanent or sea ice)           23DEC04.3470
C Calculate THSI and THWT for sea ice surface type                               23DEC04.3471
C                                                                                23DEC04.3472
        PSURF=PS(I)                                                              23DEC04.3473
        IF(NINT(SATBRT(I,J)).EQ.ISICE)THEN                                       23DEC04.3474
C Permanent ice surface type                                                     23DEC04.3475
          E1=0.611*EXP(22.514-6.15E3/TGDSA(I))                                   23DEC04.3476
          QGH(I)=0.622*E1/(PSURF-E1)                                             23DEC04.3477
        ELSEIF(SEAICE(I,J).GT.0.02)THEN                                          23DEC04.3478
C Sea ice surface type                                                           23DEC04.3479
          E1=SVP1*EXP(SVP2*(271.4-SVPT0)/(271.4-SVP3))                           23DEC04.3480
c         write(6,*) 'i = ',i,' j = ',j,' tgsi = ',tgsi(i,j),' in = ',in         23DEC04.3481
          ESG=0.611*EXP(22.514-6.15E3/TGSI(I,J))                                 23DEC04.3482
          QGH(I)=EP2*E1/(PSURF-E1)*(1.-SEAICE(I,J))+                             23DEC04.3483
     +       0.622*ESG/(PSURF-ESG)*SEAICE(I,J)                                   23DEC04.3484
          QSSI(I)=0.622*ESG/(PSURF-ESG)                                          23DEC04.3485
          QSWT(I)=EP2*E1/(PSURF-E1)                                              23DEC04.3486
C                                                                                23DEC04.3487
C Calculate potential temperature of sea ice surface                             23DEC04.3488
C                                                                                23DEC04.3489
          THSI(I)=TGSI(I,J)/PISFC(I)                                             23DEC04.3490
          THWT(I)=271.4/PISFC(I)                                                 23DEC04.3491
        ENDIF                                                                    23DEC04.3492
   60 CONTINUE                                                                   MRFPBL.201
C                                                                                MRFPBL.202
      IF(IMOIST(IN).EQ.1)GOTO 80                                                 MRFPBL.203
      DO 70 K=1,KL                                                               MRFPBL.204
        DO 70 I=IST,IEN                                                          07NOV00.1861
          QCX(I,K)=QC3D(I,J,K)                                                   MRFPBL.206
          QIX(I,K)=0.                                                            MRFPBL.207
          IF(IICE.EQ.1)QIX(I,K)=QI3D(I,J,K)                                      MRFPBL.208
          IF(IICEG.EQ.1)THEN                                                     MRFPBL.209
            QNCX(I,K)=QNC3D(I,J,K)                                               MRFPBL.210
          ENDIF                                                                  MRFPBL.211
   70   CONTINUE                                                                 MRFPBL.212
                                                                                 MRFPBL.213
   80 CONTINUE                                                                   MRFPBL.214
C                                                                                MRFPBL.215
C-----COMPUTE THE HEIGHT OF FULL- AND HALF-SIGMA LEVELS ABOVE GROUND             MRFPBL.216
C     LEVEL, AND THE LAYER THICKNESSES.                                          MRFPBL.217
C                                                                                MRFPBL.218
      DO 90 I=IST,IEN                                                            07NOV00.1862
        ZQ(I,KLP1)=0.                                                            MRFPBL.220
        RHOX(I)=PS(I)*1000./(R*SCR4(I,KL))                                       MRFPBL.221
   90 CONTINUE                                                                   MRFPBL.222
C                                                                                MRFPBL.223
      DO 110 KK=1,KL                                                             MRFPBL.224
        K=KLP1-KK                                                                MRFPBL.225
C                                                                                MRFPBL.226
        DO 100 I=IST,IEN                                                         07NOV00.1863
          DUM1(I)=ZQ(I,K+1)                                                      MRFPBL.228
  100   CONTINUE                                                                 MRFPBL.229
C                                                                                MRFPBL.230
        DO 110 I=IST,IEN                                                         07NOV00.1864
          CELL=PTOP*RPSB(I,J)                                                    MRFPBL.232
          ZQ(I,K)=ROVG*T0(I,J,K)*ALOG((SIGMA(K+1)+CELL)/(SIGMA(K)+CELL))         MRFPBL.233
     +            +DUM1(I)                                                       MRFPBL.234
  110   CONTINUE                                                                 MRFPBL.235
                                                                                 MRFPBL.236
C                                                                                MRFPBL.237
      DO 120 K=1,KL                                                              MRFPBL.238
        DO 120 I=IST,IEN                                                         07NOV00.1865
          ZA(I,K)=0.5*(ZQ(I,K)+ZQ(I,K+1))                                        MRFPBL.240
          DZQ(I,K)=ZQ(I,K)-ZQ(I,K+1)                                             MRFPBL.241
  120   CONTINUE                                                                 MRFPBL.242
                                                                                 MRFPBL.243
C                                                                                MRFPBL.244
      DO 130 K=1,KLM                                                             MRFPBL.245
        DO 130 I=IST,IEN                                                         07NOV00.1866
          DZA(I,K)=ZA(I,K)-ZA(I,K+1)                                             MRFPBL.247
  130   CONTINUE                                                                 MRFPBL.248
                                                                                 MRFPBL.249
      DTSTEP=DT                                                                  MRFPBL.250
C                                                                                MRFPBL.251
      DO 160 I=IST,IEN                                                           07NOV00.1867
        GOVRTH(I)=G/THX(I,KL)                                                    MRFPBL.253
  160 CONTINUE                                                                   MRFPBL.254
C                                                                                MRFPBL.255
      DO 170 I=IST,IEN                                                           07NOV00.1868
        TGDSB(I)=TGB(I,J)                                                        MRFPBL.257
        IF(ISOIL.EQ.1)THEN                                                       MRFPBL.258
          DO L=1,5                                                               MRFPBL.259
            TSLDSB(I,L)=TSLB(I,J,L)                                              MRFPBL.260
          ENDDO                                                                  MRFPBL.261
        ENDIF                                                                    MRFPBL.262
  170 CONTINUE                                                                   MRFPBL.263
C                                                                                MRFPBL.264
C                                                                                MRFPBL.265
C-----INITIALIZE VERTICAL TENDENCIES AND                                         MRFPBL.266
C                                                                                MRFPBL.267
      DO 220 K=1,KL                                                              MRFPBL.268
        DO 220 I=IST,IEN                                                         07NOV00.1869
          UTNP(I,K)=0.                                                           MRFPBL.270
          VTNP(I,K)=0.                                                           MRFPBL.271
          TTNP(I,K)=0.                                                           MRFPBL.272
  220   CONTINUE                                                                 MRFPBL.273
                                                                                 MRFPBL.274
C                                                                                MRFPBL.275
      IF(IDRY(IN).EQ.1)GOTO 250                                                  MRFPBL.276
      DO 230 K=1,KL                                                              MRFPBL.277
        DO 230 I=IST,IEN                                                         07NOV00.1870
          QTNP(I,K)=0.                                                           MRFPBL.279
  230   CONTINUE                                                                 MRFPBL.280
                                                                                 MRFPBL.281
C                                                                                MRFPBL.282
      IF(IMOIST(IN).EQ.1)GOTO 250                                                MRFPBL.283
      DO 240 K=1,KL                                                              MRFPBL.284
        DO 240 I=IST,IEN                                                         07NOV00.1871
          QCTNP(I,K)=0.                                                          MRFPBL.286
          QITNP(I,K)=0.                                                          MRFPBL.287
          IF(IICEG.EQ.1)THEN                                                     MRFPBL.288
            QNCNP(I,K)=0.                                                        MRFPBL.289
          ENDIF                                                                  MRFPBL.290
  240   CONTINUE                                                                 MRFPBL.291
                                                                                 MRFPBL.292
  250 CONTINUE                                                                   MRFPBL.293
C                                                                                MRFPBL.294
C-----CALCULATE BULK RICHARDSON NO. OF SURFACE LAYER, ACCORDING TO               MRFPBL.295
C     AKB(1976), EQ(12).                                                         MRFPBL.296
C                                                                                MRFPBL.297
      DO 260 I=IST,IEN                                                           07NOV00.1872
        GZ1OZ0(I)=ALOG(ZA(I,KL)/ZNT(I,J))                                        MRFPBL.299
        GZ10OZ0(I)=ALOG(10./ZNT(I,J))                                            MRFPBL.300
        GZ2OZ0(I)=ALOG(2./ZNT(I,J))                                              MRFPBL.301
        IF(IZ0TOPT.EQ.0)THEN                                                     05DEC01.991
C CARLSON-BOLAND FORMULA                                                         05DEC01.992
          IF((XLAND(I,J)-1.5).GE.0)THEN                                          05DEC01.993
            ZL=ZNT(I,J)                                                          05DEC01.994
          ELSE                                                                   05DEC01.995
            ZL=0.01                                                              05DEC01.996
          ENDIF                                                                  05DEC01.997
          GZ1OZ0Q(I)=ALOG(KARMAN*UST(I,J)*ZA(I,KL)/XKA+ZA(I,KL)/ZL)              05DEC01.998
          GZ2OZ0Q(I)=ALOG(KARMAN*UST(I,J)*2./XKA+2./ZL)                          05DEC01.999
        ENDIF                                                                    05DEC01.1000
        WSPD(I)=SQRT(UX(I,KL)*UX(I,KL)+VX(I,KL)*VX(I,KL))                        MRFPBL.308
        TGBV=THGB(I)*(1.+XMOIST(IN)*EP1*QGH(I)*MAVAIL(I,J))                      07NOV00.1873
        DTHVDZ=(THVX(I,KL)-TGBV)                                                 MRFPBL.310
C THIS WAS USED UP TO VERSION 3.6                                                23DEC04.3493
C       IF(-DTHVDZ.GE.0)THEN                                                     23DEC04.3494
C         DTHVM=-DTHVDZ                                                          23DEC04.3495
C       ELSE                                                                     23DEC04.3496
C         DTHVM=0.                                                               23DEC04.3497
C       ENDIF                                                                    23DEC04.3498
C       VCONV=VCONVC*SQRT(DTHVM)                                                 23DEC04.3499
C       WSPD(I)=SQRT(WSPD(I)*WSPD(I)+VCONV*VCONV)                                23DEC04.3500
C  New in 3.7                                                                    23DEC04.3501
C  Convective velocity scale Vc and subgrid-scale velocity Vsg                   23DEC04.3502
C  following Beljaars (1995, QJRMS) and Mahrt and Sun (1995, MWR)                23DEC04.3503
C  from changes by Y.Liu and S.-Y. Hong                                          23DEC04.3504
C                                                                                23DEC04.3505
        fluxc = max(hfx(i,j)/rhox(i)/cpm(i)                                      23DEC04.3506
     -       + ep1*thvx(i,kl)*qfx(i,j)/rhox(i),0.)                               23DEC04.3507
        VCONV = 1.0*(g/thvx(i,kl)*pbl(i,j)*fluxc)**.33                           23DEC04.3508
C VCONV comes from Beljaars only                                                 23DEC04.3509
C VSGD would require passing in DX. Not used here.                               23DEC04.3510
C       VSGD = 0.32 * (max(dx/5000.-1.,0.))**.33                                 23DEC04.3511
        VSGD = 0.                                                                23DEC04.3512
        WSPD(I)=SQRT(WSPD(I)*WSPD(I)+VCONV*VCONV+vsgd*vsgd)                      23DEC04.3513
        WSPD(I)=AMAX1(WSPD(I),1.)                                                MRFPBL.318
        BR(I)=GOVRTH(I)*ZA(I,KL)*DTHVDZ/(WSPD(I)*WSPD(I))                        MRFPBL.319
  260 CONTINUE                                                                   MRFPBL.320
C                                                                                MRFPBL.321
C-----DIAGNOSE BASIC PARAMETERS FOR THE APPROPRIATED STABILITY CLASS:            MRFPBL.322
C                                                                                MRFPBL.323
C                                                                                MRFPBL.324
C     THE STABILITY CLASSES ARE DETERMINED BY BR (BULK RICHARDSON NO.)           MRFPBL.325
C     AND HOL (HEIGHT OF PBL/MONIN-OBUKHOV LENGTH).                              MRFPBL.326
C                                                                                MRFPBL.327
C     CRITERIA FOR THE CLASSES ARE AS FOLLOWS:                                   MRFPBL.328
C                                                                                MRFPBL.329
C        1. BR .GE. 0.2;                                                         MRFPBL.330
C               REPRESENTS NIGHTTIME STABLE CONDITIONS (REGIME=1),               MRFPBL.331
C                                                                                MRFPBL.332
C        2. BR .LT. 0.2 .AND. BR .GT. 0.0;                                       MRFPBL.333
C               REPRESENTS DAMPED MECHANICAL TURBULENT CONDITIONS                MRFPBL.334
C               (REGIME=2),                                                      MRFPBL.335
C                                                                                MRFPBL.336
C        3. BR .EQ. 0.0                                                          MRFPBL.337
C               REPRESENTS FORCED CONVECTION CONDITIONS (REGIME=3),              MRFPBL.338
C                                                                                MRFPBL.339
C        4. BR .LT. 0.0                                                          MRFPBL.340
C               REPRESENTS FREE CONVECTION CONDITIONS (REGIME=4).                MRFPBL.341
C                                                                                MRFPBL.342
CCCCCC                                                                           MRFPBL.343
      DO 320 I=IST,IEN                                                           07NOV00.1874
CCCCCC                                                                           MRFPBL.345
CCC     REMOVE REGIME 3 DEPENDENCE ON PBL HEIGHT                                 MRFPBL.346
CCC          IF(BR(I).LT.0..AND.HOL(I,J).GT.1.5)GOTO 310                         MRFPBL.347
        IF(BR(I).LT.0.)GOTO 310                                                  MRFPBL.348
C                                                                                MRFPBL.349
C-----CLASS 1; STABLE (NIGHTTIME) CONDITIONS:                                    MRFPBL.350
C                                                                                MRFPBL.351
        IF(BR(I).LT.0.2)GOTO 270                                                 MRFPBL.352
        REGIME(I,J)=1.                                                           MRFPBL.353
        PSIM(I)=-10.*GZ1OZ0(I)                                                   MRFPBL.354
C    LOWER LIMIT ON PSI IN STABLE CONDITIONS                                     MRFPBL.355
        PSIM(I)=AMAX1(PSIM(I),-10.)                                              MRFPBL.356
        PSIH(I)=PSIM(I)                                                          MRFPBL.357
        IF(ISFMTHD.EQ.0)THEN                                                     23DEC04.3514
          PSIM10(I)=-10.*GZ10OZ0(I)                                              23DEC04.3515
        ELSE                                                                     23DEC04.3516
          PSIM10(I)=10./ZA(I,KL)*PSIM(I)                                         23DEC04.3517
        ENDIF                                                                    23DEC04.3518
        PSIM10(I)=AMAX1(PSIM10(I),-10.)                                          MRFPBL.359
        PSIH10(I)=PSIM10(I)                                                      MRFPBL.360
        IF(ISFMTHD.EQ.0)THEN                                                     23DEC04.3519
          PSIM2(I)=-10.*GZ2OZ0(I)                                                23DEC04.3520
        ELSE                                                                     23DEC04.3521
          PSIM2(I)=2./ZA(I,KL)*PSIM(I)                                           23DEC04.3522
        ENDIF                                                                    23DEC04.3523
        PSIM2(I)=AMAX1(PSIM2(I),-10.)                                            MRFPBL.362
        PSIH2(I)=PSIM2(I)                                                        MRFPBL.363
        HOL(I,J)=0.0                                                             MRFPBL.364
        PBL(I,J)=0.0                                                             MRFPBL.365
        GOTO 320                                                                 MRFPBL.366
C                                                                                MRFPBL.367
C-----CLASS 2; DAMPED MECHANICAL TURBULENCE:                                     MRFPBL.368
C                                                                                MRFPBL.369
  270   IF(BR(I).EQ.0.0)GOTO 280                                                 MRFPBL.370
        REGIME(I,J)=2.                                                           MRFPBL.371
        PSIM(I)=-5.0*BR(I)*GZ1OZ0(I)/(1.1-5.0*BR(I))                             MRFPBL.372
C    LOWER LIMIT ON PSI IN STABLE CONDITIONS                                     MRFPBL.373
        PSIM(I)=AMAX1(PSIM(I),-10.)                                              MRFPBL.374
C.....AKB(1976), EQ(16).                                                         MRFPBL.375
        PSIH(I)=PSIM(I)                                                          MRFPBL.376
        IF(ISFMTHD.EQ.0)THEN                                                     23DEC04.3524
          PSIM10(I)=-5.0*BR(I)*GZ10OZ0(I)/(1.1-5.0*BR(I))                        23DEC04.3525
        ELSE                                                                     23DEC04.3526
          PSIM10(I)=10./ZA(I,KL)*PSIM(I)                                         23DEC04.3527
        ENDIF                                                                    23DEC04.3528
        PSIM10(I)=AMAX1(PSIM10(I),-10.)                                          MRFPBL.378
        PSIH10(I)=PSIM10(I)                                                      MRFPBL.379
        IF(ISFMTHD.EQ.0)THEN                                                     23DEC04.3529
          PSIM2(I)=-5.0*BR(I)*GZ2OZ0(I)/(1.1-5.0*BR(I))                          23DEC04.3530
        ELSE                                                                     23DEC04.3531
          PSIM2(I)=2./ZA(I,KL)*PSIM(I)                                           23DEC04.3532
        ENDIF                                                                    23DEC04.3533
        PSIM2(I)=AMAX1(PSIM2(I),-10.)                                            MRFPBL.381
        PSIH2(I)=PSIM2(I)                                                        MRFPBL.382
        HOL(I,J)=0.0                                                             MRFPBL.383
        PBL(I,J)=0.0                                                             MRFPBL.384
        GOTO 320                                                                 MRFPBL.385
C                                                                                MRFPBL.386
C-----CLASS 3; FORCED CONVECTION:                                                MRFPBL.387
C                                                                                MRFPBL.388
  280   REGIME(I,J)=3.                                                           MRFPBL.389
        PSIM(I)=0.0                                                              MRFPBL.390
        PSIH(I)=PSIM(I)                                                          MRFPBL.391
        PSIM10(I)=0.                                                             MRFPBL.392
        PSIH10(I)=PSIM10(I)                                                      MRFPBL.393
        PSIM2(I)=0.                                                              MRFPBL.394
        PSIH2(I)=PSIM2(I)                                                        MRFPBL.395
C                                                                                MRFPBL.396
        DO 290 KK=1,KLM                                                          MRFPBL.397
          K=KL-KK                                                                MRFPBL.398
          IF(THVX(I,K).GT.THVX(I,KL))GOTO 300                                    MRFPBL.399
  290   CONTINUE                                                                 MRFPBL.400
        STOP 290                                                                 MRFPBL.401
  300   PBL(I,J)=ZQ(I,K+1)                                                       MRFPBL.402
        IF(UST(I,J).LT.0.01)THEN                                                 MRFPBL.403
          ZOL(I,J)=BR(I)*GZ1OZ0(I)                                               MRFPBL.404
        ELSE                                                                     MRFPBL.405
          ZOL(I,J)=KARMAN*GOVRTH(I)*ZA(I,KL)*MOL(I,J)/(UST(I,J)*UST(I,J)         MRFPBL.406
     +             )                                                             MRFPBL.407
        ENDIF                                                                    MRFPBL.408
        HOL(I,J)=-ZOL(I,J)*PBL(I,J)/ZA(I,KL)                                     MRFPBL.409
        GOTO 320                                                                 MRFPBL.410
C                                                                                MRFPBL.411
C-----CLASS 4; FREE CONVECTION:                                                  MRFPBL.412
C                                                                                MRFPBL.413
C 310     IF(THVX(I,KLM).GT.THVX(I,KL))GOTO 280                                  MRFPBL.414
  310   CONTINUE                                                                 MRFPBL.415
        REGIME(I,J)=4.                                                           MRFPBL.416
        IF(UST(I,J).LT.0.01)THEN                                                 MRFPBL.417
          ZOL(I,J)=BR(I)*GZ1OZ0(I)                                               MRFPBL.418
        ELSE                                                                     MRFPBL.419
          ZOL(I,J)=KARMAN*GOVRTH(I)*ZA(I,KL)*MOL(I,J)/(UST(I,J)*UST(I,J)         MRFPBL.420
     +             )                                                             MRFPBL.421
        ENDIF                                                                    MRFPBL.422
        ZOL10=10./ZA(I,KL)*ZOL(I,J)                                              MRFPBL.423
        ZOL2=2./ZA(I,KL)*ZOL(I,J)                                                MRFPBL.424
        ZOL(I,J)=AMIN1(ZOL(I,J),0.)                                              MRFPBL.425
        ZOL(I,J)=AMAX1(ZOL(I,J),-9.9999)                                         MRFPBL.426
        ZOL10=AMIN1(ZOL10,0.)                                                    MRFPBL.427
        ZOL10=AMAX1(ZOL10,-9.9999)                                               MRFPBL.428
        ZOL2=AMIN1(ZOL2,0.)                                                      MRFPBL.429
        ZOL2=AMAX1(ZOL2,-9.9999)                                                 MRFPBL.430
        NZOL=INT(-ZOL(I,J)*100.)                                                 MRFPBL.431
        RZOL=-ZOL(I,J)*100.-NZOL                                                 MRFPBL.432
        NZOL10=INT(-ZOL10*100.)                                                  MRFPBL.433
        RZOL10=-ZOL10*100.-NZOL10                                                MRFPBL.434
        NZOL2=INT(-ZOL2*100.)                                                    MRFPBL.435
        RZOL2=-ZOL2*100.-NZOL2                                                   MRFPBL.436
        PSIM(I)=PSIMTB(NZOL)+RZOL*(PSIMTB(NZOL+1)-PSIMTB(NZOL))                  MRFPBL.437
        PSIH(I)=PSIHTB(NZOL)+RZOL*(PSIHTB(NZOL+1)-PSIHTB(NZOL))                  MRFPBL.438
        PSIM10(I)=PSIMTB(NZOL10)+RZOL10*(PSIMTB(NZOL10+1)-PSIMTB(NZOL10)         MRFPBL.439
     +            )                                                              MRFPBL.440
        PSIH10(I)=PSIHTB(NZOL10)+RZOL10*(PSIHTB(NZOL10+1)-PSIHTB(NZOL10)         MRFPBL.441
     +            )                                                              MRFPBL.442
        PSIM2(I)=PSIMTB(NZOL2)+RZOL2*(PSIMTB(NZOL2+1)-PSIMTB(NZOL2))             MRFPBL.443
        PSIH2(I)=PSIHTB(NZOL2)+RZOL2*(PSIHTB(NZOL2+1)-PSIHTB(NZOL2))             MRFPBL.444
C---LIMIT PSIH AND PSIM IN THE CASE OF THIN LAYERS AND HIGH ROUGHNESS            MRFPBL.445
C---  THIS PREVENTS DENOMINATOR IN FLUXES FROM GETTING TOO SMALL                 MRFPBL.446
        PSIH(I)=AMIN1(PSIH(I),0.9*GZ1OZ0(I))                                     MRFPBL.447
        PSIM(I)=AMIN1(PSIM(I),0.9*GZ1OZ0(I))                                     MRFPBL.448
  320 CONTINUE                                                                   MRFPBL.449
C                                                                                MRFPBL.450
C-----COMPUTE THE FRICTIONAL VELOCITY:                                           MRFPBL.451
C     ZA(1982) EQS(2.60),(2.61).                                                 MRFPBL.452
C                                                                                MRFPBL.453
      DO 330 I=IST,IEN                                                           07NOV00.1875
        DTG=THX(I,KL)-THGB(I)                                                    07NOV00.1876
        PSIX=GZ1OZ0(I)-PSIM(I)                                                   MRFPBL.456
        PSIX10=GZ10OZ0(I)-PSIM10(I)                                              MRFPBL.457
        UST(I,J)=KARMAN*WSPD(I)/PSIX                                             MRFPBL.467
        IF((XLAND(I,J)-1.5).LE.0.)THEN                                           05DEC01.1001
C LAND: LOWER LIMIT ON USTAR                                                     05DEC01.1002
          UST(I,J)=AMAX1(UST(I,J),0.1)                                           05DEC01.1003
        ELSE                                                                     05DEC01.1004
C WATER: ROUGHNESS LENGTH DEPENDS ON USTAR                                       05DEC01.1005
          ZNT(I,J)=CZO*UST(I,J)*UST(I,J)/G+OZO                                   05DEC01.1006
        ENDIF                                                                    05DEC01.1007
        IF(IZ0TOPT.EQ.0)THEN                                                     05DEC01.1008
C CARLSON-BOLAND FORMULA                                                         05DEC01.1009
C     LOWER LIMIT ADDED TO PREVENT LARGE FLHC IN SOIL MODEL                      19DEC02.1717
C     ACTIVATES IN UNSTABLE CONDITIONS WITH THIN LAYERS OR HIGH Z0               19DEC02.1718
          PSIT=AMAX1(GZ1OZ0(I)-PSIH(I),2.)                                       19DEC02.1719
          PSIQ=GZ1OZ0Q(I)-PSIH(I)                                                05DEC01.1011
          PSIT2=GZ2OZ0(I)-PSIH2(I)                                               05DEC01.1012
          PSIQ2=GZ2OZ0Q(I)-PSIH2(I)                                              05DEC01.1013
        ELSE IF(IZ0TOPT.EQ.1)THEN                                                05DEC01.1014
C GARRATT FORMULA                                                                05DEC01.1015
         IF((XLAND(I,J)-1.5).GE.0)THEN                                           05DEC01.1016
C WATER: CALCULATE ROUGHNESS REYNOLDS NUMBER                                     05DEC01.1017
C        KINEMATIC VISCOSITY OF AIR (LINEAR APPROX TO                            05DEC01.1018
C                 TEMP DEPENDENCE AT SEA LEVEL)                                  05DEC01.1019
            VISC=(1.32+0.009*(TX(I,KL)-273.15))*1.E-5                            05DEC01.1020
            RESTAR=UST(I,J)*ZNT(I,J)/VISC                                        05DEC01.1021
            PSIT=GZ1OZ0(I)-PSIH(I)+2.48*SQRT(SQRT(RESTAR))-2.                    05DEC01.1022
            PSIQ=GZ1OZ0(I)-PSIH(I)+2.28*SQRT(SQRT(RESTAR))-2.                    05DEC01.1023
            PSIT2=GZ2OZ0(I)-PSIH2(I)+2.48*SQRT(SQRT(RESTAR))-2.                  05DEC01.1024
            PSIQ2=GZ2OZ0(I)-PSIH2(I)+2.28*SQRT(SQRT(RESTAR))-2.                  05DEC01.1025
          ELSE                                                                   05DEC01.1026
C LAND                                                                           05DEC01.1027
            PSIT=GZ1OZ0(I)-PSIH(I)+2.                                            05DEC01.1028
            PSIQ=GZ1OZ0(I)-PSIH(I)+2.                                            05DEC01.1029
            PSIT2=GZ2OZ0(I)-PSIH2(I)+2.                                          05DEC01.1030
            PSIQ2=GZ2OZ0(I)-PSIH2(I)+2.                                          05DEC01.1031
          ENDIF                                                                  05DEC01.1032
        ELSE IF(IZ0TOPT.EQ.2)THEN                                                05DEC01.1033
          IF((XLAND(I,J)-1.5).GE.0)THEN                                          05DEC01.1034
C WATER (Z0T=Z0)                                                                 05DEC01.1035
            PSIT=GZ1OZ0(I)-PSIH(I)                                               05DEC01.1036
            PSIQ=GZ1OZ0(I)-PSIH(I)                                               05DEC01.1037
            PSIT2=GZ2OZ0(I)-PSIH2(I)                                             05DEC01.1038
            PSIQ2=GZ2OZ0(I)-PSIH2(I)                                             05DEC01.1039
          ELSE                                                                   05DEC01.1040
C LAND ZILITINKEVICH FORMULA                                                     05DEC01.1041
C        KINEMATIC VISCOSITY OF AIR (LINEAR APPROX TO                            05DEC01.1042
C                 TEMP DEPENDENCE AT SEA LEVEL)                                  05DEC01.1043
            VISC=(1.32+0.009*(SCR3(I,KL)-273.15))*1.E-5                          05DEC01.1044
            RESTAR=UST(I,J)*ZNT(I,J)/VISC                                        05DEC01.1045
            PSIT=GZ1OZ0(I)-PSIH(I)+0.1*KARMAN*SQRT(RESTAR)                       05DEC01.1046
            PSIQ=GZ1OZ0(I)-PSIH(I)+0.1*KARMAN*SQRT(RESTAR)                       05DEC01.1047
            PSIT2=GZ2OZ0(I)-PSIH2(I)+0.1*KARMAN*SQRT(RESTAR)                     05DEC01.1048
            PSIQ2=GZ2OZ0(I)-PSIH2(I)+0.1*KARMAN*SQRT(RESTAR)                     05DEC01.1049
          ENDIF                                                                  05DEC01.1050
        ENDIF                                                                    05DEC01.1051
        U10(I)=UX(I,KL)*PSIX10/PSIX                                              MRFPBL.468
        V10(I)=VX(I,KL)*PSIX10/PSIX                                              MRFPBL.469
        TH2(I)=THGB(I)+DTG*PSIT2/PSIT                                            07NOV00.1877
        Q2(I)=QGH(I)+(QX(I,KL)-QGH(I))*PSIQ2/PSIQ                                MRFPBL.471
C       LATER Q2 WILL BE OVERWRITTEN FOR LAND POINTS IN SURFCE                   07NOV00.1878
        TA2(I,J) = TH2(I)*(PS(I)/100.)**ROVCP                                    07NOV00.1879
        QA2(I,J) = Q2(I)                                                         07NOV00.1880
        UA10(I,J) = U10(I)                                                       07NOV00.1881
        VA10(I,J) = V10(I)                                                       07NOV00.1882
C                                                                                MRFPBL.472
        MOL(I,J)=KARMAN*DTG/PSIT/PRT                                             05DEC01.1052
        DENOMQ(I)=PSIQ                                                           05DEC01.1053
        DENOMQ2(I)=PSIQ2                                                         05DEC01.1054
  330 CONTINUE                                                                   MRFPBL.480
C                                                                                MRFPBL.481
  335 CONTINUE                                                                   MRFPBL.482
C                                                                                MRFPBL.483
C-----COMPUTE RADIATION AND SOIL THERMAL CAPACITY:                               MRFPBL.484
C                                                                                MRFPBL.485
      IF(MOD(KTAU,NTRAD(IN)).EQ.0)CALL SFCRAD(IYY,JXX,J,IN,T3D,QV3D,PP3D         MRFPBL.486
#ifdef VECTOR                                                                    07NOV00.1883
     +        ,SVP1,SVP2,SVP3,SVPT0,EP2,                                         07NOV00.1884
     +         PSB,GLW,GSW,CAPG,MAVAIL,XLAT,XLONG,                               07NOV00.1885
     +         PRW,ALB,SHC,THC,                                                  07NOV00.1886
     +         IST,IEN                                                           07NOV00.1887
#endif                                                                           07NOV00.1888
     +  )                                                                        MRFPBL.487
C                                                                                MRFPBL.488
C-----COMPUTE THE SURFACE SENSIBLE AND LATENT HEAT FLUXES:                       MRFPBL.489
C                                                                                MRFPBL.490
      DO 350 I=IST,IEN                                                           07NOV00.1889
        QFX(I,J)=0.                                                              MRFPBL.492
        HFX(I,J)=0.                                                              MRFPBL.493
        QFXSI(I,J)=0.                                                            23DEC04.3534
        HFXSI(I,J)=0.                                                            23DEC04.3535
  350 CONTINUE                                                                   MRFPBL.494
C                                                                                MRFPBL.495
C                                                                                MRFPBL.496
      IF(ISFFLX(IN).EQ.0)GOTO 410                                                MRFPBL.497
C                                                                                MRFPBL.498
      DO 360 I=IST,IEN                                                           07NOV00.1890
        FLQC(I)=RHOX(I)*MAVAIL(I,J)*UST(I,J)*KARMAN/DENOMQ(I)                    05DEC01.1055
        DTTHX=ABS(THX(I,KL)-THGB(I))                                             07NOV00.1891
        IF(DTTHX.GT.1.E-5)THEN                                                   MRFPBL.513
          FLHC(I)=CPM(I)*RHOX(I)*UST(I,J)*MOL(I,J)/(THX(I,KL)-THGB(I))           07NOV00.1892
        ELSE                                                                     MRFPBL.515
          FLHC(I)=0.                                                             MRFPBL.516
        ENDIF                                                                    MRFPBL.517
C     FLHC(I)=CPM(I)*RHOX(I)*UST(I,J)*MOL(I,J)/(THX(I,KL)-THGB(I))               07NOV00.1893
  360 CONTINUE                                                                   MRFPBL.519
      IF(ISOIL.LE.1)THEN                                                         MRFPBL.520
#ifdef SOIL1                                                                     MRFPBL.521
C                                                                                MRFPBL.522
C-----COMPUTE SURFACE MOIST FLUX:                                                MRFPBL.523
C                                                                                MRFPBL.524
        IF(IDRY(IN).EQ.1)GOTO 390                                                MRFPBL.525
C                                                                                MRFPBL.526
        DO 370 I=IST,IEN                                                         07NOV00.1894
          QFX(I,J)=FLQC(I)*(QGH(I)-QX(I,KL))                                     MRFPBL.528
          IF(SEAICE(I,J).GT.0.02)THEN                                            23DEC04.3536
            HLFLXSI=FLQC(I)*(QSSI(I)-QX(I,KL))                                   23DEC04.3537
            HLFLXSI=AMAX1(HLFLXSI,0.)                                            23DEC04.3538
            HLFLXWT=FLQC(I)*(QSWT(I)-QX(I,KL))                                   23DEC04.3539
            HLFLXWT=AMAX1(HLFLXWT,0.)                                            23DEC04.3540
            QFX(I,J)=HLFLXSI*SEAICE(I,J)+HLFLXWT*(1.-SEAICE(I,J))                23DEC04.3541
            QFXSI(I,J)=HLFLXSI                                                   23DEC04.3542
          ENDIF                                                                  23DEC04.3543
          QFX(I,J)=AMAX1(QFX(I,J),0.)                                            MRFPBL.529
#ifndef VECTOR                                                                   07NOV00.1895
          PARJSUM(ITQEVA_SUM,J)=PARJSUM(ITQEVA_SUM,J)+QFX(I,J)*DX*DX*0.5         MRFPBL.530
     +                          *DTSTEP                                          MRFPBL.531
#endif                                                                           07NOV00.1896
C          TQEVA=TQEVA+QFX(I,J)*DX*DX*0.5*DTSTEP                                 MRFPBL.532
  370   CONTINUE                                                                 MRFPBL.533
C                                                                                MRFPBL.534
C-----COMPUTE SURFACE HEAT FLUX:                                                 MRFPBL.535
C                                                                                MRFPBL.536
  390   CONTINUE                                                                 MRFPBL.537
        DO 400 I=IST,IEN                                                         07NOV00.1897
          IF(XLAND(I,J)-1.5.GT.0.)THEN                                           MRFPBL.539
            HFX(I,J)=FLHC(I)*(THGB(I)-THX(I,KL))                                 07NOV00.1898
          ELSEIF(XLAND(I,J)-1.5.LT.0.)THEN                                       MRFPBL.541
            HFX(I,J)=FLHC(I)*(THGB(I)-THX(I,KL))                                 07NOV00.1899
            HFX(I,J)=AMAX1(HFX(I,J),-250.)                                       MRFPBL.543
          ENDIF                                                                  MRFPBL.544
          IF(SEAICE(I,J).GT.0.02)THEN                                            23DEC04.3544
            HSFLXSI=FLHC(I)*(THSI(I)-THX(I,KL))                                  23DEC04.3545
            HSFLXWT=FLHC(I)*(THWT(I)-THX(I,KL))                                  23DEC04.3546
            HFX(I,J)=HSFLXSI*SEAICE(I,J)+HSFLXWT*(1.-SEAICE(I,J))                23DEC04.3547
            HFXSI(I,J)=HSFLXSI                                                   23DEC04.3548
          ENDIF                                                                  23DEC04.3549
  400   CONTINUE                                                                 MRFPBL.545
C                                                                                MRFPBL.546
C-----COMPUTE THE GROUND TEMPERATURE:                                            MRFPBL.547
C                                                                                MRFPBL.548
        IF(ITGFLG(IN).NE.1)GOTO 430                                              MRFPBL.549
        DELTSM=DTSTEP                                                            MRFPBL.550
#ifndef VECTOR                                                                   07NOV00.1900
        CALL SLAB(DELTSM,J,IN,THX,QX,FLHC,FLQC,PP3D,T3D,IST,IEN)                 07NOV00.1901
#else                                                                            07NOV00.1902
        CALL SLAB(DELTSM,J,IN,THX,QX,FLHC,FLQC,PP3D,T3D,                         07NOV00.1903
     +            XLV,PPB,RPSB,TSLB,SVP1,SVP2,SVP3,SVPT0,EP2,                    07NOV00.1904
     +            XLAND,PSB,TGB,EMISS,GLW,GSW,TMN,HFX,QFX,CAPG,SNOWC,            07NOV00.1905
     +            SATBRT,RAINP,RAINC,RAINNC,THC,MAVAIL,ALB,                      05DEC01.1534
     +            XMOIST,IST,IEN )                                               07NOV00.1907
#endif                                                                           07NOV00.1908
  430   CONTINUE                                                                 MRFPBL.552
#endif                                                                           MRFPBL.553
      ELSEIF(ISOIL.EQ.2)THEN                                                     MRFPBL.554
#ifdef SOIL2                                                                     07NOV00.1909
#  ifndef VECTOR                                                                 07NOV00.1910
        DO 405 I=IST,IEN                                                         07NOV00.1911
c LSM is restricted to same exchange coeff for T and Q so use Q                  05DEC01.1056
          CHS(I)=UST(I,J)*KARMAN/DENOMQ(I)                                       05DEC01.1057
          CHS2(I)=UST(I,J)*KARMAN/DENOMQ2(I)                                     05DEC01.1058
          CHFF(I)=CPM(I)*RHOX(I)*CHS(I)                                          MRFPBL.576
C        PREC (KG/M^2/S) FROM RAINP (CM) ACCUMULATION IN ONE TIMESTEP            MRFPBL.577
          PREC(I)=RAINP(I,J)*10./(DT/2.)                                         MRFPBL.578
  405   CONTINUE                                                                 MRFPBL.579
        CALL SURFCE(J,ZA,QX,THX,TGB,THGB,TGDSA,CHS,CHFF,CPM,PREC,                07NOV00.1912
     +       HFX,QFX,QGH,                                                        07NOV00.1913
     +       GSW,GLW,SCR3,UX,VX,ALB,PP3D,PSB,RHOX,TMN,XLAND,QCX,IN,U10,          MRFPBL.581
     +       V10,TH2,Q2,RAINC,RAINNC,SNOWC,CHS2,CKLQ,ZNT,EMISS)                  23DEC04.3550
        DO 406 I=IST,IEN                                                         07NOV00.1914
          QA2(I,J) = Q2(I)                                                       07NOV00.1915
          TA2(I,J) = TH2(I)*(PS(I)/100.)**ROVCP                                  23DEC04.3551
  406   CONTINUE                                                                 07NOV00.1916
#  else                                                                          07NOV00.1917
  --- VECTOR OPTIMIZATION OF MRF NOT IMPLEMENTED FOR ISOIL=2 ---                 07NOV00.1918
  --- COMPILER ERROR GENERATED ---                                               07NOV00.1919
#  endif                                                                         07NOV00.1920
#endif                                                                           MRFPBL.583
      ENDIF                                                                      MRFPBL.584
C                                                                                MRFPBL.585
C-----COMPUTE SURFACE MOMENTUM FLUXES:                                           MRFPBL.586
C                                                                                MRFPBL.587
  410 CONTINUE                                                                   MRFPBL.588
      DO 420 I=IST,IEN                                                           07NOV00.1921
        WSPD1(I)=SQRT(UX(I,KL)*UX(I,KL)+VX(I,KL)*VX(I,KL))+1.E-9                 23DEC04.3552
  420 CONTINUE                                                                   MRFPBL.591
C                                                                                MRFPBL.592
C---- COMPUTE VERTICAL DIFFUSION                                                 MRFPBL.593
C                                                                                MRFPBL.594
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -          MRFPBL.595
C     COMPUTE PRELIMINARY VARIABLES                                              MRFPBL.596
C                                                                                MRFPBL.597
C                                                                                MRFPBL.598
      DT4=2.*DTSTEP                                                              MRFPBL.599
      RDT=1./DT4                                                                 MRFPBL.600
C                                                                                MRFPBL.601
C                                                                                MRFPBL.602
      DO I=IST,IEN                                                               07NOV00.1922
        HGAMT(I)=0.                                                              MRFPBL.604
        HGAMQ(I)=0.                                                              MRFPBL.605
        WSCALE(I)=0.                                                             MRFPBL.606
        KPBL(I)=KL                                                               MRFPBL.607
        PBL(I,J)=ZQ(I,KL)                                                        MRFPBL.608
        KPBL0(I)=KL                                                              23DEC04.3553
        PBL0(I)=ZQ(I,KL)                                                         23DEC04.3554
        PBLFLG(I)=.TRUE.                                                         MRFPBL.609
        SFCFLG(I)=.TRUE.                                                         MRFPBL.610
        IF(BR(I).GT.0.0)SFCFLG(I)=.FALSE.                                        MRFPBL.611
        ZL1(I)=ZA(I,KL)                                                          MRFPBL.612
        THERMAL(I)=THVX(I,KL)                                                    MRFPBL.613
      ENDDO                                                                      MRFPBL.614
C                                                                                MRFPBL.615
C                                                                                MRFPBL.616
C                                                                                MRFPBL.617
C     COMPUTE THE FIRST GUESS OF PBL HEIGHT                                      MRFPBL.618
C                                                                                MRFPBL.619
      DO I=IST,IEN                                                               07NOV00.1923
        STABLE(I)=.FALSE.                                                        MRFPBL.621
        BRUP(I)=BR(I)                                                            MRFPBL.622
      ENDDO                                                                      MRFPBL.623
      DO K=KLM,KLPBL,-1                                                          MRFPBL.624
        DO I=IST,IEN                                                             07NOV00.1924
          IF(.NOT.STABLE(I))THEN                                                 MRFPBL.626
            BRDN(I)=BRUP(I)                                                      MRFPBL.627
            SPDK2=MAX(UX(I,K)**2+VX(I,K)**2,1.)                                  MRFPBL.628
            BRUP(I)=(THVX(I,K)-THERMAL(I))*(G*ZA(I,K)/THVX(I,KL))/SPDK2          MRFPBL.629
            KPBL(I)=K                                                            MRFPBL.630
            STABLE(I)=BRUP(I).GT.BRCR                                            MRFPBL.631
          ENDIF                                                                  MRFPBL.632
        ENDDO                                                                    MRFPBL.633
      ENDDO                                                                      MRFPBL.634
C                                                                                MRFPBL.635
      DO I=IST,IEN                                                               07NOV00.1925
        K=KPBL(I)                                                                MRFPBL.637
        IF(BRDN(I).GE.BRCR)THEN                                                  MRFPBL.638
          BRINT=0.                                                               MRFPBL.639
        ELSEIF(BRUP(I).LE.BRCR)THEN                                              MRFPBL.640
          BRINT=1.                                                               MRFPBL.641
        ELSE                                                                     MRFPBL.642
          BRINT=(BRCR-BRDN(I))/(BRUP(I)-BRDN(I))                                 MRFPBL.643
        ENDIF                                                                    MRFPBL.644
        PBL(I,J)=ZA(I,K+1)+BRINT*(ZA(I,K)-ZA(I,K+1))                             MRFPBL.645
        IF(PBL(I,J).LT.ZQ(I,KPBL(I)+1))KPBL(I)=KPBL(I)+1                         MRFPBL.646
      ENDDO                                                                      MRFPBL.647
C                                                                                MRFPBL.648
      DO I=IST,IEN                                                               07NOV00.1926
        FM=GZ1OZ0(I)-PSIM(I)                                                     MRFPBL.650
        FH=GZ1OZ0(I)-PSIH(I)                                                     MRFPBL.651
        HOL(I,J)=MAX(BR(I)*FM*FM/FH,RIMIN)                                       MRFPBL.652
        IF(SFCFLG(I))THEN                                                        MRFPBL.653
          HOL(I,J)=MIN(HOL(I,J),-ZFMIN)                                          MRFPBL.654
        ELSE                                                                     MRFPBL.655
          HOL(I,J)=MAX(HOL(I,J),ZFMIN)                                           MRFPBL.656
        ENDIF                                                                    MRFPBL.657
C                                                                                MRFPBL.658
        HOL1=HOL(I,J)*PBL(I,J)/ZL1(I)*SFCFRAC                                    MRFPBL.659
        HOL(I,J)=-HOL(I,J)*PBL(I,J)/ZL1(I)                                       MRFPBL.660
        IF(SFCFLG(I))THEN                                                        MRFPBL.661
          PHIM(I)=(1.-APHI16*HOL1)**(-1./4.)                                     MRFPBL.662
          PHIH(I)=(1.-APHI16*HOL1)**(-1./2.)                                     MRFPBL.663
        ELSE                                                                     MRFPBL.664
          PHIM(I)=(1.+APHI5*HOL1)                                                MRFPBL.665
          PHIH(I)=PHIM(I)                                                        MRFPBL.666
        ENDIF                                                                    MRFPBL.667
        WSCALE(I)=UST(I,J)/PHIM(I)                                               MRFPBL.668
        WSCALE(I)=MIN(WSCALE(I),UST(I,J)*APHI16)                                 MRFPBL.669
        WSCALE(I)=MAX(WSCALE(I),UST(I,J)/APHI5)                                  MRFPBL.670
      ENDDO                                                                      MRFPBL.671
C                                                                                MRFPBL.672
C     COMPUTE THE SURFACE VARIABLES FOR PBL HEIGHT ESTIMATION                    MRFPBL.673
C     UNDER UNSTABLE CONDITIONS                                                  MRFPBL.674
C                                                                                MRFPBL.675
      DO I=IST,IEN                                                               07NOV00.1927
        IF(SFCFLG(I))THEN                                                        MRFPBL.677
          GAMFAC=CFAC/RHOX(I)/WSCALE(I)                                          MRFPBL.678
          HGAMT(I)=MIN(GAMFAC*HFX(I,J)/CPM(I),GAMCRT)                            MRFPBL.679
          HGAMQ(I)=MIN(GAMFAC*QFX(I,J),GAMCRQ)                                   MRFPBL.680
          IF((XLAND(I,J)-1.5).GE.0)HGAMQ(I)=0.                                   MRFPBL.681
          VPERT=HGAMT(I)+EP1*THX(I,KL)*HGAMQ(I)                                  MRFPBL.682
          VPERT=MIN(VPERT,GAMCRT)                                                MRFPBL.683
          THERMAL(I)=THERMAL(I)+MAX(VPERT,0.)                                    MRFPBL.684
          HGAMT(I)=MAX(HGAMT(I),0.0)                                             MRFPBL.685
          HGAMQ(I)=MAX(HGAMQ(I),0.0)                                             MRFPBL.686
        ELSE                                                                     MRFPBL.687
          PBLFLG(I)=.FALSE.                                                      MRFPBL.688
        ENDIF                                                                    MRFPBL.689
      ENDDO                                                                      MRFPBL.690
C                                                                                MRFPBL.691
      DO I=IST,IEN                                                               07NOV00.1928
        IF(PBLFLG(I))THEN                                                        MRFPBL.693
          KPBL(I)=KL                                                             MRFPBL.694
          PBL(I,J)=ZQ(I,KL)                                                      MRFPBL.695
        ENDIF                                                                    MRFPBL.696
      ENDDO                                                                      MRFPBL.697
C                                                                                MRFPBL.698
C     ENHANCE THE PBL HEIGHT BY CONSIDERING THE THERMAL                          MRFPBL.699
C                                                                                MRFPBL.700
      DO I=IST,IEN                                                               07NOV00.1929
        IF(PBLFLG(I))THEN                                                        MRFPBL.702
          STABLE(I)=.FALSE.                                                      MRFPBL.703
          BRUP(I)=BR(I)                                                          MRFPBL.704
        ENDIF                                                                    MRFPBL.705
      ENDDO                                                                      MRFPBL.706
      DO K=KLM,KLPBL,-1                                                          MRFPBL.707
        DO I=IST,IEN                                                             07NOV00.1930
          IF(.NOT.STABLE(I).AND.PBLFLG(I))THEN                                   MRFPBL.709
            BRDN(I)=BRUP(I)                                                      MRFPBL.710
            SPDK2=MAX((UX(I,K)**2+VX(I,K)**2),1.)                                MRFPBL.711
            BRUP(I)=(THVX(I,K)-THERMAL(I))*(G*ZA(I,K)/THVX(I,KL))/SPDK2          MRFPBL.712
            KPBL(I)=K                                                            MRFPBL.713
            STABLE(I)=BRUP(I).GT.BRCR                                            MRFPBL.714
          ENDIF                                                                  MRFPBL.715
        ENDDO                                                                    MRFPBL.716
      ENDDO                                                                      MRFPBL.717
C                                                                                MRFPBL.718
      DO I=IST,IEN                                                               07NOV00.1931
        IF(PBLFLG(I))THEN                                                        MRFPBL.720
          K=KPBL(I)                                                              MRFPBL.721
          IF(BRDN(I).GE.BRCR)THEN                                                MRFPBL.722
            BRINT=0.                                                             MRFPBL.723
          ELSEIF(BRUP(I).LE.BRCR)THEN                                            MRFPBL.724
            BRINT=1.                                                             MRFPBL.725
          ELSE                                                                   MRFPBL.726
            BRINT=(BRCR-BRDN(I))/(BRUP(I)-BRDN(I))                               MRFPBL.727
          ENDIF                                                                  MRFPBL.728
          PBL(I,J)=ZA(I,K+1)+BRINT*(ZA(I,K)-ZA(I,K+1))                           MRFPBL.729
          IF(PBL(I,J).LT.ZQ(I,KPBL(I)+1))KPBL(I)=KPBL(I)+1                       MRFPBL.730
          IF(KPBL(I).LE.1)PBLFLG(I)=.FALSE.                                      MRFPBL.731
        ENDIF                                                                    MRFPBL.732
      ENDDO                                                                      MRFPBL.733
C                                                                                23DEC04.3555
C     DIAGNOSTIC PBL HEIGHT WITH BRCR EFFECTIVELY ZERO (PBL0)                    23DEC04.3556
C                                                                                23DEC04.3557
      DO I=IST,IEN                                                               23DEC04.3558
        IF(PBLFLG(I))THEN                                                        23DEC04.3559
          STABLE(I)=.FALSE.                                                      23DEC04.3560
          BRUP(I)=BR(I)                                                          23DEC04.3561
        ENDIF                                                                    23DEC04.3562
      ENDDO                                                                      23DEC04.3563
      DO K=KLM,KLPBL,-1                                                          23DEC04.3564
        DO I=IST,IEN                                                             23DEC04.3565
          IF(.NOT.STABLE(I).AND.PBLFLG(I))THEN                                   23DEC04.3566
            BRDN(I)=BRUP(I)                                                      23DEC04.3567
            SPDK2=MAX((UX(I,K)**2+VX(I,K)**2),1.)                                23DEC04.3568
            BRUP(I)=(THVX(I,K)-THERMAL(I))*(G*ZA(I,K)/THVX(I,KL))/SPDK2          23DEC04.3569
            KPBL0(I)=K                                                           23DEC04.3570
            STABLE(I)=BRUP(I).GT.0.0                                             23DEC04.3571
          ENDIF                                                                  23DEC04.3572
                                                                                 23DEC04.3573
        ENDDO                                                                    23DEC04.3574
      ENDDO                                                                      23DEC04.3575
C                                                                                23DEC04.3576
      DO I=IST,IEN                                                               23DEC04.3577
        IF(PBLFLG(I))THEN                                                        23DEC04.3578
          K=KPBL0(I)                                                             23DEC04.3579
          IF(BRDN(I).GE.0.0)THEN                                                 23DEC04.3580
            BRINT=0.                                                             23DEC04.3581
          ELSEIF(BRUP(I).LE.0.0)THEN                                             23DEC04.3582
            BRINT=1.                                                             23DEC04.3583
          ELSE                                                                   23DEC04.3584
            BRINT=(0.0-BRDN(I))/(BRUP(I)-BRDN(I))                                23DEC04.3585
          ENDIF                                                                  23DEC04.3586
          PBL0(I)=ZA(I,K+1)+BRINT*(ZA(I,K)-ZA(I,K+1))                            23DEC04.3587
          IF(PBL0(I).LT.ZQ(I,KPBL0(I)+1))KPBL0(I)=KPBL0(I)+1                     23DEC04.3588
          IF(KPBL0(I).LE.1)PBLFLG(I)=.FALSE.                                     23DEC04.3589
        ENDIF                                                                    23DEC04.3590
      ENDDO                                                                      23DEC04.3591
C                                                                                MRFPBL.734
C     COMPUTE DIFFUSION COEFFICIENTS BELOW PBL                                   MRFPBL.735
C                                                                                MRFPBL.736
c+---+-----------------------------------------------------------------+   
C     Initialize columns to min value initially, prepare to compute.
      DO K=1,KL
        DO I=IST,IEN
           XKZM(I,K)=XKZMIN
           XKZH(I,K)=XKZMIN
        ENDDO
      ENDDO
c+---+-----------------------------------------------------------------+  
      DO K=KL,KLPBL,-1                                                           MRFPBL.737
        DO I=IST,IEN                                                             07NOV00.1932
          IF(KPBL(I).LT.K)THEN                                                   MRFPBL.739
            PRNUM=(PHIH(I)/PHIM(I)+CFAC*KARMAN*SFCFRAC)                          MRFPBL.740
            PRNUM=MIN(PRNUM,PRMAX)                                               MRFPBL.741
            PRNUM=MAX(PRNUM,PRMIN)                                               MRFPBL.742
            ZFAC=MAX((1.-(ZQ(I,K)-ZL1(I))/(PBL(I,J)-ZL1(I))),ZFMIN)              MRFPBL.743
            XKZO=CKZ*DZA(I,K-1)                                                  19DEC02.1721
            XKZM(I,K)=XKZO+WSCALE(I)*KARMAN*ZQ(I,K)*ZFAC**PFAC                   MRFPBL.745
            XKZH(I,K)=XKZM(I,K)/PRNUM                                            MRFPBL.746
            XKZM(I,K)=MIN(XKZM(I,K),XKZMAX)                                      MRFPBL.747
            XKZM(I,K)=MAX(XKZM(I,K),XKZMIN)                                      MRFPBL.748
            XKZH(I,K)=MIN(XKZH(I,K),XKZMAX)                                      MRFPBL.749
            XKZH(I,K)=MAX(XKZH(I,K),XKZMIN)                                      MRFPBL.750
          ENDIF                                                                  MRFPBL.751
        ENDDO                                                                    MRFPBL.752
      ENDDO                                                                      MRFPBL.753
C                                                                                MRFPBL.754
C     COMPUTE DIFFUSION COEFFICIENTS OVER PBL (FREE ATMOSPHERE)                  MRFPBL.755
C                                                                                MRFPBL.756
      DO K=2,KL                                                                  MRFPBL.757
        DO I=IST,IEN                                                             07NOV00.1933
          XKZO=CKZ*DZA(I,K-1)                                                    19DEC02.1722
CB        IF(K.LE.KPBL(I))THEN 
            SS=((UX(I,K-1)-UX(I,K))*(UX(I,K-1)-UX(I,K))+(VX(I,K-1)-              MRFPBL.761
     +         VX(I,K))*(VX(I,K-1)-VX(I,K)))/(DZA(I,K-1)*DZA(I,K-1))+            MRFPBL.762
     +         1.E-6 
CB   +         1.E-9
CB          RI=GOVRTH(I)*(THVX(I,K-1)-THVX(I,K))/(SS*DZA(I,K-1)) 
            RI=G/THVX(I,K)*(THVX(I,K-1)-THVX(I,K))
     +         / (SS*DZA(I,K-1))
            IF(IMVDIF.EQ.1.AND.IMOIST(IN).EQ.2)THEN                              MRFPBL.765
C     CHANGED FROM K AND K-1 TO K IN NEXT LINE.                                  05DEC01.1060
C     CLOUDY MIXING NOW EXTENDS ABOVE CLOUD.                                     05DEC01.1061
CB            IF((QCX(I,K)+QIX(I,K)).GT.0.01E-3)THEN   
              IF((QCX(I,K)+QIX(I,K)).GT.0.01E-4)THEN  
C      IN CLOUD                                                                  MRFPBL.768
                QMEAN=0.5*(QX(I,K)+QX(I,K-1))                                    MRFPBL.769
                TMEAN=0.5*(SCR3(I,K)+SCR3(I,K-1))                                MRFPBL.770
                ALPH=XLV*QMEAN/R/TMEAN                                           MRFPBL.771
                CHI=XLV*XLV*QMEAN/CP/RV/TMEAN/TMEAN                              MRFPBL.772
                RI=(1.+ALPH)*(RI-G*G/SS/TMEAN/CP*((CHI-ALPH)/(1.+CHI)))          MRFPBL.773
              ENDIF                                                              MRFPBL.774
            ENDIF                                                                MRFPBL.775
            ZK=KARMAN*ZQ(I,K)                                                    MRFPBL.776
C..Compute variable RLAM based on vertical spacing.
            RLAMX = DZA(I,K-1) * 0.25
            RLAMX = AMIN1(RLAM, RLAMX)
            RL2=(ZK*RLAMX/(RLAMX+ZK))**2
CB          RL2=(ZK*RLAM/(RLAM+ZK))**2 
            DK=RL2*SQRT(SS)                                                      MRFPBL.778
            IF(RI.LT.0.)THEN                                                     MRFPBL.779
! UNSTABLE REGIME                                                                MRFPBL.780
              SRI=SQRT(-RI)                                                      MRFPBL.781
CB            XKZM(I,K)=XKZO+DK*(1+8.*(-RI)/(1+1.746*SRI))
CB            XKZH(I,K)=XKZO+DK*(1+8.*(-RI)/(1+1.286*SRI))
              XKZM(I,K)=AMAX1(XKZM(I,K),
     +                  XKZO+DK*(1+8.*(-RI)/(1+1.746*SRI)))
              XKZH(I,K)=AMAX1(XKZH(I,K),
     +                  XKZO+DK*(1+8.*(-RI)/(1+1.286*SRI)))
            ELSE                                                                 MRFPBL.784
! STABLE REGIME                                                                  MRFPBL.785
CB            XKZH(I,K)=XKZO+DK/(1+5.*RI)**2  
              XKZH(I,K)=MAX(XKZH(I,K),
     +                  XKZO+DK/(1+5.*RI)**2)  
              PRNUM=1.0+2.1*RI                                                   MRFPBL.787
              PRNUM=MIN(PRNUM,PRMAX)                                             MRFPBL.788
CB            XKZM(I,K)=(XKZH(I,K)-XKZO)*PRNUM+XKZO
              XKZM(I,K)=MAX(XKZM(I,K),
     +                  (XKZH(I,K)-XKZO)*PRNUM+XKZO)
            ENDIF                                                                MRFPBL.790
C                                                                                MRFPBL.791
            XKZM(I,K)=MIN(XKZM(I,K),XKZMAX)                                      MRFPBL.792
            XKZM(I,K)=MAX(XKZM(I,K),XKZMIN)                                      MRFPBL.793
            XKZH(I,K)=MIN(XKZH(I,K),XKZMAX)                                      MRFPBL.794
            XKZH(I,K)=MAX(XKZH(I,K),XKZMIN)                                      MRFPBL.795
CB        ENDIF  
C                                                                                MRFPBL.797
C                                                                                MRFPBL.798
        ENDDO                                                                    MRFPBL.799
      ENDDO                                                                      MRFPBL.800
C                                                                                MRFPBL.801
C     COMPUTE TRIDIAGONAL MATRIX ELEMENTS FOR HEAT AND MOISTURE                  MRFPBL.802
C                                                                                MRFPBL.803
      DO I=IST,IEN                                                               07NOV00.1934
        AD(I,1)=1.                                                               MRFPBL.805
        A1(I,1)=SCR3(I,KL)+HFX(I,J)/(RHOX(I)*CPM(I))/ZQ(I,KL)*DT4                MRFPBL.806
        A2(I,1)=QX(I,KL)+QFX(I,J)/(RHOX(I))/ZQ(I,KL)*DT4                         MRFPBL.807
      ENDDO                                                                      MRFPBL.808
C                                                                                MRFPBL.809
      DO K=KL,2,-1                                                               MRFPBL.810
        KK=KL-K+1                                                                MRFPBL.811
        DTODSD=DT4/DSIGMA(K)                                                     MRFPBL.812
        DTODSU=DT4/DSIGMA(K-1)                                                   MRFPBL.813
        DSIG=A(K)-A(K-1)                                                         MRFPBL.814
        DO I=IST,IEN                                                             07NOV00.1935
          RDZ=1./DZA(I,K-1)                                                      MRFPBL.816
          IF(PBLFLG(I).AND.KPBL(I).LT.K)THEN                                     MRFPBL.817
            DSDZT=DSIG*XKZH(I,K)*RDZ*(G/CP-HGAMT(I)/PBL(I,J))                    MRFPBL.818
            DSDZQ=DSIG*XKZH(I,K)*RDZ*(-HGAMQ(I)/PBL(I,J))                        MRFPBL.819
            A2(I,KK)=A2(I,KK)+DTODSD*DSDZQ                                       MRFPBL.820
            A2(I,KK+1)=QX(I,K-1)-DTODSU*DSDZQ                                    MRFPBL.821
          ELSE                                                                   MRFPBL.822
            DSDZT=DSIG*XKZH(I,K)*RDZ*(G/CP)                                      MRFPBL.823
            A2(I,KK+1)=QX(I,K-1)                                                 MRFPBL.824
          ENDIF                                                                  MRFPBL.825
          DSDZ2=DSIG*XKZH(I,K)*RDZ*RDZ                                           MRFPBL.826
          AU(I,KK)=-DTODSD*DSDZ2                                                 MRFPBL.827
          AL(I,KK)=-DTODSU*DSDZ2                                                 MRFPBL.828
          AD(I,KK)=AD(I,KK)-AU(I,KK)                                             MRFPBL.829
          AD(I,KK+1)=1.-AL(I,KK)                                                 MRFPBL.830
          A1(I,KK)=A1(I,KK)+DTODSD*DSDZT                                         MRFPBL.831
          A1(I,KK+1)=SCR3(I,K-1)-DTODSU*DSDZT                                    MRFPBL.832
        ENDDO                                                                    MRFPBL.833
      ENDDO                                                                      MRFPBL.834
C                                                                                MRFPBL.835
C     SOLVE TRIDIAGONAL PROBLEM FOR HEAT AND MOISTURE                            MRFPBL.836
C                                                                                MRFPBL.837
      CALL TRIDI2(IST,IEN,KL,AL,AD,AU,A1,A2,AU,A1,A2)                            07NOV00.1936
C                                                                                MRFPBL.839
C     RECOVER TENDENCIES OF HEAT AND MOISTURE                                    MRFPBL.840
C                                                                                MRFPBL.841
      DO K=KL,1,-1                                                               MRFPBL.842
        KK=KL-K+1                                                                MRFPBL.843
        DO I=IST,IEN                                                             07NOV00.1937
          TTEND=(A1(I,KK)-SCR3(I,K))*RDT                                         MRFPBL.845
          QTEND=(A2(I,KK)-QX(I,K))*RDT                                           MRFPBL.846
          TTNP(I,K)=TTNP(I,K)+TTEND                                              MRFPBL.847
          QTNP(I,K)=QTNP(I,K)+QTEND                                              MRFPBL.848
          DTSFC(I)=DTSFC(I)+CONT*DSIGMA(K)*PSB(I,J)*TTEND                        MRFPBL.849
          DQSFC(I)=DQSFC(I)+CONQ*DSIGMA(K)*PSB(I,J)*QTEND                        MRFPBL.850
        ENDDO                                                                    MRFPBL.851
      ENDDO                                                                      MRFPBL.852
C                                                                                MRFPBL.853
C     COMPUTE TRIDIAGONAL MATRIX ELEMENTS FOR MOMENTUM                           MRFPBL.854
C                                                                                MRFPBL.855
      DO I=IST,IEN                                                               07NOV00.1938
        AD(I,1)=1.                                                               MRFPBL.857
        A1(I,1)=UX(I,KL)-UX(I,KL)/WSPD1(I)*UST(I,J)*UST(I,J)/ZQ(I,KL)            23DEC04.3592
     -          *DT4*(WSPD1(I)/WSPD(I))**2                                       23DEC04.3593
        A2(I,1)=VX(I,KL)-VX(I,KL)/WSPD1(I)*UST(I,J)*UST(I,J)/ZQ(I,KL)            23DEC04.3594
     -          *DT4*(WSPD1(I)/WSPD(I))**2                                       23DEC04.3595
      ENDDO                                                                      MRFPBL.860
C                                                                                MRFPBL.861
      DO K=KL,2,-1                                                               MRFPBL.862
        KK=KL-K+1                                                                MRFPBL.863
        DTODSD=DT4/DSIGMA(K)                                                     MRFPBL.864
        DTODSU=DT4/DSIGMA(K-1)                                                   MRFPBL.865
        DSIG=A(K)-A(K-1)                                                         MRFPBL.866
        DO I=IST,IEN                                                             07NOV00.1939
          RDZ=1./DZA(I,K-1)                                                      MRFPBL.868
          DSDZ2=DSIG*XKZM(I,K)*RDZ*RDZ                                           MRFPBL.869
          AU(I,KK)=-DTODSD*DSDZ2                                                 MRFPBL.870
          AL(I,KK)=-DTODSU*DSDZ2                                                 MRFPBL.871
          AD(I,KK)=AD(I,KK)-AU(I,KK)                                             MRFPBL.872
          AD(I,KK+1)=1.-AL(I,KK)                                                 MRFPBL.873
          A1(I,KK+1)=UX(I,K-1)                                                   MRFPBL.874
          A2(I,KK+1)=VX(I,K-1)                                                   MRFPBL.875
        ENDDO                                                                    MRFPBL.876
      ENDDO                                                                      MRFPBL.877
C                                                                                MRFPBL.878
C     SOLVE TRIDIAGONAL PROBLEM FOR MOMENTUM                                     MRFPBL.879
C                                                                                MRFPBL.880
      CALL TRIDI2(IST,IEN,KL,AL,AD,AU,A1,A2,AU,A1,A2)                            07NOV00.1940
C                                                                                MRFPBL.882
C     RECOVER TENDENCIES OF MOMENTUM                                             MRFPBL.883
C                                                                                MRFPBL.884
      DO K=KL,1,-1                                                               MRFPBL.885
        KK=KL-K+1                                                                MRFPBL.886
        DO I=IST,IEN                                                             07NOV00.1941
          UTEND=(A1(I,KK)-UX(I,K))*RDT                                           MRFPBL.888
          VTEND=(A2(I,KK)-VX(I,K))*RDT                                           MRFPBL.889
          UTNP(I,K)=UTNP(I,K)+UTEND                                              MRFPBL.890
          VTNP(I,K)=VTNP(I,K)+VTEND                                              MRFPBL.891
          DUSFC(I)=DUSFC(I)+CONW*DSIGMA(K)*PSB(I,J)*UTEND                        MRFPBL.892
          DVSFC(I)=DVSFC(I)+CONW*DSIGMA(K)*PSB(I,J)*VTEND                        MRFPBL.893
        ENDDO                                                                    MRFPBL.894
      ENDDO                                                                      MRFPBL.895
C                                                                                MRFPBL.896
C     COMPUTE TRIDIAGONAL MATRIX ELEMENTS FOR CLOUD                              MRFPBL.897
C                                                                                MRFPBL.898
      IF(IMOIST(IN).EQ.1)GOTO 690                                                MRFPBL.899
      DO I=IST,IEN                                                               07NOV00.1942
        AD(I,1)=1.                                                               MRFPBL.901
        A1(I,1)=QCX(I,KL)                                                        MRFPBL.902
        A2(I,1)=QIX(I,KL)                                                        MRFPBL.903
      ENDDO                                                                      MRFPBL.904
C                                                                                MRFPBL.905
      DO K=KL,2,-1                                                               MRFPBL.906
        KK=KL-K+1                                                                MRFPBL.907
        DTODSD=DT4/DSIGMA(K)                                                     MRFPBL.908
        DTODSU=DT4/DSIGMA(K-1)                                                   MRFPBL.909
        DSIG=A(K)-A(K-1)                                                         MRFPBL.910
        DO I=IST,IEN                                                             07NOV00.1943
          RDZ=1./DZA(I,K-1)                                                      MRFPBL.912
          A1(I,KK+1)=QCX(I,K-1)                                                  MRFPBL.913
          A2(I,KK+1)=QIX(I,K-1)                                                  MRFPBL.914
          DSDZ2=DSIG*XKZH(I,K)*RDZ*RDZ                                           MRFPBL.915
          AU(I,KK)=-DTODSD*DSDZ2                                                 MRFPBL.916
          AL(I,KK)=-DTODSU*DSDZ2                                                 MRFPBL.917
          AD(I,KK)=AD(I,KK)-AU(I,KK)                                             MRFPBL.918
          AD(I,KK+1)=1.-AL(I,KK)                                                 MRFPBL.919
        ENDDO                                                                    MRFPBL.920
      ENDDO                                                                      MRFPBL.921
C                                                                                MRFPBL.922
C     SOLVE TRIDIAGONAL PROBLEM FOR CLOUD                                        MRFPBL.923
C                                                                                MRFPBL.924
      CALL TRIDI2(IST,IEN,KL,AL,AD,AU,A1,A2,AU,A1,A2)                            07NOV00.1944
C                                                                                MRFPBL.926
C     RECOVER TENDENCIES OF CLOUD                                                MRFPBL.927
C                                                                                MRFPBL.928
      DO K=KL,1,-1                                                               MRFPBL.929
        KK=KL-K+1                                                                MRFPBL.930
        DO I=IST,IEN                                                             07NOV00.1945
          QCTEND=(A1(I,KK)-QCX(I,K))*RDT                                         MRFPBL.932
          QITEND=(A2(I,KK)-QIX(I,K))*RDT                                         MRFPBL.933
          QCTNP(I,K)=QCTNP(I,K)+QCTEND                                           MRFPBL.934
          QITNP(I,K)=QITNP(I,K)+QITEND                                           MRFPBL.935
        ENDDO                                                                    MRFPBL.936
      ENDDO                                                                      MRFPBL.937
C                                                                                MRFPBL.938
C---- END OF VERTICAL DIFFUSION                                                  MRFPBL.939
C                                                                                MRFPBL.940
  690 CONTINUE                                                                   MRFPBL.941
C                                                                                MRFPBL.942
C-----CONVERT ALL THE TENDENCIES TO FLUX FORM:                                   MRFPBL.943
C                                                                                MRFPBL.944
      DO 780 K=1,KL                                                              MRFPBL.945
        DO 780 I=IST,IEN                                                         07NOV00.1946
          UTNP(I,K)=PSB(I,J)*UTNP(I,K)                                           MRFPBL.947
          VTNP(I,K)=PSB(I,J)*VTNP(I,K)                                           MRFPBL.948
          TTNP(I,K)=PSB(I,J)*TTNP(I,K)                                           MRFPBL.949
  780   CONTINUE                                                                 MRFPBL.950
                                                                                 MRFPBL.951
C                                                                                MRFPBL.952
      IF(IDRY(IN).EQ.1)GOTO 810                                                  MRFPBL.953
      DO 790 K=1,KL                                                              MRFPBL.954
        DO 790 I=IST,IEN                                                         07NOV00.1947
          QTNP(I,K)=PSB(I,J)*QTNP(I,K)                                           MRFPBL.956
  790   CONTINUE                                                                 MRFPBL.957
                                                                                 MRFPBL.958
C                                                                                MRFPBL.959
      IF(IMOIST(IN).EQ.1)GOTO 810                                                MRFPBL.960
      DO 800 K=1,KL                                                              MRFPBL.961
        DO 800 I=IST,IEN                                                         07NOV00.1948
          QCTNP(I,K)=PSB(I,J)*QCTNP(I,K)                                         MRFPBL.963
          QITNP(I,K)=PSB(I,J)*QITNP(I,K)                                         MRFPBL.964
          IF(IICEG.EQ.1)THEN                                                     MRFPBL.965
            QNCNP(I,K)=PSB(I,J)*QNCNP(I,K)                                       MRFPBL.966
          ENDIF                                                                  MRFPBL.967
  800   CONTINUE                                                                 MRFPBL.968
                                                                                 MRFPBL.969
  810 CONTINUE                                                                   MRFPBL.970
C                                                                                MRFPBL.971
C-----CALCULATION OF NEW VALUES DUE TO VERTICAL EXCHANGE PROCESSES IS            MRFPBL.972
C     COMPLETED. THE FINAL STEP IS TO ADD THE TENDENCIES CALCULATED              MRFPBL.973
C     IN HIRPBL TO THOSE OF MM4.                                                 MRFPBL.974
C                                                                                MRFPBL.975
C                                                                                MRFPBL.976
      DO 820 K=1,KL                                                              MRFPBL.977
        DO 820 I=IST,IEN                                                         07NOV00.1949
          U3DTEN(I,J,K)=UTNP(I,K)                                                MRFPBL.979
          V3DTEN(I,J,K)=VTNP(I,K)                                                MRFPBL.980
  820   CONTINUE                                                                 MRFPBL.981
                                                                                 MRFPBL.982
C                                                                                MRFPBL.983
  825 CONTINUE                                                                   MRFPBL.985
      DO 830 K=1,KL                                                              MRFPBL.999
        DO 830 I=IST,IEN                                                         07NOV00.1950
          IF ( INTERIOR_MASK(I,J) .EQ. 1. ) THEN                                 07NOV00.1951
            T3DTEN(I,J,K)=T3DTEN(I,J,K)+TTNP(I,K)                                07NOV00.1952
          ENDIF                                                                  07NOV00.1953
  830   CONTINUE                                                                 MRFPBL.1002
                                                                                 MRFPBL.1003
C                                                                                MRFPBL.1004
      IF(IDRY(IN).EQ.1)GOTO 860                                                  MRFPBL.1005
      DO 840 K=1,KL                                                              MRFPBL.1006
        DO 840 I=IST,IEN                                                         07NOV00.1954
          IF ( INTERIOR_MASK(I,J) .EQ. 1. ) THEN                                 07NOV00.1955
            QV3DTEN(I,J,K)=QV3DTEN(I,J,K)+QTNP(I,K)                              07NOV00.1956
          ENDIF                                                                  07NOV00.1957
  840   CONTINUE                                                                 MRFPBL.1009
                                                                                 MRFPBL.1010
C                                                                                MRFPBL.1011
      IF(IMOIST(IN).EQ.1)GOTO 860                                                MRFPBL.1012
      DO 850 K=1,KL                                                              MRFPBL.1013
        DO 850 I=IST,IEN                                                         07NOV00.1958
          IF ( INTERIOR_MASK(I,J) .EQ. 1. ) THEN                                 07NOV00.1959
            QC3DTEN(I,J,K)=QC3DTEN(I,J,K)+QCTNP(I,K)                             07NOV00.1960
            IF(IICE.EQ.1)QI3DTEN(I,J,K)=QI3DTEN(I,J,K)+QITNP(I,K)                07NOV00.1961
            IF(IICEG.EQ.1)QNC3DTEN(I,J,K)=QNC3DTEN(I,J,K)+QNCNP(I,K)             07NOV00.1962
          ENDIF                                                                  07NOV00.1963
  850   CONTINUE                                                                 MRFPBL.1018
                                                                                 MRFPBL.1019
  860 CONTINUE                                                                   MRFPBL.1020
C                                                                                MRFPBL.1021
C-----APPLY ASSELIN FILTER TO TGD FOR LARGE TIME STEP:                           MRFPBL.1022
C                                                                                MRFPBL.1023
      IF(ITGFLG(IN).NE.1)GOTO 880                                                MRFPBL.1024
      DO 870 I=IST,IEN                                                           07NOV00.1964
        TGC=TGB(I,J)                                                             MRFPBL.1026
        TGB(I,J)=OMUHF*TGA(I,J)+GNUHF*(TGC+TGDSB(I))                             MRFPBL.1027
        TGA(I,J)=TGC                                                             MRFPBL.1028
        IF(ISOIL.EQ.1)THEN                                                       MRFPBL.1029
          DO L=1,5                                                               MRFPBL.1030
            TSLC=TSLB(I,J,L)                                                     MRFPBL.1031
            TSLB(I,J,L)=OMUHF*TSLA(I,J,L)+GNUHF*(TSLC+TSLDSB(I,L))               MRFPBL.1032
            TSLA(I,J,L)=TSLC                                                     MRFPBL.1033
          ENDDO                                                                  MRFPBL.1034
        ENDIF                                                                    MRFPBL.1035
  870 CONTINUE                                                                   MRFPBL.1036
  880 CONTINUE                                                                   MRFPBL.1037
C                                                                                MRFPBL.1042
C                                                                                MRFPBL.1043
      IF(IN.EQ.1)THEN                                                            MRFPBL.1044
        XXT=XTIME+DTMIN                                                          MRFPBL.1045
      ELSE                                                                       MRFPBL.1046
        XXT=XTIME+FLOAT(IRAX)*DTMIN                                              MRFPBL.1047
      ENDIF                                                                      MRFPBL.1048
C                                                                                MRFPBL.1049
  940 CONTINUE                                                                   MRFPBL.1050
C                                                                                MRFPBL.1051
C KPBL IS NEEDED FOR THE FDDA, AND SINCE THERE IS NO LONGER JUST ONE             MRFPBL.1052
C LARGE "J LOOP" IT MUST BE STORED AS (I,J)...                                   MRFPBL.1053
C                                                                                MRFPBL.1054
      DO 950 I=IST,IEN                                                           07NOV00.1965
C FOR 3.7 WE USE NEW DIAGNOSED PBL DEPTH                                         23DEC04.3596
        KPBL2D(I,J)=KPBL0(I)                                                     23DEC04.3597
        PBL(I,J)=PBL0(I)                                                         23DEC04.3598
  950 CONTINUE                                                                   MRFPBL.1057
      RETURN                                                                     MRFPBL.1058
      END                                                                        MRFPBL.1059
                                                                                 MRFPBL.1060
C-----------------------------------------------------------------------         MRFPBL.1061
