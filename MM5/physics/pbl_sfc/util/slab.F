#include <defines.incl>                                                          07NOV00.2000
#ifndef MPP1_COLUMNWISE                                                          SLAB.1
#  ifndef VECTOR                                                                 07NOV00.2001
      SUBROUTINE SLAB(DELTSM,J,INEST,THX,QX,FLHC,FLQC,PP3D,T3D,IST,IEN)          07NOV00.2002
#  else                                                                          07NOV00.2003
      SUBROUTINE SLAB(DELTSM,J,INEST,THX,QX,FLHC,FLQC,PP3D,T3D,                  07NOV00.2004
     +         XLV,PPB,RPSB,TSLB,SVP1,SVP2,SVP3,SVPT0,EP2,                       07NOV00.2005
     +         XLAND,PSB,TGB,EMISS,GLW,GSW,TMN,HFX,QFX,CAPG,SNOWC,               07NOV00.2006
     +         SATBRT,RAINP,RAINC,RAINNC,THC,MAVAIL,ALB,                         05DEC01.1535
     +         XMOIST,IST,IEN )                                                  07NOV00.2008
#  endif                                                                         07NOV00.2009
#else                                                                            SLAB.3
#  undef VECTOR                                                                  05DEC01.1314
      SUBROUTINE SLAB_COL(DELTSM,I,J,INEST,THX,QX,FLHC,FLQC,PP3D,T3D)            07NOV00.2010
#endif                                                                           SLAB.5
      IMPLICIT NONE                                                              07NOV00.2011
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          SLAB.6
C                                                                     C          SLAB.7
C                                                                     C          SLAB.8
C     SUBROUTINE SLAB CALCULATES THE GROUND TEMPERATURE TENDENCY      C          SLAB.9
C     ACCORDING TO THE RESIDUAL OF THE SURFACE ENERGY BUDGET          C          SLAB.10
C     (BLACKADAR, 1978B).                                             C          SLAB.11
C                                                                     C          SLAB.12
C     CHANGES:                                                        C          SLAB.13
C          FOR SOIL SUB-TIMESTEPS UPDATE SURFACE HFX AND QFX AS TG    C          SLAB.14
C          CHANGES TO PREVENT POSSIBLE INSTABILITY FOR LONG MODEL     C          SLAB.15
C          STEPS (DT > ~200 SEC).                                     C          SLAB.16
C                                                                     C          SLAB.17
C          PUT SNOW COVER CHECK ON SOIL SUB-TIMESTEPS                 C          SLAB.18
C                                                                     C          SLAB.19
C          MAKE UPPER LIMIT ON SOIL SUB-STEP LENGTH MORE CONSERVATIVE C          SLAB.20
C                                                                     C          SLAB.21
C          USE TGB AS A TEMPERATURE NOT THETA IN PBL                  C          07NOV00.2012
C                                                                     C          07NOV00.2013
C          V3.4: BUCKET MODEL                                         C          07NOV00.2014
C                                                                     C          07NOV00.2015
C                                                                     C          07NOV00.2016
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          SLAB.22
#     include <parame.incl>                                                      SLAB.23
C                                                                                SLAB.24
#     include <param2.incl>                                                      SLAB.25
#     include <param3.incl>                                                      SLAB.26
#ifndef VECTOR                                                                   07NOV00.2017
#     include <pmoist.incl>                                                      SLAB.27
#     include <varia.incl>                                                       SLAB.28
#     include <nonhyd.incl>                                                      SLAB.29
#     include <point2d.incl>                                                     07NOV00.2018
#     include <soil.incl>                                                        07NOV00.2019
#else                                                                            07NOV00.2020
      REAL      PPB(MIX,MJX,MKX),TSLB(MIX,MJX,MLX)                               07NOV00.2021
      REAL      XMOIST(*),SVP1,SVP2,SVP3,SVPT0,EP2,XLV                           07NOV00.2022
      REAL      RPSB(MIX,MJX)                                                    07NOV00.2023
      REAL XLAND(MIX,MJX),PSB(MIX,MJX),TGB(MIX,MJX),EMISS(MIX,MJX)               07NOV00.2024
      REAL GLW(MIX,MJX),GSW(MIX,MJX),TMN(MIX,MJX),HFX(MIX,MJX)                   07NOV00.2025
      REAL QFX(MIX,MJX),CAPG(MIX,MJX),SNOWC(MIX,MJX)                             07NOV00.2026
C new with 3.4                                                                   07NOV00.2027
      REAL SATBRT(MIX,MJX),RAINP(MIX,MJX)    ! IN                                07NOV00.2028
      REAL THC(MIX,MJX),MAVAIL(MIX,MJX)      ! OUT                               07NOV00.2029
      REAL ALB(MIX,MJX)                      ! INOUT                             07NOV00.2030
C RAINC and RAINNC new with 3.5                                                  05DEC01.1536
      REAL RAINC(MIX,MJX),RAINNC(MIX,MJX)    ! IN                                05DEC01.1537
#endif                                                                           07NOV00.2031
      INTEGER   IST,IEN                                                          07NOV00.2032
C                                                                                SLAB.30
      REAL      DTHGDT(MIX),TG0(MIX),THTMN(MIX),XLD1(MIX)                        07NOV00.2033
      REAL      TSCVN(MIX),PISFC(MIX)                                            07NOV00.2034
C                                                                                SLAB.33
      REAL      UPFLUX(MIX), HM(MIX), RNET(MIX), XINET(MIX),                     07NOV00.2035
     1          QS(MIX)                                                          SLAB.35
      REAL      THX(MIX,MKX), QX(MIX,MKX), FLHC(MIX), FLQC(MIX)                  07NOV00.2036
      REAL      PP3D(MIX,MJX,MKX), T3D(MIX,MJX,MKX)                              07NOV00.2037
      REAL      FLUX(MIX,MLX),DTSDT(MIX)                                         07NOV00.2038
#     include <various.incl>                                                     SLAB.39
#     include <soilcnst.incl>                                                    SLAB.41
#ifndef VECTOR                                                                   07NOV00.2039
#     include <rpstar.incl>                                                      SLAB.43
#endif                                                                           07NOV00.2040
C ADD NEW ARRAY WITH DENSITY OF SNOW AT MULTIPLE DEPTHS                          19DEC02.1879
      REAL DENSNW(8)                                                             19DEC02.1880
      DATA DENSNW/0.356,0.357,0.359,0.363,0.371,0.387,0.418,0.456/               19DEC02.1881
      LOGICAL PRINTWARN                                                          07NOV00.2041
C                                                                                07NOV00.2042
C IMPLICIT NONE DECLARATIONS                                                     07NOV00.2043
      REAL XLDCOL,RNSOIL,THG,ESG,QSG,HFXT,QFXT,TGBX,XXT,PS,DELTSM                07NOV00.2044
      REAL TGWX,UPFLSI,UPFLXW,ESGW,QSGW,ESGI,QSGI                                19DEC02.1882
      REAL HFXTW,HFXTI,QFXTW,QFXTI,DIFSNW,TGBXX                                  19DEC02.1883
      real zc                                                                    23DEC04.3817
      INTEGER I,J,K,L,INEST                                                      07NOV00.2045
      INTEGER NSOIL,NSOILI,ITSOIL,IPW                                            07NOV00.2046
                                                                                 SLAB.44
C DATA USED IN BUCKET MODEL                                                      07NOV00.2047
#     include <landuse.incl>                                                     07NOV00.2048
#     include <bucket.incl>                                                      07NOV00.2049
      INTEGER LLUS                                                               07NOV00.2050
      REAL    PRECIP, RUNOFF, XMM, EVAP, AVAIL, TIMFAC                           05DEC01.1315
      real avsnow, zntfac, dstemp                                                19DEC02.1884
      data zc/0.7639888525/                                                      23DEC04.3818
#ifdef IBMopt                                                                    23DEC04.3819
      integer is,nn                                                              23DEC04.3820
      real temp1(mix),temp2(mix)                                                 23DEC04.3821
#ifdef MPP1                                                                      23DEC04.3822
#define nf(A) noflic(A)  
#else                                                                            23DEC04.3824
#define nf(A) A        
#endif                                                                           23DEC04.3826
#endif                                                                           23DEC04.3827
C                                                                                SLAB.45
C----------------------------------------------------------------------          SLAB.46
C                                                                                SLAB.47
C-----DETERMINE IF ANY POINTS IN COLUMN ARE LAND (RATHER THAN OCEAN)             SLAB.48
C       POINTS.  IF NOT, SKIP DOWN TO THE PRINT STATEMENTS SINCE OCEAN           SLAB.49
C       SURFACE TEMPERATURES ARE NOT ALLOWED TO CHANGE.                          SLAB.50
C                                                                                SLAB.51
#ifdef IBMopt                                                                    23DEC04.3828
#ifdef MPP1                                                                      23DEC04.3829
      nn = ie_x0(IEN) - is_x0(IST) + 1                                           23DEC04.3830
      is = is_x0(IST)                                                            23DEC04.3831
#else                                                                            23DEC04.3832
      nn = IEN - IST + 1                                                         23DEC04.3833
      is = IST                                                                   23DEC04.3834
#endif                                                                           23DEC04.3835
#endif                                                                           23DEC04.3836
      XLDCOL=2.0                                                                 SLAB.52
      DO 10 I=IST,IEN                                                            07NOV00.2052
        XLDCOL=AMIN1(XLDCOL,XLAND(I,J))                                          SLAB.54
   10 CONTINUE                                                                   SLAB.55
C                                                                                SLAB.56
      IF(XLDCOL.GT.1.5)GOTO 90                                                   SLAB.57
C                                                                                SLAB.58
C                                                                                SLAB.59
C-----CONVERT SLAB TEMPERATURE TO POTENTIAL TEMPERATURE AND                      SLAB.60
C     SET XLD1(I) = 0. FOR OCEAN POINTS:                                         SLAB.61
C                                                                                SLAB.62
C                                                                                SLAB.63
      DO 20 I=IST,IEN                                                            07NOV00.2053
        IF((XLAND(I,J)-1.5).GE.0)THEN                                            SLAB.65
          XLD1(I)=0.                                                             SLAB.66
        ELSE                                                                     SLAB.67
          XLD1(I)=1.                                                             SLAB.68
        ENDIF                                                                    SLAB.69
   20 CONTINUE                                                                   SLAB.70
C                                                                                SLAB.71
C     TGB IS NOW ASSUMED TO CONTAIN TEMPERATURE ON INPUT AND                     07NOV00.2054
C     OUTPUT FOR SLAB                                                            07NOV00.2055
C                                                                                SLAB.75
#ifndef IBMopt                                                                   23DEC04.3837
      DO 50 I=IST,IEN                                                            07NOV00.2056
        IF(XLD1(I).LT.0.5)GOTO 50                                                SLAB.77
        PS=PSB(I,J)+PTOP+PP3D(I,J,KL)*0.001                                      SLAB.78
        PISFC(I)=(PS*0.01)**ROVCP                                                07NOV00.2057
        TG0(I)=TGB(I,J)                                                          07NOV00.2058
   50 CONTINUE                                                                   SLAB.80
#else                                                                            23DEC04.3838
                                                                                 23DEC04.3839
      ESGW=SVP1*EXP(SVP2*(271.4-SVPT0)/(271.4-SVP3))                             23DEC04.3840
                                                                                 23DEC04.3841
      DO  I=IST,IEN                                                              23DEC04.3842
        PS = PSB(I,J)+PTOP+PP3D(I,J,KL)*0.001                                    23DEC04.3843
        PISFC(I) = PS*.01                                                        23DEC04.3844
        temp1(i) = 22.514-6.15E3/TSLB(I,J,1)                                     23DEC04.3845
        temp2(i) = SVP2*(TSLB(I,J,1)-SVPT0)/(TSLB(I,J,1)-SVP3)                   23DEC04.3846
      ENDDO                                                                      23DEC04.3847
                                                                                 23DEC04.3848
      call vslog(PISFC(nf(is)),PISFC(nf(is)),nn)                                 23DEC04.3849
      call vsexp(temp1(nf(is)),temp1(nf(is)),nn)                                 23DEC04.3850
      call vsexp(temp2(nf(is)),temp2(nf(is)),nn)                                 23DEC04.3851
                                                                                 23DEC04.3852
      DO I=IST,IEN                                                               23DEC04.3853
         PISFC(i) =  - PISFC(i) * ROVCP                                          23DEC04.3854
         temp1(i) = .611 * temp1(i)                                              23DEC04.3855
         temp2(i) = SVP1 * temp2(i)                                              23DEC04.3856
      ENDDO                                                                      23DEC04.3857
      call vsexp(PISFC(nf(is)),PISFC(nf(is)),nn)                                 23DEC04.3858
#endif                                                                           23DEC04.3859
C                                                                                SLAB.81
C-----COMPUTE THE SURFACE ENERGY BUDGET:                                         SLAB.82
C                                                                                SLAB.83
      DO 70 I=IST,IEN                                                            07NOV00.2059
        IF(XLD1(I).LT.0.5)GOTO 70                                                SLAB.86
#ifdef IBMopt                                                                    23DEC04.3860
        TG0(I)=TGB(I,J)                                                          23DEC04.3861
#endif                                                                           23DEC04.3862
        UPFLUX(I)=STBOLT*TG0(I)**4                                               SLAB.88
        XINET(I)=EMISS(I,J)*(GLW(I,J)-UPFLUX(I))                                 25JAN00.681
        IF(SEAICE(I,J).GT.0.02)THEN                                              19DEC02.1885
          IF(SEAICE(I,J).GT.0.10)THEN                                            19DEC02.1886
            TGWX=271.4                                                           19DEC02.1887
          ELSEIF(SEAICE(I,J).LT.0.01)THEN                                        19DEC02.1888
            TGWX=TG0(I)                                                          19DEC02.1889
          ELSE                                                                   19DEC02.1890
           TGWX=(TG0(I)-TGSI(I,J)*SEAICE(I,J))/(1.-SEAICE(I,J))                  19DEC02.1891
          ENDIF                                                                  19DEC02.1892
          UPFLSI=STBOLT*TGSI(I,J)**4                                             19DEC02.1893
          UPFLXW=STBOLT*TGWX**4                                                  19DEC02.1894
          UPFLUX(I)=SEAICE(I,J)*UPFLSI+(1.-SEAICE(I,J))*UPFLXW                   19DEC02.1895
          XINET(I)=EMISS(I,J)*(GLW(I,J)-UPFLSI)                                  19DEC02.1896
        ENDIF                                                                    19DEC02.1897
        RNET(I)=GSW(I,J)+XINET(I)                                                SLAB.94
        HM(I)=1.18*EOMEG*(TG0(I)-TMN(I,J))                                       SLAB.95
        QS(I)=HFX(I,J)+XMOIST(INEST)*QFX(I,J)*XLV                                SLAB.96
        IF(NINT(SATBRT(I,J)).EQ.ISICE.AND.IPOLAR.EQ.1)THEN                       19DEC02.1898
C         PERMANENT ICE SURFACE TYPE                                             19DEC02.1899
          HM(I)=1.82E-05*(TG0(I)-TMN(I,J))                                       19DEC02.1900
          QS(I)=HFX(I,J)+XMOIST(INEST)*QFX(I,J)*2.836E+06                        19DEC02.1901
        ELSEIF(SEAICE(I,J).GT.0.02)THEN                                          19DEC02.1902
          HM(I)=1.82E-05*(TGSI(I,J)-TMN(I,J))                                    19DEC02.1903
          IF(SEAICE(I,J).GT.0.99)THEN                                            19DEC02.1904
            QS(I)=HFX(I,J)+XMOIST(INEST)*QFXSI(I,J)*2.836E+06                    19DEC02.1905
          ELSE                                                                   19DEC02.1906
            QS(I)=HFXSI(I,J)+XMOIST(INEST)*QFXSI(I,J)*2.836E+06                  19DEC02.1907
          ENDIF                                                                  19DEC02.1908
        ENDIF                                                                    19DEC02.1909
        IF(ISOIL.EQ.0)THEN                                                       SLAB.97
          DTHGDT(I)=(RNET(I)-QS(I))/CAPG(I,J)-HM(I)                              SLAB.98
        ELSEIF(ISLDIM.EQ.1.AND.ISOIL.EQ.1)THEN                                   SLAB.99
          DTHGDT(I)=0.                                                           SLAB.103
        ENDIF                                                                    SLAB.104
   70 CONTINUE                                                                   SLAB.105
      IF(ISOIL.EQ.1)THEN                                                         SLAB.106
        NSOIL=1+IFIX(SOILFAC*4*DIFSL/DZS(1)*DELTSM/DZS(1))                       07NOV00.2060
        RNSOIL=1./FLOAT(NSOIL)                                                   SLAB.107
C                                                                                SLAB.108
C     SOIL SUB-TIMESTEP                                                          SLAB.109
C                                                                                SLAB.110
        DO ITSOIL=1,NSOIL                                                        SLAB.111
          DO L=1,MLX-1                                                           SLAB.112
            PRINTWARN=.FALSE.                                                    07NOV00.2061
            DO I=IST,IEN                                                         07NOV00.2062
              IF(XLD1(I).LT.0.5)GOTO 75                                          SLAB.114
              IF(L.EQ.1.AND.ITSOIL.GT.1)THEN                                     SLAB.115
                PS=PSB(I,J)+PTOP+PPB(I,J,KL)*RPSB(I,J)*0.001                     SLAB.118
#ifndef IBMopt                                                                   23DEC04.3863
                THG=TSLB(I,J,1)/PISFC(I)                                         07NOV00.2063
#else                                                                            23DEC04.3864
                THG=TSLB(I,J,1)*PISFC(I)                                         23DEC04.3865
#endif                                                                           23DEC04.3866
                IF(NINT(SATBRT(I,J)).EQ.ISICE.AND.IPOLAR.EQ.1)THEN               19DEC02.1910
C      USE ICE SATURATION                                                        19DEC02.1911
#ifndef IBMopt                                                                   23DEC04.3867
                  ESG=.611*EXP(22.514-6.15E3/TSLB(I,J,1))                        19DEC02.1912
#else                                                                            23DEC04.3868
                  ESG = temp1(i)                                                 23DEC04.3869
#endif                                                                           23DEC04.3870
                  QSG=.622*ESG/(PS-ESG)                                          19DEC02.1913
                  HFXT=FLHC(I)*(THG-THX(I,KL))                                   19DEC02.1914
                  QFXT=FLQC(I)*(QSG-QX(I,KL))                                    19DEC02.1915
                  QS(I)=HFXT+XMOIST(INEST)*QFXT*2.836E+06                        19DEC02.1916
                  HFX(I,J)=HFX(I,J)+HFXT                                         19DEC02.1917
                  QFX(I,J)=QFX(I,J)+QFXT                                         19DEC02.1918
                ELSEIF(SEAICE(I,J).GT.0.02)THEN                                  19DEC02.1919
#ifndef IBMopt                                                                   23DEC04.3871
                  ESGW=SVP1*EXP(SVP2*(271.4-SVPT0)/(271.4-SVP3))                 19DEC02.1920
#endif                                                                           23DEC04.3872
                  QSGW=EP2*ESGW/(PS-ESGW)                                        19DEC02.1921
#ifndef IBMopt                                                                   23DEC04.3873
                  ESGI=.611*EXP(22.514-6.15E3/TSLB(I,J,1))                       19DEC02.1922
#else                                                                            23DEC04.3874
                  ESGI = temp1(i)                                                23DEC04.3875
#endif                                                                           23DEC04.3876
                  QSGI=.622*ESGI/(PS-ESGI)                                       19DEC02.1923
#ifndef IBMopt                                                                   23DEC04.3877
                  HFXTW=FLHC(I)*((271.4/PISFC(I))-THX(I,KL))                     19DEC02.1924
#else                                                                            23DEC04.3878
                  HFXTW=FLHC(I)*((271.4*PISFC(I))-THX(I,KL))                     23DEC04.3879
#endif                                                                           23DEC04.3880
                  HFXTI=FLHC(I)*(THG-THX(I,KL))                                  19DEC02.1925
                  QFXTW=FLQC(I)*(QSGW-QX(I,KL))                                  19DEC02.1926
                  QFXTI=FLQC(I)*(QSGI-QX(I,KL))                                  19DEC02.1927
                  QS(I)=HFXTI+XMOIST(INEST)*QFXTI*2.836E+06                      19DEC02.1928
                  HFX(I,J)=HFX(I,J)+HFXTI*SEAICE(I,J)+                           19DEC02.1929
     +              HFXTW*(1.-SEAICE(I,J))                                       19DEC02.1930
                  QFX(I,J)=QFX(I,J)+QFXTI*SEAICE(I,J)+                           19DEC02.1931
     +              QFXTW*(1.-SEAICE(I,J))                                       19DEC02.1932
                  HFXSI(I,J)=HFXSI(I,J)+HFXTI                                    19DEC02.1933
                  QFXSI(I,J)=QFXSI(I,J)+QFXTI                                    19DEC02.1934
                ELSE                                                             19DEC02.1935
#ifndef IBMopt                                                                   23DEC04.3881
                  ESG=SVP1*EXP(SVP2*(TSLB(I,J,1)-SVPT0)/                         19DEC02.1936
     -                (TSLB(I,J,1)-SVP3))                                        19DEC02.1937
#else                                                                            23DEC04.3882
                  esg = temp2(i)                                                 23DEC04.3883
#endif                                                                           23DEC04.3884
                  QSG=EP2*ESG/(PS-ESG)                                           19DEC02.1938
C     UPDATE FLUXES FOR NEW GROUND TEMPERATURE                                   SLAB.123
                  HFXT=FLHC(I)*(THG-THX(I,KL))                                   19DEC02.1939
                  QFXT=FLQC(I)*(QSG-QX(I,KL))                                    19DEC02.1940
                  QS(I)=HFXT+XMOIST(INEST)*QFXT*XLV                              19DEC02.1941
C     SUM HFX AND QFX OVER SOIL TIMESTEPS                                        SLAB.127
                  HFX(I,J)=HFX(I,J)+HFXT                                         19DEC02.1942
                  QFX(I,J)=QFX(I,J)+QFXT                                         19DEC02.1943
                ENDIF                                                            19DEC02.1944
              ENDIF                                                              SLAB.130
              FLUX(I,1)=RNET(I)-QS(I)                                            SLAB.131
              IF((SNOWC(I,J).GT.0.5).AND.(L.LE.2).AND.IPOLAR.EQ.1)THEN           19DEC02.1945
C        THERMAL DIFFUSIVITY THRU SNOW (YEN 1981)                                19DEC02.1946
CB               DIFSNW=2.22362*(0.5*(DENSNW(L)+DENSNW(L+1)))**1.885             23DEC04.3885
               DIFSNW=exp(1.885*log(zc*(DENSNW(L)+DENSNW(L+1))))                 23DEC04.3886
               FLUX(I,L+1)=-DIFSNW*(TSLB(I,J,L+1)-TSLB(I,J,L))                   19DEC02.1948
     +           /(ZS(L+1)-ZS(L))                                                19DEC02.1949
               DTSDT(I)=-(FLUX(I,L+1)-FLUX(I,L))/                                19DEC02.1950
     +           (DZS(L)*(DENSNW(L)/0.917)*917.*32000./18.)                      19DEC02.1951
              ELSEIF(NINT(SATBRT(I,J)).EQ.ISICE.AND.IPOLAR.EQ.1)THEN             19DEC02.1952
CB                DIFSNW=2.22362*(0.5*(DENSNW(L)+DENSNW(L+1)))**1.885            23DEC04.3887
               DIFSNW=exp(1.885*log(zc*(DENSNW(L)+DENSNW(L+1))))                 23DEC04.3888
                FLUX(I,L+1)=-DIFSNW*(TSLB(I,J,L+1)-TSLB(I,J,L))                  19DEC02.1954
     +            /(ZS(L+1)-ZS(L))                                               19DEC02.1955
                DTSDT(I)=-(FLUX(I,L+1)-FLUX(I,L))/                               19DEC02.1956
     +            (DZS(L)*(DENSNW(L)/0.917)*917.*34000./18.)                     19DEC02.1957
              ELSEIF(SEAICE(I,J).GT.0.02)THEN                                    19DEC02.1958
                DIFSNW=1.9                                                       19DEC02.1959
                FLUX(I,L+1)=-DIFSNW*(TSLB(I,J,L+1)-TSLB(I,J,L))                  19DEC02.1960
     +            /(ZS(L+1)-ZS(L))                                               19DEC02.1961
                DTSDT(I)=-(FLUX(I,L+1)-FLUX(I,L))/(DZS(L)*CAPG(I,J))             19DEC02.1962
              ELSE                                                               19DEC02.1963
                FLUX(I,L+1)=-DIFSL*CAPG(I,J)*(TSLB(I,J,L+1)                      19DEC02.1964
     +                      -TSLB(I,J,L))/(ZS(L+1)-ZS(L))                        19DEC02.1965
                DTSDT(I)=-(FLUX(I,L+1)-FLUX(I,L))/(DZS(L)*CAPG(I,J))             19DEC02.1966
              ENDIF                                                              19DEC02.1967
              TSLB(I,J,L)=TSLB(I,J,L)+DTSDT(I)*DELTSM*RNSOIL                     SLAB.135
              IF(TSLB(I,J,L).GT.400..OR.TSLB(I,J,L).LT.100.)THEN                 07NOV00.2064
                PRINTWARN=.TRUE.                                                 07NOV00.2065
                IPW=I                                                            07NOV00.2066
              ENDIF                                                              07NOV00.2067
              IF(NINT(SATBRT(I,J)).EQ.ISICE.AND.IPOLAR.EQ.1)THEN                 19DEC02.1968
C MELT PERMANENT ICE IF TSLB.GT.273.155                                          19DEC02.1969
                IF(TSLB(I,J,L).GT.273.155)THEN                                   19DEC02.1970
                  TSLB(I,J,L)=273.155                                            19DEC02.1971
                ENDIF                                                            19DEC02.1972
              ELSEIF(SEAICE(I,J).GT.0.02)THEN                                    19DEC02.1973
C MELT SEA ICE IF TSLB.GT.271.4 (AND SET SEA ICE DEPTH)                          19DEC02.1974
                TSLB(I,J,MLX)=271.4                                              19DEC02.1975
                IF(TSLB(I,J,L).GT.273.155)TSLB(I,J,L)=273.155                    19DEC02.1976
                IF((XLAT(I,J).GT.0.).AND.(SEAICE(I,J).LT.0.9)                    19DEC02.1977
     +            .AND.(L.EQ.7))THEN                                             19DEC02.1978
                  TSLB(I,J,L)=271.4                                              19DEC02.1979
                ENDIF                                                            19DEC02.1980
                IF((XLAT(I,J).GT.0.).AND.(SEAICE(I,J).LT.0.6)                    19DEC02.1981
     +            .AND.(L.EQ.6))THEN                                             19DEC02.1982
                  TSLB(I,J,L)=271.4                                              19DEC02.1983
                ENDIF                                                            19DEC02.1984
                IF((XLAT(I,J).LT.0.).AND.(L.GE.6))THEN                           19DEC02.1985
                  TSLB(I,J,L) = 271.4                                            19DEC02.1986
                ENDIF                                                            19DEC02.1987
              ENDIF                                                              19DEC02.1988
              IF(IFSNOW(INEST).GE.1.AND.L.EQ.1)THEN                              19DEC02.1989
                IF((SNOWC(I,J).GT.0..AND.TSLB(I,J,1).GT.273.16))THEN             SLAB.137
                  if(ifsnow(inest).eq.2)then                                     19DEC02.1990
c     snow melts                                                                 19DEC02.1991
c     maximum available snow for melting is taken as 1 mm/hr (liquid equiv)      19DEC02.1992
c      0.5 accounts for leapfrog step to avoid double-counting snow melt         19DEC02.1993
                    avsnow=amin1(snowc(i,j),0.5*deltsm*rnsoil/3600.)             19DEC02.1994
c     reduce available snow for forests and urban (large roughness length)       19DEC02.1995
                    zntfac=amin1(1.0,0.1/znt(i,j))                               19DEC02.1996
                    avsnow=zntfac*avsnow                                         19DEC02.1997
                    dstemp=TSLB(I,J,1)-273.16                                    19DEC02.1998
                    if(avsnow.gt.capg(i,j)*dzs(1)*dstemp/xlf0)then               19DEC02.1999
c     enough available snow to cool tg to 0 C                                    19DEC02.2000
                      snowc(i,j)=snowc(i,j)-capg(i,j)*dzs(1)*dstemp/xlf0         19DEC02.2001
                      tslb(i,j,1)=273.16                                         19DEC02.2002
                    else                                                         19DEC02.2003
c     not enough available snow to cool tg to 0 C                                19DEC02.2004
c         so cool to melt all available snow                                     19DEC02.2005
                      tslb(i,j,1)=tslb(i,j,1)-xlf0*avsnow/                       19DEC02.2006
     -                            (capg(i,j)*dzs(1))                             19DEC02.2007
                      snowc(i,j)=snowc(i,j)-avsnow                               19DEC02.2008
                    endif                                                        19DEC02.2009
                  else                                                           19DEC02.2010
                    tslb(i,j,1)=273.16                                           19DEC02.2011
                  endif                                                          19DEC02.2012
                ENDIF                                                            SLAB.139
              ENDIF                                                              SLAB.140
              IF(L.EQ.1)DTHGDT(I)=DTHGDT(I)+RNSOIL*DTSDT(I)                      SLAB.141
              IF(ITSOIL.EQ.NSOIL.AND.L.EQ.1)THEN                                 SLAB.142
C     AVERAGE HFX AND QFX OVER SOIL TIMESTEPS FOR OUTPUT TO PBL                  SLAB.143
                HFX(I,J)=HFX(I,J)*RNSOIL                                         SLAB.144
                QFX(I,J)=QFX(I,J)*RNSOIL                                         SLAB.145
                IF(SEAICE(I,J).GT.0.02)THEN                                      19DEC02.2013
                  HFXSI(I,J)=HFXSI(I,J)*RNSOIL                                   19DEC02.2014
                  QFXSI(I,J)=QFXSI(I,J)*RNSOIL                                   19DEC02.2015
                ENDIF                                                            19DEC02.2016
              ENDIF                                                              SLAB.146
   75         CONTINUE                                                           SLAB.147
            ENDDO                                                                SLAB.148
            IF(PRINTWARN)THEN                                                    07NOV00.2068
              PRINT *,'WARNING IN SLAB: SOIL TEMP AT ',IPW,J,L,'=',              07NOV00.2069
     -                 TSLB(IPW,J,L)                                             07NOV00.2070
            ENDIF                                                                07NOV00.2071
          ENDDO                                                                  SLAB.149
        ENDDO                                                                    SLAB.150
      ENDIF                                                                      SLAB.151
C                                                                                SLAB.152
      DO 80 I=IST,IEN                                                            07NOV00.2072
        IF(XLD1(I).LT.0.5)GOTO 80                                                SLAB.154
        TGB(I,J)=TG0(I)+DELTSM*DTHGDT(I)                                         07NOV00.2073
        IF(NINT(SATBRT(I,J)).EQ.ISICE.AND.IPOLAR.EQ.1)THEN                       19DEC02.2017
          IF(TGB(I,J).GT.273.155)THEN                                            19DEC02.2018
            TGB(I,J)=273.155                                                     19DEC02.2019
          ENDIF                                                                  19DEC02.2020
        ELSEIF(SEAICE(I,J).GT.0.02)THEN                                          19DEC02.2021
            TGSI(I,J)=TGSI(I,J)+DELTSM*DTHGDT(I)                                 19DEC02.2022
            DTHGDT(I)=SEAICE(I,J)*DTHGDT(I)                                      19DEC02.2023
            TGB(I,J)=TG0(I)+DELTSM*DTHGDT(I)                                     19DEC02.2024
            TGBXX=SEAICE(I,J)*273.155+(1.-SEAICE(I,J))*271.4                     19DEC02.2025
            IF(TGB(I,J).GT.TGBXX)THEN                                            19DEC02.2026
              TGB(I,J)=TGBXX                                                     19DEC02.2027
            ENDIF                                                                19DEC02.2028
            IF(TGSI(I,J).GT.273.155)THEN                                         19DEC02.2029
              TGSI(I,J)=273.155                                                  19DEC02.2030
            ENDIF                                                                19DEC02.2031
        ENDIF                                                                    19DEC02.2032
   80 CONTINUE                                                                   SLAB.158
C                                                                                SLAB.159
C-----MODIFY THE THE GROUND TEMPERATURE IF THE SNOW COVER EFFECTS ARE            SLAB.160
C     CONSIDERED: LIMIT THE GROUND TEMPERATURE UNDER 0 C.                        SLAB.161
C                                                                                SLAB.162
      IF(IFSNOW(INEST).EQ.0)GOTO 90                                              SLAB.163
      DO 85 I=IST,IEN                                                            07NOV00.2074
        IF(XLD1(I).LT.0.5)GOTO 85                                                SLAB.165
        TSCVN(I)=TGB(I,J)                                                        07NOV00.2075
        IF((SNOWC(I,J).GT.0..AND.TSCVN(I).GT.273.16))THEN                        SLAB.168
          if(ifsnow(inest).eq.2.and.isoil.eq.0)then                              19DEC02.2033
c     snow melts                                                                 19DEC02.2034
c     maximum available snow for melting is taken as 1 mm/hr (liquid equiv)      19DEC02.2035
c      0.5 accounts for leapfrog step to avoid double-counting snow melt         19DEC02.2036
            avsnow=amin1(snowc(i,j),0.5*deltsm/3600.)                            19DEC02.2037
c     reduce available snow for forests and urban (large roughness length)       19DEC02.2038
            zntfac=amin1(1.0,0.1/znt(i,j))                                       19DEC02.2039
            avsnow=zntfac*avsnow                                                 19DEC02.2040
            dstemp=TSCVN(I)-273.16                                               19DEC02.2041
            if(avsnow.gt.capg(i,j)*dzs(1)*dstemp/xlf0)then                       19DEC02.2042
c     enough available snow to cool tg to 0 C                                    19DEC02.2043
              snowc(i,j)=snowc(i,j)-capg(i,j)*dzs(1)*dstemp/xlf0                 19DEC02.2044
              TSCVN(I)=273.16                                                    19DEC02.2045
            else                                                                 19DEC02.2046
c     not enough available snow to cool tg to 0 C                                19DEC02.2047
c         so cool to melt all available snow                                     19DEC02.2048
              TSCVN(I)=TSCVN(I)-xlf0*avsnow/(capg(i,j)*dzs(1))                   19DEC02.2049
              snowc(i,j)=snowc(i,j)-avsnow                                       19DEC02.2050
            endif                                                                19DEC02.2051
          else if(ifsnow(inest).eq.2.and.isoil.eq.1)then                         19DEC02.2052
c   snow melt already accounted for so just set ground temp                      19DEC02.2053
            TSCVN(I)=TSLB(I,J,1)                                                 19DEC02.2054
          else                                                                   19DEC02.2055
            TSCVN(I)=273.16                                                      19DEC02.2056
          endif                                                                  19DEC02.2057
        ENDIF                                                                    SLAB.172
        TGB(I,J)=TSCVN(I)                                                        07NOV00.2076
      if(ifsnow(inest).eq.2)then                                                 19DEC02.2058
c      snow sublimates proportionally to QFX                                     19DEC02.2059
        snowc(i,j)=amax1(snowc(i,j)-0.5*qfx(i,j)*deltsm,0.0)                     19DEC02.2060
      endif                                                                      19DEC02.2061
   85 CONTINUE                                                                   SLAB.174
C                                                                                SLAB.175
   90 CONTINUE                                                                   SLAB.176
C                                                                                SLAB.177
      IF(INEST.EQ.1)THEN                                                         SLAB.178
        XXT=XTIME+DTMIN                                                          SLAB.179
      ELSE                                                                       SLAB.180
        XXT=XTIME+FLOAT(IRAX)*DTMIN                                              SLAB.181
      ENDIF                                                                      SLAB.182
C                                                                                SLAB.183
  140 CONTINUE                                                                   SLAB.184
C                                                                                07NOV00.2077
C UPDATE MOISTURE AVAILABILITY                                                   07NOV00.2078
C                                                                                07NOV00.2079
      IF (IMOIAV(INEST).GE.1) THEN                                               07NOV00.2080
        TIMFAC = 1.                                                              07NOV00.2081
        IF (IBLTYP(INEST).EQ.2.OR.IBLTYP(INEST).EQ.6)                            19DEC02.2062
     -      TIMFAC = DELTSM/DT                                                   19DEC02.2063
C                                                                                07NOV00.2083
        DO 200 I=IST,IEN                                                         07NOV00.2084
C                                                                                07NOV00.2085
C ONLY COMPUTE MAVAIL OVER LAND AND WHERE THERE IS NO SNOW COVER                 07NOV00.2086
C                                                                                07NOV00.2087
          IF (XLAND(I,J)-1.5 .LT. 0. .AND. SNOWC(I,J) .LE. 0.) THEN              07NOV00.2088
            LLUS = SATBRT(I,J)                                                   07NOV00.2089
C                                                                                07NOV00.2090
C IF IT'S PERMANENT ICE, DON'T CHANGE IT                                         07NOV00.2091
C                                                                                07NOV00.2092
            IF (LLUS .NE. ISICE) THEN                                            07NOV00.2093
C                                                                                07NOV00.2094
C RAIN TENDENCY IN CM IS IN ARRAY RAINP FROM SOLVE, ADJUSTED FOR                 07NOV00.2095
C      MITER LOOP FOR BLACKADAR SCHEME                                           07NOV00.2096
C                                                                                07NOV00.2097
            PRECIP = RAINP(I,J) * TIMFAC                                         07NOV00.2098
C                                                                                07NOV00.2099
C IF WE'VE ACCUMULATED A MM OF ACCUM PRECIP (1 CM OF SNOW) AND IT'S              05DEC01.1316
C    COLD, PUT DOWN A SNOW COVER...                                              07NOV00.2101
C                                                                                07NOV00.2102
            IF (T3D(I,J,KL) .le. 270.94 .AND. PRECIP .GT. 0. .and.               05DEC01.1317
     &          (RAINC(I,J) + RAINNC(I,J)) .GT. 0.1 ) THEN                       05DEC01.1318
C                                                                                07NOV00.2105
C    CHANGE THE ALBEDO, ETC. TO ACCOUNT FOR SNOW                                 07NOV00.2106
C                                                                                07NOV00.2107
              SNOWC(I,J) = 1.                                                    07NOV00.2108
              THC(I,J)=.05                                                       07NOV00.2109
              EMISS(I,J)=.95                                                     07NOV00.2110
              MAVAIL(I,J)=.95                                                    07NOV00.2111
              ALB(I,J)=ALB(I,J)+SCFX(LLUS)*ALB(I,J)                              07NOV00.2112
C             WRITE(6,*) 'SET SNOW COVER AT I = ',I,' J = ',J                    07NOV00.2113
              GO TO 210                                                          07NOV00.2114
            ENDIF                                                                07NOV00.2115
C                                                                                07NOV00.2116
C RUNOFF CODE:                                                                   07NOV00.2117
C FOR URBAN AND DESERT AREAS, THE FRACTION SHOULD BE HIGHER. FOR MARSHES,        07NOV00.2118
C    IT COULD BE LOWER. FOR LOW RAIN RATES, NOTHING RUNS OFF. FOR HIGH           07NOV00.2119
C    RAIN RATES, MUCH OF IT RUNS OFF. FOR DRY GROUND NOT MUCH RUNS OFF,          07NOV00.2120
C    FOR SOAKED GROUND, ALL OF IT RUNS OFF.                                      07NOV00.2121
C    BUT CURRENTLY, THE RUNOFF FACTOR IS SET TO 0.5 FOR ALL LANDUSE TYPES.       07NOV00.2122
C                                                                                07NOV00.2123
            RUNOFF = 0.5 * PRECIP                                                07NOV00.2124
C                                                                                07NOV00.2125
C EVAPORATION ( CM PER CM**2 PER TIMESTEP)                                       07NOV00.2126
C                                                                                07NOV00.2127
            EVAP = (QFX(I,J)*1000./10000.*0.5*DT) * ERATE(LLUS)                  07NOV00.2128
C                                                                                07NOV00.2129
C THE MODEL IS EVAPORATING TOO MUCH. CUT THE EVAP IN HALF. JFB 5/30/99           07NOV00.2130
C                                                                                07NOV00.2131
C           EVAP = EVAP * .5                                                     07NOV00.2132
C                                                                                07NOV00.2133
C CALCULATE THE MOISTURE AVAILABILITY INCREMENT                                  07NOV00.2134
C                                                                                07NOV00.2135
            XMM = .25 * (PRECIP - EVAP - RUNOFF)                                 07NOV00.2136
C                                                                                07NOV00.2137
C         IF (I .EQ. 15 .AND. J .EQ. 22) THEN                                    07NOV00.2138
C         IF (XTIME .GT. 180. .AND. XTIME .LT. 720 .AND.                         07NOV00.2139
C    &        XMM .GT. 1.E-3 .AND. J.GE.15 .AND. I.LE.25) THEN                   07NOV00.2140
C           WRITE(6,*) 'I = ',I,' J = ',J,' P = ', PRECIP                        07NOV00.2141
C           WRITE(6,*) 'R = ',RUNOFF,' E = ',EVAP,' QFX = ',QFX(I,J)             07NOV00.2142
C           WRITE(6,*) 'OLD MAVAIL = ',MAVAIL(I,J)                               07NOV00.2143
C         ENDIF                                                                  07NOV00.2144
C         ENDIF                                                                  07NOV00.2145
C                                                                                07NOV00.2146
            AVAIL = MAVAIL(I,J) + XMM                                            07NOV00.2147
C                                                                                07NOV00.2148
C RESTRICT THE MAVAIL TO LIMITS THAT DEPEND ON THE LAND USE CATEGORY.            07NOV00.2149
C                                                                                07NOV00.2150
            MAVAIL(I,J) = AMIN1(AVAIL,MAMAX(LLUS))                               07NOV00.2151
            MAVAIL(I,J) = AMAX1(MAVAIL(I,J),MAMIN(LLUS))                         07NOV00.2152
C                                                                                07NOV00.2153
C         IF (I .EQ. 15 .AND. J .EQ. 22) THEN                                    07NOV00.2154
C         IF (XTIME .GT. 180. .AND. XTIME .LT. 720 .AND.                         07NOV00.2155
C    &        XMM .GT. 1.E-3 .AND. J.GE.15 .AND. I.LE.25) THEN                   07NOV00.2156
C           WRITE(6,*) 'NEW MAVAIL = ',MAVAIL(I,J)                               07NOV00.2157
C           WRITE(6,*) 'LLUS = ',LLUS,' MIN = ',MAMIN(LLUS),                     07NOV00.2158
C    & ' MAX = ',MAMAX(LLUS)                                                     07NOV00.2159
C         ENDIF                                                                  07NOV00.2160
C         ENDIF                                                                  07NOV00.2161
C                                                                                07NOV00.2162
            END IF                                                               07NOV00.2163
          END IF                                                                 07NOV00.2164
  210   CONTINUE                                                                 07NOV00.2165
  200   CONTINUE                                                                 07NOV00.2166
                                                                                 07NOV00.2167
      END IF                                                                     07NOV00.2168
      RETURN                                                                     SLAB.185
      END                                                                        SLAB.186
