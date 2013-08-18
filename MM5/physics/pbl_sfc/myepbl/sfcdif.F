C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&         SFCDIF.1
      SUBROUTINE SFCDIF(LMHK,ISM,THS,QS,UZ0,VZ0,THZ0,QZ0                         SFCDIF.2
     &,                 USTAR,WSTAR,Z0,WETM,AKMS,AKHS,HPBL,CT                    SFCDIF.3
     &,                 U10,V10,TH02,TH10,Q02,Q10                                SFCDIF.4
     &,                 ULM,VLM,T,Q,APE,Z,HSFLX,HLFLX,AKHS02                     19DEC02.1769
     &,                 HSFLXSI,HLFLXSI,SFCTYP,SIFRAC                            19DEC02.1770
     &,                 THSI,QSSI,THWT,QSWT)                                     19DEC02.1771
C     ******************************************************************         SFCDIF.6
C     *                                                                *         SFCDIF.7
C     *                        SURFACE LAYER                           *         SFCDIF.8
C     *                                                                *         SFCDIF.9
C     ******************************************************************         SFCDIF.10
C     Ammended to use the "Effective roughness" of Mason (1986, see              SFCDIF.11
C     Georgelin et al., MWR July 1994), by FM, RW, June 1995                     SFCDIF.12
C-----------------------------------------------------------------------         SFCDIF.13
#     include <parame.incl>                                                      SFCDIF.14
      PARAMETER (LM=MKX)                                                         SFCDIF.15
C-----------------------------------------------------------------------         SFCDIF.16
                             P A R A M E T E R                                   SFCDIF.17
     &(LP1=LM+1)                                                                 SFCDIF.18
C-----------------------------------------------------------------------         SFCDIF.19
                             P A R A M E T E R                                   SFCDIF.20
     &(WWST=1.2,WWST2=WWST*WWST,G=9.8,USTFC=0.018/G                              SFCDIF.21
     &,VKRM=0.40,RIC=0.183,RFC=0.191,FHNEU=0.8                                   SFCDIF.22
     &,RRIC=1.0/RIC,RFAC=RIC/(FHNEU*RFC*RFC),EXCM=0.000015                       SFCDIF.23
     &,BETA=1./270.,BTG=BETA*G                                                   SFCDIF.24
     &,ELFC=VKRM*BTG,CNV=0.608*G/BTG                                             SFCDIF.25
     &,WOLD=.15,WNEW=1.-WOLD,ITRMX=05                                            SFCDIF.26
     &,PIHF=3.14159265/2.,PIFR=3.14159265/4.                                     SFCDIF.27
C-----------------------------------------------------------------------         SFCDIF.28
     &,EPSU2=1.E-4,EPSUST=0.07,EPSIT=1.E-4,EPSA=1.E-8                            SFCDIF.29
     &,ZTMIN=-5.,ZTMAX=1.                                                        SFCDIF.30
C-----------------------------------------------------------------------         SFCDIF.31
     &,SMALL=0.35, GLKBS=30.0,GLKBR=10.0,GRRS=GLKBR/GLKBS                        SFCDIF.32
     &,CZIV=SMALL*GLKBS                                                          SFCDIF.33
     &,VISC=1.5E-5, TVISC=2.1E-5, QVISC=2.1E-5                                   SFCDIF.34
     &,RVISC=1./VISC,RTVISC=1./TVISC,RQVISC=1./QVISC                             SFCDIF.35
     &,SQPR=0.84,SQSC=0.84,ZQRZT=SQSC/SQPR                                       SFCDIF.36
     &,USTR=0.225,USTC=0.7                                                       SFCDIF.37
     &,FZU1=CZIV*VISC,FZT1=RVISC *TVISC*SQPR,   FZQ1=RTVISC*QVISC*ZQRZT          SFCDIF.38
     &,               FZT2=CZIV*GRRS*TVISC*SQPR,FZQ2=RTVISC*QVISC*ZQRZT          SFCDIF.39
C-----------------------------------------------------------------------         SFCDIF.40
     &,ZTFC=1.0                                                                  SFCDIF.41
     &,CZIL=.1000,SQVISC=258.2,ZILFC=-CZIL*VKRM*SQVISC)                          SFCDIF.42
C-----------------------------------------------------------------------         SFCDIF.43
                             D I M E N S I O N                                   SFCDIF.44
     & T     (LM),Q     (LM)                                                     SFCDIF.45
                             D I M E N S I O N                                   SFCDIF.46
     & APE   (LM)                                                                SFCDIF.47
     &,Z     (LP1)                                                               SFCDIF.48
C-----------------------------------------------------------------------         SFCDIF.49
      REAL HSFLXSI,HLFLXSI,SFCTYP,SIFRAC,THSI,QSSI,THWT,QSWT                     19DEC02.1772
      REAL HSFLXWT,HLFLXWT                                                       19DEC02.1773
      PSLMU(ZZ)=-0.96*ALOG(1.0-4.5*ZZ)                                           SFCDIF.50
      PSLMS(ZZ)=ZZ*RRIC-2.076*(1.-1./(ZZ+1.))                                    SFCDIF.51
      PSLHU(ZZ)=-0.96*ALOG(1.0-4.5*ZZ)                                           SFCDIF.52
      PSLHS(ZZ)=ZZ*RFAC-2.076*(1.-1./(ZZ+1.))                                    SFCDIF.53
C                                                                                SFCDIF.54
      PSPMU(XX)=-2.*ALOG((XX+1.)*0.5)-ALOG((XX*XX+1.)*0.5)+2.*ATAN(XX)           SFCDIF.55
     &          -PIHF                                                            SFCDIF.56
      PSPMS(YY)=5.*YY                                                            SFCDIF.57
      PSPHU(XX)=-2.*ALOG((XX*XX+1.)*0.5)                                         SFCDIF.58
      PSPHS(YY)=5.*YY                                                            SFCDIF.59
C***********************************************************************         SFCDIF.60
C                                                                                19DEC02.1774
C Initialize sea ice heat flux arrays to zero                                    19DEC02.1775
C                                                                                19DEC02.1776
      HSFLXSI=0.                                                                 19DEC02.1777
      HLFLXSI=0.                                                                 19DEC02.1778
      HSFLXWT=0.                                                                 19DEC02.1779
      HLFLXWT=0.                                                                 19DEC02.1780
c     write(6,*) 'sfctyp = ',sfctyp,' SIFRAC = ',SIFRAC,' z0 = ',z0              19DEC02.1781
      LMHP=LMHK+1                                                                SFCDIF.61
C                                                                                SFCDIF.62
      THLM=T(LMHK)*APE(LMHK)                                                     SFCDIF.63
      QLM=Q(LMHK)                                                                SFCDIF.64
C-----------------------------------------------------------------------         SFCDIF.65
      Z0=(1-ISM)*Z0+ISM*AMAX1(USTFC*USTAR*USTAR,1.59E-5)                         SFCDIF.66
C--------------VISCOUS SUBLAYER-----------------------------------------         SFCDIF.67
      IF(FLOAT(ISM).GT.0.5.AND.USTAR.LT.USTC)THEN                                SFCDIF.68
C-----------------------------------------------------------------------         SFCDIF.69
        IF(USTAR.LT.USTR)THEN                                                    SFCDIF.70
C                                                                                SFCDIF.71
          ZU=FZU1*SQRT(SQRT(Z0*USTAR*RVISC))/USTAR                               SFCDIF.72
c         write(6,*) 'at sfcdif.72, zu = ',zu                                    19DEC02.1782
          WGHT=AKMS*ZU*RVISC                                                     SFCDIF.73
          RWGH=WGHT/(WGHT+1.)                                                    SFCDIF.74
c         UZ0=(ULM*RWGH+UZ0)*0.5                                                 SFCDIF.75
c         VZ0=(VLM*RWGH+VZ0)*0.5                                                 SFCDIF.76
          UZ0=ULM*RWGH                                                           SFCDIF.77
          VZ0=VLM*RWGH                                                           SFCDIF.78
C                                                                                SFCDIF.79
          ZT=FZT1*ZU                                                             SFCDIF.80
          WGHT=AKHS*ZT*RTVISC                                                    SFCDIF.81
c         THZ0=((WGHT*THLM+THS)/(WGHT+1.)+THZ0)*0.5                              SFCDIF.82
          THZ0=(WGHT*THLM+THS)/(WGHT+1.)                                         SFCDIF.83
C                                                                                SFCDIF.84
          ZQ=FZQ1*ZT                                                             SFCDIF.85
          WGHT=AKHS*ZQ*RQVISC                                                    SFCDIF.86
c         QZ0 =((WGHT*QLM+QS)/(WGHT+1.)+QZ0)*0.5                                 SFCDIF.87
          QZ0=(WGHT*QLM+QS)/(WGHT+1.)                                            SFCDIF.88
        ENDIF                                                                    SFCDIF.89
cwrite(6,*) 'at sfcdif.89, zu = ',zu                                             19DEC02.1783
C                                                                                SFCDIF.90
        IF(USTAR.GE.USTR.AND.USTAR.LT.USTC)THEN                                  SFCDIF.91
C                                                                                SFCDIF.92
          ZU=Z0                                                                  SFCDIF.93
cwrite(6,*) 'at sfcdif.93, zu = ',zu                                             19DEC02.1784
          UZ0=0.                                                                 SFCDIF.94
          VZ0=0.                                                                 SFCDIF.95
C                                                                                SFCDIF.96
          ZT=FZT2*SQRT(SQRT(Z0*USTAR*RVISC))/USTAR                               SFCDIF.97
          WGHT=AKHS*ZT*RTVISC                                                    SFCDIF.98
c         THZ0=((WGHT*THLM+THS)/(WGHT+1.)+THZ0)*0.5                              SFCDIF.99
          THZ0=(WGHT*THLM+THS)/(WGHT+1.)                                         SFCDIF.100
C                                                                                SFCDIF.101
          ZQ=FZQ2*ZT                                                             SFCDIF.102
          WGHT=AKHS*ZQ*RQVISC                                                    SFCDIF.103
c         QZ0 =((WGHT*QLM+QS)/(WGHT+1.)+QZ0)*0.5                                 SFCDIF.104
          QZ0=(WGHT*QLM+QS)/(WGHT+1.)                                            SFCDIF.105
        ENDIF                                                                    SFCDIF.106
      ELSE                                                                       SFCDIF.107
C-----------------------------------------------------------------------         SFCDIF.108
C-----------------------------------------------------------------------         SFCDIF.109
        ZU=Z0                                                                    SFCDIF.110
cwrite(6,*) 'at sfcdif.110, zu = ',zu                                            19DEC02.1785
        UZ0=0.                                                                   SFCDIF.111
        VZ0=0.                                                                   SFCDIF.112
C                                                                                SFCDIF.113
        ZT=Z0                                                                    SFCDIF.114
        THZ0=THS                                                                 SFCDIF.115
C                                                                                SFCDIF.116
        ZQ=Z0                                                                    SFCDIF.117
        QZ0=QS                                                                   SFCDIF.118
      ENDIF                                                                      SFCDIF.119
C-----------------------------------------------------------------------         SFCDIF.120
C-----------------------------------------------------------------------         SFCDIF.121
      ZSL=(Z(LMHK)-Z(LMHP))*0.5                                                  SFCDIF.122
      RDZ=1./ZSL                                                                 SFCDIF.123
      CXCH=EXCM*RDZ                                                              SFCDIF.124
C-----------------------------------------------------------------------         SFCDIF.125
      IF(ISM.GT.0)THEN                                                           SFCDIF.126
        DTHV=(0.608*QLM+1.)*THLM-(0.608*QZ0+1.)*THZ0                             SFCDIF.127
      ELSE                                                                       SFCDIF.128
        DTHV=(QLM-QZ0)*CNV*WETM+THLM-THZ0                                        SFCDIF.129
        ZT=Z0*ZTFC                                                               SFCDIF.130
      ENDIF                                                                      SFCDIF.131
C                                                                                SFCDIF.132
      DU2=AMAX1((ULM-UZ0)**2+(VLM-VZ0)**2,EPSU2)                                 SFCDIF.133
C-----------------------------------------------------------------------         SFCDIF.134
      RIB=BTG*DTHV*ZSL/DU2                                                       SFCDIF.135
C--------------BELJARS CORRECTION OF USTAR------------------------------         SFCDIF.136
      BTGH=BTG*HPBL                                                              SFCDIF.137
      WSTAR2=WWST2*ABS(BTGH*AKHS*DTHV)**(2./3.)                                  SFCDIF.138
      USTAR=AMAX1(SQRT(AKMS*SQRT(DU2+WSTAR2)),EPSUST)                            SFCDIF.139
C--------------ZILITINKEVITCH FIX FOR ZT--------------------------------         SFCDIF.140
      IF(ISM.LT.1)ZT=EXP(ZILFC*SQRT(USTAR*Z0))*Z0                                SFCDIF.141
C-----------------------------------------------------------------------         SFCDIF.142
c  switching off turbulence for Ri>=RIC everywhere                               SFCDIF.143
      IF(ISM.GT.0.AND.RIB.GE.RIC)THEN                                            SFCDIF.144
c     IF(RIB.GE.RIC)THEN                                                         SFCDIF.145
C-----------------------------------------------------------------------         SFCDIF.146
        AKMS=AMAX1(VISC*RDZ,CXCH)                                                SFCDIF.147
        AKHS=AMAX1(TVISC*RDZ,CXCH)                                               SFCDIF.148
      ELSE                                                                       SFCDIF.149
C-----------------------------------------------------------------------         SFCDIF.150
C-----------------------------------------------------------------------         SFCDIF.151
cwrite(6,*) 'zsl = ',zsl,' zu = ',zu                                             19DEC02.1786
        ZSLU=ZSL+ZU                                                              SFCDIF.152
cwrite(6,*) 'zsl = ',zsl,' zt = ',zt                                             19DEC02.1787
        ZSLT=ZSL+ZT                                                              SFCDIF.153
C                                                                                SFCDIF.154
        RLOGU=ALOG(ZSLU/ZU)                                                      SFCDIF.155
        RLOGT=ALOG(ZSLT/ZT)                                                      SFCDIF.156
C                                                                                SFCDIF.157
        RLMO=ELFC*AKHS*DTHV/USTAR**3                                             SFCDIF.158
C--------------SEA POINTS FIRST ... ------------------------------------         SFCDIF.159
        IF(ISM.GT.0)THEN                                                         SFCDIF.160
          DO 100 ITR=1,ITRMX                                                     SFCDIF.161
C--------------1./MONIN-OBUKKHOV LENGTH-SCALE---------------------------         SFCDIF.162
            ZETALT=AMAX1(ZSLT*RLMO,ZTMIN)                                        SFCDIF.163
            RLMO=ZETALT/ZSLT                                                     SFCDIF.164
            ZETALU=ZSLU*RLMO                                                     SFCDIF.165
C                                                                                SFCDIF.166
            ZETAU=ZU*RLMO                                                        SFCDIF.167
            ZETAT=ZT*RLMO                                                        SFCDIF.168
C--------------LL FUNCTIONS OVER SEA------------------------------------         SFCDIF.169
            IF(RLMO.LT.0.)THEN                                                   SFCDIF.170
              PSMZ=PSLMU(ZETAU)                                                  SFCDIF.171
              SIMM=PSLMU(ZETALU)-PSMZ+RLOGU                                      SFCDIF.172
              PSHZ=PSLHU(ZETAT)                                                  SFCDIF.173
              SIMH=FHNEU*(PSLHU(ZETALT)-PSHZ+RLOGT)                              SFCDIF.174
            ELSE                                                                 SFCDIF.175
              PSMZ=PSLMS(ZETAU)                                                  SFCDIF.176
              SIMM=PSLMS(ZETALU)-PSMZ+RLOGU                                      SFCDIF.177
              PSHZ=PSLHS(ZETAT)                                                  SFCDIF.178
              SIMH=FHNEU*(PSLHS(ZETALT)-PSHZ+RLOGT)                              SFCDIF.179
            ENDIF                                                                SFCDIF.180
C--------------BELJAARS CORRECTION FOR USTAR----------------------------         SFCDIF.181
            USTAR=AMAX1(SQRT(AKMS*SQRT(DU2+WSTAR2)),EPSUST)                      SFCDIF.182
C-----------------------------------------------------------------------         SFCDIF.183
            USTARK=USTAR*VKRM                                                    SFCDIF.184
            AKMS=AMAX1(USTARK/SIMM,CXCH)                                         SFCDIF.185
            AKHS=AMAX1(USTARK/SIMH,CXCH)                                         SFCDIF.186
C-----------------------------------------------------------------------         SFCDIF.187
            WSTAR2=WWST2*ABS(BTGH*AKHS*DTHV)**(2./3.)                            SFCDIF.188
            IF(RLMO.GE.0.)WSTAR2=AMIN1(WSTAR2,1.)                                SFCDIF.189
            RLMN=ELFC*AKHS*DTHV/USTAR**3                                         SFCDIF.190
C-----------------------------------------------------------------------         SFCDIF.191
            RLMP=RLMO                                                            SFCDIF.192
            RLMA=RLMO*WOLD+RLMN*WNEW                                             SFCDIF.193
C-----------------------------------------------------------------------         SFCDIF.194
C          IF(ABS((RLMN-RLMO)/RLMA).LT.EPSIT)    GO TO 110                       SFCDIF.195
C-----------------------------------------------------------------------         SFCDIF.196
            RLMO=RLMA                                                            SFCDIF.197
C-----------------------------------------------------------------------         SFCDIF.198
  100     CONTINUE                                                               SFCDIF.199
C-----------------------------------------------------------------------         SFCDIF.200
  110     CONTINUE                                                               SFCDIF.201
        ELSE                                                                     SFCDIF.202
C--------------END OF SEA POINT PROCESSING------------------------------         SFCDIF.203
C--------------NOW LAND POINTS ...--------------------------------------         SFCDIF.204
          DO 200 ITR=1,ITRMX                                                     SFCDIF.205
C--------------1./MONIN-OBUKKHOV LENGTH-SCALE---------------------------         SFCDIF.206
            ZETALT=AMAX1(ZSLT*RLMO,ZTMIN)                                        SFCDIF.207
            RLMO=ZETALT/ZSLT                                                     SFCDIF.208
            ZETALU=ZSLU*RLMO                                                     SFCDIF.209
C                                                                                SFCDIF.210
            ZETAU=ZU*RLMO                                                        SFCDIF.211
            ZETAT=ZT*RLMO                                                        SFCDIF.212
C--------------PAULSON 1970 FUNCTIONS OVER LAND W RAD. SKIN T-----------         SFCDIF.213
            IF(RLMO.LT.0.)THEN                                                   SFCDIF.214
              XLU4=1.-16.*ZETALU                                                 SFCDIF.215
              XLT4=1.-16.*ZETALT                                                 SFCDIF.216
              XU4=1.-16.*ZETAU                                                   SFCDIF.217
              XT4=1.-16.*ZETAT                                                   SFCDIF.218
C                                                                                SFCDIF.219
              XLU=SQRT(SQRT(XLU4))                                               SFCDIF.220
              XLT=SQRT(SQRT(XLT4))                                               SFCDIF.221
              XU=SQRT(SQRT(XU4))                                                 SFCDIF.222
              XT=SQRT(SQRT(XT4))                                                 SFCDIF.223
C                                                                                SFCDIF.224
              PSMZ=PSPMU(XU)                                                     SFCDIF.225
              SIMM=PSPMU(XLU)-PSMZ+RLOGU                                         SFCDIF.226
              PSHZ=PSPHU(XT)                                                     SFCDIF.227
              SIMH=PSPHU(XLT)-PSHZ+RLOGT                                         SFCDIF.228
            ELSE                                                                 SFCDIF.229
              ZETAU=AMIN1(ZETAU,ZTMAX)                                           SFCDIF.230
              ZETAT=AMIN1(ZETAT,ZTMAX)                                           SFCDIF.231
              ZETALU=AMIN1(ZETALU,ZTMAX)                                         SFCDIF.232
              ZETALT=AMIN1(ZETALT,ZTMAX)                                         SFCDIF.233
              PSMZ=PSPMS(ZETAU)                                                  SFCDIF.234
              SIMM=PSPMS(ZETALU)-PSMZ+RLOGU                                      SFCDIF.235
              PSHZ=PSPHS(ZETAT)                                                  SFCDIF.236
              SIMH=PSPHS(ZETALT)-PSHZ+RLOGT                                      SFCDIF.237
            ENDIF                                                                SFCDIF.238
C--------------BELJAARS CORRECTION FOR USTAR----------------------------         SFCDIF.239
            USTAR=AMAX1(SQRT(AKMS*SQRT(DU2+WSTAR2)),EPSUST)                      SFCDIF.240
C--------------ZILITINKEVITCH FIX FOR ZT--------------------------------         SFCDIF.241
            ZT=EXP(ZILFC*SQRT(USTAR*Z0))*Z0                                      SFCDIF.242
            ZSLT=ZSL+ZT                                                          SFCDIF.243
            RLOGT=ALOG(ZSLT/ZT)                                                  SFCDIF.244
C-----------------------------------------------------------------------         SFCDIF.245
            USTARK=USTAR*VKRM                                                    SFCDIF.246
            AKMS=AMAX1(USTARK/SIMM,CXCH)                                         SFCDIF.247
            AKHS=AMAX1(USTARK/SIMH,CXCH)                                         SFCDIF.248
C-----------------------------------------------------------------------         SFCDIF.249
            WSTAR2=WWST2*ABS(BTGH*AKHS*DTHV)**(2./3.)                            SFCDIF.250
            IF(RLMO.GE.0.)WSTAR2=AMIN1(WSTAR2,1.)                                SFCDIF.251
            RLMN=ELFC*AKHS*DTHV/USTAR**3                                         SFCDIF.252
C-----------------------------------------------------------------------         SFCDIF.253
            RLMP=RLMO                                                            SFCDIF.254
            RLMA=RLMO*WOLD+RLMN*WNEW                                             SFCDIF.255
C-----------------------------------------------------------------------         SFCDIF.256
C          IF(ABS((RLMN-RLMO)/RLMA).LT.EPSIT)    GO TO 210                       SFCDIF.257
C-----------------------------------------------------------------------         SFCDIF.258
            RLMO=RLMA                                                            SFCDIF.259
C-----------------------------------------------------------------------         SFCDIF.260
  200     CONTINUE                                                               SFCDIF.261
C-----------------------------------------------------------------------         SFCDIF.262
  210     CONTINUE                                                               SFCDIF.263
        ENDIF                                                                    SFCDIF.264
      ENDIF                                                                      SFCDIF.265
C--------------END OF LAND POINT PROCESSING AND SEA-LAND BRANCHING------         SFCDIF.266
C--------------END OF TURBULENCE-NO TURBULENCE BRANCHING----------------         SFCDIF.267
C--------------COUNTERGRADIENT FIX--------------------------------------         SFCDIF.268
C      HV=-AKHS*DTHV                                                             SFCDIF.269
C      IF(HV.GT.0.)THEN                                                          SFCDIF.270
C        FCT=-10.*(BTG)**(-1./3.)                                                SFCDIF.271
C        CT=FCT*(HV/(HPBL*HPBL))**(2./3.)                                        SFCDIF.272
C      ELSE                                                                      SFCDIF.273
      CT=0.                                                                      SFCDIF.274
C      ENDIF                                                                     SFCDIF.275
C--------------DIAGNOSTIC BLOCK-----------------------------------------         SFCDIF.276
      WSTAR=SQRT(WSTAR2)/WWST                                                    SFCDIF.277
C                                                                                SFCDIF.278
      UMFLX=AKMS*(ULM-UZ0)                                                       SFCDIF.279
      VMFLX=AKMS*(VLM-VZ0)                                                       SFCDIF.280
      HSFLX=AKHS*(THLM-THZ0)                                                     SFCDIF.281
      HLFLX=AKHS*WETM*(QLM-QZ0)                                                  SFCDIF.282
c     write(6,*) 'at SFCDIF.282'                                                 19DEC02.1788
C                                                                                19DEC02.1789
C Calculate turbulent heat fluxes over sea ice grid point                        19DEC02.1790
C                                                                                19DEC02.1791
      IF(SIFRAC.GT.0.02)THEN                                                     19DEC02.1792
        HSFLXSI=AKHS*(THLM-THSI)                                                 19DEC02.1793
        HLFLXSI=AKHS*(QLM-QSSI)                                                  19DEC02.1794
        HSFLXWT=AKHS*(THLM-THWT)                                                 19DEC02.1795
        HLFLXWT=AKHS*(QLM-QSWT)                                                  19DEC02.1796
        HSFLX=HSFLXSI*SIFRAC+HSFLXWT*(1.-SIFRAC)                                 19DEC02.1797
        HLFLX=HLFLXSI*SIFRAC+HLFLXWT*(1.-SIFRAC)                                 19DEC02.1798
c     write(6,*) 'at endif SFCDIF.283'                                           19DEC02.1799
      ENDIF                                                                      19DEC02.1800
C-----------------------------------------------------------------------         SFCDIF.283
c  switching off turbulence for Ri>=RIC everywhere                               SFCDIF.284
      IF(ISM.GT.0.AND.RIB.GE.RIC)THEN                                            SFCDIF.285
c     IF(RIB.GE.RIC)THEN                                                         SFCDIF.286
C-----------------------------------------------------------------------         SFCDIF.287
        AKMS10=AMAX1(VISC/10.,CXCH)                                              SFCDIF.288
        AKHS02=AMAX1(TVISC/02.,CXCH)                                             SFCDIF.289
        AKHS10=AMAX1(TVISC/10.,CXCH)                                             SFCDIF.290
      ELSE                                                                       SFCDIF.291
C-----------------------------------------------------------------------         SFCDIF.292
C-----------------------------------------------------------------------         SFCDIF.293
        ZU10=ZU+10.                                                              SFCDIF.294
        ZT02=ZT+02.                                                              SFCDIF.295
        ZT10=ZT+10.                                                              SFCDIF.296
C                                                                                SFCDIF.297
        RLNU10=ALOG(ZU10/ZU)                                                     SFCDIF.298
        RLNT02=ALOG(ZT02/ZT)                                                     SFCDIF.299
        RLNT10=ALOG(ZT10/ZT)                                                     SFCDIF.300
C                                                                                SFCDIF.301
        ZTAU10=ZU10*RLMP                                                         SFCDIF.302
        ZTAT02=ZT02*RLMP                                                         SFCDIF.303
        ZTAT10=ZT10*RLMP                                                         SFCDIF.304
C--------------LL FUNCTIONS OVER SEA------------------------------------         SFCDIF.305
        IF(ISM.GT.0)THEN                                                         SFCDIF.306
C-----------------------------------------------------------------------         SFCDIF.307
          IF(RLMP.LT.0.)THEN                                                     SFCDIF.308
            SIMM10=PSLMU(ZTAU10)-PSMZ+RLNU10                                     SFCDIF.309
            SIMH02=FHNEU*(PSLHU(ZTAT02)-PSHZ+RLNT02)                             SFCDIF.310
            SIMH10=FHNEU*(PSLHU(ZTAT10)-PSHZ+RLNT10)                             SFCDIF.311
          ELSE                                                                   SFCDIF.312
            SIMM10=PSLMS(ZTAU10)-PSMZ+RLNU10                                     SFCDIF.313
            SIMH02=FHNEU*(PSLHS(ZTAT02)-PSHZ+RLNT02)                             SFCDIF.314
            SIMH10=FHNEU*(PSLHS(ZTAT10)-PSHZ+RLNT10)                             SFCDIF.315
          ENDIF                                                                  SFCDIF.316
        ELSE                                                                     SFCDIF.317
C--------------PAULSON 1970 FUNCTIONS OVER LAND W RAD. SKIN T-----------         SFCDIF.318
C-----------------------------------------------------------------------         SFCDIF.319
          IF(RLMP.LT.0.)THEN                                                     SFCDIF.320
            XLU104=1.-16.*ZTAU10                                                 SFCDIF.321
            XLT024=1.-16.*ZTAT02                                                 SFCDIF.322
            XLT104=1.-16.*ZTAT10                                                 SFCDIF.323
C                                                                                SFCDIF.324
            XLU10=SQRT(SQRT(XLU104))                                             SFCDIF.325
            XLT02=SQRT(SQRT(XLT024))                                             SFCDIF.326
            XLT10=SQRT(SQRT(XLT104))                                             SFCDIF.327
C                                                                                SFCDIF.328
            SIMM10=PSPMU(XLU10)-PSMZ+RLNU10                                      SFCDIF.329
            SIMH02=PSPHU(XLT02)-PSHZ+RLNT02                                      SFCDIF.330
            SIMH10=PSPHU(XLT10)-PSHZ+RLNT10                                      SFCDIF.331
          ELSE                                                                   SFCDIF.332
            ZTAU10=AMIN1(ZTAU10,ZTMAX)                                           SFCDIF.333
            ZTAT02=AMIN1(ZTAT02,ZTMAX)                                           SFCDIF.334
            ZTAT10=AMIN1(ZTAT10,ZTMAX)                                           SFCDIF.335
C                                                                                SFCDIF.336
            SIMM10=PSPMS(ZTAU10)-PSMZ+RLNU10                                     SFCDIF.337
            SIMH02=PSPHS(ZTAT02)-PSHZ+RLNT02                                     SFCDIF.338
            SIMH10=PSPHS(ZTAT10)-PSHZ+RLNT10                                     SFCDIF.339
          ENDIF                                                                  SFCDIF.340
        ENDIF                                                                    SFCDIF.341
C-----------------------------------------------------------------------         SFCDIF.342
C-----------------------------------------------------------------------         SFCDIF.343
        AKMS10=AMAX1(USTARK/SIMM10,CXCH)                                         SFCDIF.344
        AKHS02=AMAX1(USTARK/SIMH02,CXCH)                                         SFCDIF.345
        AKHS10=AMAX1(USTARK/SIMH10,CXCH)                                         SFCDIF.346
      ENDIF                                                                      SFCDIF.347
C-----------------------------------------------------------------------         SFCDIF.348
C-----------------------------------------------------------------------         SFCDIF.349
      U10=UMFLX/AKMS10+UZ0                                                       SFCDIF.350
      V10=VMFLX/AKMS10+VZ0                                                       SFCDIF.351
      TH02=HSFLX/AKHS02+THZ0                                                     SFCDIF.352
      TH10=HSFLX/AKHS10+THZ0                                                     SFCDIF.353
      Q02=HLFLX/AKHS02+QZ0                                                       SFCDIF.354
      Q10=HLFLX/AKHS10+QZ0                                                       SFCDIF.355
C                                                                                SFCDIF.356
      IF(Q02.LT.0.)Q02=QLM                                                       SFCDIF.357
      IF(Q10.LT.0.)Q10=QLM                                                       SFCDIF.358
C-----------------------------------------------------------------------         SFCDIF.359
      RETURN                                                                     SFCDIF.360
      END                                                                        SFCDIF.361
                                                                                 SFCDIF.362
