      SUBROUTINE FEEDBK (INEST,IYY,JXX)                                          FEEDBK.1
      IMPLICIT NONE                                                              25JAN00.152
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          FEEDBK.2
C                                                                     C          FEEDBK.3
C                                                                     C          FEEDBK.4
C     THIS SUBROUTINE COMPUTES THE FEEDBACK EFFECTS FROM THE          C          FEEDBK.5
C     NESTED DOMAIN TO THE LARGE DOMAIN.                              C          FEEDBK.6
C     CHANGES:                                                        C          FEEDBK.7
C       ADD TERRAIN AND PSTAR FEEDBACK CHECK                          C          FEEDBK.8
C       ADD CAPABILITY FOR NINE-POINT WEIGHTINGS IN FEEDBACK          C          FEEDBK.9
C                                                                     C          FEEDBK.10
C                                                                     C          FEEDBK.11
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          FEEDBK.12
#     include <parame.incl>                                                      FEEDBK.13
#     include <param2.incl>                                                      FEEDBK.14
#     include <param3.incl>                                                      FEEDBK.15
#     include <addr0.incl>                                                       FEEDBK.16
#     include <point3d.incl>                                                     FEEDBK.17
#     include <point2d.incl>                                                     FEEDBK.18
#     include <nonhyd.incl>                                                      FEEDBK.19
#     include <navypb.incl>                                                      FEEDBK.20
#     include <soil.incl>                                                        FEEDBK.21
#     include <soilp.incl>                                                       05DEC01.339
#     include <nnnhyd.incl>                                                      FEEDBK.22
#     include <various.incl>                                                     FEEDBK.23
#     include <point3dn.incl>                                                    FEEDBK.24
#     include <point2dn.incl>                                                    FEEDBK.25
#     include <variousn.incl>                                                    FEEDBK.26
#     include <nestl.incl>                                                       FEEDBK.27
#     include <varia.incl>                                                       FEEDBK.28
#     include <pmoist.incl>                                                      FEEDBK.29
#     include <pnavyn.incl>                                                      FEEDBK.30
#     include <soiln.incl>                                                       FEEDBK.31
#     include <soilpn.incl>                                                      05DEC01.340
#     include <rpstar.incl>                                                      FEEDBK.32
#     include <rpstarn.incl>                                                     FEEDBK.33
#     include <nhcnst.incl>                                                      FEEDBK.34
#     include <nncnst.incl>                                                      FEEDBK.35
#     include <pbltke.incl>                                                      19DEC02.893
#     include <pbltken.incl>                                                     19DEC02.894
#     include <zdiffu.incl>                                                      23DEC04.245
C DECLARATIONS ADDED FOR IMPL NONE                                               25JAN00.153
      REAL F1,F2,F3                                                              25JAN00.154
      INTEGER INEST                                                              25JAN00.155
      INTEGER I,J,K                                                              25JAN00.156
      INTEGER ICY,JCX,JFX,JFP,IFY,IFP,IFM                                        25JAN00.157
      INTEGER JFM,L                                                              25JAN00.158
      REAL    TIJ,TPJ,TMG,TIP,TIM,TPP,TPM,TMP,TMM,TAV                            25JAN00.159
      REAL    QVIJ,QVPJ,QVMJ,QVIP,QVIM,QVPP,QVPM                                 25JAN00.160
      REAL    QVMP,QVMM,QVAV,QCIJ,QCPJ,QCMJ,QCIP,QCIM,QCPP                       25JAN00.161
      REAL TMJ,QCPM,QCMP,QCMM,QCAV,QRIJ,QRPJ,QRMJ,QRIP,QRIM,QRPP,QRPM            25JAN00.162
      REAL QRMP,QRMM,QRAV,QIIJ,QIPJ,QIMJ,QIIP,QIIM,QIPP,QIPM,QIMP                25JAN00.163
      REAL QIMM,QIAV,QNIIJ,QNIPJ,QNIMJ,QNIIP,QNIIM                               25JAN00.164
      REAL QNIPP,QNIPM,QNIMP,QNIMM,QNIAV,QGIJ,QGPJ,QGMJ,QGIP,QGIM                25JAN00.165
      REAL QGPP,QGPM,QGMP,QGMM,QGAV,QNCIJ,QNCPJ,QNCIP,QNCIM                      25JAN00.166
      REAL QNCPP,QNCPM,QNCMP,QNCMM,QNCAV,QNCMJ                                   25JAN00.167
      REAL UIJ,UPJ,UMJ                                                           25JAN00.168
      INTEGER JFM2,IFM2                                                          25JAN00.169
      REAL UIP,UIM,UPP,UPM,UMP,UMM,UAV                                           25JAN00.170
      REAL VIJ,VPJ,VMJ,VIP,VIM,VPP,VPM,VMP,VMM,VAV                               25JAN00.171
      INTEGER IYY,JXX                                                            25JAN00.172
C                                                                                25JAN00.173
      REAL HSC1(MIX,MJX),HSC2(MIX,MJX)                                           25JAN00.174
      REAL HSC3(MIX,MJX),HSC4(MIX,MJX)                                           25JAN00.175
      REAL HSC5(MIX,MJX)                                                         25JAN00.176
      REAL HSC6(MIX,MJX)                                                         25JAN00.177
      REAL SLP(MIX,MJX)                                                          25JAN00.178
C                                                                                FEEDBK.42
C---SET AVERAGING WEIGHTS                                                        FEEDBK.43
C                                                                                FEEDBK.44
C---OLD WEIGHTS                                                                  FEEDBK.45
      F1=0.25                                                                    FEEDBK.46
      F2=0.125                                                                   FEEDBK.47
      F3=0.0625                                                                  FEEDBK.48
C---NEW WEIGHTS (9-POINT UNWEIGHTED AVERAGE)                                     FEEDBK.49
C      F1=1./9.                                                                  FEEDBK.50
C      F2=1./9.                                                                  FEEDBK.51
C      F3=1./9.                                                                  FEEDBK.52
C                                                                                FEEDBK.53
C---CHECK SUM                                                                    FEEDBK.54
C      SUMF=F1+4.*F2+4.*F3                                                       FEEDBK.55
C---SUMF MUST EQUAL 1.                                                           FEEDBK.56
C                                                                                FEEDBK.57
C----------------------------------------------------------------------          FEEDBK.58
C     DECOUPLE NEST REGION OF COARSE DOMAIN AND EXTRA FOR SMOOTHER               FEEDBK.59
C     PRIOR TO ANY UPDATES FROM NEST OF PDOTA                                    FEEDBK.60
cmic$ do all autoscope                                                           FEEDBK.61
c$omp parallel do default(shared)                                                FEEDBK.62
c$omp&private(icy,jcx,k)                                                         FEEDBK.63
      DO JCX=JWEST,JEAST                                                         FEEDBK.64
        DO ICY=ISOUTH,INORTH                                                     FEEDBK.65
          DO K=1,KL                                                              FEEDBK.66
            UA(ICY,JCX,K)=UA(ICY,JCX,K)*RPDOTA(ICY,JCX)                          FEEDBK.67
            VA(ICY,JCX,K)=VA(ICY,JCX,K)*RPDOTA(ICY,JCX)                          FEEDBK.68
          ENDDO                                                                  FEEDBK.69
        ENDDO                                                                    FEEDBK.70
      ENDDO                                                                      FEEDBK.71
C                                                                                FEEDBK.72
C           CHECK TO SEE IF TERRAIN FEEDBACK REQUIRED                            FEEDBK.73
C                                                                                FEEDBK.74
      IF(IFEED.GE.2.AND.LFBTER(INEST))THEN                                       FEEDBK.75
        if ((ITPDIF.gt.1).and.(IOVERW(inest).EQ.2)) then                
          print*, 'Opening new domain with IOVERW=2 and z-diffusion'   
          print*, 'requires recomputing interpolation coefficients'   
          do i=1,numnc(inest)                                        
            diffpar(i)=.true.                                       
          enddo                                                    
        endif                                                     
C       ON FIRST TIME DATA IS FED BACK FROM NEST, FEED BACK TERRAIN,             FEEDBK.76
C       LAND-USE, AND PSTAR FIELDS                                               FEEDBK.77
cmic$ do all autoscope                                                           FEEDBK.78
c$omp parallel do default(shared)                                                FEEDBK.79
c$omp&private(icy,jcx,k,jfx,ify)                                                 FEEDBK.80
        DO 15 JCX=JWP1,JEM2                                                      FEEDBK.81
          JFX=2+(JCX-JWEST)*IRAX                                                 FEEDBK.82
          DO 15 ICY=ISP1,INM2                                                    FEEDBK.83
            IFY=2+(ICY-ISOUTH)*IRAX                                              FEEDBK.84
            HT(ICY,JCX)=HTN(IFY,JFX)                                             FEEDBK.85
            SATBRT(ICY,JCX)=SATBRN(IFY,JFX)                                      FEEDBK.86
            XLAND(ICY,JCX)=XLANDN(IFY,JFX)                                       07NOV00.811
            PSA(ICY,JCX)=PSAN(IFY,JFX)                                           FEEDBK.87
            PSB(ICY,JCX)=PSBN(IFY,JFX)                                           FEEDBK.88
            RPSA(ICY,JCX)=RPSAN(IFY,JFX)                                         FEEDBK.89
            RPSB(ICY,JCX)=RPSBN(IFY,JFX)                                         FEEDBK.90
            RPSC(ICY,JCX)=RPSCN(IFY,JFX)                                         FEEDBK.91
            PS0(ICY,JCX)=PS0N(IFY,JFX)                                           FEEDBK.92
            DO K=1,KL                                                            FEEDBK.93
              T0(ICY,JCX,K)=T0N(IFY,JFX,K)                                       FEEDBK.94
              PR0(ICY,JCX,K)=PR0N(IFY,JFX,K)                                     FEEDBK.95
              RHO0(ICY,JCX,K)=RHO0N(IFY,JFX,K)                                   FEEDBK.96
            ENDDO                                                                FEEDBK.97
   15     CONTINUE                                                               FEEDBK.98
                                                                                 FEEDBK.99
        CALL DOTS(PSA,PDOTA,IL,JL,MIX,MJX)                                       FEEDBK.100
        DO J=1,JL                                                                FEEDBK.101
          DO I=1,IL                                                              FEEDBK.102
            RPDOTA(I,J)=1./PDOTA(I,J)                                            FEEDBK.103
            RPDOTB(I,J)=RPDOTA(I,J)                                              FEEDBK.104
            PDOTB(I,J)=PDOTA(I,J)                                                FEEDBK.105
          ENDDO                                                                  FEEDBK.106
        ENDDO                                                                    FEEDBK.107
        LFBTER(INEST)=.FALSE.                                                    FEEDBK.108
C IF PARENT IS A NEST, HAVE IT FEED BACK NEW TERRAIN TO ITS PARENT               FEEDBK.109
        IF(NUMNC(INEST).GT.1.AND.NUMNC(INEST).LE.MAXNES)THEN                     FEEDBK.110
          LFBTER(NUMNC(INEST))=.TRUE.                                            FEEDBK.111
        ENDIF                                                                    FEEDBK.112
      ENDIF                                                                      FEEDBK.113
C                                                                                FEEDBK.114
C----CALCULATE THE GROUND TEMPERATURE FEEDBK:                                    FEEDBK.115
C                                                                                FEEDBK.116
      IF(ITGFLG(INEST).NE.1)GOTO 25                                              FEEDBK.117
cmic$ do all autoscope                                                           FEEDBK.118
c$omp parallel do default(shared)                                                FEEDBK.119
c$omp&private(icy,jcx,jfx,jfp,jfm,ify,ifp,ifm)                                   FEEDBK.120
      DO 20 JCX=JWP1,JEM2                                                        FEEDBK.121
        JFX=2+(JCX-JWEST)*IRAX                                                   FEEDBK.122
        JFP=JFX+1                                                                FEEDBK.123
        JFM=JFX-1                                                                FEEDBK.124
        DO 20 ICY=ISP1,INM2                                                      FEEDBK.125
          IFY=2+(ICY-ISOUTH)*IRAX                                                FEEDBK.126
          IFP=IFY+1                                                              FEEDBK.127
          IFM=IFY-1                                                              FEEDBK.128
          IF(IFEED.EQ.1)then                                                     19DEC02.895
            TGA(ICY,JCX)=F1*TGAN(IFY,JFX)+F2*(TGAN(IFP,JFX)+                     19DEC02.896
     +      TGAN(IFM,JFX)+TGAN(IFY,JFP)+TGAN(IFY,JFM))+F3*(TGAN(IFP,JFP)         FEEDBK.130
     +      +TGAN(IFP,JFM)+TGAN(IFM,JFP)+TGAN(IFM,JFM))                          FEEDBK.131
          endif                                                                  23DEC04.253
          IF(IFEED.GE.2) then                                                    19DEC02.902
            TGA(ICY,JCX)=TGAN(IFY,JFX)                                           19DEC02.903
          endif                                                                  19DEC02.905
   20   CONTINUE                                                                 FEEDBK.133
                                                                                 FEEDBK.134
C                                                                                FEEDBK.135
C----CALCULATE MULTI-LAYER SOIL TEMPERATURE FEEDBACK                             FEEDBK.136
C                                                                                FEEDBK.137
      IF(ISOIL.NE.1)GOTO 25                                                      FEEDBK.138
cmic$ do all autoscope                                                           FEEDBK.139
c$omp parallel do default(shared)                                                FEEDBK.140
c$omp&private(icy,jcx,l,jfx,jfp,jfm,ify,ifp,ifm,tij,tpj,tmj,tip,tim,             FEEDBK.141
c$omp&tpp,tpm,tmp,tmm,tav)                                                       FEEDBK.142
      DO JCX=JWP1,JEM2                                                           FEEDBK.143
        JFX=2+(JCX-JWEST)*IRAX                                                   FEEDBK.144
        JFP=JFX+1                                                                FEEDBK.145
        JFM=JFX-1                                                                FEEDBK.146
        DO ICY=ISP1,INM2                                                         FEEDBK.147
          IFY=2+(ICY-ISOUTH)*IRAX                                                FEEDBK.148
          IFP=IFY+1                                                              FEEDBK.149
          IFM=IFY-1                                                              FEEDBK.150
          DO L=1,MLX                                                             FEEDBK.151
            IF(IFEED.EQ.1)THEN                                                   FEEDBK.152
              TIJ=TSLAN(IFY,JFX,L)                                               FEEDBK.153
              TPJ=TSLAN(IFP,JFX,L)                                               FEEDBK.154
              TMJ=TSLAN(IFM,JFX,L)                                               FEEDBK.155
              TIP=TSLAN(IFY,JFP,L)                                               FEEDBK.156
              TIM=TSLAN(IFY,JFM,L)                                               FEEDBK.157
              TPP=TSLAN(IFP,JFP,L)                                               FEEDBK.158
              TPM=TSLAN(IFP,JFM,L)                                               FEEDBK.159
              TMP=TSLAN(IFM,JFP,L)                                               FEEDBK.160
              TMM=TSLAN(IFM,JFM,L)                                               FEEDBK.161
              TAV=F1*TIJ+F2*(TIP+TIM+TPJ+TMJ)+F3*(TPP+TPM+TMP+TMM)               FEEDBK.162
              TSLA(ICY,JCX,L)=TAV                                                FEEDBK.163
            ENDIF                                                                FEEDBK.164
            IF(IFEED.GE.2)TSLA(ICY,JCX,L)=TSLAN(IFY,JFX,L)                       FEEDBK.165
          ENDDO                                                                  FEEDBK.166
        ENDDO                                                                    FEEDBK.167
      ENDDO                                                                      FEEDBK.168
C                                                                                FEEDBK.169
C-----CALCULATE THE TEMPERATURE FEEDBACK:                                        FEEDBK.170
C                                                                                FEEDBK.171
   25 CONTINUE                                                                   FEEDBK.172
C                                                                                FEEDBK.173
C--- RAINFALL FIELDS FEEDBACK                                                    FEEDBK.174
C                                                                                FEEDBK.175
cmic$ do all autoscope                                                           FEEDBK.176
c$omp parallel do default(shared)                                                FEEDBK.177
c$omp&private(icy,jcx,jfx,jfp,jfm,ify,ifp,ifm)                                   FEEDBK.178
      DO 22 JCX=JWP1,JEM2                                                        FEEDBK.179
        JFX=2+(JCX-JWEST)*IRAX                                                   FEEDBK.180
        JFP=JFX+1                                                                FEEDBK.181
        JFM=JFX-1                                                                FEEDBK.182
        DO 22 ICY=ISP1,INM2                                                      FEEDBK.183
          IFY=2+(ICY-ISOUTH)*IRAX                                                FEEDBK.184
          IFP=IFY+1                                                              FEEDBK.185
          IFM=IFY-1                                                              FEEDBK.186
          RAINC(ICY,JCX)=(RAINCN(IFY,JFX)+RAINCN(IFP,JFX)+                       FEEDBK.187
     +                   RAINCN(IFM,JFX)+RAINCN(IFY,JFP)+RAINCN(IFY,JFM)         FEEDBK.188
     +                   +RAINCN(IFP,JFP)+RAINCN(IFP,JFM)+                       FEEDBK.189
     +                   RAINCN(IFM,JFP)+RAINCN(IFM,JFM))/9.                     FEEDBK.190
          RAINNC(ICY,JCX)=(RAINNN(IFY,JFX)+RAINNN(IFP,JFX)+                      FEEDBK.191
     +                    RAINNN(IFM,JFX)+RAINNN(IFY,JFP)+                       FEEDBK.192
     +                    RAINNN(IFY,JFM)+RAINNN(IFP,JFP)+                       FEEDBK.193
     +                    RAINNN(IFP,JFM)+RAINNN(IFM,JFP)+                       FEEDBK.194
     +                    RAINNN(IFM,JFM))/9.                                    FEEDBK.195
   22   CONTINUE                                                                 FEEDBK.196
C                                                                                07NOV00.812
C--- MOISTURE AVAILABILITY FIELD FEEDBACK                                        07NOV00.813
C                                                                                07NOV00.814
      IF (IMOIAV(INEST).GE.1) THEN                                               07NOV00.815
cmic$ do all autoscope                                                           07NOV00.816
c$omp parallel do default(shared)                                                07NOV00.817
c$omp&private(icy,jcx,jfx,jfp,jfm,ify,ifp,ifm)                                   07NOV00.818
        DO 23 JCX=JWP1,JEM2                                                      07NOV00.819
          JFX=2+(JCX-JWEST)*IRAX                                                 07NOV00.820
          JFP=JFX+1                                                              07NOV00.821
          JFM=JFX-1                                                              07NOV00.822
          DO 23 ICY=ISP1,INM2                                                    07NOV00.823
            IFY=2+(ICY-ISOUTH)*IRAX                                              07NOV00.824
            IFP=IFY+1                                                            07NOV00.825
            IFM=IFY-1                                                            07NOV00.826
            MAVAIL(ICY,JCX)=(MAVAIN(IFY,JFX)+MAVAIN(IFP,JFX)+                    07NOV00.827
     +                   MAVAIN(IFM,JFX)+MAVAIN(IFY,JFP)+MAVAIN(IFY,JFM)         07NOV00.828
     +                   +MAVAIN(IFP,JFP)+MAVAIN(IFP,JFM)+                       07NOV00.829
     +                   MAVAIN(IFM,JFP)+MAVAIN(IFM,JFM))/9.                     07NOV00.830
   23     CONTINUE                                                               07NOV00.831
      END IF                                                                     07NOV00.832
C                                                                                05DEC01.341
C--- SNOW COVER FEEDBACK                                                         05DEC01.342
C                                                                                05DEC01.343
      IF (IFSNOW(INEST).EQ.2) THEN                                               05DEC01.344
cmic$ do all autoscope                                                           05DEC01.345
c$omp parallel do default(shared)                                                05DEC01.346
c$omp&private(icy,jcx,jfx,jfp,jfm,ify,ifp,ifm)                                   05DEC01.347
        DO 24 JCX=JWP1,JEM2                                                      05DEC01.348
          JFX=2+(JCX-JWEST)*IRAX                                                 05DEC01.349
          JFP=JFX+1                                                              05DEC01.350
          JFM=JFX-1                                                              05DEC01.351
          DO 24 ICY=ISP1,INM2                                                    05DEC01.352
            IFY=2+(ICY-ISOUTH)*IRAX                                              05DEC01.353
            IFP=IFY+1                                                            05DEC01.354
            IFM=IFY-1                                                            05DEC01.355
            SNOWC(ICY,JCX)=(SNOWCN(IFY,JFX)+SNOWCN(IFP,JFX)+                     05DEC01.356
     +                   SNOWCN(IFM,JFX)+SNOWCN(IFY,JFP)+SNOWCN(IFY,JFM)         05DEC01.357
     +                   +SNOWCN(IFP,JFP)+SNOWCN(IFP,JFM)+                       05DEC01.358
     +                   SNOWCN(IFM,JFP)+SNOWCN(IFM,JFM))/9.                     05DEC01.359
   24     CONTINUE                                                               05DEC01.360
      END IF                                                                     05DEC01.361
C                                                                                23DEC04.254
C--- SEA-ICE FEEDBACK                                                            23DEC04.255
C                                                                                23DEC04.256
      IF(IEXSI.GE.1)THEN                                                         23DEC04.257
cmic$ do all autoscope                                                           23DEC04.258
c$omp parallel do default(shared)                                                23DEC04.259
c$omp&private(icy,jcx,jfx,jfp,jfm,ify,ifp,ifm)                                   23DEC04.260
      DO JCX=JWP1,JEM2                                                           23DEC04.261
        JFX=2+(JCX-JWEST)*IRAX                                                   23DEC04.262
        JFP=JFX+1                                                                23DEC04.263
        JFM=JFX-1                                                                23DEC04.264
        DO ICY=ISP1,INM2                                                         23DEC04.265
          IFY=2+(ICY-ISOUTH)*IRAX                                                23DEC04.266
          IFP=IFY+1                                                              23DEC04.267
          IFM=IFY-1                                                              23DEC04.268
          IF(IFEED.GE.1) then                                                    23DEC04.269
            SEAICE(ICY,JCX)=SEAICN(IFY,JFX)                                      23DEC04.270
            TGSI(ICY,JCX)=TGSIN(IFY,JFX)                                         23DEC04.271
          ENDIF                                                                  23DEC04.272
        ENDDO                                                                    23DEC04.273
      ENDDO                                                                      23DEC04.274
      ENDIF                                                                      23DEC04.275
                                                                                 23DEC04.276
      IF (ISOIL.EQ.2) THEN                                                       05DEC01.362
C FOR LSM JUST FEED BACK SINGLE POINT (AGREES WITH LANDUSE/SOIL FEEDBACK)        19DEC02.906
cmic$ do all autoscope                                                           05DEC01.363
c$omp parallel do default(shared)                                                05DEC01.364
c$omp&private(icy,jcx,jfx,jfp,jfm,ify,ifp,ifm)                                   05DEC01.365
        DO 241 JCX=JWP1,JEM2                                                     05DEC01.366
          JFX=2+(JCX-JWEST)*IRAX                                                 05DEC01.367
          JFP=JFX+1                                                              05DEC01.368
          JFM=JFX-1                                                              05DEC01.369
          DO 241 ICY=ISP1,INM2                                                   05DEC01.370
            IFY=2+(ICY-ISOUTH)*IRAX                                              05DEC01.371
            IFP=IFY+1                                                            05DEC01.372
            IFM=IFY-1                                                            05DEC01.373
            SNOA(ICY,JCX)=SNOAN(IFY,JFX)                                         19DEC02.907
            SNOWHA(ICY,JCX)=SNOWHAN(IFY,JFX)                                     19DEC02.908
            CMCA(ICY,JCX)=CMCAN(IFY,JFX)                                         19DEC02.909
            SNOWC(ICY,JCX)=SNOWCN(IFY,JFX)                                       19DEC02.910
            DO L = 1,MLX                                                         19DEC02.911
              STCA(ICY,JCX,L)=STCAN(IFY,JFX,L)                                   19DEC02.912
              SMCA(ICY,JCX,L)=SMCAN(IFY,JFX,L)                                   19DEC02.913
              SWCA(ICY,JCX,L)=SWCAN(IFY,JFX,L)                                   19DEC02.914
            ENDDO                                                                19DEC02.915
  241     CONTINUE                                                               05DEC01.378
      END IF                                                                     05DEC01.379
                                                                                 FEEDBK.197
      IF(INAV.EQ.1)THEN                                                          FEEDBK.198
cmic$ do all autoscope                                                           FEEDBK.199
c$omp parallel do default(shared)                                                FEEDBK.200
c$omp&private(icy,jcx,k,jfx,jfp,jfm,ify,ifp,ifm,tij,tpj,tmj,tip,tim,             FEEDBK.201
c$omp&tpp,tpm,tmp,tmm,tav)                                                       FEEDBK.202
        DO 26 JCX=JWP1,JEM2                                                      FEEDBK.203
          JFX=2+(JCX-JWEST)*IRAX                                                 FEEDBK.204
          JFP=JFX+1                                                              FEEDBK.205
          JFM=JFX-1                                                              FEEDBK.206
          DO 26 ICY=ISP1,INM2                                                    FEEDBK.207
            IFY=2+(ICY-ISOUTH)*IRAX                                              FEEDBK.208
            IFP=IFY+1                                                            FEEDBK.209
            IFM=IFY-1                                                            FEEDBK.210
            DO 26 K=1,KL                                                         FEEDBK.211
              IF(IFEED.EQ.1)THEN                                                 FEEDBK.212
                TIJ=TURKEN(IFY,JFX,K)                                            FEEDBK.213
                TPJ=TURKEN(IFP,JFX,K)                                            FEEDBK.214
                TMJ=TURKEN(IFM,JFX,K)                                            FEEDBK.215
                TIP=TURKEN(IFY,JFP,K)                                            FEEDBK.216
                TIM=TURKEN(IFY,JFM,K)                                            FEEDBK.217
                TPP=TURKEN(IFP,JFP,K)                                            FEEDBK.218
                TPM=TURKEN(IFP,JFM,K)                                            FEEDBK.219
                TMP=TURKEN(IFM,JFP,K)                                            FEEDBK.220
                TMM=TURKEN(IFM,JFM,K)                                            FEEDBK.221
                TAV=F1*TIJ+F2*(TIP+TIM+TPJ+TMJ)+F3*(TPP+TPM+TMP+TMM)             FEEDBK.222
                TURKE(ICY,JCX,K)=TAV                                             FEEDBK.223
              ENDIF                                                              FEEDBK.224
              IF(IFEED.GE.2)TURKE(ICY,JCX,K)=TURKEN(IFY,JFX,K)                   FEEDBK.225
   26       CONTINUE                                                             FEEDBK.226
                                                                                 FEEDBK.227
                                                                                 FEEDBK.228
C                                                                                FEEDBK.229
cmic$ do all autoscope                                                           FEEDBK.230
c$omp parallel do default(shared)                                                FEEDBK.231
c$omp&private(icy,jcx,k,jfx,jfp,jfm,ify,ifp,ifm,tij,tpj,tmj,tip,tim,             FEEDBK.232
c$omp&tpp,tpm,tmp,tmm,tav)                                                       FEEDBK.233
        DO 27 JCX=JWP1,JEM2                                                      FEEDBK.234
          JFX=2+(JCX-JWEST)*IRAX                                                 FEEDBK.235
          JFP=JFX+1                                                              FEEDBK.236
          JFM=JFX-1                                                              FEEDBK.237
          DO 27 ICY=ISP1,INM2                                                    FEEDBK.238
            IFY=2+(ICY-ISOUTH)*IRAX                                              FEEDBK.239
            IFP=IFY+1                                                            FEEDBK.240
            IFM=IFY-1                                                            FEEDBK.241
            DO 27 K=1,KL                                                         FEEDBK.242
              IF(IFEED.EQ.1)THEN                                                 FEEDBK.243
                TIJ=PTHLSN(IFY,JFX,K)                                            FEEDBK.244
                TPJ=PTHLSN(IFP,JFX,K)                                            FEEDBK.245
                TMJ=PTHLSN(IFM,JFX,K)                                            FEEDBK.246
                TIP=PTHLSN(IFY,JFP,K)                                            FEEDBK.247
                TIM=PTHLSN(IFY,JFM,K)                                            FEEDBK.248
                TPP=PTHLSN(IFP,JFP,K)                                            FEEDBK.249
                TPM=PTHLSN(IFP,JFM,K)                                            FEEDBK.250
                TMP=PTHLSN(IFM,JFP,K)                                            FEEDBK.251
                TMM=PTHLSN(IFM,JFM,K)                                            FEEDBK.252
                TAV=F1*TIJ+F2*(TIP+TIM+TPJ+TMJ)+F3*(TPP+TPM+TMP+TMM)             FEEDBK.253
                PTHLSB(ICY,JCX,K)=TAV                                            FEEDBK.254
              ENDIF                                                              FEEDBK.255
              IF(IFEED.GE.2)PTHLSB(ICY,JCX,K)=PTHLSN(IFY,JFX,K)                  FEEDBK.256
   27       CONTINUE                                                             FEEDBK.257
                                                                                 FEEDBK.258
                                                                                 FEEDBK.259
C                                                                                FEEDBK.260
cmic$ do all autoscope                                                           FEEDBK.261
c$omp parallel do default(shared)                                                FEEDBK.262
c$omp&private(icy,jcx,k,jfx,jfp,jfm,ify,ifp,ifm,tij,tpj,tmj,tip,tim,             FEEDBK.263
c$omp&tpp,tpm,tmp,tmm,tav)                                                       FEEDBK.264
        DO 28 JCX=JWP1,JEM2                                                      FEEDBK.265
          JFX=2+(JCX-JWEST)*IRAX                                                 FEEDBK.266
          JFP=JFX+1                                                              FEEDBK.267
          JFM=JFX-1                                                              FEEDBK.268
          DO 28 ICY=ISP1,INM2                                                    FEEDBK.269
            IFY=2+(ICY-ISOUTH)*IRAX                                              FEEDBK.270
            IFP=IFY+1                                                            FEEDBK.271
            IFM=IFY-1                                                            FEEDBK.272
            DO 28 K=1,KL                                                         FEEDBK.273
              IF(IFEED.EQ.1)THEN                                                 FEEDBK.274
                TIJ=PTHLQNB(IFY,JFX,K)                                           FEEDBK.275
                TPJ=PTHLQNB(IFP,JFX,K)                                           FEEDBK.276
                TMJ=PTHLQNB(IFM,JFX,K)                                           FEEDBK.277
                TIP=PTHLQNB(IFY,JFP,K)                                           FEEDBK.278
                TIM=PTHLQNB(IFY,JFM,K)                                           FEEDBK.279
                TPP=PTHLQNB(IFP,JFP,K)                                           FEEDBK.280
                TPM=PTHLQNB(IFP,JFM,K)                                           FEEDBK.281
                TMP=PTHLQNB(IFM,JFP,K)                                           FEEDBK.282
                TMM=PTHLQNB(IFM,JFM,K)                                           FEEDBK.283
                TAV=F1*TIJ+F2*(TIP+TIM+TPJ+TMJ)+F3*(TPP+TPM+TMP+TMM)             FEEDBK.284
                PTHLQWB(ICY,JCX,K)=TAV                                           FEEDBK.285
              ENDIF                                                              FEEDBK.286
              IF(IFEED.GE.2)PTHLQWB(ICY,JCX,K)=PTHLQNB(IFY,JFX,K)                FEEDBK.287
   28       CONTINUE                                                             FEEDBK.288
                                                                                 FEEDBK.289
                                                                                 FEEDBK.290
C                                                                                FEEDBK.291
cmic$ do all autoscope                                                           FEEDBK.292
c$omp parallel do default(shared)                                                FEEDBK.293
c$omp&private(icy,jcx,k,jfx,jfp,jfm,ify,ifp,ifm,tij,tpj,tmj,tip,tim,             FEEDBK.294
c$omp&tpp,tpm,tmp,tmm,tav)                                                       FEEDBK.295
        DO 29 JCX=JWP1,JEM2                                                      FEEDBK.296
          JFX=2+(JCX-JWEST)*IRAX                                                 FEEDBK.297
          JFP=JFX+1                                                              FEEDBK.298
          JFM=JFX-1                                                              FEEDBK.299
          DO 29 ICY=ISP1,INM2                                                    FEEDBK.300
            IFY=2+(ICY-ISOUTH)*IRAX                                              FEEDBK.301
            IFP=IFY+1                                                            FEEDBK.302
            IFM=IFY-1                                                            FEEDBK.303
            DO 29 K=1,KL                                                         FEEDBK.304
              IF(IFEED.EQ.1)THEN                                                 FEEDBK.305
                TIJ=PQWSBN(IFY,JFX,K)                                            FEEDBK.306
                TPJ=PQWSBN(IFP,JFX,K)                                            FEEDBK.307
                TMJ=PQWSBN(IFM,JFX,K)                                            FEEDBK.308
                TIP=PQWSBN(IFY,JFP,K)                                            FEEDBK.309
                TIM=PQWSBN(IFY,JFM,K)                                            FEEDBK.310
                TPP=PQWSBN(IFP,JFP,K)                                            FEEDBK.311
                TPM=PQWSBN(IFP,JFM,K)                                            FEEDBK.312
                TMP=PQWSBN(IFM,JFP,K)                                            FEEDBK.313
                TMM=PQWSBN(IFM,JFM,K)                                            FEEDBK.314
                TAV=F1*TIJ+F2*(TIP+TIM+TPJ+TMJ)+F3*(TPP+TPM+TMP+TMM)             FEEDBK.315
                PQWSB(ICY,JCX,K)=TAV                                             FEEDBK.316
              ENDIF                                                              FEEDBK.317
              IF(IFEED.GE.2)PQWSB(ICY,JCX,K)=PQWSBN(IFY,JFX,K)                   FEEDBK.318
   29       CONTINUE                                                             FEEDBK.319
                                                                                 FEEDBK.320
                                                                                 FEEDBK.321
      ENDIF                                                                      FEEDBK.322
C  Add TKEA feedback for GS - 20021217                                           19DEC02.916
      IF(IGSPBL.EQ.1)THEN                                                        19DEC02.917
C                                                                                19DEC02.918
cmic$ do all autoscope                                                           19DEC02.919
c$omp parallel do default(shared)                                                19DEC02.920
c$omp&private(icy,jcx,k,jfx,jfp,jfm,ify,ifp,ifm,tij,tpj,tmj,tip,tim,             19DEC02.921
c$omp&tpp,tpm,tmp,tmm,tav)                                                       19DEC02.922
      DO JCX=JWP1,JEM2                                                           19DEC02.923
        JFX=2+(JCX-JWEST)*IRAX                                                   19DEC02.924
        JFP=JFX+1                                                                19DEC02.925
        JFM=JFX-1                                                                19DEC02.926
        DO ICY=ISP1,INM2                                                         19DEC02.927
          IFY=2+(ICY-ISOUTH)*IRAX                                                19DEC02.928
          IFP=IFY+1                                                              19DEC02.929
          IFM=IFY-1                                                              19DEC02.930
          DO K=1,KL                                                              19DEC02.931
            IF(IFEED.EQ.1)THEN                                                   19DEC02.932
              TIJ=TKEAN(IFY,JFX,K)                                               19DEC02.933
              TPJ=TKEAN(IFP,JFX,K)                                               19DEC02.934
              TMJ=TKEAN(IFM,JFX,K)                                               19DEC02.935
              TIP=TKEAN(IFY,JFP,K)                                               19DEC02.936
              TIM=TKEAN(IFY,JFM,K)                                               19DEC02.937
              TPP=TKEAN(IFP,JFP,K)                                               19DEC02.938
              TPM=TKEAN(IFP,JFM,K)                                               19DEC02.939
              TMP=TKEAN(IFM,JFP,K)                                               19DEC02.940
              TMM=TKEAN(IFM,JFM,K)                                               19DEC02.941
              TAV=F1*TIJ+F2*(TIP+TIM+TPJ+TMJ)+F3*(TPP+TPM+TMP+TMM)               19DEC02.942
              TKEA(ICY,JCX,K)=TAV                                                19DEC02.943
            ENDIF                                                                19DEC02.944
            IF(IFEED.GE.2)TKEA(ICY,JCX,K)=TKEAN(IFY,JFX,K)                       19DEC02.945
          ENDDO                                                                  19DEC02.946
        ENDDO                                                                    19DEC02.947
      ENDDO                                                                      19DEC02.948
      ENDIF                                                                      19DEC02.949
C                                                                                19DEC02.950
C                                                                                FEEDBK.323
cmic$ do all autoscope                                                           FEEDBK.324
c$omp parallel do default(shared)                                                FEEDBK.325
c$omp&private(icy,jcx,k,jfx,jfp,jfm,ify,ifp,ifm,tij,tpj,tmj,tip,tim,             FEEDBK.326
c$omp&tpp,tpm,tmp,tmm,tav)                                                       FEEDBK.327
      DO 30 JCX=JWP1,JEM2                                                        FEEDBK.328
        JFX=2+(JCX-JWEST)*IRAX                                                   FEEDBK.329
        JFP=JFX+1                                                                FEEDBK.330
        JFM=JFX-1                                                                FEEDBK.331
        DO 30 ICY=ISP1,INM2                                                      FEEDBK.332
          IFY=2+(ICY-ISOUTH)*IRAX                                                FEEDBK.333
          IFP=IFY+1                                                              FEEDBK.334
          IFM=IFY-1                                                              FEEDBK.335
          DO 30 K=1,KL                                                           FEEDBK.336
            IF(IFEED.EQ.1)THEN                                                   FEEDBK.337
              TIJ=TAN(IFY,JFX,K)                                                 FEEDBK.338
              TPJ=TAN(IFP,JFX,K)                                                 FEEDBK.339
              TMJ=TAN(IFM,JFX,K)                                                 FEEDBK.340
              TIP=TAN(IFY,JFP,K)                                                 FEEDBK.341
              TIM=TAN(IFY,JFM,K)                                                 FEEDBK.342
              TPP=TAN(IFP,JFP,K)                                                 FEEDBK.343
              TPM=TAN(IFP,JFM,K)                                                 FEEDBK.344
              TMP=TAN(IFM,JFP,K)                                                 FEEDBK.345
              TMM=TAN(IFM,JFM,K)                                                 FEEDBK.346
              TAV=F1*TIJ+F2*(TIP+TIM+TPJ+TMJ)+F3*(TPP+TPM+TMP+TMM)               FEEDBK.347
              TA(ICY,JCX,K)=TAV                                                  FEEDBK.348
            ENDIF                                                                FEEDBK.349
            IF(IFEED.GE.2)TA(ICY,JCX,K)=TAN(IFY,JFX,K)                           FEEDBK.350
   30     CONTINUE                                                               FEEDBK.351
                                                                                 FEEDBK.352
                                                                                 FEEDBK.353
C                                                                                FEEDBK.354
C--- NONHYDROSTATIC FEEDBACK                                                     FEEDBK.355
C                                                                                FEEDBK.356
cmic$ do all autoscope                                                           FEEDBK.357
c$omp parallel do default(shared)                                                FEEDBK.358
c$omp&private(icy,jcx,k,jfx,jfp,jfm,ify,ifp,ifm,tij,tpj,tmj,tip,tim,             FEEDBK.359
c$omp&tpp,tpm,tmp,tmm,tav)                                                       FEEDBK.360
      DO 35 JCX=JWP1,JEM2                                                        FEEDBK.361
        JFX=2+(JCX-JWEST)*IRAX                                                   FEEDBK.362
        JFP=JFX+1                                                                FEEDBK.363
        JFM=JFX-1                                                                FEEDBK.364
        DO 35 ICY=ISP1,INM2                                                      FEEDBK.365
          IFY=2+(ICY-ISOUTH)*IRAX                                                FEEDBK.366
          IFP=IFY+1                                                              FEEDBK.367
          IFM=IFY-1                                                              FEEDBK.368
          DO 35 K=1,KL+1                                                         FEEDBK.369
            IF(IFEED.EQ.1)THEN                                                   FEEDBK.370
              TIJ=WAN(IFY,JFX,K)                                                 FEEDBK.371
              TPJ=WAN(IFP,JFX,K)                                                 FEEDBK.372
              TMJ=WAN(IFM,JFX,K)                                                 FEEDBK.373
              TIP=WAN(IFY,JFP,K)                                                 FEEDBK.374
              TIM=WAN(IFY,JFM,K)                                                 FEEDBK.375
              TPP=WAN(IFP,JFP,K)                                                 FEEDBK.376
              TPM=WAN(IFP,JFM,K)                                                 FEEDBK.377
              TMP=WAN(IFM,JFP,K)                                                 FEEDBK.378
              TMM=WAN(IFM,JFM,K)                                                 FEEDBK.379
              TAV=F1*TIJ+F2*(TIP+TIM+TPJ+TMJ)+F3*(TPP+TPM+TMP+TMM)               FEEDBK.380
              WA(ICY,JCX,K)=TAV                                                  FEEDBK.381
            ENDIF                                                                FEEDBK.382
            IF(IFEED.GE.2)WA(ICY,JCX,K)=WAN(IFY,JFX,K)                           FEEDBK.383
   35     CONTINUE                                                               FEEDBK.384
                                                                                 FEEDBK.385
                                                                                 FEEDBK.386
C                                                                                FEEDBK.387
cmic$ do all autoscope                                                           FEEDBK.388
c$omp parallel do default(shared)                                                FEEDBK.389
c$omp&private(icy,jcx,k,jfx,jfp,jfm,ify,ifp,ifm,tij,tpj,tmj,tip,tim,             FEEDBK.390
c$omp&tpp,tpm,tmp,tmm,tav)                                                       FEEDBK.391
      DO 37 JCX=JWP1,JEM2                                                        FEEDBK.392
        JFX=2+(JCX-JWEST)*IRAX                                                   FEEDBK.393
        JFP=JFX+1                                                                FEEDBK.394
        JFM=JFX-1                                                                FEEDBK.395
        DO 37 ICY=ISP1,INM2                                                      FEEDBK.396
          IFY=2+(ICY-ISOUTH)*IRAX                                                FEEDBK.397
          IFP=IFY+1                                                              FEEDBK.398
          IFM=IFY-1                                                              FEEDBK.399
          DO 37 K=1,KL                                                           FEEDBK.400
            IF(IFEED.EQ.1)THEN                                                   FEEDBK.401
              TIJ=PPAN(IFY,JFX,K)                                                FEEDBK.402
              TPJ=PPAN(IFP,JFX,K)                                                FEEDBK.403
              TMJ=PPAN(IFM,JFX,K)                                                FEEDBK.404
              TIP=PPAN(IFY,JFP,K)                                                FEEDBK.405
              TIM=PPAN(IFY,JFM,K)                                                FEEDBK.406
              TPP=PPAN(IFP,JFP,K)                                                FEEDBK.407
              TPM=PPAN(IFP,JFM,K)                                                FEEDBK.408
              TMP=PPAN(IFM,JFP,K)                                                FEEDBK.409
              TMM=PPAN(IFM,JFM,K)                                                FEEDBK.410
              TAV=F1*TIJ+F2*(TIP+TIM+TPJ+TMJ)+F3*(TPP+TPM+TMP+TMM)               FEEDBK.411
              PPA(ICY,JCX,K)=TAV                                                 FEEDBK.412
            ENDIF                                                                FEEDBK.413
            IF(IFEED.GE.2)PPA(ICY,JCX,K)=PPAN(IFY,JFX,K)                         FEEDBK.414
   37     CONTINUE                                                               FEEDBK.415
                                                                                 FEEDBK.416
                                                                                 FEEDBK.417
C                                                                                FEEDBK.418
C-----CALCULATE THE QV FEEDBACK:                                                 FEEDBK.419
C                                                                                FEEDBK.420
      IF(IDRY(INEST).EQ.1)GOTO 60                                                FEEDBK.421
cmic$ do all autoscope                                                           FEEDBK.422
c$omp parallel do default(shared)                                                FEEDBK.423
c$omp&private(icy,jcx,k,jfx,jfp,jfm,ify,ifp,ifm,qvij,qvpj,qvmj,                  FEEDBK.424
c$omp&qvip,qvim,qvpp,qvpm,qvmp,qvmm,qvav)                                        FEEDBK.425
      DO 40 JCX=JWP1,JEM2                                                        FEEDBK.426
        JFX=2+(JCX-JWEST)*IRAX                                                   FEEDBK.427
        JFP=JFX+1                                                                FEEDBK.428
        JFM=JFX-1                                                                FEEDBK.429
        DO 40 ICY=ISP1,INM2                                                      FEEDBK.430
          IFY=2+(ICY-ISOUTH)*IRAX                                                FEEDBK.431
          IFP=IFY+1                                                              FEEDBK.432
          IFM=IFY-1                                                              FEEDBK.433
          DO 40 K=1,KL                                                           FEEDBK.434
            IF(IFEED.EQ.1)THEN                                                   FEEDBK.435
              QVIJ=QVAN(IFY,JFX,K)                                               FEEDBK.436
              QVPJ=QVAN(IFP,JFX,K)                                               FEEDBK.437
              QVMJ=QVAN(IFM,JFX,K)                                               FEEDBK.438
              QVIP=QVAN(IFY,JFP,K)                                               FEEDBK.439
              QVIM=QVAN(IFY,JFM,K)                                               FEEDBK.440
              QVPP=QVAN(IFP,JFP,K)                                               FEEDBK.441
              QVPM=QVAN(IFP,JFM,K)                                               FEEDBK.442
              QVMP=QVAN(IFM,JFP,K)                                               FEEDBK.443
              QVMM=QVAN(IFM,JFM,K)                                               FEEDBK.444
              QVAV=F1*QVIJ+F2*(QVIP+QVIM+QVPJ+QVMJ)+F3*(QVPP+QVPM+QVMP+          FEEDBK.445
     +             QVMM)                                                         FEEDBK.446
              QVA(ICY,JCX,K)=QVAV                                                FEEDBK.447
            ENDIF                                                                FEEDBK.448
            IF(IFEED.GE.2)QVA(ICY,JCX,K)=QVAN(IFY,JFX,K)                         FEEDBK.449
   40     CONTINUE                                                               FEEDBK.450
                                                                                 FEEDBK.451
                                                                                 FEEDBK.452
C                                                                                FEEDBK.453
C-----CALCULATE THE QC AND QR FEEDBACK:                                          FEEDBK.454
C                                                                                FEEDBK.455
      IF(IMOIST(INEST).NE.2)GOTO 60                                              FEEDBK.456
cmic$ do all autoscope                                                           FEEDBK.457
c$omp parallel do default(shared)                                                FEEDBK.458
c$omp&private(icy,jcx,k,jfx,jfp,jfm,ify,ifp,ifm,                                 FEEDBK.459
c$omp&qcij,qcpj,qcmj,qcip,qcim,qcpp,qcpm,qcmp,qcmm,qcav,                         FEEDBK.460
c$omp&qrij,qrpj,qrmj,qrip,qrim,qrpp,qrpm,qrmp,qrmm,qrav,                         FEEDBK.461
c$omp&qiij,qipj,qimj,qiip,qiim,qipp,qipm,qimp,qimm,qiav,                         FEEDBK.462
c$omp&qniij,qnipj,qnimj,qniip,qniim,qnipp,qnipm,qnimp,qnimm,qniav,               FEEDBK.463
c$omp&qgij,qgpj,qgmj,qgip,qgim,qgpp,qgpm,qgmp,qgmm,qgav,                         FEEDBK.464
c$omp&qncij,qncpj,qncmj,qncip,qncim,qncpp,qncpm,qncmp,qncmm,qncav)               FEEDBK.465
      DO 50 JCX=JWP1,JEM2                                                        FEEDBK.466
        JFX=2+(JCX-JWEST)*IRAX                                                   FEEDBK.467
        JFP=JFX+1                                                                FEEDBK.468
        JFM=JFX-1                                                                FEEDBK.469
        DO 50 ICY=ISP1,INM2                                                      FEEDBK.470
          IFY=2+(ICY-ISOUTH)*IRAX                                                FEEDBK.471
          IFP=IFY+1                                                              FEEDBK.472
          IFM=IFY-1                                                              FEEDBK.473
          DO 51 K=1,KL                                                           FEEDBK.474
            IF(IFEED.EQ.1)THEN                                                   FEEDBK.475
              QCIJ=QCAN(IFY,JFX,K)                                               FEEDBK.476
              QCPJ=QCAN(IFP,JFX,K)                                               FEEDBK.477
              QCMJ=QCAN(IFM,JFX,K)                                               FEEDBK.478
              QCIP=QCAN(IFY,JFP,K)                                               FEEDBK.479
              QCIM=QCAN(IFY,JFM,K)                                               FEEDBK.480
              QCPP=QCAN(IFP,JFP,K)                                               FEEDBK.481
              QCPM=QCAN(IFP,JFM,K)                                               FEEDBK.482
              QCMP=QCAN(IFM,JFP,K)                                               FEEDBK.483
              QCMM=QCAN(IFM,JFM,K)                                               FEEDBK.484
              QCAV=F1*QCIJ+F2*(QCIP+QCIM+QCPJ+QCMJ)+F3*(QCPP+QCPM+QCMP+          FEEDBK.485
     +             QCMM)                                                         FEEDBK.486
              QCA(ICY,JCX,K)=QCAV                                                FEEDBK.487
            ENDIF                                                                FEEDBK.488
            IF(IFEED.GE.2)QCA(ICY,JCX,K)=QCAN(IFY,JFX,K)                         FEEDBK.489
   51     CONTINUE                                                               FEEDBK.490
          DO 52 K=1,KL                                                           FEEDBK.491
            IF(IFEED.EQ.1)THEN                                                   FEEDBK.492
              QRIJ=QRAN(IFY,JFX,K)                                               FEEDBK.493
              QRPJ=QRAN(IFP,JFX,K)                                               FEEDBK.494
              QRMJ=QRAN(IFM,JFX,K)                                               FEEDBK.495
              QRIP=QRAN(IFY,JFP,K)                                               FEEDBK.496
              QRIM=QRAN(IFY,JFM,K)                                               FEEDBK.497
              QRPP=QRAN(IFP,JFP,K)                                               FEEDBK.498
              QRPM=QRAN(IFP,JFM,K)                                               FEEDBK.499
              QRMP=QRAN(IFM,JFP,K)                                               FEEDBK.500
              QRMM=QRAN(IFM,JFM,K)                                               FEEDBK.501
              QRAV=F1*QRIJ+F2*(QRIP+QRIM+QRPJ+QRMJ)+F3*(QRPP+QRPM+QRMP+          FEEDBK.502
     +             QRMM)                                                         FEEDBK.503
              QRA(ICY,JCX,K)=QRAV                                                FEEDBK.504
            ENDIF                                                                FEEDBK.505
            IF(IFEED.GE.2)QRA(ICY,JCX,K)=QRAN(IFY,JFX,K)                         FEEDBK.506
   52     CONTINUE                                                               FEEDBK.507
          IF(IICE.EQ.1)THEN                                                      FEEDBK.508
            DO 53 K=1,KL                                                         FEEDBK.509
              IF(IFEED.GE.2)THEN                                                 FEEDBK.510
                QIA(ICY,JCX,K)=QIAN(IFY,JFX,K)                                   FEEDBK.511
                QNIA(ICY,JCX,K)=QNIAN(IFY,JFX,K)                                 FEEDBK.512
              ELSE                                                               FEEDBK.513
                QIIJ=QIAN(IFY,JFX,K)                                             FEEDBK.514
                QIPJ=QIAN(IFP,JFX,K)                                             FEEDBK.515
                QIMJ=QIAN(IFM,JFX,K)                                             FEEDBK.516
                QIIP=QIAN(IFY,JFP,K)                                             FEEDBK.517
                QIIM=QIAN(IFY,JFM,K)                                             FEEDBK.518
                QIPP=QIAN(IFP,JFP,K)                                             FEEDBK.519
                QIPM=QIAN(IFP,JFM,K)                                             FEEDBK.520
                QIMP=QIAN(IFM,JFP,K)                                             FEEDBK.521
                QIMM=QIAN(IFM,JFM,K)                                             FEEDBK.522
                QIAV=F1*QIIJ+F2*(QIIP+QIIM+QIPJ+QIMJ)+F3*(QIPP+QIPM+QIMP         FEEDBK.523
     +               +QIMM)                                                      FEEDBK.524
                QIA(ICY,JCX,K)=QIAV                                              FEEDBK.525
                QNIIJ=QNIAN(IFY,JFX,K)                                           FEEDBK.526
                QNIPJ=QNIAN(IFP,JFX,K)                                           FEEDBK.527
                QNIMJ=QNIAN(IFM,JFX,K)                                           FEEDBK.528
                QNIIP=QNIAN(IFY,JFP,K)                                           FEEDBK.529
                QNIIM=QNIAN(IFY,JFM,K)                                           FEEDBK.530
                QNIPP=QNIAN(IFP,JFP,K)                                           FEEDBK.531
                QNIPM=QNIAN(IFP,JFM,K)                                           FEEDBK.532
                QNIMP=QNIAN(IFM,JFP,K)                                           FEEDBK.533
                QNIMM=QNIAN(IFM,JFM,K)                                           FEEDBK.534
                QNIAV=F1*QNIIJ+F2*(QNIIP+QNIIM+QNIPJ+QNIMJ)+F3*(QNIPP+           FEEDBK.535
     +                QNIPM+QNIMP+QNIMM)                                         FEEDBK.536
                QNIA(ICY,JCX,K)=QNIAV                                            FEEDBK.537
              ENDIF                                                              FEEDBK.538
              IF(IICEG.EQ.1)THEN                                                 FEEDBK.539
                IF(IFEED.GE.2)THEN                                               FEEDBK.540
                  QGA(ICY,JCX,K)=QGAN(IFY,JFX,K)                                 FEEDBK.541
                  QNCA(ICY,JCX,K)=QNCAN(IFY,JFX,K)                               FEEDBK.542
                ELSE                                                             FEEDBK.543
                  QGIJ=QGAN(IFY,JFX,K)                                           FEEDBK.544
                  QGPJ=QGAN(IFP,JFX,K)                                           FEEDBK.545
                  QGMJ=QGAN(IFM,JFX,K)                                           FEEDBK.546
                  QGIP=QGAN(IFY,JFP,K)                                           FEEDBK.547
                  QGIM=QGAN(IFY,JFM,K)                                           FEEDBK.548
                  QGPP=QGAN(IFP,JFP,K)                                           FEEDBK.549
                  QGPM=QGAN(IFP,JFM,K)                                           FEEDBK.550
                  QGMP=QGAN(IFM,JFP,K)                                           FEEDBK.551
                  QGMM=QGAN(IFM,JFM,K)                                           FEEDBK.552
                  QGAV=F1*QGIJ+F2*(QGIP+QGIM+QGPJ+QGMJ)+F3*(QGPP+QGPM+           FEEDBK.553
     +                 QGMP+QGMM)                                                FEEDBK.554
                  QGA(ICY,JCX,K)=QGAV                                            FEEDBK.555
                  QNCIJ=QNCAN(IFY,JFX,K)                                         FEEDBK.556
                  QNCPJ=QNCAN(IFP,JFX,K)                                         FEEDBK.557
                  QNCMJ=QNCAN(IFM,JFX,K)                                         FEEDBK.558
                  QNCIP=QNCAN(IFY,JFP,K)                                         FEEDBK.559
                  QNCIM=QNCAN(IFY,JFM,K)                                         FEEDBK.560
                  QNCPP=QNCAN(IFP,JFP,K)                                         FEEDBK.561
                  QNCPM=QNCAN(IFP,JFM,K)                                         FEEDBK.562
                  QNCMP=QNCAN(IFM,JFP,K)                                         FEEDBK.563
                  QNCMM=QNCAN(IFM,JFM,K)                                         FEEDBK.564
                  QNCAV=F1*QNCIJ+F2*(QNCIP+QNCIM+QNCPJ+QNCMJ)+F3*(QNCPP+         FEEDBK.565
     +                  QNCPM+QNCMP+QNCMM)                                       FEEDBK.566
                  QNCA(ICY,JCX,K)=QNCAV                                          FEEDBK.567
                ENDIF                                                            FEEDBK.568
              ENDIF                                                              FEEDBK.569
   53       CONTINUE                                                             FEEDBK.570
          ENDIF                                                                  FEEDBK.571
   50   CONTINUE                                                                 FEEDBK.572
                                                                                 FEEDBK.573
C                                                                                FEEDBK.574
C-----CALCULATE THE U AND V FEEDBACK:                                            FEEDBK.575
C     VELOCITIES ARE FED BACK IN DECOUPLED FORM.                                 FEEDBK.576
C                                                                                FEEDBK.577
   60 CONTINUE                                                                   FEEDBK.578
cmic$ do all autoscope                                                           FEEDBK.579
c$omp parallel do default(shared)                                                FEEDBK.580
c$omp&private(icy,jcx,k,jfx,jfp,jfm,jfm2,ify,ifp,ifm,ifm2,                       FEEDBK.581
c$omp&uij,upj,umj,uip,uim,upp,upm,ump,umm,uav,                                   FEEDBK.582
c$omp&vij,vpj,vmj,vip,vim,vpp,vpm,vmp,vmm,vav)                                   FEEDBK.583
C     FEED BACK, DECOUPLING WITH NEST PDOTA AS WE GO                             FEEDBK.584
      DO 70 JCX=JWP1,JEM1                                                        FEEDBK.585
        JFX=1+(JCX-JWEST)*IRAX                                                   FEEDBK.586
        JFP=JFX+1                                                                FEEDBK.587
        JFM=JFX-1                                                                FEEDBK.588
        JFM2=JFX-2                                                               FEEDBK.589
        DO 70 ICY=ISP1,INM1                                                      FEEDBK.590
          IFY=1+(ICY-ISOUTH)*IRAX                                                FEEDBK.591
          IFP=IFY+1                                                              FEEDBK.592
          IFM=IFY-1                                                              FEEDBK.593
          IFM2=IFY-2                                                             FEEDBK.594
          DO 70 K=1,KL                                                           FEEDBK.595
            IF(IFEED.EQ.1)THEN                                                   FEEDBK.596
              UPP=UAN(IFP,JFP,K)*RPDOTAN(IFP,JFP)                                FEEDBK.597
              UPJ=UAN(IFP,JFX,K)*RPDOTAN(IFP,JFX)                                FEEDBK.598
              UPM=UAN(IFP,JFM,K)*RPDOTAN(IFP,JFM)                                FEEDBK.599
              UIP=UAN(IFY,JFP,K)*RPDOTAN(IFY,JFP)                                FEEDBK.600
              UIJ=UAN(IFY,JFX,K)*RPDOTAN(IFY,JFX)                                FEEDBK.601
              UIM=UAN(IFY,JFM,K)*RPDOTAN(IFY,JFM)                                FEEDBK.602
              UMP=UAN(IFM,JFP,K)*RPDOTAN(IFM,JFP)                                FEEDBK.603
              UMJ=UAN(IFM,JFX,K)*RPDOTAN(IFM,JFX)                                FEEDBK.604
              UMM=UAN(IFM,JFM,K)*RPDOTAN(IFM,JFM)                                FEEDBK.605
              UAV=F1*UIJ+F2*(UIP+UIM+UPJ+UMJ)+F3*(UPP+UPM+UMP+UMM)               FEEDBK.606
              UA(ICY,JCX,K)=UAV                                                  FEEDBK.607
            ENDIF                                                                FEEDBK.608
            IF(IFEED.GE.2)UA(ICY,JCX,K)=UAN(IFY,JFX,K)*RPDOTAN(IFY,JFX)          FEEDBK.609
            IF(IFEED.EQ.1)THEN                                                   FEEDBK.610
              VPP=VAN(IFP,JFP,K)*RPDOTAN(IFP,JFP)                                FEEDBK.611
              VPJ=VAN(IFP,JFX,K)*RPDOTAN(IFP,JFX)                                FEEDBK.612
              VPM=VAN(IFP,JFM,K)*RPDOTAN(IFP,JFM)                                FEEDBK.613
              VIP=VAN(IFY,JFP,K)*RPDOTAN(IFY,JFP)                                FEEDBK.614
              VIJ=VAN(IFY,JFX,K)*RPDOTAN(IFY,JFX)                                FEEDBK.615
              VIM=VAN(IFY,JFM,K)*RPDOTAN(IFY,JFM)                                FEEDBK.616
              VMP=VAN(IFM,JFP,K)*RPDOTAN(IFM,JFP)                                FEEDBK.617
              VMJ=VAN(IFM,JFX,K)*RPDOTAN(IFM,JFX)                                FEEDBK.618
              VMM=VAN(IFM,JFM,K)*RPDOTAN(IFM,JFM)                                FEEDBK.619
              VAV=F1*VIJ+F2*(VIP+VIM+VPJ+VMJ)+F3*(VPP+VPM+VMP+VMM)               FEEDBK.620
              VA(ICY,JCX,K)=VAV                                                  FEEDBK.621
            ENDIF                                                                FEEDBK.622
            IF(IFEED.GE.2)VA(ICY,JCX,K)=VAN(IFY,JFX,K)*RPDOTAN(IFY,JFX)          FEEDBK.623
   70     CONTINUE                                                               FEEDBK.624
                                                                                 FEEDBK.625
                                                                                 FEEDBK.626
C                                                                                FEEDBK.627
C--- SMOOTH FIELDS FOR FEEDBACK (IF IFEED GT.2)                                  FEEDBK.628
C                                                                                FEEDBK.629
      IF(IFEED.GE.3)THEN                                                         FEEDBK.630
cmic$ do all autoscope                                                           FEEDBK.631
c$omp parallel do default(shared)                                                FEEDBK.632
c$omp&private(i,j)                                                               FEEDBK.633
        DO 1141 I=1,ILX                                                          FEEDBK.634
          DO 1141 J=1,JLX                                                        FEEDBK.635
            PTEN(I,J)=PSA(I,J)                                                   FEEDBK.636
 1141     CONTINUE                                                               FEEDBK.637
                                                                                 FEEDBK.638
C                                                                                FEEDBK.639
C                                                                                FEEDBK.640
cmic$ do all autoscope                                                           FEEDBK.641
cmic$1 shared (il,jl,isp1,inm2,jwp1,jem2)                                        FEEDBK.642
cmic$2 private (hsc1,hsc2,hsc3,hsc4,hsc5,hsc6)                                   FEEDBK.643
c$omp parallel do default(shared)                                                FEEDBK.644
c$omp&private(i,j,k,hsc1,hsc2,hsc3,hsc4,hsc5,hsc6)                               25JAN00.179
        DO 140 K=1,KL                                                            FEEDBK.646
C                                                                                FEEDBK.647
C                                                                                FEEDBK.648
          DO 101 I=1,INM1+2                                                      FEEDBK.649
            DO 101 J=1,JEM1+2                                                    FEEDBK.650
              HSC1(I,J)=PPA(I,J,K)/PSA(I,J)                                      FEEDBK.651
  101       CONTINUE                                                             FEEDBK.652
                                                                                 FEEDBK.653
          IF(IFEED.EQ.3)THEN                                                     FEEDBK.654
            CALL SMTHER(HSC1,MIX,MJX,2,ISP1-2,INM2+2,JWP1-2,JEM2+2)              FEEDBK.655
          ELSEIF(IFEED.EQ.4)THEN                                                 FEEDBK.656
            CALL SMT2(HSC1,MIX,MJX,ISP1,INM2,JWP1,JEM2)                          FEEDBK.657
          ENDIF                                                                  FEEDBK.658
          DO 102 I=ISP1,INM2                                                     FEEDBK.659
            DO 102 J=JWP1,JEM2                                                   FEEDBK.660
              PPA(I,J,K)=HSC1(I,J)*PTEN(I,J)                                     FEEDBK.661
  102       CONTINUE                                                             FEEDBK.662
                                                                                 FEEDBK.663
C                                                                                FEEDBK.664
C                                                                                FEEDBK.665
          DO 100 I=2,INM1                                                        FEEDBK.666
            DO 100 J=2,JEM1                                                      FEEDBK.667
              HSC1(I,J)=TA(I,J,K)/PSA(I,J)                                       FEEDBK.668
  100       CONTINUE                                                             FEEDBK.669
                                                                                 FEEDBK.670
          IF(IFEED.EQ.3)THEN                                                     FEEDBK.671
            CALL SMTHER(HSC1,MIX,MJX,2,ISP1,INM2,JWP1,JEM2)                      FEEDBK.672
          ELSEIF(IFEED.EQ.4)THEN                                                 FEEDBK.673
            CALL SMT2(HSC1,MIX,MJX,ISP1,INM2,JWP1,JEM2)                          FEEDBK.674
          ENDIF                                                                  FEEDBK.675
          DO 105 I=ISP1,INM2                                                     FEEDBK.676
            DO 105 J=JWP1,JEM2                                                   FEEDBK.677
              TA(I,J,K)=HSC1(I,J)*PTEN(I,J)                                      FEEDBK.678
  105       CONTINUE                                                             FEEDBK.679
                                                                                 FEEDBK.680
C                                                                                FEEDBK.681
C                                                                                FEEDBK.682
C                                                                                FEEDBK.683
          IF(IDRY(INEST).EQ.0)THEN                                               FEEDBK.684
            DO 110 I=1,INM1                                                      FEEDBK.685
              DO 110 J=1,JEM1                                                    FEEDBK.686
                HSC1(I,J)=QVA(I,J,K)/PSA(I,J)                                    FEEDBK.687
  110         CONTINUE                                                           FEEDBK.688
                                                                                 FEEDBK.689
            IF(IFEED.EQ.3)THEN                                                   FEEDBK.690
              CALL SMTHER(HSC1,MIX,MJX,2,ISP1,INM2,JWP1,JEM2)                    FEEDBK.691
            ELSEIF(IFEED.EQ.4)THEN                                               FEEDBK.692
              CALL SMT2(HSC1,MIX,MJX,ISP1,INM2,JWP1,JEM2)                        FEEDBK.693
            ENDIF                                                                FEEDBK.694
            DO 115 I=ISP1,INM2                                                   FEEDBK.695
              DO 115 J=JWP1,JEM2                                                 FEEDBK.696
                IF(HSC1(I,J).LE.0)HSC1(I,J)=1.E-14                               FEEDBK.697
                QVA(I,J,K)=HSC1(I,J)*PTEN(I,J)                                   FEEDBK.698
  115         CONTINUE                                                           FEEDBK.699
                                                                                 FEEDBK.700
C                                                                                FEEDBK.701
C                                                                                FEEDBK.702
C                                                                                FEEDBK.703
            IF(IMOIST(INEST).EQ.2)THEN                                           FEEDBK.704
              DO 120 I=1,INM1+1                                                  25JAN00.180
                DO 120 J=1,JEM1+1                                                25JAN00.181
                  HSC1(I,J)=QCA(I,J,K)/PSA(I,J)                                  FEEDBK.707
                  HSC2(I,J)=QRA(I,J,K)/PSA(I,J)                                  FEEDBK.708
                  IF(IICE.EQ.1)THEN                                              FEEDBK.709
                    HSC3(I,J)=QIA(I,J,K)/PSA(I,J)                                FEEDBK.710
                    HSC4(I,J)=QNIA(I,J,K)/PSA(I,J)                               FEEDBK.711
                    IF(IICEG.EQ.1)THEN                                           FEEDBK.712
                      HSC5(I,J)=QGA(I,J,K)/PSA(I,J)                              FEEDBK.713
                      HSC6(I,J)=QNCA(I,J,K)/PSA(I,J)                             FEEDBK.714
                    ENDIF                                                        FEEDBK.715
                  ENDIF                                                          FEEDBK.716
  120           CONTINUE                                                         FEEDBK.717
                                                                                 FEEDBK.718
              IF(IFEED.EQ.3)THEN                                                 FEEDBK.719
                CALL SMTHER(HSC1,MIX,MJX,2,ISP1,INM2,JWP1,JEM2)                  FEEDBK.720
                CALL SMTHER(HSC2,MIX,MJX,2,ISP1,INM2,JWP1,JEM2)                  FEEDBK.721
                IF(IICE.EQ.1)THEN                                                FEEDBK.722
                  CALL SMTHER(HSC3,MIX,MJX,2,ISP1,INM2,JWP1,JEM2)                FEEDBK.723
                  CALL SMTHER(HSC4,MIX,MJX,2,ISP1,INM2,JWP1,JEM2)                FEEDBK.724
                  IF(IICEG.EQ.1)THEN                                             FEEDBK.725
                    CALL SMTHER(HSC5,MIX,MJX,2,ISP1,INM2,JWP1,JEM2)              FEEDBK.726
                    CALL SMTHER(HSC6,MIX,MJX,2,ISP1,INM2,JWP1,JEM2)              FEEDBK.727
                  ENDIF                                                          FEEDBK.728
                ENDIF                                                            FEEDBK.729
              ELSEIF(IFEED.EQ.4)THEN                                             FEEDBK.730
                CALL SMT2(HSC1,MIX,MJX,ISP1,INM2,JWP1,JEM2)                      FEEDBK.731
                CALL SMT2(HSC2,MIX,MJX,ISP1,INM2,JWP1,JEM2)                      FEEDBK.732
                IF(IICE.EQ.1)THEN                                                FEEDBK.733
                  CALL SMT2(HSC3,MIX,MJX,ISP1,INM2,JWP1,JEM2)                    FEEDBK.734
                  CALL SMT2(HSC4,MIX,MJX,ISP1,INM2,JWP1,JEM2)                    FEEDBK.735
                  IF(IICEG.EQ.1)THEN                                             FEEDBK.736
                    CALL SMT2(HSC5,MIX,MJX,ISP1,INM2,JWP1,JEM2)                  25JAN00.182
                    CALL SMT2(HSC6,MIX,MJX,ISP1,INM2,JWP1,JEM2)                  25JAN00.183
                  ENDIF                                                          FEEDBK.739
                ENDIF                                                            FEEDBK.740
              ENDIF                                                              FEEDBK.741
              DO 125 I=ISP1,INM2                                                 FEEDBK.742
                DO 125 J=JWP1,JEM2                                               FEEDBK.743
                  IF(HSC1(I,J).LT.0)HSC1(I,J)=0.                                 FEEDBK.744
                  IF(HSC2(I,J).LT.0)HSC2(I,J)=0.                                 FEEDBK.745
                  QCA(I,J,K)=HSC1(I,J)*PTEN(I,J)                                 FEEDBK.746
                  QRA(I,J,K)=HSC2(I,J)*PTEN(I,J)                                 FEEDBK.747
                  IF(IICE.EQ.1)THEN                                              FEEDBK.748
                    IF(HSC3(I,J).LT.0.)HSC3(I,J)=0.                              FEEDBK.749
                    IF(HSC4(I,J).LT.0.)HSC4(I,J)=0.                              FEEDBK.750
                    QIA(I,J,K)=HSC3(I,J)*PTEN(I,J)                               FEEDBK.751
                    QNIA(I,J,K)=HSC4(I,J)*PTEN(I,J)                              FEEDBK.752
                    IF(IICEG.EQ.1)THEN                                           FEEDBK.753
                      IF(HSC5(I,J).LT.0)HSC5(I,J)=0.                             FEEDBK.754
                      IF(HSC6(I,J).LT.0)HSC6(I,J)=0.                             FEEDBK.755
                      QGA(I,J,K)=HSC5(I,J)*PTEN(I,J)                             FEEDBK.756
                      QNCA(I,J,K)=HSC6(I,J)*PTEN(I,J)                            FEEDBK.757
                    ENDIF                                                        FEEDBK.758
                  ENDIF                                                          FEEDBK.759
  125           CONTINUE                                                         FEEDBK.760
                                                                                 FEEDBK.761
            ENDIF                                                                FEEDBK.762
          ENDIF                                                                  FEEDBK.763
C                                                                                FEEDBK.764
C                                                                                FEEDBK.765
C                                                                                FEEDBK.766
          DO 130 J=JWEST,JEAST                                                   FEEDBK.767
            DO 130 I=ISOUTH,INORTH                                               FEEDBK.768
              HSC1(I,J)=UA(I,J,K)                                                FEEDBK.769
              HSC2(I,J)=VA(I,J,K)                                                FEEDBK.770
  130       CONTINUE                                                             FEEDBK.771
                                                                                 FEEDBK.772
          IF(IFEED.EQ.3)THEN                                                     FEEDBK.773
            CALL SMTHER(HSC1,MIX,MJX,2,ISP1,INM1,JWP1,JEM1)                      FEEDBK.774
            CALL SMTHER(HSC2,MIX,MJX,2,ISP1,INM1,JWP1,JEM1)                      FEEDBK.775
          ELSEIF(IFEED.EQ.4)THEN                                                 FEEDBK.776
            CALL SMT2(HSC1,MIX,MJX,ISP1,INM1,JWP1,JEM1)                          FEEDBK.777
            CALL SMT2(HSC2,MIX,MJX,ISP1,INM1,JWP1,JEM1)                          FEEDBK.778
          ENDIF                                                                  FEEDBK.779
          DO 135 I=ISP1,INM1                                                     FEEDBK.780
            DO 135 J=JWP1,JEM1                                                   FEEDBK.781
              UA(I,J,K)=HSC1(I,J)                                                FEEDBK.782
              VA(I,J,K)=HSC2(I,J)                                                FEEDBK.783
  135       CONTINUE                                                             FEEDBK.784
                                                                                 FEEDBK.785
C                                                                                FEEDBK.786
C                                                                                FEEDBK.787
C                                                                                FEEDBK.788
          IF(INAV.EQ.1)THEN                                                      FEEDBK.789
            DO 136 I=1,INM1                                                      FEEDBK.790
              DO 136 J=1,JEM1                                                    FEEDBK.791
                HSC1(I,J)=TURKE(I,J,K)                                           FEEDBK.792
                HSC2(I,J)=PTHLSB(I,J,K)                                          FEEDBK.793
  136         CONTINUE                                                           FEEDBK.794
                                                                                 FEEDBK.795
            IF(IFEED.EQ.3)THEN                                                   FEEDBK.796
              CALL SMTHER(HSC1,MIX,MJX,2,ISP1,INM2,JWP1,JEM2)                    FEEDBK.797
              CALL SMTHER(HSC2,MIX,MJX,2,ISP1,INM2,JWP1,JEM2)                    FEEDBK.798
            ELSEIF(IFEED.EQ.4)THEN                                               FEEDBK.799
              CALL SMT2(HSC1,MIX,MJX,ISP1,INM2,JWP1,JEM2)                        FEEDBK.800
              CALL SMT2(HSC2,MIX,MJX,ISP1,INM2,JWP1,JEM2)                        FEEDBK.801
            ENDIF                                                                FEEDBK.802
            DO 137 I=ISP1,INM2                                                   FEEDBK.803
              DO 137 J=JWP1,JEM2                                                 FEEDBK.804
                TURKE(I,J,K)=HSC1(I,J)                                           FEEDBK.805
                IF(TURKE(I,J,K).LT.1.E-06)TURKE(I,J,K)=1.E-06                    FEEDBK.806
                PTHLSB(I,J,K)=HSC2(I,J)                                          FEEDBK.807
  137         CONTINUE                                                           FEEDBK.808
                                                                                 FEEDBK.809
            DO 138 I=1,INM1                                                      FEEDBK.810
              DO 138 J=1,JEM1                                                    FEEDBK.811
                HSC1(I,J)=PQWSB(I,J,K)                                           FEEDBK.812
                HSC2(I,J)=PTHLQWB(I,J,K)                                         FEEDBK.813
  138         CONTINUE                                                           FEEDBK.814
                                                                                 FEEDBK.815
            IF(IFEED.EQ.3)THEN                                                   FEEDBK.816
              CALL SMTHER(HSC1,MIX,MJX,2,ISP1,INM2,JWP1,JEM2)                    FEEDBK.817
              CALL SMTHER(HSC2,MIX,MJX,2,ISP1,INM2,JWP1,JEM2)                    FEEDBK.818
            ELSEIF(IFEED.EQ.4)THEN                                               FEEDBK.819
              CALL SMT2(HSC1,MIX,MJX,ISP1,INM2,JWP1,JEM2)                        FEEDBK.820
              CALL SMT2(HSC2,MIX,MJX,ISP1,INM2,JWP1,JEM2)                        FEEDBK.821
            ENDIF                                                                FEEDBK.822
            DO 139 I=ISP1,INM2                                                   FEEDBK.823
              DO 139 J=JWP1,JEM2                                                 FEEDBK.824
                PQWSB(I,J,K)=HSC1(I,J)                                           FEEDBK.825
                PTHLQWB(I,J,K)=HSC2(I,J)                                         FEEDBK.826
  139         CONTINUE                                                           FEEDBK.827
                                                                                 FEEDBK.828
          ENDIF                                                                  FEEDBK.829
C  Add TKEA feedback for GS - 20021217                                           19DEC02.951
          IF(IGSPBL.EQ.1)THEN                                                    19DEC02.952
            DO 141 I=1,INM1                                                      19DEC02.953
              DO 141 J=1,JEM1                                                    19DEC02.954
                HSC1(I,J)=TKEA(I,J,K)                                            19DEC02.955
  141         CONTINUE                                                           19DEC02.956
                                                                                 19DEC02.957
            IF(IFEED.EQ.3)THEN                                                   19DEC02.958
              CALL SMTHER(HSC1,MIX,MJX,2,ISP1,INM2,JWP1,JEM2)                    19DEC02.959
            ELSEIF(IFEED.EQ.4)THEN                                               19DEC02.960
              CALL SMT2(HSC1,MIX,MJX,ISP1,INM2,JWP1,JEM2)                        19DEC02.961
            ENDIF                                                                19DEC02.962
            DO 142 I=ISP1,INM2                                                   19DEC02.963
              DO 142 J=JWP1,JEM2                                                 19DEC02.964
                TKEA(I,J,K)=HSC1(I,J)                                            19DEC02.965
                IF(TKEA(I,J,K).LT.1.E-06)TKEA(I,J,K)=1.E-06                      19DEC02.966
  142         CONTINUE                                                           19DEC02.967
            ENDIF                                                                19DEC02.968
  140   CONTINUE                                                                 FEEDBK.830
C                                                                                FEEDBK.831
C--- NON-HYDROSTATIC ONLY: W                                                     FEEDBK.832
cmic$ do all autoscope                                                           FEEDBK.833
cmic$1 shared (il,jl,isp1,inm2,jwp1,jem2)                                        FEEDBK.834
cmic$2 private (hsc1,hsc2)                                                       FEEDBK.835
c$omp parallel do default(shared)                                                FEEDBK.836
c$omp&private(i,j,k,hsc1,hsc2)                                                   25JAN00.184
        DO 160 K=1,KL+1                                                          FEEDBK.838
          DO 150 I=1,INM1+2                                                      FEEDBK.839
            DO 150 J=1,JEM1+2                                                    FEEDBK.840
              HSC1(I,J)=WA(I,J,K)/PSA(I,J)                                       FEEDBK.841
  150       CONTINUE                                                             FEEDBK.842
                                                                                 FEEDBK.843
          IF(IFEED.EQ.3)THEN                                                     FEEDBK.844
            CALL SMTHER(HSC1,MIX,MJX,2,ISP1-2,INM2+2,JWP1-2,JEM2+2)              FEEDBK.845
          ELSEIF(IFEED.EQ.4)THEN                                                 FEEDBK.846
            CALL SMT2(HSC1,MIX,MJX,ISP1,INM2,JWP1,JEM2)                          FEEDBK.847
          ENDIF                                                                  FEEDBK.848
          DO 155 I=ISP1,INM2                                                     FEEDBK.849
            DO 155 J=JWP1,JEM2                                                   FEEDBK.850
              WA(I,J,K)=HSC1(I,J)*PTEN(I,J)                                      FEEDBK.851
  155       CONTINUE                                                             FEEDBK.852
                                                                                 FEEDBK.853
  160   CONTINUE                                                                 FEEDBK.854
      ENDIF                                                                      FEEDBK.855
C     RE-COUPLE NEST REGION OF COARSE DOMAIN FOR U AND V                         FEEDBK.856
cmic$ do all autoscope                                                           FEEDBK.857
c$omp parallel do default(shared)                                                FEEDBK.858
c$omp&private(icy,jcx,k)                                                         FEEDBK.859
      DO K=1,KL                                                                  FEEDBK.860
        DO JCX=JWEST,JEAST                                                       FEEDBK.861
          DO ICY=ISOUTH,INORTH                                                   FEEDBK.862
            UA(ICY,JCX,K)=UA(ICY,JCX,K)*PDOTA(ICY,JCX)                           FEEDBK.863
            VA(ICY,JCX,K)=VA(ICY,JCX,K)*PDOTA(ICY,JCX)                           FEEDBK.864
          ENDDO                                                                  FEEDBK.865
        ENDDO                                                                    FEEDBK.866
      ENDDO                                                                      FEEDBK.867
C                                                                                FEEDBK.868
      RETURN                                                                     FEEDBK.869
      END                                                                        FEEDBK.870
                                                                                 FEEDBK.871
