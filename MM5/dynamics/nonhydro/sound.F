#ifndef IBMopt                                                                   23DEC04.1057
                                                                                 23DEC04.1058
      SUBROUTINE SOUND(IYY,JXX,UB,VB,TB,PR1,RHO1,                                SOUND.1
     &                 UA,VA,TA,QVB,PSA,HT,MSFD,MSFX,                            SOUND.2
     &                 UTENS,VTENS,PPTENS,WTENS,                                 SOUND.3
     &                 DX,DTL,INEST,KTAU)                                        SOUND.4
      IMPLICIT NONE                                                              SOUND.5
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         SOUND.6
C                                                                      C         SOUND.7
C     CHANGES:                                                         C         SOUND.8
C          REORDERING LOOPS FOR PARALLEL EFFICIENCY                    C         SOUND.9
C          ADDITION OF HP PARALLEL DIRECTIVES                          C         SOUND.10
C          REMOVAL OF SGI SHARE PARALLEL DIRECTIVES (UNNECESSARY)      C         SOUND.11
C          REPLACE UA -> U3D ETC                                       C         SOUND.12
C          IMPROVE UPPER RAD CONDITION EFFICIENCY                      C         SOUND.13
C     NOTE:                                                            C         SOUND.14
C          THE LINE NUMBERS IN SOUND.F HAVE BEEN REDONE FOR            C         SOUND.15
C          RELEASE-2-7 (DECEMBER 21, 1997)                             C         SOUND.16
C     CHANGES:                                                         C         SOUND.17
C          USE COMMON BLOCKS TO STORE PSTAR-DERIVED VARIABLES (2.9)    C         SOUND.18
C          USE COMMON BLOCKS TO STORE REF STATE AND UP. RAD. VARIABLES C         SOUND.19
C           (2.9)                                                      C         SOUND.20
C          CHANGE NEST B.C. ON P` TO JUST INTERPOLATE 2ND PT FROM      C         SOUND.21
C           COARSE MESH LIKE ALL OTHER VARIABLES                       C         SOUND.22
C                                                                      C         SOUND.23
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         SOUND.24
#     include <parame.incl>                                                      SOUND.25
#     include <param2.incl>                                                      SOUND.26
#     include <param3.incl>                                                      SOUND.27
#     include <pmoist.incl>                                                      SOUND.28
#     include <nonhyd.incl>                                                      SOUND.29
#     include <nonhydb.incl>                                                     SOUND.30
#     include <nhcnst.incl>                                                      SOUND.31
C                                                                                SOUND.32
#     include <uprad.incl>                                                       SOUND.33
#     include <rpstar.incl>                                                      SOUND.34
C                                                                                SOUND.35
      INTEGER   IYY,JXX,INEST,KTAU                                               SOUND.36
      REAL      DX,DTL                                                           SOUND.37
C                                                                                SOUND.38
      REAL XMUT,XNUT,XMUU,XNUU                                                   SOUND.39
      REAL E,F                                                                   SOUND.40
      COMMON /ASSEL/ XMUT(MIX,MJX,MAXNES),XNUT(MIX,MJX,MAXNES),                  SOUND.41
     1               XMUU(MIX,MJX,MAXNES),XNUU(MIX,MJX,MAXNES)                   SOUND.42
      COMMON /SOUNDL/E(MIX,MJX,KXP1),F(MIX,MJX,KXP1)                             SOUND.43
      REAL      UA(MIX,MJX,MKX), UB(MIX,MJX,MKX),                                SOUND.44
     &          VA(MIX,MJX,MKX), VB(MIX,MJX,MKX),                                SOUND.45
     &          TA(MIX,MJX,MKX), TB(MIX,MJX,MKX),                                SOUND.46
     &          HT(MIX,MJX),    QVB(MIX,MJX,MKX),                                SOUND.47
     &         PSA(MIX,MJX)                                                      SOUND.48
      REAL      MSFD(MIX,MJX),   MSFX(MIX,MJX)                                   SOUND.49
      REAL      PR1(MIX,MJX,MKX),RHO1(MIX,MJX,MKX)                               SOUND.50
      REAL      WTENS(MIXNH,MJXNH,KXP1NH),PPTENS(MIXNH,MJXNH,MKXNH),             SOUND.51
     1     UTENS(MIXNH,MJXNH,MKXNH),VTENS(MIXNH,MJXNH,MKXNH)                     SOUND.52
C                                                                                SOUND.53
      REAL      RHS(MIX,KXP1),WO(MIX,KXP1),PPO(MIX,MKX)                          SOUND.54
      REAL      AA(MIX,KXP1),B(MIX,KXP1),C(MIX,KXP1)                             SOUND.55
      REAL      CA(MIX,MKX),CC(MIX,MKX),CDD(MIX,MKX),CJ(MIX,MKX)                 SOUND.56
#ifndef MPP1                                                                     SOUND.57
      REAL      ESTORE(MIX,MJX),ASTORE(MIX,MJX),WPVAL(MIX)                       SOUND.58
#else                                                                            SOUND.59
      REAL      ESTORE(-3:MIX+4,-3:MJX+4),ASTORE(MIX,MJX),WPVAL(MIX)             SOUND.60
#endif                                                                           SOUND.61
      REAL      PYVP(MIX,MKX),PXUP(MIX,MKX),PTEND(MIX,MKX)                       SOUND.62
      REAL      G1(MIX,MKX),G2(MIX,MKX)                                          SOUND.63
C                                                                                SOUND.64
      REAL      FK(0:6),FL(0:6),FI(-6:6),FJ(-6:6)                                SOUND.65
      REAL      TK(MIX,MKX)                                                      SOUND.66
C                                                                                SOUND.67
      REAL      PI(MIX,MJX,MKX)                                                  SOUND.68
      REAL      SIGDOT(MIX,KXP1), UCRS(MIX,MKX),VCRS(MIX,MKX)                    SOUND.69
C                                                                                SOUND.70
#ifndef MPP1                                                                     SOUND.71
      REAL      U3D(MIX,  MJX,  MKX),    V3D(MIX,  MJX,  MKX),                   SOUND.72
     1          T3D(MIX,  MJX,  MKX),    W3D(MIX,  MJX,  MKX+1),                 SOUND.73
     2         PP3D(MIX,  MJX,  MKX),   QV3D(MIX,  MJX,  MKX)                    SOUND.74
#else                                                                            SOUND.75
#define FULL_DECLARE                                                             SOUND.76
#  include <mpp_dusolve3.incl>                                                   SOUND.77
#endif                                                                           SOUND.78
C                                                                                SOUND.79
      REAL      BET,BP,BM,BPXBP,BPXBM,XKD,GAMMA,DX8,CS,DTSMAX                    SOUND.80
      REAL      DTS                                                              SOUND.81
      REAL      RHO,DPPDP0,CHH,ROFAC,DENOM,ATOT,RHONTOT,XMSFTOT                  SOUND.82
      REAL      ENSQ,ABAR,RHON,XMSFBAR,CPI,DXMSFB                                SOUND.83
      REAL      XKEFF,XLEFF,XKLEFF                                               SOUND.84
      REAL      CFL                                                              SOUND.85
      REAL      RHO0S,CHECK,PPOLD,CDDTMP,CJTMP,CPM,DPTERM                        SOUND.86
C                                                                                SOUND.87
      INTEGER   I,J,K,L,LL,M,N                                                   SOUND.88
      INTEGER   IT,ITL                                                           SOUND.89
      INTEGER   LP1,MP1,NP1,KP1,KM1                                              SOUND.90
      INTEGER   NSJ,JN,NSI,IN                                                    SOUND.91
      INTEGER   ICUT,ISTART,IEND,JSTART,JEND                                     SOUND.92
      INTEGER   ISTEP                                                            SOUND.94
      INTEGER   NPTS,NOCHECK                                                     SOUND.95
C                                                                                SOUND.96
#ifdef MPP1                                                                      SOUND.97
#  include "cflcom.incl"                                                         SOUND.98
#  include <mpp_sound_00.incl>                                                   SOUND.99
#endif                                                                           SOUND.100
C     HT IS G*(TERR. HT.)                                                        SOUND.101
C     UTENS, VTENS, PPTENS AND WTENS ARE SUPPLIED TO THIS ROUTINE                SOUND.102
C     UTENS,VTENS=(ADVECTION+CORIOLIS+DIFFUSION)                                 SOUND.103
C     TTENS=G*W*(TLP/R/T0-1/CP)+DP`DT/RHO/CP+Q/CP (ADIABATIC+MEAN ADVECT         SOUND.104
C        DIABATIC)+ADVECTION+DIFFUSION                                           SOUND.105
C     PPTENS(I,K)=                                                               SOUND.106
C        GAMMA*PR0(I,K)*QQ(I,K)/CP/T0(I,K)(HEATING)                              SOUND.107
C        +ADVECTION+DIFFUSION                                                    SOUND.108
C     WTENS(I,K)=WTL*G*((T(I,K)-T0(I,K))/T0(I,K)-R/CP*PP3D(I,K)/PR0(I,K)         SOUND.109
C        - WTU*G*((T(I,K-1)-T0(I,K-1))/T0(I,K-1)-R/CP*PP3D(I,K-1)/PR0(I,         SOUND.110
C        (BUOYANCY)+ADVECTION+DIFFUSION                                          SOUND.111
C         WHERE WTL=(SIGMA(K)-SIGMA(K-1))/(SIGMA(K+1)-SIGMA(K-1))                SOUND.112
C               WTU=(SIGMA(K+1)-SIGMA(K))/(SIGMA(K+1)-SIGMA(K-1))                SOUND.113
C     TIME-STEPS(ISTEP)                                                          SOUND.114
C                                                                                SOUND.115
C  STATEMENT FUNCTIONS USED FOR P AND RHO                                        SOUND.116
C      Z0(I,J,K)=-(R*TLP/(2.*G)*(ALOG(PR0(I,J,K)/P0))**2                         SOUND.117
C     -     +R*TS0/G*ALOG(PR0(I,J,K)/P0))                                        SOUND.118
C     IF(KTAU.EQ.0)THEN                                                          SOUND.119
C     BET IS IKAWA BETA PARAMETER (0.=CENTERED, 1.=BACKWARD)                     SOUND.120
      BET=0.4                                                                    SOUND.121
      BP=(1.+BET)*0.5                                                            SOUND.123
      BM=(1.-BET)*0.5                                                            SOUND.124
      BPXBP=BP*BP                                                                SOUND.125
      BPXBM=BP*BM                                                                SOUND.126
      XKD=0.1                                                                    SOUND.127
C                                                                                SOUND.128
C     IF(KTAU.LE.1)THEN                                                          SOUND.129
      GAMMA=1./(1.-ROVCP)                                                        SOUND.130
      DX8=DX*8.                                                                  SOUND.131
C     CALCULATE SHORT TIME-STEP                                                  SOUND.132
cto enable MPI_IO_NODE to work correctly                                         19DEC02.1300
C      CS=SQRT(GAMMA*R*T0(1,1,KX))                                               19DEC02.1301
C#ifdef MPP1                                                                     19DEC02.1302
C#  include <mpp_sound_10.incl>                                                  19DEC02.1303
C#endif                                                                          19DEC02.1304
      CS=SQRT(GAMMA*R*TS0)                                                       19DEC02.1305
      DTSMAX=DX/CS/(1.+XKD)                                                      SOUND.137
C     DTL LONG TIME-STEP (XXB-XXC)                                               SOUND.138
      ISTEP=IFIX(DTL/DTSMAX)+1                                                   SOUND.139
      IF(KTAU.GE.1)ISTEP=MAX0(4,ISTEP)                                           SOUND.140
      DTS=DTL/ISTEP                                                              SOUND.141
      L=IYY-1                                                                    SOUND.142
      LP1=IYY                                                                    SOUND.143
      M=JXX-1                                                                    SOUND.144
      MP1=JXX                                                                    SOUND.145
      N=KX                                                                       SOUND.146
      NP1=KXP1                                                                   SOUND.147
C                                                                                SOUND.148
C  CALCULATE THE LOOP BOUNDARIES                                                 SOUND.149
C                                                                                SOUND.150
      ICUT=0                                                                     SOUND.151
      IF(INEST.GT.1)ICUT=1                                                       SOUND.152
      ISTART=2+ICUT                                                              SOUND.153
      IEND=L-1-ICUT                                                              SOUND.154
      JSTART=2+ICUT                                                              SOUND.155
      JEND=M-1-ICUT                                                              SOUND.156
      IF(KTAU.EQ.0)THEN                                                          SOUND.157
      PRINT *,'SHORT TIME STEP ',DTS,ISTEP,' BETA = ',BET,                       SOUND.158
     -     ' XKD = ',XKD                                                         SOUND.159
cmic$ do all autoscope                                                           SOUND.160
c$doacross                                                                       SOUND.161
c$& local(i,j)                                                                   SOUND.162
c$omp parallel do default(shared)                                                SOUND.163
c$omp&private(i,j)                                                               SOUND.164
        DO 3 J=-6,6                                                              SOUND.165
          DO 3 I=-6,6                                                            SOUND.166
            TMASK(I,J)=0.                                                        SOUND.167
    3     CONTINUE                                                               SOUND.168
                                                                                 SOUND.169
        IF(IFUPR.EQ.1)THEN                                                       SOUND.170
C                                                                                SOUND.171
C  DEFINE VALUES OF FK, FL, FI & FJ FOR UPPER RADIATIVE BC                       SOUND.172
C                                                                                SOUND.173
          DO I=1,5                                                               SOUND.174
            FK(I)=2.0                                                            SOUND.175
            FL(I)=2.0                                                            SOUND.176
          ENDDO                                                                  SOUND.177
          FK(0)=1.0                                                              SOUND.178
          FL(0)=1.0                                                              SOUND.179
          FK(6)=1.0                                                              SOUND.180
          FL(6)=1.0                                                              SOUND.181
          DO I=-5,5                                                              SOUND.182
            FI(I)=1.0                                                            SOUND.183
            FJ(I)=1.0                                                            SOUND.184
          ENDDO                                                                  SOUND.185
          FI(-6)=0.5                                                             SOUND.186
          FJ(-6)=0.5                                                             SOUND.187
          FI(6)=0.5                                                              SOUND.188
          FJ(6)=0.5                                                              SOUND.189
        ENDIF                                                                    SOUND.190
      ENDIF                                                                      SOUND.191
C                                                                                SOUND.192
cmic$ do all autoscope                                                           SOUND.193
c$doacross local(i,j,k)                                                          SOUND.194
c$omp parallel do default(shared)                                                SOUND.195
c$omp&private(i,j,k)                                                             SOUND.196
Ccsd$ parallel do private(i,j,k) schedule(static,1)                              23DEC04.1059
      DO J=1,JXX                                                                 SOUND.197
C                                                                                SOUND.198
C  PREMULTIPLY THE TENDENCY ARRAYS BY DTS                                        SOUND.199
C                                                                                SOUND.200
C  CALCULATE INITIAL ARRAYS FOR SHORT TIMESTEP                                   SOUND.201
C  XXB STORES FILTERED OLD XXA WITHOUT XXC TERM                                  SOUND.202
C  NO ASSELIN FILTER ON BOUNDARY                                                 SOUND.203
C                                                                                SOUND.204
        IF(J.LE.MP1)THEN                                                         SOUND.205
          DO K=1,N                                                               SOUND.206
            DO 200 I=1,LP1                                                       SOUND.207
              UTENS(I,J,K)=UTENS(I,J,K)*DTS                                      SOUND.208
              VTENS(I,J,K)=VTENS(I,J,K)*DTS                                      SOUND.209
              U3D(I,J,K)=UB(I,J,K)*RPDOTB(I,J)                                   SOUND.210
              V3D(I,J,K)=VB(I,J,K)*RPDOTB(I,J)                                   SOUND.211
              UB(I,J,K)=XMUU(I,J,INEST)*UA(I,J,K)/MSFD(I,J)+                     SOUND.212
     +                  XNUU(I,J,INEST)*UB(I,J,K)                                SOUND.213
              VB(I,J,K)=XMUU(I,J,INEST)*VA(I,J,K)/MSFD(I,J)+                     SOUND.214
     +                  XNUU(I,J,INEST)*VB(I,J,K)                                SOUND.215
  200       CONTINUE                                                             SOUND.216
          ENDDO                                                                  SOUND.217
        ENDIF                                                                    SOUND.218
        IF(J.LE.M)THEN                                                           SOUND.219
          DO K=1,N                                                               SOUND.220
            DO 202 I=1,L                                                         SOUND.221
              PPTENS(I,J,K)=PPTENS(I,J,K)*DTS                                    SOUND.222
              QV3D(I,J,K)=QVB(I,J,K)*RPSA(I,J)                                   SOUND.223
              PP3D(I,J,K)=PPB(I,J,K)*RPSA(I,J)                                   SOUND.224
              PPB(I,J,K)=XMUT(I,J,INEST)*PPA(I,J,K)+XNUT(I,J,INEST)*             SOUND.225
     +                   PPB(I,J,K)                                              SOUND.226
  202       CONTINUE                                                             SOUND.227
          ENDDO                                                                  SOUND.228
          DO K=1,NP1                                                             SOUND.229
            DO 204 I=1,L                                                         SOUND.230
              WTENS(I,J,K)=WTENS(I,J,K)*DTS                                      SOUND.231
              W3D(I,J,K)=WB(I,J,K)*RPSA(I,J)                                     SOUND.232
              WB(I,J,K)=XMUT(I,J,INEST)*WA(I,J,K)+XNUT(I,J,INEST)*               SOUND.233
     +                  WB(I,J,K)                                                SOUND.234
  204       CONTINUE                                                             SOUND.235
          ENDDO                                                                  SOUND.236
        ENDIF                                                                    SOUND.237
      ENDDO                                                                      SOUND.238
Ccsd$ end parallel do                                                            23DEC04.1060
      DO 700 IT=1,ISTEP                                                          SOUND.239
        ITL=MAX0(KTAU-1,0)                                                       SOUND.240
        IF(IT.EQ.1)GOTO 311                                                      SOUND.242
cmic$ do all autoscope                                                           SOUND.243
c$doacross                                                                       SOUND.244
c$& local(k,j,i)                                                                 SOUND.245
c$omp parallel do default(shared)                                                SOUND.246
c$omp&private(k,j,i)                                                             SOUND.247
Ccsd$ parallel do private(i,j,k) schedule(static,1)                              23DEC04.1061
        DO J=1,JXX                                                               SOUND.248
          IF(J.GE.JSTART.AND.J.LE.JEND)THEN                                      SOUND.249
            DO K=1,N                                                             SOUND.250
              DO 310 I=ISTART,IEND                                               SOUND.251
                PP3D(I,J,K)=PP3D(I,J,K)+XKD*PI(I,J,K)                            SOUND.252
  310         CONTINUE                                                           SOUND.253
            ENDDO                                                                SOUND.254
          ENDIF                                                                  SOUND.255
        ENDDO                                                                    SOUND.256
Ccsd$ end parallel do                                                            23DEC04.1062
  311   CONTINUE                                                                 SOUND.257
cmic$ do all autoscope                                                           SOUND.258
cmic$1 shared  (l,m,n,icut,pp3d,pr0,t3d)                                         SOUND.259
cmic$2 private (i,j,k,kp1,km1)                                                   SOUND.260
c$doacross                                                                       SOUND.261
c$& local(i,j,k,kp1,km1)                                                         SOUND.262
c$omp parallel do default(shared)                                                SOUND.263
c$omp&private(i,j,k,kp1,km1)                                                     SOUND.264
Ccsd$ parallel do private(i,j,k,kp1,km1) schedule(static,1)                      23DEC04.1063
        DO J=1,JXX                                                               SOUND.265
          IF(J.GE.1+ICUT.AND.J.LE.M-ICUT)THEN                                    SOUND.266
            DO K=1,N                                                             SOUND.267
              KP1=MIN0(N,K+1)                                                    SOUND.268
              KM1=MAX0(1,K-1)                                                    SOUND.269
              DO I=1+ICUT,L-ICUT                                                 SOUND.270
                T3D(I,J,K)=(PP3D(I,J,KM1)-PP3D(I,J,KP1))/(PR0(I,J,KM1)-          SOUND.271
     +                     PR0(I,J,KP1))                                         SOUND.272
              ENDDO                                                              SOUND.273
            ENDDO                                                                SOUND.274
          ENDIF                                                                  SOUND.275
        ENDDO                                                                    SOUND.276
Ccsd$ end parallel do                                                            23DEC04.1064
#ifdef MPP1                                                                      SOUND.277
#  include <mpp_sound_20.incl>                                                   SOUND.278
#endif                                                                           SOUND.279
C                                                                                SOUND.280
C  ADVANCE U AND V                                                               SOUND.281
C                                                                                SOUND.282
cmic$ do all autoscope                                                           SOUND.283
cmic$1 shared  (pp3d,pr0,rho1,msfd,u3d,v3d,dx,dts,t3d)                           SOUND.284
cmic$2 private (kp1,km1,rho,chh,dppdp0)                                          SOUND.285
c$doacross                                                                       SOUND.286
c$& local(i,j,k,rho,chh,dppdp0)                                                  SOUND.287
c$omp parallel do default(shared)                                                SOUND.288
c$omp&private(i,j,k,rho,chh,dppdp0)                                              SOUND.289
Ccsd$ parallel do private(i,j,k,rho,chh,dppdp0)                                  23DEC04.1065
Ccsd$& schedule(static,1)                                                        23DEC04.1066
        DO J=1,JXX                                                               SOUND.290
          IF(J.GE.2+ICUT.AND.J.LE.M-ICUT)THEN                                    SOUND.291
            DO K=1,N                                                             SOUND.292
              DO 350 I=2+ICUT,L-ICUT                                             SOUND.293
C     PREDICT U AND V                                                            SOUND.294
                RHO=0.25*(RHO1(I,J,K)+RHO1(I,J-1,K)+RHO1(I-1,J,K)+               SOUND.295
     +              RHO1(I-1,J-1,K))                                             SOUND.296
                DPPDP0=0.25*(T3D(I,J,K)+T3D(I,J-1,K)+T3D(I-1,J,K)+               SOUND.297
     +                 T3D(I-1,J-1,K))                                           SOUND.298
C     DIVIDE BY MAP SCALE FACTOR                                                 SOUND.299
                CHH=0.5*DTS/(RHO*DX)/MSFD(I,J)                                   SOUND.300
                U3D(I,J,K)=U3D(I,J,K)                                            SOUND.301
     +             -CHH*(PP3D(I,J,K)-PP3D(I,J-1,K)                               SOUND.302
     +                  +PP3D(I-1,J,K)-PP3D(I-1,J-1,K)                           SOUND.303
     +                  -(PR0(I,J,K)-PR0(I,J-1,K)                                SOUND.304
     +                   +PR0(I-1,J,K)-PR0(I-1,J-1,K))*DPPDP0)                   SOUND.305
                V3D(I,J,K)=V3D(I,J,K)                                            SOUND.306
     +             -CHH*(PP3D(I,J,K)-PP3D(I-1,J,K)                               SOUND.307
     +                  +PP3D(I,J-1,K)-PP3D(I-1,J-1,K)                           SOUND.308
     +                  -(PR0(I,J,K)-PR0(I-1,J,K)                                SOUND.309
     +                   +PR0(I,J-1,K)-PR0(I-1,J-1,K))*DPPDP0)                   SOUND.310
  350         CONTINUE                                                           SOUND.311
            ENDDO                                                                SOUND.312
          ENDIF                                                                  SOUND.313
        ENDDO                                                                    SOUND.314
Ccsd$ end parallel do                                                            23DEC04.1067
cmic$ do all autoscope                                                           SOUND.315
cmic$1 shared  (lp1,mp1,n,u3d,v3d,utens,vtens)                                   SOUND.316
cmic$2 private (i,j,k)                                                           SOUND.317
c$doacross                                                                       SOUND.318
c$& local(i,j,k)                                                                 SOUND.319
c$omp parallel do default(shared)                                                SOUND.320
c$omp&private(i,j,k)                                                             SOUND.321
Ccsd$ parallel do private(i,j,k) schedule(static,1)                              23DEC04.1068
        DO J=1,JXX                                                               SOUND.322
          IF(J.LE.MP1)THEN                                                       SOUND.323
            DO K=1,N                                                             SOUND.324
              DO 360 I=1,LP1                                                     SOUND.325
                U3D(I,J,K)=U3D(I,J,K)+UTENS(I,J,K)                               SOUND.326
                V3D(I,J,K)=V3D(I,J,K)+VTENS(I,J,K)                               SOUND.327
  360         CONTINUE                                                           SOUND.328
            ENDDO                                                                SOUND.329
          ENDIF                                                                  SOUND.330
        ENDDO                                                                    SOUND.331
Ccsd$ end parallel do                                                            23DEC04.1069
C                                                                                SOUND.332
        IF(IT.EQ.1)GOTO 321                                                      SOUND.333
cmic$ do all autoscope                                                           SOUND.334
c$doacross                                                                       SOUND.335
c$& local(k,j,i)                                                                 SOUND.336
c$omp parallel do default(shared)                                                SOUND.337
c$omp&private(k,j,i)                                                             SOUND.338
Ccsd$ parallel do private(i,j,k) schedule(static,1)                              23DEC04.1070
        DO J=1,JXX                                                               SOUND.339
          IF(J.GE.JSTART.AND.J.LE.JEND)THEN                                      SOUND.340
            DO K=1,N                                                             SOUND.341
              DO 320 I=ISTART,IEND                                               SOUND.342
                PP3D(I,J,K)=PP3D(I,J,K)-XKD*PI(I,J,K)                            SOUND.343
  320         CONTINUE                                                           SOUND.344
            ENDDO                                                                SOUND.345
          ENDIF                                                                  SOUND.346
        ENDDO                                                                    SOUND.347
Ccsd$ end parallel do                                                            23DEC04.1071
  321   CONTINUE                                                                 SOUND.348
#ifdef MPP1                                                                      SOUND.349
#  include <mpp_sound_30.incl>                                                   SOUND.350
#endif                                                                           SOUND.351
C                                                                                SOUND.352
C  SEMI-IMPLICIT SOLUTION FOR W AND P                                            SOUND.353
C                                                                                SOUND.354
cmic$ do all autoscope                                                           SOUND.355
cmic$1 shared  (psa,pp3d,xmut,xnut,tb)                                           SOUND.356
cmic$2 private (kp1,km1,tk)                                                      SOUND.357
cmic$2 private(rhs,wo,aa,b,c,ca,cc,cdd,cj,pyvp,pxup,ptend,g1,g2)                 SOUND.358
c$doacross                                                                       SOUND.359
c$& local(i,j,k,kp1,km1,wo,cc,cdd,cj,pyvp,ptend,rofac,                           SOUND.360
c$& tk,ca,g1,g2,c,b,aa,rhs,denom,pxup)                                           SOUND.361
c$omp parallel do default(shared)                                                SOUND.362
c$omp&private(i,j,k,kp1,km1,wo,cc,cdd,cj,pyvp,ptend,rofac,                       SOUND.363
c$omp&tk,ca,g1,g2,c,b,aa,rhs,denom,pxup)                                         SOUND.364
Ccsd$ parallel do private(tk,ca,g1,g2,c,b,aa,rhs,denom,pxup,                     23DEC04.1072
Ccsd$& i,j,k,kp1,km1,wo,cc,cdd,cj,pyvp,ptend,rofac)                              23DEC04.1073
Ccsd$& schedule(static,1)                                                        23DEC04.1074
        DO J=1,JXX                                                               SOUND.365
          IF(J.GE.JSTART.AND.J.LE.JEND)THEN                                      SOUND.366
            DO K=1,NP1                                                           SOUND.367
              DO I=2,L-1                                                         SOUND.368
                WO(I,K)=W3D(I,J,K)                                               SOUND.369
              ENDDO                                                              SOUND.370
            ENDDO                                                                SOUND.371
C                                                                                SOUND.372
C     VERTICAL BOUNDARY CONDITIONS, W=V.DH/DY AT BOTTOM, LID AT TOP              SOUND.373
C                                                                                SOUND.374
            DO I=ISTART,IEND                                                     SOUND.375
              W3D(I,J,NP1)=0.25*((V3D(I+1,J,N)+V3D(I,J,N)                        SOUND.376
     +                         +V3D(I+1,J+1,N)+V3D(I,J+1,N))                     SOUND.377
     +                         *(HT(I+1,J)-HT(I-1,J))                            SOUND.378
     +                        +(U3D(I+1,J,N)+U3D(I,J,N)                          SOUND.379
     +                         +U3D(I+1,J+1,N)+U3D(I,J+1,N))                     SOUND.380
     +                        *(HT(I,J+1)-HT(I,J-1)))                            SOUND.381
     +                       /(2.*DX*MSFX(I,J)*G)                                SOUND.382
              E(I,J,N)=0.                                                        SOUND.383
              F(I,J,N)=W3D(I,J,NP1)                                              SOUND.384
C                                                                                SOUND.385
              CC(I,1)=GAMMA*PR1(I,J,1)*DTS/(DX*MSFX(I,J))                        SOUND.386
              CDD(I,1)=GAMMA*PR1(I,J,1)*RHO0(I,J,1)*G*DTS/(PS0(I,J)*             SOUND.387
     +                 DSIGMA(1))                                                SOUND.388
              CJ(I,1)=RHO0(I,J,1)*G*DTS/2.                                       SOUND.389
              PXUP(I,1)=0.0625*(PR0(I,J+1,1)-PR0(I,J-1,1))*(U3D(I,J,1)+          SOUND.390
     +                  U3D(I+1,J,1)+U3D(I,J+1,1)+U3D(I+1,J+1,1)-                SOUND.391
     +                  U3D(I,J,2)-U3D(I+1,J,2)-U3D(I,J+1,2)-                    SOUND.392
     +                  U3D(I+1,J+1,2))/(PR0(I,J,1)-PR0(I,J,2))                  SOUND.393
              PYVP(I,1)=0.0625*(PR0(I+1,J,1)-PR0(I-1,J,1))*(V3D(I,J,1)+          SOUND.394
     +                  V3D(I+1,J,1)+V3D(I,J+1,1)+V3D(I+1,J+1,1)-                SOUND.395
     +                  V3D(I,J,2)-V3D(I+1,J,2)-V3D(I,J+1,2)-                    SOUND.396
     +                  V3D(I+1,J+1,2))/(PR0(I,J,1)-PR0(I,J,2))                  SOUND.397
C      ZERO GRADIENT (FREE SLIP) B.C.S ON V AT TOP AND BOTTOM                    SOUND.398
              PTEND(I,1)=PPTENS(I,J,1)-0.5*CC(I,1)                               SOUND.399
     +                 *((V3D(I+1,J,1)*MSFD(I+1,J)                               SOUND.400
     +                   -V3D(I,J,1)*MSFD(I,J)                                   SOUND.401
     +                   +V3D(I+1,J+1,1)*MSFD(I+1,J+1)                           SOUND.402
     +                   -V3D(I,J+1,1)*MSFD(I,J+1)                               SOUND.403
     +                   +U3D(I,J+1,1)*MSFD(I,J+1)                               SOUND.404
     +                   -U3D(I,J,1)*MSFD(I,J)                                   SOUND.405
     +                   +U3D(I+1,J+1,1)*MSFD(I+1,J+1)                           SOUND.406
     +                   -U3D(I+1,J,1)*MSFD(I+1,J))/MSFX(I,J)                    SOUND.407
     +                   -2.*(PYVP(I,1)+PXUP(I,1)))                              SOUND.408
              TK(I,1)=PS0(I,J)*T0(I,J,1)/(2.*GAMMA*PR0(I,J,1)*TB(I,J,1)*         SOUND.409
     +                RPSA(I,J))                                                 SOUND.410
            ENDDO                                                                SOUND.411
C                                                                                SOUND.412
            DO 100 K=2,N                                                         SOUND.413
              KP1=MIN0(N,K+1)                                                    SOUND.414
              KM1=K-1                                                            SOUND.415
              DO 100 I=ISTART,IEND                                               SOUND.416
                TK(I,K)=PS0(I,J)*T0(I,J,K)/(2.*GAMMA*PR0(I,J,K)*                 SOUND.417
     +                  TB(I,J,K)*RPSA(I,J))                                     SOUND.418
                ROFAC=(DSIGMA(K-1)*RHO0(I,J,K)+DSIGMA(K)*RHO0(I,J,K-1))/         SOUND.419
     +                (DSIGMA(K-1)*RHO1(I,J,K)+DSIGMA(K)*RHO1(I,J,K-1))          SOUND.420
C                                                                                SOUND.421
C     SET FACTORS FOR DIFFERENCING                                               SOUND.422
C                                                                                SOUND.423
                CC(I,K)=GAMMA*PR1(I,J,K)*DTS/(DX*MSFX(I,J))                      SOUND.424
                CDD(I,K)=GAMMA*PR1(I,J,K)*RHO0(I,J,K)*G*DTS/(PS0(I,J)*           SOUND.425
     +                   DSIGMA(K))                                              SOUND.426
                CJ(I,K)=RHO0(I,J,K)*G*DTS/2.                                     SOUND.427
                CA(I,K)=G*DTS/(PR0(I,J,K)-PR0(I,J,K-1))*ROFAC                    SOUND.428
                G1(I,K)=1.-DSIGMA(K-1)*TK(I,K)                                   SOUND.429
                G2(I,K)=1.+DSIGMA(K)*TK(I,K-1)                                   SOUND.430
C                                                                                SOUND.431
C      IMPLICIT W EQUATION COEFFICIENT ARRAYS AND RHS (IKAWA METHOD)             SOUND.432
C                                                                                SOUND.433
                C(I,K)=-CA(I,K)*(CDD(I,K-1)-CJ(I,K-1))*G2(I,K)*BPXBP             SOUND.434
                B(I,K)=1.+CA(I,K)*(G1(I,K)*(CDD(I,K)-CJ(I,K))+G2(I,K)*(          SOUND.435
     +                 CDD(I,K-1)+CJ(I,K-1)))*BPXBP                              SOUND.436
                AA(I,K)=-CA(I,K)*(CDD(I,K)+CJ(I,K))*G1(I,K)*BPXBP                SOUND.437
                PYVP(I,K)=0.125*(PR0(I+1,J,K)-PR0(I-1,J,K))                      SOUND.438
     +           *(V3D(I,J,KM1)+V3D(I+1,J,KM1)                                   SOUND.439
     +            +V3D(I,J+1,KM1)+V3D(I+1,J+1,KM1)                               SOUND.440
     +            -V3D(I,J,KP1)-V3D(I+1,J,KP1)                                   SOUND.441
     +            -V3D(I,J+1,KP1)-V3D(I+1,J+1,KP1))                              SOUND.442
     +            /(PR0(I,J,KM1)-PR0(I,J,KP1))                                   SOUND.443
                PXUP(I,K)=0.125*(PR0(I,J+1,K)-PR0(I,J-1,K))                      SOUND.444
     +           *(U3D(I,J,KM1)+U3D(I+1,J,KM1)                                   SOUND.445
     +            +U3D(I,J+1,KM1)+U3D(I+1,J+1,KM1)                               SOUND.446
     +            -U3D(I,J,KP1)-U3D(I+1,J,KP1)                                   SOUND.447
     +            -U3D(I,J+1,KP1)-U3D(I+1,J+1,KP1))                              SOUND.448
     +            /(PR0(I,J,KM1)-PR0(I,J,KP1))                                   SOUND.449
  100         CONTINUE                                                           SOUND.450
                                                                                 SOUND.451
C                                                                                SOUND.452
C      ZERO GRADIENT (FREE SLIP) B.C.S ON V AT TOP AND BOTTOM                    SOUND.453
C                                                                                SOUND.454
            DO I=ISTART,IEND                                                     SOUND.455
              PYVP(I,N)=PYVP(I,N)*0.5                                            SOUND.456
              PXUP(I,N)=PXUP(I,N)*0.5                                            SOUND.457
            ENDDO                                                                SOUND.458
C                                                                                SOUND.459
            DO 101 K=2,N                                                         SOUND.460
              DO 101 I=ISTART,IEND                                               SOUND.461
                PTEND(I,K)=PPTENS(I,J,K)-0.5*CC(I,K)                             SOUND.462
     +                 *((V3D(I+1,J,K)*MSFD(I+1,J)                               SOUND.463
     +                   -V3D(I,J,K)*MSFD(I,J)                                   SOUND.464
     +                   +V3D(I+1,J+1,K)*MSFD(I+1,J+1)                           SOUND.465
     +                   -V3D(I,J+1,K)*MSFD(I,J+1)                               SOUND.466
     +                   +U3D(I,J+1,K)*MSFD(I,J+1)                               SOUND.467
     +                   -U3D(I,J,K)*MSFD(I,J)                                   SOUND.468
     +                   +U3D(I+1,J+1,K)*MSFD(I+1,J+1)                           SOUND.469
     +                   -U3D(I+1,J,K)*MSFD(I+1,J))/MSFX(I,J)                    SOUND.470
     +                   -2.*(PYVP(I,K)+PXUP(I,K)))                              SOUND.471
                RHS(I,K)=W3D(I,J,K)+WTENS(I,J,K)                                 SOUND.472
     +                     +CA(I,K)*(BPXBM                                       SOUND.473
     +                     *((CDD(I,K-1)-CJ(I,K-1))*G2(I,K)*WO(I,K-1)            SOUND.474
     +                     -((CDD(I,K-1)+CJ(I,K-1))*G2(I,K)                      SOUND.475
     +                      +(CDD(I,K)-CJ(I,K))*G1(I,K))*WO(I,K)                 SOUND.476
     +                      +(CDD(I,K)+CJ(I,K))*G1(I,K)*WO(I,K+1))               SOUND.477
     +                   +(PP3D(I,J,K)*G1(I,K)-PP3D(I,J,K-1)*G2(I,K))            SOUND.478
     +                   +(G1(I,K)*PTEND(I,K)-G2(I,K)*PTEND(I,K-1))*BP)          SOUND.479
  101         CONTINUE                                                           SOUND.480
                                                                                 SOUND.481
C                                                                                SOUND.482
            DO 110 K=1,N                                                         SOUND.483
              DO 110 I=ISTART,IEND                                               SOUND.484
                PI(I,J,K)=PP3D(I,J,K)                                            SOUND.485
                PP3D(I,J,K)=PP3D(I,J,K)+PTEND(I,K)+(CJ(I,K)*(WO(I,K+1)+          SOUND.486
     +                      WO(I,K))+CDD(I,K)*(WO(I,K+1)-WO(I,K)))*BM            SOUND.487
  110         CONTINUE                                                           SOUND.488
                                                                                 SOUND.489
C                                                                                SOUND.490
C     UPWARD CALCULATION OF COEFFICIENTS                                         SOUND.491
C                                                                                SOUND.492
C Cdir$ no unroll                                                                23DEC04.1075
Cdir$ interchange(i,K)                                                           23DEC04.1076
            DO 5 K=N,2,-1                                                        SOUND.493
              DO 5 I=ISTART,IEND                                                 SOUND.494
                DENOM=AA(I,K)*E(I,J,K)+B(I,K)                                    SOUND.495
                E(I,J,K-1)=-C(I,K)/DENOM                                         SOUND.496
                F(I,J,K-1)=(RHS(I,K)-F(I,J,K)*AA(I,K))/DENOM                     SOUND.497
    5         CONTINUE                                                           SOUND.498
                                                                                 SOUND.499
C                                                                                SOUND.500
            DO 400 I=ISTART,IEND                                                 SOUND.501
              DENOM=(CDD(I,1)+CJ(I,1))*BP                                        SOUND.502
              ESTORE(I,J)=PP3D(I,J,1)+F(I,J,1)*DENOM                             SOUND.503
              ASTORE(I,J)=DENOM*E(I,J,1)+(CJ(I,1)-CDD(I,1))*BP                   SOUND.504
  400       CONTINUE                                                             SOUND.505
          ENDIF                                                                  SOUND.506
        ENDDO                                                                    SOUND.507
C                                                                                SOUND.508
Ccsd$ end parallel do                                                            23DEC04.1077
C IF FIRST TIME THROUGH AND UPPER RADIATION B.C`S ARE USED                       SOUND.509
C NEED TO CALC SOME COEFFICIENTS                                                 SOUND.510
C                                                                                SOUND.511
        IF(IFUPR.EQ.1.AND.KTAU.EQ.0.AND.IT.EQ.1)THEN                             SOUND.512
C         PRINT *,' CALCULATING MEANS FOR UP. RAD. B.C.'                         07NOV00.1099
          ATOT=0.                                                                SOUND.514
          RHONTOT=0.                                                             SOUND.515
          XMSFTOT=0.                                                             SOUND.516
          NPTS=0                                                                 SOUND.517
#ifndef MPP1                                                                     SOUND.518
          DO 401 J=JSTART,JEND                                                   SOUND.519
            DO 401 I=ISTART,IEND                                                 SOUND.520
              ATOT=ATOT+ASTORE(I,J)                                              SOUND.521
              ENSQ=G*G/CP/(TB(I,J,1)*RPSA(I,J))                                  SOUND.522
              RHONTOT=RHONTOT+RHO1(I,J,1)*SQRT(ENSQ)                             SOUND.523
              XMSFTOT=XMSFTOT+MSFX(I,J)                                          SOUND.524
  401       CONTINUE                                                             SOUND.525
                                                                                 SOUND.526
          NPTS=(IEND-ISTART+1)*(JEND-JSTART+1)                                   SOUND.527
          ABAR=ATOT/NPTS                                                         SOUND.528
          RHON=RHONTOT/NPTS                                                      SOUND.529
          XMSFBAR=XMSFTOT/NPTS                                                   SOUND.530
C         PRINT *, 'INEST ABAR RHON XMSFBAR NPTS ',                              07NOV00.1100
C    +              INEST,ABAR,RHON,XMSFBAR,NPTS                                 07NOV00.1101
#else                                                                            SOUND.533
#  include <mpp_sound_40.incl>                                                   SOUND.534
#endif                                                                           SOUND.535
          CPI=4.*ATAN(1.)                                                        SOUND.536
          DXMSFB=2./DX/XMSFBAR                                                   SOUND.537
C                                                                                SOUND.538
          DO 4 LL=0,6                                                            SOUND.539
            DO 4 K=0,6                                                           SOUND.540
              XKEFF=DXMSFB*SIN(CPI*K/12.)*COS(CPI*LL/12.)                        SOUND.541
              XLEFF=DXMSFB*SIN(CPI*LL/12.)*COS(CPI*K/12.)                        SOUND.542
              XKLEFF=SQRT(XKEFF*XKEFF+XLEFF*XLEFF)                               SOUND.543
cmic$ do all autoscope                                                           SOUND.544
c$doacross                                                                       SOUND.545
c$& local(i,j)                                                                   SOUND.546
c$omp parallel do default(shared)                                                SOUND.547
c$omp&private(i,j)                                                               SOUND.548
              DO 8 J=-6,6                                                        SOUND.549
                DO 8 I=-6,6                                                      SOUND.550
                  TMASK(I,J)=TMASK(I,J)+FI(I)*FJ(J)*FK(K)*FL(LL)/144.*           SOUND.551
     +                       COS(2.*CPI*K*I/12.)*COS(2.*CPI*LL*J/12.)*           SOUND.552
     +                       XKLEFF/(RHON-ABAR*XKLEFF)                           SOUND.553
    8           CONTINUE                                                         SOUND.554
                                                                                 SOUND.555
    4       CONTINUE                                                             SOUND.556
                                                                                 SOUND.557
C         PRINT2002,ABAR,RHON                                                    07NOV00.1102
C         DO 9 J=0,6                                                             07NOV00.1103
C           PRINT2001,(DX*XMSFBAR*TMASK(I,J),I=0,6)                              07NOV00.1104
 2001       FORMAT (1X,7F10.5)                                                   SOUND.561
 2002 FORMAT(1X,'ABAR = ',F10.5,' RHON = ',E12.5,//,1X,' TMASK ')                SOUND.562
C   9     CONTINUE                                                               07NOV00.1105
        ENDIF                                                                    SOUND.564
#ifdef MPP1                                                                      SOUND.565
#  include <mpp_sound_50.incl>                                                   SOUND.566
#endif                                                                           SOUND.567
C                                                                                SOUND.568
C  FINISHED INITIAL COEFFICIENT COMPUTE                                          SOUND.569
C  NOW DO DOWNWARD SWEEP FOR W                                                   SOUND.570
C                                                                                SOUND.571
cmic$ do all autoscope                                                           SOUND.572
cmic$1 shared  (rho1,rho0,insi,jnsj,wtij)                                        SOUND.573
cmic$2 private(wpval)                                                            SOUND.574
c$doacross                                                                       SOUND.575
c$& local(i,j,k,cdd,cj,wpval,cfl,rho0s,ucrs,vcrs,                                SOUND.576
c$&       check,sigdot,nsj,nsi,in,jn)                                            SOUND.577
c$omp parallel do default(shared)                                                SOUND.578
c$omp&private(i,j,k,cdd,cj,wpval,cfl,rho0s,ucrs,vcrs,check,sigdot,               SOUND.579
c$omp&nsj,nsi,in,jn)                                                             SOUND.580
Ccsd$ parallel do private(i,j,k,wpval,cfl,rho0s,ucrs,vcrs,                       23DEC04.1078
Ccsd$& check,sigdot,nsj,nsi,in,jn)                                               23DEC04.1079
Ccsd$& schedule(static,1)                                                        23DEC04.1080
        DO 120 J=1,JXX                                                           SOUND.581
          IF(J.GE.JSTART.AND.J.LE.JEND)THEN                                      SOUND.582
C                                                                                SOUND.583
C  FIRST, SET UPPER BOUNDARY CONDITION, EITHER W=0 OR RADIATION                  SOUND.584
C                                                                                SOUND.585
            DO I=ISTART,IEND                                                     SOUND.586
              WPVAL(I)=0.                                                        SOUND.587
            ENDDO                                                                SOUND.588
C                                                                                SOUND.589
            IF(IFUPR.NE.1)GOTO 2125                                              SOUND.590
C                                                                                SOUND.591
C  APPLY UPPER RAD COND. NO W3D(TOP) IN LATERAL SPONGE                           SOUND.592
C                                                                                SOUND.593
            IF(J.LT.5.OR.J.GT.M-4)GOTO 2125                                      SOUND.594
            DO 2150 NSJ=-6,6                                                     SOUND.595
              JN=JNSJ(J,NSJ,INEST)                                               SOUND.596
              DO 2150 NSI=-6,6                                                   SOUND.597
                DO 2150 I=5,L-4                                                  SOUND.598
                  IN=INSI(I,NSI,INEST)                                           SOUND.599
#ifndef MPP1                                                                     SOUND.600
                  WPVAL(I)=WPVAL(I)+ESTORE(IN,JN)*TMASK(NSI,NSJ)*                SOUND.601
     +                     WTIJ(I,J,INEST)                                       SOUND.602
#else                                                                            SOUND.603
C  SEE IMPORTANT COMMENT IN THIS INCLUDED FILE RELATING TO MEMORY                SOUND.604
C  STORAGE AND INDEXING OF THE ARRAY ESTORE IN THE MPP CODE IN                   SOUND.605
C  THIS INCLUDED FILE                                                            SOUND.606
#  include <mpp_sound_55.incl>                                                   SOUND.607
#endif                                                                           SOUND.608
 2150           CONTINUE                                                         SOUND.609
                                                                                 SOUND.610
                                                                                 SOUND.611
 2125       CONTINUE                                                             SOUND.612
C                                                                                SOUND.613
C  FINISHED CALC OF RADIATION W                                                  SOUND.614
C                                                                                SOUND.615
            DO I=ISTART,IEND                                                     SOUND.616
              W3D(I,J,1)=WPVAL(I)                                                SOUND.617
            ENDDO                                                                SOUND.618
C                                                                                SOUND.619
C     DOWNWARD CALCULATION OF W                                                  SOUND.620
C                                                                                SOUND.621
            DO 10 K=1,N                                                          SOUND.622
              DO 10 I=ISTART,IEND                                                SOUND.623
                W3D(I,J,K+1)=E(I,J,K)*W3D(I,J,K)+F(I,J,K)                        SOUND.624
C                                                                                SOUND.625
C --- CALCULATE WIND AT CROSS POINT                                              SOUND.626
C                                                                                SOUND.627
                UCRS(I,K)=U3D(I,J,K)+U3D(I+1,J,K)+U3D(I,J+1,K)+                  SOUND.628
     +                    U3D(I+1,J+1,K)                                         SOUND.629
                VCRS(I,K)=V3D(I,J,K)+V3D(I+1,J,K)+V3D(I,J+1,K)+                  SOUND.630
     +                    V3D(I+1,J+1,K)                                         SOUND.631
   10         CONTINUE                                                           SOUND.632
                                                                                 SOUND.633
#ifdef MPP1                                                                      SOUND.634
#  include <mpp_sound_60.incl>                                                   SOUND.635
#endif                                                                           SOUND.636
C                                                                                SOUND.637
            CFL=0.0                                                              SOUND.638
            DO K=N,2,-1                                                          SOUND.639
              DO I=ISTART,IEND                                                   SOUND.640
                RHO0S=TWT(K,1)*RHO0(I,J,K)+TWT(K,2)*RHO0(I,J,K-1)                SOUND.641
                SIGDOT(I,K)=-RHO0S*G*W3D(I,J,K)*RPSA(I,J)*0.001                  SOUND.642
     &                   -SIGMA(K)*(DPSDXM(I,J)*(TWT(K,1)*UCRS(I,K)              SOUND.643
     &                                          +TWT(K,2)*UCRS(I,K-1))           SOUND.644
     &                             +DPSDYM(I,J)*(TWT(K,1)*VCRS(I,K)              SOUND.645
     &                                          +TWT(K,2)*VCRS(I,K-1)))          SOUND.646
                CHECK=ABS(SIGDOT(I,K))*DTL/(DSIGMA(K)+DSIGMA(K-1))               SOUND.647
                CFL=AMAX1(CHECK,CFL)                                             SOUND.648
              ENDDO                                                              SOUND.649
            ENDDO                                                                SOUND.650
#ifdef MPP1                                                                      SOUND.651
C CFLJ IS DEFINED IN CFLCOM.INCL AND USED IN mpp_sound_70.incl                   SOUND.652
            CFLJ(J)=CFL                                                          SOUND.653
#endif                                                                           SOUND.654
C                                                                                SOUND.655
            IF(CFL.GT.1.0)THEN                                                   SOUND.656
              DO K=N,2,-1                                                        SOUND.657
                DO I=ISTART,IEND                                                 SOUND.658
                  CFL=ABS(SIGDOT(I,K))*DTL/(DSIGMA(K)+DSIGMA(K-1))               SOUND.659
                  IF(CFL.GT.1.)THEN                                              SOUND.660
#ifndef MPP1                                                                     SOUND.661
                    PRINT650,CFL,W3D(I,J,K),I,J,K,INEST                          SOUND.662
#else                                                                            SOUND.663
                    PRINT650,CFL,W3D(I,J,K),FLIC_L2G_M(I),FLIC_L2G_N(J),         SOUND.664
     +                       K,INEST                                             SOUND.665
#endif                                                                           SOUND.666
                  ENDIF                                                          SOUND.667
                ENDDO                                                            SOUND.668
              ENDDO                                                              SOUND.669
            ENDIF                                                                SOUND.670
          ENDIF                                                                  SOUND.671
C                                                                                SOUND.672
  650     FORMAT('CFL>1: CFL = ',F7.4,' W = ',F8.4,'  I = ',I5,                  SOUND.673
     +    '  J = ',I5,'  K = ',I5,'  INEST = ',I3)                               SOUND.674
  120   CONTINUE                                                                 SOUND.675
Ccsd$ end parallel do                                                            23DEC04.1081
#ifdef MPP1                                                                      SOUND.676
#  include <mpp_sound_70.incl>                                                   SOUND.677
#endif                                                                           SOUND.678
C                                                                                SOUND.679
C SET LATERAL W IF THIS IS A NESTED GRID                                         SOUND.680
C                                                                                SOUND.681
        IF(INEST.NE.1)THEN                                                       SOUND.682
cmic$ do all autoscope                                                           SOUND.683
c$doacross                                                                       SOUND.684
c$& local(k,i,j)                                                                 SOUND.685
c$omp parallel do default(shared)                                                SOUND.686
c$omp&private(k,i,j)                                                             SOUND.687
Ccsd$ parallel do private(k,i,j)                                                 23DEC04.1082
Ccsd$& schedule(static,1)                                                        23DEC04.1083
          DO J=1,JXX                                                             SOUND.688
            IF(J.EQ.2)THEN                                                       SOUND.689
              DO K=1,NP1                                                         SOUND.690
                DO I=2,L-1                                                       SOUND.691
                  W3D(I,2,K)=W3D(I,2,K)+WTENS(I,2,K)                             SOUND.692
                ENDDO                                                            SOUND.693
              ENDDO                                                              SOUND.694
              DO K=1,N                                                           SOUND.695
                DO I=2,L-1                                                       SOUND.696
                  PP3D(I,2,K)=PP3D(I,2,K)+PPTENS(I,2,K)                          SOUND.697
                ENDDO                                                            SOUND.698
              ENDDO                                                              SOUND.699
            ELSEIF(J.EQ.M-1)THEN                                                 SOUND.700
              DO K=1,NP1                                                         SOUND.701
                DO I=2,L-1                                                       SOUND.702
                  W3D(I,M-1,K)=W3D(I,M-1,K)+WTENS(I,M-1,K)                       SOUND.703
                ENDDO                                                            SOUND.704
              ENDDO                                                              SOUND.705
              DO K=1,N                                                           SOUND.706
                DO I=2,L-1                                                       SOUND.707
                  PP3D(I,M-1,K)=PP3D(I,M-1,K)+PPTENS(I,M-1,K)                    SOUND.708
                ENDDO                                                            SOUND.709
              ENDDO                                                              SOUND.710
            ELSEIF(J.GE.3.AND.J.LE.M-2)THEN                                      SOUND.711
              DO K=1,NP1                                                         SOUND.712
                W3D(2,J,K)=W3D(2,J,K)+WTENS(2,J,K)                               SOUND.713
                W3D(L-1,J,K)=W3D(L-1,J,K)+WTENS(L-1,J,K)                         SOUND.714
              ENDDO                                                              SOUND.715
              DO K=1,N                                                           SOUND.716
                PP3D(2,J,K)=PP3D(2,J,K)+PPTENS(2,J,K)                            SOUND.717
                PP3D(L-1,J,K)=PP3D(L-1,J,K)+PPTENS(L-1,J,K)                      SOUND.718
              ENDDO                                                              SOUND.719
            ENDIF                                                                SOUND.720
          ENDDO                                                                  SOUND.721
Ccsd$ end parallel do                                                            23DEC04.1084
        ENDIF                                                                    SOUND.722
C                                                                                SOUND.723
C  NOW COMPUTE THE NEW PRESSURE                                                  SOUND.724
C                                                                                SOUND.725
cmic$ do all autoscope                                                           SOUND.726
cmic$2 private(cdd,cj)                                                           SOUND.727
c$doacross                                                                       SOUND.728
c$& local(j,k,i,ppold,cddtmp,cjtmp,cpm,dpterm)                                   SOUND.729
c$omp parallel do default(shared)                                                SOUND.730
c$omp&private(j,k,i,ppold,cddtmp,cjtmp,cpm,dpterm)                               SOUND.731
Ccsd$ parallel do private(j,k,i,ppold,cddtmp,cjtmp,cpm,dpterm)                   23DEC04.1085
Ccsd$& schedule(static,1)                                                        23DEC04.1086
        DO J=1,JXX                                                               SOUND.732
          IF(J.GE.JSTART.AND.J.LE.JEND)THEN                                      SOUND.733
            DO K=1,N                                                             SOUND.734
              DO 15 I=ISTART,IEND                                                SOUND.735
                PPOLD=PI(I,J,K)                                                  SOUND.736
                CDDTMP=GAMMA*PR1(I,J,K)*RHO0(I,J,K)*G*DTS/(PS0(I,J)*             SOUND.737
     +                 DSIGMA(K))                                                SOUND.738
                CJTMP=RHO0(I,J,K)*G*DTS/2.                                       SOUND.739
                PP3D(I,J,K)=PP3D(I,J,K)+(CJTMP*(W3D(I,J,K+1)+W3D(I,J,K))         SOUND.740
     +                      +CDDTMP*(W3D(I,J,K+1)-W3D(I,J,K)))*BP                SOUND.741
                PI(I,J,K)=PP3D(I,J,K)-PPOLD-PPTENS(I,J,K)                        SOUND.742
C                                                                                SOUND.743
C COMPUTE PRESSURE DP`/DT CORRECTION TO THE TEMPERATURE                          SOUND.744
C                                                                                SOUND.745
                CPM=CP*(1.+0.8*QV3D(I,J,K)*XMOIST(INEST))                        SOUND.746
                DPTERM=PSA(I,J)*(PP3D(I,J,K)-PPOLD)/(CPM*RHO1(I,J,K))            SOUND.747
                TB(I,J,K)=TB(I,J,K)+XNUT(I,J,INEST)*DPTERM                       SOUND.748
                TA(I,J,K)=TA(I,J,K)+DPTERM                                       SOUND.749
   15         CONTINUE                                                           SOUND.750
            ENDDO                                                                SOUND.751
          ENDIF                                                                  SOUND.752
        ENDDO                                                                    SOUND.753
Ccsd$ end parallel do                                                            23DEC04.1087
C                                                                                SOUND.754
        NOCHECK=1                                                                SOUND.755
C      IF(NOCHECK.EQ.1)GO TO 71                                                  SOUND.756
C      CHECK IMPLICIT W PREDICTION                                               SOUND.757
C      I=L-1-ICUT                                                                SOUND.758
C      J=M-1-ICUT                                                                SOUND.759
C      DO 70 K=2,N                                                               SOUND.760
C      CHECK=(W3D(I,J,K)-WO(K)-WTENS(I,J,K)-                                     SOUND.761
C     -     CA(K)*(PP3D(I,J,K)*G1(K)-PP3D(I,J,K-1)*G2(K))*BP-                    SOUND.762
C     -     CA(K)*(PPO(K)*G1(K)-PPO(K-1)*G2(K))*BM)/                             SOUND.763
C     -     AMAX1(1.,ABS(W3D(I,J,K)))                                            SOUND.764
C      IF(ABS(CHECK).GT.1.E-9)THEN                                               SOUND.765
C         PRINT 600,I,J,K,CHECK,W3D(I,J,K),IT                                    SOUND.766
C  600 FORMAT(1X,' ERROR AT ',3I5,' = ',E12.5,' W = ',E12.5,                     SOUND.767
C     -     ' TIMESTEP = ',I5)                                                   SOUND.768
C        STOP 'CHECK'                                                            SOUND.769
C      ENDIF                                                                     SOUND.770
C   70 CONTINUE                                                                  SOUND.771
C   71 CONTINUE                                                                  SOUND.772
        IF(INEST.EQ.1)THEN                                                       SOUND.773
cmic$ do all autoscope                                                           SOUND.774
c$doacross                                                                       SOUND.775
c$& local(k,j,i)                                                                 SOUND.776
c$omp parallel do default(shared)                                                SOUND.777
c$omp&private(k,j,i)                                                             SOUND.778
Ccsd$ parallel do private(i,j,k) schedule(static,1)                              23DEC04.1088
          DO 500 K=1,NP1                                                         SOUND.779
C     ZERO GRADIENT CONDITIONS ON W,  SPECIFIED ON PP                            SOUND.780
            DO 510 J=2,M-1                                                       SOUND.781
              W3D(1,J,K)=W3D(2,J,K)                                              SOUND.782
              W3D(L,J,K)=W3D(L-1,J,K)                                            SOUND.783
  510       CONTINUE                                                             SOUND.784
            DO 520 I=1,L                                                         SOUND.785
              W3D(I,1,K)=W3D(I,2,K)                                              SOUND.786
              W3D(I,M,K)=W3D(I,M-1,K)                                            SOUND.787
  520       CONTINUE                                                             SOUND.788
            IF(K.NE.NP1)THEN                                                     SOUND.789
              DO 517 J=2,M-1                                                     SOUND.790
                PP3D(1,J,K)=PP3D(1,J,K)+PPTENS(1,J,K)                            SOUND.791
                PP3D(L,J,K)=PP3D(L,J,K)+PPTENS(L,J,K)                            SOUND.792
  517         CONTINUE                                                           SOUND.793
              DO 518 I=1,L                                                       SOUND.794
                PP3D(I,1,K)=PP3D(I,1,K)+PPTENS(I,1,K)                            SOUND.795
                PP3D(I,M,K)=PP3D(I,M,K)+PPTENS(I,M,K)                            SOUND.796
  518         CONTINUE                                                           SOUND.797
            ENDIF                                                                SOUND.798
  500     CONTINUE                                                               SOUND.799
Ccsd$ end parallel do                                                            23DEC04.1089
        ELSEIF(INEST.GT.1)THEN                                                   SOUND.800
cmic$ do all autoscope                                                           SOUND.801
c$doacross                                                                       SOUND.802
c$& local(k,j,i)                                                                 SOUND.803
c$omp parallel do default(shared)                                                SOUND.804
c$omp&private(k,j,i)                                                             SOUND.805
Ccsd$ parallel do private(i,j,k) schedule(static,1)                              23DEC04.1090
          DO 540 K=1,NP1                                                         SOUND.806
C     SPECIFIED ON W AND PP                                                      SOUND.807
            DO 530 J=2,M-1                                                       SOUND.808
              W3D(1,J,K)=W3D(1,J,K)+WTENS(1,J,K)                                 SOUND.809
              W3D(L,J,K)=W3D(L,J,K)+WTENS(L,J,K)                                 SOUND.810
  530       CONTINUE                                                             SOUND.811
            DO 535 I=1,L                                                         SOUND.812
              W3D(I,M,K)=W3D(I,M,K)+WTENS(I,M,K)                                 SOUND.813
              W3D(I,1,K)=W3D(I,1,K)+WTENS(I,1,K)                                 SOUND.814
  535       CONTINUE                                                             SOUND.815
            IF(K.NE.NP1)THEN                                                     SOUND.816
              DO 536 J=2,M-1                                                     SOUND.817
                PP3D(1,J,K)=PP3D(1,J,K)+PPTENS(1,J,K)                            SOUND.818
                PP3D(L,J,K)=PP3D(L,J,K)+PPTENS(L,J,K)                            SOUND.819
  536         CONTINUE                                                           SOUND.820
              DO 537 I=1,L                                                       SOUND.821
                PP3D(I,1,K)=PP3D(I,1,K)+PPTENS(I,1,K)                            SOUND.822
                PP3D(I,M,K)=PP3D(I,M,K)+PPTENS(I,M,K)                            SOUND.823
  537         CONTINUE                                                           SOUND.824
            ENDIF                                                                SOUND.825
  540     CONTINUE                                                               SOUND.826
Ccsd$ end parallel do                                                            23DEC04.1091
        ENDIF                                                                    SOUND.827
C      END OF TIME LOOP 700                                                      SOUND.828
#ifdef MPP1                                                                      07NOV00.1106
#  include <mpp_sound_80.incl>                                                   07NOV00.1107
#endif                                                                           07NOV00.1108
  700 CONTINUE                                                                   SOUND.836
C     TRANSFER XXA TO XXB, NEW VALUES TO XXA AND APPLY TIME FILTER               SOUND.837
cmic$ do all autoscope                                                           SOUND.838
c$doacross                                                                       SOUND.839
c$& local(i,j,k)                                                                 SOUND.840
c$omp parallel do default(shared)                                                SOUND.841
c$omp&private(i,j,k)                                                             SOUND.842
Ccsd$ parallel do private(i,j,k)                                                 23DEC04.1092
Ccsd$& schedule(static,1)                                                        23DEC04.1093
      DO J=1,JXX                                                                 SOUND.843
        IF(J.LE.MP1)THEN                                                         SOUND.844
          DO K=1,N                                                               SOUND.845
            DO I=1,LP1                                                           SOUND.846
              UA(I,J,K)=PDOTB(I,J)*U3D(I,J,K)                                    SOUND.847
              VA(I,J,K)=PDOTB(I,J)*V3D(I,J,K)                                    SOUND.848
              UB(I,J,K)=UB(I,J,K)+XNUU(I,J,INEST)*UA(I,J,K)                      SOUND.849
              VB(I,J,K)=VB(I,J,K)+XNUU(I,J,INEST)*VA(I,J,K)                      SOUND.850
            ENDDO                                                                SOUND.851
          ENDDO                                                                  SOUND.852
        ENDIF                                                                    SOUND.853
        IF(J.LE.M)THEN                                                           SOUND.854
          DO K=1,N                                                               SOUND.855
            DO I=1,L                                                             SOUND.856
              PPA(I,J,K)=PSA(I,J)*PP3D(I,J,K)                                    SOUND.857
              PPB(I,J,K)=PPB(I,J,K)+XNUT(I,J,INEST)*PPA(I,J,K)                   SOUND.858
            ENDDO                                                                SOUND.859
          ENDDO                                                                  SOUND.860
          DO K=1,NP1                                                             SOUND.861
            DO I=1,L                                                             SOUND.862
              WA(I,J,K)=PSA(I,J)*W3D(I,J,K)                                      SOUND.863
              WB(I,J,K)=WB(I,J,K)+XNUT(I,J,INEST)*WA(I,J,K)                      SOUND.864
            ENDDO                                                                SOUND.865
          ENDDO                                                                  SOUND.866
        ENDIF                                                                    SOUND.867
      ENDDO                                                                      SOUND.868
Ccsd$ end parallel do                                                            23DEC04.1094
      RETURN                                                                     SOUND.869
      END                                                                        SOUND.870
                                                                                 SOUND.871
                                                                                 23DEC04.1095
                                                                                 23DEC04.1096
#else                                                                            23DEC04.1097
C IBM optimized SOUND routine                                                    23DEC04.1098
                                                                                 23DEC04.1099
      SUBROUTINE SOUND(IYY,JXX,UB,VB,TB,PR1,RHO1,                                23DEC04.1100
     &                 UA,VA,TA,QVB,PSA,HT,MSFD,MSFX,                            23DEC04.1101
     &                 UTENS,VTENS,PPTENS,WTENS,                                 23DEC04.1102
     &                 DX,DTL,INEST,KTAU)                                        23DEC04.1103
      IMPLICIT NONE                                                              23DEC04.1104
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         23DEC04.1105
C                                                                      C         23DEC04.1106
C     CHANGES:                                                         C         23DEC04.1107
C          REORDERING LOOPS FOR PARALLEL EFFICIENCY                    C         23DEC04.1108
C          ADDITION OF HP PARALLEL DIRECTIVES                          C         23DEC04.1109
C          REMOVAL OF SGI SHARE PARALLEL DIRECTIVES (UNNECESSARY)      C         23DEC04.1110
C          REPLACE UA -> U3D ETC                                       C         23DEC04.1111
C          IMPROVE UPPER RAD CONDITION EFFICIENCY                      C         23DEC04.1112
C     NOTE:                                                            C         23DEC04.1113
C          THE LINE NUMBERS IN SOUND.F HAVE BEEN REDONE FOR            C         23DEC04.1114
C          RELEASE-2-7 (DECEMBER 21, 1997)                             C         23DEC04.1115
C     CHANGES:                                                         C         23DEC04.1116
C          USE COMMON BLOCKS TO STORE PSTAR-DERIVED VARIABLES (2.9)    C         23DEC04.1117
C          USE COMMON BLOCKS TO STORE REF STATE AND UP. RAD. VARIABLES C         23DEC04.1118
C           (2.9)                                                      C         23DEC04.1119
C          CHANGE NEST B.C. ON P` TO JUST INTERPOLATE 2ND PT FROM      C         23DEC04.1120
C           COARSE MESH LIKE ALL OTHER VARIABLES                       C         23DEC04.1121
C                                                                      C         23DEC04.1122
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         23DEC04.1123
#     include <parame.incl>                                                      23DEC04.1124
#     include <param2.incl>                                                      23DEC04.1125
#     include <param3.incl>                                                      23DEC04.1126
#     include <pmoist.incl>                                                      23DEC04.1127
#     include <nonhyd.incl>                                                      23DEC04.1128
#     include <nonhydb.incl>                                                     23DEC04.1129
#     include <nhcnst.incl>                                                      23DEC04.1130
C                                                                                23DEC04.1131
#     include <uprad.incl>                                                       23DEC04.1132
#     include <rpstar.incl>                                                      23DEC04.1133
C                                                                                23DEC04.1134
      INTEGER   IYY,JXX,INEST,KTAU                                               23DEC04.1135
      REAL      DX,DTL                                                           23DEC04.1136
C                                                                                23DEC04.1137
      REAL XMUT,XNUT,XMUU,XNUU                                                   23DEC04.1138
      REAL E,F                                                                   23DEC04.1139
      COMMON /ASSEL/ XMUT(MIX,MJX,MAXNES),XNUT(MIX,MJX,MAXNES),                  23DEC04.1140
     1               XMUU(MIX,MJX,MAXNES),XNUU(MIX,MJX,MAXNES)                   23DEC04.1141
      COMMON /SOUNDL/E(MIX,MJX,KXP1),F(MIX,MJX,KXP1)                             23DEC04.1142
      REAL      UA(MIX,MJX,MKX), UB(MIX,MJX,MKX),                                23DEC04.1143
     &          VA(MIX,MJX,MKX), VB(MIX,MJX,MKX),                                23DEC04.1144
     &          TA(MIX,MJX,MKX), TB(MIX,MJX,MKX),                                23DEC04.1145
     &          HT(MIX,MJX),    QVB(MIX,MJX,MKX),                                23DEC04.1146
     &         PSA(MIX,MJX)                                                      23DEC04.1147
      REAL      MSFD(MIX,MJX),   MSFX(MIX,MJX)                                   23DEC04.1148
      REAL      PR1(MIX,MJX,MKX),RHO1(MIX,MJX,MKX)                               23DEC04.1149
      REAL      WTENS(MIXNH,MJXNH,KXP1NH),PPTENS(MIXNH,MJXNH,MKXNH),             23DEC04.1150
     1     UTENS(MIXNH,MJXNH,MKXNH),VTENS(MIXNH,MJXNH,MKXNH)                     23DEC04.1151
C                                                                                23DEC04.1152
      REAL      RHS(MIX,KXP1),WO(MIX,KXP1),PPO(MIX,MKX)                          23DEC04.1153
      REAL      AA(MIX,KXP1),B(MIX,KXP1),C(MIX,KXP1)                             23DEC04.1154
      REAL      CA(MIX,MKX),CC(MIX,MKX),CDD(MIX,MKX),CJ(MIX,MKX)                 23DEC04.1155
#ifndef MPP1                                                                     23DEC04.1156
      REAL      ESTORE(MIX,MJX),ASTORE(MIX,MJX),WPVAL(MIX)                       23DEC04.1157
#else                                                                            23DEC04.1158
      REAL      ESTORE(-3:MIX+4,-3:MJX+4),ASTORE(MIX,MJX),WPVAL(MIX)             23DEC04.1159
#endif                                                                           23DEC04.1160
      REAL      PYVP(MIX,MKX),PXUP(MIX,MKX),PTEND(MIX,MKX)                       23DEC04.1161
      REAL      G1(MIX,MKX),G2(MIX,MKX)                                          23DEC04.1162
C                                                                                23DEC04.1163
      REAL      FK(0:6),FL(0:6),FI(-6:6),FJ(-6:6)                                23DEC04.1164
      REAL      TK(MIX,MKX)                                                      23DEC04.1165
C                                                                                23DEC04.1166
      REAL      PI(MIX,MJX,MKX)                                                  23DEC04.1167
      REAL      SIGDOT(MIX,KXP1), UCRS(MIX,MKX),VCRS(MIX,MKX)                    23DEC04.1168
C                                                                                23DEC04.1169
#ifndef MPP1                                                                     23DEC04.1170
      REAL      U3D(MIX,  MJX,  MKX),    V3D(MIX,  MJX,  MKX),                   23DEC04.1171
     1          T3D(MIX,  MJX,  MKX),    W3D(MIX,  MJX,  MKX+1),                 23DEC04.1172
     2         PP3D(MIX,  MJX,  MKX),   QV3D(MIX,  MJX,  MKX)                    23DEC04.1173
#else                                                                            23DEC04.1174
#define FULL_DECLARE                                                             23DEC04.1175
#  include <mpp_dusolve3.incl>                                                   23DEC04.1176
#endif                                                                           23DEC04.1177
C                                                                                23DEC04.1178
      REAL      BET,BP,BM,BPXBP,BPXBM,XKD,GAMMA,DX8,CS,DTSMAX                    23DEC04.1179
      REAL      DTS                                                              23DEC04.1180
      REAL      RHO,DPPDP0,CHH,ROFAC,DENOM,ATOT,RHONTOT,XMSFTOT                  23DEC04.1181
      REAL      ENSQ,ABAR,RHON,XMSFBAR,CPI,cpi12,DXMSFB                          23DEC04.1182
      REAL      XKEFF,XLEFF,XKLEFF                                               23DEC04.1183
      REAL      CFL                                                              23DEC04.1184
      REAL      RHO0S,CHECK,PPOLD,CDDTMP,CJTMP,CPM,DPTERM                        23DEC04.1185
C                                                                                23DEC04.1186
      INTEGER   I,J,K,L,LL,M,N                                                   23DEC04.1187
      INTEGER   IT,ITL                                                           23DEC04.1188
      INTEGER   LP1,MP1,NP1,KP1,KM1                                              23DEC04.1189
      INTEGER   NSJ,JN,NSI,IN                                                    23DEC04.1190
      INTEGER   ICUT,ISTART,IEND,JSTART,JEND                                     23DEC04.1191
      INTEGER   ISTEP                                                            23DEC04.1192
      INTEGER   NPTS,NOCHECK                                                     23DEC04.1193
                                                                                 23DEC04.1194
      real rmsfd(MIX,MJX),rmsfx(MIX,MJX),rps0(MIXNH,MJXNH)                       23DEC04.1195
      real dsigmai(MKX),dsigmas(MKX),dxi,gi,rnpts,rrhon,ttime                    23DEC04.1196
      real rp1(MIXNH,MJXNH,MKXNH),rp2(MIXNH,MJXNH,MKXNH),rrr,rr1                 23DEC04.1197
      data ttime/1.0/,kkk/1/,iinest/0/                                           23DEC04.1198
      save ttime,dsigmai,dsigmas,rmsfd,rmsfx,rps0,rp1,rp2,kkk,iinest             23DEC04.1199
      integer is,is1,ie,ie1,is2,ie2,kkk,nn,nn3,is3,iinest                        23DEC04.1200
      integer js,je,is4,nn4,is5,nn5,is6,nn6                                      23DEC04.1201
      real dtsx1,dtsx2,dtsx3,dtsx4,dtsx5,dtsx6,dtsx7,dtsx8                       23DEC04.1202
      real tmp(mix,6)                                                            23DEC04.1203
      real aux0,aux2,aux4,aux6,aux8,ua1,va1,ub1,vb1,xnu,wa1,wb1                  23DEC04.1204
                                                                                 23DEC04.1205
C                                                                                23DEC04.1206
#ifdef MPP1                                                                      23DEC04.1207
#  include "cflcom.incl"                                                         23DEC04.1208
#  include <mpp_sound_00.incl>                                                   23DEC04.1209
#endif                                                                           23DEC04.1210
                                                                                 23DEC04.1211
#ifdef MPP1                                                                      23DEC04.1212
#define nf(A) noflic(A) 
#else                                                                            23DEC04.1214
#define nf(A) A        
#endif                                                                           23DEC04.1216
C     BET IS IKAWA BETA PARAMETER (0.=CENTERED, 1.=BACKWARD)                     23DEC04.1217
      BET=0.4                                                                    23DEC04.1218
      BP=(1.+BET)*0.5                                                            23DEC04.1219
      BM=(1.-BET)*0.5                                                            23DEC04.1220
      BPXBP=BP*BP                                                                23DEC04.1221
      BPXBM=BP*BM                                                                23DEC04.1222
      XKD=0.1                                                                    23DEC04.1223
C                                                                                23DEC04.1224
C     IF(KTAU.LE.1)THEN                                                          23DEC04.1225
      GAMMA=1./(1.-ROVCP)                                                        23DEC04.1226
      DX8=DX*8.                                                                  23DEC04.1227
      dxi=1./dx                                                                  23DEC04.1228
      gi=1./g                                                                    23DEC04.1229
C     CALCULATE SHORT TIME-STEP                                                  23DEC04.1230
cto enable MPI_IO_NODE to work correctly                                         23DEC04.1231
C      CS=SQRT(GAMMA*R*T0(1,1,KX))                                               23DEC04.1232
C#ifdef MPP1                                                                     23DEC04.1233
C#  include <mpp_sound_10.incl>                                                  23DEC04.1234
C#endif                                                                          23DEC04.1235
      CS=SQRT(GAMMA*R*TS0)                                                       23DEC04.1236
      DTSMAX=DX/CS/(1.+XKD)                                                      23DEC04.1237
C     DTL LONG TIME-STEP (XXB-XXC)                                               23DEC04.1238
      ISTEP=IFIX(DTL/DTSMAX)+1                                                   23DEC04.1239
      IF(KTAU.GE.1)ISTEP=MAX0(4,ISTEP)                                           23DEC04.1240
      DTS=DTL/ISTEP                                                              23DEC04.1241
      dtsx1 = 0.5*dts*dxi                                                        23DEC04.1242
      dtsx2 = 0.125 * dxi * gi                                                   23DEC04.1243
      dtsx3 = GAMMA * DTS * DXi                                                  23DEC04.1244
      dtsx4 = GAMMA * DTS * g                                                    23DEC04.1245
      dtsx5 = 0.5 * DTS * g                                                      23DEC04.1246
                                                                                 23DEC04.1247
      L=IYY-1                                                                    23DEC04.1248
      LP1=IYY                                                                    23DEC04.1249
      M=JXX-1                                                                    23DEC04.1250
      MP1=JXX                                                                    23DEC04.1251
      N=KX                                                                       23DEC04.1252
      NP1=KXP1                                                                   23DEC04.1253
C                                                                                23DEC04.1254
C  CALCULATE THE LOOP BOUNDARIES                                                 23DEC04.1255
C                                                                                23DEC04.1256
      ICUT=0                                                                     23DEC04.1257
      IF(INEST.GT.1)ICUT=1                                                       23DEC04.1258
      ISTART=2+ICUT                                                              23DEC04.1259
      IEND=L-1-ICUT                                                              23DEC04.1260
      JSTART=2+ICUT                                                              23DEC04.1261
      JEND=M-1-ICUT                                                              23DEC04.1262
                                                                                 23DEC04.1263
#ifdef MPP1                                                                      23DEC04.1264
      nn = ie_x8(iend) - is_x8(istart) + 1                                       23DEC04.1265
      nn3 = ie_x8(L-ICUT) - is_x8(2+ICUT) + 1                                    23DEC04.1266
      is2 = is_x8(istart)                                                        23DEC04.1267
      is3 = is_x8(2+ICUT)                                                        23DEC04.1268
#else                                                                            23DEC04.1269
      nn = iend - istart + 1                                                     23DEC04.1270
      is2 = istart                                                               23DEC04.1271
      nn3 = L-ICUT - 1 + ICUT                                                    23DEC04.1272
      is3 = 2+ICUT                                                               23DEC04.1273
#endif                                                                           23DEC04.1274
                                                                                 23DEC04.1275
      if(ttime.gt.0.) then                                                       23DEC04.1276
         dsigmai(1) = 1./dsigma(1)                                               23DEC04.1277
         do k=2,mkx                                                              23DEC04.1278
            dsigmai(k) = 1./dsigma(k)                                            23DEC04.1279
            dsigmas(k) = 1./(dsigma(k)+dsigma(k-1))                              23DEC04.1280
         end do                                                                  23DEC04.1281
         ttime=-1.                                                               23DEC04.1282
      end if                                                                     23DEC04.1283
                                                                                 23DEC04.1284
      if(iinest.ne.inest) then                                                   23DEC04.1285
                                                                                 23DEC04.1286
         is  = min0(ISTART,1+ICUT)                                               23DEC04.1287
         ie  = max0(IEND,L-ICUT)                                                 23DEC04.1288
         is1 = min0(2+icut,istart)                                               23DEC04.1289
         ie1 = max0(L,L-ICUT)                                                    23DEC04.1290
         js = min0(JSTART,1+ICUT)                                                23DEC04.1291
         je = max0(JEND,M-ICUT)                                                  23DEC04.1292
                                                                                 23DEC04.1293
#ifdef MPP1                                                                      23DEC04.1294
         is4 = is_x8(is)                                                         23DEC04.1295
         is5 = is_x8(is1)                                                        23DEC04.1296
         is6 = is_x8(1)                                                          23DEC04.1297
         nn4 = ie_x8(ie)  - is_x8(is) + 1                                        23DEC04.1298
         nn5 = ie_x8(ie1) - is_x8(is1) + 1                                       23DEC04.1299
         nn6 = ie_x8(lp1) - is_x8(1) + 1                                         23DEC04.1300
#else                                                                            23DEC04.1301
         is4 = is                                                                23DEC04.1302
         is5 = is1                                                               23DEC04.1303
         is6 = 1                                                                 23DEC04.1304
         nn4 = ie - is + 1                                                       23DEC04.1305
         nn5 = ie1 - is1 + 1                                                     23DEC04.1306
         nn6 = lp1                                                               23DEC04.1307
#endif                                                                           23DEC04.1308
                                                                                 23DEC04.1309
c$omp parallel do default(shared)                                                23DEC04.1310
c$omp&private(i,j,k,kp1,km1,tmp)                                                 23DEC04.1311
         do j=1,jxx                                                              23DEC04.1312
            if(j.ge.jstart.and.j.le.jend) then                                   23DEC04.1313
               do i=is1,ie1                                                      23DEC04.1314
                  tmp(i,1) =  ps0(i,j)                                           23DEC04.1315
               end do                                                            23DEC04.1316
               call vsrec(rps0(nf(is5),j),tmp(nf(is5),1),nn5)                    23DEC04.1317
            end if                                                               23DEC04.1318
                                                                                 23DEC04.1319
            do i=1,lp1                                                           23DEC04.1320
               tmp(i,1) = msfd(i,j)                                              23DEC04.1321
               tmp(i,2) = msfx(i,j)                                              23DEC04.1322
            end do                                                               23DEC04.1323
            call vsrec(rmsfd(nf(is6),j),tmp(nf(is6),1),nn6)                      23DEC04.1324
            call vsrec(rmsfx(nf(is6),j),tmp(nf(is6),2),nn6)                      23DEC04.1325
                                                                                 23DEC04.1326
            if(j.ge.js.and.j.le.je) then                                         23DEC04.1327
               do k=1,n-1                                                        23DEC04.1328
                  km1 = k                                                        23DEC04.1329
                  kp1 = k+1                                                      23DEC04.1330
                  do i=is,ie                                                     23DEC04.1331
                     tmp(i,1) = pr0(i,j,km1) - pr0(i,j,kp1)                      23DEC04.1332
                  end do                                                         23DEC04.1333
                 call vsrec(rp1(nf(is4),j,k),tmp(nf(is4),1),nn4)                 23DEC04.1334
               end do                                                            23DEC04.1335
                                                                                 23DEC04.1336
               do k=2,n-1                                                        23DEC04.1337
                  km1 = k-1                                                      23DEC04.1338
                  kp1 = k+1                                                      23DEC04.1339
                  do i=is,ie                                                     23DEC04.1340
                     tmp(i,1) = pr0(i,j,km1) - pr0(i,j,kp1)                      23DEC04.1341
                  end do                                                         23DEC04.1342
                 call vsrec(rp2(nf(is4),j,k),tmp(nf(is4),1),nn4)                 23DEC04.1343
               end do                                                            23DEC04.1344
                                                                                 23DEC04.1345
               k=1                                                               23DEC04.1346
               km1=k                                                             23DEC04.1347
               kp1=k+1                                                           23DEC04.1348
               do i=is,ie                                                        23DEC04.1349
                  tmp(i,1) = pr0(i,j,km1) - pr0(i,j,kp1)                         23DEC04.1350
               end do                                                            23DEC04.1351
               call vsrec(rp2(nf(is4),j,k),tmp(nf(is4),1),nn4)                   23DEC04.1352
               k=n                                                               23DEC04.1353
               km1=k-1                                                           23DEC04.1354
               kp1=k                                                             23DEC04.1355
               do i=is,ie                                                        23DEC04.1356
                  tmp(i,1) = pr0(i,j,km1) - pr0(i,j,kp1)                         23DEC04.1357
               end do                                                            23DEC04.1358
               call vsrec(rp2(nf(is4),j,k),tmp(nf(is4),1),nn4)                   23DEC04.1359
            end if                                                               23DEC04.1360
         end do                                                                  23DEC04.1361
         iinest = inest                                                          23DEC04.1362
      end if                                                                     23DEC04.1363
                                                                                 23DEC04.1364
      IF(KTAU.EQ.0)THEN                                                          23DEC04.1365
      PRINT *,'SHORT TIME STEP ',DTS,ISTEP,' BETA = ',BET,                       23DEC04.1366
     -     ' XKD = ',XKD                                                         23DEC04.1367
cmic$ do all autoscope                                                           23DEC04.1368
c$doacross                                                                       23DEC04.1369
c$& local(i,j)                                                                   23DEC04.1370
c$omp parallel do default(shared)                                                23DEC04.1371
c$omp&private(i,j)                                                               23DEC04.1372
        DO 3 J=-6,6                                                              23DEC04.1373
          DO 3 I=-6,6                                                            23DEC04.1374
            TMASK(I,J)=0.                                                        23DEC04.1375
    3     CONTINUE                                                               23DEC04.1376
                                                                                 23DEC04.1377
        IF(IFUPR.EQ.1)THEN                                                       23DEC04.1378
C                                                                                23DEC04.1379
C  DEFINE VALUES OF FK, FL, FI & FJ FOR UPPER RADIATIVE BC                       23DEC04.1380
C                                                                                23DEC04.1381
          DO I=1,5                                                               23DEC04.1382
            FK(I)=2.0                                                            23DEC04.1383
            FL(I)=2.0                                                            23DEC04.1384
          ENDDO                                                                  23DEC04.1385
          FK(0)=1.0                                                              23DEC04.1386
          FL(0)=1.0                                                              23DEC04.1387
          FK(6)=1.0                                                              23DEC04.1388
          FL(6)=1.0                                                              23DEC04.1389
          DO I=-5,5                                                              23DEC04.1390
            FI(I)=1.0                                                            23DEC04.1391
            FJ(I)=1.0                                                            23DEC04.1392
          ENDDO                                                                  23DEC04.1393
          FI(-6)=0.5                                                             23DEC04.1394
          FJ(-6)=0.5                                                             23DEC04.1395
          FI(6)=0.5                                                              23DEC04.1396
          FJ(6)=0.5                                                              23DEC04.1397
        ENDIF                                                                    23DEC04.1398
      ENDIF                                                                      23DEC04.1399
C                                                                                23DEC04.1400
cmic$ do all autoscope                                                           23DEC04.1401
c$doacross local(i,j,k)                                                          23DEC04.1402
c$omp parallel do default(shared)                                                23DEC04.1403
c$omp&private(i,j,k,aux2,aux4,aux6)                                              23DEC04.1404
      DO J=1,JXX                                                                 23DEC04.1405
C                                                                                23DEC04.1406
C  PREMULTIPLY THE TENDENCY ARRAYS BY DTS                                        23DEC04.1407
C                                                                                23DEC04.1408
C  CALCULATE INITIAL ARRAYS FOR SHORT TIMESTEP                                   23DEC04.1409
C  XXB STORES FILTERED OLD XXA WITHOUT XXC TERM                                  23DEC04.1410
C  NO ASSELIN FILTER ON BOUNDARY                                                 23DEC04.1411
C                                                                                23DEC04.1412
        IF(J.LE.MP1)THEN                                                         23DEC04.1413
          DO K=1,N                                                               23DEC04.1414
            DO 200 I=1,LP1                                                       23DEC04.1415
              UTENS(I,J,K)=UTENS(I,J,K)*DTS                                      23DEC04.1416
              VTENS(I,J,K)=VTENS(I,J,K)*DTS                                      23DEC04.1417
              aux2 = UB(I,J,K)                                                   23DEC04.1418
              aux4 = VB(I,J,K)                                                   23DEC04.1419
              aux6 = XMUU(I,J,INEST) * rMSFD(I,J)                                23DEC04.1420
              U3D(I,J,K)=aux2 * RPDOTB(I,J)                                      23DEC04.1421
              V3D(I,J,K)=aux4 * RPDOTB(I,J)                                      23DEC04.1422
              aux2 = aux6 * UA(I,J,K) + XNUU(I,J,INEST)*aux2                     23DEC04.1423
              aux4 = aux6 * VA(I,J,K) + XNUU(I,J,INEST)*aux4                     23DEC04.1424
              UB(I,J,K) = aux2                                                   23DEC04.1425
              VB(I,J,K) = aux4                                                   23DEC04.1426
  200       CONTINUE                                                             23DEC04.1427
          ENDDO                                                                  23DEC04.1428
        ENDIF                                                                    23DEC04.1429
        IF(J.LE.M)THEN                                                           23DEC04.1430
          DO K=1,N                                                               23DEC04.1431
            DO 202 I=1,L                                                         23DEC04.1432
              aux2 = XMUT(I,J,INEST)                                             23DEC04.1433
              aux4 = PPB(I,J,K)                                                  23DEC04.1434
              aux6 = aux4 * XNUT(I,J,INEST)                                      23DEC04.1435
                                                                                 23DEC04.1436
              PPTENS(I,J,K)=PPTENS(I,J,K)*DTS                                    23DEC04.1437
              QV3D(I,J,K)=QVB(I,J,K)*RPSA(I,J)                                   23DEC04.1438
              PP3D(I,J,K)=aux4 * RPSA(I,J)                                       23DEC04.1439
              aux4 = XMUT(I,J,INEST)*PPA(I,J,K) + aux6                           23DEC04.1440
              PPB(I,J,K) = aux4                                                  23DEC04.1441
  202       CONTINUE                                                             23DEC04.1442
          ENDDO                                                                  23DEC04.1443
          DO K=1,NP1                                                             23DEC04.1444
            DO 204 I=1,L                                                         23DEC04.1445
              aux4 = WB(I,J,K)                                                   23DEC04.1446
              aux6 = XNUT(I,J,INEST)*aux4                                        23DEC04.1447
              WTENS(I,J,K)=WTENS(I,J,K)*DTS                                      23DEC04.1448
              W3D(I,J,K)= aux4*RPSA(I,J)                                         23DEC04.1449
              aux4=XMUT(I,J,INEST)*WA(I,J,K) +  aux6                             23DEC04.1450
              WB(I,J,K) = aux4                                                   23DEC04.1451
                                                                                 23DEC04.1452
  204       CONTINUE                                                             23DEC04.1453
          ENDDO                                                                  23DEC04.1454
        ENDIF                                                                    23DEC04.1455
      ENDDO                                                                      23DEC04.1456
c                                                                                23DEC04.1457
c     miter step                                                                 23DEC04.1458
c                                                                                23DEC04.1459
      DO 700 IT=1,ISTEP                                                          23DEC04.1460
                                                                                 23DEC04.1461
        ITL=MAX0(KTAU-1,0)                                                       23DEC04.1462
        IF(IT.EQ.1)GOTO 311                                                      23DEC04.1463
cmic$ do all autoscope                                                           23DEC04.1464
c$doacross                                                                       23DEC04.1465
c$& local(k,j,i)                                                                 23DEC04.1466
c$omp parallel do default(shared)                                                23DEC04.1467
c$omp&private(k,j,i,aux2,aux4)                                                   23DEC04.1468
        DO J=1,JXX                                                               23DEC04.1469
          IF(J.GE.JSTART.AND.J.LE.JEND)THEN                                      23DEC04.1470
            DO K=1,N                                                             23DEC04.1471
              DO 310 I=ISTART,IEND                                               23DEC04.1472
                PP3D(I,J,K) = PP3D(I,J,K) + XKD*PI(I,J,K)                        23DEC04.1473
  310         CONTINUE                                                           23DEC04.1474
            ENDDO                                                                23DEC04.1475
          ENDIF                                                                  23DEC04.1476
        ENDDO                                                                    23DEC04.1477
  311   CONTINUE                                                                 23DEC04.1478
                                                                                 23DEC04.1479
                                                                                 23DEC04.1480
cmic$ do all autoscope                                                           23DEC04.1481
cmic$1 shared  (l,m,n,icut,pp3d,pr0,t3d)                                         23DEC04.1482
cmic$2 private (i,j,k,kp1,km1)                                                   23DEC04.1483
c$doacross                                                                       23DEC04.1484
c$& local(i,j,k,kp1,km1)                                                         23DEC04.1485
c$omp parallel do default(shared)                                                23DEC04.1486
c$omp&private(i,j,k,kp1,km1)                                                     23DEC04.1487
        DO J=1,JXX                                                               23DEC04.1488
          IF(J.GE.1+ICUT.AND.J.LE.M-ICUT)THEN                                    23DEC04.1489
            DO K=2,N-1                                                           23DEC04.1490
              KP1=K+1                                                            23DEC04.1491
              KM1=K-1                                                            23DEC04.1492
                                                                                 23DEC04.1493
              DO I=1+ICUT,L-ICUT                                                 23DEC04.1494
                T3D(I,J,K)=(PP3D(I,J,KM1)-PP3D(I,J,KP1))*rP2(I,J,K)              23DEC04.1495
              ENDDO                                                              23DEC04.1496
            ENDDO                                                                23DEC04.1497
                                                                                 23DEC04.1498
              DO I=1+ICUT,L-ICUT                                                 23DEC04.1499
                T3D(I,J,1)=(PP3D(I,J,1)-PP3D(I,J,2))*rP2(I,J,1)                  23DEC04.1500
                T3D(I,J,n)=(PP3D(I,J,n-1)-PP3D(I,J,n))*rP2(I,J,n)                23DEC04.1501
              end do                                                             23DEC04.1502
                                                                                 23DEC04.1503
          ENDIF                                                                  23DEC04.1504
        ENDDO                                                                    23DEC04.1505
#ifdef MPP1                                                                      23DEC04.1506
#  include <mpp_sound_20.incl>                                                   23DEC04.1507
#endif                                                                           23DEC04.1508
C                                                                                23DEC04.1509
C  ADVANCE U AND V                                                               23DEC04.1510
C                                                                                23DEC04.1511
cmic$ do all autoscope                                                           23DEC04.1512
cmic$1 shared  (pp3d,pr0,rho1,msfd,u3d,v3d,dx,dts,t3d)                           23DEC04.1513
cmic$2 private (kp1,km1,rho,chh,dppdp0)                                          23DEC04.1514
c$doacross                                                                       23DEC04.1515
c$& local(i,j,k,rho,chh,dppdp0)                                                  23DEC04.1516
c$omp parallel do default(shared)                                                23DEC04.1517
c$omp&private(i,j,k,rho,chh,dppdp0,tmp,aux2,aux4,                                23DEC04.1518
c$omp&        aux6,aux8,aux0)                                                    23DEC04.1519
        DO J=1,JXX                                                               23DEC04.1520
          IF(J.GE.2+ICUT.AND.J.LE.M-ICUT)THEN                                    23DEC04.1521
            DO K=1,N                                                             23DEC04.1522
              DO I=2+ICUT,L-ICUT                                                 23DEC04.1523
                tmp(i,1)=0.25*(RHO1(I,J,K)+RHO1(I,J-1,K)+RHO1(I-1,J,K)+          23DEC04.1524
     +              RHO1(I-1,J-1,K))                                             23DEC04.1525
              end do                                                             23DEC04.1526
                                                                                 23DEC04.1527
              DO I=2+ICUT,L-ICUT                                                 23DEC04.1528
                tmp(i,2) =   T3D(I,J-1,K)  + T3D(I-1,J-1,K)                      23DEC04.1529
                tmp(i,3) = - PP3D(I,J-1,K) - PP3D(I-1,J-1,K)                     23DEC04.1530
                tmp(i,4) = - PR0(I,J-1,K)  - PR0(I-1,J-1,K)                      23DEC04.1531
                tmp(i,5) =   PR0(I,J-1,K)  - PR0(I-1,J-1,K)                      23DEC04.1532
                tmp(i,6) =   PP3D(I,J-1,K) - PP3D(I-1,J-1,K)                     23DEC04.1533
              end do                                                             23DEC04.1534
                                                                                 23DEC04.1535
            call vsrec(tmp(nf(is3),1),tmp(nf(is3),1),nn3)                        23DEC04.1536
                                                                                 23DEC04.1537
              DO  I=2+ICUT,L-ICUT                                                23DEC04.1538
                  aux2 = T3D(I,J,K)  + T3D(I-1,J,K)                              23DEC04.1539
                  aux4 = PP3D(I,J,K) + PP3D(I-1,J,K)                             23DEC04.1540
                  aux6 = PR0(I,J,K)  + PR0(I-1,J,K)                              23DEC04.1541
                  aux8 = PP3D(I,J,K) - PP3D(I-1,J,K)                             23DEC04.1542
                  aux0 = PR0(I,J,K)  - PR0(I-1,J,K)                              23DEC04.1543
                                                                                 23DEC04.1544
                CHH= dtsx1 *rMSFD(I,J) * tmp(i,1)                                23DEC04.1545
                                                                                 23DEC04.1546
                DPPDP0=0.25*(tmp(i,2)+aux2)                                      23DEC04.1547
                                                                                 23DEC04.1548
                U3D(I,J,K) = U3D(I,J,K)                                          23DEC04.1549
     +                      -CHH*(aux4 + tmp(i,3)                                23DEC04.1550
     +                      -(aux6 + tmp(i,4))*DPPDP0)                           23DEC04.1551
                                                                                 23DEC04.1552
                V3D(I,J,K) = V3D(I,J,K)                                          23DEC04.1553
     +                      -CHH*(aux8 + tmp(i,6)                                23DEC04.1554
     +                      -(aux0 + tmp(i,5)) *DPPDP0)                          23DEC04.1555
              end do                                                             23DEC04.1556
            ENDDO                                                                23DEC04.1557
          ENDIF                                                                  23DEC04.1558
        ENDDO                                                                    23DEC04.1559
                                                                                 23DEC04.1560
cmic$ do all autoscope                                                           23DEC04.1561
cmic$1 shared  (lp1,mp1,n,u3d,v3d,utens,vtens)                                   23DEC04.1562
cmic$2 private (i,j,k,aux2,aux4)                                                 23DEC04.1563
c$doacross                                                                       23DEC04.1564
c$& local(i,j,k)                                                                 23DEC04.1565
c$omp parallel do default(shared)                                                23DEC04.1566
c$omp&private(i,j,k)                                                             23DEC04.1567
        DO J=1,JXX                                                               23DEC04.1568
          IF(J.LE.MP1)THEN                                                       23DEC04.1569
            DO K=1,N                                                             23DEC04.1570
              DO 360 I=1,LP1                                                     23DEC04.1571
                 U3D(I,J,K) = U3D(I,J,K) + UTENS(I,J,K)                          23DEC04.1572
                 V3D(I,J,K) = V3D(I,J,K) + VTENS(I,J,K)                          23DEC04.1573
  360         CONTINUE                                                           23DEC04.1574
            ENDDO                                                                23DEC04.1575
          ENDIF                                                                  23DEC04.1576
        ENDDO                                                                    23DEC04.1577
C                                                                                23DEC04.1578
        IF(IT.EQ.1)GOTO 321                                                      23DEC04.1579
cmic$ do all autoscope                                                           23DEC04.1580
c$doacross                                                                       23DEC04.1581
c$& local(k,j,i)                                                                 23DEC04.1582
c$omp parallel do default(shared)                                                23DEC04.1583
c$omp&private(k,j,i,aux2,aux4)                                                   23DEC04.1584
        DO J=1,JXX                                                               23DEC04.1585
          IF(J.GE.JSTART.AND.J.LE.JEND)THEN                                      23DEC04.1586
            DO K=1,N                                                             23DEC04.1587
              DO 320 I=ISTART,IEND                                               23DEC04.1588
                 PP3D(I,J,K) = PP3D(I,J,K) - XKD * PI(I,J,K)                     23DEC04.1589
  320         CONTINUE                                                           23DEC04.1590
            ENDDO                                                                23DEC04.1591
          ENDIF                                                                  23DEC04.1592
        ENDDO                                                                    23DEC04.1593
  321   CONTINUE                                                                 23DEC04.1594
                                                                                 23DEC04.1595
#ifdef MPP1                                                                      23DEC04.1596
#  include <mpp_sound_30.incl>                                                   23DEC04.1597
#endif                                                                           23DEC04.1598
C                                                                                23DEC04.1599
C  SEMI-IMPLICIT SOLUTION FOR W AND P                                            23DEC04.1600
C                                                                                23DEC04.1601
cmic$ do all autoscope                                                           23DEC04.1602
cmic$1 shared  (psa,pp3d,xmut,xnut,tb)                                           23DEC04.1603
cmic$2 private (kp1,km1,tk)                                                      23DEC04.1604
cmic$2 private(rhs,wo,aa,b,c,ca,cc,cdd,cj,pyvp,pxup,ptend,g1,g2)                 23DEC04.1605
c$doacross                                                                       23DEC04.1606
c$& local(i,j,k,kp1,km1,wo,cc,cdd,cj,pyvp,ptend,rofac,                           23DEC04.1607
c$& tk,ca,g1,g2,c,b,aa,rhs,denom,pxup)                                           23DEC04.1608
c$omp parallel do default(shared)                                                23DEC04.1609
c$omp&private(i,j,k,kp1,km1,wo,cc,cdd,cj,pyvp,ptend,rofac,rrr,rr1,               23DEC04.1610
c$omp&tk,ca,g1,g2,c,b,aa,rhs,denom,pxup,tmp,aux2,aux4,                           23DEC04.1611
c$omp&     aux6,aux8,aux0)                                                       23DEC04.1612
        DO J=1,JXX                                                               23DEC04.1613
          IF(J.GE.JSTART.AND.J.LE.JEND)THEN                                      23DEC04.1614
            DO K=1,NP1                                                           23DEC04.1615
              DO I=2,L-1                                                         23DEC04.1616
                WO(I,K)=W3D(I,J,K)                                               23DEC04.1617
              ENDDO                                                              23DEC04.1618
            ENDDO                                                                23DEC04.1619
                                                                                 23DEC04.1620
          do k=1,n                                                               23DEC04.1621
             do i=istart,iend                                                    23DEC04.1622
                CC(I,K)= dtsx3 * PR1(I,J,K) * rMSFX(I,J)                         23DEC04.1623
                CDD(I,K)= PR1(I,J,K)*RHO0(I,J,K) *rPS0(I,J)*                     23DEC04.1624
     +                   (dtsx4 * DSIGMAi(K))                                    23DEC04.1625
                CJ(I,K)=RHO0(I,J,K)* dtsx5                                       23DEC04.1626
                tmp(i,1) = (2.*GAMMA)*PR0(I,J,K)* TB(I,J,K) * RPSA(I,J)          23DEC04.1627
             end do                                                              23DEC04.1628
                                                                                 23DEC04.1629
             call vsrec(tmp(nf(is2),1),tmp(nf(is2),1),nn)                        23DEC04.1630
                                                                                 23DEC04.1631
              DO  I=ISTART,IEND                                                  23DEC04.1632
                TK(I,K)=PS0(I,J)*T0(I,J,K) * tmp(i,1)                            23DEC04.1633
              end do                                                             23DEC04.1634
                                                                                 23DEC04.1635
          end do                                                                 23DEC04.1636
                                                                                 23DEC04.1637
          do k=2,n                                                               23DEC04.1638
             DO I=ISTART,IEND                                                    23DEC04.1639
                tmp(i,1) =                                                       23DEC04.1640
     *            DSIGMA(K-1)*RHO1(I,J,K)+DSIGMA(K)*RHO1(I,J,K-1)                23DEC04.1641
             end do                                                              23DEC04.1642
                                                                                 23DEC04.1643
             call vsrec(tmp(nf(is2),1),tmp(nf(is2),1),nn)                        23DEC04.1644
                                                                                 23DEC04.1645
             DO I=ISTART,IEND                                                    23DEC04.1646
                G1(I,K)=1.-DSIGMA(K-1)*TK(I,K)                                   23DEC04.1647
                G2(I,K)=1.+DSIGMA(K)*TK(I,K-1)                                   23DEC04.1648
                rr1 = - rp1(i,j,k-1)                                             23DEC04.1649
                ROFAC=(DSIGMA(K-1)*RHO0(I,J,K)+DSIGMA(K)*RHO0(I,J,K-1))          23DEC04.1650
     *                * tmp(i,1)                                                 23DEC04.1651
                                                                                 23DEC04.1652
                aux0 = rr1 * (G*DTS) * ROFAC                                     23DEC04.1653
                aux2 = CDD(I,K-1) - CJ(I,K-1)                                    23DEC04.1654
                aux4 = CDD(I,K)   - CJ(I,K)                                      23DEC04.1655
                aux6 = CDD(I,K)   + CJ(I,K)                                      23DEC04.1656
                aux8 = CDD(I,K-1) + CJ(I,K-1)                                    23DEC04.1657
C                                                                                23DEC04.1658
C      IMPLICIT W EQUATION COEFFICIENT ARRAYS AND RHS (IKAWA METHOD)             23DEC04.1659
C                                                                                23DEC04.1660
                C(I,K) = - (aux0*BPXBP)*aux2 * G2(I,K)                           23DEC04.1661
                B(I,K) = 1.+ (aux0*BPXBP)*(G1(I,K)*aux4+G2(I,K)*aux8)            23DEC04.1662
                AA(I,K)= - (aux0*BPXBP) * aux6 * G1(I,K)                         23DEC04.1663
                ca(i,k) = aux0                                                   23DEC04.1664
              end do                                                             23DEC04.1665
            end do                                                               23DEC04.1666
C                                                                                23DEC04.1667
C     VERTICAL BOUNDARY CONDITIONS, W=V.DH/DY AT BOTTOM, LID AT TOP              23DEC04.1668
C                                                                                23DEC04.1669
            DO I=ISTART,IEND                                                     23DEC04.1670
               tmp(i,2) = V3D(I+1,J+1,N) + V3D(I,J+1,N)                          23DEC04.1671
               tmp(i,3) = U3D(I+1,J+1,N) + U3D(I,J+1,N)                          23DEC04.1672
               tmp(i,4) = HT(I,J+1)      - HT(I,J-1)                             23DEC04.1673
            end do                                                               23DEC04.1674
                                                                                 23DEC04.1675
            DO I=ISTART,IEND                                                     23DEC04.1676
               aux2 = V3D(I+1,J,N) + V3D(I,J,N)                                  23DEC04.1677
               aux4 = HT(I+1,J)    - HT(I-1,J)                                   23DEC04.1678
               aux6 = U3D(I+1,J,N) + U3D(I,J,N)                                  23DEC04.1679
                                                                                 23DEC04.1680
               W3D(I,J,NP1)=rMSFX(I,J)* dtsx2*                                   23DEC04.1681
     +                       ((aux2 + tmp(i,2)) * aux4                           23DEC04.1682
     +                     + (aux6 +  tmp(i,3)) * tmp(i,4))                      23DEC04.1683
                                                                                 23DEC04.1684
              E(I,J,N)=0.                                                        23DEC04.1685
              F(I,J,N)=W3D(I,J,NP1)                                              23DEC04.1686
            end do                                                               23DEC04.1687
C                                                                                23DEC04.1688
            DO I=ISTART,IEND                                                     23DEC04.1689
               tmp(i,1) = V3D(I,J,1) + V3D(I+1,J,1)                              23DEC04.1690
     +                   -V3D(I,J,2) - V3D(I+1,J,2)                              23DEC04.1691
               tmp(i,2) = U3D(I,J,1) + U3D(I+1,J,1)                              23DEC04.1692
     +                   -U3D(I,J,2) - U3D(I+1,J,2)                              23DEC04.1693
            end do                                                               23DEC04.1694
                                                                                 23DEC04.1695
            DO I=ISTART,IEND                                                     23DEC04.1696
               tmp(i,3) = V3D(I,J+1,1)+V3D(I+1,J+1,1)                            23DEC04.1697
     +                   -V3D(I,J+1,2)-V3D(I+1,J+1,2)                            23DEC04.1698
               tmp(i,4) = U3D(I,J+1,1)+U3D(I+1,J+1,1)                            23DEC04.1699
     +                   -U3D(I,J+1,2)-U3D(I+1,J+1,2)                            23DEC04.1700
            end do                                                               23DEC04.1701
                                                                                 23DEC04.1702
            DO I=ISTART,IEND                                                     23DEC04.1703
                rrr =  rp1(I,J,1)                                                23DEC04.1704
                aux2 = (0.0625*rrr) * (PR0(I+1,J,1)-PR0(I-1,J,1))                23DEC04.1705
                aux4 = (0.0625*rrr) * (PR0(I,J+1,1)-PR0(I,J-1,1))                23DEC04.1706
c                                                                                23DEC04.1707
                PYVP(I,1) = aux2 * ( tmp(i,1) + tmp(i,3))                        23DEC04.1708
                PXUP(I,1) = aux4 * ( tmp(i,2) + tmp(i,4))                        23DEC04.1709
c                                                                                23DEC04.1710
            end do                                                               23DEC04.1711
                                                                                 23DEC04.1712
              DO I=ISTART,IEND                                                   23DEC04.1713
                 tmp(i,1) = -(V3D(I,J+1,1)-U3D(I,J+1,1))    *MSFD(I,J+1)         23DEC04.1714
                 tmp(i,2) =(V3D(I+1,J+1,1)+U3D(I+1,J+1,1))*MSFD(I+1,J+1)         23DEC04.1715
              end do                                                             23DEC04.1716
c                                                                                23DEC04.1717
              DO I=ISTART,IEND                                                   23DEC04.1718
                aux2 = (V3D(I+1,J,1)-U3D(I+1,J,1))    *MSFD(I+1,J)               23DEC04.1719
                aux4 = (V3D(I,J,1)  +U3D(I,J,1))      *MSFD(I,J)                 23DEC04.1720
c                                                                                23DEC04.1721
                PTEND(I,1)=PPTENS(I,J,1)-0.5*CC(I,1)*(                           23DEC04.1722
     +                   (aux2 - aux4 + tmp(i,1) + tmp(i,2)                      23DEC04.1723
     +                    )*rMSFX(I,J)  -2.*(PYVP(I,1)+PXUP(I,1)))               23DEC04.1724
                                                                                 23DEC04.1725
              ENDDO                                                              23DEC04.1726
                                                                                 23DEC04.1727
            DO K=2,N-1                                                           23DEC04.1728
              KP1=K+1                                                            23DEC04.1729
              KM1=K-1                                                            23DEC04.1730
              DO I=ISTART,IEND                                                   23DEC04.1731
                 tmp(i,1) = V3D(I,J,KM1)+V3D(I+1,J,KM1)                          23DEC04.1732
     +                     -V3D(I,J,KP1)-V3D(I+1,J,KP1)                          23DEC04.1733
                 tmp(i,2) = U3D(I,J,KM1)+U3D(I+1,J,KM1)                          23DEC04.1734
     +                     -U3D(I,J,KP1)-U3D(I+1,J,KP1)                          23DEC04.1735
              end do                                                             23DEC04.1736
              DO I=ISTART,IEND                                                   23DEC04.1737
                 tmp(i,3) = V3D(I,J+1,KM1)+V3D(I+1,J+1,KM1)                      23DEC04.1738
     +                     -V3D(I,J+1,KP1)-V3D(I+1,J+1,KP1)                      23DEC04.1739
                 tmp(i,4) = U3D(I,J+1,KM1)+U3D(I+1,J+1,KM1)                      23DEC04.1740
     +                     -U3D(I,J+1,KP1)-U3D(I+1,J+1,KP1)                      23DEC04.1741
              end do                                                             23DEC04.1742
                                                                                 23DEC04.1743
              DO I=ISTART,IEND                                                   23DEC04.1744
                rrr =  rp2(I,J,k)                                                23DEC04.1745
                aux2 = (0.125*rrr) * (PR0(I+1,J,K)-PR0(I-1,J,K))                 23DEC04.1746
                aux4 = (0.125*rrr) * (PR0(I,J+1,K)-PR0(I,J-1,K))                 23DEC04.1747
c                                                                                23DEC04.1748
                PYVP(I,K) = aux2 * ( tmp(i,1) + tmp(i,3))                        23DEC04.1749
                PXUP(I,K) = aux4 * ( tmp(i,2) + tmp(i,4))                        23DEC04.1750
c                                                                                23DEC04.1751
              end do                                                             23DEC04.1752
            end do                                                               23DEC04.1753
                                                                                 23DEC04.1754
                                                                                 23DEC04.1755
            k=n                                                                  23DEC04.1756
            KP1=k                                                                23DEC04.1757
            KM1=K-1                                                              23DEC04.1758
                                                                                 23DEC04.1759
            DO I=ISTART,IEND                                                     23DEC04.1760
               tmp(i,1) = V3D(I,J,KM1)+V3D(I+1,J,KM1)                            23DEC04.1761
     +                   -V3D(I,J,KP1)-V3D(I+1,J,KP1)                            23DEC04.1762
               tmp(i,2) = U3D(I,J,KM1)+U3D(I+1,J,KM1)                            23DEC04.1763
     +                   -U3D(I,J,KP1)-U3D(I+1,J,KP1)                            23DEC04.1764
            end do                                                               23DEC04.1765
            DO I=ISTART,IEND                                                     23DEC04.1766
               tmp(i,3) = V3D(I,J+1,KM1)+V3D(I+1,J+1,KM1)                        23DEC04.1767
     +                   -V3D(I,J+1,KP1)-V3D(I+1,J+1,KP1)                        23DEC04.1768
               tmp(i,4) = U3D(I,J+1,KM1)+U3D(I+1,J+1,KM1)                        23DEC04.1769
     +                   -U3D(I,J+1,KP1)-U3D(I+1,J+1,KP1)                        23DEC04.1770
            end do                                                               23DEC04.1771
                                                                                 23DEC04.1772
            DO I=ISTART,IEND                                                     23DEC04.1773
                rrr = rp2(i,j,k)                                                 23DEC04.1774
                aux2 = (0.0625*rrr) * (PR0(I+1,J,K)-PR0(I-1,J,K))                23DEC04.1775
                aux4 = (0.0625*rrr) * (PR0(I,J+1,K)-PR0(I,J-1,K))                23DEC04.1776
c                                                                                23DEC04.1777
                PYVP(I,K) = aux2 * ( tmp(i,1) + tmp(i,3))                        23DEC04.1778
                PXUP(I,K) = aux4 * ( tmp(i,2) + tmp(i,4))                        23DEC04.1779
            end do                                                               23DEC04.1780
                                                                                 23DEC04.1781
C                                                                                23DEC04.1782
C      ZERO GRADIENT (FREE SLIP) B.C.S ON V AT TOP AND BOTTOM                    23DEC04.1783
C                                                                                23DEC04.1784
            DO K=2,N                                                             23DEC04.1785
              DO I=ISTART,IEND                                                   23DEC04.1786
                 tmp(i,1) = -(V3D(I,J+1,K)-U3D(I,J+1,K))    *MSFD(I,J+1)         23DEC04.1787
                 tmp(i,2) =(V3D(I+1,J+1,K)+U3D(I+1,J+1,K))*MSFD(I+1,J+1)         23DEC04.1788
              end do                                                             23DEC04.1789
                                                                                 23DEC04.1790
              DO I=ISTART,IEND                                                   23DEC04.1791
                aux2 = (V3D(I+1,J,K)-U3D(I+1,J,K))    *MSFD(I+1,J)               23DEC04.1792
                aux4 = (V3D(I,J,K)  +U3D(I,J,K))      *MSFD(I,J)                 23DEC04.1793
                                                                                 23DEC04.1794
                PTEND(I,K)=PPTENS(I,J,K)-0.5*CC(I,K)*(                           23DEC04.1795
     +                   (aux2 - aux4 + tmp(i,1) + tmp(i,2)                      23DEC04.1796
     +                    )*rMSFX(I,J)  -2.*(PYVP(I,K)+PXUP(I,K)))               23DEC04.1797
                                                                                 23DEC04.1798
              end do                                                             23DEC04.1799
                                                                                 23DEC04.1800
              DO I=ISTART,IEND                                                   23DEC04.1801
                 tmp(i,1) = (CDD(I,K-1) - CJ(I,K-1))*WO(I,K-1)                   23DEC04.1802
                 tmp(i,2) =  CDD(I,K-1) + CJ(I,K-1)                              23DEC04.1803
              end do                                                             23DEC04.1804
                                                                                 23DEC04.1805
              DO I=ISTART,IEND                                                   23DEC04.1806
                 aux2 = G1(I,K)                                                  23DEC04.1807
                 aux4 = G2(I,K)                                                  23DEC04.1808
                 aux6 = CDD(I,K)-CJ(I,K)                                         23DEC04.1809
                 aux8 = (CDD(I,K)+CJ(I,K)) * WO(I,K+1)                           23DEC04.1810
                 RHS(I,K)=W3D(I,J,K)+WTENS(I,J,K)                                23DEC04.1811
     +                     +CA(I,K)*(BPXBM*(                                     23DEC04.1812
     +                       tmp(i,1)*aux4                                       23DEC04.1813
     +                     -(tmp(i,2)*aux4                                       23DEC04.1814
     +                      + aux6*aux2)*WO(I,K)                                 23DEC04.1815
     +                      +aux8 * aux2)                                        23DEC04.1816
     +                   +(PP3D(I,J,K)*aux2-PP3D(I,J,K-1)*aux4)                  23DEC04.1817
     +                   +(aux2*PTEND(I,K)-aux4*PTEND(I,K-1))*BP)                23DEC04.1818
              end do                                                             23DEC04.1819
            end do                                                               23DEC04.1820
C                                                                                23DEC04.1821
            DO 110 K=1,N                                                         23DEC04.1822
              DO 110 I=ISTART,IEND                                               23DEC04.1823
                PI(I,J,K)=PP3D(I,J,K)                                            23DEC04.1824
                PP3D(I,J,K)=PP3D(I,J,K)+PTEND(I,K)+(CJ(I,K)*(WO(I,K+1)+          23DEC04.1825
     +                      WO(I,K))+CDD(I,K)*(WO(I,K+1)-WO(I,K)))*BM            23DEC04.1826
  110         CONTINUE                                                           23DEC04.1827
                                                                                 23DEC04.1828
C                                                                                23DEC04.1829
C     UPWARD CALCULATION OF COEFFICIENTS                                         23DEC04.1830
C                                                                                23DEC04.1831
            DO 5 K=N,2,-1                                                        23DEC04.1832
              DO I=ISTART,IEND                                                   23DEC04.1833
                tmp(i,1) = AA(I,K)*E(I,J,K)+B(I,K)                               23DEC04.1834
              end do                                                             23DEC04.1835
                                                                                 23DEC04.1836
              call vsrec(tmp(nf(is2),1),tmp(nf(is2),1),nn)                       23DEC04.1837
                                                                                 23DEC04.1838
              DO I=ISTART,IEND                                                   23DEC04.1839
                E(I,J,K-1)=-C(I,K)*tmp(i,1)                                      23DEC04.1840
                F(I,J,K-1)=(RHS(I,K)-F(I,J,K)*AA(I,K))*tmp(i,1)                  23DEC04.1841
              end do                                                             23DEC04.1842
    5         CONTINUE                                                           23DEC04.1843
                                                                                 23DEC04.1844
C                                                                                23DEC04.1845
            DO 400 I=ISTART,IEND                                                 23DEC04.1846
              DENOM=(CDD(I,1)+CJ(I,1))*BP                                        23DEC04.1847
              ESTORE(I,J)=PP3D(I,J,1)+F(I,J,1)*DENOM                             23DEC04.1848
              ASTORE(I,J)=DENOM*E(I,J,1)+(CJ(I,1)-CDD(I,1))*BP                   23DEC04.1849
  400       CONTINUE                                                             23DEC04.1850
          ENDIF                                                                  23DEC04.1851
        ENDDO                                                                    23DEC04.1852
C                                                                                23DEC04.1853
C IF FIRST TIME THROUGH AND UPPER RADIATION B.C`S ARE USED                       23DEC04.1854
C NEED TO CALC SOME COEFFICIENTS                                                 23DEC04.1855
C                                                                                23DEC04.1856
        IF(IFUPR.EQ.1.AND.KTAU.EQ.0.AND.IT.EQ.1)THEN                             23DEC04.1857
C         PRINT *,' CALCULATING MEANS FOR UP. RAD. B.C.'                         23DEC04.1858
          ATOT=0.                                                                23DEC04.1859
          RHONTOT=0.                                                             23DEC04.1860
          XMSFTOT=0.                                                             23DEC04.1861
          NPTS=0                                                                 23DEC04.1862
#ifndef MPP1                                                                     23DEC04.1863
          DO 401 J=JSTART,JEND                                                   23DEC04.1864
            DO 401 I=ISTART,IEND                                                 23DEC04.1865
              ATOT=ATOT+ASTORE(I,J)                                              23DEC04.1866
              ENSQ=G*G/(CP*(TB(I,J,1)*RPSA(I,J)))                                23DEC04.1867
c                                                                                23DEC04.1868
              RHONTOT=RHONTOT+RHO1(I,J,1)*SQRT(ENSQ)                             23DEC04.1869
              XMSFTOT=XMSFTOT+MSFX(I,J)                                          23DEC04.1870
  401       CONTINUE                                                             23DEC04.1871
                                                                                 23DEC04.1872
          NPTS=(IEND-ISTART+1)*(JEND-JSTART+1)                                   23DEC04.1873
          rnpts =1./NPTS                                                         23DEC04.1874
          ABAR=ATOT*rNPTS                                                        23DEC04.1875
          RHON=RHONTOT*rNPTS                                                     23DEC04.1876
          XMSFBAR=XMSFTOT*rNPTS                                                  23DEC04.1877
#else                                                                            23DEC04.1878
#  include <mpp_sound_40.incl>                                                   23DEC04.1879
#endif                                                                           23DEC04.1880
          CPI=4.*ATAN(1.)                                                        23DEC04.1881
          cpi12=cpi/12.                                                          23DEC04.1882
                                                                                 23DEC04.1883
          DXMSFB=2./(DX*XMSFBAR)                                                 23DEC04.1884
C                                                                                23DEC04.1885
          DO 4 LL=0,6                                                            23DEC04.1886
            DO 4 K=0,6                                                           23DEC04.1887
              xkeff = sin(cpi12*k)  * cos(cpi12*ll)                              23DEC04.1888
              xleff = sin(cpi12*ll) * cos(cpi12*k)                               23DEC04.1889
              xkleff=dxmsfb*sqrt(xkeff*xkeff+xleff*xleff)                        23DEC04.1890
              rrr = FK(K)*FL(LL)/144.                                            23DEC04.1891
              aux2 = 2*k * cpi12                                                 23DEC04.1892
              aux4 = 2*ll* cpi12                                                 23DEC04.1893
c              XKEFF=DXMSFB*SIN(CPI*K/12.)*COS(CPI*LL/12.)                       23DEC04.1894
c              XLEFF=DXMSFB*SIN(CPI*LL/12.)*COS(CPI*K/12.)                       23DEC04.1895
c              XKLEFF=SQRT(XKEFF*XKEFF+XLEFF*XLEFF)                              23DEC04.1896
              XKLEFF = XKLEFF/(RHON-ABAR*XKLEFF)                                 23DEC04.1897
cmic$ do all autoscope                                                           23DEC04.1898
c$doacross                                                                       23DEC04.1899
c$& local(i,j)                                                                   23DEC04.1900
c$omp parallel do default(shared)                                                23DEC04.1901
c$omp&private(i,j,rr1)                                                           23DEC04.1902
              DO 8 J=-6,6                                                        23DEC04.1903
                 rr1 = rrr * FJ(J)                                               23DEC04.1904
                 DO 8 I=-6,6                                                     23DEC04.1905
c                  TMASK(I,J)=TMASK(I,J)+FI(I)*FJ(J)*FK(K)*FL(LL)/144.*          23DEC04.1906
c     +                       COS(2.*CPI*K*I/12.)*COS(2.*CPI*LL*J/12.)*          23DEC04.1907
c     +                       XKLEFF                                             23DEC04.1908
                    TMASK(I,J)=TMASK(I,J) + rr1 * FI(I) * XKLEFF *               23DEC04.1909
     +                         COS(aux2*I)*COS(aux4*J)                           23DEC04.1910
                                                                                 23DEC04.1911
    8           CONTINUE                                                         23DEC04.1912
    4       CONTINUE                                                             23DEC04.1913
                                                                                 23DEC04.1914
C         PRINT2002,ABAR,RHON                                                    23DEC04.1915
C         DO 9 J=0,6                                                             23DEC04.1916
C           PRINT2001,(DX*XMSFBAR*TMASK(I,J),I=0,6)                              23DEC04.1917
 2001       FORMAT (1X,7F10.5)                                                   23DEC04.1918
 2002 FORMAT(1X,'ABAR = ',F10.5,' RHON = ',E12.5,//,1X,' TMASK ')                23DEC04.1919
C   9     CONTINUE                                                               23DEC04.1920
        ENDIF                                                                    23DEC04.1921
                                                                                 23DEC04.1922
#ifdef MPP1                                                                      23DEC04.1923
#  include <mpp_sound_50.incl>                                                   23DEC04.1924
#endif                                                                           23DEC04.1925
C                                                                                23DEC04.1926
C  FINISHED INITIAL COEFFICIENT COMPUTE                                          23DEC04.1927
C  NOW DO DOWNWARD SWEEP FOR W                                                   23DEC04.1928
C                                                                                23DEC04.1929
cmic$ do all autoscope                                                           23DEC04.1930
cmic$1 shared  (rho1,rho0,insi,jnsj,wtij)                                        23DEC04.1931
cmic$2 private(wpval)                                                            23DEC04.1932
c$doacross                                                                       23DEC04.1933
c$& local(i,j,k,cdd,cj,wpval,cfl,rho0s,ucrs,vcrs,                                23DEC04.1934
c$&       check,sigdot,nsj,nsi,in,jn)                                            23DEC04.1935
c$omp parallel do default(shared)                                                23DEC04.1936
c$omp&private(i,j,k,cdd,cj,wpval,cfl,rho0s,ucrs,vcrs,check,sigdot,               23DEC04.1937
c$omp&nsj,nsi,in,jn,tmp,aux2,aux4)                                               23DEC04.1938
        DO 120 J=1,JXX                                                           23DEC04.1939
          IF(J.GE.JSTART.AND.J.LE.JEND)THEN                                      23DEC04.1940
C                                                                                23DEC04.1941
C  FIRST, SET UPPER BOUNDARY CONDITION, EITHER W=0 OR RADIATION                  23DEC04.1942
C                                                                                23DEC04.1943
            DO I=ISTART,IEND                                                     23DEC04.1944
              WPVAL(I)=0.                                                        23DEC04.1945
            ENDDO                                                                23DEC04.1946
C                                                                                23DEC04.1947
            IF(IFUPR.NE.1)GOTO 2125                                              23DEC04.1948
C                                                                                23DEC04.1949
C  APPLY UPPER RAD COND. NO W3D(TOP) IN LATERAL SPONGE                           23DEC04.1950
C                                                                                23DEC04.1951
            IF(J.LT.5.OR.J.GT.M-4)GOTO 2125                                      23DEC04.1952
            DO 2150 NSJ=-6,6                                                     23DEC04.1953
              JN=JNSJ(J,NSJ,INEST)                                               23DEC04.1954
              DO 2150 NSI=-6,6                                                   23DEC04.1955
                DO 2150 I=5,L-4                                                  23DEC04.1956
                  IN=INSI(I,NSI,INEST)                                           23DEC04.1957
#ifndef MPP1                                                                     23DEC04.1958
                  WPVAL(I)=WPVAL(I)+ESTORE(IN,JN)*TMASK(NSI,NSJ)*                23DEC04.1959
     +                     WTIJ(I,J,INEST)                                       23DEC04.1960
#else                                                                            23DEC04.1961
C  SEE IMPORTANT COMMENT IN THIS INCLUDED FILE RELATING TO MEMORY                23DEC04.1962
C  STORAGE AND INDEXING OF THE ARRAY ESTORE IN THE MPP CODE IN                   23DEC04.1963
C  THIS INCLUDED FILE                                                            23DEC04.1964
#  include <mpp_sound_55.incl>                                                   23DEC04.1965
#endif                                                                           23DEC04.1966
 2150           CONTINUE                                                         23DEC04.1967
                                                                                 23DEC04.1968
                                                                                 23DEC04.1969
 2125       CONTINUE                                                             23DEC04.1970
C                                                                                23DEC04.1971
C  FINISHED CALC OF RADIATION W                                                  23DEC04.1972
C                                                                                23DEC04.1973
            DO I=ISTART,IEND                                                     23DEC04.1974
              W3D(I,J,1)=WPVAL(I)                                                23DEC04.1975
            ENDDO                                                                23DEC04.1976
C                                                                                23DEC04.1977
C     DOWNWARD CALCULATION OF W                                                  23DEC04.1978
C                                                                                23DEC04.1979
            DO 10 K=1,N                                                          23DEC04.1980
              DO  I=ISTART,IEND                                                  23DEC04.1981
                  tmp(i,1) = U3D(I,J+1,K)+U3D(I+1,J+1,K)                         23DEC04.1982
                  tmp(i,2) = V3D(I,J+1,K)+V3D(I+1,J+1,K)                         23DEC04.1983
              end do                                                             23DEC04.1984
                                                                                 23DEC04.1985
              DO 10 I=ISTART,IEND                                                23DEC04.1986
                W3D(I,J,K+1)=E(I,J,K)*W3D(I,J,K)+F(I,J,K)                        23DEC04.1987
                aux2 = U3D(I,J,K)+U3D(I+1,J,K)                                   23DEC04.1988
                aux4 = V3D(I,J,K)+V3D(I+1,J,K)                                   23DEC04.1989
C                                                                                23DEC04.1990
C --- CALCULATE WIND AT CROSS POINT                                              23DEC04.1991
C                                                                                23DEC04.1992
                UCRS(I,K)= tmp(i,1) + aux2                                       23DEC04.1993
                VCRS(I,K)= tmp(i,2) + aux4                                       23DEC04.1994
   10         CONTINUE                                                           23DEC04.1995
                                                                                 23DEC04.1996
#ifdef MPP1                                                                      23DEC04.1997
#  include <mpp_sound_60.incl>                                                   23DEC04.1998
#endif                                                                           23DEC04.1999
C                                                                                23DEC04.2000
                                                                                 23DEC04.2001
            CFL=0.0                                                              23DEC04.2002
            DO K=N,2,-1                                                          23DEC04.2003
              DO I=ISTART,IEND                                                   23DEC04.2004
                 aux2 = TWT(K,2)*RHO0(I,J,K-1)                                   23DEC04.2005
                 aux4 = TWT(K,2)*UCRS(I,K-1)+TWT(K,1)*UCRS(I,K)                  23DEC04.2006
                 aux6 = TWT(K,2)*VCRS(I,K-1)+TWT(K,1)*VCRS(I,K)                  23DEC04.2007
                                                                                 23DEC04.2008
                RHO0S=TWT(K,1)*RHO0(I,J,K)+aux2                                  23DEC04.2009
                SIGDOT(I,K)=-RHO0S*(G*0.001)*W3D(I,J,K)*RPSA(I,J)                23DEC04.2010
     &                   -SIGMA(K)*(DPSDXM(I,J)*aux4                             23DEC04.2011
     &                             +DPSDYM(I,J)*aux6)                            23DEC04.2012
                                                                                 23DEC04.2013
                CHECK = SIGDOT(I,K) * DTL*DSIGMAs(K)                             23DEC04.2014
c                CHECK=ABS(SIGDOT(I,K))*(DTL*DSIGMAs(K))                         23DEC04.2015
                CFL=AMAX1(CHECK*check,CFL)                                       23DEC04.2016
              ENDDO                                                              23DEC04.2017
            ENDDO                                                                23DEC04.2018
#ifdef MPP1                                                                      23DEC04.2019
C CFLJ IS DEFINED IN CFLCOM.INCL AND USED IN mpp_sound_70.incl                   23DEC04.2020
            CFLJ(J)=CFL                                                          23DEC04.2021
#endif                                                                           23DEC04.2022
C                                                                                23DEC04.2023
            IF(CFL.GT.1.0)THEN                                                   23DEC04.2024
              DO K=N,2,-1                                                        23DEC04.2025
                DO I=ISTART,IEND                                                 23DEC04.2026
c                  CFL=ABS(SIGDOT(I,K))*DTL/(DSIGMA(K)+DSIGMA(K-1))              23DEC04.2027
                  CFL=ABS(SIGDOT(I,K))*(DTL*DSIGMAs(K))                          23DEC04.2028
                  IF(CFL.GT.1.)THEN                                              23DEC04.2029
#ifndef MPP1                                                                     23DEC04.2030
                    PRINT650,CFL,W3D(I,J,K),I,J,K,INEST                          23DEC04.2031
#else                                                                            23DEC04.2032
                    PRINT650,CFL,W3D(I,J,K),FLIC_L2G_M(I),FLIC_L2G_N(J),         23DEC04.2033
     +                       K,INEST                                             23DEC04.2034
#endif                                                                           23DEC04.2035
                  ENDIF                                                          23DEC04.2036
                ENDDO                                                            23DEC04.2037
              ENDDO                                                              23DEC04.2038
            ENDIF                                                                23DEC04.2039
          ENDIF                                                                  23DEC04.2040
C                                                                                23DEC04.2041
  650     FORMAT('CFL>1: CFL = ',F7.4,' W = ',F8.4,'  I = ',I5,                  23DEC04.2042
     +    '  J = ',I5,'  K = ',I5,'  INEST = ',I3)                               23DEC04.2043
  120   CONTINUE                                                                 23DEC04.2044
                                                                                 23DEC04.2045
#ifdef MPP1                                                                      23DEC04.2046
#  include <mpp_sound_70.incl>                                                   23DEC04.2047
#endif                                                                           23DEC04.2048
C                                                                                23DEC04.2049
C SET LATERAL W IF THIS IS A NESTED GRID                                         23DEC04.2050
C                                                                                23DEC04.2051
        IF(INEST.NE.1)THEN                                                       23DEC04.2052
cmic$ do all autoscope                                                           23DEC04.2053
c$doacross                                                                       23DEC04.2054
c$& local(k,i,j)                                                                 23DEC04.2055
c$omp parallel do default(shared)                                                23DEC04.2056
c$omp&private(k,i,j)                                                             23DEC04.2057
          DO J=1,JXX                                                             23DEC04.2058
            IF(J.EQ.2)THEN                                                       23DEC04.2059
              DO K=1,NP1                                                         23DEC04.2060
                DO I=2,L-1                                                       23DEC04.2061
                  W3D(I,2,K)=W3D(I,2,K)+WTENS(I,2,K)                             23DEC04.2062
                ENDDO                                                            23DEC04.2063
              ENDDO                                                              23DEC04.2064
              DO K=1,N                                                           23DEC04.2065
                DO I=2,L-1                                                       23DEC04.2066
                  PP3D(I,2,K)=PP3D(I,2,K)+PPTENS(I,2,K)                          23DEC04.2067
                ENDDO                                                            23DEC04.2068
              ENDDO                                                              23DEC04.2069
            ELSEIF(J.EQ.M-1)THEN                                                 23DEC04.2070
              DO K=1,NP1                                                         23DEC04.2071
                DO I=2,L-1                                                       23DEC04.2072
                  W3D(I,M-1,K)=W3D(I,M-1,K)+WTENS(I,M-1,K)                       23DEC04.2073
                ENDDO                                                            23DEC04.2074
              ENDDO                                                              23DEC04.2075
              DO K=1,N                                                           23DEC04.2076
                DO I=2,L-1                                                       23DEC04.2077
                  PP3D(I,M-1,K)=PP3D(I,M-1,K)+PPTENS(I,M-1,K)                    23DEC04.2078
                ENDDO                                                            23DEC04.2079
              ENDDO                                                              23DEC04.2080
            ELSEIF(J.GE.3.AND.J.LE.M-2)THEN                                      23DEC04.2081
              DO K=1,NP1                                                         23DEC04.2082
                W3D(2,J,K)=W3D(2,J,K)+WTENS(2,J,K)                               23DEC04.2083
                W3D(L-1,J,K)=W3D(L-1,J,K)+WTENS(L-1,J,K)                         23DEC04.2084
              ENDDO                                                              23DEC04.2085
              DO K=1,N                                                           23DEC04.2086
                PP3D(2,J,K)=PP3D(2,J,K)+PPTENS(2,J,K)                            23DEC04.2087
                PP3D(L-1,J,K)=PP3D(L-1,J,K)+PPTENS(L-1,J,K)                      23DEC04.2088
              ENDDO                                                              23DEC04.2089
            ENDIF                                                                23DEC04.2090
          ENDDO                                                                  23DEC04.2091
        ENDIF                                                                    23DEC04.2092
C                                                                                23DEC04.2093
C  NOW COMPUTE THE NEW PRESSURE                                                  23DEC04.2094
C                                                                                23DEC04.2095
cmic$ do all autoscope                                                           23DEC04.2096
cmic$2 private(cdd,cj)                                                           23DEC04.2097
c$doacross                                                                       23DEC04.2098
c$& local(j,k,i,ppold,cddtmp,cjtmp,cpm,dpterm)                                   23DEC04.2099
c$omp parallel do default(shared)                                                23DEC04.2100
c$omp&private(j,k,i,ppold,cddtmp,cjtmp,cpm,dpterm,tmp,                           23DEC04.2101
c$omp&        ua1,xnu,aux2)                                                      23DEC04.2102
        DO J=1,JXX                                                               23DEC04.2103
          IF(J.GE.JSTART.AND.J.LE.JEND)THEN                                      23DEC04.2104
            DO K=1,N                                                             23DEC04.2105
              DO I=ISTART,IEND                                                   23DEC04.2106
                tmp(i,1) = CP*(1.+0.8*QV3D(I,J,K)*XMOIST(INEST)) *               23DEC04.2107
     *                     RHO1(I,J,K)                                           23DEC04.2108
                tmp(i,2) =  W3D(I,J,K+1)+W3D(I,J,K)                              23DEC04.2109
                tmp(i,3) =  W3D(I,J,K+1)-W3D(I,J,K)                              23DEC04.2110
              end do                                                             23DEC04.2111
                                                                                 23DEC04.2112
              call vsrec(tmp(nf(is2),1),tmp(nf(is2),1),nn)                       23DEC04.2113
                                                                                 23DEC04.2114
              DO 15 I=ISTART,IEND                                                23DEC04.2115
                                                                                 23DEC04.2116
                PPOLD=PI(I,J,K)                                                  23DEC04.2117
                aux2 = PP3D(I,J,K)                                               23DEC04.2118
                xnu = XNUT(I,J,INEST)                                            23DEC04.2119
                                                                                 23DEC04.2120
                CDDTMP= PR1(I,J,K)*RHO0(I,J,K) * rPS0(I,J)*                      23DEC04.2121
     +                 (dtsx4*DSIGMAi(K))                                        23DEC04.2122
                CJTMP=RHO0(I,J,K)* dtsx5                                         23DEC04.2123
                aux2=aux2 + (CJTMP*tmp(i,2) + CDDTMP*tmp(i,3))*BP                23DEC04.2124
                PI(I,J,K)= aux2 - PPOLD-PPTENS(I,J,K)                            23DEC04.2125
C                                                                                23DEC04.2126
C COMPUTE PRESSURE DP`/DT CORRECTION TO THE TEMPERATURE                          23DEC04.2127
C                                                                                23DEC04.2128
                DPTERM=PSA(I,J)*(aux2 - PPOLD) * tmp(i,1)                        23DEC04.2129
                PP3D(I,J,K) = aux2                                               23DEC04.2130
                TB(I,J,K)=TB(I,J,K)+XNU * DPTERM                                 23DEC04.2131
                TA(I,J,K)=TA(I,J,K)+DPTERM                                       23DEC04.2132
                                                                                 23DEC04.2133
   15         CONTINUE                                                           23DEC04.2134
            ENDDO                                                                23DEC04.2135
          ENDIF                                                                  23DEC04.2136
        ENDDO                                                                    23DEC04.2137
                                                                                 23DEC04.2138
        NOCHECK=1                                                                23DEC04.2139
        IF(INEST.EQ.1)THEN                                                       23DEC04.2140
                                                                                 23DEC04.2141
cmic$ do all autoscope                                                           23DEC04.2142
c$doacross                                                                       23DEC04.2143
c$& local(k,j,i)                                                                 23DEC04.2144
c$omp parallel do default(shared)                                                23DEC04.2145
c$omp&private(k,j,i)                                                             23DEC04.2146
          DO 500 K=1,NP1                                                         23DEC04.2147
C     ZERO GRADIENT CONDITIONS ON W,  SPECIFIED ON PP                            23DEC04.2148
            DO 510 J=2,M-1                                                       23DEC04.2149
              W3D(1,J,K)=W3D(2,J,K)                                              23DEC04.2150
              W3D(L,J,K)=W3D(L-1,J,K)                                            23DEC04.2151
  510       CONTINUE                                                             23DEC04.2152
            DO 520 I=1,L                                                         23DEC04.2153
              W3D(I,1,K)=W3D(I,2,K)                                              23DEC04.2154
              W3D(I,M,K)=W3D(I,M-1,K)                                            23DEC04.2155
  520       CONTINUE                                                             23DEC04.2156
            IF(K.NE.NP1)THEN                                                     23DEC04.2157
              DO 517 J=2,M-1                                                     23DEC04.2158
                PP3D(1,J,K)=PP3D(1,J,K)+PPTENS(1,J,K)                            23DEC04.2159
                PP3D(L,J,K)=PP3D(L,J,K)+PPTENS(L,J,K)                            23DEC04.2160
  517         CONTINUE                                                           23DEC04.2161
              DO 518 I=1,L                                                       23DEC04.2162
                PP3D(I,1,K)=PP3D(I,1,K)+PPTENS(I,1,K)                            23DEC04.2163
                PP3D(I,M,K)=PP3D(I,M,K)+PPTENS(I,M,K)                            23DEC04.2164
  518         CONTINUE                                                           23DEC04.2165
            ENDIF                                                                23DEC04.2166
  500     CONTINUE                                                               23DEC04.2167
        ELSEIF(INEST.GT.1)THEN                                                   23DEC04.2168
cmic$ do all autoscope                                                           23DEC04.2169
c$doacross                                                                       23DEC04.2170
c$& local(k,j,i)                                                                 23DEC04.2171
c$omp parallel do default(shared)                                                23DEC04.2172
c$omp&private(k,j,i)                                                             23DEC04.2173
          DO 540 K=1,NP1                                                         23DEC04.2174
C     SPECIFIED ON W AND PP                                                      23DEC04.2175
            DO 530 J=2,M-1                                                       23DEC04.2176
              W3D(1,J,K)=W3D(1,J,K)+WTENS(1,J,K)                                 23DEC04.2177
              W3D(L,J,K)=W3D(L,J,K)+WTENS(L,J,K)                                 23DEC04.2178
  530       CONTINUE                                                             23DEC04.2179
            DO 535 I=1,L                                                         23DEC04.2180
              W3D(I,M,K)=W3D(I,M,K)+WTENS(I,M,K)                                 23DEC04.2181
              W3D(I,1,K)=W3D(I,1,K)+WTENS(I,1,K)                                 23DEC04.2182
  535       CONTINUE                                                             23DEC04.2183
            IF(K.NE.NP1)THEN                                                     23DEC04.2184
              DO 536 J=2,M-1                                                     23DEC04.2185
                PP3D(1,J,K)=PP3D(1,J,K)+PPTENS(1,J,K)                            23DEC04.2186
                PP3D(L,J,K)=PP3D(L,J,K)+PPTENS(L,J,K)                            23DEC04.2187
  536         CONTINUE                                                           23DEC04.2188
              DO 537 I=1,L                                                       23DEC04.2189
                PP3D(I,1,K)=PP3D(I,1,K)+PPTENS(I,1,K)                            23DEC04.2190
                PP3D(I,M,K)=PP3D(I,M,K)+PPTENS(I,M,K)                            23DEC04.2191
  537         CONTINUE                                                           23DEC04.2192
            ENDIF                                                                23DEC04.2193
  540     CONTINUE                                                               23DEC04.2194
        ENDIF                                                                    23DEC04.2195
C      END OF TIME LOOP 700                                                      23DEC04.2196
#ifdef MPP1                                                                      23DEC04.2197
#  include <mpp_sound_80.incl>                                                   23DEC04.2198
#endif                                                                           23DEC04.2199
                                                                                 23DEC04.2200
  700 CONTINUE                                                                   23DEC04.2201
                                                                                 23DEC04.2202
C     TRANSFER XXA TO XXB, NEW VALUES TO XXA AND APPLY TIME FILTER               23DEC04.2203
cmic$ do all autoscope                                                           23DEC04.2204
c$doacross                                                                       23DEC04.2205
c$& local(i,j,k)                                                                 23DEC04.2206
c$omp parallel do default(shared)                                                23DEC04.2207
c$omp&private(i,j,k,aux2,aux4,aux6,aux8,xnu)                                     23DEC04.2208
      DO J=1,JXX                                                                 23DEC04.2209
        IF(J.LE.MP1)THEN                                                         23DEC04.2210
          DO K=1,N                                                               23DEC04.2211
            DO I=1,LP1                                                           23DEC04.2212
              aux2 = PDOTB(I,J)*U3D(I,J,K)                                       23DEC04.2213
              aux4 = PDOTB(I,J)*V3D(I,J,K)                                       23DEC04.2214
              aux6 = UB(I,J,K)                                                   23DEC04.2215
              aux8 = VB(I,J,K)                                                   23DEC04.2216
              xnu = XNUU(I,J,INEST)                                              23DEC04.2217
                                                                                 23DEC04.2218
              aux6 = aux6 + aux2 * xnu                                           23DEC04.2219
              aux8 = aux8 + aux4 * xnu                                           23DEC04.2220
              UA(I,J,K) = aux2                                                   23DEC04.2221
              VA(I,J,K) = aux4                                                   23DEC04.2222
              UB(I,J,K) = aux6                                                   23DEC04.2223
              VB(I,J,K) = aux8                                                   23DEC04.2224
                                                                                 23DEC04.2225
            ENDDO                                                                23DEC04.2226
          ENDDO                                                                  23DEC04.2227
        ENDIF                                                                    23DEC04.2228
        IF(J.LE.M)THEN                                                           23DEC04.2229
          DO K=1,N                                                               23DEC04.2230
            DO I=1,L                                                             23DEC04.2231
              aux2 = PSA(I,J) * PP3D(I,J,K)                                      23DEC04.2232
              aux4 = PPB(I,J,K)                                                  23DEC04.2233
              xnu = XNUT(I,J,INEST)                                              23DEC04.2234
                                                                                 23DEC04.2235
              aux4 = aux4 + aux2 * xnu                                           23DEC04.2236
              PPA(I,J,K) = aux2                                                  23DEC04.2237
              PPB(I,J,K) = aux4                                                  23DEC04.2238
            ENDDO                                                                23DEC04.2239
          ENDDO                                                                  23DEC04.2240
          DO K=1,NP1                                                             23DEC04.2241
            DO I=1,L                                                             23DEC04.2242
              aux2 = PSA(I,J) * W3D(I,J,K)                                       23DEC04.2243
              aux4 = WB(I,J,K)                                                   23DEC04.2244
              xnu = XNUT(I,J,INEST)                                              23DEC04.2245
              aux4 = aux4 + aux2 * xnu                                           23DEC04.2246
                                                                                 23DEC04.2247
              WA(I,J,K) = aux2                                                   23DEC04.2248
              WB(I,J,K) = aux4                                                   23DEC04.2249
            ENDDO                                                                23DEC04.2250
          ENDDO                                                                  23DEC04.2251
        ENDIF                                                                    23DEC04.2252
      ENDDO                                                                      23DEC04.2253
                                                                                 23DEC04.2254
      RETURN                                                                     23DEC04.2255
      END                                                                        23DEC04.2256
                                                                                 23DEC04.2257
                                                                                 23DEC04.2258
#endif                                                                           23DEC04.2259
                                                                                 23DEC04.2260
                                                                                 23DEC04.2261
#ifdef vsLIB                                                                     23DEC04.2262
C_FLIC_BEGIN_NOFLIC                                                              23DEC04.2263
      subroutine vdiv(z,x,y,n)                                                   23DEC04.2264
      real*8 x(*),y(*),z(*)                                                      23DEC04.2265
      do 10 j=1,n                                                                23DEC04.2266
      z(j)=x(j)/y(j)                                                             23DEC04.2267
   10 continue                                                                   23DEC04.2268
      return                                                                     23DEC04.2269
      end                                                                        23DEC04.2270
                                                                                 23DEC04.2271
      subroutine vsdiv(z,x,y,n)                                                  23DEC04.2272
      real*4 x(*),y(*),z(*)                                                      23DEC04.2273
      do 10 j=1,n                                                                23DEC04.2274
      z(j)=x(j)/y(j)                                                             23DEC04.2275
   10 continue                                                                   23DEC04.2276
      return                                                                     23DEC04.2277
      end                                                                        23DEC04.2278
                                                                                 23DEC04.2279
      subroutine vexp(y,x,n)                                                     23DEC04.2280
      real*8 x(*),y(*)                                                           23DEC04.2281
      do 10 j=1,n                                                                23DEC04.2282
      y(j)=exp(x(j))                                                             23DEC04.2283
   10 continue                                                                   23DEC04.2284
      return                                                                     23DEC04.2285
      end                                                                        23DEC04.2286
                                                                                 23DEC04.2287
      subroutine vsexp(y,x,n)                                                    23DEC04.2288
      real*4 x(*),y(*)                                                           23DEC04.2289
      do 10 j=1,n                                                                23DEC04.2290
      y(j)=exp(x(j))                                                             23DEC04.2291
   10 continue                                                                   23DEC04.2292
      return                                                                     23DEC04.2293
      end                                                                        23DEC04.2294
                                                                                 23DEC04.2295
      subroutine vlog(y,x,n)                                                     23DEC04.2296
      real*8 x(*),y(*)                                                           23DEC04.2297
      do 10 j=1,n                                                                23DEC04.2298
      y(j)=log(x(j))                                                             23DEC04.2299
   10 continue                                                                   23DEC04.2300
      return                                                                     23DEC04.2301
      end                                                                        23DEC04.2302
                                                                                 23DEC04.2303
      subroutine vslog(y,x,n)                                                    23DEC04.2304
      real*4 x(*),y(*)                                                           23DEC04.2305
      do 10 j=1,n                                                                23DEC04.2306
      y(j)=log(x(j))                                                             23DEC04.2307
   10 continue                                                                   23DEC04.2308
      return                                                                     23DEC04.2309
      end                                                                        23DEC04.2310
                                                                                 23DEC04.2311
      subroutine vrec(y,x,n)                                                     23DEC04.2312
      real*8 x(*),y(*)                                                           23DEC04.2313
      do 10 j=1,n                                                                23DEC04.2314
      y(j)=1.d0/x(j)                                                             23DEC04.2315
   10 continue                                                                   23DEC04.2316
      return                                                                     23DEC04.2317
      end                                                                        23DEC04.2318
                                                                                 23DEC04.2319
      subroutine vsrec(y,x,n)                                                    23DEC04.2320
      real*4 x(*),y(*)                                                           23DEC04.2321
      do 10 j=1,n                                                                23DEC04.2322
      y(j)=1.d0/x(j)                                                             23DEC04.2323
   10 continue                                                                   23DEC04.2324
      return                                                                     23DEC04.2325
      end                                                                        23DEC04.2326
                                                                                 23DEC04.2327
c@process float(rsqrt)                                                           23DEC04.2328
      subroutine vrsqrt(y,x,n)                                                   23DEC04.2329
      real*8 x(*),y(*)                                                           23DEC04.2330
      do 10 j=1,n                                                                23DEC04.2331
      y(j)=1.d0/sqrt(x(j))                                                       23DEC04.2332
   10 continue                                                                   23DEC04.2333
      return                                                                     23DEC04.2334
      end                                                                        23DEC04.2335
                                                                                 23DEC04.2336
c@process float(rsqrt)                                                           23DEC04.2337
      subroutine vsrsqrt(y,x,n)                                                  23DEC04.2338
      real*4 x(*),y(*)                                                           23DEC04.2339
      do 10 j=1,n                                                                23DEC04.2340
      y(j)=1.d0/sqrt(x(j))                                                       23DEC04.2341
   10 continue                                                                   23DEC04.2342
      return                                                                     23DEC04.2343
      end                                                                        23DEC04.2344
                                                                                 23DEC04.2345
      subroutine vsincos(x,y,z,n)                                                23DEC04.2346
      real*8 x(*),y(*),z(*)                                                      23DEC04.2347
      do 10 j=1,n                                                                23DEC04.2348
      x(j)=sin(z(j))                                                             23DEC04.2349
      y(j)=cos(z(j))                                                             23DEC04.2350
   10 continue                                                                   23DEC04.2351
      return                                                                     23DEC04.2352
      end                                                                        23DEC04.2353
                                                                                 23DEC04.2354
      subroutine vssincos(x,y,z,n)                                               23DEC04.2355
      real*4 x(*),y(*),z(*)                                                      23DEC04.2356
      do 10 j=1,n                                                                23DEC04.2357
      x(j)=sin(z(j))                                                             23DEC04.2358
      y(j)=cos(z(j))                                                             23DEC04.2359
   10 continue                                                                   23DEC04.2360
      return                                                                     23DEC04.2361
      end                                                                        23DEC04.2362
                                                                                 23DEC04.2363
      subroutine vsqrt(y,x,n)                                                    23DEC04.2364
      real*8 x(*),y(*)                                                           23DEC04.2365
      do 10 j=1,n                                                                23DEC04.2366
      y(j)=sqrt(x(j))                                                            23DEC04.2367
   10 continue                                                                   23DEC04.2368
      return                                                                     23DEC04.2369
      end                                                                        23DEC04.2370
                                                                                 23DEC04.2371
      subroutine vssqrt(y,x,n)                                                   23DEC04.2372
      real*4 x(*),y(*)                                                           23DEC04.2373
      do 10 j=1,n                                                                23DEC04.2374
      y(j)=sqrt(x(j))                                                            23DEC04.2375
   10 continue                                                                   23DEC04.2376
      return                                                                     23DEC04.2377
      end                                                                        23DEC04.2378
                                                                                 23DEC04.2379
      subroutine vtan(y,x,n)                                                     23DEC04.2380
      real*8 x(*),y(*)                                                           23DEC04.2381
      do 10 j=1,n                                                                23DEC04.2382
      y(j)=tan(x(j))                                                             23DEC04.2383
   10 continue                                                                   23DEC04.2384
      return                                                                     23DEC04.2385
      end                                                                        23DEC04.2386
                                                                                 23DEC04.2387
      subroutine vstan(y,x,n)                                                    23DEC04.2388
      real*4 x(*),y(*)                                                           23DEC04.2389
      do 10 j=1,n                                                                23DEC04.2390
      y(j)=tan(x(j))                                                             23DEC04.2391
   10 continue                                                                   23DEC04.2392
      return                                                                     23DEC04.2393
      end                                                                        23DEC04.2394
                                                                                 23DEC04.2395
      subroutine vatan2(z,y,x,n)                                                 23DEC04.2396
      real*8 x(*),y(*),z(*)                                                      23DEC04.2397
      do 10 j=1,n                                                                23DEC04.2398
      z(j)=atan2(y(j),x(j))                                                      23DEC04.2399
   10 continue                                                                   23DEC04.2400
      return                                                                     23DEC04.2401
      end                                                                        23DEC04.2402
                                                                                 23DEC04.2403
      subroutine vsatan2(z,y,x,n)                                                23DEC04.2404
      real*4 x(*),y(*),z(*)                                                      23DEC04.2405
      do 10 j=1,n                                                                23DEC04.2406
      z(j)=atan2(y(j),x(j))                                                      23DEC04.2407
   10 continue                                                                   23DEC04.2408
      return                                                                     23DEC04.2409
      end                                                                        23DEC04.2410
                                                                                 23DEC04.2411
      subroutine vasin(y,x,n)                                                    23DEC04.2412
      real*8 x(*),y(*)                                                           23DEC04.2413
      do 10 j=1,n                                                                23DEC04.2414
      y(j)=asin(x(j))                                                            23DEC04.2415
   10 continue                                                                   23DEC04.2416
      return                                                                     23DEC04.2417
      end                                                                        23DEC04.2418
                                                                                 23DEC04.2419
      subroutine vsin(y,x,n)                                                     23DEC04.2420
      real*8 x(*),y(*)                                                           23DEC04.2421
      do 10 j=1,n                                                                23DEC04.2422
      y(j)=sin(x(j))                                                             23DEC04.2423
   10 continue                                                                   23DEC04.2424
      return                                                                     23DEC04.2425
      end                                                                        23DEC04.2426
                                                                                 23DEC04.2427
      subroutine vssin(y,x,n)                                                    23DEC04.2428
      real*4 x(*),y(*)                                                           23DEC04.2429
      do 10 j=1,n                                                                23DEC04.2430
      y(j)=sin(x(j))                                                             23DEC04.2431
   10 continue                                                                   23DEC04.2432
      return                                                                     23DEC04.2433
      end                                                                        23DEC04.2434
                                                                                 23DEC04.2435
      subroutine vacos(y,x,n)                                                    23DEC04.2436
      real*8 x(*),y(*)                                                           23DEC04.2437
      do 10 j=1,n                                                                23DEC04.2438
      y(j)=acos(x(j))                                                            23DEC04.2439
   10 continue                                                                   23DEC04.2440
      return                                                                     23DEC04.2441
      end                                                                        23DEC04.2442
                                                                                 23DEC04.2443
      subroutine vcos(y,x,n)                                                     23DEC04.2444
      real*8 x(*),y(*)                                                           23DEC04.2445
      do 10 j=1,n                                                                23DEC04.2446
      y(j)=cos(x(j))                                                             23DEC04.2447
   10 continue                                                                   23DEC04.2448
      return                                                                     23DEC04.2449
      end                                                                        23DEC04.2450
                                                                                 23DEC04.2451
      subroutine vscos(y,x,n)                                                    23DEC04.2452
      real*4 x(*),y(*)                                                           23DEC04.2453
      do 10 j=1,n                                                                23DEC04.2454
      y(j)=cos(x(j))                                                             23DEC04.2455
   10 continue                                                                   23DEC04.2456
      return                                                                     23DEC04.2457
      end                                                                        23DEC04.2458
                                                                                 23DEC04.2459
      subroutine vcosisin(y,x,n)                                                 23DEC04.2460
      complex*16 y(*)                                                            23DEC04.2461
      real*8 x(*)                                                                23DEC04.2462
      do 10 j=1,n                                                                23DEC04.2463
      y(j)=dcmplx(cos(x(j)),sin(x(j)))                                           23DEC04.2464
   10 continue                                                                   23DEC04.2465
      return                                                                     23DEC04.2466
      end                                                                        23DEC04.2467
                                                                                 23DEC04.2468
      subroutine vscosisin(y,x,n)                                                23DEC04.2469
      complex*8 y(*)                                                             23DEC04.2470
      real*4 x(*)                                                                23DEC04.2471
      do 10 j=1,n                                                                23DEC04.2472
      y(j)= cmplx(cos(x(j)),sin(x(j)))                                           23DEC04.2473
   10 continue                                                                   23DEC04.2474
      return                                                                     23DEC04.2475
      end                                                                        23DEC04.2476
                                                                                 23DEC04.2477
      subroutine vdint(y,x,n)                                                    23DEC04.2478
      real*8 x(*),y(*)                                                           23DEC04.2479
      do 10 j=1,n                                                                23DEC04.2480
      y(j)=dint(x(j))                                                            23DEC04.2481
   10 continue                                                                   23DEC04.2482
      return                                                                     23DEC04.2483
      end                                                                        23DEC04.2484
                                                                                 23DEC04.2485
      subroutine vdnint(y,x,n)                                                   23DEC04.2486
      real*8 x(*),y(*)                                                           23DEC04.2487
      do 10 j=1,n                                                                23DEC04.2488
      y(j)=dnint(x(j))                                                           23DEC04.2489
   10 continue                                                                   23DEC04.2490
      return                                                                     23DEC04.2491
      end                                                                        23DEC04.2492
                                                                                 23DEC04.2493
      subroutine vlog10(y,x,n)                                                   23DEC04.2494
      real*8 x(*),y(*)                                                           23DEC04.2495
      do 10 j=1,n                                                                23DEC04.2496
      y(j)=log10(x(j))                                                           23DEC04.2497
   10 continue                                                                   23DEC04.2498
      return                                                                     23DEC04.2499
      end                                                                        23DEC04.2500
                                                                                 23DEC04.2501
      subroutine vcosh(y,x,n)                                                    23DEC04.2502
      real*8 x(*),y(*)                                                           23DEC04.2503
      do 10 j=1,n                                                                23DEC04.2504
      y(j)=cosh(x(j))                                                            23DEC04.2505
   10 continue                                                                   23DEC04.2506
      return                                                                     23DEC04.2507
      end                                                                        23DEC04.2508
                                                                                 23DEC04.2509
      subroutine vsinh(y,x,n)                                                    23DEC04.2510
      real*8 x(*),y(*)                                                           23DEC04.2511
      do 10 j=1,n                                                                23DEC04.2512
      y(j)=sinh(x(j))                                                            23DEC04.2513
   10 continue                                                                   23DEC04.2514
      return                                                                     23DEC04.2515
      end                                                                        23DEC04.2516
                                                                                 23DEC04.2517
      subroutine vtanh(y,x,n)                                                    23DEC04.2518
      real*8 x(*),y(*)                                                           23DEC04.2519
      do 10 j=1,n                                                                23DEC04.2520
      y(j)=tanh(x(j))                                                            23DEC04.2521
   10 continue                                                                   23DEC04.2522
      return                                                                     23DEC04.2523
      end                                                                        23DEC04.2524
                                                                                 23DEC04.2525
      subroutine vpow(z,y,x,n)                                                   23DEC04.2526
      real*8 x(*),y(*),z(*)                                                      23DEC04.2527
      do 10 j=1,n                                                                23DEC04.2528
      z(j)=y(j)**x(j)                                                            23DEC04.2529
   10 continue                                                                   23DEC04.2530
      return                                                                     23DEC04.2531
      end                                                                        23DEC04.2532
                                                                                 23DEC04.2533
      subroutine vsasin(y,x,n)                                                   23DEC04.2534
      real*4 x(*),y(*)                                                           23DEC04.2535
      do 10 j=1,n                                                                23DEC04.2536
      y(j)=asin(x(j))                                                            23DEC04.2537
   10 continue                                                                   23DEC04.2538
      return                                                                     23DEC04.2539
      end                                                                        23DEC04.2540
                                                                                 23DEC04.2541
      subroutine vsacos(y,x,n)                                                   23DEC04.2542
      real*4 x(*),y(*)                                                           23DEC04.2543
      do 10 j=1,n                                                                23DEC04.2544
      y(j)=acos(x(j))                                                            23DEC04.2545
   10 continue                                                                   23DEC04.2546
      return                                                                     23DEC04.2547
      end                                                                        23DEC04.2548
                                                                                 23DEC04.2549
      subroutine vscosh(y,x,n)                                                   23DEC04.2550
      real*4 x(*),y(*)                                                           23DEC04.2551
      do 10 j=1,n                                                                23DEC04.2552
      y(j)=cosh(x(j))                                                            23DEC04.2553
   10 continue                                                                   23DEC04.2554
      return                                                                     23DEC04.2555
      end                                                                        23DEC04.2556
                                                                                 23DEC04.2557
      subroutine vslog10(y,x,n)                                                  23DEC04.2558
      real*4 x(*),y(*)                                                           23DEC04.2559
      do 10 j=1,n                                                                23DEC04.2560
      y(j)=log10(x(j))                                                           23DEC04.2561
   10 continue                                                                   23DEC04.2562
      return                                                                     23DEC04.2563
      end                                                                        23DEC04.2564
                                                                                 23DEC04.2565
      subroutine vspow(z,y,x,n)                                                  23DEC04.2566
      real*4 x(*),y(*),z(*)                                                      23DEC04.2567
      do 10 j=1,n                                                                23DEC04.2568
      z(j)=y(j)**x(j)                                                            23DEC04.2569
   10 continue                                                                   23DEC04.2570
      return                                                                     23DEC04.2571
      end                                                                        23DEC04.2572
                                                                                 23DEC04.2573
      subroutine vssinh(y,x,n)                                                   23DEC04.2574
      real*4 x(*),y(*)                                                           23DEC04.2575
      do 10 j=1,n                                                                23DEC04.2576
      y(j)=sinh(x(j))                                                            23DEC04.2577
   10 continue                                                                   23DEC04.2578
      return                                                                     23DEC04.2579
      end                                                                        23DEC04.2580
                                                                                 23DEC04.2581
      subroutine vstanh(y,x,n)                                                   23DEC04.2582
      real*4 x(*),y(*)                                                           23DEC04.2583
      do 10 j=1,n                                                                23DEC04.2584
      y(j)=tanh(x(j))                                                            23DEC04.2585
   10 continue                                                                   23DEC04.2586
      return                                                                     23DEC04.2587
      end                                                                        23DEC04.2588
C_FLIC_END_NOFLIC                                                                23DEC04.2589
#endif                                                                           23DEC04.2590
