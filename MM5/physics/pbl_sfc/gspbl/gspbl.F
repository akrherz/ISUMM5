CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          GSPBL.1
C                                                                     C          GSPBL.2
C                G A Y N O  -  S E A M A N   P B L                    C          GSPBL.3
C                          P S U                                      C          GSPBL.4
C                                                                     C          GSPBL.5
C     REFERENCES:                                                     C          GSPBL.6
C                                                                     C          GSPBL.7
C        GAYNO (1994) M.S. THESIS                                     C          GSPBL.8
C                                                                     C          GSPBL.9
C        MUSSON-GENON (1987, MWR)                                     C          GSPBL.10
C                                                                     C          GSPBL.11
C        BALLARD ET. AL. (1991, MWR)                                  C          GSPBL.12
C                                                                     C          GSPBL.13
C                                                                     C          GSPBL.14
C                                                                     C          GSPBL.15
C     CHANGES:                                                        C          07NOV00.1681
C     USE TGB AS TEMPERATURE, THGB AS THETA                           C          07NOV00.1682
C                                                                     C          GSPBL.16
C                                                                     C          GSPBL.17
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          GSPBL.18
C                                                                                GSPBL.19
C                                                                                GSPBL.20
      SUBROUTINE GSPBL(IYY,JXX,KZZ,J,IN,NST,U3D,V3D,T3D,QV3D,QC3D,               GSPBL.21
     &     PP3D,U3DTEN,V3DTEN,QC3DTEN,T3DTEN,QV3DTEN,TKETEND,IADVTKE)            GSPBL.22
      IMPLICIT NONE                                                              25JAN00.528
C                                                                                GSPBL.23
C                                                                                GSPBL.24
C                                                                                GSPBL.25
C INDEX OF SUBROUTINE                                                            GSPBL.26
C                                                                                GSPBL.27
C 0. PREAMBLE SECTION                                                            GSPBL.28
C                                                                                GSPBL.29
C 1. INITIALIZATION OF VARIABLES AND INITIAL COMPUTATIONS                        GSPBL.30
C                                                                                GSPBL.31
C 2. CALCULATION OF PBL HEIGHT                                                   GSPBL.32
C                                                                                GSPBL.33
C 3. OTHER CALCULATIONS                                                          GSPBL.34
C                                                                                GSPBL.35
C 4. COMPUTATION OF BLACKADAR BASIC LENGTH SCALE                                 GSPBL.36
C                                                                                GSPBL.37
C 5. COMPUTATION OF BETABT, BETABW AND NSQUAR                                    GSPBL.38
C                                                                                GSPBL.39
C 6. CALCULATE EDDY DIFFUSIVITIES FOR MOMENTUM, THL, QX AND TKE                  GSPBL.40
C                                                                                GSPBL.41
C 7. CALCULATE THE VARIANCES OF THETAL AND QWX AND THEIR COVARIANCE              GSPBL.42
C    ACCORDING TO APPENDIX B OF B91. RESULT IS AT THE FULL LEVELS.               GSPBL.43
C                                                                                GSPBL.44
C 8. CALCULATE FOG PARAMETERS                                                    GSPBL.45
C                                                                                GSPBL.46
C 9. COMPUTE SURFACE FLUXES                                                      GSPBL.47
C                                                                                GSPBL.48
C 10. CALCULATE GAMMAG AND HOL FOR REGIME 4                                      07NOV00.1683
C                                                                                GSPBL.52
C 11. COMPUTE THE GROUND TEMPERATURE: CALL TO SLAB                               07NOV00.1684
C                                                                                07NOV00.1685
C 12. CALCULATE THE VERTICAL FLUX OF TKE AT THE HALF LEVELS, TKEFLX.             GSPBL.53
C                                                                                GSPBL.54
C 13. CALCULATE NEW VALUE OF TOTAL WATER QWX                                     GSPBL.55
C                                                                                GSPBL.56
C 14.  CALCULATE TENDENCY FOR THETA L                                            GSPBL.57
C                                                                                GSPBL.58
C 15. FOG CALCULATIONS (OFF)                                                     GSPBL.59
C                                                                                GSPBL.60
C 16. CALCULATE NEW VALUE OF THLX                                                GSPBL.61
C                                                                                GSPBL.62
C 17. PARTITION TOTAL WATER INTO VAPOR AND LIQUID. (OFF)                         GSPBL.63
C                                                                                GSPBL.64
C 18. CALCULATE NEW VALUES OF THX, QX AND QCX USING NEW VALUES OF THLX A         GSPBL.65
C                                                                                GSPBL.66
C 19.  CALCULATE TENDENCIES FOR WINDS.                                           GSPBL.67
C                                                                                GSPBL.68
C 20. CALCULATE TENDENCIES FOR THETA, QX AND QC FOR USE                          GSPBL.69
C     OUTSIDE BOUNDARY LAYER CODE.                                               GSPBL.70
C                                                                                GSPBL.71
C 21. CALL TO GSRADSUB AND UPDATE TTNP (OFF)                                     GSPBL.72
C                                                                                GSPBL.73
C 22. CALCULATE NEW VALUES OF TKE                                                GSPBL.74
C                                                                                GSPBL.75
C 23. CONVERT TENDENCIES TO FLUX FORM AND CALCULATE OUTPUTS                      GSPBL.76
C     OF THE SUBROUTINE                                                          GSPBL.77
C                                                                                GSPBL.78
C 24. RESULTANT GROUND TEMPERATURE                                               GSPBL.79
C                                                                                GSPBL.80
C 25. SAVING KPBL IN KPBL2D                                                      GSPBL.81
C                                                                                GSPBL.82
C 26. VISIBILITY COMPUTATION (OFF)                                               GSPBL.83
C                                                                                GSPBL.84
C 27. FILE OUTPUT SECTION (OFF)                                                  GSPBL.85
C                                                                                GSPBL.86
C 28. RETURN                                                                     GSPBL.87
C                                                                                GSPBL.88
C                                                                                GSPBL.89
C                                                                                GSPBL.90
C 0. PREAMBLE SECTION                                                            GSPBL.91
C                                                                                GSPBL.92
C     INCLUDES                                                                   GSPBL.93
C                                                                                GSPBL.94
#     include <defines.incl>                                                     GSPBL.95
#     include <parame.incl>                                                      GSPBL.96
#     include <rpstar.incl>                                                      GSPBL.97
#     include <varia.incl>                                                       GSPBL.98
#     include <dusolve1.incl>                                                    GSPBL.99
#     include <param2.incl>                                                      GSPBL.100
#     include <param3.incl>                                                      GSPBL.101
#     include <pmoist.incl>                                                      GSPBL.102
#     include <point2d.incl>                                                     GSPBL.103
#     include <various.incl>                                                     GSPBL.104
#     include <nonhyd.incl>                                                      GSPBL.105
#     include <nhcnst.incl>                                                      GSPBL.106
#     include <addrcu.incl>                                                      GSPBL.107
#     include <radiat.incl>                                                      GSPBL.108
#     include <fogstuf.incl>                                                     GSPBL.109
#     include <addrfog.incl>                                                     GSPBL.110
#     include <fog1d.incl>                                                       GSPBL.111
#     include <pbltke.incl>                                                      GSPBL.112
#     include <surface.incl>                                                     GSPBL.113
#     include <surfslab.incl>                                                    GSPBL.114
#     include <soil.incl>                                                        GSPBL.115
#     include <sum.incl>                                                         GSPBL.116
C                                                                                GSPBL.117
      REAL      U3D(MIX,MJX,MKX),V3D(MIX,MJX,MKX),T3D(MIX,MJX,MKX),              25JAN00.529
     -    QV3D(MIX,MJX,MKX),PP3D(MIX,MJX,MKX),QC3D(MIXM,MJXM,MKXM)               GSPBL.119
      REAL      U3DTEN(MIX,MJX,MKX),V3DTEN(MIX,MJX,MKX),                         25JAN00.530
     -   T3DTEN(MIX,MJX,MKX),QV3DTEN(MIX,MJX,MKX),                               GSPBL.121
     -   QC3DTEN(MIXM,MJXM,MKXM)                                                 GSPBL.122
      REAL      TKETEND(MIXG,MJXG,MKXG)                                          25JAN00.531
      REAL      TSLDSB(MIXS,MLX)                                                 25JAN00.532
      INTEGER   IADVTKE                                                          25JAN00.533
C                                                                                GSPBL.125
C     TYPE AND DIMENSION DECLARATIONS                                            GSPBL.126
C                                                                                GSPBL.127
      REAL      KA,KH,KZOW,                                                      25JAN00.534
     &     KETHL,KBTHL,KCTHL                                                     GSPBL.129
C                                                                                GSPBL.130
      REAL      BBLS(MIX,MKX),                                                   GSPBL.131
     &     KQ(MIX,KXP1),       KTH(MIX,MKX+1),                                   GSPBL.132
     &     KET(MIX,MKX+1),                                                       GSPBL.133
     &     KBT(MIX,MKX+1),     KCT(MIX,MKX+1),                                   GSPBL.134
     &     LAMBDA(MIX,MKX),                                                      GSPBL.135
     &     LSUBH(MIX,MKX),     LSUBM(MIX,MKX),                                   GSPBL.136
     &     NSQUAR(MIX,MKX)                                                       GSPBL.137
C                                                                                GSPBL.138
C KZM IS REAL                                                                    25JAN00.535
      REAL BETABT(MIX,MKX),    BETABW(MIX,MKX),                                  25JAN00.536
     &     BOUYAN(MIX,MKX),                                                      25JAN00.537
     &     CRIC(MIX,MKX),                                                        25JAN00.538
     &     DISSIP(MIX,MKX),                                                      25JAN00.539
     &     DQWDZ(MIX,MKX),     DTKEDT(MIX,MKX),                                  25JAN00.540
     &     DTLDZ (MIX,MKX),    DTVDZ(MIX,MKX),                                   25JAN00.541
     &     DUDZ(MIX,MKX),      DVDZ(MIX,MKX),                                    25JAN00.542
     &     DZA(MIX,MKX),       DZQ(MIX,MKX),                                     25JAN00.543
     &     FACTQWDRY(MIX,MKX), FACTQWSAT(MIX,MKX),                               25JAN00.544
     &     FACTTLDRY(MIX,MKX), FACTTLSAT(MIX,MKX),                               25JAN00.545
     &     GAMMAG(MIX,MKX),                                                      25JAN00.546
     &     GOVTHV(MIX,MKX),                                                      25JAN00.547
     &     KZM(MIX,MKX),                                                         25JAN00.548
     &     PRESHL(MIX,MKX),                                                      25JAN00.549
     &     QCITNP(MIXG,MKXG),                                                    25JAN00.550
     &     QCX(MIXG,MKXG),     QCXS(MIXG,MKXG),                                  25JAN00.551
     &     QCXSS(MIX,MKX),     QCTNP(MIXG,MKXG),                                 25JAN00.552
     &     QTNP(MIX,MKX),      QWTNP(MIX,MKX),                                   25JAN00.553
     &     QWVAR(MIX,KXP1),                                                      25JAN00.554
     &     QWX(MIX,MKX),       QXS(MIX,MKX),                                     25JAN00.555
     &     QXSS(MIX,MKX),                                                        25JAN00.556
     &     RHOXFL(MIX,MKX),    RHOXHL(MIX,MKX),                                  25JAN00.557
     &     RICHN (MIX,MKX),                                                      25JAN00.558
     &     SHEAR(MIX,MKX),                                                       25JAN00.559
     &     SVS(MIX,MKX),                                                         25JAN00.560
     &     TAU0(MIX,MKX),                                                        25JAN00.561
     &     THLTNP(MIX,MKX),                                                      25JAN00.562
     &     THLX(MIX,MKX),                                                        25JAN00.563
     &     THXS(MIX,MKX),      THXSS(MIX,MKX),                                   25JAN00.564
     &     TKEBOLD(MIX,KXP1),                                                    25JAN00.565
     &     TKEFLX(MIX,MKX),                                                      25JAN00.566
     &     TLVAR(MIX,KXP1),    TQCVAR(MIX,KXP1),                                 25JAN00.567
     &     TTNP(MIX,MKX),                                                        25JAN00.568
     &     TURBT(MIX,MKX),                                                       25JAN00.569
     &     UTNP(MIX,MKX),       UXS(MIX,MKX),                                    25JAN00.570
     &     VTNP(MIX,MKX),       VXS(MIX,MKX),                                    25JAN00.571
     &     XLVP(MIX,MKX)                                                         25JAN00.572
C                                                                                25JAN00.573
      REAL ALFA(MKX),                                                            25JAN00.574
     &     BM(MKX),     EM(MKX),                                                 25JAN00.575
     &     FQ(MKX),     FQC(MKX),                                                25JAN00.576
     &     FTKE(MKX),   FTHX(MKX),                                               25JAN00.577
     &     FU(MKX),     FV(MKX),                                                 25JAN00.578
     &     QCXSV(MIX),  QXSV(MIX),                                               25JAN00.579
     &     THXSV(MIX),  THGB(MIX),                                               07NOV00.1686
     &     UXSV(MIX),   VXSV(MIX)                                                25JAN00.581
      INTEGER                                                                    25JAN00.582
     &     MINIMUM(MKX),NTKELYR(MKX)                                             25JAN00.583
C     DECLARATIONS FOR IMPLICIT NONE                                             07NOV00.1687
C                                                                                07NOV00.1688
      INTEGER I,J,K,L,KK,MM,IN,KB,KE                                             07NOV00.1689
      INTEGER IYY,JXX,KZZ,NST,KF                                                 07NOV00.1690
      INTEGER ITKESAT,IDRS1,IDRS2,IDRS3,ILAMBDA,MITERING,IKBKC,MSTEPS            07NOV00.1691
      INTEGER KTEMP1,KTEMP2,KTEMP,ICOUNT,NOVERMIN,INDSATUP,INDSATDW              07NOV00.1692
      INTEGER INDTRANS,KLIM,IBGN,IEND,JBGN,JEND                                  07NOV00.1693
      REAL QCMIN,XMITPAR,PL,THCON,TVCON,PS,E1,PS1,CELL,PRESB,FRACZ               07NOV00.1694
      REAL DTTKE,PTKEMAX,PTKEFRACT,PTKEWEAK,PTKEZMIN,TKEBMAX,TKELIM              07NOV00.1695
      REAL FRACT,ZAMX,TGBV,DTHVDZ,DTHVM,VCONV,FRCTN,THXFL,GOVTH,THVXFL           07NOV00.1696
      REAL DEPTH,TKELMIN,XLMIN,XFR,FRACMINLIM,TKEMAXUP,TKEMAXDW,FRACMIN          07NOV00.1697
      REAL ZUPP,ZDW,BETAW,BETAT,EXNER,EVAP,ALFAL,ABARL,BBARL,BETAL               07NOV00.1698
      REAL BBTFL,BBWFL,XLMIN1,XLMIN2,XLMAX,XLSTAB,CZERO,FSUBL,TOP                07NOV00.1699
      REAL TOP2,FSUB2,BOT,FSUB1,FSUB3,FSUB0                                      07NOV00.1700
      REAL TAU1,TAU4,TAU6,TAU8,TAU9,WVAR,WBCVAR,TERM1,TERM2,TERM3                07NOV00.1701
      REAL DENOM,TAU3,PSIX,USTM,DTG,SIGFGT,XMAVL,WSPDX,DELTSM,TSTAR              07NOV00.1702
      REAL WSTAR,BUOYFL,XMOL,TERM4,TERM5,TERM6,BETAK,EES,QSAT,FRT                07NOV00.1703
      REAL XLOVCPT,XLOVRT,ALFA1,BETA1,XLAMDA1,FACTTLFL,FACTQWFL,BUOFLUX          07NOV00.1704
      REAL CHECK,FMULT,TBOVTH,ZL,XKMAXM,XKMAXH,XKMAX,XDTM,TGC,TSLC               07NOV00.1705
      REAL TGBTMP                                                                07NOV00.1706
C                                                                                07NOV00.1707
C                                                                                GSPBL.186
C     COMMON SECTION                                                             GSPBL.187
C                                                                                GSPBL.188
C                                                                                GSPBL.189
C                                                                                GSPBL.190
C     DATA SECTION                                                               GSPBL.191
C                                                                                GSPBL.192
C     THESE CONSTANTS ARE USED IN DETERMINING THE DISSIPATION TIME               GSPBL.193
C     SCALE (TAU0) ACCORDING TO B91 PAGE 2108 EQ 14.  FSUBL IS ALSO              GSPBL.194
C     USED IN FINDING B91'S LENGTH SCALES.  SEE APPENDIX A FOR DETAILS.          GSPBL.195
C                                                                                GSPBL.196
      DATA CZERO /5.52427/                                                       GSPBL.197
      DATA FSUBL /7.09759/                                                       GSPBL.198
C                                                                                GSPBL.199
C     THESE CONSTANTS ARE USED IN FINDING B91'S LENGTH SCALES.                   GSPBL.200
C     SEE APPENDIX A.                                                            GSPBL.201
C                                                                                GSPBL.202
      DATA FSUB0 /0.7307/                                                        GSPBL.203
      DATA FSUB1 /8.5773/                                                        GSPBL.204
      DATA FSUB2 /11.579/                                                        GSPBL.205
      DATA FSUB3 /14.884/                                                        GSPBL.206
C                                                                                GSPBL.207
      DATA CZO/0.032/                                                            GSPBL.208
      DATA OZO/1.E-4/                                                            GSPBL.209
      DATA KA/2.4E-5/                                                            GSPBL.210
C                                                                                GSPBL.211
C     MINIMUM DIFFUSIVITY FOR POINTS OVER WATER                                  GSPBL.212
C                                                                                GSPBL.213
      DATA KZOW/0.05/                                                            GSPBL.214
C                                                                                GSPBL.215
C                                                                                GSPBL.216
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.217
C                                                                                GSPBL.218
C     MAIN OPTIONS OF THE ROUTINE                                                GSPBL.219
C                                                                                GSPBL.220
C                                                                                GSPBL.221
      ITKESAT=1                                                                  GSPBL.222
!MAKE ACTIVE THE USE OF SATURATED EXPRESSIONS FOR TH                             GSPBL.223
C                    OF TKE AND THE SATURATED EXPRESSION FOR NSQUAR IN T         GSPBL.224
C                    ARE SATURATED.                                              GSPBL.225
C                                                                                GSPBL.226
      QCMIN=0.00001                                                              GSPBL.227
!THRESHOLD FOR CONSIDERING SATURATED CONDITIONS IN T                             GSPBL.228
C                    THIS PARAMETER IS EFFECTIVE ONLY IF ITKESAT=1               GSPBL.229
C                                                                                GSPBL.230
      IDRS1=1                                                                    GSPBL.231
!DAVE MODS RELATED WITH XLVP(TEMP)                                               GSPBL.232
      IDRS2=1                                                                    GSPBL.233
!DAVE MODS FOR NEW CALCULATION OF THX(THLX)                                      GSPBL.234
      IDRS3=1                                                                    GSPBL.235
!DAVE MODS FOR CALCULATING QCX WITH A CONSERVATION E                             GSPBL.236
C                                                                                GSPBL.237
      ILAMBDA=2                                                                  GSPBL.238
!DEFINES SCHEME FOR LAMBDA                                                       GSPBL.239
!ILAMBDA=1 FOR SCHEME BASED ON DRY RICH PROFILES                                 GSPBL.240
!ILAMBDA=2 FOR SCHEME BASED ON TKE PROFILE                                       GSPBL.241
C                                                                                GSPBL.242
      MITERING=1                                                                 GSPBL.243
!USE MITERING OR NOT                                                             GSPBL.244
C                                                                                GSPBL.245
      XMITPAR=0.45                                                               GSPBL.246
!PARAMETER TO CONTROL STRENGTH OF MITERING (EFFECTIV                             GSPBL.247
C                                                                                GSPBL.248
      IKBKC=0                                                                    GSPBL.249
!0 FOR NOT CONSIDERING KB AND KC                                                 GSPBL.250
                                                                                 25JAN00.604
#ifdef MPP1                                                                      25JAN00.605
CMPP1      FLIC_DO_M(I,1,ILX)                                                    25JAN00.606
#endif                                                                           25JAN00.607
                                                                                 25JAN00.608
C                                                                                GSPBL.251
C                                                                                GSPBL.252
C     MITERING                                                                   GSPBL.253
C                                                                                GSPBL.254
#ifndef MPP1                                                                     25JAN00.609
      IF(MITERING.EQ.0)THEN                                                      GSPBL.255
        MSTEPS=1                                                                 GSPBL.256
      ELSE                                                                       GSPBL.257
        IF(KTAU.EQ.0)THEN                                                        GSPBL.258
          MSTEPS=20                                                              07NOV00.1708
        ELSE                                                                     GSPBL.260
          MSTEPS=MSTEPSS(J)                                                      GSPBL.261
        ENDIF                                                                    GSPBL.262
      ENDIF                                                                      GSPBL.263
#else                                                                            25JAN00.610
      IF(MITERING.EQ.0)THEN                                                      25JAN00.611
        MSTEPS=1                                                                 25JAN00.612
      ELSE                                                                       25JAN00.613
        IF(KTAU.EQ.0)THEN                                                        25JAN00.614
          MSTEPS=20                                                              07NOV00.1709
        ELSE                                                                     25JAN00.616
          MSTEPS=MSTEPSS(NOFLIC(I),J)                                            25JAN00.617
        ENDIF                                                                    25JAN00.618
      ENDIF                                                                      25JAN00.619
#endif                                                                           25JAN00.620
C                                                                                25JAN00.621
C                                                                                25JAN00.622
#ifdef BIT_FOR_BIT_KLUDGE                                                        25JAN00.623
C NEVER SET THIS OPERATIONALLY.  STRICTLY FOR ASCERTAINING ABILITY TO            25JAN00.624
C GIVE BIT-REPRODUCIBLE RESULTS BETWEEN MPP AND NON-MPP VERSIONS.                25JAN00.625
          MSTEPS=13                                                              25JAN00.626
#endif                                                                           25JAN00.627
C                                                                                25JAN00.628
C                                                                                GSPBL.264
C     SET QCMIN TO A LARGE VALUE IF ITKESAT=0                                    GSPBL.265
C                                                                                GSPBL.266
      IF(ITKESAT.EQ.0)QCMIN=1000.0                                               GSPBL.267
C                                                                                GSPBL.268
C                                                                                GSPBL.269
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.270
C                                                                                GSPBL.271
C 1. INITIALIZATION OF VARIABLES AND INITIAL COMPUTATIONS                        GSPBL.272
C                                                                                GSPBL.273
C                                                                                GSPBL.274
C     SAVING INITIAL VALUE OF TKE                                                GSPBL.275
C                                                                                GSPBL.276
      DO I=1,ILX                                                                 GSPBL.277
        DO K=1,KXP1                                                              GSPBL.278
          TKEBOLD(I,K)=TKEB(I,J,K)                                               GSPBL.279
        ENDDO                                                                    GSPBL.280
      ENDDO                                                                      GSPBL.281
C                                                                                GSPBL.282
C     INITIALIZE VARIABLES THAT COME FROM THE MAIN MODEL                         GSPBL.283
C                                                                                GSPBL.284
C     WIND COMPONENTS                                                            GSPBL.285
C                                                                                GSPBL.286
      DO I=1,ILX                                                                 GSPBL.287
        DO K=1,KL                                                                GSPBL.288
          UX(I,K)=0.25*(U3D(I,J,K)+U3D(I+1,J,K)+U3D(I,J+1,K)+                    GSPBL.289
     +            U3D(I+1,J+1,K))                                                GSPBL.290
          VX(I,K)=0.25*(V3D(I,J,K)+V3D(I+1,J,K)+V3D(I,J+1,K)+                    GSPBL.291
     +            V3D(I+1,J+1,K))                                                GSPBL.292
        ENDDO                                                                    GSPBL.293
      ENDDO                                                                      GSPBL.294
C                                                                                GSPBL.295
C     PRESSURE AT HALF LEVELS AND AT THE SURFACE (IN CENTIBARS)                  GSPBL.296
C                                                                                GSPBL.297
      DO I=1,ILX                                                                 GSPBL.298
        DO K=1,KL                                                                GSPBL.299
          PRESHL(I,K)=A(K)*PSB(I,J)+PTOP+PP3D(I,J,K)*0.001                       GSPBL.300
        ENDDO                                                                    GSPBL.301
      ENDDO                                                                      GSPBL.302
      DO I=1,ILX                                                                 GSPBL.303
C     CHANGE TO USE PP3D (PPB) FOR BOTH PSURFB AND PSURFA (JD)                   GSPBL.304
        PSURFA(I)=PSA(I,J)+PTOP+PP3D(I,J,KL)*0.001                               GSPBL.305
        PSURFB(I)=PSB(I,J)+PTOP+PP3D(I,J,KL)*0.001                               GSPBL.306
      ENDDO                                                                      GSPBL.307
C                                                                                GSPBL.308
C     AIR TEMPERATURE AND WATER VARIABLES                                        GSPBL.309
C                                                                                GSPBL.310
C     SCR3(I,K) STORE TEMPERATURE,                                               GSPBL.311
C     SCR4(I,K) STORE VIRTUAL TEMPERATURE.                                       GSPBL.312
C                                                                                GSPBL.313
C     FIRST ASSUME DRY CASE                                                      GSPBL.314
C                                                                                GSPBL.315
      DO I=1,ILX                                                                 GSPBL.316
        DO K=1,KL                                                                GSPBL.317
          SCR3(I,K)=T3D(I,J,K)                                                   GSPBL.318
          PL=PRESHL(I,K)                                                         GSPBL.319
          THCON=(100./PL)**ROVCP                                                 GSPBL.320
          THX(I,K)=SCR3(I,K)*THCON                                               GSPBL.321
          QX(I,K)=0.                                                             GSPBL.322
          SCR4(I,K)=SCR3(I,K)                                                    GSPBL.323
          THVX(I,K)=THX(I,K)                                                     GSPBL.324
          QWX(I,K)=0.0                                                           GSPBL.325
          QCX(I,K)=0.0                                                           GSPBL.326
          THLX(I,K)=THX(I,K)                                                     GSPBL.327
          CPM(I,K)=CP                                                            GSPBL.328
          IF(IDRS1.NE.1)THEN                                                     GSPBL.329
            XLVP(I,K)=XLV                                                        GSPBL.330
          ELSE                                                                   GSPBL.331
            IF(SCR3(I,K).GT.TO. OR. IICE.EQ.1)THEN                               07NOV00.1710
              XLVP(I,K)=XLV0-XLV1*SCR3(I,K)                                      GSPBL.333
            ELSE                                                                 GSPBL.334
              XLVP(I,K)=XLS                                                      GSPBL.335
            ENDIF                                                                GSPBL.336
          ENDIF                                                                  GSPBL.337
        ENDDO                                                                    GSPBL.338
      ENDDO                                                                      GSPBL.339
C                                                                                GSPBL.340
C     THEN CONSIDER CASES WITH WATER                                             GSPBL.341
C                                                                                GSPBL.342
      IF(IDRY(IN).NE.1)THEN                                                      GSPBL.343
        DO I=1,ILX                                                               GSPBL.344
          DO K=1,KL                                                              GSPBL.345
            QX(I,K)=QV3D(I,J,K)                                                  GSPBL.346
            TVCON=(1.+XMOIST(IN)*EP1*QX(I,K))                                    GSPBL.347
            SCR4(I,K)=SCR3(I,K)*TVCON                                            GSPBL.348
            THVX(I,K)=THX(I,K)*TVCON                                             GSPBL.349
            QWX(I,K)=QX(I,K)                                                     GSPBL.350
            CPM(I,K)=CP*(1.+0.8*QX(I,K)*XMOIST(IN))                              GSPBL.351
          ENDDO                                                                  GSPBL.352
        ENDDO                                                                    GSPBL.353
        DO I=1,ILX                                                               GSPBL.354
          DO K=1,KL                                                              GSPBL.355
            IF(IMOIST(IN).NE.1)QCX(I,K)=QC3D(I,J,K)                              GSPBL.356
            THVX(I,K)=THVX(I,K)-THX(I,K)*QCX(I,K)                                GSPBL.357
            QWX(I,K)=QX(I,K)+QCX(I,K)                                            GSPBL.358
            IF(IDRS2.NE.1)THEN                                                   GSPBL.359
              THLX(I,K)=THX(I,K)-THX(I,K)*XLVP(I,K)/(SCR3(I,K)*CPM(I,K))         GSPBL.360
     +                  *QCX(I,K)                                                GSPBL.361
            ELSE                                                                 GSPBL.362
              THLX(I,K)=THX(I,K)-THX(I,K)*XLVP(I,K)/(                            GSPBL.363
     +                  AMAX1(SCR3(I,K),253.)*CPM(I,K))*QCX(I,K)                 GSPBL.364
            ENDIF                                                                GSPBL.365
          ENDDO                                                                  GSPBL.366
        ENDDO                                                                    GSPBL.367
      ENDIF                                                                      GSPBL.368
C                                                                                GSPBL.369
C     OTHER INITIALIZATIONS                                                      GSPBL.370
C                                                                                GSPBL.371
      DO I=1,ILX                                                                 GSPBL.372
        QGG(I)=0.                                                                GSPBL.373
        HFKLMX(I)=200.                                                           GSPBL.374
        FLHC(I)=0.                                                               GSPBL.375
        FLQC(I)=0.                                                               GSPBL.376
      ENDDO                                                                      GSPBL.377
      IF(IDRY(IN).NE.1)THEN                                                      GSPBL.378
        DO I=1,ILX                                                               GSPBL.379
          PS=PSURFB(I)                                                           GSPBL.380
          E1=SVP1*EXP(SVP2*(TGB(I,J)-SVPT0)/(TGB(I,J)-SVP3))                     GSPBL.381
          QGG(I)=EP2*E1/(PS-E1)                                                  GSPBL.382
        ENDDO                                                                    GSPBL.383
      ENDIF                                                                      GSPBL.384
C                                                                                GSPBL.385
C     SAVE SOIL TEMPERATURE BEFORE CONVERTING TO POTENTIAL TEMPERATURE           GSPBL.386
C                                                                                GSPBL.387
      DO I=1,ILX                                                                 GSPBL.388
        TGDSA(I)=TGB(I,J)                                                        GSPBL.389
      ENDDO                                                                      GSPBL.390
C                                                                                GSPBL.391
C     COMPUTE SOIL POTENTIAL TEMPERATURE                                         GSPBL.392
C                                                                                GSPBL.393
      DO I=1,ILX                                                                 GSPBL.394
        PS1=PSURFB(I)                                                            GSPBL.395
        THGB(I)=TGB(I,J)*(100.0/PS1)**ROVCP                                      07NOV00.1711
        PS1=PSURFA(I)                                                            GSPBL.397
      ENDDO                                                                      GSPBL.399
C                                                                                GSPBL.400
C     SAVE THE INITIAL VALUE OF TGB                                              GSPBL.401
C                                                                                GSPBL.402
      DO I=1,ILX                                                                 GSPBL.403
        TGDSB(I)=TGB(I,J)                                                        GSPBL.404
        IF(ISOIL.EQ.1)THEN                                                       GSPBL.405
          DO L=1,5                                                               GSPBL.406
            TSLDSB(I,L)=TSLB(I,J,L)                                              GSPBL.407
          ENDDO                                                                  GSPBL.408
        ENDIF                                                                    GSPBL.409
      ENDDO                                                                      GSPBL.410
C                                                                                GSPBL.411
C     INITIALIZE TENDENCIES OF U, V, T AND Q'S                                   GSPBL.412
C                                                                                GSPBL.413
      DO I=1,ILX                                                                 GSPBL.414
        DO K=1,KL                                                                GSPBL.415
          UTNP(I,K)=0.                                                           GSPBL.416
          VTNP(I,K)=0.                                                           GSPBL.417
          TTNP(I,K)=0.                                                           GSPBL.418
        ENDDO                                                                    GSPBL.419
      ENDDO                                                                      GSPBL.420
      IF(IDRY(IN).NE.1)THEN                                                      GSPBL.421
        DO I=1,ILX                                                               GSPBL.422
          DO K=1,KL                                                              GSPBL.423
            QTNP(I,K)=0.                                                         GSPBL.424
            QWTNP(I,K)=0.                                                        GSPBL.425
            QCTNP(I,K)=0.                                                        GSPBL.426
            QCITNP(I,K)=0.                                                       GSPBL.427
          ENDDO                                                                  GSPBL.428
        ENDDO                                                                    GSPBL.429
      ENDIF                                                                      GSPBL.430
C                                                                                GSPBL.431
C     HEIGHT LEVELS                                                              GSPBL.432
C                                                                                GSPBL.433
      DO I=1,ILX                                                                 GSPBL.434
        ZQ(I,KLP1)=0.                                                            GSPBL.435
      ENDDO                                                                      GSPBL.436
      DO KK=1,KL                                                                 GSPBL.437
        K=KLP1-KK                                                                GSPBL.438
        DO I=1,ILX                                                               GSPBL.439
          DUM1(I)=ZQ(I,K+1)                                                      GSPBL.440
        ENDDO                                                                    GSPBL.441
        DO I=1,ILX                                                               GSPBL.442
          CELL=PTOP/PSB(I,J)                                                     GSPBL.443
          ZQ(I,K)=ROVG*T0(I,J,K)*ALOG((SIGMA(K+1)+CELL)/(SIGMA(K)+CELL))         GSPBL.444
     +            +DUM1(I)                                                       GSPBL.445
        ENDDO                                                                    GSPBL.446
      ENDDO                                                                      GSPBL.447
      DO I=1,ILX                                                                 GSPBL.448
        DO K=1,KL                                                                GSPBL.449
          ZA(I,K)=0.5*(ZQ(I,K)+ZQ(I,K+1))                                        GSPBL.450
          DZQ(I,K)=ZQ(I,K)-ZQ(I,K+1)                                             GSPBL.451
        ENDDO                                                                    GSPBL.452
      ENDDO                                                                      GSPBL.453
      DO I=1,ILX                                                                 GSPBL.454
        DO K=1,KLM                                                               GSPBL.455
          DZA(I,K)=ZA(I,K)-ZA(I,K+1)                                             GSPBL.456
        ENDDO                                                                    GSPBL.457
      ENDDO                                                                      GSPBL.458
C                                                                                GSPBL.459
C     DENSITY AT THE SURFACE, AT HALF AND FULL LAYERS                            GSPBL.460
C                                                                                GSPBL.461
      DO I=1,ILX                                                                 GSPBL.462
        RHOX(I)=(PSURFB(I)*1000.)/(R*SCR4(I,KL))                                 GSPBL.463
      ENDDO                                                                      GSPBL.464
      DO I=1,ILX                                                                 GSPBL.465
        DO K=1,KL                                                                GSPBL.466
          PRESB=PRESHL(I,K)*1000.0                                               GSPBL.467
          RHOXHL(I,K)=PRESB/(R*SCR4(I,K))                                        GSPBL.468
        ENDDO                                                                    GSPBL.469
      ENDDO                                                                      GSPBL.470
      DO I=1,ILX                                                                 GSPBL.471
        DO K=2,KL                                                                GSPBL.472
          FRACZ=(ZQ(I,K)-ZA(I,K))/DZA(I,K-1)                                     GSPBL.473
          RHOXFL(I,K)=RHOXHL(I,K)+(RHOXHL(I,K-1)-RHOXHL(I,K))*FRACZ              GSPBL.474
        ENDDO                                                                    GSPBL.475
      ENDDO                                                                      GSPBL.476
C                                                                                GSPBL.477
C     CALCULATE THE CRITICAL RICHARDSON NUMBER AT THE FULL LEVELS.               GSPBL.478
C     SEE EQ 3.4.3 IN NCAR TECHNICAL NOTE.                                       GSPBL.479
C                                                                                GSPBL.480
      IF(ILAMBDA.EQ.1)THEN                                                       GSPBL.481
        DO I=1,ILX                                                               GSPBL.482
          DO K=2,KL                                                              GSPBL.483
            CRIC(I,K)=0.257*DZA(I,K-1)**0.175                                    GSPBL.484
          ENDDO                                                                  GSPBL.485
        ENDDO                                                                    GSPBL.486
      ENDIF                                                                      GSPBL.487
C                                                                                GSPBL.488
C     DTSTEP AND DTTKE                                                           GSPBL.489
C                                                                                GSPBL.490
      DTSTEP=DT/FLOAT(MSTEPS)                                                    GSPBL.491
C                                                                                GSPBL.492
      IF(IADVTKE.EQ.1)THEN                                                       GSPBL.493
        DTTKE=DTSTEP                                                             GSPBL.494
      ELSE                                                                       GSPBL.495
        DTTKE=DTSTEP/2.0                                                         GSPBL.496
      ENDIF                                                                      GSPBL.497
C                                                                                GSPBL.498
C     SAVE INITIAL VARIABLES                                                     GSPBL.499
C                                                                                GSPBL.500
      DO I=1,ILX                                                                 GSPBL.501
        DO K=1,KL                                                                GSPBL.502
          THXS(I,K)=THX(I,K)                                                     GSPBL.503
          UXS(I,K)=UX(I,K)                                                       GSPBL.504
          VXS(I,K)=VX(I,K)                                                       GSPBL.505
        ENDDO                                                                    GSPBL.506
      ENDDO                                                                      GSPBL.507
      IF(IDRY(IN).NE.1)THEN                                                      GSPBL.508
        DO I=1,ILX                                                               GSPBL.509
          DO K=1,KL                                                              GSPBL.510
            QXS(I,K)=QX(I,K)                                                     GSPBL.511
          ENDDO                                                                  GSPBL.512
        ENDDO                                                                    GSPBL.513
        DO I=1,ILX                                                               GSPBL.514
          DO K=1,KL                                                              GSPBL.515
            QCXS(I,K)=QCX(I,K)                                                   GSPBL.516
          ENDDO                                                                  GSPBL.517
        ENDDO                                                                    GSPBL.518
      ENDIF                                                                      GSPBL.519
C                                                                                GSPBL.520
C     MITERING SAVE VARIABLES                                                    GSPBL.521
C                                                                                GSPBL.522
      DO K=1,KL                                                                  GSPBL.523
        DO I=1,ILX                                                               GSPBL.524
          THXSS(I,K)=THXS(I,K)                                                   GSPBL.525
          QXSS(I,K)=QXS(I,K)                                                     GSPBL.526
          QCXSS(I,K)=QCXS(I,K)                                                   GSPBL.527
        ENDDO                                                                    GSPBL.528
      ENDDO                                                                      GSPBL.529
C                                                                                GSPBL.530
C     ----MITER LOOP START----------                                             GSPBL.531
C                                                                                GSPBL.532
      DO MM=1,MSTEPS                                                             GSPBL.533
C                                                                                GSPBL.534
        IF(MM.GT.1)THEN                                                          GSPBL.535
          DO K=1,KL                                                              GSPBL.536
            DO I=1,ILX                                                           GSPBL.537
              PL=PRESHL(I,K)                                                     GSPBL.538
              THCON=(100./PL)**ROVCP                                             GSPBL.539
              SCR3(I,K)=THX(I,K)/THCON                                           GSPBL.540
              TVCON=(1.+XMOIST(IN)*EP1*QX(I,K))                                  GSPBL.541
              SCR4(I,K)=SCR3(I,K)*TVCON                                          GSPBL.542
              THVX(I,K)=THX(I,K)*TVCON                                           GSPBL.543
              THVX(I,K)=THVX(I,K)-THX(I,K)*QCX(I,K)                              GSPBL.544
              CPM(I,K)=CP*(1.+0.8*QX(I,K)*XMOIST(IN))                            GSPBL.545
              IF(IDRS2.NE.1)THEN                                                 GSPBL.546
                THLX(I,K)=THX(I,K)-THX(I,K)*XLVP(I,K)/(SCR3(I,K)*                GSPBL.547
     +                    CPM(I,K))*QCX(I,K)                                     GSPBL.548
              ELSE                                                               GSPBL.549
                THLX(I,K)=THX(I,K)-THX(I,K)*XLVP(I,K)/(                          GSPBL.550
     +                    AMAX1(SCR3(I,K),253.)*CPM(I,K))*QCX(I,K)               GSPBL.551
              ENDIF                                                              GSPBL.552
              QWX(I,K)=QX(I,K)+QCX(I,K)                                          GSPBL.553
            ENDDO                                                                GSPBL.554
          ENDDO                                                                  GSPBL.555
          DO I=1,ILX                                                             GSPBL.556
            RHOX(I)=(PSURFB(I)*1000.)/(R*SCR4(I,KL))                             GSPBL.557
          ENDDO                                                                  GSPBL.558
          DO K=1,KL                                                              GSPBL.559
            DO I=1,ILX                                                           GSPBL.560
              UTNP(I,K)=0.                                                       GSPBL.561
              VTNP(I,K)=0.                                                       GSPBL.562
              TTNP(I,K)=0.                                                       GSPBL.563
              QTNP(I,K)=0.                                                       GSPBL.564
              QWTNP(I,K)=0.                                                      GSPBL.565
              QCTNP(I,K)=0.                                                      GSPBL.566
              QCITNP(I,K)=0.                                                     GSPBL.567
            ENDDO                                                                GSPBL.568
          ENDDO                                                                  GSPBL.569
        ENDIF                                                                    GSPBL.570
C                                                                                GSPBL.571
C                                                                                GSPBL.572
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.573
C                                                                                GSPBL.574
C                                                                                GSPBL.575
C 2. CALCULATION OF PBL HEIGHT                                                   GSPBL.576
C                                                                                GSPBL.577
C                                                                                GSPBL.578
C     DESCRIPTION OF ALGORITHM:                                                  GSPBL.579
C                                                                                GSPBL.580
C     PBL HEIGHT IS DETERMINED BASED ON THE TKE PROFILE. IN GENERAL CASE         GSPBL.581
C     THE PBL HEIGHT IS THE HEIGHT WHERE THE TKE DROPS BELOW A CERTAIN           GSPBL.582
C     MINIMUM THRESHOLD. THIS THRESHOLD IS 0.1 M2/S2 IF THE MAXIMUM              GSPBL.583
C     TKE IS LARGER THAN 0.2 M2/S2, AND IT IS 0.5*(MAX TKE) WHEN                 GSPBL.584
C     THE MAXIMUM TKE IS SMALLER THAN 0.2 M2/S2.  IN CASES IN WHICH              GSPBL.585
C     THE TKE PROFILE IS WEAK, THE PBL HEIGHT IS SET TO THE HEIGHT OF            GSPBL.586
C     THE LOWEST FULL LAYER.                                                     GSPBL.587
C                                                                                GSPBL.588
C     ALGORITHM PARAMETERS:                                                      GSPBL.589
C                                                                                GSPBL.590
        PTKEMAX=0.20                                                             GSPBL.591
!(M2/S2) MAX TKE THRESHOLD. IF MAX TKE >                                         GSPBL.592
C                                PBL HEIGHT IS SET WHERE TKE=PTKEFRACT*P         GSPBL.593
        PTKEFRACT=0.50                                                           GSPBL.594
!FRACTION OF TKE MAX (OR PTKEMAX) THAT D                                         GSPBL.595
        PTKEWEAK=0.04                                                            GSPBL.596
!(M2/S2) TKE VALUE THAT DEFINES A WEAK T                                         GSPBL.597
        PTKEZMIN=150.0                                                           GSPBL.598
!(M) MINIMUM LAYER ABOVE THE SURFACE WHE                                         GSPBL.599
C                                THE TKE MAXIMUM IN THE PROFILE                  GSPBL.600
C                                                                                GSPBL.601
        DO I=1,ILX                                                               GSPBL.602
C                                                                                GSPBL.603
C     SET INITIAL VALUE OF KPBL(I)=-99                                           GSPBL.604
C                                                                                GSPBL.605
          KPBL(I)=-99                                                            GSPBL.606
C                                                                                GSPBL.607
C     DEFINITION OF THE LAYER IN WHICH TO LOOK FOR THE MAXIMUM TKE               GSPBL.608
C                                                                                GSPBL.609
          KTEMP1=KL                                                              GSPBL.610
          DO K=KL,2,-1                                                           GSPBL.611
            IF(TKEB(I,J,K-1).LT.TKEB(I,J,K))THEN                                 GSPBL.612
              KTEMP1=K-1                                                         GSPBL.613
              GOTO 3185                                                          GSPBL.614
            ENDIF                                                                GSPBL.615
          ENDDO                                                                  GSPBL.616
 3185     CONTINUE                                                               GSPBL.617
          KTEMP2=KL                                                              GSPBL.618
          DO K=KL,1,-1                                                           GSPBL.619
            IF(ZQ(I,K).GE.PTKEZMIN)THEN                                          GSPBL.620
              KTEMP2=K                                                           GSPBL.621
              GOTO 3186                                                          GSPBL.622
            ENDIF                                                                GSPBL.623
          ENDDO                                                                  GSPBL.624
 3186     CONTINUE                                                               GSPBL.625
          KTEMP=MIN(KTEMP1,KTEMP2)                                               GSPBL.626
C                                                                                GSPBL.627
C     SEARCH FOR THE MAXIMUM TKE BELOW KTEMP                                     GSPBL.628
C                                                                                GSPBL.629
          TKEBMAX=TKEB(I,J,KL)                                                   GSPBL.630
          DO K=KLM,KTEMP,-1                                                      GSPBL.631
            IF(TKEBMAX.LT.TKEB(I,J,K))TKEBMAX=TKEB(I,J,K)                        GSPBL.632
          ENDDO                                                                  GSPBL.633
C                                                                                GSPBL.634
C     DETERMINATION OF THE PBL HEIGHT                                            GSPBL.635
C                                                                                GSPBL.636
C     CASES OF WEAK TURBULENCE                                                   GSPBL.637
C                                                                                GSPBL.638
          IF(TKEBMAX.LT.PTKEWEAK.OR.TKEB(I,J,KL).LT.PTKEWEAK)THEN                GSPBL.639
            PBL(I,J)=ZQ(I,KL)                                                    GSPBL.640
            KPBL(I)=KL                                                           GSPBL.641
            GOTO 301                                                             GSPBL.642
          ELSE                                                                   GSPBL.643
C                                                                                GSPBL.644
C     CASES OF STRONGER TURBULENCE                                               GSPBL.645
C                                                                                GSPBL.646
            TKELIM=PTKEFRACT*MIN(TKEBMAX,PTKEMAX)                                GSPBL.647
            DO K=KLM,1,-1                                                        GSPBL.648
              IF(TKEB(I,J,K).LT.TKELIM.AND.TKEB(I,J,K).LT.TKEB(I,J,K+1))         GSPBL.649
     +          THEN                                                             GSPBL.650
                FRACT=(TKEB(I,J,K+1)-TKELIM)/(TKEB(I,J,K+1)-TKEB(I,J,K))         GSPBL.651
                IF(FRACT.GE.0.0.AND.FRACT.LE.1.0)THEN                            GSPBL.652
                  PBL(I,J)=ZQ(I,K+1)+FRACT*(ZQ(I,K)-ZQ(I,K+1))                   GSPBL.653
                  KPBL(I)=K                                                      GSPBL.654
                  GOTO 301                                                       GSPBL.655
                ENDIF                                                            GSPBL.656
              ELSEIF(ZQ(I,K).GT.5000.)THEN                                       GSPBL.657
C                      PRINT *,'PBL HT LIMITED TO 5 KM AGL AT ',I,J              25JAN00.629
                PBL(I,J)=5000.                                                   GSPBL.659
                KPBL(I)=K                                                        GSPBL.660
                GOTO 301                                                         GSPBL.661
              ENDIF                                                              GSPBL.662
            ENDDO                                                                GSPBL.663
            STOP 294                                                             GSPBL.664
          ENDIF                                                                  GSPBL.665
  301     CONTINUE                                                               GSPBL.666
C                                                                                GSPBL.667
C     IF KPBL WAS NOT FOUND THEN STOP                                            GSPBL.668
C                                                                                GSPBL.669
          IF(KPBL(I).LE.2)THEN                                                   GSPBL.670
            STOP '540 IN GSPBL'                                                  GSPBL.671
          ENDIF                                                                  GSPBL.672
        ENDDO                                                                    GSPBL.673
C                                                                                GSPBL.674
        IF(ISOIL.LE.1)THEN                                                       GSPBL.675
C                                                                                GSPBL.676
C     CALCULATION OF SURFACE LAYER PARAMETERS                                    GSPBL.677
          DO I=1,ILX                                                             GSPBL.678
            ZAMX=AMAX1(ZA(I,KL),10.)                                             GSPBL.679
            GZ1OZ0(I)=ALOG(ZAMX/ZNT(I,J))                                        GSPBL.680
            WSPD(I)=SQRT(UX(I,KL)*UX(I,KL)+VX(I,KL)*VX(I,KL))                    GSPBL.681
            TGBV=THGB(I)*(1.+XMOIST(IN)*EP1*QGG(I)*MAVAIL(I,J))                  07NOV00.1712
            GOVRTH(I)=G/THX(I,KL)                                                GSPBL.683
            DTHVDZ=(THVX(I,KL)-TGBV)                                             GSPBL.684
            IF(-DTHVDZ.GE.0.)THEN                                                GSPBL.685
              DTHVM=-DTHVDZ                                                      GSPBL.686
            ELSE                                                                 GSPBL.687
              DTHVM=0.0                                                          GSPBL.688
            ENDIF                                                                GSPBL.689
            VCONV=2.*SQRT(DTHVM)                                                 GSPBL.690
            WSPD(I)=SQRT(WSPD(I)*WSPD(I)+VCONV*VCONV)                            GSPBL.691
            WSPD(I)=AMAX1(WSPD(I),1.)                                            GSPBL.692
            BR(I)=GOVRTH(I)*ZA(I,KL)*DTHVDZ/(WSPD(I)*WSPD(I))                    GSPBL.693
          ENDDO                                                                  GSPBL.694
C                                                                                GSPBL.695
C     SURFACE LAYER PARAMETERS                                                   GSPBL.696
C                                                                                GSPBL.697
C     DIAGNOSE BASIC PARAMETERS FOR THE APPROPRIATE STABILITY CLASS:             GSPBL.698
C                                                                                GSPBL.699
C     THE STABILITY CLASSES ARE DETERMINED BY BR (BULK RICHARDSON NO.)           GSPBL.700
C     AND HOL (HEIGHT OF PBL/MONIN-OBUKHOV LENGTH).                              GSPBL.701
C                                                                                GSPBL.702
C     CRITERIA FOR THE CLASSES ARE AS FOLLOWS:                                   GSPBL.703
C                                                                                GSPBL.704
C        1. BR .GE. 0.2;                                                         GSPBL.705
C               REPRESENTS NIGHTTIME STABLE CONDITIONS (REGIME=1),               GSPBL.706
C                                                                                GSPBL.707
C        2. BR .LT. 0.2 .AND. BR .GT. 0.0;                                       GSPBL.708
C               REPRESENTS DAMPED MECHANICAL TURBULENT CONDITIONS                GSPBL.709
C               (REGIME=2),                                                      GSPBL.710
C                                                                                GSPBL.711
C        3. BR .LT. 0.0 .AND. HOL .LT. 1.5;                                      GSPBL.712
C               REPRESENTS FORCED CONVECTION CONDITIONS (REGIME=3),              GSPBL.713
C                                                                                GSPBL.714
C        4. BR .LT. 0.0 .AND. HOL .GT. 1.5;                                      GSPBL.715
C               REPRESENTS FREE CONVECTION CONDITIONS (REGIME=4).                GSPBL.716
C                                                                                GSPBL.717
          DO 320 I=1,ILX                                                         GSPBL.718
C                                                                                GSPBL.719
C     REGIME, PSIM AND PSIH DEPENDING ON STABILITY CLASS                         GSPBL.720
C                                                                                GSPBL.721
            IF(BR(I).LT.0..AND.HOL(I,J).GT.1.5)GOTO 310                          GSPBL.722
            IF(BR(I).LT.0.2)GOTO 270                                             GSPBL.723
C                                                                                GSPBL.724
C     CLASS 1; STABLE (NIGHTTIME) CONDITIONS:                                    GSPBL.725
C                                                                                GSPBL.726
            REGIME(I,J)=1.                                                       GSPBL.727
            PSIM(I)=-10.*GZ1OZ0(I)                                               19DEC02.1692
C    LOWER LIMIT ON PSI IN STABLE CONDITIONS                                     05DEC01.873
            PSIM(I)=AMAX1(PSIM(I),-10.)                                          05DEC01.874
            PSIH(I)=PSIM(I)                                                      19DEC02.1693
            GOTO 320                                                             GSPBL.730
C                                                                                GSPBL.731
  270       CONTINUE                                                             GSPBL.732
            IF(BR(I).LT.0.0)GOTO 280                                             GSPBL.733
C                                                                                GSPBL.734
C     CLASS 2; DAMPED MECHANICAL TURBULENCE:                                     GSPBL.735
C                                                                                GSPBL.736
            REGIME(I,J)=2.                                                       GSPBL.737
            PSIM(I)=-5.0*BR(I)*GZ1OZ0(I)/(1.1-5.0*BR(I))                         19DEC02.1694
C    LOWER LIMIT ON PSI IN STABLE CONDITIONS                                     05DEC01.875
            PSIM(I)=AMAX1(PSIM(I),-10.)                                          05DEC01.876
            PSIH(I)=PSIM(I)                                                      19DEC02.1695
            GOTO 320                                                             GSPBL.740
C                                                                                GSPBL.741
C     CLASS 3; FORCED CONVECTION:                                                GSPBL.742
C                                                                                GSPBL.743
  280       CONTINUE                                                             GSPBL.744
            REGIME(I,J)=3.                                                       GSPBL.745
            PSIM(I)=0.0                                                          GSPBL.746
            PSIH(I)=PSIM(I)                                                      GSPBL.747
            ZOL(I,J)=BR(I)*GZ1OZ0(I)                                             GSPBL.748
            GOTO 320                                                             GSPBL.749
C                                                                                GSPBL.750
C     CLASS 4; FREE CONVECTION:                                                  GSPBL.751
C                                                                                GSPBL.752
  310       CONTINUE                                                             GSPBL.753
            IF(THVX(I,KLM).GT.THVX(I,KL))GOTO 280                                GSPBL.754
            REGIME(I,J)=4.                                                       GSPBL.755
            ZOL(I,J)=BR(I)*GZ1OZ0(I)                                             GSPBL.756
            ZOL(I,J)=AMAX1(ZOL(I,J),-2.0)                                        GSPBL.757
            PSIM(I)=-ZOL(I,J)*(1.86+ZOL(I,J)*(1.07+0.249*ZOL(I,J)))              GSPBL.758
            PSIH(I)=-ZOL(I,J)*(3.23+ZOL(I,J)*(1.99+0.474*ZOL(I,J)))              GSPBL.759
  320     CONTINUE                                                               GSPBL.760
C                                                                                GSPBL.761
C CALCULATION OF HOL AND PBL, KPBL DEPENDING ON REGIME                           GSPBL.762
C                                                                                GSPBL.763
          DO 321 I=1,ILX                                                         GSPBL.764
            IF(REGIME(I,J).LE.1.)THEN                                            GSPBL.765
              PBL(I,J)=0.0                                                       GSPBL.766
              KPBL(I)=KL                                                         GSPBL.767
            ELSE                                                                 GSPBL.768
              PBL(I,J)=PBL(I,J)                                                  GSPBL.769
              KPBL(I)=KPBL(I)                                                    GSPBL.770
            ENDIF                                                                GSPBL.771
C                                                                                GSPBL.772
C     CALCULATION OF HOL FOR REGIMES 1,2,3                                       GSPBL.773
C     (FOR REGIME 4 IT IS DONE IN SECTION 11 IN GSPBL SUBROUTINE)                GSPBL.774
C                                                                                GSPBL.775
            IF(REGIME(I,J).LE.2.)HOL(I,J)=0.0                                    GSPBL.776
            IF(REGIME(I,J).EQ.3.)HOL(I,J)=-ZOL(I,J)*PBL(I,J)/ZA(I,KL)            GSPBL.777
  321     CONTINUE                                                               GSPBL.778
#ifdef IPLACE                                                                    GSPBL.779
        ELSEIF(ISOIL.EQ.3)THEN                                                   GSPBL.780
# ifndef MPP1                                                                    25JAN00.630
          CALL SURFPLACE(J,IN)                                                   GSPBL.781
# else                                                                           25JAN00.631
          WRITE(0,*)' MPP VERSION NOT READY FOR THIS OPTION '                    25JAN00.632
          WRITE(*,*)' MPP VERSION NOT READY FOR THIS OPTION '                    25JAN00.633
          CALL FKILL_MODEL( 'GSPBL.781' )                                        25JAN00.634
# endif                                                                          25JAN00.635
#endif                                                                           GSPBL.782
        ENDIF                                                                    GSPBL.783
C                                                                                GSPBL.784
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.785
C                                                                                GSPBL.786
C                                                                                GSPBL.787
C 3. OTHER CALCULATIONS                                                          GSPBL.788
C                                                                                GSPBL.789
C     VERTICAL DERIVATIVES                                                       GSPBL.790
C                                                                                GSPBL.791
C     THIS LOOP CALCULATES VARIOUS VERTICAL DERIVATIVES AT THE FULL              GSPBL.792
C     SIGMA LEVELS.                                                              GSPBL.793
C                                                                                GSPBL.794
C     DUDZ AND DVDZ ARE THE VERTICAL DERIVATIVES OF U AND V WIND.                GSPBL.795
C     SVS IS THE SQUARE OF THE VERTICAL WIND SHEAR.                              GSPBL.796
C     DTVDZ IS THE VERTICAL GRADIENT OF THETA V.                                 GSPBL.797
C     DTLDZ IS THE VERTICAL GRADIENT OF THETA L.                                 GSPBL.798
C     DQWDZ IS THE VERTICAL GRADIENT OF TOTAL WATER.                             GSPBL.799
C                                                                                GSPBL.800
        DO I=1,ILX                                                               GSPBL.801
          DO K=2,KL                                                              GSPBL.802
            DUDZ(I,K)=(UX(I,K-1)-UX(I,K))/DZA(I,K-1)                             GSPBL.803
            DVDZ(I,K)=(VX(I,K-1)-VX(I,K))/DZA(I,K-1)                             GSPBL.804
            SVS(I,K)=(DUDZ(I,K)*DUDZ(I,K))+(DVDZ(I,K)*DVDZ(I,K))                 GSPBL.805
C                                                                                GSPBL.806
C     PREVENT NON-ZERO SVS.  IM NOT SURE WHAT MINIMUM VALUE                      GSPBL.807
C     TO ASSIGN TO SVS.                                                          GSPBL.808
C                                                                                GSPBL.809
            SVS(I,K)=SVS(I,K)+1.E-9                                              GSPBL.810
            DTVDZ(I,K)=(THVX(I,K-1)-THVX(I,K))/DZA(I,K-1)                        GSPBL.811
            DTLDZ(I,K)=(THLX(I,K-1)-THLX(I,K))/DZA(I,K-1)                        GSPBL.812
            DQWDZ(I,K)=(QWX(I,K-1)-QWX(I,K))/DZA(I,K-1)                          GSPBL.813
          ENDDO                                                                  GSPBL.814
        ENDDO                                                                    GSPBL.815
C                                                                                GSPBL.816
C     CALCULATE THE RICHARDSON NUMBER (RICHN) AND GOVTHV                         GSPBL.817
C     AT THE FULL LEVELS.                                                        GSPBL.818
C                                                                                GSPBL.819
        DO I=1,ILX                                                               GSPBL.820
          DO K=2,KL                                                              GSPBL.821
C                                                                                GSPBL.822
C     LINEARLY INTERPOLATE THETA TO THE FULL LEVELS.                             GSPBL.823
C                                                                                GSPBL.824
            FRCTN=(ZQ(I,K)-ZA(I,K))/DZA(I,K-1)                                   GSPBL.825
            THXFL=THX(I,K)+FRCTN*(THX(I,K-1)-THX(I,K))                           GSPBL.826
C                                                                                GSPBL.827
C     CALC THE RICHARDSON NUMBER ACCORDING TO EQ 3.4.4                           GSPBL.828
C     IN NCAR TECHNICAL NOTE.                                                    GSPBL.829
C                                                                                GSPBL.830
            GOVTH=G/THXFL                                                        GSPBL.831
            DTHVDZ=(THVX(I,K-1)-THVX(I,K))/DZA(I,K-1)                            GSPBL.832
            RICHN(I,K)=GOVTH*DTHVDZ/SVS(I,K)                                     GSPBL.833
C                                                                                GSPBL.834
C     CALC G OVER THETAV AT THE FULL LEVELS FOR USE LATER AS A PART              GSPBL.835
C     OF THE BOUYANCY TERM OF THE TKE EQUATION.                                  GSPBL.836
C     FIRST, LINEARLY INTERPOLATE THETAV TO THE FULL LEVELS.                     GSPBL.837
C                                                                                GSPBL.838
            THVXFL=THVX(I,K)+FRCTN*(THVX(I,K-1)-THVX(I,K))                       GSPBL.839
            GOVTHV(I,K)=G/THVXFL                                                 GSPBL.840
          ENDDO                                                                  GSPBL.841
        ENDDO                                                                    GSPBL.842
C                                                                                GSPBL.843
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.844
C                                                                                GSPBL.845
C                                                                                GSPBL.846
C 4. COMPUTATION OF BLACKADAR BASIC LENGTH SCALE                                 GSPBL.847
C                                                                                GSPBL.848
C     CALCULATE LAMBDA FOR FULL LEVELS ABOVE THE BOUNDARY LAYER.                 GSPBL.849
C     LAMBDA IS ONE PART OF THE BLACKADAR BASIC LENGTH SCALE.                    GSPBL.850
C     SEE B91 PAGE 2108.                                                         GSPBL.851
C                                                                                GSPBL.852
        IF(ILAMBDA.EQ.1)THEN                                                     GSPBL.853
          DO I=1,ILX                                                             GSPBL.854
            DEPTH=0.0                                                            GSPBL.855
            ICOUNT=0                                                             GSPBL.856
C                                                                                GSPBL.857
C     STARTING AT THE NEXT TO HIGHEST FULL SIGMA LEVEL, SEARCH DOWN FOR          GSPBL.858
C     TURBULENT LAYERS, IE, WHERE RICHARDSON NUMBER IS LESS THAN THE             GSPBL.859
C     CRITICAL RICH. NUMBER.                                                     GSPBL.860
C                                                                                GSPBL.861
            DO K=2,KPBL(I)                                                       GSPBL.862
C                                                                                GSPBL.863
C     AS WE ENCOUNTER TURBULENT LAYERS, KEEP A RUNNING TAB OF                    GSPBL.864
C     ITS TOTAL DEPTH.                                                           GSPBL.865
C                                                                                GSPBL.866
              IF(RICHN(I,K).LT.CRIC(I,K))THEN                                    GSPBL.867
                DEPTH=DEPTH+DZA(I,K-1)                                           GSPBL.868
                ICOUNT=ICOUNT+1                                                  GSPBL.869
C                                                                                GSPBL.870
C     ONCE WEVE REACHED THE BOTTOM OF THE FREE ATMOSPHERE, CALCULATE             GSPBL.871
C     LAMBDA.  NOTE HOW THE VALUE OF KB AND KE NOW DIFFER.                       GSPBL.872
C                                                                                GSPBL.873
                IF(K.EQ.KPBL(I))THEN                                             GSPBL.874
                  KB=K+1-ICOUNT                                                  GSPBL.875
                  KE=K                                                           GSPBL.876
                  DO KK=KB,KE                                                    GSPBL.877
                    LAMBDA(I,KK)=0.3333333*DEPTH                                 GSPBL.878
                    IF(LAMBDA(I,KK).LT.DZA(I,KK-1))THEN                          GSPBL.879
                      LAMBDA(I,KK)=DZA(I,KK-1)                                   GSPBL.880
                    ENDIF                                                        GSPBL.881
                  ENDDO                                                          GSPBL.882
                  DEPTH=0.0                                                      GSPBL.883
                  ICOUNT=0                                                       GSPBL.884
                ENDIF                                                            GSPBL.885
              ELSE                                                               GSPBL.886
C                                                                                GSPBL.887
C     OTHERWISE WE HAVE A STABLE LAYER.  IF ICOUNT IS GREATER THAN ONE           GSPBL.888
C     A TURBULENT LAYER EXISTS ABOVE.  THE TOP AND BOTTOM OF THIS LAYER          GSPBL.889
C     IS KB AND KE RESPECTIVELY.                                                 GSPBL.890
C                                                                                GSPBL.891
                IF(ICOUNT.GT.0)THEN                                              GSPBL.892
                  KB=K-ICOUNT                                                    GSPBL.893
                  KE=K-1                                                         GSPBL.894
                  DO KK=KB,KE                                                    GSPBL.895
                    LAMBDA(I,KK)=0.3333333*DEPTH                                 GSPBL.896
                    IF(LAMBDA(I,KK).LT.DZA(I,KK-1))THEN                          GSPBL.897
                      LAMBDA(I,KK)=DZA(I,KK-1)                                   GSPBL.898
                    ENDIF                                                        GSPBL.899
                  ENDDO                                                          GSPBL.900
                  DEPTH=0.0                                                      GSPBL.901
                  ICOUNT=0                                                       GSPBL.902
                ENDIF                                                            GSPBL.903
C                                                                                GSPBL.904
C     CALCULATE LAMBDA FOR THE STABLE LAYER AT THE CURRENT K,                    GSPBL.905
C     WHICH EQUALS THE MODEL LAYER THICKNESS.                                    GSPBL.906
C                                                                                GSPBL.907
                LAMBDA(I,K)=DZA(I,K-1)                                           GSPBL.908
              ENDIF                                                              GSPBL.909
            ENDDO                                                                GSPBL.910
          ENDDO                                                                  GSPBL.911
C                                                                                GSPBL.912
C     CALCULATE LAMBDA IN THE BOUNDARY LAYER.                                    GSPBL.913
C                                                                                GSPBL.914
C     A BOUNDARY LAYER EXISTS WHEN KPBL IS LESS THAN KL.                         GSPBL.915
C     NOTE: SINCE PBL HEIGHTS ARE DETERMINED BY INTERPOLATING                    GSPBL.916
C     BETWEEN THE HALF LEVELS,  FULL LEVEL KPBL+1 IS NOT NECESSARILY             GSPBL.917
C     LOCATED WITHIN THE BOUNDARY LAYER.  HOWEVER, FOR                           GSPBL.918
C     SIMPLICITY IT WILL BE CONSIDERED WITHIN THE PBL.                           GSPBL.919
C                                                                                GSPBL.920
          DO I=1,ILX                                                             GSPBL.921
            IF(KPBL(I).LT.KL)THEN                                                GSPBL.922
              DEPTH=0.3333333*PBL(I,J)                                           GSPBL.923
C                                                                                GSPBL.924
C     SINCE SIGMA LEVEL KPBL(I) IS ABOVE THE BOUNDARY LAYER,                     GSPBL.925
C     ADD ONE TO IT TO OBTAIN FIRST LOOP BOUND.                                  GSPBL.926
C                                                                                GSPBL.927
              DO K=(KPBL(I)+1),KL                                                GSPBL.928
                IF(DEPTH.LT.DZA(I,K-1))THEN                                      GSPBL.929
                  LAMBDA(I,K)=DZA(I,K-1)                                         GSPBL.930
                ELSE                                                             GSPBL.931
                  LAMBDA(I,K)=DEPTH                                              GSPBL.932
                ENDIF                                                            GSPBL.933
              ENDDO                                                              GSPBL.934
            ENDIF                                                                GSPBL.935
          ENDDO                                                                  GSPBL.936
        ENDIF                                                                    GSPBL.937
C                                                                                GSPBL.938
C     ALTERNATIVE ALGORITHM FOR CALCULATING LAMBDA                               GSPBL.939
C                                                                                GSPBL.940
C     IN THIS ALGORITHM THE TKE PROFILE IS USED FOR DETERMINING LAMBDA.          GSPBL.941
C     IF THE TKE IN THE LEVEL IS LESS THAN TKELMIN LAMBDA                        GSPBL.942
C     IS SET TO A MINIMUM VALUE XLMIN.                                           GSPBL.943
C     IF THE TKE IS LARGER THAN TKELMIN THEN  THE DEPTH OF THE                   GSPBL.944
C     TURBULENT LAYER IS SOUGHT.  THE DEPTH OF THE TURBULENT                     GSPBL.945
C     LAYER IS DEFINED BASED ON THE TKE PROFILE. IN GENERAL THE                  GSPBL.946
C     UPPER AND LOWER LIMITS OF THE TURBULENT LAYER WILL BE WHERE                GSPBL.947
C     THE TKE DECREASES BELOW TKELMIN.   HOWEVER IF THE TKE                      GSPBL.948
C     PROFILE HAS A LOCAL MINIMUM, THIS MINIMUM IS COMPARED                      GSPBL.949
C     AGAINST THE ADJACENT MAXIMA.  IF THE TKE IN THE LOCAL                      GSPBL.950
C     MINIMUM IS LOWER THAN XFR TIMES THE MAXIMUM, THEN THE                      GSPBL.951
C     LOCATION OF THE MINIMUM DEFINES THE BOUNDARY OF THE                        GSPBL.952
C     TURBULENT LAYER.                                                           GSPBL.953
C                                                                                GSPBL.954
        IF(ILAMBDA.EQ.2)THEN                                                     GSPBL.955
          TKELMIN=0.005                                                          GSPBL.956
!MINIMUM TKE TO DEFINE TURBULENT LAYER                                           GSPBL.957
          XLMIN=-999.0                                                           GSPBL.958
!MINIMUM LAMBDA                                                                  GSPBL.959
          XFR=0.1                                                                GSPBL.960
!FRACTION OF TURBULENT LAYER TO BE CONSIDER                                      GSPBL.961
          FRACMINLIM=0.5                                                         GSPBL.962
!FRACTION OF LOCAL MINIMUM WITH RESPECT TO                                       GSPBL.963
          DO I=1,ILX                                                             GSPBL.964
            NOVERMIN=0                                                           GSPBL.965
            IF(TKEB(I,J,1).GT.TKELMIN)NOVERMIN=1                                 GSPBL.966
            NTKELYR(1)=1                                                         GSPBL.967
            MINIMUM(1)=0                                                         GSPBL.968
            DO K=2,KL                                                            GSPBL.969
              NTKELYR(K)=NTKELYR(K-1)                                            GSPBL.970
              MINIMUM(K)=0                                                       GSPBL.971
              IF(MINIMUM(K-1).EQ.1)THEN                                          GSPBL.972
                NTKELYR(K)=NTKELYR(K)+1                                          GSPBL.973
              ELSEIF(NOVERMIN.EQ.0.AND.TKEB(I,J,K).GT.TKELMIN)THEN               GSPBL.974
                NOVERMIN=1                                                       GSPBL.975
                NTKELYR(K)=NTKELYR(K)+1                                          GSPBL.976
              ELSEIF(NOVERMIN.EQ.1.AND.TKEB(I,J,K).LE.TKELMIN)THEN               GSPBL.977
                NOVERMIN=0                                                       GSPBL.978
                NTKELYR(K)=NTKELYR(K)+1                                          GSPBL.979
              ELSEIF(NOVERMIN.EQ.1.AND.TKEB(I,J,K).LT.TKEB(I,J,K-1).AND.         GSPBL.980
     +              TKEB(I,J,K).LT.TKEB(I,J,K+1))THEN                            GSPBL.981
                TKEMAXUP=TKEB(I,J,1)                                             GSPBL.982
                DO KK=K-1,2,-1                                                   GSPBL.983
                  IF(TKEB(I,J,KK-1).LE.TKEB(I,J,KK))THEN                         GSPBL.984
                    TKEMAXUP=TKEB(I,J,KK)                                        GSPBL.985
                    GOTO 7180                                                    GSPBL.986
                  ENDIF                                                          GSPBL.987
                ENDDO                                                            GSPBL.988
 7180           CONTINUE                                                         GSPBL.989
                TKEMAXDW=TKEB(I,J,KL)                                            GSPBL.990
                DO KK=K+1,KL-1                                                   GSPBL.991
                  IF(TKEB(I,J,KK+1).LE.TKEB(I,J,KK))THEN                         GSPBL.992
                    TKEMAXDW=TKEB(I,J,KK)                                        GSPBL.993
                    GOTO 7182                                                    GSPBL.994
                  ENDIF                                                          GSPBL.995
                ENDDO                                                            GSPBL.996
 7182           CONTINUE                                                         GSPBL.997
                FRACMIN=TKEB(I,J,K)/MIN(TKEMAXUP,TKEMAXDW)                       GSPBL.998
                IF(FRACMIN.LT.FRACMINLIM)THEN                                    GSPBL.999
                  NTKELYR(K)=NTKELYR(K)+1                                        GSPBL.1000
                  MINIMUM(K)=1                                                   GSPBL.1001
                ENDIF                                                            GSPBL.1002
              ENDIF                                                              GSPBL.1003
            ENDDO                                                                GSPBL.1004
            DO K=2,KL                                                            GSPBL.1005
              IF(TKEB(I,J,K).LE.TKELMIN)THEN                                     GSPBL.1006
                LAMBDA(I,K)=XLMIN                                                GSPBL.1007
              ELSEIF(NTKELYR(K).EQ.NTKELYR(K-1).AND.K.NE.2)THEN                  GSPBL.1008
                LAMBDA(I,K)=LAMBDA(I,K-1)                                        GSPBL.1009
              ELSEIF(MINIMUM(K).EQ.0)THEN                                        GSPBL.1010
                ZUPP=ZQ(I,1)                                                     GSPBL.1011
                IF(K.NE.2)THEN                                                   GSPBL.1012
                  IF(MINIMUM(K-1).EQ.1)THEN                                      GSPBL.1013
                    ZUPP=ZQ(I,K-1)                                               GSPBL.1014
                  ELSE                                                           GSPBL.1015
                    FRACT=(TKELMIN-TKEB(I,J,K))/(TKEB(I,J,K-1)-                  GSPBL.1016
     +                    TKEB(I,J,K))                                           GSPBL.1017
                    IF(FRACT.GT.1.0.OR.FRACT.LT.0.0)THEN                         GSPBL.1018
                      STOP 2095                                                  GSPBL.1019
                    ENDIF                                                        GSPBL.1020
                    ZUPP=ZQ(I,K)+FRACT*(ZQ(I,K-1)-ZQ(I,K))                       GSPBL.1021
                  ENDIF                                                          GSPBL.1022
                ENDIF                                                            GSPBL.1023
                ZDW=0.0                                                          GSPBL.1024
                DO KK=K+1,KL                                                     GSPBL.1025
                  IF(MINIMUM(KK).EQ.1)THEN                                       GSPBL.1026
                    ZDW=ZQ(I,KK)                                                 GSPBL.1027
                    GOTO 7184                                                    GSPBL.1028
                  ENDIF                                                          GSPBL.1029
                  IF(TKEB(I,J,KK).LT.TKELMIN)THEN                                GSPBL.1030
                    FRACT=(TKELMIN-TKEB(I,J,KK))/(TKEB(I,J,KK-1)-                GSPBL.1031
     +                    TKEB(I,J,KK))                                          GSPBL.1032
                    IF(FRACT.GT.1.0.OR.FRACT.LT.0.0)STOP 2096                    GSPBL.1033
                    ZDW=ZQ(I,KK)+FRACT*(ZQ(I,KK-1)-ZQ(I,KK))                     GSPBL.1034
                    GOTO 7184                                                    GSPBL.1035
                  ENDIF                                                          GSPBL.1036
                ENDDO                                                            GSPBL.1037
 7184           CONTINUE                                                         GSPBL.1038
                DEPTH=XFR*(ZUPP-ZDW)                                             GSPBL.1039
                LAMBDA(I,K)=DEPTH                                                GSPBL.1040
              ENDIF                                                              GSPBL.1041
            ENDDO                                                                GSPBL.1042
            DO K=2,KL                                                            GSPBL.1043
              IF(MINIMUM(K).EQ.1)THEN                                            GSPBL.1044
                IF(K.NE.2)THEN                                                   GSPBL.1045
                  LAMBDA(I,K)=0.5*(LAMBDA(I,K-1)+LAMBDA(I,K+1))                  GSPBL.1046
                  LAMBDA(I,K)=LAMBDA(I,K)                                        GSPBL.1047
                ELSE                                                             GSPBL.1048
                  LAMBDA(I,K)=LAMBDA(I,K+1)                                      GSPBL.1049
                ENDIF                                                            GSPBL.1050
              ENDIF                                                              GSPBL.1051
            ENDDO                                                                GSPBL.1052
          ENDDO                                                                  GSPBL.1053
        ENDIF                                                                    GSPBL.1054
C                                                                                GSPBL.1055
C     CALCULATE THE BLACKADAR BASIC LENGTH SCALE ACCORDING TO B91 PAGE           GSPBL.1056
C     2108 BOTTOM LEFT COLUMN.  RESULT IS AT THE FULL LEVELS.                    GSPBL.1057
C                                                                                GSPBL.1058
        DO I=1,ILX                                                               GSPBL.1059
          DO K=2,KL                                                              GSPBL.1060
            BBLS(I,K)=(KARMAN*ZQ(I,K)*LAMBDA(I,K))/(KARMAN*ZQ(I,K)+              GSPBL.1061
     +                LAMBDA(I,K))                                               GSPBL.1062
          ENDDO                                                                  GSPBL.1063
        ENDDO                                                                    GSPBL.1064
C                                                                                GSPBL.1065
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.1066
C                                                                                GSPBL.1067
C                                                                                GSPBL.1068
C 5. COMPUTATION OF BETABT, BETABW AND NSQUAR                                    GSPBL.1069
C                                                                                GSPBL.1070
C     COMPUTATION OF BETABT AND BETABW                                           GSPBL.1071
C                                                                                GSPBL.1072
C     CALCULATE A SIMPLIFIED FORM OF BETA BAR T AND BETA BAR W                   GSPBL.1073
C     ACCORDING TO B91 TOP LEFT OF PAGE 2108.                                    GSPBL.1074
C     RESULT IS AT THE HALF LEVELS.                                              GSPBL.1075
C                                                                                GSPBL.1076
        DO I=1,ILX                                                               GSPBL.1077
          DO K=1,KL                                                              GSPBL.1078
            BETAW=(EP1*G)/(1.0+EP1*QX(I,K)-QCX(I,K))                             GSPBL.1079
            BETAT=G/THX(I,K)                                                     GSPBL.1080
            IF(QCX(I,K).LE.QCMIN)THEN                                            GSPBL.1081
              BETABT(I,K)=BETAT                                                  GSPBL.1082
              BETABW(I,K)=BETAW                                                  GSPBL.1083
            ELSE                                                                 GSPBL.1084
              EXNER=SCR3(I,K)/THX(I,K)                                           GSPBL.1085
              EVAP=QX(I,K)*PRESHL(I,K)/(EP1+QX(I,K))                             GSPBL.1086
              ALFAL=0.622*XLVP(I,K)*QX(I,K)*PRESHL(I,K)/(R*SCR3(I,K)*            GSPBL.1087
     +              SCR3(I,K)*(PRESHL(I,K)-0.378*EVAP))                          GSPBL.1088
              ABARL=1./(1.+XLVP(I,K)*ALFAL/CPM(I,K))                             GSPBL.1089
              BBARL=ALFAL*EXNER                                                  GSPBL.1090
              BETAL=XLVP(I,K)*BETAT/(CPM(I,K)*EXNER)-BETAW/0.378                 GSPBL.1091
              BETABT(I,K)=BETAT-ABARL*BBARL*BETAL                                GSPBL.1092
              BETABW(I,K)=BETAW+ABARL*BETAL                                      GSPBL.1093
            ENDIF                                                                GSPBL.1094
          ENDDO                                                                  GSPBL.1095
        ENDDO                                                                    GSPBL.1096
C                                                                                GSPBL.1097
C     COMPUTATION OF N SQUARED                                                   GSPBL.1098
C                                                                                GSPBL.1099
C     CALCULATE A SIMPLIFIED FORM OF N SQUARED AFTER B-91 PAGE 2108 EQ 6         GSPBL.1100
C     RESULT IS AT THE FULL LEVELS.                                              GSPBL.1101
C                                                                                GSPBL.1102
        DO I=1,ILX                                                               GSPBL.1103
          DO K=2,KL                                                              GSPBL.1104
C                                                                                GSPBL.1105
C     LINEARLY INTERPOLATE BETA BAR T TO THE FULL LEVELS.                        GSPBL.1106
C                                                                                GSPBL.1107
            INDSATUP=0                                                           GSPBL.1108
            INDSATDW=0                                                           GSPBL.1109
            INDTRANS=0                                                           GSPBL.1110
            IF(QCX(I,K-1).GT.QCMIN)INDSATUP=1                                    GSPBL.1111
            IF(QCX(I,K).GT.QCMIN)INDSATDW=1                                      GSPBL.1112
            IF(INDSATUP.EQ.INDSATDW)THEN                                         GSPBL.1113
              FRCTN=(ZQ(I,K)-ZA(I,K))/DZA(I,K-1)                                 GSPBL.1114
              BBTFL=BETABT(I,K)+FRCTN*(BETABT(I,K-1)-BETABT(I,K))                GSPBL.1115
              BBWFL=BETABW(I,K)+FRCTN*(BETABW(I,K-1)-BETABW(I,K))                GSPBL.1116
            ELSE                                                                 GSPBL.1117
              INDTRANS=1                                                         GSPBL.1118
              IF(INDSATUP.EQ.0)THEN                                              GSPBL.1119
                BBTFL=BETABT(I,K-1)                                              GSPBL.1120
                BBWFL=BETABW(I,K-1)                                              GSPBL.1121
              ELSE                                                               GSPBL.1122
                BBTFL=BETABT(I,K)                                                GSPBL.1123
                BBWFL=BETABW(I,K)                                                GSPBL.1124
              ENDIF                                                              GSPBL.1125
            ENDIF                                                                GSPBL.1126
            IF(INDTRANS.EQ.0.AND.ITKESAT.EQ.1)THEN                               GSPBL.1127
              NSQUAR(I,K)=BBTFL*DTLDZ(I,K)+BBWFL*DQWDZ(I,K)                      GSPBL.1128
            ELSE                                                                 GSPBL.1129
              NSQUAR(I,K)=GOVTHV(I,K)*DTVDZ(I,K)                                 GSPBL.1130
            ENDIF                                                                GSPBL.1131
C                                                                                GSPBL.1132
C     DURING UNSTABLE CONDITIONS, SET NSQUAR EQUAL TO ZERO.                      GSPBL.1133
C     OTHERWISE WE TAKE THE SQUARE ROOT OF A NEG NUMBER LATER.                   GSPBL.1134
C                                                                                GSPBL.1135
            IF(NSQUAR(I,K).LT.0.0)THEN                                           GSPBL.1136
              NSQUAR(I,K)=0.0                                                    GSPBL.1137
            ENDIF                                                                GSPBL.1138
          ENDDO                                                                  GSPBL.1139
        ENDDO                                                                    GSPBL.1140
C                                                                                GSPBL.1141
C     THE FOLLOWING PARAMETERS DEFINE LIMITS TO THE BBLS.                        GSPBL.1142
C                                                                                GSPBL.1143
        XLMIN1=2.0                                                               GSPBL.1144
!MINIMUM LIMIT IN TURBULENT REGIONS                                              GSPBL.1145
        XLMIN2=10.0                                                              GSPBL.1146
!VALUE TO USE IN UNSTABLE NON-TURBULENT REGIONS                                  GSPBL.1147
        XLMAX=100.0                                                              GSPBL.1148
!MAXIMUM VALUE IN TURBULENT REGIONS                                              GSPBL.1149
C                                                                                GSPBL.1150
        DO I=1,ILX                                                               GSPBL.1151
          DO K=2,KL                                                              GSPBL.1152
            IF(LAMBDA(I,K).NE.-999.)THEN                                         GSPBL.1153
!TURBULENT REGIO                                                                 GSPBL.1154
              BBLS(I,K)=MAX(BBLS(I,K),XLMIN1)                                    GSPBL.1155
              IF(NSQUAR(I,K).GT.0.)THEN                                          GSPBL.1156
!STABLE CONDI                                                                    GSPBL.1157
                XLSTAB=0.36*SQRT(TKEB(I,J,K)/NSQUAR(I,K))                        GSPBL.1158
                BBLS(I,K)=MIN(XLSTAB,BBLS(I,K))                                  GSPBL.1159
              ELSE                                                               GSPBL.1160
!UNSTABLE CON                                                                    GSPBL.1161
                BBLS(I,K)=BBLS(I,K)                                              GSPBL.1162
              ENDIF                                                              GSPBL.1163
              BBLS(I,K)=MIN(BBLS(I,K),XLMAX)                                     GSPBL.1164
            ELSE                                                                 GSPBL.1165
!NON-TURBULENT R                                                                 GSPBL.1166
              IF(NSQUAR(I,K).GT.0.)THEN                                          GSPBL.1167
!STABLE CONDI                                                                    GSPBL.1168
                BBLS(I,K)=0.36*SQRT(TKEB(I,J,K)/NSQUAR(I,K))                     GSPBL.1169
              ELSE                                                               GSPBL.1170
!UNSTABLE CON                                                                    GSPBL.1171
                BBLS(I,K)=XLMIN2                                                 GSPBL.1172
              ENDIF                                                              GSPBL.1173
            ENDIF                                                                GSPBL.1174
          ENDDO                                                                  GSPBL.1175
        ENDDO                                                                    GSPBL.1176
C                                                                                GSPBL.1177
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.1178
C                                                                                GSPBL.1179
C                                                                                GSPBL.1180
C 6. CALCULATE EDDY DIFFUSIVITIES FOR MOMENTUM, THL, QX AND TKE                  GSPBL.1181
C                                                                                GSPBL.1182
C     CALCULATE THE DISSIPATION TIME SCALE (TAU0) ACCORDING                      GSPBL.1183
C     TO B91 EQ 14 P 2108.                                                       GSPBL.1184
C                                                                                GSPBL.1185
        DO I=1,ILX                                                               GSPBL.1186
          DO K=2,KL                                                              GSPBL.1187
            TAU0(I,K)=(CZERO*BBLS(I,K))/                                         GSPBL.1188
     +                SQRT(TKEB(I,J,K)+FSUBL*BBLS(I,K)*BBLS(I,K)*                GSPBL.1189
     +                NSQUAR(I,K))                                               GSPBL.1190
          ENDDO                                                                  GSPBL.1191
        ENDDO                                                                    GSPBL.1192
C                                                                                GSPBL.1193
C     CALCULATE MOMENTUM, THL AND QX DIFFUSIVITIES                               GSPBL.1194
C                                                                                GSPBL.1195
C     CALCULATE EDDY DIFFUSIVITIES ACCORDING TO B91.                             GSPBL.1196
C     RESULT IS AT FULL LEVELS.                                                  GSPBL.1197
C                                                                                GSPBL.1198
        DO I=1,ILX                                                               GSPBL.1199
          DO K=2,KL                                                              GSPBL.1200
C                                                                                GSPBL.1201
C     CALCULATE THE LENGTH SCALES FOR MOMENTUM, LSUBM, AND LENGTH SCALES         GSPBL.1202
C     FOR THETA AND MOISTURE, LSUBH.  LSUBH IS INTERNAL TO THIS LOOP.            GSPBL.1203
C     THEREFORE, IT DOES NOT NEED TO BE DIMENSIONED IF ONE WANTS TO              GSPBL.1204
C     SAVE SOME MEMORY.                                                          GSPBL.1205
C                                                                                GSPBL.1206
            TOP=(0.9422*SQRT(TKEB(I,J,K))*BBLS(I,K))*                            GSPBL.1207
     +          SQRT(TKEB(I,J,K)+FSUBL*BBLS(I,K)*BBLS(I,K)*NSQUAR(I,K))          GSPBL.1208
            TOP2=TKEB(I,J,K)+FSUB2*BBLS(I,K)*BBLS(I,K)*NSQUAR(I,K)               GSPBL.1209
            BOT=TKEB(I,J,K)+FSUB1*BBLS(I,K)*BBLS(I,K)*NSQUAR(I,K)                GSPBL.1210
            LSUBH(I,K)=TOP/(TKEB(I,J,K)+(FSUB3*BBLS(I,K)*BBLS(I,K)*              GSPBL.1211
     +                 NSQUAR(I,K))+(FSUB0*BBLS(I,K)*BBLS(I,K)*SVS(I,K)*         GSPBL.1212
     +                 TOP2/BOT))                                                GSPBL.1213
            LSUBM(I,K)=LSUBH(I,K)*0.7407*(TOP2/BOT)                              GSPBL.1214
C                                                                                GSPBL.1215
C     CALCULATE EDDY DIFFUSIVITIES FOR MOMENTUM, KZM, AND THETA, KTH.            GSPBL.1216
C                                                                                GSPBL.1217
            KZM(I,K)=LSUBM(I,K)*SQRT(TKEB(I,J,K))                                GSPBL.1218
            KTH(I,K)=LSUBH(I,K)*SQRT(TKEB(I,J,K))                                GSPBL.1219
C                                                                                GSPBL.1220
C     RESTRICT LOWEST VALUE OF KZM BY KZOGS.                                     GSPBL.1221
C                                                                                GSPBL.1222
            IF(XLAND(I,J).GE.1.5)THEN                                            GSPBL.1223
C                                                                                GSPBL.1224
C     USE CONSTANT KZOW OVER WATER                                               GSPBL.1225
C                                                                                GSPBL.1226
              KZM(I,K)=AMAX1(KZM(I,K),KZOW)                                      GSPBL.1227
C                                                                                GSPBL.1228
C     RESTRICT LOWEST VALUE OF KTH BY KZO, MODIFIED BY THE RATIO OF              GSPBL.1229
C     THE MIXING LENGTHS, LSUBH AND LSUBM.                                       GSPBL.1230
C                                                                                GSPBL.1231
              KTH(I,K)=AMAX1(KTH(I,K),(LSUBH(I,K)/LSUBM(I,K)*KZOW))              GSPBL.1232
            ELSE                                                                 GSPBL.1233
C                                                                                GSPBL.1234
C     USE KZO PROFILE OVER LAND                                                  GSPBL.1235
C                                                                                GSPBL.1236
              KZM(I,K)=AMAX1(KZM(I,K),KZOGS(K))                                  GSPBL.1237
C                                                                                GSPBL.1238
C     RESTRICT LOWEST VALUE OF KTH BY KZOGS, MODIFIED BY THE RATIO OF            GSPBL.1239
C     THE MIXING LENGTHS, LSUBH AND LSUBM.                                       GSPBL.1240
C                                                                                GSPBL.1241
              KTH(I,K)=AMAX1(KTH(I,K),(LSUBH(I,K)/LSUBM(I,K)*KZOGS(K)))          GSPBL.1242
            ENDIF                                                                GSPBL.1243
C                                                                                GSPBL.1244
C     THE EDDY DIFFUSIVITY FOR MOISTURE IS THE SAME AS THAT FOR THETA.           GSPBL.1245
C                                                                                GSPBL.1246
            KQ(I,K)=KTH(I,K)                                                     GSPBL.1247
          ENDDO                                                                  GSPBL.1248
C                                                                                GSPBL.1249
C     CALCULATE DIFFUSIVITIES FOR TKE EQUATION                                   GSPBL.1250
C                                                                                GSPBL.1251
C     DETERMINE REGION WHERE TO USE KBT AND KCT TERMS IN TKE EQUATION            GSPBL.1252
C                                                                                GSPBL.1253
          IF(KPBL(I).EQ.KL)THEN                                                  GSPBL.1254
            KLIM=KXP1                                                            GSPBL.1255
          ELSE                                                                   GSPBL.1256
            DO K=KL,2,-1                                                         GSPBL.1257
              IF(TKEB(I,J,K).GE.TKEB(I,J,K-1))THEN                               GSPBL.1258
                KLIM=K                                                           GSPBL.1259
                GOTO 3187                                                        GSPBL.1260
              ENDIF                                                              GSPBL.1261
            ENDDO                                                                GSPBL.1262
 3187       CONTINUE                                                             GSPBL.1263
          ENDIF                                                                  GSPBL.1264
C                                                                                GSPBL.1265
C     CALCULATE KET ACCORDING TO B91 APPENDIX B                                  GSPBL.1266
C                                                                                GSPBL.1267
C     FIRST, FIND TAUS.                                                          GSPBL.1268
C                                                                                GSPBL.1269
          DO K=2,KL                                                              GSPBL.1270
            TAU1=0.1895*TAU0(I,K)                                                GSPBL.1271
            TAU4=TAU1                                                            GSPBL.1272
            TAU6=0.1535*TAU0(I,K)                                                GSPBL.1273
            TAU8=TAU6                                                            GSPBL.1274
            TAU9=3.0*TAU8                                                        GSPBL.1275
C                                                                                GSPBL.1276
C     WVAR IS THE VERTICAL VELOCITY VARIANCE ACCORDING TO APPENDIX B IN          GSPBL.1277
C     I THINK THERE MAY BE A TYPO IN THE EXPRESSION FOR WVAR. I WOULD EX         GSPBL.1278
C     SHEAR TO ALWAYS BE A PRODUCTION TERM AND THEREFORE THERE SHOULD BE         GSPBL.1279
C     NEGATIVE SIGN BEFORE THE SHEAR TERM IN THE EQUATION FOR WVAR. THIS         GSPBL.1280
C     CORRECTED HERE.                                                            GSPBL.1281
C                                                                                GSPBL.1282
            WVAR=0.6667*TKEB(I,J,K)+0.6667*TAU1*(KZM(I,K)*SVS(I,K)-2.0*          GSPBL.1283
     +           KTH(I,K)*NSQUAR(I,K))                                           GSPBL.1284
            WVAR=MAX(WVAR,0.0)                                                   GSPBL.1285
C                                                                                GSPBL.1286
C     WBCVAR IS THE VERTICAL FLUX OF B.                                          GSPBL.1287
C                                                                                GSPBL.1288
            WBCVAR=-(KTH(I,K)*NSQUAR(I,K))                                       GSPBL.1289
            TERM1=1.0+TAU8*TAU9*NSQUAR(I,K)                                      GSPBL.1290
            TERM2=1.0+TAU4*TAU6*NSQUAR(I,K)                                      GSPBL.1291
            TERM3=4.0*TAU6*TAU8*NSQUAR(I,K)                                      GSPBL.1292
            DENOM=(TERM1*TERM2)+TERM3                                            GSPBL.1293
            KET(I,K)=((TERM1+TERM3)*WVAR+TERM1*TAU6*WBCVAR)*TAU4/DENOM           GSPBL.1294
C                                                                                GSPBL.1295
C     CALCULATE KET, EDDY DIFFUSIVITY FOR TKE. RESTRICT LOWEST                   GSPBL.1296
C     VALUE OF KET BY KZOGS.                                                     GSPBL.1297
C                                                                                GSPBL.1298
            IF(XLAND(I,J).GE.1.5)THEN                                            GSPBL.1299
C                                                                                GSPBL.1300
C     USE CONSTANT KZOW OVER WATER                                               GSPBL.1301
C                                                                                GSPBL.1302
              KET(I,K)=AMAX1(KET(I,K),KZOW)                                      GSPBL.1303
            ELSE                                                                 GSPBL.1304
C                                                                                GSPBL.1305
C     USE KZO PROFILE OVER LAND                                                  GSPBL.1306
C                                                                                GSPBL.1307
              KET(I,K)=AMAX1(KET(I,K),KZOGS(K))                                  GSPBL.1308
            ENDIF                                                                GSPBL.1309
C                                                                                GSPBL.1310
C     CALCULATE KBT AND KCT, COEFFICIENTS USED IN THE VERTICAL FLUX OF T         GSPBL.1311
C     ACCORDING TO APPENDIX B.                                                   GSPBL.1312
C                                                                                GSPBL.1313
            IF(K.GE.KLIM)THEN                                                    GSPBL.1314
              KBT(I,K)=(TERM1*WVAR+2.*TAU8*WBCVAR)*TAU4*TAU6/DENOM               GSPBL.1315
              KCT(I,K)=(WVAR+TAU9*WBCVAR)*TAU4*TAU6*TAU8/DENOM                   GSPBL.1316
              KBT(I,K)=AMAX1(KBT(I,K),0.0)                                       GSPBL.1317
              KCT(I,K)=AMAX1(KCT(I,K),0.0)                                       GSPBL.1318
            ELSE                                                                 GSPBL.1319
              KBT(I,K)=0.0                                                       GSPBL.1320
              KCT(I,K)=0.0                                                       GSPBL.1321
            ENDIF                                                                GSPBL.1322
            KBT(I,K)=KBT(I,K)*IKBKC                                              GSPBL.1323
            KCT(I,K)=KCT(I,K)*IKBKC                                              GSPBL.1324
          ENDDO                                                                  GSPBL.1325
C                                                                                GSPBL.1326
C     NEED KET, KBT, AND KCT AT TOP OF THE MODEL ATMOSPHERE AND AT THE           GSPBL.1327
C     GROUND.                                                                    GSPBL.1328
C                                                                                GSPBL.1329
          KET(I,1)=KET(I,2)                                                      GSPBL.1330
          KBT(I,1)=KBT(I,2)                                                      GSPBL.1331
          KCT(I,1)=KCT(I,2)                                                      GSPBL.1332
C                                                                                GSPBL.1333
C     AT THE GROUND, NONTURB. BY DEFINITION, SET TERMS EQUAL TO ZERO.            GSPBL.1334
C                                                                                GSPBL.1335
          KET(I,KXP1)=0.0                                                        GSPBL.1336
          KBT(I,KXP1)=0.0                                                        GSPBL.1337
          KCT(I,KXP1)=0.0                                                        GSPBL.1338
        ENDDO                                                                    GSPBL.1339
C                                                                                GSPBL.1340
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.1341
C                                                                                GSPBL.1342
C                                                                                GSPBL.1343
C 7. CALCULATE THE VARIANCES OF THETAL AND QWX AND THEIR COVARIANCE              GSPBL.1344
C    ACCORDING TO APPENDIX B OF B91. RESULT IS AT THE FULL LEVELS.               GSPBL.1345
C                                                                                GSPBL.1346
        DO I=1,ILX                                                               GSPBL.1347
C                                                                                GSPBL.1348
C     TO PREVENT FLOATING UNDERFLOWS, SET QWVAR EQUAL TO ZERO WHEN               GSPBL.1349
C     THE ABSOLUTE VALUE OF DQWDZ IS SMALL.                                      GSPBL.1350
C                                                                                GSPBL.1351
          DO K=2,KL                                                              GSPBL.1352
            TAU3=0.5584*TAU0(I,K)                                                GSPBL.1353
            TLVAR(I,K)=2.*TAU3*KTH(I,K)*DTLDZ(I,K)*DTLDZ(I,K)                    GSPBL.1354
            IF(ABS(DQWDZ(I,K)).LT.1.E-10)THEN                                    GSPBL.1355
              QWVAR(I,K)=0.0                                                     GSPBL.1356
            ELSE                                                                 GSPBL.1357
              QWVAR(I,K)=2.*TAU3*KQ(I,K)*DQWDZ(I,K)*DQWDZ(I,K)                   GSPBL.1358
            ENDIF                                                                GSPBL.1359
            TQCVAR(I,K)=(KTH(I,K)*DTLDZ(I,K)*DQWDZ(I,K)+KQ(I,K)*                 GSPBL.1360
     +                  DQWDZ(I,K)*DTLDZ(I,K))*TAU3                              GSPBL.1361
          ENDDO                                                                  GSPBL.1362
C                                                                                GSPBL.1363
C     B91 DOES NOT SPECIFY HOW TO CALCULATE THESE VARIANCES/COVARIENCES          GSPBL.1364
C     THE GROUND EVEN THOUGH ONE IS NEEDED IN ORDER TO                           GSPBL.1365
C     TAKE THEIR VERTICAL DERIVATIVE AT THE KL HALF-LEVEL (TO FIND THE           GSPBL.1366
C     TURBULENT TRANSPORT TERM.)  FOR SIMPLICITY,                                GSPBL.1367
C     I WILL SET THE VARIANCES/COVARIANCES AT THE GROUND EQUAL TO                GSPBL.1368
C     THE ONES AT THE KL LEVEL.  THIS WILL ESSENTIALLY SET THE 2ND               GSPBL.1369
C     CORRECTION TERM ON THE TKE FLUX EQUATION TO ZERO WHEN CALCULATING          GSPBL.1370
C     THE TKE FLUX AT THE KL HALF LEVEL.  THIS MAY HAVE BEEN WHAT THE AU         GSPBL.1371
C     INTENDED BECAUSE KBT AND KCT ALWAYS DECREASE TOWARD THE GROUND -           GSPBL.1372
C     THEREFORE THE CORRECTIONS TO THE DOWNGRADIENT ASSUMPTION                   GSPBL.1373
C     BECOME LESS IMPORTANT NEAR THE GROUND.                                     GSPBL.1374
C                                                                                GSPBL.1375
          TLVAR(I,KXP1)=TLVAR(I,KL)                                              GSPBL.1376
          QWVAR(I,KXP1)=QWVAR(I,KL)                                              GSPBL.1377
          TQCVAR(I,KXP1)=TQCVAR(I,KL)                                            GSPBL.1378
        ENDDO                                                                    GSPBL.1379
C                                                                                GSPBL.1380
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.1381
C                                                                                GSPBL.1382
C                                                                                GSPBL.1383
C 8. CALCULATE FOG PARAMETERS                                                    GSPBL.1384
C                                                                                GSPBL.1385
C    INITIAL FOG PARAMETERS                                                      GSPBL.1386
C                                                                                GSPBL.1387
        DO I=1,ILX                                                               GSPBL.1388
          FOGFR(I)=0.0                                                           GSPBL.1389
          FOGFRT(I)=0.0                                                          GSPBL.1390
          KFOGT(I)=KL                                                            GSPBL.1391
        ENDDO                                                                    GSPBL.1392
C                                                                                GSPBL.1393
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.1394
C                                                                                GSPBL.1395
C                                                                                GSPBL.1396
C 9. COMPUTE SURFACE FLUXES                                                      GSPBL.1397
C                                                                                GSPBL.1398
        IF(ISOIL.LE.1)THEN                                                       GSPBL.1399
C                                                                                GSPBL.1400
C     COMPUTE THE FRICTIONAL VELOCITY: ZA(1982) EQS(2.60),(2.61).                GSPBL.1401
C                                                                                GSPBL.1402
          DO I=1,ILX                                                             GSPBL.1403
            PSIX=GZ1OZ0(I)-PSIM(I)                                               GSPBL.1404
            UST(I,J)=KARMAN*WSPD(I)/PSIX                                         GSPBL.1405
            USTM=AMAX1(UST(I,J),0.1)                                             GSPBL.1406
            IF((XLAND(I,J)-1.5).LT.0.)THEN                                       GSPBL.1407
              UST(I,J)=USTM                                                      GSPBL.1408
            ENDIF                                                                GSPBL.1409
          ENDDO                                                                  GSPBL.1410
C     WARNING: MOL IS NOT MOLIN LENGTH BUT RATHER KT*                            07NOV00.1713
C     COMPUTE MOL(I,J)                                                           GSPBL.1412
C                                                                                GSPBL.1413
          DO I=1,ILX                                                             GSPBL.1414
            DTG=THX(I,KL)-THGB(I)                                                07NOV00.1714
            MOL(I,J)=KARMAN*DTG/(GZ1OZ0(I)-PSIH(I))                              GSPBL.1416
          ENDDO                                                                  GSPBL.1417
C                                                                                GSPBL.1418
C     COMPUTE RADIATION AND SOIL THERMAL CAPACITY: CALL TO SFCRAD                GSPBL.1419
C                                                                                GSPBL.1420
                                                                                 25JAN00.636
#ifndef MPP1                                                                     25JAN00.637
          IF(MOD(KTAU,NTRAD(IN)).EQ.0) THEN                                      25JAN00.638
            CALL SFCRAD(IYY,JXX,J,IN,T3D,QV3D,PP3D)                              25JAN00.639
          ENDIF                                                                  25JAN00.640
#else                                                                            25JAN00.641
          IF(MOD(KTAU,NTRAD(IN)).EQ.0) THEN                                      25JAN00.642
            CALL SFCRAD_COL(IYY,JXX,I,J,IN,T3D,QV3D,PP3D)                        25JAN00.643
          ENDIF                                                                  25JAN00.644
#endif                                                                           25JAN00.645
                                                                                 25JAN00.646
C                                                                                GSPBL.1422
C     CALCULATE FOG DEPTH (METERS)                                               GSPBL.1423
C                                                                                GSPBL.1424
          DO I=1,ILX                                                             GSPBL.1425
            KF=KFOGT(I)                                                          GSPBL.1426
            SIGFGT=SIGMA(KF+1)-FOGFRT(I)*DSIGMA(KF)                              GSPBL.1427
            FOGDEP(IN,I,J)=ZQ(I,KF+1)+FOGFRT(I)*(ZQ(I,KF)-ZQ(I,KF+1))            GSPBL.1428
          ENDDO                                                                  GSPBL.1429
C                                                                                GSPBL.1430
C     COMPUTE THE SURFACE SENSIBLE AND LATENT HEAT FLUXES:                       GSPBL.1431
C                                                                                GSPBL.1432
C     INITIALLY BLANK OUT THE FLUXES                                             GSPBL.1433
C                                                                                GSPBL.1434
          DO I=1,ILX                                                             GSPBL.1435
            QFX(I,J)=0.                                                          GSPBL.1436
            HFX(I,J)=0.                                                          GSPBL.1437
          ENDDO                                                                  GSPBL.1438
C                                                                                GSPBL.1439
C     CALCULATE HEAT FLUXES ONLY IF ISSFLX.NE.0                                  GSPBL.1440
C                                                                                GSPBL.1441
          IF(ISFFLX(IN).NE.0)THEN                                                GSPBL.1442
C                                                                                GSPBL.1443
C     CALCULATE KERNELS OF FLUXES                                                GSPBL.1444
C                                                                                GSPBL.1445
            DO I=1,ILX                                                           GSPBL.1446
C                                                                                GSPBL.1447
C     DETERMINE SURFACE ROUGHNESS                                                GSPBL.1448
C                                                                                GSPBL.1449
C     OVER WATER, ALTER ROUGHNESS LENGTH (ZNT) ACCORDING TO WIND (UST).          GSPBL.1450
C                                                                                GSPBL.1451
              IF((XLAND(I,J)-1.5).GE.0.0)THEN                                    GSPBL.1452
                ZNT(I,J)=CZO*UST(I,J)*UST(I,J)/G+OZO                             GSPBL.1453
              ENDIF                                                              GSPBL.1454
              IF((XLAND(I,J)-1.5).GE.0.0)THEN                                    GSPBL.1455
                ZL=ZNT(I,J)                                                      GSPBL.1456
              ELSE                                                               GSPBL.1457
                ZL=0.01                                                          GSPBL.1458
              ENDIF                                                              GSPBL.1459
C                                                                                GSPBL.1460
C     DETERMINE EFFECTIVE MOISTURE AVAILABILITY                                  GSPBL.1461
C                                                                                GSPBL.1462
C     ASSUME MOISTURE AVAILABILITY = 1.0 WHEN FOG EXISTS DUE TO                  GSPBL.1463
C     SUSPENDED LIQUID AND DEW                                                   GSPBL.1464
C                                                                                GSPBL.1465
              XMAVL=FOGFR(I)+(1.0-FOGFR(I))*MAVAIL(I,J)                          GSPBL.1466
C                                                                                GSPBL.1467
C     CALCULATE KERNEL FOR LATENT HEAT FLUX                                      GSPBL.1468
C                                                                                GSPBL.1469
              FLQC(I)=RHOX(I)*XMAVL*UST(I,J)*KARMAN/(                            GSPBL.1470
     +                ALOG(KARMAN*UST(I,J)*ZA(I,KL)/KA+ZA(I,KL)/ZL)-             GSPBL.1471
     +                PSIH(I))                                                   GSPBL.1472
C                                                                                GSPBL.1473
C    CALCULATE KERNEL FOR SENSIBLE HEAT FLUX                                     GSPBL.1474
C                                                                                GSPBL.1475
              IF(THX(I,KL).EQ.THGB(I))THEN                                       07NOV00.1715
                FLHC(I)=0.0                                                      GSPBL.1477
              ELSE                                                               GSPBL.1478
                FLHC(I)=CPM(I,KL)*RHOX(I)*UST(I,J)*MOL(I,J)/(THX(I,KL)-          GSPBL.1479
     +                  THGB(I))                                                 07NOV00.1716
              ENDIF                                                              GSPBL.1481
            ENDDO                                                                GSPBL.1482
C                                                                                GSPBL.1483
C     COMPUTE SURFACE MOIST FLUX                                                 GSPBL.1484
C                                                                                GSPBL.1485
            IF(IDRY(IN).NE.1)THEN                                                GSPBL.1486
              DO I=1,ILX                                                         GSPBL.1487
                QFX(I,J)=FLQC(I)*(QGG(I)-QX(I,KL))                               GSPBL.1488
                QFX(I,J)=AMAX1(QFX(I,J),0.)                                      GSPBL.1489
C                                                                                GSPBL.1490
C     UPDATE EVAPORATION INTEGRATOR                                              GSPBL.1491
C                                                                                GSPBL.1492
                PARJSUM(ITQEVA_SUM,J)=PARJSUM(ITQEVA_SUM,J)+QFX(I,J)*DX*         GSPBL.1493
     +                                DX*0.5*DTSTEP                              GSPBL.1494
c                    TQEVA=TQEVA+QFX(I,J)*DX*DX*0.5*DTSTEP                       GSPBL.1495
              ENDDO                                                              GSPBL.1496
            ENDIF                                                                GSPBL.1497
C                                                                                GSPBL.1498
C     COMPUTE SURFACE HEAT FLUX:                                                 GSPBL.1499
C                                                                                GSPBL.1500
            DO I=1,ILX                                                           GSPBL.1501
              HFX(I,J)=FLHC(I)*(THGB(I)-THX(I,KL))                               07NOV00.1717
              HFX(I,J)=AMAX1(HFX(I,J),-100.)                                     GSPBL.1503
C                                                                                GSPBL.1504
C     CALCULATE HFKLMX(I)                                                        GSPBL.1505
C                                                                                GSPBL.1506
              HFKLMX(I)=AMAX1(200.,(HFX(I,J)*2.))                                GSPBL.1507
            ENDDO                                                                GSPBL.1508
          ENDIF                                                                  GSPBL.1509
C                                                                                GSPBL.1510
C     COMPUTE SURFACE MOMENTUM FLUXES:                                           GSPBL.1511
C                                                                                GSPBL.1512
          DO I=1,ILX                                                             GSPBL.1513
            WSPDX=SQRT(UX(I,KL)*UX(I,KL)+VX(I,KL)*VX(I,KL))+1.E-9                GSPBL.1514
            UFLXP(I)=-UST(I,J)*UST(I,J)*UX(I,KL)/WSPDX                           GSPBL.1515
            VFLXP(I)=-UST(I,J)*UST(I,J)*VX(I,KL)/WSPDX                           GSPBL.1516
          ENDDO                                                                  GSPBL.1517
        ENDIF                                                                    GSPBL.1528
C                                                                                GSPBL.1529
C                                                                                GSPBL.1530
C                                                                                GSPBL.1531
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.1532
C                                                                                GSPBL.1533
C                                                                                GSPBL.1534
C 10. CALCULATE GAMMAG AND HOL FOR REGIME 4                                      07NOV00.1718
C                                                                                GSPBL.1536
        DO I=1,ILX                                                               GSPBL.1537
C                                                                                GSPBL.1538
C     CALCULATE GAMMAG                                                           GSPBL.1539
C                                                                                GSPBL.1540
C     CALCULATE THE VALUE OF THE COUNTERGRADIENT HEAT FLUX (GAMMAG).             GSPBL.1541
C     GAMMAG IS DEFINED FROM MG87 EQ (5).  TSTAR IS FROM EQ 3.2.26               GSPBL.1542
C     OF THE NCAR TECH NOTE.  WSTAR IS AFTER STULL PAGE 118.                     GSPBL.1543
C                                                                                GSPBL.1544
C                                                                                GSPBL.1545
            TSTAR=(THVX(I,KL)-THGB(I)*(1.+XMOIST(IN)*EP1*QGG(I)*                 07NOV00.1719
     +            MAVAIL(I,J)))/(GZ1OZ0(I)-PSIH(I))                              GSPBL.1548
            IF(TSTAR.LE.0.0)THEN                                                 GSPBL.1549
              WSTAR=(-G*UST(I,J)*TSTAR*PBL(I,J)*KARMAN/THVX(I,KL))**(1./         GSPBL.1550
     +              3.)                                                          GSPBL.1551
            ELSE                                                                 GSPBL.1552
              WSTAR=0.0                                                          GSPBL.1553
            ENDIF                                                                GSPBL.1554
            IF(ABS(TSTAR).LT.1.E-5)TSTAR=1.E-5                                   GSPBL.1555
C                                                                                GSPBL.1565
          DO K=1,KL                                                              GSPBL.1566
C                                                                                GSPBL.1567
C     GAMMAG IS ZERO ABOVE THE BOUNDARY LAYER.                                   GSPBL.1568
C                                                                                GSPBL.1569
            IF(ZQ(I,K).GE.1.2*PBL(I,J).OR.WSTAR.EQ.0.0)THEN                      GSPBL.1570
              GAMMAG(I,K)=0.0                                                    GSPBL.1571
            ELSE                                                                 GSPBL.1572
              GAMMAG(I,K)=(5.0*HFX(I,J))/(WSTAR*PBL(I,J)*RHOX(I)*                GSPBL.1573
     +                    CPM(I,K))                                              GSPBL.1574
            ENDIF                                                                GSPBL.1575
          ENDDO                                                                  GSPBL.1576
C                                                                                GSPBL.1577
C     CALCULATE HOL FOR REGIME 4                                                 GSPBL.1578
C                                                                                GSPBL.1579
C     CALCULATE THE MONIN-OBUKHOV LENGTH ACCORDING TO                            GSPBL.1580
C     THE MM4 BOOK EQ 3.2.33 AND 3.2.31.                                         GSPBL.1581
C                                                                                GSPBL.1582
          IF(ISOIL.LE.1)THEN                                                     GSPBL.1583
            XMOL=(THX(I,KL)*UST(I,J)*UST(I,J))/(KARMAN*KARMAN*G*TSTAR)           GSPBL.1584
            IF(REGIME(I,J).EQ.4.)THEN                                            GSPBL.1585
              HOL(I,J)=-(PBL(I,J)/XMOL)                                          GSPBL.1586
            ENDIF                                                                GSPBL.1587
          ENDIF                                                                  GSPBL.1588
C                                                                                GSPBL.1589
C                                                                                GSPBL.1590
        ENDDO                                                                    GSPBL.1591
C                                                                                07NOV00.1720
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         07NOV00.1721
C                                                                                07NOV00.1722
C                                                                                07NOV00.1723
C 11. COMPUTE THE GROUND TEMPERATURE: CALL TO SLAB                               07NOV00.1724
C                                                                                07NOV00.1725
        IF(ISOIL.LE.1)THEN                                                       07NOV00.1726
                                                                                 07NOV00.1727
          IF(ITGFLG(IN).EQ.1)THEN                                                07NOV00.1728
            DELTSM=DTSTEP                                                        07NOV00.1729
#ifndef MPP1                                                                     07NOV00.1730
            CALL SLAB(DELTSM,J,IN,THX,QX,FLHC,FLQC,PP3D,T3D,1,ILX)               07NOV00.1731
#else                                                                            07NOV00.1732
            CALL SLAB_COL(DELTSM,I,J,IN,THX,QX,FLHC,FLQC,PP3D,T3D)               07NOV00.1733
#endif                                                                           07NOV00.1734
          ENDIF                                                                  07NOV00.1735
        ENDIF                                                                    07NOV00.1736
C                                                                                GSPBL.1592
C       THGB AND QGG ARE UPDATED AFTER USE FOR CALCULATING TSTAR                 07NOV00.1737
C                                                                                07NOV00.1738
      IF(IDRY(IN).NE.1)THEN                                                      07NOV00.1739
          DO I=1,ILX                                                             07NOV00.1740
            PS1=PSURFB(I)                                                        07NOV00.1741
            THGB(I)=TGB(I,J)*(100.0/PS1)**ROVCP                                  07NOV00.1742
            IF(IDRY(IN).NE.1)THEN                                                07NOV00.1743
              E1=SVP1*EXP(SVP2*(TGB(I,J)-SVPT0)/(TGB(I,J)-SVP3))                 07NOV00.1744
              QGG(I)=EP2*E1/(PS1-E1)                                             07NOV00.1745
            ENDIF                                                                07NOV00.1746
          ENDDO                                                                  07NOV00.1747
      ENDIF                                                                      07NOV00.1748
C                                                                                GSPBL.1593
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.1594
C                                                                                GSPBL.1595
C                                                                                GSPBL.1596
C 12. CALCULATE THE VERTICAL FLUX OF TKE AT THE HALF LEVELS, TKEFLX.             GSPBL.1597
C                                                                                GSPBL.1598
C     THE RESULT IS USED TO FIND THE TURBULENT TRANSPORT TERM OF THE             GSPBL.1599
C     TKE EQUATION.                                                              GSPBL.1600
C                                                                                GSPBL.1601
C     TKEFLX IS THE VERTICAL FLUX OF TKE, W PRIME E PRIME OVERBAR.               GSPBL.1602
C     SEE B91 EQUATION 16 TOP RIGHT PAGE 2108.                                   GSPBL.1603
C                                                                                GSPBL.1604
        DO I=1,ILX                                                               GSPBL.1605
C                                                                                GSPBL.1606
C     SET TKEFLX EQUAL TO ZERO AT THE HIGHEST HALF LEVEL                         GSPBL.1607
C                                                                                GSPBL.1608
          TKEFLX(I,1)=0.0                                                        GSPBL.1609
          DO K=2,KL                                                              GSPBL.1610
C                                                                                GSPBL.1611
C     AVERAGE KBT AND KCT TO THE HALF LEVELS.                                    GSPBL.1612
C                                                                                GSPBL.1613
            KBTHL=0.5*(KBT(I,K)+KBT(I,K+1))                                      GSPBL.1614
            KCTHL=0.5*(KCT(I,K)+KCT(I,K+1))                                      GSPBL.1615
C                                                                                GSPBL.1616
C     CALCULATE TERMS OF EQUATION 2.20 IN GS THESIS                              GSPBL.1617
C                                                                                GSPBL.1618
C     THE TERM VARIABLES REFER TO THE 6 VERTICAL DERIVATIVES IN                  GSPBL.1619
C     EQUATION 16 OF B91. TERM 1 IS A DIFFUSION TERM.                            GSPBL.1620
C     AT THE LOWEST HALF LEVEL, USE HFX AND QFX TO CALC VERT DERIV.              GSPBL.1621
C                                                                                GSPBL.1622
            IF(K.EQ.KL)THEN                                                      GSPBL.1623
              TERM2=BETABT(I,K)*(KTH(I,K)*DTLDZ(I,K)+HFX(I,J)/(RHOX(I)*          GSPBL.1624
     +              CPM(I,K)))/DZQ(I,K)                                          GSPBL.1625
              TERM3=BETABW(I,K)*(KQ(I,K)*DQWDZ(I,K)+QFX(I,J)/RHOX(I))/           GSPBL.1626
     +              DZQ(I,K)                                                     GSPBL.1627
            ELSE                                                                 GSPBL.1628
              TERM2=BETABT(I,K)*(KTH(I,K)*DTLDZ(I,K)-KTH(I,K+1)*                 GSPBL.1629
     +              DTLDZ(I,K+1))/DZQ(I,K)                                       GSPBL.1630
              TERM3=BETABW(I,K)*(KQ(I,K)*DQWDZ(I,K)-KQ(I,K+1)*                   GSPBL.1631
     +              DQWDZ(I,K+1))/DZQ(I,K)                                       GSPBL.1632
            ENDIF                                                                GSPBL.1633
            TERM4=BETABT(I,K)*BETABT(I,K)*(TLVAR(I,K)-TLVAR(I,K+1))/             GSPBL.1634
     +            DZQ(I,K)                                                       GSPBL.1635
            TERM5=2.*BETABW(I,K)*BETABT(I,K)*(TQCVAR(I,K)-TQCVAR(I,K+1))         GSPBL.1636
     +            /DZQ(I,K)                                                      GSPBL.1637
            TERM6=BETABW(I,K)*BETABW(I,K)*(QWVAR(I,K)-QWVAR(I,K+1))/             GSPBL.1638
     +            DZQ(I,K)                                                       GSPBL.1639
C                                                                                GSPBL.1640
C     CALCULATE TKEFLX                                                           GSPBL.1641
C                                                                                GSPBL.1642
            TKEFLX(I,K)=KBTHL*(TERM2+TERM3)-KCTHL*(TERM4+TERM5+TERM6)            GSPBL.1643
C                                                                                GSPBL.1644
          ENDDO                                                                  GSPBL.1645
        ENDDO                                                                    GSPBL.1646
C                                                                                GSPBL.1647
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.1648
C                                                                                GSPBL.1649
C                                                                                GSPBL.1650
C 13. CALCULATE NEW VALUE OF TOTAL WATER QWX                                     GSPBL.1651
C                                                                                GSPBL.1652
C                                                                                GSPBL.1653
C     REFERENCE MG87 EQ3.                                                        GSPBL.1654
C     FOR SIMPLICITY, SET TENDENCY EQUAL TO ZERO AT THE TOP SIGMA LEVEL          GSPBL.1655
C     WHEN IDRY IS ONE (DRY FORECAST), SKIP PAST MOISTURE EQUATIONS.             GSPBL.1656
C                                                                                GSPBL.1657
        IF(IDRY(IN).NE.1)THEN                                                    GSPBL.1658
C                                                                                GSPBL.1659
C     CALCULATE NEW TOTAL WATER QWX AT THE LOWEST HALF LEVEL.                    GSPBL.1660
C                                                                                GSPBL.1661
          DO I=1,ILX                                                             GSPBL.1662
            QWTNP(I,KL)=(QFX(I,J)/RHOXHL(I,KL)-(RHOXFL(I,KL)/                    GSPBL.1663
     +                  RHOXHL(I,KL))*KQ(I,KL)*(QWX(I,KL)-QWX(I,KLM))/           GSPBL.1664
     +                  DZA(I,KLM))/DZQ(I,KL)                                    GSPBL.1665
            QWX(I,KL)=QWX(I,KL)+QWTNP(I,KL)*DTSTEP                               GSPBL.1666
          ENDDO                                                                  GSPBL.1667
C                                                                                GSPBL.1668
C     CALCULATE NEW TOTAL WATER QWX AT UPPER LEVELS                              GSPBL.1669
C                                                                                GSPBL.1670
C     SOLVE THE IMPLICIT DIFFUSION EQUATION FOR QWX                              GSPBL.1671
C                                                                                GSPBL.1672
          EM(1)=0.0                                                              GSPBL.1673
          DO I=1,ILX                                                             GSPBL.1674
            FQ(1)=QWX(I,1)                                                       GSPBL.1675
            DO K=2,KLM                                                           GSPBL.1676
              ALFA(K)=(RHOXFL(I,K)/RHOXHL(I,K))*KQ(I,K)*DTSTEP/(DZQ(I,K)         GSPBL.1677
     +                *DZA(I,K-1))                                               GSPBL.1678
              BETAK=(RHOXFL(I,K+1)/RHOXHL(I,K))*KQ(I,K+1)*DTSTEP/(               GSPBL.1679
     +              DZQ(I,K)*DZA(I,K))                                           GSPBL.1680
              BM(K)=1.+ALFA(K)+BETAK-ALFA(K)*EM(K-1)                             GSPBL.1681
              EM(K)=BETAK/BM(K)                                                  GSPBL.1682
              FQ(K)=(QWX(I,K)+DTSTEP*QWTNP(I,K)+ALFA(K)*FQ(K-1))/BM(K)           GSPBL.1683
            ENDDO                                                                GSPBL.1684
            DO K=KLM,2,-1                                                        GSPBL.1685
              QWX(I,K)=EM(K)*QWX(I,K+1)+FQ(K)                                    GSPBL.1686
            ENDDO                                                                GSPBL.1687
          ENDDO                                                                  GSPBL.1688
C                                                                                GSPBL.1689
          IF(IDRS3.EQ.1)THEN                                                     GSPBL.1690
C                                                                                GSPBL.1691
C     CALCULATE NEW CLOUD/ICE WATER QWX AT THE LOWEST HALF LEVEL                 GSPBL.1692
C                                                                                GSPBL.1693
            DO I=1,ILX                                                           GSPBL.1694
              QCITNP(I,KL)=(0.0/RHOXHL(I,KL)-(RHOXFL(I,KL)/RHOXHL(I,KL))         GSPBL.1695
     +                     *KQ(I,KL)*(QCX(I,KL)-QCX(I,KLM))/DZA(I,KLM))/         GSPBL.1696
     +                     DZQ(I,KL)                                             GSPBL.1697
              QCX(I,KL)=QCX(I,KL)+QCITNP(I,KL)*DTSTEP                            GSPBL.1698
            ENDDO                                                                GSPBL.1699
C                                                                                GSPBL.1700
C     CALCULATE NEW TOTAL WATER QCX AT UPPER LEVELS                              GSPBL.1701
C                                                                                GSPBL.1702
C     SOLVE THE IMPLICIT DIFFUSION EQUATION FOR QCX                              GSPBL.1703
C                                                                                GSPBL.1704
            EM(1)=0.0                                                            GSPBL.1705
            DO I=1,ILX                                                           GSPBL.1706
              FQ(1)=QCX(I,1)                                                     GSPBL.1707
              DO K=2,KLM                                                         GSPBL.1708
                ALFA(K)=(RHOXFL(I,K)/RHOXHL(I,K))*KQ(I,K)*DTSTEP/(               GSPBL.1709
     +                  DZQ(I,K)*DZA(I,K-1))                                     GSPBL.1710
                BETAK=(RHOXFL(I,K+1)/RHOXHL(I,K))*KQ(I,K+1)*DTSTEP/(             GSPBL.1711
     +                DZQ(I,K)*DZA(I,K))                                         GSPBL.1712
                BM(K)=1.+ALFA(K)+BETAK-ALFA(K)*EM(K-1)                           GSPBL.1713
                EM(K)=BETAK/BM(K)                                                GSPBL.1714
                FQ(K)=(QCX(I,K)+DTSTEP*QCITNP(I,K)+ALFA(K)*FQ(K-1))/             GSPBL.1715
     +                BM(K)                                                      GSPBL.1716
              ENDDO                                                              GSPBL.1717
              DO K=KLM,2,-1                                                      GSPBL.1718
                QCX(I,K)=EM(K)*QCX(I,K+1)+FQ(K)                                  GSPBL.1719
              ENDDO                                                              GSPBL.1720
            ENDDO                                                                GSPBL.1721
          ENDIF                                                                  GSPBL.1722
        ENDIF                                                                    GSPBL.1723
C                                                                                GSPBL.1724
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.1725
C                                                                                GSPBL.1726
C                                                                                GSPBL.1727
C 14.  CALCULATE TENDENCY FOR THETA L                                            GSPBL.1728
C                                                                                GSPBL.1729
C     NOTE: AT THE KL LEVEL, THETA L TENDENCY WILL NEVER NEED A                  GSPBL.1730
C     GAMMAG.  WHEN THERE IS NO BOUNDARY LAYER, GAMMAG IS ZERO.  WHEN            GSPBL.1731
C     THERE IS A BL, GAMMAG WILL BE THE SAME AT KL AND KXP1. THEREFORE,          GSPBL.1732
C     ITS VALUE IS IRRELEVANT WHEN CALCULATING THE 2ND DERIVATIVE.               GSPBL.1733
C                                                                                GSPBL.1734
C     CALCULATE THL TENDENCY AT THE SURFACE LEVEL                                GSPBL.1735
C                                                                                GSPBL.1736
        DO I=1,ILX                                                               GSPBL.1737
          THLTNP(I,KL)=(HFX(I,J)/(RHOXHL(I,KL)*CPM(I,KL))-(RHOXFL(I,KL)/         GSPBL.1738
     +                 RHOXHL(I,KL))*KTH(I,KL)*(THLX(I,KL)-THLX(I,KLM))/         GSPBL.1739
     +                 DZA(I,KLM))/DZQ(I,KL)                                     GSPBL.1740
        ENDDO                                                                    GSPBL.1741
C                                                                                GSPBL.1742
C     CALCULATE THL TENDENCY AT UPPER LEVELS                                     GSPBL.1743
C                                                                                GSPBL.1744
C     CALCULATE NEW VALUE OF THL AT SURFACE LEVEL (USED IN 14.2.3)               GSPBL.1745
C                                                                                GSPBL.1746
        DO I=1,ILX                                                               GSPBL.1747
          THXSV(I)=THLX(I,KL)+DTSTEP*THLTNP(I,KL)                                GSPBL.1748
C                                                                                GSPBL.1749
C     CALCULATE EXPLICIT FORCING FOR THL EQUATION IN UPPER LEVELS                GSPBL.1750
C                                                                                GSPBL.1751
          DO K=2,KLM                                                             GSPBL.1752
            THLTNP(I,K)=((RHOXFL(I,K)/RHOXHL(I,K))*KTH(I,K)*(-                   GSPBL.1753
     +                  GAMMAG(I,K))-(RHOXFL(I,K+1)/RHOXHL(I,K))*                GSPBL.1754
     +                  KTH(I,K+1)*(-GAMMAG(I,K+1)))/DZQ(I,K)                    GSPBL.1755
          ENDDO                                                                  GSPBL.1756
        ENDDO                                                                    GSPBL.1757
C                                                                                GSPBL.1758
C     SOLVE IMPLICIT DIFFUSION EQUATION FOR THL: RESULT IS                       GSPBL.1759
C     TENDENCY THLTNP                                                            GSPBL.1760
C                                                                                GSPBL.1761
        EM(1)=0.0                                                                GSPBL.1762
        DO I=1,ILX                                                               GSPBL.1763
          FTHX(1)=THLX(I,1)                                                      GSPBL.1764
          DO K=2,KLM                                                             GSPBL.1765
            ALFA(K)=(RHOXFL(I,K)/RHOXHL(I,K))*KTH(I,K)*DTSTEP/(DZQ(I,K)*         GSPBL.1766
     +              DZA(I,K-1))                                                  GSPBL.1767
            BETAK=(RHOXFL(I,K+1)/RHOXHL(I,K))*KTH(I,K+1)*DTSTEP/(                GSPBL.1768
     +            DZQ(I,K)*DZA(I,K))                                             GSPBL.1769
            BM(K)=1.+ALFA(K)+BETAK-ALFA(K)*EM(K-1)                               GSPBL.1770
            EM(K)=BETAK/BM(K)                                                    GSPBL.1771
            FTHX(K)=(THLX(I,K)+DTSTEP*THLTNP(I,K)+ALFA(K)*FTHX(K-1))/            GSPBL.1772
     +              BM(K)                                                        GSPBL.1773
          ENDDO                                                                  GSPBL.1774
          DO K=KLM,2,-1                                                          GSPBL.1775
            THXSV(I)=EM(K)*THXSV(I)+FTHX(K)                                      GSPBL.1776
            THLTNP(I,K)=(THXSV(I)-THLX(I,K))/DTSTEP                              GSPBL.1777
                                                                                 25JAN00.652
          ENDDO                                                                  GSPBL.1778
        ENDDO                                                                    GSPBL.1779
C                                                                                GSPBL.1780
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.1781
C                                                                                GSPBL.1782
C                                                                                GSPBL.1783
C 15. FOG CALCULATIONS (OFF)                                                     GSPBL.1784
C                                                                                GSPBL.1785
C                                                                                GSPBL.1786
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.1787
C                                                                                GSPBL.1788
C                                                                                GSPBL.1789
C 16. CALCULATE NEW VALUE OF THLX                                                GSPBL.1790
C                                                                                GSPBL.1791
        DO I=1,ILX                                                               GSPBL.1792
          DO K=2,KL                                                              GSPBL.1793
            THLX(I,K)=THLX(I,K)+DTSTEP*THLTNP(I,K)                               GSPBL.1794
          ENDDO                                                                  GSPBL.1795
        ENDDO                                                                    GSPBL.1796
C                                                                                GSPBL.1797
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.1798
C                                                                                GSPBL.1799
C                                                                                GSPBL.1800
C 17. PARTITION TOTAL WATER INTO VAPOR AND LIQUID (OFF)                          GSPBL.1801
C                                                                                GSPBL.1802
C                                                                                GSPBL.1803
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.1804
C                                                                                GSPBL.1805
C                                                                                GSPBL.1806
C 18. CALCULATE NEW VALUES OF THX, QX AND QCX USING NEW VALUES OF THLX A         GSPBL.1807
C                                                                                GSPBL.1808
        DO I=1,ILX                                                               GSPBL.1809
          DO K=1,KL                                                              GSPBL.1810
C                                                                                GSPBL.1811
C     CALCULATE QX, THX IN DRY CASES                                             GSPBL.1812
C                                                                                GSPBL.1813
            IF(IMOIST(IN).EQ.1)THEN                                              GSPBL.1814
              THX(I,K)=THLX(I,K)                                                 GSPBL.1815
              QX(I,K)=QWX(I,K)                                                   GSPBL.1816
            ELSE                                                                 GSPBL.1817
C                                                                                GSPBL.1818
C     CALCULATE QX, QCX, THX IN WET CASE                                         GSPBL.1819
C                                                                                GSPBL.1820
C     CALCULATE CLOUD WATER AND WATER VAPOR IN SAME MANNER AS IN MICROPH         GSPBL.1821
C     OF MM5.  IF THE TOTAL LIQUID WATER MIXING RATIO IS GREATER THAN TH         GSPBL.1822
C     SATURATION MIXING RATIO, THEN DO TWO THINGS:  DO NOT LET THE WATER         GSPBL.1823
C     VAPOR MIXING RATIO EXCEED THE SATURATION MIXING RATIO (QX=QSAT) AN         GSPBL.1824
C     THE REST OF THE TOTAL LIQUID WATER IS PARTITIONED TO BE CLOUD WATE         GSPBL.1825
C     OTHERWISE, NO CLOUD WATER IS ALLOWED TO EXIST SINCE SUPERSATURATIO         GSPBL.1826
C     CONDITIONS DO NOT EXIST (I.E. QWX IS LESS THAN OR EQUAL TO QSAT).          GSPBL.1827
C                                                                                GSPBL.1828
C     CALCULATE SATURATION WATER MIXING RATIO                                    GSPBL.1829
C                                                                                GSPBL.1830
              PL=PRESHL(I,K)                                                     GSPBL.1831
              IF(SCR3(I,K).GT.TO)THEN                                            GSPBL.1832
                EES=SVP1*EXP(SVP2*(SCR3(I,K)-SVPT0)/(SCR3(I,K)-SVP3))            GSPBL.1833
              ELSE                                                               GSPBL.1834
                EES=.611*EXP(22.514-6.15E3/SCR3(I,K))                            GSPBL.1835
              ENDIF                                                              GSPBL.1836
              QSAT=.622*EES/(PL-EES)                                             GSPBL.1837
C                                                                                GSPBL.1838
C    CALCULATE QX AND QCX ACCORDING TO SATURATION STATE                          GSPBL.1839
C                                                                                GSPBL.1840
              IF(IDRS3.NE.1)THEN                                                 GSPBL.1841
                IF(QWX(I,K).GT.QSAT)THEN                                         GSPBL.1842
                  QX(I,K)=QSAT                                                   GSPBL.1843
                  QCX(I,K)=QWX(I,K)-QX(I,K)                                      GSPBL.1844
                ELSE                                                             GSPBL.1845
                  QX(I,K)=QWX(I,K)                                               GSPBL.1846
                  QCX(I,K)=0.                                                    GSPBL.1847
                ENDIF                                                            GSPBL.1848
              ELSE                                                               GSPBL.1849
                QX(I,K)=QWX(I,K)-QCX(I,K)                                        GSPBL.1850
                IF(QX(I,K).LT.0.0)STOP 1822                                      GSPBL.1851
              ENDIF                                                              GSPBL.1852
C                                                                                GSPBL.1853
C    CALCULATE THX                                                               GSPBL.1854
C                                                                                GSPBL.1855
              IF(IDRS2.NE.1)THEN                                                 GSPBL.1856
                THCON=(100./PL)**0.286                                           GSPBL.1857
                THX(I,K)=THLX(I,K)+(XLVP(I,K)/CPM(I,K))*QCX(I,K)*THCON           GSPBL.1858
              ELSE                                                               GSPBL.1859
                CPM(I,K)=CP*(1.+0.8*QX(I,K)*XMOIST(IN))                          GSPBL.1860
                THX(I,K)=THLX(I,K)/(1.-XLVP(I,K)*QCX(I,K)/(CPM(I,K)*             24SEP99.418
     +                   AMAX1(SCR3(I,K),253.)))                                 GSPBL.1862
              ENDIF                                                              GSPBL.1863
            ENDIF                                                                GSPBL.1864
          ENDDO                                                                  GSPBL.1865
        ENDDO                                                                    GSPBL.1866
C                                                                                GSPBL.1867
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.1868
C                                                                                GSPBL.1869
C                                                                                GSPBL.1870
C 19.  CALCULATE TENDENCIES FOR WINDS.                                           GSPBL.1871
C                                                                                GSPBL.1872
C     CALCULATE WIND TENDENCY AT THE SURFACE LAYER                               GSPBL.1873
C                                                                                GSPBL.1874
        DO I=1,ILX                                                               GSPBL.1875
          UTNP(I,KL)=(UFLXP(I)*(RHOX(I)/RHOXHL(I,KL))-(RHOXFL(I,KL)/             GSPBL.1876
     +               RHOXHL(I,KL))*KZM(I,KL)*(UX(I,KL)-UX(I,KLM))/               GSPBL.1877
     +               DZA(I,KLM))/DZQ(I,KL)                                       GSPBL.1878
          VTNP(I,KL)=(VFLXP(I)*(RHOX(I)/RHOXHL(I,KL))-(RHOXFL(I,KL)/             GSPBL.1879
     +               RHOXHL(I,KL))*KZM(I,KL)*(VX(I,KL)-VX(I,KLM))/               GSPBL.1880
     +               DZA(I,KLM))/DZQ(I,KL)                                       GSPBL.1881
        ENDDO                                                                    GSPBL.1882
C                                                                                GSPBL.1883
C     CALCULATE WIND TENDENCY AT UPPER LAYERS                                    GSPBL.1884
C                                                                                GSPBL.1885
C     CALCULATE NEW VALUES OF WINDS AT SURFACE LEVEL (USED IN 19.2.2)            GSPBL.1886
C                                                                                GSPBL.1887
        DO I=1,ILX                                                               GSPBL.1888
          UXSV(I)=UX(I,KL)+DTSTEP*UTNP(I,KL)                                     GSPBL.1889
          VXSV(I)=VX(I,KL)+DTSTEP*VTNP(I,KL)                                     GSPBL.1890
        ENDDO                                                                    GSPBL.1891
C                                                                                GSPBL.1892
C     SOLVE IMPLICIT DIFFUSION EQUATION FOR WINDS IN UPPER LEVELS                GSPBL.1893
C                                                                                GSPBL.1894
        EM(1)=0.0                                                                GSPBL.1895
        DO I=1,ILX                                                               GSPBL.1896
          FU(1)=UX(I,1)                                                          GSPBL.1897
          FV(1)=VX(I,1)                                                          GSPBL.1898
          DO K=2,KLM                                                             GSPBL.1899
            ALFA(K)=(RHOXFL(I,K)/RHOXHL(I,K))*KZM(I,K)*DTSTEP/(DZQ(I,K)*         GSPBL.1900
     +              DZA(I,K-1))                                                  GSPBL.1901
            BETAK=(RHOXFL(I,K+1)/RHOXHL(I,K))*KZM(I,K+1)*DTSTEP/(                GSPBL.1902
     +            DZQ(I,K)*DZA(I,K))                                             GSPBL.1903
            BM(K)=1.+ALFA(K)+BETAK-ALFA(K)*EM(K-1)                               GSPBL.1904
            EM(K)=BETAK/BM(K)                                                    GSPBL.1905
            FU(K)=(UX(I,K)+ALFA(K)*FU(K-1))/BM(K)                                GSPBL.1906
            FV(K)=(VX(I,K)+ALFA(K)*FV(K-1))/BM(K)                                GSPBL.1907
          ENDDO                                                                  GSPBL.1908
          DO K=KLM,2,-1                                                          GSPBL.1909
            UXSV(I)=EM(K)*UXSV(I)+FU(K)                                          GSPBL.1910
            VXSV(I)=EM(K)*VXSV(I)+FV(K)                                          GSPBL.1911
            UTNP(I,K)=(UXSV(I)-UX(I,K))/DTSTEP                                   GSPBL.1912
            VTNP(I,K)=(VXSV(I)-VX(I,K))/DTSTEP                                   GSPBL.1913
          ENDDO                                                                  GSPBL.1914
        ENDDO                                                                    GSPBL.1915
C                                                                                GSPBL.1916
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.1917
C                                                                                GSPBL.1918
C                                                                                GSPBL.1919
C 20. CALCULATE TENDENCIES FOR THETA, QX AND QC                                  GSPBL.1920
C                                                                                GSPBL.1921
C     CALCULATE TENDENCY FOR THX                                                 GSPBL.1922
C                                                                                GSPBL.1923
        DO I=1,ILX                                                               GSPBL.1924
          TTNP(I,1)=0.0                                                          GSPBL.1925
          DO K=2,KL                                                              GSPBL.1926
            TTNP(I,K)=(THX(I,K)-THXS(I,K))/DTSTEP                                GSPBL.1927
          ENDDO                                                                  GSPBL.1928
        ENDDO                                                                    GSPBL.1929
C                                                                                GSPBL.1930
C     CALCULATE TENDENCY FOR QX AND QC                                           GSPBL.1931
C                                                                                GSPBL.1932
        IF(IDRY(IN).NE.1)THEN                                                    GSPBL.1933
          DO I=1,ILX                                                             GSPBL.1934
            QTNP(I,1)=0.0                                                        GSPBL.1935
            DO K=2,KL                                                            GSPBL.1936
              QTNP(I,K)=(QX(I,K)-QXS(I,K))/DTSTEP                                GSPBL.1937
            ENDDO                                                                GSPBL.1938
          ENDDO                                                                  GSPBL.1939
          IF(IMOIST(IN).NE.1)THEN                                                GSPBL.1940
            DO K=2,KL                                                            GSPBL.1941
              DO I=1,ILX                                                         GSPBL.1942
                QCTNP(I,K)=(QCX(I,K)-QCXS(I,K))/DTSTEP                           GSPBL.1943
              ENDDO                                                              GSPBL.1944
            ENDDO                                                                GSPBL.1945
          ENDIF                                                                  GSPBL.1946
        ENDIF                                                                    GSPBL.1947
C                                                                                GSPBL.1948
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.1949
C                                                                                GSPBL.1950
C                                                                                GSPBL.1951
C 21. CALL TO GSRADSUB AND UPDATE TTNP                                           GSPBL.1952
C                                                                                GSPBL.1953
C                                                                                GSPBL.1954
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.1955
C                                                                                GSPBL.1956
C                                                                                GSPBL.1957
C 22. CALCULATE NEW VALUES OF TKE                                                GSPBL.1958
C                                                                                GSPBL.1959
C     CALCULATE EXPLICIT FORCINGS OF TKE EQUATION                                GSPBL.1960
C                                                                                GSPBL.1961
C     CALCULATE TERM ASSOCIATED WITH TKEFLX                                      GSPBL.1962
C                                                                                GSPBL.1963
C     FIND THE TURB TRANSPORT TERM AT THE FULL LEVELS BY TAKING                  GSPBL.1964
C     THE VERTICAL DERIVATIVE OF TKEFLX.                                         GSPBL.1965
C                                                                                GSPBL.1966
        DO I=1,ILX                                                               GSPBL.1967
          DO K=2,KL                                                              GSPBL.1968
            TURBT(I,K)=((RHOXHL(I,K-1)*TKEFLX(I,K-1)-RHOXHL(I,K)*                GSPBL.1969
     +                 TKEFLX(I,K))/RHOXFL(I,K))/DZA(I,K-1)                      GSPBL.1970
          ENDDO                                                                  GSPBL.1971
        ENDDO                                                                    GSPBL.1972
C                                                                                GSPBL.1973
C     CALCULATE SHEAR, BUOYANCY AND DISSIPATION TERMS OF TKE EQUATION            GSPBL.1974
C                                                                                GSPBL.1975
C     CALCULATE THE TKE TENDENCY AFTER B91 WHERE SHEAR IS THE                    GSPBL.1976
C     SHEAR TERM, BOUYAN IS THE BOUYANCY TERM, DISSIP IS THE DISSIPATION         GSPBL.1977
C     TERM, AND TURBT IS THE TURBULENT TRANSPORT TERM.                           GSPBL.1978
C     NOTE, AS IN MG87, INCLUDE GAMMAG IN THE BOUYANCY TERM.                     GSPBL.1979
C                                                                                GSPBL.1980
        DO I=1,ILX                                                               GSPBL.1981
          DO K=1,KL                                                              GSPBL.1982
            FRT=THVX(I,K)/THXS(I,K)                                              GSPBL.1983
            FACTTLDRY(I,K)=FRT                                                   GSPBL.1984
            FACTQWDRY(I,K)=0.61*THXS(I,K)                                        GSPBL.1985
            XLOVCPT=XLVP(I,K)/(CPM(I,K)*SCR3(I,K))                               GSPBL.1986
            XLOVRT=XLVP(I,K)/(R*SCR3(I,K))                                       GSPBL.1987
            ALFA1=0.622*XLOVRT*QXS(I,K)/THXS(I,K)                                GSPBL.1988
            BETA1=0.622*XLOVRT*XLOVCPT                                           GSPBL.1989
            XLAMDA1=1./(1.+BETA1*QXS(I,K))                                       GSPBL.1990
            FACTTLSAT(I,K)=FRT-ALFA1*XLAMDA1*THXS(I,K)*(FRT*XLOVCPT-1.61         GSPBL.1991
     +                     )                                                     GSPBL.1992
            FACTQWSAT(I,K)=(FACTTLSAT(I,K)*XLOVCPT-1.0)*THXS(I,K)                GSPBL.1993
          ENDDO                                                                  GSPBL.1994
          DO K=2,KL                                                              GSPBL.1995
            INDSATUP=0                                                           GSPBL.1996
            INDSATDW=0                                                           GSPBL.1997
            INDTRANS=0                                                           GSPBL.1998
            IF(QCXS(I,K-1).GT.QCMIN)INDSATUP=1                                   GSPBL.1999
            IF(QCXS(I,K).GT.QCMIN)INDSATDW=1                                     GSPBL.2000
            IF(INDSATUP.EQ.INDSATDW)THEN                                         GSPBL.2001
              FRACZ=(ZQ(I,K)-ZA(I,K))/DZA(I,K-1)                                 GSPBL.2002
              IF(INDSATUP.EQ.1)THEN                                              GSPBL.2003
                FACTTLFL=FACTTLSAT(I,K)+FRACZ*(FACTTLSAT(I,K-1)-                 GSPBL.2004
     +                   FACTTLSAT(I,K))                                         GSPBL.2005
                FACTQWFL=FACTQWSAT(I,K)+FRACZ*(FACTQWSAT(I,K-1)-                 GSPBL.2006
     +                   FACTQWSAT(I,K))                                         GSPBL.2007
              ELSE                                                               GSPBL.2008
                FACTTLFL=FACTTLDRY(I,K)+FRACZ*(FACTTLDRY(I,K-1)-                 GSPBL.2009
     +                   FACTTLDRY(I,K))                                         GSPBL.2010
                FACTQWFL=FACTQWDRY(I,K)+FRACZ*(FACTQWDRY(I,K-1)-                 GSPBL.2011
     +                   FACTQWDRY(I,K))                                         GSPBL.2012
              ENDIF                                                              GSPBL.2013
            ELSE                                                                 GSPBL.2014
              INDTRANS=1                                                         GSPBL.2015
              IF(INDSATUP.EQ.0)THEN                                              GSPBL.2016
                FACTTLFL=FACTTLDRY(I,K-1)                                        GSPBL.2017
                FACTQWFL=FACTQWDRY(I,K-1)                                        GSPBL.2018
              ELSE                                                               GSPBL.2019
                FACTTLFL=FACTTLDRY(I,K)                                          GSPBL.2020
                FACTQWFL=FACTQWDRY(I,K)                                          GSPBL.2021
              ENDIF                                                              GSPBL.2022
            ENDIF                                                                GSPBL.2023
            BUOFLUX=FACTTLFL*(-KTH(I,K)*(DTLDZ(I,K)-GAMMAG(I,K)))+               GSPBL.2024
     +              FACTQWFL*(-KQ(I,K)*DQWDZ(I,K))                               GSPBL.2025
            IF(INDTRANS.EQ.0.AND.ITKESAT.EQ.1)THEN                               GSPBL.2026
              BOUYAN(I,K)=-GOVTHV(I,K)*BUOFLUX                                   GSPBL.2027
            ELSE                                                                 GSPBL.2028
              BOUYAN(I,K)=GOVTHV(I,K)*KTH(I,K)*(DTVDZ(I,K)-GAMMAG(I,K))          GSPBL.2029
            ENDIF                                                                GSPBL.2030
          ENDDO                                                                  GSPBL.2031
        ENDDO                                                                    GSPBL.2032
C                                                                                GSPBL.2033
        DO I=1,ILX                                                               GSPBL.2034
          DO K=2,KL                                                              GSPBL.2035
            SHEAR(I,K)=KZM(I,K)*SVS(I,K)                                         GSPBL.2036
            DISSIP(I,K)=TKEB(I,J,K)/TAU0(I,K)                                    GSPBL.2037
C                                                                                GSPBL.2038
C     CHECK TO SEE IF MODIFICATIONS TO TKEB REDUCE IT BELOW THE BACKGROU         GSPBL.2039
C     MINIMUM.  SMALL OR NEGATIVE VALUES OF TKEB CAN GIVE THE IMPLICIT           GSPBL.2040
C     SCHEME PROBLEMS.  SEE APPENDIX B OF HOLTSLAG AND BOVILLE J OF CLIM         GSPBL.2041
C     1993 FOR DETAILS.                                                          GSPBL.2042
C                                                                                GSPBL.2043
            CHECK=TKEB(I,J,K)+(DTTKE)*(SHEAR(I,K)-BOUYAN(I,K)-                   GSPBL.2044
     +            DISSIP(I,K)-TURBT(I,K))                                        GSPBL.2045
            IF(CHECK.LT.0.001.AND.ABS(TKEB(I,J,K)-CHECK).GT.1.E-10)THEN          23DEC04.3423
              FMULT=(TKEB(I,J,K)-0.001)/(TKEB(I,J,K)-CHECK)                      GSPBL.2047
              TURBT(I,K)=TURBT(I,K)*FMULT                                        GSPBL.2048
              SHEAR(I,K)=SHEAR(I,K)*FMULT                                        GSPBL.2049
              BOUYAN(I,K)=BOUYAN(I,K)*FMULT                                      GSPBL.2050
              DISSIP(I,K)=DISSIP(I,K)*FMULT                                      GSPBL.2051
            ENDIF                                                                GSPBL.2052
          ENDDO                                                                  GSPBL.2053
        ENDDO                                                                    GSPBL.2054
C                                                                                GSPBL.2055
C     CALCULATE NEW VALUES OF TKE                                                GSPBL.2056
C                                                                                GSPBL.2057
C     CALCULATE THE NEXT VALUE OF TKE FOR ALL FULL LEVELS.                       GSPBL.2058
C     PREVENT NEGATIVE VALUES OF TURBULENT KINETIC ENERGY.                       GSPBL.2059
C     LET THE LOWER BOUND BE 1.E-03                                              GSPBL.2060
C     FOR SIMPLICITY, SET TKE AT THE TOP OF MODEL EQUAL TO THE TKE AT            GSPBL.2061
C     THE SECOND FULL SIGMA LEVEL.                                               GSPBL.2062
C                                                                                GSPBL.2063
C     SOLVE IMPLICIT DIFFUSION EQUATION FOR TKEB                                 GSPBL.2064
C                                                                                GSPBL.2065
        EM(1)=0.0                                                                GSPBL.2066
        DO I=1,ILX                                                               GSPBL.2067
          FTKE(1)=TKEB(I,J,1)                                                    GSPBL.2068
          DO K=2,KL                                                              GSPBL.2069
            KETHL=0.5*(KET(I,K)+KET(I,K-1))                                      GSPBL.2070
            ALFA(K)=(RHOXHL(I,K-1)/RHOXFL(I,K))*KETHL*(DTTKE)/(                  GSPBL.2071
     +              DZQ(I,K-1)*DZA(I,K-1))                                       GSPBL.2072
            KETHL=0.5*(KET(I,K)+KET(I,K+1))                                      GSPBL.2073
            BETAK=(RHOXHL(I,K)/RHOXFL(I,K))*KETHL*(DTTKE)/(DZQ(I,K)*             GSPBL.2074
     +            DZA(I,K-1))                                                    GSPBL.2075
            BM(K)=1.+ALFA(K)+BETAK-ALFA(K)*EM(K-1)                               GSPBL.2076
            EM(K)=BETAK/BM(K)                                                    GSPBL.2077
            FTKE(K)=(TKEB(I,J,K)+(DTTKE)*(SHEAR(I,K)-BOUYAN(I,K)-                GSPBL.2078
     +              DISSIP(I,K)-TURBT(I,K))+ALFA(K)*FTKE(K-1))/BM(K)             GSPBL.2079
          ENDDO                                                                  GSPBL.2080
          DO K=KL,2,-1                                                           GSPBL.2081
            TKEB(I,J,K)=EM(K)*TKEB(I,J,K+1)+FTKE(K)                              GSPBL.2082
          ENDDO                                                                  GSPBL.2083
C                                                                                GSPBL.2084
C     IMPOSE POSITIVE SIGN ON TKE                                                GSPBL.2085
C                                                                                GSPBL.2086
          DO K=KL,2,-1                                                           GSPBL.2087
            TKEB(I,J,K)=AMAX1(TKEB(I,J,K),0.001)                                 GSPBL.2088
          ENDDO                                                                  GSPBL.2089
C                                                                                GSPBL.2090
C     DEFINE TKEB AT LEVELS 1 AND KXP1                                           GSPBL.2091
C                                                                                GSPBL.2092
          TKEB(I,J,1)=TKEB(I,J,2)                                                GSPBL.2093
C                                                                                GSPBL.2094
C     TKE IS ZERO AT THE GROUND.                                                 GSPBL.2095
C                                                                                GSPBL.2096
          TKEB(I,J,KXP1)=0.0                                                     GSPBL.2097
        ENDDO                                                                    GSPBL.2098
C                                                                                GSPBL.2099
C                                                                                GSPBL.2100
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.2101
C                                                                                GSPBL.2102
C     CALCULATE NEW VALUES OF VARIABLES INSIDE MITERING CYCLE                    GSPBL.2103
C                                                                                GSPBL.2104
        IF(MSTEPS.GT.1)THEN                                                      GSPBL.2105
          DO K=1,KL                                                              GSPBL.2106
            DO I=1,ILX                                                           GSPBL.2107
              UX(I,K)=UX(I,K)+DTSTEP*UTNP(I,K)                                   GSPBL.2108
              VX(I,K)=VX(I,K)+DTSTEP*VTNP(I,K)                                   GSPBL.2109
              THX(I,K)=THXS(I,K)+DTSTEP*TTNP(I,K)                                GSPBL.2110
              THXS(I,K)=THX(I,K)                                                 GSPBL.2111
              QX(I,K)=QXS(I,K)+DTSTEP*QTNP(I,K)                                  GSPBL.2112
              QXS(I,K)=QX(I,K)                                                   GSPBL.2113
              QCX(I,K)=QCXS(I,K)+DTSTEP*QCTNP(I,K)                               GSPBL.2114
              QCXS(I,K)=QCX(I,K)                                                 GSPBL.2115
            ENDDO                                                                GSPBL.2116
          ENDDO                                                                  GSPBL.2117
        ENDIF                                                                    GSPBL.2118
      ENDDO                                                                      GSPBL.2119
!----------END OF MITERING LOOP-----------------                                 GSPBL.2120
C                                                                                GSPBL.2121
C                                                                                GSPBL.2122
C     CALCULATING OUTPUT TENDENCIES                                              GSPBL.2123
C                                                                                GSPBL.2124
C                                                                                GSPBL.2125
      DO I=1,ILX                                                                 GSPBL.2126
        DO K=1,KL                                                                GSPBL.2127
          TKETEND(I,J,K)=(TKEB(I,J,K)-TKEBOLD(I,K))/(DTTKE*FLOAT(MSTEPS)         GSPBL.2128
     +                   )                                                       GSPBL.2129
          TKEB(I,J,K)=TKEBOLD(I,K)                                               GSPBL.2130
        ENDDO                                                                    GSPBL.2131
      ENDDO                                                                      GSPBL.2132
C                                                                                GSPBL.2133
      IF(MSTEPS.GT.1)THEN                                                        GSPBL.2134
        DO K=1,KL                                                                GSPBL.2135
          DO I=1,ILX                                                             GSPBL.2136
            UTNP(I,K)=(UX(I,K)-UXS(I,K))/DT                                      GSPBL.2137
            VTNP(I,K)=(VX(I,K)-VXS(I,K))/DT                                      GSPBL.2138
            TTNP(I,K)=THX(I,K)/DT-THXSS(I,K)/DT                                  GSPBL.2139
            QTNP(I,K)=(QX(I,K)-QXSS(I,K))/DT                                     GSPBL.2140
            QCTNP(I,K)=(QCX(I,K)-QCXSS(I,K))/DT                                  GSPBL.2141
            THXS(I,K)=THXSS(I,K)                                                 GSPBL.2142
          ENDDO                                                                  GSPBL.2143
        ENDDO                                                                    GSPBL.2144
      ENDIF                                                                      GSPBL.2145
                                                                                 25JAN00.653
C                                                                                GSPBL.2146
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.2147
C                                                                                GSPBL.2148
C                                                                                GSPBL.2149
C 23. CONVERT TENDENCIES TO FLUX FORM AND CALCULATE OUTPUTS                      GSPBL.2150
C     OF THE SUBROUTINE                                                          GSPBL.2151
C                                                                                GSPBL.2152
C     CONVERT ALL THE TENDENCIES TO FLUX FORM                                    GSPBL.2153
C                                                                                GSPBL.2154
      DO I=1,ILX                                                                 GSPBL.2155
        DO K=1,KL                                                                GSPBL.2156
          UTNP(I,K)=PSB(I,J)*UTNP(I,K)                                           GSPBL.2157
          VTNP(I,K)=PSB(I,J)*VTNP(I,K)                                           GSPBL.2158
          TTNP(I,K)=PSB(I,J)*TTNP(I,K)                                           GSPBL.2159
        ENDDO                                                                    GSPBL.2160
      ENDDO                                                                      GSPBL.2161
      IF(IDRY(IN).NE.1)THEN                                                      GSPBL.2162
        DO I=1,ILX                                                               GSPBL.2163
          DO K=1,KL                                                              GSPBL.2164
            QTNP(I,K)=PSB(I,J)*QTNP(I,K)                                         GSPBL.2165
          ENDDO                                                                  GSPBL.2166
        ENDDO                                                                    GSPBL.2167
        IF(IMOIST(IN).NE.1)THEN                                                  GSPBL.2168
          DO I=1,ILX                                                             GSPBL.2169
            DO K=1,KL                                                            GSPBL.2170
              QCTNP(I,K)=PSB(I,J)*QCTNP(I,K)                                     GSPBL.2171
            ENDDO                                                                GSPBL.2172
          ENDDO                                                                  GSPBL.2173
        ENDIF                                                                    GSPBL.2174
      ENDIF                                                                      GSPBL.2175
C                                                                                GSPBL.2176
C     ADD PBL TENDENCIES TO THE MM5 TENDENCIES                                   GSPBL.2177
C                                                                                GSPBL.2178
C     CALCULATION OF NEW VALUES DUE TO VERTICAL EXCHANGE PROCESSES IS            GSPBL.2179
C     COMPLETED. THE FINAL STEP IS TO ADD THE TENDENCIES                         GSPBL.2180
C     CALCULATED IN GSPBL TO THOSE OF MM5.                                       GSPBL.2181
C                                                                                GSPBL.2182
C                                                                                GSPBL.2183
      DO I=1,ILX                                                                 GSPBL.2184
        DO K=1,KL                                                                GSPBL.2185
          U3DTEN(I,J,K)=UTNP(I,K)                                                GSPBL.2186
          V3DTEN(I,J,K)=VTNP(I,K)                                                GSPBL.2187
        ENDDO                                                                    GSPBL.2188
      ENDDO                                                                      GSPBL.2189
C IBGN and IEND code fix (GKH/DRS)                                               GSPBL.2190
      IBGN=2                                                                     GSPBL.2191
      IEND=ILXM                                                                  GSPBL.2192
      IF(IN.GT.1)THEN                                                            GSPBL.2193
        IBGN=3                                                                   GSPBL.2194
        IEND=ILXM-1                                                              GSPBL.2195
      ENDIF                                                                      GSPBL.2196
      JBGN=2                                                                     GSPBL.2197
      JEND=JLXM                                                                  GSPBL.2198
      IF(IN.GT.1)THEN                                                            GSPBL.2199
        JBGN=3                                                                   GSPBL.2200
        JEND=JLXM-1                                                              GSPBL.2201
      ENDIF                                                                      GSPBL.2202
#ifdef MPP1                                                                      25JAN00.654
CMPP1      IF(FLIC_L2G_M(I).GE.IBGN.AND.FLIC_L2G_M(I).LE.IEND)THEN               25JAN00.655
#endif                                                                           25JAN00.656
      IF(J.GE.JBGN.AND.J.LE.JEND)THEN                                            GSPBL.2203
        DO K=1,KL                                                                GSPBL.2204
          DO I=IBGN,IEND                                                         GSPBL.2205
            TBOVTH=T3D(I,J,K)/THXS(I,K)                                          GSPBL.2206
            T3DTEN(I,J,K)=T3DTEN(I,J,K)+TBOVTH*TTNP(I,K)                         GSPBL.2207
          ENDDO                                                                  GSPBL.2208
        ENDDO                                                                    GSPBL.2209
        IF(IDRY(IN).NE.1)THEN                                                    GSPBL.2210
          DO K=1,KL                                                              GSPBL.2211
            DO I=IBGN,IEND                                                       GSPBL.2212
              QV3DTEN(I,J,K)=QV3DTEN(I,J,K)+QTNP(I,K)                            GSPBL.2213
            ENDDO                                                                GSPBL.2214
          ENDDO                                                                  GSPBL.2215
          IF(IMOIST(IN).NE.1)THEN                                                GSPBL.2216
            DO K=1,KL                                                            GSPBL.2217
              DO I=IBGN,IEND                                                     GSPBL.2218
                QC3DTEN(I,J,K)=QC3DTEN(I,J,K)+QCTNP(I,K)                         GSPBL.2219
              ENDDO                                                              GSPBL.2220
            ENDDO                                                                GSPBL.2221
          ENDIF                                                                  GSPBL.2222
        ENDIF                                                                    GSPBL.2223
      ENDIF                                                                      GSPBL.2224
#ifdef MPP1                                                                      25JAN00.657
CMPP1      ENDIF                                                                 25JAN00.658
#endif                                                                           25JAN00.659
C                                                                                GSPBL.2225
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.2226
C                                                                                GSPBL.2227
C     COMPUTE MITERING STEPS                                                     GSPBL.2228
C                                                                                GSPBL.2229
CDRSM COMPUTE NUMBER OF MITER STEPS FOR NUMERICAL STABILITY FOR                  GSPBL.2230
CDRSM THE NEXT TIME STEP                                                         GSPBL.2231
CDRSM                                                                            GSPBL.2232
CDRSM FIRST, COMPUTE MAXIMUM KZM AND KTH IN THE SURFACE LAYER                    GSPBL.2233
CDRSM WHERE EXPLICIT DIFFUSION COMPUTATIONS ARE PERFORMED                        GSPBL.2234
      IF(MITERING.NE.0)THEN                                                      GSPBL.2235
#ifndef MPP1                                                                     25JAN00.660
        XKMAXM=-999.                                                             GSPBL.2236
        XKMAXH=-999.                                                             GSPBL.2237
        DO I=IBGN,IEND                                                           GSPBL.2238
          XKMAXM=AMAX1(XKMAXM,KZM(I,KL))                                         GSPBL.2239
          XKMAXH=AMAX1(XKMAXH,KTH(I,KL))                                         GSPBL.2240
        ENDDO                                                                    GSPBL.2241
        XKMAX=AMAX1(XKMAXM,XKMAXH)                                               GSPBL.2242
        XDTM=XMITPAR*DZQ(ILX/2,KL)*DZQ(ILX/2,KL)/XKMAX                           GSPBL.2243
        IF(XDTM.LE.DT)THEN                                                       GSPBL.2244
          MSTEPSS(J)=NINT(DT/XDTM)                                               GSPBL.2245
        ELSE                                                                     GSPBL.2246
          MSTEPSS(J)=1                                                           GSPBL.2247
        ENDIF                                                                    GSPBL.2248
        IF(MSTEPSS(J).GT.50.)THEN                                                GSPBL.2249
           PRINT*,' YOUR MSTEPS ARE TOO LARGE: MSTEPS = ',MSTEPSS(J),            GSPBL.2250
     *           ' AT J = ',J,', AND XTIME = ',XTIME,' ON INEST = ',IN           GSPBL.2251
           PRINT*,' YOUR SURFACE-LAYER (K=KL) KZM OR KTH IS TOO LARGE',          GSPBL.2252
     *           ' OR YOUR SURFACE LAYER DEPTH (DZQ) IS TOO SMALL...'            GSPBL.2253
          STOP 50                                                                GSPBL.2254
        ENDIF                                                                    GSPBL.2255
#else                                                                            25JAN00.661
        XKMAX=AMAX1(KZM(NOFLIC(I),KL),KTH(NOFLIC(I),KL))                         25JAN00.662
        XDTM=XMITPAR*DZQ(NOFLIC(I),KL)*DZQ(NOFLIC(I),KL)/XKMAX                   25JAN00.663
        IF(XDTM.LE.DT)THEN                                                       25JAN00.664
          MSTEPSS(NOFLIC(I),J)=NINT(DT/XDTM)                                     25JAN00.665
        ELSE                                                                     25JAN00.666
          MSTEPSS(NOFLIC(I),J)=1                                                 25JAN00.667
        ENDIF                                                                    25JAN00.668
        IF(MSTEPSS(NOFLIC(I),J).GT.50.)THEN                                      25JAN00.669
           PRINT*,' YOUR MSTEPS ARE TOO LARGE: MSTEPS = ',                       25JAN00.670
     *            MSTEPSS(NOFLIC(I),J),                                          25JAN00.671
     *           ' AT J = ',J,', AND XTIME = ',XTIME,' ON INEST = ',IN           25JAN00.672
           PRINT*,' YOUR SURFACE-LAYER (K=KL) KZM OR KTH IS TOO LARGE',          25JAN00.673
     *           ' OR YOUR SURFACE LAYER DEPTH (DZQ) IS TOO SMALL...'            25JAN00.674
          STOP 50                                                                25JAN00.675
        ENDIF                                                                    25JAN00.676
#endif                                                                           25JAN00.677
      ENDIF                                                                      GSPBL.2256
CDRSM                                                                            GSPBL.2257
C                                                                                GSPBL.2258
C                                                                                GSPBL.2259
C 24. RESULTANT GROUND TEMPERATURE                                               GSPBL.2260
C                                                                                GSPBL.2261
C     APPLY ASSELIN FILTER TO TGD FOR LARGE TIME STEP                            GSPBL.2262
C                                                                                GSPBL.2263
C                                                                                GSPBL.2264
      IF(ISOIL.LE.1)THEN                                                         GSPBL.2265
        IF(ITGFLG(IN).EQ.1)THEN                                                  GSPBL.2266
          DO I=1,ILX                                                             GSPBL.2267
            TGC=TGB(I,J)                                                         GSPBL.2268
            TGB(I,J)=OMUHF*TGA(I,J)+GNUHF*(TGC+TGDSB(I))                         GSPBL.2269
            TGA(I,J)=TGC                                                         GSPBL.2270
            IF(ISOIL.EQ.1)THEN                                                   GSPBL.2271
              DO L=1,5                                                           GSPBL.2272
                TSLC=TSLB(I,J,L)                                                 GSPBL.2273
                TSLB(I,J,L)=OMUHF*TSLA(I,J,L)+GNUHF*(TSLC+TSLDSB(I,L))           GSPBL.2274
                TSLA(I,J,L)=TSLC                                                 GSPBL.2275
              ENDDO                                                              GSPBL.2276
            ENDIF                                                                GSPBL.2277
          ENDDO                                                                  GSPBL.2278
        ENDIF                                                                    GSPBL.2279
C                                                                                GSPBL.2280
      ENDIF                                                                      GSPBL.2289
C                                                                                GSPBL.2290
C                                                                                GSPBL.2291
C                                                                                GSPBL.2292
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.2293
C                                                                                GSPBL.2294
C                                                                                GSPBL.2295
C 25. SAVING KPBL IN KPBL2D                                                      GSPBL.2296
C                                                                                GSPBL.2297
C     KPBL IS NEEDED FOR THE FDDA, AND SINCE THERE IS NO LONGER JUST ONE         GSPBL.2298
C     LARGE "J LOOP" IT MUST BE STORED AS (I,J)...                               GSPBL.2299
C                                                                                GSPBL.2300
      DO I=1,ILX                                                                 GSPBL.2301
        KPBL2D(I,J)=KPBL(I)                                                      GSPBL.2302
      ENDDO                                                                      GSPBL.2303
#ifdef MPP1                                                                      25JAN00.678
CMPP1      FLIC_ENDDO                                                            25JAN00.679
#endif                                                                           25JAN00.680
C                                                                                GSPBL.2304
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.2305
C                                                                                GSPBL.2306
C                                                                                GSPBL.2307
C 26. VISIBILITY COMPUTATION                                                     GSPBL.2308
C                                                                                GSPBL.2309
C                                                                                GSPBL.2310
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.2311
C                                                                                GSPBL.2312
C                                                                                GSPBL.2313
C 27. FILE OUTPUT SECTION                                                        GSPBL.2314
C                                                                                GSPBL.2315
C                                                                                GSPBL.2316
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.2317
C                                                                                GSPBL.2318
C                                                                                GSPBL.2319
C 28. RETURN                                                                     GSPBL.2320
C                                                                                GSPBL.2321
      RETURN                                                                     GSPBL.2322
      END                                                                        GSPBL.2323
                                                                                 GSPBL.2324
CCCCCCCCCCCCCCCCCCCCCC END OF SUBROUTINE GSPBL CCCCCCCCCCCCCCCCCCCCCCCCC         GSPBL.2325
