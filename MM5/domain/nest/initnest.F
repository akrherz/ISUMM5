      SUBROUTINE INITNEST(IYY,JXX,KZZ,IYYN,JXXN,KZZN,                            INITNEST.1
     1        IYYM,JXXM,KZZM,IYYNM,JXXNM,KZZNM,NESTII,NESTJJ,                    INITNEST.2
     2        NESCOU,HTNO,INSTES,ISOUTHO,JWESTO)                                 INITNEST.3
      IMPLICIT NONE                                                              INITNEST.4
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         INITNEST.5
C                                                                                INITNEST.6
C     CHANGES:                                                                   INITNEST.7
C       ADD IOVERW=2 OPTION (TERRAIN ADJUSTMENT FOR NEW NEST)                    INITNEST.8
C                                                                                INITNEST.9
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         INITNEST.10
#     include <defines.incl>                                                     INITNEST.11
#     include <parame.incl>                                                      INITNEST.12
#     include <pmoist.incl>                                                      INITNEST.13
#     include <param2.incl>                                                      INITNEST.14
#     include <parfdda.incl>                                                     INITNEST.15
#     include <param3.incl>                                                      INITNEST.16
#     include <addr0.incl>                                                       INITNEST.17
#undef VECTOR
#     include <varia.incl>                                                       INITNEST.18
#     include <point3d.incl>                                                     INITNEST.19
#     include <point2d.incl>                                                     INITNEST.20
#     include <navypb.incl>                                                      INITNEST.21
#     include <various.incl>                                                     INITNEST.22
#     include <fddagd.incl>                                                      INITNEST.23
#     include <fddaob.incl>                                                      INITNEST.24
#     include <dusolve1.incl>                                                    INITNEST.25
#     include <soil.incl>                                                        INITNEST.26
#     include <nestl.incl>                                                       INITNEST.27
#     include <point3dn.incl>                                                    INITNEST.28
#     include <point2dn.incl>                                                    INITNEST.29
#     include <variousn.incl>                                                    INITNEST.30
#     include <nonhyd.incl>                                                      INITNEST.31
#     include <nhcnst.incl>                                                      INITNEST.32
#     include <nnnhyd.incl>                                                      INITNEST.33
#     include <nncnst.incl>                                                      INITNEST.34
#     include <pnavyn.incl>                                                      INITNEST.35
#     include <addrcun.incl>                                                     INITNEST.36
#     include <soiln.incl>                                                       INITNEST.37
#     include <soilp.incl>                                                       INITNEST.38
#     include <soilpn.incl>                                                      INITNEST.39
#     include <rpstar.incl>                                                      INITNEST.40
#     include <rpstarn.incl>                                                     INITNEST.41
#     include <uprad.incl>                                                       INITNEST.42
#ifdef MPP1                                                                      INITNEST.43
#     include <radiat.incl>                                                      INITNEST.44
#endif                                                                           INITNEST.46
#     include <pbltke.incl>                                                      INITNEST.47
#     include <pbltken.incl>                                                     INITNEST.48
#     include <soilcnst.incl>                                                    25JAN00.185
#     include <paramveg_USGS.incl>                                               19DEC02.969
                                                                                 INITNEST.49
      INTEGER IYY,JXX,KZZ,IYYN,JXXN,KZZN                                         INITNEST.50
      INTEGER IYYM,JXXM,KZZM,IYYNM,JXXNM,KZZNM,NESTII,NESTJJ                     INITNEST.51
      INTEGER NESCOU,INSTES,ISOUTHO,JWESTO                                       INITNEST.52
      EXTERNAL DM_IONODE                                                         19DEC02.970
      LOGICAL  DM_IONODE                                                         19DEC02.971
      INTEGER  IER                                                               19DEC02.972
C                                                                                INITNEST.53
      INTEGER I,J,K,L,N,JN,IN,NS,NN,II,JJ                                        INITNEST.54
      INTEGER IDLEV,ICM,JCM                                                      24SEP99.296
      INTEGER NIJ,MAXJ,MINJ,MAXI,MINI                                            INITNEST.56
      INTEGER LPLOOP,IC,JC,IUNITH,IHEADER,IUNIT,ICUT                             INITNEST.57
      INTEGER ISTOP,IS,ISML,JSML,NV,NOC,NOV                                      24SEP99.297
      REAL    AC,B,ALNP,PS0NEW,PSANEW,PISO,PR0NEW,T0NEW,DT0                      INITNEST.59
      REAL    TVMASS,TCMASS,TRMASS,WTJ,WTI                                       INITNEST.60
      REAL    AMAXROT,AMINROT,XLATD,DLONG,DLAT,ROTANG,DMAXROT,DMINROT            INITNEST.61
      REAL    SHCLND,SNOALB,ALBBRD,SHDFAC,SHDMIN,SNCOVR                          19DEC02.973
                                                                                 INITNEST.63
#ifndef MPP1                                                                     INITNEST.64
      REAL      HSC3(MIX,MJX)                                                    INITNEST.65
      REAL      HSCR1M(MIX,MJX),HSCR2M(MIX,MJX)                                  INITNEST.66
      REAL      HSCR3M(MIX,MJX),HSCR4M(MIX,MJX)                                  INITNEST.67
      REAL      HSCR5M(MIX,MJX),HSCR6M(MIX,MJX)                                  INITNEST.68
      REAL      HSCR5NM(MIX,MJX),HSCR6NM(MIX,MJX)                                INITNEST.69
      REAL      HELP5N(MIX,MJX),HELP6N(MIX,MJX)                                  INITNEST.70
      REAL      HSCR1N(MIX,MJX),HSCR2N(MIX,MJX)                                  INITNEST.71
      REAL      HSCR3N(MIX,MJX),HSCR4N(MIX,MJX)                                  INITNEST.72
      REAL      HSCR1NM(MIX,MJX),HSCR2NM(MIX,MJX)                                INITNEST.73
      REAL      HSCR3NM(MIX,MJX),HSCR4NM(MIX,MJX)                                INITNEST.74
      REAL      SLP(MIX,MJX),SLPN(MIX,MJX),RAINNP(MIX,MJX)                       INITNEST.75
      REAL      HELP1(MIX,MJX),HELP2(MIX,MJX),                                   INITNEST.76
     1          HELP1N(MIX,MJX),HELP2N(MIX,MJX),                                 INITNEST.77
     1          HELP3N(MIX,MJX),HELP4N(MIX,MJX)                                  INITNEST.78
      REAL      HTNO(IYYN,JXXN)  ! THIS IS A DUMMY ARGUMENT                      INITNEST.79
      REAL      PSBNO1(MIX,MJX),PSBNO2(MIX,MJX),SLPON(MIX,MJX)                   INITNEST.80
#else                                                                            INITNEST.81
#  include <mpp_initnest_10.incl>                                                INITNEST.82
#endif                                                                           INITNEST.83
      INTEGER   ISTATE(2)                                                        INITNEST.84
C                                                                                INITNEST.85
#     include <landuse.incl>                                                     INITNEST.86
      PRINT9000                                                                  INITNEST.87
      PRINT1005,NESCOU                                                           INITNEST.88
 1005 FORMAT('***************  WELCOME TO NEST ',I1,'  ***************')         INITNEST.89
      PRINT9000                                                                  INITNEST.90
      NUMNES=NESCOU                                                              INITNEST.91
#ifdef MPP1                                                                      INITNEST.92
#  include <mpp_initnest_20.incl>                                                INITNEST.93
#endif                                                                           INITNEST.94
C                                                                                INITNEST.95
C--- ADJUST NEXT LEVEL NESTS CORNER POINTS, IF THIS IS MOVING NEST               INITNEST.96
C                                                                                INITNEST.97
      IF(INSTES.EQ.11)THEN                                                       INITNEST.98
        IDLEV=LEVIDN(NUMNES)+1                                                   INITNEST.99
        PRINT *,'ADJUSTING NESTS ON LEVEL ',IDLEV                                INITNEST.100
        DO 997 L=NESCOU+1,NUMLV(IDLEV,NUMNC(NESCOU))                             INITNEST.101
          CALL ADDRX1N(IAXALL(1,L))                                              INITNEST.102
          CALL SUBCH(NESCOU,L,NESTII,NESTJJ,ISOUTHO,JWESTO)                      INITNEST.103
  997   CONTINUE                                                                 INITNEST.104
        CALL ADDRX1N(IAXALL(1,NESCOU))                                           INITNEST.105
        INSTES=1                                                                 INITNEST.106
      ENDIF                                                                      INITNEST.107
C                                                                                INITNEST.114
C--- VARIOUS INITIALIZATIONS OF THE GUTS OF THE NEST                             INITNEST.115
C                                                                                INITNEST.116
      IBOUDY(NUMNES)=2                                                           INITNEST.117
      DXN=DX/FLOAT(IRAX)                                                         INITNEST.118
      KTAUN=0                                                                    INITNEST.119
      XTIMEN=XTIME                                                               INITNEST.120
      ITAPCNTN=ITAPCNT                                                           INITNEST.121
      JULDAN=JULDAY                                                              INITNEST.122
      GMTN=GMT                                                                   INITNEST.123
      INL=IYYN                                                                   INITNEST.124
      JNL=JXXN                                                                   INITNEST.125
      KNL=KZZN                                                                   INITNEST.126
      IF(KNL.NE.KL)STOP 'KNL'                                                    INITNEST.127
      KNLP1=KNL+1                                                                INITNEST.128
      KNLM=KNL-1                                                                 INITNEST.129
      INLX=INL-1                                                                 INITNEST.130
      INLXM=INLX-1                                                               INITNEST.131
      JNLX=JNL-1                                                                 INITNEST.132
      JNLXM=JNLX-1                                                               INITNEST.133
      JWEST=NESTJJ                                                               INITNEST.134
      ISOUTH=NESTII                                                              INITNEST.135
      JEAST=JWEST+(JNL-1)/IRAX                                                   INITNEST.136
      INORTH=ISOUTH+(INL-1)/IRAX                                                 INITNEST.137
C     CHECK NEST SIZE                                                            INITNEST.138
      IF(ISOUTH-5.LT.1)PRINT*,'NEST TOO CLOSE TO SOUTH BOUNDARY'                 INITNEST.139
      IF(INORTH+5.GT.IYY)PRINT*,'NEST TOO CLOSE TO NORTH BOUNDARY'               INITNEST.140
      IF(JWEST-5.LT.1)PRINT*,'NEST TOO CLOSE TO WEST BOUNDARY'                   INITNEST.141
      IF(JEAST+5.GT.JXX)PRINT*,'NEST TOO CLOSE TO EAST BOUNDARY'                 INITNEST.142
      IF(MOD(INL,3).NE.1)PRINT*,'NEST I DIMENSION SHOULD BE 3N+1'                INITNEST.143
      IF(MOD(JNL,3).NE.1)PRINT*,'NEST J DIMENSION SHOULD BE 3N+1'                INITNEST.144
      IF(ISOUTH-5.LT.1.OR.INORTH+5.GT.IYY.OR.JWEST-5.LT.1.OR.JEAST+5.GT.         INITNEST.145
     +  JXX.OR.MOD(INL,3).NE.1.OR.MOD(JNL,3).NE.1)STOP 'NEST SIZE'               INITNEST.146
      JEP1=JEAST+1                                                               INITNEST.147
      JEP2=JEAST+2                                                               INITNEST.148
      JEM1=JEAST-1                                                               INITNEST.149
      JEM2=JEAST-2                                                               INITNEST.150
      JWP1=JWEST+1                                                               INITNEST.151
      JWP2=JWEST+2                                                               INITNEST.152
      JWM1=JWEST-1                                                               INITNEST.153
      JWM2=JWEST-2                                                               INITNEST.154
      ISP1=ISOUTH+1                                                              INITNEST.155
      ISP2=ISOUTH+2                                                              INITNEST.156
      ISM1=ISOUTH-1                                                              INITNEST.157
      ISM2=ISOUTH-2                                                              INITNEST.158
      INP1=INORTH+1                                                              INITNEST.159
      INP2=INORTH+2                                                              INITNEST.160
      INM1=INORTH-1                                                              INITNEST.161
      INM2=INORTH-2                                                              INITNEST.162
      DXN2=2.*DXN                                                                INITNEST.163
      DXN4=4.*DXN                                                                INITNEST.164
      DXN8=8.*DXN                                                                INITNEST.165
      DXN16=16.*DXN                                                              INITNEST.166
      DXNSQ=DXN*DXN                                                              INITNEST.167
      C200N=KARMAN*KARMAN*DXN/4.                                                 INITNEST.168
      C201N=(100.-PTOP)/DXNSQ                                                    INITNEST.169
      C203N=1./DXNSQ                                                             INITNEST.170
      DTN=.5*DT2/IRAX                                                            INITNEST.171
      DTN2=2.*DTN                                                                INITNEST.172
      DTMINN=DTN/60.                                                             INITNEST.173
      NTRAD(NUMNES)=NINT(RADFRQ/DTMINN)                                          INITNEST.174
C     XKHZN=3.E-3*DXNSQ/DTN                                                      19DEC02.974
      XKHZN=CKH*DXN                                                              19DEC02.975
      XKHMAN=2.*DXNSQ/(64.*DTN)                                                  INITNEST.176
#ifdef MPP1                                                                      INITNEST.177
#  include <mpp_initnest_30.incl>                                                INITNEST.178
#else                                                                            INITNEST.179
C                                                                                INITNEST.180
C--- FOR OTHER NESTS AT SAME LEVEL AND TIME.....                                 INITNEST.181
C                                                                                INITNEST.182
      IF(INSTES.EQ.1)THEN                                                        INITNEST.183
cmic$ do all autoscope                                                           INITNEST.184
c$omp parallel do default(shared)                                                INITNEST.185
c$omp&private(i,j)                                                               INITNEST.186
        DO 9 J=1,JNLX                                                            INITNEST.187
          DO 9 I=1,INLX                                                          INITNEST.188
            HTNO(I,J)=HTN(I,J)                                                   INITNEST.189
            PSBNO1(I,J)=PSAN(I,J)                                                INITNEST.190
    9     CONTINUE                                                               INITNEST.191
                                                                                 INITNEST.192
      ENDIF                                                                      INITNEST.193
C                                                                                INITNEST.194
C--- ANY OVERLAYS?                                                               INITNEST.195
C                                                                                INITNEST.196
      IF(MAXNES.GT.2)CALL OVLCHK(NUMNES)                                         INITNEST.197
      CALL FILL(HT,HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                            INITNEST.198
      CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)                 INITNEST.199
      CALL FILL(HSCR1N,HTN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)                      INITNEST.200
cmic$ do all autoscope                                                           INITNEST.201
c$omp parallel do default(shared)                                                INITNEST.202
c$omp&private(i,j,icm,jcm)                                                       INITNEST.203
      DO 591 J=1,JXXN-1                                                          INITNEST.204
        DO 591 I=1,IYYN-1                                                        INITNEST.205
          ICM=(I-1)/3+ISOUTH                                                     INITNEST.206
          JCM=(J-1)/3+JWEST                                                      INITNEST.207
          SATBRN(I,J)=SATBRT(ICM,JCM)                                            INITNEST.208
  591   CONTINUE                                                                 INITNEST.209
                                                                                 INITNEST.210
C                                                                                INITNEST.211
      CALL FILL(PSA,HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                           INITNEST.212
      CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)                 INITNEST.213
      CALL FILL(HSCR1N,PSBN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)                     INITNEST.214
      IF(INSTES.LE.0)THEN                                                        INITNEST.215
        IF(IMOIST(NUMNES).EQ.2)THEN                                              INITNEST.216
cmic$ do all autoscope                                                           INITNEST.217
c$omp parallel do default(shared)                                                INITNEST.218
c$omp&private(i,j,k)                                                             INITNEST.219
          DO 100 J=1,JNL                                                         INITNEST.220
            DO 100 K=1,KNL                                                       INITNEST.221
              DO 100 I=1,INL                                                     INITNEST.222
                QCBN(I,J,K)=0.                                                   INITNEST.223
                QRBN(I,J,K)=0.                                                   INITNEST.224
                QRAN(I,J,K)=0.                                                   INITNEST.225
                QCAN(I,J,K)=0.                                                   INITNEST.226
  100         CONTINUE                                                           INITNEST.227
                                                                                 INITNEST.228
                                                                                 INITNEST.229
          IF(IICE.EQ.1)THEN                                                      INITNEST.230
cmic$ do all autoscope                                                           INITNEST.231
c$omp parallel do default(shared)                                                INITNEST.232
c$omp&private(i,j,k)                                                             INITNEST.233
            DO 101 J=1,JNL                                                       INITNEST.234
              DO 101 K=1,KNL                                                     INITNEST.235
                DO 101 I=1,INL                                                   INITNEST.236
                  QIBN(I,J,K)=0.                                                 INITNEST.237
                  QNIBN(I,J,K)=0.                                                INITNEST.238
                  QIAN(I,J,K)=0.                                                 INITNEST.239
                  QNIAN(I,J,K)=0.                                                INITNEST.240
                  IF(IICEG.EQ.1)THEN                                             INITNEST.241
                    QGBN(I,J,K)=0.                                               INITNEST.242
                    QNCBN(I,J,K)=0.                                              INITNEST.243
                    QGAN(I,J,K)=0.                                               INITNEST.244
                    QNCAN(I,J,K)=0.                                              INITNEST.245
                  ENDIF                                                          INITNEST.246
  101           CONTINUE                                                         INITNEST.247
                                                                                 INITNEST.248
                                                                                 INITNEST.249
          ENDIF                                                                  INITNEST.250
        ENDIF                                                                    INITNEST.251
      ENDIF                                                                      INITNEST.252
C                                                                                INITNEST.253
C*********** NOW INTERPOLATE REST OF DOMAIN *****************                    INITNEST.254
C                                                                                INITNEST.255
C--- U AND V                                                                     INITNEST.256
C                                                                                INITNEST.257
      CALL DOTS(PSA,RAINNP,IYY,JXX,MIX,MJX)                                      INITNEST.258
      CALL DOTS(PSBN,PSAN,IYYN,JXXN,MIX,MJX)                                     INITNEST.259
      IF(INSTES.EQ.1)CALL DOTS(PSBNO1,PSBNO2,IYYN,JXXN,MIX,MJX)                  INITNEST.260
cmic$ do all autoscope                                                           INITNEST.261
cmic$1 private (help1,help2,hscr1n,hscr2n,help1n,help2n)                         INITNEST.262
cmic$2 shared (isoutho,jwesto,iyy,jxx,iyyn,jxxn,isouth,jwest,irax)               INITNEST.263
c$omp parallel do default(shared)                                                INITNEST.264
c$omp&private(k,i,j,help1,help2,hscr1n,hscr2n,help1n,help2n)                     INITNEST.265
c$omp&shared(iyy,jxx,iyyn,jxxn,isoutho,jwesto)                                   INITNEST.266
      DO 210 K=1,KZZ                                                             INITNEST.267
        DO 200 J=1,JXX                                                           INITNEST.268
          DO 200 I=1,IYY                                                         INITNEST.269
            HELP1(I,J)=UA(I,J,K)/RAINNP(I,J)                                     INITNEST.270
            HELP2(I,J)=VA(I,J,K)/RAINNP(I,J)                                     INITNEST.271
  200     CONTINUE                                                               INITNEST.272
                                                                                 INITNEST.273
        CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,0)               INITNEST.274
        CALL EXAINT(HELP2,IYY,JXX,HSCR2N,IYYN,JXXN,ISOUTH,JWEST,0)               INITNEST.275
        IF(INSTES.EQ.1)THEN                                                      INITNEST.276
          DO 202 J=1,JXXN                                                        INITNEST.277
            DO 202 I=1,IYYN                                                      INITNEST.278
              HELP1N(I,J)=UAN(I,J,K)/PSBNO2(I,J)                                 INITNEST.279
              HELP2N(I,J)=VAN(I,J,K)/PSBNO2(I,J)                                 INITNEST.280
  202       CONTINUE                                                             INITNEST.281
                                                                                 INITNEST.282
          CALL FILSLB(HSCR1N,HELP1N,ISOUTH,ISOUTHO,JWEST,JWESTO,IYYN,            INITNEST.283
     +         JXXN,0)                                                           INITNEST.284
          CALL FILSLB(HSCR2N,HELP2N,ISOUTH,ISOUTHO,JWEST,JWESTO,IYYN,            INITNEST.285
     +         JXXN,0)                                                           INITNEST.286
        ENDIF                                                                    INITNEST.287
        DO 205 J=1,JXXN                                                          INITNEST.288
          DO 205 I=1,IYYN                                                        INITNEST.289
            UAN(I,J,K)=HSCR1N(I,J)*PSAN(I,J)                                     INITNEST.290
            VAN(I,J,K)=HSCR2N(I,J)*PSAN(I,J)                                     INITNEST.291
            UBN(I,J,K)=HSCR1N(I,J)*PSAN(I,J)                                     INITNEST.292
            VBN(I,J,K)=HSCR2N(I,J)*PSAN(I,J)                                     INITNEST.293
  205     CONTINUE                                                               INITNEST.294
                                                                                 INITNEST.295
  210 CONTINUE                                                                   INITNEST.296
C                                                                                INITNEST.297
C--- TEMPERATURE AND MOISTURE                                                    INITNEST.298
C                                                                                INITNEST.299
cmic$ do all autoscope                                                           INITNEST.300
cmic$1 private (help1,help2,hscr1n,hscr2n,help1n,help2n)                         INITNEST.301
cmic$2 shared (isoutho,jwesto,iyy,jxx,iyyn,jxxn,isouth,jwest,irax)               INITNEST.302
c$omp parallel do default(shared)                                                INITNEST.303
c$omp&private(k,i,j,help1,help2,hscr1n,hscr2n,help1n,help2n)                     INITNEST.304
c$omp&shared(iyy,jxx,iyyn,jxxn,isoutho,jwesto)                                   INITNEST.305
      DO 260 K=1,KZZ                                                             INITNEST.306
        DO 250 J=1,JXX-1                                                         INITNEST.307
          DO 250 I=1,IYY-1                                                       INITNEST.308
            HELP1(I,J)=TA(I,J,K)/PSA(I,J)                                        INITNEST.309
  250     CONTINUE                                                               INITNEST.310
                                                                                 INITNEST.311
        CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)               INITNEST.312
C                                                                                INITNEST.313
        IF(IDRY(NUMNES).EQ.0)THEN                                                INITNEST.314
          DO 251 J=1,JXX-1                                                       INITNEST.315
            DO 251 I=1,IYY-1                                                     INITNEST.316
              HELP2(I,J)=QVA(I,J,K)/PSA(I,J)                                     INITNEST.317
  251       CONTINUE                                                             INITNEST.318
                                                                                 INITNEST.319
          CALL EXAINT(HELP2,IYY,JXX,HSCR2N,IYYN,JXXN,ISOUTH,JWEST,1)             INITNEST.320
        ENDIF                                                                    INITNEST.321
C                                                                                INITNEST.322
        IF(INSTES.EQ.1)THEN                                                      INITNEST.323
          DO 252 J=1,JXXN-1                                                      INITNEST.324
            DO 252 I=1,IYYN-1                                                    INITNEST.325
              HELP1N(I,J)=TAN(I,J,K)/PSBNO1(I,J)                                 INITNEST.326
  252       CONTINUE                                                             INITNEST.327
                                                                                 INITNEST.328
          CALL FILSLB(HSCR1N,HELP1N,ISOUTH,ISOUTHO,JWEST,JWESTO,IYYN,            INITNEST.329
     +         JXXN,1)                                                           INITNEST.330
          IF(IDRY(NUMNES).EQ.0)THEN                                              INITNEST.331
            DO 253 J=1,JXXN-1                                                    INITNEST.332
              DO 253 I=1,IYYN-1                                                  INITNEST.333
                HELP2N(I,J)=QVAN(I,J,K)/PSBNO1(I,J)                              INITNEST.334
  253         CONTINUE                                                           INITNEST.335
                                                                                 INITNEST.336
            CALL FILSLB(HSCR2N,HELP2N,ISOUTH,ISOUTHO,JWEST,JWESTO,IYYN,          INITNEST.337
     +           JXXN,1)                                                         INITNEST.338
          ENDIF                                                                  INITNEST.339
        ENDIF                                                                    INITNEST.340
C                                                                                INITNEST.341
        DO 255 J=1,JXXN-1                                                        INITNEST.342
          DO 255 I=1,IYYN-1                                                      INITNEST.343
            TAN(I,J,K)=HSCR1N(I,J)*PSBN(I,J)                                     INITNEST.344
            TBN(I,J,K)=HSCR1N(I,J)*PSBN(I,J)                                     INITNEST.345
  255     CONTINUE                                                               INITNEST.346
                                                                                 INITNEST.347
C                                                                                INITNEST.348
        IF(IDRY(NUMNES).EQ.0)THEN                                                INITNEST.349
          DO 256 J=1,JXXN-1                                                      INITNEST.350
            DO 256 I=1,IYYN-1                                                    INITNEST.351
              IF(HSCR2N(I,J).LE.0.)HSCR2N(I,J)=1.E-10                            INITNEST.352
              QVAN(I,J,K)=HSCR2N(I,J)*PSBN(I,J)                                  INITNEST.353
              QVBN(I,J,K)=HSCR2N(I,J)*PSBN(I,J)                                  INITNEST.354
  256       CONTINUE                                                             INITNEST.355
                                                                                 INITNEST.356
        ENDIF                                                                    INITNEST.357
C                                                                                INITNEST.358
  260 CONTINUE                                                                   INITNEST.359
C                                                                                INITNEST.360
C--- LEVEL 3 CLOSURE PBL SCHEME                                                  INITNEST.361
C                                                                                INITNEST.362
      IF(INAV.EQ.1)THEN                                                          INITNEST.363
cmic$ do all autoscope                                                           INITNEST.364
cmic$1 private (help1,help2,hscr1n,hscr2n,help2n,help1n)                         INITNEST.365
cmic$2 shared (isoutho,jwesto,iyy,jxx,iyyn,jxxn,isouth,jwest,irax)               INITNEST.366
c$omp parallel do default(shared)                                                INITNEST.367
c$omp&private(k,i,j,help1,help2,hscr1n,hscr2n,help2n,help1n)                     INITNEST.368
        DO 264 K=1,KZZ                                                           INITNEST.369
          DO 261 J=1,JXX-1                                                       INITNEST.370
            DO 261 I=1,IYY-1                                                     INITNEST.371
              HELP1(I,J)=TURKE(I,J,K)                                            INITNEST.372
              HELP2(I,J)=PQWSB(I,J,K)                                            INITNEST.373
  261       CONTINUE                                                             INITNEST.374
                                                                                 INITNEST.375
          CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)             INITNEST.376
          CALL EXAINT(HELP2,IYY,JXX,HSCR2N,IYYN,JXXN,ISOUTH,JWEST,1)             INITNEST.377
C                                                                                INITNEST.378
          IF(INSTES.EQ.1)THEN                                                    INITNEST.379
            DO 262 J=1,JXXN-1                                                    INITNEST.380
              DO 262 I=1,IYYN-1                                                  INITNEST.381
                HELP1N(I,J)=TURKEN(I,J,K)                                        INITNEST.382
                HELP2N(I,J)=PQWSBN(I,J,K)                                        INITNEST.383
  262         CONTINUE                                                           INITNEST.384
                                                                                 INITNEST.385
            CALL FILSLB(HSCR1N,HELP1N,ISOUTH,ISOUTHO,JWEST,JWESTO,IYYN,          INITNEST.386
     +           JXXN,1)                                                         INITNEST.387
            CALL FILSLB(HSCR2N,HELP2N,ISOUTH,ISOUTHO,JWEST,JWESTO,IYYN,          INITNEST.388
     +           JXXN,1)                                                         INITNEST.389
          ENDIF                                                                  INITNEST.390
C                                                                                INITNEST.391
          DO 263 J=1,JXXN-1                                                      INITNEST.392
            DO 263 I=1,IYYN-1                                                    INITNEST.393
              TURKEN(I,J,K)=HSCR1N(I,J)                                          INITNEST.394
              PQWSBN(I,J,K)=HSCR2N(I,J)                                          INITNEST.395
  263       CONTINUE                                                             INITNEST.396
                                                                                 INITNEST.397
C                                                                                INITNEST.398
C                                                                                INITNEST.399
  264   CONTINUE                                                                 INITNEST.400
cmic$ do all autoscope                                                           INITNEST.401
cmic$1 private (help1,help2,hscr1n,hscr2n,help2n,help1n)                         INITNEST.402
cmic$2 shared (isoutho,jwesto,iyy,jxx,iyyn,jxxn,isouth,jwest,irax)               INITNEST.403
c$omp parallel do default(shared)                                                INITNEST.404
c$omp&private(k,i,j,help1,help2,hscr1n,hscr2n,help2n,help1n)                     INITNEST.405
        DO 268 K=1,KZZ                                                           INITNEST.406
          DO 265 J=1,JXX-1                                                       INITNEST.407
            DO 265 I=1,IYY-1                                                     INITNEST.408
              HELP1(I,J)=PTHLSB(I,J,K)                                           INITNEST.409
              HELP2(I,J)=PTHLQWB(I,J,K)                                          INITNEST.410
  265       CONTINUE                                                             INITNEST.411
                                                                                 INITNEST.412
          CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)             INITNEST.413
          CALL EXAINT(HELP2,IYY,JXX,HSCR2N,IYYN,JXXN,ISOUTH,JWEST,1)             INITNEST.414
C                                                                                INITNEST.415
          IF(INSTES.EQ.1)THEN                                                    INITNEST.416
            DO 266 J=1,JXXN-1                                                    INITNEST.417
              DO 266 I=1,IYYN-1                                                  INITNEST.418
                HELP1N(I,J)=PTHLSN(I,J,K)                                        INITNEST.419
                HELP2N(I,J)=PTHLQNB(I,J,K)                                       INITNEST.420
  266         CONTINUE                                                           INITNEST.421
                                                                                 INITNEST.422
            CALL FILSLB(HSCR1N,HELP1N,ISOUTH,ISOUTHO,JWEST,JWESTO,IYYN,          INITNEST.423
     +           JXXN,1)                                                         INITNEST.424
            CALL FILSLB(HSCR2N,HELP2N,ISOUTH,ISOUTHO,JWEST,JWESTO,IYYN,          INITNEST.425
     +           JXXN,1)                                                         INITNEST.426
          ENDIF                                                                  INITNEST.427
C                                                                                INITNEST.428
          DO 267 J=1,JXXN-1                                                      INITNEST.429
            DO 267 I=1,IYYN-1                                                    INITNEST.430
              PTHLSN(I,J,K)=HSCR1N(I,J)                                          INITNEST.431
              PTHLQNB(I,J,K)=HSCR2N(I,J)                                         INITNEST.432
  267       CONTINUE                                                             INITNEST.433
                                                                                 INITNEST.434
C                                                                                INITNEST.435
C                                                                                INITNEST.436
  268   CONTINUE                                                                 INITNEST.437
      ENDIF                                                                      INITNEST.438
C                                                                                INITNEST.439
C                                                                                INITNEST.440
      IF(IMOIST(NUMNES).EQ.2)THEN                                                INITNEST.441
cmic$ do all autoscope                                                           INITNEST.442
cmic$1 private (help1,help2,hscr1n,hscr2n,help2n,help1n,hscr1m,                  INITNEST.443
cmic$1 hscr5m,hscr6m,hscr5nm,hscr6nm,help5n,help6n,                              INITNEST.444
cmic$1 hscr2m,hscr3m,hscr4m,hscr1nm,hscr2nm,hscr3nm,hscr4nm,hsc3,                INITNEST.445
cmic$1 help3n,help4n)                                                            INITNEST.446
cmic$2 shared (isoutho,jwesto,iyy,jxx,iyyn,jxxn,isouth,jwest,irax,               INITNEST.447
cmic$2 iyym,jxxm,iyynm,jxxnm)                                                    INITNEST.448
c$omp parallel do default(shared)                                                INITNEST.449
c$omp&private(k,i,j,help1,help2,hscr1n,hscr2n,help2n,help1n,hscr1m,              INITNEST.450
c$omp&hscr5m,hscr6m,hscr5nm,hscr6nm,help5n,help6n,                               INITNEST.451
c$omp&hscr2m,hscr3m,hscr4m,hscr1nm,hscr2nm,hscr3nm,hscr4nm,hsc3,                 INITNEST.452
c$omp&help3n,help4n)                                                             INITNEST.453
c$omp&shared(iyym,jxxm,iyynm,jxxnm,isoutho,jwesto)                               INITNEST.454
        DO 280 K=1,KZZ                                                           INITNEST.455
          DO 270 J=1,JXXM-1                                                      INITNEST.456
            DO 270 I=1,IYYM-1                                                    INITNEST.457
              HSCR1M(I,J)=QCA(I,J,K)/PSA(I,J)                                    INITNEST.458
              HSCR2M(I,J)=QRA(I,J,K)/PSA(I,J)                                    INITNEST.459
              IF(IICE.EQ.1)THEN                                                  INITNEST.460
                HSCR3M(I,J)=QIA(I,J,K)/PSA(I,J)                                  INITNEST.461
                HSCR4M(I,J)=QNIA(I,J,K)/PSA(I,J)                                 INITNEST.462
                IF(IICEG.EQ.1)THEN                                               INITNEST.463
                  HSCR5M(I,J)=QGA(I,J,K)/PSA(I,J)                                INITNEST.464
                  HSCR6M(I,J)=QNCA(I,J,K)/PSA(I,J)                               INITNEST.465
                ENDIF                                                            INITNEST.466
              ENDIF                                                              INITNEST.467
  270       CONTINUE                                                             INITNEST.468
                                                                                 INITNEST.469
          IF(IICE.EQ.1)THEN                                                      INITNEST.470
            CALL EXAINT(HSCR3M,IYYM,JXXM,HSCR3NM,IYYNM,JXXNM,ISOUTH,             INITNEST.471
     +           JWEST,1)                                                        INITNEST.472
            CALL EXAINT(HSCR4M,IYYM,JXXM,HSCR4NM,IYYNM,JXXNM,ISOUTH,             INITNEST.473
     +           JWEST,1)                                                        INITNEST.474
            IF(IICEG.EQ.1)THEN                                                   INITNEST.475
              CALL EXAINT(HSCR5M,IYYM,JXXM,HSCR5NM,IYYNM,JXXNM,ISOUTH,           INITNEST.476
     +             JWEST,1)                                                      INITNEST.477
              CALL EXAINT(HSCR6M,IYYM,JXXM,HSCR6NM,IYYNM,JXXNM,ISOUTH,           INITNEST.478
     +             JWEST,1)                                                      INITNEST.479
            ENDIF                                                                INITNEST.480
          ENDIF                                                                  INITNEST.481
          CALL EXAINT(HSCR1M,IYYM,JXXM,HSCR1NM,IYYNM,JXXNM,ISOUTH,JWEST,         INITNEST.482
     +         1)                                                                INITNEST.483
          CALL EXAINT(HSCR2M,IYYM,JXXM,HSCR2NM,IYYNM,JXXNM,ISOUTH,JWEST,         INITNEST.484
     +         1)                                                                INITNEST.485
          IF(INSTES.EQ.1)THEN                                                    INITNEST.486
            DO 272 J=1,JXXNM-1                                                   INITNEST.487
              DO 272 I=1,IYYNM-1                                                 INITNEST.488
                HELP1N(I,J)=QCAN(I,J,K)/PSBNO1(I,J)                              INITNEST.489
                HELP2N(I,J)=QRAN(I,J,K)/PSBNO1(I,J)                              INITNEST.490
                IF(IICE.EQ.1)THEN                                                INITNEST.491
                  HELP3N(I,J)=QIAN(I,J,K)/PSBNO1(I,J)                            INITNEST.492
                  HELP4N(I,J)=QNIAN(I,J,K)/PSBNO1(I,J)                           INITNEST.493
                  IF(IICEG.EQ.1)THEN                                             INITNEST.494
                    HELP5N(I,J)=QGAN(I,J,K)/PSBNO1(I,J)                          INITNEST.495
                    HELP6N(I,J)=QNCAN(I,J,K)/PSBNO1(I,J)                         INITNEST.496
                  ENDIF                                                          INITNEST.497
                ENDIF                                                            INITNEST.498
  272         CONTINUE                                                           INITNEST.499
                                                                                 INITNEST.500
            IF(IICE.EQ.1)THEN                                                    INITNEST.501
              CALL FILSLB(HSCR3NM,HELP3N,ISOUTH,ISOUTHO,JWEST,JWESTO,            INITNEST.502
     +             IYYNM,JXXNM,1)                                                INITNEST.503
              CALL FILSLB(HSCR4NM,HELP4N,ISOUTH,ISOUTHO,JWEST,JWESTO,            INITNEST.504
     +             IYYNM,JXXNM,1)                                                INITNEST.505
              IF(IICEG.EQ.1)THEN                                                 INITNEST.506
                CALL FILSLB(HSCR5NM,HELP5N,ISOUTH,ISOUTHO,JWEST,JWESTO,          INITNEST.507
     +               IYYNM,JXXNM,1)                                              INITNEST.508
                CALL FILSLB(HSCR6NM,HELP6N,ISOUTH,ISOUTHO,JWEST,JWESTO,          INITNEST.509
     +               IYYNM,JXXNM,1)                                              INITNEST.510
              ENDIF                                                              INITNEST.511
            ENDIF                                                                INITNEST.512
            CALL FILSLB(HSCR1NM,HELP1N,ISOUTH,ISOUTHO,JWEST,JWESTO,IYYNM         INITNEST.513
     +           ,JXXNM,1)                                                       INITNEST.514
            CALL FILSLB(HSCR2NM,HELP2N,ISOUTH,ISOUTHO,JWEST,JWESTO,IYYNM         INITNEST.515
     +           ,JXXNM,1)                                                       INITNEST.516
          ENDIF                                                                  INITNEST.517
          DO 275 J=1,JXXNM-1                                                     INITNEST.518
            DO 275 I=1,IYYNM-1                                                   INITNEST.519
              IF(HSCR1NM(I,J).LE.0.)HSCR1NM(I,J)=0.                              INITNEST.520
              IF(HSCR2NM(I,J).LE.0.)HSCR2NM(I,J)=0.                              INITNEST.521
              QCAN(I,J,K)=HSCR1NM(I,J)*PSBN(I,J)                                 INITNEST.522
              QRAN(I,J,K)=HSCR2NM(I,J)*PSBN(I,J)                                 INITNEST.523
              QCBN(I,J,K)=HSCR1NM(I,J)*PSBN(I,J)                                 INITNEST.524
              QRBN(I,J,K)=HSCR2NM(I,J)*PSBN(I,J)                                 INITNEST.525
              IF(IICE.EQ.1)THEN                                                  INITNEST.526
                IF(HSCR3NM(I,J).LE.0.)HSCR3NM(I,J)=0.                            INITNEST.527
                IF(HSCR4NM(I,J).LE.0.)HSCR4NM(I,J)=0.                            INITNEST.528
                QIAN(I,J,K)=HSCR3NM(I,J)*PSBN(I,J)                               INITNEST.529
                QNIAN(I,J,K)=HSCR4NM(I,J)*PSBN(I,J)                              INITNEST.530
                QIBN(I,J,K)=HSCR3NM(I,J)*PSBN(I,J)                               INITNEST.531
                QNIBN(I,J,K)=HSCR4NM(I,J)*PSBN(I,J)                              INITNEST.532
                IF(IICEG.EQ.1)THEN                                               INITNEST.533
                  IF(HSCR5NM(I,J).LE.0.)HSCR5NM(I,J)=0.                          INITNEST.534
                  IF(HSCR6NM(I,J).LE.0.)HSCR6NM(I,J)=0.                          INITNEST.535
                  QGAN(I,J,K)=HSCR5NM(I,J)*PSBN(I,J)                             INITNEST.536
                  QGBN(I,J,K)=HSCR5NM(I,J)*PSBN(I,J)                             INITNEST.537
                  QNCAN(I,J,K)=HSCR6NM(I,J)*PSBN(I,J)                            INITNEST.538
                  QNCBN(I,J,K)=HSCR6NM(I,J)*PSBN(I,J)                            INITNEST.539
                ENDIF                                                            INITNEST.540
              ENDIF                                                              INITNEST.541
  275       CONTINUE                                                             INITNEST.542
                                                                                 INITNEST.543
  280   CONTINUE                                                                 INITNEST.544
      ENDIF                                                                      INITNEST.545
C                                                                                INITNEST.546
C--- BEGIN NON-HYDROSTATIC VARIABLES                                             INITNEST.547
C                                                                                INITNEST.548
C                                                                                INITNEST.549
cmic$ do all autoscope                                                           INITNEST.550
cmic$1 private (help1,help2,hscr1n,hscr2n,help2n,help1n)                         INITNEST.551
cmic$2 shared (isoutho,jwesto,iyy,jxx,iyyn,jxxn,isouth,jwest,irax)               INITNEST.552
c$omp parallel do default(shared)                                                INITNEST.553
c$omp&private(k,i,j,help1,help2,hscr1n,hscr2n,help2n,help1n)                     INITNEST.554
c$omp&shared(iyy,jxx,iyyn,jxxn,isoutho,jwesto)                                   INITNEST.555
      DO 285 K=1,KZZ                                                             INITNEST.556
        DO 282 J=1,JXX-1                                                         INITNEST.557
          DO 282 I=1,IYY-1                                                       INITNEST.558
            HELP1(I,J)=T0(I,J,K)                                                 INITNEST.559
            HELP2(I,J)=PPA(I,J,K)/PSA(I,J)                                       INITNEST.560
  282     CONTINUE                                                               INITNEST.561
                                                                                 INITNEST.562
        CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)               INITNEST.563
        CALL EXAINT(HELP2,IYY,JXX,HSCR2N,IYYN,JXXN,ISOUTH,JWEST,1)               INITNEST.564
C                                                                                INITNEST.565
        IF(INSTES.EQ.1)THEN                                                      INITNEST.566
          DO 283 J=1,JXXN-1                                                      INITNEST.567
            DO 283 I=1,IYYN-1                                                    INITNEST.568
              HELP1N(I,J)=T0N(I,J,K)                                             INITNEST.569
              HELP2N(I,J)=PPAN(I,J,K)/PSBNO1(I,J)                                INITNEST.570
  283       CONTINUE                                                             INITNEST.571
                                                                                 INITNEST.572
          CALL FILSLB(HSCR1N,HELP1N,ISOUTH,ISOUTHO,JWEST,JWESTO,IYYN,            INITNEST.573
     +         JXXN,1)                                                           INITNEST.574
          CALL FILSLB(HSCR2N,HELP2N,ISOUTH,ISOUTHO,JWEST,JWESTO,IYYN,            INITNEST.575
     +         JXXN,1)                                                           INITNEST.576
        ENDIF                                                                    INITNEST.577
C                                                                                INITNEST.578
        DO 284 J=1,JXXN-1                                                        INITNEST.579
          DO 284 I=1,IYYN-1                                                      INITNEST.580
            T0N(I,J,K)=HSCR1N(I,J)                                               INITNEST.581
            PPAN(I,J,K)=HSCR2N(I,J)*PSBN(I,J)                                    INITNEST.582
            PPBN(I,J,K)=HSCR2N(I,J)*PSBN(I,J)                                    INITNEST.583
  284     CONTINUE                                                               INITNEST.584
                                                                                 INITNEST.585
C                                                                                INITNEST.586
  285 CONTINUE                                                                   INITNEST.587
C                                                                                INITNEST.588
cmic$ do all autoscope                                                           INITNEST.589
cmic$1 private (help1,help2,hscr1n,hscr2n,help2n,help1n)                         INITNEST.590
cmic$2 shared (isoutho,jwesto,iyy,jxx,iyyn,jxxn,isouth,jwest,irax)               INITNEST.591
c$omp parallel do default(shared)                                                INITNEST.592
c$omp&private(k,i,j,help1,help2,hscr1n,hscr2n,help2n,help1n)                     INITNEST.593
c$omp&shared(iyy,jxx,iyyn,jxxn,isoutho,jwesto)                                   INITNEST.594
      DO 290 K=1,KZZ+1                                                           INITNEST.595
        DO 286 J=1,JXX-1                                                         INITNEST.596
          DO 286 I=1,IYY-1                                                       INITNEST.597
            HELP2(I,J)=WA(I,J,K)/PSA(I,J)                                        INITNEST.598
  286     CONTINUE                                                               INITNEST.599
                                                                                 INITNEST.600
        CALL EXAINT(HELP2,IYY,JXX,HSCR2N,IYYN,JXXN,ISOUTH,JWEST,1)               INITNEST.601
C                                                                                INITNEST.602
        IF(INSTES.EQ.1)THEN                                                      INITNEST.603
          DO 287 J=1,JXXN-1                                                      INITNEST.604
            DO 287 I=1,IYYN-1                                                    INITNEST.605
              HELP2N(I,J)=WAN(I,J,K)/PSBNO1(I,J)                                 INITNEST.606
  287       CONTINUE                                                             INITNEST.607
                                                                                 INITNEST.608
          CALL FILSLB(HSCR2N,HELP2N,ISOUTH,ISOUTHO,JWEST,JWESTO,IYYN,            INITNEST.609
     +         JXXN,1)                                                           INITNEST.610
        ENDIF                                                                    INITNEST.611
C                                                                                INITNEST.612
        DO 288 J=1,JXXN-1                                                        INITNEST.613
          DO 288 I=1,IYYN-1                                                      INITNEST.614
            WAN(I,J,K)=HSCR2N(I,J)*PSBN(I,J)                                     INITNEST.615
            WBN(I,J,K)=HSCR2N(I,J)*PSBN(I,J)                                     INITNEST.616
  288     CONTINUE                                                               INITNEST.617
                                                                                 INITNEST.618
  290 CONTINUE                                                                   INITNEST.619
C                                                                                INITNEST.620
      IF(IBLTYP(NESCOU).EQ.6)THEN                                                INITNEST.621
cmic$ do all autoscope                                                           INITNEST.622
cmic$1 private (help1,help2,hscr1n,hscr2n,help2n,help1n)                         INITNEST.623
cmic$2 shared (isoutho,jwesto,iyy,jxx,iyyn,jxxn,isouth,jwest,irax)               INITNEST.624
        DO 292 K=1,KZZ+1                                                         INITNEST.625
          DO J=1,JXX-1                                                           INITNEST.626
            DO I=1,IYY-1                                                         INITNEST.627
              HELP2(I,J)=TKEA(I,J,K)                                             INITNEST.628
            ENDDO                                                                INITNEST.629
          ENDDO                                                                  INITNEST.630
          CALL EXAINT(HELP2,IYY,JXX,HSCR2N,IYYN,JXXN,ISOUTH,JWEST,1)             INITNEST.631
C                                                                                INITNEST.632
          IF(INSTES.EQ.1)THEN                                                    INITNEST.633
            DO J=1,JXXN-1                                                        INITNEST.634
              DO I=1,IYYN-1                                                      INITNEST.635
                HELP2N(I,J)=TKEAN(I,J,K)                                         INITNEST.636
              ENDDO                                                              INITNEST.637
            ENDDO                                                                INITNEST.638
C                                                                                INITNEST.639
            CALL FILSLB(HSCR2N,HELP2N,ISOUTH,ISOUTHO,JWEST,JWESTO,IYYN,          INITNEST.640
     +           JXXN,1)                                                         INITNEST.641
          ENDIF                                                                  INITNEST.642
          DO J=1,JXXN-1                                                          INITNEST.643
            DO I=1,IYYN-1                                                        INITNEST.644
              TKEAN(I,J,K)=HSCR2N(I,J)                                           INITNEST.645
              TKEBN(I,J,K)=HSCR2N(I,J)                                           INITNEST.646
            ENDDO                                                                INITNEST.647
          ENDDO                                                                  INITNEST.648
C                                                                                INITNEST.649
  292   CONTINUE                                                                 INITNEST.650
      ENDIF                                                                      INITNEST.651
C                                                                                INITNEST.652
C--- 2-D ARRAYS NEXT,FIRST MAPFACTOR                                             INITNEST.653
C                                                                                INITNEST.654
cmic$ do all autoscope                                                           INITNEST.655
cmic$1 private (help1,hscr1n,help2,hscr2n)                                       INITNEST.656
cmic$1 shared (msfx,msfd,msfdn,msfxn,xlat,xlatn,xlong,xlongn,                    INITNEST.657
cmic$1        snowc,snowcn,f,fn,tmn,tmnn,mix,mjx,mkx,iyy,jxx,                    INITNEST.658
cmic$1        iyyn,jxxn,isouth,jwest,tga,tgan,tss,tssn)                          24SEP99.298
c$omp parallel do default(shared)                                                INITNEST.660
c$omp&private(lploop,help1,hscr1n,help2,hscr2n,ic,jc)                            INITNEST.661
c$omp&shared(iyy,jxx,iyyn,jxxn)                                                  INITNEST.662
      DO 404 LPLOOP=1,9                                                          24SEP99.299
        IF(LPLOOP.EQ.1)THEN                                                      INITNEST.664
          CALL FILL(MSFX,HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                      INITNEST.665
          CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)             INITNEST.666
          CALL FILL(HSCR1N,MSFXN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)                INITNEST.667
C                                                                                INITNEST.668
        ELSEIF(LPLOOP.EQ.2)THEN                                                  INITNEST.669
          CALL FILL(MSFD,HELP2,MIX,MJX,IYY,JXX,IYY,JXX)                          INITNEST.670
          CALL EXAINT(HELP2,IYY,JXX,HSCR2N,IYYN,JXXN,ISOUTH,JWEST,0)             INITNEST.671
          CALL FILL(HSCR2N,MSFDN,IYYN,JXXN,MIX,MJX,IYYN,JXXN)                    INITNEST.672
C                                                                                INITNEST.673
C--- LAT/LON                                                                     INITNEST.674
C                                                                                INITNEST.675
        ELSEIF(LPLOOP.EQ.3)THEN                                                  INITNEST.676
          CALL FILL(XLAT,HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                      INITNEST.677
          CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)             INITNEST.678
          CALL FILL(HSCR1N,XLATN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)                INITNEST.679
C                                                                                INITNEST.680
        ELSEIF(LPLOOP.EQ.4)THEN                                                  INITNEST.681
          CALL FILL(XLONG,HELP2,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                     INITNEST.682
C     DATE-LINE HANDLING MODIFICATION                                            INITNEST.683
          DO J=1,JXX-1                                                           INITNEST.684
            DO I=1,IYY-1                                                         INITNEST.685
C     HELP1 IS WELL-BEHAVED NEAR DATE-LINE, HELP2 NEAR GREENWICH MERIDIA         INITNEST.686
C       SO INTERPOLATE BOTH AND USE HSCR1N WHEN NEARER DATELINE                  INITNEST.687
              IF(HELP2(I,J).LT.0.)THEN                                           INITNEST.688
                HELP1(I,J)=HELP2(I,J)+360.                                       INITNEST.689
              ELSE                                                               INITNEST.690
                HELP1(I,J)=HELP2(I,J)                                            INITNEST.691
              ENDIF                                                              INITNEST.692
            ENDDO                                                                INITNEST.693
          ENDDO                                                                  INITNEST.694
          CALL EXAINT(HELP2,IYY,JXX,HSCR2N,IYYN,JXXN,ISOUTH,JWEST,1)             INITNEST.695
          CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)             INITNEST.696
          DO J=1,JXXN-1                                                          INITNEST.697
            DO I=1,IYYN-1                                                        INITNEST.698
C     HAVE TO USE NEAREST COARSE-MESH LONGITUDE TO TEST BECAUSE                  INITNEST.699
C       FINE-MESH VALUE MAY BE UNRELIABLE AFTER INTERPOLATION                    INITNEST.700
              IC=ISOUTH+NINT(FLOAT(I-2)/3.)                                      INITNEST.701
              JC=JWEST+NINT(FLOAT(J-2)/3.)                                       INITNEST.702
              IF(ABS(HELP2(IC,JC)).GT.90.)THEN                                   INITNEST.703
C     NEARER DATE-LINE                                                           INITNEST.704
                IF(HSCR1N(I,J).GT.180)THEN                                       INITNEST.705
                  HSCR2N(I,J)=HSCR1N(I,J)-360.                                   INITNEST.706
                ELSE                                                             INITNEST.707
                  HSCR2N(I,J)=HSCR1N(I,J)                                        INITNEST.708
                ENDIF                                                            INITNEST.709
              ENDIF                                                              INITNEST.710
            ENDDO                                                                INITNEST.711
          ENDDO                                                                  INITNEST.712
          CALL FILL(HSCR2N,XLONGN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)               INITNEST.713
C                                                                                INITNEST.714
C--- SNOW-COVER                                                                  INITNEST.715
C                                                                                INITNEST.716
        ELSEIF(LPLOOP.EQ.5)THEN                                                  INITNEST.717
          CALL FILL(SNOWC,HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                     INITNEST.718
          CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)             INITNEST.719
          CALL FILL(HSCR1N,SNOWCN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)               INITNEST.720
C                                                                                INITNEST.721
C--- CORIOLIS                                                                    INITNEST.722
C                                                                                INITNEST.723
        ELSEIF(LPLOOP.EQ.6)THEN                                                  INITNEST.724
          CALL FILL(F,HELP2,MIX,MJX,IYY,JXX,IYY,JXX)                             INITNEST.725
          CALL EXAINT(HELP2,IYY,JXX,HSCR2N,IYYN,JXXN,ISOUTH,JWEST,0)             INITNEST.726
          CALL FILL(HSCR2N,FN,IYYN,JXXN,MIX,MJX,IYYN,JXXN)                       INITNEST.727
C                                                                                INITNEST.728
C                                                                                INITNEST.729
C                                                                                INITNEST.730
        ELSEIF(LPLOOP.EQ.7)THEN                                                  INITNEST.731
          CALL FILL(TGA,HELP2,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                       INITNEST.732
          CALL EXAINT(HELP2,IYY,JXX,HSCR2N,IYYN,JXXN,ISOUTH,JWEST,1)             INITNEST.733
          CALL FILL(HSCR2N,TGAN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)                 INITNEST.734
C                                                                                INITNEST.735
        ELSEIF(LPLOOP.EQ.8)THEN                                                  INITNEST.736
          CALL FILL(TMN,HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                       INITNEST.737
          CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)             INITNEST.738
          CALL FILL(HSCR1N,TMNN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)                 INITNEST.739
C                                                                                24SEP99.300
C---SEA SURFACE TEMPERATURE                                                      24SEP99.301
        ELSEIF(LPLOOP.EQ.9)THEN                                                  24SEP99.302
          CALL FILL(TSS,HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                       24SEP99.303
          CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)             24SEP99.304
          CALL FILL(HSCR1N,TSSN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)                 24SEP99.305
C                                                                                24SEP99.306
        ENDIF                                                                    INITNEST.740
C                                                                                INITNEST.741
C--- END MULTITASKED LOOP                                                        INITNEST.742
C                                                                                INITNEST.743
  404 CONTINUE                                                                   INITNEST.744
C                                                                                INITNEST.745
C--- RAINFALL FIELDS                                                             INITNEST.746
C                                                                                INITNEST.747
      CALL FILL(RAINC,HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                         INITNEST.748
      CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)                 INITNEST.749
      CALL FILL(HSCR1N,RAINCN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)                   INITNEST.750
C                                                                                INITNEST.751
      CALL FILL(RAINNC,HELP2,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                        INITNEST.752
      CALL EXAINT(HELP2,IYY,JXX,HSCR2N,IYYN,JXXN,ISOUTH,JWEST,1)                 INITNEST.753
      CALL FILL(HSCR2N,RAINNN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)                   INITNEST.754
C                                                                                INITNEST.755
C--- SOIL LAYER TEMPERATURE                                                      INITNEST.756
C                                                                                INITNEST.757
      IF(ISOIL.EQ.1)THEN                                                         INITNEST.758
        DO L=1,MLX                                                               INITNEST.759
          CALL FILL(TSLA(1,1,L),HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)               INITNEST.760
          CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)             INITNEST.761
          CALL FILL(HSCR1N,TSLAN(1,1,L),IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)         INITNEST.762
          DO J=1,JXXN-1                                                          INITNEST.763
            DO I=1,IYYN-1                                                        INITNEST.764
              TSLBN(I,J,L)=TSLAN(I,J,L)                                          INITNEST.765
            ENDDO                                                                INITNEST.766
          ENDDO                                                                  INITNEST.767
        ENDDO                                                                    INITNEST.768
        IF(IEXSI.GE.1)THEN                                                       23DEC04.277
          CALL FILL(SEAICE,HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                    23DEC04.278
          CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)             23DEC04.279
          CALL FILL(HSCR1N,SEAICN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)               23DEC04.280
        ENDIF                                                                    23DEC04.281
      ENDIF                                                                      INITNEST.769
C                                                                                07NOV00.834
C--- MOISTURE AVAILABILITY IF INPUT IS USED                                      07NOV00.835
C                                                                                07NOV00.836
      IF(IMOIAV(NESCOU).EQ.2)THEN                                                07NOV00.837
cmic$ do all autoscope                                                           07NOV00.838
c$omp parallel do default(shared)                                                07NOV00.839
c$omp&private(i,j,icm,jcm)                                                       07NOV00.840
        DO J=1,JXXN-1                                                            07NOV00.841
          DO I=1,IYYN-1                                                          07NOV00.842
            ICM=(I-1)/3+ISOUTH                                                   07NOV00.843
            JCM=(J-1)/3+JWEST                                                    07NOV00.844
            MAVAIN(I,J)=MAVAIL(ICM,JCM)                                          07NOV00.845
          ENDDO                                                                  07NOV00.846
        ENDDO                                                                    07NOV00.847
      ENDIF                                                                      07NOV00.848
C                                                                                INITNEST.770
C--- LAND-SURFACE MODEL VARIABLES                                                INITNEST.771
      IF(ISOIL.EQ.2.OR.ISOIL.EQ.3)THEN                                           07NOV00.849
cmic$ do all autoscope                                                           INITNEST.773
c$omp parallel do default(shared)                                                INITNEST.774
c$omp&private(i,j,icm,jcm)                                                       INITNEST.775
        DO J=1,JXXN-1                                                            INITNEST.776
          DO I=1,IYYN-1                                                          INITNEST.777
            ICM=(I-1)/3+ISOUTH                                                   INITNEST.778
            JCM=(J-1)/3+JWEST                                                    INITNEST.779
            IVGTYPN(I,J)=IVGTYP(ICM,JCM)                                         INITNEST.780
            ISLTYPN(I,J)=ISLTYP(ICM,JCM)                                         INITNEST.781
          ENDDO                                                                  INITNEST.782
        ENDDO                                                                    INITNEST.783
C                                                                                19DEC02.976
        CALL FILL(XVEGMN,HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                      19DEC02.977
        CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)               19DEC02.978
        CALL FILL(HSCR1N,XVEGMNN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)                19DEC02.979
C                                                                                INITNEST.784
        CALL FILL(VEGFRC,HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                      INITNEST.785
        CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)               INITNEST.786
        CALL FILL(HSCR1N,VEGFRCN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)                INITNEST.787
C                                                                                INITNEST.788
        CALL FILL(XICE,HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                        07NOV00.850
        CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)               07NOV00.851
        CALL FILL(HSCR1N,XICEN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)                  07NOV00.852
C                                                                                07NOV00.853
        CALL FILL(CMCA,HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                        INITNEST.789
        CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)               INITNEST.790
        CALL FILL(HSCR1N,CMCAN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)                  INITNEST.791
        DO J=1,JXXN-1                                                            INITNEST.792
          DO I=1,IYYN-1                                                          INITNEST.793
            CMCBN(I,J)=CMCAN(I,J)                                                INITNEST.794
          ENDDO                                                                  INITNEST.795
        ENDDO                                                                    INITNEST.796
C                                                                                INITNEST.797
        CALL FILL(SNOWHA,HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                      19DEC02.980
        CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)               19DEC02.981
        CALL FILL(HSCR1N,SNOWHAN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)                19DEC02.982
        DO J=1,JXXN-1                                                            19DEC02.983
          DO I=1,IYYN-1                                                          19DEC02.984
            IF (SNOWHAN(I,J) .LT. 0.) SNOWHAN(I,J)=0.                            19DEC02.985
            SNOWHBN(I,J)=SNOWHAN(I,J)                                            19DEC02.986
          ENDDO                                                                  19DEC02.987
        ENDDO                                                                    19DEC02.988
C                                                                                19DEC02.989
        CALL FILL(SNOA,HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                        INITNEST.798
        CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)               INITNEST.799
        CALL FILL(HSCR1N,SNOAN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)                  INITNEST.800
        DO J=1,JXXN-1                                                            INITNEST.801
          DO I=1,IYYN-1                                                          INITNEST.802
            IF (SNOAN(I,J) .LT. 0.) SNOAN(I,J)=0.                                07NOV00.854
            SNOBN(I,J)=SNOAN(I,J)                                                INITNEST.803
          ENDDO                                                                  INITNEST.804
        ENDDO                                                                    INITNEST.805
C                                                                                INITNEST.806
        DO L=1,MLX                                                               INITNEST.807
          CALL FILL(STCA(1,1,L),HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)               INITNEST.808
          CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)             INITNEST.809
          CALL FILL(HSCR1N,STCAN(1,1,L),IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)         INITNEST.810
          DO J=1,JXXN-1                                                          INITNEST.811
            DO I=1,IYYN-1                                                        INITNEST.812
              STCBN(I,J,L)=STCAN(I,J,L)                                          INITNEST.813
            ENDDO                                                                INITNEST.814
          ENDDO                                                                  INITNEST.815
C                                                                                INITNEST.816
          CALL FILL(SMCA(1,1,L),HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)               INITNEST.817
          CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)             INITNEST.818
          CALL FILL(HSCR1N,SMCAN(1,1,L),IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)         INITNEST.819
          DO J=1,JXXN-1                                                          INITNEST.820
            DO I=1,IYYN-1                                                        INITNEST.821
              SMCBN(I,J,L)=SMCAN(I,J,L)                                          INITNEST.822
            ENDDO                                                                INITNEST.823
          ENDDO                                                                  INITNEST.824
C                                                                                19DEC02.990
          CALL FILL(SWCA(1,1,L),HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)               19DEC02.991
          CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)             19DEC02.992
          CALL FILL(HSCR1N,SWCAN(1,1,L),IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)         19DEC02.993
          DO J=1,JXXN-1                                                          19DEC02.994
            DO I=1,IYYN-1                                                        19DEC02.995
              SWCBN(I,J,L)=SWCAN(I,J,L)                                          19DEC02.996
            ENDDO                                                                19DEC02.997
          ENDDO                                                                  19DEC02.998
        ENDDO                                                                    INITNEST.825
        DO L=1,12                                                                05DEC01.380
          CALL FILL(VEGFMO(1,1,L),HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)             05DEC01.381
          CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)             05DEC01.382
          CALL FILL(HSCR1N,VEGFMON(1,1,L),IYYN,JXXN,MIX,MJX,IYYN-1,              05DEC01.383
     -              JXXN-1)                                                      05DEC01.384
        ENDDO                                                                    05DEC01.385
        IF(RDMAXALB)THEN                                                         19DEC02.999
          CALL FILL(ALBMAX,HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                    19DEC02.1000
          CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)             19DEC02.1001
          CALL FILL(HSCR1N,ALBMAXN,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)              19DEC02.1002
        ENDIF                                                                    19DEC02.1003
        IF(RDBRDALB)THEN                                                         19DEC02.1004
          CALL FILL(ALBEDO,HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                    19DEC02.1005
          CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)             19DEC02.1006
          CALL FILL(HSCR1N,ALBEDON,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)              19DEC02.1007
          DO L=1,12                                                              19DEC02.1008
            CALL FILL(ALBMON(1,1,L),HELP1,MIX,MJX,IYY,JXX,IYY-1,JXX-1)           19DEC02.1009
            CALL EXAINT(HELP1,IYY,JXX,HSCR1N,IYYN,JXXN,ISOUTH,JWEST,1)           19DEC02.1010
            CALL FILL(HSCR1N,ALBMONN(1,1,L),IYYN,JXXN,MIX,MJX,IYYN-1,            19DEC02.1011
     -              JXXN-1)                                                      19DEC02.1012
          ENDDO                                                                  19DEC02.1013
        ENDIF                                                                    19DEC02.1014
      ENDIF                                                                      INITNEST.826
C                                                                                INITNEST.827
C                                                                                INITNEST.828
C--- NON-HYDROSTATIC                                                             INITNEST.829
C                                                                                INITNEST.830
      CALL FILL(PS0,HELP2,MIX,MJX,IYY,JXX,IYY-1,JXX-1)                           INITNEST.831
      CALL EXAINT(HELP2,IYY,JXX,HSCR2N,IYYN,JXXN,ISOUTH,JWEST,1)                 INITNEST.832
      CALL FILL(HSCR2N,PS0N,IYYN,JXXN,MIX,MJX,IYYN-1,JXXN-1)                     INITNEST.833
C                                                                                INITNEST.834
cmic$ do all autoscope                                                           INITNEST.835
c$doacross                                                                       INITNEST.836
c$& share (pr0n,a,ps0n,ptop,rho0n,r,t0n),                                        INITNEST.837
c$& local (i,j,k)                                                                INITNEST.838
c$omp parallel do default(shared)                                                INITNEST.839
c$omp&private(i,j,k)                                                             INITNEST.840
      DO J=1,JXXN-1                                                              INITNEST.841
        DO K=1,KZZN                                                              INITNEST.842
          DO I=1,IYYN-1                                                          INITNEST.843
            PR0N(I,J,K)=A(K)*PS0N(I,J)+PTOP*1000.                                INITNEST.844
            RHO0N(I,J,K)=PR0N(I,J,K)/(R*T0N(I,J,K))                              INITNEST.845
          ENDDO                                                                  INITNEST.846
        ENDDO                                                                    INITNEST.847
      ENDDO                                                                      INITNEST.848
C                                                                                INITNEST.849
cmic$ do all autoscope                                                           INITNEST.850
c$doacross                                                                       INITNEST.851
c$& share (jxxn,iyyn,psbn,rpsan,rpsbn,rpscn),                                    INITNEST.852
c$& local(i,j)                                                                   INITNEST.853
c$omp parallel do default(shared)                                                INITNEST.854
c$omp&private(i,j)                                                               INITNEST.855
      DO J=1,JXXN-1                                                              INITNEST.856
        DO I=1,IYYN-1                                                            INITNEST.857
          RPSAN(I,J)=1.0/PSBN(I,J)                                               INITNEST.858
          RPSBN(I,J)=RPSAN(I,J)                                                  INITNEST.859
          RPSCN(I,J)=RPSAN(I,J)                                                  INITNEST.860
        ENDDO                                                                    INITNEST.861
      ENDDO                                                                      INITNEST.862
C                                                                                INITNEST.863
      CALL DOTS(PSBN,PDOTAN,IYYN,JXXN,MIX,MJX)                                   INITNEST.864
C                                                                                INITNEST.865
cmic$ do all autoscope                                                           INITNEST.866
c$doacross                                                                       INITNEST.867
c$& share (pdotan,pdotbn,rpdotan,rpdotbn),                                       INITNEST.868
c$& local(i,j)                                                                   INITNEST.869
c$omp parallel do default(shared)                                                INITNEST.870
c$omp&private(i,j)                                                               INITNEST.871
      DO J=1,JXXN                                                                INITNEST.872
        DO I=1,IYYN                                                              INITNEST.873
          RPDOTAN(I,J)=1.0/PDOTAN(I,J)                                           INITNEST.874
          RPDOTBN(I,J)=RPDOTAN(I,J)                                              INITNEST.875
          PDOTBN(I,J)=PDOTAN(I,J)                                                INITNEST.876
        ENDDO                                                                    INITNEST.877
      ENDDO                                                                      INITNEST.878
C                                                                                INITNEST.879
cmic$ do all autoscope                                                           INITNEST.880
c$omp parallel do default(shared)                                                INITNEST.881
c$omp&private(i,j)                                                               INITNEST.882
      DO 407 J=1,JXXN-1                                                          INITNEST.883
        DO 407 I=1,IYYN-1                                                        INITNEST.884
          TGBN(I,J)=TGAN(I,J)                                                    INITNEST.885
  407   CONTINUE                                                                 INITNEST.886
                                                                                 INITNEST.887
CEND OF NON-MPP INTERPOLATION                                                    INITNEST.888
#endif                                                                           INITNEST.889
cmic$ do all autoscope                                                           INITNEST.890
c$omp parallel do default(shared)                                                INITNEST.891
c$omp&private(i,j)                                                               INITNEST.892
      DO 400 J=1,JXXN-1                                                          INITNEST.893
        DO 400 I=1,IYYN-1                                                        INITNEST.894
         IF(IFSNOW(NUMNES).EQ.1.AND.ISOIL.NE.2)THEN                              19DEC02.1015
          IF(SNOWCN(I,J).GE.0.5)THEN                                             INITNEST.895
            SNOWCN(I,J)=1.0                                                      INITNEST.896
          ELSE                                                                   INITNEST.897
            SNOWCN(I,J)=-0.01                                                    INITNEST.898
          ENDIF                                                                  INITNEST.899
         ELSE IF(IFSNOW(NUMNES).EQ.2 .OR. ISOIL.EQ.2)THEN                        19DEC02.1016
            SNOWCN(I,J)=AMAX1(0.0,SNOWCN(I,J))                                   05DEC01.388
         ENDIF                                                                   05DEC01.389
  400   CONTINUE                                                                 INITNEST.900
                                                                                 INITNEST.901
C                                                                                INITNEST.902
C--- DO YOU HAVE YOUR OWN ANALYSIS FOR THE NEST?                                 INITNEST.903
C                                                                                INITNEST.904
C                                                                                INITNEST.905
      IF(IOVERW(NUMNES).EQ.1)THEN                                                INITNEST.906
        PRINT *,'IOVERW(N)=1: READING INPUT FILE FOR NEST ',NUMNES               INITNEST.907
        CALL ADDRX1C(IAXALL(1,NUMNES))                                           INITNEST.908
C                                                                                INITNEST.909
        IUNITH=11+(NUMNES-1)                                                     INITNEST.910
        IHEADER=1                                                                INITNEST.911
#ifdef MPP1                                                                      INITNEST.912
        CALL MP_INITDOMAIN(NUMNES)                                               INITNEST.913
#endif                                                                           INITNEST.914
        CALL RDINIT(IUNITH,NUMNES,IL,JL)                                         25JAN00.186
C                                                                                INITNEST.916
cmic$ do all autoscope                                                           INITNEST.917
c$omp parallel do default(shared)                                                INITNEST.918
c$omp&private(i,j)                                                               INITNEST.919
        DO 2468 J=1,JLX                                                          INITNEST.920
          DO 2468 I=1,ILX                                                        INITNEST.921
            HT(I,J)=HT(I,J)*G                                                    INITNEST.922
 2468     CONTINUE                                                               INITNEST.923
                                                                                 INITNEST.924
C                                                                                INITNEST.925
cmic$ do all autoscope                                                           INITNEST.926
c$doacross                                                                       INITNEST.927
c$& share (ilx,jlx,psb,rpsa,rpsb,rpsc),                                          INITNEST.928
c$& local(i,j)                                                                   INITNEST.929
c$omp parallel do default(shared)                                                INITNEST.930
c$omp&private(i,j)                                                               INITNEST.931
        DO J=1,JLX                                                               INITNEST.932
          DO I=1,ILX                                                             INITNEST.933
            RPSA(I,J)=1.0/PSA(I,J)                                               INITNEST.934
            RPSB(I,J)=RPSA(I,J)                                                  INITNEST.935
            RPSC(I,J)=RPSA(I,J)                                                  INITNEST.936
          ENDDO                                                                  INITNEST.937
        ENDDO                                                                    INITNEST.938
C                                                                                INITNEST.939
        CALL DOTS(PSA,PDOTA,IL,JL,MIX,MJX)                                       INITNEST.940
C                                                                                INITNEST.941
cmic$ do all autoscope                                                           INITNEST.942
c$doacross                                                                       INITNEST.943
c$& share (pdota,pdotb,rpdota,rpdotb),                                           INITNEST.944
c$& local(i,j)                                                                   INITNEST.945
c$omp parallel do default(shared)                                                INITNEST.946
c$omp&private(i,j)                                                               INITNEST.947
        DO J=1,JL                                                                INITNEST.948
          DO I=1,IL                                                              INITNEST.949
            RPDOTA(I,J)=1.0/PDOTA(I,J)                                           INITNEST.950
            RPDOTB(I,J)=RPDOTA(I,J)                                              INITNEST.951
            PDOTB(I,J)=PDOTA(I,J)                                                INITNEST.952
          ENDDO                                                                  INITNEST.953
        ENDDO                                                                    INITNEST.954
C                                                                                INITNEST.955
C--- INVERT MAP-SCALE FACTORS                                                    24SEP99.307
C                                                                                INITNEST.957
cmic$ do all autoscope                                                           INITNEST.958
c$omp parallel do default(shared)                                                INITNEST.959
c$omp&private(i,j)                                                               INITNEST.960
        DO 2469 J=1,JL                                                           INITNEST.961
          DO 2469 I=1,IL                                                         INITNEST.962
            MSFD(I,J)=1./MSFD(I,J)                                               INITNEST.963
 2469     CONTINUE                                                               INITNEST.964
                                                                                 INITNEST.965
cmic$ do all autoscope                                                           INITNEST.966
c$omp parallel do default(shared)                                                INITNEST.967
c$omp&private(i,j)                                                               INITNEST.968
        DO 2467 J=1,JLX                                                          INITNEST.969
          DO 2467 I=1,ILX                                                        INITNEST.970
            MSFX(I,J)=1./MSFX(I,J)                                               INITNEST.971
 2467     CONTINUE                                                               INITNEST.972
      IF(MLX.NE.1)THEN                                                           25JAN00.187
        IF(ISOIL.EQ.1)THEN                                                       25JAN00.188
C                                                                                25JAN00.189
C-----INITIALIZE SOIL TEMPERATURE PROFILE (LINEAR WITH DEPTH)                    25JAN00.190
C                                                                                25JAN00.191
cmic$ do all autoscope                                                           25JAN00.192
c$omp parallel do default(shared)                                                25JAN00.193
c$omp&private(i,j,l)                                                             25JAN00.194
          DO L=1,MLX                                                             25JAN00.195
            DO J=1,JLX                                                           25JAN00.196
              DO I=1,ILX                                                         25JAN00.197
                TSLA(I,J,L)=(TGA(I,J)*(ZS(MLX)-ZS(L))+TMN(I,J)*(ZS(L)-           25JAN00.198
     +                      ZS(1)))/(ZS(MLX)-ZS(1))                              25JAN00.199
                TSLB(I,J,L)=(TGB(I,J)*(ZS(MLX)-ZS(L))+TMN(I,J)*(ZS(L)-           25JAN00.200
     +                      ZS(1)))/(ZS(MLX)-ZS(1))                              25JAN00.201
              ENDDO                                                              25JAN00.202
            ENDDO                                                                25JAN00.203
          ENDDO                                                                  25JAN00.204
        ENDIF                                                                    25JAN00.205
      ENDIF                                                                      25JAN00.206
C                                                                                25JAN00.207
C  CORRECT GROUND TEMPERATURE IN SNOW LAYERS                                     05DEC01.390
      IF(IFSNOW(NUMNES).EQ.2.AND.ISOIL.EQ.1)THEN                                 05DEC01.391
      DO J=1,JLX                                                                 05DEC01.392
        DO I=1,ILX                                                               05DEC01.393
          IF(SNOWC(I,J)*0.01.GT.ZS(1).AND.TSLA(I,J,1).GT.273.16)THEN             05DEC01.394
             TGA(I,J)=273.16                                                     05DEC01.397
             TGB(I,J)=273.16                                                     05DEC01.398
          ENDIF                                                                  05DEC01.399
        ENDDO                                                                    05DEC01.420
      ENDDO                                                                      05DEC01.421
      DO L=1,MLX                                                                 23DEC04.282
        DO J=1,JLX                                                               23DEC04.283
          DO I=1,ILX                                                             23DEC04.284
          IF(SNOWC(I,J)*0.01.GT.ZS(L).AND.TSLA(I,J,L).GT.273.16)THEN             23DEC04.285
             TSLA(I,J,L)=273.16                                                  23DEC04.286
             TSLB(I,J,L)=273.16                                                  23DEC04.287
          ENDIF                                                                  23DEC04.288
          ENDDO                                                                  23DEC04.289
        ENDDO                                                                    23DEC04.290
      ENDDO                                                                      23DEC04.291
      ENDIF                                                                      05DEC01.422
                                                                                 INITNEST.973
        CALL ADDRX1C(IAXALL(1,1))                                                INITNEST.974
        CALL ADDRX1N(IAXALL(1,NUMNES))                                           INITNEST.975
      ENDIF                                                                      INITNEST.976
C                                                                                INITNEST.977
C--- START NEST WITH NEW TERRAIN/LAND-USE AND INTERPOLATED FIELDS                INITNEST.978
C--- IOVERW=2 OPTION                                                             INITNEST.979
C                                                                                INITNEST.980
      IF(IOVERW(NUMNES).EQ.2)THEN                                                INITNEST.981
        PRINT *,'IOVERW(N)=2: READING TERRAIN FILE FOR NEST ',NUMNES             INITNEST.982
        CALL ADDRX1C(IAXALL(1,NUMNES))                                           INITNEST.983
        IUNIT=11+(NUMNES-1)                                                      INITNEST.984
#ifdef MPP1                                                                      INITNEST.985
        CALL MP_INITDOMAIN(NUMNES)                                               INITNEST.986
#endif                                                                           INITNEST.987
        CALL RDTER(IUNIT,NUMNES,IYYN,JXXN,HT,SATBRT,TMN,ISLTYP,XLAT,             INITNEST.988
     +       XLONG,MSFD,MSFX,F,ISOIL,VEGFRC,VEGFMO,IVGTYP)                       05DEC01.423
C  SET CROSS POINT FIELDS                                                        INITNEST.990
        DO J=1,JLX                                                               INITNEST.991
          DO I=1,ILX                                                             INITNEST.992
C  NEW PS0 BASED ON NEW HT                                                       INITNEST.993
            AC=G*HT(I,J)/(2.*TLP*R)                                              INITNEST.994
            B=TS0/TLP                                                            INITNEST.995
            ALNP=-B+SQRT(B*B-4*AC)                                               INITNEST.996
            PS0NEW=P0*EXP(ALNP)-PTOP*1000.                                       INITNEST.997
            PSANEW=PS0NEW*0.001                                                  INITNEST.998
            PISO=P0*EXP((TISO-TS0)/TLP)                                          INITNEST.999
            IF(PISO.GT.PS0NEW+PTOP*1000.)THEN                                    INITNEST.1000
            PRINT *,'STOP IN INITNEST: TOPOGRAPHY IN ISOTHERMAL LAYER'           INITNEST.1001
            PRINT *,'REDUCE TISO IN INTERPF'                                     INITNEST.1002
              STOP 'TISO'                                                        INITNEST.1003
            ENDIF                                                                INITNEST.1004
            DO K=1,KX                                                            INITNEST.1005
              PR0NEW=PS0NEW*A(K)+PTOP*1000.                                      INITNEST.1006
              T0NEW=AMAX1(TS0+TLP*ALOG(PR0NEW/P0),TISO)                          INITNEST.1007
              DT0=T0NEW-T0(I,J,K)                                                INITNEST.1008
C  KEEP T-T0 SAME WITH NEW TERRAIN                                               INITNEST.1009
              TA(I,J,K)=(TA(I,J,K)*RPSA(I,J)+DT0)*PSANEW                         INITNEST.1010
              TB(I,J,K)=TA(I,J,K)                                                INITNEST.1011
              IF(K.EQ.KX)THEN                                                    INITNEST.1012
C  SIMILARLY ADJUST TG, TMN AND TSL                                              07NOV00.856
                TGA(I,J)=TGA(I,J)+DT0                                            INITNEST.1014
                TGB(I,J)=TGA(I,J)                                                INITNEST.1015
                IF(ISOIL.EQ.1)THEN                                               INITNEST.1016
                  TMN(I,J)=TMN(I,J)+DT0                                          07NOV00.857
                  DO L=1,MLX                                                     INITNEST.1017
                    TSLA(I,J,L)=TSLA(I,J,L)+DT0                                  INITNEST.1018
                    TSLB(I,J,L)=TSLA(I,J,L)                                      INITNEST.1019
                  ENDDO                                                          INITNEST.1020
                ENDIF                                                            INITNEST.1021
              ENDIF                                                              INITNEST.1022
C  OTHER UNCOUPLED VARIABLES AUTOMATICALLY REMAIN SAME (E.G. TKE)                INITNEST.1023
C  KEEP PP SAME                                                                  INITNEST.1024
              PPA(I,J,K)=PPA(I,J,K)*RPSA(I,J)*PSANEW                             INITNEST.1025
              PPB(I,J,K)=PPA(I,J,K)                                              INITNEST.1026
C  KEEP W SAME (WA AT KXP1 CHANGED LATER)                                        INITNEST.1027
              WA(I,J,K)=WA(I,J,K)*RPSA(I,J)*PSANEW                               INITNEST.1028
              WB(I,J,K)=WA(I,J,K)                                                INITNEST.1029
C  KEEP QV SAME                                                                  INITNEST.1030
              QVA(I,J,K)=QVA(I,J,K)*RPSA(I,J)*PSANEW                             INITNEST.1031
              QVB(I,J,K)=QVA(I,J,K)                                              INITNEST.1032
              IF(IMOIST(NUMNES).EQ.2)THEN                                        INITNEST.1033
C  KEEP QC AND QR ETC. SAME                                                      INITNEST.1034
                QCA(I,J,K)=QCA(I,J,K)*RPSA(I,J)*PSANEW                           INITNEST.1035
                QCB(I,J,K)=QCA(I,J,K)                                            INITNEST.1036
                QRA(I,J,K)=QRA(I,J,K)*RPSA(I,J)*PSANEW                           INITNEST.1037
                QRB(I,J,K)=QRA(I,J,K)                                            INITNEST.1038
                IF(IICE.EQ.1)THEN                                                INITNEST.1039
                  QIA(I,J,K)=QIA(I,J,K)*RPSA(I,J)*PSANEW                         INITNEST.1040
                  QIB(I,J,K)=QIA(I,J,K)                                          INITNEST.1041
                  QNIA(I,J,K)=QNIA(I,J,K)*RPSA(I,J)*PSANEW                       INITNEST.1042
                  QNIB(I,J,K)=QNIA(I,J,K)                                        INITNEST.1043
                  IF(IICEG.EQ.1)THEN                                             INITNEST.1044
                    QNCA(I,J,K)=QNCA(I,J,K)*RPSA(I,J)*PSANEW                     INITNEST.1045
                    QNCB(I,J,K)=QNCA(I,J,K)                                      INITNEST.1046
                    QGA(I,J,K)=QGA(I,J,K)*RPSA(I,J)*PSANEW                       INITNEST.1047
                    QGB(I,J,K)=QGA(I,J,K)                                        INITNEST.1048
                  ENDIF                                                          INITNEST.1049
                ENDIF                                                            INITNEST.1050
              ENDIF                                                              INITNEST.1051
C  RESET REFERENCE FIELDS                                                        INITNEST.1052
              T0(I,J,K)=T0NEW                                                    INITNEST.1053
              PR0(I,J,K)=PR0NEW                                                  INITNEST.1054
              RHO0(I,J,K)=PR0(I,J,K)/R/T0(I,J,K)                                 INITNEST.1055
            ENDDO                                                                INITNEST.1056
            PS0(I,J)=PS0NEW                                                      INITNEST.1057
C  NOTE THAT PSA IS RESET HERE NOT RPSA, ETC.                                    INITNEST.1058
            PSA(I,J)=PSANEW                                                      INITNEST.1059
            PSB(I,J)=PSANEW                                                      INITNEST.1060
            WA(I,J,KXP1)=WA(I,J,KXP1)*RPSA(I,J)*PSANEW                           INITNEST.1061
            WB(I,J,KXP1)=WA(I,J,KXP1)                                            INITNEST.1062
            HT(I,J)=HT(I,J)*G                                                    INITNEST.1063
          ENDDO                                                                  INITNEST.1064
        ENDDO                                                                    INITNEST.1065
#ifdef MPP1                                                                      INITNEST.1066
#  include <mpp_initnest_33.incl>                                                INITNEST.1067
#endif                                                                           INITNEST.1068
cmic$ do all autoscope                                                           INITNEST.1069
c$doacross                                                                       INITNEST.1070
c$& share (ilx,jlx,psb,rpsa,rpsb,rpsc),                                          INITNEST.1071
c$& local(i,j)                                                                   INITNEST.1072
c$omp parallel do default(shared)                                                INITNEST.1073
c$omp&private(i,j)                                                               INITNEST.1074
        DO J=1,JLX                                                               INITNEST.1075
          DO I=1,ILX                                                             INITNEST.1076
            RPSA(I,J)=1.0/PSA(I,J)                                               INITNEST.1077
            RPSB(I,J)=RPSA(I,J)                                                  INITNEST.1078
            RPSC(I,J)=RPSA(I,J)                                                  INITNEST.1079
          ENDDO                                                                  INITNEST.1080
        ENDDO                                                                    INITNEST.1081
C                                                                                INITNEST.1082
C  DOT POINT FIELDS                                                              INITNEST.1083
C  NOTE THAT PDOTA IS SET HERE BUT NOT RPDOTA, ETC.                              INITNEST.1084
        CALL DOTS(PSA,PDOTA,IL,JL,MIX,MJX)                                       INITNEST.1085
        DO J=1,JL                                                                INITNEST.1086
          DO I=1,IL                                                              INITNEST.1087
            DO K=1,KX                                                            INITNEST.1088
C KEEP U AND V SAME                                                              INITNEST.1089
              UA(I,J,K)=UA(I,J,K)*RPDOTA(I,J)*PDOTA(I,J)                         INITNEST.1090
              UB(I,J,K)=UA(I,J,K)                                                INITNEST.1091
              VA(I,J,K)=VA(I,J,K)*RPDOTA(I,J)*PDOTA(I,J)                         INITNEST.1092
              VB(I,J,K)=VA(I,J,K)                                                INITNEST.1093
            ENDDO                                                                INITNEST.1094
          ENDDO                                                                  INITNEST.1095
        ENDDO                                                                    INITNEST.1096
cmic$ do all autoscope                                                           INITNEST.1097
c$doacross                                                                       INITNEST.1098
c$& share (pdota,pdotb,rpdota,rpdotb),                                           INITNEST.1099
c$& local(i,j)                                                                   INITNEST.1100
c$omp parallel do default(shared)                                                INITNEST.1101
c$omp&private(i,j)                                                               INITNEST.1102
        DO J=1,JL                                                                INITNEST.1103
          DO I=1,IL                                                              INITNEST.1104
            RPDOTA(I,J)=1.0/PDOTA(I,J)                                           INITNEST.1105
            RPDOTB(I,J)=RPDOTA(I,J)                                              INITNEST.1106
            PDOTB(I,J)=PDOTA(I,J)                                                INITNEST.1107
          ENDDO                                                                  INITNEST.1108
        ENDDO                                                                    INITNEST.1109
C                                                                                24SEP99.309
C--- INVERT MAP-SCALE FACTORS                                                    24SEP99.310
C                                                                                24SEP99.311
cmic$ do all autoscope                                                           24SEP99.312
c$omp parallel do default(shared)                                                24SEP99.313
c$omp&private(i,j)                                                               24SEP99.314
        DO J=1,JL                                                                24SEP99.315
          DO I=1,IL                                                              24SEP99.316
            MSFD(I,J)=1./MSFD(I,J)                                               24SEP99.317
          ENDDO                                                                  24SEP99.318
        ENDDO                                                                    24SEP99.319
                                                                                 24SEP99.320
cmic$ do all autoscope                                                           24SEP99.321
c$omp parallel do default(shared)                                                24SEP99.322
c$omp&private(i,j)                                                               24SEP99.323
        DO J=1,JLX                                                               24SEP99.324
          DO I=1,ILX                                                             24SEP99.325
            MSFX(I,J)=1./MSFX(I,J)                                               24SEP99.326
          ENDDO                                                                  24SEP99.327
        ENDDO                                                                    24SEP99.328
C                                                                                07NOV00.858
#ifdef SOIL2                                                                     07NOV00.859
        IF (ISOIL .EQ. 2) THEN                                                   07NOV00.860
           CALL ADDRX1C(IAXALL(1,NUMNC(NUMNES)))                                 23DEC04.292
           CALL ADDRX1N(IAXALL(1,NUMNES))                                        07NOV00.862
C          Pass the help arrays simply as work space.                            07NOV00.863
           PRINT *, 'LSM NEST INITIALIZATION FOR DOMAIN ', NUMNES                07NOV00.864
           CALL NESTLSM(NESCOU, HELP1, HELP1N, HELP2, HELP2N)                    07NOV00.865
        ENDIF                                                                    07NOV00.866
#endif                                                                           07NOV00.867
C                                                                                07NOV00.868
        CALL ADDRX1C(IAXALL(1,1))                                                INITNEST.1110
        CALL ADDRX1N(IAXALL(1,NUMNES))                                           INITNEST.1111
      ENDIF                                                                      INITNEST.1112
#ifdef MPP1                                                                      24SEP99.329
#  include <mpp_initnest_33.incl>                                                24SEP99.330
#endif                                                                           24SEP99.331
cmic$ do all autoscope                                                           INITNEST.1113
c$doacross                                                                       INITNEST.1114
c$& share(inlx,jnlx,inlxm,jnlxm,                                                 INITNEST.1115
c$&       psbn,msfxn,msfdn,a,sigma,ptop,                                         INITNEST.1116
c$&       dpsdymn,dpsdxmn,dmdxn,dmdyn,exn,crxn,srxn,                             INITNEST.1117
c$&       cosrtn,sinrtn,efn,dxn8,dxn),                                           INITNEST.1118
c$& local(i,j,k)                                                                 INITNEST.1119
c$omp parallel do default(shared)                                                INITNEST.1120
c$omp&private(i,j,k)                                                             INITNEST.1121
      DO J=1,JNLX                                                                INITNEST.1122
        DO I=1,INLX                                                              INITNEST.1123
          IF(I.EQ.1)THEN                                                         INITNEST.1124
            DPSDYMN(I,J)=(PSBN(I+1,J)-PSBN(I,J))/(PSBN(I,J)*DXN8*                INITNEST.1125
     +                   MSFXN(I,J))                                             INITNEST.1126
          ELSEIF(I.EQ.INLX)THEN                                                  INITNEST.1127
            DPSDYMN(I,J)=(PSBN(I,J)-PSBN(I-1,J))/(PSBN(I,J)*DXN8*                INITNEST.1128
     +                   MSFXN(I,J))                                             INITNEST.1129
          ELSE                                                                   INITNEST.1130
            DPSDYMN(I,J)=(PSBN(I+1,J)-PSBN(I-1,J))/(PSBN(I,J)*DXN8*              INITNEST.1131
     +                   MSFXN(I,J))                                             INITNEST.1132
          ENDIF                                                                  INITNEST.1133
        ENDDO                                                                    INITNEST.1134
C                                                                                INITNEST.1135
        IF(J.EQ.1)THEN                                                           INITNEST.1136
          DO I=1,INLX                                                            INITNEST.1137
            DPSDXMN(I,J)=(PSBN(I,J+1)-PSBN(I,J))/(PSBN(I,J)*DXN8*                INITNEST.1138
     +                   MSFXN(I,J))                                             INITNEST.1139
          ENDDO                                                                  INITNEST.1140
        ELSEIF(J.EQ.JNLX)THEN                                                    INITNEST.1141
          DO I=1,INLX                                                            INITNEST.1142
            DPSDXMN(I,J)=(PSBN(I,J)-PSBN(I,J-1))/(PSBN(I,J)*DXN8*                INITNEST.1143
     +                   MSFXN(I,J))                                             INITNEST.1144
          ENDDO                                                                  INITNEST.1145
        ELSE                                                                     INITNEST.1146
          DO I=1,INLX                                                            INITNEST.1147
            DPSDXMN(I,J)=(PSBN(I,J+1)-PSBN(I,J-1))/(PSBN(I,J)*DXN8*              INITNEST.1148
     +                   MSFXN(I,J))                                             INITNEST.1149
          ENDDO                                                                  INITNEST.1150
        ENDIF                                                                    INITNEST.1151
C                                                                                INITNEST.1152
        IF(J.NE.1)THEN                                                           INITNEST.1153
          DO I=2,INLX                                                            INITNEST.1154
            DMDYN(I,J)=-0.5*(MSFXN(I,J)+MSFXN(I,J-1)-MSFXN(I-1,J)-               INITNEST.1155
     +                 MSFXN(I-1,J-1))/(DXN*MSFDN(I,J)*MSFDN(I,J))               INITNEST.1156
            DMDXN(I,J)=-0.5*(MSFXN(I,J)+MSFXN(I-1,J)-MSFXN(I,J-1)-               INITNEST.1157
     +                 MSFXN(I-1,J-1))/(DXN*MSFDN(I,J)*MSFDN(I,J))               INITNEST.1158
          ENDDO                                                                  INITNEST.1159
        ENDIF                                                                    INITNEST.1160
      ENDDO                                                                      INITNEST.1161
C                                                                                07NOV00.869
C     READ IN LOWBDY FOR NEST (IF IT EXISTS)                                     07NOV00.870
C     SKIP LOWBDY IF NOT ALLOWING SST/SNOWC/SEAICE TO VARY IN TIME               07NOV00.871
C                                                                                07NOV00.872
      IF (XTIME.EQ.0. .OR. ISSTVAR.EQ.1) THEN                                    07NOV00.873
      CALL ADDRX1C(IAXALL(1,NUMNES))                                             07NOV00.874
      CALL LBDYIN(NUMNES,TBDYLBE,BDYLTIM,BDYLFRQ,ISOIL,IACTIV,                   05DEC01.424
     -     IFSNOW,IEXSI,IMOIAV,IL,JL)                                            23DEC04.293
      END IF                                                                     07NOV00.876
C                                                                                19DEC02.1018
C SPECIFY SEA ICE BASED ON SST IF IEXSI.EQ.1                                     19DEC02.1019
C READ SEA ICE FRACTION FROM LOWBDY IF IEXSI.EQ.2                                19DEC02.1020
C                                                                                19DEC02.1021
      IF(IEXSI.GE.1)THEN                                                         19DEC02.1022
        IF(IEXSI.EQ.1)THEN                                                       19DEC02.1023
          DO I=1,INLX                                                            19DEC02.1024
            DO J=1,JNLX                                                          19DEC02.1025
              IF((TGAN(I,J).LE.271.4).AND.                                       19DEC02.1026
     +           NINT(SATBRN(I,J)).EQ.ISWATER)THEN                               19DEC02.1027
                SEAICN(I,J)=0.8                                                  19DEC02.1028
              ELSE                                                               19DEC02.1029
                SEAICN(I,J)=0.0                                                  19DEC02.1030
              ENDIF                                                              19DEC02.1031
            ENDDO                                                                19DEC02.1032
          ENDDO                                                                  19DEC02.1033
          PRINT *,'FINISHED SPECIFYING SEAICE BASED ON TG'                       19DEC02.1034
        ENDIF                                                                    19DEC02.1035
C                                                                                19DEC02.1036
C SET DUMMY END VALUES FOR SEAICE AND CHECK FOR ERRORS IN SEAICE INPUT DATA      19DEC02.1037
C                                                                                19DEC02.1038
        DO I=1,INLX                                                              19DEC02.1039
          SEAICN(I,JNL)=SEAICN(I,JNLX)                                           19DEC02.1040
        ENDDO                                                                    19DEC02.1041
        DO J=1,JNLX                                                              19DEC02.1042
          SEAICN(INL,J)=SEAICN(INLX,J)                                           19DEC02.1043
        ENDDO                                                                    19DEC02.1044
        SEAICN(IL,JNL)=SEAICN(ILX,JNLX)                                          19DEC02.1045
C                                                                                19DEC02.1046
C CHECK FOR ERRORS IN SEAICE INPUT DATA                                          19DEC02.1047
C ALLOW SEAICE SURFACE TYPE AT WATER GRID POINTS ONLY, WITH SEAICE               19DEC02.1048
C FRACTION > 0.02                                                                19DEC02.1049
C                                                                                19DEC02.1050
        DO I=1,INLX                                                              19DEC02.1051
          DO J=1,JNLX                                                            19DEC02.1052
            IF((SEAICN(I,J).GT.1.001).OR.                                        19DEC02.1053
     +       (SEAICN(I,J).LT.-0.001))THEN                                        19DEC02.1054
              WRITE(6,*) 'SEA ICE INITIAL ERROR AT I, J',I,', ',J                19DEC02.1055
              STOP 'initnest sea ice'                                            19DEC02.1056
            ENDIF                                                                19DEC02.1057
            IF((NINT(SATBRN(I,J)).NE.ISWATER).OR.                                19DEC02.1058
     -              (SEAICN(I,J).LE.0.02))THEN                                   19DEC02.1059
              SEAICN(I,J)=0.0                                                    19DEC02.1060
            ENDIF                                                                19DEC02.1061
C                                                                                19DEC02.1062
C INITIALIZE SEAICE GROUND TEMPERATURE (TGSI)                                    19DEC02.1063
C                                                                                19DEC02.1064
            IF(SEAICN(I,J).GT.0.02)THEN                                          19DEC02.1065
C ASSUME INITIAL GROUND TEMPERATURE (TGA) REPRESENTS TEMPERATURE OVER SEAICE     19DEC02.1066
              TGSIN(I,J)=TGAN(I,J)                                               19DEC02.1067
C MODIFY TGA (AND TGB) TO REPRESENT AREA AVERAGE OF SEAICE GROUND TEMPERATURE    19DEC02.1068
C AND OCEAN WATER TEMPERATURE AT FREEZING POINT (271.4 K)                        19DEC02.1069
              TGAN(I,J)=SEAICN(I,J)*TGAN(I,J)+(1.-SEAICN(I,J))*271.4             19DEC02.1070
              TGBN(I,J)=TGAN(I,J)                                                19DEC02.1071
C                                                                                19DEC02.1072
C SET TMN TO FREEZING POINT OF SEA WATER (271.4 K) FOR SEAICE GRID POINTS        19DEC02.1073
C SET SNOWC TO 1 FOR SEAICE AND PERMANENT ICE GRID POINTS                        19DEC02.1074
C                                                                                19DEC02.1075
              IF(SEAICN(I,J).GT.0.02.AND.(IEXSI.GE.1))THEN                       19DEC02.1076
                TMNN(I,J)=271.4                                                  19DEC02.1077
                SNOWCN(I,J)=1.0                                                  19DEC02.1078
              ENDIF                                                              19DEC02.1079
              IF(NINT(SATBRN(I,J)).EQ.ISICE)THEN                                 19DEC02.1080
                SNOWCN(I,J)=1.0                                                  19DEC02.1081
              ENDIF                                                              19DEC02.1082
            ELSE                                                                 19DEC02.1083
              TGSIN(I,J)=-1.*TGAN(I,J)                                           19DEC02.1084
            ENDIF                                                                19DEC02.1085
          ENDDO                                                                  19DEC02.1086
        ENDDO                                                                    19DEC02.1087
      ELSE                                                                       19DEC02.1088
C                                                                                19DEC02.1089
C IEXSI.LT.1                                                                     19DEC02.1090
C                                                                                19DEC02.1091
        DO I=1,INL                                                               19DEC02.1092
          DO J=1,JNL                                                             19DEC02.1093
            SEAICN(I,J)=0.0                                                      19DEC02.1094
            TGSIN(I,J)=0.0                                                       19DEC02.1095
          ENDDO                                                                  19DEC02.1096
        ENDDO                                                                    19DEC02.1097
      ENDIF                                                                      19DEC02.1098
C                                                                                INITNEST.1162
C--- PRINT OUT INFORMATION                                                       INITNEST.1163
C                                                                                INITNEST.1164
      CALL ADDRX1N(IAXALL(1,NUMNES))                                             07NOV00.877
   99 CONTINUE                                                                   INITNEST.1165
      N=NUMNES                                                                   INITNEST.1166
      PRINT9000                                                                  INITNEST.1167
      IF(IDRY(N).EQ.1)ICUPA(N)=1                                                 INITNEST.1168
      IF(IDRY(N).EQ.1)PRINT1000,IDRY(N)                                          INITNEST.1169
      IF(IMPHYS(N).EQ.1)PRINT1001,IMPHYS(N)                                      INITNEST.1170
      IF(IDRY(N).EQ.0.AND.IMOIST(N).EQ.0)PRINT1010,IMOIST(N)                     INITNEST.1171
      IF(IMPHYS(N).EQ.2)PRINT1002,IMPHYS(N)                                      INITNEST.1172
      IF(IDRY(N).EQ.0.AND.IMOIST(N).EQ.1)PRINT1020,IMOIST(N)                     INITNEST.1173
      IF(IDRY(N).EQ.0.AND.IMOIST(N).EQ.2)THEN                                    INITNEST.1174
        IF(IMPHYS(N).EQ.3)PRINT1021,IMPHYS(N)                                    INITNEST.1175
        IF(IMPHYS(N).EQ.4)PRINT1022,IMPHYS(N)                                    INITNEST.1176
        IF(IMPHYS(N).EQ.5)PRINT1023,IMPHYS(N)                                    INITNEST.1177
        IF(IMPHYS(N).EQ.6)PRINT1024,IMPHYS(N)                                    INITNEST.1178
        IF(IMPHYS(N).EQ.7)PRINT1025,IMPHYS(N)                                    INITNEST.1179
        IF(IMPHYS(N).EQ.8)PRINT1027,IMPHYS(N)                                    INITNEST.1180
        PRINT1020,IMOIST(N)                                                      INITNEST.1181
        IF(IFDRY.EQ.1)PRINT1029                                                  INITNEST.1182
        IF(IEVAP(N).LT.0)PRINT1041,IEVAP(N)                                      INITNEST.1183
        IF(IEVAP(N).EQ.0)PRINT1042                                               INITNEST.1184
      ENDIF                                                                      INITNEST.1185
      IF(ICUPA(N).EQ.1)PRINT1031,ICUPA(N)                                        INITNEST.1186
      IF(ICUPA(N).EQ.2)PRINT1032,ICUPA(N)                                        INITNEST.1187
      IF(ICUPA(N).EQ.2.AND.ICUSTB.EQ.0)PRINT1039                                 INITNEST.1188
      IF(ICUPA(N).EQ.3)PRINT1033,ICUPA(N)                                        INITNEST.1189
      IF(ICUPA(N).EQ.4)PRINT1034,ICUPA(N)                                        INITNEST.1190
      IF(ICUPA(N).EQ.5)PRINT1035,ICUPA(N)                                        INITNEST.1191
      IF(ICUPA(N).EQ.6)PRINT1036,ICUPA(N)                                        INITNEST.1192
      IF(ICUPA(N).EQ.7)PRINT1037,ICUPA(N)                                        INITNEST.1193
      IF(ICUPA(N).EQ.8)PRINT1049,ICUPA(N)                                        05DEC01.426
      IF(ISHALLO(N).EQ.1)PRINT1038                                               INITNEST.1194
C     IF(IEXICE.EQ.1.AND.IMOIST(N).EQ.2)                                         INITNEST.1195
C    1   PRINT *,'THIS RUN WITH ICE PHYSICS EFFECTS, NOT MIXED PHASE'            INITNEST.1196
C                                                                                INITNEST.1197
      IF(IBLTYP(N).EQ.0)PRINT1040,IBLTYP(N)                                      INITNEST.1198
      IF(IBLTYP(N).EQ.1)PRINT1050,IBLTYP(N)                                      INITNEST.1199
      IF(IBLTYP(N).EQ.2)PRINT1060,IBLTYP(N)                                      INITNEST.1200
      IF(IBLTYP(N).EQ.3)PRINT1061,IBLTYP(N)                                      INITNEST.1201
      IF(IBLTYP(N).EQ.4)PRINT1062,IBLTYP(N)                                      INITNEST.1202
      IF(IBLTYP(N).EQ.5)PRINT1063,IBLTYP(N)                                      INITNEST.1203
      IF(IBLTYP(N).EQ.6)PRINT1064,IBLTYP(N)                                      INITNEST.1204
      IF(IBLTYP(N).EQ.7)PRINT1065,IBLTYP(N)                                      07NOV00.878
      IF(IBLTYP(N).EQ.7.AND.IOVERW(N).NE.1) THEN                                 07NOV00.879
         PRINT *,'IBLTYP=7 REQUIRES IOVERW=1 FOR NEST INPUT'                     07NOV00.880
         STOP 'REQUIRE MMINPUT FILE FOR THE NEST'                                07NOV00.881
      END IF                                                                     07NOV00.882
      IF(IMVDIF.EQ.1)PRINT1132                                                   INITNEST.1205
      IF(IVMIXM(N).EQ.0.AND.IBLTYP(N).EQ.2)PRINT1130                             INITNEST.1206
      IF(IBLTYP(N).EQ.1.AND.ICDCON(N).EQ.1)PRINT1120,CD,CDSEA,CH,CHSEA           INITNEST.1207
      IF(ITGFLG(N).EQ.1)PRINT1070,ITGFLG(N),JULDAY,GMT                           INITNEST.1208
      IF(ITGFLG(N).EQ.3)PRINT1090,ITGFLG(N)                                      INITNEST.1209
      IF(ISFFLX(N).EQ.0)PRINT1100,ISFFLX(N)                                      INITNEST.1210
      IF(ISFFLX(N).EQ.1)PRINT1110,ISFFLX(N)                                      INITNEST.1211
      IF(ISFFLX(N).NE.1.AND.ITGFLG(N).NE.1)GOTO 760                              INITNEST.1212
      IF(ISFPAR(N).EQ.0)PRINT1160                                                INITNEST.1213
      IF(ISFPAR(N).EQ.1)PRINT1170                                                INITNEST.1214
  760 CONTINUE                                                                   INITNEST.1216
      IF(IMOIAV(N).EQ.1)PRINT1168,IMOIAV(N)                                      07NOV00.883
      IF(IMOIAV(N).EQ.2)PRINT1169,IMOIAV(N)                                      07NOV00.884
      IF(IFSNOW(N).EQ.1)PRINT1171                                                INITNEST.1217
      IF(IFSNOW(N).EQ.2)PRINT1177                                                05DEC01.427
      IF(ICLOUD(N).EQ.0)PRINT1140                                                INITNEST.1218
      IF(ICLOUD(N).EQ.1)PRINT1150                                                INITNEST.1219
      IF(ICLOUD(N).EQ.2.AND.IFRAD.EQ.3)PRINT1152                                 05DEC01.428
      PRINT1151,NTRAD(N)                                                         INITNEST.1220
C                                                                                INITNEST.1221
  770 IF(IBOUDY(N).EQ.2)PRINT1184                                                INITNEST.1222
      IF(IFEED.EQ.0)PRINT1185,IFEED                                              INITNEST.1223
      IF(IFEED.EQ.1)PRINT1186,IFEED                                              INITNEST.1224
      IF(IFEED.EQ.2)PRINT1187,IFEED                                              INITNEST.1225
      IF(IFEED.EQ.3)PRINT1188,IFEED                                              INITNEST.1226
      IF(IFEED.EQ.4)PRINT1189,IFEED                                              INITNEST.1227
      PRINT1250,DTN                                                              INITNEST.1228
      PRINT1260,DXN                                                              INITNEST.1229
      PRINT1270,JNL,INL                                                          INITNEST.1230
      PRINT1280,KNL                                                              INITNEST.1231
      PRINT1290,XKHZN                                                            INITNEST.1232
      PRINT1300,XKHMAN                                                           INITNEST.1233
C                                                                                INITNEST.1234
 9000 FORMAT (//)                                                                INITNEST.1235
 1000 FORMAT(' IDRY   = ',I3,'  THIS NEST IS DRY.')                              INITNEST.1236
 1001 FORMAT(' IMPHYS = ',I3,'  THIS NEST IS DRY WITH PASSIVE ',                 07NOV00.885
     1 'MOISTURE VARIABLES.')                                                    INITNEST.1238
 1010 FORMAT(12X,'IMOIST = ',I3)                                                 INITNEST.1239
 1002 FORMAT(' IMPHYS = ',I3,'  THIS NEST IS MOIST WITH STABLE ',                07NOV00.886
#ifndef MPP1                                                                     INITNEST.1241
     1 '(OR NONCONVECIVE) PRECIP SCHEME.')                                       INITNEST.1242
#else                                                                            INITNEST.1243
     1 'PRECIP SCHEME.')                                                         INITNEST.1244
#endif                                                                           INITNEST.1245
 1020 FORMAT(12X,'IMOIST = ',I3)                                                 INITNEST.1246
 1038 FORMAT(12X,'SHALLOW CONVECTION IS INCLUDED.')                              INITNEST.1247
 1039 FORMAT(12X,'STABILITY CHECK IN ANTHES-KUO SCHEME IS TURNED OFF.')          INITNEST.1248
 1021 FORMAT(' IMPHYS = ',I3,'  WARM RAIN SCHEME (HSIE).')                       INITNEST.1249
 1022 FORMAT(' IMPHYS = ',I3,'  SIMPLE ICE SCHEME (DUDHIA).')                    INITNEST.1250
 1023 FORMAT(' IMPHYS = ',I3,'  MIXED PHASE SCHEME (REISNER).')                  INITNEST.1251
 1024 FORMAT(' IMPHYS = ',I3,'  GRAUPEL INCLUDED (GODDARD).')                    INITNEST.1252
 1025 FORMAT(' IMPHYS = ',I3,'  GRAUPEL INCLUDED (REISNER).')                    INITNEST.1253
 1027 FORMAT(' IMPHYS = ',I3,'  GRAUPEL INCLUDED (SCHULTZ).')                    INITNEST.1254
 1029 FORMAT(12X,'IFDRY  = ',I3,'  LATENT HEATING NOT CONSIDERED')               INITNEST.1255
 1041 FORMAT(12X,'IEVAP  = ',I3,'  EVAPORATION EFFECT OF RAINWATER ',            INITNEST.1256
     1 'NOT CONSIDERED IN THIS EXPLICIT SCHEME.')                                INITNEST.1257
 1042 FORMAT(12X,'IEVAP  = ',I3,'  EVAPORATION EFFECT OF BOTH RAIN',             INITNEST.1258
     1 'WATER AND CLOUD WATER NOT CONSIDERED IN THIS EXPLICIT SCHEME.')          INITNEST.1259
 1031 FORMAT(/' ICUPA  = ',I3,'  NO CONVECTIVE PARAMETERIZATION')                INITNEST.1260
 1032 FORMAT(/' ICUPA  = ',I3,'  ANTHES KUO CONVECTIVE ',                        INITNEST.1261
     1 'PARAMETERIZATION IS USED')                                               INITNEST.1262
 1033 FORMAT(/' ICUPA  = ',I3,'  GRELL CONVECTIVE PARAMETERIZATION ',            INITNEST.1263
     1 'IS USED')                                                                INITNEST.1264
 1034 FORMAT(/' ICUPA  = ',I3,'  MODIFIED ARAKAWA SCUBERT ',                     INITNEST.1265
     1 'CONVECTIVE PARAMETERIZATION  IS USED')                                   INITNEST.1266
 1035 FORMAT(/' ICUPA  = ',I3,'  FRITSCH/CHAPPEL CONVECTIVE ',                   INITNEST.1267
     1 'PARAMETERIZATION SCHEME.')                                               INITNEST.1268
 1036 FORMAT(/' ICUPA  = ',I3,'  KAIN/FRITSCH CONVECTIVE ',                      INITNEST.1269
     1 'PARAMETERIZATION SCHEME.')                                               INITNEST.1270
 1037 FORMAT(/' ICUPA  = ',I3,'  BETTS/MILLER CONVECTIVE ',                      INITNEST.1271
     1 'PARAMETERIZATION SCHEME.')                                               INITNEST.1272
 1049 FORMAT(/' ICUPA  = ',I3,'  KAIN/FRITSCH 2 CONVECTIVE ',                    05DEC01.429
     1 'PARAM SCHEME WITH SHALLOW CONVECTION OPTION.')                           05DEC01.430
 1040 FORMAT(/' IBLTYP = ',I3,'  FRICTIONLESS AND INSULATED FOR THE ',           INITNEST.1273
     1 'LOWER BOUNDARY.')                                                        INITNEST.1274
 1050 FORMAT(/' IBLTYP = ',I3,'  BULK PBL IS USED.')                             INITNEST.1275
 1060 FORMAT(/' IBLTYP = ',I3,'  BLACKADAR PBL IS USED.')                        INITNEST.1276
 1061 FORMAT(/' IBLTYP = ',I3,'  BURK-THOMPSON PBL PARAMETERIZATION ',           INITNEST.1277
     1 'IS USED.')                                                               INITNEST.1278
 1062 FORMAT(/' IBLTYP = ',I3,'  ETA MELLOR-YAMADA PBL IS USED.')                INITNEST.1279
 1063 FORMAT(/' IBLTYP = ',I3,'  MRF PBL IS USED.')                              INITNEST.1280
 1064 FORMAT(/' IBLTYP = ',I3,'  GAYNO-SEAMAN PBL IS USED.')                     INITNEST.1281
 1065 FORMAT(/' IBLTYP = ',I3,'  PLEIM-XIU PBL AND LSM ARE USED.')               07NOV00.887
 1070 FORMAT(/' ITGFLG = ',I3,'  THE SURFACE ENERGY BUDGET IS USED TO ',         INITNEST.1282
     1 'CALCULATE THE GROUND TEMPERATURE.',/,                                    INITNEST.1283
     2 12X,'JULDAY = ',I3,'  GMT = ',F4.1)                                       INITNEST.1284
 1090 FORMAT(/' ITGFLG = ',I3,'  THE GROUND TEMPERATURE DOES NOT VARY ',         INITNEST.1285
     1 'IN TIME.')                                                               INITNEST.1286
 1100 FORMAT(' ISFFLX = ',I3, '  HEAT AND MOISTURE FLUXES FROM THE ',            INITNEST.1287
     1 'GROUND ARE TURNED OFF.')                                                 INITNEST.1288
 1110 FORMAT(' ISFFLX = ',I3, '  HEAT AND MOISTURE FLUXES FROM THE ',            INITNEST.1289
     1 'GROUND ARE CONSIDERED.')                                                 INITNEST.1290
 1120 FORMAT(12X,'DRAG COEFFICIENTS ARE CONSTANTS, CD = ',F5.4,                  INITNEST.1291
     1 ',   CDSEA = ',F5.4,',   CT = ',F5.4,',   CTSEA = ',F5.4)                 INITNEST.1292
 1130 FORMAT(12X,'VERTICAL MIXING OF MOMENTUM IS TURNED OFF.')                   INITNEST.1293
 1132 FORMAT(12X,'VERTICAL MIXING MOIST ADIABATIC IN CLOUDS.')                   INITNEST.1294
 1140 FORMAT(12X,'THE RADIATION EFFECTS DUE TO CLOUDS ARE NOT ',                 INITNEST.1295
     1 'CONSIDERED.')                                                            INITNEST.1296
 1150 FORMAT(12X,'THE RADIATION EFFECTS DUE TO CLOUDS ARE CONSIDERED.')          INITNEST.1297
 1152 FORMAT(12X,'THE CLOUD EFFECT IN CCM2 IS CONSIDERED THROUGH RH.')           05DEC01.431
 1151 FORMAT(12X,'THE RADIATION IS COMPUTED EVERY ',I4,' STEPS.')                INITNEST.1298
 1160 FORMAT(12X,'SURFACE PARAMETERS ARE CONSTANT OVER THE WHOLE ',              INITNEST.1299
     1 'DOMAIN.')                                                                INITNEST.1300
 1170 FORMAT(12X,'SURFACE PARAMETERS ARE VARIABLE.')                             INITNEST.1301
 1168 FORMAT(' IMOIAV = ',I3, '  MOISTURE AVAIL WILL VARY IN TIME ',             07NOV00.888
     1 'USING BUCKET MODEL,',/,15X,'USING TABLE VALUE TO START.')                07NOV00.889
 1169 FORMAT(' IMOIAV = ',I3, '  MOISTURE AVAIL WILL VARY IN TIME ',             07NOV00.890
     1 'USING BUCKET MODEL,',/,15X,                                              07NOV00.891
     2 'USING SOIL-MOISTURE CONVERTED VALUE TO START.')                          07NOV00.892
 1171 FORMAT(12X,'IFSNOW=1: SNOW EFFECT IS CONSIDERED.')                         05DEC01.432
 1177 FORMAT(12X,'IFSNOW=2: SNOW COVER AMOUNT IS PREDICTED.')                    05DEC01.433
 1184 FORMAT(/' TIME DEPENDENT BOUNDARY CONDITIONS ARE USED.')                   INITNEST.1308
 1185 FORMAT(/' IFEED  = ',I3,'  NO FEEDBACK IS USED.')                          INITNEST.1309
 1186 FORMAT(/' IFEED  = ',I3,'  WEIGHTED 9-PT FEEDBACK IS USED.')               INITNEST.1310
 1187 FORMAT(/' IFEED  = ',I3,'  1-PT FEEDBACK IS USED.')                        INITNEST.1311
 1188 FORMAT(/' IFEED  = ',I3,'  1-PT FEEDBACK WITH LIGHT SMOOTHING.')           INITNEST.1312
 1189 FORMAT(/' IFEED  = ',I3,'  1-PT FEEDBACK WITH HEAVY SMOOTHING.')           INITNEST.1313
 1210 FORMAT('0 K',4X,'SIGMA(K)',4X,' A(K)',5X,'DSIGMA(K)',4X,'TWT(K,1)'         INITNEST.1314
     1,5X,'TWT(K,2)',5X,'QCON(K)'/)                                              INITNEST.1315
 1220 FORMAT (1X,I2,5X,F6.4,5X,F6.4,5X,F6.4,5X,F8.4,5X,F8.4,5X,F8.4)             INITNEST.1316
 1230 FORMAT (1X,I2,5X,F6.4)                                                     INITNEST.1317
 1240 FORMAT(//' MAXIMUM TIME      = ',F7.0,' MINUTES')                          25JAN00.208
 1250 FORMAT(' TIME STEP         = ',F7.2,' SECONDS')                            INITNEST.1319
 1260 FORMAT(' FINE MESH DX      = ',F7.0,' METERS')                             INITNEST.1320
 1270 FORMAT(' GRID POINTS (X,Y) = (',I3,',',I3,')')                             INITNEST.1321
 1280 FORMAT(' NUMBER OF LEVELS  = ',I2,/)                                       INITNEST.1322
 1290 FORMAT(' CONSTANT HOR. DIFF. COEF. = ',E12.5,' M*M/S')                     INITNEST.1323
 1300 FORMAT(' MAXIMUM  HOR. DIFF. COEF. = ',E12.5,' M*M/S')                     INITNEST.1324
C--- THE REST                                                                    INITNEST.1325
      TDNINI=0.                                                                  INITNEST.1326
      TDNADV=0.                                                                  INITNEST.1327
      TQNINI=0.                                                                  INITNEST.1328
      TQNADV=0.                                                                  INITNEST.1329
      TQNEVA=0.                                                                  INITNEST.1330
      TQNRAI=0.                                                                  INITNEST.1331
      IF(IBLTYP(N).EQ.0)GOTO 320                                                 INITNEST.1332
      DO 310 J=1,JNLX                                                            INITNEST.1333
        DO 310 I=1,INLX                                                          INITNEST.1334
          MOLN(I,J)=0.                                                           INITNEST.1335
          ZOLN(I,J)=0.                                                           INITNEST.1336
          HOLN(I,J)=0.                                                           INITNEST.1337
          PBLN(I,J)=0.                                                           INITNEST.1338
          HFXN(I,J)=0.                                                           INITNEST.1339
          QFXN(I,J)=0.                                                           INITNEST.1340
          USTN(I,J)=0.                                                           INITNEST.1341
          PRWN(I,J)=0.                                                           INITNEST.1342
          GSWN(I,J)=0.                                                           INITNEST.1343
          GLWN(I,J)=0.                                                           INITNEST.1344
          REGIMN(I,J)=0.                                                         INITNEST.1345
  310   CONTINUE                                                                 INITNEST.1346
                                                                                 INITNEST.1347
  320 CONTINUE                                                                   INITNEST.1348
cmic$ do all autoscope                                                           INITNEST.1349
c$doacross                                                                       INITNEST.1350
c$& share (jxxn,iyyn,psan,psbn,xmuu,xnuu,xmut,xnut),                             INITNEST.1351
c$& local(i,j)                                                                   INITNEST.1352
c$omp parallel do default(shared)                                                INITNEST.1353
c$omp&private(i,j)                                                               INITNEST.1354
#ifdef MPP1                                                                      INITNEST.1355
#  include <mpp_initnest_35.incl>                                                INITNEST.1356
#endif                                                                           INITNEST.1357
      DO 420 J=1,JXXN-1                                                          INITNEST.1358
        DO 420 I=1,IYYN-1                                                        INITNEST.1359
          PSAN(I,J)=PSBN(I,J)                                                    INITNEST.1360
          XMUU(I,J,NUMNES)=OMUHF                                                 INITNEST.1361
          XNUU(I,J,NUMNES)=GNUHF                                                 INITNEST.1362
          XMUT(I,J,NUMNES)=OMUHF                                                 INITNEST.1363
          XNUT(I,J,NUMNES)=GNUHF                                                 INITNEST.1364
  420   CONTINUE                                                                 INITNEST.1365
                                                                                 INITNEST.1366
  425 CONTINUE                                                                   INITNEST.1367
#ifndef MPP1                                                                     INITNEST.1368
      CALL TMASS(TDNINI,TQNINI,TVMASS,TCMASS,TRMASS,INL,JNL,KNL,IDRY(N),         INITNEST.1369
     +     IMOIST(N),PSAN,QVAN,QCAN,QRAN,DSIGMA,DXN,G)                           INITNEST.1370
      PRINT430,TDNINI,TQNINI                                                     INITNEST.1371
  430 FORMAT(' *** INITIAL TOTAL AIR = ',E12.5,' KG, TOTAL WATER = ',            INITNEST.1372
     1       E12.5,' KG IN NESTED DOMAIN.')                                      INITNEST.1373
#endif                                                                           INITNEST.1374
      DO 435 J=1,JXXN                                                            INITNEST.1375
        XMUU(1,J,NUMNES)=1.                                                      INITNEST.1376
        XNUU(1,J,NUMNES)=0.                                                      INITNEST.1377
        XMUT(1,J,NUMNES)=1.                                                      INITNEST.1378
        XNUT(1,J,NUMNES)=0.                                                      INITNEST.1379
        XMUU(2,J,NUMNES)=1.                                                      INITNEST.1380
        XNUU(2,J,NUMNES)=0.                                                      INITNEST.1381
        XMUT(2,J,NUMNES)=1.                                                      INITNEST.1382
        XNUT(2,J,NUMNES)=0.                                                      INITNEST.1383
        I=IYYN                                                                   INITNEST.1384
        XMUU(I,J,NUMNES)=1.                                                      INITNEST.1385
        XNUU(I,J,NUMNES)=0.                                                      INITNEST.1386
        I=IYYN-1                                                                 INITNEST.1387
        XMUT(I,J,NUMNES)=1.                                                      INITNEST.1388
        XNUT(I,J,NUMNES)=0.                                                      INITNEST.1389
        XMUU(I,J,NUMNES)=1.                                                      INITNEST.1390
        XNUU(I,J,NUMNES)=0.                                                      INITNEST.1391
        I=IYYN-2                                                                 INITNEST.1392
        XMUT(I,J,NUMNES)=1.                                                      INITNEST.1393
        XNUT(I,J,NUMNES)=0.                                                      INITNEST.1394
  435 CONTINUE                                                                   INITNEST.1395
      DO 437 I=3,IYYN-2                                                          INITNEST.1396
        J=1                                                                      INITNEST.1397
        XMUT(I,J,NUMNES)=1.                                                      INITNEST.1398
        XNUT(I,J,NUMNES)=0.                                                      INITNEST.1399
        XMUU(I,J,NUMNES)=1.                                                      INITNEST.1400
        XNUU(I,J,NUMNES)=0.                                                      INITNEST.1401
        J=2                                                                      INITNEST.1402
        XMUT(I,J,NUMNES)=1.                                                      INITNEST.1403
        XNUT(I,J,NUMNES)=0.                                                      INITNEST.1404
        XMUU(I,J,NUMNES)=1.                                                      INITNEST.1405
        XNUU(I,J,NUMNES)=0.                                                      INITNEST.1406
        J=JXXN                                                                   INITNEST.1407
        XMUU(I,J,NUMNES)=1.                                                      INITNEST.1408
        XNUU(I,J,NUMNES)=0.                                                      INITNEST.1409
        J=JXXN-1                                                                 INITNEST.1410
        XMUU(I,J,NUMNES)=1.                                                      INITNEST.1411
        XNUU(I,J,NUMNES)=0.                                                      INITNEST.1412
        XMUT(I,J,NUMNES)=1.                                                      INITNEST.1413
        XNUT(I,J,NUMNES)=0.                                                      INITNEST.1414
        J=JXXN-2                                                                 INITNEST.1415
        XMUT(I,J,NUMNES)=1.                                                      INITNEST.1416
        XNUT(I,J,NUMNES)=0.                                                      INITNEST.1417
  437 CONTINUE                                                                   INITNEST.1418
C                                                                                INITNEST.1419
cmic$ do all autoscope                                                           INITNEST.1420
c$doacross                                                                       INITNEST.1421
c$& share(inl,jnl,wtij),                                                         INITNEST.1422
c$& local(i,j)                                                                   INITNEST.1423
c$omp parallel do default(shared)                                                INITNEST.1424
c$omp&private(i,j)                                                               INITNEST.1425
      DO J=1,JNL                                                                 INITNEST.1426
        DO I=1,INL                                                               INITNEST.1427
          WTIJ(I,J,NUMNES)=0.                                                    INITNEST.1428
        ENDDO                                                                    INITNEST.1429
      ENDDO                                                                      INITNEST.1430
cmic$ do all autoscope                                                           INITNEST.1431
c$doacross                                                                       INITNEST.1432
c$& share(inl,jnl,wtij)                                                          INITNEST.1433
c$& local(i,j,wti,wtj)                                                           INITNEST.1434
c$omp parallel do default(shared)                                                INITNEST.1435
c$omp&private(i,j,wti,wtj)                                                       INITNEST.1436
#ifndef MPP1                                                                     INITNEST.1437
      DO J=5,JNL-5                                                               INITNEST.1438
        WTJ=1.0                                                                  INITNEST.1439
        IF(J.LT.8)WTJ=0.25*FLOAT(J-4)                                            INITNEST.1440
        IF(J.GT.JNL-8)WTJ=0.25*FLOAT(JNL-4-J)                                    INITNEST.1441
        DO I=5,INL-5                                                             INITNEST.1442
          WTI=1.0                                                                INITNEST.1443
          IF(I.LT.8)WTI=0.25*FLOAT(I-4)                                          INITNEST.1444
          IF(I.GT.INL-8)WTI=0.25*FLOAT(INL-4-I)                                  INITNEST.1445
          WTIJ(I,J,NUMNES)=WTI                                                   INITNEST.1446
          IF(WTI.GT.WTJ)WTIJ(I,J,NUMNES)=WTJ                                     INITNEST.1447
        ENDDO                                                                    INITNEST.1448
      ENDDO                                                                      INITNEST.1449
#else                                                                            INITNEST.1450
      DO J=5,JNL-5                                                               INITNEST.1451
        WTJ=1.0                                                                  INITNEST.1452
        IF(J.LT.8)WTJ=0.25*FLOAT(FLIC_L2G_N(J)-4)                                INITNEST.1453
        IF(J.GT.JNL-8)WTJ=0.25*FLOAT(JNL-4-FLIC_L2G_N(J))                        INITNEST.1454
        DO I=5,INL-5                                                             INITNEST.1455
          WTI=1.0                                                                INITNEST.1456
          IF(I.LT.8)WTI=0.25*FLOAT(FLIC_L2G_M(I)-4)                              INITNEST.1457
          IF(I.GT.INL-8)WTI=0.25*FLOAT(INL-4-FLIC_L2G_M(I))                      INITNEST.1458
          WTIJ(I,J,NUMNES)=WTI                                                   INITNEST.1459
          IF(WTI.GT.WTJ)WTIJ(I,J,NUMNES)=WTJ                                     INITNEST.1460
        ENDDO                                                                    INITNEST.1461
      ENDDO                                                                      INITNEST.1462
#endif                                                                           INITNEST.1463
C                                                                                INITNEST.1464
      ICUT=1                                                                     INITNEST.1465
cmic$ do all autoscope                                                           INITNEST.1466
c$doacross                                                                       INITNEST.1467
c$& share(inl,jnl,icut,insi,jnsj),                                               INITNEST.1468
c$& local(i,j,nij,in,jn)                                                         INITNEST.1469
c$omp parallel do default(shared)                                                INITNEST.1470
c$omp&private(i,j,nij,in,jn)                                                     INITNEST.1471
      DO NIJ=-6,6                                                                INITNEST.1472
        DO J=1,JNL-1                                                             INITNEST.1473
#ifndef MPP1                                                                     INITNEST.1474
          JN=J+NIJ                                                               INITNEST.1475
#else                                                                            INITNEST.1476
          JN=FLIC_L2G_N(J+NIJ)                                                   INITNEST.1477
#endif                                                                           INITNEST.1478
          IF(JN.LT.2+ICUT)JN=2+ICUT                                              INITNEST.1479
          IF(JN.GT.JNL-2-ICUT)JN=JNL-2-ICUT                                      INITNEST.1480
          JNSJ(J,NIJ,NUMNES)=JN                                                  INITNEST.1481
        ENDDO                                                                    INITNEST.1482
C                                                                                INITNEST.1483
        DO I=1,INL-1                                                             INITNEST.1484
#ifndef MPP1                                                                     INITNEST.1485
          IN=I+NIJ                                                               INITNEST.1486
#else                                                                            INITNEST.1487
          IN=FLIC_L2G_M(I+NIJ)                                                   INITNEST.1488
#endif                                                                           INITNEST.1489
          IF(IN.LT.2+ICUT)IN=2+ICUT                                              INITNEST.1490
          IF(IN.GT.INL-2-ICUT)IN=INL-2-ICUT                                      INITNEST.1491
          INSI(I,NIJ,NUMNES)=IN                                                  INITNEST.1492
        ENDDO                                                                    INITNEST.1493
      ENDDO                                                                      INITNEST.1494
      AMAXROT=-99.                                                               INITNEST.1495
      AMINROT=99.                                                                INITNEST.1496
#ifdef MPP1                                                                      INITNEST.1497
#  include <mpp_initnest_37.incl>                                                INITNEST.1498
#endif                                                                           INITNEST.1499
CFLIC_RUNPAD(2)                                                                  INITNEST.1500
      DO 438 J=2,JXXN-1                                                          INITNEST.1501
        DO 438 I=2,IYYN-1                                                        INITNEST.1502
          EFN(I,J)=0.                                                            INITNEST.1503
          COSRTN(I,J)=0.                                                         INITNEST.1504
          SINRTN(I,J)=0.                                                         INITNEST.1505
          EXN(I,J)=0.                                                            INITNEST.1506
          CRXN(I,J)=0.                                                           INITNEST.1507
          SRXN(I,J)=0.                                                           INITNEST.1508
          IF(ICOR3D.EQ.1)THEN                                                    INITNEST.1509
C     CALCULATE CONSTANTS FOR 3D CORIOLIS OPTION                                 INITNEST.1510
            XLATD=0.25*(XLATN(I-1,J-1)+XLATN(I-1,J)+XLATN(I,J-1)+                INITNEST.1511
     +            XLATN(I,J))                                                    INITNEST.1512
            EFN(I,J)=2.*EOMEG*COS(DEGRAD*XLATD)                                  INITNEST.1513
            DLONG=0.5*(XLONGN(I,J-1)+XLONGN(I,J)-XLONGN(I-1,J-1)-                INITNEST.1514
     +            XLONGN(I-1,J))                                                 INITNEST.1515
C     DATELINE                                                                   INITNEST.1516
            IF(DLONG.GT.180.)DLONG=DLONG-360.                                    INITNEST.1517
            IF(DLONG.LT.-180.)DLONG=DLONG+360.                                   INITNEST.1518
            DLAT=0.5*(XLATN(I,J-1)+XLATN(I,J)-XLATN(I-1,J-1)-                    INITNEST.1519
     +           XLATN(I-1,J))                                                   INITNEST.1520
            IF(ABS(DLAT).LT.1.E-5)DLAT=1.E-5                                     INITNEST.1521
C     ROTATION ANGLE OF NORTH RELATIVE TO Y-AXIS (+VE CLOCKWISE)                 INITNEST.1522
            ROTANG=-ATAN(DLONG/DLAT*COS(DEGRAD*XLATD))                           INITNEST.1523
            IF(DLAT.LT.0.)ROTANG=ROTANG+4.*ATAN(1.)                              INITNEST.1524
            COSRTN(I,J)=COS(ROTANG)                                              INITNEST.1525
            SINRTN(I,J)=SIN(ROTANG)                                              INITNEST.1526
            AMAXROT=AMAX1(ROTANG,AMAXROT)                                        INITNEST.1527
            AMINROT=AMIN1(ROTANG,AMINROT)                                        INITNEST.1528
          ENDIF                                                                  INITNEST.1529
  438   CONTINUE                                                                 INITNEST.1530
                                                                                 INITNEST.1531
      IF(ICOR3D.EQ.1)THEN                                                        INITNEST.1532
cmic$ do all autoscope                                                           INITNEST.1533
c$doacross                                                                       INITNEST.1534
c$& share(iyyn,jxxn,                                                             INITNEST.1535
c$&       exn,crxn,srxn,cosrtn,sinrtn,efn),                                      INITNEST.1536
c$& local(i,j)                                                                   INITNEST.1537
c$omp parallel do default(shared)                                                23DEC04.294
c$omp&private(i,j)                                                               23DEC04.295
        DO J=2,JXXN-2                                                            INITNEST.1538
          DO I=2,IYYN-2                                                          INITNEST.1539
            EXN(I,J)=0.25*(EFN(I,J)+EFN(I+1,J)+EFN(I,J+1)+EFN(I+1,J+1))          INITNEST.1540
            CRXN(I,J)=0.25*(COSRTN(I,J)+COSRTN(I+1,J)+COSRTN(I,J+1)+             INITNEST.1541
     +                COSRTN(I+1,J+1))                                           INITNEST.1542
            SRXN(I,J)=0.25*(SINRTN(I,J)+SINRTN(I+1,J)+SINRTN(I,J+1)+             INITNEST.1543
     +                SINRTN(I+1,J+1))                                           INITNEST.1544
          ENDDO                                                                  INITNEST.1545
        ENDDO                                                                    INITNEST.1546
CFLIC_RUNPAD(0)                                                                  INITNEST.1547
        DMAXROT=AMAXROT/DEGRAD                                                   INITNEST.1548
        DMINROT=AMINROT/DEGRAD                                                   INITNEST.1549
      ENDIF                                                                      INITNEST.1550
C     PRINT *,' FULL 3D CORIOLIS'                                                INITNEST.1551
C     PRINT *,' MAX ROT. ANG. = ',DMAXROT                                        INITNEST.1552
C     PRINT *,' MIN ROT. ANG. = ',DMINROT                                        INITNEST.1553
      IF(ISOIL.EQ.2)THEN                                                         INITNEST.1554
         PRINT *,'skip ADJUST NNRP SOIL MOISTURE VALUES IN INITNEST'             INITNEST.1555
         IF(IOVERW(N).EQ.0)GOTO 2001                                             05DEC01.434
C COMMENT THE FOLLOWING LINE IF USING THIS ADJUSTMENT (ALSO IN INIT)             23DEC04.296
         GOTO 2001                                                               05DEC01.435
        DO J=1,JNLX                                                              INITNEST.1556
          DO I=1,INLX                                                            INITNEST.1557
C                                                                                INITNEST.1560
C            Reduce the high NNRP soil moisture from Month 1-6                   INITNEST.1561
            IF(JULDAY.LE.183)THEN                                                INITNEST.1562
              DO NS=1,MLX                                                        INITNEST.1563
                IF(SMCBN(I,J,NS).GT.0.28)THEN                                    INITNEST.1564
                  SMCBN(I,J,NS)=MIN(0.7*SMCBN(I,J,NS)+0.084,0.35)                INITNEST.1565
                  SMCAN(I,J,NS)=SMCBN(I,J,NS)                                    INITNEST.1566
                ENDIF                                                            INITNEST.1567
              ENDDO                                                              INITNEST.1568
            ELSE                                                                 INITNEST.1569
C          Increase the low NNRP soil moisture from Month 7-12                   INITNEST.1570
              DO NS=1,MLX                                                        INITNEST.1571
                IF(SMCBN(I,J,NS).LT.0.28)THEN                                    INITNEST.1572
                  IF(SMCBN(I,J,NS).GE.0.21)THEN                                  INITNEST.1573
                    SMCBN(I,J,NS)=0.429*SMCBN(I,J,NS)+0.16                       INITNEST.1574
                    SMCAN(I,J,NS)=SMCBN(I,J,NS)                                  INITNEST.1575
                  ELSE                                                           INITNEST.1576
                    SMCBN(I,J,NS)=1.19*SMCBN(I,J,NS)                             INITNEST.1577
                    SMCAN(I,J,NS)=SMCBN(I,J,NS)                                  INITNEST.1578
                  ENDIF                                                          INITNEST.1579
                ENDIF                                                            INITNEST.1580
              ENDDO                                                              INITNEST.1581
            ENDIF                                                                INITNEST.1582
          ENDDO                                                                  INITNEST.1584
        ENDDO                                                                    INITNEST.1585
 2001   CONTINUE                                                                 05DEC01.436
      ENDIF                                                                      INITNEST.1586
      IF(ISFPAR(N).EQ.1)GOTO 450                                                 INITNEST.1587
cmic$ do all autoscope                                                           INITNEST.1589
c$omp parallel do default(shared)                                                INITNEST.1590
c$omp&private(i,j)                                                               INITNEST.1591
      DO 440 J=1,JNLX                                                            INITNEST.1592
        DO 440 I=1,INLX                                                          INITNEST.1593
          SATBRN(I,J)=0.                                                         INITNEST.1594
          ALBN(I,J)=ALBLND                                                       INITNEST.1595
          THCN(I,J)=THINLD                                                       INITNEST.1596
          EMISSN(I,J)=1.0                                                        INITNEST.1597
          IF(HTN(I,J)-1.E-6.GE.0)THEN                                            INITNEST.1598
            ZNTN(I,J)=ZZLND                                                      INITNEST.1599
          ELSE                                                                   INITNEST.1600
            ZNTN(I,J)=ZZWTR                                                      INITNEST.1601
          ENDIF                                                                  INITNEST.1602
          IF(HTN(I,J)-1.E-6.GE.0)THEN                                            INITNEST.1603
            XLANDN(I,J)=1.0                                                      INITNEST.1604
          ELSE                                                                   INITNEST.1605
            XLANDN(I,J)=2.0                                                      INITNEST.1606
          ENDIF                                                                  INITNEST.1607
          IF(HTN(I,J)-1.E-6.GE.0)THEN                                            INITNEST.1608
            MAVAIN(I,J)=XMAVA                                                    INITNEST.1609
          ELSE                                                                   INITNEST.1610
            MAVAIN(I,J)=1.0                                                      INITNEST.1611
          ENDIF                                                                  INITNEST.1612
  440   CONTINUE                                                                 INITNEST.1613
                                                                                 INITNEST.1614
      GOTO 470                                                                   INITNEST.1615
  450 CONTINUE                                                                   INITNEST.1616
#ifndef MPP1                                                                     INITNEST.1619
#else                                                                            INITNEST.1621
#  include <mpp_initnest_40.incl>                                                INITNEST.1622
#endif                                                                           INITNEST.1623
      ISTOP=0                                                                    INITNEST.1624
cmic$ do all autoscope                                                           INITNEST.1625
c$omp parallel do default(shared)                                                INITNEST.1626
c$omp&private(i,j,is)                                                            INITNEST.1627
      DO 460 J=1,JNLX                                                            INITNEST.1628
        DO 460 I=1,INLX                                                          INITNEST.1629
          IS=INT(SATBRN(I,J)+0.001)                                              INITNEST.1630
          IF(IS.LT.0.OR.IS.GT.LUN)THEN                                           INITNEST.1631
            PRINT *,'ERROR: LANDUSE OUTSIDE RANGE =',IS,' AT ',I,J               INITNEST.1632
            ISTOP=1                                                              INITNEST.1633
          ENDIF                                                                  INITNEST.1634
C   SET NO-DATA POINTS (IS=0) TO WATER                                           INITNEST.1635
          IF(IS.EQ.0)THEN                                                        INITNEST.1636
            IS=ISWATER                                                           INITNEST.1637
          ENDIF                                                                  INITNEST.1638
          ALBN(I,J)=ALBD(IS,ISN)/100.                                            INITNEST.1639
          THCN(I,J)=THERIN(IS,ISN)/100.                                          INITNEST.1640
          ZNTN(I,J)=SFZ0(IS,ISN)/100.                                            INITNEST.1641
          EMISSN(I,J)=SFEM(IS,ISN)                                               INITNEST.1642
          IF(IMOIAV(NESCOU).NE.2.OR.IS.EQ.ISWATER)                               07NOV00.893
     -      MAVAIN(I,J)=SLMO(IS,ISN)                                             07NOV00.894
          IF(IS.NE.ISWATER)THEN                                                  INITNEST.1644
            XLANDN(I,J)=1.0                                                      INITNEST.1645
          ELSE                                                                   INITNEST.1646
            XLANDN(I,J)=2.0                                                      INITNEST.1647
          ENDIF                                                                  INITNEST.1648
          IF(SEAICN(I,J).GT.0.02)THEN                                            19DEC02.1099
C                                                                                19DEC02.1100
C CALCULATE AVERAGE SURFACE PROPERTIES FOR SEAICE GRID POINTS AS SIMPLE AVERAGE  19DEC02.1101
C OF SEAICE SURFACE PROPERTIES AND WATER SURFACE PROPERTIES                      19DEC02.1102
C                                                                                19DEC02.1103
            ALBN(I,J)=ALBD(ISICE,ISN)/100.*SEAICN(I,J)+                          19DEC02.1104
     +        ALBD(ISWATER,ISN)/100.*(1.-SEAICN(I,J))                            19DEC02.1105
            THCN(I,J)=THERIN(ISICE,ISN)/100.                                     19DEC02.1106
c sea-ice roughness=0.1 cm                                                       19DEC02.1107
            ZNTN(I,J)=0.1/100.                                                   19DEC02.1108
            EMISSN(I,J)=SFEM(ISICE,ISN)*SEAICN(I,J)+                             19DEC02.1109
     +        SFEM(ISWATER,ISN)*(1.-SEAICN(I,J))                                 19DEC02.1110
            MAVAIN(I,J)=SLMO(ISICE,ISN)*SEAICN(I,J)+                             19DEC02.1111
     +        SLMO(ISWATER,ISN)*(1.-SEAICN(I,J))                                 19DEC02.1112
C TREAT SEAICE GRID POINTS AS LAND GRID POINTS                                   19DEC02.1113
            XLANDN(I,J)=1.0                                                      19DEC02.1114
          ENDIF                                                                  19DEC02.1115
  460   CONTINUE                                                                 INITNEST.1649
                                                                                 INITNEST.1650
      IF(ISTOP.EQ.1)STOP 'INITNEST: LAND-USE'                                    INITNEST.1651
      IF(IFSNOW(N).EQ.0.AND.ISOIL.NE.2)GOTO 470                                  INITNEST.1652
C     FOR LSM SNOWC IS SET ACCORDING TO WEASD IN INPUT (SNOA > 0.1E-3)           INITNEST.1653
cmic$ do all autoscope                                                           INITNEST.1654
c$omp parallel do default(shared)                                                INITNEST.1655
c$omp&private(i,j,is,snoalb,albbrd,shdfac,shdmin,sncovr)                         19DEC02.1116
      DO 465 J=1,JNLX                                                            INITNEST.1657
        DO 465 I=1,INLX                                                          INITNEST.1658
          IS=INT(SATBRN(I,J)+0.001)                                              INITNEST.1659
C         comment following line if snowc contains sea-ice (and in init.         INITNEST.1660
          IF(XLANDN(I,J).GT.1.5)SNOWCN(I,J)=-0.01                                INITNEST.1661
          IF(SNOWCN(I,J).GT.0.)THEN                                              INITNEST.1662
            ALBN(I,J)=ALBN(I,J)+SCFX(IS)*ALBN(I,J)                               INITNEST.1663
            THCN(I,J)=THERIN(ISICE,ISN)/100.                                     INITNEST.1664
            EMISSN(I,J)=SFEM(ISICE,ISN)                                          INITNEST.1665
            MAVAIN(I,J)=SLMO(ISICE,ISN)                                          INITNEST.1666
          ENDIF                                                                  INITNEST.1667
C         uncomment these lines if snowc contains sea-ice (and in init.F         INITNEST.1668
C           IF(XLANDN(I,J).GT.1.5.AND.SNOWCN(I,J).GT.0.)THEN                     05DEC01.437
C             XLANDN(I,J)=1.0                                                    INITNEST.1670
C             PRINT *,'SNOWC OVER WATER ASSUMED TO BE SEA ICE AT ',I,J           INITNEST.1671
C           ENDIF                                                                INITNEST.1672
          IF(ISOIL.EQ.2.AND.XICEN(I,J).GT.0.5)THEN                               INITNEST.1673
C         SEA ICE ALBEDO AND ROUGHNESS                                           INITNEST.1674
            ALBN(I,J)=ALBD(ISICE,ISN)/100.                                       INITNEST.1675
            ZNTN(I,J)=SFZ0(ISICE,ISN)/100.                                       INITNEST.1676
C         SEA ICE WILL HAVE XLAND=1 (LAND VALUE)                                 INITNEST.1677
            XLANDN(I,J)=1.0                                                      INITNEST.1678
          ENDIF                                                                  INITNEST.1679
C  NEW NOAH ALBEDO CALCULATION                                                   19DEC02.1117
          IF(ISOIL.EQ.2)THEN                                                     19DEC02.1118
C                                                                                19DEC02.1119
            IF(RDMAXALB) THEN                                                    19DEC02.1120
              SNOALB=ALBMAXN(I,J)*0.01                                           19DEC02.1121
            ELSE                                                                 19DEC02.1122
              SNOALB=MAXALB(IS)*0.01                                             19DEC02.1123
            ENDIF                                                                19DEC02.1124
C                                                                                19DEC02.1125
            IF(RDBRDALB) THEN                                                    19DEC02.1126
              ALBBRD=ALBEDON(I,J)*0.01                                           19DEC02.1127
            ELSE                                                                 19DEC02.1128
              ALBBRD=ALBD(IS,ISN)*0.01                                           19DEC02.1129
            ENDIF                                                                19DEC02.1130
C                                                                                19DEC02.1131
            SHDFAC = VEGFRCN(I,J)/100.                                           19DEC02.1132
            SHDMIN = XVEGMNN(I,J)/100.                                           19DEC02.1133
            SNCOVR = SNOWCN(I,J)                                                 19DEC02.1134
C                                                                                19DEC02.1135
            ALBN(I,J) = ALBBRD + (1.0- (SHDFAC - SHDMIN))* SNCOVR *              19DEC02.1136
     -                 (SNOALB - ALBBRD)                                         19DEC02.1137
                                                                                 19DEC02.1138
          ENDIF                                                                  19DEC02.1139
  465   CONTINUE                                                                 INITNEST.1680
                                                                                 INITNEST.1681
  470 CONTINUE                                                                   07NOV00.895
C                                                                                07NOV00.896
C-----CALCULATE SLAB THERMAL CAPACITY CAPGN(I,J): IT ARE DEPENDENT ON            07NOV00.897
C     THCN(I,J) - SOIL THERMAL INERTIAL, ONLY.                                   07NOV00.898
C                                                                                07NOV00.899
cmic$ do all autoscope                                                           07NOV00.900
c$omp parallel do default(shared)                                                07NOV00.901
c$omp&private(i,j)                                                               07NOV00.902
      DO 480 J=1,JNLX                                                            07NOV00.903
        DO 480 I=1,INLX                                                          07NOV00.904
          CAPGN(I,J)=3.298E6*THCN(I,J)                                           07NOV00.905
          IF(ISOIL.EQ.1)THEN                                                     07NOV00.906
C---- CAPG REPRESENTS SOIL HEAT CAPACITY (J/K/M^3) WHEN DIFSL=5.E-7 (M^2/S)      07NOV00.907
C     TO GIVE A CORRECT THERMAL INERTIA (=CAPG*DIFSL^0.5)                        07NOV00.908
          IF(NINT(SATBRN(I,J)).EQ.ISICE)THEN                                     19DEC02.1140
C              BASED UPON YEN (1981)                                             19DEC02.1141
C              DENSITY * SPECIFIC HEAT / MOL / K / MOLECULAR WEIGHT              19DEC02.1142
            CAPGN(I,J)=917.*34000./18.                                           19DEC02.1143
          ELSEIF(SEAICN(I,J).GT.0.02)THEN                                        19DEC02.1144
C              DENSITY * SPECIFIC HEAT / KG / K                                  19DEC02.1145
            CAPGN(I,J)=900.*3400.                                                19DEC02.1146
          ELSE                                                                   19DEC02.1147
            CAPGN(I,J)=5.9114E7*THCN(I,J)                                        07NOV00.909
          ENDIF                                                                  19DEC02.1148
          ENDIF                                                                  07NOV00.910
  480   CONTINUE                                                                 07NOV00.911
                                                                                 INITNEST.1719
C   WATER TEMP ADJUSTMENT TO MATCH COASTLINES                                    25JAN00.209
      DO J=1,JNLX                                                                25JAN00.210
        DO I=1,INLX                                                              25JAN00.211
C       CHECK IF WATER POINT AND IF TSS > 10 K (I.E. IT IS DEFINED)              25JAN00.212
          IF(XLANDN(I,J).GT.1.5.AND.TSSN(I,J).GT.10.)THEN                        25JAN00.213
            TGAN(I,J)=TSSN(I,J)                                                  25JAN00.214
            TGBN(I,J)=TSSN(I,J)                                                  25JAN00.215
          ENDIF                                                                  25JAN00.216
        ENDDO                                                                    25JAN00.217
      ENDDO                                                                      25JAN00.218
  550 CONTINUE                                                                   INITNEST.1721
cmic$ do all autoscope                                                           INITNEST.1722
c$omp parallel do default(shared)                                                INITNEST.1723
c$omp&private(i,j)                                                               INITNEST.1724
      DO 560 J=1,JNLX                                                            INITNEST.1725
        DO 560 I=1,INLX                                                          INITNEST.1726
          RAINPN(I,J)=0.                                                         INITNEST.1727
          IF(IKFFC.EQ.1) THEN                                                    05DEC01.438
             NCAN(I,J)=-100                                                      05DEC01.439
             NCADN(I,J)=-100                                                     05DEC01.440
             TNCAN(i,j)=0.                                                       05DEC01.441
             PSRCN(I,J)=0.                                                       05DEC01.442
             PCLBN(I,J)=0.                                                       05DEC01.443
             UMFBN(I,J)=0.                                                       05DEC01.444
             SPSRCN(I,J)=0.                                                      05DEC01.445
             SPCLBN(I,J)=0.                                                      05DEC01.446
             SUMFBN(I,J)=0.                                                      05DEC01.447
             CINN(I,J)=0.                                                        05DEC01.448
          END IF                                                                 05DEC01.449
  560   CONTINUE                                                                 INITNEST.1729
                                                                                 INITNEST.1730
      IF(IKFFC.EQ.1)THEN                                                         INITNEST.1731
        DO 289 J=1,MJX                                                           INITNEST.1732
          DO 289 I=1,MIX                                                         INITNEST.1733
            RAINCVN(I,J)=0.                                                      INITNEST.1734
            DO 289 K=1,KL                                                        INITNEST.1735
              DTDTN(I,J,K)=0.                                                    INITNEST.1736
              DQDTN(I,J,K)=0.                                                    INITNEST.1737
              DUDTN(I,J,K)=0.                                                    INITNEST.1738
              DVDTN(I,J,K)=0.                                                    INITNEST.1739
              DQRDTN(I,J,K)=0.                                                   INITNEST.1740
              DQLDTN(I,J,K)=0.                                                   INITNEST.1741
              DQSDTN(I,J,K)=0.                                                   INITNEST.1742
              DQIDTN(I,J,K)=0.                                                   INITNEST.1743
              W0AVGN(I,J,K)=0.                                                   INITNEST.1744
  289       CONTINUE                                                             INITNEST.1745
                                                                                 INITNEST.1746
                                                                                 INITNEST.1747
      ENDIF                                                                      INITNEST.1748
C     DO 565 J=1,JNLX                                                            INITNEST.1749
CCCC    SATBRN(1,J)=SATBRN(1,J)+100.                                             INITNEST.1750
C 565 CONTINUE                                                                   INITNEST.1751
C                                                                                INITNEST.1752
C                                                                                INITNEST.1753
C                                                                                INITNEST.1754
C--- FDDA INITIALIZATION                                                         INITNEST.1755
C                                                                                INITNEST.1756
      CALL ADDRX1C(IAXALL(1,NUMNES))                                             INITNEST.1757
#ifdef MPP1                                                                      INITNEST.1758
      IF(IRDDIM.EQ.1)THEN                                                        INITNEST.1759
        DO K=1,MKXR                                                              INITNEST.1760
          DO J=1,MJXR                                                            INITNEST.1761
            DO I=1,MIXR                                                          INITNEST.1762
              RTTEN(I,J,K)=0.                                                    INITNEST.1763
            ENDDO                                                                INITNEST.1764
          ENDDO                                                                  INITNEST.1765
        ENDDO                                                                    INITNEST.1766
      ENDIF                                                                      INITNEST.1767
#endif                                                                           INITNEST.1768
      CALL EQUATE(PSA,MIX,MJX,1,PSAINI,MIX,MJX,1)                                INITNEST.1769
C ADD PERTURBATION PRESSURE TO PSAINI IF NONHYDROSTATIC                          INITNEST.1770
      DO 23 ISML=1,ILX                                                           INITNEST.1771
        DO 23 JSML=1,JLX                                                         INITNEST.1772
          PSAINI(ISML,JSML)=PSA(ISML,JSML)+PPA(ISML,JSML,KL)/                    INITNEST.1773
     +                      PSA(ISML,JSML)/1000.                                 INITNEST.1774
   23   CONTINUE                                                                 INITNEST.1775
                                                                                 INITNEST.1776
      IF(IFDDAG.EQ.0.AND.IFDDAO.EQ.0)GOTO 9999                                   INITNEST.1777
C THIS OPTION NOT TESTED YET                                                     INITNEST.1778
      DO 1505 NV=1,NVAR                                                          INITNEST.1779
        IDHK(NV)=1                                                               INITNEST.1780
        TIME(NV)=0.                                                              INITNEST.1781
        NTE(NV)=1                                                                INITNEST.1782
 1505 CONTINUE                                                                   INITNEST.1783
      DO 1510 NOC=1,NCHA                                                         INITNEST.1784
        IDDATE(NOC)=-999                                                         INITNEST.1785
        DO 1510 NOV=1,NVAR                                                       INITNEST.1786
          IDCHK(NOC,NOV)=1                                                       INITNEST.1787
 1510   CONTINUE                                                                 INITNEST.1788
                                                                                 INITNEST.1789
C                                                                                INITNEST.1790
C                                                                                INITNEST.1791
C ECHO THE IDDATE CHANGES...                                                     INITNEST.1792
C                                                                                INITNEST.1793
      IF(I4D(NUMNES,2).NE.1)GOTO 1517                                            INITNEST.1794
      IF(IDDATE(1).NE.-999)THEN                                                  INITNEST.1795
        DO 1515 NOC=1,NCHA                                                       INITNEST.1796
          IF(IDDATE(NOC).EQ.-999)GOTO 1517                                       INITNEST.1797
          PRINT1516,IDDATE(NOC),(IDCHK(NOC,NN),NN=1,8)                           INITNEST.1798
1516  FORMAT(1X,'IDDATE: IDCHK(NV=1,8)  ',I8,2X,8(2X,I1))                        INITNEST.1799
 1515   CONTINUE                                                                 INITNEST.1800
      ENDIF                                                                      INITNEST.1801
 1517 CONTINUE                                                                   INITNEST.1802
C                                                                                INITNEST.1803
C                                                                                INITNEST.1804
      IF(I4DI(NUMNES).EQ.1.AND.NIOBF.EQ.1)THEN                                   INITNEST.1805
        PRINT1306                                                                INITNEST.1806
1306  FORMAT(' !XXX! ATTEMPTING OBS NUDGING WITH NIOBF STILL SET',               INITNEST.1807
     1 ' TO ONE...')                                                             INITNEST.1808
        STOP 1306                                                                INITNEST.1809
      ENDIF                                                                      INITNEST.1810
C                                                                                05DEC01.450
      IF(I4D(NUMNES,1).NE.1)GOTO 900                                             INITNEST.1820
C COMPUTE WXYTOP AND/OR WXYTOPN FOR FDDA BASED ON TERRAIN HEIGHT                 INITNEST.1821
cmic$ do all autoscope                                                           INITNEST.1822
c$omp parallel do default(shared)                                                INITNEST.1823
c$omp&private(i,j,maxj,minj,maxi,mini)                                           INITNEST.1824
      DO 810 J=1,JLX                                                             INITNEST.1825
        DO 810 I=1,ILX                                                           INITNEST.1826
          WXYTOP(I,J)=1.0                                                        INITNEST.1827
          IF(HT(I,J).LT.0)THEN                                                   INITNEST.1828
            WXYTOP(I,J)=0.2                                                      INITNEST.1829
            MAXJ=J+1                                                             INITNEST.1830
            MAXJ=MIN0(JLX,MAXJ)                                                  INITNEST.1831
            MINJ=J-1                                                             INITNEST.1832
            MINJ=MAX0(1,MINJ)                                                    INITNEST.1833
            MAXI=I+1                                                             INITNEST.1834
            MAXI=MIN0(ILX,MAXI)                                                  INITNEST.1835
            MINI=I-1                                                             INITNEST.1836
            MINI=MAX0(1,MINI)                                                    INITNEST.1837
            DO 820 II=MINI,MAXI                                                  INITNEST.1838
              DO 820 JJ=MINJ,MAXJ                                                INITNEST.1839
                IF(HT(II,JJ).GT.0)THEN                                           INITNEST.1840
                  WXYTOP(I,J)=0.5                                                INITNEST.1841
                  GOTO 810                                                       INITNEST.1842
                ENDIF                                                            INITNEST.1843
  820         CONTINUE                                                           INITNEST.1844
                                                                                 INITNEST.1845
          ENDIF                                                                  INITNEST.1846
  810   CONTINUE                                                                 INITNEST.1847
                                                                                 INITNEST.1848
      DO 830 I=1,IL                                                              INITNEST.1849
        WXYTOP(I,JL)=WXYTOP(I,JLX)                                               INITNEST.1850
  830 CONTINUE                                                                   INITNEST.1851
      DO 840 J=1,JL                                                              INITNEST.1852
        WXYTOP(IL,J)=WXYTOP(ILX,J)                                               INITNEST.1853
  840 CONTINUE                                                                   INITNEST.1854
  900 CONTINUE                                                                   INITNEST.1855
#ifdef FDDAOBS1                                                                  INITNEST.1856
      IF(I4DI(NUMNES).EQ.1)THEN                                                  INITNEST.1857
        CALL IN4DOB(NUMNES,XTIME,KTAU,KTAUR,DTMIN,JULDAY,GMT)                    INITNEST.1858
        PRINT *,' CALLING IN4DOB FOR DOMAIN ',NUMNES                             INITNEST.1859
      ENDIF                                                                      INITNEST.1860
#endif                                                                           INITNEST.1861
#ifdef FDDAGD1                                                                   INITNEST.1862
      IF(I4D(NUMNES,1).EQ.1)CALL IN4DGD(NUMNES,IL,JL,KL)                         INITNEST.1863
#endif                                                                           INITNEST.1864
      CALL ADDRX1C(IAXALL(1,1))                                                  INITNEST.1865
      CALL ADDRX1N(IAXALL(1,NUMNES))                                             INITNEST.1866
 9999 CONTINUE                                                                   INITNEST.1867
C                                                                                07NOV00.912
C-----INITIALIZE LOCATIONS FOR TIME SERIES OUTPUT                                07NOV00.913
C                                                                                07NOV00.914
      IF (IFTSOUT) THEN                                                          07NOV00.915
         CALL ADDRX1C(IAXALL(1,NUMNES))                                          07NOV00.916
         CALL INITTS(NUMNES,IL,JL)                                               07NOV00.917
         CALL ADDRX1C(IAXALL(1,1))                                               07NOV00.918
      ENDIF                                                                      07NOV00.919
#ifndef MPP1                                                                     INITNEST.1868
      IF(IFPRT.EQ.1)THEN                                                         INITNEST.1869
        CALL ADDRX1C(IAXALL(1,NUMNES))                                           INITNEST.1870
        CALL OUTPRT(1,NUMNES)                                                    INITNEST.1871
        CALL ADDRX1C(IAXALL(1,1))                                                INITNEST.1872
      ENDIF                                                                      INITNEST.1873
#else                                                                            INITNEST.1874
#  include <mpp_initnest_50.incl>                                                INITNEST.1875
#endif                                                                           INITNEST.1876
C                                                                                INITNEST.1877
C                                                                                INITNEST.1878
      RETURN                                                                     INITNEST.1879
      END                                                                        INITNEST.1880
                                                                                 INITNEST.1881
