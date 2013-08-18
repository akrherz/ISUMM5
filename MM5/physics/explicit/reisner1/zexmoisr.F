#include <defines.incl>                                                          ZEXMOISR.1
      SUBROUTINE ZEXMOISR(QC3DTEN,QR3DTEN,QI3DTEN,QNI3DTEN,QV3DTEN,              ZEXMOISR.2
     &                   T3DTEN,T3D,QV3D,QC3D,QR3D,QI3D,QNI3D,PP3D,              ZEXMOISR.3
     &                   PSB,RPSB,RAINNC,SNOWNCT,IN,J,IST,IEN)                   ZEXMOISR.4
      IMPLICIT NONE                                                              ZEXMOISR.5
                                                                                 ZEXMOISR.6
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          ZEXMOISR.7
C                                                                     C          ZEXMOISR.8
C                                                                     C          ZEXMOISR.9
C     THIS SUBROUTINE COMPUTES THE MOISTURE TENDENCIES                C          ZEXMOISR.10
C     THE PROGNOSTIC EQUATIONS INCLUDE                                C          ZEXMOISR.11
C     WATER VAPOR (QV3DTEN), CLOUD WATER (QC3DTEN), RAIN WATER        C          ZEXMOISR.12
C     (QR3DTEN), CLOUD ICE (QI3DTEN), AND SNOW (QNI3DTEN)             C          ZEXMOISR.13
C                                                                     C          ZEXMOISR.14
C     IN = 1 : FOR LARGE DOMAIN                                       C          ZEXMOISR.15
C        = 2 : FOR NESTED DOMAIN                                      C          ZEXMOISR.16
C                                                                     C          ZEXMOISR.17
C     ALL THE OTHER ARGUMENTS ARE PASSED FROM SUBROUTINE "SOLVE" AND  C          ZEXMOISR.18
C     EXPLAINED IN "SOLVE".                                           C          ZEXMOISR.19
C                                                                     C          ZEXMOISR.20
C     CHANGES                                                         C          ZEXMOISR.21
C       JRG COMMON BLOCK MADE CONSISTENT WITH EXMOISG                 C          ZEXMOISR.22
C       VALUE OF CNP CHANGED                                          C          ZEXMOISR.23
C                                                                     C          ZEXMOISR.24
C     ADD FLAG (MEYERS) FOR USE OF MEYERS ICE NUCLEI CONC. EQ         C          ZEXMOISR.25
C     IN PLACE OF FLETCHER EQ                                         C          ZEXMOISR.26
C                                                                     C          ZEXMOISR.27
C                                                                     C          ZEXMOISR.28
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          ZEXMOISR.29
C_FLIC_BEGIN_NOFLIC                                                              ZEXMOISR.30
#     include <parame.incl>                                                      ZEXMOISR.31
#     include <varia.incl>                                                       ZEXMOISR.32
#     include <param3.incl>                                                      ZEXMOISR.33
#     include <param2.incl>                                                      ZEXMOISR.34
#     include <pmoist.incl>                                                      ZEXMOISR.35
#     include <various.incl>                                                     ZEXMOISR.36
#     include <nhcnst.incl>                                                      ZEXMOISR.37
C_FLIC_END_NOFLIC                                                                ZEXMOISR.38
#     include <jrg.incl>                                                         ZEXMOISR.39
      REAL RPSB(MIX,MJX)                                                         ZEXMOISR.40
      REAL RPSC(MIX,MJX)                                                         ZEXMOISR.41
                                                                                 ZEXMOISR.42
C INPUT ARRAYS                                                                   ZEXMOISR.43
                                                                                 ZEXMOISR.44
      REAL  T3D(MIX,MJX,MKX),        QV3D(MIX,MJX,MKX),                          ZEXMOISR.45
     &      U3D(MIX,MJX,MKX),         V3D(MIX,MJX,MKX),                          ZEXMOISR.46
     &     QC3D(MIXM,MJXM,MKXM),     QR3D(MIXM,MJXM,MKXM),                       ZEXMOISR.47
     &     QI3D(MIXIC,MJXIC,MKXIC), QNI3D(MIXIC,MJXIC,MKXIC),                    ZEXMOISR.48
     &     PP3D(MIX,MJX,MKX),         PSB(MIX,MJX)                               ZEXMOISR.49
      REAL RAINNC(MIX,MJX),      SNOWNCT(MIX,MJX)                                ZEXMOISR.50
                                                                                 ZEXMOISR.51
C ARRAYS FOR BASE VARIABLES                                                      ZEXMOISR.52
                                                                                 ZEXMOISR.53
      REAL TAOUT(MIX,MKX),PRES(MIX,MKX),RHO(MIX),                                ZEXMOISR.54
     1     T3DTEN(MIX,MJX,MKX),SCR7(MIX,MKX),SCR60(MIX,MKX)                      ZEXMOISR.55
                                                                                 ZEXMOISR.56
                                                                                 ZEXMOISR.57
#ifndef VECTOR                                                                   ZEXMOISR.58
      REAL FR(mix,MKX),FS(mix,MKX),FI(mix,MKX),RHOFAC(mix,MKX)                   ZEXMOISR.59
      REAL FALOUTR(MKX),FALOUTS(MKX),FALOUTI(MKX)                                ZEXMOISR.60
#else                                                                            ZEXMOISR.61
      REAL FR(MIX,MKX),FS(MIX,MKX),FI(MIX,MKX),RHOFAC(MIX,MKX)                   ZEXMOISR.62
      REAL FALOUTR(MIX,MKX),FALOUTS(MIX,MKX),FALOUTI(MIX,MKX)                    ZEXMOISR.63
#endif                                                                           ZEXMOISR.64
                                                                                 ZEXMOISR.65
C ARRAYS FOR QV                                                                  ZEXMOISR.66
                                                                                 ZEXMOISR.67
      REAL QAOUT(MIX,MKX),QVQVS(MIX,MKX),QVQVSI(MIX,MKX),                        ZEXMOISR.68
     1     QVS(MIX),QVSI(MIX),AB(MIX),prdi(MIX,MKX),                             ZEXMOISR.69
     1     ABI(MIX),ABW(MIX),QV3DTEN(MIX,MJX,MKX)                                ZEXMOISR.70
                                                                                 ZEXMOISR.71
C ARRAYS FOR QC                                                                  ZEXMOISR.72
                                                                                 ZEXMOISR.73
      REAL CLOUD(MIX),PRC(MIX),                                                  ZEXMOISR.74
     1     NUFCI(MIX),PSACW(MIX),                                                ZEXMOISR.75
     1     PRA(MIX),QC3DTEN(MIXM,MJXM,MKXM),SCR3(MIX,MKX)                        ZEXMOISR.76
                                                                                 ZEXMOISR.77
C ARRAYS FOR QR                                                                  ZEXMOISR.78
                                                                                 ZEXMOISR.79
      REAL RAIN(MIX),SLOR(MIX,MKX),                                              ZEXMOISR.80
     1     PRE(MIX),                                                             ZEXMOISR.81
     1     QR3DTEN(MIXM,MJXM,MKXM),SCR4R(MIX,MKX)                                ZEXMOISR.82
                                                                                 ZEXMOISR.83
C ARRAYS FOR QI                                                                  ZEXMOISR.84
                                                                                 ZEXMOISR.85
      REAL ICE(MIX,MKX),PRI(MIX),PRCI(MIX),                                      ZEXMOISR.86
     1     PRAI(MIX),                                                            ZEXMOISR.87
     1     PRD(MIX),QI3DTEN(MIXIC,MJXIC,MKXIC)                                   ZEXMOISR.88
      REAL SCR4I(MIX,MKX)                                                        ZEXMOISR.89
                                                                                 ZEXMOISR.90
C ARRAYS FOR QS                                                                  ZEXMOISR.91
                                                                                 ZEXMOISR.92
      REAL SNOW(MIX),SLOS(MIX,MKX),                                              ZEXMOISR.93
     1     PREI(MIX),                                                            ZEXMOISR.94
     1     PSMLT(MIX),PMLTEV(MIX),QNI3DTEN(MIXIC,MJXIC,MKXIC),                   ZEXMOISR.95
     1     SCR4S(MIX,MKX)                                                        ZEXMOISR.96
      REAL SONV(MIX,MKX)                                                         ZEXMOISR.97
                                                                                 ZEXMOISR.98
                                                                                 ZEXMOISR.99
C EXTRA ARRAYS                                                                   ZEXMOISR.100
                                                                                 ZEXMOISR.101
      REAL DUM11(MIX),DUM21(MIX),DUM31(MIX,MKX),                                 ZEXMOISR.102
     1     SCR4(MIX),SCR8                                                        ZEXMOISR.103
C DECLARATIONS FOR IMPLICIT NONE                                                 ZEXMOISR.104
      INTEGER I,J,K                                                              ZEXMOISR.105
      INTEGER IST,IEN,IN,ISAT(mix),N                                             ZEXMOISR.106
#ifdef VECTOR                                                                    ZEXMOISR.107
      INTEGER NSTEP(MIX)                                                         ZEXMOISR.108
      LOGICAL MORETODO                                                           ZEXMOISR.109
#else                                                                            ZEXMOISR.110
      INTEGER NSTEP(mix),nstep1                                                  ZEXMOISR.111
#endif                                                                           ZEXMOISR.112
      REAL EVS,EIS,QVS1,QVI1,XNC,XMI,SUPICE(mix),DI,XLATF,TEMP                   ZEXMOISR.113
      REAL RHO2(mix,mkx),SLOR1,SLOS1,VT2R,VT2S,RHO3,VSED,RGVM                    ZEXMOISR.114
      REAL FALTNDR,FALTNDS,FALTNDI                                               ZEXMOISR.115
      real r1, r2                                                                ZEXMOISR.116
      real temp1(mix),temp2(mix),temp3(mix),temp4(mix)                           ZEXMOISR.117
      real temp5(mix),temp6(mix),temp7(mix),temp8(mix)                           ZEXMOISR.118
      real rhor(mix),snowr(mix)                                                  ZEXMOISR.119
      real dsigmai(mkx),ttime,rnstep,dtf,dti,tem,dum1r,dum11i,tom                ZEXMOISR.120
      real scr6,consb,tmp1,tmp2,tmp3,bs1,psm2sq,depr2s,deps2s                    ZEXMOISR.121
      save dsigmai,ttime,tt                                                      ZEXMOISR.122
      data ttime/1.0/                                                            ZEXMOISR.123
      integer nn,nn1,is,is1,ii,ind(mix),icount                                   ZEXMOISR.124
      real*8 tt,t1,t2,rtc                                                        ZEXMOISR.125
      data tt/0/                                                                 ZEXMOISR.126
                                                                                 ZEXMOISR.127
C                                                                                ZEXMOISR.128
C                                                                                ZEXMOISR.129
C ADD FLAG FOR MEYERS ICE NUCLEI CONC. EQ                                        ZEXMOISR.130
      INTEGER MEYERS                                                             ZEXMOISR.131
C MEYERS=1 : USE MEYERS ICE NUCLEI CONC. EQ.                                     ZEXMOISR.132
C MEYERS=0 : USE FLETCHER or cooper ICE NUCLEI CONC. EQ.                         ZEXMOISR.133
      MEYERS= 0                                                                  ZEXMOISR.134
      IF(IPOLAR.EQ.1)MEYERS=1                                                    ZEXMOISR.135
      r1 = 1.e-25                                                                ZEXMOISR.136
                                                                                 ZEXMOISR.137
      if(ttime.gt.0.) then                                                       ZEXMOISR.138
         do k=1,mkx                                                              ZEXMOISR.139
            dsigmai(k) = 1./dsigma(k)                                            ZEXMOISR.140
         end do                                                                  ZEXMOISR.141
         ttime=-1.                                                               ZEXMOISR.142
      end if                                                                     ZEXMOISR.143
      dtf = DTMIN*6000./g                                                        ZEXMOISR.144
      consb = 1./CONST1B                                                         ZEXMOISR.145
      bs1 = 4.0 / (4-.94 * bs)                                                   ZEXMOISR.146
      dti=1./dt                                                                  ZEXMOISR.147
      psm2sq = sqrt(PSM2)                                                        ZEXMOISR.148
      depr2s = sqrt(DEPR2)                                                       ZEXMOISR.149
      deps2s = sqrt(DEPS2)                                                       ZEXMOISR.150
                                                                                 ZEXMOISR.151
#ifdef MPP1                                                                      ZEXMOISR.152
      nn=ie_x 0 ( IEN  ) - is_x 0 ( IST ) +1                                     ZEXMOISR.153
      nn1=ie_x 0 ( ilxm  ) - is_x 0 ( 2 ) +1                                     ZEXMOISR.154
      is = is_x 0 ( IST )                                                        ZEXMOISR.155
      is1 = is_x 0 ( 2 )                                                         ZEXMOISR.156
#else                                                                            ZEXMOISR.157
      nn = IEN - IST + 1                                                         ZEXMOISR.158
      nn1 = ILXM - 1                                                             ZEXMOISR.159
      is = IST                                                                   ZEXMOISR.160
      is1 = 2                                                                    ZEXMOISR.161
#endif                                                                           ZEXMOISR.162
                                                                                 ZEXMOISR.163
#ifndef VECTOR                                                                   ZEXMOISR.164
      DO I = IST, IEN                                                            ZEXMOISR.165
          RPSC(I,J) = RPSB(I,J)                                                  ZEXMOISR.166
      ENDDO                                                                      ZEXMOISR.167
#else                                                                            ZEXMOISR.168
      DO I = 1, MIX*MJX                                                          ZEXMOISR.169
          RPSC(I,J) = RPSB(I,J)                                                  ZEXMOISR.170
      ENDDO                                                                      ZEXMOISR.171
#endif                                                                           ZEXMOISR.172
                                                                                 ZEXMOISR.173
#ifdef MPP1                                                                      ZEXMOISR.174
#define nf(A) noflic(A)   
#else                                                                            ZEXMOISR.176
#define nf(A) A       
#endif                                                                           ZEXMOISR.178
                                                                                 ZEXMOISR.179
      DO K=1,KL                                                                  ZEXMOISR.180
        DO I=IST,IEN                                                             ZEXMOISR.181
           PRES(I,K)=(A(K)*PSB(I,J)+PTOP)*1000.+PP3D(I,J,K)                      ZEXMOISR.182
        end do                                                                   ZEXMOISR.183
      end do                                                                     ZEXMOISR.184
                                                                                 ZEXMOISR.185
      DO 15 K=1,KX                                                               ZEXMOISR.186
        DO I=2,ILXM                                                              ZEXMOISR.187
          TAOUT(I,K)=T3D(I,J,K)                                                  ZEXMOISR.188
          QAOUT(I,K)=QV3D(I,J,K)                                                 ZEXMOISR.189
          temp3(i) = CP*(1.+0.887*QAOUT(I,K))                                    ZEXMOISR.190
          temp1(i) = TAOUT(I,K)-SVP3                                             ZEXMOISR.191
          temp2(i) = TAOUT(I,K)                                                  ZEXMOISR.192
        end do                                                                   ZEXMOISR.193
        call vsrec(temp1(nf(is1)),temp1(nf(is1)),nn1)                            ZEXMOISR.194
        call vsrec(temp2(nf(is1)),temp2(nf(is1)),nn1)                            ZEXMOISR.195
        DO I=2,ILXM                                                              ZEXMOISR.196
          temp1(i) = SVP2*(TAOUT(I,K)-SVPT0) * temp1(i)                          ZEXMOISR.197
          temp2(i) = 22.514-6.15E3 * temp2(i)                                    ZEXMOISR.198
        end do                                                                   ZEXMOISR.199
                                                                                 ZEXMOISR.200
        call vsexp(temp1(nf(is1)),temp1(nf(is1)),nn1)                            ZEXMOISR.201
        call vsexp(temp2(nf(is1)),temp2(nf(is1)),nn1)                            ZEXMOISR.202
        call vsrec(temp3(nf(is1)),temp3(nf(is1)),nn1)                            ZEXMOISR.203
                                                                                 ZEXMOISR.204
        DO 15 I=2,ILXM                                                           ZEXMOISR.205
          EVS=SVP1* temp1(i)                                                     ZEXMOISR.206
          EIS=.611* temp2(i)                                                     ZEXMOISR.207
          QVS1=(pres(i,k)*0.001-EVS)/(.622*EVS)                                  ZEXMOISR.208
          QVI1=(pres(i,k)*0.001-EIS)/(.622*EIS)                                  ZEXMOISR.209
          QVQVS(I,K)=QV3D(I,J,K)  * QVS1                                         ZEXMOISR.210
          QVQVSI(I,K)=QV3D(I,J,K) * QVI1                                         ZEXMOISR.211
          prdi(i,k) = temp3(i)                                                   ZEXMOISR.212
   15   CONTINUE                                                                 ZEXMOISR.213
                                                                                 ZEXMOISR.214
                                                                                 ZEXMOISR.215
c ---------large k loop -------                                                  ZEXMOISR.216
                                                                                 ZEXMOISR.217
      DO 20 K=1,KL                                                               ZEXMOISR.218
        DO  I=IST,IEN                                                            ZEXMOISR.219
            temp1(i) = r*TAOUT(I,K)                                              ZEXMOISR.220
        end do                                                                   ZEXMOISR.221
        call vsrec(temp1(nf(is)),temp1(nf(is)),nn)                               ZEXMOISR.222
                                                                                 ZEXMOISR.223
        DO  I=IST,IEN                                                            ZEXMOISR.224
          RHO(I)=PRES(I,K)* temp1(i)                                             ZEXMOISR.225
          CLOUD(I)=AMAX1(r1,QC3D(I,J,K))                                         ZEXMOISR.226
          ICE(I,K)=AMAX1(r1,QI3D(I,J,K))                                         ZEXMOISR.227
          RAIN(I)=AMAX1(r1,QR3D(I,J,K))                                          ZEXMOISR.228
          SNOW(I)=AMAX1(r1,QNI3D(I,J,K))                                         ZEXMOISR.229
        end do                                                                   ZEXMOISR.230
                                                                                 ZEXMOISR.231
        call vsrec(RHOr(nf(is)),RHO(nf(is)),nn)                                  ZEXMOISR.232
        call vsrec(SNOWr(nf(is)),SNOW(nf(is)),nn)                                ZEXMOISR.233
                                                                                 ZEXMOISR.234
        icount = is - 1                                                          ZEXMOISR.235
        DO  I=IST,IEN                                                            ZEXMOISR.236
          IF(SNOW(I).LE.1.E-8)THEN                                               ZEXMOISR.237
            SONV(I,K)=SON                                                        ZEXMOISR.238
          ELSE                                                                   ZEXMOISR.239
                                                                                 ZEXMOISR.240
            tem = RHOr(i)*SNOWr(i)                                               ZEXMOISR.241
            icount = icount + 1                                                  ZEXMOISR.242
            ind(nf(icount)) = i                                                  ZEXMOISR.243
            temp1(nf(icount)) = PI * DSNOW * tem                                 ZEXMOISR.244
            temp2(nf(icount)) = DRAIN * CONSB * tem                              ZEXMOISR.245
                                                                                 ZEXMOISR.246
          ENDIF                                                                  ZEXMOISR.247
        end do                                                                   ZEXMOISR.248
                                                                                 ZEXMOISR.249
       if(icount.ge.is) then                                                     ZEXMOISR.250
        call vslog(temp1(nf(is)),temp1(nf(is)),icount)                           ZEXMOISR.251
                                                                                 ZEXMOISR.252
        do ii=is,icount                                                          ZEXMOISR.253
           i = nf(ii)                                                            ZEXMOISR.254
           temp1(nf(i)) = temp1(nf(i)) * (0.25 * BS)                             ZEXMOISR.255
        end do                                                                   ZEXMOISR.256
                                                                                 ZEXMOISR.257
        call vsexp(temp1(nf(is)),temp1(nf(is)),icount)                           ZEXMOISR.258
                                                                                 ZEXMOISR.259
        do ii=is,icount                                                          ZEXMOISR.260
           i = nf(ii)                                                            ZEXMOISR.261
           temp1(nf(i)) = temp1(nf(i)) * temp2(nf(i))                           
        end do                                                                   ZEXMOISR.263
                                                                                 ZEXMOISR.264
        call vslog(temp1(nf(is)),temp1(nf(is)),icount)                           ZEXMOISR.265
                                                                                 ZEXMOISR.266
        do ii=is,icount                                                          ZEXMOISR.267
           i = nf(ii)                                                            ZEXMOISR.268
           temp1(nf(i)) = temp1(nf(i)) * 0.94                                    ZEXMOISR.269
        end do                                                                   ZEXMOISR.270
                                                                                 ZEXMOISR.271
        call vsexp(temp1(nf(is)),temp1(nf(is)),icount)                           ZEXMOISR.272
                                                                                 ZEXMOISR.273
        do ii=is,icount                                                          ZEXMOISR.274
           i = nf(ii)                                                            ZEXMOISR.275
           temp1(nf(i)) = temp1(nf(i)) * CONST1A                                 ZEXMOISR.276
        end do                                                                   ZEXMOISR.277
                                                                                 ZEXMOISR.278
        call vslog(temp1(nf(is)),temp1(nf(is)),icount)                           ZEXMOISR.279
                                                                                 ZEXMOISR.280
        do ii=is,icount                                                          ZEXMOISR.281
           i = nf(ii)                                                            ZEXMOISR.282
           temp1(nf(i)) = temp1(nf(i)) * bs1                                     ZEXMOISR.283
        end do                                                                   ZEXMOISR.284
                                                                                 ZEXMOISR.285
        call vsexp(temp1(nf(is)),temp1(nf(is)),icount)                           ZEXMOISR.286
                                                                                 ZEXMOISR.287
        do ii=is,icount                                                          ZEXMOISR.288
           i = nf(ii)                                                            ZEXMOISR.289
           SONV(nf(ind(nf(i))),K) = temp1(nf(i))                                
        end do                                                                   ZEXMOISR.291
        endif                                                                    ZEXMOISR.292
                                                                                 ZEXMOISR.293
        DO  I=IST,IEN                                                            ZEXMOISR.294
          IF(SONV(I,K).GE.SON)SONV(I,K)=SON                                      ZEXMOISR.295
          SONV(I,K)=SONV(I,K)/SON                                                ZEXMOISR.296
          temp1(i) = RHO(I)*RAIN(I)/TOPR                                         ZEXMOISR.297
          temp2(i) = RHO(I)*SNOW(I)/(TOPS*SONV(I,K))                             ZEXMOISR.298
          temp3(i) =   TAOUT(I,K)**3                                             ZEXMOISR.299
        end do                                                                   ZEXMOISR.300
                                                                                 ZEXMOISR.301
        call vssqrt(temp1(nf(is)),temp1(nf(is)),nn)                              ZEXMOISR.302
        call vssqrt(temp1(nf(is)),temp1(nf(is)),nn)                              ZEXMOISR.303
        call vssqrt(temp2(nf(is)),temp2(nf(is)),nn)                              ZEXMOISR.304
        call vssqrt(temp2(nf(is)),temp2(nf(is)),nn)                              ZEXMOISR.305
        call vssqrt(temp3(nf(is)),temp3(nf(is)),nn)                              ZEXMOISR.306
        call  vslog(temp4(nf(is)),TAOUT(nf(is),k),nn)                            ZEXMOISR.307
        DO  I=IST,IEN                                                            ZEXMOISR.308
            temp4(i) =  - temp4(i) * 1.81                                        ZEXMOISR.309
            slor(i,k) = temp1(i)                                                 ZEXMOISR.310
            slos(i,k) = temp2(i)                                                 ZEXMOISR.311
            DUM21(I) = TAOUT(I,K)+120.                                           ZEXMOISR.312
        end do                                                                   ZEXMOISR.313
        call vsexp(temp4(nf(is)),temp4(nf(is)),nn)                               ZEXMOISR.314
        call vsrec(temp2(nf(is)),QAOUT(nf(is),k),nn)                             ZEXMOISR.315
        call vsrec(dum21(nf(is)),dum21(nf(is)),nn)                               ZEXMOISR.316
                                                                                 ZEXMOISR.317
        DO  I=IST,IEN                                                            ZEXMOISR.318
          DUM21(I) = 1.496E-6 * temp3(i) * DUM21(I)                              ZEXMOISR.319
          dum11(i) = 11371.39*PRES(I,K)*temp4(i)                                 ZEXMOISR.320
          QVS(I)= temp2(i) * QVQVS(I,K)                                          ZEXMOISR.321
          QVSI(I)=temp2(i) * QVQVSI(I,K)                                         ZEXMOISR.322
          DUM31(I,K)=3.1484E6-XLV1*TAOUT(I,K)                                    ZEXMOISR.323
          SCR4(I)=1.414E3*DUM21(I)                                               ZEXMOISR.324
          temp3(i) = SCR4(I)*RV*TAOUT(I,K)*TAOUT(I,K)                            ZEXMOISR.325
        end do                                                                   ZEXMOISR.326
                                                                                 ZEXMOISR.327
        call vsrec(temp3(nf(is)),temp3(nf(is)),nn)                               ZEXMOISR.328
                                                                                 ZEXMOISR.329
        DO  I=IST,IEN                                                            ZEXMOISR.330
          tem = RHO(I)*(XLS*XLS)*temp3(i)                                        ZEXMOISR.331
          AB(I)= tem   + (DUM11(I)*QVS(I))                                       ZEXMOISR.332
          ABW(I)=RHO(I)*DUM31(I,K)*DUM31(I,K)*temp3(i)+(DUM11(I)*QVS(I))         ZEXMOISR.333
          ABI(I)= tem  + DUM11(I)*QVSI(I)                                        ZEXMOISR.334
          PRE(I)=DUM21(I)*RHOr(I)                                                ZEXMOISR.335
          PRC(I)=PRE(I) * DUM11(I)                                               ZEXMOISR.336
       end do                                                                    ZEXMOISR.337
                                                                                 ZEXMOISR.338
        call vslog(temp1(nf(is)),prc(nf(is)),nn)                                 ZEXMOISR.339
        call vsrsqrt(temp2(nf(is)),pre(nf(is)),nn)                               ZEXMOISR.340
        call vsrec(ABw(nf(is)),ABw(nf(is)),nn)                                   ZEXMOISR.341
        call vsrec(ABI(nf(is)),ABI(nf(is)),nn)                                   ZEXMOISR.342
        call vsrec(AB(nf(is)),AB(nf(is)),nn)                                     ZEXMOISR.343
                                                                                 ZEXMOISR.344
        DO I=IST,IEN                                                             ZEXMOISR.345
           temp1(i) = 0.3333 * temp1(i)                                          ZEXMOISR.346
        end do                                                                   ZEXMOISR.347
                                                                                 ZEXMOISR.348
        call vsexp(temp1(nf(is)),temp1(nf(is)),nn)                               ZEXMOISR.349
                                                                                 ZEXMOISR.350
        DO I=IST,IEN                                                             ZEXMOISR.351
          dum11(i) = temp1(i) * temp2(i)                                         ZEXMOISR.352
          PRI(I)=0.                                                              ZEXMOISR.353
          NUFCI(I)=0.                                                            ZEXMOISR.354
          PRCI(I)=0.                                                             ZEXMOISR.355
          PRAI(I)=0.                                                             ZEXMOISR.356
          PSACW(I)=0.                                                            ZEXMOISR.357
          PRD(I)=0.                                                              ZEXMOISR.358
          PREI(I)=0.                                                             ZEXMOISR.359
          PRC(I)=0.                                                              ZEXMOISR.360
          PRA(I)=0.                                                              ZEXMOISR.361
          PRE(I)=0.                                                              ZEXMOISR.362
          PSMLT(I)=0.                                                            ZEXMOISR.363
          PMLTEV(I)=0.                                                           ZEXMOISR.364
       end do                                                                    ZEXMOISR.365
                                                                                 ZEXMOISR.366
        IF(MEYERS.EQ.1)THEN                                                      ZEXMOISR.367
           DO I=IST,IEN                                                          ZEXMOISR.368
              temp1(i)=(-0.639+0.1296*(100.* (QVQVSI(I,K)-1.)))                  ZEXMOISR.369
           end do                                                                ZEXMOISR.370
                                                                                 ZEXMOISR.371
        call vsexp(temp1(nf(is)),temp1(nf(is)),nn)                               ZEXMOISR.372
                                                                                 ZEXMOISR.373
           DO I=IST,IEN                                                          ZEXMOISR.374
              temp1(i)=(1000.*RHOr(I))*AMAX1(temp1(i),10.)                       ZEXMOISR.375
           end do                                                                ZEXMOISR.376
        ELSE                                                                     ZEXMOISR.377
           DO I=IST,IEN                                                          ZEXMOISR.378
              temp1(i)=ATO*(TO-TAOUT(I,K))                                       ZEXMOISR.379
           end do                                                                ZEXMOISR.380
                                                                                 ZEXMOISR.381
        call vsexp(temp1(nf(is)),temp1(nf(is)),nn)                               ZEXMOISR.382
                                                                                 ZEXMOISR.383
           DO I=IST,IEN                                                          ZEXMOISR.384
              temp1(i)=TNO * temp1(i)*RHOr(I)                                    ZEXMOISR.385
           end do                                                                ZEXMOISR.386
        ENDIF                                                                    ZEXMOISR.387
                                                                                 ZEXMOISR.388
        call vsdiv(temp2(nf(is)),ice(nf(is),k),                                  ZEXMOISR.389
     *             temp1(nf(is)),nn)                                             ZEXMOISR.390
        call vssqrt(temp2(nf(is)),temp2(nf(is)),nn)                              ZEXMOISR.391
                                                                                 ZEXMOISR.392
        DO I=IST,IEN                                                             ZEXMOISR.393
          xnc = temp1(i)                                                         ZEXMOISR.394
          DI=16.3*temp2(i)                                                       ZEXMOISR.395
                                                                                 ZEXMOISR.396
          ISAT(i)=0                                                              ZEXMOISR.397
                                                                                 ZEXMOISR.398
          IF(TAOUT(I,K).GT.TO) XNC=0.                                            ZEXMOISR.399
          SUPICE(i)=(QAOUT(I,K)-1./QVSI(I))*DTi                                  ZEXMOISR.400
                                                                                 ZEXMOISR.401
          IF(SUPICE(i).GT.0.)THEN                                                ZEXMOISR.402
            PRI(I)=AMAX1(0.,(XM01*XNC-ICE(I,K))*DTi)                             ZEXMOISR.403
            PRI(I)=AMIN1(PRI(I),SUPICE(i))                                       ZEXMOISR.404
            IF(PRI(I).GT.SUPICE(i)*.99) ISAT(i)=1                                ZEXMOISR.405
          ENDIF                                                                  ZEXMOISR.406
                                                                                 ZEXMOISR.407
          IF(ISAT(i).EQ.1)THEN                                                   ZEXMOISR.408
            PRD(I)=0.                                                            ZEXMOISR.409
          ELSE                                                                   ZEXMOISR.410
            PRD(I)=4.*DI*RHO(I)*SUPICE(i)*DT*XNC*QVSI(I)*ABI(I)                  ZEXMOISR.411
          ENDIF                                                                  ZEXMOISR.412
                                                                                 ZEXMOISR.413
          IF(PRD(I).LT.0.)THEN                                                   ZEXMOISR.414
            PRD(I)=AMAX1(PRD(I),0.5*SUPICE(i))                                   ZEXMOISR.415
          ELSE                                                                   ZEXMOISR.416
            PRD(I)=AMIN1(PRD(I),0.5*SUPICE(i))                                   ZEXMOISR.417
          ENDIF                                                                  ZEXMOISR.418
                                                                                 ZEXMOISR.419
          PRCI(I)=AMAX1(0.,(ICE(I,K)-XSMAX*XNC)*DTi)                             ZEXMOISR.420
                                                                                 ZEXMOISR.421
          IF(ABS(PRD(I)+PRI(I)).GT.ABS(.99*SUPICE(i))) ISAT(i)=1                 ZEXMOISR.422
       end do                                                                    ZEXMOISR.423
                                                                                 ZEXMOISR.424
                                                                                 ZEXMOISR.425
c -- cloud                                                                       ZEXMOISR.426
                                                                                 ZEXMOISR.427
        icount = is - 1                                                          ZEXMOISR.428
        DO I=IST,IEN                                                             ZEXMOISR.429
          IF(TAOUT(I,K).LT.TO)THEN                                               ZEXMOISR.430
            IF(CLOUD(I).GT.r1)THEN                                               ZEXMOISR.431
                                                                                 ZEXMOISR.432
              icount = icount + 1                                                ZEXMOISR.433
                                                                                 ZEXMOISR.434
              ind(nf(icount)) = i                                                ZEXMOISR.435
              temp1(nf(icount)) = BP*RHO(I)* CLOUD(I)**2/(CNP*DRAIN)             ZEXMOISR.436
              temp2(nf(icount)) = AP*(TO-TAOUT(I,K))                             ZEXMOISR.437
                                                                                 ZEXMOISR.438
            ENDIF                                                                ZEXMOISR.439
          ENDIF                                                                  ZEXMOISR.440
       end do                                                                    ZEXMOISR.441
                                                                                 ZEXMOISR.442
       if(icount.ge.is) then                                                     ZEXMOISR.443
           call vsexp(temp2(nf(is)),temp2(nf(is)),icount)                        ZEXMOISR.444
           do ii=is,icount                                                       ZEXMOISR.445
              i = nf(ii)                                                         ZEXMOISR.446
              NUFCI(nf(ind(nf(i)))) = (temp2(nf(i))-1.) *                        
     *                                      temp1(nf(i))                         ZEXMOISR.448
           end do                                                                ZEXMOISR.449
        end if                                                                   ZEXMOISR.450
                                                                                 ZEXMOISR.451
c ------- snow ---------                                                         ZEXMOISR.452
                                                                                 ZEXMOISR.453
        icount = is - 1                                                          ZEXMOISR.454
        DO I=IST,IEN                                                             ZEXMOISR.455
           IF(SNOW(I).GT.r1) then                                                ZEXMOISR.456
             if (ISAT(i).NE.1) THEN                                              ZEXMOISR.457
                                                                                 ZEXMOISR.458
              icount = icount + 1                                                ZEXMOISR.459
              tem = SONV(I,K)*DEPS1*(QAOUT(I,K)*QVSI(I)-1.)*ABI(I)               ZEXMOISR.460
              tmp1 = 0.65*SLOS(I,K)*SLOS(I,K)                                    ZEXMOISR.461
              tmp2 = DEPS2s*DEPS3*DUM11(I)                                       ZEXMOISR.462
                                                                                 ZEXMOISR.463
              ind(nf(icount)) = i                                                ZEXMOISR.464
              temp1(nf(icount)) = SLOS(I,K)                                      ZEXMOISR.465
              temp2(nf(icount)) = tem*tmp1                                       ZEXMOISR.466
              temp3(nf(icount)) = tem*tmp2                                       ZEXMOISR.467
                                                                                 ZEXMOISR.468
             end if                                                              ZEXMOISR.469
           end if                                                                ZEXMOISR.470
        end do                                                                   ZEXMOISR.471
                                                                                 ZEXMOISR.472
        if(icount.ge.is) then                                                    ZEXMOISR.473
        call vslog(temp1(nf(is)),temp1(nf(is)),nn)                               ZEXMOISR.474
                                                                                 ZEXMOISR.475
           do ii=is,icount                                                       ZEXMOISR.476
              i = nf(ii)                                                         ZEXMOISR.477
              temp1(nf(i)) = temp1(nf(i))*DEPS4                                  ZEXMOISR.478
           end do                                                                ZEXMOISR.479
                                                                                 ZEXMOISR.480
        call vsexp(temp1(nf(is)),temp1(nf(is)),nn)                               ZEXMOISR.481
                                                                                 ZEXMOISR.482
           do ii=is,icount                                                       ZEXMOISR.483
              i = nf(ii)                                                         ZEXMOISR.484
              prei(nf(ind(nf(i)))) =                                             ZEXMOISR.485
     *        temp2(nf(i))  + temp1(nf(i)) * temp3(nf(i))                       
           end do                                                                ZEXMOISR.487
        end if                                                                   ZEXMOISR.488
                                                                                 ZEXMOISR.489
        icount = is - 1                                                          ZEXMOISR.490
        DO I=IST,IEN                                                             ZEXMOISR.491
           IF(SNOW(I).GT.r1) then                                                ZEXMOISR.492
             if (QAOUT(I,K).lt.QVS(I)) THEN                                      ZEXMOISR.493
                                                                                 ZEXMOISR.494
              icount = icount + 1                                                ZEXMOISR.495
              tem = SONV(I,K)*DEPS1*(QAOUT(I,K)*QVS(I)-1.)*AB(I)                 ZEXMOISR.496
              tmp1 = 0.65*SLOS(I,K)*SLOS(I,K)                                    ZEXMOISR.497
              tmp2 = DEPS2s*DEPS3*DUM11(I)                                       ZEXMOISR.498
                                                                                 ZEXMOISR.499
              ind(nf(icount)) = i                                                ZEXMOISR.500
              temp1(nf(icount)) = SLOS(I,K)                                      ZEXMOISR.501
              temp2(nf(icount)) = tem*tmp1                                       ZEXMOISR.502
              temp3(nf(icount)) = tem*tmp2                                       ZEXMOISR.503
             endif                                                               ZEXMOISR.504
           ENDIF                                                                 ZEXMOISR.505
        end do                                                                   ZEXMOISR.506
                                                                                 ZEXMOISR.507
        if(icount.ge.is) then                                                    ZEXMOISR.508
        call vslog(temp1(nf(is)),temp1(nf(is)),nn)                               ZEXMOISR.509
                                                                                 ZEXMOISR.510
           do ii=is,icount                                                       ZEXMOISR.511
              i = nf(ii)                                                         ZEXMOISR.512
              temp1(nf(i)) = temp1(nf(i))*DEPS4                                  ZEXMOISR.513
           end do                                                                ZEXMOISR.514
                                                                                 ZEXMOISR.515
        call vsexp(temp1(nf(is)),temp1(nf(is)),nn)                               ZEXMOISR.516
                                                                                 ZEXMOISR.517
           do ii=is,icount                                                       ZEXMOISR.518
              i = nf(ii)                                                         ZEXMOISR.519
              PMLTEV(nf(ind(nf(i)))) =                                           ZEXMOISR.520
     *        temp2(nf(i))  + temp1(nf(i)) * temp3(nf(i))                        
           end do                                                                ZEXMOISR.522
        end if                                                                   ZEXMOISR.523
                                                                                 ZEXMOISR.524
        DO I=IST,IEN                                                             ZEXMOISR.525
          IF(PREI(I).LT.0.)THEN                                                  ZEXMOISR.526
            PREI(I)=AMAX1(PREI(I),0.5*SUPICE(i))                                 ZEXMOISR.527
          ELSE                                                                   ZEXMOISR.528
            PREI(I)=AMIN1(PREI(I),0.5*SUPICE(i))                                 ZEXMOISR.529
          ENDIF                                                                  ZEXMOISR.530
                                                                                 ZEXMOISR.531
          PRC(I)=AMAX1(0.,QCK1*(CLOUD(I)-QCTH))                                  ZEXMOISR.532
        end do                                                                   ZEXMOISR.533
                                                                                 ZEXMOISR.534
c -- rain                                                                        ZEXMOISR.535
                                                                                 ZEXMOISR.536
                                                                                 ZEXMOISR.537
        icount = is - 1                                                          ZEXMOISR.538
        DO I=IST,IEN                                                             ZEXMOISR.539
          IF(RAIN(I).gt.r1) THEN                                                 ZEXMOISR.540
                                                                                 ZEXMOISR.541
            icount = icount + 1                                                  ZEXMOISR.542
            tem = DEPR1*ABW(I)*(QAOUT(I,K)*QVS(I)-1.)                            ZEXMOISR.543
            ind(nf(icount)) = i                                                  ZEXMOISR.544
            temp1(nf(icount)) = SLOR(I,K)                                        ZEXMOISR.545
            temp2(nf(icount)) = ACRCR*CLOUD(I)                                   ZEXMOISR.546
            temp3(nf(icount)) = tem * 0.78*SLOR(I,K)*SLOR(I,K)                   ZEXMOISR.547
            temp4(nf(icount)) = tem * DEPR2s*DEPR3*DUM11(I)                      ZEXMOISR.548
          ENDIF                                                                  ZEXMOISR.549
        end do                                                                   ZEXMOISR.550
        if(icount.ge.is) then                                                    ZEXMOISR.551
        call vslog(temp1(nf(is)),temp1(nf(is)),icount)                           ZEXMOISR.552
                                                                                 ZEXMOISR.553
        do ii=is,icount                                                          ZEXMOISR.554
           i = nf(ii)                                                            ZEXMOISR.555
           temp5(nf(i)) = temp1(nf(i)) * BACRCR                                  ZEXMOISR.556
           temp6(nf(i)) = temp1(nf(i)) * DEPR4                                   ZEXMOISR.557
        end do                                                                   ZEXMOISR.558
                                                                                 ZEXMOISR.559
        call vsexp(temp5(nf(is)),temp5(nf(is)),icount)                           ZEXMOISR.560
        call vsexp(temp6(nf(is)),temp6(nf(is)),icount)                           ZEXMOISR.561
                                                                                 ZEXMOISR.562
        do ii=is,icount                                                          ZEXMOISR.563
           i = nf(ii)                                                            ZEXMOISR.564
           pra(nf(ind(nf(i)))) = temp2(nf(i)) *                                 
     *                                   temp5(nf(i))                            ZEXMOISR.566
           pre(nf(ind(nf(i)))) = temp3(nf(i)) +                                
     *                temp4(nf(i)) * temp6(nf(i))                                ZEXMOISR.568
        end do                                                                   ZEXMOISR.569
        end if                                                                   ZEXMOISR.570
                                                                                 ZEXMOISR.571
c -- snow                                                                        ZEXMOISR.572
                                                                                 ZEXMOISR.573
        icount = is - 1                                                          ZEXMOISR.574
        DO I=IST,IEN                                                             ZEXMOISR.575
          IF(SNOW(I).GT.r1) THEN                                                 ZEXMOISR.576
                                                                                 ZEXMOISR.577
            icount = icount + 1                                                  ZEXMOISR.578
            XLATF=1./(XLS-DUM31(I,K))                                            ZEXMOISR.579
            tem = SONV(I,K)*PSM1*SCR4(I)*rhor(i)*XLATF*(TO-TAOUT(I,K))           ZEXMOISR.580
            ind(nf(icount)) = i                                                  ZEXMOISR.581
            temp1(nf(icount)) = SLOs(I,K)                                        ZEXMOISR.582
            temp2(nf(icount)) = ACRIS*ICE(I,K)*SONV(I,K)                         ZEXMOISR.583
            temp7(nf(icount)) = ACRCS*CLOUD(I)*SONV(I,K)                         ZEXMOISR.584
            temp3(nf(icount)) = tem * 0.65*SLOS(I,K)*SLOS(I,K)                   ZEXMOISR.585
            temp4(nf(icount)) = tem * PSM3*PSM2sq * DUM11(I)                     ZEXMOISR.586
          ENDIF                                                                  ZEXMOISR.587
        end do                                                                   ZEXMOISR.588
                                                                                 ZEXMOISR.589
        if(icount.ge.is) then                                                    ZEXMOISR.590
                                                                                 ZEXMOISR.591
        call vslog(temp1(nf(is)),temp1(nf(is)),icount)                           ZEXMOISR.592
                                                                                 ZEXMOISR.593
        do ii=is,icount                                                          ZEXMOISR.594
           i = nf(ii)                                                            ZEXMOISR.595
           temp5(nf(i)) = temp1(nf(i)) * BACRIS                                  ZEXMOISR.596
           temp6(nf(i)) = temp1(nf(i)) * BACRCS                                  ZEXMOISR.597
           temp8(nf(i)) = temp1(nf(i)) * PSM4                                    ZEXMOISR.598
        end do                                                                   ZEXMOISR.599
                                                                                 ZEXMOISR.600
        call vsexp(temp5(nf(is)),temp5(nf(is)),icount)                           ZEXMOISR.601
        call vsexp(temp6(nf(is)),temp6(nf(is)),icount)                           ZEXMOISR.602
        call vsexp(temp8(nf(is)),temp8(nf(is)),icount)                           ZEXMOISR.603
                                                                                 ZEXMOISR.604
        do ii=is,icount                                                          ZEXMOISR.605
           i = nf(ii)                                                            ZEXMOISR.606
           prai(nf(ind(nf(i))))  = temp2(nf(i)) *                                
     *                                     temp5(nf(i))                          ZEXMOISR.608
           psacw(nf(ind(nf(i)))) = temp7(nf(i)) *                               
     *                                     temp6(nf(i))                          ZEXMOISR.610
                                                                                 ZEXMOISR.611
           PSMLT(nf(ind(nf(i)))) = temp3(nf(i)) +                              
     *                temp4(nf(i)) * temp8(nf(i))                                ZEXMOISR.613
        end do                                                                   ZEXMOISR.614
        end if                                                                   ZEXMOISR.615
                                                                                 ZEXMOISR.616
        DO I=IST,IEN                                                             ZEXMOISR.617
          PRE(I)=AMAX1(-RAIN(I)*DTi,PRE(I))                                      ZEXMOISR.618
          PREI(I)=AMAX1(-SNOW(I)*DTi,PREI(I))                                    ZEXMOISR.619
          IF(IEVAP(IN).LT.0)PRE(I)=AMAX1(PRE(I),0.)                              ZEXMOISR.620
          IF(IEVAP(IN).LT.0)PREI(I)=AMAX1(PREI(I),0.)                            ZEXMOISR.621
          IF(TAOUT(I,K).LE.TO)THEN                                               ZEXMOISR.622
            DUM11(I)=(PRC(I)+PRA(I)+PSACW(I)+NUFCI(I))*DT                        ZEXMOISR.623
            IF(DUM11(I).GT.CLOUD(I))THEN                                         ZEXMOISR.624
              dum1r = 1./DUM11(I)                                                ZEXMOISR.625
              NUFCI(I)=CLOUD(I)*NUFCI(I)*DUM1r                                   ZEXMOISR.626
              PRC(I)=CLOUD(I)*PRC(I)*DUM1r                                       ZEXMOISR.627
              PRA(I)=CLOUD(I)*PRA(I)*DUM1r                                       ZEXMOISR.628
              PSACW(I)=CLOUD(I)*PSACW(I)*DUM1r                                   ZEXMOISR.629
            ENDIF                                                                ZEXMOISR.630
            DUM11(I)=(PRCI(I)+PRAI(I)-PRI(I)-PRD(I)-NUFCI(I)                     ZEXMOISR.631
     +                 )*DT                                                      ZEXMOISR.632
            IF(DUM11(I).GT.ICE(I,K))THEN                                         ZEXMOISR.633
              dum1r = 1./DUM11(I)                                                ZEXMOISR.634
              PRCI(I)=ICE(I,K)*PRCI(I)*DUM1r                                     ZEXMOISR.635
              PRAI(I)=ICE(I,K)*PRAI(I)*DUM1r                                     ZEXMOISR.636
              PRI(I)=ICE(I,K)*PRI(I)*DUM1r                                       ZEXMOISR.637
              PRD(I)=ICE(I,K)*PRD(I)*DUM1r                                       ZEXMOISR.638
              NUFCI(I)=ICE(I,K)*NUFCI(I)*DUM1r                                   ZEXMOISR.639
            ENDIF                                                                ZEXMOISR.640
                                                                                 ZEXMOISR.641
            DUM11(I)=-(PRE(I)+PREI(I)+PRI(I)+PRD(I))*PSB(I,J)                    ZEXMOISR.642
            QV3DTEN(I,J,K)=QV3DTEN(I,J,K)+DUM11(I)                               ZEXMOISR.643
            QC3DTEN(I,J,K)=QC3DTEN(I,J,K)-(PRC(I)+PRA(I)+PSACW(I)+               ZEXMOISR.644
     +                     NUFCI(I))*PSB(I,J)                                    ZEXMOISR.645
            QR3DTEN(I,J,K)=QR3DTEN(I,J,K)+(PRC(I)+PRA(I)+PRE(I))*                ZEXMOISR.646
     +                     PSB(I,J)                                              ZEXMOISR.647
            QI3DTEN(I,J,K)=QI3DTEN(I,J,K)-(PRCI(I)+PRAI(I)-PRI(I)-               ZEXMOISR.648
     +                     PRD(I)-NUFCI(I))*PSB(I,J)                             ZEXMOISR.649
            QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)+(PREI(I)+PRCI(I)+                    ZEXMOISR.650
     +                      PRAI(I)+PSACW(I))*PSB(I,J)                           ZEXMOISR.651
            XLATF=XLS-DUM31(I,K)                                                 ZEXMOISR.652
            TEMP=-XLS*(PREI(I)+PRD(I)+PRI(I))*PSB(I,J)-DUM31(I,K)*               ZEXMOISR.653
     +           PRE(I)*PSB(I,J)-XLATF*(PSACW(I)+NUFCI(I))*                      ZEXMOISR.654
     +           PSB(I,J)                                                        ZEXMOISR.655
            IF(IEVAP(IN).EQ.0)TEMP=AMIN1(TEMP,0.)                                ZEXMOISR.656
            T3DTEN(I,J,K)=T3DTEN(I,J,K)-TEMP*PRDi(I,k)*(1-IFDRY)                 ZEXMOISR.657
          ELSE                                                                   ZEXMOISR.658
            DUM11(I)=(PRC(I)+PRA(I)+PSACW(I))*DT                                 ZEXMOISR.659
            IF(DUM11(I).GT.CLOUD(I))THEN                                         ZEXMOISR.660
              dum1r = 1./DUM11(I)                                                ZEXMOISR.661
              PRC(I)=CLOUD(I)*PRC(I)*DUM1r                                       ZEXMOISR.662
              PRA(I)=CLOUD(I)*PRA(I)*DUM1r                                       ZEXMOISR.663
              PSACW(I)=CLOUD(I)*PSACW(I)*DUM1r                                   ZEXMOISR.664
            ENDIF                                                                ZEXMOISR.665
                                                                                 ZEXMOISR.666
            DUM11(I)=(-PMLTEV(I)-PSMLT(I))*DT                                    ZEXMOISR.667
            IF(DUM11(I).GT.SNOW(I))THEN                                          ZEXMOISR.668
              dum1r = 1./DUM11(I)                                                ZEXMOISR.669
              PMLTEV(I)=SNOW(I)*PMLTEV(I)*DUM1r                                  ZEXMOISR.670
              PSMLT(I)=SNOW(I)*PSMLT(I)*DUM1r                                    ZEXMOISR.671
            ENDIF                                                                ZEXMOISR.672
            DUM11(I)=-(PRE(I)+PMLTEV(I))*PSB(I,J)                                ZEXMOISR.673
            QV3DTEN(I,J,K)=QV3DTEN(I,J,K)+DUM11(I)                               ZEXMOISR.674
            QC3DTEN(I,J,K)=QC3DTEN(I,J,K)-(PRC(I)+PRA(I)+PSACW(I))               ZEXMOISR.675
     +                     *PSB(I,J)                                             ZEXMOISR.676
            QR3DTEN(I,J,K)=QR3DTEN(I,J,K)+(PRC(I)+PRA(I)+PRE(I)-                 ZEXMOISR.677
     +                     PSMLT(I)+PSACW(I))*PSB(I,J)                           ZEXMOISR.678
            QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)+(PSMLT(I)+PMLTEV(I))*                ZEXMOISR.679
     +                      PSB(I,J)                                             ZEXMOISR.680
            XLATF=XLS-DUM31(I,K)                                                 ZEXMOISR.681
            TEMP=-XLATF*(PSMLT(I))*PSB(I,J)-DUM31(I,K)*(PRE(I)+                  ZEXMOISR.682
     +           PMLTEV(I))*PSB(I,J)                                             ZEXMOISR.683
            IF(IEVAP(IN).EQ.0)TEMP=AMIN1(TEMP,0.)                                ZEXMOISR.684
            T3DTEN(I,J,K)=T3DTEN(I,J,K)-TEMP*PRDi(I,k)*(1-IFDRY)                 ZEXMOISR.685
          ENDIF                                                                  ZEXMOISR.686
         end do                                                                  ZEXMOISR.687
                                                                                 ZEXMOISR.688
        DO I=IST,IEN                                                             ZEXMOISR.689
          DUM11(I)=QV3D(I,J,K)+DT*QV3DTEN(I,J,K)*RPSC(I,J)                       ZEXMOISR.690
          SCR4(I)=AMAX1(1.E-12,DUM11(I))                                         ZEXMOISR.691
          DUM21(I)=QC3D(I,J,K)+DT*QC3DTEN(I,J,K)*RPSC(I,J)                       ZEXMOISR.692
          SCR3(I,K)=AMAX1(0.,DUM21(I))                                           ZEXMOISR.693
          PRE(I)=pres(i,k)                                                       ZEXMOISR.694
          SCR7(I,K)=T3D(I,J,K)+DT*T3DTEN(I,J,K)*RPSC(I,J)                        ZEXMOISR.695
        end do                                                                   ZEXMOISR.696
                                                                                 ZEXMOISR.697
        DO I=IST,IEN                                                             ZEXMOISR.698
           temp1(i) = SCR7(I,K)-SVP3                                             ZEXMOISR.699
        end do                                                                   ZEXMOISR.700
        call vsrec(temp1(nf(is)),temp1(nf(is)),nn)                               ZEXMOISR.701
        DO I=IST,IEN                                                             ZEXMOISR.702
          temp1(i) = SVP2*(SCR7(I,K)-SVPT0) * temp1(i)                           ZEXMOISR.703
        end do                                                                   ZEXMOISR.704
        call vsexp(temp1(nf(is)),temp1(nf(is)),nn)                               ZEXMOISR.705
c        DO I=IST,IEN                                                            ZEXMOISR.706
c          DUM11(I)=1.E3*SVP1*                                                   ZEXMOISR.707
c     +               EXP(SVP2*(SCR7(I,K)-SVPT0)/(SCR7(I,K)-SVP3))               ZEXMOISR.708
c        end do                                                                  ZEXMOISR.709
                                                                                 ZEXMOISR.710
        DO I=IST,IEN                                                             ZEXMOISR.711
          dum11(i) = 1.E3*SVP1*temp1(i)                                          ZEXMOISR.712
          DUM21(I)=DUM31(I,K)*DUM31(I,K)*PRDi(I,k)/(RV)                          ZEXMOISR.713
          temp2(i) = PRE(I)-DUM11(I)                                             ZEXMOISR.714
        end do                                                                   ZEXMOISR.715
        call vsrec(temp2(nf(is)),temp2(nf(is)),nn)                               ZEXMOISR.716
        DO I=IST,IEN                                                             ZEXMOISR.717
          PRC(I)=.622*DUM11(I) * temp2(i)                                        ZEXMOISR.718
          temp3(i) = SCR7(I,K)*SCR7(I,K) + DUM21(I)*PRC(I)                       ZEXMOISR.719
        end do                                                                   ZEXMOISR.720
        call vsrec(temp3(nf(is)),temp3(nf(is)),nn)                               ZEXMOISR.721
                                                                                 ZEXMOISR.722
        DO I=IST,IEN                                                             ZEXMOISR.723
                                                                                 ZEXMOISR.724
          SCR8=(SCR4(I)-PRC(I))*SCR7(I,K)*SCR7(I,K) * temp3(i)                   ZEXMOISR.725
                                                                                 ZEXMOISR.726
          r2 = r1 * psb(i,j)                                                     ZEXMOISR.727
          DUM11(I)=SCR3(I,K)+SCR8                                                ZEXMOISR.728
          IF(DUM11(I).GE.0)THEN                                                  ZEXMOISR.729
            SCR6=SCR8*DTi                                                        ZEXMOISR.730
          ELSE                                                                   ZEXMOISR.731
            SCR6=-SCR3(I,K)*DTi                                                  ZEXMOISR.732
          ENDIF                                                                  ZEXMOISR.733
          QV3DTEN(I,J,K)=QV3DTEN(I,J,K)-PSB(I,J)*SCR6                            ZEXMOISR.734
          QC3DTEN(I,J,K)=QC3DTEN(I,J,K)+PSB(I,J)*SCR6                            ZEXMOISR.735
          DUM21(I)=DUM31(I,K)*PRDi(I,k)                                          ZEXMOISR.736
          IF(IEVAP(IN).EQ.0) SCR6=AMAX1(SCR6,0.)                                 ZEXMOISR.737
          T3DTEN(I,J,K)=T3DTEN(I,J,K)+PSB(I,J)*SCR6*DUM21(I)*(1-                 ZEXMOISR.738
     +                  IFDRY)                                                   ZEXMOISR.739
          SCR4R(I,K)=                                                            ZEXMOISR.740
     +               AMAX1(r2,QR3D(I,J,K)*PSB(I,J)+DT*QR3DTEN(I,J,K)             ZEXMOISR.741
     +               )                                                           ZEXMOISR.742
          SCR4S(I,K)=                                                            ZEXMOISR.743
     +               AMAX1(r2,QNI3D(I,J,K)*PSB(I,J)+DT*                          ZEXMOISR.744
     +               QNI3DTEN(I,J,K))                                            ZEXMOISR.745
          SCR4I(I,K)=                                                            ZEXMOISR.746
     +               AMAX1(r2,QI3D(I,J,K)*PSB(I,J)+DT*QI3DTEN(I,J,K)             ZEXMOISR.747
     +               )                                                           ZEXMOISR.748
          SCR7(I,K)=T3D(I,J,K)*PSB(I,J)+DT*T3DTEN(I,J,K)                         ZEXMOISR.749
          SCR60(I,K)=SCR7(I,K)*RPSC(I,J)                                         ZEXMOISR.750
       end do                                                                    ZEXMOISR.751
  20  continue                                                                   ZEXMOISR.752
                                                                                 ZEXMOISR.753
#ifdef VECTOR                                                                    ZEXMOISR.754
#  include <vec_exmoisr_80.incl>                                                 ZEXMOISR.755
#else                                                                            ZEXMOISR.756
      DO I=IST,IEN                                                               ZEXMOISR.757
         nstep(i) = 1                                                            ZEXMOISR.758
      end do                                                                     ZEXMOISR.759
                                                                                 ZEXMOISR.760
      DO K=1,KL                                                                  ZEXMOISR.761
        DO I=IST,IEN                                                             ZEXMOISR.762
           RHO2(i,k)=(pres(i,k)*0.001)/(R*SCR7(I,K))                             ZEXMOISR.763
           temp1(i) = (1000./TOPR)*(RHO2(i,k)*SCR4R(I,K))                        ZEXMOISR.764
           temp2(i) = (1000./TOPS)*(RHO2(i,k)*SCR4S(I,K))/SONV(I,K)              ZEXMOISR.765
        end do                                                                   ZEXMOISR.766
        call vslog(temp1(nf(is)),temp1(nf(is)),nn)                               ZEXMOISR.767
        call vslog(temp2(nf(is)),temp2(nf(is)),nn)                               ZEXMOISR.768
        DO I=IST,IEN                                                             ZEXMOISR.769
           temp1(i) = temp1(i) * 0.25*br                                         ZEXMOISR.770
           temp2(i) = temp2(i) * 0.25*bs                                         ZEXMOISR.771
        end do                                                                   ZEXMOISR.772
        call vsexp(temp1(nf(is)),temp1(nf(is)),nn)                               ZEXMOISR.773
        call vsexp(temp2(nf(is)),temp2(nf(is)),nn)                               ZEXMOISR.774
                                                                                 ZEXMOISR.775
        icount = is - 1                                                          ZEXMOISR.776
        DO I=IST,IEN                                                             ZEXMOISR.777
          IF(ICE(I,K).GT.r1)THEN                                                 ZEXMOISR.778
               icount = icount+1                                                 ZEXMOISR.779
               ind(i) = icount                                                   ZEXMOISR.780
               RHO3=pres(i,k)/(R*SCR7(I,K)*RPSC(I,J))                            ZEXMOISR.781
               temp3(nf(icount)) = RHO3*ICE(I,K)                                 ZEXMOISR.782
            end if                                                               ZEXMOISR.783
         end do                                                                  ZEXMOISR.784
                                                                                 ZEXMOISR.785
                                                                                 ZEXMOISR.786
         call vslog(temp3(nf(is)),temp3(nf(is)),icount)                          ZEXMOISR.787
        do ii=is,icount                                                          ZEXMOISR.788
           i = nf(ii)                                                            ZEXMOISR.789
           temp3(nf(i)) = temp3(nf(i)) * 0.16                                    ZEXMOISR.790
        end do                                                                   ZEXMOISR.791
         call vsexp(temp3(nf(is)),temp3(nf(is)),icount)                          ZEXMOISR.792
                                                                                 ZEXMOISR.793
        DO I=IST,IEN                                                             ZEXMOISR.794
          IF(ICE(I,K).GT.r1)THEN                                                 ZEXMOISR.795
            Vsed = 3.29*temp3(nf(ind(i)))                                        ZEXMOISR.796
          ELSE                                                                   ZEXMOISR.797
            VSED=0.                                                              ZEXMOISR.798
          ENDIF                                                                  ZEXMOISR.799
                                                                                 ZEXMOISR.800
          VT2R=FRAIN*temp1(i)                                                    ZEXMOISR.801
          VT2S=FSNOW*temp2(i)                                                    ZEXMOISR.802
          RHOFAC(i,K)=RHO0(I,J,K)/(PSB(I,J)*1000.*RHO2(i,k))                     ZEXMOISR.803
          FR(i,K)=G*RHO2(i,k)*VT2R                                               ZEXMOISR.804
          FS(i,K)=G*RHO2(i,k)*VT2S                                               ZEXMOISR.805
          FI(i,K)=G*RHO2(i,k)*VSED                                               ZEXMOISR.806
          RGVM=AMAX1(FR(i,K),FS(i,K),FI(i,K))                                    ZEXMOISR.807
          nstep1 = RGVM*DT*DSIGMAi(K)+1.                                         ZEXMOISR.808
          NSTEP(i)=MAX0(nstep1,NSTEP(i))                                         ZEXMOISR.809
        end do                                                                   ZEXMOISR.810
      end do                                                                     ZEXMOISR.811
                                                                                 ZEXMOISR.812
      DO I=IST,IEN                                                               ZEXMOISR.813
          IF(Nstep(i).GT.1000)THEN                                               ZEXMOISR.814
            PRINT *, 'STOPPING IN EXMOISR, NEST = ',IN,' I = ',I,                ZEXMOISR.815
     +               ' J = ',J                                                   ZEXMOISR.816
            STOP 'IN SUB. EXMOISR, NSTEP TOO LARGE, PROBABLY NAN'                ZEXMOISR.817
          END IF                                                                 ZEXMOISR.818
                                                                                 ZEXMOISR.819
        rnstep = 1./nstep(i)                                                     ZEXMOISR.820
        DO 100 N=1,NSTEP(i)                                                      ZEXMOISR.821
          DO 110 K=1,KL                                                          ZEXMOISR.822
            FALOUTR(K)=FR(i,K)*SCR4R(I,K)                                        ZEXMOISR.823
            FALOUTS(K)=FS(i,K)*SCR4S(I,K)                                        ZEXMOISR.824
            FALOUTI(K)=FI(i,K)*SCR4I(I,K)                                        ZEXMOISR.825
  110     CONTINUE                                                               ZEXMOISR.826
          K=1                                                                    ZEXMOISR.827
                                                                                 ZEXMOISR.828
          FALTNDR=RHOFAC(i,K)*FALOUTR(K)*DSIGMAi(K)                              ZEXMOISR.829
          FALTNDS=RHOFAC(i,K)*FALOUTS(K)*DSIGMAi(K)                              ZEXMOISR.830
          FALTNDI=RHOFAC(i,K)*FALOUTI(K)*DSIGMAi(K)                              ZEXMOISR.831
          QR3DTEN(I,J,K)=QR3DTEN(I,J,K)-FALTNDR*rNSTEP                           ZEXMOISR.832
          QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)-FALTNDS*rNSTEP                         ZEXMOISR.833
          QI3DTEN(I,J,K)=QI3DTEN(I,J,K)-FALTNDI*rNSTEP                           ZEXMOISR.834
          SCR4R(I,K)=SCR4R(I,K)-FALTNDR*DT*rNSTEP                                ZEXMOISR.835
          SCR4S(I,K)=SCR4S(I,K)-FALTNDS*DT*rNSTEP                                ZEXMOISR.836
          SCR4I(I,K)=SCR4I(I,K)-FALTNDI*DT*rNSTEP                                ZEXMOISR.837
          DO 120 K=2,KL                                                          ZEXMOISR.838
            FALTNDR=RHOFAC(i,K)*(FALOUTR(K)-FALOUTR(K-1))*DSIGMAi(K)             ZEXMOISR.839
            FALTNDS=RHOFAC(i,K)*(FALOUTS(K)-FALOUTS(K-1))*DSIGMAi(K)             ZEXMOISR.840
            FALTNDI=RHOFAC(i,K)*(FALOUTI(K)-FALOUTI(K-1))*DSIGMAi(K)             ZEXMOISR.841
            QR3DTEN(I,J,K)=QR3DTEN(I,J,K)-FALTNDR*rNSTEP                         ZEXMOISR.842
            QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)-FALTNDS*rNSTEP                       ZEXMOISR.843
            QI3DTEN(I,J,K)=QI3DTEN(I,J,K)-FALTNDI*rNSTEP                         ZEXMOISR.844
            SCR4R(I,K)=SCR4R(I,K)-FALTNDR*DT*rNSTEP                              ZEXMOISR.845
            SCR4S(I,K)=SCR4S(I,K)-FALTNDS*DT*rNSTEP                              ZEXMOISR.846
            SCR4I(I,K)=SCR4I(I,K)-FALTNDI*DT*rNSTEP                              ZEXMOISR.847
            FR(i,K)=AMAX1(FR(i,K),FR(i,K-1)*DSIGMAi(K-1)*DSIGMA(K))              ZEXMOISR.848
            FS(i,K)=AMAX1(FS(i,K),FS(i,K-1)*DSIGMAi(K-1)*DSIGMA(K))              ZEXMOISR.849
            FI(i,K)=AMAX1(FI(i,K),FI(i,K-1)*DSIGMAi(K-1)*DSIGMA(K))              ZEXMOISR.850
  120     CONTINUE                                                               ZEXMOISR.851
                                                                                 ZEXMOISR.852
      RAINNC(I,J)=RAINNC(I,J)+(FALOUTR(KL)+FALOUTS(KL)+FALOUTI(KL))*             ZEXMOISR.853
     *                 dtf*rNSTEP                                                ZEXMOISR.854
      SNOWNCT(I,J)=(FALOUTS(KL)+FALOUTI(KL))* dtf*rNSTEP                         ZEXMOISR.855
  100   CONTINUE                                                                 ZEXMOISR.856
      end do                                                                     ZEXMOISR.857
                                                                                 ZEXMOISR.858
      DO  K=2,KL                                                                 ZEXMOISR.859
        DO  I=IST,IEN                                                            ZEXMOISR.860
         IF(SCR60(I,K).GT.TO.AND.(QI3DTEN(I,J,K).GT.0.OR.QI3D(I,J,K).GT.         ZEXMOISR.861
     +      0.))THEN                                                             ZEXMOISR.862
            XLATF=XLS-DUM31(I,K)                                                 ZEXMOISR.863
            DUM11I=QI3D(I,J,K)*PSB(I,J)+DT*QI3DTEN(I,J,K)                        ZEXMOISR.864
            QI3DTEN(I,J,K)=-QI3D(I,J,K)*PSB(I,J)*DTi                             ZEXMOISR.865
            QC3DTEN(I,J,K)=QC3DTEN(I,J,K)+DUM11I*DTi                             ZEXMOISR.866
            T3DTEN(I,J,K)=T3DTEN(I,J,K)-XLATF*PRDi(I,k)*DUM11I*DTi               ZEXMOISR.867
     +                    *(1- IFDRY)                                            ZEXMOISR.868
          ENDIF                                                                  ZEXMOISR.869
          IF(SCR60(I,K).LT.HGFR.AND.(QC3DTEN(I,J,K).GT.0.OR.QC3D(I,J,K)          ZEXMOISR.870
     +      .GT.0.))THEN                                                         ZEXMOISR.871
            XLATF=XLS-DUM31(I,K)                                                 ZEXMOISR.872
            DUM11I=QC3D(I,J,K)*PSB(I,J)+DT*QC3DTEN(I,J,K)                        ZEXMOISR.873
            QC3DTEN(I,J,K)=-QC3D(I,J,K)*PSB(I,J)*DTi                             ZEXMOISR.874
            QI3DTEN(I,J,K)=QI3DTEN(I,J,K)+DUM11I*DTi                             ZEXMOISR.875
            T3DTEN(I,J,K)=T3DTEN(I,J,K)+XLATF*PRDi(I,k)*DUM11I*DTi               ZEXMOISR.876
     +                    *(1- IFDRY)                                            ZEXMOISR.877
          ENDIF                                                                  ZEXMOISR.878
        end do                                                                   ZEXMOISR.879
      end do                                                                     ZEXMOISR.880
#endif                                                                           ZEXMOISR.881
      RETURN                                                                     ZEXMOISR.882
      END                                                                        ZEXMOISR.883
