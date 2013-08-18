      SUBROUTINE SURFCE(J,ZA,QX,THX,TGB,THGB,TGDSA,CHS,CHFF,CPM,PREC,            SURFCE.1
     &HFX,QFX,QGH,GSW,GLW,TX,UX,VX,ALB,PP3D,PSB,RHOX,TMN,XLAND,qcx               SURFCE.2
     &,inest,u10,v10,th2,q2,rainc,rainnc,SNOWC,chs2,cklq,ZNT,EMISS)              23DEC04.3788
      IMPLICIT NONE                                                              SURFCE.4
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                             SURFCE.5
C                .      .    .                                                   SURFCE.6
C SUBPROGRAM:    SURFCE      CALCULATE SURFACE CONDITIONS                        SURFCE.7
C   PRGRMMR: F. CHEN         DATE: 97-12-06                                      SURFCE.8
C                                                                                SURFCE.9
C ABSTRACT:                                                                      SURFCE.10
C   THIS ROUTINE IS THE DRIVER FOR COMPUTATION OF GROUND CONDITIONS              SURFCE.11
C   BY USING A LAND SURFACE MODEL (LSM).                                         SURFCE.12
C                                                                                SURFCE.13
C PROGRAM HISTORY LOG:                                                           SURFCE.14
C   97-12-06  CHEN - ORIGINATOR                                                  SURFCE.15
C   00-05-09  USE THGB FOR THETA TGB FOR T (DUDHIA)                              SURFCE.16
C                                                                                SURFCE.17
C REFERENCES:                                                                    SURFCE.18
C   PAN AND MAHRT (1987) BOUN. LAYER METEOR.                                     SURFCE.19
C   CHEN ET AL. (1996)  J. GEOPHYS. RES.                                         SURFCE.20
C   CHEN ET AL. (1997)  BOUN. LAYER METEOR.                                      SURFCE.21
C   CHEN AND DUDHIA (2001) MON. WEA. REV.                                        SURFCE.22
C                                                                                SURFCE.23
C USAGE:    CALL SURFCE FROM SUBROUTINE SOLV3                                    SURFCE.24
C                                                                                SURFCE.25
C   SUBPROGRAMS CALLED:                                                          SURFCE.26
C     SFLX                                                                       SURFCE.27
C                                                                                SURFCE.28
C$$$                                                                             SURFCE.29
C                                                                                SURFCE.30
C-----------------------------------------------------------------------         SURFCE.31
C***  INCLUDE GLOBAL PARAMETERS.                                                 SURFCE.32
C-----------------------------------------------------------------------         SURFCE.33
#     include <parame.incl>                                                      SURFCE.34
#     include <rpstar.incl>                                                      SURFCE.35
C     SET LOCAL PARAMETERS.                                                      SURFCE.36
C-----------------------------------------------------------------------         SURFCE.37
                          R E A L                                                SURFCE.38
     & PQ0,TRESH,A2,A3,A4,T0,T1,ROW,ELWV,ELIV,ELIW,A23M4,RLIVWV,ROWLIW           SURFCE.39
     &,ROWLIV                                                                    SURFCE.40
                          I N T E G E R                                          SURFCE.41
     & NSOIL,NROOT                                                               SURFCE.42
                          P A R A M E T E R                                      SURFCE.43
C     & (PQ0=379.90516E0,TRESH=.95E0                                             SURFCE.44
C     &, A2=17.2693882E0,A3=273.16E0,A4=35.86E0                                  SURFCE.45
     & (PQ0=1000.0*0.6112*0.622, TRESH=.95E0                                     SURFCE.46
     &, A2=17.67,A3=273.15,A4=29.65                                              SURFCE.47
     &, T0=273.16E0,T1=274.16E0                                                  SURFCE.48
     &, ROW=1.E3                                                                 SURFCE.49
     &, ELWV=2.50E6,ELIV=2.8337E6,ELIW=.3337E6)                                  SURFCE.50
C                                                                                SURFCE.51
                          P A R A M E T E R                                      SURFCE.52
     & (A23M4=A2*(A3-A4)                                                         SURFCE.53
     &, RLIVWV=ELIV/ELWV,ROWLIW=ROW*ELIW,ROWLIV=ROW*ELIV)                        SURFCE.54
C                                                                                SURFCE.55
       PARAMETER(NSOIL=MLX)                                                      SURFCE.56
C-----------------------------------------------------------------------         SURFCE.57
                          I N T E G E R                                          SURFCE.58
     & J,INEST                                                                   SURFCE.59
C-----------------------------------------------------------------------         SURFCE.60
                          R E A L                                                SURFCE.61
     & SMCK  (NSOIL),STCK  (NSOIL),SWCK(NSOIL), ET(NSOIL)                        SURFCE.62
C-----------------------------------------------------------------------         SURFCE.63
                          R E A L                                                SURFCE.64
     & ZA(MIX,MKX),QX(MIX,MKX),THX(MIX,MKX),CHS(MIX),CHFF(MIX),CPM(MIX)          SURFCE.65
     &,TGB(MIX,MJX),THGB(MIX),TGDSA(MIX),PREC(MIX),QGH(MIX),CHS2(MIX)            SURFCE.66
     &,UX(MIX,MKX),VX(MIX,MKX),CKLQ(MIX,MJX),ZNT(MIX,MJX)                        SURFCE.67
     &,HFX(MIX,MJX),QFX(MIX,MJX),GSW(MIX,MJX),GLW(MIX,MJX),TX(MIX,MKX)           SURFCE.68
     &,ALB(MIX,MJX),PP3D(MIX,MJX,MKX),PSB(MIX,MJX),RHOX(MIX)                     SURFCE.69
     &,TMN(MIX,MJX),XLAND(MIX,MJX),qcx(mix,mkx),SNOWC(MIX,MJX)                   SURFCE.70
     &,u10(mix),v10(mix),th2(mix),q2(mix),rainc(mix,mjx),rainnc(mix,mjx)         SURFCE.71
     &,EMISS(MIX,MJX)                                                            23DEC04.3789
C----------------------------------------------------------------------          SURFCE.72
C***  DECLARATIONS FOR IMPLICIT NONE                                             SURFCE.73
                          R E A L                                                SURFCE.74
     & DTHALF,FDTLIW,Q2SAT,DTK,Z,FK,SOLDN,SFCTMP,SFCTH2,SFCPRS,PRCP              SURFCE.75
     &,RAINK,Q2K,DQSDTK,TBOTK,CHK,VGFRCK,T1K,CMCK,U10K                           SURFCE.76
     &,V10K,TH2K,Q2M,SNODPK,PLFLX,ELFLX,HFLX,GFLX,RNOF1K,RNOF2K                  SURFCE.77
     &,Q1K,SMELTK,SOILQW,SOILQM,T2K,PRESK,LWDN,SNOWHK,EMISSI,DUMMY               23DEC04.3790
     &,EC,EDIR,ETT,ESNOW,DRIP,DEW,BET,FLX1,FLX2,FLX3,SNOWCK                      SURFCE.79
     &,ALBEDOK,RNOF3K,RC,PC,RSMIN,XLAIK,RCS,RCT,RCQ,RCSOIL,SNOALB                SURFCE.80
     &,ALBBRD,SHDMIN,SMCWLT,SMCDRY,SMCREF,SMCMAX,ELFLXKIN,Z0,Z0BRD               SURFCE.81
                          I N T E G E R                                          SURFCE.82
     & I,K,NS,ICE,IVGTPK,ISLTPK,NOOUT,SLOPETYP                                   SURFCE.83
                          L O G I C A L                                          SURFCE.84
     & SATFLG,LCH,OFFLINE                                                        SURFCE.85
                                                                                 SURFCE.86
C***  INCLUDE COMMON BLOCKS.                                                     SURFCE.87
C-----------------------------------------------------------------------         SURFCE.88
#     include <soilp.incl>                                                       SURFCE.89
#     include <soilcnst.incl>                                                    SURFCE.90
#     include <various.incl>                                                     SURFCE.91
#     include <param3.incl>                                                      SURFCE.92
#     include <landuse.incl>                                                     SURFCE.93
#     include <paramveg_USGS.incl>                                               SURFCE.94
#     include <paramsoil_STAS.incl>                                              SURFCE.95
C***********************************************************************         SURFCE.96
C                         START SURFCE HERE                                      SURFCE.97
C***                                                                             SURFCE.98
C***  SET CONSTANTS CALCULATED HERE FOR CLARITY.                                 SURFCE.99
C***                                                                             SURFCE.100
      DTHALF=DTMIN*60.0                                                          SURFCE.101
      FDTLIW=DTHALF/ROWLIW                                                       SURFCE.102
C      FDTLIV=DTHALF/ROWLIV                                                      SURFCE.103
C      FDTW=DTHALF/2.5E9                                                         SURFCE.104
C***                                                                             SURFCE.105
C***  SET LSM CONSTANTS AND TIME INDEPENDENT VARIABLES                           SURFCE.106
C***  INITIALIZE LSM HISTORICAL VARIABLES                                        SURFCE.107
C***                                                                             SURFCE.108
C-----------------------------------------------------------------------         SURFCE.109
      IF(KTAU.EQ.0)THEN                                                          SURFCE.110
        DO 50 I=1,ILX                                                            SURFCE.111
C*** SET ZERO-VALUE FOR SOME OUTPUT DIAGNOSTIC ARRAYS                            SURFCE.112
          IF((XLAND(I,J)-1.5).GE.0.)THEN                                         SURFCE.113
C check sea-ice point                                                            SURFCE.114
            IF(XICE(I,J).GE..5)PRINT*,' sea-ice at water point, I=',I,           23DEC04.3791
     +        'J=',J,' nest =',INEST                                             SURFCE.116
C***   Open Water Case                                                           SURFCE.117
            SMSTAV(I,J)=1.0                                                      SURFCE.118
            SMSTOT(I,J)=1.0                                                      SURFCE.119
C           SUBSHX(I,J)=0.0                                                      SURFCE.120
            DO NS=1,NSOIL                                                        SURFCE.121
              SMCA(I,J,NS)=1.0                                                   SURFCE.122
              SMCB(I,J,NS)=1.0                                                   SURFCE.123
              SWCA(I,J,NS)=1.0                                                   SURFCE.124
              SWCB(I,J,NS)=1.0                                                   SURFCE.125
              STCA(I,J,NS)=273.16                                                SURFCE.126
              STCB(I,J,NS)=273.16                                                SURFCE.127
            ENDDO                                                                SURFCE.128
          ELSE                                                                   SURFCE.129
            IF(XICE(I,J).GE.0.5)THEN                                             23DEC04.3792
C***        SEA-ICE CASE                                                         SURFCE.131
              SMSTAV(I,J)=1.0                                                    SURFCE.132
              SMSTOT(I,J)=1.0                                                    SURFCE.133
              DO NS=1,NSOIL                                                      SURFCE.134
                SMCA(I,J,NS)=1.0                                                 SURFCE.135
                SMCB(I,J,NS)=1.0                                                 SURFCE.136
                SWCA(I,J,NS)=0.0                                                 SURFCE.137
                SWCB(I,J,NS)=0.0                                                 SURFCE.138
              ENDDO                                                              SURFCE.139
            ELSE                                                                 SURFCE.140
C*** Land point                                                                  SURFCE.141
CFEI                                                                             SURFCE.142
!KWM            SNOWHB(i,j)=SNOB(I,J)*5.0!KWM *0.001                             SURFCE.143
!KWM            SNOWHA(i,j)=SNOB(I,J)*5.0!KWM *0.001                             SURFCE.144
!KWM                                                                             SURFCE.145
!KWM              DO NS=1,NSOIL                                                  SURFCE.146
!KWM                SWCB(I,J,NS)=SMCB(I,J,NS)                                    SURFCE.147
!KWM                SWCA(I,J,NS)=SMCB(I,J,NS)                                    SURFCE.148
!KWM              ENDDO                                                          SURFCE.149
CFEI                                                                             SURFCE.150
C**** for land-ice points                                                        SURFCE.151
              IF(IVGTYP(I,J).EQ.24) THEN                                         SURFCE.152
                  DO NS=1,NSOIL                                                  SURFCE.153
                    SWCB(I,J,NS)=0.0                                             SURFCE.154
                    SWCA(I,J,NS)=0.0                                             SURFCE.155
                  ENDDO                                                          SURFCE.156
              ENDIF                                                              SURFCE.157
                                                                                 SURFCE.158
            ENDIF                                                                SURFCE.159
          ENDIF                                                                  SURFCE.160
C                                                                                SURFCE.161
   50   CONTINUE                                                                 SURFCE.162
      ENDIF                                                                      SURFCE.163
C-----------------------------------------------------------------------         SURFCE.164
      DO 100 I=1,ILX                                                             SURFCE.165
C***                                                                             SURFCE.166
C***  LOADING AND UNLOADING MM5/LSM LAND SOIL VARIABLES                          SURFCE.167
C***                                                                             SURFCE.168
        IF((XLAND(I,J)-1.5).GE.0.)THEN                                           SURFCE.169
C*** Water                                                                       SURFCE.170
CCC     SFCPRS=(A(KL)*PSB(I,J)+PTOP+PP3D(I,J,KL)*0.001)*1.E3                     SURFCE.171
CCC     Q2SAT=PQ0/SFCPRS*EXP(A2*(TGDSA(I)-A3)/(TGDSA(I)-A4))                     SURFCE.172
          Q2SAT=QGH(I)                                                           SURFCE.173
          HFX(I,J)=CHFF(I)*(THGB(I)-THX(I,KL))                                   SURFCE.174
          QFX(I,J)=RHOX(I)*CHS(I)*(Q2SAT-QX(I,KL))                               SURFCE.175
          SFCEVP(I,J)=SFCEVP(I,J)+QFX(I,J)*DTHALF                                SURFCE.176
        ELSE                                                                     SURFCE.177
C*** LAND OR SEA-ICE                                                             SURFCE.178
          IF(XICE(I,J).GE.0.5) THEN                                              23DEC04.3793
              ICE = 1                                                            23DEC04.3794
          ELSE                                                                   23DEC04.3795
              ICE = 0                                                            23DEC04.3796
          ENDIF                                                                  23DEC04.3797
          DTK=DT                                                                 SURFCE.180
          Z=ZA(I,KL)                                                             SURFCE.181
          SOLDN=GSW(I,J)/(1.-ALB(I,J))                                           SURFCE.182
          EMISSI = EMISS(I,J)                                                    23DEC04.3798
          LWDN=GLW(I,J)*EMISSI
          SFCTMP=TX(I,KL)                                                        SURFCE.184
          SFCTH2=SFCTMP+(0.0097545*Z)                                            SURFCE.185
          SFCPRS=(PSB(I,J)+PTOP+PP3D(I,J,KL)*0.001)*1.E3                         SURFCE.186
          PRCP=PREC(I)                                                           SURFCE.187
C          RAINK=(RAINC(I,J)+RAINNC(I,J))*10.                                    SURFCE.188
          RAINK=PRCP*DTHALF                                                      SURFCE.189
          Q2K=QX(I,KL)                                                           SURFCE.190
          Q2SAT=PQ0/SFCPRS*EXP(A2*(SFCTMP-A3)/(SFCTMP-A4))                       SURFCE.191
          DQSDTK=Q2SAT*A23M4/(SFCTMP-A4)**2                                      SURFCE.192
C***  CHECK FOR SATURATION AT THE LOWEST MODEL LEVEL                             SURFCE.193
          IF((Q2K.GE.Q2SAT*TRESH))THEN                                           SURFCE.194
            SATFLG=.true.                                                        SURFCE.195
            CKLQ(I,J)=0.                                                         SURFCE.196
          ELSE                                                                   SURFCE.197
            SATFLG=.false.                                                       SURFCE.198
            CKLQ(I,J)=1.                                                         SURFCE.199
          ENDIF                                                                  SURFCE.200
          IF(ICE.EQ.0)THEN                                                       SURFCE.201
            TBOTK=TMN(I,J)                                                       SURFCE.202
          ELSE                                                                   SURFCE.203
            TBOTK=271.16                                                         SURFCE.204
          ENDIF                                                                  SURFCE.205
          CHK=CHS(I)                                                             SURFCE.206
!CHK*RHO*CP                                                                      SURFCE.207
          IVGTPK=IVGTYP(I,J)                                                     SURFCE.208
          ISLTPK=ISLTYP(I,J)                                                     SURFCE.209
          VGFRCK=VEGFRC(I,J)/100.                                                SURFCE.210
          SNOWCK=SNOWC(I,J)                                                      SURFCE.211
          Z0=ZNT(I,J)                                                            SURFCE.212
CFEI                                                                             SURFCE.213
          IF(RDMAXALB) THEN                                                      SURFCE.214
            SNOALB=ALBMAX(I,J)*0.01                                              SURFCE.215
          ELSE                                                                   SURFCE.216
            SNOALB=MAXALB(IVGTPK)*0.01                                           SURFCE.217
          ENDIF                                                                  SURFCE.218
CFEI                                                                             SURFCE.219
          XLAIK=LAITBL(IVGTPK)                                                   SURFCE.220
          NROOT=NROTBL(IVGTPK)                                                   SURFCE.221
          Z0BRD=SFZ0(IVGTPK,ISN)*0.01                                            SURFCE.222
CFEI                                                                             SURFCE.223
          IF(RDBRDALB) THEN                                                      SURFCE.224
            ALBBRD=ALBEDO(I,J)*0.01                                              SURFCE.225
          ELSE                                                                   SURFCE.226
            ALBBRD=ALBD(IVGTPK,ISN)*0.01                                         SURFCE.227
          ENDIF                                                                  SURFCE.228
CFEI                                                                             SURFCE.229
          LCH=.true.                                                             SURFCE.230
          OFFLINE=.false.                                                        SURFCE.231
          SLOPETYP=2                                                             SURFCE.232
CFEI                                                                             SURFCE.233
          SHDMIN=XVEGMN(I,J)                                                     23DEC04.3799
          IF(IVGTPK.EQ.19.OR.IVGTPK.EQ.24.OR.IVGTPK.EQ.26.OR.IVGTPK              SURFCE.235
     &   .EQ.27.) THEN                                                           SURFCE.236
             VGFRCK=0.0                                                          SURFCE.237
             SHDMIN=0.0                                                          SURFCE.238
          ENDIF                                                                  SURFCE.239
                                                                                 SURFCE.240
          IF(ISLTPK.EQ.14.AND.XICE(I,J).LT.0.5)THEN                              23DEC04.3800
         PRINT*,' SOIL TYPE FOUND TO BE WATER AT A LAND-POINT'                   SURFCE.242
         PRINT*,i,j,inest,'RESET SOIL in surfce.F'                               SURFCE.243
            ISLTYP(I,J)=7                                                        SURFCE.244
            ISLTPK=7                                                             SURFCE.245
          ENDIF                                                                  SURFCE.246
          T1K=TGDSA(I)                                                           SURFCE.247
          CMCK=MAX(CMCB(I,J),1.E-10)                                             SURFCE.248
          U10K=U10(I)                                                            SURFCE.249
          V10K=V10(I)                                                            SURFCE.250
          TH2K=TH2(I)                                                            SURFCE.251
          Q2M=Q2(I)                                                              SURFCE.252
C*** convert water equivalent of snow depth from mm to meter                     SURFCE.253
          SNODPK=SNOB(I,J)*0.001                                                 SURFCE.254
!KWM          SNOWHK=SNOWHB(I,J)                                                 SURFCE.255
          SNOWHK=SNOWHB(I,J)*0.001 !KWM                                          SURFCE.256
cFEI check the initialization of snowb, why it's < 0.0                           SURFCE.257
!KWM          IF(SNOB(I,J).LE.0.001.OR.SNOWHK.LE.0.0001) THEN                    SURFCE.258
          IF(SNOB(I,J).LE.0.001.OR.SNOWHB(I,J).LE.0.0001) THEN !KWM              SURFCE.259
               SNODPK=0.0                                                        SURFCE.260
               SNOWHK=0.0                                                        SURFCE.261
          ENDIF                                                                  SURFCE.262
!KWM          IF(SNOWHK.LT.SNOB(I,J)) THEN                                       SURFCE.263
          IF(SNOWHB(I,J).LT.SNOB(I,J)) THEN !KWM                                 SURFCE.264
!KWM            SNOWHK=5.0*SNOB(I,J)                                             SURFCE.265
!KWM            SNOWHB(I,J)=SNOWHK                                               SURFCE.266
            SNOWHB(I,J)=5.0*SNOB(I,J) !KWM                                       SURFCE.267
            SNOWHK=SNOWHB(I,J) * 0.001 !KWM                                      SURFCE.268
          ENDIF                                                                  SURFCE.269
cFEI                                                                             SURFCE.270
C                                                                                SURFCE.271
          DO 70 NS=1,NSOIL                                                       SURFCE.272
            SMCK(NS)=MAX(SMCB(I,J,NS),1.E-6)                                     SURFCE.273
            STCK(NS)=STCB(I,J,NS)                                                SURFCE.274
            SWCK(NS)=MAX(SWCB(I,J,NS),1.E-6)                                     SURFCE.275
   70     CONTINUE                                                               SURFCE.276
C                                                                                SURFCE.277
C-----------------------------------------------------------------------         SURFCE.278
c         CALL SFLX(ICE,SATFLG,DTK,Z,NSOIL,NROOT,DZS,FK,SOLDN,SFCPRS,            SURFCE.279
c    +         PRCP,SFCTMP,SFCTH2,Q2K,Q2SAT,DQSDTK,TBOTK,CHK,CHKFF,              SURFCE.280
c    +         IVGTPK,ISLTPK,VGFRCK,PLFLX,ELFLX,HFLX,GFLX,RNOF1K,RNOF2K,         SURFCE.281
c    +         Q1K,SMELTK,T1K,CMCK,SMCK,STCK,SNODPK,SOILQW,SOILQM)               SURFCE.282
C-----------------------------------------------------------------------         SURFCE.283
        NOOUT=0                                                                  SURFCE.284
        if(NOOUT.eq.1) then                                                      SURFCE.285
C        IF(INEST.EQ.2.AND.I.EQ.24.AND.J.EQ.3)  then                             SURFCE.286
         IF(INEST.EQ.1.AND.I.EQ.39.AND.J.EQ.70)  then                            SURFCE.287
         print*,'outputting at xtime =', xtime                                   23DEC04.3801
         print*,' before SFLX','INEST',INEST,'I',I,'J',J                         23DEC04.3802
        print*,' SFCTMP ',SFCTMP,'SFCPRS ',SFCPRS,'CH ',CHK,                     SURFCE.290
     & 'PRCP ',PRCP,                                                             SURFCE.291
     & 'IVGTPK ',IVGTPK, 'ISLTPK ', ISLTPK,                                      SURFCE.292
     &  'Q2 ',Q2K,'Q2SAT ',Q2SAT,'ETP ',PLFLX,                                   SURFCE.293
     &  'SMELTK ',SMELTK,'SOLDN ',SOLDN,'LWDN ',LWDN,                            SURFCE.294
     &  'DQSDT2 ',DQSDTK,'FLX2 ',FLX2,                                           SURFCE.295
     &  'ALBEDO ',ALBEDOK,                                                       SURFCE.296
     &  'ALB ',ALB(I,J),'ALBBRD ',ALBBRD,'T1K ',T1K,                             SURFCE.297
     &  'ICE',ICE,'DTK',DTK,'Z',Z                                                SURFCE.298
      print*,' RNOF ', RNOF1K, RNOF2K, RNOF3K                                    SURFCE.299
      print*,' SMC ', SMCK                                                       SURFCE.300
      print*,' STC ', STCK                                                       SURFCE.301
      print*,' SH2O ', SWCK                                                      SURFCE.302
      print*,' SNEQV',SNODPK, ' SNOWH ',SNOWHK, ' CMCK ',CMCK                    SURFCE.303
      print*,' HFLX',HFLX,'ELFLX ',ELFLX, ' GFLX',GFLX,' FDOWN',FK               SURFCE.304
         endif                                                                   SURFCE.305
        endif                                                                    SURFCE.306
                                                                                 SURFCE.307
         CALL SFLX (ICE,DTK,Z,NSOIL,DZS,                                         SURFCE.308
     +         SATFLG,LCH,OFFLINE,                                               SURFCE.309
     +         LUTYPE, SLTYPE,                                                   SURFCE.310
     +         LWDN,SOLDN,SFCPRS,PRCP,SFCTMP,Q2K,DUMMY,                          SURFCE.311
     +         DUMMY,DUMMY, DUMMY,                                               SURFCE.312
     +         SFCTH2,Q2SAT,DQSDTK,                                              SURFCE.313
     +         IVGTPK,ISLTPK,SLOPETYP,VGFRCK,SHDMIN,DUMMY,                       SURFCE.314
     +         ALBBRD, SNOALB,TBOTK,Z0BRD, Z0, EMISSI,                           23DEC04.3803
     +         CMCK,T1K,STCK,SMCK,SWCK,SNOWHK,SNODPK,ALBEDOK,CHK,DUMMY,          SURFCE.316
     +         ELFLX,HFLX,ELFLXKIN,FK,                                           SURFCE.317
     +         EC,EDIR,ET,ETT,ESNOW,DRIP,DEW,                                    SURFCE.318
     +         BET,PLFLX,GFLX,                                                   SURFCE.319
     +         FLX1,FLX2,FLX3,                                                   SURFCE.320
     +         SMELTK,SNOWCK,                                                    SURFCE.321
     +         RNOF1K,RNOF2K,RNOF3K,                                             SURFCE.322
     +         RC,PC,RSMIN,XLAIK,RCS,RCT,RCQ,RCSOIL,                             SURFCE.323
     +         SOILQW,SOILQM,Q1K,                                                SURFCE.324
     +         SMCWLT,SMCDRY,SMCREF,SMCMAX,NROOT)                                SURFCE.325
                                                                                 SURFCE.326
        if(NOOUT.eq.1) then                                                      SURFCE.327
c        IF(INEST.EQ.2.AND.I.EQ.24.AND.J.EQ.3)  then                             SURFCE.328
         IF(INEST.EQ.1.AND.I.EQ.39.AND.J.EQ.70)  then                            SURFCE.329
         print*,' after SFLX', 'INEST',INEST,'I',I,'J',J                         23DEC04.3804
         print*,'outputting at xtime =', xtime                                   23DEC04.3805
       print*,' SFCTMP',SFCTMP,' SFCPRS',SFCPRS,' CH',CHK,                       SURFCE.332
     & ' PRCP',PRCP,                                                             SURFCE.333
     & ' IVGTPK ',IVGTPK, 'ISLTPK ', ISLTPK,                                     SURFCE.334
     &  ' Q2',Q2K,' Q2SAT ',Q2SAT,' ETP',PLFLX,                                  SURFCE.335
     &  ' SMELTK',SMELTK,' SOLDN',SOLDN,' LWDN',LWDN,                            SURFCE.336
     &  'DQSDT2 ',DQSDTK,'FLX2 ',FLX2,                                           SURFCE.337
     &  'ALBEDO ',ALBEDOK,                                                       SURFCE.338
     &  'ALB ',ALB(I,J),'ALBBRD ',ALBBRD,'T1K ',T1K,                             SURFCE.339
     &  'ICE',ICE,'DTK',DTK,'Z',Z                                                SURFCE.340
      print*,' RNOF ', RNOF1K, RNOF2K, RNOF3K                                    SURFCE.341
      print*,' SMC ', SMCK                                                       SURFCE.342
      print*,' STC ', STCK                                                       SURFCE.343
      print*,' SH2O ', SWCK                                                      SURFCE.344
      print*,' SNEQV ',SNODPK, 'SNOWH ',SNOWHK, 'CMCK ',CMCK                     SURFCE.345
      print*,'HFLX ',HFLX,'ELFLX ',ELFLX, 'GFLX ',GFLX,'FDOWN ',FK               SURFCE.346
         endif                                                                   SURFCE.347
        endif                                                                    23DEC04.3806
C                                                                                SURFCE.349
C      SCHECK=Z*CHK                                                              SURFCE.350
C      IF(SCHECK.LE.1.3E-3)THEN                                                  SURFCE.351
C        PLFLX=0.                                                                SURFCE.352
C        ELFLX=0.                                                                SURFCE.353
C      ENDIF                                                                     SURFCE.354
C***                                                                             SURFCE.355
C***  DIAGNOSTICS                                                                SURFCE.356
C***                                                                             SURFCE.357
C        Convert the water unit into mm                                          SURFCE.358
          SSROFF(I,J)=SSROFF(I,J)+RNOF1K*DTHALF*1000.0                           SURFCE.359
          BGROFF(I,J)=BGROFF(I,J)+RNOF2K*DTHALF*1000.0                           SURFCE.360
          SMSTAV(I,J)=SOILQW                                                     SURFCE.361
          SMSTOT(I,J)=SOILQM*1000.                                               SURFCE.362
          SFCEXC(I,J)=CHK                                                        SURFCE.363
C       IF(SNOB(I,J).GT.0..OR.SICE(I,J).GT.0.)THEN                               SURFCE.364
C         QFC1(I,J)=QFC1(I,J)*RLIVWV                                             SURFCE.365
C       ENDIF                                                                    SURFCE.366
          IF(TX(I,KL).LE.T0)THEN                                                 SURFCE.367
            ACSNOW(I,J)=ACSNOW(I,J)+PREC(I)*DTK*0.5                              SURFCE.368
          ENDIF                                                                  SURFCE.369
          IF(SNOB(I,J).GT.0.)THEN                                                SURFCE.370
            ACSNOM(I,J)=ACSNOM(I,J)+SMELTK/2.0*1000.                             SURFCE.371
            SNOPCX(I,J)=SNOPCX(I,J)-SMELTK/FDTLIW                                SURFCE.372
          ENDIF                                                                  SURFCE.373
C       POTEVP(I,J)=POTEVP(I,J)+PLFLX*DTHALF                                     SURFCE.374
C       SUBSHX(I,J)=SUBSHX(I,J)+GFLX                                             SURFCE.375
C***                                                                             SURFCE.376
C***  MM5 LOWER BOUNDARY CONDITIONS                                              SURFCE.377
C***                                                                             SURFCE.378
c       GRNFLX(I,J)=-GFLX                                                        SURFCE.379
c       HFX(I,J)=-HFLX                                                           SURFCE.380
c       QFX(I,J)=-ELFLX/ELWV                                                     SURFCE.381
          GRNFLX(I,J)=GFLX                                                       SURFCE.382
          HFX(I,J)=HFLX                                                          SURFCE.383
          QFX(I,J)=ELFLXKIN                                                      SURFCE.384
          SFCEVP(I,J)=SFCEVP(I,J)+QFX(I,J)*DTHALF                                SURFCE.385
          TGB(I,J)=T1K                                                           SURFCE.386
          T2K=T1K-HFX(I,J)/(RHOX(I)*CPM(I)*CHS2(I))                              SURFCE.387
          Q2M=Q1K-QFX(I,J)/(RHOX(I)*CHS2(I))                                     SURFCE.388
          Q2(I)=Q2M                                                              SURFCE.389
          TH2(I)=T2K*(1.E5/SFCPRS)**ROVCP                                        23DEC04.3807
C        t2k=th2k/(1.E5/SFCPRS)**ROVCP                                           SURFCE.390
C        QS(I,J)=Q1K                                                             SURFCE.391
C***                                                                             SURFCE.392
C***  APPLY ASSELIN FILTER TO HISTORICAL VARIABLES FOR LARGE TIME STEP           SURFCE.393
C***                                                                             SURFCE.394
!KWM          SNOWHB(I,J)=OMUHF*SNOWHA(I,J)+GNUHF*(SNOWHK+SNOWHB(I,J))           SURFCE.395
!KWM          SNOWHA(I,J)=SNOWHK                                                 SURFCE.396
          SNOWHB(I,J)=OMUHF*SNOWHA(I,J)+GNUHF*(SNOWHK*1.E3+SNOWHB(I,J))          23DEC04.3808
          SNOWHA(I,J)=SNOWHK*1.E3  !KWM                                          SURFCE.398
          SNOB(I,J)=OMUHF*SNOA(I,J)+GNUHF*(SNODPK*1000.0+SNOB(I,J))              SURFCE.399
          SNOA(I,J)=SNODPK*1000.0                                                SURFCE.400
          SNOWC(I,J)=SNOWCK                                                      SURFCE.401
           if(SNOWHB(I,J).le.0.0.OR.SNOA(I,J).LE.0.0) then                       SURFCE.402
              SNOB(I,J)=0.0                                                      SURFCE.403
              SNOWHB(I,J)=0.0                                                    SURFCE.404
           endif                                                                 SURFCE.405
          CMCB(I,J)=OMUHF*CMCA(I,J)+GNUHF*(CMCK+CMCB(I,J))                       SURFCE.406
          CMCA(I,J)=CMCK                                                         SURFCE.407
          ALB(I,J)=ALBEDOK                                                       SURFCE.408
          ZNT(I,J)=Z0                                                            SURFCE.409
c       IF (ICE .EQ. 0)THEN                                                      SURFCE.410
c         IF(SNOB(I,J).GT.1.0)THEN                                               SURFCE.411
c           ALB(I,J)=0.01*ALBD(IVGTPK,ISN)*(1.+SCFX(IVGTPK))                     SURFCE.412
c         ELSE                                                                   SURFCE.413
c           ALB(I,J)=0.01*ALBD(IVGTPK,ISN)                                       SURFCE.414
c         ENDIF                                                                  SURFCE.415
C       ENDIF                                                                    SURFCE.416
C                                                                                SURFCE.417
          DO 80 NS=1,NSOIL                                                       SURFCE.418
            SMCB(I,J,NS)=OMUHF*SMCA(I,J,NS)+GNUHF*(SMCK(NS)+SMCB(I,J,NS)         SURFCE.419
     +                   )                                                       SURFCE.420
            SMCA(I,J,NS)=SMCK(NS)                                                SURFCE.421
            SWCB(I,J,NS)=OMUHF*SWCA(I,J,NS)+GNUHF*(SWCK(NS)+SWCB(I,J,NS)         SURFCE.422
     +                   )                                                       SURFCE.423
            SWCA(I,J,NS)=SWCK(NS)                                                SURFCE.424
            STCB(I,J,NS)=OMUHF*STCA(I,J,NS)+GNUHF*(STCK(NS)+STCB(I,J,NS)         SURFCE.425
     +                   )                                                       SURFCE.426
            STCA(I,J,NS)=STCK(NS)                                                SURFCE.427
   80     CONTINUE                                                               SURFCE.428
        ENDIF                                                                    SURFCE.429
        IF(NOOUT.EQ.0)GOTO 100                                                   SURFCE.430
c     write output to 29                                                         SURFCE.431
        IF(KTAU.EQ.0.AND.I.EQ.20.AND.J.EQ.27) then                               SURFCE.432
            if (INEST.EQ.1.or.INEST.eq.2) then                                   SURFCE.433
             WRITE(29+INEST,*)                                                   SURFCE.434
     +    '  XTIME   ','   FK     ','  SOLDN   ','  SFCPR   ',                   SURFCE.435
     +    '  RAINK   ','  SFCTMP  ','    Q2K   ','   TBOTK  ',                   SURFCE.436
     +    '   CHK    ','  ELFLX   ','   HFLX   ','   GFLX   ',                   SURFCE.437
     +    ' RNOF1K   ',' RNOF2K   ','    T1K   ','   CMCK   ',                   SURFCE.438
     +    '  SMCK1   ','  SMCK2   ','  SMCK3   ','  SMCK4   ',                   SURFCE.439
     +    '  STCK1   ','  STCK2   ','  STCK3   ','  STCK4   ',                   SURFCE.440
     +    ' SNODPK   ','    U10   ','    V10   ','     T2   ',                   SURFCE.441
     +    '     Q2   ',' SMSTOT   ',' SFCEVP   ','  SWC1'    ,                   SURFCE.442
     +    '  SWC2    ',' SWC3     ',' SWC4   '                                   SURFCE.443
           endif                                                                 SURFCE.444
        ENDIF                                                                    SURFCE.445
C I: Y-direction lat.                                                            SURFCE.446
C J: X-direction lon.                                                            SURFCE.447
        IF((KTAU.EQ.0.OR.MOD(KTAU,1).EQ.0)) THEN                                 SURFCE.448
         IF((INEST.EQ.2.AND.I.EQ.34.AND.J.EQ.51).OR.                             SURFCE.449
     +    (INEST.EQ.1.AND.I.EQ.36.AND.J.EQ.36)) THEN                             SURFCE.450
       WRITE(29+INEST,*) 'outputting at xtime =', xtime,'INEST ', INEST          SURFCE.451
       WRITE(29+INEST,1029)XTIME,FK,SOLDN,SFCPRS/100.,RAINK,SFCTMP,              SURFCE.452
     &         1000.*Q2K,TBOTK,1000.*CHK,ELFLX,HFLX,GFLX,SSROFF(I,J)             SURFCE.453
     +                   ,BGROFF(I,J),T1K,CMCK,SMCK,STCK,SNODPK,U10K,            SURFCE.454
     +                   V10K,T2K,Q2M,SMSTOT(I,J),SFCEVP(I,J),SWCK               SURFCE.455
 1029     FORMAT (7F10.5)                                                        SURFCE.456
          IF(KTAU.EQ.0)WRITE (37+INEST,*)'   P      ','   T      ',              SURFCE.457
     +      '   TH     ','   Q      ','   U      ','   V      ',                 SURFCE.458
     +      '   QC     '                                                         SURFCE.459
          WRITE (37+INEST,1039)XTIME                                             SURFCE.460
          DO K=1,MKX                                                             SURFCE.461
            PRESK=(A(K)*PSB(I,J)+PTOP+PP3D(I,J,K)*0.001)*10.                     SURFCE.462
            WRITE (37+INEST,1039)PRESK,TX(I,K),THX(I,K),1000.*QX(I,K),           SURFCE.463
     +     UX(I,K),                                                              SURFCE.464
     +                     VX(I,K),1000.*QCX(I,K)                                SURFCE.465
 1039       FORMAT (7F10.5)                                                      SURFCE.466
                                                                                 SURFCE.467
                                                                                 SURFCE.468
          ENDDO                                                                  SURFCE.469
        ENDIF                                                                    SURFCE.470
        ENDIF                                                                    SURFCE.471
C                                                                                SURFCE.472
  100 CONTINUE                                                                   SURFCE.473
C                                                                                SURFCE.474
C***  ACCUMULATE SURFACE HEAT FLUXES HERE.                                       SURFCE.475
C***  ACCUMULATE ACTUAL AND POTENTIAL EVAPORATION.                               SURFCE.476
C***  ACCUMULATE TOTAL SNOW MELT AND THE ASSOCIATED NET HEAT FLUX.               SURFCE.477
C                                                                                SURFCE.478
C     DO 200 I=1,ILX                                                             SURFCE.479
C      SFCSHX(I,J)=SFCSHX(I,J)+HFX(I,J)                                          SURFCE.480
C      SFCLHX(I,J)=SFCLHX(I,J)+QFX(I,J)*ELWV                                     SURFCE.481
C      SFCEVP(I,J)=SFCEVP(I,J)+QFX(I,J)*DTHALF                                   SURFCE.482
C      POTEVP(I,J)=POTEVP(I,J)-QWBS(I,J)*SM(I,J)*FDTW                            SURFCE.483
C      POTFLX(I,J)=POTFLX(I,J)+QWBS(I,J)*SM(I,J)                                 SURFCE.484
C                                                                                SURFCE.485
C***  IF COLD ENOUGH, IT SNOWS (IN LSM )...                                      SURFCE.486
C***  ACCUMULATE TOTAL SNOWFALL.                                                 SURFCE.487
C                                                                                SURFCE.488
C     IF(TX(I,KL)*RPSB(I,J).LE.T0.AND.XLAND(I,J).LT.1.5)THEN                     SURFCE.489
C       ACSNOW(I,J)=ACSNOW(I,J)+PREC(I)*DTK*0.5                                  SURFCE.490
C***                                                                             SURFCE.491
C***  ... OTHERWISE IT RAINS.                                                    SURFCE.492
C***                                                                             SURFCE.493
C     ENDIF                                                                      SURFCE.494
C 200 CONTINUE                                                                   SURFCE.495
C                                                                                SURFCE.496
C-----------------------------------------------------------------------         SURFCE.497
      RETURN                                                                     SURFCE.498
      END                                                                        SURFCE.499
