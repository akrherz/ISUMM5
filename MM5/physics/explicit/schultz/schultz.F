      SUBROUTINE SCHULTZ(T3DTEN,QV3DTEN,QC3DTEN,QR3DTEN,                         SCHULTZ.1
     &                   QI3DTEN,QNI3DTEN,QG3DTEN,QNC3DTEN,                      SCHULTZ.2
     &                   T3D,QV3D,QC3D,QR3D,QI3D,QNI3D,                          SCHULTZ.3
     &                   QG3D,QNC3D,PP3D,PSB,RAINNC,                             SCHULTZ.4
     &                   J,IN,IST,IEN)                                           SCHULTZ.5
                                                                                 SCHULTZ.6
#     include <parame.incl>                                                      SCHULTZ.7
#     include <varia.incl>                                                       SCHULTZ.8
#     include <param3.incl>                                                      SCHULTZ.9
#     include <param2.incl>                                                      SCHULTZ.10
#     include <pmoist.incl>                                                      SCHULTZ.11
C     include <point3d.incl>                                                     SCHULTZ.12
C     include <point2d.incl>                                                     SCHULTZ.13
#     include <various.incl>                                                     SCHULTZ.14
#     include <nhcnst.incl>                                                      SCHULTZ.15
#     include <rpstar.incl>                                                      SCHULTZ.16
                                                                                 SCHULTZ.17
C INPUT VARIABLES                                                                SCHULTZ.18
                                                                                 SCHULTZ.19
      REAL  T3D(MIX,MJX,MKX),        QV3D(MIX,MJX,MKX),                          SCHULTZ.20
     &     QC3D(MIXM,MJXM,MKXM),     QR3D(MIXM,MJXM,MKXM),                       SCHULTZ.21
     &     QI3D(MIXIC,MJXIC,MKXIC), QNI3D(MIXIC,MJXIC,MKXIC),                    SCHULTZ.22
     &     QG3D(MIXIG,MJXIG,MKXIG), QNC3D(MIXIG,MJXIG,MKXIG),                    SCHULTZ.23
     &     PP3D(MIX,MJX,MKX),         PSB(MIX,MJX)                               SCHULTZ.24
      REAL RAINNC(MIX,MJX)                                                       SCHULTZ.25
                                                                                 SCHULTZ.26
C ARRAYS FOR BASE VARIABLES                                                      SCHULTZ.27
                                                                                 SCHULTZ.28
      REAL TAOUT(MIX,MKX),PRES(MIX,MKX),RHO(MIX,MKX)                             SCHULTZ.29
     +     ,T3DTEN(MIX,MJX,MKX)                                                  SCHULTZ.30
                                                                                 SCHULTZ.31
C ARRAYS FOR QV                                                                  SCHULTZ.32
                                                                                 SCHULTZ.33
      REAL QAOUT(MIX,MKX),QV3DTEN(MIX,MJX,MKX)                                   SCHULTZ.34
                                                                                 SCHULTZ.35
C ARRAYS FOR QC                                                                  SCHULTZ.36
                                                                                 SCHULTZ.37
      REAL CLOUD(MIX,MKX),QC3DTEN(MIXM,MJXM,MKXM)                                SCHULTZ.38
                                                                                 SCHULTZ.39
C ARRAYS FOR QR                                                                  SCHULTZ.40
                                                                                 SCHULTZ.41
      REAL RAIN(MIX,MKX),QR3DTEN(MIXM,MJXM,MKXM),FR(MKX)                         SCHULTZ.42
     +     ,FALOUTR(MKX),SCR4R(MIX,MKX)                                          SCHULTZ.43
                                                                                 SCHULTZ.44
C ARRAYS FOR QI                                                                  SCHULTZ.45
                                                                                 SCHULTZ.46
      REAL ICE(MIX,MKX),QI3DTEN(MIXIC,MJXIC,MKXIC),FI(MKX)                       SCHULTZ.47
     +     ,FALOUTI(MKX),SCR4I(MIX,MKX)                                          SCHULTZ.48
                                                                                 SCHULTZ.49
C ARRAYS FOR QS                                                                  SCHULTZ.50
                                                                                 SCHULTZ.51
      REAL SNOW(MIX,MKX),QNI3DTEN(MIXIC,MJXIC,MKXIC),FS(MKX)                     SCHULTZ.52
     +     ,FALOUTS(MKX),SCR4S(MIX,MKX)                                          SCHULTZ.53
                                                                                 SCHULTZ.54
C ARRAYS FOR QG                                                                  SCHULTZ.55
                                                                                 SCHULTZ.56
      REAL GRAUPEL(MIX,MKX),QG3DTEN(MIXIG,MJXIG,MKXIG),FG(MKX)                   SCHULTZ.57
     +     ,FALOUTG(MKX),SCR4G(MIX,MKX)                                          SCHULTZ.58
                                                                                 SCHULTZ.59
C ARRAYS FOR NCI                                                                 SCHULTZ.60
                                                                                 SCHULTZ.61
      REAL NCON(MIX,MKX),QNC3DTEN(MIXIG,MJXIG,MKXIG)                             SCHULTZ.62
                                                                                 SCHULTZ.63
C SOME SCRATCH VARIABLES IN GENERAL                                              SCHULTZ.64
                                                                                 SCHULTZ.65
      REAL VT2R, VT2I, VT2S, VT2G, TAOUT1, QAOUT1, CLOUD1, ICE1, RAIN1           SCHULTZ.66
     +     ,SNOW1, GRAUP1, SCR7, RHO2, DTO2                                      SCHULTZ.67
     +     ,FALTNDR, FALTNDS, FALTNDG, FALTNDI, RGVM                             SCHULTZ.68
      INTEGER NSTEP, K, I, J, IST, IEN, IN                                       SCHULTZ.69
      LOGICAL PRINT_FLAG, TIME_FLAG                                              SCHULTZ.70
                                                                                 SCHULTZ.71
      real tbar, dtemp, deezee, lapse, dxkm                                      SCHULTZ.72
                                                                                 SCHULTZ.73
      dxkm = dx/1000.                                                            SCHULTZ.74
                                                                                 SCHULTZ.75
C     INTEGER TOTAL_TIME, IUNIT, NCALL                                           SCHULTZ.76
C     DATA NCALL /0/                                                             SCHULTZ.77
C     DATA TOTAL_TIME /0/                                                        SCHULTZ.78
C     DATA IUNIT /76/                                                            SCHULTZ.79
C     SAVE                                                                       SCHULTZ.80
C                                                                                SCHULTZ.81
C     NCALL = NCALL + 1                                                          SCHULTZ.82
C     PRINT_FLAG = .FALSE.                                                       SCHULTZ.83
C     TIME_FLAG = .FALSE.                                                        SCHULTZ.84
C     IF (NCALL.GT.1) THEN                                                       SCHULTZ.85
C        DTO2 = DT/2.                                                            SCHULTZ.86
C     ELSE                                                                       SCHULTZ.87
C        DTO2 = DT                                                               SCHULTZ.88
C     ENDIF                                                                      SCHULTZ.89
C     TOTAL_TIME = TOTAL_TIME + NINT(DTO2)                                       SCHULTZ.90
C     IF (MOD(TOTAL_TIME,3600).EQ.0) TIME_FLAG = .TRUE.                          SCHULTZ.91
                                                                                 SCHULTZ.92
      DO K=1,KL                                                                  SCHULTZ.93
        DO I=IST,IEN                                                             SCHULTZ.94
          TAOUT(I,K)=T3D(I,J,K)                                                  SCHULTZ.95
          QAOUT(I,K)=AMAX1(0.0,QV3D(I,J,K))                                      SCHULTZ.96
          PRES(I,K)=(A(K)*PSB(I,J)+PTOP)*1.E3+PP3D(I,J,K)                        SCHULTZ.97
          RHO(I,K)=PRES(I,K)/(R*TAOUT(I,K))                                      SCHULTZ.98
        ENDDO                                                                    SCHULTZ.99
      ENDDO                                                                      SCHULTZ.100
                                                                                 SCHULTZ.101
C Calculation of lapse rate is done over the layer *below*                       SCHULTZ.102
C the level.  The lapse rate is assumed isothermal from the                      SCHULTZ.103
C dirt to the lowest level.                                                      SCHULTZ.104
                                                                                 SCHULTZ.105
C Probably should include moisture/virtual effects in lapse                      SCHULTZ.106
C rate calculation.                                                              SCHULTZ.107
                                                                                 SCHULTZ.108
      DO K=1,KL                                                                  SCHULTZ.109
        DO I=IST,IEN                                                             SCHULTZ.110
          CLOUD(I,K)=AMAX1(0.0,QC3D(I,J,K))                                      SCHULTZ.111
          ICE(I,K)=AMAX1(0.0,QI3D(I,J,K))                                        SCHULTZ.112
          RAIN(I,K)=AMAX1(0.0,QR3D(I,J,K))                                       SCHULTZ.113
          SNOW(I,K)=AMAX1(0.0,QNI3D(I,J,K))                                      SCHULTZ.114
          GRAUPEL(I,K)=AMAX1(0.0,QG3D(I,J,K))                                    SCHULTZ.115
          VT2R=0.0                                                               SCHULTZ.116
          VT2I=0.0                                                               SCHULTZ.117
          VT2S=0.0                                                               SCHULTZ.118
          VT2G=0.0                                                               SCHULTZ.119
          if (k.lt.kl) then                                                      SCHULTZ.120
            tbar = .5*(taout(i,k)+taout(i,k+1))                                  SCHULTZ.121
            dtemp = (taout(i,k)-taout(i,k+1))                                    SCHULTZ.122
            deezee = -ROVG*tbar*(alog(pres(i,k)/pres(i,k+1)))                    SCHULTZ.123
            lapse = 1000.*dtemp/deezee                                           SCHULTZ.124
          else                                                                   SCHULTZ.125
            lapse = 0.                                                           SCHULTZ.126
          end if                                                                 SCHULTZ.127
          TAOUT1=TAOUT(I,K)                                                      SCHULTZ.128
          TAOUT(I,K)=TAOUT(I,K)+T3DTEN(I,J,K)*RPSB(I,J)*DT                       SCHULTZ.129
          QAOUT1=QAOUT(I,K)                                                      SCHULTZ.130
          QAOUT(I,K)=QAOUT(I,K)+QV3DTEN(I,J,K)*RPSB(I,J)*DT                      SCHULTZ.131
          CLOUD1=CLOUD(I,K)                                                      SCHULTZ.132
          CLOUD(I,K)=CLOUD(I,K)+QC3DTEN(I,J,K)*RPSB(I,J)*DT                      SCHULTZ.133
          ICE1=ICE(I,K)                                                          SCHULTZ.134
          ICE(I,K)=ICE(I,K)+QI3DTEN(I,J,K)*RPSB(I,J)*DT                          SCHULTZ.135
          RAIN1=RAIN(I,K)                                                        SCHULTZ.136
          RAIN(I,K)=RAIN(I,K)+QR3DTEN(I,J,K)*RPSB(I,J)*DT                        SCHULTZ.137
          SNOW1=SNOW(I,K)                                                        SCHULTZ.138
          SNOW(I,K)=SNOW(I,K)+QNI3DTEN(I,J,K)*RPSB(I,J)*DT                       SCHULTZ.139
          GRAUP1=GRAUPEL(I,K)                                                    SCHULTZ.140
          GRAUPEL(I,K)=GRAUPEL(I,K)+QG3DTEN(I,J,K)*RPSB(I,J)*DT                  SCHULTZ.141
CC            IF (I.EQ.INT(FLOAT(IEN)*45./100.).AND.TIME_FLAG)                   SCHULTZ.142
CC     +           PRINT_FLAG = .TRUE.                                           SCHULTZ.143
          CALL SCHULTZ_MIC(PRES(I,K),TAOUT(I,K),RHO(I,K),lapse,dxkm,DT,          SCHULTZ.144
     +         QAOUT(I,K),CLOUD(I,K),RAIN(I,K),ICE(I,K),SNOW(I,K),               SCHULTZ.145
     +         GRAUPEL(I,K),VT2R,VT2I,VT2S,VT2G,PRINT_FLAG)                      SCHULTZ.146
          SCR4R(I,K)=-VT2R                                                       SCHULTZ.147
          SCR4S(I,K)=-VT2S                                                       SCHULTZ.148
          SCR4G(I,K)=-VT2G                                                       SCHULTZ.149
          SCR4I(I,K)=-VT2I                                                       SCHULTZ.150
C           T3DTEN(I,J,K)  = T3DTEN(I,J,K)                                       SCHULTZ.151
C    +                     + PSB(I,J)*(TAOUT(I,K)-TAOUT1)/DT                     SCHULTZ.152
C           QV3DTEN(I,J,K) = QV3DTEN(I,J,K)                                      SCHULTZ.153
C    +                     + PSB(I,J)*(QAOUT(I,K)-QAOUT1)/DT                     SCHULTZ.154
          T3DTEN(I,J,K)=PSB(I,J)*(TAOUT(I,K)-TAOUT1)/DT                          SCHULTZ.155
          QV3DTEN(I,J,K)=PSB(I,J)*(QAOUT(I,K)-QAOUT1)/DT                         SCHULTZ.156
          QC3DTEN(I,J,K)=PSB(I,J)*(CLOUD(I,K)-CLOUD1)/DT                         SCHULTZ.157
          QI3DTEN(I,J,K)=PSB(I,J)*(ICE(I,K)-ICE1)/DT                             SCHULTZ.158
          QR3DTEN(I,J,K)=PSB(I,J)*(RAIN(I,K)-RAIN1)/DT                           SCHULTZ.159
          QNI3DTEN(I,J,K)=PSB(I,J)*(SNOW(I,K)-SNOW1)/DT                          SCHULTZ.160
          QG3DTEN(I,J,K)=PSB(I,J)*(GRAUPEL(I,K)-GRAUP1)/DT                       SCHULTZ.161
          QNC3DTEN(I,J,K)=0.0                                                    SCHULTZ.162
CC            PRINT_FLAG = .FALSE.                                               SCHULTZ.163
        ENDDO                                                                    SCHULTZ.164
      ENDDO                                                                      SCHULTZ.165
C--COMPUTE THE FALLOUT TERMS:                                                    SCHULTZ.166
C     COMPUTE FALL TERM WITH SHORTER TIME STEPS WHERE VFALL>DZ/DT                SCHULTZ.167
      DO I=IST,IEN                                                               SCHULTZ.168
        NSTEP=0                                                                  SCHULTZ.169
        DO K=1,KL                                                                SCHULTZ.170
          SCR7=T3D(I,J,K)*PSB(I,J)+DT*T3DTEN(I,J,K)                              SCHULTZ.171
          RHO2=(PRES(I,K)*1.E-3)/(R*SCR7)                                        SCHULTZ.172
C UNITS ARE G-M/S2, RHO2-S2/M2,VT2-M/S                                           SCHULTZ.173
          FR(K)=G*RHO2*SCR4R(I,K)                                                SCHULTZ.174
          FS(K)=G*RHO2*SCR4S(I,K)                                                SCHULTZ.175
          FG(K)=G*RHO2*SCR4G(I,K)                                                SCHULTZ.176
          FI(K)=G*RHO2*SCR4I(I,K)                                                SCHULTZ.177
          RGVM=AMAX1(FR(K),FS(K),FG(K),FI(K))                                    SCHULTZ.178
C 1 IS TO ROUND UP, REPRESENTS NUMBER OF STEPS                                   SCHULTZ.179
          IF(RGVM.GT.0.0)NSTEP=MAX0(INT(RGVM*DT/DSIGMA(K)+1.),NSTEP)             SCHULTZ.180
C--COMPUTE P*T AND P*QR (WITHOUT FALLOUT TERM) AT TAU+1:                         SCHULTZ.181
          SCR4R(I,K)=AMAX1(0.0,QR3D(I,J,K)*PSB(I,J)+DT*QR3DTEN(I,J,K))           SCHULTZ.182
          SCR4S(I,K)=AMAX1(0.0,QNI3D(I,J,K)*PSB(I,J)+DT*QNI3DTEN(I,J,K))         SCHULTZ.183
          SCR4G(I,K)=AMAX1(0.0,QG3D(I,J,K)*PSB(I,J)+DT*QG3DTEN(I,J,K))           SCHULTZ.184
          SCR4I(I,K)=AMAX1(0.0,QI3D(I,J,K)*PSB(I,J)+DT*QI3DTEN(I,J,K))           SCHULTZ.185
        ENDDO                                                                    SCHULTZ.186
        DO N=1,NSTEP                                                             SCHULTZ.187
          DO K=1,KL                                                              SCHULTZ.188
            FALOUTR(K)=FR(K)*SCR4R(I,K)                                          SCHULTZ.189
            FALOUTS(K)=FS(K)*SCR4S(I,K)                                          SCHULTZ.190
            FALOUTG(K)=FG(K)*SCR4G(I,K)                                          SCHULTZ.191
            FALOUTI(K)=FI(K)*SCR4I(I,K)                                          SCHULTZ.192
          ENDDO                                                                  SCHULTZ.193
C FOR TOP OF MODEL                                                               SCHULTZ.194
          K=1                                                                    SCHULTZ.195
          FALTNDR=FALOUTR(K)/DSIGMA(K)                                           SCHULTZ.196
          FALTNDS=FALOUTS(K)/DSIGMA(K)                                           SCHULTZ.197
          FALTNDG=FALOUTG(K)/DSIGMA(K)                                           SCHULTZ.198
          FALTNDI=FALOUTI(K)/DSIGMA(K)                                           SCHULTZ.199
          QR3DTEN(I,J,K)=QR3DTEN(I,J,K)-FALTNDR/NSTEP                            SCHULTZ.200
          QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)-FALTNDS/NSTEP                          SCHULTZ.201
          QG3DTEN(I,J,K)=QG3DTEN(I,J,K)-FALTNDG/NSTEP                            SCHULTZ.202
          QI3DTEN(I,J,K)=QI3DTEN(I,J,K)-FALTNDI/NSTEP                            SCHULTZ.203
          SCR4R(I,K)=SCR4R(I,K)-FALTNDR*DT/NSTEP                                 SCHULTZ.204
          SCR4S(I,K)=SCR4S(I,K)-FALTNDS*DT/NSTEP                                 SCHULTZ.205
          SCR4G(I,K)=SCR4G(I,K)-FALTNDG*DT/NSTEP                                 SCHULTZ.206
          SCR4I(I,K)=SCR4I(I,K)-FALTNDI*DT/NSTEP                                 SCHULTZ.207
          DO K=2,KL                                                              SCHULTZ.208
            FALTNDR=(FALOUTR(K)-FALOUTR(K-1))/DSIGMA(K)                          SCHULTZ.209
            FALTNDS=(FALOUTS(K)-FALOUTS(K-1))/DSIGMA(K)                          SCHULTZ.210
            FALTNDG=(FALOUTG(K)-FALOUTG(K-1))/DSIGMA(K)                          SCHULTZ.211
            FALTNDI=(FALOUTI(K)-FALOUTI(K-1))/DSIGMA(K)                          SCHULTZ.212
            QR3DTEN(I,J,K)=QR3DTEN(I,J,K)-FALTNDR/NSTEP                          SCHULTZ.213
            QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)-FALTNDS/NSTEP                        SCHULTZ.214
            QG3DTEN(I,J,K)=QG3DTEN(I,J,K)-FALTNDG/NSTEP                          SCHULTZ.215
            QI3DTEN(I,J,K)=QI3DTEN(I,J,K)-FALTNDI/NSTEP                          SCHULTZ.216
            SCR4R(I,K)=SCR4R(I,K)-FALTNDR*DT/NSTEP                               SCHULTZ.217
            SCR4S(I,K)=SCR4S(I,K)-FALTNDS*DT/NSTEP                               SCHULTZ.218
            SCR4G(I,K)=SCR4G(I,K)-FALTNDG*DT/NSTEP                               SCHULTZ.219
            SCR4I(I,K)=SCR4I(I,K)-FALTNDI*DT/NSTEP                               SCHULTZ.220
          ENDDO                                                                  SCHULTZ.221
C     ACCUMULATED RAIN                                                           SCHULTZ.222
C 6000 CONVERTS FROM SEC TO MIN AND FROM M TO CM                                 SCHULTZ.223
          RAINNC(I,J)=RAINNC(I,J)+(FALOUTR(KL)+FALOUTS(KL)+FALOUTG(KL)+          SCHULTZ.224
     +                FALOUTI(KL))*DTMIN*6000./G/NSTEP                           SCHULTZ.225
        ENDDO                                                                    SCHULTZ.226
      ENDDO                                                                      SCHULTZ.227
C     IF (TIME_FLAG) THEN                                                        SCHULTZ.228
C        PRINT*, ' OPENING PRECIP (SCHULTZ) FILE CALLED FORT.',IUNIT             SCHULTZ.229
C        OPEN(UNIT=IUNIT, FORM='FORMATTED')                                      SCHULTZ.230
C        DO I=IST,IEN                                                            SCHULTZ.231
C           WRITE (IUNIT, '(F9.6)') RAINNC(I,2)                                  SCHULTZ.232
C        ENDDO                                                                   SCHULTZ.233
C        CLOSE(IUNIT)                                                            SCHULTZ.234
C        IUNIT = IUNIT + 1                                                       SCHULTZ.235
C     ENDIF                                                                      SCHULTZ.236
      RETURN                                                                     SCHULTZ.237
      END                                                                        SCHULTZ.238
