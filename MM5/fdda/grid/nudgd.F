      SUBROUTINE NUDGD(MTIM,MVAR,INVAR,J,XB,XOBJK,XTEN,                          NUDGD.1
     1PSTF,PSTO,GX,ID,WXY,WXY2,TFAC,ZFAC,GP,GR,                                  NUDGD.2
     2MSD,IVAR,VORDIF,IN,KPBLT,BLWXY,BLWNV,SFCOBS,SFCTIM,QSATF,                  NUDGD.3
     3IQCHK,TIMB,TIME,NTB,NTE,SCR2D,BLDUM2D,BLPSOC,BLPSOD,REGJ,ZNTJ,             NUDGD.4
     4INITFLAG)                                                                  NUDGD.5
      IMPLICIT NONE                                                              NUDGD.6
C                                                                                NUDGD.7
C THIS SUBROUTINE COMPUTES THE NUDGING TERM OF THE TENDENCY                      NUDGD.8
C EQUATION (XTEN) FOR XB, WHERE XB CAN BE UB,VB,TB,QB OR PSB.                    NUDGD.9
C GX IS THE NUDGING COEFFICIENT FOR XB AND GP IS THAT FOR                        NUDGD.10
C PSB.  PSTF AND PSTO ARE THE FORECAST AND OBSERVED SFC                          NUDGD.11
C PRESSURES (PS-PTOP) IN CB AT TAU-1. PSTF AND PSTO ARE ON DOT POINTS            NUDGD.12
C FOR THE WINDS; OTHERWISE, THEY ARE ON CROSS POINTS.                            NUDGD.13
C XB IS THE 3-D ARRAY OF THE MODEL VARIABLE AT                                   NUDGD.14
C TAU-1, AND XOBJK IS A N-S VERTICAL SLICE OF THE OBS FIELD.                     NUDGD.15
C IDC=0 IF NUDGING A DOT POINT FIELD, AND ID=1 WHEN THE                          NUDGD.16
C PSB IS NUDGED IN THE CONTINUITY EQ.  WXY IS WDT OR WCS                         NUDGD.17
C FOR THE FINE OR COARSE MESH, DEPENDING ON WHETHER A DOT                        NUDGD.18
C OR CROSS POINT FIELD IS BEING NUDGED.  WXY2 IS EITHER                          NUDGD.19
C WCS OR WCSN, OR WQ OR WQN,  DEPENDING ON IN AND IMOISP(IN).                    NUDGD.20
C ISTORE CONTAINS IL,JL,ETC. AND PSTORE CONTAINS XTIME ETC.                      NUDGD.21
C AND ARE AUTOMATICALLY ISTORN AND PSTORN WHEN IN=2 FOR THE FINE MESH.           NUDGD.22
C IN=1 FOR THE COARSE MESH.  XOBJK IS ALREADY DECOUPLED FROM P*                  NUDGD.23
C WHILE XB IS DECOUPLED HERE.  ZFAC AND TFAC ARE THE VERTICAL                    NUDGD.24
C SPACE FACTOR AND THE TIME FACTOR DEFINED IN SETUPGD AND                        NUDGD.25
C OBTAINED FROM COMMON BLOCK FDDA.                                               NUDGD.26
C   IF INT4=1 (TEMPORAL INTERPOLATION), THEN XB IS THE INTERPOLATED              NUDGD.27
C VALUE, IF INT4=0, THEN THE NUDGING IS DONE TOWARD A                            NUDGD.28
C TARGET VALUE.                                                                  NUDGD.29
C IF IROT AND IWIND ARE 1 THEN THIS SUBROUTINE DOES ROTATIONAL                   NUDGD.30
C OR VORTICITY GRID NUDGING. MSD IS THE MAP SCALE FACTOR ON DOT                  NUDGD.31
C AND GR IS THE ROTATIONAL NUDGING COEFFICIENT (10**7)                           NUDGD.32
C IVAR =1 FOR U AND 2 FOR V.  VORDIF IS THE OBS-FCST VORTICITY                   NUDGD.33
C CALCULATED IN BUFVDGD.                                                         NUDGD.34
C IVAR=3 FOR T, IVAR=4 FOR Q, AND IVAR=5 FOR P*                                  NUDGD.35
C INVAR(IN) IS THE NUDGING SWITCH FOR IVAR                                       NUDGD.36
C     IVAR             FIELD          INVAR                                      NUDGD.37
C      1,2              U,V           IWIND                                      NUDGD.38
C       3                T            ITEMP                                      NUDGD.39
C       4                Q            IMOIS                                      NUDGD.40
C       5                PS           IPSTR                                      NUDGD.41
C                                                                                NUDGD.42
C NOTE: P* CAN BE NUDGED IN THE CONT EQ. OR IN THE SECOND TERM                   NUDGD.43
C       OF THE NUDGING FORMULATION ONLY ONCE IN EITHER NUDGD                     NUDGD.44
C       OR BLNUDGD, ELSE YOU ARE DOUBLING THE TERM.                              NUDGD.45
C       IT IS ASSUMED THAT IF I4D(INP2)=1 AND IPSTR(IN) OR                       NUDGD.46
C       IPSTR(INP2)=1 THEN THE CONTRIBUTION TO THE NUDGING                       NUDGD.47
C       TERM FROM OBS P* WILL BE COMPUTED IN BLNUDGD:                            NUDGD.48
C          IF IPSTR(INP2)=1  SURFACE PRESSURE FROM BL FILE                       NUDGD.49
C                            IS USED FOR PSTO IN BLNUDGD                         NUDGD.50
C          IF IPSTR(INP2)=0  SURFACE PRESSURE FROM 12-HLY DATA                   NUDGD.51
C                            IS USED FOR PSTO IN BLNUDGD                         NUDGD.52
C       IF I4D(INP2)=0 AND IPSTR(IN)=1 THEN THE CONTRIBUTION FROM                NUDGD.53
C       OBS P* WILL BE COMPUTED IN NUDGD AS USUAL.                               NUDGD.54
C                 (THIS CODE HAS NOT BEEN TESTED)                                NUDGD.55
C                                                                                NUDGD.56
C ID=0 NORMAL COMPUTATION OF NUDGING TERM FOR U,V,T,Q                            NUDGD.57
C ID=1 COMPUTATION OF NUDGING TERM IN CONTINUITY (PTEN) EQ.                      NUDGD.58
C ID=2 ROTATIONAL (VORTICITY) NUDGING FOR U AND V                                NUDGD.59
C ID=3 MOISTURE NUDGING USING PRECIP DATA                                        NUDGD.60
C                                                                                NUDGD.61
#     include <parame.incl>                                                      NUDGD.62
#     include <param2.incl>                                                      NUDGD.63
#     include <parfdda.incl>                                                     NUDGD.64
#     include <various.incl>                                                     NUDGD.65
      INTEGER MTIM,MVAR,IN,IVAR                                                  NUDGD.66
      REAL      XTEN(MIX,MJX,MKX),XB(MIX,MJX,MKX),XOBJK(MIXFG,MKXFG),            NUDGD.67
     1          PSTF(MIXFG,MJXFG),PSTO(MIXFG,MJXFG),                             NUDGD.68
     2          WXY(MIXFG,MJXFG),WXY2(MIXFG,MJXFG),GX(MAXSES,2),                 NUDGD.69
     3          TFAC(MAXNES,2),ZFAC(MAXNES,2,MKX),GP(MAXSES,2),                  NUDGD.70
     4          GR(MAXSES),                                                      NUDGD.71
     4          VORDIF(MIXFG,MJXFG,MKXFG),                                       NUDGD.72
     5          WPBL(MIX,MKX),                                                   NUDGD.73
     5          SFCOBS(MTIM,MVAR,MIXFG,MJXFG),                                   NUDGD.74
     6          SFCTIM(MTIM),                                                    NUDGD.75
     7          TIMB(MVAR),TIME(MVAR),                                           NUDGD.76
     8      QSATF(MIXFG,MKXFG),SCR2D(MIXFG,MJXFG),BLDUM2D(MIXFG,MJXFG),          NUDGD.77
     9      BLPSOC(MIXFG,MJXFG),BLPSOD(MIXFG,MJXFG),BLWXY(MIXFG,MJXFG),          NUDGD.78
     1      BLWNV(MVAR,MIXFG,MJXFG),REGJ(MIX),                                   NUDGD.79
     2      ZNTJ(MIX)                                                            NUDGD.80
      INTEGER   INVAR(MAXSES,2),                                                 NUDGD.81
     +          KPBLT(MIX),                                                      NUDGD.82
     +          NTB(MVAR),NTE(MVAR),                                             NUDGD.83
     +          IQCHK(MTIM,MVAR)                                                 NUDGD.84
      REAL MSD(MIXFG,MJXFG)                                                      NUDGD.85
C                                                                                NUDGD.86
      INTEGER I,J,K,ICUT,IDC,II,JJ,KK                                            NUDGD.87
      INTEGER IFPST,ID                                                           NUDGD.88
      REAL    XTH                                                                NUDGD.89
      LOGICAL INITFLAG                                                           NUDGD.90
      real dxi2                                                                  23DEC04.2693
C ILX IS IL-1 AND ILXM IS IL-2                                                   NUDGD.91
C TEND IS NOT CALLED ON THE LATERAL BOUNDARIES ON DOT OR                         NUDGD.92
C CROSS POINTS.  THEREFORE, THE NUDGING TERM IS COMPUTED FOR                     NUDGD.93
C I=2,ILX FOR DOT POINTS (U AND V) AND FROM I=2,ILXM FOR                         NUDGD.94
C CROSS POINTS (T,Q,PSTAR).                                                      NUDGD.95
C                                                                                NUDGD.96
CCCCCCCCCCCCCCCCCCCCCCCCCCC                                                      NUDGD.97
      dxi2 = 0.5/dx                                                              23DEC04.2694
      IF ( INITFLAG ) THEN                                                       NUDGD.98
        IF (ID.EQ.1)THEN                                                         NUDGD.99
          IF(IPSTR(IN,2).EQ.1.AND.I4D(IN,2).EQ.1)THEN                            NUDGD.100
            CALL BLNUDGD(MTIM,MVAR,J,XB,XTEN,PSTF,PSTO,GX,ID,BLWXY,WXY2,         NUDGD.101
     +         BLWNV,WPBL,TFAC,ZFAC,GP,IVAR,IN,KPBLT,SFCOBS,SFCTIM,QSATF         NUDGD.102
     +         ,IQCHK,TIMB,TIME,NTB,NTE,SCR2D,BLDUM2D,BLPSOC,BLPSOD,REGJ         NUDGD.103
     +         ,ZNTJ,.TRUE.)                                                     NUDGD.104
            RETURN                                                               NUDGD.105
          ENDIF                                                                  NUDGD.106
        ENDIF                                                                    NUDGD.107
        IF(INVAR(IN,2).EQ.1.AND.I4D(IN,2).EQ.1) THEN                             NUDGD.108
          CALL BLNUDGD(MTIM,MVAR,                                                NUDGD.109
     +   J,XB,                                                                   NUDGD.110
     +   XTEN,PSTF,PSTO,GX,ID,BLWXY,WXY2,BLWNV,WPBL,TFAC,ZFAC,GP,IVAR,IN         NUDGD.111
     +  ,KPBLT,SFCOBS,SFCTIM,QSATF,IQCHK,TIMB,TIME,NTB,NTE,SCR2D,BLDUM2D         NUDGD.112
     +  ,BLPSOC,BLPSOD,REGJ,ZNTJ,.TRUE.)                                         NUDGD.113
        ENDIF                                                                    NUDGD.114
C IF INITFLAG IS TRUE, THIS IS ALL THAT GETS DONE                                NUDGD.115
        RETURN                                                                   NUDGD.116
      ENDIF                                                                      NUDGD.117
                                                                                 NUDGD.118
      ICUT=0                                                                     NUDGD.119
      IF(IN.GT.1)ICUT=1                                                          NUDGD.120
      IDC=0                                                                      NUDGD.121
      IF(IVAR.GT.2)IDC=1                                                         NUDGD.122
      IF(MOD(KTAU,NPF).EQ.0.AND.J.EQ.10)THEN                                     NUDGD.123
        PRINT3131,IN,IVAR,INT4(IN,2),J,KTAU,NPFV                                 NUDGD.124
3131  FORMAT(1H0,'IN NUDGD-TOP: IN,NV,INT4,J,KTAU: ',3(I1,1X),I2,1X,I4,          NUDGD.125
     1'- PRINTS IN THE VERTICAL ARE FOR K=KL,1,-NPFV: NPFV = ',I2)               NUDGD.126
        PRINT3132,KPBLT(10),(XTEN(10,10,KK),KK=KL,1,-NPFV)                       NUDGD.127
3132  FORMAT(1X,'IN NUDGD: KPBL,XTEN(KL,KL-NPFV...): ',                          NUDGD.128
     1I2,8(1X,G10.3))                                                            NUDGD.129
      ENDIF                                                                      NUDGD.130
      IF(ID.EQ.1)THEN                                                            NUDGD.131
C CALL BLNUDGD FOR P*:                                                           NUDGD.132
        IF(IPSTR(IN,2).EQ.1.AND.I4D(IN,2).EQ.1)THEN                              NUDGD.133
          CALL BLNUDGD(MTIM,MVAR,J,XB,XTEN,PSTF,PSTO,GX,ID,BLWXY,WXY2,           NUDGD.134
     +         BLWNV,WPBL,TFAC,ZFAC,GP,IVAR,IN,KPBLT,SFCOBS,SFCTIM,QSATF         NUDGD.135
     +         ,IQCHK,TIMB,TIME,NTB,NTE,SCR2D,BLDUM2D,BLPSOC,BLPSOD,REGJ         NUDGD.136
     +         ,ZNTJ,.FALSE.)                                                    NUDGD.137
          RETURN                                                                 NUDGD.138
        ELSE                                                                     NUDGD.139
          GOTO 10                                                                NUDGD.140
        ENDIF                                                                    NUDGD.141
      ENDIF                                                                      NUDGD.142
C J WILL NEVER EQUAL ONE IN V3                                                   NUDGD.143
C      IF(J.NE.1)THEN                                                            NUDGD.144
        IF((INVAR(IN,2).EQ.1.AND.I4D(IN,2).EQ.1).OR.INONBL(IN,IVAR).EQ.1         NUDGD.145
     +    )THEN                                                                  NUDGD.146
C COMPUTE WPBL(I,K), THE WEIGHT USED TO RESTRICT NUDGING                         NUDGD.147
C 3-D RAWINS ANALYSES IN THE MODEL-SIMULATED BOUNDARY LAYER                      NUDGD.148
          DO 150 I=2,ILX-IDC                                                     NUDGD.149
            IF(KPBLT(I).GT.KL.OR.KPBLT(I).LE.2)THEN                              NUDGD.150
              PRINT200,KPBLT(I),I,J                                              NUDGD.151
200   FORMAT(1X,'*** STOP200 IN NUDGD: KPBLT = ',I2,' AT I = ',I3,               NUDGD.152
     1' AND J = ',I3,' ***')                                                     NUDGD.153
              STOP 200                                                           NUDGD.154
            ENDIF                                                                NUDGD.155
  150     CONTINUE                                                               NUDGD.156
C AVOID PERFORMING IVAR NUDGING WITHIN THE PBL WHEN INONBL(IN,IVAR)=1,           NUDGD.157
C OR WHEN I4D(IN,2)=1 (NUDGING SURFACE DATA IN THE BOUNDARY LAYER).              NUDGD.158
C USE 0.0 WEIGHT WITHIN THE BOUNDARY LAYER, 0.1 WEIGHT ONE                       NUDGD.159
C LAYER ABOVE KPBLT AND FULL NUDGING TWO OR MORE LAYERS ABOVE KPBLT              NUDGD.160
C                                                                                NUDGD.161
C NOTE: WPBL IS PASSED INTO BLNUDGD WHERE (1.-WPBL) IS USED TO                   NUDGD.162
C DETERMINE THE NUDGING STRENGTH WHEN NUDGING SURFACE ANALYSES OF                NUDGD.163
C U, V, T, OR QV WITHIN THE BOUNDARY LAYER.                                      NUDGD.164
C                                                                                NUDGD.165
          DO 201 I=2,ILX-IDC                                                     NUDGD.166
            DO 201 K=KL,KPBLT(I),-1                                              NUDGD.167
              WPBL(I,K)=0.0                                                      NUDGD.168
  201       CONTINUE                                                             NUDGD.169
                                                                                 NUDGD.170
          DO 202 I=2,ILX-IDC                                                     NUDGD.171
            DO 202 K=1,KPBLT(I)-2                                                NUDGD.172
              WPBL(I,K)=1.0                                                      NUDGD.173
  202       CONTINUE                                                             NUDGD.174
                                                                                 NUDGD.175
          DO 203 I=2,ILX-IDC                                                     NUDGD.176
            WPBL(I,KPBLT(I)-1)=0.1                                               NUDGD.177
  203     CONTINUE                                                               NUDGD.178
        ELSE                                                                     NUDGD.179
          DO 301 K=1,KL                                                          NUDGD.180
            DO 301 I=2,ILX-IDC                                                   NUDGD.181
              WPBL(I,K)=1.0                                                      NUDGD.182
  301       CONTINUE                                                             NUDGD.183
                                                                                 NUDGD.184
        ENDIF                                                                    NUDGD.185
C      ENDIF                                                                     NUDGD.186
      IF(J.EQ.10.AND.MOD(KTAU,NPF).EQ.0)THEN                                     NUDGD.187
        PRINT500,KPBLT(10),(WPBL(10,KK),KK=KL,1,-NPFV)                           NUDGD.188
500   FORMAT(1X,'IN NUDGD: KPBL,WPBL(KL,KL-NPFV...): ',                          NUDGD.189
     1I2,8(1X,F6.2))                                                             NUDGD.190
      ENDIF                                                                      NUDGD.191
C NUDGING SURFACE ANALYSES OF INVAR IN THE BOUNDARY LAYER                        NUDGD.192
      IF(INVAR(IN,2).EQ.1.AND.I4D(IN,2).EQ.1)CALL BLNUDGD(MTIM,MVAR,J,XB         NUDGD.193
     +  ,XTEN,PSTF,PSTO,GX,ID,BLWXY,WXY2,BLWNV,WPBL,TFAC,ZFAC,GP,IVAR,IN         NUDGD.194
     +  ,KPBLT,SFCOBS,SFCTIM,QSATF,IQCHK,TIMB,TIME,NTB,NTE,SCR2D,BLDUM2D         NUDGD.195
     +  ,BLPSOC,BLPSOD,REGJ,ZNTJ,.FALSE.)                                        NUDGD.196
C IF NUDGING P* IN BLNUDGD, DO NOT NUDGE IT HERE (SET IFPST=0)                   NUDGD.197
      IF((INVAR(IN,2).EQ.1.AND.I4D(IN,2).EQ.1))THEN                              NUDGD.198
        IFPST=0                                                                  NUDGD.199
      ELSE                                                                       NUDGD.200
        IFPST=IPSTR(IN,2)                                                        NUDGD.201
      ENDIF                                                                      NUDGD.202
C TEMPORARY...                                                                   NUDGD.203
      IF(IFPST.NE.0)STOP 1                                                       NUDGD.204
C TEMPORARY                                                                      NUDGD.205
C VORTICITY NUDGING...                                                           NUDGD.206
      IF(ID.EQ.2)GOTO 30                                                         NUDGD.207
C NUDGING MOISTURE USING OBSERVED PRECIP...                                      NUDGD.208
      IF(ID.EQ.3)GOTO 15                                                         NUDGD.209
C FOR NORMAL NUDGING OF THE 3-D RAWINS ANALYSES OF U, V, T, OR Q...              NUDGD.210
      DO 1 K=1,KL                                                                NUDGD.211
        DO 1 I=2+ICUT,ILX-IDC-ICUT                                               NUDGD.212
CB          XTEN(I,J,K)=XTEN(I,J,K)+GX(IN,1)*TFAC(IN,1)*PSTF(I,J)*(              23DEC04.2695
CB     +                XOBJK(I,K)-XB(I,J,K)/PSTF(I,J))*WXY(I,J)*                23DEC04.2696
          XTEN(I,J,K)=XTEN(I,J,K)+GX(IN,1)*TFAC(IN,1)*(PSTF(I,J)*                23DEC04.2697
     +                XOBJK(I,K)-XB(I,J,K))*WXY(I,J)*                            23DEC04.2698
     +                ZFAC(IN,1,K)*WPBL(I,K)+GP(IN,1)*FLOAT(IFPST)*              NUDGD.215
     +                XB(I,J,K)/PSTF(I,J)*(PSTO(I,J)-PSTF(I,J))*                 NUDGD.216
     +                TFAC(IN,1)*ZFAC(IN,1,K)*WXY(I,J)                           NUDGD.217
    1   CONTINUE                                                                 NUDGD.218
                                                                                 NUDGD.219
      GOTO 4645                                                                  NUDGD.220
   15 CONTINUE                                                                   NUDGD.221
C FOR MOISTURE NUDGING, USE WEIGHT COMPUTED IN SETUPGD WHICH TAKES               NUDGD.222
C INTO ACCOUNT THE OBSERVED PRECIP DATA (WXY2 IS WQ OR WQN)                      NUDGD.223
C TO NUDGE RAWINS MOISTURE ANALYSES                                              NUDGD.224
      DO 2 K=1,KL                                                                NUDGD.225
        DO 2 I=2+ICUT,ILX-IDC-ICUT                                               NUDGD.226
CB          XTEN(I,J,K)=XTEN(I,J,K)+GX(IN,1)*TFAC(IN,1)*PSTF(I,J)*(              23DEC04.2699
CB     +                XOBJK(I,K)-XB(I,J,K)/PSTF(I,J))*WXY2(I,J)*               23DEC04.2700
          XTEN(I,J,K)=XTEN(I,J,K)+GX(IN,1)*TFAC(IN,1)*(PSTF(I,J)*                23DEC04.2701
     +                XOBJK(I,K)-XB(I,J,K))*WXY2(I,J)*                           23DEC04.2702
     +                ZFAC(IN,1,K)*WPBL(I,K)+GP(IN,1)*FLOAT(IFPST)*              NUDGD.229
     +                XB(I,J,K)/PSTF(I,J)*(PSTO(I,J)-PSTF(I,J))*                 NUDGD.230
     +                TFAC(IN,1)*ZFAC(IN,1,K)*WXY(I,J)                           NUDGD.231
    2   CONTINUE                                                                 NUDGD.232
                                                                                 NUDGD.233
 4645 IF(MOD(KTAU,NPF).EQ.0.AND.J.EQ.10)GOTO 4646                                NUDGD.234
      GOTO 4747                                                                  NUDGD.235
 4646 CONTINUE                                                                   NUDGD.236
      IF(ID.EQ.3)THEN                                                            NUDGD.237
        XTH=KTAU*120./3600.                                                      NUDGD.238
        PRINT3133                                                                NUDGD.239
3133  FORMAT(1X,' WQ IN NUDGD: --------------------')                            NUDGD.240
        CALL MAPSMP(WXY2,MIXFG,MJXFG,1,ILX,1,1,JLX,1,0.,1,8HWXY2(WQ),XTH         NUDGD.241
     +       )                                                                   NUDGD.242
      ENDIF                                                                      NUDGD.243
      PRINT3333,TFAC(IN,1),ZFAC(IN,1,KL),IFPST,GX(IN,1),WXY(10,10),              NUDGD.244
     +          XB(10,J,KL),XTEN(10,10,KL),PSTF(10,J),PSTO(10,J)                 NUDGD.245
3333  FORMAT(1X,'IN NUDGD: TF,ZF,IFPS,',                                         NUDGD.246
     1'GX,WXY,XB,XTN,PF,PO: ',2F5.2,1X,I2,1X,4G10.3,2(1X,F5.1))                  NUDGD.247
      PRINT3130,KPBLT(10),(XTEN(10,10,KK),KK=KL,1,-NPFV)                         NUDGD.248
3130  FORMAT(1X,'IN NUDGD-BOT: KPBL,XTEN(KL,KL-NPFV...): ',                      NUDGD.249
     1I2,8(1X,G10.3))                                                            NUDGD.250
 4747 CONTINUE                                                                   NUDGD.251
      RETURN                                                                     NUDGD.252
C                                                                                NUDGD.253
C COMPUTE THE NUDGING TERM IN THE THE CONT. (PTEN) EQ.                           NUDGD.254
C                                                                                NUDGD.255
   10 CONTINUE                                                                   NUDGD.256
      DO 20 I=2,ILXM                                                             NUDGD.257
C       PTEN(I)=PTEN(I)+GP(IN,1)*TFAC(IN,1)*WXY(I,J)*(PSTO(I,J)-                 NUDGD.258
C    +          PSTF(I,J))                                                       NUDGD.259
   20 CONTINUE                                                                   NUDGD.260
      RETURN                                                                     NUDGD.261
   30 CONTINUE                                                                   NUDGD.262
C COMPUTE THE ROTATIONAL NUDGING TERM FOR U OR V                                 NUDGD.263
C U COMP...                                                                      NUDGD.264
108   FORMAT(1X,'IN NUDGD: IVAR,I,J,K,ZETDIF,GR,DX: ',4I5,3E15.5)                NUDGD.265
      IF(IVAR.EQ.1)THEN                                                          NUDGD.266
        DO 40 K=1,KL                                                             NUDGD.267
          DO 40 I=2+ICUT,ILX-IDC-ICUT                                            NUDGD.268
            XTEN(I,J,K)=XTEN(I,J,K)+GR(IN)*TFAC(IN,1)*PSTF(I,J)*MSD(I,J)         NUDGD.269
     +                  *(VORDIF(I,J,K)+VORDIF(I,J-1,K)-VORDIF(I-1,J,K)-         NUDGD.270
CB     +                  VORDIF(I-1,J-1,K))*0.5/DX*WXY(I,J)*ZFAC(IN,1,K)+       23DEC04.2703
     +                  VORDIF(I-1,J-1,K))*DXi2*WXY(I,J)*ZFAC(IN,1,K)+           23DEC04.2704
     +                  GP(IN,1)*FLOAT(IPSTR(IN,1))*XB(I,J,K)/PSTF(I,J)*         NUDGD.272
     +                  (PSTO(I,J)-PSTF(I,J))*TFAC(IN,1)*ZFAC(IN,1,K)*           NUDGD.273
     +                  WXY(I,J)                                                 NUDGD.274
   40     CONTINUE                                                               NUDGD.275
                                                                                 NUDGD.276
      ENDIF                                                                      NUDGD.277
      IF(IVAR.EQ.2)THEN                                                          NUDGD.278
        DO 50 K=1,KL                                                             NUDGD.279
          DO 50 I=2+ICUT,ILX-IDC-ICUT                                            NUDGD.280
            XTEN(I,J,K)=XTEN(I,J,K)-GR(IN)*TFAC(IN,1)*PSTF(I,J)*MSD(I,J)         NUDGD.281
     +                  *(VORDIF(I,J,K)+VORDIF(I-1,J,K)-VORDIF(I,J-1,K)-         NUDGD.282
CB     +                  VORDIF(I-1,J-1,K))*0.5/DX*WXY(I,J)*ZFAC(IN,1,K)+       23DEC04.2705
     +                  VORDIF(I-1,J-1,K))*DXi2*WXY(I,J)*ZFAC(IN,1,K)+           23DEC04.2706
     +                  GP(IN,1)*FLOAT(IPSTR(IN,1))*XB(I,J,K)/PSTF(I,J)*         NUDGD.284
     +                  (PSTO(I,J)-PSTF(I,J))*TFAC(IN,1)*ZFAC(IN,1,K)*           NUDGD.285
     +                  WXY(I,J)                                                 NUDGD.286
   50     CONTINUE                                                               NUDGD.287
                                                                                 NUDGD.288
      ENDIF                                                                      NUDGD.289
      IF(J.EQ.2.AND.MOD(KTAU,NPF).EQ.0)PRINT3335,IN,J,KTAU,TFAC(IN,1),           NUDGD.290
     +  VORDIF(2,J,2),GX(IN,1),GR(IN),XB(2,J,2),XTEN(2,2,2),PSTF(2,J),           NUDGD.291
     +  PSTO(2,J)                                                                NUDGD.292
3335  FORMAT(1X,'IN NUDGD: IN,J,TAU,TF,VORDIF,',                                 NUDGD.293
     1'GX,GR,XB,XTN,PF,PO',3I3,1X,F5.3,1X,7G10.3)                                NUDGD.294
      RETURN                                                                     NUDGD.295
      END                                                                        NUDGD.296
                                                                                 NUDGD.297
