      SUBROUTINE BLNUDGD(MTIM,MVAR,J,XB,XTEN,                                    BLNUDGD.1
     1PSTF,PSTO,GX,ID,WXY,WXY2,BLWNV,WPBL,TFAC,ZFAC,                             BLNUDGD.2
     2GP,IVAR,IN,KPBLT,SFCOBS,SFCTIM,QSATF,IQCHK,TIMB,TIME,NTB,NTE,              BLNUDGD.3
     3SCR2D,BLDUM2D,BLPSOC,BLPSOD,REGJ,ZNTJ,                                     BLNUDGD.4
     4INITFLAG)                                                                  BLNUDGD.5
      IMPLICIT NONE                                                              BLNUDGD.6
C                                                                                BLNUDGD.7
C                                                                                BLNUDGD.8
C THIS SUBROUTINE COMPUTES THE NUDGING TERM, WITHIN THE                          BLNUDGD.9
C PLANETARY BOUNDARY LAYER (K=KL,KPBLT,-1), OF THE TENDENCY                      BLNUDGD.10
C EQUATION (XTEN) FOR XB, WHERE XB CAN BE UB,VB,TB,QB, OR PSB.                   BLNUDGD.11
C GX IS THE NUDGING COEFFICIENT FOR XB AND GP IS THAT FOR                        BLNUDGD.12
C PSB.  PSTF AND PSTO ARE THE FORECAST AND OBSERVED 12-HLY SFC                   BLNUDGD.13
C PRESSURES IN CB AT TAU-1. PSTF AND PSTO ARE ON DOT POINTS                      BLNUDGD.14
C FOR THE WINDS; OTHERWISE, THEY ARE ON CROSS POINTS.                            BLNUDGD.15
C BLPOC AND BLPOD ARE PSOC AND PSOD FROM SURFACE DATA,                           BLNUDGD.16
C AND ARE BUFFERED INTO BLPS1D EACH TIME STEP IF IPSTR(IN)=1                     BLNUDGD.17
C XB IS THE 3-D ARRAY OF THE MODEL VARIABLE AT                                   BLNUDGD.18
C TAU-1, AND SFCOBS IS THE 4-D ARRAY OF SURFACE OBS                              BLNUDGD.19
C AT THE VARIOUS TIME LEVELS WITHIN THE DIFTIM INTERVAL                          BLNUDGD.20
C SET IN PARAM (USUALLY 720. MINUTES) FOR THE 3-D OBS.                           BLNUDGD.21
C IDC=0 IF NUDGING A DOT POINT FIELD (IVAR.LE.2), AND ID=1 WHEN                  BLNUDGD.22
C PSB IS NUDGED IN THE CONTINUITY EQ.  WXY IS BLWDT OR BLWCS                     BLNUDGD.23
C FOR THE FINE OR COARSE MESH, DEPENDING ON WHETHER A DOT                        BLNUDGD.24
C OR CROSS POINT FIELD IS BEING NUDGED.  WXY2 IS EITHER                          BLNUDGD.25
C BLWCS OR BLWCSN, OR WQ OR WQN, DEPENDING ON IN AND IMOISP(IN).                 BLNUDGD.26
C FOR CROSS POINTS, WXY IS BLWCS. ISTORE CONTAINS IL,JL,ETC. AND                 BLNUDGD.27
C PSTORE CONTAINS XTIME ETC. AND ARE AUTOMATICALLY ISTORN AND                    BLNUDGD.28
C PSTORN WHEN IN (DEFINED HERE AS INEST+2)=4 FOR THE FINE MESH AND IN=3          BLNUDGD.29
C FOR THE COARSE MESH.  XB IS DECOUPLED HERE, AND SFCOBS ARE NOT                 BLNUDGD.30
C COUPLED WITH PSB.  ZFAC AND TFAC ARE THE VERTICAL                              BLNUDGD.31
C SPACE FACTOR AND THE TIME FACTOR DEFINED IN SETUPGD AND                        BLNUDGD.32
C OBTAINED FROM COMMON BLOCK FDDA. WPBL IS DEFINED IN NUDGD                      BLNUDGD.33
C   IF INT4=1 (TEMPORAL INTERPOLATION), THEN BLNUDGD WILL INTERPOLATE            BLNUDGD.34
C THE VALUES IN TIME, IF INT4=0, THEN THE NUDGING IS DONE TOWARD A               BLNUDGD.35
C TARGET VALUE (WITH A TIME FACTOR OF 1 FOR NOW).                                BLNUDGD.36
C THIS SUBROUTINE IS NOT CURRENTLY DESIGNED FOR VORTICITY NUDGING                BLNUDGD.37
C                                                                                BLNUDGD.38
C ID=0 NORMAL COMPUTATION OF NUDGING TERM FOR U,V,T,Q                            BLNUDGD.39
C ID=1 COMPUTATION OF NUDGING TERM IN CONTINUITY (PTEN) EQ.                      BLNUDGD.40
C ID=2 VORTICITY NUDGING (NOT CURRENTLY AVAILABLE FOR SURFACE DATA)              BLNUDGD.41
C ID=3 MOISTURE NUDGING USING PRECIP DATA                                        BLNUDGD.42
C                                                                                BLNUDGD.43
C NOTE: P* CAN BE NUDGED IN THE CONT EQ. OR IN THE SECOND TERM                   BLNUDGD.44
C       OF THE NUDGING FORMULATION ONLY ONCE IN EITHER NUDGD                     BLNUDGD.45
C       OR BLNUDGD, ELSE YOU ARE DOUBLING THE TERM.                              BLNUDGD.46
C       IT IS ASSUMED THAT IF I4D(INP2)=1 AND IPSTR(IN) OR                       BLNUDGD.47
C       IPSTR(INP2)=1 THEN THE CONTRIBUTION TO THE NUDGING                       BLNUDGD.48
C       TERM FROM OBS P* WILL BE COMPUTED IN BLNUDGD:                            BLNUDGD.49
C          IF IPSTR(INP2)=1  SURFACE PRESSURE FROM BL FILE                       BLNUDGD.50
C                            IS USED FOR PSTO IN BLNUDGD                         BLNUDGD.51
C          IF IPSTR(INP2)=0  SURFACE PRESSURE FROM 12-HLY DATA                   BLNUDGD.52
C                            IS USED FOR PSTO IN BLNUDGD                         BLNUDGD.53
C       IF I4D(INP2)=0 AND IPSTR(IN)=1 THEN THE CONTRIBUTION FROM                BLNUDGD.54
C       OBS P* WILL BE COMPUTED IN NUDGD AS USUAL.                               BLNUDGD.55
C                 (THIS CODE HAS NOT BEEN TESTED)                                BLNUDGD.56
C                                                                                BLNUDGD.57
CCC NOTE: PARAMETER "IN" HERE IN BLNUDGD IS INEST+2                              BLNUDGD.58
C                                                                                BLNUDGD.59
C IBLNOP(IN)=0 CORRECTION TERM IN THE BL IS CONSTANT WITH K FOR NV               BLNUDGD.60
C   CORR(K)=XOBBL(KL)-XB(KL)                                                     BLNUDGD.61
C IBLNOP(IN)=1 CORRECTION TERM IN THE BL VARIES WITH K                           BLNUDGD.62
C   CORR(K)=XOBBL(KL)-XB(K)                                                      BLNUDGD.63
C                                                                                BLNUDGD.64
C KEEP IN MIND THAT ZFAC(IN,K), DEFINED TO BE CONSTANT-IN-TIME IN                BLNUDGD.65
C SUBROUTINE SETUPGD, OPERATES IN COMBINATION WITH WPBL=F(KPBLT).                BLNUDGD.66
C ZFAC, OF COURSE, CAN BE DEFINED LOCALLY BASED ON PBL DEPTH, ETC., AND          BLNUDGD.67
C USED AS AN ADDITIONAL MEANS TO VARY THE NUDGING IN THE VERTICAL.               BLNUDGD.68
C OTHERWISE, BY DEFAULT, ZFAC IS CONSTANT IN TIME AND MUST BE DEFINED            BLNUDGD.69
C WHEN INDZ(INEST+2)=1 IN SUBROUTINE SETUPGD.                                    BLNUDGD.70
#     include <parame.incl>                                                      BLNUDGD.71
#     include <param2.incl>                                                      BLNUDGD.72
#     include <parfdda.incl>                                                     BLNUDGD.73
#     include <various.incl>                                                     BLNUDGD.74
      INTEGER MTIM,MVAR,IN                                                       BLNUDGD.75
      REAL   XTEN(MIX,MJX,MKX),XB(MIX,MJX,MKX),XOBBL(MIXFG,MKXFG),               BLNUDGD.76
     +       PSTF(MIXFG,MJXFG),PSTO(MIXFG,MJXFG),                                BLNUDGD.77
     +       WXY(MIXFG,MJXFG),WXY2(MIXFG,MJXFG),GX(MAXSES,2),                    BLNUDGD.78
     +       TFAC(MAXNES,2),ZFAC(MAXNES,2,MKX),GP(MAXSES,2),                     BLNUDGD.79
     +       WPBL(MIX,MKX),                                                      BLNUDGD.80
     +       SFCOBS(MTIM,MVAR,MIXFG,MJXFG),                                      BLNUDGD.81
     +       SFCTIM(MTIM),                                                       BLNUDGD.82
     +       TIMB(MVAR),TIME(MVAR),                                              BLNUDGD.83
     +       QSATF(MIXFG,MKXFG),SCR2D(MIXFG,MJXFG),BLDUM2D(MIXFG,MJXFG),         BLNUDGD.84
     +       BLPSOC(MIXFG,MJXFG),BLPSOD(MIXFG,MJXFG),BLPS1D(MIXFG),              BLNUDGD.85
     +       BLWNV(MVAR,MIXFG,MJXFG),BLW1D(MIXFG),                               BLNUDGD.86
     +       REGJ(MIX),ZNTJ(MIX)                                                 BLNUDGD.87
      INTEGER                                                                    BLNUDGD.88
     +       KPBLT(MIX),                                                         BLNUDGD.89
     +       IQCHK(MTIM,MVAR),                                                   BLNUDGD.90
     +       NTB(MVAR),NTE(MVAR)                                                 BLNUDGD.91
      LOGICAL INITFLAG                                                           BLNUDGD.92
      INTEGER ICUT,IDC,IVAR,NSS,IFPST,ID                                         BLNUDGD.93
      INTEGER I,J,K,KK,NV,MV,II,JJ                                               BLNUDGD.94
      REAL   BLFDT,WNDCORP,WNDCORL,WNDCOR,XTH                                    BLNUDGD.95
#ifdef IBMopt                                                                    23DEC04.2591
      integer is,nn                                                              23DEC04.2592
      real temp1(mix),tim0                                                       23DEC04.2593
#ifdef MPP1                                                                      23DEC04.2594
#define nf(A) noflic(A)
#else                                                                            23DEC04.2596
#define nf(A) A       
#endif                                                                           23DEC04.2598
#endif                                                                           23DEC04.2599
C AS ABOVE, "IN" IN BLNUDGD IS INEST+2                                           BLNUDGD.96
C ILX IS IL-1 AND ILXM IS IL-2                                                   BLNUDGD.97
C TEND IS NOT CALLED ON THE LATERAL BOUNDARIES ON DOT OR                         BLNUDGD.98
C CROSS POINTS.  THEREFORE, THE NUDGING TERM IS COMPUTED FOR                     BLNUDGD.99
C I=2,ILX FOR DOT POINTS (U AND V) AND FROM I=2,ILXM FOR                         BLNUDGD.100
C CROSS POINTS (T,Q,PSTAR).                                                      BLNUDGD.101
C                                                                                BLNUDGD.102
C IF INDXY(IN)=1, THEN THE HORIZONTAL WEIGHT IS DETERMINED INDEPENDENTLY         BLNUDGD.103
C FROM THE SURFACE DATA DENSITY ARRAY WRITTEN IN THE SURFACE RAWINS FILE         BLNUDGD.104
C THE 1-D ARRAY IS THEN EQUAL TO WXY FOR THAT N-S SLICE.                         BLNUDGD.105
C IF INDXY(IN)=2, THEN THE HORIZONTAL WEIGHT IS TEMPORALLY INTERPOLATED          BLNUDGD.106
C FROM A SET OF WEIGHTS BASED ON THE FIELD READ INTO SFCOBS(NT,8,I,J)            BLNUDGD.107
C FROM THE SURFACE RAWINS TAPE.  IN GENERAL, THIS                                BLNUDGD.108
C WEIGHT AT ANY GIVEN TIME CAN BE DIFFERENT FOR EACH VARIABLE TYPE               BLNUDGD.109
C SINCE THE CODE CAN BE INTERPOLATING OBS T OVER 9 H, OBS WIND                   BLNUDGD.110
C OVER 3 H, ETC. FOR NOW, WHEN NUDGING PSTAR, THE WEIGHT USED IN THE             BLNUDGD.111
C SECOND TERM OF THE NUDGING EQUATION IS INTERPOLATED FROM TIMB AND TIME         BLNUDGD.112
C OF IVAR ( THE VARIABLE BEING NUDGED ), WHILE THAT USED IN THE CONTINUI         BLNUDGD.113
C EQUATION IS BASED ON TIMB AND TIME FOR PSTAR.                                  BLNUDGD.114
      ICUT=0                                                                     BLNUDGD.115
      IF(IN.GT.1)ICUT=1                                                          BLNUDGD.116
      IDC=0                                                                      BLNUDGD.117
      IF(IVAR.GT.2)IDC=1                                                         BLNUDGD.118
      IF(MOD(KTAU,NPF).EQ.0.AND.J.EQ.10)THEN                                     BLNUDGD.119
        PRINT500,IN,IVAR,INT4(IN,2),J,KTAU,NPFV                                  BLNUDGD.120
500   FORMAT(1H0,'IN BLNUDGD-TOP: IN,NV,INT4,J,KTAU: ',3(I1,1X),I2,1X,           BLNUDGD.121
     1I4,'- PRINTS IN THE VERTICAL ARE FOR K=KL,1,-NPFV: NPFV = ',I2)            BLNUDGD.122
        PRINT501,KPBLT(10),(WPBL(10,KK),KK=KL,1,-NPFV)                           BLNUDGD.123
501   FORMAT(1X,'IN BLNUDGD: KPBL,WPBL(KL,KL-NPFV...): ',                        BLNUDGD.124
     1I2,8(1X,F6.2))                                                             BLNUDGD.125
      ENDIF                                                                      BLNUDGD.126
#ifdef IBMopt                                                                    23DEC04.2600
#ifdef MPP1                                                                      23DEC04.2601
      nn = ie_x0(ILX-IDC-ICUT) - is_x0(2+ICUT) + 1                               23DEC04.2602
      is = is_x0(2+ICUT)                                                         23DEC04.2603
#else                                                                            23DEC04.2604
      nn = ILX -IDC-ICUT                                                         23DEC04.2605
      is = 2+ICUT                                                                23DEC04.2606
#endif                                                                           23DEC04.2607
#endif                                                                           23DEC04.2608
C                                                                                BLNUDGD.127
C OBTAIN THE OBSERVED ANALYSIS FOR IVAR TO BE NUDGED IN THE BL                   BLNUDGD.128
C     GO TO 300                                                                  BLNUDGD.129
C                                                                                BLNUDGD.130
      NV=IVAR                                                                    BLNUDGD.131
      IF (.NOT.INITFLAG) GOTO 300                                                BLNUDGD.132
      IF(NV.EQ.5)GOTO 290                                                        BLNUDGD.133
CCC                                                                              BLNUDGD.134
      IF((XTIME.GE.TIME(NV).AND.ABS(XTIME-TIME(NV)).LT.DTMIN.AND.                BLNUDGD.135
     +  IEOD(IN).EQ.0).OR.(IFREST.AND.KTAU.EQ.KTAUR.AND.IEOD(IN).EQ.0))          BLNUDGD.136
     +  THEN                                                                     BLNUDGD.137
        CALL BLBRGD(NV,MVAR,MTIM,XTIME,SFCTIM,TIMB,TIME,NTB,NTE,IQCHK,IN         BLNUDGD.138
     +       ,KTAU,KTAUR,IFREST)                                                 BLNUDGD.139
C                                                                                BLNUDGD.140
C NOW CHECK TO SEE IF XTIME WAS BRACKETED WITH TWO QUALITY DATA TIMES            BLNUDGD.141
C FOR NV = IVAR                                                                  BLNUDGD.142
C                                                                                BLNUDGD.143
        PRINT12,NV,XTIME,TIMB(NV),TIME(NV),NTB(NV),NTE(NV)                       BLNUDGD.144
12    FORMAT(1X,'IN BLNUDGD: NV,XTIME,TIMB,TIME,NTB,NTE ',I2,1X,                 BLNUDGD.145
     13(1X,F7.2),2(1X,I2))                                                       BLNUDGD.146
        IF(TIMB(NV).LT.-900..OR.TIME(NV).LT.-900.)STOP 1234                      BLNUDGD.147
        IF(INDXY(IN,2).NE.2)GOTO 290                                             BLNUDGD.148
      ENDIF                                                                      BLNUDGD.149
C                                                                                BLNUDGD.150
C THE BL WEIGHTING ARRAY USED FOR NV (IVAR) WILL BE INTERPOLATED                 BLNUDGD.151
C BETWEEN TIMB AND TIME FOR NV (IVAR) RATHER THAN CALLING BLBRGD                 BLNUDGD.152
C TO DETERMINE THEM SPECIFICALLY FOR IVAR=8.                                     BLNUDGD.153
C     IF(INDXY(IN,2).EQ.2) THEN                                                  BLNUDGD.154
C     CALL BLBRGD(NV,MVAR,MTIM,XTIME,SFCTIM,TIMB,TIME,NTB,NTE,                   BLNUDGD.155
C    1IQCHK,IN,KTAU,KTAUR,IFREST)                                                BLNUDGD.156
C                                                                                BLNUDGD.157
C NOW CHECK TO SEE IF XTIME WAS BRACKETED WITH TWO QUALITY DATA TIMES            BLNUDGD.158
C FOR NV = 8 (THE BL SURFACE DATA DENSITY ARRAY)                                 BLNUDGD.159
C                                                                                BLNUDGD.160
C     MV=8                                                                       BLNUDGD.161
C     PRINT 12,MV,XTIME,TIMB(MV),TIME(MV),NTB(MV),NTE(MV)                        BLNUDGD.162
C     IF(TIMB(8).LT.-900..OR.TIME(8).LT.-900.) STOP8                             BLNUDGD.163
C     ENDIF                                                                      BLNUDGD.164
CCC                                                                              BLNUDGD.165
  290 CONTINUE                                                                   BLNUDGD.166
      IF((IPSTR(IN,2).EQ.1.AND.XTIME.GE.TIME(5).AND.ABS(XTIME-TIME(5))           BLNUDGD.167
     +  .LT.DTMIN.AND.IEOD(IN).EQ.0).OR.(IPSTR(IN,2).EQ.1.AND.IFREST             BLNUDGD.168
     +  .AND.KTAU.EQ.KTAUR.AND.IEOD(IN).EQ.0))THEN                               BLNUDGD.169
        CALL BLBRGD(5,MVAR,MTIM,XTIME,SFCTIM,TIMB,TIME,NTB,NTE,IQCHK,IN,         BLNUDGD.170
     +       KTAU,KTAUR,IFREST)                                                  BLNUDGD.171
C                                                                                BLNUDGD.172
C NOW CHECK TO SEE IF XTIME WAS BRACKETED WITH TWO QUALITY DATA TIMES            BLNUDGD.173
C FOR NV = 5 (THE BL SURFACE PRESSURE FIELD) WHEN IVAR.NE.5                      BLNUDGD.174
C                                                                                BLNUDGD.175
        MV=5                                                                     BLNUDGD.176
        PRINT12,MV,XTIME,TIMB(MV),TIME(MV),NTB(MV),NTE(MV)                       BLNUDGD.177
        IF(TIMB(5).LT.-900..OR.TIME(5).LT.-900.)STOP 5                           BLNUDGD.178
      ENDIF                                                                      BLNUDGD.179
      BLFDT=XTIME-DTMIN-TIMB(NV)                                                 BLNUDGD.180
      BLFDT=AMAX1(BLFDT,0.)                                                      BLNUDGD.181
CCC                                                                              BLNUDGD.182
      IF(IPSTR(IN,2).EQ.1)THEN                                                   BLNUDGD.183
C FOR NUDGING PSTAR OR FOR TERM 2 IN THE BL THE NUDGING FORMULATION              BLNUDGD.184
C NEEDS THE OBSERVED BL PSTAR ON DOT OR CROSS POINTS.  PSTF                      BLNUDGD.185
C IS ALREADY ON DOT OR CROSS POINTS WHEN NUDGD CALLS BLNUDGD                     BLNUDGD.186
C CALCULATE BLPSTO AND THEN INTERPOLATE TO DOT POINTS IF NECESSARY               BLNUDGD.187
C STORE BL PSOTEN IN SCR2D                                                       BLNUDGD.188
C STORE INITIAL PSTO IN BLDUM2D                                                  BLNUDGD.189
#ifdef IBMopt                                                                    23DEC04.2609
        tim0 = 1./(TIME(5)-TIMB(5))                                              23DEC04.2610
#endif                                                                           23DEC04.2611
        DO 400 II=1,ILX                                                          BLNUDGD.190
          DO 400 JJ=1,JLX                                                        BLNUDGD.191
            SCR2D(II,JJ)=(SFCOBS(NTE(5),5,II,JJ)-SFCOBS(NTB(5),5,II,JJ))         BLNUDGD.192
#ifndef IBMopt                                                                   23DEC04.2612
     +                   /(TIME(5)-TIMB(5))                                      BLNUDGD.193
#else                                                                            23DEC04.2613
     +                   * tim0                                                  23DEC04.2614
#endif                                                                           23DEC04.2615
            BLDUM2D(II,JJ)=SFCOBS(NTB(5),5,II,JJ)                                BLNUDGD.194
  400     CONTINUE                                                               BLNUDGD.195
                                                                                 BLNUDGD.196
        IF(INT4(IN,2).NE.1)THEN                                                  BLNUDGD.197
          DO 405 I=1,ILX                                                         BLNUDGD.198
            DO 405 JJ=1,JLX                                                      BLNUDGD.199
              BLDUM2D(I,JJ)=SFCOBS(NTE(5),5,I,JJ)                                BLNUDGD.200
  405       CONTINUE                                                             BLNUDGD.201
                                                                                 BLNUDGD.202
        ENDIF                                                                    BLNUDGD.203
C INTERPOLATE IN TIME IF NECESSARY AND TO DOT POINTS                             BLNUDGD.204
        CALL INTPSGD(BLDUM2D,SCR2D,BLPSOC,BLPSOD,BLFDT,IN)                       BLNUDGD.205
      ENDIF                                                                      BLNUDGD.206
C RESULT IS BLPSOD AND BLPSOC                                                    BLNUDGD.207
C                                                                                BLNUDGD.208
CCC                                                                              BLNUDGD.209
      IF(INT4(IN,2).EQ.1.AND.INDXY(IN,2).EQ.2)THEN                               BLNUDGD.210
C CALCULATE BLWNV BETWEEN TIMB AND TIME FOR NV (=IVAR) AND INTERPOLATE T         BLNUDGD.211
C POINTS IF NECESSARY                                                            BLNUDGD.212
C STORE BL WXYTEN IN SCR2D                                                       BLNUDGD.213
#ifdef IBMopt                                                                    23DEC04.2616
        tim0 = 1./(TIME(NV)-TIMB(NV))                                            23DEC04.2617
#endif                                                                           23DEC04.2618
        DO 450 II=1,ILX                                                          BLNUDGD.214
          DO 450 JJ=1,JLX                                                        BLNUDGD.215
            SCR2D(II,JJ)=(SFCOBS(NTE(NV),8,II,JJ)-                               BLNUDGD.216
#ifndef IBMopt                                                                   23DEC04.2619
     +                   SFCOBS(NTB(NV),8,II,JJ))/(TIME(NV)-TIMB(NV))            BLNUDGD.217
#else                                                                            23DEC04.2620
     +                   SFCOBS(NTB(NV),8,II,JJ)) * tim0                         23DEC04.2621
#endif                                                                           23DEC04.2622
C INTERPOLATE IN TIME                                                            BLNUDGD.218
            BLWNV(NV,II,JJ)=SFCOBS(NTB(NV),8,II,JJ)+BLFDT*SCR2D(II,JJ)           BLNUDGD.219
  450     CONTINUE                                                               BLNUDGD.220
                                                                                 BLNUDGD.221
      ENDIF                                                                      BLNUDGD.222
      IF(INT4(IN,2).NE.1)THEN                                                    BLNUDGD.223
        DO 455 II=1,ILX                                                          BLNUDGD.224
          DO 455 JJ=1,JLX                                                        BLNUDGD.225
            BLWNV(NV,II,JJ)=SFCOBS(NTE(NV),8,II,JJ)                              BLNUDGD.226
  455     CONTINUE                                                               BLNUDGD.227
                                                                                 BLNUDGD.228
      ENDIF                                                                      BLNUDGD.229
      IF(INITFLAG) RETURN                                                        BLNUDGD.230
  300 CONTINUE                                                                   BLNUDGD.231
      BLFDT=XTIME-DTMIN-TIMB(NV)                                                 BLNUDGD.232
      BLFDT=AMAX1(BLFDT,0.)                                                      BLNUDGD.233
C                                                                                BLNUDGD.234
CCC END OF CODE EXECUTED ONLY WHEN J=2                                           BLNUDGD.235
C                                                                                BLNUDGD.236
C                                                                                BLNUDGD.237
C DETERMINE XOBBL FOR THIS  N-S SLICE (J) AND IVAR (=NV)                         BLNUDGD.238
C WE ALREADY KNOW NTB ,NTE, ETC. FOR ALL VARAIBLES INVOLVED IN THIS              BLNUDGD.239
C CALL OF BLNUDGD                                                                BLNUDGD.240
C                                                                                BLNUDGD.241
C IF INTERPOLATING SURFACE DATA IN TIME (INT4(IN,2)=1)                           BLNUDGD.242
      IF(NV.EQ.5)GOTO 365                                                        BLNUDGD.243
      IF(INT4(IN,2).EQ.1)THEN                                                    BLNUDGD.244
#ifdef IBMopt                                                                    23DEC04.2623
        tim0 = 1./(TIME(NV)-TIMB(NV))                                            23DEC04.2624
#endif                                                                           23DEC04.2625
        DO 350 K=1,KL                                                            BLNUDGD.245
          DO 350 I=2+ICUT,ILX-IDC-ICUT                                           BLNUDGD.246
            XOBBL(I,K)=SFCOBS(NTB(NV),NV,I,J)+BLFDT*(                            BLNUDGD.247
#ifndef IBMopt                                                                   23DEC04.2626
     +                 SFCOBS(NTE(NV),NV,I,J)-SFCOBS(NTB(NV),NV,I,J))/(          BLNUDGD.248
     +                 TIME(NV)-TIMB(NV))                                        BLNUDGD.249
#else                                                                            23DEC04.2627
     +                 SFCOBS(NTE(NV),NV,I,J)-SFCOBS(NTB(NV),NV,I,J))*           23DEC04.2628
     +                 tim0                                                      23DEC04.2629
#endif                                                                           23DEC04.2630
  350     CONTINUE                                                               BLNUDGD.250
                                                                                 BLNUDGD.251
      ELSE                                                                       BLNUDGD.252
        DO 360 K=1,KL                                                            BLNUDGD.253
          DO 360 I=2+ICUT,ILX-IDC-ICUT                                           BLNUDGD.254
            XOBBL(I,K)=SFCOBS(NTE(NV),NV,I,J)                                    BLNUDGD.255
  360     CONTINUE                                                               BLNUDGD.256
                                                                                 BLNUDGD.257
      ENDIF                                                                      BLNUDGD.258
C                                                                                BLNUDGD.259
C IF IVAR=1 OR IVAR=2, AND REGJ(I)=4.0 (FREE CONVECTION), ADJUST                 BLNUDGD.260
C THE SURFACE WIND (10M) TO THE LOWEST MODEL LAYER (40M) USING                   BLNUDGD.261
C A FUNCTION DERIVED FROM SIMILARITY RELATIONSHIPS AND BASED                     BLNUDGD.262
C ON ROUGHNESS (ZNTJ).                                                           BLNUDGD.263
C                                                                                BLNUDGD.264
      IF(NV.LT.3.AND.IWINDS(IN,2).EQ.1)THEN                                      BLNUDGD.265
#ifdef IBMopt                                                                    23DEC04.2631
                                                                                 23DEC04.2632
      call vslog(temp1(nf(is)),ZNTJ(nf(is)),nn)                                  23DEC04.2633
                                                                                 23DEC04.2634
        do i=2+ICUT,ILX-IDC-ICUT                                                 23DEC04.2635
           temp1(i) = temp1(i) * 0.2                                             23DEC04.2636
        end do                                                                   23DEC04.2637
                                                                                 23DEC04.2638
      call vsexp(temp1(nf(is)),temp1(nf(is)),nn)                                 23DEC04.2639
                                                                                 23DEC04.2640
#endif                                                                           23DEC04.2641
        DO 361 K=1,KL                                                            BLNUDGD.266
          DO  I=2+ICUT,ILX-IDC-ICUT                                              23DEC04.2642
C THE RATIO U(40M)/U(10M), THE CORRECTION  OF THE SURFACE WIND                   BLNUDGD.268
C AT 10M FOR USE AT 40M (WITHIN THE SURFACE LAYER) AND THROUGH                   BLNUDGD.269
C THE FREE CONVECTIVE BOUNDARY LAYER IS A FUNCTION OF STABILITY                  BLNUDGD.270
C AND ROUGHNESS.  IT WAS FOUND THAT FOR TWO STABILITIES (L=-10M                  BLNUDGD.271
C AND L=-100M WHERE L IS THE MONIN LENGTH IN M) THIS RATIO                       BLNUDGD.272
C WAS A STRONGER FUNCTION OF ROUGHNESS THAN STABILITY.  THEREFORE,               BLNUDGD.273
C A FUNCTIONAL RELATIONSHIP WAS DERIVED FOR THIS RATIO (AVERAGED                 BLNUDGD.274
C FOR THESE TWO STABILITY REGIMES).                                              BLNUDGD.275
C                                                                                BLNUDGD.276
C FOR ZNT.LT.0.2M, THE RATIO IS APPROXIMATELY A POWER LAW:                       BLNUDGD.277
#ifndef IBMopt                                                                   23DEC04.2643
            WNDCORP=1.0+0.320*ZNTJ(I)**0.2                                       BLNUDGD.278
#else                                                                            23DEC04.2644
             WNDCORP=1.0+0.320*temp1(i)                                          23DEC04.2645
#endif                                                                           23DEC04.2646
C FOR ZNT.GE.0.2M AND ZNT.LE.1.0M THE RATIO IS APPROXIMATELY LINEAR:             BLNUDGD.279
            WNDCORL=1.169+0.315*ZNTJ(I)                                          BLNUDGD.280
C ASSIGN THE CORRECT FUNCTIONAL FORM FOR THIS GRID POINT                         BLNUDGD.281
            IF(ZNTJ(I).LE.0.2)THEN                                               BLNUDGD.282
              WNDCOR=WNDCORP                                                     BLNUDGD.283
            ELSE                                                                 BLNUDGD.284
              WNDCOR=WNDCORL                                                     BLNUDGD.285
            ENDIF                                                                BLNUDGD.286
C IF A(KL).LE.10 M, NO CORRECTION...                                             BLNUDGD.287
            IF(DIFZ.LE.10.)THEN                                                  BLNUDGD.288
              WNDCOR=1.0                                                         BLNUDGD.289
            ELSE                                                                 BLNUDGD.290
              WNDCOR=WNDFAC*WNDCOR                                               BLNUDGD.291
            ENDIF                                                                BLNUDGD.292
C TEMPORARY TEST...                                                              BLNUDGD.293
            IF(NV.EQ.1.AND.J.EQ.30.AND.KTAU.EQ.0.AND.K.EQ.KL)PRINT*,             BLNUDGD.294
     +        'IN,I,DIFZ,WNDFAC,WNDCOR: ',IN,I,DIFZ,WNDFAC,WNDCOR                BLNUDGD.295
C TEMPORARY TEST...                                                              BLNUDGD.296
C DO THE CORRECTION ONLY IF THE GRID POINT IS IN REGIME 4 (FREE CONV)            BLNUDGD.297
            IF(REGJ(I).GT.3.5)THEN                                               BLNUDGD.298
              XOBBL(I,K)=WNDCOR*XOBBL(I,K)                                       BLNUDGD.299
            ENDIF                                                                BLNUDGD.300
          ENDDO                                                                  23DEC04.2647
  361     CONTINUE                                                               BLNUDGD.301
                                                                                 BLNUDGD.302
      ENDIF                                                                      BLNUDGD.303
C                                                                                BLNUDGD.304
C IF IVAR=4, MAKE SURE OBS QV IS NOT GREATER THAN QSAT OF THE MODEL QV           BLNUDGD.305
C                                                                                BLNUDGD.306
      IF(IVAR.EQ.4)THEN                                                          BLNUDGD.307
        DO 362 K=1,KL                                                            BLNUDGD.308
          NSS=0                                                                  BLNUDGD.309
          IF(MOD(KTAU,NPF).EQ.0.AND.J.EQ.10)THEN                                 BLNUDGD.310
            PRINT6000,K,XOBBL(10,K),QSATF(10,K)                                  BLNUDGD.311
6000  FORMAT(1X,'IN BLNUDGD FOR Q: K,XOB,QSAT: ',1X,I3,                          BLNUDGD.312
     12(1X,E15.5))                                                               BLNUDGD.313
          ENDIF                                                                  BLNUDGD.314
          DO 362 I=2+ICUT,ILX-IDC-ICUT                                           BLNUDGD.315
            IF(XOBBL(I,K).GE.QSATF(I,K))THEN                                     BLNUDGD.316
              XOBBL(I,K)=QSATF(I,K)                                              BLNUDGD.317
              NSS=NSS+1                                                          BLNUDGD.318
            ENDIF                                                                BLNUDGD.319
  362     CONTINUE                                                               BLNUDGD.320
                                                                                 BLNUDGD.321
        IF(MOD(KTAU,NPF).EQ.0.AND.J.EQ.10)PRINT363,NSS,KTAU,J                    BLNUDGD.322
363   FORMAT(1X,'IN BLNUDGD:  OBSERVED QV ADJUSTED FOR SUPERATURATION',          BLNUDGD.323
     1' AT 'I4,' GRID POINTS IN MODEL SURFACE LAYER AT KTAU/J = ',               BLNUDGD.324
     2I4,1X,I3)                                                                  BLNUDGD.325
      ENDIF                                                                      BLNUDGD.326
C                                                                                BLNUDGD.327
C                                                                                BLNUDGD.328
C IF IPSTR(IN)=1, DEPENDING ON IVAR, BLDUM2D IS EITHER BLPSOC OR BLPSOD          BLNUDGD.329
C IF(IPSTR(IN)=0 AND IPSTR(IN-2)=1,  BLDUM2D IS PSTO (FROM 12-HLY DATA)          BLNUDGD.330
C                                                                                BLNUDGD.331
CCCC NOTE: BLDUM2D CAN`T BE STORED. IT CHANGES WITH NV AND MUST BE SET           BLNUDGD.332
C TO EITHER PSTO (FROM 12-HLY RAWINS DATA) OR BLPSOC OR BLPSOD (BASED            BLNUDGD.333
C ON RAWINS 3-HLY ANALYSES).                                                     BLNUDGD.334
CCCC                                                                             BLNUDGD.335
  365 CONTINUE                                                                   BLNUDGD.336
      IF(IPSTR(IN,2).NE.1)THEN                                                   BLNUDGD.337
        DO 410 I=1,ILX                                                           BLNUDGD.338
          BLPS1D(I)=PSTO(I,J)                                                    BLNUDGD.339
  410   CONTINUE                                                                 BLNUDGD.340
      ELSE                                                                       BLNUDGD.341
        IF(NV.LE.2)THEN                                                          BLNUDGD.342
          DO 415 I=1,ILX                                                         BLNUDGD.343
            BLPS1D(I)=BLPSOD(I,J)                                                BLNUDGD.344
  415     CONTINUE                                                               BLNUDGD.345
        ELSE                                                                     BLNUDGD.346
          DO 420 I=1,ILX                                                         BLNUDGD.347
            BLPS1D(I)=BLPSOC(I,J)                                                BLNUDGD.348
  420     CONTINUE                                                               BLNUDGD.349
        ENDIF                                                                    BLNUDGD.350
      ENDIF                                                                      BLNUDGD.351
C                                                                                BLNUDGD.352
C BY NOW, BLPS1D IS BLPSOC OR BLPSOD (IF IPSTR(IN)=1) ELSE PSTO FROM 12H         BLNUDGD.353
C                                                                                BLNUDGD.354
C SIMILARLY, THE HORIZONTAL WEIGHTING FUNCTION MUST BE DETERMINED FROM B         BLNUDGD.355
C IF(INDXY(IN)=2, OR BASED ON WXY IF INDXY(IN).NE.2.                             BLNUDGD.356
C                                                                                BLNUDGD.357
      IF(INDXY(IN,2).NE.2)THEN                                                   BLNUDGD.358
        DO 510 I=1,ILX                                                           BLNUDGD.359
          BLW1D(I)=WXY(I,J)                                                      BLNUDGD.360
  510   CONTINUE                                                                 BLNUDGD.361
      ELSE                                                                       BLNUDGD.362
        DO 515 I=1,ILX                                                           BLNUDGD.363
          BLW1D(I)=BLWNV(NV,I,J)                                                 BLNUDGD.364
  515   CONTINUE                                                                 BLNUDGD.365
      ENDIF                                                                      BLNUDGD.366
C NUDGE P* HERE WITH P* BL DATA OR 12-HLY DATA FROM RAWINS                       BLNUDGD.367
C ONLY NUDGE P* IN NUDGD IF BLNUDGD IS NOT CALLED                                BLNUDGD.368
      IFPST=MAX0(IPSTR(IN,2),IPSTR(IN,1))                                        BLNUDGD.369
C TEMPORARY...                                                                   BLNUDGD.370
      IF(IFPST.NE.0)STOP 1                                                       BLNUDGD.371
C TEMPORARY                                                                      BLNUDGD.372
      IF(ID.EQ.1)GOTO 10                                                         BLNUDGD.373
      IF(ID.EQ.3)GOTO 15                                                         BLNUDGD.374
      IF(IBLNOP(IN,NV).EQ.0)THEN                                                 BLNUDGD.375
C COMPUTE CORRECTION TERM IN THE BOUNDARY LAYER SOLELY ON XB(I,J,KL)             BLNUDGD.376
        DO 1 K=1,KL                                                              BLNUDGD.377
          DO 1 I=2+ICUT,ILX-IDC-ICUT                                             BLNUDGD.378
CB            XTEN(I,J,K)=XTEN(I,J,K)+GX(IN,2)*TFAC(IN,2)*PSTF(I,J)*(            23DEC04.2648
CB     +                  XOBBL(I,KL)-XB(I,J,KL)/PSTF(I,J))*BLW1D(I)*            23DEC04.2649
            XTEN(I,J,K)=XTEN(I,J,K)+GX(IN,2)*TFAC(IN,2)*(PSTF(I,J)*              23DEC04.2650
     +                  XOBBL(I,KL)-XB(I,J,KL))*BLW1D(I)*                        23DEC04.2651
     +                  ZFAC(IN,2,K)*(1.-WPBL(I,K))+GP(IN,2)*                    BLNUDGD.381
     +                  FLOAT(IFPST)*XB(I,J,K)/PSTF(I,J)*(BLPS1D(I)-             BLNUDGD.382
     +                  PSTF(I,J))*TFAC(IN,2)*ZFAC(IN,2,K)*BLW1D(I)              BLNUDGD.383
    1     CONTINUE                                                               BLNUDGD.384
                                                                                 BLNUDGD.385
      ENDIF                                                                      BLNUDGD.386
      IF(IBLNOP(IN,NV).EQ.1)THEN                                                 BLNUDGD.387
C COMPUTE CORRECTION TERM IN THE BOUNDARY LAYER ON XB(I,J,K) WHILE               BLNUDGD.388
C XOBBL(K) = XOBBL(KL) FOR ALL K                                                 BLNUDGD.389
        DO 5 K=1,KL                                                              BLNUDGD.390
          DO 5 I=2+ICUT,ILX-IDC-ICUT                                             BLNUDGD.391
CB            XTEN(I,J,K)=XTEN(I,J,K)+GX(IN,2)*TFAC(IN,2)*PSTF(I,J)*(            23DEC04.2652
CB     +                  XOBBL(I,K)-XB(I,J,K)/PSTF(I,J))*BLW1D(I)*              23DEC04.2653
            XTEN(I,J,K)=XTEN(I,J,K)+GX(IN,2)*TFAC(IN,2)*(PSTF(I,J)*              23DEC04.2654
     +                  XOBBL(I,K)-XB(I,J,K))*BLW1D(I)*                          23DEC04.2655
     +                  ZFAC(IN,2,K)*(1.-WPBL(I,K))+GP(IN,2)*                    BLNUDGD.394
     +                  FLOAT(IFPST)*XB(I,J,K)/PSTF(I,J)*(BLPS1D(I)-             BLNUDGD.395
     +                  PSTF(I,J))*TFAC(IN,2)*ZFAC(IN,2,K)*BLW1D(I)              BLNUDGD.396
    5     CONTINUE                                                               BLNUDGD.397
                                                                                 BLNUDGD.398
      ENDIF                                                                      BLNUDGD.399
      GOTO 4645                                                                  BLNUDGD.400
   15 CONTINUE                                                                   BLNUDGD.401
C FOR MOISTURE NUDGING, USE WEIGHT COMPUTED IN SETUPGD WHICH TAKES               BLNUDGD.402
C INTO ACCOUNT THE OBSERVED PRECIP DATA                                          BLNUDGD.403
C WXY2 IS WQ OR WQN                                                              BLNUDGD.404
      IF(IBLNOP(IN,NV).EQ.0)THEN                                                 BLNUDGD.405
        DO 2 K=1,KL                                                              BLNUDGD.406
          DO 2 I=2+ICUT,ILX-IDC-ICUT                                             BLNUDGD.407
CB            XTEN(I,J,K)=XTEN(I,J,K)+GX(IN,2)*TFAC(IN,2)*PSTF(I,J)*(            23DEC04.2656
CB     +                  XOBBL(I,KL)-XB(I,J,KL)/PSTF(I,J))*WXY2(I,J)*           23DEC04.2657
            XTEN(I,J,K)=XTEN(I,J,K)+GX(IN,2)*TFAC(IN,2)*(PSTF(I,J)*              23DEC04.2658
     +                  XOBBL(I,KL)-XB(I,J,KL))*WXY2(I,J)*                       23DEC04.2659
     +                  ZFAC(IN,2,K)*(1.-WPBL(I,K))*BLW1D(I)+GP(IN,2)*           BLNUDGD.410
     +                  FLOAT(IFPST)*XB(I,J,K)/PSTF(I,J)*(BLPS1D(I)-             BLNUDGD.411
     +                  PSTF(I,J))*TFAC(IN,2)*ZFAC(IN,2,K)*BLW1D(I)              BLNUDGD.412
    2     CONTINUE                                                               BLNUDGD.413
                                                                                 BLNUDGD.414
      ENDIF                                                                      BLNUDGD.415
      IF(IBLNOP(IN,NV).EQ.1)THEN                                                 BLNUDGD.416
        DO 4 K=1,KL                                                              BLNUDGD.417
          DO 4 I=2+ICUT,ILX-IDC-ICUT                                             BLNUDGD.418
CB            XTEN(I,J,K)=XTEN(I,J,K)+GX(IN,2)*TFAC(IN,2)*PSTF(I,J)*(            23DEC04.2660
CB     +                  XOBBL(I,K)-XB(I,J,K)/PSTF(I,J))*WXY2(I,J)*             23DEC04.2661
            XTEN(I,J,K)=XTEN(I,J,K)+GX(IN,2)*TFAC(IN,2)*(PSTF(I,J)*              23DEC04.2662
     +                  XOBBL(I,K)-XB(I,J,K))*WXY2(I,J)*                         23DEC04.2663
     +                  ZFAC(IN,2,K)*(1.-WPBL(I,K))*BLW1D(I)+GP(IN,2)*           BLNUDGD.421
     +                  FLOAT(IFPST)*XB(I,J,K)/PSTF(I,J)*(BLPS1D(I)-             BLNUDGD.422
     +                  PSTF(I,J))*TFAC(IN,2)*ZFAC(IN,2,K)*BLW1D(I)              BLNUDGD.423
    4     CONTINUE                                                               BLNUDGD.424
                                                                                 BLNUDGD.425
      ENDIF                                                                      BLNUDGD.426
 4645 IF(MOD(KTAU,NPF).EQ.0.AND.J.EQ.10)GOTO 4646                                BLNUDGD.427
      GOTO 4747                                                                  BLNUDGD.428
 4646 CONTINUE                                                                   BLNUDGD.429
      IF(ID.EQ.3)THEN                                                            BLNUDGD.430
        XTH=KTAU*120./3600.                                                      BLNUDGD.431
        PRINT3133                                                                BLNUDGD.432
3133  FORMAT(1X,' WQ IN BLNUDGD: --------------------')                          BLNUDGD.433
        CALL MAPSMP(WXY2,MIXFG,MJXFG,1,ILX,1,1,JLX,1,0.,1,8HWXY2(WQ),XTH         BLNUDGD.434
     +       )                                                                   BLNUDGD.435
      ENDIF                                                                      BLNUDGD.436
      PRINT3333,IBLNOP(IN,NV),BLPS1D(10),BLW1D(10),XOBBL(10,KL),WNDFAC,          BLNUDGD.437
     +          DIFZ,BLFDT                                                       BLNUDGD.438
3333  FORMAT(1X,'IN BLNUDGD: IBLNOP,BLPS,BLWXY,XOB,WNDFAC,DIFZ,BLFDT : '         BLNUDGD.439
     1,1X,I1,6(1X,F9.3))                                                         BLNUDGD.440
      PRINT3334,TFAC(IN,2),ZFAC(IN,2,KL),IFPST,GX(IN,2),WXY(10,10),              BLNUDGD.441
     +          XB(10,J,KL),XTEN(10,10,KL),PSTF(10,J),PSTO(10,J)                 BLNUDGD.442
3334  FORMAT(1X,'IN BLNUDGD: TF,ZF,IFPS,',                                       BLNUDGD.443
     1'GX,WXY,XB,XTN,PF,PO: ',2F5.2,1X,I2,1X,4G10.3,                             BLNUDGD.444
     22(1X,F5.1))                                                                BLNUDGD.445
      PRINT3130,KPBLT(10),(XTEN(10,10,KK),KK=KL,1,-NPFV)                         BLNUDGD.446
3130  FORMAT(1X,'IN BLNUDGD-BOT: KPBL,XTEN(KL,KL-NPFV...): ',                    BLNUDGD.447
     1I2,8(1X,G10.3))                                                            BLNUDGD.448
 4747 CONTINUE                                                                   BLNUDGD.449
      RETURN                                                                     BLNUDGD.450
C                                                                                BLNUDGD.451
C COMPUTE THE NUDGING TERM IN THE THE CONT. (PTEN) EQ.                           BLNUDGD.452
C                                                                                BLNUDGD.453
   10 CONTINUE                                                                   BLNUDGD.454
      DO 20 I=2+ICUT,ILXM-ICUT                                                   BLNUDGD.455
C       PTEN(I)=PTEN(I)+GP(IN,2)*TFAC(IN,2)*BLW1D(I)*(BLPS1D(I)-                 BLNUDGD.456
C    +          PSTF(I,J))                                                       BLNUDGD.457
   20 CONTINUE                                                                   BLNUDGD.458
      RETURN                                                                     BLNUDGD.459
      END                                                                        BLNUDGD.460
                                                                                 BLNUDGD.461
