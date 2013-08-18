      PROGRAM TERRAIN                                                            TERRAIN.1
C                                                                                TERRAIN.2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC           TERRAIN.3
C                                                                    C           TERRAIN.4
C     TERRAIN IS THE FIRST COMPONENT OF THE MM5 MODELING SYSTEM AND  C           TERRAIN.5
C   USED TO ACCESS ARCHIVED TERRAIN HEIGHT AND LAND-USE CHARACTIS-   C           TERRAIN.6
C   TICS DATA AT REGULAR LATITUDE-LONGITITUDE INTERVALS AND INTER-   C           TERRAIN.7
C   POLATE TO THE MESOSCALE GRID FOR A SPECIFIED MAP PROJECTION.     C           TERRAIN.8
C                                                                    C           TERRAIN.9
C      THE TERRAIN PROGRAM DYNAMICALLY ALLOCATES THE MEMORY.         C           TERRAIN.10
C   THEREFORE, USERS DON'T NEED TO RECOMPILE THE PROGRAM IF THERE    C           TERRAIN.11
C   ARE NO USERS' OWN MODIFICATIONS TO THE PROGRAM.                  C           TERRAIN.12
C                                                                    C           TERRAIN.13
C                                                                    C           TERRAIN.14
C                                        YONG-RUN GUO                C           TERRAIN.15
C                                        SUE CHEN                    C           TERRAIN.16
C                                                      10/21/1993    C           TERRAIN.17
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC           TERRAIN.18
# include <parame.incl>                                                          TERRAIN.19
# include <paramesv.incl>                                                        TERRAIN.20
# include <io.incl>                                                              06NOV00.539
# include <maps.incl>                                                            TERRAIN.21
# include <nestdmn.incl>                                                         TERRAIN.22
# include <option.incl>                                                          TERRAIN.23
C                                                                                TERRAIN.24
      DIMENSION WORK1(NSIZE), WORK2(NSIZE), WORK3(NSIZE), WORK4(NSIZE),          TERRAIN.25
     -          WORK5(NSIZE), LAND(NSIZE)                                        TERRAIN.26
      DIMENSION V_WORK(NVC*NSIZE), LV_WORK(NSIZE), VF_WORK(12*NSIZE),            TERRAIN.27
     -          S_WORK(NSC*NSIZE), LS_WORK(NSIZE), T_WORK(NSIZE),                TERRAIN.28
     -        WLM_WORK(NSIZE), XLV_WORK(NSIZE), XLS_WORK(NSIZE)                  TERRAIN.29
      DIMENSION V_WORK1(NVC*NSIZE), VF_WORK1(12*NSIZE),                          TERRAIN.30
     -          S_WORK1(NSC*NSIZE),  T_WORK1(NSIZE),                             TERRAIN.31
     -        WLM_WORK1(NSIZE), XLV_WORK1(NSIZE), XLS_WORK1(NSIZE)               TERRAIN.32
      DIMENSION IWD(100),JWD(100)                                                TERRAIN.33
      CHARACTER*80  FNAME                                                        TERRAIN.34
                                                                                 TERRAIN.35
# include <ezwater.incl>                                                         TERRAIN.36
      CHARACTER OUTLINE*2,EZLINE*120                                             TERRAIN.37
C                                                                                TERRAIN.38
#ifdef NCARG                                                                     TERRAIN.39
      FNAME='TER.PLT'                                                            TERRAIN.40
                                                                                 TERRAIN.41
C   OPEN GKS                                                                     TERRAIN.42
                                                                                 TERRAIN.43
      CALL GOPKS(6,IDUM)                                                         TERRAIN.44
      CALL GESC(-1391,1,FNAME,1,IDUM,CDUM)                                       TERRAIN.45
      CALL GOPWK( 1, 3, 1 )                                                      TERRAIN.46
      CALL GACWK(1)                                                              TERRAIN.47
      CALL GOPWK (9,17,3)                                                        TERRAIN.48
#endif                                                                           TERRAIN.49
                                                                                 TERRAIN.50
C   SET UP THE PARAMETERS AND CONSTANTS                                          TERRAIN.51
      CALL SETUP                                                                 TERRAIN.52
                                                                                 TERRAIN.53
#ifdef NCARG                                                                     TERRAIN.54
C                                                                                TERRAIN.55
C   GET EZMAP WATER BODY INFO, ASSIGN HEIGHTS OF WATER BODIES                    TERRAIN.56
C                                                                                TERRAIN.57
      IF (.NOT.IFEZFUG) GOTO 66                                                  TERRAIN.58
      OPEN (UNIT=19,FILE='psids',FORM='FORMATTED',STATUS='UNKNOWN')              TERRAIN.59
      NPSAREAS=0                                                                 TERRAIN.60
 50   READ(18,'(A120)',END=60,ERR=60) EZLINE                                     TERRAIN.61
      IF (EZLINE(1:2).EQ.'PS') THEN                                              TERRAIN.62
         WRITE(19,'(A118)') EZLINE(3:120)                                        TERRAIN.63
         NPSAREAS=NPSAREAS+1                                                     TERRAIN.64
      ENDIF                                                                      TERRAIN.65
      GOTO 50                                                                    TERRAIN.66
 60   CONTINUE                                                                   TERRAIN.67
      CLOSE (19)                                                                 TERRAIN.68
      IF (NPSAREAS.LT.550) THEN                                                  TERRAIN.69
         PRINT*,'LESS THAN 550 PS AREA IDS READ. MUST BE A PROBLEM.'             TERRAIN.70
         STOP                                                                    TERRAIN.71
      ENDIF                                                                      TERRAIN.72
      OPEN (UNIT=19,FILE='psids',FORM='FORMATTED',STATUS='OLD')                  TERRAIN.73
      NWAREAS=0                                                                  TERRAIN.74
      DO 64 I=1,NPSAREAS                                                         TERRAIN.75
         READ(19,*) IDAREA,INDCOLR                                               TERRAIN.76
         IF (INDCOLR.EQ.1) THEN                                                  TERRAIN.77
            NWAREAS=NWAREAS+1                                                    TERRAIN.78
            IDWATER(NWAREAS)=IDAREA                                              TERRAIN.79
            HTWATER(NWAREAS)=9E9                                                 TERRAIN.80
            IF (HTPS(IDAREA).NE.9E9) HTWATER(NWAREAS)=HTPS(IDAREA)               TERRAIN.81
         ENDIF                                                                   TERRAIN.82
 64   CONTINUE                                                                   TERRAIN.83
      CLOSE (19)                                                                 TERRAIN.84
 66   CONTINUE                                                                   TERRAIN.85
#endif                                                                           TERRAIN.86
C                                                                                TERRAIN.87
      DO 10 NM = 1,MAXNES                                                        TERRAIN.88
C                                                                                TERRAIN.89
      PRINT 11, NM                                                               TERRAIN.90
11    FORMAT(/1X,'========== CREATE TERRESTRIAL DATA ',                          06NOV00.540
     1           'FOR DOMAIN:'I3,' =========='/)                                 06NOV00.541
C                                                                                TERRAIN.93
      IF (NM.EQ.1) THEN                                                          TERRAIN.94
      IMAX = IXEX                                                                TERRAIN.95
      JMAX = JXEX                                                                TERRAIN.96
      ELSE                                                                       TERRAIN.97
      IMAX = NESTIX(NM)                                                          TERRAIN.98
      JMAX = NESTJX(NM)                                                          TERRAIN.99
      ENDIF                                                                      TERRAIN.100
      IWD(NM)=IMAX                                                               TERRAIN.101
      JWD(NM)=JMAX                                                               TERRAIN.102
C                                                                                TERRAIN.103
      IF(.NOT.IFTER) GO TO 10                                                    TERRAIN.104
C                                                                                TERRAIN.105
      CALL TERDRV(WORK1,LAND,WORK2,WORK3,IMAX, JMAX, NM,                         TERRAIN.106
     >            V_WORK, LV_WORK, VF_WORK, S_WORK, LS_WORK, T_WORK)             TERRAIN.107
      PRINT 12, NM                                                               TERRAIN.108
12    FORMAT(/5X,'****** DOMAIN ',I2,' TERRESTRIAL DATA CREATED ******')         06NOV00.542
C                                                                                TERRAIN.110
10    CONTINUE                                                                   TERRAIN.111
C                                                                                TERRAIN.112
C  IF ONLY MAP BACKGROUND IS DESIRED, GO TO THE PLOT ROUTINE                     TERRAIN.113
C                                                                                TERRAIN.114
      IF(.NOT.IFTER) GO TO 25                                                    TERRAIN.115
C                                                                                TERRAIN.116
      IF (MAXNES.GT.1) THEN                                                      TERRAIN.117
C =====================================================================          TERRAIN.118
C                                                                                TERRAIN.119
C  BOUNDARIES BLENDING, 1-2-1 SMOOTHING,...... FOR NESTS:                        TERRAIN.122
      DO 100 MN = MAXNES, 2, -1                                                  TERRAIN.123
        IINF = IVOL_mdl + MN                                                     06NOV00.543
C                                                                                TERRAIN.125
C  TO GET THE UNIT NUMBER FOR THE MOTHER (UPPER LEVEL) DOMAIN:                   TERRAIN.126
        IINC = IVOL_mdl + NUMNC(MN)                                              06NOV00.544
        PRINT 3, MN                                                              06NOV00.545
        MNC = NUMNC(MN)                                                          TERRAIN.128
        IF (MNC.EQ.1) THEN                                                       TERRAIN.129
        IMAX = IXEX                                                              TERRAIN.130
        JMAX = JXEX                                                              TERRAIN.131
        ELSE                                                                     TERRAIN.132
        IMAX = NESTIX(MNC)                                                       TERRAIN.133
        JMAX = NESTJX(MNC)                                                       TERRAIN.134
        ENDIF                                                                    TERRAIN.135
        CALL TFUDGE(WORK1,    WORK2,V_WORK ,S_WORK ,VF_WORK ,T_WORK ,            TERRAIN.136
     -          WLM_WORK ,XLV_WORK ,XLS_WORK ,                                   TERRAIN.137
     -              WORK3,    WORK4,V_WORK1,S_WORK1,VF_WORK1,T_WORK1,            TERRAIN.138
     -          WLM_WORK1,XLV_WORK1,XLS_WORK1,                                   TERRAIN.139
     1              WORK5,IMAX,JMAX,IINC,NESTIX(MN),NESTJX(MN),IINF,             TERRAIN.140
     2              IPFLAG,MIX,MJX,VEGESOIL,VEGONLY,IPRINT)                      06NOV00.546
100   CONTINUE                                                                   TERRAIN.142
3     FORMAT(/1X,'---------- BOUNDARY BLENDING ',                                06NOV00.547
     1           'FOR DOMAIN:'I3,' ----------'/)                                 06NOV00.548
C                                                                                TERRAIN.143
C=======================================================================         TERRAIN.144
C   REPLACE THE TERRAIN HEIGHT OF OVERLAPING POINTS FROM THE HIGHEST NES         TERRAIN.145
C   LEVELS DOWN TO THE COARSE MESH                                               TERRAIN.146
C                                                                                TERRAIN.147
      DO 200 MN = MAXNES, 2, -1                                                  TERRAIN.148
C                                                                                TERRAIN.149
        IINF = IVOL_mdl + MN                                                     06NOV00.549
C                                                                                TERRAIN.151
C  TO GET THE UNIT NUMBER FOR THE MOTHER (UPPER LEVEL) DOMAIN:                   TERRAIN.152
        IINC = IVOL_mdl + NUMNC(MN)                                              06NOV00.550
        MNC = NUMNC(MN)                                                          TERRAIN.154
C                                                                                TERRAIN.155
        IF (MNC.EQ.1) THEN                                                       TERRAIN.156
        IMAX = IXEX                                                              TERRAIN.157
        JMAX = JXEX                                                              TERRAIN.158
        ELSE                                                                     TERRAIN.159
        IMAX = NESTIX(MNC)                                                       TERRAIN.160
        JMAX = NESTJX(MNC)                                                       TERRAIN.161
        ENDIF                                                                    TERRAIN.162
C                                                                                TERRAIN.163
        IF(NSTTYP(MN).EQ.2) THEN                                                 TERRAIN.164
        PRINT 2, MNC                                                             06NOV00.551
        CALL REPLACE(WORK1,   WORK2,V_WORK ,S_WORK ,VF_WORK ,T_WORK ,            TERRAIN.167
     -            WLM_WORK,XLV_WORK,XLS_WORK,  IMAX ,  JMAX ,    IINC,           TERRAIN.168
     -               WORK3,   WORK4,V_WORK1,S_WORK1,VF_WORK1,T_WORK1,            TERRAIN.169
     -           WLM_WORK1,XLV_WORK1,XLS_WORK1,NESTIX(MN),NESTJX(MN),            TERRAIN.170
     -                IINF,IPFLAG)                                               TERRAIN.171
        ENDIF                                                                    TERRAIN.172
                                                                                 TERRAIN.173
200   CONTINUE                                                                   TERRAIN.174
2     FORMAT(/1X,'++++++++++ TWO-WAY NESTED ADJUSTMENT ',                        06NOV00.552
     -       'FOR DOMAIN:'I3,' ++++++++++'/)                                     06NOV00.553
                                                                                 TERRAIN.175
      ENDIF                                                                      TERRAIN.176
C END OF NEST ADJUSTMENT                                                         TERRAIN.177
                                                                                 TERRAIN.178
25    CONTINUE                                                                   TERRAIN.179
C=======================================================================         TERRAIN.180
C   OUTPUT TERRESTRIAL FIELDS, ONE DOMAIN AT A TIME                              TERRAIN.181
C                                                                                TERRAIN.182
      DO 300 MN = 1, MAXNES                                                      TERRAIN.183
C                                                                                TERRAIN.184
        IIN = IVOL_mdl + MN                                                      06NOV00.554
        MNC = NUMNC(MN)                                                          TERRAIN.186
        IF (MN.EQ.1) THEN                                                        TERRAIN.187
           IMAX = IXEX                                                           TERRAIN.188
           JMAX = JXEX                                                           TERRAIN.189
        ELSE                                                                     TERRAIN.190
           IMAX = NESTIX(MN)                                                     TERRAIN.191
           JMAX = NESTJX(MN)                                                     TERRAIN.192
        ENDIF                                                                    TERRAIN.193
C                                                                                TERRAIN.194
        CALL OUTPUT(WORK1,WORK2,V_WORK,S_WORK,VF_WORK,T_WORK,                    TERRAIN.195
     &           WLM_WORK,XLV_WORK,XLS_WORK,IMAX,JMAX,                           TERRAIN.196
     &           IIN,IPFLAG)                                                     TERRAIN.197
                                                                                 TERRAIN.198
300   CONTINUE                                                                   TERRAIN.199
                                                                                 TERRAIN.200
C                                                                                TERRAIN.201
#ifdef NCARG                                                                     TERRAIN.202
      PRINT *, ' '                                                               06NOV00.555
      PRINT *, 'PLOT TERRESTIAL FIELDS.'                                         06NOV00.556
      DO 400 MN=1,MAXNES                                                         TERRAIN.203
C        PRINT *, 'PLOT TERRESTIAL FIELDS. IX,JX = ',IWD(MN),JWD(MN)             06NOV00.557
         CALL FINPRT(WORK1,LAND,WORK2,V_WORK,S_WORK,VF_WORK,T_WORK,              TERRAIN.205
     &            WLM_WORK,LV_WORK,LS_WORK,IWD(MN),JWD(MN),MN,WORK5)             TERRAIN.206
400   CONTINUE                                                                   TERRAIN.207
#endif                                                                           TERRAIN.208
40    CONTINUE                                                                   TERRAIN.209
                                                                                 TERRAIN.210
      PRINT 900                                                                  TERRAIN.211
900   FORMAT(//2X,'== NORMAL TERMINATION OF TERRAIN PROGRAM ==')                 TERRAIN.212
                                                                                 TERRAIN.213
#ifdef NCARG                                                                     TERRAIN.214
C   CLOSE GKS                                                                    TERRAIN.215
      CALL GCLWK(9)                                                              TERRAIN.216
      CALL GDAWK (1)                                                             TERRAIN.217
      CALL GCLWK (1)                                                             TERRAIN.218
      CALL GCLKS                                                                 TERRAIN.219
#endif                                                                           TERRAIN.220
      STOP 99999                                                                 TERRAIN.221
      END                                                                        TERRAIN.222
