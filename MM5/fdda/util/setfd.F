         SUBROUTINE SETFD (INEST,FDTIM)                                          SETFD.1
#     include <defines.incl>                                                     SETFD.2
#     include <parame.incl>                                                      SETFD.3
#     include <addr0.incl>                                                       SETFD.4
#     include <varia.incl>                                                       SETFD.5
#     include <param2.incl>                                                      SETFD.6
#     include <parfdda.incl>                                                     SETFD.7
#     include <param3.incl>                                                      SETFD.8
#     include <pmoist.incl>                                                      SETFD.9
#     include <point3d.incl>                                                     SETFD.10
#     include <point2d.incl>                                                     SETFD.11
#     include <pointbc.incl>                                                     SETFD.12
#     include <various.incl>                                                     SETFD.13
#     include <dusolve1.incl>                                                    SETFD.14
#     include <fddagd.incl>                                                      SETFD.15
#     include <fddaob.incl>                                                      SETFD.16
C                                                                                SETFD.17
      DIMENSION QSATF(MIXFG,MKXFG),REGJ(MIX),                                    SETFD.18
     1  ZNTJ(MIX),KPBLJ(MIX)                                                     SETFD.19
                                                                                 SETFD.20
C_FLIC_RUNPAD(8)                                                                 07NOV00.1114
C--- FDDA INITIAL SETUP CALLS                                                    SETFD.22
C                                                                                SETFD.23
#ifdef FDDAGD1                                                                   SETFD.24
C                                                                                SETFD.25
      IF(I4D(INEST,1).NE.1)GOTO 318                                              SETFD.26
      IF(ABS(XTIME-XTNEX(INEST)).LT.DTMIN.AND.XTIME.GE.XTNEX(INEST).OR.(         SETFD.27
     +  KTAU.EQ.KTAUR.AND.IFREST))CALL IN4DGD(INEST,IL,JL,KL)                    SETFD.28
C CALCULATE OBS PSTAR AT TAU-1 IF INT4=1, OTHERWISE,                             SETFD.29
C NO INTERPOLATION IS NEEDED.                                                    SETFD.30
      IF(INT4(INEST,1).EQ.1)THEN                                                 SETFD.31
        FDTIM=XTIME-DTMIN-XTIM1(INEST)                                           SETFD.32
        FDTIM=AMAX1(FDTIM,0.)                                                    SETFD.33
      ELSE                                                                       SETFD.34
        STOP 311                                                                 SETFD.35
      ENDIF                                                                      SETFD.36
      CALL INTPSGD(PSO,PSOTEN,PSOC,PSOD,FDTIM,INEST)                             SETFD.37
C SETUP THE TIME AND VERTICAL SPACE FACTOR (TFAC AND ZFAC) FOR GRIDDED           SETFD.38
C NUDGING AND CALCULATE THE MODEL P* ON DOT POINTS IF NUDGING THE WINDS,         SETFD.39
C IE. IWIND(INEST).EQ.1.                                                         SETFD.40
      CALL SETUPGD(PSB,PSBD,INEST)                                               SETFD.41
C                                                                                SETFD.42
C CALCULATE THE OBS-FCST VORDIF IF ROTATIONAL GRID NUDGING (IROT=1)              SETFD.43
      IF(IROT(INEST).EQ.1.AND.IWIND(INEST,1).EQ.1.AND.INEST.EQ.1)THEN            SETFD.44
        IF(KTAU.EQ.KTAUR.AND.INEST.EQ.1)THEN                                     SETFD.45
C RE-INVERT MAP SCALE FACTORS BACK TO M FROM 1/M                                 SETFD.46
          DO 316 II=1,IL                                                         SETFD.47
            DO 316 JJ=1,JL                                                       SETFD.48
              MPSD(II,JJ)=1./MSFD(II,JJ)                                         SETFD.49
  316       CONTINUE                                                             SETFD.50
                                                                                 SETFD.51
          DO 317 II=1,ILX                                                        SETFD.52
            DO 317 JJ=1,JLX                                                      SETFD.53
              MPSX(II,JJ)=1./MSFX(II,JJ)                                         SETFD.54
  317       CONTINUE                                                             SETFD.55
                                                                                 SETFD.56
        ENDIF                                                                    SETFD.57
#  ifdef MPP1                                                                    SETFD.58
#    include <mpp_setfd_00.incl>                                                 SETFD.59
#  endif                                                                         SETFD.60
        CALL BUFVDGD(UBO,VBO,UBOTEN,VBOTEN,UB,VB,PSOD,FDTIM,MPSD,MPSX,           SETFD.61
     +       VORDIF,PSBD,INEST)                                                  SETFD.62
      ENDIF                                                                      SETFD.63
C                                                                                SETFD.64
C INTPSGD, BUFVDGD, AND SETUPGD ARE ONLY CALLED ONCE EVERY TIME STEP             SETFD.65
C AND THUS THEIR CALLS ARE ABOVE THE "J LOOPS" IN SOLVE.                         SETFD.66
C IN4DGD IS CALLED THE FIRST TIME IN INIT AND IN TEND THEREAFTER                 SETFD.67
      J=1                                                                        SETFD.68
        IF(ITEMP(INEST,1).EQ.1)CALL NUDGD(NTIM,NVAR,ITEMP,J,TB,TBOC,TTEN         SETFD.69
     +    ,PSB,PSOC,GT,0,WCS,DUM2D,TFAC,ZFAC,GP,GR,MPSD,3,VORDIF,INEST,          SETFD.70
     +    KPBLJ,BLWCS,BLWNV,SFCOBS,SFCTIM,QSATF,IQCHK,TIMB,TIME,NTB,NTE,         SETFD.71
     +    SCR2D,BLDUM2D,BLPOC,BLPOD,REGJ,ZNTJ,.TRUE.)                            SETFD.72
C                                                                                SETFD.73
        IF(IMOIS(INEST,1).EQ.1)CALL NUDGD(NTIM,NVAR,IMOIS,J,QVB,QBOC,            SETFD.74
     +    QVTEN,PSB,PSOC,GQ,IDQ,WCS,WQ,TFAC,ZFAC,GP,GR,MPSD,4,VORDIF,            SETFD.75
     +    INEST,KPBLJ,BLWCS,BLWNV,SFCOBS,SFCTIM,QSATF,IQCHK,TIMB,TIME,           SETFD.76
     +    NTB,NTE,SCR2D,BLDUM2D,BLPOC,BLPOD,REGJ,ZNTJ,.TRUE.)                    SETFD.77
C                                                                                SETFD.78
        IF(IWIND(INEST,1).EQ.1)THEN                                              SETFD.79
          ID=0                                                                   SETFD.80
          IF(IROT(INEST).EQ.1)ID=2                                               SETFD.81
C                                                                                SETFD.82
          CALL NUDGD(NTIM,NVAR,IWIND,J,UB,UBOD,UTEN,PSBD,PSOD,GV,ID,WDT,         SETFD.83
     +         WCS,TFAC,ZFAC,GP,GR,MPSD,1,VORDIF,INEST,KPBLJ,BLWDT,BLWNV         SETFD.84
     +         ,SFCOBS,SFCTIM,QSATF,IQCHK,TIMB,TIME,NTB,NTE,SCR2D,               SETFD.85
     +         BLDUM2D,BLPOC,BLPOD,REGJ,ZNTJ,.TRUE.)                             SETFD.86
C                                                                                SETFD.87
          CALL NUDGD(NTIM,NVAR,IWIND,J,VB,VBOD,VTEN,PSBD,PSOD,GV,ID,WDT,         SETFD.88
     +         WCS,TFAC,ZFAC,GP,GR,MPSD,2,VORDIF,INEST,KPBLJ,BLWDT,BLWNV         SETFD.89
     +         ,SFCOBS,SFCTIM,QSATF,IQCHK,TIMB,TIME,NTB,NTE,SCR2D,               SETFD.90
     +         BLDUM2D,BLPOC,BLPOD,REGJ,ZNTJ,.TRUE.)                             SETFD.91
        ENDIF                                                                    SETFD.92
C                                                                                SETFD.93
  318 CONTINUE                                                                   SETFD.94
C                                                                                SETFD.95
#endif                                                                           SETFD.96
C                                                                                SETFD.97
#ifdef FDDAOBS1                                                                  SETFD.98
C                                                                                SETFD.99
C DEFINE NSTA WHEN NOT NUDGING TO IND. OBS.                                      SETFD.100
      IF(KTAU.EQ.KTAUR)PRINT3333,I4DITOT                                         SETFD.101
3333  FORMAT(1X,'IN SOLVE: I4DITOT = ',I2)                                       SETFD.102
      NSTA=NSTAT(INEST)                                                          SETFD.103
      IF(I4DITOT.EQ.0)THEN                                                       SETFD.104
        NSTAT(INEST)=0                                                           SETFD.105
        NSTA=0                                                                   SETFD.106
        GOTO 319                                                                 SETFD.107
      ENDIF                                                                      SETFD.108
      INFR=IONF*(IRATIO**LEVIDN(INEST))                                          SETFD.109
      IF((KTAU.GT.KTAUR.AND.MOD(KTAU,INFR).EQ.0).OR.(KTAU.EQ.KTAUR.AND.          SETFD.110
     +  IFREST))CALL IN4DOB(INEST,XTIME,KTAU,KTAUR,DTMIN,JULDAY,GMT)             SETFD.111
C                                                                                SETFD.112
      IF(IFREST.AND.KTAU.EQ.KTAUR.AND.I4DI(INEST).EQ.1.AND.NSTA.GT.0)            SETFD.113
     +  THEN                                                                     SETFD.114
        IDARST=1                                                                 SETFD.115
      ELSE                                                                       SETFD.116
        IDARST=0                                                                 SETFD.117
      ENDIF                                                                      SETFD.118
C                                                                                SETFD.119
      IF((I4DI(INEST).EQ.1.AND.NSTA.GT.0.AND.MOD(KTAU,INFR).EQ.0).OR.            SETFD.120
     +  IDARST.EQ.1)CALL ERROB(INEST,UB,VB,TB,QVB,PSB)                           SETFD.121
  319 CONTINUE                                                                   SETFD.122
      TFACI(INEST)=1.0                                                           SETFD.123
      IF(IDYNIN.EQ.1.AND.I4DI(INEST).EQ.1)THEN                                   SETFD.124
        DTR=ABS(DTRAMP)                                                          SETFD.125
        TCONST=1./DTR                                                            SETFD.126
C FDAEND(IN) IS THE TIME IN MINUTES TO END THE DYNAMIC INITIALIZATION CY         SETFD.127
        IF(XTIME.LT.FDAEND(INEST)-DTR)THEN                                       SETFD.128
          TFACI(INEST)=1.                                                        SETFD.129
        ELSEIF(XTIME.GE.FDAEND(INEST)-DTR.AND.XTIME.LE.FDAEND(INEST))            SETFD.130
     +        THEN                                                               SETFD.131
          TFACI(INEST)=(FDAEND(INEST)-XTIME)*TCONST                              SETFD.132
        ELSE                                                                     SETFD.133
          TFACI(INEST)=0.0                                                       SETFD.134
        ENDIF                                                                    SETFD.135
        IF(KTAU.EQ.KTAUR.OR.MOD(KTAU,10).EQ.0)THEN                               SETFD.136
         PRINT*,' DYNINOBS: IN,KTAU,XTIME,FDAEND,DTRAMP,DTR,TCONST',             SETFD.137
     1   ',TFACI: ',INEST,KTAU,XTIME,FDAEND(INEST),DTRAMP,DTR,TCONST,            SETFD.138
     2   TFACI(INEST)                                                            SETFD.139
        ENDIF                                                                    SETFD.140
      ENDIF                                                                      SETFD.141
C                                                                                SETFD.142
#endif                                                                           SETFD.143
      RETURN                                                                     SETFD.144
      END                                                                        SETFD.145
                                                                                 SETFD.146
