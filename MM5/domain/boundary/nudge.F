      SUBROUTINE NUDGE(IP,FCOEF,GCOEF,DTB,FTEN,FEB,FWB,FSB,FNB,                  NUDGE.1
     1                 FEBT,FWBT,FSBT,FNBT,FB,C203,KD,IE,JE)                     NUDGE.2
      IMPLICIT NONE                                                              NUDGE.3
C                                                                                NUDGE.4
C input variables: TTEN,TEB,TWB,TSB,TNB,TEBT,TWBT,TSBT,TNBT,FB                   NUDGE.5
C                                                                                NUDGE.6
C input constants: IP,FCOEF,GCOEF,C203,KD,IE,JE                                  NUDGE.7
C                                                                                NUDGE.8
C output variables: TTEN                                                         NUDGE.9
C                                                                                NUDGE.10
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          NUDGE.11
C                                                                     C          NUDGE.12
C                                                                     C          NUDGE.13
C     THIS SUBROUTINE APPLIES RELAXATION BOUNDARY CONDITIONS TO THE   C          NUDGE.14
C     TENDENCY TERM - FTEN.                                           C          NUDGE.15
C                                                                     C          NUDGE.16
C                                                                     C          NUDGE.17
C     IP    : IS THE NUMBER OF SLICES AFFECTED BY NUDGING.            C          NUDGE.18
C                                                                     C          NUDGE.19
C     FCOEF : ARE THE COEFFICIENTS FOR THE NEWTONIAN TERM.            C          NUDGE.20
C                                                                     C          NUDGE.21
C     GCOEF : ARE THE COEFFICIENTS FOR THE DIFFUSION TERM.            C          NUDGE.22
C                                                                     C          NUDGE.23
C     FTEN  : IS THE TENDENCY CALCULATED FROM THE MODEL.              C          NUDGE.24
C                                                                     C          NUDGE.25
C     FEB, FWB, FSB, FNB : ARE THE OBSERVED BOUNDARY VALUES AT TIME   C          NUDGE.26
C             "TBE" ON EAST, WEST, SOUTH, AND NORTH BOUNDARIES.       C          NUDGE.27
C                                                                     C          NUDGE.28
C     FEBT, FWBT, FSBT, FNBT : ARE THE LARGE-SCALE OR OBSERVED        C          NUDGE.29
C             TENDENCIES AT EAST, WEST, SOUTH, AND NORTH BOUNDARIES.  C          NUDGE.30
C                                                                     C          NUDGE.31
C     FB    : IS THE VARIABLE AT TAU-1.                               C          NUDGE.32
C                                                                     C          NUDGE.33
C     C203  : = 1./(DX*DX), DEFINED IN 'PARAM'.                       C          NUDGE.34
C                                                                     C          NUDGE.35
C     KD : VERTICAL  DIMENSION  FOR ALL THE VARIABLES.                C          NUDGE.36
C                                                                     C          NUDGE.37
C     IE = IL, JE = JL FOR DOT-POINT VARIABLES.                       C          NUDGE.38
C     IE = IL-1, JE = JL-1 FOR CROSS-POINT VARIABLES.                 C          NUDGE.39
C                                                                     C          NUDGE.40
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          NUDGE.41
#       include <parame.incl>                                                    NUDGE.42
C                                                                                NUDGE.43
      INTEGER   IP,KD,IE,JE                                                      NUDGE.44
      REAL      FCOEF,GCOEF,DTB,C203                                             NUDGE.45
      REAL      FTEN(MIX,MJX,KD)                                                 NUDGE.46
      REAL      FEBT(MIX,KD,IP+1),FWBT(MIX,KD,IP+1),                             NUDGE.47
     1          FNBT(MJX,KD,IP+1),FSBT(MJX,KD,IP+1),                             NUDGE.48
     2          FEB(MIX,KD,IP+1),FWB(MIX,KD,IP+1),                               NUDGE.49
     3          FSB(MJX,KD,IP+1),FNB(MJX,KD,IP+1),                               NUDGE.50
     4          FB(MIX,MJX,KD)                                                   NUDGE.51
      INTEGER   NN, NN1                                                          NUDGE.52
      INTEGER   I,J,K                                                            NUDGE.53
      INTEGER   IBGN,IEND,JBGN,JEND                                              NUDGE.54
      INTEGER   INORTH,ISOUTH,JEAST,JWEST                                        NUDGE.55
      REAL      XFUN,FLS0,FLS1,FLS2,FLS3,FLS4                                    NUDGE.56
C                                                                                NUDGE.57
C----------------------------------------------------------------------          NUDGE.58
C                                                                                NUDGE.59
      REAL      FCX(NSPGD),GCX(NSPGD)                                            NUDGE.60
C                                                                                NUDGE.61
      DO NN=1,NSPGD                                                              NUDGE.62
#ifdef EXPOBC
        XFUN=EXP(-FLOAT(NN-2)/3.)
#else
        XFUN=FLOAT(NSPGD-NN)/(NSPGD-2.)                                          NUDGE.63
#endif
        FCX(NN)=FCOEF*XFUN                                                       NUDGE.64
        GCX(NN)=GCOEF*XFUN                                                       NUDGE.65
      ENDDO                                                                      NUDGE.66
C                                                                                NUDGE.67
      DO 100 NN1=2,IP                                                            NUDGE.68
        IBGN=NN1                                                                 NUDGE.69
        IEND=IE+1-NN1                                                            NUDGE.70
        JBGN=NN1+1                                                               NUDGE.71
        JEND=JE-NN1                                                              NUDGE.72
#ifndef MPP1                                                                     NUDGE.73
        NN=NN1                                                                   NUDGE.74
#else                                                                            NUDGE.75
        NN=NOFLIC(NN1)                                                           NUDGE.76
#endif                                                                           NUDGE.77
C                                                                                NUDGE.78
CJM Reason for JWEST and JEAST: see related comment below.                       NUDGE.79
        JWEST=NN                                                                 NUDGE.80
        JEAST=JE+1-NN                                                            NUDGE.81
cmic$ do all autoscope                                                           NUDGE.82
cmic$1 shared(JE,IBGN,IEND,KD,NN,FCX,GCX,DTB,C203,                               NUDGE.83
cmic$1        FWB,FWBT,FEB,FEBT,FB,FTEN)                                         NUDGE.84
cmic$2 private(I,K,FLS0,FLS1,FLS2,FLS3,FLS4)                                     07NOV00.54
c$doacross                                                                       NUDGE.86
c$& local(i,k,fls0,fls1,fls2,fls3,fls4)                                          07NOV00.55
c$omp parallel do default(shared)                                                NUDGE.88
c$omp&private(i,k,fls0,fls1,fls2,fls3,fls4)                                      07NOV00.56
        DO 10 I=IBGN,IEND                                                        NUDGE.90
C                                                                                NUDGE.91
C-----WEST-BOUNDARY SLICE:                                                       NUDGE.92
C                                                                                NUDGE.93
#ifdef MPP1                                                                      24SEP99.3
         IF ( FLIC_G2L_N(JWEST,MJX) .GT. 1 ) THEN                                24SEP99.4
#endif                                                                           24SEP99.5
          DO K=1,KD                                                              NUDGE.94
            FLS0=(FWB(I,K,NN)+DTB*FWBT(I,K,NN))-FB(I,JWEST,K)                    NUDGE.95
            FLS1=(FWB(I-1,K,NN)+DTB*FWBT(I-1,K,NN))-FB(I-1,JWEST,K)              NUDGE.96
            FLS2=(FWB(I+1,K,NN)+DTB*FWBT(I+1,K,NN))-FB(I+1,JWEST,K)              NUDGE.97
            FLS3=(FWB(I,K,NN-1)+DTB*FWBT(I,K,NN-1))-FB(I,JWEST-1,K)              NUDGE.98
            FLS4=(FWB(I,K,NN+1)+DTB*FWBT(I,K,NN+1))-FB(I,JWEST+1,K)              NUDGE.99
            FTEN(I,JWEST,K)=FTEN(I,JWEST,K)+FCX(NN)*FLS0-GCX(NN)*C203*(          NUDGE.100
     +                      FLS1+FLS2+FLS3+FLS4-4.*FLS0)                         NUDGE.101
          ENDDO                                                                  NUDGE.102
#ifdef MPP1                                                                      24SEP99.6
         END IF                                                                  24SEP99.7
#endif                                                                           24SEP99.8
C                                                                                NUDGE.103
C-----EAST-BOUNDARY SLICE:                                                       NUDGE.104
C                                                                                NUDGE.105
#ifdef MPP1                                                                      24SEP99.9
         IF ( FLIC_G2L_N(JEAST,MJX) .LT. MJX ) THEN                              24SEP99.10
#endif                                                                           24SEP99.11
          DO K=1,KD                                                              NUDGE.106
            FLS0=(FEB(I,K,NN)+DTB*FEBT(I,K,NN))-FB(I,JEAST,K)                    NUDGE.107
            FLS1=(FEB(I-1,K,NN)+DTB*FEBT(I-1,K,NN))-FB(I-1,JEAST,K)              NUDGE.108
            FLS2=(FEB(I+1,K,NN)+DTB*FEBT(I+1,K,NN))-FB(I+1,JEAST,K)              NUDGE.109
            FLS3=(FEB(I,K,NN-1)+DTB*FEBT(I,K,NN-1))-FB(I,JEAST+1,K)              NUDGE.110
            FLS4=(FEB(I,K,NN+1)+DTB*FEBT(I,K,NN+1))-FB(I,JEAST-1,K)              NUDGE.111
            FTEN(I,JEAST,K)=FTEN(I,JEAST,K)+FCX(NN)*FLS0-GCX(NN)*C203*(          NUDGE.112
     +                      FLS1+FLS2+FLS3+FLS4-4.*FLS0)                         NUDGE.113
          ENDDO                                                                  NUDGE.114
#ifdef MPP1                                                                      24SEP99.12
         END IF                                                                  24SEP99.13
#endif                                                                           24SEP99.14
   10   CONTINUE                                                                 NUDGE.115
C                                                                                NUDGE.116
CJM Reason for ISOUTH and INORTH - put def of these outside multi                NUDGE.117
CJM tasked loops; that way FLIC will put definition of temporaries               NUDGE.118
CJM outside the loop, and there won't be any need to declare these               NUDGE.119
CJM FLIC temporaries as private in the sm directive block.                       NUDGE.120
        ISOUTH=NN                                                                NUDGE.121
        INORTH=IE+1-NN                                                           NUDGE.122
cmic$ do all autoscope                                                           NUDGE.123
cmic$1 shared(IE,JBGN,JEND,KD,NN,FCX,GCX,DTB,C203,                               NUDGE.124
cmic$1        FSB,FSBT,FNB,FNBT,FB,FTEN)                                         NUDGE.125
cmic$2 private(J,K,FLS0,FLS1,FLS2,FLS3,FLS4)                                     07NOV00.57
c$doacross                                                                       NUDGE.127
c$& local(j,k,fls0,fls1,fls2,fls3,fls4)                                          07NOV00.58
c$omp parallel do default(shared)                                                NUDGE.129
c$omp&private(j,k,fls0,fls1,fls2,fls3,fls4)                                      07NOV00.59
        DO 20 J=JBGN,JEND                                                        NUDGE.131
C                                                                                NUDGE.132
C.....SOUTH BOUNDARY:                                                            NUDGE.133
C                                                                                NUDGE.134
#ifdef MPP1                                                                      24SEP99.15
         IF ( FLIC_G2L_M(ISOUTH,MIX) .GT. 1 ) THEN                               24SEP99.16
#endif                                                                           24SEP99.17
          DO K=1,KD                                                              NUDGE.135
            FLS0=(FSB(J,K,NN)+DTB*FSBT(J,K,NN))-FB(ISOUTH,J,K)                   NUDGE.136
            FLS1=(FSB(J-1,K,NN)+DTB*FSBT(J-1,K,NN))-FB(ISOUTH,J-1,K)             NUDGE.137
            FLS2=(FSB(J+1,K,NN)+DTB*FSBT(J+1,K,NN))-FB(ISOUTH,J+1,K)             NUDGE.138
            FLS3=(FSB(J,K,NN-1)+DTB*FSBT(J,K,NN-1))-FB(ISOUTH-1,J,K)             NUDGE.139
            FLS4=(FSB(J,K,NN+1)+DTB*FSBT(J,K,NN+1))-FB(ISOUTH+1,J,K)             NUDGE.140
            FTEN(ISOUTH,J,K)=FTEN(ISOUTH,J,K)+FCX(NN)*FLS0-GCX(NN)*C203*         NUDGE.141
     +                       (FLS1+FLS2+FLS3+FLS4-4.*FLS0)                       NUDGE.142
          ENDDO                                                                  NUDGE.143
#ifdef MPP1                                                                      24SEP99.18
         END IF                                                                  24SEP99.19
#endif                                                                           24SEP99.20
C                                                                                NUDGE.144
C.....NORTH BOUNDARY:                                                            NUDGE.145
C                                                                                NUDGE.146
#ifdef MPP1                                                                      24SEP99.21
         IF ( FLIC_G2L_M(INORTH,MIX) .LT. MIX ) THEN                             24SEP99.22
#endif                                                                           24SEP99.23
          DO K=1,KD                                                              NUDGE.147
            FLS0=(FNB(J,K,NN)+DTB*FNBT(J,K,NN))-FB(INORTH,J,K)                   NUDGE.148
            FLS1=(FNB(J-1,K,NN)+DTB*FNBT(J-1,K,NN))-FB(INORTH,J-1,K)             NUDGE.149
            FLS2=(FNB(J+1,K,NN)+DTB*FNBT(J+1,K,NN))-FB(INORTH,J+1,K)             NUDGE.150
            FLS3=(FNB(J,K,NN-1)+DTB*FNBT(J,K,NN-1))-FB(INORTH+1,J,K)             NUDGE.151
            FLS4=(FNB(J,K,NN+1)+DTB*FNBT(J,K,NN+1))-FB(INORTH-1,J,K)             NUDGE.152
            FTEN(INORTH,J,K)=FTEN(INORTH,J,K)+FCX(NN)*FLS0-GCX(NN)*C203*         NUDGE.153
     +                       (FLS1+FLS2+FLS3+FLS4-4.*FLS0)                       NUDGE.154
          ENDDO                                                                  NUDGE.155
#ifdef MPP1                                                                      24SEP99.24
         END IF                                                                  24SEP99.25
#endif                                                                           24SEP99.26
   20   CONTINUE                                                                 NUDGE.156
  100 CONTINUE                                                                   NUDGE.157
C                                                                                NUDGE.158
      RETURN                                                                     NUDGE.159
      END                                                                        NUDGE.160
                                                                                 NUDGE.161
