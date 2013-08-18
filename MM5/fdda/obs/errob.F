      SUBROUTINE ERROB(IN,                                                       ERROB.1
     1                 UB,VB,TB,QVB,PSB)                                         ERROB.2
      IMPLICIT NONE                                                              05DEC01.602
C                                                                                ERROB.3
C THIS SUBROUTINE CALCULATES THE DIFFERENCE BETWEEN THE                          ERROB.4
C OBSERVED VALUES AND THE FORECAST VALUES AT THE OBSERVATION                     ERROB.5
C POINTS.  THE OBSERVED VALUES CLOSEST TO THE CURRENT                            ERROB.6
C FORECAST TIME (XTIME) WERE DETERMINED IN SUBROUTINE                            ERROB.7
C IN4DOB AND STORED IN ARRAY VAROBS.  THE DIFFERENCES                            ERROB.8
C CALCULATED BY SUBROUTINE ERROB WILL BE STORED IN ARRAY                         ERROB.9
C ERRF FOR THE NSTA OBSERVATION LOCATIONS.  MISSING                              ERROB.10
C OBSERVATIONS ARE DENOTED BY THE DUMMY VALUE 99999.                             ERROB.11
C                                                                                ERROB.12
C THE STORAGE ORDER IN VAROBS AND ERRF IS AS FOLLOWS:                            ERROB.13
C        IVAR                VARIABLE TYPE(TAU-1)                                ERROB.14
C        ----                --------------------                                ERROB.15
C         1                           U                                          ERROB.16
C         2                           V                                          ERROB.17
C         3                           T                                          ERROB.18
C         4                           Q                                          ERROB.19
C         5                         P*(CROSS)                                    ERROB.20
C         6                    P*(DOT)--ERRF ONLY                                ERROB.21
C                                                                                ERROB.22
C                                                                                ERROB.23
#     include <parame.incl>                                                      ERROB.24
#     include <fddaob.incl>                                                      ERROB.25
#     include <param2.incl>                                                      ERROB.26
#     include <parfdda.incl>                                                     ERROB.27
#     include <various.incl>                                                     ERROB.28
#     include <rpstar.incl>                                                      ERROB.29
                                                                                 ERROB.30
      DIMENSION IA(NIOBF),IB(NIOBF),IC(NIOBF),RA(NIOBF),RB(NIOBF),               ERROB.31
     1          RC(NIOBF)                                                        ERROB.32
      DIMENSION UB(MIX,MJX,MKX),VB(MIX,MJX,MKX),TB(MIX,MJX,MKX),                 ERROB.33
     1          QVB(MIX,MJX,MKX),PSB(MIX,MJX)                                    ERROB.34
      DIMENSION MM(MAXSES)                                                       ERROB.35
                                                                                 ERROB.36
C Declarations for Implicit None                                                 05DEC01.603
      integer i,mm,ierr,ierror                                                   05DEC01.604
      integer nlocal_dot,nlocal_crs                                              05DEC01.605
      integer iiis,iiie,jjjs,jjje                                                05DEC01.606
      integer ia,ib,ic,ityp,n,in,inpf                                            05DEC01.607
      integer iob,job,kob,iobm,jobm,iobp,jobp,kobp                               05DEC01.608
      integer ivar,nml,nlb,nnl,nle,nsta                                          05DEC01.609
      real    pd1,pd2,pd3,pd4,ra,rb,rc                                           05DEC01.610
      real    dxob,dyob,dzob,fnpf                                                05DEC01.611
      real    ub,vb,psb,qvb,tb,grfac                                             05DEC01.612
      real    dummy,grid                                                         05DEC01.613
                                                                                 05DEC01.614
#ifdef MPP1                                                                      ERROB.37
C_FLIC_BEGIN_NOFLIC                                                              05DEC01.615
      INCLUDE 'mpif.h'                                                           05DEC01.616
C_FLIC_END_NOFLIC                                                                05DEC01.617
#  include "mpp_errob_00.incl"                                                   ERROB.39
#endif                                                                           ERROB.40
C                                                                                ERROB.41
      NSTA=NSTAT(IN)                                                             ERROB.42
C FIRST, DETERMINE THE GRID TYPE CORRECTION FOR DOT AND CROSS                    ERROB.43
C POINTS, AND WHEN IN=2, CONVERT THE STORED COARSE MESH INDICES                  ERROB.44
C TO THE FINE MESH INDEX EQUIVALENTS                                             ERROB.45
C                                                                                ERROB.46
C ITYP=1 FOR DOT POINTS AND 2 FOR CROSS POINTS                                   ERROB.47
#ifdef MPP1                                                                      ERROB.48
#  include "mpp_errob_10.incl"                                                   ERROB.49
#endif                                                                           ERROB.50
      PRINT5,KTAU,IN,NSTA                                                        ERROB.51
5     FORMAT(1X,'++++++CALL ERROB AT KTAU = ',I5,' AND INEST = ',                ERROB.52
     1I2,':  NSTA = ',I5,' ++++++')                                              ERROB.53
      GRID=0.0                                                                   ERROB.54
      DUMMY=99999.                                                               ERROB.55
      DO 50 IVAR=1,6                                                             ERROB.56
        DO 50 N=1,NSTA                                                           ERROB.57
          ERRF(IVAR,N)=0.0                                                       ERROB.58
   50   CONTINUE                                                                 ERROB.59
                                                                                 ERROB.60
      DO 150 ITYP=1,2                                                            ERROB.61
        IF(ITYP.EQ.2)GRID=0.5                                                    ERROB.62
        IF(IN.NE.1)GOTO 20                                                       ERROB.63
C COARSE MESH CASE...                                                            ERROB.64
        DO 10 N=1,NSTA                                                           ERROB.65
          RA(N)=RIO(N)-GRID                                                      ERROB.66
          RB(N)=RJO(N)-GRID                                                      ERROB.67
          RC(N)=RKO(N)                                                           ERROB.68
   10   CONTINUE                                                                 ERROB.69
        GOTO 40                                                                  ERROB.70
C FINE MESH CASE...CONVERT (I,J,K) OF OBSERVATIONS TO THE                        ERROB.71
C EQUIVALENT FINE MESH VALUES.                                                   ERROB.72
   20   DO 30 N=1,NSTA                                                           ERROB.73
C COMPUTE THE OBS LOCATION WITH RESPECT TO THIS MESH (IN)...                     ERROB.74
          NML=IN                                                                 ERROB.75
          MM(LEVIDN(IN)+1)=IN                                                    ERROB.76
C WORKING TOWARD COARSER MESHES, DETERMINE THE HIERARCHY OF MOTHER               ERROB.77
C MESHES WITH RESPECT TO EACH MOTHER MESH STARTING AT MESH "IN"...               ERROB.78
C THAT IS, DETERMINE ITS MOTHER, GRANDMOTHER, GREAT-GRANDMOTHER, ETC.            ERROB.79
C OUT TO THE COARSE GRID MESH (IN=1).                                            ERROB.80
C LEVIDN HOLDS THE NEST LEVEL AND NUMNC HOLDS THE MOTHER MESH FOR EACH           ERROB.81
C GRID (E.G., FOR 3 MESHES AND 2 NEST LEVELS, IN=1 IS THE COARSE GRID            ERROB.82
C MESH, IN=2 HAS LEVIDN(2)=1 AND NUMNC(2)=1, AND IN=3 HAS LEVIDN(3)=2            ERROB.83
C AND NUMNC(3)=2...)                                                             ERROB.84
          DO 21 NNL=LEVIDN(IN),1,-1                                              ERROB.85
            MM(NNL)=NUMNC(NML)                                                   ERROB.86
            NML=MM(NNL)                                                          ERROB.87
   21     CONTINUE                                                               ERROB.88
C MM(1) MUST BE THE COARSE GRID MESH (IN=1)                                      ERROB.89
          IF(MM(1).NE.1)STOP 21                                                  ERROB.90
          RA(N)=RIO(N)                                                           ERROB.91
          RB(N)=RJO(N)                                                           ERROB.92
          DO 22 NNL=1,LEVIDN(IN)                                                 ERROB.93
            GRFAC=0.                                                             ERROB.94
C COMPUTE THE OBS LOCATION WITH RESPECT TO THE INNER GRID IN DOT-POINT           ERROB.95
C SPACE (GRID=0.).  WHEN WE REACH MESH IN, THEN USE "GRID" TO GO TO              ERROB.96
C CROSS OR DOT DEPENDING ON THE VARIABLE...                                      ERROB.97
            IF(NNL.EQ.LEVIDN(IN))GRFAC=GRID                                      ERROB.98
            RA(N)=(RA(N)-FLOAT(NESTI(MM(NNL+1))))*FLOAT(IRATIO)+1.-GRFAC         ERROB.99
            RB(N)=(RB(N)-FLOAT(NESTJ(MM(NNL+1))))*FLOAT(IRATIO)+1.-GRFAC         ERROB.100
   22     CONTINUE                                                               ERROB.101
          RC(N)=RKO(N)                                                           ERROB.102
   30   CONTINUE                                                                 ERROB.103
   40   CONTINUE                                                                 ERROB.104
C                                                                                ERROB.105
C INITIALIZE THE ARRAY OF DIFFERENCES BETWEEN THE OBSERVATIONS                   ERROB.106
C AND THE LOCAL FORECAST VALUES TO ZERO.  FOR THE FINE MESH                      ERROB.107
C ONLY, SET THE DIFFERENCE TO A LARGE DUMMY VALUE IF THE                         ERROB.108
C OBSERVATION IS OUTSIDE THE FINE MESH DOMAIN.                                   ERROB.109
C                                                                                ERROB.110
C                                                                                ERROB.111
C SET DIFFERENCE VALUE TO A DUMMY VALUE FOR OBS POINTS OUTSIDE                   ERROB.112
C CURRENT DOMAIN                                                                 ERROB.113
        NLB=1                                                                    ERROB.114
        NLE=2                                                                    ERROB.115
        IF(ITYP.EQ.2)NLB=3                                                       ERROB.116
        IF(ITYP.EQ.2)NLE=5                                                       ERROB.117
        DO 60 IVAR=NLB,NLE                                                       ERROB.118
          DO 60 N=1,NSTA                                                         ERROB.119
            IF((RA(N)-1.).LT.0)THEN                                              ERROB.120
              ERRF(IVAR,N)=ERRF(IVAR,N)+DUMMY                                    ERROB.121
            ENDIF                                                                ERROB.122
            IF((RB(N)-1.).LT.0)THEN                                              ERROB.123
              ERRF(IVAR,N)=ERRF(IVAR,N)+DUMMY                                    ERROB.124
            ENDIF                                                                ERROB.125
            IF((FLOAT(IL)-2.0*GRID-RA(N)-1.E-10).LT.0)THEN                       ERROB.126
              ERRF(IVAR,N)=ERRF(IVAR,N)+DUMMY                                    ERROB.127
            ENDIF                                                                ERROB.128
            IF((FLOAT(JL)-2.0*GRID-RB(N)-1.E-10).LT.0)THEN                       ERROB.129
              ERRF(IVAR,N)=ERRF(IVAR,N)+DUMMY                                    ERROB.130
            ENDIF                                                                ERROB.131
   60     CONTINUE                                                               ERROB.132
                                                                                 ERROB.133
C                                                                                ERROB.134
C NOW FIND THE EXACT OFFSET OF EACH OBSERVATION FROM THE                         ERROB.135
C GRID POINT TOWARD THE LOWER LEFT                                               ERROB.136
C                                                                                ERROB.137
        DO 80 N=1,NSTA                                                           ERROB.138
          IA(N)=RA(N)                                                            ERROB.139
          IB(N)=RB(N)                                                            ERROB.140
          IC(N)=RC(N)                                                            ERROB.141
   80   CONTINUE                                                                 ERROB.142
        DO 90 N=1,NSTA                                                           ERROB.143
          RA(N)=RA(N)-FLOAT(IA(N))                                               ERROB.144
          RB(N)=RB(N)-FLOAT(IB(N))                                               ERROB.145
          RC(N)=RC(N)-FLOAT(IC(N))                                               ERROB.146
   90   CONTINUE                                                                 ERROB.147
C PERFORM A TRILINEAR EIGHT-POINT (3-D) INTERPOLATION                            ERROB.148
C TO FIND THE FORECAST VALUE AT THE EXACT OBSERVATION                            ERROB.149
C POINTS FOR U, V, T, AND Q.                                                     ERROB.150
        IF(ITYP.NE.1)GOTO 110                                                    ERROB.151
C FIRST PROCESS DOT POINT VARIABLES,IVAR=1 (U) AND                               ERROB.152
C IVAR=2 (V) IN LOOP 100.  ALSO, FIND THE DIFFERENCE                             ERROB.153
C OF THE OBSERVED AND FORECAST SURFACE PRESSURE IN TERMS                         ERROB.154
C OF DOT POINT INDICES BY BILINEAR INTERPOLATION.                                ERROB.155
        IF(ISWIND(IN).NE.1.AND.ISPSTR(IN).NE.1)GOTO 150                          ERROB.156
        DO 100 N=1,NSTA                                                          ERROB.157
          IOB=MAX0(1,IA(N))                                                      ERROB.158
          IOB=MIN0(IOB,ILX)                                                      ERROB.159
          IOBM=MAX0(1,IOB-1)                                                     ERROB.160
          IOBP=MIN0(ILX,IOB+1)                                                   ERROB.161
          JOB=MAX0(1,IB(N))                                                      ERROB.162
          JOB=MIN0(JOB,JLX)                                                      ERROB.163
          JOBM=MAX0(1,JOB-1)                                                     ERROB.164
          JOBP=MIN0(JLX,JOB+1)                                                   ERROB.165
          KOB=MAX0(1,IC(N))                                                      ERROB.166
#ifdef MPP1                                                                      ERROB.167
#  include "mpp_errob_20.incl"                                                   ERROB.168
#endif                                                                           ERROB.169
C     KOB=IC(N)                                                                  ERROB.170
          KOBP=MIN0(KOB+1,KL)                                                    ERROB.171
          DXOB=RB(N)                                                             ERROB.172
          DYOB=RA(N)                                                             ERROB.173
          DZOB=RC(N)                                                             ERROB.174
          PD1=0.25*(PSB(IOB,JOB)+PSB(IOBM,JOB)+PSB(IOB,JOBM)+                    ERROB.175
     +        PSB(IOBM,JOBM))                                                    ERROB.176
          PD2=0.25*(PSB(IOB,JOBP)+PSB(IOBM,JOBP)+PSB(IOB,JOB)+                   ERROB.177
     +        PSB(IOBM,JOB))                                                     ERROB.178
          PD3=0.25*(PSB(IOBP,JOB)+PSB(IOB,JOB)+PSB(IOBP,JOBM)+                   ERROB.179
     +        PSB(IOB,JOBM))                                                     ERROB.180
          PD4=0.25*(PSB(IOBP,JOBP)+PSB(IOB,JOBP)+PSB(IOBP,JOB)+                  ERROB.181
     +        PSB(IOB,JOB))                                                      ERROB.182
C INITIAL ERRF(IVAR,N) IS ZERO FOR OBSERVATIONS POINTS                           ERROB.183
C WITHIN THE DOMAIN, AND A LARGE DUMMY VALUE FOR POINTS                          ERROB.184
C OUTSIDE THE CURRENT DOMAIN                                                     ERROB.185
C U COMPONENT                                                                    ERROB.186
          ERRF(1,N)=ERRF(1,N)+VAROBS(1,N)-((1.-DZOB)*((1.-DXOB)*((1.-            ERROB.187
     +              DYOB)*UB(IOB,JOB,KOB)/PD1+DYOB*UB(IOB+1,JOB,KOB)/PD3         ERROB.188
     +              )+DXOB*((1.-DYOB)*UB(IOB,JOB+1,KOB)/PD2+DYOB*                ERROB.189
     +              UB(IOB+1,JOB+1,KOB)/PD4))+DZOB*((1.-DXOB)*((1.-DYOB)         ERROB.190
     +              *UB(IOB,JOB,KOBP)/PD1+DYOB*UB(IOB+1,JOB,KOBP)/PD3)+          ERROB.191
     +              DXOB*((1.-DYOB)*UB(IOB,JOB+1,KOBP)/PD2+DYOB*                 ERROB.192
     +              UB(IOB+1,JOB+1,KOBP)/PD4)))                                  ERROB.193
C V COMPONENT                                                                    ERROB.194
          ERRF(2,N)=ERRF(2,N)+VAROBS(2,N)-((1.-DZOB)*((1.-DXOB)*((1.-            ERROB.195
     +              DYOB)*VB(IOB,JOB,KOB)/PD1+DYOB*VB(IOB+1,JOB,KOB)/PD3         ERROB.196
     +              )+DXOB*((1.-DYOB)*VB(IOB,JOB+1,KOB)/PD2+DYOB*                ERROB.197
     +              VB(IOB+1,JOB+1,KOB)/PD4))+DZOB*((1.-DXOB)*((1.-DYOB)         ERROB.198
     +              *VB(IOB,JOB,KOBP)/PD1+DYOB*VB(IOB+1,JOB,KOBP)/PD3)+          ERROB.199
     +              DXOB*((1.-DYOB)*VB(IOB,JOB+1,KOBP)/PD2+DYOB*                 ERROB.200
     +              VB(IOB+1,JOB+1,KOBP)/PD4)))                                  ERROB.201
C SURFACE PRESSURE DIFFERENCE AT DOT POINTS (P-OBS IS IVAR=5)                    ERROB.202
          ERRF(6,N)=ERRF(6,N)+VAROBS(5,N)-((1.-DXOB)*((1.-DYOB)*PD1+DYOB         ERROB.203
     +              *PD3)+DXOB*((1.-DYOB)*PD2+DYOB*PD4))                         ERROB.204
          IF(ERRF(6,N)-9.E4.GE.0)THEN                                            ERROB.205
            ERRF(6,N)=0.0                                                        ERROB.206
          ENDIF                                                                  ERROB.207
1443  FORMAT(1X,'E NSTA,IN,IA,IB,IC,EUVTQP,TOB',I4,I2,3(1X,I6),                  ERROB.208
     16(1X,E11.4))                                                               ERROB.209
1444  FORMAT(1X,'E NSTA,IN,RA,RB,RC,VUVTQP,TOB',I4,I2,3(1X,F6.2),                ERROB.210
     16(1X,E11.4))                                                               ERROB.211
#ifdef MPP1                                                                      ERROB.212
#  include "mpp_errob_30.incl"                                                   ERROB.213
#endif                                                                           ERROB.214
  100   CONTINUE                                                                 ERROB.215
        GOTO 150                                                                 ERROB.216
C                                                                                ERROB.217
  110   CONTINUE                                                                 ERROB.218
C NEXT 3-D CROSS POINT VARIABLES IVAR=3 (T) AND IVAR=4 (QV)                      ERROB.219
        IF(ISTEMP(IN).NE.1.AND.ISMOIS(IN).NE.1)GOTO 130                          ERROB.220
        DO 120 N=1,NSTA                                                          ERROB.221
          IOB=MAX0(1,IA(N))                                                      ERROB.222
          IOB=MIN0(IOB,ILX-1)                                                    ERROB.223
          JOB=MAX0(1,IB(N))                                                      ERROB.224
          JOB=MIN0(JOB,JLX-1)                                                    ERROB.225
#ifdef MPP1                                                                      ERROB.226
#  include "mpp_errob_40.incl"                                                   ERROB.227
#endif                                                                           ERROB.228
          KOB=MAX0(1,IC(N))                                                      ERROB.229
C     KOB=IC(N)                                                                  ERROB.230
          KOBP=MIN0(KOB+1,KL)                                                    ERROB.231
          DXOB=RB(N)                                                             ERROB.232
          DYOB=RA(N)                                                             ERROB.233
          DZOB=RC(N)                                                             ERROB.234
C TEMPERATURE                                                                    ERROB.235
          ERRF(3,N)=ERRF(3,N)+VAROBS(3,N)-((1.-DZOB)*((1.-DXOB)*((1.-            ERROB.236
     +              DYOB)*TB(IOB,JOB,KOB)*RPSB(IOB,JOB)+DYOB*                    ERROB.237
     +              TB(IOB+1,JOB,KOB)*RPSB(IOB+1,JOB))+DXOB*((1.-DYOB)*          ERROB.238
     +              TB(IOB,JOB+1,KOB)*RPSB(IOB,JOB+1)+DYOB*                      ERROB.239
     +              TB(IOB+1,JOB+1,KOB)/PSB(IOB+1,JOB+1)))+DZOB*((1.-            ERROB.240
     +              DXOB)*((1.-DYOB)*TB(IOB,JOB,KOBP)/PSB(IOB,JOB)+DYOB*         ERROB.241
     +              TB(IOB+1,JOB,KOBP)*RPSB(IOB+1,JOB))+DXOB*((1.-DYOB)*         ERROB.242
     +              TB(IOB,JOB+1,KOBP)*RPSB(IOB,JOB+1)+DYOB*                     ERROB.243
     +              TB(IOB+1,JOB+1,KOBP)*RPSB(IOB+1,JOB+1))))                    ERROB.244
C MIXING RATIO                                                                   ERROB.245
          ERRF(4,N)=ERRF(4,N)+VAROBS(4,N)-((1.-DZOB)*((1.-DXOB)*((1.-            ERROB.246
     +              DYOB)*QVB(IOB,JOB,KOB)*RPSB(IOB,JOB)+DYOB*                   ERROB.247
     +              QVB(IOB+1,JOB,KOB)*RPSB(IOB+1,JOB))+DXOB*((1.-DYOB)*         ERROB.248
     +              QVB(IOB,JOB+1,KOB)*RPSB(IOB,JOB+1)+DYOB*                     ERROB.249
     +              QVB(IOB+1,JOB+1,KOB)/PSB(IOB+1,JOB+1)))+DZOB*((1.-           ERROB.250
     +              DXOB)*((1.-DYOB)*QVB(IOB,JOB,KOBP)/PSB(IOB,JOB)+DYOB         ERROB.251
     +              *QVB(IOB+1,JOB,KOBP)*RPSB(IOB+1,JOB))+DXOB*((1.-DYOB         ERROB.252
     +              )*QVB(IOB,JOB+1,KOBP)*RPSB(IOB,JOB+1)+DYOB*                  ERROB.253
     +              QVB(IOB+1,JOB+1,KOBP)*RPSB(IOB+1,JOB+1))))                   ERROB.254
#ifdef MPP1                                                                      ERROB.255
#  include "mpp_errob_50.incl"                                                   ERROB.256
#endif                                                                           ERROB.257
  120   CONTINUE                                                                 ERROB.258
C PROCESS SURFACE PRESSURE CROSS-POINT FIELD, IVAR=5,                            ERROB.259
C USING BILINEAR FOUR-POINT 2-D INTERPOLATION                                    ERROB.260
  130   CONTINUE                                                                 ERROB.261
        IF(ISPSTR(IN).NE.1)GOTO 150                                              ERROB.262
        DO 140 N=1,NSTA                                                          ERROB.263
          IOB=MAX0(1,IA(N))                                                      ERROB.264
          IOB=MIN0(IOB,ILX-1)                                                    ERROB.265
          JOB=MAX0(1,IB(N))                                                      ERROB.266
          JOB=MIN0(JOB,JLX-1)                                                    ERROB.267
          DXOB=RB(N)                                                             ERROB.268
          DYOB=RA(N)                                                             ERROB.269
          ERRF(5,N)=ERRF(5,N)+VAROBS(5,N)-((1.-DXOB)*((1.-DYOB)*                 ERROB.270
     +              PSB(IOB,JOB)+DYOB*PSB(IOB+1,JOB))+DXOB*((1.-DYOB)*           ERROB.271
     +              PSB(IOB,JOB+1)+DYOB*PSB(IOB+1,JOB+1)))                       ERROB.272
  140   CONTINUE                                                                 ERROB.273
  150 CONTINUE                                                                   ERROB.274
#ifdef MPP1                                                                      ERROB.275
#  include "mpp_errob_60.incl"                                                   ERROB.276
#endif                                                                           ERROB.277
C                                                                                ERROB.278
C DIFFERENCE BETWEEN OBS AND FCST IS COMPLETED                                   ERROB.279
C                                                                                ERROB.280
      IF(IN.EQ.1)THEN                                                            ERROB.281
        INPF=NPFI                                                                ERROB.282
      ELSE                                                                       ERROB.283
        FNPF=IRATIO**LEVIDN(IN)                                                  ERROB.284
        INPF=FNPF*NPFI                                                           ERROB.285
      ENDIF                                                                      ERROB.286
      IF(MOD(KTAU,INPF).NE.0)RETURN                                              ERROB.287
      PRINT1622,IN,KTAU                                                          ERROB.288
1622  FORMAT(38X,'*** OBS FDDA DIAGNOSTICS FOR INEST=',I2,' AT KTAU=',           ERROB.289
     1I5,' ***')                                                                 ERROB.290
      DO 1623 N=1,NSTA                                                           ERROB.291
        PRINT1443,N,IN,IA(N),IB(N),IC(N),ERRF(1,N),ERRF(2,N),ERRF(3,N),          ERROB.292
     +            ERRF(4,N),ERRF(5,N),TIMEOB(N)                                  ERROB.293
        PRINT1444,N,IN,RA(N),RB(N),RC(N),VAROBS(1,N),VAROBS(2,N),                ERROB.294
     +            VAROBS(3,N),VAROBS(4,N),VAROBS(5,N),TIMEOB(N)                  ERROB.295
 1623 CONTINUE                                                                   ERROB.296
      RETURN                                                                     ERROB.297
      END                                                                        ERROB.298
                                                                                 ERROB.299
