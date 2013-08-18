      SUBROUTINE EXAINT(TA,IYY,JXX,TAN,IYYN,JXXN,ISOUTH,                         EXAINT.1
     1            JWEST,ICRSDOT,MIXX,MJXX,PSCA)                                  EXAINT.2
C                                                                                EXAINT.3
C--- TA IS THE COARSE, TAN IS THE NEST, ISOUTH, JWEST ARE NESTI AND NEST         EXAINT.4
C---                                                                             EXAINT.5
C                                                                                EXAINT.6
C   PURPOSE       : GETS THE FIELD ON THE FINE GRID FROM THE FIELD ON            EXAINT.7
C                   ITS MOTHER GRID BY USING THE SEMI-LAGRANGIAN                 EXAINT.8
C                   INTERPOLATION SCHEME.                                        EXAINT.9
C   TA(IYY,JXX)   : THE FIELD ON THE MOTHER GRID.                                EXAINT.10
C   IYY,JXX       : THE DIMENSION OF THE MOTHER DOMAIN.                          EXAINT.11
C   IYYN,JXXN     : THE DIMENSIONS OF THE FINE DOMAIN.                           EXAINT.12
C   ISOUTH        : THE MOTHER DOMAIN GRID INDEX WHICH DEFINES THE               EXAINT.13
C                   SOUTHERN BOUNDARY OF THE FINE DOMAIN.                        EXAINT.14
C   JWEST         : THE MOTHER DOMAIN GRID INDEX  WHICH DEFINES THE              EXAINT.15
C                   WESTERN BOUNDARY OF THE FINE DOMAIN.                         EXAINT.16
C   ICRSDOT       : THE FLAG INDICATING WHETHER TA IS A CROSS (=1)               EXAINT.17
C                   OR DOT (=0) POINTS FIELD.                                    EXAINT.18
C   MIXX,MJXX     : THE MAXIMUM DIMENSIONS OF THE 'MAXNES' DOMAINS,              EXAINT.19
C                   INCLUDING THE EXPANDED COARSE DOMAIN IF                      EXAINT.20
C                   IEXP = .TRUE..                                               EXAINT.21
C   EXIT          :                                                              EXAINT.22
C   TAN(IYYN,JXXN): THE INTERPOLATED FIELD ON THE FINE GRID.                     EXAINT.23
C                                                                                EXAINT.24
C   CALLS THE SUBROUTINES SINT.                                                  EXAINT.25
C   CALLED BY SUBROUTINE TFUDGE.                                                 EXAINT.26
C                                                                                EXAINT.27
C   COMMENTS :                                                                   EXAINT.28
C THE NEST RATIO OF THE MOTHER GRID TO THE FINE GRID IS ALLOWED TO               EXAINT.29
C BE ONLY 3 IN THIS SUBROUTINE.  THE MIXX AND MJXX ARE USED TO                   EXAINT.30
C DEFINE THE WORKING ARRAYS PSCA, XIG, XJG, IGO, AND JGO                         EXAINT.31
C (GRELL ET AL. 1993). IF THE MOTHER DOMAIN IS THE DOMAIN 1                      EXAINT.32
C (THE COARSE DOMAIN), THE EXPANDED INCREMENTS MUST BE ADDED                     EXAINT.33
C TO THE ISOUTH AND JWEST IF THE COARSE DOMAIN IS EXPANDED                       EXAINT.34
C (IEXP = .TRUE.).                                                               EXAINT.35
# include <parame.incl>                                                          EXAINT.36
      PARAMETER(N1=JJMX,N2=IIMX)                                                 EXAINT.37
      DIMENSION TA(IYY,JXX),TAN(IYYN,JXXN)                                       EXAINT.38
      PARAMETER (NF=9)                                                           EXAINT.39
      COMMON /DEPAR2/ XIG(NF),XJG(NF),IG0(N1,N2),JG0(N1,N2)                      EXAINT.40
      DIMENSION PSCA(MJXX,MIXX,NF)                                               EXAINT.41
C                                                                                EXAINT.42
      IF (MJXX.GT.JJMX) THEN                                                     EXAINT.43
        PRINT 2,MJXX,JJMX                                                        EXAINT.44
2       FORMAT('==> IN EXAINT, MJXX=',I3,' > JJMX=',I3)                          EXAINT.45
        STOP                                                                     EXAINT.46
      ENDIF                                                                      EXAINT.47
      IF (MIXX.GT.IIMX) THEN                                                     EXAINT.48
        PRINT 3,MIXX,IIMX                                                        EXAINT.49
3       FORMAT('==> IN EXAINT, MIXX=',I3,' > IIMX=',I3)                          EXAINT.50
        STOP                                                                     EXAINT.51
      ENDIF                                                                      EXAINT.52
C                                                                                EXAINT.53
C--- FILL IN COMMON BLOCK                                                        EXAINT.54
      NONOS=1                                                                    EXAINT.55
      N1OS=MJXX*NONOS+1-NONOS                                                    EXAINT.56
      N2OS=MIXX*NONOS+1-NONOS                                                    EXAINT.57
C                                                                                EXAINT.58
      DO 11 I=1,3                                                                EXAINT.59
      DO 11 J=1,3                                                                EXAINT.60
      XIG(J+(I-1)*3)=1./3.-FLOAT(J-1)*1./3                                       EXAINT.61
      XJG(J+(I-1)*3)=1./3.-FLOAT(I-1)*1./3.                                      EXAINT.62
 11   CONTINUE                                                                   EXAINT.63
      DO 21 I=1,MIXX                                                             EXAINT.64
      DO 21 J=1,MJXX                                                             EXAINT.65
        IG0(J,I)=J                                                               EXAINT.66
        JG0(J,I)=I                                                               EXAINT.67
 21    CONTINUE                                                                  EXAINT.68
C                                                                                EXAINT.69
      JENDC=INT(FLOAT(JXXN-1)/3.+.001)+JWEST                                     EXAINT.70
      IENDC=INT(FLOAT(IYYN-1)/3.+.001)+ISOUTH                                    EXAINT.71
      JW2=JWEST-4                                                                EXAINT.72
      IS2=ISOUTH-5                                                               EXAINT.73
      JC2=JENDC+4                                                                EXAINT.74
      IC2=IENDC+5                                                                EXAINT.75
      JW3=JWEST-3                                                                EXAINT.76
      IS3=ISOUTH-4                                                               EXAINT.77
      JC3=JENDC+3                                                                EXAINT.78
      IC3=IENDC+4                                                                EXAINT.79
      DO 4 NFL=1,9                                                               EXAINT.80
      DO 4 I=1,MIXX                                                              EXAINT.81
      DO 4 J=1,MJXX                                                              EXAINT.82
        PSCA(J,I,NFL)=0.                                                         EXAINT.83
 4    CONTINUE                                                                   EXAINT.84
C                                                                                EXAINT.85
C--- INTERPOLATE FIELD, FIRST CROSS POINT                                        EXAINT.86
C                                                                                EXAINT.87
      IF(ICRSDOT.EQ.1)THEN                                                       EXAINT.88
      DO 1245 NFL=1,9                                                            EXAINT.89
      DO 1245 I=IS2,IC2                                                          EXAINT.90
      DO 1245 J=JW2,JC2                                                          EXAINT.91
        PSCA(J,I,NFL)=TA(I,J)                                                    EXAINT.92
1245  CONTINUE                                                                   EXAINT.93
      CALL SINT(PSCA,MJXX,MIXX,JW3,JC3,IS3,IC3)                                  EXAINT.94
      DO 1270 J=1,JXXN-1                                                         EXAINT.95
        J2=(J-1)/3+JWEST                                                         EXAINT.96
      DO 1250 I=1,IYYN-1                                                         EXAINT.97
        II=(I-1)/3+ISOUTH                                                        EXAINT.98
        JJ=MOD(I-1,3)*3+MOD(J-1,3)+1                                             EXAINT.99
        TAN(I,J)=PSCA(J2,II,JJ)                                                  EXAINT.100
1250  CONTINUE                                                                   EXAINT.101
1270  CONTINUE                                                                   EXAINT.102
C                                                                                EXAINT.103
      ELSE IF (ICRSDOT.EQ.0)THEN                                                 EXAINT.104
      DO 1220  NFL=1,9                                                           EXAINT.105
      DO 1220 I=IS2,IC2                                                          EXAINT.106
      DO 1220 J=JW2,JC2                                                          EXAINT.107
        PSCA(J,I,NFL)=TA(I,J)                                                    EXAINT.108
1220  CONTINUE                                                                   EXAINT.109
      CALL SINT(PSCA,MJXX,MIXX,JW3,JC3,IS3,IC3)                                  EXAINT.110
      DO 1240 J=1,JXXN                                                           EXAINT.111
        J2=J/3+JWEST                                                             EXAINT.112
      DO 1230 I=1,IYYN                                                           EXAINT.113
        II=I/3+ISOUTH                                                            EXAINT.114
        JJ=3*MOD(I,3)+MOD(J,3)+1                                                 EXAINT.115
        TAN(I,J)=PSCA(J2,II,JJ)                                                  EXAINT.116
1230  CONTINUE                                                                   EXAINT.117
1240  CONTINUE                                                                   EXAINT.118
      ENDIF                                                                      EXAINT.119
C                                                                                EXAINT.120
      RETURN                                                                     EXAINT.121
      END                                                                        EXAINT.122
