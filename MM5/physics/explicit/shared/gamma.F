C REV PROCESSED 213 LINES OF CODE. PROGRAM DONE.                                 GAMMA.1
      REAL FUNCTION GAMMA(X)                                                     GAMMA.2
CD    DOUBLE PRECISION FUNCTION DGAMMA(X)                                        GAMMA.3
C----------------------------------------------------------------------          GAMMA.4
C                                                                                GAMMA.5
C THIS ROUTINE CALCULATES THE GAMMA FUNCTION FOR A REAL ARGUMENT X.              GAMMA.6
C   COMPUTATION IS BASED ON AN ALGORITHM OUTLINED IN REFERENCE 1.                GAMMA.7
C   THE PROGRAM USES RATIONAL FUNCTIONS THAT APPROXIMATE THE GAMMA               GAMMA.8
C   FUNCTION TO AT LEAST 20 SIGNIFICANT DECIMAL DIGITS.  COEFFICIENTS            GAMMA.9
C   FOR THE APPROXIMATION OVER THE INTERVAL (1,2) ARE UNPUBLISHED.               GAMMA.10
C   THOSE FOR THE APPROXIMATION FOR X .GE. 12 ARE FROM REFERENCE 2.              GAMMA.11
C   THE ACCURACY ACHIEVED DEPENDS ON THE ARITHMETIC SYSTEM, THE                  GAMMA.12
C   COMPILER, THE INTRINSIC FUNCTIONS, AND PROPER SELECTION OF THE               GAMMA.13
C   MACHINE-DEPENDENT CONSTANTS.                                                 GAMMA.14
C                                                                                GAMMA.15
C                                                                                GAMMA.16
C*******************************************************************             GAMMA.17
C*******************************************************************             GAMMA.18
C                                                                                GAMMA.19
C EXPLANATION OF MACHINE-DEPENDENT CONSTANTS                                     GAMMA.20
C                                                                                GAMMA.21
C BETA   - RADIX FOR THE FLOATING-POINT REPRESENTATION                           GAMMA.22
C MAXEXP - THE SMALLEST POSITIVE POWER OF BETA THAT OVERFLOWS                    GAMMA.23
C XBIG   - THE LARGEST ARGUMENT FOR WHICH GAMMA(X) IS REPRESENTABLE              GAMMA.24
C          IN THE MACHINE, I.E., THE SOLUTION TO THE EQUATION                    GAMMA.25
C                  GAMMA(XBIG) = BETA**MAXEXP                                    GAMMA.26
C XINF   - THE LARGEST MACHINE REPRESENTABLE FLOATING-POINT NUMBER;              GAMMA.27
C          APPROXIMATELY BETA**MAXEXP                                            GAMMA.28
C EPS    - THE SMALLEST POSITIVE FLOATING-POINT NUMBER SUCH THAT                 GAMMA.29
C          1.0+EPS .GT. 1.0                                                      GAMMA.30
C XMININ - THE SMALLEST POSITIVE FLOATING-POINT NUMBER SUCH THAT                 GAMMA.31
C          1/XMININ IS MACHINE REPRESENTABLE                                     GAMMA.32
C                                                                                GAMMA.33
C     APPROXIMATE VALUES FOR SOME IMPORTANT MACHINES ARE:                        GAMMA.34
C                                                                                GAMMA.35
C                            BETA       MAXEXP        XBIG                       GAMMA.36
C                                                                                GAMMA.37
C CRAY-1         (S.P.)        2         8191        966.961                     GAMMA.38
C CYBER 180/855                                                                  GAMMA.39
C   UNDER NOS    (S.P.)        2         1070        177.803                     GAMMA.40
C IEEE (IBM/XT,                                                                  GAMMA.41
C   SUN, ETC.)   (S.P.)        2          128        35.040                      GAMMA.42
C IEEE (IBM/XT,                                                                  GAMMA.43
C   SUN, ETC.)   (D.P.)        2         1024        171.624                     GAMMA.44
C IBM 3033       (D.P.)       16           63        57.574                      GAMMA.45
C VAX D-FORMAT   (D.P.)        2          127        34.844                      GAMMA.46
C VAX G-FORMAT   (D.P.)        2         1023        171.489                     GAMMA.47
C                                                                                GAMMA.48
C                            XINF         EPS        XMININ                      GAMMA.49
C                                                                                GAMMA.50
C CRAY-1         (S.P.)   5.45E+2465   7.11E-15    1.84E-2466                    GAMMA.51
C CYBER 180/855                                                                  GAMMA.52
C   UNDER NOS    (S.P.)   1.26E+322    3.55E-15    3.14E-294                     GAMMA.53
C IEEE (IBM/XT,                                                                  GAMMA.54
C   SUN, ETC.)   (S.P.)   3.40E+38     1.19E-7     1.18E-38                      GAMMA.55
C IEEE (IBM/XT,                                                                  GAMMA.56
C   SUN, ETC.)   (D.P.)   1.79D+308    2.22D-16    2.23D-308                     GAMMA.57
C IBM 3033       (D.P.)   7.23D+75     2.22D-16    1.39D-76                      GAMMA.58
C VAX D-FORMAT   (D.P.)   1.70D+38     1.39D-17    5.88D-39                      GAMMA.59
C VAX G-FORMAT   (D.P.)   8.98D+307    1.11D-16    1.12D-308                     GAMMA.60
C                                                                                GAMMA.61
C*******************************************************************             GAMMA.62
C*******************************************************************             GAMMA.63
C                                                                                GAMMA.64
C ERROR RETURNS                                                                  GAMMA.65
C                                                                                GAMMA.66
C  THE PROGRAM RETURNS THE VALUE XINF FOR SINGULARITIES OR                       GAMMA.67
C     WHEN OVERFLOW WOULD OCCUR.  THE COMPUTATION IS BELIEVED                    GAMMA.68
C     TO BE FREE OF UNDERFLOW AND OVERFLOW.                                      GAMMA.69
C                                                                                GAMMA.70
C                                                                                GAMMA.71
C  INTRINSIC FUNCTIONS REQUIRED ARE:                                             GAMMA.72
C                                                                                GAMMA.73
C     INT, DBLE, EXP, LOG, REAL, SIN                                             GAMMA.74
C                                                                                GAMMA.75
C                                                                                GAMMA.76
C REFERENCES:  AN OVERVIEW OF SOFTWARE DEVELOPMENT FOR SPECIAL                   GAMMA.77
C              FUNCTIONS   W. J. CODY, LECTURE NOTES IN MATHEMATICS,             GAMMA.78
C              506, NUMERICAL ANALYSIS DUNDEE, 1975, G. A. WATSON                GAMMA.79
C              (ED.), SPRINGER VERLAG, BERLIN, 1976.                             GAMMA.80
C                                                                                GAMMA.81
C              COMPUTER APPROXIMATIONS, HART, ET. AL., WILEY AND                 GAMMA.82
C              SONS, NEW YORK, 1968.                                             GAMMA.83
C                                                                                GAMMA.84
C  LATEST MODIFICATION: OCTOBER 12, 1989                                         GAMMA.85
C                                                                                GAMMA.86
C  AUTHORS: W. J. CODY AND L. STOLTZ                                             GAMMA.87
C           APPLIED MATHEMATICS DIVISION                                         GAMMA.88
C           ARGONNE NATIONAL LABORATORY                                          GAMMA.89
C           ARGONNE, IL 60439                                                    GAMMA.90
C                                                                                GAMMA.91
C----------------------------------------------------------------------          GAMMA.92
      INTEGER I,N                                                                GAMMA.93
      LOGICAL PARITY                                                             GAMMA.94
      REAL                                                                       GAMMA.95
CD    DOUBLE PRECISION                                                           GAMMA.96
     1    C,CONV,EPS,FACT,HALF,ONE,P,PI,Q,RES,SQRTPI,SUM,TWELVE,                 GAMMA.97
     2    TWO,X,XBIG,XDEN,XINF,XMININ,XNUM,Y,Y1,YSQ,Z,ZERO                       GAMMA.98
      DIMENSION C(7),P(8),Q(8)                                                   GAMMA.99
C----------------------------------------------------------------------          GAMMA.100
C  MATHEMATICAL CONSTANTS                                                        GAMMA.101
C----------------------------------------------------------------------          GAMMA.102
      DATA ONE,HALF,TWELVE,TWO,ZERO/1.0E0,0.5E0,12.0E0,2.0E0,0.0E0/,             GAMMA.103
     1     SQRTPI/0.9189385332046727417803297E0/,                                GAMMA.104
     2     PI/3.1415926535897932384626434E0/                                     GAMMA.105
CD    DATA ONE,HALF,TWELVE,TWO,ZERO/1.0D0,0.5D0,12.0D0,2.0D0,0.0D0/,             GAMMA.106
CD   1     SQRTPI/0.9189385332046727417803297D0/,                                GAMMA.107
CD   2     PI/3.1415926535897932384626434D0/                                     GAMMA.108
C----------------------------------------------------------------------          GAMMA.109
C  MACHINE DEPENDENT PARAMETERS                                                  GAMMA.110
C----------------------------------------------------------------------          GAMMA.111
      DATA XBIG,XMININ,EPS/35.040E0,1.18E-38,1.19E-7/,                           GAMMA.112
     1     XINF/3.4E38/                                                          GAMMA.113
CD    DATA XBIG,XMININ,EPS/171.624D0,2.23D-308,2.22D-16/,                        GAMMA.114
CD   1     XINF/1.79D308/                                                        GAMMA.115
C----------------------------------------------------------------------          GAMMA.116
C  NUMERATOR AND DENOMINATOR COEFFICIENTS FOR RATIONAL MINIMAX                   GAMMA.117
C     APPROXIMATION OVER (1,2).                                                  GAMMA.118
C----------------------------------------------------------------------          GAMMA.119
      DATA P/-1.71618513886549492533811E+0,2.47656508055759199108314E+1,         GAMMA.120
     1       -3.79804256470945635097577E+2,6.29331155312818442661052E+2,         GAMMA.121
     2       8.66966202790413211295064E+2,-3.14512729688483675254357E+4,         GAMMA.122
     3       -3.61444134186911729807069E+4,6.64561438202405440627855E+4/         GAMMA.123
      DATA Q/-3.08402300119738975254353E+1,3.15350626979604161529144E+2,         GAMMA.124
     1      -1.01515636749021914166146E+3,-3.10777167157231109440444E+3,         GAMMA.125
     2        2.25381184209801510330112E+4,4.75584627752788110767815E+3,         GAMMA.126
     3      -1.34659959864969306392456E+5,-1.15132259675553483497211E+5/         GAMMA.127
CD    DATA P/-1.71618513886549492533811D+0,2.47656508055759199108314D+1,         GAMMA.128
CD   1       -3.79804256470945635097577D+2,6.29331155312818442661052D+2,         GAMMA.129
CD   2       8.66966202790413211295064D+2,-3.14512729688483675254357D+4,         GAMMA.130
CD   3       -3.61444134186911729807069D+4,6.64561438202405440627855D+4/         GAMMA.131
CD    DATA Q/-3.08402300119738975254353D+1,3.15350626979604161529144D+2,         GAMMA.132
CD   1      -1.01515636749021914166146D+3,-3.10777167157231109440444D+3,         GAMMA.133
CD   2        2.25381184209801510330112D+4,4.75584627752788110767815D+3,         GAMMA.134
CD   3      -1.34659959864969306392456D+5,-1.15132259675553483497211D+5/         GAMMA.135
C----------------------------------------------------------------------          GAMMA.136
C  COEFFICIENTS FOR MINIMAX APPROXIMATION OVER (12, INF).                        GAMMA.137
C----------------------------------------------------------------------          GAMMA.138
      DATA C/-1.910444077728E-03,8.4171387781295E-04,                            GAMMA.139
     1     -5.952379913043012E-04,7.93650793500350248E-04,                       GAMMA.140
     2     -2.777777777777681622553E-03,8.333333333333333331554247E-02,          GAMMA.141
     3      5.7083835261E-03/                                                    GAMMA.142
CD    DATA C/-1.910444077728D-03,8.4171387781295D-04,                            GAMMA.143
CD   1     -5.952379913043012D-04,7.93650793500350248D-04,                       GAMMA.144
CD   2     -2.777777777777681622553D-03,8.333333333333333331554247D-02,          GAMMA.145
CD   3      5.7083835261D-03/                                                    GAMMA.146
C----------------------------------------------------------------------          GAMMA.147
C  STATEMENT FUNCTIONS FOR CONVERSION BETWEEN INTEGER AND FLOAT                  GAMMA.148
C----------------------------------------------------------------------          GAMMA.149
      CONV(I) = REAL(I)                                                          GAMMA.150
CD    CONV(I) = DBLE(I)                                                          GAMMA.151
      PARITY=.FALSE.                                                             GAMMA.152
      FACT=ONE                                                                   GAMMA.153
      N=0                                                                        GAMMA.154
      Y=X                                                                        GAMMA.155
      IF(Y.LE.ZERO)THEN                                                          GAMMA.156
C----------------------------------------------------------------------          GAMMA.157
C  ARGUMENT IS NEGATIVE                                                          GAMMA.158
C----------------------------------------------------------------------          GAMMA.159
        Y=-X                                                                     GAMMA.160
        Y1=AINT(Y)                                                               GAMMA.161
        RES=Y-Y1                                                                 GAMMA.162
        IF(RES.NE.ZERO)THEN                                                      GAMMA.163
          IF(Y1.NE.AINT(Y1*HALF)*TWO)PARITY=.TRUE.                               GAMMA.164
          FACT=-PI/SIN(PI*RES)                                                   GAMMA.165
          Y=Y+ONE                                                                GAMMA.166
        ELSE                                                                     GAMMA.167
          RES=XINF                                                               GAMMA.168
          GOTO 900                                                               GAMMA.169
        ENDIF                                                                    GAMMA.170
      ENDIF                                                                      GAMMA.171
C----------------------------------------------------------------------          GAMMA.172
C  ARGUMENT IS POSITIVE                                                          GAMMA.173
C----------------------------------------------------------------------          GAMMA.174
      IF(Y.LT.EPS)THEN                                                           GAMMA.175
C----------------------------------------------------------------------          GAMMA.176
C  ARGUMENT .LT. EPS                                                             GAMMA.177
C----------------------------------------------------------------------          GAMMA.178
        IF(Y.GE.XMININ)THEN                                                      GAMMA.179
          RES=ONE/Y                                                              GAMMA.180
        ELSE                                                                     GAMMA.181
          RES=XINF                                                               GAMMA.182
          GOTO 900                                                               GAMMA.183
        ENDIF                                                                    GAMMA.184
      ELSEIF(Y.LT.TWELVE)THEN                                                    GAMMA.185
        Y1=Y                                                                     GAMMA.186
        IF(Y.LT.ONE)THEN                                                         GAMMA.187
C----------------------------------------------------------------------          GAMMA.188
C  0.0 .LT. ARGUMENT .LT. 1.0                                                    GAMMA.189
C----------------------------------------------------------------------          GAMMA.190
          Z=Y                                                                    GAMMA.191
          Y=Y+ONE                                                                GAMMA.192
        ELSE                                                                     GAMMA.193
C----------------------------------------------------------------------          GAMMA.194
C  1.0 .LT. ARGUMENT .LT. 12.0, REDUCE ARGUMENT IF NECESSARY                     GAMMA.195
C----------------------------------------------------------------------          GAMMA.196
          N=INT(Y)-1                                                             GAMMA.197
          Y=Y-CONV(N)                                                            GAMMA.198
          Z=Y-ONE                                                                GAMMA.199
        ENDIF                                                                    GAMMA.200
C----------------------------------------------------------------------          GAMMA.201
C  EVALUATE APPROXIMATION FOR 1.0 .LT. ARGUMENT .LT. 2.0                         GAMMA.202
C----------------------------------------------------------------------          GAMMA.203
        XNUM=ZERO                                                                GAMMA.204
        XDEN=ONE                                                                 GAMMA.205
        DO 260 I=1,8                                                             GAMMA.206
          XNUM=(XNUM+P(I))*Z                                                     GAMMA.207
          XDEN=XDEN*Z+Q(I)                                                       GAMMA.208
  260   CONTINUE                                                                 GAMMA.209
        RES=XNUM/XDEN+ONE                                                        GAMMA.210
        IF(Y1.LT.Y)THEN                                                          GAMMA.211
C----------------------------------------------------------------------          GAMMA.212
C  ADJUST RESULT FOR CASE  0.0 .LT. ARGUMENT .LT. 1.0                            GAMMA.213
C----------------------------------------------------------------------          GAMMA.214
          RES=RES/Y1                                                             GAMMA.215
        ELSEIF(Y1.GT.Y)THEN                                                      GAMMA.216
C----------------------------------------------------------------------          GAMMA.217
C  ADJUST RESULT FOR CASE  2.0 .LT. ARGUMENT .LT. 12.0                           GAMMA.218
C----------------------------------------------------------------------          GAMMA.219
          DO 290 I=1,N                                                           GAMMA.220
            RES=RES*Y                                                            GAMMA.221
            Y=Y+ONE                                                              GAMMA.222
  290     CONTINUE                                                               GAMMA.223
        ENDIF                                                                    GAMMA.224
      ELSE                                                                       GAMMA.225
C----------------------------------------------------------------------          GAMMA.226
C  EVALUATE FOR ARGUMENT .GE. 12.0,                                              GAMMA.227
C----------------------------------------------------------------------          GAMMA.228
        IF(Y.LE.XBIG)THEN                                                        GAMMA.229
          YSQ=Y*Y                                                                GAMMA.230
          SUM=C(7)                                                               GAMMA.231
          DO 350 I=1,6                                                           GAMMA.232
            SUM=SUM/YSQ+C(I)                                                     GAMMA.233
  350     CONTINUE                                                               GAMMA.234
          SUM=SUM/Y-Y+SQRTPI                                                     GAMMA.235
          SUM=SUM+(Y-HALF)*LOG(Y)                                                GAMMA.236
          RES=EXP(SUM)                                                           GAMMA.237
        ELSE                                                                     GAMMA.238
          RES=XINF                                                               GAMMA.239
          GOTO 900                                                               GAMMA.240
        ENDIF                                                                    GAMMA.241
      ENDIF                                                                      GAMMA.242
C----------------------------------------------------------------------          GAMMA.243
C  FINAL ADJUSTMENTS AND RETURN                                                  GAMMA.244
C----------------------------------------------------------------------          GAMMA.245
      IF(PARITY)RES=-RES                                                         GAMMA.246
      IF(FACT.NE.ONE)RES=FACT/RES                                                GAMMA.247
  900 GAMMA=RES                                                                  GAMMA.248
CD900 DGAMMA = RES                                                               GAMMA.249
      RETURN                                                                     GAMMA.250
C ---------- LAST LINE OF GAMMA ----------                                       GAMMA.251
      END                                                                        GAMMA.252
                                                                                 GAMMA.253
