      FUNCTION BINT(XX,YY,LIST,III,JJJ,FLAG)                                     BINT.1
C                                                                                BINT.2
C   PERPUSE : PERFORM THE OVERLAPPING-QUADRATIC INTERPOLATION TO                 BINT.3
C             THE MESOSCALE GRID POINT (XX, YY) FROM 16 SURROUNDING              BINT.4
C             GRID POINTS.                                                       BINT.5
C                                                                                BINT.6
C   LIST(III,JJJ):   THE INPUT SOURCE TERRAIN HEIGHT OR LAND-USE DATA,           BINT.7
C                    NOT ON THE MESOSCALE GRID.                                  BINT.8
C   XX,YY        :   X AND Y INDEX VALUES (INTERPOLATION LOCATION) OF            BINT.9
C                    THE MESOSCALE GRID POINT IN THE COORDINATE SYSTEM           BINT.10
C                    OF THE INPUT GRID.                                          BINT.11
C   FLAG         :   FLAG = .FALSE.: 16-POINT INTERPOLATION;                     BINT.12
C                    FLAG = .TRUE.: 4-POINT INTERPOLATION.                       BINT.13
C   BINT         :   THE INTERPOLATED VALUE OF THE VARIABLE IN LIST,             BINT.14
C                    AT POINT (XX, YY).                                          BINT.15
C                                                                                BINT.16
C   CALLS FUNCTION ONED.                                                         BINT.17
C   CALLED BY SUBROUTINES CRLND AND INTERP.                                      BINT.18
C                                                                                BINT.19
C   COMMENTS :                                                                   BINT.20
C                                                                                BINT.21
C SUBROUTINE BINT IS CALLED FOR EACH MESOSCALE GRID POINT IN EACH                BINT.22
C FIELD. FOR EACH MESOSCALE GRID POINT, BINT CONSTRUCTS THE ARRAY                BINT.23
C STL(4,4) WHICH CONTAINS THE VALUES AT THE SIXTEEN SURROUNDING INPUT            BINT.24
C GRID POINTS.  NEXT, BINT CALLS FUNCTION ONED FOUR TIMES TO CALCULATE           BINT.25
C FOUR INTERPOLATED VALUES ALONG THE LINE X = XX.  FINALLY, BINT CALLS           BINT.26
C FUNCTION ONED ONE MORE TIME TO INTERPOLATE FROM THE FOUR NEW VALUES            BINT.27
C TO THE POINT (XX,YY). IN CASE OF FLAG = .TRUE., THE OUTER 12 ELEMENTS          BINT.28
C OF ARRAY STL ARE ALWAYS SET TO ZERO. ONLY THE CENTRAL 4 ELEMENTS               BINT.29
C HAVE THE VALUES FROM LIST AND FUNCTION ONED WILL COMPLETE THE LINEAR           BINT.30
C INTERPOLATION. IN THE TERRAIN PROGRAM, THE FLAG IS ALWAYS SET TO BE            BINT.31
C .FALSE..                                                                       BINT.32
C                                                                                BINT.33
      REAL LIST(III,JJJ),STL(4,4)                                                BINT.34
      LOGICAL FLAG                                                               BINT.35
C                                                                                BINT.36
      BINT = 0.0                                                                 BINT.37
      N    = 0                                                                   BINT.38
      I    = INT(XX+0.00001)                                                     BINT.39
      J    = INT(YY+0.00001)                                                     BINT.40
      X    = XX-I                                                                BINT.41
      Y    = YY-J                                                                BINT.42
      IF (ABS(X).GT.0.0001.OR.ABS(Y).GT.0.0001) GOTO 10                          BINT.43
      BINT = LIST(I,J)                                                           BINT.44
      RETURN                                                                     BINT.45
   10 CONTINUE                                                                   BINT.46
      DO 20 K=1,4                                                                BINT.47
      KK = I+K-2                                                                 BINT.48
      DO 20 L=1,4                                                                BINT.49
      STL(K,L) = 0.                                                              BINT.50
      IF (FLAG.AND.(L.EQ.1)) GOTO 20                                             BINT.51
      IF (FLAG.AND.(L.EQ.4)) GOTO 20                                             BINT.52
      IF (FLAG.AND.(K.EQ.1)) GOTO 20                                             BINT.53
      IF (FLAG.AND.(K.EQ.4)) GOTO 20                                             BINT.54
      LL = J+L-2                                                                 BINT.55
      IF (KK.LT.1.OR.KK.GT.III) GOTO 20                                          BINT.56
      IF (LL.GT.JJJ.OR.LL.LT.1) GOTO 20                                          BINT.57
      IF (LIST(KK,LL).EQ.-999.9) GOTO 20                                         BINT.58
      STL(K,L) = LIST(KK,LL)                                                     BINT.59
      N = N+1                                                                    BINT.60
      IF (STL(K,L).LE.0.0) STL(K,L) = -1.E-10                                    BINT.61
   20 CONTINUE                                                                   BINT.62
C                                                                                BINT.63
C-----FIND INDEX OF CLOSEST POINT TO XX,YY.                                      BINT.64
C                                                                                BINT.65
      KNEAR = FLOAT(2) + X + 0.5                                                 BINT.66
      LNEAR = FLOAT(2) + Y + 0.5                                                 BINT.67
      A = ONED(X,STL(1,1),STL(2,1),STL(3,1),STL(4,1))                            BINT.68
      B = ONED(X,STL(1,2),STL(2,2),STL(3,2),STL(4,2))                            BINT.69
      C = ONED(X,STL(1,3),STL(2,3),STL(3,3),STL(4,3))                            BINT.70
      D = ONED(X,STL(1,4),STL(2,4),STL(3,4),STL(4,4))                            BINT.71
      BINT = ONED(Y,A,B,C,D)                                                     BINT.72
C                                                                                BINT.73
C--------IF CLOSEST POINT IS OCEAN, AUTOMATICALLY RESET TERRAIN TO               BINT.74
C--------PRESERVE COASTLINE.                                                     BINT.75
C                                                                                BINT.76
      IF (N.EQ.16) RETURN                                                        BINT.77
      IF (FLAG.AND.N.EQ.4) RETURN                                                BINT.78
      E = ONED(Y,STL(1,1),STL(1,2),STL(1,3),STL(1,4))                            BINT.79
      F = ONED(Y,STL(2,1),STL(2,2),STL(2,3),STL(2,4))                            BINT.80
      G = ONED(Y,STL(3,1),STL(3,2),STL(3,3),STL(3,4))                            BINT.81
      H = ONED(Y,STL(4,1),STL(4,2),STL(4,3),STL(4,4))                            BINT.82
      BINT = (BINT+ONED(X,E,F,G,H))/2.                                           BINT.83
      RETURN                                                                     BINT.84
      END                                                                        BINT.85
