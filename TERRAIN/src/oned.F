      FUNCTION ONED(X,A,B,C,D)                                                   ONED.1
      ONED = 0.                                                                  ONED.2
      IF (X.EQ.0.) ONED = B                                                      ONED.3
      IF (X.EQ.1.) ONED = C                                                      ONED.4
      IF (B*C.EQ.0.) RETURN                                                      ONED.5
      IF (A*D.EQ.0.) GOTO 20                                                     ONED.6
      ONED = (1.0-X)*(B+X*(0.5*(C-A)+X*(0.5*(C+A)-B)))+X*(C+(1.0-X)*             ONED.7
     1   (0.5*(B-D)+(1.0-X)*(0.5*(B+D)-C)))                                      ONED.8
      RETURN                                                                     ONED.9
   20 ONED = B*(1.0-X)+C*X                                                       ONED.10
      IF (A.NE.0.0) ONED = B+X*(0.5*(C-A)+X*(0.5*(C+A)-B))                       ONED.11
      IF (D.NE.0.0) ONED = C+(1.0-X)*(0.5*(B-D)+(1.0-X)*(0.5*(B+D)-C))           ONED.12
      RETURN                                                                     ONED.13
      END                                                                        ONED.14
