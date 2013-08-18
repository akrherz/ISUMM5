      SUBROUTINE PATCH(IX,JX,A,II,JJ,R,DS,SPVAL)                                 PATCH.1
C                                                                                PATCH.2
      DIMENSION A(IX,JX)                                                         PATCH.3
C                                                                                PATCH.4
      RR = R/DS                                                                  PATCH.5
      NIX1 = II - NINT(RR)                                                       PATCH.6
      NIX1 = MAX0(NIX1,   1)                                                     PATCH.7
      NIX2 = II + NINT(RR)                                                       PATCH.8
      NIX2 = MIN0(NIX2,IX-1)                                                     PATCH.9
      NJX1 = JJ - NINT(RR)                                                       PATCH.10
      NJX1 = MAX0(NJX1,   1)                                                     PATCH.11
      NJX2 = JJ + NINT(RR)                                                       PATCH.12
      NJX2 = MIN0(NJX2,JX-1)                                                     PATCH.13
C                                                                                PATCH.14
      SUM = 0.0                                                                  PATCH.15
      NUMP = 0                                                                   PATCH.16
      DO I = NIX1,NIX2                                                           PATCH.17
      DO J = NJX1,NJX2                                                           PATCH.18
        IF (A(I,J).NE.SPVAL.AND.A(I,J).GE.0.) THEN                               05DEC01.12
          SUM = SUM + A(I,J)                                                     PATCH.20
          NUMP = NUMP + 1                                                        PATCH.21
        ENDIF                                                                    PATCH.22
      END DO                                                                     PATCH.23
      END DO                                                                     PATCH.24
C                                                                                PATCH.25
      IF (NUMP.GT.0) A(II,JJ) = SUM/FLOAT(NUMP)                                  PATCH.26
CC      PRINT 10,II,JJ,A(II,JJ),NUMP                                             PATCH.27
 10   FORMAT('==> POINT(',I3,',',I3,') PATCHED ',F8.2,I6)                        PATCH.28
C                                                                                PATCH.29
      RETURN                                                                     PATCH.30
      END                                                                        PATCH.31
                                                                                 PATCH.32
