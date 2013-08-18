      SUBROUTINE FILLCRS(HSCR1,IY1,JX1,KZ1,IYY,JXX,KZZ)                          FILLCRS.1
C                                                                                FILLCRS.2
C     PURPOSE  FILL OUTER ROW AND COLUMN OF CROSS POINT FIELD FOR OUTPUT         FILLCRS.3
C                                                                                FILLCRS.4
      IMPLICIT NONE                                                              FILLCRS.5
      INTEGER I,J,K,IYY,JXX,KZZ,IY1,JX1,KZ1                                      FILLCRS.6
      REAL    HSCR1(IY1,JX1,KZ1)                                                 FILLCRS.7
                                                                                 FILLCRS.8
      DO 100 K=1,KZZ                                                             FILLCRS.9
        DO 10 I=1,IYY-1                                                          FILLCRS.10
          HSCR1(I,JXX,K)=HSCR1(I,JXX-1,K)                                        FILLCRS.11
   10   CONTINUE                                                                 FILLCRS.12
        DO 20 J=1,JXX                                                            FILLCRS.13
          HSCR1(IYY,J,K)=HSCR1(IYY-1,J,K)                                        FILLCRS.14
   20   CONTINUE                                                                 FILLCRS.15
  100 CONTINUE                                                                   FILLCRS.16
      RETURN                                                                     FILLCRS.17
      END                                                                        FILLCRS.18
