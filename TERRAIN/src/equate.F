      SUBROUTINE EQUATE(A,B,IX,JX,KX,NAME)                                       EQUATE.1
C                                                                                EQUATE.2
      DIMENSION A(IX,JX,KX), B(IX,JX,KX)                                         EQUATE.3
      CHARACTER*8 NAME                                                           EQUATE.4
C                                                                                EQUATE.5
      DO K = 1,KX                                                                EQUATE.6
C                                                                                EQUATE.7
      DO I = 1,IX                                                                EQUATE.8
      DO J = 1,JX                                                                EQUATE.9
        B(I,J,K) = A(I,J,K)                                                      EQUATE.10
      END DO                                                                     EQUATE.11
      END DO                                                                     EQUATE.12
C                                                                                EQUATE.13
      END DO                                                                     EQUATE.14
C                                                                                EQUATE.15
C     PRINT 10,NAME                                                              EQUATE.16
 10   FORMAT(5X,'==> FIELD: ',A8,' READ IN')                                     EQUATE.17
C                                                                                EQUATE.18
      RETURN                                                                     EQUATE.19
      END                                                                        EQUATE.20
