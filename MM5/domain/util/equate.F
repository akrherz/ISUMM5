C                                                                                EQUATE.1
      SUBROUTINE EQUATE(FIN,IXI,JXI,KXI,FOUT,IXO,JXO,KXO)                        EQUATE.2
      IMPLICIT NONE                                                              EQUATE.3
C                                                                                EQUATE.4
C     PURPOSE  PUT VALUES OF F-IN INTO F-OUT                                     EQUATE.5
C                                                                                EQUATE.6
      INTEGER IXI,JXI,KXI,IXO,JXO,KXO                                            EQUATE.7
      REAL    FIN(IXI,JXI,KXI),FOUT(IXO,JXO,KXO)                                 EQUATE.8
      INTEGER I,J,K                                                              EQUATE.9
      INTEGER ISMALL,JSMALL,KSMALL                                               EQUATE.10
C                                                                                EQUATE.11
C     ... USE SMALLEST DIMENSION SIZES                                           EQUATE.12
C                                                                                EQUATE.13
      ISMALL=MIN0(IXI,IXO)                                                       EQUATE.14
      JSMALL=MIN0(JXI,JXO)                                                       EQUATE.15
      KSMALL=MIN0(KXI,KXO)                                                       EQUATE.16
C                                                                                EQUATE.17
C     ... STORE DATA IN OTHER ARRAY                                              EQUATE.18
C                                                                                EQUATE.19
      DO 100 K=1,KSMALL                                                          EQUATE.20
        DO 100 J=1,JSMALL                                                        EQUATE.21
          DO 100 I=1,ISMALL                                                      EQUATE.22
            FOUT(I,J,K)=FIN(I,J,K)                                               EQUATE.23
  100     CONTINUE                                                               EQUATE.24
                                                                                 EQUATE.25
                                                                                 EQUATE.26
      RETURN                                                                     EQUATE.27
      END                                                                        EQUATE.28
                                                                                 EQUATE.29
      SUBROUTINE EQUATO(FIN,IXI,JXI,KXI,FOUT,IXO,JXO,KXO)                        24SEP99.335
      IMPLICIT NONE                                                              07NOV00.989
      INTEGER IXI,JXI,KXI,IXO,JXO,KXO                                            07NOV00.990
      REAL    FIN(IXI,JXI,KXI),FOUT(IXO,JXO,KXO)                                 07NOV00.991
      CALL EQUATE(FIN,IXI,JXI,KXI,FOUT,IXO,JXO,KXO)                              24SEP99.336
      RETURN                                                                     24SEP99.337
      END                                                                        24SEP99.338
