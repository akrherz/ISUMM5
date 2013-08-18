#ifndef MPP1                                                                     DECOUPLE.1
      SUBROUTINE DECOUPLE(FIN,IXI,JXI,KXI,PS,FOUT,IXO,JXO,KXO)                   DECOUPLE.2
      IMPLICIT NONE                                                              DECOUPLE.3
C                                                                                DECOUPLE.4
C     PURPOSE  PUT VALUES OF F-IN INTO F-OUT AND DIVIDE                          DECOUPLE.5
C              F-IN BY PS TO DECOUPLE THE FIELD                                  DECOUPLE.6
C                                                                                DECOUPLE.7
      INTEGER IXI,JXI,KXI,IXO,JXO,KXO,ISMALL,JSMALL,KSMALL                       DECOUPLE.8
      INTEGER I,J,K                                                              DECOUPLE.9
      REAL      FIN(IXI,JXI,KXI),FOUT(IXO,JXO,KXO),                              DECOUPLE.10
     -           PS(IXI,JXI)                                                     DECOUPLE.11
C                                                                                DECOUPLE.12
C     ... USE SMALLEST DIMENSION SIZES                                           DECOUPLE.13
C                                                                                DECOUPLE.14
      ISMALL=MIN0(IXI,IXO)                                                       DECOUPLE.15
      JSMALL=MIN0(JXI,JXO)                                                       DECOUPLE.16
      KSMALL=MIN0(KXI,KXO)                                                       DECOUPLE.17
C                                                                                DECOUPLE.18
C     ... STORE DATA IN OTHER ARRAY                                              DECOUPLE.19
C                                                                                DECOUPLE.20
c$omp parallel do default(shared)                                                DECOUPLE.21
c$omp&private(i,j,k)                                                             DECOUPLE.22
      DO 100 J=1,JSMALL                                                          DECOUPLE.23
        DO 100 K=1,KSMALL                                                        DECOUPLE.24
          DO 100 I=1,ISMALL                                                      DECOUPLE.25
            FOUT(I,J,K)=FIN(I,J,K)/PS(I,J)                                       DECOUPLE.26
  100     CONTINUE                                                               DECOUPLE.27
                                                                                 DECOUPLE.28
      RETURN                                                                     DECOUPLE.29
      END                                                                        DECOUPLE.30
#else                                                                            DECOUPLE.31
      SUBROUTINE DECOUPLE(FIN,MIX,MJX,MKX,PS,FOUT,IXO,JXO,KXO)                   DECOUPLE.32
      IMPLICIT NONE                                                              DECOUPLE.33
C                                                                                DECOUPLE.34
C     PURPOSE  PUT VALUES OF F-IN INTO F-OUT AND DIVIDE                          DECOUPLE.35
C              F-IN BY PS TO DECOUPLE THE FIELD                                  DECOUPLE.36
C                                                                                DECOUPLE.37
      INTEGER MIX,MJX,MKX,IXO,JXO,KXO,ISMALL,JSMALL,KSMALL                       DECOUPLE.38
      INTEGER I,J,K                                                              DECOUPLE.39
                                                                                 DECOUPLE.40
      REAL      FIN(MIX,MJX,MKX),FOUT(MIX,MJX,MKX),                              DECOUPLE.41
     -           PS(MIX,MJX)                                                     DECOUPLE.42
C                                                                                DECOUPLE.43
C ON MPP THIS IS JUST A COPY, AND ASSUMES SIZE OF OUTPUT VAR                     DECOUPLE.44
C                                                                                DECOUPLE.45
      ISMALL=IXO                                                                 DECOUPLE.46
      JSMALL=JXO                                                                 DECOUPLE.47
      KSMALL=KXO                                                                 DECOUPLE.48
C                                                                                DECOUPLE.49
C     ... STORE DATA IN OTHER ARRAY                                              DECOUPLE.50
C                                                                                DECOUPLE.51
      DO 100 K=1,KSMALL                                                          DECOUPLE.52
        DO 100 J=1,JSMALL                                                        DECOUPLE.53
          DO 100 I=1,ISMALL                                                      DECOUPLE.54
            IF(PS(I,J).NE.0.)FOUT(I,J,K)=FIN(I,J,K)/PS(I,J)                      DECOUPLE.55
  100     CONTINUE                                                               DECOUPLE.56
                                                                                 DECOUPLE.57
      RETURN                                                                     DECOUPLE.58
      END                                                                        DECOUPLE.59
#endif                                                                           DECOUPLE.60
