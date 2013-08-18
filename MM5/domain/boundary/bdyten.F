      SUBROUTINE BDYTEN(TTEN,KZZ,IEND,JEND,INEST,IBMOIST,                        BDYTEN.1
     &                  TSBT,TNBT,TEBT,TWBT)                                     BDYTEN.2
      IMPLICIT NONE                                                              BDYTEN.3
# include <parame.incl>                                                          BDYTEN.4
      INTEGER KZZ,IEND,JEND,INEST,IBMOIST                                        BDYTEN.5
      REAL      TTEN(MIX,MJX,KZZ)                                                BDYTEN.6
      REAL      TEBT(MIX,KZZ,NSPGX), TWBT(MIX,KZZ,NSPGX),                        BDYTEN.7
     1          TNBT(MJX,KZZ,NSPGX), TSBT(MJX,KZZ,NSPGX)                         BDYTEN.8
      INTEGER NB,NB1,IS,IE,JS,JE,LB                                              BDYTEN.9
      INTEGER I,J,K,KL                                                           BDYTEN.10
C                                                                                BDYTEN.11
      KL=KZZ                                                                     BDYTEN.12
C                                                                                BDYTEN.13
C_FLIC_RUNPAD(2)                                                                 BDYTEN.14
      IF(IBMOIST.GT.3)GOTO 100                                                   BDYTEN.15
C                                                                                BDYTEN.16
cmic$ do all autoscope                                                           BDYTEN.17
c$doacross                                                                       BDYTEN.18
c$& local(i,j,k)                                                                 BDYTEN.19
c$omp parallel do default(shared)                                                BDYTEN.20
c$omp&private(i,j,k)                                                             BDYTEN.21
      DO 10 J=1,JEND                                                             BDYTEN.22
        DO 10 K=1,KL                                                             BDYTEN.23
          DO 10 I=1,IEND                                                         BDYTEN.24
            TTEN(I,J,K)=0.0                                                      BDYTEN.25
   10     CONTINUE                                                               BDYTEN.26
                                                                                 BDYTEN.27
                                                                                 BDYTEN.28
C                                                                                BDYTEN.29
  100 CONTINUE                                                                   BDYTEN.30
C                                                                                BDYTEN.31
      IF(INEST.EQ.1.AND.IBMOIST.EQ.0)RETURN                                      BDYTEN.32
C                                                                                BDYTEN.33
      LB=1                                                                       BDYTEN.34
      IF(INEST.GT.1)THEN                                                         BDYTEN.35
        LB=2                                                                     BDYTEN.36
        IF(IBMOIST.EQ.4)THEN                                                     BDYTEN.37
C                                                                                BDYTEN.38
C --- for W boundaries only.                                                     BDYTEN.39
C                                                                                BDYTEN.40
          LB=4                                                                   BDYTEN.41
          KL=1                                                                   BDYTEN.42
        ENDIF                                                                    BDYTEN.43
      ENDIF                                                                      BDYTEN.44
C                                                                                BDYTEN.45
      DO 200 NB1=1,LB                                                            BDYTEN.46
#ifndef MPP1                                                                     BDYTEN.47
        NB=NB1                                                                   BDYTEN.48
        IS=NB1                                                                   BDYTEN.49
        JS=NB1                                                                   BDYTEN.50
        IE=IEND+1-NB1                                                            BDYTEN.51
        JE=JEND+1-NB1                                                            BDYTEN.52
#else                                                                            BDYTEN.53
        NB=NOFLIC(NB1)                                                           BDYTEN.54
        IS=NOFLIC(NB1)                                                           BDYTEN.55
        JS=NOFLIC(NB1)                                                           BDYTEN.56
        IE=IEND+1-NOFLIC(NB1)                                                    BDYTEN.57
        JE=JEND+1-NOFLIC(NB1)                                                    BDYTEN.58
#endif                                                                           BDYTEN.59
cmic$ do all autoscope                                                           BDYTEN.60
c$doacross                                                                       BDYTEN.61
c$& local(i,j,k)                                                                 BDYTEN.62
c$omp parallel do default(shared)                                                BDYTEN.63
c$omp&private(i,j,k)                                                             BDYTEN.64
        DO 180 K=1,KL                                                            BDYTEN.65
          DO 120 J=NB+1,JE-1                                                     BDYTEN.66
            TTEN(IS,J,K)=TSBT(J,K,NB)                                            BDYTEN.67
            TTEN(IE,J,K)=TNBT(J,K,NB)                                            BDYTEN.68
  120     CONTINUE                                                               BDYTEN.69
          DO 130 I=NB,IE                                                         BDYTEN.70
            TTEN(I,JS,K)=TWBT(I,K,NB)                                            BDYTEN.71
            TTEN(I,JE,K)=TEBT(I,K,NB)                                            BDYTEN.72
  130     CONTINUE                                                               BDYTEN.73
  180   CONTINUE                                                                 BDYTEN.74
  200 CONTINUE                                                                   BDYTEN.75
C                                                                                BDYTEN.76
      RETURN                                                                     BDYTEN.77
      END                                                                        BDYTEN.78
                                                                                 BDYTEN.79
