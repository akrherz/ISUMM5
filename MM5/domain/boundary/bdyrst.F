      SUBROUTINE BDYRST(TA,TB,TTEN,KZZ,IEND,JEND,DTBC,                           BDYRST.1
     &                  TSB,TNB,TEB,TWB,INEST,IBQ,EPSI)                          BDYRST.2
      IMPLICIT NONE                                                              BDYRST.3
# include <parame.incl>                                                          BDYRST.4
      INTEGER KZZ,IEND,JEND,INEST,IBQ                                            BDYRST.5
      REAL      TA(MIX,MJX,KZZ), TB(MIX,MJX,KZZ), TTEN(MIX,MJX,KZZ)              BDYRST.6
      REAL      TEB(MIX,KZZ,NSPGX), TWB(MIX,KZZ,NSPGX),                          BDYRST.7
     1          TNB(MJX,KZZ,NSPGX), TSB(MJX,KZZ,NSPGX)                           BDYRST.8
      REAL      EPSI,DTBC                                                        BDYRST.9
      INTEGER I,K,J,KL,LB,NB,NB1,IS,IE,JS,JE                                     BDYRST.10
C                                                                                BDYRST.11
      KL=KZZ                                                                     BDYRST.12
C                                                                                BDYRST.13
      LB=1                                                                       BDYRST.14
      IF(INEST.GT.1)THEN                                                         BDYRST.15
        LB=2                                                                     BDYRST.16
      ENDIF                                                                      BDYRST.17
C                                                                                BDYRST.18
      DO 200 NB1=1,LB                                                            BDYRST.19
#ifndef MPP1                                                                     BDYRST.20
        NB=NB1                                                                   BDYRST.21
        IS=NB1                                                                   BDYRST.22
        JS=NB1                                                                   BDYRST.23
        IE=IEND+1-NB1                                                            BDYRST.24
        JE=JEND+1-NB1                                                            BDYRST.25
#else                                                                            BDYRST.26
        NB=NOFLIC(NB1)                                                           BDYRST.27
        IS=NOFLIC(NB1)                                                           BDYRST.28
        JS=NOFLIC(NB1)                                                           BDYRST.29
        IE=IEND+1-NOFLIC(NB1)                                                    BDYRST.30
        JE=JEND+1-NOFLIC(NB1)                                                    BDYRST.31
#endif                                                                           BDYRST.32
cmic$ do all autoscope                                                           BDYRST.33
cmic$2 private(I,J,K)                                                            BDYRST.34
c$doacross                                                                       BDYRST.35
c$& local(i,j,k)                                                                 BDYRST.36
c$omp parallel do default(shared)                                                BDYRST.37
c$omp&private(i,j,k)                                                             BDYRST.38
        DO 180 K=1,KL                                                            BDYRST.39
          DO 120 J=NB+1,JE-1                                                     BDYRST.40
            TB(IS,J,K)=TA(IS,J,K)                                                BDYRST.41
            TB(IE,J,K)=TA(IE,J,K)                                                BDYRST.42
            TA(IS,J,K)=TSB(J,K,NB)+DTBC*TTEN(IS,J,K)                             BDYRST.43
            TA(IE,J,K)=TNB(J,K,NB)+DTBC*TTEN(IE,J,K)                             BDYRST.44
  120     CONTINUE                                                               BDYRST.45
          DO 130 I=NB,IE                                                         BDYRST.46
            TB(I,JS,K)=TA(I,JS,K)                                                BDYRST.47
            TB(I,JE,K)=TA(I,JE,K)                                                BDYRST.48
            TA(I,JS,K)=TWB(I,K,NB)+DTBC*TTEN(I,JS,K)                             BDYRST.49
            TA(I,JE,K)=TEB(I,K,NB)+DTBC*TTEN(I,JE,K)                             BDYRST.50
  130     CONTINUE                                                               BDYRST.51
          IF(IBQ.EQ.1)THEN                                                       BDYRST.52
            DO J=NB+1,JE-1                                                       BDYRST.53
              IF(TA(IS,J,K).LT.EPSI)TA(IS,J,K)=EPSI                              BDYRST.54
              IF(TA(IE,J,K).LT.EPSI)TA(IE,J,K)=EPSI                              BDYRST.55
            ENDDO                                                                BDYRST.56
            DO I=NB,IE                                                           BDYRST.57
              IF(TA(I,JS,K).LT.EPSI)TA(I,JS,K)=EPSI                              BDYRST.58
              IF(TA(I,JE,K).LT.EPSI)TA(I,JE,K)=EPSI                              BDYRST.59
            ENDDO                                                                BDYRST.60
          ENDIF                                                                  BDYRST.61
  180   CONTINUE                                                                 BDYRST.62
  200 CONTINUE                                                                   BDYRST.63
C                                                                                BDYRST.64
      RETURN                                                                     BDYRST.65
      END                                                                        BDYRST.66
                                                                                 BDYRST.67
