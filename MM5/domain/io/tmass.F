C-----------------------------------------------------------------------         TMASS.1
      SUBROUTINE TMASS(TDRYM,TQMASS,TVMASS,TCMASS,TRMASS,IL,JL,KL,               TMASS.2
     1                 IDRY,IMOIST,PSA,QVA,QCA,QRA,DSIGMA,                       TMASS.3
     2                 DX,G)                                                     TMASS.4
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          TMASS.5
#     include <parame.incl>                                                      TMASS.6
#     include <sum.incl>                                                         TMASS.7
C                                                                                TMASS.8
      DIMENSION QVA(MIX,MJX,MKX),QCA(MIXM,MJXM,MKXM),                            TMASS.9
     -     QRA(MIXM,MJXM,MKXM),PSA(MIX,MJX),DSIGMA(MKX)                          TMASS.10
C                                                                                TMASS.11
      TDRYM=0.                                                                   TMASS.12
      TVMASS=0.                                                                  TMASS.13
      TCMASS=0.                                                                  TMASS.14
      TRMASS=0.                                                                  TMASS.15
      TQMASS=0.                                                                  TMASS.16
c$doacross local(i,j,k)                                                          TMASS.17
c$omp parallel do default(shared)                                                TMASS.18
c$omp&private(i,j,k)                                                             TMASS.19
      DO J=1,JL                                                                  TMASS.20
        IF(J.LE.JL-1)THEN                                                        TMASS.21
C                                                                                TMASS.22
C-----DRY AIR (UNIT = KG):                                                       TMASS.23
C                                                                                TMASS.24
          PARJSUM(ITDRYM_SUM,J)=0.                                               TMASS.25
          DO 10 K=1,KL                                                           TMASS.26
            DO 10 I=1,IL-1                                                       TMASS.27
              PARJSUM(ITDRYM_SUM,J)=PARJSUM(ITDRYM_SUM,J)+PSA(I,J)*DX*DX         TMASS.28
     +                              *DSIGMA(K)                                   TMASS.29
   10       CONTINUE                                                             TMASS.30
                                                                                 TMASS.31
C      TDRYM=TDRYM*1000./G                                                       TMASS.32
C                                                                                TMASS.33
C-----WATER SUBSTANCE (UNIT = KG):                                               TMASS.34
C                                                                                TMASS.35
          IF(IDRY.EQ.1)GOTO 80                                                   TMASS.36
          PARJSUM(ITVMASS_SUM,J)=0.                                              TMASS.37
          DO 20 K=1,KL                                                           TMASS.38
            DO 20 I=1,IL-1                                                       TMASS.39
              PARJSUM(ITVMASS_SUM,J)=PARJSUM(ITVMASS_SUM,J)+QVA(I,J,K)*          TMASS.40
     +                               DX*DX*DSIGMA(K)                             TMASS.41
   20       CONTINUE                                                             TMASS.42
                                                                                 TMASS.43
C                                                                                TMASS.44
          IF(IMOIST.EQ.1.OR.IMOIST.EQ.3)GOTO 80                                  TMASS.45
          PARJSUM(ITCMASS_SUM,J)=0.                                              TMASS.46
          PARJSUM(ITRMASS_SUM,J)=0.                                              TMASS.47
          DO 30 K=1,KL                                                           TMASS.48
            DO 30 I=1,IL-1                                                       TMASS.49
              PARJSUM(ITCMASS_SUM,J)=PARJSUM(ITCMASS_SUM,J)+QCA(I,J,K)*          TMASS.50
     +                               DX*DX*DSIGMA(K)                             TMASS.51
              PARJSUM(ITRMASS_SUM,J)=PARJSUM(ITRMASS_SUM,J)+QRA(I,J,K)*          TMASS.52
     +                               DX*DX*DSIGMA(K)                             TMASS.53
   30       CONTINUE                                                             TMASS.54
                                                                                 TMASS.55
   80     CONTINUE                                                               TMASS.56
        ENDIF                                                                    TMASS.57
      ENDDO                                                                      TMASS.58
      DO J=1,JL-1                                                                TMASS.59
        TDRYM=TDRYM+PARJSUM(ITDRYM_SUM,J)                                        TMASS.60
      ENDDO                                                                      TMASS.61
      TDRYM=TDRYM*1000./G                                                        TMASS.62
      IF(IDRY.EQ.1)RETURN                                                        TMASS.63
      DO J=1,JL-1                                                                TMASS.64
        TVMASS=TVMASS+PARJSUM(ITVMASS_SUM,J)                                     TMASS.65
      ENDDO                                                                      TMASS.66
      IF(IMOIST.EQ.1.OR.IMOIST.EQ.3)GOTO 40                                      TMASS.67
      DO J=1,JL-1                                                                TMASS.68
        TCMASS=TCMASS+PARJSUM(ITCMASS_SUM,J)                                     TMASS.69
        TRMASS=TRMASS+PARJSUM(ITRMASS_SUM,J)                                     TMASS.70
      ENDDO                                                                      TMASS.71
C                                                                                TMASS.72
      TCMASS=TCMASS*1000./G                                                      TMASS.73
      TRMASS=TRMASS*1000./G                                                      TMASS.74
   40 TVMASS=TVMASS*1000./G                                                      TMASS.75
      TQMASS=TVMASS+TCMASS+TRMASS                                                TMASS.76
C                                                                                TMASS.77
      RETURN                                                                     TMASS.78
      END                                                                        TMASS.79
                                                                                 TMASS.80
