       SUBROUTINE FDAOFF (NUMNES)                                                FDAOFF.1
#     include <parame.incl>                                                      FDAOFF.2
#     include <param2.incl>                                                      FDAOFF.3
#     include <parfdda.incl>                                                     FDAOFF.4
C                                                                                FDAOFF.5
C--- INITIALIZE NESTED DOMAINS FDDA SWITCHES                                     FDAOFF.6
C                                                                                FDAOFF.7
      NN=NUMNES                                                                  FDAOFF.8
      IWTSIG(NN)=0                                                               FDAOFF.9
      I4D(NN,1)=0                                                                FDAOFF.10
      I4D(NN,2)=0                                                                FDAOFF.11
      IWIND(NN,1)=0                                                              FDAOFF.12
      IWIND(NN,2)=0                                                              FDAOFF.13
      IWINDS(NN,1)=-99                                                           FDAOFF.14
      IWINDS(NN,2)=0                                                             FDAOFF.15
      ITEMP(NN,1)=0                                                              FDAOFF.16
      ITEMP(NN,2)=0                                                              FDAOFF.17
      IMOIS(NN,1)=0                                                              FDAOFF.18
      IMOIS(NN,2)=0                                                              FDAOFF.19
      IROT(NN)=0                                                                 FDAOFF.20
      I4DI(NN)=0                                                                 FDAOFF.21
C                                                                                FDAOFF.22
      ISTEMP(NN)=0                                                               FDAOFF.23
      ISWIND(NN)=0                                                               FDAOFF.24
      ISMOIS(NN)=0                                                               FDAOFF.25
      IPSTR(NN,2)=0                                                              FDAOFF.26
      ISPSTR(NN)=0                                                               FDAOFF.27
    8 CONTINUE                                                                   FDAOFF.28
C                                                                                FDAOFF.29
      PRINT *,'FDDA SWITCHED OFF FOR DOMAIN = ',NUMNES                           FDAOFF.30
      RETURN                                                                     FDAOFF.31
      END                                                                        FDAOFF.32
                                                                                 FDAOFF.33
