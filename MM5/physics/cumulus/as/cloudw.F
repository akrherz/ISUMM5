C-----------------------------------------------------------------------         CLOUDW.1
C  IMSL NAME:  ZX4LP (SINGLE/DOUBLE PRECISION VERSION)                           CLOUDW.2
C                                                                                CLOUDW.3
C  COMPUTER:   VAX/DOUBLE                                                        CLOUDW.4
C                                                                                CLOUDW.5
C  REVISED:    JUNE 1, 1985                                                      CLOUDW.6
C                                                                                CLOUDW.7
C  PURPOSE:    INTERFACE FOR ZX4LP.                                              CLOUDW.8
C                                                                                CLOUDW.9
C  USAGE:      CALL ZX4LP (A, IA, B, C, N, M1, M2, S, PSOL, DSOL, RW,            CLOUDW.10
C                          IW, IER)                                              CLOUDW.11
C                                                                                CLOUDW.12
C  ARGUMENTS:  SEE ZX4LP DOCUMENTATION.                                          CLOUDW.13
C                                                                                CLOUDW.14
C  REMARKS:                                                                      CLOUDW.15
C  1. OUTPUT AND INPUT/OUTPUT ARGUMENT(S):                                       CLOUDW.16
C       S                                                                        CLOUDW.17
C       PSOL                                                                     CLOUDW.18
C       DSOL                                                                     CLOUDW.19
C       IER                                                                      CLOUDW.20
C                                                                                CLOUDW.21
C  2. NEW ROUTINE(S):                                                            CLOUDW.22
C       DDLPRS                                                                   CLOUDW.23
C                                                                                CLOUDW.24
C  COPYRIGHT:  1985 BY IMSL, INC.  ALL RIGHTS RESERVED.                          CLOUDW.25
C                                                                                CLOUDW.26
C  WARRANTY:   IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN APPLIED             CLOUDW.27
C              TO THIS CODE.  NO OTHER WARRANTY, EXPRESSED OR IMPLIED,           CLOUDW.28
C              IS APPLICABLE.                                                    CLOUDW.29
C                                                                                CLOUDW.30
C----------------------------------------------------------------------          CLOUDW.31
C                                                                                CLOUDW.32
C                                                                                CLOUDW.33
C                                                                                CLOUDW.34
C**********************************************************************          CLOUDW.35
C*                                                                    *          CLOUDW.36
C*      CALCULATION  OF  CLOUD WORK FUNCTIONS                         *          CLOUDW.37
C*                                                                    *          CLOUDW.38
C**********************************************************************          CLOUDW.39
C*                                                                               CLOUDW.40
         SUBROUTINE CLOUDW (L1,HC,QES,HES,ZU,Z,                                  CLOUDW.41
     *                 TEMPP,KDIM,AX,KBCON,KNUM,KTOP)                            CLOUDW.42
      REAL LHEAT                                                                 CLOUDW.43
      DIMENSION Z(KDIM)                                                          CLOUDW.44
      DIMENSION HC(KDIM,KNUM),QES(KDIM),HES(KDIM),ZU(KDIM,KNUM)                  CLOUDW.45
     *         ,TEMPP(KDIM)                                                      CLOUDW.46
      INTEGER KTOP(KNUM)                                                         CLOUDW.47
C                                                                                CLOUDW.48
C------- CONSTANTS                                                               CLOUDW.49
C                                                                                CLOUDW.50
      LHEAT=2.5E06                                                               CLOUDW.51
      G=9.81                                                                     CLOUDW.52
      RV=461.5                                                                   CLOUDW.53
      CP=1004.                                                                   CLOUDW.54
      AX=0.                                                                      CLOUDW.55
C                                                                                CLOUDW.56
C                                                                                CLOUDW.57
C------- CALCULATE CLOUDWORKFUNCTION FOR UPDRAFT                                 CLOUDW.58
C                                                                                CLOUDW.59
C                                                                                CLOUDW.60
      DO 154 K=KBCON,KTOP(L1)-1                                                  CLOUDW.61
        GAMMA1=(LHEAT/CP)*(LHEAT/(RV*(TEMPP(K)**2)))*QES(K)                      CLOUDW.62
        GAMMA2=(LHEAT/CP)*(LHEAT/(RV*(TEMPP(K+1)**2)))*QES(K+1)                  CLOUDW.63
        DHH=HC(K,L1)                                                             CLOUDW.64
        ZZU=ZU(K,L1)                                                             CLOUDW.65
        DT=.5*(TEMPP(K)+TEMPP(K+1))                                              CLOUDW.66
        DG=.5*(GAMMA1+GAMMA2)                                                    CLOUDW.67
        DH=.5*(HES(K)+HES(K+1))                                                  CLOUDW.68
        DZ=Z(K+1)-Z(K)                                                           CLOUDW.69
        AA=DZ*(G/(CP*DT))*ZZU*((DHH-(DH))/(1.+DG))                               CLOUDW.70
        AX=AX+AA                                                                 CLOUDW.71
        AA=0.0                                                                   CLOUDW.72
  154 CONTINUE                                                                   CLOUDW.73
C                                                                                CLOUDW.74
      RETURN                                                                     CLOUDW.75
      END                                                                        CLOUDW.76
                                                                                 CLOUDW.77
