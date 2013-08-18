         SUBROUTINE CLOUDWS(L1,HC,QES,HES,ZU,Z,                                  CLOUDWS.1
     *                 TEMPP,KDIM,AX,KBCON,KNUM,KTOP)                            CLOUDWS.2
      REAL LHEAT                                                                 CLOUDWS.3
      DIMENSION Z(KDIM)                                                          CLOUDWS.4
      DIMENSION HC(KDIM,KNUM),QES(KDIM),HES(KDIM),ZU(KDIM,KNUM)                  CLOUDWS.5
     *         ,TEMPP(KDIM)                                                      CLOUDWS.6
      INTEGER KTOP(KNUM)                                                         CLOUDWS.7
C                                                                                CLOUDWS.8
C------- CONSTANTS                                                               CLOUDWS.9
C                                                                                CLOUDWS.10
      LHEAT=2.5E06                                                               CLOUDWS.11
      G=9.81                                                                     CLOUDWS.12
      RV=461.5                                                                   CLOUDWS.13
      CP=1004.                                                                   CLOUDWS.14
      AX=0.                                                                      CLOUDWS.15
C                                                                                CLOUDWS.16
C                                                                                CLOUDWS.17
C------- CALCULATE CLOUDWORKFUNCTION FOR UPDRAFT                                 CLOUDWS.18
C                                                                                CLOUDWS.19
C                                                                                CLOUDWS.20
      DO 154 K=KBCON,KTOP(L1)-0                                                  CLOUDWS.21
        GAMMA1=(LHEAT/CP)*(LHEAT/(RV*(TEMPP(K)**2)))*QES(K)                      CLOUDWS.22
        GAMMA2=(LHEAT/CP)*(LHEAT/(RV*(TEMPP(K+1)**2)))*QES(K+1)                  CLOUDWS.23
        DHH=HC(K,L1)                                                             CLOUDWS.24
        ZZU=ZU(K,L1)                                                             CLOUDWS.25
        DT=.5*(TEMPP(K)+TEMPP(K+1))                                              CLOUDWS.26
        DG=.5*(GAMMA1+GAMMA2)                                                    CLOUDWS.27
        DH=.5*(HES(K)+HES(K+1))                                                  CLOUDWS.28
        DZ=Z(K+1)-Z(K)                                                           CLOUDWS.29
        AA=DZ*(G/(CP*DT))*ZZU*((DHH-(DH))/(1.+DG))                               CLOUDWS.30
        AX=AX+AA                                                                 CLOUDWS.31
        AA=0.0                                                                   CLOUDWS.32
  154 CONTINUE                                                                   CLOUDWS.33
C                                                                                CLOUDWS.34
      RETURN                                                                     CLOUDWS.35
      END                                                                        CLOUDWS.36
                                                                                 CLOUDWS.37
