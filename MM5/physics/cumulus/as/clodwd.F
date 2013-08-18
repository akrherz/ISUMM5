         SUBROUTINE CLODWD (L1,KS,KMIN,HCD,QES,HES,ZU,Z,                         CLODWD.1
     *                 TEMPP,KDIM,AXD,KNUM)                                      CLODWD.2
      REAL LHEAT                                                                 CLODWD.3
      DIMENSION Z(KDIM)                                                          CLODWD.4
      DIMENSION HCD(KDIM,KNUM),QES(KDIM),HES(KDIM),ZU(KDIM,KNUM)                 CLODWD.5
     *    ,TEMPP(KDIM)                                                           CLODWD.6
C                                                                                CLODWD.7
C                                                                                CLODWD.8
C------E                                                                         CLODWD.9
C                                                                                CLODWD.10
C                                                                                CLODWD.11
      LHEAT=2.5E06                                                               CLODWD.12
      G=9.81                                                                     CLODWD.13
      RV=461.5                                                                   CLODWD.14
      CP=1004.                                                                   CLODWD.15
      AXD=0.                                                                     CLODWD.16
C                                                                                CLODWD.17
C                                                                                CLODWD.18
C------- CALCULATE CLOUDWORKFUNCTION FOR DOWNDRAFT                               CLODWD.19
C                                                                                CLODWD.20
C                                                                                CLODWD.21
      AA=0.                                                                      CLODWD.22
      DO 154 K=1,KMIN-1                                                          CLODWD.23
        KK=KMIN-K                                                                CLODWD.24
        GAMMA1=(LHEAT/CP)*(LHEAT/(RV*(TEMPP(KK)**2)))*QES(KK)                    CLODWD.25
        GAMMA2=(LHEAT/CP)*(LHEAT/(RV*(TEMPP(KK+1)**2)))*QES(KK+1)                CLODWD.26
        DHH=HCD(KK,L1)                                                           CLODWD.27
        ZZU=ZU(KK,L1)                                                            CLODWD.28
        DT=.5*(TEMPP(KK)+TEMPP(KK+1))                                            CLODWD.29
        DG=.5*(GAMMA1+GAMMA2)                                                    CLODWD.30
        DH=.5*(HES(KK)+HES(KK+1))                                                CLODWD.31
        DZ=-1.*(Z(KK+1)-Z(KK))                                                   CLODWD.32
        AA=DZ*(G/(CP*DT))*ZZU*((DHH-DH)/(1.+DG))                                 CLODWD.33
        AXD=AXD+AA                                                               CLODWD.34
        AA=0.                                                                    CLODWD.35
  154 CONTINUE                                                                   CLODWD.36
      RETURN                                                                     CLODWD.37
      END                                                                        CLODWD.38
                                                                                 CLODWD.39
