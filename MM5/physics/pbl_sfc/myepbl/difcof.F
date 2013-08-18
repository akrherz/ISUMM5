C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&         DIFCOF.1
      SUBROUTINE DIFCOF(LMHK,GM,GH,EL,Q2,Z,AKM,AKH)                              DIFCOF.2
C     ******************************************************************         DIFCOF.3
C     *                                                                *         DIFCOF.4
C     *                LEVEL 2.5 DIFFUSION COEFFICIENTS                *         DIFCOF.5
C     *                                                                *         DIFCOF.6
C     ******************************************************************         DIFCOF.7
C-----------------------------------------------------------------------         DIFCOF.8
#     include <parame.incl>                                                      DIFCOF.9
      PARAMETER (LM=MKX)                                                         DIFCOF.10
C-----------------------------------------------------------------------         DIFCOF.11
                             P A R A M E T E R                                   DIFCOF.12
     &(LP1=LM+1,LM1=LM-1)                                                        DIFCOF.13
C-----------------------------------------------------------------------         DIFCOF.14
                             P A R A M E T E R                                   DIFCOF.15
C-----------------------------------------------------------------------         DIFCOF.16
     &(G=9.8,BETA=1./270.,BTG=BETA*G                                             DIFCOF.17
     &,PRT=1.0,GAM1=0.2222222222222222222                                        DIFCOF.18
     &,A1=0.659888514560862645,A2=0.6574209922667784586                          DIFCOF.19
     &,B1=11.87799326209552761,B2=7.226971804046074028                           DIFCOF.20
     &,C1=0.000830955950095854396)                                               DIFCOF.21
C-----------------------------------------------------------------------         DIFCOF.22
CUN     &(G=9.8,BETA=1./270.,BTG=BETA*G                                          DIFCOF.23
CUN     &,PRT=1.0,GAM1=0.2222222222222222222                                     DIFCOF.24
CUN     &,A1=0.3310949523016403346,A2=0.8273378704055731278                      DIFCOF.25
CUN     &,B1=5.959709141429526024,B2=3.626088092074591135                        DIFCOF.26
CUN     &,C1=-0.3330651924968952113)                                             DIFCOF.27
C-----------------------------------------------------------------------         DIFCOF.28
CMY     &(G=9.8,BETA=1./270.,BTG=BETA*G                                          DIFCOF.29
CMY     &,PRT=0.8,GAM1=0.2222222222222222222                                     DIFCOF.30
CMY     &,A1=0.9222222350809054114,A2=0.7350190142719400952                      DIFCOF.31
CMY     &,B1=16.60000023145629741,B2=10.10000014082581951                        DIFCOF.32
CMY     &,C1=0.0805318118080613468)                                              DIFCOF.33
C-----------------------------------------------------------------------         DIFCOF.34
                             P A R A M E T E R                                   DIFCOF.35
C--------------COEFFICIENTS FOR THE SM AND SH DETERMINANTS--------------         DIFCOF.36
     &(BSMH=-3.*A1*A2*(3.*A2+3.*B2*C1+12.*A1*C1-B2)*BTG                          DIFCOF.37
     &,CESM=    A1*(1.-3.*C1)                                                    DIFCOF.38
C                                                                                DIFCOF.39
     &,BSHM=18.*A1*A1*A2*C1                                                      DIFCOF.40
     &,BSHH= 9.*A1*A2*A2*BTG                                                     DIFCOF.41
     &,CESH=    A2                                                               DIFCOF.42
C--------------COEFFICIENTS OF THE TERMS IN THE DENOMINATOR-------------         DIFCOF.43
     &,ADNM=18.*A1*A1*A2*(B2-3.*A2)*BTG                                          DIFCOF.44
     &,ADNH= 9.*A1*A2*A2*(12.*A1+3.*B2)*BTG*BTG                                  DIFCOF.45
     &,BDNM= 6.*A1*A1                                                            DIFCOF.46
     &,BDNH= 3.*A2*(7.*A1+B2)*BTG)                                               DIFCOF.47
C-----------------------------------------------------------------------         DIFCOF.48
                             D I M E N S I O N                                   DIFCOF.49
     & Q2    (LM)                                                                DIFCOF.50
                             D I M E N S I O N                                   DIFCOF.51
     & GM    (LM1),GH    (LM1),EL    (LM1)                                       DIFCOF.52
     &,AKM   (LM1),AKH   (LM1)                                                   DIFCOF.53
     &,Z     (LP1)                                                               DIFCOF.54
C-----------------------------------------------------------------------         DIFCOF.55
C***********************************************************************         DIFCOF.56
      LMHM=LMHK-1                                                                DIFCOF.57
      LMHP=LMHK+1                                                                DIFCOF.58
      DO 100 L=1,LMHM                                                            DIFCOF.59
        ELL=EL(L)                                                                DIFCOF.60
C-----------------------------------------------------------------------         DIFCOF.61
        ELOQ2=ELL*ELL/Q2(L)                                                      DIFCOF.62
        ELOQ4=ELOQ2*ELOQ2                                                        DIFCOF.63
C-----------------------------------------------------------------------         DIFCOF.64
        GML=GM(L)                                                                DIFCOF.65
        GHL=GH(L)                                                                DIFCOF.66
C--------------COEFFICIENTS OF THE TERMS IN THE DENOMINATOR-------------         DIFCOF.67
        ADEN=(ADNM*GML+ADNH*GHL)*GHL                                             DIFCOF.68
        BDEN=BDNM*GML+BDNH*GHL                                                   DIFCOF.69
        CDEN=1.                                                                  DIFCOF.70
C--------------COEFFICIENTS FOR THE SM DETERMINANT----------------------         DIFCOF.71
        BESM=BSMH*GHL                                                            DIFCOF.72
C--------------COEFFICIENTS FOR THE SH DETERMINANT----------------------         DIFCOF.73
        BESH=BSHM*GML+BSHH*GHL                                                   DIFCOF.74
C--------------1./DENOMINATOR-------------------------------------------         DIFCOF.75
        RDEN=1./(ADEN*ELOQ4+BDEN*ELOQ2+CDEN)                                     DIFCOF.76
C--------------SM AND SH------------------------------------------------         DIFCOF.77
        ESM=(BESM*ELOQ2+CESM)*RDEN                                               DIFCOF.78
        ESH=(BESH*ELOQ2+CESH)*RDEN                                               DIFCOF.79
C--------------DIFFUSION COEFFICIENTS-----------------------------------         DIFCOF.80
        RDZ=2./(Z(L)-Z(L+2))                                                     DIFCOF.81
        Q1L=SQRT(Q2(L))                                                          DIFCOF.82
        ELQDZ=ELL*Q1L*RDZ                                                        DIFCOF.83
        AKM(L)=ELQDZ*ESM                                                         DIFCOF.84
        AKH(L)=ELQDZ*ESH                                                         DIFCOF.85
C-----------------------------------------------------------------------         DIFCOF.86
  100 CONTINUE                                                                   DIFCOF.87
C-----------------------------------------------------------------------         DIFCOF.88
      RETURN                                                                     DIFCOF.89
      END                                                                        DIFCOF.90
                                                                                 DIFCOF.91
