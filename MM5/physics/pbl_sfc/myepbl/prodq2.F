C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&         PRODQ2.1
      SUBROUTINE PRODQ2(LMHK,DTQ2,USTAR,GM,GH,EL,Q2)                             PRODQ2.2
C     ******************************************************************         PRODQ2.3
C     *                                                                *         PRODQ2.4
C     *  LEVEL 2.5 Q2 PRODUCTION/DISSIPATION                           *         PRODQ2.5
C     *                                                                *         PRODQ2.6
C     ******************************************************************         PRODQ2.7
C-----------------------------------------------------------------------         PRODQ2.8
#     include <parame.incl>                                                      PRODQ2.9
      PARAMETER (LM=MKX)                                                         PRODQ2.10
C-----------------------------------------------------------------------         PRODQ2.11
                             P A R A M E T E R                                   PRODQ2.12
     &(LM1=LM-1)                                                                 PRODQ2.13
C-----------------------------------------------------------------------         PRODQ2.14
                             P A R A M E T E R                                   PRODQ2.15
     &(EPSQ2=0.2,EPSL=0.32,EPSTRB=1.E-24,EPS1=1.E-12,EPS2=0.)                    PRODQ2.16
C-----------------------------------------------------------------------         PRODQ2.17
                             P A R A M E T E R                                   PRODQ2.18
C-----------------------------------------------------------------------         PRODQ2.19
     &(G=9.8,BETA=1./270.,BTG=BETA*G                                             PRODQ2.20
     &,PRT=1.0,GAM1=0.2222222222222222222                                        PRODQ2.21
     &,A1=0.659888514560862645,A2=0.6574209922667784586                          PRODQ2.22
     &,B1=11.87799326209552761,B2=7.226971804046074028                           PRODQ2.23
     &,C1=0.000830955950095854396)                                               PRODQ2.24
C-----------------------------------------------------------------------         PRODQ2.25
CUN     &(G=9.8,BETA=1./270.,BTG=BETA*G                                          PRODQ2.26
CUN     &,PRT=1.0,GAM1=0.2222222222222222222                                     PRODQ2.27
CUN     &,A1=0.3310949523016403346,A2=0.8273378704055731278                      PRODQ2.28
CUN     &,B1=5.959709141429526024,B2=3.626088092074591135                        PRODQ2.29
CUN     &,C1=-0.3330651924968952113)                                             PRODQ2.30
C-----------------------------------------------------------------------         PRODQ2.31
CMY     &(G=9.8,BETA=1./270.,BTG=BETA*G                                          PRODQ2.32
CMY     &,PRT=0.8,GAM1=0.2222222222222222222                                     PRODQ2.33
CMY     &,A1=0.9222222350809054114,A2=0.7350190142719400952                      PRODQ2.34
CMY     &,B1=16.60000023145629741,B2=10.10000014082581951                        PRODQ2.35
CMY     &,C1=0.0805318118080613468)                                              PRODQ2.36
C-----------------------------------------------------------------------         PRODQ2.37
                             P A R A M E T E R                                   PRODQ2.38
     &(RB1=1./B1                                                                 PRODQ2.39
C--------------COEFFICIENTS OF THE TERMS IN THE NUMERATOR---------------         PRODQ2.40
     &,ANMM=-3.*A1*A2*(3.*A2+3.*B2*C1+18.*A1*C1-B2)*BTG                          PRODQ2.41
     &,ANMH=-9.*A1*A2*A2*BTG*BTG                                                 PRODQ2.42
     &,BNMM=    A1*(1.-3.*C1)                                                    PRODQ2.43
     &,BNMH=   -A2*BTG                                                           PRODQ2.44
C--------------COEFFICIENTS OF THE TERMS IN THE DENOMINATOR-------------         PRODQ2.45
     &,ADNM=18.*A1*A1*A2*(B2-3.*A2)*BTG                                          PRODQ2.46
     &,ADNH= 9.*A1*A2*A2*(12.*A1+3.*B2)*BTG*BTG                                  PRODQ2.47
     &,BDNM= 6.*A1*A1                                                            PRODQ2.48
     &,BDNH= 3.*A2*(7.*A1+B2)*BTG                                                PRODQ2.49
C--------------COEFFICIENTS OF THE EQUILIBRIUM EQUATION-----------------         PRODQ2.50
     &,AEQM= 3.*A1*A2*B1*(3.*A2+3.*B2*C1+18.*A1*C1-B2)*BTG                       PRODQ2.51
     &     +18.*A1*A1*A2*(B2-3.*A2)*BTG                                          PRODQ2.52
     &,AEQH= 9.*A1*A2*A2*B1*BTG*BTG+9.*A1*A2*A2*(12.*A1+3.*B2)*BTG*BTG           PRODQ2.53
     &,BEQM=-A1*B1*(1.-3.*C1)+6.*A1*A1                                           PRODQ2.54
     &,BEQH= A2*B1*BTG+3.*A2*(7.*A1+B2)*BTG                                      PRODQ2.55
C--------------FORBIDDEN TURBULENCE AREA--------------------------------         PRODQ2.56
     &,REQU=-AEQH/AEQM*1.02,EPSGH=1.E-9)                                         PRODQ2.57
C-----------------------------------------------------------------------         PRODQ2.58
                             D I M E N S I O N                                   PRODQ2.59
     & Q2    (LM)                                                                PRODQ2.60
                             D I M E N S I O N                                   PRODQ2.61
     & GM    (LM1),GH    (LM1),EL    (LM1)                                       PRODQ2.62
C-----------------------------------------------------------------------         PRODQ2.63
C***********************************************************************         PRODQ2.64
      LMHM=LMHK-1                                                                PRODQ2.65
C                                                                                PRODQ2.66
      DO 150 L=1,LMHM                                                            PRODQ2.67
        GML=GM(L)                                                                PRODQ2.68
        GHL=GH(L)                                                                PRODQ2.69
C--------------COEFFICIENTS OF THE EQUILIBRIUM EQUATION-----------------         PRODQ2.70
        AEQU=(AEQM*GML+AEQH*GHL)*GHL                                             PRODQ2.71
        BEQU=BEQM*GML+BEQH*GHL                                                   PRODQ2.72
C--------------EQUILIBRIUM SOLUTION FOR L/Q-----------------------------         PRODQ2.73
        EQOL2=-0.5*BEQU+SQRT(BEQU*BEQU*0.25-AEQU)                                PRODQ2.74
C--------------IS THERE PRODUCTION/DISSIPATION ?------------------------         PRODQ2.75
        IF((GML+GHL*GHL.LE.EPSTRB).OR.(GHL.GE.EPSGH.AND.GML/GHL.LE.REQU)         PRODQ2.76
     +    .OR.(EQOL2.LE.EPS2))THEN                                               PRODQ2.77
C--------------NO TURBULENCE--------------------------------------------         PRODQ2.78
          Q2(L)=EPSQ2                                                            PRODQ2.79
          EL(L)=EPSL                                                             PRODQ2.80
        ELSE                                                                     PRODQ2.81
C--------------END OF THE NO TURBULENCE BRANCH--------------------------         PRODQ2.82
C--------------COEFFICIENTS OF THE TERMS IN THE NUMERATOR---------------         PRODQ2.83
          ANUM=(ANMM*GML+ANMH*GHL)*GHL                                           PRODQ2.84
          BNUM=BNMM*GML+BNMH*GHL                                                 PRODQ2.85
C--------------COEFFICIENTS OF THE TERMS IN THE DENOMINATOR-------------         PRODQ2.86
          ADEN=(ADNM*GML+ADNH*GHL)*GHL                                           PRODQ2.87
          BDEN=BDNM*GML+BDNH*GHL                                                 PRODQ2.88
          CDEN=1.                                                                PRODQ2.89
C--------------COEFFICIENTS OF THE NUMERATOR OF THE LINEARIZED EQ.------         PRODQ2.90
          ARHS=-(ANUM*BDEN-BNUM*ADEN)*2.                                         PRODQ2.91
          BRHS=-ANUM*4.                                                          PRODQ2.92
          CRHS=-BNUM*2.                                                          PRODQ2.93
C--------------INITIAL VALUE OF L/Q-------------------------------------         PRODQ2.94
          DLOQ1=EL(L)/SQRT(Q2(L))                                                PRODQ2.95
C--------------FIRST ITERATION FOR L/Q, RHS=0---------------------------         PRODQ2.96
          ELOQ21=1./EQOL2                                                        PRODQ2.97
          ELOQ11=SQRT(ELOQ21)                                                    PRODQ2.98
          ELOQ31=ELOQ21*ELOQ11                                                   PRODQ2.99
          ELOQ41=ELOQ21*ELOQ21                                                   PRODQ2.100
          ELOQ51=ELOQ21*ELOQ31                                                   PRODQ2.101
C--------------1./DENOMINATOR-------------------------------------------         PRODQ2.102
          RDEN1=1./(ADEN*ELOQ41+BDEN*ELOQ21+CDEN)                                PRODQ2.103
C--------------D(RHS)/D(L/Q)--------------------------------------------         PRODQ2.104
          RHSP1=(ARHS*ELOQ51+BRHS*ELOQ31+CRHS*ELOQ11)*RDEN1*RDEN1                PRODQ2.105
C--------------FIRST-GUESS SOLUTION-------------------------------------         PRODQ2.106
          ELOQ12=ELOQ11+(DLOQ1-ELOQ11)*EXP(RHSP1*DTQ2)                           PRODQ2.107
C-----------------------------------------------------------------------         PRODQ2.108
          ELOQ12=AMAX1(ELOQ12,EPS1)                                              PRODQ2.109
C--------------SECOND ITERATION FOR L/Q---------------------------------         PRODQ2.110
          ELOQ22=ELOQ12*ELOQ12                                                   PRODQ2.111
          ELOQ32=ELOQ22*ELOQ12                                                   PRODQ2.112
          ELOQ42=ELOQ22*ELOQ22                                                   PRODQ2.113
          ELOQ52=ELOQ22*ELOQ32                                                   PRODQ2.114
C--------------1./DENOMINATOR-------------------------------------------         PRODQ2.115
          RDEN2=1./(ADEN*ELOQ42+BDEN*ELOQ22+CDEN)                                PRODQ2.116
C-----------------------------------------------------------------------         PRODQ2.117
          RHS2=-(ANUM*ELOQ42+BNUM*ELOQ22)*RDEN2+RB1                              PRODQ2.118
          RHSP2=(ARHS*ELOQ52+BRHS*ELOQ32+CRHS*ELOQ12)*RDEN2*RDEN2                PRODQ2.119
          RHST2=RHS2/RHSP2                                                       PRODQ2.120
C--------------CORRECTED SOLUTION---------------------------------------         PRODQ2.121
          ELOQ13=ELOQ12-RHST2+(RHST2+DLOQ1-ELOQ12)*EXP(RHSP2*DTQ2)               PRODQ2.122
C-----------------------------------------------------------------------         PRODQ2.123
          ELOQ13=AMAX1(ELOQ13,EPS1)                                              PRODQ2.124
C--------------TWO ITERATIONS IS ENOUGH IN MOST CASES ...---------------         PRODQ2.125
          ELOQN=ELOQ13                                                           PRODQ2.126
C-----------------------------------------------------------------------         PRODQ2.127
          IF(ELOQN.GT.EPS1)THEN                                                  PRODQ2.128
            Q2(L)=EL(L)*EL(L)/(ELOQN*ELOQN)                                      PRODQ2.129
            Q2(L)=AMAX1(Q2(L),EPSQ2)                                             PRODQ2.130
          ELSE                                                                   PRODQ2.131
            Q2(L)=EPSQ2                                                          PRODQ2.132
          ENDIF                                                                  PRODQ2.133
        ENDIF                                                                    PRODQ2.134
C--------------END OF TURBULENT BRANCH----------------------------------         PRODQ2.135
C--------------END OF PRODUCTION/DISSIPATION LOOP-----------------------         PRODQ2.136
  150 CONTINUE                                                                   PRODQ2.137
C--------------LOWER BOUNDARY CONDITION FOR Q2--------------------------         PRODQ2.138
      Q2(LMHK)=AMAX1(B1**(2./3.)*USTAR*USTAR,EPSQ2)                              PRODQ2.139
C-----------------------------------------------------------------------         PRODQ2.140
      RETURN                                                                     PRODQ2.141
      END                                                                        PRODQ2.142
                                                                                 PRODQ2.143
