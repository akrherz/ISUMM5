C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&         MIXLEN.1
      SUBROUTINE MIXLEN(LMHK,LPBL,HPBL,U,V,T,Q,Q2,APE,Z,GM,GH,EL)                MIXLEN.2
C     ******************************************************************         MIXLEN.3
C     *                                                                *         MIXLEN.4
C     *                   LEVEL 2.5 MIXING LENGTH                      *         MIXLEN.5
C     *                                                                *         MIXLEN.6
C     ******************************************************************         MIXLEN.7
C-----------------------------------------------------------------------         MIXLEN.8
#     include <parame.incl>                                                      MIXLEN.9
      PARAMETER (LM=MKX)                                                         MIXLEN.10
C-----------------------------------------------------------------------         MIXLEN.11
                             P A R A M E T E R                                   MIXLEN.12
     &(LP1=LM+1,LM1=LM-1)                                                        MIXLEN.13
C-----------------------------------------------------------------------         MIXLEN.14
                             P A R A M E T E R                                   MIXLEN.15
     &(EPSQ2=0.2,EPSL=0.32,EPSRU=1.E-12,EPSRS=1.E-12                             MIXLEN.16
     &,REPSR1=1./(1.+EPSRU)                                                      MIXLEN.17
     &,ALPH=0.20,VKRM=.40,ELFC=0.23*0.5                                          MIXLEN.18
     &,EL0MIN=1.,EL0MAX=1000.)                                                   MIXLEN.19
C-----------------------------------------------------------------------         MIXLEN.20
                             P A R A M E T E R                                   MIXLEN.21
C-----------------------------------------------------------------------         MIXLEN.22
     &(G=9.8,BETA=1./270.,BTG=BETA*G                                             MIXLEN.23
     &,PRT=1.0,GAM1=0.2222222222222222222                                        MIXLEN.24
     &,A1=0.659888514560862645,A2=0.6574209922667784586                          MIXLEN.25
     &,B1=11.87799326209552761,B2=7.226971804046074028                           MIXLEN.26
     &,C1=0.000830955950095854396)                                               MIXLEN.27
C-----------------------------------------------------------------------         MIXLEN.28
CUN     &(G=9.8,BETA=1./270.,BTG=BETA*G                                          MIXLEN.29
CUN     &,PRT=1.0,GAM1=0.2222222222222222222                                     MIXLEN.30
CUN     &,A1=0.3310949523016403346,A2=0.8273378704055731278                      MIXLEN.31
CUN     &,B1=5.959709141429526024,B2=3.626088092074591135                        MIXLEN.32
CUN     &,C1=-0.3330651924968952113)                                             MIXLEN.33
C-----------------------------------------------------------------------         MIXLEN.34
CMY     &(G=9.8,BETA=1./270.,BTG=BETA*G                                          MIXLEN.35
CMY     &,PRT=0.8,GAM1=0.2222222222222222222                                     MIXLEN.36
CMY     &,A1=0.9222222350809054114,A2=0.7350190142719400952                      MIXLEN.37
CMY     &,B1=16.60000023145629741,B2=10.10000014082581951                        MIXLEN.38
CMY     &,C1=0.0805318118080613468)                                              MIXLEN.39
C--------------COEFFICIENTS OF THE TERMS IN THE DENOMINATOR-------------         MIXLEN.40
                             P A R A M E T E R                                   MIXLEN.41
     &(ADNM=18.*A1*A1*A2*(B2-3.*A2)*BTG                                          MIXLEN.42
     &,ADNH= 9.*A1*A2*A2*(12.*A1+3.*B2)*BTG*BTG                                  MIXLEN.43
     &,BDNM= 6.*A1*A1                                                            MIXLEN.44
     &,BDNH= 3.*A2*(7.*A1+B2)*BTG                                                MIXLEN.45
C--------------FREE TERM IN THE EQUILIBRIUM EQUATION FOR (L/Q)**2-------         MIXLEN.46
     &,AEQM= 3.*A1*A2*B1*(3.*A2+3.*B2*C1+18.*A1*C1-B2)*BTG                       MIXLEN.47
     &     +18.*A1*A1*A2*(B2-3.*A2)*BTG                                          MIXLEN.48
     &,AEQH= 9.*A1*A2*A2*B1*BTG*BTG+9.*A1*A2*A2*(12.*A1+3.*B2)*BTG*BTG           MIXLEN.49
C--------------FORBIDDEN TURBULENCE AREA--------------------------------         MIXLEN.50
     &,REQU=-AEQH/AEQM,EPSGH=1.E-9,EPSGM=REQU*EPSGH                              MIXLEN.51
C--------------NEAR ISOTROPY FOR SHEAR TURBULENCE, WW/Q2 LOWER LIMIT----         MIXLEN.52
     &,UBRYL=(18.*REQU*A1*A1*A2*B2*C1*BTG+9.*A1*A2*A2*B2*BTG*BTG)                MIXLEN.53
     &      /(REQU*ADNM+ADNH)                                                    MIXLEN.54
     &,UBRY=(1.+EPSRS)*UBRYL                                                     MIXLEN.55
     &,UBRY3=3.*UBRY                                                             MIXLEN.56
     &,AUBM=54.*A1*A1*A2*B2*C1*BTG -ADNM*UBRY3                                   MIXLEN.57
     &,AUBH=27.*A1*A2*A2*B2*BTG*BTG-ADNH*UBRY3                                   MIXLEN.58
     &,BUBM=18.*A1*A1*C1           -BDNM*UBRY3                                   MIXLEN.59
     &,BUBH=(9.*A1*A2+3.*A2*B2)*BTG-BDNH*UBRY3                                   MIXLEN.60
     &,CUBR=1.                     -     UBRY3,RCUBR=1./CUBR)                    MIXLEN.61
C-----------------------------------------------------------------------         MIXLEN.62
                             D I M E N S I O N                                   MIXLEN.63
     & U     (LM),V     (LM)                                                     MIXLEN.64
     &,T     (LM),Q     (LM),Q2    (LM)                                          MIXLEN.65
                             D I M E N S I O N                                   MIXLEN.66
     & GM    (LM1),GH    (LM1),EL    (LM1)                                       MIXLEN.67
     &,APE   (LM )                                                               MIXLEN.68
     &,Z     (LP1)                                                               MIXLEN.69
C-----------------------------------------------------------------------         MIXLEN.70
                             D I M E N S I O N                                   MIXLEN.71
     & ELM   (LM1)                                                               MIXLEN.72
     &,Q1    (LM ),THV   (LM )                                                   MIXLEN.73
C-----------------------------------------------------------------------         MIXLEN.74
C***********************************************************************         MIXLEN.75
      LMHM=LMHK-1                                                                MIXLEN.76
      LMHP=LMHK+1                                                                MIXLEN.77
C-----------------------------------------------------------------------         MIXLEN.78
      DO 100 L=1,LMHK                                                            MIXLEN.79
        THV(L)=(0.608*Q(L)+1.)*T(L)*APE(L)                                       MIXLEN.80
        Q1(L)=0.                                                                 MIXLEN.81
  100 CONTINUE                                                                   MIXLEN.82
C-----------------------------------------------------------------------         MIXLEN.83
      DO 110 L=1,LMHM                                                            MIXLEN.84
        RDZ=2./(Z(L)-Z(L+2))                                                     MIXLEN.85
        GML=((U(L)-U(L+1))**2+(V(L)-V(L+1))**2)*RDZ*RDZ                          MIXLEN.86
        GM(L)=AMAX1(GML,EPSGM)                                                   MIXLEN.87
        GHL=(THV(L)-THV(L+1))*RDZ                                                MIXLEN.88
        IF(ABS(GHL).LE.EPSGH)GHL=EPSGH                                           MIXLEN.89
        GH(L)=GHL                                                                MIXLEN.90
  110 CONTINUE                                                                   MIXLEN.91
C--------------FIND MAXIMUM MIXING LENGTHS & THE LEVEL OF THE PBL TOP---         MIXLEN.92
      LPBL=LMHK                                                                  MIXLEN.93
      DO 120 L=1,LMHM                                                            MIXLEN.94
        GML=GM(L)                                                                MIXLEN.95
        GHL=GH(L)                                                                MIXLEN.96
        IF(GHL.GE.EPSGH)THEN                                                     MIXLEN.97
          IF(GML/GHL.LE.REQU)THEN                                                MIXLEN.98
            ELM(L)=EPSL                                                          MIXLEN.99
            LPBL=L                                                               MIXLEN.100
          ELSE                                                                   MIXLEN.101
            AUBR=(AUBM*GML+AUBH*GHL)*GHL                                         MIXLEN.102
            BUBR=BUBM*GML+BUBH*GHL                                               MIXLEN.103
            QOL2ST=(-0.5*BUBR+SQRT(BUBR*BUBR*0.25-AUBR*CUBR))*RCUBR              MIXLEN.104
            QOL2ST=AMAX1(QOL2ST,1.E-20)                                          24SEP99.420
            ELOQ2X=1./QOL2ST                                                     MIXLEN.105
            ELM(L)=AMAX1(SQRT(ELOQ2X*Q2(L)),EPSL)                                MIXLEN.106
          ENDIF                                                                  MIXLEN.107
        ELSE                                                                     MIXLEN.108
          ADEN=(ADNM*GML+ADNH*GHL)*GHL                                           MIXLEN.109
          BDEN=BDNM*GML+BDNH*GHL                                                 MIXLEN.110
          QOL2UN=-0.5*BDEN+SQRT(BDEN*BDEN*0.25-ADEN)                             MIXLEN.111
          QOL2UN=AMAX1(QOL2UN,1.E-20)                                            24SEP99.421
          ELOQ2X=REPSR1/QOL2UN                                                   MIXLEN.112
          ELM(L)=AMAX1(SQRT(ELOQ2X*Q2(L)),EPSL)                                  MIXLEN.113
        ENDIF                                                                    MIXLEN.114
  120 CONTINUE                                                                   MIXLEN.115
C-----------------------------------------------------------------------         MIXLEN.116
      IF(ELM(LMHM).EQ.EPSL)LPBL=LMHK                                             MIXLEN.117
C--------------THE HEIGHT OF THE PBL------------------------------------         MIXLEN.118
      HPBL=Z(LPBL)-Z(LMHP)                                                       MIXLEN.119
C-----------------------------------------------------------------------         MIXLEN.120
      DO 150 L=LPBL,LMHK                                                         MIXLEN.121
        Q1(L)=SQRT(Q2(L))                                                        MIXLEN.122
  150 CONTINUE                                                                   MIXLEN.123
C-----------------------------------------------------------------------         MIXLEN.124
      SZQ=0.                                                                     MIXLEN.125
      SQ=0.                                                                      MIXLEN.126
      DO 160 L=1,LMHM                                                            MIXLEN.127
        QDZL=(Q1(L)+Q1(L+1))*(Z(L+1)-Z(L+2))                                     MIXLEN.128
        SZQ=(Z(L+1)+Z(L+2)-Z(LMHP)-Z(LMHP))*QDZL+SZQ                             MIXLEN.129
        SQ=QDZL+SQ                                                               MIXLEN.130
  160 CONTINUE                                                                   MIXLEN.131
C--------------COMPUTATION OF ASYMPTOTIC L IN BLACKADAR FORMULA---------         MIXLEN.132
      EL0=AMIN1(ALPH*SZQ*0.5/SQ,EL0MAX)                                          MIXLEN.133
      EL0=AMAX1(EL0,EL0MIN)                                                      MIXLEN.134
C--------------INSIDE THE PBL-------------------------------------------         MIXLEN.135
      DO 170 L=1,LMHM                                                            MIXLEN.136
        VKRMZ=(Z(L+1)-Z(LMHP))*VKRM                                              MIXLEN.137
        EL(L)=AMIN1(VKRMZ/(VKRMZ/EL0+1.),ELM(L))                                 MIXLEN.138
  170 CONTINUE                                                                   MIXLEN.139
C--------------CORRECTION ABOVE THE PBL TOP-----------------------------         MIXLEN.140
      LPBLM=MAX(LPBL-1,1)                                                        MIXLEN.141
      DO 180 L=1,LPBLM                                                           MIXLEN.142
        EL(L)=AMIN1((Z(L)-Z(L+2))*ELFC,ELM(L))                                   MIXLEN.143
  180 CONTINUE                                                                   MIXLEN.144
C-----------------------------------------------------------------------         MIXLEN.145
      RETURN                                                                     MIXLEN.146
      END                                                                        MIXLEN.147
                                                                                 MIXLEN.148
