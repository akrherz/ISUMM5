C***********************************************************************         CONSAT.1
      SUBROUTINE CONSAT                                                          CONSAT.2
C***********************************************************************         CONSAT.3
C     (LIN) SPECIFY SOME CONSTANTS IN SATICE ROUTINE                   *         CONSAT.4
C     LIN ET.AL.  J. CLIM. APPL. METEOR.  22, 1065-1092                *         CONSAT.5
C     MODIFIED AND CODED BY TAO AND SIMPSON (JAS, 1989; TAO, 1993)     *         CONSAT.6
C     RECENT CHANGES (6.25.01) BY SCOTT BRAUN INCLUDE                            05DEC01.754
C         * OPTION TO CHOOSE HAIL OR GRAUPEL AS THIRD ICE CATEGORY               05DEC01.755
C         * OPTION OF SATURATION ADJUSTMENT SCHEMES                              05DEC01.756
C         * INCLUSION OF GAMMA FUNCTION FOR EASIER VARIATION OF FALL             05DEC01.757
C           SPEED COEFFICIENTS                                                   05DEC01.758
C***********************************************************************         CONSAT.7
      COMMON/GSIZE/ TNW,TNSS,TNG,ROQR,ROQS,ROQG                                  05DEC01.759
      COMMON/CONT/ C76,C358,C172,C409,C218,C580,C610,C149,C879,C141              CONSAT.9
      COMMON/B3CS/ AGG,BGG,ASS,BSS,AWW,BWW,BGH,BGQ,BSH,BSQ,BWH,BWQ               CONSAT.10
      COMMON/BTERV/ ZRC,ZGC,ZSC,VRC,VGC,VSC                                      CONSAT.11
      COMMON/BSNW/ ALV,ALF,ALS,T0K,T00K,AVC,AFC,ASC,RN1,BND1,RN2,BND2,           CONSAT.12
     1   RN3,RN4,RN5,RN6,RN7,RN8,RN9,RN10,RN101,RN10A,RN11,RN11A,                CONSAT.13
     2   RN12,RN12A(31),RN12B(31),RN14,RN15,RN15A,RN16,RN17,                     05DEC01.760
     3   RN17A,RN17B,RN17C,RN18,RN18A,RN19,RN19A,RN19B,RN20,RN20A,RN20B,         CONSAT.15
     4   BND3,RN21,RN22,RN23,RN23A,RN23B,RN25,RN25A(31),RN30A,RN30B,             CONSAT.16
     5   RN30C,RN31,BETA,RN32,AMI40,AMI50,AMI100,IHAIL,IWATER                    05DEC01.761
C                                                                                CONSAT.18
      DIMENSION A1(31),A2(31)                                                    CONSAT.19
      DATA A1/.7939E-7,.7841E-6,.3369E-5,.4336E-5,.5285E-5,.3728E-5,             CONSAT.20
     1   .1852E-5,.2991E-6,.4248E-6,.7434E-6,.1812E-5,.4394E-5,.9145E-5,         CONSAT.21
     2   .1725E-4,.3348E-4,.1725E-4,.9175E-5,.4412E-5,.2252E-5,.9115E-6,         CONSAT.22
     3   .4876E-6,.3473E-6,.4758E-6,.6306E-6,.8573E-6,.7868E-6,.7192E-6,         CONSAT.23
     4   .6513E-6,.5956E-6,.5333E-6,.4834E-6/                                    CONSAT.24
      DATA A2/.4006,.4831,.5320,.5307,.5319,.5249,.4888,.3894,.4047,             CONSAT.25
     1   .4318,.4771,.5183,.5463,.5651,.5813,.5655,.5478,.5203,.4906,            CONSAT.26
     2   .4447,.4126,.3960,.4149,.4320,.4506,.4483,.4460,.4433,.4413,            CONSAT.27
     3   .4382,.4361/                                                            CONSAT.28
C                                                                                05DEC01.762
C                                                                                05DEC01.763
C     USE GRAUPEL OR HAIL (0=GRAUPEL, 1=HAIL)                                    05DEC01.764
C                                                                                05DEC01.765
                                                                                 05DEC01.766
      IHAIL=0                                                                    05DEC01.767
                                                                                 05DEC01.768
C                                                                                05DEC01.769
C*****   IWATER=0 USES A SLIGHT VARIANT OF THE ORIGINAL TAO ET AL METHOD.        05DEC01.770
C*****   THE ONLY DIFFERENCE IS THAT THE WEIGHTING BETWEEN THE LIQUID            05DEC01.771
C*****   AND ICE SATURATION VALUES IS DONE BY TEMPERATURE RATHER THAN MASS.      05DEC01.772
C*****   IWATER=1 USES A SEQUENTIAL METHOD IN WHICH THE ADJUSTMENT IS FIRST      05DEC01.773
C*****   DONE FOR LIQUID WATER FOR TEMPERATURES WARMER THAN 253K, THEN FOR       05DEC01.774
C*****   ICE ONLY WHERE THE TEMPERATURE IS COLDER THAN 258K. THE MAIN EFFECT OF  05DEC01.775
C*****   THIS CHANGE IS TO REDUCE THE AMOUNT OF SUPERCOOLED WATER AT VERY COLD   05DEC01.776
C*****   TEMPERATURES                                                            05DEC01.777
                                                                                 05DEC01.778
      IWATER=1                                                                   05DEC01.779
                                                                                 05DEC01.780
C*****************************************************************               05DEC01.781
                                                                                 05DEC01.782
C                                                                                CONSAT.45
      CP=1.003E7                                                                 05DEC01.783
      CPI=4.*ATAN(1.)                                                            CONSAT.46
      CPI2=CPI*CPI                                                               CONSAT.47
      GRVT=980.                                                                  CONSAT.48
      TCA=2.43E3                                                                 CONSAT.49
      DWV=.226                                                                   CONSAT.50
      DVA=1.718E-4                                                               CONSAT.51
      AMW=18.016                                                                 CONSAT.52
      ARS=8.314E7                                                                CONSAT.53
      SCV=2.2904487                                                              CONSAT.54
      T0K=273.16                                                                 CONSAT.55
      T00K=238.16                                                                CONSAT.56
      ALV=2.5E10                                                                 CONSAT.57
      ALF=3.336E9                                                                CONSAT.58
      ALS=2.8336E10                                                              CONSAT.59
      AVC=ALV/CP                                                                 CONSAT.60
      AFC=ALF/CP                                                                 CONSAT.61
      ASC=ALS/CP                                                                 CONSAT.62
      RW=4.615E6                                                                 CONSAT.63
      CW=4.187E7                                                                 CONSAT.64
      CI=2.093E7                                                                 CONSAT.65
      C76=7.66                                                                   CONSAT.66
      C358=35.86                                                                 CONSAT.67
      C172=17.26939                                                              CONSAT.68
      C409=4098.026                                                              CONSAT.69
      C218=21.87456                                                              CONSAT.70
      C580=5807.695                                                              CONSAT.71
      C610=6.1078E3                                                              CONSAT.72
      C149=1.496286E-5                                                           CONSAT.73
      C879=8.794142                                                              CONSAT.74
      C141=1.4144354E7                                                           CONSAT.75
                                                                                 05DEC01.784
C***   DEFINE THE DENSITY AND SIZE DISTRIBUTION OF PRECIPITATION                 05DEC01.785
C***   DEFINE THE COEFFICIENTS USED IN TERMINAL VELOCITY                         CONSAT.76
C**********   HAIL OR GRAUPEL PARAMETERS   **********                            05DEC01.786
      if (ihail .eq. 1) then                                                     05DEC01.787
        ROQG=.9                                                                  05DEC01.788
        TNG=.002                                                                 05DEC01.789
        CD1=6.E-1                                                                05DEC01.790
        CD2=4.*GRVT/(3.*CD1)                                                     05DEC01.791
        AGG=SQRT(CD2*ROQG)                                                       05DEC01.792
        BGG=.5                                                                   05DEC01.793
      else                                                                       05DEC01.794
        ROQG=.4                                                                  05DEC01.795
        TNG=.04                                                                  05DEC01.796
        AGG=351.2                                                                05DEC01.797
        BGG=.37                                                                  05DEC01.798
      endif                                                                      05DEC01.799
C**********         SNOW PARAMETERS        **********                            05DEC01.800
      ROQS=.1                                                                    05DEC01.801
      TNSS=1.                                                                    05DEC01.802
      ASS=152.93                                                                 CONSAT.79
      BSS=.25                                                                    CONSAT.80
C**********         RAIN PARAMETERS        **********                            05DEC01.803
      ROQR=1.                                                                    05DEC01.804
      TNW=.08                                                                    05DEC01.805
      AWW=2115.                                                                  CONSAT.81
      BWW=.8                                                                     CONSAT.82
C*****************************************************************               05DEC01.806
C                                                                                05DEC01.807
C                                                                                05DEC01.808
      BGH=.5*BGG                                                                 CONSAT.83
      BSH=.5*BSS                                                                 CONSAT.84
      BWH=.5*BWW                                                                 CONSAT.85
      BGQ=.25*BGG                                                                CONSAT.86
      BSQ=.25*BSS                                                                CONSAT.87
      BWQ=.25*BWW                                                                CONSAT.88
      GA3B=gamma(3.+BWW)                                                         05DEC01.809
      GA4B=gamma(4.+BWW)                                                         05DEC01.810
      GA6B=gamma(6.+BWW)                                                         05DEC01.811
      GA5BH=gamma((5.+BWW)/2.)                                                   05DEC01.812
      GA3G=gamma(3.+BGG)                                                         05DEC01.813
      GA4G=gamma(4.+BGG)                                                         05DEC01.814
      GA5GH=gamma((5.+BGG)/2.)                                                   05DEC01.815
      GA3D=gamma(3.+BSS)                                                         05DEC01.816
      GA4D=gamma(4.+BSS)                                                         05DEC01.817
      GA5DH=gamma((5.+BSS)/2.)                                                   05DEC01.818
      ZRC=(CPI*ROQR*TNW)**0.25                                                   CONSAT.123
      ZSC=(CPI*ROQS*TNSS)**0.25                                                  CONSAT.124
      ZGC=(CPI*ROQG*TNG)**0.25                                                   CONSAT.125
      VRC=AWW*GA4B/(6.*ZRC**BWW)                                                 05DEC01.819
      VSC=ASS*GA4D/(6.*ZSC**BSS)                                                 05DEC01.820
      VGC=AGG*GA4G/(6.*ZGC**BGG)                                                 05DEC01.821
C     ****************************                                               CONSAT.129
      RN1=1.E-3                                                                  CONSAT.130
      RN2=1.E-3                                                                  CONSAT.132
      BND1=5.E-4                                                                 05DEC01.822
      BND2=1.25E-3                                                               05DEC01.823
      RN3=.25*CPI*TNSS*ASS*GA3D                                                  05DEC01.824
      ESW=1.                                                                     CONSAT.135
      RN4=.25*CPI*ESW*TNSS*ASS*GA3D                                              05DEC01.825
      ERI=1.                                                                     CONSAT.137
      RN5=.25*CPI*ERI*TNW*AWW*GA3B                                               05DEC01.826
      AMI=1./(24.*4.19E-10)                                                      CONSAT.139
      RN6=CPI2*ERI*TNW*AWW*ROQR*GA6B*AMI                                         05DEC01.827
      ESR=1.                                                                     CONSAT.141
      RN7=CPI2*ESR*TNW*TNSS*ROQS                                                 CONSAT.142
      RN8=CPI2*ESR*TNW*TNSS*ROQR                                                 CONSAT.143
      RN9=CPI2*TNSS*TNG*ROQS                                                     CONSAT.144
      RN10=2.*CPI*TNSS                                                           CONSAT.145
      RN101=.31*GA5DH*SQRT(ASS)                                                  05DEC01.828
      RN10A=ALS*ALS/RW                                                           CONSAT.147
      RN11=2.*CPI*TNSS/ALF                                                       CONSAT.148
      RN11A=CW/ALF                                                               CONSAT.149
      AMI40=2.41e-8                                                              05DEC01.829
      AMI50=3.76e-8                                                              05DEC01.830
      AMI100=1.51e-7                                                             05DEC01.831
      EIW=1.                                                                     CONSAT.152
      UI50=20.                                                                   05DEC01.832
      RI50=10.e-3                                                                05DEC01.833
      CMN=1.05E-15                                                               CONSAT.155
      RN12=CPI*EIW*UI50*RI50**2                                                  CONSAT.156
      DO 10 K=1,31                                                               CONSAT.157
        Y1=1.-A2(K)                                                              CONSAT.158
        RN13=A1(K)*Y1/(AMI100**Y1-AMI40**Y1)                                     05DEC01.834
        RN12A(K)=RN13/AMI100                                                     05DEC01.835
        RN12B(K)=A1(K)*AMI100**A2(K)                                             05DEC01.836
        RN25A(K)=A1(K)*CMN**A2(K)                                                CONSAT.162
   10 CONTINUE                                                                   CONSAT.163
      EGW=1.                                                                     CONSAT.164
      EGI=.1                                                                     CONSAT.166
      EGI2=1.                                                                    05DEC01.837
      RN14=.25*CPI*EGW*TNG*GA3G*agg                                              05DEC01.838
      RN15=.25*CPI*EGI*TNG*GA3G*agg                                              05DEC01.839
      RN15A=.25*CPI*EGI2*TNG*GA3G*agg                                            05DEC01.840
      EGR=1.                                                                     CONSAT.170
      RN16=CPI2*EGR*TNG*TNW*ROQR                                                 CONSAT.171
      RN17=2.*CPI*TNG                                                            CONSAT.172
      RN17A=.31*GA5GH*sqrt(agg)                                                  05DEC01.841
      RN17B=CW-CI                                                                CONSAT.174
      RN17C=CW                                                                   CONSAT.175
      APRI=.66                                                                   CONSAT.176
      BPRI=1.E-4                                                                 CONSAT.177
      RN18=20.*CPI2*BPRI*TNW*ROQR                                                CONSAT.178
      RN18A=APRI                                                                 CONSAT.179
      RN19=2.*CPI*TNG/ALF                                                        CONSAT.180
      RN19A=.31*GA5GH*sqrt(agg)                                                  05DEC01.842
      RN19B=CW/ALF                                                               CONSAT.182
      RN20=2.*CPI*TNG                                                            CONSAT.183
      RN20A=ALS*ALS/RW                                                           CONSAT.184
      RN20B=.31*GA5GH*sqrt(agg)                                                  05DEC01.843
      BND3=.5E-3                                                                 05DEC01.844
      RN21=1.E3*1.569E-12/0.15                                                   CONSAT.187
      ERW=1.                                                                     CONSAT.188
      RN22=.25*CPI*ERW*AWW*TNW*GA3B                                              05DEC01.845
      RN23=2.*CPI*TNW                                                            CONSAT.190
      RN23A=.31*GA5BH*SQRT(AWW)                                                  05DEC01.846
      RN23B=ALV*ALV/RW                                                           CONSAT.192
      CN0=1.E-6                                                                  05DEC01.847
      RN25=CN0                                                                   05DEC01.848
      RN30A=ALV*ALS*AMW/(TCA*ARS)                                                CONSAT.195
      RN30B=ALV/TCA                                                              CONSAT.196
      RN30C=ARS/(DWV*AMW)                                                        CONSAT.197
      RN31=1.E-17                                                                CONSAT.198
      BETA=-.46                                                                  05DEC01.849
      RN32=4.*51.545E-4                                                          CONSAT.200
      RETURN                                                                     CONSAT.201
      END                                                                        CONSAT.202
                                                                                 CONSAT.203
