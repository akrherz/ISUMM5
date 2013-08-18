C-----------------------------------------------------------------------         RADINI.1
C---------------------------------------------------------------------           RADINI.2
      SUBROUTINE RADINI(GRAVX,CPAIRX,EPSILOX,STEBOLX)                            RADINI.3
C                                                                                RADINI.4
C MODIFIED VERSION OF INITIALIZATION FOR RADIATION SCHEME; DONE                  RADINI.5
C FOR THE COLUMN RADIATION MODEL                                                 RADINI.6
C                                                                                RADINI.7
C NOTE: RADIATION SCHEME USES CGS UNITS                                          RADINI.8
C                                                                                RADINI.9
C     INCLUDE 'PARAME'                                                           RADINI.10
C     INCLUDE 'RADCCM2.DATA'                                                     RADINI.11
#     include <parame.incl>                                                      RADINI.12
#     include <parccm2.incl>                                                     RADINI.13
#     include <radccm2.incl>                                                     RADINI.14
C-----------------------------------------------------------------------         RADINI.15
C                                                                                RADINI.16
C WATER VAPOR NARROW BAND CONSTANTS FOR LW COMPUTATIONS                          RADINI.17
C                                                                                RADINI.18
      REAL REALK,ST,A1,A2,B1,B2,                                                 RADINI.19
     $     COEFA,COEFB,COEFC,COEFD,                                              RADINI.20
     $     COEFE,COEFF,COEFG,COEFH,                                              RADINI.21
     $     COEFI,COEFJ,COEFK,                                                    RADINI.22
     $     C1,C2,C3,C4,C5,C6,C7,                                                 RADINI.23
     $     C8 ,C9 ,C10,C11,C12,C13,C14,C15,C16,C17,                              RADINI.24
     $     C18,C19,C20,C21,C22,C23,C24,C25,C26,C27,                              RADINI.25
     $     C28,C29,C30,C31,                                                      RADINI.26
     $     FWCOEF,FWC1,FWC2,FC1,CFA1                                             RADINI.27
C                                                                                RADINI.28
      COMMON/CRDCAE/REALK(2), ST(2), A1(2), A2(2), B1(2), B2(2),                 RADINI.29
C                                                                                RADINI.30
C CONSTANT COEFFICIENTS FOR WATER VAPOR ABSORPTIVITY AND EMISSIVI                RADINI.31
C                                                                                RADINI.32
     $              COEFA(3,4),COEFB(4,4),COEFC(3,4),COEFD(4,4),                 RADINI.33
     $              COEFE(3,4),COEFF(6,2),COEFG(2,4),COEFH(2,4),                 RADINI.34
     $              COEFI(6,2),COEFJ(3,2),COEFK(3,2),                            RADINI.35
     $              C1(4),C2(4),C3(4),C4(4),C5(4),C6(4),C7(4),                   RADINI.36
     $              C8 ,C9 ,C10,C11,C12,C13,C14,C15,C16,C17,                     RADINI.37
     $              C18,C19,C20,C21,C22,C23,C24,C25,C26,C27,                     RADINI.38
     $              C28,C29,C30,C31,                                             RADINI.39
C                                                                                RADINI.40
C FARWING CORRECTION CONSTANTS FOR NARROW-BAND EMISSIVITY MODEL                  RADINI.41
C INTRODUCE FARWING CORRECTION TO ACCOUNT FOR THE                                RADINI.42
C DEFICIENCIES IN NARROW-BAND MODEL USED TO DERIVE THE                           RADINI.43
C EMISSIVITY. TUNED WITH ARKINGS LINE CALCULATIONS.                              RADINI.44
C                                                                                RADINI.45
     $              FWCOEF,FWC1,FWC2,FC1,CFA1                                    RADINI.46
C                                                                                RADINI.47
C                                                                                RADINI.48
C INPUT ARGUMENTS                                                                RADINI.49
C                                                                                RADINI.50
      REAL GRAVX,       ! GRAVITATIONAL ACCELERATION (M/S**2)                    RADINI.51
     $     CPAIRX,      ! HEAT CAPACITY DRY AIR AT CONSTANT PRS (J/KG/K)         RADINI.52
     $     EPSILOX,     ! RATIO MEAN MOL WEIGHT H2O TO DRY AIR                   RADINI.53
     $     STEBOLX      ! STEFAN-BOLTZMANN CONSTANT (W/M**2/K**4)                RADINI.54
C                                                                                RADINI.55
C                                                                                RADINI.56
C LOCAL WORKSPACE                                                                RADINI.57
C                                                                                RADINI.58
      INTEGER   IBAND   ! H2O BAND INDEX                                         RADINI.59
C                                                                                RADINI.60
C /CRDCAE/                                                                       RADINI.61
C H2O EMISSIVITY AND ABSORTIVITY COEFFICIENTS                                    RADINI.62
C                                                                                RADINI.63
      DATA COEFD/7.03047E-01,-2.63501E-03,-1.57023E-06,0.0,                      RADINI.64
     $           5.29269E-01,-3.14754E-03, 4.39595E-06,0.0,                      RADINI.65
     $           7.88193E-02, 1.31290E-03, 4.25827E-06,-1.23982E-08,             RADINI.66
     $           1.62744E-01, 2.22847E-03, 2.60102E-06,-4.30133E-08/             RADINI.67
C                                                                                RADINI.68
      DATA COEFB/8.85675E+00,-3.51620E-02, 2.38653E-04,-1.71439E-06,             RADINI.69
     $           5.73841E+00,-1.91919E-02, 1.65993E-04,-1.54665E-06,             RADINI.70
     $           6.64034E+00, 1.56651E-02,-9.73357E-05, 0.0,                     RADINI.71
     $           7.09281E+00, 1.40056E-02,-1.15774E-04, 0.0/                     RADINI.72
C                                                                                RADINI.73
      DATA COEFE/3.93137E-02,-4.34341E-05,3.74545E-07,                           RADINI.74
     $           3.67785E-02,-3.10794E-05,2.94436E-07,                           RADINI.75
     $           7.42500E-02, 3.97397E-05,0.0,                                   RADINI.76
     $           7.52859E-02, 4.18073E-05,0.0/                                   RADINI.77
C                                                                                RADINI.78
      DATA COEFA/1.01400E+00,6.41695E-03,2.85787E-05,                            RADINI.79
     $           1.01320E+00,6.86400E-03,2.96961E-05,                            RADINI.80
     $           1.02920E+00,1.01680E-02,5.30226E-05,                            RADINI.81
     $           1.02743E+00,9.85113E-03,5.00233E-05/                            RADINI.82
C                                                                                RADINI.83
      DATA COEFC/9.90127E-01,1.22475E-03,4.90135E-06,                            RADINI.84
     $           9.89753E-01,1.97081E-03,3.42046E-06,                            RADINI.85
     $           9.75230E-01,1.03341E-03,0.0,                                    RADINI.86
     $           9.77366E-01,8.60014E-04,0.0/                                    RADINI.87
C                                                                                RADINI.88
      DATA COEFF/2.2037 E-01,1.39719E-03,-7.32011E-06,                           RADINI.89
     $          -1.40262E-08,2.13638E-10,-2.35955E-13,                           RADINI.90
     $           3.07431E-01,8.27225E-04,-1.30067E-05,                           RADINI.91
     $           3.49847E-08,2.07835E-10,-1.98937E-12/                           RADINI.92
C                                                                                RADINI.93
      DATA COEFG/9.04489E+00,-9.56499E-03,                                       RADINI.94
     $           1.80898E+01,-1.91300E-02,                                       RADINI.95
     $           8.72239E+00,-9.53359E-03,                                       RADINI.96
     $           1.74448E+01,-1.90672E-02/                                       RADINI.97
C                                                                                RADINI.98
      DATA COEFH/5.46557E+01,-7.30387E-02,                                       RADINI.99
     $           1.09311E+02,-1.46077E-01,                                       RADINI.100
     $           5.11479E+01,-6.82615E-02,                                       RADINI.101
     $           1.02296E+02,-1.36523E-01/                                       RADINI.102
C                                                                                RADINI.103
      DATA COEFI/3.31654E-01,-2.86103E-04,-7.87860E-06,                          RADINI.104
     $           5.88187E-08,-1.25340E-10,-1.37731E-12,                          RADINI.105
     $           3.14365E-01,-1.33872E-03,-2.15585E-06,                          RADINI.106
     $           6.07798E-08,-3.45612E-10,-9.34139E-15/                          RADINI.107
C                                                                                RADINI.108
      DATA COEFJ/2.82096E-02,2.47836E-04,1.16904E-06,                            RADINI.109
     $           9.27379E-02,8.04454E-04,6.88844E-06/                            RADINI.110
C                                                                                RADINI.111
      DATA COEFK/2.48852E-01,2.09667E-03,2.60377E-06,                            RADINI.112
     $           1.03594E+00,6.58620E-03,4.04456E-06/                            RADINI.113
C                                                                                RADINI.114
C NARROW BAND DATA FOR H2O                                                       RADINI.115
C 200CM DATA FOR 800-1000 CM-1 AND 1000-1200 CM-1.                               RADINI.116
C                                                                                RADINI.117
      DATA REALK/  0.18967069430426E-04, 0.70172244841851E-04   /                RADINI.118
      DATA   ST /  0.31930234492350E-03, 0.97907319939060E-03   /                RADINI.119
      DATA   A1 /  0.28775403075736E-01, 0.23236701470511E-01   /                RADINI.120
      DATA   A2 / -0.57966222388131E-04,-0.95105504388411E-04   /                RADINI.121
      DATA   B1 /  0.29927771523756E-01, 0.21737073577293E-01   /                RADINI.122
      DATA   B2 / -0.86322071248593E-04,-0.78543550629536E-04   /                RADINI.123
C                                                                                RADINI.124
C                                                                                RADINI.125
C SET GENERAL RADIATION CONSTANTS; CONVERT TO CGS UNITS WHERE APPROPRIAT         RADINI.126
C                                                                                RADINI.127
      GRAVIT=100.*GRAVX                                                          RADINI.128
      RGA=1./GRAVIT                                                              RADINI.129
      CPAIR=1.E4*CPAIRX                                                          RADINI.130
      EPSILO=EPSILOX                                                             RADINI.131
      SSLP=1.013250E6                                                            RADINI.132
      STEBOL=1.E3*STEBOLX                                                        RADINI.133
      RGSSLP=0.5/(GRAVIT*SSLP)                                                   RADINI.134
      CO2VMR=3.3E-4                                                              RADINI.135
      DPFO3=2.5E-3                                                               RADINI.136
      DPFCO2=5.0E-3                                                              RADINI.137
      DAYSPY=365.                                                                RADINI.138
      PIE=4.*ATAN(1.)                                                            RADINI.139
C                                                                                RADINI.140
C COEFFICIENTS FOR H2O EMISSIVITY AND ABSORTIVITY.                               RADINI.141
C                                                                                RADINI.142
      DO 10 IBAND=1,4                                                            RADINI.143
        C1(IBAND)=COEFE(3,IBAND)/COEFE(2,IBAND)                                  RADINI.144
        C2(IBAND)=COEFB(3,IBAND)/COEFB(2,IBAND)                                  RADINI.145
        C3(IBAND)=COEFB(4,IBAND)/COEFB(3,IBAND)                                  RADINI.146
        C4(IBAND)=COEFD(3,IBAND)/COEFD(2,IBAND)                                  RADINI.147
        C5(IBAND)=COEFD(4,IBAND)/COEFD(3,IBAND)                                  RADINI.148
        C6(IBAND)=COEFA(3,IBAND)/COEFA(2,IBAND)                                  RADINI.149
        C7(IBAND)=COEFC(3,IBAND)/COEFC(2,IBAND)                                  RADINI.150
   10 CONTINUE                                                                   RADINI.151
      C8=COEFF(3,1)/COEFF(2,1)                                                   RADINI.152
      C9=COEFF(3,2)/COEFF(2,2)                                                   RADINI.153
      C10=COEFF(4,1)/COEFF(3,1)                                                  RADINI.154
      C11=COEFF(4,2)/COEFF(3,2)                                                  RADINI.155
      C12=COEFF(5,1)/COEFF(4,1)                                                  RADINI.156
      C13=COEFF(5,2)/COEFF(4,2)                                                  RADINI.157
      C14=COEFF(6,1)/COEFF(5,1)                                                  RADINI.158
      C15=COEFF(6,2)/COEFF(5,2)                                                  RADINI.159
      C16=COEFJ(3,1)/COEFJ(2,1)                                                  RADINI.160
      C17=COEFK(3,1)/COEFK(2,1)                                                  RADINI.161
      C18=COEFI(3,1)/COEFI(2,1)                                                  RADINI.162
      C19=COEFI(3,2)/COEFI(2,2)                                                  RADINI.163
      C20=COEFI(4,1)/COEFI(3,1)                                                  RADINI.164
      C21=COEFI(4,2)/COEFI(3,2)                                                  RADINI.165
      C22=COEFI(5,1)/COEFI(4,1)                                                  RADINI.166
      C23=COEFI(5,2)/COEFI(4,2)                                                  RADINI.167
      C24=COEFI(6,1)/COEFI(5,1)                                                  RADINI.168
      C25=COEFI(6,2)/COEFI(5,2)                                                  RADINI.169
      C26=COEFJ(3,2)/COEFJ(2,2)                                                  RADINI.170
      C27=COEFK(3,2)/COEFK(2,2)                                                  RADINI.171
      C28=.5                                                                     RADINI.172
      C29=.002053                                                                RADINI.173
      C30=.1                                                                     RADINI.174
      C31=3.0E-5                                                                 RADINI.175
      CFA1=.61                                                                   RADINI.176
C                                                                                RADINI.177
C INITIALIZE FURTHER LONGWAVE CONSTANTS REFERRING TO                             RADINI.178
C FAR WING CORRECTION:                                                           RADINI.179
C                                                                                RADINI.180
      FWCOEF=.1                                                                  RADINI.181
      FWC1=.30                                                                   RADINI.182
      FWC2=4.5                                                                   RADINI.183
      FC1=2.6                                                                    RADINI.184
C                                                                                RADINI.185
      RETURN                                                                     RADINI.186
      END                                                                        RADINI.187
                                                                                 RADINI.188
