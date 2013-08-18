CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                 O3DATA.1
C----------------------------------------------------------------------          O3DATA.2
      SUBROUTINE O3DATA (J, O3PROF)                                              O3DATA.3
C                                                                                O3DATA.4
#     include <parame.incl>                                                      O3DATA.5
#     include <param3.incl>                                                      O3DATA.6
#     include <point2d.incl>                                                     O3DATA.7
#     include <point3d.incl>                                                     O3DATA.8
#     include <parccm2.incl>                                                     O3DATA.9
#     include <various.incl>                                                     O3DATA.10
#     include <nonhyd.incl>                                                      O3DATA.11
C     INCLUDE 'PARAME'                                                           O3DATA.12
C     INCLUDE 'PARAM1.CB'                                                        O3DATA.13
C     INCLUDE 'PARAM3.CB'                                                        O3DATA.14
C     INCLUDE 'MAIN.CB'                                                          O3DATA.15
C     INCLUDE 'RAD.CB'                                                           O3DATA.16
      PARAMETER (KXP2=MKX+2)                                                     O3DATA.17
      DIMENSION O3PROF(IPLOND,IPLEVP)                                            O3DATA.18
      DIMENSION PRLEVH(KXP2),PPWRKH(32),                                         O3DATA.19
     1 O3WRK(31),PPWRK(31),O3SUM(31),PPSUM(31),O3WIN(31),PPWIN(31),              O3DATA.20
     2 O3ANN(31),PPANN(31)                                                       O3DATA.21
      DATA O3SUM  /5.297E-8,5.852E-8,6.579E-8,7.505E-8,                          O3DATA.22
     1  8.577E-8,9.895E-8,1.175E-7,1.399E-7,1.677E-7,2.003E-7,                   O3DATA.23
     2  2.571E-7,3.325E-7,4.438E-7,6.255E-7,8.168E-7,1.036E-6,                   O3DATA.24
     3  1.366E-6,1.855E-6,2.514E-6,3.240E-6,4.033E-6,4.854E-6,                   O3DATA.25
     4  5.517E-6,6.089E-6,6.689E-6,1.106E-5,1.462E-5,1.321E-5,                   O3DATA.26
     5  9.856E-6,5.960E-6,5.960E-6/                                              O3DATA.27
      DATA PPSUM  /955.890,850.532,754.599,667.742,589.841,                      O3DATA.28
     1  519.421,455.480,398.085,347.171,301.735,261.310,225.360,                 O3DATA.29
     2  193.419,165.490,141.032,120.125,102.689, 87.829, 75.123,                 O3DATA.30
     3   64.306, 55.086, 47.209, 40.535, 34.795, 29.865, 19.122,                 O3DATA.31
     4    9.277,  4.660,  2.421,  1.294,  0.647/                                 O3DATA.32
C                                                                                O3DATA.33
      DATA O3WIN  /4.629E-8,4.686E-8,5.017E-8,5.613E-8,                          O3DATA.34
     1  6.871E-8,8.751E-8,1.138E-7,1.516E-7,2.161E-7,3.264E-7,                   O3DATA.35
     2  4.968E-7,7.338E-7,1.017E-6,1.308E-6,1.625E-6,2.011E-6,                   O3DATA.36
     3  2.516E-6,3.130E-6,3.840E-6,4.703E-6,5.486E-6,6.289E-6,                   O3DATA.37
     4  6.993E-6,7.494E-6,8.197E-6,9.632E-6,1.113E-5,1.146E-5,                   O3DATA.38
     5  9.389E-6,6.135E-6,6.135E-6/                                              O3DATA.39
      DATA PPWIN  /955.747,841.783,740.199,649.538,568.404,                      O3DATA.40
     1  495.815,431.069,373.464,322.354,277.190,237.635,203.433,                 O3DATA.41
     2  174.070,148.949,127.408,108.915, 93.114, 79.551, 67.940,                 O3DATA.42
     3   58.072, 49.593, 42.318, 36.138, 30.907, 26.362, 16.423,                 O3DATA.43
     4    7.583,  3.620,  1.807,  0.938,  0.469/                                 O3DATA.44
C                                                                                O3DATA.45
C     ILX = MIX-1                                                                O3DATA.46
C     JLX = MJX-1                                                                O3DATA.47
C     KL  = MKX                                                                  O3DATA.48
      DO 501 K=1,31                                                              O3DATA.49
        PPANN(K)=PPSUM(K)                                                        O3DATA.50
  501 CONTINUE                                                                   O3DATA.51
      O3ANN(1)=0.5*(O3SUM(1)+O3WIN(1))                                           O3DATA.52
C                                                                                O3DATA.53
      DO 502 K=2,31                                                              O3DATA.54
        O3ANN(K)=O3WIN(K-1)+(O3WIN(K)-O3WIN(K-1))/(PPWIN(K)-PPWIN(K-1))*         O3DATA.55
     +           (PPSUM(K)-PPWIN(K-1))                                           O3DATA.56
  502 CONTINUE                                                                   O3DATA.57
      DO 503 K=2,31                                                              O3DATA.58
        O3ANN(K)=0.5*(O3ANN(K)+O3SUM(K))                                         O3DATA.59
  503 CONTINUE                                                                   O3DATA.60
CX    IF (ISEAS .EQ. 1) THEN                                                     O3DATA.61
CX     DO 1 K=1,31                                                               O3DATA.62
CX      O3WRK(K)=O3SUM(K)                                                        O3DATA.63
CX      PPWRK(K)=PPSUM(K)                                                        O3DATA.64
CX1    CONTINUE                                                                  O3DATA.65
CX    ENDIF                                                                      O3DATA.66
C                                                                                O3DATA.67
CX    IF (ISEAS .EQ. 2) THEN                                                     O3DATA.68
CX     DO 2 K=1,31                                                               O3DATA.69
CX      O3WRK(K)=O3WIN(K)                                                        O3DATA.70
CX      PPWRK(K)=PPWIN(K)                                                        O3DATA.71
CX2    CONTINUE                                                                  O3DATA.72
CX    ENDIF                                                                      O3DATA.73
      DO 2 K=1,31                                                                O3DATA.74
        O3WRK(K)=O3ANN(K)                                                        O3DATA.75
        PPWRK(K)=PPANN(K)                                                        O3DATA.76
    2 CONTINUE                                                                   O3DATA.77
C                                                                                O3DATA.78
C   CALCULATE HALF PRESSURE LEVELS FOR MODEL AND DATA LEVELS                     O3DATA.79
C                                                                                O3DATA.80
      KLEVM1=KL-1                                                                O3DATA.81
      KLEVP1=KL+1                                                                O3DATA.82
      KLEVP2=KL+2                                                                O3DATA.83
C                                                                                O3DATA.84
C      DO 101 J=1,JLX                                                            O3DATA.85
#ifndef MPP1                                                                     O3DATA.86
      DO 100 I=1,IPLOND                                                          O3DATA.87
        ILIM=MIN0(I,ILX)                                                         O3DATA.88
        DO 10 K=KLEVP1,1,-1                                                      O3DATA.89
          KJ=KLEVP1-K+1                                                          O3DATA.90
          IF(K.GT.1.AND.K.LT.KLEVP1)PPBS=TWT(K,2)*PPB(ILIM,J,K-1)+               O3DATA.91
     +      TWT(K,1)*PPB(ILIM,J,K)                                               O3DATA.92
          IF(K.EQ.1)PPBS=PPB(ILIM,J,1)                                           O3DATA.93
          IF(K.EQ.KLEVP1)PPBS=PPB(ILIM,J,IPLEV)                                  O3DATA.94
          PRLEVH(KJ)=(SIGMA(K)*PSB(ILIM,J)+PTOP)*10.+PPBS/PSB(ILIM,J)*           O3DATA.95
     +               .01                                                         O3DATA.96
#else                                                                            O3DATA.97
          DO 100 I=1,ILX                                                         O3DATA.98
            DO 10 K=KLEVP1,1,-1                                                  O3DATA.99
              KJ=KLEVP1-K+1                                                      O3DATA.100
              IF(K.GT.1.AND.K.LT.KLEVP1)PPBS=TWT(K,2)*PPB(I,J,K-1)+              O3DATA.101
     +          TWT(K,1)*PPB(I,J,K)                                              O3DATA.102
              IF(K.EQ.1)PPBS=PPB(I,J,1)                                          O3DATA.103
              IF(K.EQ.KLEVP1)PPBS=PPB(I,J,IPLEV)                                 O3DATA.104
              PRLEVH(KJ)=(SIGMA(K)*PSB(I,J)+PTOP)*10.+PPBS/PSB(I,J)*.01          O3DATA.105
#endif                                                                           O3DATA.106
   10       CONTINUE                                                             O3DATA.107
                                                                                 O3DATA.108
          PPWRKH(1)=1100.                                                        O3DATA.109
          DO 20 K=2,31                                                           O3DATA.110
            PPWRKH(K)=(PPWRK(K)+PPWRK(K-1))/2.                                   O3DATA.111
   20     CONTINUE                                                               O3DATA.112
          PPWRKH(32)=0.                                                          O3DATA.113
          DO 30 K=1,KL                                                           O3DATA.114
            O3PROF(I,K)=0.                                                       O3DATA.115
            DO 25 JJ=1,31                                                        O3DATA.116
              IF((-(PRLEVH(K)-PPWRKH(JJ))).GE.0.)THEN                            O3DATA.117
                PB1=0.                                                           O3DATA.118
              ELSE                                                               O3DATA.119
                PB1=PRLEVH(K)-PPWRKH(JJ)                                         O3DATA.120
              ENDIF                                                              O3DATA.121
              IF((-(PRLEVH(K)-PPWRKH(JJ+1))).GE.0.)THEN                          O3DATA.122
                PB2=0.                                                           O3DATA.123
              ELSE                                                               O3DATA.124
                PB2=PRLEVH(K)-PPWRKH(JJ+1)                                       O3DATA.125
              ENDIF                                                              O3DATA.126
              IF((-(PRLEVH(K+1)-PPWRKH(JJ))).GE.0.)THEN                          O3DATA.127
                PT1=0.                                                           O3DATA.128
              ELSE                                                               O3DATA.129
                PT1=PRLEVH(K+1)-PPWRKH(JJ)                                       O3DATA.130
              ENDIF                                                              O3DATA.131
              IF((-(PRLEVH(K+1)-PPWRKH(JJ+1))).GE.0.)THEN                        O3DATA.132
                PT2=0.                                                           O3DATA.133
              ELSE                                                               O3DATA.134
                PT2=PRLEVH(K+1)-PPWRKH(JJ+1)                                     O3DATA.135
              ENDIF                                                              O3DATA.136
              O3PROF(I,K)=O3PROF(I,K)+(PB2-PB1-PT2+PT1)*O3WRK(JJ)                O3DATA.137
   25       CONTINUE                                                             O3DATA.138
            O3PROF(I,K)=O3PROF(I,K)/(PRLEVH(K)-PRLEVH(K+1))                      O3DATA.139
   30     CONTINUE                                                               O3DATA.140
  100   CONTINUE                                                                 O3DATA.141
                                                                                 O3DATA.142
C101   CONTINUE                                                                  O3DATA.143
C                                                                                O3DATA.144
      RETURN                                                                     O3DATA.145
      END                                                                        O3DATA.146
                                                                                 O3DATA.147
